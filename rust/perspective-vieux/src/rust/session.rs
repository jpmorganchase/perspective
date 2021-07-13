////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2018, the Perspective Authors.
//
// This file is part of the Perspective library, distributed under the terms
// of the Apache License 2.0.  The full license can be found in the LICENSE
// file.

mod copy;
mod download;
mod view;
mod view_subscription;

use crate::config::*;
use crate::js::perspective::*;
use crate::utils::*;

use self::view::View;
pub use self::view_subscription::TableStats;
use self::view_subscription::*;

use copy::*;
use download::*;
use serde::Deserialize;
use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt::Display;
use std::iter::FromIterator;
use std::iter::IntoIterator;
use std::ops::Deref;
use std::rc::Rc;
use wasm_bindgen::prelude::*;

use yew::prelude::*;

#[derive(Deserialize, Clone, Copy)]
pub enum Type {
    #[serde(rename = "string")]
    String,

    #[serde(rename = "integer")]
    Integer,

    #[serde(rename = "float")]
    Float,

    #[serde(rename = "boolean")]
    Bool,

    #[serde(rename = "date")]
    Date,

    #[serde(rename = "datetime")]
    Datetime,
}

impl Display for Type {
    fn fmt(
        &self,
        fmt: &mut std::fmt::Formatter<'_>,
    ) -> std::result::Result<(), std::fmt::Error> {
        write!(
            fmt,
            "{}",
            match self {
                Type::String => "string",
                Type::Integer => "integer",
                Type::Float => "float",
                Type::Bool => "bool",
                Type::Date => "date",
                Type::Datetime => "datetime",
            }
        )
    }
}

#[derive(Default)]
struct SessionMetadataCache {
    column_names: Vec<String>,
    schema: HashMap<String, Type>,
}

/// The `Session` struct is the principal interface to the Perspective engine,
/// the `Table` and `View` obejcts for this vieux, and all associated state.
#[derive(Default)]
pub struct SessionData {
    table: Option<JsPerspectiveTable>,
    metadata: Option<SessionMetadataCache>,
    config: ViewConfig,
    view_sub: Option<ViewSubscription>,
    stats: Option<TableStats>,
    on_stats: Option<Callback<()>>,
    on_update: PubSub<()>,
    on_table_loaded: PubSub<()>,
    on_view_created: PubSub<()>,
}

#[derive(Clone, Default)]
pub struct Session(Rc<RefCell<SessionData>>);

impl Deref for Session {
    type Target = Rc<RefCell<SessionData>>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Session {
    /// Set a callback for `TableStats` updates when this perspective table updates.
    pub fn set_on_stats_callback(&self, on_stats: Callback<()>) {
        if self.borrow().stats.as_ref().is_some() {
            on_stats.emit(());
        }

        self.borrow_mut().on_stats = Some(on_stats);
    }

    /// Set a callback for `TableStats` updates when this perspective table updates.
    pub fn add_on_update_callback(
        &self,
        on_update: impl Fn(()) + 'static,
    ) -> Subscription {
        self.borrow().on_update.add_listener(on_update)
    }

    /// Set a callback for `TableStats` updates when this perspective table updates.
    pub fn add_on_table_loaded(&self, on_table_loaded: Callback<()>) -> Subscription {
        self.borrow().on_table_loaded.add_listener(on_table_loaded)
    }

    /// Set a callback for `TableStats` updates when this perspective table updates.
    pub fn add_on_view_created(&self, on_view_created: Callback<()>) -> Subscription {
        self.borrow().on_view_created.add_listener(on_view_created)
    }

    pub fn reset(&self) {
        self.borrow_mut().view_sub = None;
        self.borrow_mut().config.reset();
    }

    /// Reset this (presumably shared) `Session` to its initial state, returning a
    /// bool indicating whether this `Session` had a table which was deleted.
    /// TODO decide whether to delete the table.
    pub fn delete(&self) -> bool {
        self.reset();
        self.borrow_mut().metadata = None;
        self.borrow_mut().table = None;
        false
    }

    /// Reset this `Session`'s state with a new `Table`.  Implicitly clears the
    /// `ViewSubscription`, which will need to be re-initialized later via `set_view()`.
    pub async fn set_table(
        &self,
        table: JsPerspectiveTable,
    ) -> Result<JsValue, JsValue> {
        let column_names = {
            let columns = table.columns().await?;
            if columns.length() > 0 {
                (0..columns.length())
                    .map(|i| columns.get(i).as_string().unwrap())
                    .collect::<Vec<String>>()
            } else {
                vec![]
            }
        };

        let schema = table
            .schema()
            .await?
            .into_serde::<HashMap<String, Type>>()
            .to_jserror()?;

        self.borrow_mut().view_sub = None;
        self.borrow_mut().table = Some(table);
        self.borrow_mut().metadata = Some(SessionMetadataCache {
            column_names,
            schema,
        });

        self.0.borrow().on_table_loaded.emit_all(());
        self.set_initial_stats().await?;
        Ok(JsValue::UNDEFINED)
    }

    pub fn get_table(&self) -> Option<JsPerspectiveTable> {
        self.borrow().table.clone()
    }

    /// The `table`'s unique column names.  This value is not
    pub fn get_all_columns(&self) -> Option<Vec<String>> {
        self.borrow()
            .metadata
            .as_ref()
            .map(|meta| meta.column_names.clone())
    }

    pub fn get_column_type(&self, name: &str) -> Option<Type> {
        self.borrow()
            .metadata
            .as_ref()
            .and_then(|meta| meta.schema.get(name))
            .cloned()
    }

    /// Validate an expression string (as a JsValue since it comes from `monaco`),
    /// and marshall the results.
    pub async fn validate_expr(
        self,
        expr: JsValue,
    ) -> Result<Option<PerspectiveValidationError>, JsValue> {
        let arr = js_sys::Array::from_iter([expr].iter());
        let table = self.borrow().table.as_ref().unwrap().clone();
        let errors = table.validate_expressions(arr).await?.errors();
        let error_keys = js_sys::Object::keys(&errors);
        if error_keys.length() > 0 {
            let js_err = js_sys::Reflect::get(&errors, &error_keys.get(0))?;
            Ok(Some(js_err.into_serde().unwrap()))
        } else {
            Ok(None)
        }
    }

    pub async fn copy_to_clipboard(self, flat: bool) -> Result<(), JsValue> {
        if flat {
            let table = self.borrow().table.clone();
            if let Some(table) = table {
                copy_flat(&table).await?;
            }
        } else {
            let view = self
                .borrow()
                .view_sub
                .as_ref()
                .map(|x| x.get_view().clone());

            if let Some(view) = view {
                copy(&view).await?;
            }
        };

        Ok(())
    }

    pub async fn download_as_csv(self, flat: bool) -> Result<(), JsValue> {
        if flat {
            let table = self.borrow().table.clone();
            if let Some(table) = table {
                download_flat(&table).await?;
            }
        } else {
            let view = self
                .borrow()
                .view_sub
                .as_ref()
                .map(|x| x.get_view().clone());

            if let Some(view) = view {
                download(&view).await?;
            }
        };

        Ok(())
    }

    pub fn get_view(&self) -> Option<View> {
        self.borrow()
            .view_sub
            .as_ref()
            .map(|sub| sub.get_view().clone())
    }

    pub fn get_table_stats(&self) -> Option<TableStats> {
        self.borrow().stats.clone()
    }

    pub fn get_view_config(&self) -> ViewConfig {
        self.borrow().config.clone()
    }

    /// Set a new `View` (derived from this `Session`'s `Table`), and create the
    /// `update()` subscription.
    pub async fn create_view(&self, config: ViewConfigUpdate) -> Result<View, JsValue> {
        self.borrow_mut().view_sub = None;
        self.validate_and_apply_view_config(config).await?;
        let js_config = self.borrow().config.as_jsvalue()?;
        let table = self.borrow().table.clone().unwrap();
        let view = table.view(&js_config).await?;
        let on_stats = Callback::from({
            let this = self.clone();
            move |stats| this.update_stats(stats)
        });

        let on_update = Callback::from({
            let this = self.clone();
            move |_| this.on_update()
        });

        let sub = {
            let config = self.borrow().config.clone();
            ViewSubscription::new(table, view, config, on_stats, on_update)
        };

        self.borrow_mut().view_sub = Some(sub);
        self.0.borrow().on_view_created.emit_all(());

        Ok(self
            .0
            .borrow()
            .view_sub
            .as_ref()
            .unwrap()
            .get_view()
            .clone())
    }

    pub fn reset_stats(&self) {
        self.update_stats(TableStats::default());
    }

    fn on_update(&self) {
        self.0.borrow().on_update.emit_all(());
    }

    /// Update the this `Session`'s `TableStats` data from the `Table`.
    async fn set_initial_stats(&self) -> Result<JsValue, JsValue> {
        let table = self.borrow().table.clone();
        let num_rows = table.unwrap().size().await? as u32;
        let stats = TableStats {
            is_pivot: false,
            num_rows: Some(num_rows),
            virtual_rows: None,
        };

        self.update_stats(stats);
        Ok(JsValue::UNDEFINED)
    }

    #[cfg(test)]
    pub fn set_stats(&self, stats: TableStats) {
        self.update_stats(stats)
    }

    fn update_stats(&self, stats: TableStats) {
        self.borrow_mut().stats = Some(stats);
        if let Some(cb) = self.borrow().on_stats.as_ref() {
            cb.emit(());
        }
    }

    async fn validate_and_apply_view_config(
        &self,
        update: ViewConfigUpdate,
    ) -> Result<(), JsValue> {
        let mut config = self.borrow().config.clone();
        config.apply_update(update);
        let all_column_names = self
            .borrow()
            .metadata
            .as_ref()
            .unwrap()
            .column_names
            .clone();

        let all_columns: HashSet<String> = all_column_names
            .iter()
            // .chain(config.expressions.iter())
            .cloned()
            .collect();

        let mut view_columns: HashSet<&str> = HashSet::new();

        // fix
        if config.columns.is_empty() {
            config.columns = all_column_names.into_iter().map(Some).collect();
        }

        for column in config.columns.iter() {
            if let Some(column) = column {
                if all_columns.contains(column) {
                    // TODO get real default
                    if !config.aggregates.contains_key(column) {
                        config.aggregates.insert(
                            column.to_owned(),
                            Aggregate::SingleAggregate(SingleAggregate::Count),
                        );
                    }
                    view_columns.insert(column);
                } else {
                    web_sys::console::warn_1(&JsValue::from(format!(
                        "Unknown \"{}\" in `columns`",
                        column
                    )))
                    // return Err(JsValue::from(format!(
                    //     "Unknown \"{}\" in `columns`",
                    //     column
                    // )));
                }
            }
        }

        config
            .aggregates
            .retain(|column, _| view_columns.contains(column.as_str()));

        self.borrow_mut().config = config;
        Ok(())
    }
}
