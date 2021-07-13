////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2018, the Perspective Authors.
//
// This file is part of the Perspective library, distributed under the terms
// of the Apache License 2.0.  The full license can be found in the LICENSE
// file.

mod aggregates;
mod filters;
mod sort;
mod view_config;

use crate::renderer::Renderer;

pub use aggregates::*;
pub use filters::*;
pub use sort::*;
pub use view_config::*;

use serde::Deserialize;
use serde::Deserializer;
use serde::Serialize;
use serde_json::Value;

#[derive(Serialize)]
#[serde()]
pub struct ViewerConfig {
    pub plugin: String,
    pub plugin_config: Value,

    #[serde(flatten)]
    pub view_config: ViewConfig,
}

impl ViewerConfig {
    pub fn new(renderer: &Renderer) -> ViewerConfig {
        ViewerConfig {
            plugin: renderer.get_active_plugin().unwrap().name(),
            view_config: ViewConfig::default(),
            plugin_config: Value::Null,
        }
    }
}

#[derive(Deserialize)]
#[serde()]
pub struct ViewerConfigUpdate {
    #[serde(default)]
    pub plugin: PluginUpdate,

    #[serde(default)]
    pub plugin_config: Option<Value>,

    #[serde(flatten)]
    pub view_config: ViewConfigUpdate,
}

/// The `PluginUpdate` enum must represent 3 possible states in a `ViewerConfigUpdate`:
/// - `{plugin: null}` key is present but the value is `null` or `undefined`, which
///   indicates we should update the `plugin` field to the default.
/// - `{}`, the `plugin` key is missing and this field should be ignored.
/// - `{plugin: val}` key is present and has a value, indicates we should update
///   the `plugin` field to `val`.
pub enum PluginUpdate {
    SetDefault,
    Missing,
    Update(String),
}

/// Handles `{}` when included as a field with `#[serde(default)]`.
impl Default for PluginUpdate {
    fn default() -> Self {
        Self::Missing
    }
}

/// Handles `{plugin: null}` and `{plugin: val}` by treating this type as an
/// option.
impl From<Option<String>> for PluginUpdate {
    fn from(opt: Option<String>) -> PluginUpdate {
        match opt {
            Some(v) => PluginUpdate::Update(v),
            None => PluginUpdate::SetDefault,
        }
    }
}

/// Treats `PluginUpdate` enum as an `Option<T>` when present during deserialization.
impl<'a> Deserialize<'a> for PluginUpdate {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'a>,
    {
        Option::deserialize(deserializer).map(Into::into)
    }
}
