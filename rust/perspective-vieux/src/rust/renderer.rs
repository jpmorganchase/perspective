////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2018, the Perspective Authors.
//
// This file is part of the Perspective library, distributed under the terms
// of the Apache License 2.0.  The full license can be found in the LICENSE
// file.

mod activate;
mod limits;
pub mod registry;
mod render_timer;

use crate::js::perspective::JsPerspectiveView;
use crate::js::perspective_viewer::*;
use crate::session::Session;
use crate::utils::*;

use self::activate::*;
use self::limits::*;
use self::render_timer::*;

use registry::*;
use std::cell::RefCell;
use std::future::Future;
use std::rc::Rc;
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::*;
use yew::prelude::*;

type RenderLimits = (usize, usize, Option<usize>, Option<usize>);

pub struct PluginData {
    vieux_elem: HtmlElement,
    plugins: Vec<JsPerspectiveViewerPlugin>,
    plugins_idx: Option<usize>,
    _session: Session,
    draw_lock: DebounceMutex,
    timer: MovingWindowRenderTimer,
    on_plugin_changed: PubSub<JsPerspectiveViewerPlugin>,
    on_limits_changed: PubSub<RenderLimits>,
}

#[wasm_bindgen]
pub fn register_plugin(name: &str) {
    PLUGIN_REGISTRY.register_plugin(name);
}

#[derive(Clone)]
pub struct Renderer(Rc<RefCell<PluginData>>);

impl Renderer {
    pub fn new(vieux_elem: HtmlElement, _session: Session) -> Renderer {
        Renderer(Rc::new(RefCell::new(PluginData {
            vieux_elem,
            plugins: PLUGIN_REGISTRY.create_plugins(),
            plugins_idx: None,
            _session,
            draw_lock: Default::default(),
            timer: MovingWindowRenderTimer::default(),
            on_plugin_changed: Default::default(),
            on_limits_changed: Default::default(),
        })))
    }

    pub fn reset(&self) {
        self.0.borrow_mut().plugins_idx = None;
    }

    pub fn delete(&self) -> Result<(), JsValue> {
        self.get_active_plugin()?.delete();
        Ok(())
    }

    /// Add a handler to notify when the active `JsPerspectiveViewerPlugin` has
    /// changed.
    ///
    /// # Arguments
    /// - `on_plugin_changed` A notification callback.
    pub fn add_on_plugin_changed<T>(&self, on_plugin_changed: T) -> Subscription
    where
        T: Fn(JsPerspectiveViewerPlugin) + 'static,
    {
        if self.0.borrow().plugins_idx.is_some() {
            on_plugin_changed(self.get_active_plugin().unwrap());
        }

        self.0
            .borrow()
            .on_plugin_changed
            .add_listener(on_plugin_changed)
    }

    /// Add a handler to notify when the active `RenderLimits` have changed.
    ///
    /// # Arguments
    /// - `on_limits_changed` A notification callback.
    pub fn add_on_limits_changed(
        &self,
        on_limits_changed: Callback<RenderLimits>,
    ) -> Subscription {
        self.0
            .borrow()
            .on_limits_changed
            .add_listener(on_limits_changed)
    }

    /// Return all plugin instances, whether they are active or not.  Useful
    /// for configuring all or specific plugins at application init.
    pub fn get_all_plugins(&self) -> Vec<JsPerspectiveViewerPlugin> {
        self.0.borrow().plugins.clone()
    }

    /// Gets the currently active plugin.  Calling this method before a plugin has
    /// been selected will cause the default (first) plugin to be selected, and doing
    /// so when no plugins have been registered is an error.
    pub fn get_active_plugin(&self) -> Result<JsPerspectiveViewerPlugin, JsValue> {
        if self.0.borrow().plugins_idx.is_none() {
            self.set_plugin(Some(&PLUGIN_REGISTRY.default_plugin_name()))?;
        }

        let idx = self.0.borrow().plugins_idx.unwrap_or(0);
        let result = self.0.borrow().plugins.get(idx).cloned();
        result.ok_or_else(|| JsValue::from("No Plugin"))
    }

    /// Gets a specific `JsPerspectiveViewerPlugin` by name.
    ///
    /// # Arguments
    /// - `name` The plugin name to lookup.
    pub fn get_plugin(&self, name: &str) -> Result<JsPerspectiveViewerPlugin, JsValue> {
        let idx = self.find_plugin_idx(name);
        let idx = idx.ok_or_else(|| JsValue::from(format!("No Plugin `{}`", name)))?;
        let result = self.0.borrow().plugins.get(idx).cloned();
        Ok(result.unwrap())
    }

    pub fn set_throttle(&mut self, val: Option<f64>) {
        self.0.borrow_mut().timer.set_throttle(val);
    }

    pub fn disable_active_plugin_render_warning(&self) {
        self.get_active_plugin().unwrap().set_render_warning(false);
    }

    /// Set the active plugin to the plugin registerd as `name`, or the default
    /// plugin if `None` is provided.
    ///
    /// # Arguments
    /// - `name` The optional plugin name to set as active.
    pub fn set_plugin(&self, name: Option<&str>) -> Result<bool, JsValue> {
        let default_plugin_name = PLUGIN_REGISTRY.default_plugin_name();
        let name = name.unwrap_or_else(|| default_plugin_name.as_str());
        let idx = self
            .find_plugin_idx(name)
            .ok_or_else(|| JsValue::from(format!("Unkown plugin '{}'", name)))?;

        let changed = !matches!(
            self.0.borrow().plugins_idx,
            Some(selected_idx) if selected_idx == idx
        );

        if changed {
            self.0.borrow_mut().plugins_idx = Some(idx);
            self._trigger()?;
        }

        Ok(changed)
    }

    pub async fn resize(&self) -> Result<JsValue, JsValue> {
        let draw_mutex = self.draw_lock();
        draw_mutex
            .debounce(async {
                let jsplugin = self.get_active_plugin()?;
                jsplugin.resize().await?;
                Ok(JsValue::from(true))
            })
            .await
    }

    pub async fn draw(
        &self,
        session: impl Future<Output = &Session>,
    ) -> Result<JsValue, JsValue> {
        self.draw_plugin(session, false).await
    }

    pub async fn update(&self, session: &Session) -> Result<JsValue, JsValue> {
        self.draw_plugin(async { session }, true).await
    }

    async fn draw_plugin(
        &self,
        session: impl Future<Output = &Session>,
        is_update: bool,
    ) -> Result<JsValue, JsValue> {
        let timer = self.render_timer();
        let task = async move {
            if is_update {
                set_timeout(timer.get_avg()).await?;
            }

            if let Some(view) = session.await.get_view() {
                timer.capture_time(self.draw_view(&view, is_update)).await
            } else {
                Ok(JsValue::from(true))
            }
        };

        let draw_mutex = self.draw_lock();
        if is_update {
            draw_mutex.debounce(task).await
        } else {
            draw_mutex.lock(task).await
        }
    }

    async fn draw_view(
        &self,
        view: &JsPerspectiveView,
        is_update: bool,
    ) -> Result<JsValue, JsValue> {
        let plugin = self.get_active_plugin()?;
        let limits = get_row_and_col_limits(&view, &plugin).await?;
        self.0.borrow().on_limits_changed.emit_all(limits);
        let vieux_elem = &self.0.borrow().vieux_elem.clone();
        if is_update {
            let task = plugin.update(&view, limits.2, limits.3, false);
            activate_plugin(&vieux_elem, &plugin, task).await
        } else {
            let task = plugin.draw(&view, limits.2, limits.3, false);
            activate_plugin(&vieux_elem, &plugin, task).await
        }
    }

    pub async fn presize(
        &self,
        open: bool,
        on_toggle: impl Future<Output = Result<(), JsValue>>,
    ) -> Result<JsValue, JsValue> {
        let task = async {
            let plugin = self.get_active_plugin()?;
            if open {
                on_toggle.await?;
                plugin.resize().await
            } else {
                let main_panel: web_sys::HtmlElement = self
                    .0
                    .borrow()
                    .vieux_elem
                    .children()
                    .item(0)
                    .unwrap()
                    .unchecked_into();
                let new_width =
                    format!("{}px", &self.0.borrow().vieux_elem.client_width());
                let new_height =
                    format!("{}px", &self.0.borrow().vieux_elem.client_height());
                main_panel.style().set_property("width", &new_width)?;
                main_panel.style().set_property("height", &new_height)?;
                let resize = plugin.resize().await;
                main_panel.style().set_property("width", "")?;
                main_panel.style().set_property("height", "")?;
                on_toggle.await?;
                resize
            }
        };
        self.draw_lock().lock(task).await
    }

    fn draw_lock(&self) -> DebounceMutex {
        self.0.borrow().draw_lock.clone()
    }

    fn render_timer(&self) -> MovingWindowRenderTimer {
        self.0.borrow().timer.clone()
    }

    fn _trigger(&self) -> Result<(), JsValue> {
        let plugin: JsPerspectiveViewerPlugin = self.get_active_plugin()?;
        self.0.borrow().on_plugin_changed.emit_all(plugin);
        Ok(())
    }

    fn find_plugin_idx(&self, name: &str) -> Option<usize> {
        let short_name = make_short_name(&name);
        let elements = &self.0.borrow().plugins;
        elements
            .iter()
            .position(|elem| make_short_name(&elem.name()).contains(&short_name))
    }
}

fn make_short_name(name: &str) -> String {
    name.to_lowercase()
        .chars()
        .filter(|x| x.is_alphabetic())
        .collect()
}
