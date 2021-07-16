////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2018, the Perspective Authors.
//
// This file is part of the Perspective library, distributed under the terms
// of the Apache License 2.0.  The full license can be found in the LICENSE
// file.

use crate::components::vieux::*;
use crate::config::*;
use crate::custom_elements::expression_editor::PerspectiveExpressionEditorElement;
use crate::js::perspective::*;
use crate::js::perspective_viewer::JsPerspectiveViewerPlugin;
use crate::renderer::*;
use crate::session::Session;
use crate::utils::*;
use crate::*;

use futures::channel::oneshot::*;
use js_sys::*;
use std::cell::RefCell;
use std::iter::FromIterator;
use std::rc::Rc;
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use wasm_bindgen_futures::future_to_promise;
use wasm_bindgen_futures::JsFuture;
use web_sys::*;
use yew::prelude::*;

/// A `customElements` external API.
#[wasm_bindgen]
#[derive(Clone)]
pub struct PerspectiveVieuxElement {
    elem: HtmlElement,
    root: ComponentLink<PerspectiveVieux>,
    session: Session,
    renderer: Renderer,
    subscriptions: Rc<[Subscription; 3]>,
    expression_editor: Rc<RefCell<Option<PerspectiveExpressionEditorElement>>>,
    config: Rc<RefCell<ViewerConfig>>,
}

#[wasm_bindgen]
impl PerspectiveVieuxElement {
    #[wasm_bindgen(constructor)]
    pub fn new(elem: web_sys::HtmlElement) -> PerspectiveVieuxElement {
        let init = web_sys::ShadowRootInit::new(web_sys::ShadowRootMode::Open);
        let shadow_root = elem
            .attach_shadow(&init)
            .unwrap()
            .unchecked_into::<web_sys::Element>();

        // Application State
        let session = Session::default();
        let renderer = Renderer::new(elem.clone(), session.clone());
        let config = Rc::new(RefCell::new(ViewerConfig::new(&renderer)));

        // Create Yew App
        let props = PerspectiveVieuxProps {
            elem: elem.clone(),
            session: session.clone(),
            renderer: renderer.clone(),
            weak_link: WeakComponentLink::default(),
        };

        let app = App::<PerspectiveVieux>::new();
        let root = app.mount_with_props(shadow_root, props);

        // Create callbacks
        let update_sub = session.add_on_update_callback({
            clone!(renderer, session);
            move |_| {
                clone!(renderer, session);
                drop(future_to_promise(
                    async move { renderer.update(&session).await },
                ))
            }
        });

        let plugin_sub = renderer.add_on_plugin_changed({
            clone!(elem);
            move |plugin| dispatch_plugin_changed(&elem, &plugin)
        });

        let limit_sub = {
            let callback = root.callback(|x| Msg::RenderLimits(Some(x)));
            renderer.add_on_limits_changed(callback)
        };

        PerspectiveVieuxElement {
            elem,
            root,
            session,
            renderer,
            expression_editor: Rc::new(RefCell::new(None)),
            subscriptions: Rc::new([plugin_sub, update_sub, limit_sub]),
            config,
        }
    }

    pub fn connected_callback(&self) {}

    /// Loads a promise to a `JsPerspectiveTable` in this viewer.
    pub fn js_load(&self, table: js_sys::Promise) -> js_sys::Promise {
        assert!(!table.is_undefined());
        let (sender, receiver) = channel::<Result<JsValue, JsValue>>();
        let root = self.root.clone();
        future_to_promise(async move {
            let promise = JsFuture::from(table).await?;
            let table: JsPerspectiveTable = promise.unchecked_into();
            root.send_message(Msg::LoadTable(table, sender));
            receiver.await.to_jserror()?
        })
    }

    pub fn js_delete(&self) -> Result<bool, JsValue> {
        let deleted = self.session.reset();
        self.renderer.reset()?;
        Ok(deleted)
    }

    pub fn js_get_table(&self) -> Option<JsPerspectiveTable> {
        self.session.get_table()
    }

    /// Restores this element to a full/partial `JsPerspectiveViewConfig`.
    ///
    /// # Arguments
    /// - `update` The config to restore to, as returned by `.save()`.
    pub fn js_restore(&self, update: JsPerspectiveViewConfigUpdate) -> js_sys::Promise {
        let session = self.session.clone();
        let renderer = self.renderer.clone();
        future_to_promise(async move {
            let update = update.into_serde().to_jserror()?;
            drop(
                renderer
                    .draw(async {
                        session.create_view(update).await.unwrap();
                        &session
                    })
                    .await?,
            );
            Ok(session.get_view().as_ref().unwrap().as_jsvalue())
        })
    }

    pub fn js_download(&self, flat: bool) -> js_sys::Promise {
        let session = self.session.clone();
        future_to_promise(async move {
            session.download_as_csv(flat).await?;
            Ok(JsValue::UNDEFINED)
        })
    }

    pub fn js_copy(&self, flat: bool) -> js_sys::Promise {
        let session = self.session.clone();
        future_to_promise(async move {
            session.copy_to_clipboard(flat).await?;
            Ok(JsValue::UNDEFINED)
        })
    }

    pub fn js_resize(&self) -> js_sys::Promise {
        let renderer = self.renderer.clone();
        future_to_promise(async move { renderer.resize().await })
    }

    /// Determines the render throttling behavior. Can be an integer, for
    /// millisecond window to throttle render event; or, if `None`, adaptive throttling
    /// will be calculated from the measured render time of the last 5 frames.
    ///
    /// # Examples
    /// // Only draws at most 1 frame/sec.
    /// vieux.js_set_throttle(Some(1000_f64));
    ///
    /// # Arguments
    /// `throttle` - The throttle rate - milliseconds (f64), or `None` for adaptive
    /// throttling.
    pub fn js_set_throttle(&mut self, val: Option<f64>) {
        self.renderer.set_throttle(val);
    }

    /// Toggle (or force) the config panel open/closed.
    ///
    /// # Arguments
    /// - `force` Force the state of the panel open or closed, or `None` to toggle.
    pub fn js_toggle_config(&self, force: Option<bool>) -> js_sys::Promise {
        let this = self.clone();
        let (sender, receiver) = channel::<Result<JsValue, JsValue>>();
        let msg = Msg::ToggleConfig(force, Some(sender));
        this.root.send_message(msg);
        future_to_promise(async move {
            receiver.await.map_err(|_| JsValue::from("Cancelled"))?
        })
    }

    /// Get an `Array` of all of the plugin custom elements registered for this element.
    /// This may not include plugins which called `registerPlugin()` after the host has
    /// rendered for the first time.
    pub fn js_get_all_plugins(&self) -> Array {
        Array::from_iter(self.renderer.get_all_plugins().iter())
    }

    /// Gets a plugin Custom Element with the `name` field, or get the active plugin
    /// if no `name` is provided.
    ///
    /// # Arguments
    /// - `name` The `name` property of a perspective plugin Custom Element, or `None`
    ///   for the active plugin's Custom Element.
    pub fn js_get_plugin(
        &self,
        name: Option<String>,
    ) -> Result<JsPerspectiveViewerPlugin, JsValue> {
        match name {
            None => self.renderer.get_active_plugin(),
            Some(name) => self.renderer.get_plugin(&name),
        }
    }

    /// Sets the active plugin to the plugin with the `name` field, or get the default
    /// plugin if no `name` is provided.
    ///
    /// # Arguments
    /// - `name` The `name` property of a perspective plugin Custom Element, or `None`
    ///   for the default plugin.
    pub fn js_set_plugin(&mut self, name: Option<String>) -> Result<bool, JsValue> {
        self.renderer.set_plugin(name.as_deref())
    }

    /// Opens an expression editor at the `target` position, by first creating a
    /// `perspective-expression-editor` custom element, then creating the model.
    ///
    /// # Arguments
    /// - `target` The `HtmlElements` in the DOM to pin the floating editor modal to.
    pub fn _js_open_expression_editor(&mut self, target: HtmlElement) {
        let mut x = self.expression_editor.borrow_mut();
        match x.as_mut() {
            Some(x) => x.open(target).unwrap(),
            _ => *x = Some(self.create_expression_editor(target)),
        };
    }

    fn create_expression_editor(
        &self,
        target: HtmlElement,
    ) -> PerspectiveExpressionEditorElement {
        let on_save = Callback::from({
            let this = self.clone();
            move |val| this.clone().save_expr(val)
        });

        let monaco_theme = get_theme(&target);
        let mut element = PerspectiveExpressionEditorElement::new(
            self.session.clone(),
            on_save,
            monaco_theme,
        );

        element.open(target).unwrap();
        element
    }

    fn save_expr(&mut self, x: JsValue) {
        let mut event_init = web_sys::CustomEventInit::new();
        event_init.detail(&x);
        let event = web_sys::CustomEvent::new_with_event_init_dict(
            "-perspective-add-expression",
            &event_init,
        );

        self.expression_editor
            .borrow_mut()
            .as_mut()
            .unwrap()
            .close()
            .unwrap();

        self.elem.dispatch_event(&event.unwrap()).unwrap();
    }
}

fn get_theme(elem: &HtmlElement) -> String {
    let styles = window().unwrap().get_computed_style(elem).unwrap().unwrap();
    match &styles.get_property_value("--monaco-theme") {
        Err(_) => "vs",
        Ok(ref s) if s.trim() == "" => "vs",
        Ok(x) => x.trim(),
    }
    .to_owned()
}

fn dispatch_plugin_changed(elem: &HtmlElement, plugin: &JsPerspectiveViewerPlugin) {
    let mut event_init = web_sys::CustomEventInit::new();
    event_init.detail(plugin);
    let event = web_sys::CustomEvent::new_with_event_init_dict(
        "-perspective-plugin-changed",
        &event_init,
    );

    elem.dispatch_event(&event.unwrap()).unwrap();
}
