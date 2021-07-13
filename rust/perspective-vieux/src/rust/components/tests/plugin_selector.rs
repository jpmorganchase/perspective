////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2018, the Perspective Authors.
//
// This file is part of the Perspective library, distributed under the terms
// of the Apache License 2.0.  The full license can be found in the LICENSE
// file.

use crate::components::plugin_selector::*;
use crate::js::perspective_viewer::*;
use crate::renderer::registry::*;
use crate::renderer::*;
use crate::session::*;
use crate::utils::WeakComponentLink;
use crate::*;

use std::cell::RefCell;
use std::rc::Rc;
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use wasm_bindgen_test::*;
use web_sys::*;
use yew::prelude::*;

wasm_bindgen_test::wasm_bindgen_test_configure!(run_in_browser);

////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2018, the Perspective Authors.
//
// This file is part of the Perspective library, distributed under the terms
// of the Apache License 2.0.  The full license can be found in the LICENSE
// file.

/// Perspective FFI
#[wasm_bindgen(inline_js = "

    function generatePlugin(name) {
        return class DebugPlugin extends HTMLElement {
            constructor() {
                super();
            }

            get name() {
                return 'Debug ' + name;
            }

            get selectMode() {
                return 'toggle';
            }

            async draw(view) {
                this.style.backgroundColor = '#fff';
                let perspective_viewer = this.parentElement;
                const csv = await view.to_csv({config: {delimiter: '|'}});
                const css = `margin:0;overflow:scroll;position:absolute;width:100%;height:100%`;
                this.innerHTML = `<pre style='${css}'>${csv}</pre>`;
            }

            async clear() {
                this.innerHtml = '';
            }

            async resize() {}

            save() {}

            restore() {}

            delete() {}
        };
    }

    export function register_test_components() {
        customElements.define('perspective-viewer-debug2', generatePlugin('A'));
        customElements.define('perspective-viewer-debug3', generatePlugin('B'));
        customElements.define('perspective-viewer-debug4', generatePlugin('C'));
    }        

")]
#[rustfmt::skip]
extern "C" {

    #[wasm_bindgen(js_name = "register_test_components", catch)]
    pub fn register_test_components() -> Result<(), JsValue>;

}

#[wasm_bindgen_test]
pub fn test_plugin_selected() {
    PLUGIN_REGISTRY.reset();
    register_test_components().unwrap();
    PLUGIN_REGISTRY.register_plugin("perspective-viewer-debug2");
    PLUGIN_REGISTRY.register_plugin("perspective-viewer-debug3");
    PLUGIN_REGISTRY.register_plugin("perspective-viewer-debug4");

    let link: WeakComponentLink<PluginSelector> = WeakComponentLink::default();
    let result: Rc<RefCell<Option<JsPerspectiveViewerPlugin>>> =
        Rc::new(RefCell::new(None));
    let document = window().unwrap().document().unwrap();
    let elem: HtmlElement = document.create_element("div").unwrap().unchecked_into();
    let session = Session::default();
    let renderer = Renderer::new(elem, session);
    let _sub = renderer.add_on_plugin_changed({
        clone!(result);
        move |val| {
            *result.borrow_mut() = Some(val);
        }
    });

    test_html! {
        <PluginSelector
            renderer=renderer.clone()
            weak_link=link.clone()>

        </PluginSelector>
    };

    let plugin_selector = link.borrow().clone().unwrap();
    plugin_selector.send_message(PluginSelectorMsg::ComponentSelectPlugin(
        "Debug B".to_owned(),
    ));

    assert_eq!(
        result.borrow().as_ref().map(|x| x.name()),
        Some("Debug B".to_owned())
    );

    assert_eq!(renderer.get_active_plugin().unwrap().name(), "Debug B");
}
