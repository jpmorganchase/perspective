////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2018, the Perspective Authors.
//
// This file is part of the Perspective library, distributed under the terms
// of the Apache License 2.0.  The full license can be found in the LICENSE
// file.

use crate::plugin::registry::*;

use std::cell::RefCell;
use wasm_bindgen::prelude::*;

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
                const timer = perspective_viewer._render_time();
                this.innerHTML = `<pre style='${css}'>${csv}</pre>`;
                timer();
            }

            async clear() {
                this.innerHtml = '';
            }

            async resize() {}

            save() {}

            restore() {}
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
    fn _register_test_components() -> Result<(), JsValue>;

}

thread_local! {
    static IS_REGISTERED: RefCell<bool> = RefCell::new(false);
}

pub fn register_test_components() -> Result<(), JsValue> {
    IS_REGISTERED.with(|registered| {
        if *registered.borrow() {
            Ok(())
        } else {
            *registered.borrow_mut() = true;
            PLUGIN_REGISTRY.reset();
            let result = _register_test_components();
            PLUGIN_REGISTRY.register_plugin("perspective-viewer-debug2");
            PLUGIN_REGISTRY.register_plugin("perspective-viewer-debug3");
            PLUGIN_REGISTRY.register_plugin("perspective-viewer-debug4");
            result
        }
    })
}
