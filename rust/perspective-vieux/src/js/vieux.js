/******************************************************************************
 *
 * Copyright (c) 2018, the Perspective Authors.
 *
 * This file is part of the Perspective library, distributed under the terms
 * of the Apache License 2.0.  The full license can be found in the LICENSE
 * file.
 *
 */

import init, * as internal from "../pkg/perspective_viewer.js";
import wasm_internal from "../pkg/perspective_viewer_bg.wasm";

export const wasm = init(wasm_internal).then(() => {
    internal.set_panic_hook();
    return internal;
});

let _index = undefined;
async function _await_index(f) {
    await new Promise(setTimeout);
    if (!_index) {
        _index = await wasm;
    }
    return f();
}
class PerspectiveVieuxElement extends HTMLElement {
    constructor() {
        super();
        _await_index(() => {
            this._instance = new _index.PerspectiveVieuxElement(this);
        });
    }

    connectedCallback() {
        _await_index(() => {
            this._instance.connected_callback();
        });
    }

    load(table) {
        _await_index(() => this._instance.js_load(table));
    }

    notifyResize() {
        return _await_index(() => this._instance.js_resize());
    }

    getTable() {
        return _await_index(() => this._instance.js_get_table());
    }

    restore(...args) {
        return _await_index(() => this._instance.js_restore(...args));
    }

    save(...args) {
        return _await_index(() => this._instance.js_save(...args));
    }

    delete() {
        return _await_index(() => this._instance.js_delete());
    }

    download(...args) {
        return _await_index(() => this._instance.js_download(...args));
    }

    copy(...args) {
        return _await_index(() => this._instance.js_copy(...args));
    }

    getEditPort() {
        return _await_index(() => console.error("Not Implemented"));
    }

    setThrottle(...args) {
        return _await_index(() => this._instance.js_set_throttle(...args));
    }

    toggleConfig(force) {
        return _await_index(() => this._instance.js_toggle_config(force));
    }

    get_plugin(name) {
        return _await_index(() => this._instance.js_get_plugin(name));
    }

    get_all_plugins() {
        return _await_index(() => this._instance.js_get_all_plugins());
    }

    js_set_plugin(name) {
        return _await_index(() => this._instance.js_set_plugin(name));
    }

    _open_expression_editor(target) {
        return _await_index(() => this._instance._js_open_expression_editor(target));
    }
}

if (document.createElement("perspective-viewer").constructor === HTMLElement) {
    window.customElements.define("perspective-viewer", PerspectiveVieuxElement);
}

class PerspectiveColumnStyleElement extends HTMLElement {
    constructor() {
        super();
    }

    open(target, config, default_config) {
        _await_index(() => {
            if (this._instance) {
                this._instance.reset(config, default_config);
            } else {
                this._instance = new _index.PerspectiveColumnStyleElement(this, config, default_config);
            }

            this._instance.open(target);
        });
    }
}

if (document.createElement("perspective-column-style").constructor === HTMLElement) {
    window.customElements.define("perspective-column-style", PerspectiveColumnStyleElement);
}
