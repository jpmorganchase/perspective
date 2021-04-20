/******************************************************************************
 *
 * Copyright (c) 2019, the Perspective Authors.
 *
 * This file is part of the Perspective library, distributed under the terms of
 * the Apache License 2.0.  The full license can be found in the LICENSE file.
 *
 */
import {PerspectiveModel} from "../../../src/js/model";
import {PerspectiveView} from "../../../src/js/view";
import {PerspectiveJupyterWidget} from "../../../src/js/widget";
import {ManagerBase} from "@jupyter-widgets/base-manager";
import {uuid} from "@jupyter-widgets/base";

jest.mock("@finos/perspective-vieux/pkg/perspective_vieux_bg.wasm", () => {
    return {set_panic_hook: () => {}};
});

jest.mock("../../../src/js/widget");

let numComms = 0;

// Duplicated from `@jupyter-widgets`
export class MockComm {
    comm_id = undefined;
    target_name = undefined;
    _on_msg = undefined;
    _on_open = undefined;
    _on_close = undefined;

    constructor() {
        this.comm_id = `mock-comm-id-${numComms}`;
        numComms += 1;
    }

    on_open(fn) {
        this._on_open = fn;
    }

    on_close(fn) {
        this._on_close = fn;
    }

    on_msg(fn) {
        this._on_msg = fn;
    }

    _process_msg(msg) {
        if (this._on_msg) {
            return this._on_msg(msg);
        } else {
            return Promise.resolve();
        }
    }

    open() {
        if (this._on_open) {
            this._on_open();
        }
        return "";
    }

    close() {
        if (this._on_close) {
            this._on_close();
        }
        return "";
    }

    send() {
        return "";
    }
}

export class MockManager extends ManagerBase {
    constructor() {
        super();
        this.el = window.document.createElement("div");
    }

    create_view(model) {
        const id = uuid();
        const view_promise = async () => {
            const view = new PerspectiveView({
                model: model
            });
            view.listenTo(model, "destroy", view.remove);
            view.once("remove", () => {
                delete model.views[id];
            });

            return view;
        };
        model.views[id] = view_promise;
        return view_promise;
    }

    loadClass(className, moduleName, moduleVersion) {
        return new Promise(function(resolve) {
            if (moduleName === "@finos/perspective-jupyterlab") {
                resolve(PerspectiveJupyterWidget);
            }
        }).then(function(module) {
            if (module[className]) {
                return module[className];
            } else {
                return Promise.reject(`Class ${className} not found in module ${moduleName}@${moduleVersion}`);
            }
        });
    }

    get_model(model_id) {
        return this._models[model_id];
    }

    register_model(model_id, modelPromise) {
        this._models[model_id] = modelPromise;
        modelPromise.then(model => {
            model.once("comm:close", () => {
                delete this._models[model_id];
            });
        });
    }

    /**
     * Create and return a promise for a new widget model
     *
     * @param options - the options for creating the model.
     * @param serialized_state - attribute values for the model.
     *
     * @example
     * widget_manager.new_model({
     *      model_name: 'IntSlider',
     *      model_module: '@jupyter-widgets/controls',
     *      model_module_version: '1.0.0',
     *      model_id: 'u-u-i-d'
     * }).then((model) => { console.log('Create success!', model); },
     *  (err) => {console.error(err)});
     *
     */
    async new_model(options, serialized_state) {
        let model_id;
        if (options.model_id) {
            model_id = options.model_id;
        } else if (options.comm) {
            model_id = options.model_id = options.comm.comm_id;
        } else {
            throw new Error("Neither comm nor model_id provided in options object. At least one must exist.");
        }

        const modelPromise = this._make_model(options, serialized_state);
        // this call needs to happen before the first `await`, see note in `set_state`:
        this.register_model(model_id, modelPromise);
        return await modelPromise;
    }

    async _make_model(options, serialized_state) {
        const model_id = options.model_id;
        const model_type = await Promise.resolve(PerspectiveModel);
        const attributes = await model_type._deserialize_state(serialized_state, this);
        const model_options = {
            widget_manager: this,
            model_id: model_id,
            comm: options.comm
        };
        const widget_model = new model_type(attributes, model_options);
        widget_model.name = options.model_name;
        widget_model.module = options.model_module;
        return widget_model;
    }

    _get_comm_info() {
        return Promise.resolve({});
    }

    _create_comm() {
        return Promise.resolve(new MockComm());
    }
}
