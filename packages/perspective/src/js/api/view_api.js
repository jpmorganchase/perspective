/******************************************************************************
 *
 * Copyright (c) 2019, the Perspective Authors.
 *
 * This file is part of the Perspective library, distributed under the terms of
 * the Apache License 2.0.  The full license can be found in the LICENSE file.
 *
 */

import {subscribe, unsubscribe, async_queue} from "./dispatch.js";
import {bindall} from "../utils.js";

/**
 * Construct a proxy for the view object by creating a "view" message and
 * sending it through the worker.
 *
 * @param {*} worker
 * @param {*} table_name
 * @param {*} config
 */
export function view(worker, table_name, config, resolve, reject) {
    this._worker = worker;
    //this._config = config;
    this._name = Math.random() + "";
    var msg = {
        cmd: "view",
        view_name: this._name,
        table_name: table_name,
        config: config
    };
    this._worker.post(msg, resolve, reject);
    bindall(this);
}

/**
 * Create a reference to a view located on `worker` for use by remote clients.
 *
 * @param {worker} worker the Web Worker at which the view is located.
 * @param {String} name a unique name for the view.
 */
export function proxy_view(worker, name) {
    this._worker = worker;
    this._name = name;
}

proxy_view.prototype = view.prototype;

// Send view methods that do not create new objects (getters, setters etc.) to
// the queue for processing.

view.prototype.get_config = async_queue("get_config");

view.prototype.to_json = async_queue("to_json");

view.prototype.to_arrow = async_queue("to_arrow");

view.prototype.to_columns = async_queue("to_columns");

view.prototype.to_csv = async_queue("to_csv");

view.prototype.schema = async_queue("schema");

view.prototype.computed_schema = async_queue("computed_schema");

view.prototype.column_paths = async_queue("column_paths");

view.prototype.num_columns = async_queue("num_columns");

view.prototype.num_rows = async_queue("num_rows");

view.prototype.set_depth = async_queue("set_depth");

view.prototype.get_row_expanded = async_queue("get_row_expanded");

view.prototype.expand = async_queue("expand");

view.prototype.collapse = async_queue("collapse");

view.prototype.delete = async_queue("delete");

view.prototype.col_to_js_typed_array = async_queue("col_to_js_typed_array");

view.prototype.on_update = subscribe("on_update", "view_method", true);

view.prototype.remove_update = unsubscribe("remove_update", "view_method", true);

view.prototype.on_delete = subscribe("on_delete", "view_method", true);

view.prototype.remove_delete = unsubscribe("remove_delete", "view_method", true);
