/******************************************************************************
 *
 * Copyright (c) 2019, the Perspective Authors.
 *
 * This file is part of the Perspective library, distributed under the terms of
 * the Apache License 2.0.  The full license can be found in the LICENSE file.
 *
 */

import {override_config} from "../config";

function error_to_json(error) {
    const obj = {};
    if (typeof error !== "string") {
        Object.getOwnPropertyNames(error).forEach(key => {
            obj[key] = error[key];
        }, error);
    } else {
        obj["message"] = error;
    }
    return obj;
}

/**
 * The base class for Perspective's async API. It initializes and keeps track of
 * tables, views, and processes messages from the user into Perspective.
 *
 * Child classes must implement the `post()` interface, which defines how the
 * worker sends messages.
 */
export class Server {
    constructor(perspective) {
        this.perspective = perspective;
        this._tables = {};
        this._views = {};

        this._callback_cache = new Map();
    }

    /**
     * `Server` must be extended and the `post` method implemented before it can
     * be initialized.
     */
    init(msg) {
        if (msg.config) {
            override_config(msg.config);
        }
        this.post(msg);
    }

    post() {
        throw new Error("post() not implemented!");
    }

    /**
     * Garbage collect un-needed views.
     */
    clear_views(client_id) {
        for (let key of Object.keys(this._views)) {
            if (this._views[key].client_id === client_id) {
                try {
                    this._views[key].delete();
                } catch (e) {
                    console.error(e);
                }
                delete this._views[key];
            }
        }
        console.debug(`GC ${Object.keys(this._views).length} views in memory`);
    }

    /**
     * Given a message, execute its instructions. This method is the dispatcher
     * for all Perspective actions, including table/view creation, deletion, and
     * all method calls to/from the table and view.
     *
     * @param {*} msg an Object containing `cmd` (a String instruction) and
     * associated data for that instruction
     * @param {*} client_id
     */
    process(msg, client_id) {
        switch (msg.cmd) {
            case "init_profile_thread":
                this.perspective.initialize_profile_thread();
                break;
            case "init":
                this.init(msg);
                break;
            case "table":
                if (typeof msg.args[0] === "undefined") {
                    this._tables[msg.name] = [];
                } else {
                    const msgs = this._tables[msg.name];
                    this._tables[msg.name] = this.perspective.table(msg.args[0], msg.options);
                    if (msgs) {
                        for (const msg of msgs) {
                            this.process(msg);
                        }
                    }
                }
                break;
            case "table_generate":
                let g;
                eval("g = " + msg.args);
                g(function(tbl) {
                    this._tables[msg.name] = tbl;
                    this.post({
                        id: msg.id,
                        data: "created!"
                    });
                });
                break;
            case "table_execute":
                let f;
                eval("f = " + msg.f);
                f(this._tables[msg.name]);
                break;
            case "table_method":
            case "view_method":
                this.process_method_call(msg);
                break;
            case "view":
                const tableMsgQueue = this._tables[msg.table_name];
                if (tableMsgQueue && Array.isArray(tableMsgQueue)) {
                    tableMsgQueue.push(msg);
                    this._views[msg.view_name] = [];
                } else {
                    // create a new view and track it with `client_id`
                    const msgs = this._views[msg.view_name];
                    this._views[msg.view_name] = this._tables[msg.table_name].view(msg.config);
                    this._views[msg.view_name].client_id = client_id;
                    if (msgs) {
                        for (const msg of msgs) {
                            this.process(msg);
                        }
                    }
                }
                this.post({
                    id: msg.id
                });
                break;
        }
    }

    /**
     * Send an error to the client.
     */
    process_error(msg, error) {
        this.post({
            id: msg.id,
            error: error_to_json(error)
        });
    }

    /**
     * Execute a subscription to a Perspective event.
     */
    process_subscribe(msg, obj) {
        try {
            let callback;
            if (msg.method.slice(0, 2) === "on") {
                callback = ev => {
                    let result = {
                        id: msg.id,
                        data: ev
                    };
                    try {
                        // post transferable data for arrow
                        if (msg.args && msg.args[0]) {
                            if (msg.method === "on_update" && msg.args[0]["mode"] === "row") {
                                // actual arrow is in the `delta`
                                this.post(result, [ev.delta]);
                                return;
                            }
                        }

                        this.post(result);
                    } catch (e) {
                        console.error(`Removing failed callback to \`${msg.method}()\` (presumably due to failed connection)`);
                        const remove_method = msg.method.substring(3);
                        obj[`remove_${remove_method}`](callback);
                    }
                };
                if (msg.callback_id) {
                    this._callback_cache.set(msg.callback_id, callback);
                }
            } else if (msg.callback_id) {
                callback = this._callback_cache.get(msg.callback_id);
                this._callback_cache.delete(msg.callback_id);
            }
            if (callback) {
                obj[msg.method](callback, ...msg.args);
            } else {
                console.error(`Callback not found for remote call "${JSON.stringify(msg)}"`);
            }
        } catch (error) {
            this.process_error(msg, error);
            return;
        }
    }

    process_method_call_response(msg, result) {
        if (msg.method === "delete") {
            delete this._views[msg.name];
        }
        if (msg.method === "to_arrow") {
            this.post(
                {
                    id: msg.id,
                    data: result
                },
                [result]
            );
        } else {
            this.post({
                id: msg.id,
                data: result
            });
        }
    }

    /**
     * Given a call to a table or view method, process it.
     *
     * @param {Object} msg
     */
    process_method_call(msg) {
        let obj, result;
        const name = msg.view_name || msg.name;
        msg.cmd === "table_method" ? (obj = this._tables[name]) : (obj = this._views[name]);

        if (!obj && msg.cmd === "view_method") {
            // cannot have a host without a table, but can have a host without a
            // view
            this.process_error(msg, {message: "View is not initialized"});
            return;
        }

        if (obj && obj.push) {
            obj.push(msg);
            return;
        }

        try {
            if (msg.subscribe) {
                this.process_subscribe(msg, obj);
                return;
            } else {
                result = obj[msg.method].apply(obj, msg.args);
                if (result instanceof Promise) {
                    result.then(result => this.process_method_call_response(msg, result)).catch(error => this.process_error(msg, error));
                } else {
                    this.process_method_call_response(msg, result);
                }
            }
        } catch (error) {
            this.process_error(msg, error);
            return;
        }
    }
}
