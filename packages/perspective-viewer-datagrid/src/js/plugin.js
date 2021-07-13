/******************************************************************************
 *
 * Copyright (c) 2017, the Perspective Authors.
 *
 * This file is part of the Perspective library, distributed under the terms of
 * the Apache License 2.0.  The full license can be found in the LICENSE file.
 *
 */

import "regular-table";

import {createModel, configureRegularTable, formatters, create_color_record} from "./regular_table_handlers.js";
import MATERIAL_STYLE from "../less/regular_table.less";
import {configureRowSelectable, deselect} from "./row_selection.js";
import {configureClick} from "./click.js";
import {configureEditable} from "./editing.js";
import {configureSortable} from "./sorting.js";
import {PLUGIN_SYMBOL} from "./plugin_menu.js";

customElements.define(
    "perspective-viewer-datagrid",
    class extends HTMLElement {
        constructor() {
            super();
            this.datagrid = document.createElement("regular-table");
            this.datagrid.formatters = formatters;
        }

        async activate(view) {
            let viewer = this.parentElement;
            let table = await viewer.getTable();
            if (!this._initialized) {
                this.innerHTML = "";
                this.appendChild(this.datagrid);
                this.model = await createModel(this.datagrid, table, view);
                configureRegularTable(this.datagrid, this.model);
                await configureRowSelectable.call(this.model, this.datagrid, viewer);
                await configureClick.call(this.model, this.datagrid, viewer);
                await configureEditable.call(this.model, this.datagrid, viewer);
                await configureSortable.call(this.model, this.datagrid, viewer);
                this._initialized = true;
            } else {
                await createModel(this.datagrid, table, view, this.model);
            }
        }

        get name() {
            return "Datagrid";
        }

        get selectMode() {
            return "toggle";
        }

        get deselectMode() {
            return "pivots";
        }

        async draw(view) {
            await this.activate(view);
            let viewer = this.parentElement;
            const draw = this.datagrid.draw({invalid_columns: true});
            if (!this.model._preserve_focus_state) {
                this.datagrid.scrollTop = 0;
                this.datagrid.scrollLeft = 0;
                deselect(this.datagrid, viewer);
                this.datagrid._resetAutoSize();
            } else {
                this.model._preserve_focus_state = false;
            }

            await draw;
        }

        async update(view) {
            this.model._num_rows = await view.num_rows();
            await this.datagrid.draw();
        }

        async resize() {
            if (this._initialized) {
                await this.datagrid.draw();
            }
        }

        async clear() {
            this.datagrid._resetAutoSize();
            this.datagrid.clear();
        }

        save() {
            if (this.datagrid) {
                const datagrid = this.datagrid;
                if (datagrid[PLUGIN_SYMBOL]) {
                    const token = {};
                    for (const col of Object.keys(datagrid[PLUGIN_SYMBOL])) {
                        const config = Object.assign({}, datagrid[PLUGIN_SYMBOL][col]);
                        if (config?.pos_color) {
                            config.pos_color = config.pos_color[0];
                            config.neg_color = config.neg_color[0];
                        }
                        token[col] = config;
                    }

                    return JSON.parse(JSON.stringify(token));
                }
            }
            return {};
        }

        restore(token) {
            token = JSON.parse(JSON.stringify(token));
            for (const col of Object.keys(token)) {
                const config = token[col];
                if (config?.pos_color) {
                    config.pos_color = create_color_record(config.pos_color);
                    config.neg_color = create_color_record(config.neg_color);
                }
            }

            const datagrid = this.datagrid;
            datagrid[PLUGIN_SYMBOL] = token;
        }

        delete() {
            if (this.datagrid.table_model) {
                this.datagrid._resetAutoSize();
            }
            this.datagrid.clear();
        }
    }
);

/**
 * Appends the default table CSS to `<head>`, should be run once on module
 * import.
 *
 */
function _register_global_styles() {
    const style = document.createElement("style");
    style.textContent = MATERIAL_STYLE;
    document.head.appendChild(style);
}

/******************************************************************************
 *
 * Main
 *
 */

global.registerPlugin("perspective-viewer-datagrid");

_register_global_styles();
