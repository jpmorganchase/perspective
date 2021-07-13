/******************************************************************************
 *
 * Copyright (c) 2017, the Perspective Authors.
 *
 * This file is part of the Perspective library, distributed under the terms of
 * the Apache License 2.0.  The full license can be found in the LICENSE file.
 *
 */

import "./polyfills/index";

import charts from "../charts/charts";
import {initialiseStyles} from "../series/colorStyles";
import style from "../../less/chart.less";

import * as d3 from "d3";

const DEFAULT_PLUGIN_SETTINGS = {
    initial: {
        type: "number",
        count: 1
    },
    selectMode: "select"
};

const styleWithD3FC = `${style}${getD3FCStyles()}`;
const EXCLUDED_SETTINGS = ["crossValues", "mainValues", "realValues", "splitValues", "filter", "data", "size", "colorStyles", "agg_paths"];

function getD3FCStyles() {
    const headerStyles = document.querySelector("head").querySelectorAll("style");
    const d3fcStyles = [];
    headerStyles.forEach(s => {
        if (s.innerText.indexOf("d3fc-") !== -1) {
            d3fcStyles.push(s.innerText);
        }
    });
    return d3fcStyles.join("");
}

export function register(...plugins) {
    plugins = new Set(plugins.length > 0 ? plugins : charts.map(chart => chart.plugin.name));
    charts.forEach(chart => {
        if (plugins.has(chart.plugin.name)) {
            const name = chart.plugin.name.toLowerCase().replace(/[ \t\r\n\/]*/g, "");
            const plugin_name = `perspective-viewer-d3fc-${name}`;
            customElements.define(
                plugin_name,
                class extends HTMLElement {
                    constructor() {
                        super();
                        this._chart = null;
                        this._settings = null;
                        this.render_warning = true;
                    }

                    connectedCallback() {
                        if (!this._initialized) {
                            this.attachShadow({mode: "open"});
                            this.shadowRoot.innerHTML = `<style>${styleWithD3FC}</style>`;
                            this.shadowRoot.innerHTML += `<div id="container" class="chart"></div>`;
                            this._container = this.shadowRoot.querySelector(".chart");
                            this._initialized = true;
                        }
                    }

                    get name() {
                        return chart.plugin.name;
                    }

                    get selectMode() {
                        return chart.plugin.selectMode || "select";
                    }

                    get initial() {
                        return chart.plugin.initial || DEFAULT_PLUGIN_SETTINGS.initial;
                    }

                    get max_cells() {
                        return chart.plugin.max_cells || 4000;
                    }

                    set max_cells(x) {
                        chart.plugin.max_cells = x;
                    }

                    get max_columns() {
                        return chart.plugin.max_columns || 50;
                    }

                    set max_columns(x) {
                        chart.plugin.max_columns = x;
                    }

                    async draw(view, end_col, end_row) {
                        await this.update(view, end_col, end_row, true);
                    }

                    async update(view, end_col, end_row, clear = false) {
                        if (!this.isConnected) {
                            return;
                        }

                        const viewer = this.parentElement;
                        let jsonp, metadata;
                        const realValues = JSON.parse(viewer.getAttribute("columns"));
                        const leaves_only = chart.plugin.name !== "Sunburst";
                        if (end_col && end_row) {
                            jsonp = view.to_json({end_row, end_col, leaves_only});
                        } else if (end_col) {
                            jsonp = view.to_json({end_col, leaves_only});
                        } else if (end_row) {
                            jsonp = view.to_json({end_row, leaves_only});
                        } else {
                            jsonp = view.to_json({leaves_only});
                        }
                        metadata = await Promise.all([viewer.getTable().then(x => x.schema(false)), view.expression_schema(false), view.schema(false), jsonp, view.get_config()]);

                        let [table_schema, expression_schema, view_schema, json, config] = metadata;

                        /**
                         * Retrieve a tree axis column from the table and
                         * expression schemas, returning a String type or
                         * `undefined`.
                         * @param {String} column a column name
                         */
                        const get_pivot_column_type = function(column) {
                            let type = table_schema[column];
                            if (!type) {
                                type = expression_schema[column];
                            }
                            return type;
                        };

                        const {columns, row_pivots, column_pivots, filter} = config;
                        const filtered =
                            row_pivots.length > 0
                                ? json.reduce(
                                      (acc, col) => {
                                          if (col.__ROW_PATH__ && col.__ROW_PATH__.length == row_pivots.length) {
                                              acc.agg_paths.push(acc.aggs.slice());
                                              acc.rows.push(col);
                                          } else {
                                              const len = col.__ROW_PATH__.filter(x => x !== undefined).length;
                                              acc.aggs[len] = col;
                                              acc.aggs = acc.aggs.slice(0, len + 1);
                                          }
                                          return acc;
                                      },
                                      {rows: [], aggs: [], agg_paths: []}
                                  )
                                : {rows: json};
                        const dataMap = (col, i) => (!row_pivots.length ? {...col, __ROW_PATH__: [i]} : col);
                        const mapped = filtered.rows.map(dataMap);

                        let settings = {
                            realValues,
                            crossValues: row_pivots.map(r => ({name: r, type: get_pivot_column_type(r)})),
                            mainValues: columns.map(a => ({name: a, type: view_schema[a]})),
                            splitValues: column_pivots.map(r => ({name: r, type: get_pivot_column_type(r)})),
                            filter,
                            data: mapped,
                            agg_paths: filtered.agg_paths
                        };

                        this._chart = chart;

                        const handler = {
                            set: (obj, prop, value) => {
                                if (!EXCLUDED_SETTINGS.includes(prop)) {
                                    this._container && this._container.dispatchEvent(new Event("perspective-plugin-update", {bubbles: true, composed: true}));
                                }
                                obj[prop] = value;
                                return true;
                            }
                        };

                        this._settings = new Proxy({...this._settings, ...settings}, handler);
                        initialiseStyles(this._container, this._settings);

                        if (clear) {
                            this._container.innerHTML = "";
                        }

                        this._draw();
                    }

                    async clear() {
                        if (this._container) {
                            this._container.innerHTML = "";
                        }
                    }

                    _draw() {
                        if (this._settings.data && this.isConnected) {
                            const containerDiv = d3.select(this._container);
                            const chartClass = `chart ${name}`;
                            this._settings.size = this._container.getBoundingClientRect();

                            if (this._settings.data.length > 0) {
                                this._chart(containerDiv.attr("class", chartClass), this._settings);
                            } else {
                                containerDiv.attr("class", `${chartClass} disabled`);
                            }
                        }
                    }

                    async resize() {
                        if (this.isConnected) {
                            this._draw();
                        }
                    }

                    async restyleElement(...args) {
                        let settings = this._settings;
                        if (settings) {
                            delete settings["colorStyles"];
                            await this.draw(...args);
                        }
                    }

                    async delete() {
                        this._container.innerHTML = "";
                    }

                    getContainer() {
                        return this._container;
                    }

                    save() {
                        const settings = {...this._settings};
                        EXCLUDED_SETTINGS.forEach(s => {
                            delete settings[s];
                        });
                        return settings;
                    }

                    restore(settings) {
                        const new_settings = {};
                        for (const name of EXCLUDED_SETTINGS) {
                            new_settings[name] = this._settings?.[name];
                        }
                        this._settings = {...new_settings, ...settings};
                        this._draw();
                    }
                }
            );

            window.registerPlugin(plugin_name);
        }
    });
}

if (!Element.prototype.matches) {
    Element.prototype.matches = Element.prototype.msMatchesSelector;
}
