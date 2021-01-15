/******************************************************************************
 *
 * Copyright (c) 2017, the Perspective Authors.
 *
 * This file is part of the Perspective library, distributed under the terms of
 * the Apache License 2.0.  The full license can be found in the LICENSE file.
 *
 */

import "@webcomponents/webcomponentsjs";
import "./polyfill.js";

import {bindTemplate, json_attribute, array_attribute, copy_to_clipboard, invertPromise, throttlePromise} from "./utils.js";
import {renderers, register_debug_plugin} from "./viewer/renderers.js";
import "./row.js";
import "./autocomplete_widget.js";
import "./expression_editor.js";
import "./computed_expressions/widget.js";

import template from "../html/viewer.html";

import view_style from "../less/viewer.less";
import default_style from "../less/default.less";

import {ActionElement} from "./viewer/action_element.js";
import {COMPUTED_EXPRESSION_PARSER} from "./computed_expressions/computed_expression_parser.js";

/**
 * Module for the `<perspective-viewer>` custom element.
 *
 * This module has no exports, but importing it has a side
 * effect: the {@link module:perspective_viewer~PerspectiveViewer} class is
 * registered as a custom element, after which it can be used as a standard DOM
 * element.
 *
 * The documentation in this module defines the instance structure of a
 * `<perspective-viewer>` DOM object instantiated typically, through HTML or any
 * relevent DOM method e.g. `document.createElement("perspective-viewer")` or
 * `document.getElementsByTagName("perspective-viewer")`.
 *
 * @module perspective-viewer
 */

const PERSISTENT_ATTRIBUTES = ["selectable", "editable", "plugin", "computed-columns", "row-pivots", "column-pivots", "aggregates", "filters", "sort", "columns"];

/**
 * The HTMLElement class for `<perspective-viewer>` custom element.
 *
 * This class is not exported, so this constructor cannot be invoked in the
 * typical manner; instead, instances of the class are created through the
 * Custom Elements DOM API.
 *
 * Properties of an instance of this class, such as
 * {@link module:perspective_viewer~PerspectiveViewer#columns}, are reflected on
 * the DOM element as Attributes, and should be accessed as such - e.g.
 * `instance.setAttribute("columns", JSON.stringify(["a", "b"]))`.
 *
 * @class PerspectiveViewer
 * @extends {HTMLElement}
 * @example
 * // Create a new `<perspective-viewer>`
 * const elem = document.createElement("perspective-viewer");
 * elem.setAttribute("columns", JSON.stringify(["a", "b"]));
 * document.body.appendChild(elem);
 *
 */
@bindTemplate(template, view_style, default_style) // eslint-disable-next-line no-unused-vars
class PerspectiveViewer extends ActionElement {
    constructor() {
        super();
        this._register_debounce_instance();
        this._show_config = true;
        this._show_warnings = true;
        this.__render_times = [];
        this._resize_handler = this.notifyResize.bind(this);
        this._computed_expression_parser = COMPUTED_EXPRESSION_PARSER;
        this._edit_port = null;
        this._edit_port_lock = invertPromise();
        window.addEventListener("resize", this._resize_handler);
    }

    connectedCallback() {
        if (Object.keys(renderers.getInstance()).length === 0) {
            register_debug_plugin();
        }

        this.toggleAttribute("settings", true);

        this._register_ids();
        this._register_callbacks();
        this._register_view_options();
        this.toggleConfig();
        this._check_loaded_table();
    }

    /**
     * Sets this `perspective.table.view`'s `sort` property, an Array of column
     * names.
     *
     * @kind member
     * @type {Array<String>} Array of arrays tuples of column name and
     * direction, where the possible values are "asc", "desc", "asc abs", "desc
     * abs" and "none".
     * @fires PerspectiveViewer#perspective-config-update
     * @example <caption>via Javascript DOM</caption>
     * let elem = document.getElementById('my_viewer');
     * elem.setAttribute('sort', JSON.stringify([["x","desc"]));
     * @example <caption>via HTML</caption>
     * <perspective-viewer sort='[["x","desc"]]'></perspective-viewer>
     */
    @array_attribute
    sort(sort) {
        if (sort === null || sort === undefined || sort.length === 0) {
            if (this.hasAttribute("sort")) {
                this.removeAttribute("sort");
            }
            sort = [];
        }
        var inner = this._sort.querySelector("ul");
        this._update_column_list(
            sort,
            inner,
            (s, computed_names) => {
                let dir = "asc";
                if (Array.isArray(s)) {
                    dir = s[1];
                    s = s[0];
                }
                let computed = undefined;
                if (computed_names.includes(s)) {
                    computed = s;
                }
                return this._new_row(s, false, false, false, dir, computed);
            },
            (sort, node) => {
                if (Array.isArray(sort)) {
                    return node.getAttribute("name") === sort[0] && node.getAttribute("sort-order") === sort[1];
                }
                return node.getAttribute("name") === sort;
            }
        );
        this.dispatchEvent(new Event("perspective-config-update"));
        this._debounce_update();
    }

    /**
     * The set of visible columns.
     *
     * @kind member
     * @type {Array<String>}
     * @param {Array} columns An Array of strings, the names of visible columns.
     * @fires PerspectiveViewer#perspective-config-update
     * @example <caption>via Javascript DOM</caption>
     * let elem = document.getElementById('my_viewer');
     * elem.setAttribute('columns', JSON.stringify(["x", "y'"]));
     * @example <caption>via HTML</caption>
     * <perspective-viewer columns='["x", "y"]'></perspective-viewer>
     */
    @array_attribute
    columns(show) {
        if (show === null || show === undefined || show.length === 0) {
            if (this.hasAttribute("columns")) {
                if (this._initial_col_order) {
                    this.setAttribute("columns", JSON.stringify(this._initial_col_order));
                } else {
                    this.removeAttribute("columns");
                }
            }
            show = (this._initial_col_order || []).slice();
        }
        this._update_column_view(show, true);
        this.dispatchEvent(new Event("perspective-config-update"));
        this._debounce_update();
    }

    /* eslint-disable max-len */

    /**
     * Sets new computed columns for the viewer.
     *
     * @kind member
     * @type {Array<Object>}
     * @param {Array<Object>} computed-columns An Array of computed column objects,
     * which have three properties: `column`, a column name for the new column,
     * `computed_function_name`, a String representing the computed function to
     * apply, and `inputs`, an Array of String column names to be used as
     * inputs to the computation.
     * @fires PerspectiveViewer#perspective-config-update
     * @example <caption>via Javascript DOM</caption>
     * let elem = document.getElementById('my_viewer');
     * elem.setAttribute('computed-columns', JSON.stringify([{column: "x+y", computed_function_name: "+", inputs: ["x", "y"]}]));
     * @example <caption>via HTML</caption>
     * <perspective-viewer computed-columns="[{column:'x+y',computed_function_name:'+',inputs:['x','y']}]""></perspective-viewer>
     */
    @array_attribute
    "computed-columns"(computed_columns) {
        const resolve = this._set_updating();

        (async () => {
            if (this._computed_expression_widget.style.display !== "none") {
                this._computed_expression_widget._close_expression_widget();
            }
            if (computed_columns === null || computed_columns === undefined || computed_columns.length === 0) {
                // Remove computed columns from the DOM, and reset the config
                // to exclude all computed columns.
                if (this.hasAttribute("computed-columns")) {
                    this.removeAttribute("computed-columns");
                    const parsed = this._get_view_parsed_computed_columns();
                    this._reset_computed_column_view(parsed);
                    this.removeAttribute("parsed-computed-columns");
                    resolve();
                    return;
                }
                computed_columns = [];
            }

            let parsed_computed_columns = [];

            for (const column of computed_columns) {
                if (typeof column === "string") {
                    // Either validated through the UI or here. If a `table`
                    // has not been loaded when the parsing happens,
                    // the column will be skipped.
                    if (this._computed_expression_parser.is_initialized) {
                        parsed_computed_columns = parsed_computed_columns.concat(this._computed_expression_parser.parse(column));
                    }
                } else {
                    parsed_computed_columns.push(column);
                }
            }

            // Attempt to validate the parsed computed columns against the Table
            let computed_schema = {};

            if (this._table) {
                computed_schema = await this._table.computed_schema(parsed_computed_columns);
                const validated = await this._validate_parsed_computed_columns(parsed_computed_columns, computed_schema);
                if (validated.length !== parsed_computed_columns.length) {
                    // Generate a diff error message with the invalid columns
                    const diff = [];
                    for (let i = 0; i < parsed_computed_columns.length; i++) {
                        if (i > validated.length - 1) {
                            diff.push(parsed_computed_columns[i]);
                        } else {
                            if (parsed_computed_columns[i].column !== validated[i].column) {
                                diff.push(parsed_computed_columns[i]);
                            }
                        }
                    }
                    console.warn("Could not apply these computed columns:", JSON.stringify(diff));
                }
                parsed_computed_columns = validated;
            }

            // Need to refresh the UI so that previous computed columns used in
            // pivots, columns, etc. get cleared
            const old_columns = this._get_view_parsed_computed_columns();
            const to_remove = this._diff_computed_column_view(old_columns, parsed_computed_columns);
            this._reset_computed_column_view(to_remove);

            // Always store a copy of the parsed computed columns for
            // validation of column names, etc.
            this.setAttribute("parsed-computed-columns", JSON.stringify(parsed_computed_columns));

            this._update_computed_column_view(computed_schema);
            this.dispatchEvent(new Event("perspective-config-update"));
            await this._debounce_update();
            resolve();
        })();
    }

    /* eslint-enable max-len */

    /**
     * The set of column aggregate configurations.
     *
     * @kind member
     * @type {Object}
     * @param {Object} aggregates A dictionary whose keys are column names, and
     * values are valid aggregations. The `aggregates` attribute works as an
     * override; in lieu of a key for a column supplied by the developers, a
     * default will be selected and reflected to the attribute based on the
     * column's type.  See {@link perspective/src/js/defaults.js}
     * @fires PerspectiveViewer#perspective-config-update
     * @example <caption>via Javascript DOM</caption>
     * let elem = document.getElementById('my_viewer');
     * elem.setAttribute('aggregates', JSON.stringify({x: "distinct count"}));
     * @example <caption>via HTML</caption>
     * <perspective-viewer aggregates='{"x": "distinct count"}'>
     * </perspective-viewer>
     */
    @json_attribute
    aggregates(show) {
        if (show === null || show === undefined || Object.keys(show).length === 0) {
            if (this.hasAttribute("aggregates")) {
                this.removeAttribute("aggregates");
            }
            show = {};
        }

        let lis = this._get_view_dom_columns();
        lis.map(x => {
            let agg = show[x.getAttribute("name")];
            if (agg) {
                x.setAttribute("aggregate", Array.isArray(agg) ? JSON.stringify(agg) : agg);
            }
        });
        this.dispatchEvent(new Event("perspective-config-update"));
        this._debounce_update();
    }

    /**
     * The set of column filter configurations.
     *
     * @kind member
     * @type {Array<Array>} filters An Array of filter configs. A filter
     * config is an Array of three elements: * The column name. * The filter
     * operation as a String. See
     * {@link perspective/src/js/config/constants.js} * The filter argument, as
     * a String, float or Array<String> as the filter operation demands.
     * @fires PerspectiveViewer#perspective-config-update
     * @example <caption>via Javascript DOM</caption>
     * let filters = [
     *     ["x", "<", 3],
     *     ["y", "contains", "abc"]
     * ];
     * let elem = document.getElementById('my_viewer');
     * elem.setAttribute('filters', JSON.stringify(filters));
     * @example <caption>via HTML</caption>
     * <perspective-viewer filters='[["x", "<", 3], ["y", "contains", "abc"]]'>
     * </perspective-viewer>
     */
    @array_attribute
    filters(filters) {
        if (filters === null || filters === undefined || filters.length === 0) {
            if (this.hasAttribute("filters")) {
                this.removeAttribute("filters");
            }
            filters = [];
        }
        if (!this._updating_filter) {
            var inner = this._filters.querySelector("ul");
            this._update_column_list(
                filters,
                inner,
                (filter, computed_names) => {
                    const fterms = JSON.stringify({
                        operator: filter[1],
                        operand: filter[2]
                    });
                    const name = filter[0];
                    let computed = undefined;
                    if (computed_names.includes(name)) {
                        computed = name;
                    }
                    return this._new_row(name, undefined, undefined, fterms, undefined, computed);
                },
                (filter, node) =>
                    node.getAttribute("name") === filter[0] &&
                    node.getAttribute("filter") ===
                        JSON.stringify({
                            operator: filter[1],
                            operand: filter[2]
                        })
            );
        }
        this.dispatchEvent(new Event("perspective-config-update"));
        this._debounce_update();
    }

    /**
     * Sets the currently selected plugin, via its `name` field, and removes
     * any children the previous plugin may have left behind in the light DOM.
     *
     * @type {String}
     * @fires PerspectiveViewer#perspective-config-update
     */
    set plugin(v) {
        if (v === "null" || v === null || v === undefined) {
            this.setAttribute("plugin", this._vis_selector.options[0].value);
            return;
        }
        this.innerHTML = "";
        const plugin_names = Object.keys(renderers.getInstance());
        if (this.hasAttribute("plugin")) {
            let plugin = this.getAttribute("plugin");
            if (plugin_names.indexOf(plugin) === -1) {
                const guess_plugin = plugin_names.find(x => x.indexOf(plugin) > -1);
                if (guess_plugin) {
                    console.warn(`Unknown plugin "${plugin}", using "${guess_plugin}"`);
                    this.setAttribute("plugin", guess_plugin);
                } else {
                    console.error(`Unknown plugin "${plugin}"`);
                    this.setAttribute("plugin", this._vis_selector.options[0].value);
                }
            } else {
                if (this._vis_selector.value !== plugin) {
                    this._vis_selector.value = plugin;
                    this._vis_selector_changed();
                }
                this._set_row_styles();
                this._set_column_defaults();
                this.dispatchEvent(new Event("perspective-config-update"));
            }
        } else {
            this.setAttribute("plugin", this._vis_selector.options[0].value);
        }
    }

    /**
     * Sets this `perspective.table.view`'s `column_pivots` property.
     *
     * @kind member
     * @type {Array<String>} Array of column names
     * @fires PerspectiveViewer#perspective-config-update
     */
    @array_attribute
    "column-pivots"(pivots) {
        if (pivots === null || pivots === undefined || pivots.length === 0) {
            if (this.hasAttribute("column-pivots")) {
                this.removeAttribute("column-pivots");
            }
            pivots = [];
        }

        const inner = this._column_pivots.querySelector("ul");
        this._update_column_list(pivots, inner, (pivot, computed_names) => {
            let computed = undefined;
            if (computed_names.includes(pivot)) {
                computed = pivot;
            }
            return this._new_row(pivot, undefined, undefined, undefined, undefined, computed);
        });
        this.dispatchEvent(new Event("perspective-config-update"));
        this._debounce_update();
    }

    /**
     * Sets this `perspective.table.view`'s `row_pivots` property.
     *
     * @kind member
     * @type {Array<String>} Array of column names
     * @fires PerspectiveViewer#perspective-config-update
     */
    @array_attribute
    "row-pivots"(pivots) {
        if (pivots === null || pivots === undefined || pivots.length === 0) {
            if (this.hasAttribute("row-pivots")) {
                this.removeAttribute("row-pivots");
            }
            pivots = [];
        }

        const inner = this._row_pivots.querySelector("ul");
        this._update_column_list(pivots, inner, (pivot, computed_names) => {
            let computed = undefined;
            if (computed_names.includes(pivot)) {
                computed = pivot;
            }
            return this._new_row(pivot, undefined, undefined, undefined, undefined, computed);
        });
        this.dispatchEvent(new Event("perspective-config-update"));
        this._debounce_update();
    }

    /**
     * Determines whether this viewer is editable or not (though it is
     * ultimately up to the plugin as to whether editing is implemented).
     *
     * @kind member
     * @type {Boolean} Is this viewer editable?
     * @fires PerspectiveViewer#perspective-config-update
     */
    set editable(x) {
        if (x === "null") {
            if (this.hasAttribute("editable")) {
                this.removeAttribute("editable");
            }
        } else {
            this.toggleAttribute("editable", true);
        }
        this._debounce_update({force_update: true});
        this.dispatchEvent(new Event("perspective-config-update"));
    }

    /**
     * Determines the render throttling behavior. Can be an integer, for
     * millisecond window to throttle render event; or, if `undefined`,
     * will try to determine the optimal throttle time from this component's
     * render framerate.
     *
     * @kind member
     * @type {Number|String} The throttle rate - milliseconds (integer), or the
     * enum "adaptive" for a dynamic throttle based on render time.
     * @example
     * <!-- Only draws at most 1 frame/sec. -->
     * <perspective-viewer throttle="1000"></perspective-viewer>
     */
    set throttle(x) {
        if (x === "null") {
            if (this.hasAttribute("throttle")) {
                this.removeAttribute("throttle");
            }
        }
        // Returns the throttle time, but also perform validaiton - we only want
        // the latter here.
        this._calculate_throttle_timeout();
    }

    /*
     * Determines whether row selections is enabled on this viewer (though it is
     * ultimately up to the plugin as to whether selectable is implemented).
     *
     * @kind member
     * @type {Boolean} Is this viewer editable?
     * @fires PerspectiveViewer#perspective-config-update
     */
    set selectable(x) {
        if (x === "null") {
            if (this.hasAttribute("selectable")) {
                this.removeAttribute("selectable");
            }
        } else {
            this.toggleAttribute("selectable", true);
        }
        this._debounce_update({force_update: true});
        this.dispatchEvent(new Event("perspective-config-update"));
    }

    /**
     * This element's `perspective.table` instance.
     *
     * @readonly
     */
    get table() {
        return this._table;
    }

    /**
     * This element's `perspective.table.view` instance. The instance itself
     * will change after every `PerspectiveViewer#perspective-config-update`
     * event.
     *
     * @readonly
     */
    get view() {
        return this._view;
    }

    /**
     * Load data. If `load` or `update` have already been called on this
     * element, its internal `perspective.table` will also be deleted.
     *
     * @async
     * @param {any} data The data to load, as a `perspective.Table` or
     * `Promise<perspective.Table>`.
     * @returns {Promise<void>} A promise which resolves once the data is loaded
     * and a `perspective.view` has been created.
     * @fires module:perspective_viewer~PerspectiveViewer#perspective-click
     * PerspectiveViewer#perspective-view-update
     * ]);
     * @example <caption>Load perspective.table</caption>
     * const my_viewer = document.getElementById('#my_viewer');
     * const tbl = perspective.table("x,y\n1,a\n2,b");
     * my_viewer.load(tbl);
     * @example <caption>Load Promise<perspective.table></caption>
     * const my_viewer = document.getElementById('#my_viewer');
     * const tbl = async () => perspective.table("x,y\n1,a\n2,b");
     * my_viewer.load(tbl);
     */
    async load(data) {
        let table;
        if (data instanceof Promise) {
            table = await data;
        } else {
            try {
                data = data.trim();
            } catch (e) {}
            if (data.type === "table") {
                table = data;
            } else {
                throw new Error(`Unrecognized input type ${typeof data}.  Please use a \`perspective.Table()\``);
            }
        }
        if (this.isConnected) {
            await this._load_table(table);
        } else {
            this._table = table;
        }
    }

    /**
     * Determine whether to reflow the viewer and redraw.
     *
     */
    @throttlePromise
    async notifyResize(immediate) {
        const resized = await this._check_responsive_layout();
        if (!resized && !document.hidden && this.offsetParent) {
            await this._plugin.resize.call(this, immediate);
        }
    }

    /**
     * Duplicate an existing `<perspective-element>`, including data and view
     * settings.  The underlying `perspective.table` will be shared between both
     * elements
     *
     * @param {any} widget A `<perspective-viewer>` instance to clone.
     */
    clone(widget) {
        if (this._inner_drop_target) {
            this._inner_drop_target.innerHTML = widget._inner_drop_target.innerHTML;
        }

        this._load_table(widget.table);
        this.restore(widget.save());
    }

    /**
     * Deletes this element and clears it's internal state (but not its
     * user state).  This (or the underlying `perspective.view`'s equivalent
     * method) must be called in order for its memory to be reclaimed, as well
     * as the recipcorcal method on the `perspective.table` which this viewer is
     * bound to.
     *
     * @returns {Promise<Boolean>} Whether or not this call resulted in the
     * underlying `perspective.table` actually being deleted.
     */
    delete() {
        let x = this._clear_state();
        if (this._plugin.delete) {
            this._plugin.delete.call(this);
        }
        window.removeEventListener("resize", this._resize_handler);
        return x;
    }

    /**
     * Restyles the elements and to pick up any style changes
     */
    restyleElement() {
        this._restyle_plugin();
    }

    /**
     * Serialize this element's attribute/interaction state.
     *
     * @returns {object} a serialized element.
     */
    save() {
        let obj = {};
        const cols = new Set(PERSISTENT_ATTRIBUTES);
        for (let key = 0; key < this.attributes.length; key++) {
            let attr = this.attributes[key];
            if (cols.has(attr.name)) {
                if (attr.value === "") {
                    obj[attr.name] = true;
                } else if (attr.name !== "plugin" && attr.value !== undefined && attr.value !== null) {
                    obj[attr.name] = JSON.parse(attr.value);
                } else {
                    obj[attr.name] = attr.value;
                }
                cols.delete(attr.name);
            }
        }
        for (const col of cols) {
            obj[col] = null;
        }
        if (this._plugin.save) {
            obj.plugin_config = this._plugin.save.call(this);
        }
        return obj;
    }

    /**
     * Restore this element to a state as generated by a reciprocal call to
     * `save` or `serialize`.
     *
     * @param {Object|String} config returned by `save` or `serialize`.
     * @returns {Promise<void>} A promise which resolves when the changes have
     * been applied.
     */
    async restore(config) {
        if (typeof config === "string") {
            config = JSON.parse(config);
        }
        for (const key of PERSISTENT_ATTRIBUTES) {
            if (config.hasOwnProperty(key)) {
                let val = config[key];
                if (val === true) {
                    this.toggleAttribute(key, true);
                } else if (val !== undefined && val !== null && val !== false) {
                    if (typeof val !== "string") {
                        val = JSON.stringify(val);
                    }
                    this.setAttribute(key, val);
                } else {
                    this.removeAttribute(key);
                }
            }
        }

        if (this._plugin.restore && config.plugin_config) {
            this._plugin.restore.call(this, config.plugin_config);
        }
        await this._debounce_update();
    }

    /**
     * Flush any pending attribute modifications to this element.
     *
     * @returns {Promise<void>} A promise which resolves when the current
     * attribute state has been applied.
     */
    async flush() {
        await new Promise(setTimeout);
        while (this.hasAttribute("updating")) {
            await this._updating_promise;
        }
    }

    async swap(table) {
        this._table = table;
        await this._debounce_update();
    }

    /**
     * Reset's this element's view state and attributes to default.  Does not
     * delete this element's `perspective.table` or otherwise modify the data
     * state.
     */
    reset() {
        this.removeAttribute("row-pivots");
        this.removeAttribute("column-pivots");
        this.removeAttribute("filters");
        this.removeAttribute("sort");
        if (this._initial_col_order) {
            this.setAttribute("columns", JSON.stringify(this._initial_col_order));
        } else {
            this.removeAttribute("columns");
        }
        this.setAttribute("plugin", Object.keys(renderers.getInstance())[0]);
        this.dispatchEvent(new Event("perspective-config-update"));
        this._hide_context_menu();
    }

    /**
     * Download this element's data as a CSV file.
     *
     * @param {Boolean} [flat=false] Whether to use the element's current view
     * config, or to use a default "flat" view.
     * @memberof PerspectiveViewer
     */
    async download(flat = false) {
        const view = flat ? this._table.view() : this._view;
        const csv = await view.to_csv({formatted: true});
        const element = document.createElement("a");
        const binStr = csv;
        const len = binStr.length;
        const arr = new Uint8Array(len);
        for (let i = 0; i < len; i++) {
            arr[i] = binStr.charCodeAt(i);
        }
        const blob = new Blob([arr]);
        element.setAttribute("href", window.URL.createObjectURL(blob));
        element.setAttribute("download", "perspective.csv");
        element.style.display = "none";
        document.body.appendChild(element);
        element.click();
        document.body.removeChild(element);
        this._hide_context_menu();
    }

    /**
     * Copies this element's view data (as a CSV) to the clipboard.  This method
     * must be called from an event handler, subject to the browser's
     * restrictions on clipboard access.  See
     * {@link https://www.w3.org/TR/clipboard-apis/#allow-read-clipboard}.
     */
    copy(flat = false) {
        let data;
        const view = flat ? this._table.view() : this._view;
        view.to_csv({formatted: true})
            .then(csv => {
                data = csv;
            })
            .catch(err => {
                console.error(err);
                data = "";
            });
        let count = 0;
        let f = () => {
            if (typeof data !== "undefined") {
                copy_to_clipboard(data);
            } else if (count < 200) {
                count++;
                setTimeout(f, 50);
            } else {
                console.warn("Timeout expired - copy to clipboard cancelled.");
            }
        };
        f();
        this._hide_context_menu();
    }

    /**
     * Opens/closes the element's config menu.
     *
     * @async
     */
    async toggleConfig() {
        await this._toggle_config();
    }

    /**
     * Returns a promise that resolves to the element's edit port ID, used
     * internally when edits are made using DataGrid.
     *
     * @async
     */
    async getEditPort() {
        return this._edit_port_lock;
    }
}

/**
 * `perspective-click` is fired whenever underlying `view`'s grid or chart are
 * clicked providing a detail that includes a `config`, `column_names` and
 * `row`.
 *
 * @event module:perspective_viewer~PerspectiveViewer#perspective-click
 * @type {object}
 * @property {Array} column_names - Includes a list of column names.
 * @property {object} config - Contains a property `filters` that can be applied
 * to a `<perspective-viewer>` through the use of `restore()` updating it to
 * show the filtered subset of data..
 * @property {Array} row - Includes the data row.
 */

/**
 * `perspective-config-update` is fired whenever an configuration attribute has
 * been modified, by the user or otherwise.
 *
 * @event module:perspective_viewer~PerspectiveViewer#perspective-config-update
 * @type {String}
 */

/**
 * `perspective-view-update` is fired whenever underlying `view`'s data has
 * updated, including every invocation of `load` and `update`.
 *
 * @event module:perspective_viewer~PerspectiveViewer#perspective-view-update
 * @type {String}
 */
