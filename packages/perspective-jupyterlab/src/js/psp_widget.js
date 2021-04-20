/******************************************************************************
 *
 * Copyright (c) 2018, the Perspective Authors.
 *
 * This file is part of the Perspective library, distributed under the terms of
 * the Apache License 2.0.  The full license can be found in the LICENSE file.
 *
 */

import "@finos/perspective-viewer";

import {Widget} from "@lumino/widgets";
import {MIME_TYPE, PSP_CLASS, PSP_CONTAINER_CLASS, PSP_CONTAINER_CLASS_DARK} from "./utils";

let _increment = 0;

/**
 * Class for perspective lumino widget.
 *
 * @class PerspectiveWidget (name) TODO: document
 */
export class PerspectiveWidget extends Widget {
    constructor(name = "Perspective", options = {}) {
        super({node: options.bindto || document.createElement("div")});
        this._viewer = PerspectiveWidget.createNode(this.node);

        this.title.label = name;
        this.title.caption = `${name}`;
        this.id = `${name}-` + _increment;
        _increment += 1;

        this._set_attributes(options);
    }

    /**
     * Apply user-provided options to the widget.
     *
     * @param options
     */
    _set_attributes(options) {
        const plugin = options.plugin || "datagrid";
        const columns = options.columns || [];
        const row_pivots = options.row_pivots || options["row-pivots"] || [];
        const column_pivots = options.column_pivots || options["column-pivots"] || [];
        const aggregates = options.aggregates || {};
        const sort = options.sort || [];
        const filters = options.filters || [];
        const computed_columns = options.computed_columns || options["computed-columns"] || [];
        const plugin_config = options.plugin_config || {};
        const dark = options.dark || false;
        const editable = options.editable || false;
        const server = options.server || false;
        const client = options.client || false;
        const selectable = options.selectable || false;

        this.server = server;
        this.client = client;
        this.dark = dark;
        this.editable = editable;
        this.plugin = plugin;
        this.plugin_config = plugin_config;
        this.row_pivots = row_pivots;
        this.column_pivots = column_pivots;
        this.sort = sort;
        this.columns = columns;
        this.selectable = selectable;

        // do aggregates after columns
        this.aggregates = aggregates;

        // do computed last
        this.computed_columns = computed_columns;
        this.filters = filters;
    }

    /**********************/
    /* Lumino Overrides */
    /**********************/

    /**
     * Lumino: after visible
     *
     */
    onAfterShow(msg) {
        this.notifyResize();
        super.onAfterShow(msg);
    }

    /**
     * Lumino: widget resize
     *
     */
    onResize(msg) {
        this.notifyResize();
        super.onResize(msg);
    }

    onActivateRequest(msg) {
        if (this.isAttached) {
            this.viewer.focus();
        }
        super.onActivateRequest(msg);
    }

    async notifyResize() {
        if (this.isVisible) {
            await this.viewer.notifyResize();
        }
    }

    save() {
        return this.viewer.save();
    }

    restore(config) {
        return this.viewer.restore(config);
    }

    /**
     * Load a `perspective.table` into the viewer.
     *
     * @param table A `perspective.table` object.
     */
    load(table) {
        this.viewer.load(table);
    }

    /**
     * Update the viewer with new data.
     *
     * @param data
     */
    _update(data) {
        this.viewer.table.update(data);
    }

    /**
     * Removes all rows from the viewer's table. Does not reset viewer state.
     */
    clear() {
        this.viewer.table.clear();
    }

    /**
     * Replaces the data of the viewer's table with new data. New data must
     * conform to the schema of the Table.
     *
     * @param data
     */
    replace(data) {
        this.viewer.table.replace(data);
    }

    /**
     * Deletes this element's data and clears it's internal state (but not its
     * user state). This (or the underlying `perspective.table`'s equivalent
     * method) must be called in order for its memory to be reclaimed.
     */
    delete() {
        this.viewer.delete();
    }

    /**
     * Returns a promise that resolves to the element's edit port ID, used
     * internally when edits are made using datagrid in client/server mode.
     */
    async getEditPort() {
        return await this.viewer.getEditPort();
    }

    get table() {
        return this.viewer.table;
    }

    /***************************************************************************
     *
     * Getters
     *
     */

    /**
     * Returns the underlying `PerspectiveViewer` instance.
     *
     * @returns {PerspectiveViewer} The widget's viewer instance.
     */
    get viewer() {
        return this._viewer;
    }

    /**
     * Returns the name of the widget.
     *
     * @returns {string} the widget name - "Perspective" if not set by the user.
     */
    get name() {
        return this.title.label;
    }

    /**
     * The name of the plugin which visualizes the data in `PerspectiveViewer`.
     *
     */
    get plugin() {
        return this.viewer.getAttribute("plugin");
    }
    set plugin(plugin) {
        this.viewer.setAttribute("plugin", plugin);
    }

    /**
     * The column names that are displayed in the viewer's grid/visualizations.
     *
     * If a column in the dataset is not in this array, it is not shown but can
     * be used for aggregates, sort, and filter.
     */
    get columns() {
        return JSON.parse(this.viewer.getAttribute("columns"));
    }
    set columns(columns) {
        if (columns.length > 0) {
            this.viewer.setAttribute("columns", JSON.stringify(columns));
        } else {
            this.viewer.removeAttribute("columns");
        }
    }

    get row_pivots() {
        return JSON.parse(this.viewer.getAttribute("row-pivots"));
    }
    set row_pivots(row_pivots) {
        this.viewer.setAttribute("row-pivots", JSON.stringify(row_pivots));
    }

    get column_pivots() {
        return JSON.parse(this.viewer.getAttribute("column-pivots"));
    }
    set column_pivots(column_pivots) {
        this.viewer.setAttribute("column-pivots", JSON.stringify(column_pivots));
    }

    get aggregates() {
        return JSON.parse(this.viewer.getAttribute("aggregates"));
    }
    set aggregates(aggregates) {
        this.viewer.setAttribute("aggregates", JSON.stringify(aggregates));
    }

    get sort() {
        return JSON.parse(this.viewer.getAttribute("sort"));
    }
    set sort(sort) {
        this.viewer.setAttribute("sort", JSON.stringify(sort));
    }

    get computed_columns() {
        return JSON.parse(this.viewer.getAttribute("computed-columns"));
    }
    set computed_columns(computed_columns) {
        if (computed_columns.length > 0) {
            this.viewer.setAttribute("computed-columns", JSON.stringify(computed_columns));
        } else {
            this.viewer.removeAttribute("computed-columns");
        }
    }

    get filters() {
        return JSON.parse(this.viewer.getAttribute("filters"));
    }
    set filters(filters) {
        if (filters.length > 0) {
            this.viewer.setAttribute("filters", JSON.stringify(filters));
        } else {
            this.viewer.removeAttribute("filters");
        }
    }

    get plugin_config() {
        return this._plugin_config;
    }
    set plugin_config(plugin_config) {
        this._plugin_config = plugin_config;
        if (this._plugin_config) {
            this.viewer.restore(this._plugin_config);
        }
    }

    /**
     * True if the widget is in client-only mode, i.e. the browser has ownership
     * of the widget's data.
     */
    get client() {
        return this._client;
    }
    set client(client) {
        this._client = client;
    }

    /**
     * True if the widget is in server-only mode, i.e. the Python backend has
     * full ownership of the widget's data, and the widget does not have a
     * `perspective.Table` of its own.
     */
    get server() {
        return this._server;
    }
    set server(server) {
        this._server = server;
    }

    /**
     * Enable or disable dark mode by re-rendering the viewer.
     */
    get dark() {
        return this._dark;
    }
    set dark(dark) {
        this._dark = dark;
        if (this._dark) {
            this.node.classList.add(PSP_CONTAINER_CLASS_DARK);
            this.node.classList.remove(PSP_CONTAINER_CLASS);
        } else {
            this.node.classList.add(PSP_CONTAINER_CLASS);
            this.node.classList.remove(PSP_CONTAINER_CLASS_DARK);
        }
        if (this.isAttached) {
            this.viewer.restyleElement();
        }
    }

    get editable() {
        return this._editable;
    }
    set editable(editable) {
        this._editable = editable;
        if (this._editable) {
            this.viewer.setAttribute("editable", "");
        } else {
            this.viewer.removeAttribute("editable");
        }
    }

    get selectable() {
        return this.viewer.hasAttribute("selectable");
    }

    set selectable(row_selection) {
        if (row_selection) {
            this.viewer.setAttribute("selectable", "");
        } else {
            this.viewer.removeAttribute("selectable");
        }
    }

    toggleConfig() {
        this._viewer.toggleConfig();
    }

    static createNode(node) {
        node.classList.add("p-Widget");
        node.classList.add(PSP_CONTAINER_CLASS);
        const viewer = document.createElement("perspective-viewer");
        viewer.classList.add(PSP_CLASS);
        viewer.setAttribute("type", MIME_TYPE);

        while (node.lastChild) {
            node.removeChild(node.lastChild);
        }

        node.appendChild(viewer);

        // allow perspective's event handlers to do their work
        viewer.addEventListener("contextmenu", event => event.stopPropagation(), false);

        const div = document.createElement("div");
        div.style.setProperty("display", "flex");
        div.style.setProperty("flex-direction", "row");
        node.appendChild(div);

        if (!viewer.notifyResize) {
            console.warn("Warning: not bound to real element");
        } else {
            const resize_observer = new MutationObserver(mutations => {
                if (mutations.some(x => x.attributeName === "style")) {
                    viewer.notifyResize.call(viewer);
                }
            });
            resize_observer.observe(node, {attributes: true});
        }

        return viewer;
    }

    _viewer;
    _plugin_config;
    _client;
    _server;
    _dark;
    _editable;
}
