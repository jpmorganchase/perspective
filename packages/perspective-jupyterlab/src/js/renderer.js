/******************************************************************************
 *
 * Copyright (c) 2018, the Perspective Authors.
 *
 * This file is part of the Perspective library, distributed under the terms of
 * the Apache License 2.0.  The full license can be found in the LICENSE file.
 *
 */

import {ActivityMonitor} from "@jupyterlab/coreutils";
import {ILayoutRestorer} from "@jupyterlab/application";
import {IThemeManager, WidgetTracker, Dialog, showDialog} from "@jupyterlab/apputils";
import {ABCWidgetFactory, DocumentWidget} from "@jupyterlab/docregistry";
import {PerspectiveWidget} from "./psp_widget";

const perspective = require("@finos/perspective");

/**
 * The name of the factories that creates widgets.
 */
const FACTORY_CSV = "CSVPerspective";
const FACTORY_JSON = "JSONPerspective";
const FACTORY_ARROW = "ArrowPerspective";

const RENDER_TIMEOUT = 1000;

// create here to reuse for exception handling
const baddialog = () => {
    showDialog({
        body: "Perspective could not render the data",
        buttons: [Dialog.okButton({label: "Dismiss"})],
        focusNodeSelector: "input",
        title: "Error"
    });
};

const WORKER = perspective.worker();
export class PerspectiveDocumentWidget extends DocumentWidget {
    constructor(options, type = "csv") {
        super({content: new PerspectiveWidget("Perspective"), context: options.context, reveal: options.reveal});

        this._psp = this.content;
        this._type = type;
        this._context = options.context;

        this._context.ready.then(() => {
            this._update();
            this._monitor = new ActivityMonitor({
                signal: this.context.model.contentChanged,
                timeout: RENDER_TIMEOUT
            });
            this._monitor.activityStopped.connect(this._update, this);
        });
    }

    _update() {
        try {
            let data;
            if (this._type === "csv") {
                // load csv directly
                data = this._context.model.toString();
            } else if (this._type === "arrow") {
                // load arrow directly
                data = Uint8Array.from(atob(this._context.model.toString()), c => c.charCodeAt(0)).buffer;
            } else if (this._type === "json") {
                data = this._context.model.toJSON();
                if (Array.isArray(data) && data.length > 0) {
                    // already is records form, load directly
                } else {
                    // Column-oriented or single records JSON
                    // don't handle for now, just need to implement
                    // a simple transform but we can't handle all
                    // cases
                    throw "Not handled";
                }
            } else {
                // don't handle other mimetypes for now
                throw "Not handled";
            }

            if (this._psp.viewer.table === undefined) {
                // construct new table
                this._psp.viewer.load(WORKER.table(data));
            } else {
                // replace existing table for whatever reason
                this._psp.replace(data);
            }
        } catch {
            baddialog();
        }

        // pickup theme from env
        this._psp.dark = document.body.getAttribute("data-jp-theme-light") === "false";
    }

    dispose() {
        if (this._monitor) {
            this._monitor.dispose();
        }
        this._psp.delete();
        super.dispose();
    }

    get psp() {
        return this._psp;
    }
}

/**
 * A widget factory for CSV widgets.
 */
export class PerspectiveCSVFactory extends ABCWidgetFactory {
    createNewWidget(context) {
        return new PerspectiveDocumentWidget({context}, "csv");
    }
}

/**
 * A widget factory for JSON widgets.
 */
export class PerspectiveJSONFactory extends ABCWidgetFactory {
    createNewWidget(context) {
        return new PerspectiveDocumentWidget({context}, "json");
    }
}

/**
 * A widget factory for arrow widgets.
 */
export class PerspectiveArrowFactory extends ABCWidgetFactory {
    createNewWidget(context) {
        return new PerspectiveDocumentWidget({context}, "arrow");
    }
}

/**
 * Activate cssviewer extension for CSV files
 */
function activate(app, restorer, themeManager) {
    const factorycsv = new PerspectiveCSVFactory({
        name: FACTORY_CSV,
        fileTypes: ["csv"],
        defaultFor: ["csv"],
        readOnly: true
    });

    const factoryjson = new PerspectiveJSONFactory({
        name: FACTORY_JSON,
        fileTypes: ["json", "jsonl"],
        defaultFor: ["json", "jsonl"],
        readOnly: true
    });

    try {
        app.docRegistry.addFileType({
            name: "arrow",
            displayName: "arrow",
            extensions: [".arrow"],
            mimeTypes: ["application/octet-stream"],
            contentType: "file",
            fileFormat: "base64"
        });
    } catch {
        // do nothing
    }

    const factoryarrow = new PerspectiveArrowFactory({
        name: FACTORY_ARROW,
        fileTypes: ["arrow"],
        defaultFor: ["arrow"],
        readOnly: true,
        modelName: "base64"
    });

    const trackercsv = new WidgetTracker({
        namespace: "csvperspective"
    });

    const trackerjson = new WidgetTracker({
        namespace: "jsonperspective"
    });

    const trackerarrow = new WidgetTracker({
        namespace: "arrowperspective"
    });

    if (restorer) {
        // Handle state restoration.
        void restorer.restore(trackercsv, {
            command: "docmanager:open",
            args: widget => ({path: widget.context.path, factory: FACTORY_CSV}),
            name: widget => widget.context.path
        });

        void restorer.restore(trackerjson, {
            command: "docmanager:open",
            args: widget => ({path: widget.context.path, factory: FACTORY_JSON}),
            name: widget => widget.context.path
        });

        void restorer.restore(trackerarrow, {
            command: "docmanager:open",
            args: widget => ({path: widget.context.path, factory: FACTORY_ARROW}),
            name: widget => widget.context.path
        });
    }

    app.docRegistry.addWidgetFactory(factorycsv);
    app.docRegistry.addWidgetFactory(factoryjson);
    app.docRegistry.addWidgetFactory(factoryarrow);

    const ftcsv = app.docRegistry.getFileType("csv");
    const ftjson = app.docRegistry.getFileType("json");
    const ftarrow = app.docRegistry.getFileType("arrow");

    factorycsv.widgetCreated.connect((sender, widget) => {
        // Track the widget.
        void trackercsv.add(widget);

        // Notify the widget tracker if restore data needs to update.
        widget.context.pathChanged.connect(() => {
            void trackercsv.save(widget);
        });

        if (ftcsv) {
            widget.title.iconClass = ftcsv.iconClass || "";
            widget.title.iconLabel = ftcsv.iconLabel || "";
        }
    });

    factoryjson.widgetCreated.connect((sender, widget) => {
        // Track the widget.
        void trackerjson.add(widget);

        // Notify the widget tracker if restore data needs to update.
        widget.context.pathChanged.connect(() => {
            void trackerjson.save(widget);
        });

        if (ftjson) {
            widget.title.iconClass = ftjson.iconClass || "";
            widget.title.iconLabel = ftjson.iconLabel || "";
        }
    });

    factoryarrow.widgetCreated.connect((sender, widget) => {
        // Track the widget.
        void trackerarrow.add(widget);

        // Notify the widget tracker if restore data needs to update.
        widget.context.pathChanged.connect(() => {
            void trackerarrow.save(widget);
        });

        if (ftarrow) {
            widget.title.iconClass = ftarrow.iconClass || "";
            widget.title.iconLabel = ftarrow.iconLabel || "";
        }
    });

    // Keep the themes up-to-date.
    const updateThemes = () => {
        const isLight = themeManager && themeManager.theme ? themeManager.isLight(themeManager.theme) : true;
        trackercsv.forEach(pspDocWidget => {
            pspDocWidget.psp.dark = !isLight;
        });
        trackerjson.forEach(pspDocWidget => {
            pspDocWidget.psp.dark = !isLight;
        });
        trackerarrow.forEach(pspDocWidget => {
            pspDocWidget.psp.dark = !isLight;
        });
    };

    if (themeManager) {
        themeManager.themeChanged.connect(updateThemes);
    }
}

/**
 * The perspective extension for files
 */
export const perspectiveRenderers = {
    activate: activate,
    id: "@finos/perspective-jupyterlab:renderers",
    requires: [],
    optional: [ILayoutRestorer, IThemeManager],
    autoStart: true
};
