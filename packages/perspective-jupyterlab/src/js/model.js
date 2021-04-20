/******************************************************************************
 *
 * Copyright (c) 2019, the Perspective Authors.
 *
 * This file is part of the Perspective library, distributed under the terms of
 * the Apache License 2.0.  The full license can be found in the LICENSE file.
 *
 */

import {DOMWidgetModel} from "@jupyter-widgets/base";
import {PERSPECTIVE_VERSION} from "./version";

/**
 * TODO: document
 */
export class PerspectiveModel extends DOMWidgetModel {
    defaults() {
        return {
            ...super.defaults(),
            _model_name: PerspectiveModel.model_name,
            _model_module: PerspectiveModel.model_module,
            _model_module_version: PerspectiveModel.model_module_version,
            _view_name: PerspectiveModel.view_name,
            _view_module: PerspectiveModel.view_module,
            _view_module_version: PerspectiveModel.view_module_version,

            plugin: "datagrid",
            columns: [],
            row_pivots: [],
            column_pivots: [],
            aggregates: {},
            sort: [],
            filters: [],
            computed_columns: [],
            plugin_config: {},
            dark: false,
            editable: false,
            server: false,
            client: false
        };
    }

    static serializers = {
        ...DOMWidgetModel.serializers
        // Add any extra serializers here
    };

    static model_name = "PerspectiveModel";
    static model_module = "@finos/perspective-jupyterlab";
    static model_module_version = PERSPECTIVE_VERSION;
    static view_name = "PerspectiveView";
    static view_module = "@finos/perspective-jupyterlab";
    static view_module_version = PERSPECTIVE_VERSION;
}
