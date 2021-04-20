/******************************************************************************
 *
 * Copyright (c) 2018, the Perspective Authors.
 *
 * This file is part of the Perspective library, distributed under the terms of
 * the Apache License 2.0.  The full license can be found in the LICENSE file.
 *
 */

import {IJupyterWidgetRegistry} from "@jupyter-widgets/base";

import {PerspectiveModel} from "./model";

import {PerspectiveView} from "./view";

import {PERSPECTIVE_VERSION} from "./version";

const EXTENSION_ID = "@finos/perspective-jupyterlab";

/**
 * PerspectiveJupyterPlugin Defines the Jupyterlab plugin, and registers `PerspectiveModel` and `PerspectiveView`
 * to be called on initialization.
 */
export const PerspectiveJupyterPlugin = {
    id: EXTENSION_ID,
    requires: [IJupyterWidgetRegistry],
    activate: (app, registry) => {
        registry.registerWidget({
            name: EXTENSION_ID,
            version: PERSPECTIVE_VERSION,
            exports: {
                PerspectiveModel: PerspectiveModel,
                PerspectiveView: PerspectiveView
            }
        });
    },
    autoStart: true
};
