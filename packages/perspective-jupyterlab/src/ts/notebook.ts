/******************************************************************************
 *
 * Copyright (c) 2021, the Perspective Authors.
 *
 * This file is part of the Perspective library, distributed under the terms of
 * the Apache License 2.0.  The full license can be found in the LICENSE file.
 *
 */
(window as any).__webpack_public_path__ = document.querySelector("body")!.getAttribute("data-base-url") + "nbextensions/finos-perspective-jupyterlab"; // eslint-disable-line @typescript-eslint/no-non-null-assertion

export * from "./client";
export * from "./model";
export * from "./version";
export * from "./view";
export * from "./widget";

/* css */
import "!!style-loader!css-loader!less-loader!../less/index.less";

import "@finos/perspective-viewer-datagrid";
import "@finos/perspective-viewer-d3fc";

import {PerspectiveJupyterPlugin} from "./plugin";

/**
 * Export the renderer as default.
 */
const plugins = [PerspectiveJupyterPlugin];
export default plugins;
