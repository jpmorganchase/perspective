/******************************************************************************
 *
 * Copyright (c) 2018, the Perspective Authors.
 *
 * This file is part of the Perspective library, distributed under the terms of
 * the Apache License 2.0.  The full license can be found in the LICENSE file.
 *
 */

const path = require("path");
const baseConfig = require("./plugin.config");

// use special notebook as entry point
baseConfig.entry.index = "./src/ts/notebook.ts";

// add jupyter widgets as external
baseConfig.externals = [/^([a-z0-9]|@(?!finos\/perspective-viewer)|@(?!jupyter-widgets\/base))/];

// rewrite output
baseConfig.output.path = path.resolve(__dirname, "../../../../python/perspective/perspective/nbextension");
// TODO export as amd?

module.exports = baseConfig;
