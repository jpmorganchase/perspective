/******************************************************************************
 *
 * Copyright (c) 2018, the Perspective Authors.
 *
 * This file is part of the Perspective library, distributed under the terms of
 * the Apache License 2.0.  The full license can be found in the LICENSE file.
 *
 */

const baseConfig = require("./plugin.config");

// rewrite output, blank for prebuilt as its specified in package.json
delete baseConfig.output.filename;
delete baseConfig.output.publicPath;
delete baseConfig.output.path;

module.exports = baseConfig;
