/******************************************************************************
 *
 * Copyright (c) 2019, the Perspective Authors.
 *
 * This file is part of the Perspective library, distributed under the terms of
 * the Apache License 2.0.  The full license can be found in the LICENSE file.
 *
 */
const main_config = require("../../../perspective-test/jest.config.js");

// Define a custom Jest config that properly transforms Typescript source files
// and works with the main `perspective-test` Jest config.
module.exports = Object.assign(main_config, {
    transformIgnorePatterns: ["/node_modules/(?!(lit-html|@jupyter-widgets)/).+\\.js"]
});
