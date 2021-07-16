/******************************************************************************
 *
 * Copyright (c) 2017, the Perspective Authors.
 *
 * This file is part of the Perspective library, distributed under the terms of
 * the Apache License 2.0.  The full license can be found in the LICENSE file.
 *
 */

const node_perspective = require("../../dist/cjs/perspective.node.js");

const RUNTIMES = {
    NODE: node_perspective
};

const clear_tests = require("./clear.js");
const constructor_tests = require("./constructors.js");
const pivot_tests = require("./pivots.js");
const delta_tests = require("./delta.js");
const update_tests = require("./updates.js");
const filter_tests = require("./filters.js");
const internal_tests = require("./internal.js");
const toformat_tests = require("./to_format.js");
const to_arrow_tests = require("./to_arrow.js");
const sort_tests = require("./sort.js");
const multiple_tests = require("./multiple.js");
const pivot_nulls = require("./pivot_nulls.js");
const expression_tests = require("./expressions.js");
const delete_tests = require("./delete.js");
const port_tests = require("./ports.js");

describe("perspective.js", function() {
    Object.keys(RUNTIMES).forEach(function(mode) {
        (typeof WebAssembly === "undefined" && mode === "WASM" ? xdescribe : describe)(mode, function() {
            clear_tests(RUNTIMES[mode]);
            constructor_tests(RUNTIMES[mode]);
            pivot_tests(RUNTIMES[mode]);
            delta_tests(RUNTIMES[mode]);
            update_tests(RUNTIMES[mode]);
            filter_tests(RUNTIMES[mode]);
            toformat_tests(RUNTIMES[mode]);
            to_arrow_tests(RUNTIMES[mode]);
            internal_tests(RUNTIMES[mode], mode);
            sort_tests(RUNTIMES[mode], mode);
            multiple_tests(RUNTIMES[mode], mode);
            pivot_nulls(RUNTIMES[mode], mode);
            expression_tests(RUNTIMES[mode], mode);
            delete_tests(RUNTIMES[mode], mode);
            port_tests(RUNTIMES[mode], mode);
        });
    });
});
