/******************************************************************************
 *
 * Copyright (c) 2017, the Perspective Authors.
 *
 * This file is part of the Perspective library, distributed under the terms of
 * the Apache License 2.0.  The full license can be found in the LICENSE file.
 *
 */
const jsc = require("jsverify");

const replicate = (n, g) => jsc.tuple(new Array(n).fill(g));

const generator = function(length = 100, has_zero = true) {
    const min = has_zero ? 0 : 1;
    return jsc.record({
        a: replicate(length, jsc.number(min, 1000000)),
        b: replicate(length, jsc.number(min, 1000000)),
        c: replicate(length, jsc.string),
        d: replicate(length, jsc.datetime),
        e: replicate(length, jsc.bool)
    });
};

module.exports = perspective => {
    describe("to_arrow invariance", () => {
        describe("1-sided", () => {
            // TODO: add invariant test suite for flattened row paths.
            jsc.property("reconstruct row paths", generator(), async data => {
                const table = perspective.table(data);
                const view = table.view();
                view.delete();
                table.delete();
                return true;
            });
        });
    });
};
