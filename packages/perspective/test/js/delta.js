/******************************************************************************
 *
 * Copyright (c) 2017, the Perspective Authors.
 *
 * This file is part of the Perspective library, distributed under the terms of
 * the Apache License 2.0.  The full license can be found in the LICENSE file.
 *
 */

const _ = require("underscore");

let data = [
    {x: 1, y: "a", z: true},
    {x: 2, y: "b", z: false},
    {x: 3, y: "c", z: true},
    {x: 4, y: "d", z: false}
];
//let partial_change_x = [{x: 5, y: "a"}, {x: 6, y: "b"}];
let partial_change_y = [
    {x: 1, y: "string1"},
    {x: 2, y: "string2"}
];
let partial_change_z = [
    {x: 1, z: false},
    {x: 2, z: true}
];
let partial_change_y_z = [
    {x: 1, y: "string1", z: false},
    {x: 2, y: "string2", z: true}
];
let partial_change_nonseq = [
    {x: 1, y: "string1", z: false},
    {x: 4, y: "string2", z: true}
];

async function match_delta(perspective, delta, expected) {
    let table = await perspective.table(delta);
    let view = await table.view();
    let json = await view.to_json();
    expect(json).toEqual(expected);
    view.delete();
    table.delete();
}

module.exports = perspective => {
    describe("Row delta", function() {
        describe("0-sided row delta", function() {
            it("returns changed rows", async function(done) {
                let table = await perspective.table(data, {index: "x"});
                let view = await table.view();
                view.on_update(
                    async function(updated) {
                        const expected = [
                            {x: 1, y: "string1", z: true},
                            {x: 2, y: "string2", z: false}
                        ];
                        await match_delta(perspective, updated.delta, expected);
                        view.delete();
                        table.delete();
                        done();
                    },
                    {mode: "row"}
                );
                table.update(partial_change_y);
            });

            it("returns changed rows from schema", async function(done) {
                let table = await perspective.table(
                    {
                        x: "integer",
                        y: "string",
                        z: "boolean"
                    },
                    {index: "x"}
                );
                let view = await table.view();
                view.on_update(
                    async function(updated) {
                        const expected = [
                            {x: 1, y: "d", z: false},
                            {x: 2, y: "b", z: false},
                            {x: 3, y: "c", z: true}
                        ];
                        await match_delta(perspective, updated.delta, expected);
                        view.delete();
                        table.delete();
                        done();
                    },
                    {mode: "row"}
                );
                table.update([
                    {x: 1, y: "a", z: true},
                    {x: 2, y: "b", z: false},
                    {x: 3, y: "c", z: true},
                    {x: 1, y: "d", z: false}
                ]);
            });

            it("returns added rows", async function(done) {
                let table = await perspective.table(data);
                let view = await table.view();
                view.on_update(
                    async function(updated) {
                        const expected = [
                            {x: 1, y: "string1", z: null},
                            {x: 2, y: "string2", z: null}
                        ];
                        await match_delta(perspective, updated.delta, expected);
                        view.delete();
                        table.delete();
                        done();
                    },
                    {mode: "row"}
                );
                table.update(partial_change_y);
            });

            it("returns added rows from schema", async function(done) {
                let table = await perspective.table({
                    x: "integer",
                    y: "string",
                    z: "boolean"
                });
                let view = await table.view();
                view.on_update(
                    async function(updated) {
                        await match_delta(perspective, updated.delta, data);
                        view.delete();
                        table.delete();
                        done();
                    },
                    {mode: "row"}
                );
                table.update(data);
            });

            it("returns deleted columns", async function(done) {
                let table = await perspective.table(data, {index: "x"});
                let view = await table.view();
                view.on_update(
                    async function(updated) {
                        const expected = [
                            {x: 1, y: null, z: true},
                            {x: 4, y: null, z: false}
                        ];
                        await match_delta(perspective, updated.delta, expected);
                        view.delete();
                        table.delete();
                        done();
                    },
                    {mode: "row"}
                );
                table.update([
                    {x: 1, y: null},
                    {x: 4, y: null}
                ]);
            });

            it("returns changed rows in sorted context", async function(done) {
                let table = await perspective.table(data, {index: "x"});
                let view = await table.view({
                    sort: [["x", "desc"]]
                });
                view.on_update(
                    async function(updated) {
                        const expected = [
                            {x: 2, y: "string2", z: false},
                            {x: 1, y: "string1", z: true}
                        ];
                        await match_delta(perspective, updated.delta, expected);
                        view.delete();
                        table.delete();
                        done();
                    },
                    {mode: "row"}
                );
                table.update(partial_change_y);
            });

            it("returns changed rows in sorted context from schema", async function(done) {
                let table = await perspective.table(
                    {
                        x: "integer",
                        y: "string",
                        z: "boolean"
                    },
                    {index: "x"}
                );
                let view = await table.view({
                    sort: [["x", "desc"]]
                });
                view.on_update(
                    async function(updated) {
                        const expected = [
                            {x: 4, y: "a", z: true},
                            {x: 3, y: "c", z: true},
                            {x: 2, y: "b", z: false},
                            {x: 1, y: "d", z: false}
                        ];
                        await match_delta(perspective, updated.delta, expected);
                        view.delete();
                        table.delete();
                        done();
                    },
                    {mode: "row"}
                );
                table.update([
                    {x: 1, y: "a", z: true},
                    {x: 2, y: "b", z: false},
                    {x: 3, y: "c", z: true},
                    {x: 1, y: "d", z: false},
                    {x: 4, y: "a", z: true}
                ]);
            });

            it("returns added rows in filtered context from schema", async function(done) {
                let table = await perspective.table({
                    x: "integer",
                    y: "string",
                    z: "boolean"
                });
                let view = await table.view({
                    filter: [["x", ">", 3]]
                });
                view.on_update(
                    async function(updated) {
                        await match_delta(perspective, updated.delta, [{x: 4, y: "d", z: false}]);
                        view.delete();
                        table.delete();
                        done();
                    },
                    {mode: "row"}
                );
                table.update(data);
            });

            it("returns changed rows in non-sequential update", async function(done) {
                let table = await perspective.table(data, {index: "x"});
                let view = await table.view();
                view.on_update(
                    async function(updated) {
                        const expected = partial_change_nonseq;
                        await match_delta(perspective, updated.delta, expected);
                        view.delete();
                        table.delete();
                        done();
                    },
                    {mode: "row"}
                );
                table.update(partial_change_nonseq);
            });
        });

        describe("0-sided row delta, randomized column order", function() {
            it("returns changed rows", async function(done) {
                let table = await perspective.table(data, {index: "x"});
                let columns = _.shuffle(await table.columns());
                let view = await table.view({
                    columns: columns
                });
                view.on_update(
                    async function(updated) {
                        const expected = [
                            {x: 1, y: "string1", z: true},
                            {x: 2, y: "string2", z: false}
                        ];
                        await match_delta(perspective, updated.delta, expected);
                        view.delete();
                        table.delete();
                        done();
                    },
                    {mode: "row"}
                );
                table.update(partial_change_y);
            });

            it("returns changed rows from schema", async function(done) {
                let table = await perspective.table(
                    {
                        x: "integer",
                        y: "string",
                        z: "boolean"
                    },
                    {index: "x"}
                );
                let columns = _.shuffle(await table.columns());
                let view = await table.view({
                    columns: columns
                });
                view.on_update(
                    async function(updated) {
                        const expected = [
                            {x: 1, y: "d", z: false},
                            {x: 2, y: "b", z: false},
                            {x: 3, y: "c", z: true}
                        ];
                        await match_delta(perspective, updated.delta, expected);
                        view.delete();
                        table.delete();
                        done();
                    },
                    {mode: "row"}
                );
                table.update([
                    {x: 1, y: "a", z: true},
                    {x: 2, y: "b", z: false},
                    {x: 3, y: "c", z: true},
                    {x: 1, y: "d", z: false}
                ]);
            });

            it("returns added rows", async function(done) {
                let table = await perspective.table(data);
                let columns = _.shuffle(await table.columns());
                let view = await table.view({
                    columns: columns
                });
                view.on_update(
                    async function(updated) {
                        const expected = [
                            {x: 1, y: "string1", z: null},
                            {x: 2, y: "string2", z: null}
                        ];
                        await match_delta(perspective, updated.delta, expected);
                        view.delete();
                        table.delete();
                        done();
                    },
                    {mode: "row"}
                );
                table.update(partial_change_y);
            });

            it("returns added rows from schema", async function(done) {
                let table = await perspective.table({
                    x: "integer",
                    y: "string",
                    z: "boolean"
                });
                let columns = _.shuffle(await table.columns());
                let view = await table.view({
                    columns: columns
                });
                view.on_update(
                    async function(updated) {
                        await match_delta(perspective, updated.delta, data);
                        view.delete();
                        table.delete();
                        done();
                    },
                    {mode: "row"}
                );
                table.update(data);
            });

            it("returns deleted columns", async function(done) {
                let table = await perspective.table(data, {index: "x"});
                let columns = _.shuffle(await table.columns());
                let view = await table.view({
                    columns: columns
                });
                view.on_update(
                    async function(updated) {
                        const expected = [
                            {x: 1, y: null, z: true},
                            {x: 4, y: null, z: false}
                        ];
                        await match_delta(perspective, updated.delta, expected);
                        view.delete();
                        table.delete();
                        done();
                    },
                    {mode: "row"}
                );
                table.update([
                    {x: 1, y: null},
                    {x: 4, y: null}
                ]);
            });

            it("returns changed rows in non-sequential update", async function(done) {
                let table = await perspective.table(data, {index: "x"});
                let columns = _.shuffle(await table.columns());
                let view = await table.view({
                    columns: columns
                });
                view.on_update(
                    async function(updated) {
                        const expected = partial_change_nonseq;
                        await match_delta(perspective, updated.delta, expected);
                        view.delete();
                        table.delete();
                        done();
                    },
                    {mode: "row"}
                );
                table.update(partial_change_nonseq);
            });
        });

        describe("0-sided row delta, column order subset", function() {
            it("returns changed rows", async function(done) {
                let table = await perspective.table(data, {index: "x"});
                let view = await table.view({
                    columns: ["y"]
                });
                view.on_update(
                    async function(updated) {
                        const expected = [{y: "string1"}, {y: "string2"}];
                        await match_delta(perspective, updated.delta, expected);
                        view.delete();
                        table.delete();
                        done();
                    },
                    {mode: "row"}
                );
                table.update(partial_change_y);
            });

            it("returns changed rows from schema", async function(done) {
                let table = await perspective.table(
                    {
                        x: "integer",
                        y: "string",
                        z: "boolean"
                    },
                    {index: "x"}
                );
                let view = await table.view({
                    columns: ["z"]
                });
                view.on_update(
                    async function(updated) {
                        const expected = [{z: false}, {z: false}, {z: true}];
                        await match_delta(perspective, updated.delta, expected);
                        view.delete();
                        table.delete();
                        done();
                    },
                    {mode: "row"}
                );
                table.update([
                    {x: 1, y: "a", z: true},
                    {x: 2, y: "b", z: false},
                    {x: 3, y: "c", z: true},
                    {x: 1, y: "d", z: false}
                ]);
            });

            it("returns added rows", async function(done) {
                let table = await perspective.table(data);
                let view = await table.view({
                    columns: ["y"]
                });
                view.on_update(
                    async function(updated) {
                        const expected = [{y: "string1"}, {y: "string2"}];
                        await match_delta(perspective, updated.delta, expected);
                        view.delete();
                        table.delete();
                        done();
                    },
                    {mode: "row"}
                );
                table.update(partial_change_y);
            });

            it("returns added rows from schema", async function(done) {
                let table = await perspective.table({
                    x: "integer",
                    y: "string",
                    z: "boolean"
                });
                let view = await table.view({
                    columns: ["z"]
                });
                view.on_update(
                    async function(updated) {
                        await match_delta(perspective, updated.delta, [{z: true}, {z: false}, {z: true}, {z: false}]);
                        view.delete();
                        table.delete();
                        done();
                    },
                    {mode: "row"}
                );
                table.update(data);
            });

            it("returns deleted rows", async function(done) {
                let table = await perspective.table(data, {index: "x"});
                let view = await table.view({
                    columns: ["y"]
                });
                view.on_update(
                    async function(updated) {
                        const expected = [{y: null}, {y: null}];
                        await match_delta(perspective, updated.delta, expected);
                        view.delete();
                        table.delete();
                        done();
                    },
                    {mode: "row"}
                );
                table.update([
                    {x: 1, y: null},
                    {x: 4, y: null}
                ]);
            });

            it("returns changed rows in non-sequential update", async function(done) {
                let table = await perspective.table(data, {index: "x"});
                let view = await table.view({
                    columns: ["y"]
                });
                view.on_update(
                    async function(updated) {
                        await match_delta(perspective, updated.delta, [{y: "string1"}, {y: "string2"}]);
                        view.delete();
                        table.delete();
                        done();
                    },
                    {mode: "row"}
                );
                table.update(partial_change_nonseq);
            });
        });

        describe("1-sided row delta", function() {
            it("returns changed rows", async function(done) {
                let table = await perspective.table(data, {index: "x"});
                let view = await table.view({
                    row_pivots: ["y"],
                    aggregates: {y: "distinct count", z: "distinct count"}
                });
                view.on_update(
                    async function(updated) {
                        const expected = [
                            {x: 1, y: 1, z: 1},
                            {x: 2, y: 1, z: 1}
                        ];
                        await match_delta(perspective, updated.delta, expected);
                        view.delete();
                        table.delete();
                        done();
                    },
                    {mode: "row"}
                );
                table.update(partial_change_y);
            });

            it("returns changed rows, multilevel pivot", async function(done) {
                let table = perspective.table(data, {index: "x"});
                let view = table.view({
                    row_pivots: ["y", "z"],
                    aggregates: {y: "distinct count", z: "distinct count"}
                });
                view.on_update(
                    async function(updated) {
                        const expected = [
                            {x: 1, y: 1, z: 1},
                            {x: 2, y: 1, z: 1}
                        ];
                        await match_delta(perspective, updated.delta, expected);
                        view.delete();
                        table.delete();
                        done();
                    },
                    {mode: "row"}
                );
                table.update(partial_change_y);
            });

            it("returns nothing when updated data is not in pivot", async function(done) {
                let table = await perspective.table(data, {index: "x"});
                let view = await table.view({
                    row_pivots: ["y"],
                    aggregates: {y: "distinct count", z: "distinct count"}
                });
                view.on_update(
                    async function(updated) {
                        await match_delta(perspective, updated.delta, []);
                        view.delete();
                        table.delete();
                        done();
                    },
                    {mode: "row"}
                );
                table.update(partial_change_z);
            });

            it("returns nothing when updated data is not in pivot, multilevel", async function(done) {
                let table = perspective.table(data, {index: "x"});
                let view = table.view({
                    row_pivots: ["y", "x"],
                    aggregates: {y: "distinct count", z: "distinct count"}
                });
                view.on_update(
                    async function(updated) {
                        await match_delta(perspective, updated.delta, []);
                        view.delete();
                        table.delete();
                        done();
                    },
                    {mode: "row"}
                );
                table.update(partial_change_z);
            });

            it("returns added rows", async function(done) {
                let table = await perspective.table(data);
                let view = await table.view({
                    row_pivots: ["y"],
                    aggregates: {y: "distinct count", z: "distinct count"}
                });
                view.on_update(
                    async function(updated) {
                        const expected = [
                            {x: 13, y: 6, z: 3},
                            {x: 1, y: 1, z: 1},
                            {x: 2, y: 1, z: 1}
                        ];
                        await match_delta(perspective, updated.delta, expected);
                        view.delete();
                        table.delete();
                        done();
                    },
                    {mode: "row"}
                );
                table.update(partial_change_y);
            });

            it("returns added rows, multilevel pivot", async function(done) {
                let table = perspective.table(data);
                let view = table.view({
                    row_pivots: ["y", "z"],
                    aggregates: {y: "distinct count", z: "distinct count"}
                });
                view.on_update(
                    async function(updated) {
                        const expected = [
                            {x: 13, y: 6, z: 3},
                            {x: 1, y: 1, z: 1},
                            {x: 2, y: 1, z: 1}
                        ];
                        await match_delta(perspective, updated.delta, expected);
                        view.delete();
                        table.delete();
                        done();
                    },
                    {mode: "row"}
                );
                table.update(partial_change_y);
            });

            it("returns deleted columns", async function(done) {
                let table = await perspective.table(data, {index: "x"});
                let view = await table.view({
                    row_pivots: ["y"],
                    aggregates: {y: "distinct count", z: "distinct count"}
                });
                view.on_update(
                    async function(updated) {
                        // underlying data changes, but only total aggregate row is affected
                        const expected = [{x: 10, y: 3, z: 2}];
                        await match_delta(perspective, updated.delta, expected);
                        view.delete();
                        table.delete();
                        done();
                    },
                    {mode: "row"}
                );
                table.update([
                    {x: 1, y: null},
                    {x: 4, y: null}
                ]);
            });

            it("returns deleted columns, multilevel", async function(done) {
                let table = perspective.table(data, {index: "x"});
                let view = table.view({
                    row_pivots: ["y", "z"],
                    aggregates: {y: "distinct count", z: "distinct count"}
                });
                view.on_update(
                    async function(updated) {
                        // underlying data changes, but only total aggregate row is affected
                        const expected = [{x: 10, y: 3, z: 2}];
                        await match_delta(perspective, updated.delta, expected);
                        view.delete();
                        table.delete();
                        done();
                    },
                    {mode: "row"}
                );
                table.update([
                    {x: 1, y: null},
                    {x: 4, y: null}
                ]);
            });

            it("returns changed rows in non-sequential update", async function(done) {
                let table = await perspective.table(data, {index: "x"});
                let view = await table.view({
                    row_pivots: ["y"],
                    aggregates: {y: "distinct count", z: "distinct count"}
                });
                view.on_update(
                    async function(updated) {
                        // aggregates are sorted, in this case by string comparator - "string1" and "string2" are at the end
                        const expected = [
                            {x: 1, y: 1, z: 1},
                            {x: 4, y: 1, z: 1}
                        ];
                        await match_delta(perspective, updated.delta, expected);
                        view.delete();
                        table.delete();
                        done();
                    },
                    {mode: "row"}
                );
                table.update(partial_change_nonseq);
            });

            it("returns changed rows in non-sequential update, multilevel", async function(done) {
                let table = perspective.table(data, {index: "x"});
                let view = table.view({
                    row_pivots: ["y", "z"],
                    aggregates: {y: "distinct count", z: "distinct count"}
                });
                view.on_update(
                    async function(updated) {
                        // aggregates are sorted, in this case by string comparator - "string1" and "string2" are at the end
                        const expected = [
                            {x: 1, y: 1, z: 1},
                            {x: 4, y: 1, z: 1}
                        ];
                        await match_delta(perspective, updated.delta, expected);
                        view.delete();
                        table.delete();
                        done();
                    },
                    {mode: "row"}
                );
                table.update(partial_change_nonseq);
            });
        });

        describe("2-sided row delta", function() {
            it("returns changed rows when updated data in row pivot", async function(done) {
                let table = await perspective.table(data, {index: "y"});
                let view = await table.view({
                    row_pivots: ["y"],
                    column_pivots: ["x"]
                });
                view.on_update(
                    async function(updated) {
                        const json = await view.to_json();
                        json.map(d => {
                            delete d["__ROW_PATH__"];
                        });
                        const expected = json.slice(0, 3);
                        await match_delta(perspective, updated.delta, expected);
                        view.delete();
                        table.delete();
                        done();
                    },
                    {mode: "row"}
                );
                table.update(partial_change_y);
            });

            it("returns changed rows when updated data in column pivot", async function(done) {
                let table = await perspective.table(data, {index: "x"});
                let view = await table.view({
                    row_pivots: ["y"],
                    column_pivots: ["z"]
                });
                view.on_update(
                    async function(updated) {
                        const json = await view.to_json();
                        json.map(d => {
                            delete d["__ROW_PATH__"];
                        });
                        const expected = json.slice(0, 3);
                        await match_delta(perspective, updated.delta, expected);
                        view.delete();
                        table.delete();
                        done();
                    },
                    {mode: "row"}
                );
                table.update(partial_change_z);
            });

            it("returns changed rows when updated data in row and column pivot", async function(done) {
                let table = await perspective.table(data, {index: "x"});
                let view = await table.view({
                    row_pivots: ["y"],
                    column_pivots: ["z"]
                });
                view.on_update(
                    async function(updated) {
                        const json = await view.to_json();
                        json.map(d => {
                            delete d["__ROW_PATH__"];
                        });
                        const expected = [json[0], json[3], json[4]];
                        await match_delta(perspective, updated.delta, expected);
                        view.delete();
                        table.delete();
                        done();
                    },
                    {mode: "row"}
                );
                table.update(partial_change_y_z);
            });

            it("returns nothing when updated data is not in pivot", async function(done) {
                let table = await perspective.table(data, {index: "x"});
                let view = await table.view({
                    row_pivots: ["y"],
                    column_pivots: ["x"],
                    aggregates: {y: "distinct count", z: "distinct count"}
                });
                view.on_update(
                    async function(updated) {
                        await match_delta(perspective, updated.delta, []);
                        view.delete();
                        table.delete();
                        done();
                    },
                    {mode: "row"}
                );
                table.update(partial_change_z);
            });

            it("returns added rows", async function(done) {
                let table = await perspective.table(data);
                let view = await table.view({
                    row_pivots: ["y"],
                    column_pivots: ["x"]
                });
                view.on_update(
                    async function(updated) {
                        const json = await view.to_json();
                        json.map(d => {
                            delete d["__ROW_PATH__"];
                        });
                        const expected = json.slice(0, 3);
                        await match_delta(perspective, updated.delta, expected);
                        view.delete();
                        table.delete();
                        done();
                    },
                    {mode: "row"}
                );
                table.update(partial_change_y);
            });

            it("returns deleted columns", async function(done) {
                let table = await perspective.table(data, {index: "x"});
                let view = await table.view({
                    row_pivots: ["y"],
                    column_pivots: ["x"],
                    aggregates: {y: "unique"},
                    columns: ["x", "y", "z"]
                });
                view.on_update(
                    async function(updated) {
                        // underlying data changes, but only total aggregate row is affected
                        const expected = await view.to_json();
                        expected.splice(3, 1);
                        expected.map(d => {
                            delete d["__ROW_PATH__"];
                        });
                        await match_delta(perspective, updated.delta, expected);
                        view.delete();
                        table.delete();
                        done();
                    },
                    {mode: "row"}
                );
                table.update([
                    {x: 1, y: null},
                    {x: 2, y: null},
                    {x: 4, y: null}
                ]);
            });

            it("returns changed rows in non-sequential update", async function(done) {
                let table = await perspective.table(data, {index: "x"});
                let view = await table.view({
                    row_pivots: ["y"],
                    column_pivots: ["x"],
                    aggregates: {y: "distinct count", z: "distinct count"}
                });
                view.on_update(
                    async function(updated) {
                        // aggregates are sorted, in this case by string comparator - "string1" and "string2" are at the end
                        const json = await view.to_json();
                        json.map(d => {
                            delete d["__ROW_PATH__"];
                        });
                        const expected = [json[3], json[4]];
                        await match_delta(perspective, updated.delta, expected);
                        view.delete();
                        table.delete();
                        done();
                    },
                    {mode: "row"}
                );
                table.update(partial_change_nonseq);
            });

            it("returns changed rows in column-only pivots", async function(done) {
                let table = await perspective.table(data, {index: "x"});
                let view = await table.view({
                    column_pivots: ["x"]
                });
                view.on_update(
                    async function(updated) {
                        const json = await view.to_json();
                        const expected = [
                            {"1|x": 1, "1|y": "string1", "1|z": false, "2|x": 2, "2|y": "b", "2|z": false, "3|x": 3, "3|y": "c", "3|z": true, "4|x": 4, "4|y": "string2", "4|z": true},
                            json[0],
                            json[3]
                        ];
                        await match_delta(perspective, updated.delta, expected);
                        view.delete();
                        table.delete();
                        done();
                    },
                    {mode: "row"}
                );
                table.update(partial_change_nonseq);
            });
        });
    });
};
