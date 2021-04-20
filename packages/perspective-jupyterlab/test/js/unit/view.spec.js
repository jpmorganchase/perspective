/******************************************************************************
 *
 * Copyright (c) 2019, the Perspective Authors.
 *
 * This file is part of the Perspective library, distributed under the terms of
 * the Apache License 2.0.  The full license can be found in the LICENSE file.
 *
 */

import {MockManager} from "../mocks/manager";
import {PerspectiveJupyterClient} from "../../../src/js/client";
import {PerspectiveJupyterWidget} from "../../../src/js/widget";
import {uuid} from "@jupyter-widgets/base";

// Mock the client so we can see what send() was called with.
jest.mock("../../../src/js/client");

// Mock the widget so we can track its state changes on the view.
jest.mock("../../../src/js/widget");

describe("PerspectiveView", function() {
    describe("_handle_message", function() {
        let manager, model, view;

        beforeEach(async function() {
            PerspectiveJupyterClient.mockClear();
            PerspectiveJupyterWidget.mockClear();
            manager = new MockManager();
            model = await manager.new_model({
                model_name: "PerspectiveModel",
                model_module: "@finos/perspective-jupyterlab",
                model_module_version: "",
                view_name: "PerspectiveView",
                view_module: "@finos/perspective-jupyterlab",
                view_module_version: "",
                model_id: "test_widget"
            });
        });

        /**
         * Assert that the message schema in the widget translates properly
         * to the client, and that the client is able to process the messages.
         */
        it("Should handle a well-formed data message from the kernel", async function() {
            view = await manager.create_view(model)();
            view._handle_message({
                id: 1,
                type: "cmd",
                data: {
                    a: "integer",
                    b: "string",
                    c: "datetime",
                    d: "date",
                    e: "float"
                }
            });
            const mock_client = PerspectiveJupyterClient.mock.instances[0];
            const handle_arg = mock_client._handle.mock.calls[0][0];
            expect(handle_arg).toEqual({
                id: 1,
                data: {
                    a: "integer",
                    b: "string",
                    c: "datetime",
                    d: "date",
                    e: "float"
                }
            });
        });

        /**
         * Assert that the message for loading a remote table or view work
         * properly.
         */
        it("Should handle a well-formed table message from the kernel in server mode", async function() {
            const table_name = uuid();
            view = await manager.create_view(model)();
            view.pWidget.server = true;
            view._handle_message({
                id: -2,
                type: "table",
                data: {
                    table_name: table_name
                }
            });

            const mock_client = PerspectiveJupyterClient.mock.instances[0];

            // `open_table` should be called correctly
            const open_table_arg = mock_client.open_table.mock.calls[0][0];
            expect(open_table_arg).toEqual(table_name);

            const send_arg = mock_client.send.mock.calls[0][0];
            expect(send_arg).toEqual({
                id: -1,
                cmd: "init"
            });
        });

        it("Should handle a well-formed table message from the kernel", async function() {
            view = await manager.create_view(model)();
            const mock_client = PerspectiveJupyterClient.mock.instances[0];
            mock_client.open_table.mockReturnValue({
                view: jest.fn().mockImplementation(name => ({
                    then: jest.fn().mockImplementation(() => new Promise(() => null)),
                    to_arrow: jest.fn().mockImplementation(() => new Promise(() => null)),
                    name
                }))
            });

            // Mock the output of open_table() so `view()` is valid

            const table_name = uuid();
            view._handle_message({
                id: -2,
                type: "table",
                data: {
                    table_name: table_name
                }
            });

            // `open_table` should be called correctly
            const open_table_arg = mock_client.open_table.mock.calls[0][0];
            expect(open_table_arg).toEqual(table_name);

            const send_arg = mock_client.send.mock.calls[0][0];
            expect(send_arg).toEqual({
                id: -1,
                cmd: "init"
            });
        });

        it("Should handle a well-formed table message with index from the kernel", async function() {
            view = await manager.create_view(model)();
            const mock_client = PerspectiveJupyterClient.mock.instances[0];
            mock_client.open_table.mockReturnValue({
                view: jest.fn().mockImplementation(name => ({
                    then: jest.fn().mockImplementation(() => new Promise(() => null)),
                    to_arrow: jest.fn().mockImplementation(() => new Promise(() => null)),
                    name
                }))
            });

            const table_name = uuid();
            view._handle_message({
                id: -2,
                type: "table",
                data: {
                    table_name: table_name,
                    options: {
                        index: "a"
                    }
                }
            });

            // `open_table` should be called correctly
            const open_table_arg = mock_client.open_table.mock.calls[0][0];
            expect(open_table_arg).toEqual(table_name);

            const send_arg = mock_client.send.mock.calls[0][0];
            expect(send_arg).toEqual({
                id: -1,
                cmd: "init"
            });
        });

        it("Should handle a well-formed table message with limit from the kernel", async function() {
            view = await manager.create_view(model)();
            const mock_client = PerspectiveJupyterClient.mock.instances[0];
            mock_client.open_table.mockReturnValue({
                view: jest.fn().mockImplementation(name => ({
                    then: jest.fn().mockImplementation(() => new Promise(() => null)),
                    to_arrow: jest.fn().mockImplementation(() => new Promise(() => null)),
                    name
                }))
            });

            const table_name = uuid();
            view._handle_message({
                id: -2,
                type: "table",
                data: {
                    table_name: table_name,
                    options: {
                        limit: 1000
                    }
                }
            });

            // `open_table` should be called correctly
            const open_table_arg = mock_client.open_table.mock.calls[0][0];
            expect(open_table_arg).toEqual(table_name);

            const send_arg = mock_client.send.mock.calls[0][0];
            expect(send_arg).toEqual({
                id: -1,
                cmd: "init"
            });
        });

        it("Should handle a message containing an Arrow from the kernel", async function() {
            const arrow = new ArrayBuffer(24);
            const to_arrow_pre_msg = {
                id: 1,
                cmd: "view_method",
                binary_length: arrow.byteLength,
                method: "to_arrow",
                name: "view",
                subscribe: false
            };

            view = await manager.create_view(model)();

            // Two messages are sent by the server: the first being the
            // original message with `binary_length` set to true,
            // and the second the binary itself with the message as `null`.
            view._handle_message({
                id: 1,
                type: "cmd",
                data: to_arrow_pre_msg
            });

            expect(view._pending_binary).toEqual(1);

            view._handle_message(null, [new DataView(arrow)]);

            const mock_client = PerspectiveJupyterClient.mock.instances[0];

            // _handle should only be called once with the binary result
            expect(mock_client._handle.mock.calls.length).toEqual(1);

            const handle_arg = mock_client._handle.mock.calls[0][0];
            expect(handle_arg).toEqual({
                id: 1,
                data: {
                    id: 1,
                    data: arrow
                }
            });

            // Arrow should not be transferred/dereferenced by the widget.
            expect(arrow.byteLength).toEqual(24);
        });

        it("Should handle a message containing an Arrow from `on_update` from the kernel", async function() {
            const arrow = new ArrayBuffer(1024);
            const on_update_pre_msg = {
                id: 1,
                cmd: "view_method",
                binary_length: arrow.byteLength,
                data: {
                    port_id: 123
                }
            };

            view = await manager.create_view(model)();

            // Two messages are sent by the server: the first being the
            // original message with `binary_length` set to true,
            // and the second the binary itself with the message as `null`.
            view._handle_message({
                id: 1,
                type: "cmd",
                data: on_update_pre_msg
            });

            expect(view._pending_binary).toEqual(1);
            expect(view._pending_port_id).toEqual(123);

            view._handle_message(null, [new DataView(arrow)]);

            const mock_client = PerspectiveJupyterClient.mock.instances[0];

            // _handle should only be called once with the binary result
            expect(mock_client._handle.mock.calls.length).toEqual(1);

            const handle_arg = mock_client._handle.mock.calls[0][0];
            expect(handle_arg).toEqual({
                id: 1,
                data: {
                    id: 1,
                    data: {
                        port_id: 123,
                        delta: arrow
                    }
                }
            });

            // Arrow should not be transferred/dereferenced by the widget.
            expect(arrow.byteLength).toEqual(1024);
        });

        it("Should correctly delete", async function() {
            view = await manager.create_view(model)();

            const msg = {
                id: 1,
                type: "cmd",
                data: {
                    cmd: "delete"
                }
            };

            view._handle_message(msg);

            const widget_mock = PerspectiveJupyterWidget.mock.instances[0];
            expect(widget_mock.delete.mock.calls.length).toEqual(1);
        });

        describe("Client mode", function() {
            let manager, client_model, client_view;

            beforeEach(async function() {
                PerspectiveJupyterClient.mockClear();
                PerspectiveJupyterWidget.mockClear();
                manager = new MockManager();
                client_model = await manager.new_model({
                    model_name: "PerspectiveModel",
                    model_module: "@finos/perspective-jupyterlab",
                    model_module_version: "",
                    view_name: "PerspectiveView",
                    view_module: "@finos/perspective-jupyterlab",
                    view_module_version: "",
                    model_id: "test_widget"
                });
                client_model.set("client", true);
            });

            it("Should correctly load a dataset", async function() {
                client_view = await manager.create_view(client_model)();

                // manually set client to true - it is derived from the widget
                // init state with the model, and we just care about view
                // behavior here.
                client_view.pWidget.client = true;

                // Data is automatically serialized to JSON in the kernel and
                // un-serialized to an object in JS.
                const data = {
                    a: [1, 2, 3, 4],
                    b: [new Date(), new Date(), new Date(), new Date()],
                    c: ["a", "b", "c", "d"]
                };

                const msg = {
                    id: -2,
                    type: "table",
                    data: {
                        data: data
                    }
                };

                const spy = jest.spyOn(client_view, "_handle_load_message");
                client_view._handle_message(msg);
                expect(spy).toHaveBeenCalled();
            });

            it("Should correctly load a dataset with options", async function() {
                client_view = await manager.create_view(client_model)();
                client_view.pWidget.client = true;
                const data = {
                    a: [1, 2, 3, 4],
                    b: [new Date(), new Date(), new Date(), new Date()],
                    c: ["a", "b", "c", "d"]
                };
                const options = {
                    index: "a"
                };
                const msg = {
                    id: -2,
                    type: "table",
                    data: {
                        data: data,
                        options: options
                    }
                };

                const spy = jest.spyOn(client_view, "_handle_load_message");
                client_view._handle_message(msg);
                expect(spy).toHaveBeenCalled();
            });

            it("Should correctly update a dataset", async function() {
                client_view = await manager.create_view(client_model)();
                client_view.pWidget.client = true;

                const data = {
                    a: [1, 2, 3, 4],
                    b: [new Date(), new Date(), new Date(), new Date()],
                    c: ["a", "b", "c", "d"]
                };

                const msg = {
                    id: null,
                    type: "cmd",
                    data: {
                        cmd: "update",
                        data: data
                    }
                };

                client_view._handle_message(msg);

                const widget_mock = PerspectiveJupyterWidget.mock.instances[0];
                const update_args = widget_mock._update.mock.calls[0];

                expect(update_args[0]).toEqual(data);
            });

            it("Should correctly replace a dataset", async function() {
                client_view = await manager.create_view(client_model)();
                client_view.pWidget.client = true;

                const data = {
                    a: [1, 2, 3, 4],
                    b: [new Date(), new Date(), new Date(), new Date()],
                    c: ["a", "b", "c", "d"]
                };

                const msg = {
                    id: null,
                    type: "cmd",
                    data: {
                        id: null,
                        cmd: "replace",
                        data: data
                    }
                };

                client_view._handle_message(msg);

                const widget_mock = PerspectiveJupyterWidget.mock.instances[0];
                const replace_args = widget_mock.replace.mock.calls[0];

                expect(replace_args[0]).toEqual(data);
            });

            it("Should correctly clear", async function() {
                client_view = await manager.create_view(client_model)();
                client_view.pWidget.client = true;

                const msg = {
                    id: null,
                    type: "cmd",
                    data: {
                        id: null,
                        cmd: "clear"
                    }
                };

                client_view._handle_message(msg);

                const widget_mock = PerspectiveJupyterWidget.mock.instances[0];
                expect(widget_mock.clear.mock.calls.length).toEqual(1);
            });

            it("Should correctly delete", async function() {
                client_view = await manager.create_view(client_model)();
                client_view.pWidget.client = true;

                const msg = {
                    id: null,
                    type: "cmd",
                    data: {
                        id: null,
                        cmd: "delete"
                    }
                };

                client_view._handle_message(msg);

                const widget_mock = PerspectiveJupyterWidget.mock.instances[0];
                expect(widget_mock.delete.mock.calls.length).toEqual(1);
            });
        });
    });
});
