/******************************************************************************
 *
 * Copyright (c) 2019, the Perspective Authors.
 *
 * This file is part of the Perspective library, distributed under the terms of
 * the Apache License 2.0.  The full license can be found in the LICENSE file.
 *
 */
import {PerspectiveJupyterClient} from "../../../src/js/client";
import * as widget_base from "@jupyter-widgets/base";

/**
 * Decouple the client's view from PerspectiveView, and allow the client
 * to assert message correctness.
 */
export class MockWidgetView extends widget_base.WidgetView {
    constructor(expected_message) {
        super({});
        this.expected_message = expected_message;
    }
    send(msg) {
        expect(msg).toEqual(this.expected_message);
    }
}

describe("PerspectiveJupyterClient", () => {
    /**
     * Make sure the message schema between the widget and kernel can't be
     * arbitarily changed without a test failure.
     *
     * FIXME: fails because `super()` in `client.ts:41:9` is not correctly
     * transpiled.
     */
    it.skip("Should serialize a message into the correct specification", () => {
        const msg = {
            id: 1,
            cmd: "table_method",
            name: "table",
            method: "schema",
            args: [],
            subscribe: false
        };

        const mock_view = new MockWidgetView({
            id: 1,
            type: "cmd",
            data: JSON.stringify(msg)
        });

        const client = new PerspectiveJupyterClient(mock_view);
        client.send(msg);
    });
});
