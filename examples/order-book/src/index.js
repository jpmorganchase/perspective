/******************************************************************************
 *
 * Copyright (c) 2018, the Perspective Authors.
 *
 * This file is part of the Perspective library, distributed under the terms of
 * the Apache License 2.0.  The full license can be found in the LICENSE file.
 *
 */

import perspective from "@finos/perspective";
import "@finos/perspective-viewer-datagrid";
import "@finos/perspective-viewer-d3fc";
import "@finos/perspective-workspace";

import "./index.less";

import layout from "./layout.json";

const websocket = perspective.websocket("ws://localhost:8080/websocket");
const worker = perspective.shared_worker();

window.addEventListener("load", async () => {
    const orders = worker.table(websocket.open_table("orders").view(), {index: "id"});
    const market = worker.table(websocket.open_table("market").view(), {limit: 10000});
    const ledger = worker.table(websocket.open_table("tx").view());

    window.workspace.tables.set("orders", orders);
    window.workspace.tables.set("market", market);
    window.workspace.tables.set("tx", ledger);

    window.workspace.restore(layout);

    const ticket = document.querySelector("#ticket");
    const ticket_market = document.querySelector("#ticket-market");
    const ticket_orders = document.querySelector("#ticket-orders");
    const controls = document.querySelector("#ticket #controls");
    const button = document.querySelector("#ticket #execute");
    const qty = document.querySelector("#ticket input#qty");
    const px = document.querySelector("#ticket input#px");
    const side = document.querySelector("#ticket #side");

    ticket_market.load(ledger);

    const ticket_portfolio = document.querySelector("#ticket-portfolio");
    ticket_portfolio.load(ledger);
    ticket_portfolio.restore({
        plugin: "x_bar",
        columns: ["qty"],
        "row-pivots": ["symbol"],
        filters: [["player", "==", "Developer"]]
    });

    let selected_symbol;

    ticket_market.addEventListener("perspective-select", event => {
        if (event.detail.config.filters.length > 0) {
            selected_symbol = event.detail.config.filters[1][2];
        } else {
            selected_symbol = undefined;
        }

        const is_select = event.detail.config.filters.length > 1 && event.detail.selected;
        controls.classList.toggle("enabled", is_select);
        if (!is_select) {
            px.value = "";
            qty.value = "";
            button.classList.toggle("enabled", false);
        } else {
            console.log(event.detail);
        }
        for (const viewer of window.workspace.querySelectorAll("perspective-viewer")) {
            let filter = JSON.parse(viewer.getAttribute("filters")) || [];
            filter = filter.filter(x => x[0] !== "symbol");
            if (is_select) {
                filter.push(event.detail.config.filters[1]);
            }
            viewer.setAttribute("filters", JSON.stringify(filter));
        }
    });

    ticket_market.restore({
        "row-pivots": ["symbol"],
        columns: ["px", "qty", "time"],
        filters: [["notional", "<", 0]],
        "computed-columns": ['abs("notional" / "qty") as "px"'],
        aggregates: {
            px: "last",
            qty: "sum",
            time: "last"
        }
    });

    // Show the player's outstanding orders
    ticket_orders.load(orders);
    ticket_orders.restore({
        columns: ["symbol", "px", "side", "filled", "qty", "time"],
        plugin: "datagrid",
        filters: [
            ["player", "==", "Developer"],
            ["status", "==", "OPEN"]
        ],
        sort: [["time", "desc"]]
    });

    side.addEventListener("click", () => {
        if (side.textContent === "BUY") {
            side.innerHTML = "SELL";
        } else {
            side.innerHTML = "BUY";
        }
    });

    qty.addEventListener("input", () => {
        button.classList.toggle("enabled", px.value !== "" && qty.value !== "");
    });

    px.addEventListener("input", () => {
        button.classList.toggle("enabled", px.value !== "" && qty.value !== "");
    });

    button.addEventListener("click", async () => {
        const _side = side.textContent.toLowerCase();
        const req = await fetch(`/order?name=Developer&symbol=${selected_symbol}&px=${px.value}&qty=${qty.value}&side=${_side}`);
        const json = await req.json();
        if (json) {
            px.value = "";
            qty.value = "";
            button.classList.toggle("enabled", false);
        }
    });

    ticket.style.display = "flex";
});
