var SECURITIES = ["AAPL.N", "AMZN.N", "QQQ.N", "NVDA.N", "TSLA.N", "FB.N", "MSFT.N", "TLT.N", "XIV.N", "YY.N", "CSCO.N", "GOOGL.N", "PCLN.N"];

var CLIENTS = ["Homer", "Marge", "Bart", "Lisa", "Maggie", "Moe", "Lenny", "Carl", "Krusty"];

// Create 5 random rows of data.
function newRows() {
    var rows = [];
    for (var x = 0; x < 50; x++) {
        rows.push({
            name: SECURITIES[Math.floor(Math.random() * SECURITIES.length)],
            client: CLIENTS[Math.floor(Math.random() * CLIENTS.length)],
            lastUpdate: new Date(),
            chg: Math.random() * 20 - 10,
            bid: Math.random() * 10 + 90,
            ask: Math.random() * 10 + 100,
            vol: Math.random() * 10 + 100
        });
    }
    return rows;
}

window.addEventListener("DOMContentLoaded", async function() {
    // Get element from the DOM.
    var elem = document.getElementsByTagName("perspective-viewer")[0];

    // Create a new Perspective WebWorker instance.
    var worker = perspective.worker();

    // Create a new Perspective table in our `worker`, and limit it it 500 rows.
    var table = await worker.table(newRows(), {
        limit: 500
    });

    // Load the `table` in the `<perspective-viewer>` DOM reference.
    await elem.load(Promise.resolve(table));
    // await new Promise(x => setTimeout(x, 5000));
    // elem.restore({
    //     plugin: "Datagrid",
    //     columns: ["(-)chg", "chg", "(+)chg"],
    //     expressions: ['//(-)chg\nif("chg"<0){"chg"}else{0}', '//(+)chg\nif("chg">0){"chg"}else{0}'],
    //     "row-pivots": ["name"],
    //     "column-pivots": ["client"],
    //     aggregates: {"(-)chg": "avg", "(+)chg": "avg", chg: "avg"},
    //     sort: [["chg", "desc"]],
    //     plugin_config: {
    //         "(-)chg": {
    //             color_mode: "bar",
    //             gradient: 10
    //         },
    //         "(+)chg": {
    //             color_mode: "bar",
    //             gradient: 10
    //         },
    //         chg: {
    //             color_mode: "gradient",
    //             gradient: 10
    //         }
    //     }
    // });

    // Add more rows every 50ms using the `update()` method on the `table` directly.
    (function postRow() {
        table.update(newRows());
        setTimeout(postRow, 50);
    })();
});
