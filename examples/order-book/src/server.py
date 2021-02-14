################################################################################
#
# Copyright (c) 2019, the Perspective Authors.
#
# This file is part of the Perspective library, distributed under the terms of
# the Apache License 2.0.  The full license can be found in the LICENSE file.
#

import logging
import tornado.websocket
import tornado.web
import tornado.ioloop
import threading
import random
import operator
import datetime
import perspective
import queue
import getpass

from perspective import PerspectiveManager, PerspectiveTornadoHandler


###############################################################################
#
# Perspective Order Book
#

SYMBOLS = ["TSLA", "AAPL", "JPMC", "NVDA", "GOOGL", "FB"]
ORDER_ID_GEN = 0


class PerspectiveMarket(object):
    """A simulated order book for a system of Players and Symbols."""

    def _init_tables(self):
        """Create market state tables."""
        self._market_table = perspective.Table(
            {"symbol": str, "px": float, "side": str, "time": datetime.datetime},
            limit=10000,
        )

        self._order_table = perspective.Table(
            {
                "symbol": str,
                "px": float,
                "qty": int,
                "filled": int,
                "player": str,
                "side": str,
                "status": str,
                "id": int,
                "time": datetime.datetime,
            },
            index="id",
        )

        self._tx_table = perspective.Table(
            {
                "player": str,
                "notional": float,
                "qty": int,
                "symbol": str,
                "time": datetime.datetime,
                "id": int,
            }
        )

        self._manager.host_table("market", self._market_table)
        self._manager.host_table("orders", self._order_table)
        self._manager.host_table("tx", self._tx_table)

    def _create_order(self, player, side, sym, px, qty):
        global ORDER_ID_GEN
        id = ORDER_ID_GEN
        ORDER_ID_GEN += 1
        self._order_table.update(
            [
                {
                    "symbol": sym,
                    "px": px,
                    "qty": qty,
                    "filled": 0,
                    "player": player,
                    "side": side,
                    "status": "OPEN",
                    "id": id,
                    "time": datetime.datetime.now(),
                }
            ]
        )

        return id

    def _fill_order(self, order):
        self._order_table.update(
            [
                {
                    "id": order["id"],
                    "filled": order["qty"],
                    "status": "CLOSED",
                    "time": datetime.datetime.now(),
                }
            ]
        )

    def _partial_fill_order(self, order, qty):
        self._order_table.update(
            [
                {
                    "filled": order["filled"] + qty,
                    "id": order["id"],
                    "time": datetime.datetime.now(),
                }
            ]
        )

    def _cancel_orders(self, ids):
        self._order_table.update(
            [{"id": order_id, "status": "CANCEL"} for order_id in ids]
        )

    def _transact(self, side, player, other_player, sym, qty, px):
        logging.warning(
            "{} {}s {} shares of {} at ${} from {}".format(
                player, side, qty, sym, px, other_player
            )
        )
        mside = 1 if side == "sell" else -1
        self._tx_table.update(
            [
                {
                    "player": player,
                    "notional": qty * px * mside,
                    "qty": qty * mside * -1,
                    "symbol": sym,
                    "time": datetime.datetime.now(),
                    "id": 0,
                },
                {
                    "player": other_player,
                    "notional": qty * px * mside * -1,
                    "qty": qty * mside,
                    "symbol": sym,
                    "time": datetime.datetime.now(),
                    "id": 0,
                },
            ]
        )

        self._market_table.update(
            [{"symbol": sym, "px": px, "side": side, "time": datetime.datetime.now()}]
        )

    def _borrow(self, player, notional, shares):
        self._tx_table.update(
            [
                {
                    "player": player,
                    "notional": notional,
                    "qty": shares,
                    "symbol": sym,
                    "time": datetime.datetime.now(),
                    "id": 0,
                }
                for sym in SYMBOLS
            ]
        )

    def _get_price_history(self, sym, **kwargs):
        with self._market_table.view(
            filter=[["symbol", "==", sym]],
            sort=[["time", "desc"]],
        ) as view:
            return view.to_records(**kwargs)

    def _get_order_book(self, sym, side):
        with self._order_table.view(
            filter=[
                ["status", "==", "OPEN"],
                ["symbol", "==", sym],
                ["side", "==", "buy" if side == "sell" else "sell"],
            ],
            sort=[["px", "asc" if side == "sell" else "desc"]],
        ) as view:
            return view.to_records()

    def _get_symbol_balance(self, player, symbol):
        with self._tx_table.view(
            row_pivots=["player"],
            filter=[["player", "==", player], ["symbol", "==", symbol]],
            columns=["qty"],
        ) as view:
            balance = view.to_records(end_row=2, leaves_only=True)
            return balance[0]["qty"] if len(balance) > 0 else 0

    def _get_player_balance(self, player):
        with self._tx_table.view(
            row_pivots=["player"],
            filter=[["player", "==", player]],
            columns=["notional"],
        ) as view:
            balance = view.to_records(end_row=2, leaves_only=True)
            return balance[0]["notional"] if len(balance) > 0 else 0

    def _get_symbol_outstanding(self, player, symbol):
        with self._order_table.view(
            row_pivots=["player"],
            filter=[
                ["player", "==", player],
                ["status", "==", "OPEN"],
                ["symbol", "==", symbol],
            ],
            computed_columns=[
                {
                    "column": "outstanding",
                    "computed_function_name": "-",
                    "inputs": ["qty", "filled"],
                }
            ],
            columns=["outstanding"],
        ) as view:
            outstanding = view.to_records(end_row=2, leaves_only=True)
            return outstanding[0]["outstanding"] if len(outstanding) > 0 else 0

    def _get_open_orders_older_than(self, player, side, older_than):
        with self._order_table.view(
            filter=[
                ["player", "==", player],
                ["side", "==", side],
                ["status", "==", "OPEN"],
                ["time", "<", older_than],
            ],
            columns=["id"],
        ) as view:
            return view.to_columns()

    def _get_player_outstanding(self, player):
        with self._order_table.view(
            row_pivots=["player"],
            filter=[["player", "==", player], ["status", "==", "OPEN"]],
            computed_columns=[
                {
                    "column": "outstanding",
                    "computed_function_name": "-",
                    "inputs": ["qty", "filled"],
                },
                {
                    "column": "notional",
                    "computed_function_name": "*",
                    "inputs": ["px", "outstanding"],
                },
            ],
            columns=["notional"],
        ) as view:
            outstanding = view.to_records(end_row=2, leaves_only=True)
            return outstanding[0]["notional"] if len(outstanding) > 0 else 0


###############################################################################
#
# Public Market API
#


class Market(PerspectiveMarket):
    """A simulated order book for a system of Players and Symbols."""

    def __init__(self, manager):
        """Init a new market with initial state.  Calling this method will
        create new `Table()` objects owned by the supplied `PerspectiveManager`.

        Params
        ------
        manager : `PerspectiveManager` instance to own the `Market` state.
        """
        super(Market, self).__init__()
        self._manager = manager
        self._init_tables()

    def _with_thread(f):
        q = queue.Queue()

        def _with_thread(self, *args, **kwargs):
            self._manager._loop_callback(lambda: q.put(f(self, *args, **kwargs)))
            return q.get()

        return _with_thread

    @_with_thread
    def get_qty(self, player, symbol):
        """Get number of shares of `symbol` that `player` has free to trade,
        e.g. those not committed to outstanding orders.
        """
        balance = self._get_symbol_balance(player, symbol)
        outstanding = self._get_symbol_outstanding(player, symbol)
        return balance - outstanding

    @_with_thread
    def get_balance(self, player):
        """Get a `player`'s account balance they have free to spend, e.g. cash
        not committed to oustanding orders.
        """
        balance = self._get_player_balance(player)
        outstanding = self._get_player_outstanding(player)
        return balance - outstanding

    @_with_thread
    def get_spot(self, sym):
        """Get the spot price of a `symbol`."""
        history = self._get_price_history(sym, end_row=1)
        if len(history) > 0:
            return history[0]["px"]
        else:
            return 100

    @_with_thread
    def borrow(self, player, notional, shares):
        """Borrow `notional` cash and `shares` shares for each Symbol."""
        self._borrow(player, notional, shares)

    @_with_thread
    def cancel_orders_older_than(self, player, side, older_than):
        """Cancels all "open" orders older than `older_than` for `player`."""
        ids = self._get_open_orders_older_than(player, side, older_than)
        if "id" in ids:
            self._cancel_orders(ids["id"])
            logging.warning(
                "{} cancels {} orders".format(player, len(ids["id"]))
            )

    @_with_thread
    def market_order(self, player, sym, side, qty, px):
        """Creates a market order for a `qty` and `px`.  These become limit
        orders for the balance if they cannot be immediately filled from the
        limit order book, whose id is returned.

        Returns
        -------
        The `id` of the limit order created, if the order cannot be filled
        immediately, else `None`.
        """
        comp = operator.lt if side == "sell" else operator.gt
        book = self._get_order_book(sym, side)
        while qty > 0 and len(book) > 0:
            order = book.pop()
            if comp(order["px"], px):
                break

            order_qty = order["qty"] - order["filled"]
            cp_player = order["player"]
            if order_qty <= qty:
                self._transact(side, player, cp_player, sym, order_qty, order["px"])
                self._fill_order(order)
                qty -= order_qty
            else:
                self._transact(side, player, cp_player, sym, qty, order["px"])
                self._partial_fill_order(order, qty)
                qty = 0

        if qty > 0:
            return self._create_order(player, side, sym, px, qty)


###############################################################################
#
# AI Traders
#


class AI(object):
    def __init__(self, name, market, loop):
        super(AI, self).__init__()
        self._name = name
        self._market = market
        self._loop = loop
        self._loop.add_callback(self._market.borrow, name, 10000, 500)
        self._loop.add_callback(self._trade_loop)

    def _trade_loop(self):
        self._bid_random()
        self._loop.call_later(random.random() * 3.5, self._trade_loop)

    def _bid_random(self):
        sym = SYMBOLS[random.randint(0, len(SYMBOLS) - 1)]
        qty = random.randint(1, 10)
        spot = self._market.get_spot(sym)
        since = datetime.datetime.now() - datetime.timedelta(seconds=10)
        if random.random() > 0.5:
            side = "buy"
            spot -= 2
            balance = self._market.get_balance(self._name)
            if balance < spot * qty:
                self._market.cancel_orders_older_than(self._name, "buy", since)
                return
        else:
            side = "sell"
            spot += 2
            stock = self._market.get_qty(self._name, sym)
            if stock < qty:
                self._market.cancel_orders_older_than(
                    self._name,
                    "sell",
                    since,
                )
                return

        px = spot + random.randint(-5, 5)
        self._market.market_order(self._name, sym, side, qty, px)


def ai_thread(market):
    loop = tornado.ioloop.IOLoop()
    market_players = []
    for x in range(10):
        market_players.append(AI("AI Player {}".format(x), market, loop))
    loop.add_callback(market.borrow, getpass.getuser(), 10000, 100)
    loop.start()


def perspective_thread(manager):
    loop = tornado.ioloop.IOLoop()
    manager.set_loop_callback(loop.add_callback)
    loop.start()


class OrderHandler(tornado.web.RequestHandler):
    def initialize(self, *args, **kwargs):
        self._market = kwargs["market"]

    def get(self):
        symbol = self.get_argument("symbol")
        side = self.get_argument("side")
        px = float(self.get_argument("px"))
        qty = int(self.get_argument("qty"))
        name = getpass.getuser()

        if side == "buy" and self._market.get_balance(name) < px * qty:
            self.write("false")
            return
        elif side == "sell" and self._market.get_qty(name, symbol) < qty:
            self.write("false")
            return

        id = self._market.market_order(name, symbol, side, qty, px)
        self.write(str(id)if id is not None else "true")

def make_app(manager, market):
    return tornado.web.Application(
        [
            (
                r"/websocket",
                PerspectiveTornadoHandler,
                {"manager": manager, "check_origin": True},
            ),
            (
                r"/order",
                OrderHandler,
                {"market": market},
            ),
            (
                r"/node_modules/(.*)",
                tornado.web.StaticFileHandler,
                {"path": "../../../node_modules/@finos/"},
            ),
            (
                r"/(.*)",
                tornado.web.StaticFileHandler,
                {"path": "./dist/", "default_filename": "index.html"},
            ),
        ],
        websocket_ping_interval=15,
    )


def make_market(manager, market):
    thread = threading.Thread(target=perspective_thread, args=(manager,))
    thread.daemon = True
    thread.start()

    thread = threading.Thread(target=ai_thread, args=(market,))
    thread.daemon = True
    thread.start()


if __name__ == "__main__":
    manager = PerspectiveManager()
    market = Market(manager)

    app = make_app(manager, market)
    app.listen(8080)

    logging.critical("Listening on http://localhost:8080")
    loop = tornado.ioloop.IOLoop.current()
    loop.add_callback(make_market, manager, market)
    loop.start()
