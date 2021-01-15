################################################################################
#
# Copyright (c) 2019, the Perspective Authors.
#
# This file is part of the Perspective library, distributed under the terms of
# the Apache License 2.0.  The full license can be found in the LICENSE file.
#
import os
import os.path
import logging
import tornado.websocket
import tornado.web
import tornado.ioloop
import threading
import concurrent.futures

from functools import partial
from perspective import Table, PerspectiveManager, PerspectiveTornadoHandler


here = os.path.abspath(os.path.dirname(__file__))
file_path = os.path.join(
    here, "..", "..", "node_modules", "superstore-arrow", "superstore.arrow"
)

class PerspectiveStaticFileHandler(tornado.web.StaticFileHandler):
    def set_extra_headers(self, path):
        self.set_header("Cache-control", "no-cache")


def perspective_thread(manager, table):
    """Perspective application thread starts its own tornado IOLoop, and
    adds the table with the name "data_source_one", which will be used
    in the front-end."""
    psp_loop = tornado.ioloop.IOLoop()
    with concurrent.futures.ThreadPoolExecutor() as executor:
        manager.set_loop_callback(partial(psp_loop.run_in_executor, executor))
        manager.host_table("data_source_one", table)
        psp_loop.start()


def make_app():  
    with open(file_path, mode="rb") as file:
        data = file.read();
        table = Table(data)
        for i in range(250):
            table.update(data)

    manager = PerspectiveManager()
    thread = threading.Thread(target=perspective_thread, args=(manager, table))
    thread.daemon = True
    thread.start()

    # manager2 = PerspectiveManager()
    # thread2 = threading.Thread(target=perspective_thread, args=(manager2, table))
    # thread2.daemon = True
    # thread2.start()  

    return tornado.web.Application(
        [
            (
                r"/websocket",
                PerspectiveTornadoHandler,
                {"manager": manager, "check_origin": True},
            ),
            # (
            #     r"/interactive",
            #     PerspectiveTornadoHandler,
            #     {"manager": manager2, "check_origin": True},
            # ),
            (
                r"/node_modules/(.*)",
                PerspectiveStaticFileHandler,
                {"path": "../../node_modules/@finos/"},
            ),
            (
                r"/(.*)",
                PerspectiveStaticFileHandler,
                {"path": "./", "default_filename": "index.html"},
            ),
        ]
    )


if __name__ == "__main__":
    app = make_app()
    app.listen(8080)
    logging.critical("Listening on http://localhost:8080")
    loop = tornado.ioloop.IOLoop.current()
    loop.start()
