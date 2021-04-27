################################################################################
#
# Copyright (c) 2019, the Perspective Authors.
#
# This file is part of the Perspective library, distributed under the terms of
# the Apache License 2.0.  The full license can be found in the LICENSE file.
#

from .libpsp import *
from .core import *
from .core._version import __version__
from .widget import *


def _jupyter_labextension_paths():
    return [{"src": "labextension", "dest": "@finos/perspective-jupyterlab"}]
