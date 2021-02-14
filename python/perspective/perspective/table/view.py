################################################################################
#
# Copyright (c) 2019, the Perspective Authors.
#
# This file is part of the Perspective library, distributed under the terms of
# the Apache License 2.0.  The full license can be found in the LICENSE file.
#

import os
import pandas
from functools import partial, wraps
from random import random

from .view_config import ViewConfig
from ._data_formatter import to_format, _parse_format_options
from ._constants import COLUMN_SEPARATOR_STRING
from ._utils import _str_to_pythontype
from ._callback_cache import _PerspectiveCallBackCache
from ._date_validator import _PerspectiveDateValidator
from .libbinding import (
    make_view_unit,
    make_view_zero,
    make_view_one,
    make_view_two,
    to_arrow_unit,
    to_arrow_zero,
    to_arrow_one,
    to_arrow_two,
    get_row_delta_unit,
    get_row_delta_zero,
    get_row_delta_one,
    get_row_delta_two,
)


class View(object):
    """A :class:`~perspective.View` object represents a specific transform
    (pivot, filter, sort, etc) configuration on an underlying
    :class:`~perspective.Table`.  :class:`~perspective.View` objects
    cannot be directly instantiated - they must be derived from an existing
    :class:`~perspective.Table` via the :func:`~perspective.Table.view()`
    method.

    :class:`~perspective.View` instances receive all updates from the
    :class:`~perspective.Table` from which they are derived, and can be
    serialized (via ``to_*`` methods) or trigger a callback when it is updated.
    :class:`~perspective.View` objects will remain in memory and actively
    process updates until :obj:`~perspective.View.delete()` method is called.
    """

    def __init__(self, Table, **kwargs):
        self._name = "py_" + str(random())
        self._table = Table
        self._config = ViewConfig(**kwargs)
        self._sides = self.sides()

        date_validator = _PerspectiveDateValidator()

        self._is_unit_context = (
            self._table._index == ""
            and self._sides == 0
            and len(self._config.get_row_pivots()) == 0
            and len(self._config.get_column_pivots()) == 0
            and len(self._config.get_filter()) == 0
            and len(self._config.get_sort()) == 0
            and len(self._config.get_computed_columns()) == 0
        )

        if self._is_unit_context:
            self._view = make_view_unit(
                self._table._table,
                self._name,
                COLUMN_SEPARATOR_STRING,
                self._config,
                date_validator,
            )
        elif self._sides == 0:
            self._view = make_view_zero(
                self._table._table,
                self._name,
                COLUMN_SEPARATOR_STRING,
                self._config,
                date_validator,
            )
        elif self._sides == 1:
            self._view = make_view_one(
                self._table._table,
                self._name,
                COLUMN_SEPARATOR_STRING,
                self._config,
                date_validator,
            )
        else:
            self._view = make_view_two(
                self._table._table,
                self._name,
                COLUMN_SEPARATOR_STRING,
                self._config,
                date_validator,
            )

        self._column_only = self._view.is_column_only()
        self._update_callbacks = self._table._update_callbacks
        self._delete_callbacks = _PerspectiveCallBackCache()
        self._client_id = None

    def __enter__(self):
        return self

    def __exit__(self, *args, **kwargs):
        self.delete()

    def get_config(self):
        """Returns a copy of the immutable configuration ``kwargs`` from which
        this :class:`~perspective.View` was instantiated.

        Returns:
            :obj:`dict`: ``kwargs`` supplied to the
                :func:`perspective.Table.view()` method.
        """
        return self._config.get_config()

    def sides(self):
        """An integer representing the # of hierarchial axis on this
        :class:`~perspective.View`.

        0 - Neither ``row_pivots`` nor ``column_pivots`` properties are set.

        1 - ``row_pivots`` is set.

        2 - ``column_pivots`` is set (and also maybe ``row_pivots``).

        Returns:
            :obj:`int`: 0 <= N <= 2
        """
        if (
            len(self._config.get_row_pivots()) > 0
            or len(self._config.get_column_pivots()) > 0
        ):
            if len(self._config.get_column_pivots()) > 0:
                return 2
            else:
                return 1
        else:
            return 0

    def num_rows(self):
        """The number of aggregated rows in the :class:`~perspective.View`.

        This count includes the total aggregate rows for all ``row_pivots``
        depth levels, and can also be affected by any applied ``filter``.

        Returns:
            :obj:`int`: Number of rows.
        """
        return self._view.num_rows()

    def num_columns(self):
        """The number of aggregated columns in the :class:`~perspective.View`.
        This is affected by the ``column_pivots`` that are applied to the
        :class:`~perspective.View`.

        Returns:
            :obj:`int`: Number of columns.
        """
        return self._view.num_columns()

    def get_row_expanded(self, idx):
        """Returns whether row at `idx` is expanded or collapsed.

        Returns:
            :obj:`bool`: Is this row expanded?
        """
        return self._view.get_row_expanded(idx)

    def expand(self, idx):
        """Expands the row at 'idx', i.e. displaying its leaf rows.

        Args:
            idx (:obj:`int`): Row index to expand.
        """
        return self._view.expand(idx, len(self._config.get_row_pivots()))

    def collapse(self, idx):
        """Collapses the row at 'idx', i.e. hiding its leaf rows.

        Args:
            idx (:obj:`int`): Row index to collapse.
        """
        return self._view.collapse(idx)

    def set_depth(self, depth):
        """Sets the expansion depth of the pivot tree.

        Args:
            depth (:obj:`int`): Depth to collapse all nodes to, which
                may be no greater then the length of the ``row_pivots``
                property.
        """
        return self._view.set_depth(depth, len(self._config.get_row_pivots()))

    def column_paths(self):
        """Returns the names of the columns as they show in the
        :class:`~perspective.View`, i.e. the hierarchial columns when
        ``column_pivots`` is applied.

        Returns:
            :obj:`list` of :obj`str`: Aggregated column names.
        """
        paths = self._view.column_paths()
        string_paths = []

        for path in paths:
            string_paths.append(
                COLUMN_SEPARATOR_STRING.join([p.to_string(False) for p in path])
            )

        return string_paths

    def schema(self, as_string=False):
        """The schema of this :class:`~perspective.View`, which is a key-value
        map that contains the column names and their Python data types.

        If the columns are aggregated, their aggregated types will be shown
        returned instead.

        Keyword Args:
            as_string (:obj:`bool`): returns data types as string
                representations, if ``True``.

        Returns:
            :obj:`dict`: A map of :obj:`str` column name to :obj:`str` or
                :obj:`type`, depending on the value of ``as_string`` kwarg.
        """
        if as_string:
            return {item[0]: item[1] for item in self._view.schema().items()}

        return {
            item[0]: _str_to_pythontype(item[1]) for item in self._view.schema().items()
        }

    def computed_schema(self, as_string=False):
        if as_string:
            return {item[0]: item[1] for item in self._view.computed_schema().items()}

        return {
            item[0]: _str_to_pythontype(item[1])
            for item in self._view.computed_schema().items()
        }

    def on_update(self, callback, mode=None):
        """Add a callback to be fired when :func:`perspective.Table.update()` is
        called on the parent :class:`~perspective.Table`.

        Multiple callbacks can be set through calling ``on_update`` multiple
        times, and will be called in the order they are set. Callback must be a
        callable function that takes exactly 1 or 2 parameters, depending on
        whether `on_update` is called with `mode="row"`. The first parameter is
        always `port_id`, an :obj:`int` that indicates which input port the
        update comes from. A `RuntimeError` will be thrown if the callback
        has mis-configured parameters.

        Args:
            callback (:obj:`callable`): a callable function reference that will
                be called when :func:`perspective.Table.update()` is called.
            mode (:obj:`str`): if set to "row", the callback will be passed
                an Arrow-serialized dataset of the rows that were updated.
                Defaults to "none".

        Examples:
            >>> def updater(port_id):
            ...     print("Update fired on port", port_id)
            >>> def updater_with_delta(port_id, delta):
            ...     print("Update on port", port_id, "delta len:", len(delta)))
            >>> view.on_update(updater)
            >>> view.on_update(updater, mode="row")
            >>> table.update({"a": [1]})'
            >>> Update fired on port 0
            >>> Update on port 0 delta len: 64
        """
        self._table._state_manager.call_process(self._table._table.get_id())
        mode = mode or "none"

        if not callable(callback):
            raise ValueError("Invalid callback - must be a callable function")

        if mode not in ["none", "row"]:
            raise ValueError(
                'Invalid update mode {} - valid on_update modes are "none" or "row"'.format(
                    mode
                )
            )

        if mode == "row":
            if not self._view._get_deltas_enabled():
                self._view._set_deltas_enabled(True)

        wrapped_callback = partial(
            self._wrapped_on_update_callback, mode=mode, callback=callback
        )

        self._update_callbacks.add_callback(
            {
                "name": self._name,
                "orig_callback": callback,
                "callback": wrapped_callback,
            }
        )

    def remove_update(self, callback):
        """Given a callback function, remove it from the list of callbacks.

        Args:
            callback (:obj:`func`): a function reference that will be removed.

        Examples:
            >>> table = perspective.Table(data)
            >>> view = table.view()
            >>> view2 = table.view()
            >>> def callback():
            ...     print("called!")
            >>> view.on_update(callback)
            >>> view2.on_update(callback)
            >>> table.update(new_data)
            called!
            >>> view2.remove_update(callback)
            >>> table.update(new_data) # callback removed and will not fire
        """
        self._table._state_manager.call_process(self._table._table.get_id())
        if not callable(callback):
            return ValueError("remove_update callback should be a callable function!")
        self._update_callbacks.remove_callbacks(
            lambda cb: cb["orig_callback"] == callback
        )

    def on_delete(self, callback):
        """Set a callback to be run when the :func:`perspective.View.delete()`
        method is called on this :class:`~perspective.View`.

        Args:
            callback (:obj:`callable`): A callback to run after
                :func:`perspective.View.delete()` method has been called.

        Examples:
            >>> def deleter():
            >>>     print("Delete called!")
            >>> view.on_delete(deleter)
            >>> view.delete()
            >>> Delete called!
        """
        if not callable(callback):
            return ValueError("on_delete callback must be a callable function!")
        self._delete_callbacks.add_callback(callback)

    def delete(self):
        """Delete the :class:`~perspective.View` and clean up all associated
        callbacks.

        This method must be called to clean up callbacks used by the
        :class:`~perspective.View`, as well as allow for deletion of the
        underlying :class:`~perspective.Table`.

        Examples:
            >>> table = perspective.Table(data)
            >>> view = table.view()
            >>> view.delete()
        """
        self._table._state_manager.remove_process(self._table._table.get_id())
        self._table._views.pop(self._table._views.index(self._name))
        # remove the callbacks associated with this view
        self._update_callbacks.remove_callbacks(lambda cb: cb["name"] == self._name)
        [cb() for cb in self._delete_callbacks]

    def remove_delete(self, callback):
        """Remove the delete callback associated with this
        :class:`~perspective.View`.

        Args:
            callback (:obj:`callable`): A reference to a callable function that
                will be removed from delete callbacks.

        Examples:
            >>> table = perspective.Table(data)
            >>> view = table.view()
            >>> view2 = table.view()
            >>> def callback():
            ...     print("called!")
            >>> view.on_delete(callback)
            >>> view2.on_delete(callback)
            >>> view.delete()
            called!
            >>> view2.remove_delete(callback)
            >>> view2.delete() # callback removed and will not fire
        """
        if not callable(callback):
            return ValueError("remove_delete callback should be a callable function!")
        self._delete_callbacks.remove_callbacks(lambda cb: cb == callback)

    def to_arrow(self, **kwargs):
        options = _parse_format_options(self, kwargs)
        if self._is_unit_context:
            return to_arrow_unit(
                self._view,
                options["start_row"],
                options["end_row"],
                options["start_col"],
                options["end_col"],
            )
        elif self._sides == 0:
            return to_arrow_zero(
                self._view,
                options["start_row"],
                options["end_row"],
                options["start_col"],
                options["end_col"],
            )
        elif self._sides == 1:
            return to_arrow_one(
                self._view,
                options["start_row"],
                options["end_row"],
                options["start_col"],
                options["end_col"],
            )
        else:
            return to_arrow_two(
                self._view,
                options["start_row"],
                options["end_row"],
                options["start_col"],
                options["end_col"],
            )

    def to_records(self, **kwargs):
        """Serialize the :class:`~perspective.View`'s dataset into a :obj:`list`
        of :obj:`dict` containing each row.

        By default, the entire dataset is returned, though this can be windowed
        via ``kwargs``.  When ``row_pivots`` are applied, a ``__ROW_PATH__``
        column name will be generated in addition to the applied ``columns``.
        When ``column_pivots`` are applied, column names will be qualified
        with their column group name.

        Keyword Args:
            start_row (:obj:`int`): (Defaults to 0).
            end_row (:obj:`int`): (Defaults to
                :func:`perspective.View.num_rows()`).
            start_col (:obj:`int`): (Defaults to 0).
            end_col (:obj:`int`): (Defaults to
                :func:`perspective.View.num_columns()`).
            id (:obj:`bool`): Whether to return a logical row ID for each
                row (Defaults to ``False``).
            index (:obj:`bool`): Whether to return an implicit pkey for each
                row (Defaults to ``False``).
            leaves_only (:obj:`bool`): Whether to return only the data at the
                end of the tree (Defaults to ``False``).

        Returns:
            :obj:`list` of :obj:`dict`: A list of :obj:`dict`, where each dict
                represents a row of the current state of the
                :class:`~perspective.View`.
        """
        return to_format(kwargs, self, "records")

    def to_dict(self, **options):
        """Serialize the :class:`~perspective.View`'s dataset into a :obj:`dict`
        of :obj:`str` keys and :obj:`list` values.  Each key is a column name,
        and the associated value is the column's data packed into a :obj:`list`.
        If the :class:`~perspective.View` is aggregated, the aggregated dataset
        will be returned.

        Keyword Args:
            start_row (:obj:`int`): (Defaults to 0).
            end_row (:obj:`int`): (Defaults to
                :func:`perspective.View.num_rows()`).
            start_col (:obj:`int`): (Defaults to 0).
            end_col (:obj:`int`): (Defaults to
                :func:`perspective.View.num_columns()`).
            id (:obj:`bool`): Whether to return a logical row ID for each
                row (Defaults to ``False``).
            index (:obj:`bool`): Whether to return an implicit pkey for each
                row (Defaults to ``False``).
            leaves_only (:obj:`bool`): Whether to return only the data at the
                end of the tree (Defaults to ``False``).

        Returns:
            :obj:`dict`: A dictionary with string keys and list values, where
                key = column name and value = column values.
        """
        return to_format(options, self, "dict")

    def to_numpy(self, **options):
        """Serialize the view's dataset into a :obj:`dict` of :obj:`str` keys
        and :class:`numpy.array` values.  Each key is a column name, and the
        associated value is the column's data packed into a numpy array.

        Keyword Args:
            start_row (:obj:`int`): (Defaults to 0).
            end_row (:obj:`int`): (Defaults to
                :func:`perspective.View.num_rows()`).
            start_col (:obj:`int`): (Defaults to 0).
            end_col (:obj:`int`): (Defaults to
                :func:`perspective.View.num_columns()`).
            id (:obj:`bool`): Whether to return a logical row ID for each
                row (Defaults to ``False``).
            index (:obj:`bool`): Whether to return an implicit pkey for each
                row (Defaults to ``False``).
            leaves_only (:obj:`bool`): Whether to return only the data at the
                end of the tree (Defaults to ``False``).

        Returns:
            :obj:`dict` of :class:`numpy.array`: A dictionary with string keys
                and numpy array values, where key = column name and
                value = column values.
        """
        return to_format(options, self, "numpy")

    def to_df(self, **options):
        """Serialize the view's dataset into a pandas dataframe.

        If the view is aggregated, the aggregated dataset will be returned.

        Keyword Args:
            start_row (:obj:`int`): (Defaults to 0).
            end_row (:obj:`int`): (Defaults to
                :func:`perspective.View.num_rows()`).
            start_col (:obj:`int`): (Defaults to 0).
            end_col (:obj:`int`): (Defaults to
                :func:`perspective.View.num_columns()`).
            id (:obj:`bool`): Whether to return a logical row ID for each
                row (Defaults to ``False``).
            index (:obj:`bool`): Whether to return an implicit pkey for each
                row (Defaults to ``False``).
            leaves_only (:obj:`bool`): Whether to return only the data at the
                end of the tree (Defaults to ``False``).


        Returns:
            :class:`pandas.DataFrame`: A DataFrame serialization of the current
                state of this :class:`~perspective.View`.
        """
        cols = self.to_numpy(**options)
        return pandas.DataFrame(cols)

    def to_csv(self, **options):
        """Serialize the :class:`~perspective.View`'s dataset into a CSV string.

        Keyword Args:
            start_row (:obj:`int`): (Defaults to 0).
            end_row (:obj:`int`): (Defaults to
                :func:`perspective.View.num_rows()`).
            start_col (:obj:`int`): (Defaults to 0).
            end_col (:obj:`int`): (Defaults to
                :func:`perspective.View.num_columns()`).
            id (:obj:`bool`): Whether to return a logical row ID for each
                row (Defaults to ``False``).
            index (:obj:`bool`): Whether to return an implicit pkey for each
                row (Defaults to False).
            leaves_only (:obj:`bool`): Whether to return only the data at the
                end of the tree (Defaults to False).
            date_format (:obj:`str`): How ``datetime`` objects should be
                formatted in the CSV.

        Returns:
            :obj:`str`: A CSV-formatted string containing the serialized data.
        """
        date_format = None

        # Handle to_csv calls from `<perspective-viewer>`, which uses the
        # JavaScript Intl.DateTimeFormat API that takes a locale instead of a
        # string format.
        # TODO This should move to portable code.
        if options.pop("formatted", False):
            date_format = "%Y/%m/%d %H:%M:%S"

        return self.to_df(**options).to_csv(
            date_format=date_format,
            line_terminator="\r\n" if os.name == "nt" else "\n",
        )

    @wraps(to_records)
    def to_json(self, **options):
        return self.to_records(**options)

    @wraps(to_dict)
    def to_columns(self, **options):
        return self.to_dict(**options)

    def _get_row_delta(self):
        if self._is_unit_context:
            return get_row_delta_unit(self._view)
        elif self._sides == 0:
            return get_row_delta_zero(self._view)
        elif self._sides == 1:
            return get_row_delta_one(self._view)
        else:
            return get_row_delta_two(self._view)

    def _num_hidden_cols(self):
        """Returns the number of columns that are sorted but not shown."""
        hidden = 0
        columns = self._config.get_columns()
        for sort in self._config.get_sort():
            if sort[0] not in columns:
                hidden += 1
        return hidden

    def _wrapped_on_update_callback(self, **kwargs):
        """Provide the user-defined callback function with additional metadata
        from the view.
        """
        mode = kwargs["mode"]
        port_id = kwargs["port_id"]
        cache = kwargs["cache"]
        callback = kwargs["callback"]

        if cache.get(port_id) is None:
            cache[port_id] = {}

        if mode == "row":
            if cache[port_id].get("row_delta") is None:
                cache["row_delta"] = self._get_row_delta()
            callback(port_id, cache["row_delta"])
        else:
            callback(port_id)
