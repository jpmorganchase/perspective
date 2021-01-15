/******************************************************************************
 *
 * Copyright (c) 2019, the Perspective Authors.
 *
 * This file is part of the Perspective library, distributed under the terms of
 * the Apache License 2.0.  The full license can be found in the LICENSE file.
 *
 */
#ifdef PSP_ENABLE_PYTHON
#include <perspective/python/view.h>


namespace perspective {
namespace binding {

/******************************************************************************
 *
 * View API
 */

template <>
bool
is_valid_filter(t_dtype type, t_val date_parser, t_filter_op comp, t_val filter_term) {
    if (comp == t_filter_op::FILTER_OP_IS_NULL
        || comp == t_filter_op::FILTER_OP_IS_NOT_NULL) {
        return true;
    } else if (type == DTYPE_DATE || type == DTYPE_TIME) {
        if (py::isinstance<py::str>(filter_term)) {
            t_val parsed_date = date_parser.attr("parse")(filter_term);
            return !parsed_date.is_none();
        } else {
            return !filter_term.is_none();
        }
    } else {
        return !filter_term.is_none();
    }
};

template <>
std::tuple<std::string, std::string, std::vector<t_tscalar>>
make_filter_term(t_dtype column_type, t_val date_parser, const std::string& column_name, const std::string& filter_op_str, t_val filter_term) {
    t_filter_op filter_op = str_to_filter_op(filter_op_str);
    std::vector<t_tscalar> terms;

    switch (filter_op) {
        case FILTER_OP_NOT_IN:
        case FILTER_OP_IN: {
            std::vector<std::string> filter_terms
                = filter_term.cast<std::vector<std::string>>();
            for (auto term : filter_terms) {
                terms.push_back(mktscalar(get_interned_cstr(term.c_str())));
            }
        } break;
        case FILTER_OP_IS_NULL:
        case FILTER_OP_IS_NOT_NULL: {
            terms.push_back(mktscalar(0));
        } break;
        default: {
            switch (column_type) {
                case DTYPE_INT32: {
                    terms.push_back(mktscalar(filter_term.cast<std::int32_t>()));
                } break;
                case DTYPE_INT64:
                case DTYPE_FLOAT64: {
                    terms.push_back(mktscalar(filter_term.cast<double>()));
                } break;
                case DTYPE_BOOL: {
                    terms.push_back(mktscalar(filter_term.cast<bool>()));
                } break;
                case DTYPE_DATE: {
                    if (py::isinstance<py::str>(filter_term)) {
                        t_val parsed_date = date_parser.attr("parse")(filter_term);
                        auto date_components = 
                            date_parser.attr("to_date_components")(parsed_date).cast<std::map<std::string, std::int32_t>>();
                        t_date dt = t_date(date_components["year"], date_components["month"], date_components["day"]);
                        terms.push_back(mktscalar(dt));
                    } else {
                        auto date_components = 
                            date_parser.attr("to_date_components")(filter_term).cast<std::map<std::string, std::int32_t>>();
                        t_date dt = t_date(date_components["year"], date_components["month"], date_components["day"]);
                        terms.push_back(mktscalar(dt));
                    }
                } break;
                case DTYPE_TIME: {
                    if (py::isinstance<py::str>(filter_term)) {
                        t_val parsed_date = date_parser.attr("parse")(filter_term);
                        std::int64_t ts = date_parser.attr("to_timestamp")(parsed_date).cast<std::int64_t>();
                        t_tscalar timestamp = mktscalar(t_time(ts));
                        terms.push_back(timestamp);
                    } else {
                        t_tscalar timestamp = mktscalar(
                            t_time(date_parser.attr("to_timestamp")(filter_term).cast<std::int64_t>()));
                        terms.push_back(timestamp);
                    }
                } break;
                default: {
                    terms.push_back(
                        mktscalar(get_interned_cstr(filter_term.cast<std::string>().c_str())));
                }
            }
        }
    }
    return std::make_tuple(column_name, filter_op_str, terms);
}

template <>
std::shared_ptr<t_view_config>
make_view_config(std::shared_ptr<t_schema> schema, t_val date_parser, t_val config) {
    auto row_pivots = config.attr("get_row_pivots")().cast<std::vector<std::string>>();
    auto column_pivots = config.attr("get_column_pivots")().cast<std::vector<std::string>>();
    auto columns = config.attr("get_columns")().cast<std::vector<std::string>>();
    auto sort = config.attr("get_sort")().cast<std::vector<std::vector<std::string>>>();
    auto filter_op = config.attr("get_filter_op")().cast<std::string>();

    // to preserve order, do not cast to std::map - use keys and python 3.7's guarantee that dicts respect insertion order
    auto p_aggregates = py::dict(config.attr("get_aggregates")());
    tsl::ordered_map<std::string, std::vector<std::string>> aggregates;

    for (auto& column : columns) {
        py::str py_column_name = py::str(column);
        if (p_aggregates.contains(py_column_name)) {
            if (py::isinstance<py::str>(p_aggregates[py_column_name])) {
                std::vector<std::string> agg{
                    p_aggregates[py_column_name].cast<std::string>()};
                aggregates[column] = agg;
            } else {
                aggregates[column] = p_aggregates[py_column_name].cast<std::vector<std::string>>();
            }
        }
    };

    bool column_only = false;

    // make sure that primary keys are created for column-only views
    if (row_pivots.size() == 0 && column_pivots.size() > 0) {
        row_pivots.push_back("psp_okey");
        column_only = true;
    }

    // this needs to be a py_dict
    auto p_computed_columns = config.attr("get_computed_columns")().cast<std::vector<t_val>>();
    std::vector<t_computed_column_definition> computed_columns;

    for (auto c : p_computed_columns) {
        py::dict computed_column = c.cast<py::dict>();
        std::string computed_column_name = c["column"].cast<std::string>();
        t_computed_function_name computed_function_name = 
            str_to_computed_function_name(c["computed_function_name"].cast<std::string>());
        std::vector<std::string> input_columns = c["inputs"].cast<std::vector<std::string>>();

        /**
         * Mutate the schema to add computed columns - the distinction 
         * between `natural` and `computed` columns must be erased here
         * as all lookups into `schema` must be valid for all computed
         * columns on the View.
         */
        std::vector<t_dtype> input_types(input_columns.size());

        // If the input columns are invalid, an error will be thrown here.
        for (auto i = 0; i < input_columns.size(); ++i) {
            input_types[i] = schema->get_dtype(input_columns[i]);
        }

        t_computation computation = t_computed_column::get_computation(
            computed_function_name, input_types);
        
        // Throw an exception if the computation is invalid - the UI will
        // prevent users from saving invalidly-typed computations.
        if (computation.m_name == INVALID_COMPUTED_FUNCTION) {
            // Get actual input types
            auto valid_input_types = t_computed_column::get_computation_input_types(computed_function_name);
            std::stringstream ss;
            ss << "View creation failed: could not build computed column '"
               << computed_column_name
               << "' as the input column types are invalid."
               << std::endl;
            PSP_COMPLAIN_AND_ABORT(ss.str());
        }

        t_dtype output_column_type = computation.m_return_type;

        // If the computed column does not overwrite a "real" column, add it
        // to the schema, otherwise throw an exception. The viewer UI will
        // prevent users from overwriting "real" columns, so the only case
        // here is when views are created programatically, where we should
        // fail fast and report the error cleanly.
        if (!schema->has_column(computed_column_name)) {
            schema->add_column(computed_column_name, output_column_type);

            // Add the computed column to the config.
            auto tp = std::make_tuple(
                computed_column_name,
                computed_function_name,
                input_columns,
                computation);

            computed_columns.push_back(tp);
        } else {
            std::stringstream ss;
            ss << "View creation failed: cannot overwrite Table column '"
               << computed_column_name
               << "' with a computed column."
               << std::endl;
            PSP_COMPLAIN_AND_ABORT(ss.str());
        }        
    }

    // construct filters with filter terms, and fill the vector of tuples
    auto p_filter = config.attr("get_filter")().cast<std::vector<std::vector<t_val>>>();
    std::vector<std::tuple<std::string, std::string, std::vector<t_tscalar>>> filter;

    for (auto f : p_filter) {
        // parse filter details
        std::string column_name = f[0].cast<std::string>();
        std::string filter_op_str = f[1].cast<std::string>();
        t_dtype column_type = schema->get_dtype(column_name);
        t_filter_op filter_operator = str_to_filter_op(filter_op_str);

        // validate the filter before it goes into the core engine
        t_val filter_term = py::none();
        if (f.size() > 2) {
            // null/not null filters do not have a filter term
            filter_term = f[2];
        }

        if (is_valid_filter(column_type, date_parser, filter_operator, filter_term)) {
            filter.push_back(make_filter_term(column_type, date_parser, column_name, filter_op_str, filter_term));
        }
    }

    // create the `t_view_config`
    auto view_config = std::make_shared<t_view_config>(
        row_pivots,
        column_pivots,
        aggregates,
        columns,
        filter,
        sort,
        computed_columns,
        filter_op,
        column_only);

    // transform primitive values into abstractions that the engine can use
    view_config->init(schema);

    // set pivot depths if provided
    if (! config.attr("row_pivot_depth").is_none()) {
        view_config->set_row_pivot_depth(config.attr("row_pivot_depth").cast<std::int32_t>());
    }

    if (! config.attr("column_pivot_depth").is_none()) {
        view_config->set_column_pivot_depth(config.attr("column_pivot_depth").cast<std::int32_t>());
    }

    return view_config;
}

/******************************************************************************
 *
 * make_view
 */

template <typename CTX_T>
std::shared_ptr<View<CTX_T>>
make_view(std::shared_ptr<Table> table, const std::string& name, const std::string& separator,
    t_val view_config, t_val date_parser) {
    std::shared_ptr<t_schema> schema = std::make_shared<t_schema>(table->get_schema());
    std::shared_ptr<t_view_config> config = make_view_config<t_val>(schema, date_parser, view_config);
    {
        PerspectiveScopedGILRelease acquire(table->get_pool()->get_event_loop_thread_id(), table->get_pool()->get_lock(), true);
        auto ctx = make_context<CTX_T>(table, schema, config, name);
        auto view_ptr = std::make_shared<View<CTX_T>>(table, ctx, name, separator, config);
        return view_ptr;
    }
}

std::shared_ptr<View<t_ctxunit>>
make_view_unit(std::shared_ptr<Table> table, std::string name, std::string separator,
    t_val view_config, t_val date_parser) {
    return make_view<t_ctxunit>(table, name, separator, view_config, date_parser);
}

std::shared_ptr<View<t_ctx0>>
make_view_ctx0(std::shared_ptr<Table> table, std::string name, std::string separator,
    t_val view_config, t_val date_parser) {
    return make_view<t_ctx0>(table, name, separator, view_config, date_parser);
}

std::shared_ptr<View<t_ctx1>>
make_view_ctx1(std::shared_ptr<Table> table, std::string name, std::string separator,
    t_val view_config, t_val date_parser) {
    return make_view<t_ctx1>(table, name, separator, view_config, date_parser);
}

std::shared_ptr<View<t_ctx2>>
make_view_ctx2(std::shared_ptr<Table> table, std::string name, std::string separator,
    t_val view_config, t_val date_parser) {
    return make_view<t_ctx2>(table, name, separator, view_config, date_parser);
}

/******************************************************************************
 *
 * to_arrow
 */

py::bytes
to_arrow_unit(
    std::shared_ptr<View<t_ctxunit>> view,
    std::int32_t start_row,
    std::int32_t end_row,
    std::int32_t start_col,
    std::int32_t end_col
) {
    PerspectiveScopedGILRelease acquire(view->get_event_loop_thread_id(), view->get_lock(), true);
    std::shared_ptr<std::string> str = 
        view->to_arrow(start_row, end_row, start_col, end_col);
    return py::bytes(*str);
}

py::bytes
to_arrow_zero(
    std::shared_ptr<View<t_ctx0>> view,
    std::int32_t start_row,
    std::int32_t end_row,
    std::int32_t start_col,
    std::int32_t end_col
) {
    PerspectiveScopedGILRelease acquire(view->get_event_loop_thread_id(), view->get_lock(), true);
    std::shared_ptr<std::string> str = 
        view->to_arrow(start_row, end_row, start_col, end_col);
    return py::bytes(*str);
}

py::bytes
to_arrow_one(
    std::shared_ptr<View<t_ctx1>> view,
    std::int32_t start_row,
    std::int32_t end_row,
    std::int32_t start_col, 
    std::int32_t end_col
) {
    PerspectiveScopedGILRelease acquire(view->get_event_loop_thread_id(), view->get_lock(), true);
    std::shared_ptr<std::string> str = 
        view->to_arrow(start_row, end_row, start_col, end_col);
    return py::bytes(*str);
}

py::bytes
to_arrow_two(
    std::shared_ptr<View<t_ctx2>> view,
    std::int32_t start_row,
    std::int32_t end_row,
    std::int32_t start_col, 
    std::int32_t end_col
) {
    PerspectiveScopedGILRelease acquire(view->get_event_loop_thread_id(), view->get_lock(), true);
    std::shared_ptr<std::string> str = 
        view->to_arrow(start_row, end_row, start_col, end_col);
    return py::bytes(*str);
}

/******************************************************************************
 *
 * get_row_delta
 */

py::bytes
get_row_delta_unit(std::shared_ptr<View<t_ctxunit>> view) {
    PerspectiveScopedGILRelease acquire(view->get_event_loop_thread_id(), view->get_lock(), true);
    std::shared_ptr<t_data_slice<t_ctxunit>> slice = view->get_row_delta();
    std::shared_ptr<std::string> arrow = view->data_slice_to_arrow(slice);
    return py::bytes(*arrow);
}

py::bytes
get_row_delta_zero(std::shared_ptr<View<t_ctx0>> view) {
    PerspectiveScopedGILRelease acquire(view->get_event_loop_thread_id(), view->get_lock(), true);
    std::shared_ptr<t_data_slice<t_ctx0>> slice = view->get_row_delta();
    std::shared_ptr<std::string> arrow = view->data_slice_to_arrow(slice);
    return py::bytes(*arrow);
}

py::bytes
get_row_delta_one(std::shared_ptr<View<t_ctx1>> view) {
    PerspectiveScopedGILRelease acquire(view->get_event_loop_thread_id(), view->get_lock(), true);
    std::shared_ptr<t_data_slice<t_ctx1>> slice = view->get_row_delta();
    std::shared_ptr<std::string> arrow = view->data_slice_to_arrow(slice);
    return py::bytes(*arrow);
}

py::bytes
get_row_delta_two(
    std::shared_ptr<View<t_ctx2>> view) {
    PerspectiveScopedGILRelease acquire(view->get_event_loop_thread_id(), view->get_lock(), true);
    std::shared_ptr<t_data_slice<t_ctx2>> slice = view->get_row_delta();
    std::shared_ptr<std::string> arrow = view->data_slice_to_arrow(slice);
    return py::bytes(*arrow);
}

} //namespace binding
} //namespace perspective

#endif