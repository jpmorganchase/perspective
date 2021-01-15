/******************************************************************************
 *
 * Copyright (c) 2019, the Perspective Authors.
 *
 * This file is part of the Perspective library, distributed under the terms of
 * the Apache License 2.0.  The full license can be found in the LICENSE file.
 *
 */

#include <perspective/first.h>
#include <perspective/view.h>
#include <perspective/arrow_writer.h>
#include <sstream>


namespace perspective {

std::string
join_column_names(
    const std::vector<t_tscalar>& names, const std::string& separator) {
    if (names.size() == 0) {
        return "";
    } else if (names.size() == 1) {
        return names.at(0).to_string();
    } else {
        std::ostringstream ss;
        for (auto i = 0; i < names.size() - 1; ++i) {
            std::string str = names.at(i).to_string();
            ss << str;
            ss << separator;
        }
        ss << names.at(names.size() - 1).to_string();
        return ss.str();
    }
}

template <typename CTX_T>
View<CTX_T>::View(
        std::shared_ptr<Table> table,
        std::shared_ptr<CTX_T> ctx,
        const std::string& name,
        const std::string& separator,
        std::shared_ptr<t_view_config> view_config)
    : m_table(table)
    , m_ctx(ctx)
    , m_name(name)
    , m_separator(separator)
    , m_view_config(view_config) {
    m_row_pivots = m_view_config->get_row_pivots();
    m_column_pivots = m_view_config->get_column_pivots();
    m_aggregates = m_view_config->get_aggspecs();
    m_columns = m_view_config->get_columns();
    m_filter = m_view_config->get_fterm();
    m_sort = m_view_config->get_sortspec();
    m_computed_columns = m_view_config->get_computed_columns();

    // Add hidden columns used in sorts to the `m_hidden_sort` vector.
    if (m_sort.size() > 0) {
        _find_hidden_sort(m_sort);
    }

    if (m_column_pivots.size() > 0) {
        auto column_sort = m_view_config->get_col_sortspec();
        _find_hidden_sort(column_sort);
    }

    // configure data window for `get_data` and `row_delta`
    is_column_only() ? m_row_offset = 1 : m_row_offset = 0;

    // TODO: make sure is 0 for column only - right now get_data returns row path for everything
    sides() > 0 ? m_col_offset = 1 : m_col_offset = 0;

    // TODO: add index shifting ability
}

template <typename CTX_T>
View<CTX_T>::~View() {
    auto pool = m_table->get_pool();
    auto gnode = m_table->get_gnode();
    // TODO: need to invalidate memory used by previous computed columns
    // without affecting views that depend on those computed columns.
    pool->unregister_context(gnode->get_id(), m_name);
}

template <typename CTX_T>
std::shared_ptr<t_view_config>
View<CTX_T>::get_view_config() const {
    return m_view_config;
}

template <>
std::int32_t
View<t_ctxunit>::sides() const {
    return 0;
}

template <>
std::int32_t
View<t_ctx0>::sides() const {
    return 0;
}

template <>
std::int32_t
View<t_ctx1>::sides() const {
    return 1;
}

template <>
std::int32_t
View<t_ctx2>::sides() const {
    return 2;
}

template <typename CTX_T>
std::int32_t
View<CTX_T>::num_rows() const {
    if (is_column_only()) {
        return m_ctx->get_row_count() - 1;
    } else {
        return m_ctx->get_row_count();
    }
}

template <typename CTX_T>
std::int32_t
View<CTX_T>::num_columns() const {
    return m_ctx->unity_get_column_count();
}

/**
 * @brief Return correct number of columns when headers need to be skipped.
 *
 * @tparam
 * @return std::int32_t
 */
template <>
std::int32_t
View<t_ctx2>::num_columns() const {
    if (m_sort.size() > 0) {
        auto depth = m_column_pivots.size();
        auto col_length = m_ctx->unity_get_column_count();
        auto count = 0;
        for (t_uindex i = 0; i < col_length; ++i) {
            if (m_ctx->unity_get_column_path(i + 1).size() == depth) {
                count++;
            }
        }
        return count;
    } else {
        return m_ctx->unity_get_column_count();
    }
}

// Metadata construction
template <typename CTX_T>
std::vector<std::vector<t_tscalar>>
View<CTX_T>::column_names(bool skip, std::int32_t depth) const {
    std::vector<std::vector<t_tscalar>> names;
    const std::vector<t_aggspec> aggs = m_ctx->get_aggregates();
    std::vector<std::string> aggregate_names(aggs.size());

    for (auto i = 0; i < aggs.size(); ++i) {
        aggregate_names[i] = aggs[i].name();
    }

    for (t_uindex key = 0, max = m_ctx->unity_get_column_count(); key != max; ++key) {
        std::string name = aggregate_names[key % aggregate_names.size()];

        if (name == "psp_okey") {
            continue;
        }

        std::vector<t_tscalar> col_path = m_ctx->unity_get_column_path(key + 1);
        if (skip && col_path.size() < static_cast<unsigned int>(depth)) {
            continue;
        }

        std::vector<t_tscalar> new_path;
        for (auto path = col_path.rbegin(); path != col_path.rend(); ++path) {
            new_path.push_back(*path);
        }
        new_path.push_back(m_ctx->get_aggregate_name(key % aggregate_names.size()));
        names.push_back(new_path);
    }

    return names;
}

template <>
std::vector<std::vector<t_tscalar>>
View<t_ctx0>::column_names(bool skip, std::int32_t depth) const {
    std::vector<std::vector<t_tscalar>> names;

    for (t_uindex key = 0, max = m_ctx->unity_get_column_count(); key != max; ++key) {
        t_tscalar name = m_ctx->get_column_name(key);
        if (name.to_string() == "psp_okey") {
            continue;
        };
        std::vector<t_tscalar> col_path;
        col_path.push_back(name);
        names.push_back(col_path);
    }

    return names;
}

template <>
std::vector<std::vector<t_tscalar>>
View<t_ctxunit>::column_names(bool skip, std::int32_t depth) const {
    std::vector<std::vector<t_tscalar>> names;

    for (t_uindex key = 0, max = m_ctx->unity_get_column_count(); key != max; ++key) {
        t_tscalar name = m_ctx->get_column_name(key);
        if (name.to_string() == "psp_okey") {
            continue;
        };
        std::vector<t_tscalar> col_path;
        col_path.push_back(name);
        names.push_back(col_path);
    }

    return names;
}

template <typename CTX_T>
std::vector<std::vector<t_tscalar>>
View<CTX_T>::column_paths() const {
    auto num_column_pivots = m_column_pivots.size();
    auto names = column_names(true, num_column_pivots);

    if (sides() > 0 && !is_column_only()) {
        // prepend `__ROW_PATH__` to the output vector
        t_tscalar row_path;
        row_path.set("__ROW_PATH__");
        names.insert(names.begin(), std::vector<t_tscalar>{row_path});
    }

    if (m_hidden_sort.size() > 0) {
        // make a new vector so we don't have to erase while iterating
        std::vector<std::vector<t_tscalar>> visible_column_paths;

        for (const auto& column : names) {
            // Remove undisplayed column names used to sort
            std::string name = column.back().to_string();
            if (std::find(m_hidden_sort.begin(), m_hidden_sort.end(), name) == m_hidden_sort.end()) {
                visible_column_paths.push_back(column);
            }
        }

        return visible_column_paths;
        
    }

    return names;
}

template <typename CTX_T>
std::map<std::string, std::string>
View<CTX_T>::schema() const {
    // TODO: should revert to m_table
    auto schema = m_ctx->get_schema();
    auto _types = schema.types();
    auto names = schema.columns();

    std::map<std::string, t_dtype> types;
    std::map<std::string, std::string> new_schema;

    for (std::size_t i = 0, max = names.size(); i != max; ++i) {
        types[names[i]] = _types[i];
    }

    auto col_names = column_names(false);
    for (const std::vector<t_tscalar>& name : col_names) {
        // Pull out the main aggregate column
        std::string agg_name = name.back().to_string();
        std::string type_string = dtype_to_str(types[agg_name]);
        new_schema[agg_name] = type_string;

        if (m_row_pivots.size() > 0 && !is_column_only()) {
            new_schema[agg_name] = _map_aggregate_types(agg_name, new_schema[agg_name]);
        }
    }

    return new_schema;
}

template <>
std::map<std::string, std::string>
View<t_ctxunit>::schema() const {
    t_schema schema = m_ctx->get_schema();
    std::vector<t_dtype> _types = schema.types();
    std::vector<std::string> names = schema.columns();

    std::map<std::string, t_dtype> types;
    for (std::size_t i = 0, max = names.size(); i != max; ++i) {
        types[names[i]] = _types[i];
    }

    std::vector<std::vector<t_tscalar>> cols = column_names(false);
    std::map<std::string, std::string> new_schema;

    for (std::size_t i = 0, max = cols.size(); i != max; ++i) {
        std::string name = cols[i].back().to_string();
        if (name == "psp_okey") {
            continue;
        }
        new_schema[name] = dtype_to_str(types[name]);
    }

    return new_schema;
}

template <>
std::map<std::string, std::string>
View<t_ctx0>::schema() const {
    t_schema schema = m_ctx->get_schema();
    std::vector<t_dtype> _types = schema.types();
    std::vector<std::string> names = schema.columns();

    std::map<std::string, t_dtype> types;
    for (std::size_t i = 0, max = names.size(); i != max; ++i) {
        types[names[i]] = _types[i];
    }

    std::vector<std::vector<t_tscalar>> cols = column_names(false);
    std::map<std::string, std::string> new_schema;

    for (std::size_t i = 0, max = cols.size(); i != max; ++i) {
        std::string name = cols[i].back().to_string();
        if (name == "psp_okey") {
            continue;
        }
        new_schema[name] = dtype_to_str(types[name]);
    }

    return new_schema;
}

template <typename CTX_T>
std::map<std::string, std::string>
View<CTX_T>::computed_schema() const {
    auto schema = m_ctx->get_schema();
    auto _types = schema.types();
    auto names = schema.columns();

    std::map<std::string, t_dtype> types;
    std::map<std::string, std::string> new_schema;

    for (std::size_t i = 0, max = names.size(); i != max; ++i) {
        types[names[i]] = _types[i];
    }

    for (const auto& c : m_computed_columns) {
        std::string name = std::get<0>(c);
        new_schema[name] = dtype_to_str(types[name]);

        if (m_row_pivots.size() > 0 && !is_column_only()) {
            new_schema[name] = _map_aggregate_types(name, new_schema[name]);
        }
    }

    return new_schema;
}

template <>
std::map<std::string, std::string>
View<t_ctxunit>::computed_schema() const {
    return {};
}

template <>
std::map<std::string, std::string>
View<t_ctx0>::computed_schema() const {
    t_schema schema = m_ctx->get_schema();
    std::vector<t_dtype> _types = schema.types();
    std::vector<std::string> names = schema.columns();

    std::map<std::string, t_dtype> types;
    for (std::size_t i = 0, max = names.size(); i != max; ++i) {
        types[names[i]] = _types[i];
    }

    std::map<std::string, std::string> new_schema;
    for (const auto& c : m_computed_columns) {
        std::string name = std::get<0>(c);
        new_schema[name] = dtype_to_str(types[name]);
    }

    return new_schema;
}

template <>
std::shared_ptr<t_data_slice<t_ctxunit>>
View<t_ctxunit>::get_data(
    t_uindex start_row, t_uindex end_row, t_uindex start_col, t_uindex end_col) const {
    std::vector<t_tscalar> slice = m_ctx->get_data(start_row, end_row, start_col, end_col);
    auto col_names = column_names();
    auto data_slice_ptr = std::make_shared<t_data_slice<t_ctxunit>>(m_ctx, start_row, end_row,
        start_col, end_col, m_row_offset, m_col_offset, slice, col_names);
    return data_slice_ptr;
}

template <>
std::shared_ptr<t_data_slice<t_ctx0>>
View<t_ctx0>::get_data(
    t_uindex start_row, t_uindex end_row, t_uindex start_col, t_uindex end_col) const {
    std::vector<t_tscalar> slice = m_ctx->get_data(start_row, end_row, start_col, end_col);
    auto col_names = column_names();
    auto data_slice_ptr = std::make_shared<t_data_slice<t_ctx0>>(m_ctx, start_row, end_row,
        start_col, end_col, m_row_offset, m_col_offset, slice, col_names);
    return data_slice_ptr;
}

template <>
std::shared_ptr<t_data_slice<t_ctx1>>
View<t_ctx1>::get_data(
    t_uindex start_row, t_uindex end_row, t_uindex start_col, t_uindex end_col) const {
    std::vector<t_tscalar> slice = m_ctx->get_data(start_row, end_row, start_col, end_col);
    auto col_names = column_names();
    t_tscalar row_path;
    row_path.set("__ROW_PATH__");
    col_names.insert(col_names.begin(), std::vector<t_tscalar>{row_path});
    auto data_slice_ptr = std::make_shared<t_data_slice<t_ctx1>>(m_ctx, start_row, end_row,
        start_col, end_col, m_row_offset, m_col_offset, slice, col_names);
    return data_slice_ptr;
}

template <>
std::shared_ptr<t_data_slice<t_ctx2>>
View<t_ctx2>::get_data(
    t_uindex start_row, t_uindex end_row, t_uindex start_col, t_uindex end_col) const {
    std::vector<t_tscalar> slice;
    std::vector<t_uindex> column_indices;
    std::vector<std::vector<t_tscalar>> cols;
    bool is_sorted = m_sort.size() > 0;

    if (is_column_only()) {
        start_row += m_row_offset;
        end_row += m_row_offset;
    }

    if (is_sorted) {
        /**
         * Perspective generates headers for sorted columns, so we have to
         * skip them in the underlying slice.
         */
        t_uindex start_col_index = start_col;
        t_uindex end_col_index = end_col;

        // Only construct column_indices if start_col > end_col - get_data will
        // handle the incorrect data window properly, which is consistent
        // with the implementation for when the context is not sorted.
        if (start_col < end_col) {
            auto depth = m_column_pivots.size();
            auto col_length = m_ctx->unity_get_column_count();
            column_indices.push_back(0);
            for (t_uindex i = 0; i < col_length; ++i) {
                if (m_ctx->unity_get_column_path(i + 1).size() == depth) {
                    column_indices.push_back(i + 1);
                }
            }

            cols = column_names(true, depth);

            // Filter down column indices by user-provided start/end columns
            column_indices = std::vector<t_uindex>(
                column_indices.begin() + start_col,
                column_indices.begin() + std::min(end_col, (t_uindex)column_indices.size())
            );

            // If start_col == end_col, then column_indices will be an empty
            // vector. Only try to access the first and last elements if the
            // vector is not empty. `get_data` correctly handles cases where
            // start == end and start > end.
            if (column_indices.size() > 0) {
                start_col_index = column_indices.front();
                end_col_index = column_indices.back() + 1;
            }
        }

        std::vector<t_tscalar> slice_with_headers = m_ctx->get_data(
            start_row, end_row, start_col_index, end_col_index);

        auto iter = slice_with_headers.begin();
        while (iter != slice_with_headers.end()) {
            t_uindex prev = column_indices.front();
            for (auto idx = column_indices.begin(); idx != column_indices.end(); idx++) {
                t_uindex col_num = *idx;
                iter += col_num - prev;
                prev = col_num;
                slice.push_back(*iter);
            }
            if (iter != slice_with_headers.end())
                iter++;
        }
    } else {
        cols = column_names();
        slice = m_ctx->get_data(start_row, end_row, start_col, end_col);
    }
    // TODO: we need to just use column_paths everywhere instead of row path insertion manually,
    // this causes issues with needing to skip row paths
    t_tscalar row_path;
    row_path.set("__ROW_PATH__");
    cols.insert(cols.begin(), std::vector<t_tscalar>{row_path});
    auto data_slice_ptr = std::make_shared<t_data_slice<t_ctx2>>(m_ctx, start_row, end_row,
        start_col, end_col, m_row_offset, m_col_offset, slice, cols, column_indices);
    return data_slice_ptr;
}

template <typename CTX_T>
std::shared_ptr<std::string>
View<CTX_T>::to_arrow(std::int32_t start_row, std::int32_t end_row,
    std::int32_t start_col, std::int32_t end_col) const {
    std::shared_ptr<t_data_slice<CTX_T>> data_slice = get_data(
        start_row, end_row, start_col, end_col
    );
    return data_slice_to_arrow(data_slice);
};

template <typename CTX_T>
std::shared_ptr<std::string>
View<CTX_T>::data_slice_to_arrow(
    std::shared_ptr<t_data_slice<CTX_T>> data_slice) const {
    // From the data slice, get all the metadata we need
    t_get_data_extents extents = data_slice->get_data_extents();
    std::int32_t start_col = extents.m_scol;
    std::int32_t end_col = extents.m_ecol;
    std::int32_t col_offset = data_slice->get_col_offset();
    start_col += col_offset;

    auto slice = data_slice->get_slice();
    auto stride = data_slice->get_stride();
    auto names = data_slice->get_column_names();

    std::vector<std::shared_ptr<arrow::Array>> vectors;
    std::vector<std::shared_ptr<arrow::Field>> fields;

    std::int32_t num_columns = end_col - start_col;

    if (num_columns > 0) {
        fields.reserve(num_columns);
        vectors.reserve(num_columns);
    }

    for (auto cidx = start_col; cidx < end_col; ++cidx) {
        std::vector<t_tscalar> col_path = names.at(cidx);
        t_dtype dtype = get_column_dtype(cidx);
        std::string name;

        if (sides() > 1) {
            name = join_column_names(col_path, m_separator);
        } else {
            name = col_path.at(col_path.size() - 1).to_string();
        }

        std::shared_ptr<arrow::Array> arr;
        switch (dtype) {
            case DTYPE_INT8: {
                fields.push_back(arrow::field(name, arrow::int8()));
                arr = apachearrow::numeric_col_to_array<arrow::Int8Type, std::int8_t>(slice, cidx, stride, extents);
            } break;
            case DTYPE_UINT8: {
                fields.push_back(arrow::field(name, arrow::uint8()));
                arr = apachearrow::numeric_col_to_array<arrow::UInt8Type, std::uint8_t>(slice, cidx, stride, extents);
            } break;
            case DTYPE_INT16: {
                fields.push_back(arrow::field(name, arrow::int16()));
                arr = apachearrow::numeric_col_to_array<arrow::Int16Type, std::int16_t>(slice, cidx, stride, extents);
            } break;
            case DTYPE_UINT16: {
                fields.push_back(arrow::field(name, arrow::uint16()));
                arr = apachearrow::numeric_col_to_array<arrow::UInt16Type, std::uint16_t>(slice, cidx, stride, extents);
            } break;
            case DTYPE_INT32: {
                fields.push_back(arrow::field(name, arrow::int32()));
                arr = apachearrow::numeric_col_to_array<arrow::Int32Type, std::int32_t>(slice, cidx, stride, extents);
            } break;
            case DTYPE_UINT32: {
                fields.push_back(arrow::field(name, arrow::uint32()));
                arr = apachearrow::numeric_col_to_array<arrow::UInt32Type, std::uint32_t>(slice, cidx, stride, extents);
            } break;
            case DTYPE_INT64: {
                fields.push_back(arrow::field(name, arrow::int64()));
                arr = apachearrow::numeric_col_to_array<arrow::Int64Type, std::int64_t>(slice, cidx, stride, extents);
            } break;
            case DTYPE_UINT64: {
                fields.push_back(arrow::field(name, arrow::uint64()));
                arr = apachearrow::numeric_col_to_array<arrow::UInt64Type, std::uint64_t>(slice, cidx, stride, extents);
            } break;
            case DTYPE_FLOAT32: {
                fields.push_back(arrow::field(name, arrow::float32()));
                arr = apachearrow::numeric_col_to_array<arrow::FloatType, float>(slice, cidx, stride, extents);
            } break;
            case DTYPE_FLOAT64: {
                fields.push_back(arrow::field(name, arrow::float64()));
                arr = apachearrow::numeric_col_to_array<arrow::DoubleType, double>(slice, cidx, stride, extents);
            } break;
            case DTYPE_DATE: {
                fields.push_back(arrow::field(name, arrow::date32()));
                arr = apachearrow::date_col_to_array(slice, cidx, stride, extents);
            } break;
            case DTYPE_TIME: {
                fields.push_back(arrow::field(name, arrow::timestamp(arrow::TimeUnit::MILLI)));
                arr = apachearrow::timestamp_col_to_array(slice, cidx, stride, extents);
            } break;
            case DTYPE_BOOL: {
                fields.push_back(arrow::field(name, arrow::boolean()));
                arr = apachearrow::boolean_col_to_array(slice, cidx, stride, extents);
            } break;
            case DTYPE_STR: {
                fields.push_back(arrow::field(name, arrow::dictionary(arrow::int32(), arrow::utf8())));
                arr = apachearrow::string_col_to_dictionary_array(slice, cidx, stride, extents);
            } break;
            case DTYPE_OBJECT: {
                fields.push_back(arrow::field(name, arrow::uint64()));
                arr = apachearrow::numeric_col_to_array<arrow::UInt64Type, std::uint64_t>(slice, cidx, stride, extents);
            } break;
            default: {
                std::stringstream ss;
                ss << "Cannot serialize column `" 
                   << name << "` of type `"
                   << get_dtype_descr(dtype)
                   << "` to Arrow format." << std::endl;
                PSP_COMPLAIN_AND_ABORT(ss.str());
            }
        }
        vectors.push_back(arr);
    }

    auto arrow_schema = arrow::schema(fields);
    auto num_rows = data_slice->num_rows();
    std::shared_ptr<arrow::RecordBatch> batches = 
        arrow::RecordBatch::Make(arrow_schema, num_rows, vectors);
    auto valid = batches->Validate();
    if (!valid.ok()) {
        std::stringstream ss;
        ss << "Invalid RecordBatch: " << valid.message() << std::endl;
        PSP_COMPLAIN_AND_ABORT(ss.str());
    }

    std::shared_ptr<arrow::ResizableBuffer> buffer;

#if ARROW_VERSION_MAJOR < 1
    auto allocated = arrow::AllocateResizableBuffer(0, &buffer);
    if (!allocated.ok()) {
        std::stringstream ss;
        ss << "Failed to allocate buffer: " << allocated.message() << std::endl;
        PSP_COMPLAIN_AND_ABORT(ss.str());
    }

    arrow::io::BufferOutputStream sink(buffer);        
    auto options = arrow::ipc::IpcOptions::Defaults();
    auto res = arrow::ipc::RecordBatchStreamWriter::Open(&sink, arrow_schema, options);
#else
    arrow::Result<std::shared_ptr<arrow::ResizableBuffer>> allocated = arrow::AllocateResizableBuffer(0);
    if (!allocated.ok()) {
        std::stringstream ss;
        ss << "Failed to allocate buffer: " << allocated.status().message() << std::endl;
        PSP_COMPLAIN_AND_ABORT(ss.str());
    }
   
    buffer = *allocated;    
    arrow::io::BufferOutputStream sink(buffer);    
    auto options = arrow::ipc::IpcWriteOptions::Defaults();
    auto res = arrow::ipc::NewStreamWriter(&sink, arrow_schema, options);
#endif

    std::shared_ptr<arrow::ipc::RecordBatchWriter> writer = *res;
    PSP_CHECK_ARROW_STATUS(writer->WriteRecordBatch(*batches));
    PSP_CHECK_ARROW_STATUS(writer->Close());
    return std::make_shared<std::string>(buffer->ToString());
}

// Delta calculation
template <typename CTX_T>
bool
View<CTX_T>::_get_deltas_enabled() const {
    return m_ctx->get_deltas_enabled();
}

template <typename CTX_T>
void
View<CTX_T>::_set_deltas_enabled(bool enabled_state) {
    m_ctx->set_deltas_enabled(enabled_state);
}

// Pivot table operations
template <typename CTX_T>
bool
View<CTX_T>::get_row_expanded(std::int32_t ridx) const {
    return m_ctx->unity_get_row_expanded(ridx);
}

template <>
t_index
View<t_ctxunit>::expand(std::int32_t ridx, std::int32_t row_pivot_length) {
    return ridx;
}

template <>
t_index
View<t_ctx0>::expand(std::int32_t ridx, std::int32_t row_pivot_length) {
    return ridx;
}

template <>
t_index
View<t_ctx1>::expand(std::int32_t ridx, std::int32_t row_pivot_length) {
    return m_ctx->open(ridx);
}

template <>
t_index
View<t_ctx2>::expand(std::int32_t ridx, std::int32_t row_pivot_length) {
    if (m_ctx->unity_get_row_depth(ridx) < t_uindex(row_pivot_length)) {
        return m_ctx->open(t_header::HEADER_ROW, ridx);
    } else {
        return ridx;
    }
}

template <>
t_index
View<t_ctxunit>::collapse(std::int32_t ridx) {
    return ridx;
}

template <>
t_index
View<t_ctx0>::collapse(std::int32_t ridx) {
    return ridx;
}

template <>
t_index
View<t_ctx1>::collapse(std::int32_t ridx) {
    return m_ctx->close(ridx);
}

template <>
t_index
View<t_ctx2>::collapse(std::int32_t ridx) {
    return m_ctx->close(t_header::HEADER_ROW, ridx);
}

template <>
void
View<t_ctxunit>::set_depth(std::int32_t depth, std::int32_t row_pivot_length) {}

template <>
void
View<t_ctx0>::set_depth(std::int32_t depth, std::int32_t row_pivot_length) {}

template <>
void
View<t_ctx1>::set_depth(std::int32_t depth, std::int32_t row_pivot_length) {
    if (row_pivot_length >= depth) {
        m_ctx->set_depth(depth);
    } else {
        std::cout << "Cannot expand past " << std::to_string(row_pivot_length) << std::endl;
    }
}

template <>
void
View<t_ctx2>::set_depth(std::int32_t depth, std::int32_t row_pivot_length) {
    if (row_pivot_length >= depth) {
        m_ctx->set_depth(t_header::HEADER_ROW, depth);
    } else {
        std::cout << "Cannot expand past " << std::to_string(row_pivot_length) << std::endl;
    }
}

// Getters
template <typename CTX_T>
std::shared_ptr<CTX_T>
View<CTX_T>::get_context() const {
    return m_ctx;
}

template <typename CTX_T>
std::vector<std::string>
View<CTX_T>::get_row_pivots() const {
    return m_row_pivots;
}

template <typename CTX_T>
std::vector<std::string>
View<CTX_T>::get_column_pivots() const {
    return m_column_pivots;
}

template <typename CTX_T>
std::vector<t_aggspec>
View<CTX_T>::get_aggregates() const {
    return m_aggregates;
}

template <typename CTX_T>
std::vector<t_fterm>
View<CTX_T>::get_filter() const {
    return m_filter;
}

template <typename CTX_T>
std::vector<t_sortspec>
View<CTX_T>::get_sort() const {
    return m_sort;
}

template <typename CTX_T>
std::vector<t_computed_column_definition>
View<CTX_T>::get_computed_columns() const {
    return m_computed_columns;
}

template <>
std::vector<t_tscalar>
View<t_ctx0>::get_row_path(t_uindex idx) const {
    return std::vector<t_tscalar>();
}

template <>
std::vector<t_tscalar>
View<t_ctxunit>::get_row_path(t_uindex idx) const {
    return std::vector<t_tscalar>();
}

template <typename CTX_T>
std::vector<t_tscalar>
View<CTX_T>::get_row_path(t_uindex idx) const {
    return m_ctx->unity_get_row_path(idx);
}

template <typename CTX_T>
t_stepdelta
View<CTX_T>::get_step_delta(t_index bidx, t_index eidx) const {
    return m_ctx->get_step_delta(bidx, eidx);
}

template <>
t_stepdelta
View<t_ctxunit>::get_step_delta(t_index bidx, t_index eidx) const {
    return t_stepdelta();
}

template <typename CTX_T>
std::shared_ptr<t_data_slice<CTX_T>>
View<CTX_T>::get_row_delta() const {
    t_rowdelta delta = m_ctx->get_row_delta();
    const std::vector<t_tscalar>& data = delta.data;
    t_uindex num_rows_changed = delta.num_rows_changed;
    
    auto paths = column_paths();
    if (is_column_only()) {
        // Hacky way to get column only slices working in `to_arrow`, which
        // expects to skip the first column.
        t_tscalar row_path;
        row_path.set("__ROW_PATH__");
        paths.insert(paths.begin(), std::vector<t_tscalar>{row_path});
    }

    // Column count for row delta needs to include `__ROW_PATH__`
    t_uindex num_columns = m_ctx->get_column_count();

    return std::make_shared<t_data_slice<CTX_T>>(
        m_ctx, 0, num_rows_changed, 0, num_columns,
        m_row_offset, m_col_offset, data, paths);
}

template <typename CTX_T>
t_dtype
View<CTX_T>::get_column_dtype(t_uindex idx) const {
    return m_ctx->get_column_dtype(idx);
}

template <typename CTX_T>
bool
View<CTX_T>::is_column_only() const {
    return m_view_config->is_column_only();
}

#ifdef PSP_ENABLE_PYTHON
template <typename CTX_T>
std::thread::id 
View<CTX_T>::get_event_loop_thread_id() const {
    return m_table->get_pool()->get_event_loop_thread_id();
};

template <typename CTX_T>
boost::shared_mutex*
View<CTX_T>::get_lock() const {
    return m_table->get_pool()->get_lock();
}
#endif

/******************************************************************************
 *
 * Private
 */

template <typename CTX_T>
std::string
View<CTX_T>::_map_aggregate_types(
    const std::string& name, const std::string& typestring) const {

    for (const t_aggspec& agg : m_aggregates) {
        if (agg.name() == name) {
            switch (agg.agg()) {
                case AGGTYPE_DISTINCT_COUNT:
                case AGGTYPE_COUNT: {
                    return "integer";
                } break;
                case AGGTYPE_MEAN:
                case AGGTYPE_MEAN_BY_COUNT:
                case AGGTYPE_WEIGHTED_MEAN:
                case AGGTYPE_PCT_SUM_PARENT:
                case AGGTYPE_PCT_SUM_GRAND_TOTAL: {
                    return "float";
                } break;
                default: { return typestring; } break;
            }
        }
    }

    return typestring;
}

template <typename CTX_T>
void
View<CTX_T>::_find_hidden_sort(const std::vector<t_sortspec>& sort) {
    for (const t_sortspec& s : sort) {
        bool hidden = std::find(m_columns.begin(), m_columns.end(), s.m_colname) == m_columns.end();
        if (hidden) {
            // Store the actual column, not the composite column path
            m_hidden_sort.push_back(s.m_colname);
        }
    }
}

// Explicitly instantiate View for each context
template class View<t_ctxunit>;
template class View<t_ctx0>;
template class View<t_ctx1>;
template class View<t_ctx2>;
} // end namespace perspective