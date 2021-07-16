/******************************************************************************
 *
 * Copyright (c) 2017, the Perspective Authors.
 *
 * This file is part of the Perspective library, distributed under the terms of
 * the Apache License 2.0.  The full license can be found in the LICENSE file.
 *
 */

#pragma once
#include <perspective/first.h>
#include <perspective/base.h>
#include <perspective/date.h>
#include <perspective/exports.h>
#include <perspective/scalar.h>
#include <perspective/data_table.h>
#include <perspective/get_data_extents.h>
#include <perspective/last.h>

#include <arrow/api.h>
#include <arrow/util/decimal.h>
#include <arrow/io/memory.h>
#include <arrow/ipc/reader.h>
#include <arrow/ipc/writer.h>

#include <chrono>
#include <date/date.h>

namespace perspective {
namespace apachearrow {

    /**
     * @brief Return a value from a `t_scalar` cast to `T`.
     * 
     * @tparam DataType
     * @tparam ArrowDataType
     * @tparam ArrowValueType
     * @param t 
     * @return ArrowValueType 
     */
    template <typename T>
    T get_scalar(t_tscalar& t);

    /**
     * @brief Retrieve the correct index into the data slice for the given
     * column and row. This is a redefinition of the method in `t_data_slice`,
     * except it does not rely on any instance variables as we don't use the
     * `t_data_slice` instance, instead accessing the its data vector directly.
     * 
     * @param cidx 
     * @param ridx 
     * @param stride 
     * @param extents 
     * @return std::int32_t 
     */
    std::int32_t
    get_idx(std::int32_t cidx,
            std::int32_t ridx, 
            std::int32_t stride,
            t_get_data_extents extents);

    /**
     * @brief Build an `arrow::Array` from a column typed as `DTYPE_BOOL.`
     * 
     * @param data a vector of scalars to write into an arrow array.
     * @param cidx the index of the column in the dataset, used to calculate
     *      the physical offset of the column's data in `data`, a tightly packed
     *      vector containing the dataset for the entire data slice.
     * @param stride the number of columns present in the data slice, used to
     *      calculate the data's physical offset.
     * @param extents the start/end rows and columns of the data slice, used
     *      to calculate the data's physical offset.
     */
    std::shared_ptr<arrow::Array>
    boolean_col_to_array(
        const std::vector<t_tscalar>& data,
        std::int32_t cidx,
        std::int32_t stride,
        t_get_data_extents extents);

    /**
     * @brief Build an `arrow::Array` from a column typed as `DTYPE_DATE.`
     * Separated out from the main templated `col_to_array` as `t_date` is an
     * `uint32_t` that needs to be written into the `arrow::Array` as an
     * `int32_t`.
     *
     * @param data a vector of scalars to write into an arrow array.
     * @param cidx the index of the column in the dataset, used to calculate
     *      the physical offset of the column's data in `data`, a tightly packed
     *      vector containing the dataset for the entire data slice.
     * @param stride the number of columns present in the data slice, used to
     *      calculate the data's physical offset.
     * @param extents the start/end rows and columns of the data slice, used
     *      to calculate the data's physical offset.
     */
    std::shared_ptr<arrow::Array>
    date_col_to_array(
        const std::vector<t_tscalar>& data,
        std::int32_t cidx,
        std::int32_t stride,
        t_get_data_extents extents);

    /**
     * @brief Build an `arrow::Array` from a column typed as `DTYPE_TIME`.
     * Separated out from the main templated `col_to_array` as
     * `arrow::timestamp()` has parameters that need to be filled.
     *
     * @param data a vector of scalars to write into an arrow array.
     * @param cidx the index of the column in the dataset, used to calculate
     *      the physical offset of the column's data in `data`, a tightly packed
     *      vector containing the dataset for the entire data slice.
     * @param stride the number of columns present in the data slice, used to
     *      calculate the data's physical offset.
     * @param extents the start/end rows and columns of the data slice, used
     *      to calculate the data's physical offset.
     */
    std::shared_ptr<arrow::Array>
    timestamp_col_to_array(
        const std::vector<t_tscalar>& data,
        std::int32_t cidx,
        std::int32_t stride,
        t_get_data_extents extents);

    /**
     * @brief Build an `arrow::Array` from a column typed as `DTYPE_STR`, using
     * arrow's `DictionaryArray` constructors.
     *
     * @param data a vector of scalars to write into an arrow array.
     * @param cidx the index of the column in the dataset, used to calculate
     *      the physical offset of the column's data in `data`, a tightly packed
     *      vector containing the dataset for the entire data slice.
     * @param stride the number of columns present in the data slice, used to
     *      calculate the data's physical offset.
     * @param extents the start/end rows and columns of the data slice, used
     *      to calculate the data's physical offset.
     */
    std::shared_ptr<arrow::Array>
    string_col_to_dictionary_array(
        const std::vector<t_tscalar>& data,
        std::int32_t cidx,
        std::int32_t stride,
        t_get_data_extents extents);

    /**
     * @brief Build an `arrow::Array` from a column contained in `data`. Column
     * building methods read from the vector of scalars that make up the data
     * slice, as `t_data_slice` is templated on the context type and we'd like
     * to avoid template hell.
     *
     * @tparam ArrowDataType 
     * @tparam ArrowValueType
     *
     * @param data a vector of scalars to write into an arrow array.
     * @param cidx the index of the column in the dataset, used to calculate
     *      the physical offset of the column's data in `data`, a tightly packed
     *      vector containing the dataset for the entire data slice.
     * @param stride the number of columns present in the data slice, used to
     *      calculate the data's physical offset.
     * @param extents the start/end rows and columns of the data slice, used
     *      to calculate the data's physical offset.
     */
    template <typename ArrowDataType, typename ArrowValueType>
    std::shared_ptr<arrow::Array>
    numeric_col_to_array(
        const std::vector<t_tscalar>& data,
        std::int32_t cidx,
        std::int32_t stride,
        t_get_data_extents extents) {
        // NumericBuilder encompasses the most types (int/float/datetime)
        arrow::NumericBuilder<ArrowDataType> array_builder;
        auto reserve_status = array_builder.Reserve(
            extents.m_erow - extents.m_srow);
        if (!reserve_status.ok()) {
            std::stringstream ss;
            ss << "Failed to allocate buffer for column: "
               << reserve_status.message() << std::endl;
            PSP_COMPLAIN_AND_ABORT(ss.str());
        }

        for (int ridx = extents.m_srow; ridx < extents.m_erow; ++ridx) {
            auto idx = get_idx(cidx, ridx, stride, extents);
            t_tscalar scalar = data.operator[](idx);
            if (scalar.is_valid() && scalar.get_dtype() != DTYPE_NONE) {
                ArrowValueType val = get_scalar<ArrowValueType>(scalar);
                array_builder.UnsafeAppend(val);
            } else {
                array_builder.UnsafeAppendNull();
            }
        }
        
        // Point to base `arrow::Array` instead of derived, so we don't have to
        // template the caller.
        std::shared_ptr<arrow::Array> array;
        arrow::Status status = array_builder.Finish(&array);
        if (!status.ok()) {
            PSP_COMPLAIN_AND_ABORT(status.message());
        }
        return array;
    }

} // namespace arrow
} // namespace perspective