/******************************************************************************
 *
 * Copyright (c) 2019, the Perspective Authors.
 *
 * This file is part of the Perspective library, distributed under the terms of
 * the Apache License 2.0.  The full license can be found in the LICENSE file.
 *
 */
#ifdef PSP_ENABLE_PYTHON

#include <perspective/arrow_loader.h>
#include <perspective/base.h>
#include <perspective/binding.h>
#include <perspective/python/accessor.h>
#include <perspective/python/base.h>
#include <perspective/python/fill.h>
#include <perspective/python/numpy.h>
#include <perspective/python/table.h>
#include <perspective/python/utils.h>

namespace perspective {
namespace binding {
    using namespace perspective::apachearrow;

/******************************************************************************
 *
 * Table API
 */

std::shared_ptr<Table> make_table_py(t_val table, t_data_accessor accessor,
        std::uint32_t limit, py::str index, t_op op, bool is_update, bool is_arrow, t_uindex port_id) {
    bool table_initialized = !table.is_none();
    std::shared_ptr<t_pool> pool;
    std::shared_ptr<Table> tbl;
    std::shared_ptr<t_gnode> gnode;
    std::uint32_t offset;
    void* ptr = nullptr;

    // If the Table has already been created, use it
    if (table_initialized) {
        tbl = table.cast<std::shared_ptr<Table>>();
        pool = tbl->get_pool();
        gnode = tbl->get_gnode();
        offset = tbl->get_offset();
        is_update = (is_update || gnode->mapping_size() > 0);
    } else {
        pool = std::make_shared<t_pool>();
    }

    std::vector<std::string> column_names;
    std::vector<t_dtype> data_types;
    ArrowLoader arrow_loader;
    numpy::NumpyLoader numpy_loader(accessor);

    // don't call `is_numpy` on an arrow binary
    bool is_numpy = !is_arrow && accessor.attr("_is_numpy").cast<bool>();

    // Determine metadata
    bool is_delete = op == OP_DELETE;
    if (is_arrow && !is_delete) {
        py::bytes bytes = accessor.cast<py::bytes>();
        std::int32_t size = bytes.attr("__len__")().cast<std::int32_t>();
        ptr = malloc(size);
        std::memcpy(ptr, bytes.cast<std::string>().c_str(), size);
        {
            PerspectiveScopedGILRelease acquire(pool->get_event_loop_thread_id(), pool->get_lock(), true);
        
            arrow_loader.initialize((uintptr_t)ptr, size);

            // Always use the `Table` column names and data types on update.
            if (table_initialized && is_update) {
                auto gnode_output_schema = gnode->get_output_schema();
                auto schema = gnode_output_schema.drop({"psp_okey"});
                column_names = schema.columns();
                data_types = schema.types();

                auto data_table = gnode->get_table();
                if (data_table->size() == 0) {
                    /**
                     * If updating a table created from schema, a 32-bit int/float
                     * needs to be promoted to a 64-bit int/float if specified in
                     * the Arrow schema.
                     */
                    std::vector<t_dtype> arrow_dtypes = arrow_loader.types();
                    for (auto idx = 0; idx < column_names.size(); ++idx) {
                        const std::string& name = column_names[idx];
                        bool can_retype = name != "psp_okey" && name != "psp_pkey" && name != "psp_op";
                        bool is_32_bit = data_types[idx] == DTYPE_INT32 || data_types[idx] == DTYPE_FLOAT32;
                        if (can_retype && is_32_bit) {
                            t_dtype arrow_dtype = arrow_dtypes[idx];
                            switch (arrow_dtype) {
                                case DTYPE_INT64:
                                case DTYPE_FLOAT64: {
                                    std::cout << "Promoting column `" 
                                                << column_names[idx] 
                                                << "` to maintain consistency with Arrow type."
                                                << std::endl;
                                    gnode->promote_column(name, arrow_dtype);
                                } break;
                                default: {
                                    continue;
                                }
                            }
                        }
                    }
                }
                // Make sure promoted types are used to construct data table
                auto new_schema = gnode->get_output_schema().drop({"psp_okey"});
                data_types = new_schema.types();
            } else {
                column_names = arrow_loader.names();
                data_types = arrow_loader.types();
            }
        }
    } else if (is_update || is_delete) {
        /**
         * Use the names and types of the python accessor when updating/deleting.
         * 
         * This prevents the Table from looking up new columns present in an update.
         * 
         * Example: updating a Table with a DataFrame attempts to write the "index" column, but if the table was
         * not created from a DataFrame, the "index" column would not exist.
         */
        if (is_numpy) {
            // `numpy_loader`s `m_names` and `m_types` variable contains only
            // the column names and data types present in the update dataset,
            // not the names/types of the entire `Table`.
            numpy_loader.init();
        }

        // `column_names` and `data_types` contain every single column in the 
        // dataset, as well as `__INDEX__` if it exists.
        column_names = accessor.attr("names")().cast<std::vector<std::string>>();
        data_types = accessor.attr("types")().cast<std::vector<t_dtype>>();
    } else if (is_numpy) {
        /**
         * Numpy loading depends on both the `dtype` of the individual arrays as well as the inferred type from
         * Perspective. Using `get_data_types` allows us to know the type of an array with `dtype=object`.
         */
        numpy_loader.init();

        // This will contain every single column in the dataset, as the
        // first-time data load path does not mutate the `names` property of
        // `accessor`.
        column_names = numpy_loader.names();

        // Infer data type for each column, and then use a composite of numpy
        // dtype, inferred `t_dtype`, and stringified numpy dtype to get the
        // final, canonical data type mapping.
        std::vector<t_dtype> inferred_types = get_data_types(accessor.attr("data")(), 1, column_names, accessor.attr("date_validator")().cast<t_val>());
        data_types = numpy_loader.reconcile_dtypes(inferred_types);
    }  else {
        // Infer names and types
        t_val data = accessor.attr("data")();
        std::int32_t format = accessor.attr("format")().cast<std::int32_t>();
        column_names = get_column_names(data, format);
        data_types = get_data_types(data, format, column_names, accessor.attr("date_validator")().cast<t_val>());
    }
    
    if (!table_initialized) {
        tbl = std::make_shared<Table>(pool, column_names, data_types, limit, index);
        offset = 0;
    }

    // Create input schema - an input schema contains all columns to be displayed AND index + operation columns
    t_schema input_schema(column_names, data_types);

    // strip implicit index, if present
    auto implicit_index_it = std::find(column_names.begin(), column_names.end(), "__INDEX__");
    if (implicit_index_it != column_names.end()) {
        auto idx = std::distance(column_names.begin(), implicit_index_it);
        // position of the column is at the same index in both vectors
        column_names.erase(column_names.begin() + idx);
        data_types.erase(data_types.begin() + idx);
    }

    // Create output schema - contains only columns to be displayed to the user
    t_schema output_schema(column_names, data_types); // names + types might have been mutated at this point after implicit index removal
    t_data_table data_table(output_schema);
    data_table.init();
    std::uint32_t row_count;
    if (is_arrow) {
        PerspectiveScopedGILRelease acquire(pool->get_event_loop_thread_id(), pool->get_lock(), true);
        row_count = arrow_loader.row_count();
        data_table.extend(arrow_loader.row_count());
        arrow_loader.fill_table(data_table, input_schema, index, offset, limit, is_update);
    } else if (is_numpy) {
        row_count = numpy_loader.row_count();
        data_table.extend(row_count);
        numpy_loader.fill_table(data_table, input_schema, index, offset, limit, is_update);
    } else {
        row_count = accessor.attr("row_count")().cast<std::int32_t>();
        data_table.extend(row_count);
        _fill_data(data_table, accessor, input_schema, index, offset, limit, is_update);
    }

    if (is_arrow) {
        free(ptr);
    }

    // calculate offset, limit, and set the gnode
    tbl->init(data_table, row_count, op, port_id);    

    //pool->_process();
    return tbl;
}

} //namespace binding
} //namespace perspective

#endif