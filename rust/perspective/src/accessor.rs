/******************************************************************************
 *
 * Copyright (c) 2019, the Perspective Authors.
 *
 * This file is part of the Perspective library, distributed under the terms of
 * the Apache License 2.0.  The full license can be found in the LICENSE file.
 *
 */
// #![cfg(target_arch = "wasm32")]

use std::collections::HashMap;

use arrow::array::*;
use arrow::datatypes::*;
use arrow::record_batch::RecordBatch;
use chrono::Datelike;
use chrono::NaiveDate;

use js_sys::{Array, Date};
use wasm_bindgen::prelude::*;

// A struct that allows the user to read out an Arrow record batch as a
// 2D array through Rust's WASM bindings.
pub struct ArrowAccessor {
    pub column_paths: Vec<String>,
    pub column_indices: HashMap<String, usize>,
    pub data: Option<Array>,
}

impl ArrowAccessor {
    // Convert every column in the Arrow record batch into a Javascript
    // array that matches the output of Perspective's data serialization
    // methods, i.e. datetime/dates should return Unix timestamps in
    // milliseconds since epoch, i64/u64 should be coerced to float, etc.
    pub fn new(batch: Box<RecordBatch>) -> Self {
        let schema = batch.schema();
        let num_columns = batch.num_columns();
        let mut column_paths: Vec<String> = Vec::with_capacity(num_columns);
        let mut column_indices: HashMap<String, usize> = HashMap::new();
        let data: Array = Array::new();

        for cidx in 0..num_columns {
            let col = batch.column(cidx);
            let name = schema.field(cidx).name();
            let dtype = col.data_type();
            let nrows = col.len();

            let column_data: Array = Array::new();
            match dtype {
                DataType::Boolean => {
                    let typed_col = col.as_any().downcast_ref::<BooleanArray>().unwrap();
                    for ridx in 0..nrows {
                        if col.is_valid(ridx) {
                            column_data.push(&JsValue::from(typed_col.value(ridx)));
                        } else {
                            column_data.push(&JsValue::NULL);
                        }
                    }
                }
                DataType::UInt8 => {
                    let typed_col = col.as_any().downcast_ref::<UInt8Array>().unwrap();
                    for ridx in 0..nrows {
                        if col.is_valid(ridx) {
                            column_data.push(&JsValue::from(typed_col.value(ridx)));
                        } else {
                            column_data.push(&JsValue::NULL);
                        }
                    }
                }
                DataType::UInt16 => {
                    let typed_col = col.as_any().downcast_ref::<UInt16Array>().unwrap();
                    for ridx in 0..nrows {
                        if col.is_valid(ridx) {
                            column_data.push(&JsValue::from(typed_col.value(ridx)));
                        } else {
                            column_data.push(&JsValue::NULL);
                        }
                    }
                }
                DataType::UInt32 => {
                    let typed_col = col.as_any().downcast_ref::<UInt32Array>().unwrap();
                    for ridx in 0..nrows {
                        if col.is_valid(ridx) {
                            column_data.push(&JsValue::from(typed_col.value(ridx)));
                        } else {
                            column_data.push(&JsValue::NULL);
                        }
                    }
                }
                DataType::UInt64 => {
                    let typed_col = col.as_any().downcast_ref::<UInt64Array>().unwrap();
                    for ridx in 0..nrows {
                        if col.is_valid(ridx) {
                            // no i64/u64 in wasm - coerce to float
                            column_data.push(&JsValue::from(typed_col.value(ridx) as f64));
                        } else {
                            column_data.push(&JsValue::NULL);
                        }
                    }
                }
                DataType::Int8 => {
                    let typed_col = col.as_any().downcast_ref::<Int8Array>().unwrap();
                    for ridx in 0..nrows {
                        if col.is_valid(ridx) {
                            column_data.push(&JsValue::from(typed_col.value(ridx)));
                        } else {
                            column_data.push(&JsValue::NULL);
                        }
                    }
                }
                DataType::Int16 => {
                    let typed_col = col.as_any().downcast_ref::<Int16Array>().unwrap();
                    for ridx in 0..nrows {
                        if col.is_valid(ridx) {
                            column_data.push(&JsValue::from(typed_col.value(ridx)));
                        } else {
                            column_data.push(&JsValue::NULL);
                        }
                    }
                }
                DataType::Int32 => {
                    let typed_col = col.as_any().downcast_ref::<Int32Array>().unwrap();
                    for ridx in 0..nrows {
                        if col.is_valid(ridx) {
                            column_data.push(&JsValue::from(typed_col.value(ridx)));
                        } else {
                            column_data.push(&JsValue::NULL);
                        }
                    }
                }
                DataType::Int64 => {
                    let typed_col = col.as_any().downcast_ref::<Int64Array>().unwrap();
                    for ridx in 0..nrows {
                        if col.is_valid(ridx) {
                            // no i64/u64 in wasm - coerce to float
                            column_data.push(&JsValue::from(typed_col.value(ridx) as f64));
                        } else {
                            column_data.push(&JsValue::NULL);
                        }
                    }
                }
                DataType::Float32 => {
                    let typed_col = col.as_any().downcast_ref::<Float32Array>().unwrap();
                    for ridx in 0..nrows {
                        if col.is_valid(ridx) {
                            column_data.push(&JsValue::from(typed_col.value(ridx)));
                        } else {
                            column_data.push(&JsValue::NULL);
                        }
                    }
                }
                DataType::Float64 => {
                    let typed_col = col.as_any().downcast_ref::<Float64Array>().unwrap();
                    for ridx in 0..nrows {
                        if col.is_valid(ridx) {
                            column_data.push(&JsValue::from(typed_col.value(ridx)));
                        } else {
                            column_data.push(&JsValue::NULL);
                        }
                    }
                }
                DataType::Date32(DateUnit::Day) => {
                    let typed_col = col.as_any().downcast_ref::<Date32Array>().unwrap();
                    for ridx in 0..nrows {
                        if col.is_valid(ridx) {
                            // Construct a new `Date()` object in the browser's local
                            // time, and use the object's Unix timestamp, otherwise
                            // the browser will attempt to coerce to UTC and offset the
                            // timestamp value again when it is not needed.
                            let value: NaiveDate = typed_col.value_as_date(ridx).unwrap();
                            let dt = Date::new_with_year_month_day(
                                value.year() as u32,
                                value.month0() as i32,
                                value.day() as i32,
                            );
                            column_data.push(&JsValue::from(dt.value_of()));
                        } else {
                            column_data.push(&JsValue::NULL);
                        }
                    }
                }
                DataType::Timestamp(TimeUnit::Millisecond, _) => {
                    let typed_col = col
                        .as_any()
                        .downcast_ref::<TimestampMillisecondArray>()
                        .unwrap();
                    for ridx in 0..nrows {
                        if col.is_valid(ridx) {
                            // no i64/u64 in wasm - coerce to float
                            column_data.push(&JsValue::from(typed_col.value(ridx) as f64));
                        } else {
                            column_data.push(&JsValue::NULL);
                        }
                    }
                }
                DataType::Dictionary(ref key_type, _) => match key_type.as_ref() {
                    DataType::Int32 => {
                        let dict_array =
                            col.as_any().downcast_ref::<Int32DictionaryArray>().unwrap();
                        let values = dict_array.values();
                        let strings = values.as_any().downcast_ref::<StringArray>().unwrap();
                        for ridx in 0..nrows {
                            if col.is_valid(ridx) {
                                let key = dict_array.keys_array().value(ridx) as usize;
                                let val = String::from(strings.value(key));
                                column_data.push(&JsValue::from(val));
                            } else {
                                column_data.push(&JsValue::NULL);
                            }
                        }
                    }
                    dtype => panic!("Unexpected dictionary key type {:?}", dtype),
                },
                dtype => panic!("Unexpected data type {:?}", dtype),
            };

            column_paths.push(name.clone());
            column_indices.insert(name.clone(), cidx);
            data.push(&column_data);
        }

        ArrowAccessor {
            column_paths,
            column_indices,
            data: Some(data),
        }
    }
}
