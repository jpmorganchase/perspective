/******************************************************************************
 *
 * Copyright (c) 2019, the Perspective Authors.
 *
 * This file is part of the Perspective library, distributed under the terms of
 * the Apache License 2.0.  The full license can be found in the LICENSE file.
 *
 */
// #![cfg(target_arch = "wasm32")]

mod util;

use arrow::array::{Array, Date32Array, TimestampMillisecondArray};
use chrono::{Datelike, NaiveDate, NaiveDateTime};

use js_sys::{Array as JsArray, Date};
use wasm_bindgen::JsValue;
use wasm_bindgen_test::*;

use perspective::accessor::ArrowAccessor;
use perspective::*;

use crate::util::*;

/**
 * Load from record batch generated in Rust
 */

#[wasm_bindgen_test]
fn load_arrow_from_batch() {
    let batch = make_arrow();
    let accessor = ArrowAccessor::new(batch.clone());
    let boxed = Box::new(accessor);
    compare_arrow(boxed);
}

#[wasm_bindgen_test]
fn load_arrow_from_batch_nullable() {
    let batch = make_arrow_nullable();
    let accessor = ArrowAccessor::new(batch.clone());
    let boxed = Box::new(accessor);
    compare_arrow_nullable(boxed);
}

/**
 * Loads from stream generated in Rust
 */

#[wasm_bindgen_test]
fn load_arrow_from_stream() {
    let batch = make_arrow();
    let buffer = arrow_to_arraybuffer(batch);
    let accessor = load_arrow_stream(buffer);
    compare_arrow(accessor);
}

#[wasm_bindgen_test]
fn load_nullable_arrow_from_stream() {
    let batch = make_arrow_nullable();
    let buffer = arrow_to_arraybuffer(batch);
    let accessor = load_arrow_stream(buffer);
    compare_arrow_nullable(accessor);
}

/**
 * Loads from stream generated in rust, using accessor as a raw pointer.
 */

#[wasm_bindgen_test]
fn get_data_from_accessor() {
    let batch = make_arrow();
    let buffer = arrow_to_arraybuffer(batch);
    let accessor_ptr: *mut ArrowAccessor = accessor_make(buffer);
    let data = accessor_get_data(accessor_ptr);

    // Convert to JsArray
    let array_data = JsArray::from(&data);
    assert_eq!(array_data.length(), 14);
    let col = JsArray::from(&data.get(0 as u32));
    assert_eq!(col.length(), 5);

    let expected = vec![123, u8::MIN, u8::MAX, u8::MAX, u8::MAX]
        .into_iter()
        .map(|item| JsValue::from(item))
        .collect::<Vec<JsValue>>();

    compare_arrays(col, expected);
}

#[wasm_bindgen_test]
fn get_column_paths_from_accessor() {
    let batch = make_arrow();
    let buffer = arrow_to_arraybuffer(batch);
    let accessor_ptr: *mut ArrowAccessor = accessor_make(buffer);
    let column_paths = accessor_get_column_paths(accessor_ptr);
    let expected = vec![
        "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n",
    ]
    .into_iter()
    .map(|item| JsValue::from(item))
    .collect::<Vec<JsValue>>();

    for (i, name) in column_paths.into_iter().enumerate() {
        assert_eq!(name, expected[i]);
    }
}

#[wasm_bindgen_test]
fn drop_accessor() {
    let batch = make_arrow();
    let buffer = arrow_to_arraybuffer(batch);
    let accessor_ptr: *mut ArrowAccessor = accessor_make(buffer);
    accessor_drop(accessor_ptr);
}

fn compare_arrow(accessor: Box<ArrowAccessor>) {
    let paths = vec![
        "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n",
    ];
    let num_cols = 14;

    assert_eq!(accessor.column_paths, paths);

    let data = accessor.data.unwrap();
    assert_eq!(data.length(), num_cols);

    for (i, column_name) in accessor.column_paths.iter().enumerate() {
        // Unwrap each column and make sure the data is correct.
        let column = JsArray::from(&data.get(i as u32));
        let num_rows = column.length();
        assert_eq!(num_rows, 5);

        match column_name.as_str() {
            "a" => {
                let expected = vec![123, u8::MIN, u8::MAX, u8::MAX, u8::MAX]
                    .into_iter()
                    .map(|item| JsValue::from(item))
                    .collect::<Vec<JsValue>>();

                compare_arrays(column, expected);
            }
            "b" => {
                let expected = vec![1234, u16::MIN, u16::MAX, u16::MAX, u16::MAX]
                    .into_iter()
                    .map(|item| JsValue::from(item))
                    .collect::<Vec<JsValue>>();

                compare_arrays(column, expected);
            }
            "c" => {
                let expected = vec![12345, u32::MIN, u32::MAX, u32::MAX, u32::MAX]
                    .into_iter()
                    .map(|item| JsValue::from(item))
                    .collect::<Vec<JsValue>>();

                compare_arrays(column, expected);
            }
            "d" => {
                let expected = vec![123456, u64::MIN, u64::MAX, u64::MAX, u64::MAX]
                    .into_iter()
                    .map(|i| i as f64)
                    .map(|item| JsValue::from(item))
                    .collect::<Vec<JsValue>>();

                compare_arrays(column, expected);
            }
            "e" => {
                let expected = vec![-123, i8::MIN, i8::MAX, i8::MAX, i8::MAX]
                    .into_iter()
                    .map(|item| JsValue::from(item))
                    .collect::<Vec<JsValue>>();

                compare_arrays(column, expected);
            }
            "f" => {
                let expected = vec![-1234, i16::MIN, i16::MAX, i16::MAX, i16::MAX]
                    .into_iter()
                    .map(|item| JsValue::from(item))
                    .collect::<Vec<JsValue>>();

                compare_arrays(column, expected);
            }
            "g" => {
                let expected = vec![-12345, i32::MIN, i32::MAX, i32::MAX, i32::MAX]
                    .into_iter()
                    .map(|item| JsValue::from(item))
                    .collect::<Vec<JsValue>>();

                compare_arrays(column, expected);
            }
            "h" => {
                let expected = vec![-123456, i64::MIN, i64::MAX, i64::MAX, i64::MAX]
                    .into_iter()
                    .map(|i| i as f64)
                    .map(|item| JsValue::from(item))
                    .collect::<Vec<JsValue>>();

                compare_arrays(column, expected);
            }
            "i" => {
                let expected = vec![
                    f32::MIN,
                    f32::MIN_POSITIVE,
                    f32::MAX,
                    123.456789,
                    456.789123,
                ]
                .into_iter()
                .map(|item| JsValue::from(item))
                .collect::<Vec<JsValue>>();

                compare_arrays(column, expected);
            }
            "j" => {
                let expected = vec![
                    f64::MIN,
                    f64::MIN_POSITIVE,
                    f64::MAX,
                    123.456789,
                    456.789123,
                ]
                .into_iter()
                .map(|item| JsValue::from(item))
                .collect::<Vec<JsValue>>();

                compare_arrays(column, expected);
            }
            "k" => {
                let mut expected: Vec<JsValue> = Vec::new();
                // Create an Arrow array and let Date32Array convert from
                // no. days since epoch -> NaiveDate
                let dates = Date32Array::from(vec![18000, 18100, 18200, 18300, 18400]);
                for i in 0..dates.len() {
                    let value: NaiveDate = dates.value_as_date(i).unwrap();
                    let dt = Date::new_with_year_month_day(
                        value.year() as u32,
                        value.month0() as i32,
                        value.day() as i32,
                    );
                    expected.push(JsValue::from(dt.value_of()));
                }
                compare_arrays(column, expected);
            }
            "l" => {
                let mut expected: Vec<JsValue> = Vec::new();
                let datetimes = TimestampMillisecondArray::from(vec![
                    NaiveDateTime::parse_from_str("2020-01-15 12:30:45", "%Y-%m-%d %H:%M:%S")
                        .unwrap()
                        .timestamp(),
                    NaiveDateTime::parse_from_str("2020-02-29 22:55:45", "%Y-%m-%d %H:%M:%S")
                        .unwrap()
                        .timestamp(),
                    NaiveDateTime::parse_from_str("2020-03-30 05:12:45", "%Y-%m-%d %H:%M:%S")
                        .unwrap()
                        .timestamp(),
                    NaiveDateTime::parse_from_str("2020-04-01 09:59:45", "%Y-%m-%d %H:%M:%S")
                        .unwrap()
                        .timestamp(),
                    NaiveDateTime::parse_from_str("2020-05-15 19:01:45", "%Y-%m-%d %H:%M:%S")
                        .unwrap()
                        .timestamp(),
                ]);

                for i in 0..datetimes.len() {
                    expected.push(JsValue::from(datetimes.value(i) as f64));
                }

                compare_arrays(column, expected);
            }
            "m" => {
                let expected = vec![true, false, true, false, true]
                    .into_iter()
                    .map(|item| JsValue::from(item))
                    .collect::<Vec<JsValue>>();

                compare_arrays(column, expected);
            }
            "n" => {
                let expected = vec!["abc", "def", "abc", "hij", "klm"]
                    .into_iter()
                    .map(|item| JsValue::from(item))
                    .collect::<Vec<JsValue>>();

                compare_arrays(column, expected);
            }
            _ => panic!("Unexpected column name: {}", column_name),
        }
    }
}

fn compare_arrow_nullable(accessor: Box<ArrowAccessor>) {
    let paths = vec![
        "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n",
    ];
    let num_cols = 14;

    assert_eq!(accessor.column_paths, paths);

    let data = accessor.data.unwrap();
    assert_eq!(data.length(), num_cols);

    for (i, column_name) in accessor.column_paths.iter().enumerate() {
        // Unwrap each column and make sure the data is correct.
        let column = JsArray::from(&data.get(i as u32));
        let num_rows = column.length();
        assert_eq!(num_rows, 5);

        match column_name.as_str() {
            "a" => {
                let expected = vec![None, Some(u8::MIN), Some(u8::MAX), Some(u8::MAX), None]
                    .into_iter()
                    .map(|i| match i {
                        Some(val) => JsValue::from(val),
                        None => JsValue::NULL,
                    })
                    .collect::<Vec<JsValue>>();

                compare_arrays(column, expected);
            }
            "b" => {
                let expected = vec![None, Some(u16::MIN), Some(u16::MAX), Some(u16::MAX), None]
                    .into_iter()
                    .map(|i| match i {
                        Some(val) => JsValue::from(val),
                        None => JsValue::NULL,
                    })
                    .collect::<Vec<JsValue>>();

                compare_arrays(column, expected);
            }
            "c" => {
                let expected = vec![None, Some(u32::MIN), Some(u32::MAX), Some(u32::MAX), None]
                    .into_iter()
                    .map(|i| match i {
                        Some(val) => JsValue::from(val),
                        None => JsValue::NULL,
                    })
                    .collect::<Vec<JsValue>>();

                compare_arrays(column, expected);
            }
            "d" => {
                let expected = vec![None, Some(u64::MIN), Some(u64::MAX), Some(u64::MAX), None]
                    .into_iter()
                    .map(|i| match i {
                        Some(val) => JsValue::from(val as f64),
                        None => JsValue::NULL,
                    })
                    .collect::<Vec<JsValue>>();

                compare_arrays(column, expected);
            }
            "e" => {
                let expected = vec![None, Some(i8::MIN), Some(i8::MAX), Some(i8::MAX), None]
                    .into_iter()
                    .map(|i| match i {
                        Some(val) => JsValue::from(val),
                        None => JsValue::NULL,
                    })
                    .collect::<Vec<JsValue>>();

                compare_arrays(column, expected);
            }
            "f" => {
                let expected = vec![None, Some(i16::MIN), Some(i16::MAX), Some(i16::MAX), None]
                    .into_iter()
                    .map(|i| match i {
                        Some(val) => JsValue::from(val),
                        None => JsValue::NULL,
                    })
                    .collect::<Vec<JsValue>>();

                compare_arrays(column, expected);
            }
            "g" => {
                let expected = vec![None, Some(i32::MIN), Some(i32::MAX), Some(i32::MAX), None]
                    .into_iter()
                    .map(|i| match i {
                        Some(val) => JsValue::from(val),
                        None => JsValue::NULL,
                    })
                    .collect::<Vec<JsValue>>();

                compare_arrays(column, expected);
            }
            "h" => {
                let expected = vec![None, Some(i64::MIN), Some(i64::MAX), Some(i64::MAX), None]
                    .into_iter()
                    .map(|i| match i {
                        Some(val) => JsValue::from(val as f64),
                        None => JsValue::NULL,
                    })
                    .collect::<Vec<JsValue>>();

                compare_arrays(column, expected);
            }
            "i" => {
                let expected = vec![
                    None,
                    Some(f32::MIN),
                    Some(f32::MIN_POSITIVE),
                    Some(f32::MAX),
                    None,
                ]
                .into_iter()
                .map(|i| match i {
                    Some(val) => JsValue::from(val),
                    None => JsValue::NULL,
                })
                .collect::<Vec<JsValue>>();

                compare_arrays(column, expected);
            }
            "j" => {
                let expected = vec![
                    None,
                    Some(f64::MIN),
                    Some(f64::MIN_POSITIVE),
                    Some(f64::MAX),
                    None,
                ]
                .into_iter()
                .map(|i| match i {
                    Some(val) => JsValue::from(val as f64),
                    None => JsValue::NULL,
                })
                .collect::<Vec<JsValue>>();

                compare_arrays(column, expected);
            }
            "k" => {
                let mut expected: Vec<JsValue> = Vec::new();
                // Create an Arrow array and let Date32Array convert from
                // no. days since epoch -> NaiveDate
                let dates =
                    Date32Array::from(vec![None, Some(18100), Some(18200), Some(18300), None]);
                for i in 0..dates.len() {
                    if dates.is_valid(i) {
                        let value: NaiveDate = dates.value_as_date(i).unwrap();
                        let dt = Date::new_with_year_month_day(
                            value.year() as u32,
                            value.month0() as i32,
                            value.day() as i32,
                        );
                        expected.push(JsValue::from(dt.value_of()));
                    } else {
                        expected.push(JsValue::NULL);
                    }
                }
                compare_arrays(column, expected);
            }
            "l" => {
                let mut expected: Vec<JsValue> = Vec::new();
                let datetimes = TimestampMillisecondArray::from(vec![
                    None,
                    Some(
                        NaiveDateTime::parse_from_str("2020-02-29 22:55:45", "%Y-%m-%d %H:%M:%S")
                            .unwrap()
                            .timestamp(),
                    ),
                    Some(
                        NaiveDateTime::parse_from_str("2020-03-30 05:12:45", "%Y-%m-%d %H:%M:%S")
                            .unwrap()
                            .timestamp(),
                    ),
                    Some(
                        NaiveDateTime::parse_from_str("2020-04-01 09:59:45", "%Y-%m-%d %H:%M:%S")
                            .unwrap()
                            .timestamp(),
                    ),
                    None,
                ]);

                for i in 0..datetimes.len() {
                    if datetimes.is_valid(i) {
                        expected.push(JsValue::from(datetimes.value(i) as f64));
                    } else {
                        expected.push(JsValue::NULL);
                    }
                }

                compare_arrays(column, expected);
            }
            "m" => {
                let expected = vec![None, Some(false), Some(true), Some(false), None]
                    .into_iter()
                    .map(|i| match i {
                        Some(val) => JsValue::from(val),
                        None => JsValue::NULL,
                    })
                    .collect::<Vec<JsValue>>();

                compare_arrays(column, expected);
            }
            "n" => {
                let expected = vec![None, Some("def"), Some("abc"), Some("hij"), None]
                    .into_iter()
                    .map(|i| match i {
                        Some(val) => JsValue::from(val),
                        None => JsValue::NULL,
                    })
                    .collect::<Vec<JsValue>>();

                compare_arrays(column, expected);
            }
            _ => panic!("Unexpected column name: {}", column_name),
        }
    }
}
