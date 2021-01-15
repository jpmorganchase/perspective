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
use std::sync::Arc;

use arrow::array::{
    ArrayRef, BooleanArray, Date32Array, DictionaryArray, Float32Array, Float64Array, Int16Array,
    Int32Array, Int64Array, Int8Array, TimestampMillisecondArray, UInt16Array, UInt32Array,
    UInt64Array, UInt8Array,
};
use arrow::datatypes::*;
use arrow::ipc::writer::{IpcWriteOptions, StreamWriter};
use arrow::record_batch::RecordBatch;

use chrono::NaiveDateTime;
use wasm_bindgen::JsValue;

// Generate an Arrow record batch of all types that can be converted.
pub fn make_arrow() -> Box<RecordBatch> {
    let strings = vec!["abc", "def", "abc", "hij", "klm"];
    let dict_array: DictionaryArray<Int32Type> = strings.into_iter().collect();
    let arrays: Vec<ArrayRef> = vec![
        Arc::new(UInt8Array::from(vec![
            123,
            u8::MIN,
            u8::MAX,
            u8::MAX,
            u8::MAX,
        ])),
        Arc::new(UInt16Array::from(vec![
            1234,
            u16::MIN,
            u16::MAX,
            u16::MAX,
            u16::MAX,
        ])),
        Arc::new(UInt32Array::from(vec![
            12345,
            u32::MIN,
            u32::MAX,
            u32::MAX,
            u32::MAX,
        ])),
        Arc::new(UInt64Array::from(vec![
            123456,
            u64::MIN,
            u64::MAX,
            u64::MAX,
            u64::MAX,
        ])),
        Arc::new(Int8Array::from(vec![
            -123,
            i8::MIN,
            i8::MAX,
            i8::MAX,
            i8::MAX,
        ])),
        Arc::new(Int16Array::from(vec![
            -1234,
            i16::MIN,
            i16::MAX,
            i16::MAX,
            i16::MAX,
        ])),
        Arc::new(Int32Array::from(vec![
            -12345,
            i32::MIN,
            i32::MAX,
            i32::MAX,
            i32::MAX,
        ])),
        Arc::new(Int64Array::from(vec![
            -123456,
            i64::MIN,
            i64::MAX,
            i64::MAX,
            i64::MAX,
        ])),
        Arc::new(Float32Array::from(vec![
            f32::MIN,
            f32::MIN_POSITIVE,
            f32::MAX,
            123.456789,
            456.789123,
        ])),
        Arc::new(Float64Array::from(vec![
            f64::MIN,
            f64::MIN_POSITIVE,
            f64::MAX,
            123.456789,
            456.789123,
        ])),
        Arc::new(Date32Array::from(vec![18000, 18100, 18200, 18300, 18400])),
        Arc::new(TimestampMillisecondArray::from(vec![
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
        ])),
        Arc::new(BooleanArray::from(vec![true, false, true, false, true])),
        Arc::new(dict_array),
    ];

    // Arrow schema created using `Field`
    let schema = Schema::new(vec![
        Field::new("a", DataType::UInt8, false),
        Field::new("b", DataType::UInt16, false),
        Field::new("c", DataType::UInt32, false),
        Field::new("d", DataType::UInt64, false),
        Field::new("e", DataType::Int8, false),
        Field::new("f", DataType::Int16, false),
        Field::new("g", DataType::Int32, false),
        Field::new("h", DataType::Int64, false),
        Field::new("i", DataType::Float32, false),
        Field::new("j", DataType::Float64, false),
        Field::new("k", DataType::Date32(DateUnit::Day), false),
        Field::new("l", DataType::Timestamp(TimeUnit::Millisecond, None), false),
        Field::new("m", DataType::Boolean, false),
        Field::new(
            "n",
            DataType::Dictionary(Box::new(DataType::Int32), Box::new(DataType::Utf8)),
            false,
        ),
    ]);

    let schema_ref = SchemaRef::new(schema);

    let try_batch = RecordBatch::try_new(schema_ref.clone(), arrays);

    // Accessor schema of names to `DataType`
    let mut accessor_schema: HashMap<String, DataType> = HashMap::new();

    for field in schema_ref.fields() {
        accessor_schema.insert(field.name().clone(), field.data_type().clone());
    }

    match try_batch {
        Ok(batch) => Box::new(batch),
        Err(err) => panic!(err),
    }
}

// Generate an Arrow record batch with null values in columns.
pub fn make_arrow_nullable() -> Box<RecordBatch> {
    let strings = vec![None, Some("def"), Some("abc"), Some("hij"), None];
    let dict_array: DictionaryArray<Int32Type> = strings.into_iter().collect();
    let arrays: Vec<ArrayRef> = vec![
        Arc::new(UInt8Array::from(vec![
            None,
            Some(u8::MIN),
            Some(u8::MAX),
            Some(u8::MAX),
            None,
        ])),
        Arc::new(UInt16Array::from(vec![
            None,
            Some(u16::MIN),
            Some(u16::MAX),
            Some(u16::MAX),
            None,
        ])),
        Arc::new(UInt32Array::from(vec![
            None,
            Some(u32::MIN),
            Some(u32::MAX),
            Some(u32::MAX),
            None,
        ])),
        Arc::new(UInt64Array::from(vec![
            None,
            Some(u64::MIN),
            Some(u64::MAX),
            Some(u64::MAX),
            None,
        ])),
        Arc::new(Int8Array::from(vec![
            None,
            Some(i8::MIN),
            Some(i8::MAX),
            Some(i8::MAX),
            None,
        ])),
        Arc::new(Int16Array::from(vec![
            None,
            Some(i16::MIN),
            Some(i16::MAX),
            Some(i16::MAX),
            None,
        ])),
        Arc::new(Int32Array::from(vec![
            None,
            Some(i32::MIN),
            Some(i32::MAX),
            Some(i32::MAX),
            None,
        ])),
        Arc::new(Int64Array::from(vec![
            None,
            Some(i64::MIN),
            Some(i64::MAX),
            Some(i64::MAX),
            None,
        ])),
        Arc::new(Float32Array::from(vec![
            None,
            Some(f32::MIN),
            Some(f32::MIN_POSITIVE),
            Some(f32::MAX),
            None,
        ])),
        Arc::new(Float64Array::from(vec![
            None,
            Some(f64::MIN),
            Some(f64::MIN_POSITIVE),
            Some(f64::MAX),
            None,
        ])),
        Arc::new(Date32Array::from(vec![
            None,
            Some(18100),
            Some(18200),
            Some(18300),
            None,
        ])),
        Arc::new(TimestampMillisecondArray::from(vec![
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
        ])),
        Arc::new(BooleanArray::from(vec![
            None,
            Some(false),
            Some(true),
            Some(false),
            None,
        ])),
        Arc::new(dict_array),
    ];

    // Arrow schema created using `Field`
    let schema = Schema::new(vec![
        Field::new("a", DataType::UInt8, true),
        Field::new("b", DataType::UInt16, true),
        Field::new("c", DataType::UInt32, true),
        Field::new("d", DataType::UInt64, true),
        Field::new("e", DataType::Int8, true),
        Field::new("f", DataType::Int16, true),
        Field::new("g", DataType::Int32, true),
        Field::new("h", DataType::Int64, true),
        Field::new("i", DataType::Float32, true),
        Field::new("j", DataType::Float64, true),
        Field::new("k", DataType::Date32(DateUnit::Day), true),
        Field::new("l", DataType::Timestamp(TimeUnit::Millisecond, None), true),
        Field::new("m", DataType::Boolean, true),
        Field::new(
            "n",
            DataType::Dictionary(Box::new(DataType::Int32), Box::new(DataType::Utf8)),
            true,
        ),
    ]);

    let schema_ref = SchemaRef::new(schema);

    let try_batch = RecordBatch::try_new(schema_ref.clone(), arrays);

    // Accessor schema of names to `DataType`
    let mut accessor_schema: HashMap<String, DataType> = HashMap::new();

    for field in schema_ref.fields() {
        accessor_schema.insert(field.name().clone(), field.data_type().clone());
    }

    match try_batch {
        Ok(batch) => Box::new(batch),
        Err(err) => panic!(err),
    }
}

pub fn arrow_to_arraybuffer(batch: Box<RecordBatch>) -> Box<[u8]> {
    let mut buffer: Vec<u8> = Vec::new();
    let opts = IpcWriteOptions::try_new(64, false, arrow::ipc::MetadataVersion::V4).unwrap();

    {
        let mut writer =
            StreamWriter::try_new_with_options(&mut buffer, &batch.schema(), opts).unwrap();
        writer.write(&batch).unwrap();
        writer.finish().unwrap();
    }

    buffer.into_boxed_slice()
}

pub fn compare_arrays(result: js_sys::Array, expected: Vec<JsValue>) {
    for i in 0..result.length() {
        let item = result.get(i);
        assert_eq!(item, expected[i as usize]);
    }
}
