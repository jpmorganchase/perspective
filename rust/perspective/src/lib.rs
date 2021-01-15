/******************************************************************************
 *
 * Copyright (c) 2019, the Perspective Authors.
 *
 * This file is part of the Perspective library, distributed under the terms of
 * the Apache License 2.0.  The full license can be found in the LICENSE file.
 *
 */
// #![cfg(target_arch = "wasm32")]

pub mod accessor;

use std::io::Cursor;

use arrow::ipc::reader::StreamReader;
use arrow::record_batch::RecordBatch;

use js_sys::Array;
use wasm_bindgen::prelude::*;

use crate::accessor::ArrowAccessor;

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    pub fn log(s: &str);
}

// Debug `panic!` messages inside console.error().
pub fn set_panic_hook() {
    #[cfg(feature = "console_error_panic_hook")]
    console_error_panic_hook::set_once();
}

/// Load an arrow binary in stream format.
pub fn load_arrow_stream(buffer: Box<[u8]>) -> Box<ArrowAccessor> {
    let cursor = Cursor::new(buffer);
    let reader = StreamReader::try_new(cursor).unwrap();

    // Iterate over record batches, and collect them into a vector of
    // heap-allocated batches.
    let batches = reader
        .map(|batch| {
            // Panic if it can't read the batch
            Box::new(batch.unwrap())
        })
        .collect::<Vec<Box<RecordBatch>>>();

    if let [batch] = &batches[..] {
        Box::new(ArrowAccessor::new(batch.clone()))
    } else {
        panic!("Arrow should only contain a single record batch.")
    }
}

// Create a new ArrowAccessor struct and return a raw pointer to it. This raw
// pointer can be passed in and out of Rust in order to access the members of
// the ArrowAccessor instance.
#[wasm_bindgen]
pub fn accessor_make(buffer: Box<[u8]>) -> *mut ArrowAccessor {
    let accessor = load_arrow_stream(buffer);
    Box::into_raw(accessor)
}

// Returns the dataset contained in the accessor. Once this method is called,
// it cannot be called again as the accessor's data is invalidated. Because
// accessor.data contains the entire arrow binary serialized into a 2D array,
// it can be used in Javascript like any other value and so this method should
// not be needed multiple times.
#[wasm_bindgen]
pub fn accessor_get_data(accessor: *mut ArrowAccessor) -> Array {
    let accessor = unsafe { accessor.as_mut().unwrap() };
    accessor.data.take().expect("ArrowAccessor.data is None.")
}

// Returns the column paths of the accessor - an Array of column names in the
// order that they were serialized by Perspective's `to_arrow()`.
#[wasm_bindgen]
pub fn accessor_get_column_paths(accessor: *mut ArrowAccessor) -> Vec<JsValue> {
    let accessor = unsafe { accessor.as_mut().unwrap() };
    accessor
        .column_paths
        .iter()
        .map(|path| JsValue::from(path))
        .collect::<Vec<JsValue>>()
}

// Dereference the accessor and clean up its memory.
#[wasm_bindgen]
pub fn accessor_drop(accessor: *mut ArrowAccessor) {
    if !accessor.is_null() {
        unsafe {
            Box::from_raw(accessor);
        };
    }
}

// Rust panic! will call into the JS console on WASM instantiation.
#[wasm_bindgen(start)]
pub fn main_js() {
    set_panic_hook();
}
