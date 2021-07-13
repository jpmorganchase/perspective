////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2018, the Perspective Authors.
//
// This file is part of the Perspective library, distributed under the terms
// of the Apache License 2.0.  The full license can be found in the LICENSE
// file.

// use crate::*;

use js_sys::Array;
use serde::Deserialize;
// use std::collections::HashMap;
// use std::iter::FromIterator;
// use std::iter::IntoIterator;
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;

// #[cfg(test)]
// use wasm_bindgen_test::*;

// `wasm-bindgen` only supports `JsValue` return types from `extern async fn`, so use
// this macro to generate well-typed versions.
macro_rules! async_typed {
    (@jsvalue $sym:ident ()) => {{ $sym.await?; }};
    (@jsvalue $sym:ident f64) => { $sym.await?.as_f64().unwrap() };
    (@jsvalue $sym:ident $type:ty) => { $sym.await?.unchecked_into::<$type>() };
    ($old_fn:ident, $new_fn:ident(&self $(, $arg:ident : $argtype:ty)*) -> $($type:tt)+) => {
        pub async fn $new_fn(&self, $($arg: $argtype),*) -> Result<$($type)*, JsValue> {
            let fut = self.$old_fn($($arg)*);
            Ok(async_typed!(@jsvalue fut $($type)*))
        }
    };
    ($old_fn:ident, $new_fn:ident(self $(, $arg:ident : $argtype:ty)*) -> $($type:tt)+) => {
        pub async fn $new_fn(self, $($arg: $argtype),*) -> Result<$($type)*, JsValue> {
            let fut = self.$old_fn($($arg)*);
            Ok(async_typed!(@jsvalue fut $($type)*))
        }
    };
}

/// Perspective FFI
#[wasm_bindgen]
#[rustfmt::skip]
extern "C" {

    #[derive(Clone)]
    pub type JsPerspectiveWorker;

    #[wasm_bindgen(method, catch, js_name = delete)]
    pub async fn _delete(this: &JsPerspectiveWorker) -> Result<JsValue, JsValue>;

    #[wasm_bindgen(method, catch, js_name = table)]
    pub async fn _table(
        this: &JsPerspectiveWorker,
        data: js_sys::Object
    ) -> Result<JsValue, JsValue>;

    #[derive(Clone)]
    pub type JsPerspectiveTable;

    pub type JsPerspectiveTableSchema;

    #[wasm_bindgen(method, catch, js_name = columns)]
    pub async fn _columns(this: &JsPerspectiveTable) -> Result<JsValue, JsValue>;

    #[wasm_bindgen(method, catch, js_name = delete)]
    pub async fn _delete(this: &JsPerspectiveTable) -> Result<JsValue, JsValue>;

    #[wasm_bindgen(method, catch, js_name = schema)]
    pub async fn _schema(this: &JsPerspectiveTable) -> Result<JsValue, JsValue>;

    #[wasm_bindgen(method, catch, js_name = size)]
    pub async fn _size(this: &JsPerspectiveTable) -> Result<JsValue, JsValue>;

    #[wasm_bindgen(method, catch, js_name = validate_expressions)]
    pub async fn _validate_expressions(this: &JsPerspectiveTable, exprs: Array) -> Result<JsValue, JsValue>;

    #[wasm_bindgen(method, catch, js_name = view)]
    pub async fn _view(
        this: &JsPerspectiveTable, 
        config: &JsPerspectiveViewConfig
    ) -> Result<JsValue, JsValue>;

    pub type JsPerspectiveView;

    #[wasm_bindgen(method, catch, js_name = delete)]
    pub async fn _delete(this: &JsPerspectiveView) -> Result<JsValue, JsValue>;

    #[wasm_bindgen(method, catch, js_name = to_csv)]
    pub async fn _to_csv(
        this: &JsPerspectiveView,
        options: js_sys::Object,
    ) -> Result<JsValue, JsValue>;

    #[wasm_bindgen(method, catch, js_name = num_rows)]
    pub async fn _num_rows(this: &JsPerspectiveView) -> Result<JsValue, JsValue>;

    #[wasm_bindgen(method)]
    pub fn on_update(this: &JsPerspectiveView, callback: &js_sys::Function);

    #[wasm_bindgen(method)]
    pub fn remove_update(this: &JsPerspectiveView, callback: &js_sys::Function);

    #[wasm_bindgen(method, catch, js_name = schema)]
    pub async fn _schema(this: &JsPerspectiveView) -> Result<JsValue, JsValue>;

    #[wasm_bindgen(method, catch, js_name = num_columns)]
    pub async fn _num_columns(this: &JsPerspectiveView) -> Result<JsValue, JsValue>;

    // #[wasm_bindgen(method, catch, js_name = get_config)]
    // pub async fn _get_config(this: &JsPerspectiveView) -> Result<JsValue, JsValue>;

    pub type JsPerspectiveViewConfig;

    pub type JsPerspectiveViewConfigUpdate;

    // #[wasm_bindgen(method, getter)]
    // pub fn row_pivots(this: &JsPerspectiveViewConfig) -> js_sys::Array;

    // #[wasm_bindgen(method, getter)]
    // pub fn column_pivots(this: &JsPerspectiveViewConfig) -> js_sys::Array;

    pub type JsPerspectiveValidatedExpressions;

    #[wasm_bindgen(method, getter)]
    pub fn errors(this: &JsPerspectiveValidatedExpressions) -> js_sys::Object;
}

impl JsPerspectiveWorker {
    async_typed!(_delete, delete(&self) -> ());
    async_typed!(_table, table(&self, data: js_sys::Object) -> JsPerspectiveTable);
}

impl JsPerspectiveTable {
    async_typed!(_columns, columns(&self) -> js_sys::Array);
    async_typed!(_delete, delete(&self) -> ());
    async_typed!(_validate_expressions, validate_expressions(&self, exprs: Array) -> JsPerspectiveValidatedExpressions);
    async_typed!(_schema, schema(&self) -> JsPerspectiveTableSchema);
    async_typed!(_view, view(&self, config: &JsPerspectiveViewConfig) -> JsPerspectiveView);
    async_typed!(_size, size(&self) -> f64);
}

impl JsPerspectiveView {
    async_typed!(_to_csv, to_csv(&self, options: js_sys::Object) -> js_sys::JsString);
    async_typed!(_num_rows, num_rows(&self) -> f64);
    async_typed!(_num_columns, num_columns(&self) -> f64);
    async_typed!(_schema, schema(&self) -> js_sys::Object);
    async_typed!(_delete, delete(self) -> ());
    // async_typed!(_get_config, get_config(&self) -> JsPerspectiveViewConfig);
}

#[derive(Deserialize)]
#[serde()]
pub struct PerspectiveValidationError {
    pub error_message: String,
    pub line: u32,
    pub column: u32,
}
