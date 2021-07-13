////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2018, the Perspective Authors.
//
// This file is part of the Perspective library, distributed under the terms
// of the Apache License 2.0.  The full license can be found in the LICENSE
// file.

// `rustfmt` removes `async` from extern blocks in rust stable
// [issue](https://github.com/rust-lang/rustfmt/issues/4288)

use serde::Serialize;
use wasm_bindgen::prelude::*;
use web_sys::HtmlElement;

pub enum KeyMod {
    Shift = 1024,
}

pub enum KeyCode {
    Enter = 3,
}

#[cfg_attr(
    not(test),
    wasm_bindgen(module = "monaco-editor/esm/vs/editor/editor.worker")
)]
#[cfg_attr(test, wasm_bindgen(inline_js = "export default function() {}"))]
extern "C" {
    #[wasm_bindgen(js_name = "default")]
    pub type EditorWorker;

    #[wasm_bindgen(constructor, js_class = "default")]
    pub fn new() -> EditorWorker;
}

#[cfg_attr(
    not(test),
    wasm_bindgen(inline_js = "
    export async function monaco_module() {
        return import(
            /* webpackChunkName: \"monaco-exts\" */
            /* webpackMode: \"eager\" */
            '../../../../src/js/monaco.js'
        ); 
    }
")
)]
#[cfg_attr(
    test,
    wasm_bindgen(inline_js = "export async function monaco_module() {}")
)]
#[rustfmt::skip]
extern "C" {
    #[wasm_bindgen(js_name = "monaco_module")]
    pub async fn monaco_exts();
}

#[cfg_attr(
    not(test),
    wasm_bindgen(inline_js = "
    export async function monaco_module() { 
        return import(
            /* webpackChunkName: \"monaco\" */
            /* webpackMode: \"eager\" */
            'monaco-editor/esm/vs/editor/editor.api'
        ); 
    }
")
)]
#[cfg_attr(
    test,
    wasm_bindgen(inline_js = "export async function monaco_module() {}")
)]
#[rustfmt::skip]
extern "C" {
    pub type MonacoModule;

    #[wasm_bindgen(js_name = "monaco_module")]
    pub async fn monaco_module() -> JsValue;

    #[wasm_bindgen(method, getter)]
    pub fn editor(this: &MonacoModule) -> Editor;

    #[wasm_bindgen(method, getter)]
    pub fn languages(this: &MonacoModule) -> Languages;

    #[derive(Clone)]
    pub type Editor;

    #[wasm_bindgen(method)]
    pub fn create(
        this: &Editor,
        container: HtmlElement,
        options: JsValue,
    ) -> JsMonacoEditor;

    #[wasm_bindgen(method, js_name = "defineTheme")]
    pub fn define_theme(this: &Editor, id: &str, options: JsValue);

    #[wasm_bindgen(method, js_name = "setModelMarkers")]
    pub fn set_model_markers(
        this: &Editor,
        model: &JsMonacoModel,
        id: &str,
        errors: &js_sys::Array,
    );

    pub type Languages;

    #[wasm_bindgen(method)]
    pub fn register(this: &Languages, options: JsValue);

    #[wasm_bindgen(method, js_name = "setMonarchTokensProvider")]
    pub fn set_monarch_tokens_provider(this: &Languages, id: &str, options: JsValue);

    #[wasm_bindgen(method, js_name = "setLanguageConfiguration")]
    pub fn set_language_configuration(this: &Languages, id: &str, options: JsValue);

    #[wasm_bindgen(method, js_name = "registerCompletionItemProvider")]
    pub fn register_completion_item_provider(
        this: &Languages,
        id: &str,
        options: JsValue,
    );

    #[derive(Clone)]
    pub type JsMonacoEditor;

    #[wasm_bindgen(method, js_name = "getModel")]
    pub fn get_model(this: &JsMonacoEditor) -> JsMonacoModel;

    #[wasm_bindgen(method, js_name = "getValue")]
    pub fn get_value(this: &JsMonacoEditor) -> JsValue;

    #[wasm_bindgen(method, js_name = "setValue")]
    pub fn set_value(this: &JsMonacoEditor, value: &str);

    #[wasm_bindgen(method, js_name = "addCommand")]
    pub fn add_command(this: &JsMonacoEditor, key_code: u32, value: &js_sys::Function);

    // #[wasm_bindgen(method, js_name = "getValue")]
    // pub fn get_value_str(this: &JsMonacoEditor) -> String;

    #[wasm_bindgen(method)]
    pub fn focus(this: &JsMonacoEditor);

    pub type JsMonacoModel;

    #[wasm_bindgen(method, js_name = "onDidChangeContent")]
    pub fn on_did_change_content(this: &JsMonacoModel, callback: &js_sys::Function);

    #[wasm_bindgen(method, js_name = "getWordUntilPosition")]
    pub fn get_word_until_position(
        this: &JsMonacoModel,
        position: &JsMonacoPosition,
    ) -> JsValue;

    #[wasm_bindgen(method, js_name = "getLineTokens")]
    pub fn get_line_tokens(this: &JsMonacoModel, line_number: u32) -> JsMonacoTokens;

    pub type JsMonacoPosition;

    #[wasm_bindgen(method, getter, js_name = "lineNumber")]
    pub fn line_number(this: &JsMonacoPosition) -> u32;

    #[wasm_bindgen(method, getter, js_name = "column")]
    pub fn column(this: &JsMonacoPosition) -> u32;

    pub type JsMonacoTokens;

    #[wasm_bindgen(method, js_name = "findTokenIndexAtOffset")]
    pub fn find_token_index_at_offset(this: &JsMonacoTokens, offset: u32) -> u32;

    #[wasm_bindgen(method, js_name = "getClassName")]
    pub fn get_class_name(this: &JsMonacoTokens, index: u32) -> JsValue;

    #[wasm_bindgen(method, js_name = "getStandardTokenType")]
    pub fn get_standard_token_type(this: &JsMonacoTokens, index: u32) -> u32;

    pub type JsMonacoTriggerToken;

    #[wasm_bindgen(method, getter, js_name = "triggerKind")]
    pub fn trigger_kind(this: &JsMonacoTriggerToken) -> u32;

    #[wasm_bindgen(method, getter, js_name = "triggerCharacter")]
    pub fn trigger_character(this: &JsMonacoTriggerToken) -> String;
}

// Serde does not support closures and wasm_bindgen does not support anonymous object
// construction without `Reflect`, so this signature is not possible and is instead
// constructed manually.

// #[derive(Serialize)]
// #[serde(rename_all = "camelCase")]
// pub struct RegisterCompletionItemProviderArgs {
//     pub provider_completion_items: Closure< ... >,
// }

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct RegisterCompletionItemSuggestions {
    pub suggestions: Vec<CompletionItemSuggestion>,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct CompletionItemSuggestion {
    pub label: String,
    pub kind: u32,
    pub insert_text: String,
    pub insert_text_rules: u32,
    pub documentation: String,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct EditorArgs {
    pub theme: &'static str,
    pub value: &'static str,
    pub language: &'static str,
    pub automatic_layout: bool,
    pub minimap: MinimapArgs,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct MinimapArgs {
    pub enabled: bool,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct RegisterArgs {
    pub id: &'static str,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct MonarchTokensProviderArgs<'a> {
    pub tokenizer: MonarchTokenizer<'a>,
    pub brackets: Vec<Vec<&'a str>>,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct LanguageConfigurationArgs<'a> {
    pub auto_closing_pairs: Vec<AutoClosingPairs<'a>>,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct AutoClosingPairs<'a> {
    pub open: &'a str,
    pub close: &'a str,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct MonarchTokenizer<'a> {
    pub root: Vec<Vec<&'a str>>,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct DefineThemeArgs<'a> {
    pub base: &'a str,
    pub inherit: bool,
    pub rules: Vec<DefineThemeToken<'a>>,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct DefineThemeToken<'a> {
    pub token: &'a str,
    pub foreground: &'a str,
    pub font_style: Option<&'a str>,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct JsMonacoModelMarker<'a> {
    pub code: String,
    pub start_line_number: u32,
    pub end_line_number: u32,
    pub start_column: u32,
    pub end_column: u32,
    pub severity: &'a str,
    pub message: String,
}
