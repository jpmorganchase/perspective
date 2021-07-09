////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2018, the Perspective Authors.
//
// This file is part of the Perspective library, distributed under the terms
// of the Apache License 2.0.  The full license can be found in the LICENSE
// file.

use crate::components::plugin_selector::*;
use crate::js::perspective_viewer::*;
use crate::plugin::*;
use crate::session::*;
use crate::utils::WeakComponentLink;
use crate::*;

use super::test_plugin::*;

use std::cell::RefCell;
use std::rc::Rc;
use wasm_bindgen_test::*;
use yew::prelude::*;

wasm_bindgen_test::wasm_bindgen_test_configure!(run_in_browser);

#[wasm_bindgen_test]
pub fn test_plugin_selected() {
    register_test_components().unwrap();
    let link: WeakComponentLink<PluginSelector> = WeakComponentLink::default();
    let result: Rc<RefCell<Option<PerspectiveViewerJsPlugin>>> =
        Rc::new(RefCell::new(None));
    let session = Session::new();
    let plugin = Plugin::new(session);
    plugin.add_on_plugin_changed({
        clone!(result);
        move |val| {
            *result.borrow_mut() = Some(val);
        }
    });

    test_html! {
        <PluginSelector
            plugin=plugin.clone()
            weak_link=link.clone()>

        </PluginSelector>
    };

    let plugin_selector = link.borrow().clone().unwrap();
    plugin_selector
        .send_message(PluginSelectorMsg::PluginSelected("Debug B".to_owned()));

    assert_eq!(
        result.borrow().as_ref().map(|x| x.name()),
        Some("Debug A".to_owned())
    );

    assert_eq!(plugin.get_plugin(None).unwrap().name(), "Debug A");
}
