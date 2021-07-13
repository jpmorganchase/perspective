////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2018, the Perspective Authors.
//
// This file is part of the Perspective library, distributed under the terms
// of the Apache License 2.0.  The full license can be found in the LICENSE
// file.

use crate::dragdrop::*;
use crate::renderer::*;
use crate::utils::*;
use crate::*;
use crate::{components::vieux::*, session::Session};

use futures::channel::oneshot::*;
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use wasm_bindgen_test::*;
use web_sys::*;
use yew::prelude::*;

wasm_bindgen_test::wasm_bindgen_test_configure!(run_in_browser);

fn set_up_html() -> (
    WeakComponentLink<PerspectiveVieux>,
    web_sys::ShadowRoot,
    Session,
) {
    let link: WeakComponentLink<PerspectiveVieux> = WeakComponentLink::default();
    let root = NodeRef::default();
    let document = window().unwrap().document().unwrap();
    let elem: HtmlElement = document.create_element("div").unwrap().unchecked_into();
    let session = Session::default();
    let renderer = Renderer::new(elem.clone(), session.clone());
    let dragdrop = DragDrop::default();
    test_html! {
        <PerspectiveVieux
            weak_link=link.clone()
            ref=root.clone()
            elem=elem
            dragdrop=dragdrop
            renderer=renderer
            session=session.clone()>
        </PerspectiveVieux>
    };

    let root: web_sys::ShadowRoot = root
        .cast::<HtmlElement>()
        .unwrap()
        .parent_node()
        .unwrap()
        .unchecked_into();

    (link, root, session)
}

#[wasm_bindgen_test]
pub fn test_settings_closed() {
    let (_, root, _) = set_up_html();
    for selector in ["slot[name=main_panel]", "#config_button"].iter() {
        assert!(root
            .query_selector(selector)
            .unwrap()
            .unwrap()
            .is_connected());
    }

    assert_eq!(root.query_selector("#app_panel").unwrap(), None);
}

#[wasm_bindgen_test]
pub async fn test_settings_open() {
    let (link, root, _) = set_up_html();
    let vieux = link.borrow().clone().unwrap();
    vieux.send_message(Msg::ToggleConfig(Some(true), None));
    let (sender, receiver) = channel::<()>();
    vieux.send_message(Msg::ToggleConfigFinished(sender));
    receiver.await.unwrap();
    for selector in [
        "#app_panel",
        "slot[name=main_panel]",
        "#config_button",
        "#status_bar",
    ]
    .iter()
    {
        assert!(root
            .query_selector(selector)
            .unwrap()
            .unwrap()
            .is_connected());
    }
}

#[wasm_bindgen_test]
pub async fn test_load_table() {
    let (link, root, session) = set_up_html();
    let table = get_mock_table().await;
    let vieux = link.borrow().clone().unwrap();
    vieux.send_message(Msg::ToggleConfig(Some(true), None));
    session.set_table(table).await.unwrap();
    assert_eq!(
        root.query_selector("#rows").unwrap().unwrap().inner_html(),
        "<span>3 rows</span>"
    );
}
