////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2018, the Perspective Authors.
//
// This file is part of the Perspective library, distributed under the terms
// of the Apache License 2.0.  The full license can be found in the LICENSE
// file.

use crate::config::*;
use crate::dragdrop::*;
use crate::renderer::*;
use crate::session::*;
use crate::utils::*;

use std::rc::Rc;
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use wasm_bindgen_futures::future_to_promise;
use web_sys::*;
use yew::prelude::*;

#[derive(Properties, Clone)]
pub struct ColumnSelectorProps {
    pub session: Session,
    pub renderer: Renderer,
    pub dragdrop: DragDrop,
}

pub enum ColumnSelectorMsg {
    TableLoaded,
    ViewCreated,
    HoverActiveIndex(Option<i32>),
    Drop(String, DropAction),
}

#[derive(Clone)]
pub struct ColumnSelector {
    props: ColumnSelectorProps,
    link: ComponentLink<ColumnSelector>,
    subscriptions: Rc<[Subscription; 3]>,
}

impl Component for ColumnSelector {
    type Message = ColumnSelectorMsg;
    type Properties = ColumnSelectorProps;

    fn create(props: Self::Properties, link: ComponentLink<Self>) -> Self {
        let cb = link.callback(|_| ColumnSelectorMsg::TableLoaded);
        let table_sub = props.session.add_on_table_loaded(cb);

        let cb = link.callback(|_| ColumnSelectorMsg::ViewCreated);
        let view_sub = props.session.add_on_view_created(cb);

        let cb =
            link.callback(|x: (String, DropAction)| ColumnSelectorMsg::Drop(x.0, x.1));
        let drop_sub = props.dragdrop.add_on_drop_action(cb);

        let subscriptions = Rc::new([table_sub, view_sub, drop_sub]);

        ColumnSelector {
            props,
            link,
            subscriptions,
        }
    }

    fn update(&mut self, msg: Self::Message) -> ShouldRender {
        match msg {
            ColumnSelectorMsg::TableLoaded => true,
            ColumnSelectorMsg::ViewCreated => true,
            ColumnSelectorMsg::HoverActiveIndex(index) => match index {
                Some(index) => self.props.dragdrop.set_active_column_index(index),
                None => {
                    self.props.dragdrop.reset_hover_column();
                    true
                }
            },
            ColumnSelectorMsg::Drop(column, DropAction::Active(index)) => {
                insert_column(column, index, &self.props.session, &self.props.renderer);
                false
            }
            ColumnSelectorMsg::Drop(_, _) => false,
        }
    }

    fn change(&mut self, _props: Self::Properties) -> ShouldRender {
        true
    }

    fn view(&self) -> Html {
        if self.props.session.get_view().is_none() {
            return html! {};
        }

        if let Some(all_columns) = self.props.session.get_all_columns() {
            let config = self.props.session.get_view_config();
            let is_dragover_column = self.props.dragdrop.is_dragover_active();

            let inactive_columns = all_columns
                .into_iter()
                .filter(|name| !config.columns.contains(&Some(name.to_string())))
                .filter(|name| {
                    is_dragover_column
                        .as_ref()
                        .map(|x| &x.1 != name)
                        .unwrap_or(true)
                });

            let is_pivot = config.is_pivot();
            let active_columns = match self.props.dragdrop.is_dragover_active() {
                Some((index, column)) => config
                    .columns
                    .iter()
                    .filter(|x| match x {
                        Some(x) => *x != column,
                        None => true,
                    })
                    .take(index as usize)
                    .chain([None].iter())
                    .chain(
                        config
                            .columns
                            .iter()
                            .filter(|x| match x {
                                Some(x) => *x != column,
                                None => true,
                            })
                            .skip(index as usize),
                    )
                    .cloned()
                    .collect::<Vec<Option<String>>>(),
                _ => config.columns.clone(),
            };

            let dragleave = dragleave_helper({
                let link = self.link.clone();
                move || link.send_message(ColumnSelectorMsg::HoverActiveIndex(None))
            });

            let ondrop = Callback::from({
                let dragdrop = self.props.dragdrop.clone();
                move |_| dragdrop.notify_drop()
            });

            let dragover = Callback::from(|_event: DragEvent| _event.prevent_default());

            html! {
                <>
                    <div
                        id="active_columns"
                        ondragover={ dragover }
                        ondragleave={ dragleave }
                        ondrop={ ondrop }>{
                        for active_columns.iter().enumerate().map(|(idx, name)| {
                            let dragenter = Callback::from({
                                let link = self.link.clone();
                                move |event: DragEvent| {
                                    let target = event.current_target().unwrap();
                                    let index = target.unchecked_into::<HtmlElement>().dataset().get("index").unwrap();
                                    link.send_message(ColumnSelectorMsg::HoverActiveIndex(Some(index.parse::<i32>().unwrap())))
                                }
                            });


                            let pivot_class = if is_pivot {
                                "show-aggregate column_selector_draggable"
                            } else {
                                "column_selector_draggable"
                            };

                            match name {
                                None => {
                                    let name = self.props.dragdrop.hover_column().unwrap();
                                    let col_type = self.props.session.get_column_type(&name).expect("Unknown column");
                                    html! {
                                        <div
                                            class="column_selector_column"
                                            data-index={ idx.to_string() }
                                            ondragenter=dragenter>

                                            <span class="is_column_active"></span>
                                            <div style="opacity:0" class=pivot_class>
                                                <span class={ format!("column_name {}", col_type) }>{ name }</span>
                                                {
                                                    if is_pivot {
                                                        html! { <span style="opacity:0">{ "DROP" }</span> }
                                                    } else {
                                                        html! {}
                                                    }
                                                }
                                            </div>
                                        </div>
                                    }
                                },
                                Some(name) => {
                                    let col_type = self.props.session.get_column_type(&name).expect("Unknown column");
                                    let remove_column = Callback::from({
                                        let event_name = name.to_owned();
                                        let session = self.props.session.clone();
                                        let renderer = self.props.renderer.clone();
                                        move |event: MouseEvent| deactivate_column(event_name.to_owned(), event.shift_key(), &session, &renderer)
                                    });

                                    let noderef = NodeRef::default();

                                    let dragstart = Callback::from({
                                        let event_name = name.to_owned();
                                        let noderef = noderef.clone();
                                        let dragdrop = self.props.dragdrop.clone();
                                        move |event: DragEvent| {
                                            let elem = noderef.cast::<HtmlElement>().unwrap();
                                            event.data_transfer().unwrap().set_drag_image(&elem, 0, 0);
                                            dragdrop.set_hover_column(event_name.to_string())
                                        }
                                    });

                                    let dragend = Callback::from({
                                        let dragdrop = self.props.dragdrop.clone();
                                        move |_event| dragdrop.reset()
                                    });


                                    html! {
                                        <div
                                            class="column_selector_column"
                                            data-index={ idx.to_string() }
                                            ondragenter=dragenter>

                                            <span class="is_column_active" onclick=remove_column></span>
                                            <div
                                                class=pivot_class
                                                draggable="true"
                                                ondragstart=dragstart
                                                ondragend=dragend>

                                                <span ref=noderef.clone() class={ format!("column_name {}", col_type) }>{ name }</span>
                                                {
                                                    if is_pivot {
                                                        let aggregate = config.aggregates.get(name).unwrap_or(&Aggregate::SingleAggregate(SingleAggregate::Count));
                                                        html! { <span>{ format!("{}", aggregate) }</span> }
                                                    } else {
                                                        html! {}
                                                    }
                                                }
                                            </div>
                                        </div>
                                    }
                                }
                            }
                        })
                    }</div>

                    <div id="inactive_columns">{
                        for inactive_columns.enumerate().map(|(idx, name)| {
                            let col_type = self.props.session.get_column_type(&name).expect("Unknown column");
                            let add_column = Callback::from({
                                let event_name = name.to_owned();
                                let session = self.props.session.clone();
                                let renderer = self.props.renderer.clone();
                                move |event: MouseEvent| activate_column(event_name.to_owned(), event.shift_key(), &session, &renderer)
                            });

                            let noderef = NodeRef::default();

                            let dragstart = Callback::from({
                                let event_name = name.to_owned();
                                let noderef = noderef.clone();
                                let dragdrop = self.props.dragdrop.clone();
                                move |_event: DragEvent| {
                                    let elem = noderef.cast::<HtmlElement>().unwrap();
                                    _event.data_transfer().unwrap().set_drag_image(&elem, 0, 0);
                                    dragdrop.set_hover_column(event_name.to_string())
                                }
                            });

                            let dragend = Callback::from({
                                let dragdrop = self.props.dragdrop.clone();
                                move |_event| dragdrop.reset()
                            });

                            html! {
                                <div
                                    class="column_selector_column"
                                    data-index={ idx.to_string() }>
                                    <span class="is_column_active" onclick=add_column></span>
                                    <div class="column_selector_draggable"
                                        draggable="true"
                                        ondragstart=dragstart
                                        ondragend=dragend>

                                        <span ref=noderef.clone() class={ format!("column_name {}", col_type) }>{ name }</span>
                                    </div>
                                </div>
                            }
                        })
                    }</div>
                    <div
                        id="add-expression"
                        class="side_panel-action">
                        <span class="psp-icon psp-icon__add"></span>
                        <span class="psp-title__columnName">{ "New Column" }</span>
                    </div>
                </>
            }
        } else {
            html! {}
        }
    }
}

fn activate_column(name: String, shift: bool, session: &Session, renderer: &Renderer) {
    let ViewConfig { mut columns, .. } = session.get_view_config();
    if shift {
        columns.clear();
        columns.push(Some(name));
    } else {
        columns.retain(|x| x.as_ref() != Some(&name));
        columns.push(Some(name));
    }

    render(columns, session, renderer);
}

fn deactivate_column(
    name: String,
    shift: bool,
    session: &Session,
    renderer: &Renderer,
) {
    let ViewConfig { mut columns, .. } = session.get_view_config();
    if !shift && columns.len() > 1 {
        columns.retain(|x| x.as_ref() != Some(&name));
        render(columns, session, renderer);
    } else if shift {
        columns.clear();
        columns.push(Some(name));
        render(columns, session, renderer);
    }
}

fn insert_column(name: String, index: i32, session: &Session, renderer: &Renderer) {
    let ViewConfig { mut columns, .. } = session.get_view_config();
    columns.retain(|x| x.as_ref() != Some(&name));
    columns.insert(index as usize, Some(name));
    render(columns, session, renderer);
}

fn render(columns: Vec<Option<String>>, session: &Session, renderer: &Renderer) {
    let columns = Some(columns);
    let update = ViewConfigUpdate {
        columns,
        ..ViewConfigUpdate::default()
    };
    let session = session.clone();
    let renderer = renderer.clone();
    let _ = future_to_promise(async move {
        drop(session.create_view(update).await?);
        drop(renderer.draw(async { &session }).await?);
        Ok(JsValue::UNDEFINED)
    });
}
