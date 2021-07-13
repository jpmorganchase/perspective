////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2018, the Perspective Authors.
//
// This file is part of the Perspective library, distributed under the terms
// of the Apache License 2.0.  The full license can be found in the LICENSE
// file.

use crate::dragdrop::*;

use web_sys::*;
use yew::prelude::*;
use std::rc::Rc;

#[derive(Properties)]
pub struct PivotSelectorProps<T: Component, U: PartialEq + Clone + 'static> {
    pub parent: ComponentLink<T>,
    pub dragdrop: DragDrop,
    pub columns: Vec<U>,
    pub name: &'static str,
    pub dragenter: fn(i32) -> T::Message,
    pub close: fn(i32) -> T::Message,
    pub dragleave: fn() -> T::Message,
    pub render: Rc<dyn Fn(usize, &U) -> Html>,

    #[prop_or_default]
    pub is_dragover: Option<(i32, U)>,

    #[prop_or_default]
    pub allow_duplicates: bool,
}

/// Cannot be derived, else `T` and associated types must implement `Clone`.
impl<T: Component, U: PartialEq + Clone> Clone for PivotSelectorProps<T, U> {
    fn clone(&self) -> Self {
        PivotSelectorProps {
            is_dragover: self.is_dragover.clone(),
            parent: self.parent.clone(),
            dragdrop: self.dragdrop.clone(),
            columns: self.columns.clone(),
            name: self.name,
            dragenter: self.dragenter,
            close: self.close,
            dragleave: self.dragleave,
            allow_duplicates: self.allow_duplicates,
            render: self.render.clone(),
        }
    }
}

pub enum PivotSelectorMsg {
    Freeze(bool)
}

/// A sub-selector for a list-like component of a `JsViewConfig`.
pub struct PivotSelector<T: Component + 'static, U: PartialEq + Clone + 'static> {
    props: PivotSelectorProps<T, U>,
    link: ComponentLink<Self>,
    elem: NodeRef,
    frozen_size: Option<i32>
}

impl<T: Component + 'static, U: Clone + PartialEq + 'static> Component for PivotSelector<T, U> {
    type Message = PivotSelectorMsg;
    type Properties = PivotSelectorProps<T, U>;

    fn create(props: Self::Properties, link: ComponentLink<Self>) -> Self {
        PivotSelector { props, link, elem: NodeRef::default(), frozen_size: None }
    }

    fn update(&mut self, msg: Self::Message) -> ShouldRender {
        match msg {

            // When a dragover occurs and a new Column is inserted into the selector,
            // the geometry of the selector may expand and cause a parent reflow,
            // which annoyingly changes the drag status and glitchiness occurs.
            // By using `Freeze` when a dragenter occurs, the element's width will be
            // frozen until `drop` or `dragleave`.
            PivotSelectorMsg::Freeze(freeze) => {
                if freeze && self.frozen_size.is_none() {
                    let elem = self.elem.cast::<HtmlElement>().unwrap();
                    self.frozen_size = Some(elem.client_width());
                } else if !freeze {
                    self.frozen_size = None;
                }
            }
        }
        true
    }

    /// Should always render on change, as this component only depends on the props from
    /// its parent and has no `Msg` enums.
    fn change(&mut self, props: Self::Properties) -> ShouldRender {
        self.props = props;
        self.frozen_size = None;
        true
    }

    fn view(&self) -> Html {
        let dragover = Callback::from(|_event: DragEvent| _event.prevent_default());

        // On dragleave, signal the parent but no need to redraw as parent will call
        // `change()` when resetting props.
        let dragleave = dragleave_helper({
            let parent = self.props.parent.clone();
            let dragleave = self.props.dragleave;
            move || parent.send_message(dragleave())     
        });

        let drop = Callback::from({
            let dragdrop = self.props.dragdrop.clone();
            move |_| {
                dragdrop.notify_drop();
            }
        });

        let columns_html = {
            let mut columns = self
                .props
                .columns
                .iter()
                .map(|x| ("", x))
                .collect::<Vec<(&str, &U)>>();

            if let Some((x, column)) = &self.props.is_dragover {
                let index = *x as usize;
                if !self.props.allow_duplicates {
                    columns.retain(|x| x.1 != column);
                }

                // If inserting into the middle of the list, use
                // the length of the existing element to prevent
                // jitter as the underlying dragover zone moves.
                if index < columns.len() {
                    columns.insert(
                        index,
                        ("config-drop", columns.get(index).as_ref().unwrap().1),
                    );
                } else {
                    columns.push(("config-drop", column));
                }
            }

            columns.iter().enumerate().map(|(idx, (class, column))| {
                let close_cb = self.props.parent.callback({
                    let close = self.props.close;
                    move |_| close(idx as i32)
                });

                let dragenter = self.props.parent.callback({
                    let dragenter = self.props.dragenter;
                    let link = self.link.clone();
                    move |event: DragEvent| {                    
                        event.stop_propagation();
                        link.send_message(PivotSelectorMsg::Freeze(true));
                        dragenter(idx as i32)
                    }
                });

                html! {
                    <div class={ format!("pivot-column {}", class) } ondragenter=dragenter>
                        {{
                            (*self.props.render)(idx, &column) 
                        }}
                        <span class="row_close" onclick=close_cb></span>
                    </div>
                }
            }).collect::<Html>()
        };

        let total = self.props.columns.len() as i32;
        let dragenter = self.props.parent.callback({
            let dragenter = self.props.dragenter;
            let link = self.link.clone();
            move |event: DragEvent| {                    
                event.stop_propagation();
                link.send_message(PivotSelectorMsg::Freeze(true));
                dragenter(total)
            }
        });

        let style = match self.frozen_size {
            Some(x) => format!("width:{}px", x),
            None => "".to_owned()
        };

        html! {
            <div style=style ref=self.elem.clone() class="rrow">
                <div
                    id={ self.props.name }
                    ondragover=dragover
                    ondragenter=dragenter
                    ondragleave=dragleave
                    ondrop=drop>

                    <div class="psp-text-field">
                        <ul class="psp-text-field__input" for={ self.props.name }>
                            { columns_html }
                        </ul>
                        <label for={ self.props.name }></label>
                    </div>
                </div>
            </div>
        }
    }
}
