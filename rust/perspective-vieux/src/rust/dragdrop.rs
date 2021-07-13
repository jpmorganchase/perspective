////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2018, the Perspective Authors.
//
// This file is part of the Perspective library, distributed under the terms
// of the Apache License 2.0.  The full license can be found in the LICENSE
// file.

use crate::config::*;
use crate::utils::*;

use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;

use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::*;
use yew::prelude::*;

// TODO make these usize and convert just as needed by wasm_bindgen
#[derive(Clone, Copy)]
pub enum DropAction {
    Active(i32),
    RowPivots(i32),
    ColumnPivots(i32),
    Sort(i32),
    Filter(i32),
}

#[derive(Clone)]
pub struct DragState {
    column: String,
    state: Option<DropAction>,
}

#[derive(Clone)]
pub struct DragDropState {
    drag_state: Option<DragState>,
    on_drop_action: PubSub<(String, DropAction)>,
}

/// The `<perspective-viewer>` drag-drop service, which manages drag/drop user
/// interactions across components.  It is a component-level service, since only one
/// drag/drop action can be executed by the user at a time, and has a 3 states:
/// - `None` No drag/drop action is in effect.
/// - `Some(DragDropState { state: None }` Drag is in effect.
/// - `Some(DragDropState { state: Some(_) }` Drag and Hover are in effect.
#[derive(Clone)]
pub struct DragDrop(Rc<RefCell<DragDropState>>);

impl Deref for DragDrop {
    type Target = Rc<RefCell<DragDropState>>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Default for DragDrop {
    fn default() -> Self {
        DragDrop(Rc::new(RefCell::new(DragDropState {
            drag_state: None,
            on_drop_action: PubSub::default(),
        })))
    }
}

impl DragDrop {
    pub fn set_hover_column(&self, column: String) {
        self.borrow_mut().drag_state = Some(DragState {
            column,
            state: None,
        });
    }

    pub fn reset(&self) {
        self.borrow_mut().drag_state = None;
    }

    pub fn add_on_drop_action(
        &self,
        callback: Callback<(String, DropAction)>,
    ) -> Subscription {
        self.borrow().on_drop_action.add_listener(callback)
    }

    pub fn notify_drop(&self) {
        let action = match self.borrow().drag_state {
            Some(DragState {
                ref column,
                state: Some(action),
                ..
            }) => Some((column.to_owned(), action)),
            _ => None,
        };

        if let Some(action) = action {
            let pubsub = self.borrow().on_drop_action.clone();
            pubsub.emit_all(action);
        }

        self.reset();
    }

    pub fn hover_column(&self) -> Option<String> {
        match self.borrow().drag_state {
            Some(DragState { ref column, .. }) => Some(column.clone()),
            _ => None,
        }
    }

    pub fn reset_hover_column(&self) {
        match self.hover_column() {
            Some(column) => self.set_hover_column(column),
            None => self.reset(),
        }
    }

    pub fn set_row_pivots(&self, index: i32) {
        let mut x = self.borrow_mut();
        x.drag_state.as_mut().expect("Hover Index").state =
            Some(DropAction::RowPivots(index));
    }

    pub fn set_column_pivots(&self, index: i32) {
        let mut x = self.borrow_mut();
        x.drag_state.as_mut().expect("Hover Index").state =
            Some(DropAction::ColumnPivots(index));
    }

    pub fn set_sort(&self, index: i32) {
        let mut x = self.borrow_mut();
        x.drag_state.as_mut().expect("Hover Index").state =
            Some(DropAction::Sort(index));
    }

    pub fn set_filter(&self, index: i32) {
        let mut x = self.borrow_mut();
        x.drag_state.as_mut().expect("Hover Index").state =
            Some(DropAction::Filter(index));
    }

    pub fn is_dragover_row_pivots(&self) -> Option<(i32, String)> {
        match self.borrow().drag_state {
            Some(DragState {
                ref column,
                state: Some(DropAction::RowPivots(index)),
                ..
            }) => Some((index, column.clone())),
            _ => None,
        }
    }

    pub fn is_dragover_column_pivots(&self) -> Option<(i32, String)> {
        match self.borrow().drag_state {
            Some(DragState {
                ref column,
                state: Some(DropAction::ColumnPivots(index)),
                ..
            }) => Some((index, column.clone())),
            _ => None,
        }
    }

    pub fn is_dragover_active(&self) -> Option<(i32, String)> {
        match self.borrow().drag_state {
            Some(DragState {
                ref column,
                state: Some(DropAction::Active(index)),
                ..
            }) => Some((index, column.clone())),
            _ => None,
        }
    }

    pub fn is_dragover_filter(&self) -> Option<(i32, Filter)> {
        match self.borrow().drag_state {
            Some(DragState {
                ref column,
                state: Some(DropAction::Filter(index)),
                ..
            }) => Some((index, Filter(column.clone(), FilterOp::EQ, Scalar::Null))),
            _ => None,
        }
    }

    pub fn is_dragover_sort(&self) -> Option<(i32, Sort)> {
        match self.borrow().drag_state {
            Some(DragState {
                ref column,
                state: Some(DropAction::Sort(index)),
                ..
            }) => Some((index, Sort(column.clone(), SortDir::Asc))),
            _ => None,
        }
    }

    pub fn set_active_column_index(&self, index: i32) -> bool {
        let mut r = self.borrow_mut();
        let should_render = match r.drag_state {
            Some(DragState {
                state: Some(DropAction::Active(x)),
                ..
            }) => x != index,
            _ => true,
        };

        r.drag_state
            .as_mut()
            .expect("Hover index without hover")
            .state = Some(DropAction::Active(index));

        should_render
    }
}

/// HTML drag/drop will fire a bubbling `dragleave` event over all children of a
/// `dragleave`-listened-to element, so we need to filter out the events from the
/// children elements with this esoteric DOM arcana.
pub fn dragleave_helper(callback: impl Fn() + 'static) -> Callback<DragEvent> {
    Callback::from({
        move |event: DragEvent| {
            let related_target = event
                .related_target()
                .or_else(|| Some(JsValue::UNDEFINED.unchecked_into::<EventTarget>()))
                .map(|x| x.unchecked_into::<Node>());

            let current_target = event
                .current_target()
                .unwrap()
                .unchecked_into::<HtmlElement>();

            if !current_target.contains(related_target.as_ref()) {
                callback();
            }
        }
    })
}
