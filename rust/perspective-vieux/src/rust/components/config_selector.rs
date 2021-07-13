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
use crate::*;

use super::dropdown::*;
use super::pivot_selector::*;

use std::rc::Rc;
use wasm_bindgen::prelude::*;
use wasm_bindgen_futures::future_to_promise;
use web_sys::*;
use yew::prelude::*;

#[derive(Properties, Clone)]
pub struct ConfigSelectorProps {
    pub session: Session,
    pub renderer: Renderer,
    pub dragdrop: DragDrop,
}

pub enum ConfigSelectorMsg {
    DragOverRowPivots(i32),
    DragOverColumnPivots(i32),
    DragOverSort(i32),
    DragOverFilter(i32),
    DragClear,
    Drop(String, DropAction),
    CloseRowPivot(i32),
    CloseColumnPivot(i32),
    CloseSort(i32),
    CloseFilter(i32),
    ViewCreated,
}

#[derive(Clone)]
pub struct ConfigSelector {
    props: ConfigSelectorProps,
    link: ComponentLink<ConfigSelector>,
    subscriptions: [Rc<Subscription>; 2],
}

type RowPivotSelector = PivotSelector<ConfigSelector, String>;
type ColumnPivotSelector = PivotSelector<ConfigSelector, String>;
type SortSelector = PivotSelector<ConfigSelector, Sort>;
type FilterSelector = PivotSelector<ConfigSelector, Filter>;

impl Component for ConfigSelector {
    type Message = ConfigSelectorMsg;
    type Properties = ConfigSelectorProps;

    fn create(props: Self::Properties, link: ComponentLink<Self>) -> Self {
        let cb =
            link.callback(|x: (String, DropAction)| ConfigSelectorMsg::Drop(x.0, x.1));
        let drop_sub = Rc::new(props.dragdrop.add_on_drop_action(cb));

        let cb = link.callback(|_| ConfigSelectorMsg::ViewCreated);
        let view_sub = Rc::new(props.session.add_on_view_created(cb));

        let subscriptions = [drop_sub, view_sub];
        ConfigSelector {
            props,
            link,
            subscriptions,
        }
    }

    fn update(&mut self, msg: Self::Message) -> ShouldRender {
        match msg {
            ConfigSelectorMsg::ViewCreated => true,
            ConfigSelectorMsg::DragOverRowPivots(index) => {
                self.props.dragdrop.set_row_pivots(index);
                true
            }
            ConfigSelectorMsg::DragOverColumnPivots(index) => {
                self.props.dragdrop.set_column_pivots(index);
                true
            }
            ConfigSelectorMsg::DragOverSort(index) => {
                self.props.dragdrop.set_sort(index);
                true
            }
            ConfigSelectorMsg::DragOverFilter(index) => {
                self.props.dragdrop.set_filter(index);
                true
            }
            ConfigSelectorMsg::DragClear => {
                self.props.dragdrop.reset_hover_column();
                true
            }
            ConfigSelectorMsg::CloseSort(index) => {
                let ViewConfig { mut sort, .. } = self.props.session.get_view_config();
                sort.remove(index as usize);
                let sort = Some(sort);
                self.render(ViewConfigUpdate {
                    sort,
                    ..ViewConfigUpdate::default()
                });

                false
            }
            ConfigSelectorMsg::CloseRowPivot(index) => {
                let ViewConfig { mut row_pivots, .. } =
                    self.props.session.get_view_config();
                row_pivots.remove(index as usize);
                let row_pivots = Some(row_pivots);
                self.render(ViewConfigUpdate {
                    row_pivots,
                    ..ViewConfigUpdate::default()
                });

                false
            }
            ConfigSelectorMsg::CloseColumnPivot(index) => {
                let ViewConfig {
                    mut column_pivots, ..
                } = self.props.session.get_view_config();
                column_pivots.remove(index as usize);
                self.render(ViewConfigUpdate {
                    column_pivots: Some(column_pivots),
                    ..ViewConfigUpdate::default()
                });

                false
            }
            ConfigSelectorMsg::CloseFilter(index) => {
                let ViewConfig { mut filter, .. } =
                    self.props.session.get_view_config();
                filter.remove(index as usize);
                self.render(ViewConfigUpdate {
                    filter: Some(filter),
                    ..ViewConfigUpdate::default()
                });

                false
            }
            ConfigSelectorMsg::Drop(column, DropAction::RowPivots(index)) => {
                let ViewConfig { mut row_pivots, .. } =
                    self.props.session.get_view_config();
                row_pivots.retain(|x| x != &column);
                let index = std::cmp::min(index as usize, row_pivots.len());
                row_pivots.insert(index, column);
                let row_pivots = Some(row_pivots);
                self.render(ViewConfigUpdate {
                    row_pivots,
                    ..ViewConfigUpdate::default()
                });

                false
            }
            ConfigSelectorMsg::Drop(column, DropAction::ColumnPivots(index)) => {
                let ViewConfig {
                    mut column_pivots, ..
                } = self.props.session.get_view_config();
                column_pivots.retain(|x| x != &column);
                let len = column_pivots.len();
                column_pivots.insert(std::cmp::min(index as usize, len), column);
                let column_pivots = Some(column_pivots);
                let update = ViewConfigUpdate {
                    column_pivots,
                    ..ViewConfigUpdate::default()
                };

                self.render(update);
                false
            }
            ConfigSelectorMsg::Drop(column, DropAction::Sort(index)) => {
                let ViewConfig { mut sort, .. } = self.props.session.get_view_config();
                sort.insert(
                    std::cmp::min(index as usize, sort.len()),
                    Sort(column, SortDir::Asc),
                );

                let sort = Some(sort);
                let update = ViewConfigUpdate {
                    sort,
                    ..ViewConfigUpdate::default()
                };

                self.render(update);
                false
            }
            ConfigSelectorMsg::Drop(column, DropAction::Filter(index)) => {
                let ViewConfig { mut filter, .. } =
                    self.props.session.get_view_config();
                filter.insert(
                    std::cmp::min(index as usize, filter.len()),
                    Filter(column, FilterOp::EQ, Scalar::Null),
                );

                let filter = Some(filter);
                let update = ViewConfigUpdate {
                    filter,
                    ..ViewConfigUpdate::default()
                };

                self.render(update);
                false
            }
            ConfigSelectorMsg::Drop(_, _) => false,
        }
    }

    /// Should not render on change, as this component only depends on service state.
    fn change(&mut self, _props: Self::Properties) -> ShouldRender {
        false
    }

    fn view(&self) -> Html {
        let config = self.props.session.get_view_config();

        html! {
            <div slot="top_panel" id="top_panel">
                <RowPivotSelector
                    name="row_pivots"
                    parent=self.link.clone()
                    is_dragover=self.props.dragdrop.is_dragover_row_pivots()
                    dragdrop=self.props.dragdrop.clone()
                    columns=config.row_pivots.clone()
                    dragenter={ ConfigSelectorMsg::DragOverRowPivots as fn(i32) -> ConfigSelectorMsg }
                    close={ ConfigSelectorMsg::CloseRowPivot as fn(i32) -> ConfigSelectorMsg }
                    dragleave={ (|| ConfigSelectorMsg::DragClear) as fn() -> ConfigSelectorMsg }
                    render={ Rc::new(render_pivot) as Rc<dyn Fn(usize, &String) -> Html> }>
                </RowPivotSelector>
                <span
                    id="transpose_button"
                    class="rrow centered"
                    title="Transpose Pivots">

                    { "\u{21C4}" }
                </span>
                <ColumnPivotSelector
                    name="column_pivots"
                    parent=self.link.clone()
                    is_dragover=self.props.dragdrop.is_dragover_column_pivots()
                    dragdrop=self.props.dragdrop.clone()
                    columns=config.column_pivots.clone()
                    dragenter={ ConfigSelectorMsg::DragOverColumnPivots as fn(i32) -> ConfigSelectorMsg }
                    close={ ConfigSelectorMsg::CloseColumnPivot as fn(i32) -> ConfigSelectorMsg }
                    dragleave={ (|| ConfigSelectorMsg::DragClear) as fn() -> ConfigSelectorMsg }
                    render={ Rc::new(render_pivot) as Rc<dyn Fn(usize, &String) -> Html> }>
                </ColumnPivotSelector>
                <SortSelector
                    name="sort"
                    parent=self.link.clone()
                    is_dragover=self.props.dragdrop.is_dragover_sort()
                    dragdrop=self.props.dragdrop.clone()
                    allow_duplicates=false
                    columns=config.sort.clone()
                    dragenter={ ConfigSelectorMsg::DragOverSort as fn(i32) -> ConfigSelectorMsg }
                    close={ ConfigSelectorMsg::CloseSort as fn(i32) -> ConfigSelectorMsg }
                    dragleave={ (|| ConfigSelectorMsg::DragClear) as fn() -> ConfigSelectorMsg }
                    render={{
                        let session = self.props.session.clone();
                        let renderer = self.props.renderer.clone();
                        Rc::new(move |idx: usize, sort: &Sort| render_sort(&session, &renderer, idx, sort)) as Rc<dyn Fn(usize, &Sort) -> Html>
                    }}>
                </SortSelector>
                <FilterSelector
                    name="filter"
                    parent=self.link.clone()
                    is_dragover=self.props.dragdrop.is_dragover_filter()
                    dragdrop=self.props.dragdrop.clone()
                    allow_duplicates=false
                    columns=config.filter.clone()
                    dragenter={ ConfigSelectorMsg::DragOverFilter as fn(i32) -> ConfigSelectorMsg }
                    close={ ConfigSelectorMsg::CloseFilter as fn(i32) -> ConfigSelectorMsg }
                    dragleave={ (|| ConfigSelectorMsg::DragClear) as fn() -> ConfigSelectorMsg }
                    render={{
                        let session = self.props.session.clone();
                        let renderer = self.props.renderer.clone();
                        Rc::new(move |idx: usize, filter: &Filter| render_filter(&session, &renderer, idx, filter)) as Rc<dyn Fn(usize, &Filter) -> Html>
                    }}>
                </FilterSelector>
            </div>
        }
    }
}

#[allow(clippy::ptr_arg)]
fn render_pivot(_idx: usize, column: &String) -> Html {
    html! {
        <span>{ column }</span>
    }
}

fn render_sort(
    session: &Session,
    renderer: &Renderer,
    idx: usize,
    sort: &Sort,
) -> Html {
    clone!(session, renderer);
    let onclick = Callback::from(move |event: MouseEvent| {
        let ViewConfig {
            mut sort,
            column_pivots,
            ..
        } = session.get_view_config();
        let sort_item = &mut sort.get_mut(idx).expect("Sort on no column");
        sort_item.1 = sort_item
            .1
            .cycle(!column_pivots.is_empty(), event.shift_key());

        let update = ViewConfigUpdate {
            sort: Some(sort),
            ..ViewConfigUpdate::default()
        };

        clone!(session, renderer);
        let _ = future_to_promise(async move {
            drop(session.create_view(update).await?);
            drop(renderer.draw(async { &session }).await?);
            Ok(JsValue::UNDEFINED)
        });
    });

    html! {
        <>
            <span>{ sort.0.to_owned() }</span>
            <span
                class={ format!("sort-icon {}", sort.1) }
                onclick=onclick>
            </span>
        </>
    }
}

fn render_filter(
    session: &Session,
    renderer: &Renderer,
    idx: usize,
    filter: &Filter,
) -> Html {
    clone!(session, renderer);
    let select = Callback::from(move |op: FilterOp| {
        let ViewConfig { mut filter, .. } = session.get_view_config();
        let filter_item = &mut filter.get_mut(idx).expect("Sort on no column");
        filter_item.1 = op;
        let update = ViewConfigUpdate {
            filter: Some(filter),
            ..ViewConfigUpdate::default()
        };

        clone!(session, renderer);
        let _ = future_to_promise(async move {
            drop(session.create_view(update).await?);
            drop(renderer.draw(async { &session }).await?);
            Ok(JsValue::UNDEFINED)
        });
    });

    html! {
        <>
            <span>{ filter.0.to_owned() }</span>
            <FilterOpSelector
                values=vec!(FilterOp::EQ, FilterOp::GT)
                selected=FilterOp::EQ
                on_select=select>

            </FilterOpSelector>
        </>
    }
}

type FilterOpSelector = DropDown<FilterOp>;

impl ConfigSelector {
    fn render(&self, update: ViewConfigUpdate) {
        let session = self.props.session.clone();
        let renderer = self.props.renderer.clone();
        let _ = future_to_promise(async move {
            drop(session.create_view(update).await?);
            drop(renderer.draw(async { &session }).await?);
            Ok(JsValue::UNDEFINED)
        });
    }
}
