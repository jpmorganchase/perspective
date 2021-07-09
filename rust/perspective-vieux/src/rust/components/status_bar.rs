////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2018, the Perspective Authors.
//
// This file is part of the Perspective library, distributed under the terms
// of the Apache License 2.0.  The full license can be found in the LICENSE
// file.

use crate::components::status_bar_counter::StatusBarRowsCounter;
use crate::js::perspective_viewer::*;
use crate::plugin::*;
use crate::session::TableStats;
use crate::*;

#[cfg(test)]
use crate::utils::WeakComponentLink;

use js_intern::*;
use js_sys::*;
use yew::prelude::*;

#[derive(Properties, Clone)]
pub struct StatusBarProps {
    pub id: String,
    pub on_reset: Callback<()>,
    pub on_download: Callback<bool>,
    pub on_copy: Callback<bool>,
    pub plugin: Plugin,

    #[prop_or(None)]
    pub stats: Option<TableStats>,

    #[cfg(test)]
    #[prop_or_default]
    pub weak_link: WeakComponentLink<StatusBar>,
}

pub enum StatusBarMsg {
    Reset,
    Export(bool),
    Copy(bool),
    PluginChanged(PerspectiveViewerJsPlugin),
    Render,
}

/// A toolbar with buttons, and `Table` & `View` status information.
pub struct StatusBar {
    link: ComponentLink<Self>,
    is_render: bool,
    pub props: StatusBarProps,
}

impl Component for StatusBar {
    type Message = StatusBarMsg;
    type Properties = StatusBarProps;

    fn create(props: Self::Properties, link: ComponentLink<Self>) -> Self {
        enable_weak_link_test!(props, link);
        props.plugin.add_on_plugin_changed({
            clone!(link);
            move |plugin| link.send_message(StatusBarMsg::PluginChanged(plugin))
        });

        let plugin = props.plugin.get_plugin(None).unwrap();
        let is_render = Reflect::has(&plugin, js_intern!("render")).unwrap();
        Self {
            props,
            link,
            is_render,
        }
    }

    fn update(&mut self, msg: Self::Message) -> ShouldRender {
        match msg {
            StatusBarMsg::Reset => self.props.on_reset.emit(()),
            StatusBarMsg::Export(flat) => self.props.on_download.emit(flat),
            StatusBarMsg::Copy(flat) => self.props.on_copy.emit(flat),
            StatusBarMsg::Render => {
                if let Ok(plugin) = self.props.plugin.get_plugin(None) {
                    plugin.render();
                }
            }
            StatusBarMsg::PluginChanged(plugin) => {
                self.is_render = Reflect::has(&plugin, js_intern!("render")).unwrap();
                return true;
            }
        }
        false
    }

    fn change(&mut self, props: Self::Properties) -> ShouldRender {
        let should_render = props.stats != self.props.stats;
        self.props = props;
        should_render
    }

    fn view(&self) -> Html {
        let class_name = self.status_class_name();
        let reset = self.link.callback(|_| StatusBarMsg::Reset);
        let export = self
            .link
            .callback(|event: MouseEvent| StatusBarMsg::Export(event.shift_key()));

        let copy = self
            .link
            .callback(|event: MouseEvent| StatusBarMsg::Copy(event.shift_key()));

        html! {
            <div id=self.props.id.clone()>
                <div class="section">
                    <span id="status" class=class_name></span>
                </div>
                <div class="section">
                    <span id="reset" class="button" onclick=reset>
                        <span>{ "Reset" }</span>
                    </span>
                    <span id="export" class="button" onclick=export>
                        <span>{ "Export" }</span>
                    </span>
                    <span id="copy" class="button" onclick=copy>
                        <span>{ "Copy" }</span>
                    </span>
                    {
                        if self.is_render {
                            let render = self
                                .link
                                .callback(|_| StatusBarMsg::Render);

                            html! {
                                <span id="render" class="button" onclick=render>
                                    <span>{ "Render" }</span>
                                </span>
                            }
                        } else {
                            html! {}
                        }
                    }
                </div>
                <div id="rows" class="section">
                    <StatusBarRowsCounter stats=self.props.stats.clone() />
                </div>
            </div>
        }
    }
}

impl StatusBar {
    fn status_class_name(&self) -> &'static str {
        match &self.props.stats {
            Some(TableStats {
                num_rows: Some(_),
                virtual_rows: Some(_),
                is_pivot: true,
            })
            | Some(TableStats {
                num_rows: Some(_), ..
            }) => "connected",
            Some(TableStats { num_rows: None, .. }) => "initializing",
            None => "uninitialized",
        }
    }
}
