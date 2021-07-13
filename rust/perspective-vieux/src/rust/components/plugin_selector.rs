////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2018, the Perspective Authors.
//
// This file is part of the Perspective library, distributed under the terms
// of the Apache License 2.0.  The full license can be found in the LICENSE
// file.

use crate::js::perspective_viewer::*;
use crate::renderer::registry::*;
use crate::renderer::*;
use crate::utils::*;
use crate::*;

use super::dropdown::DropDown;

use yew::prelude::*;

#[derive(Properties, Clone)]
pub struct PluginSelectorProps {
    pub renderer: Renderer,

    #[cfg(test)]
    #[prop_or_default]
    pub weak_link: WeakComponentLink<PluginSelector>,
}

pub enum PluginSelectorMsg {
    ComponentSelectPlugin(String),
    RendererSelectPlugin(String),
}

pub struct PluginSelector {
    props: PluginSelectorProps,
    _plugin_sub: Subscription,
    link: ComponentLink<PluginSelector>,
}

impl Component for PluginSelector {
    type Message = PluginSelectorMsg;
    type Properties = PluginSelectorProps;

    fn create(props: Self::Properties, link: ComponentLink<Self>) -> Self {
        enable_weak_link_test!(props, link);
        let _plugin_sub = props.renderer.add_on_plugin_changed({
            clone!(link);
            move |plugin: JsPerspectiveViewerPlugin| {
                let name = plugin.name();
                link.send_message(PluginSelectorMsg::RendererSelectPlugin(name))
            }
        });

        PluginSelector {
            props,
            link,
            _plugin_sub,
        }
    }

    fn update(&mut self, msg: Self::Message) -> ShouldRender {
        match msg {
            PluginSelectorMsg::RendererSelectPlugin(_plugin_name) => true,
            PluginSelectorMsg::ComponentSelectPlugin(plugin_name) => {
                self.props.renderer.set_plugin(Some(&plugin_name)).unwrap();
                false
            }
        }
    }

    fn change(&mut self, _props: Self::Properties) -> ShouldRender {
        false
    }

    fn view(&self) -> Html {
        let callback = self.link.callback(PluginSelectorMsg::ComponentSelectPlugin);
        let plugin_name = self.props.renderer.get_active_plugin().unwrap().name();

        html! {
            <div id="plugin_selector_container">
                <DropDown<String>
                    id="plugin_selector"
                    values=PLUGIN_REGISTRY.available_plugin_names()
                    selected=plugin_name
                    on_select=callback>

                </DropDown<String>>
            </div>
        }
    }
}
