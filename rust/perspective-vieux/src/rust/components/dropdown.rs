////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2018, the Perspective Authors.
//
// This file is part of the Perspective library, distributed under the terms
// of the Apache License 2.0.  The full license can be found in the LICENSE
// file.

use std::fmt::Debug;
use std::fmt::Display;
use std::str::FromStr;
use yew::prelude::*;

#[derive(Properties, Clone)]
pub struct DropDownProps<T>
where
    T: Clone + Display + FromStr + PartialEq + 'static,
    T::Err: Clone + Debug + 'static,
{
    pub values: Vec<T>,
    pub selected: T,
    pub on_select: Callback<T>,

    #[prop_or_default]
    pub id: Option<&'static str>,
}

pub struct DropDown<T>
where
    T: Clone + Display + FromStr + PartialEq + 'static,
    T::Err: Clone + Debug + 'static,
{
    props: DropDownProps<T>,
}

impl<T> Component for DropDown<T>
where
    T: Clone + Display + FromStr + PartialEq + 'static,
    T::Err: Clone + Debug + 'static,
{
    type Message = ();
    type Properties = DropDownProps<T>;

    fn create(props: Self::Properties, _link: ComponentLink<Self>) -> Self {
        DropDown { props }
    }

    fn update(&mut self, _msg: Self::Message) -> ShouldRender {
        false
    }

    fn change(&mut self, props: Self::Properties) -> ShouldRender {
        self.props = props;
        true
    }

    fn view(&self) -> Html {
        let callback = self.props.on_select.reform(|data: ChangeData| match data {
            ChangeData::Select(e) => T::from_str(e.value().as_str()).unwrap(),
            ChangeData::Value(x) => T::from_str(x.as_str()).unwrap(),
            ChangeData::Files(_) => panic!("No idea ..."),
        });

        html! {
            <select id={ self.props.id } class="noselect" onchange=callback>{
                for self.props.values.iter().map(|value| {
                    let selected = *value == self.props.selected;
                    html! {
                        <option
                            selected=selected
                            value={ format!("{}", value) }>

                            { format!("{}", value) }
                        </option>
                    }
                })
            }</select>
        }
    }
}
