////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2018, the Perspective Authors.
//
// This file is part of the Perspective library, distributed under the terms
// of the Apache License 2.0.  The full license can be found in the LICENSE
// file.

use serde::Deserialize;
use serde::Serialize;
use std::fmt::Display;

#[derive(Clone, Deserialize, Debug, PartialEq, Serialize)]
#[serde()]
pub struct Sort(pub String, pub SortDir);

#[derive(Clone, Copy, Deserialize, Debug, PartialEq, Serialize)]
#[serde()]
pub enum SortDir {
    #[serde(rename = "none")]
    None,

    #[serde(rename = "desc")]
    Desc,

    #[serde(rename = "asc")]
    Asc,

    #[serde(rename = "col desc")]
    ColDesc,

    #[serde(rename = "col asc")]
    ColAsc,

    #[serde(rename = "desc abs")]
    DescAbs,

    #[serde(rename = "asc abs")]
    AscAbs,

    #[serde(rename = "col desc abs")]
    ColDescAbs,

    #[serde(rename = "col asc abs")]
    ColAscAbs,
}

impl Display for SortDir {
    fn fmt(
        &self,
        fmt: &mut std::fmt::Formatter<'_>,
    ) -> std::result::Result<(), std::fmt::Error> {
        write!(
            fmt,
            "{}",
            match self {
                SortDir::None => "none",
                SortDir::Desc => "desc",
                SortDir::Asc => "asc",
                SortDir::ColDesc => "col desc",
                SortDir::ColAsc => "col asc",
                SortDir::DescAbs => "desc abs",
                SortDir::AscAbs => "asc abs",
                SortDir::ColDescAbs => "col desc abs",
                SortDir::ColAscAbs => "col asc abs",
            }
        )
    }
}

impl SortDir {
    /// Increment the `SortDir` in logical order, given an `abs()` modifier.
    pub fn cycle(&self, col_pivots: bool, abs: bool) -> SortDir {
        let order: &[SortDir] = match (col_pivots, abs) {
            (false, false) => &[SortDir::None, SortDir::Asc, SortDir::Desc],
            (false, true) => &[SortDir::None, SortDir::AscAbs, SortDir::DescAbs],
            (true, false) => &[
                SortDir::None,
                SortDir::Asc,
                SortDir::Desc,
                SortDir::ColAsc,
                SortDir::ColDesc,
            ],
            (true, true) => &[
                SortDir::None,
                SortDir::AscAbs,
                SortDir::DescAbs,
                SortDir::ColAscAbs,
                SortDir::ColDescAbs,
            ],
        };

        let index = order.iter().position(|x| x == self).unwrap_or(0);
        order[(index + 1) % order.len()]
    }
}
