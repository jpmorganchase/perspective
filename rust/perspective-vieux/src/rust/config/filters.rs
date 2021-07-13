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
use std::str::FromStr;

#[derive(Clone, Deserialize, Debug, PartialEq, Serialize)]
#[serde(untagged)]
pub enum Scalar {
    Float(f64),
    String(String),
    Bool(bool),
    DateTime(u64),
    Null,
    // // Can only have one u64 representation ...
    // Date(u64)
    // Int(u32)
}

#[derive(Clone, Deserialize, Debug, PartialEq, Serialize)]
#[serde()]
pub enum FilterOp {
    #[serde(rename = "contains")]
    Contains,

    #[serde(rename = "in")]
    In,

    #[serde(rename = "begins with")]
    BeginsWith,

    #[serde(rename = "ends with")]
    EndsWith,

    #[serde(rename = "is null")]
    IsNull,

    #[serde(rename = "is not null")]
    IsNotNull,

    #[serde(rename = ">")]
    GT,

    #[serde(rename = "<")]
    LT,

    #[serde(rename = "==")]
    EQ,

    #[serde(rename = ">=")]
    GTE,

    #[serde(rename = "<=")]
    LTE,

    #[serde(rename = "!=")]
    NE,
}

impl Display for FilterOp {
    fn fmt(
        &self,
        fmt: &mut std::fmt::Formatter<'_>,
    ) -> std::result::Result<(), std::fmt::Error> {
        let op = match self {
            FilterOp::Contains => "contains",
            FilterOp::In => "in",
            FilterOp::BeginsWith => "begins with",
            FilterOp::EndsWith => "ends with",
            FilterOp::IsNull => "is null",
            FilterOp::IsNotNull => "is not null",
            FilterOp::GT => ">",
            FilterOp::LT => "<",
            FilterOp::EQ => "==",
            FilterOp::GTE => ">=",
            FilterOp::LTE => "<=",
            FilterOp::NE => "!=",
        };

        write!(fmt, "{}", op)
    }
}

impl FromStr for FilterOp {
    type Err = String;
    fn from_str(
        input: &str,
    ) -> std::result::Result<Self, <Self as std::str::FromStr>::Err> {
        match input {
            "contains" => Ok(FilterOp::Contains),
            "in" => Ok(FilterOp::In),
            "begins with" => Ok(FilterOp::BeginsWith),
            "ends with" => Ok(FilterOp::EndsWith),
            "is null" => Ok(FilterOp::IsNull),
            "is not null" => Ok(FilterOp::IsNotNull),
            ">" => Ok(FilterOp::GT),
            "<" => Ok(FilterOp::LT),
            "==" => Ok(FilterOp::EQ),
            ">=" => Ok(FilterOp::GTE),
            "<=" => Ok(FilterOp::LTE),
            "!=" => Ok(FilterOp::NE),
            x => Err(format!("Unknown filter operator {}", x)),
        }
    }
}

#[derive(Clone, Deserialize, Debug, PartialEq, Serialize)]
#[serde()]
pub struct Filter(pub String, pub FilterOp, pub Scalar);
