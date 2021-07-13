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

#[derive(Debug, Clone, Deserialize, Serialize)]
#[cfg_attr(test, derive(PartialEq))]
#[serde()]
pub enum SingleAggregate {
    #[serde(rename = "sum")]
    Sum,

    #[serde(rename = "sum abs")]
    SumAbs,

    #[serde(rename = "sum not null")]
    SumNotNull,

    #[serde(rename = "abs sum")]
    AbsSum,

    #[serde(rename = "pct sum parent")]
    PctSumParent,

    #[serde(rename = "pct sum grand total")]
    PctSumGrandTotal,

    #[serde(rename = "any")]
    Any,

    #[serde(rename = "unique")]
    Unique,

    #[serde(rename = "dominant")]
    Dominant,

    #[serde(rename = "median")]
    Median,

    #[serde(rename = "first")]
    First,

    #[serde(rename = "last by index")]
    LastByIndex,

    #[serde(rename = "last")]
    Last,

    #[serde(rename = "count")]
    Count,

    #[serde(rename = "distinct count")]
    DistinctCount,

    #[serde(rename = "avg")]
    Avg,

    #[serde(rename = "mean")]
    Mean,

    #[serde(rename = "join")]
    Join,

    #[serde(rename = "high")]
    High,

    #[serde(rename = "low")]
    Low,
}

impl Display for SingleAggregate {
    fn fmt(
        &self,
        fmt: &mut std::fmt::Formatter<'_>,
    ) -> std::result::Result<(), std::fmt::Error> {
        for char in format!("{:?}", self).chars() {
            if char.is_lowercase() {
                write!(fmt, "{}", char)?;
            } else {
                write!(fmt, " {}", char.to_lowercase().next().unwrap())?;
            }
        }

        Ok(())
    }
}

#[derive(Clone, Deserialize, Serialize)]
#[cfg_attr(test, derive(Debug, PartialEq))]
#[serde()]
pub enum MultiAggregate {
    #[serde(rename = "weighted mean")]
    WeightedMean,
}

#[derive(Clone, Deserialize, Serialize)]
#[cfg_attr(test, derive(Debug, PartialEq))]
#[serde(untagged)]
pub enum Aggregate {
    SingleAggregate(SingleAggregate),
    MultiAggregate(MultiAggregate, String),
}

impl Display for Aggregate {
    fn fmt(
        &self,
        fmt: &mut std::fmt::Formatter<'_>,
    ) -> std::result::Result<(), std::fmt::Error> {
        match self {
            Aggregate::SingleAggregate(x) => write!(fmt, "{}", x)?,
            Aggregate::MultiAggregate(MultiAggregate::WeightedMean, x) => {
                write!(fmt, "mean by {}", x)?
            }
        };
        Ok(())
    }
}
