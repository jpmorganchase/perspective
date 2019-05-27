/******************************************************************************
 *
 * Copyright (c) 2017, the Perspective Authors.
 *
 * This file is part of the Perspective library, distributed under the terms of
 * the Apache License 2.0.  The full license can be found in the LICENSE file.
 *
 */

import multiChart from "./multi";
import barChart from "./bar";
import columnChart from "./column";
import lineChart from "./line";
import areaChart from "./area";
import yScatter from "./y-scatter";
import xyScatter from "./xy-scatter";
import heatmap from "./heatmap";
import ohlc from "./ohlc";
import candlestick from "./candlestick";
import sunburst from "./sunburst";
import treemap from "./treemap";

const chartClasses = [multiChart, barChart, columnChart, lineChart, areaChart, yScatter, xyScatter, heatmap, ohlc, candlestick, sunburst, treemap];

export default chartClasses;
