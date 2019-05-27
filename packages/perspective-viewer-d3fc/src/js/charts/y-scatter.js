/******************************************************************************
 *
 * Copyright (c) 2017, the Perspective Authors.
 *
 * This file is part of the Perspective library, distributed under the terms of
 * the Apache License 2.0.  The full license can be found in the LICENSE file.
 *
 */
import * as fc from "d3fc";
import {axisFactory} from "../axis/axisFactory";
import {AXIS_TYPES} from "../axis/axisType";
import {chartSvgFactory} from "../axis/chartFactory";
import {axisSplitter, dataSplitFunction} from "../axis/axisSplitter";
import {seriesColors} from "../series/seriesColors";
import {categoryPointSeries, symbolType} from "../series/categoryPointSeries";
import {groupData} from "../data/groupData";
import {symbolLegend} from "../legend/legend";
import {filterData} from "../legend/filter";
import withGridLines from "../gridlines/gridlines";
import {hardLimitZeroPadding} from "../d3fc/padding/hardLimitZero";
import zoomableChart from "../zoom/zoomableChart";
import nearbyTip from "../tooltip/nearbyTip";

function yScatter(container, settings) {
    const color = seriesColors(settings);
    const symbols = symbolType(settings);
    const {data, series, splitFn} = getDataAndSeries(settings, color, symbols);

    const legend = symbolLegend()
        .settings(settings)
        .scale(symbols)
        .color(color);

    const paddingStrategy = hardLimitZeroPadding()
        .pad([0.05, 0.05])
        .padUnit("percent");

    const xAxis = axisFactory(settings)
        .excludeType(AXIS_TYPES.linear)
        .settingName("crossValues")
        .valueName("crossValue")(data);
    const yAxisFactory = axisFactory(settings)
        .settingName("mainValues")
        .valueName("mainValue")
        .orient("vertical")
        .paddingStrategy(paddingStrategy);

    // Check whether we've split some values into a second y-axis
    const splitter = axisSplitter(settings, data, splitFn).color(color);

    const yAxis1 = yAxisFactory(splitter.data());

    // No grid lines if splitting y-axis
    const plotSeries = splitter.haveSplit() ? series : withGridLines(series).orient("vertical");

    const chart = chartSvgFactory(xAxis, yAxis1)
        .axisSplitter(splitter)
        .plotArea(plotSeries);

    chart.yNice && chart.yNice();

    const zoomChart = zoomableChart()
        .chart(chart)
        .settings(settings)
        .xScale(xAxis.scale);

    const toolTip = nearbyTip()
        .settings(settings)
        .xScale(xAxis.scale)
        .yScale(yAxis1.scale)
        .color(color)
        .data(data);

    if (splitter.haveSplit()) {
        // Create the y-axis data for the alt-axis
        const yAxis2 = yAxisFactory(splitter.altData());
        chart.altAxis(yAxis2);
        // Give the tooltip the information (i.e. 2 datasets with different scales)
        toolTip.data(splitter.data()).altDataWithScale({yScale: yAxis2.scale, data: splitter.altData()});
    }

    // render
    container.datum(splitter.data()).call(zoomChart);
    container.call(toolTip);
    if (legend) {
        container.call(legend);
    }
}
yScatter.plugin = {
    type: "d3_y_scatter",
    name: "Y Scatter Chart",
    max_size: 25000
};

export default yScatter;

const getData = settings => groupData(settings, filterData(settings));
const getSeries = (settings, data, color, symbols) =>
    fc
        .seriesSvgMulti()
        .mapping((data, index) => data[index])
        .series(data.map(series => categoryPointSeries(settings, series.key, color, symbols)));

export const getDataAndSeries = (settings, color, symbols) => {
    const data = getData(settings);
    return {
        data,
        series: getSeries(settings, data, color, symbols),
        splitFn: dataSplitFunction
    };
};
