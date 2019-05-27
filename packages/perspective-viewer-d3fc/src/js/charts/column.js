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
import {chartSvgFactory} from "../axis/chartFactory";
import domainMatchOrigins from "../axis/domainMatchOrigins";
import {axisSplitter, groupBlankFunction, multiGroupBlankFunction, groupRemoveFunction, multiGroupRemoveFunction} from "../axis/axisSplitter";
import {axisType, AXIS_TYPES} from "../axis/axisType";
import {barSeries} from "../series/barSeries";
import {seriesColors} from "../series/seriesColors";
import {groupAndStackData} from "../data/groupData";
import {colorLegend} from "../legend/legend";
import {filterData} from "../legend/filter";
import withGridLines from "../gridlines/gridlines";
import {hardLimitZeroPadding} from "../d3fc/padding/hardLimitZero";
import zoomableChart from "../zoom/zoomableChart";

function columnChart(container, settings) {
    const color = seriesColors(settings);
    const {data, series, splitFn, xScaleFn} = getDataAndSeries(settings, color);

    const legend = colorLegend()
        .settings(settings)
        .scale(color);

    const xAxis = axisFactory(settings)
        .excludeType(AXIS_TYPES.linear)
        .settingName("crossValues")
        .valueName("crossValue")(data);

    const yAxisFactory = axisFactory(settings)
        .settingName("mainValues")
        .valueName("mainValue")
        .excludeType(AXIS_TYPES.ordinal)
        .orient("vertical")
        .include([0])
        .paddingStrategy(hardLimitZeroPadding());

    // Check whether we've split some values into a second y-axis
    const splitter = axisSplitter(settings, data, splitFn).color(color);

    const yAxis1 = yAxisFactory(splitter.data());

    // No grid lines if splitting y-axis
    const plotSeries = splitter.haveSplit() ? series : withGridLines(series).orient("vertical");

    const chart = chartSvgFactory(xAxis, yAxis1)
        .axisSplitter(splitter)
        .plotArea(plotSeries);

    xScaleFn(xAxis.scale);

    chart.yNice && chart.yNice();

    const zoomChart = zoomableChart()
        .chart(chart)
        .settings(settings)
        .xScale(xAxis.scale);

    if (splitter.haveSplit()) {
        // Create the y-axis data for the alt-axis
        const yAxis2 = yAxisFactory(splitter.altData());

        domainMatchOrigins(yAxis1.domain, yAxis2.domain);
        chart.yDomain(yAxis1.domain).altAxis(yAxis2);
    }

    // render
    container.datum(splitter.data()).call(zoomChart);
    container.call(legend);
}
columnChart.plugin = {
    type: "d3_y_bar",
    name: "Y Bar Chart",
    max_size: 25000
};

export default columnChart;

const getData = settings => groupAndStackData(settings, filterData(settings));
const getSeries = (settings, data, color, options = {mixCharts: false}) => {
    const bars = barSeries(settings, color, options).orient("vertical");

    if (axisType(settings).excludeType(AXIS_TYPES.linear)() == AXIS_TYPES.ordinal) {
        bars.align("left");
    }

    return fc
        .seriesSvgMulti()
        .mapping((data, index) => data[index])
        .series(data.map(() => bars));
};

const getSplitFn = (settings, options) => {
    const grouped = settings.mainValues.length > 1;
    if (options.mixCharts) {
        return grouped ? multiGroupRemoveFunction : groupRemoveFunction;
    }
    return grouped ? multiGroupBlankFunction : groupBlankFunction;
};

export const getDataAndSeries = (settings, color, symbols, options = {mixCharts: false}) => {
    const data = getData(settings);
    return {
        data,
        series: getSeries(settings, data, color, options),
        splitFn: getSplitFn(settings, options),
        xScaleFn: scale => {
            if (scale.paddingInner) {
                scale.paddingInner(0.5);
                scale.paddingOuter(0.25);
            }
        }
    };
};
