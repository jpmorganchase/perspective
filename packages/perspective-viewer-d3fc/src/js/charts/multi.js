/******************************************************************************
 *
 * Copyright (c) 2017, the Perspective Authors.
 *
 * This file is part of the Perspective library, distributed under the terms of
 * the Apache License 2.0.  The full license can be found in the LICENSE file.
 *
 */
import {axisFactory} from "../axis/axisFactory";
import {AXIS_TYPES} from "../axis/axisType";
import {chartSvgFactory} from "../axis/chartFactory";
import {axisSplitter} from "../axis/axisSplitter";
import domainMatchOrigins from "../axis/domainMatchOrigins";
import {seriesColors} from "../series/seriesColors";
import {colorLegend} from "../legend/legend";
import withGridLines from "../gridlines/gridlines";
import {symbolType} from "../series/categoryPointSeries";

import {hardLimitZeroPadding} from "../d3fc/padding/hardLimitZero";
import zoomableChart from "../zoom/zoomableChart";
import nearbyTip from "../tooltip/nearbyTip";

import {getDataAndSeries as getLineDataAndSeries} from "./line";
import {getDataAndSeries as getColumnDataAndSeries} from "./column";
import {getDataAndSeries as getAreaDataAndSeries} from "./area";
import {getDataAndSeries as getYScatterDataAndSeries} from "./y-scatter";

import seriesPicker from "../picker/series-picker";
import {getChartElement} from "../plugin/root";

const seriesFunctions = {
    line: getLineDataAndSeries,
    column: getColumnDataAndSeries,
    area: getAreaDataAndSeries,
    yscatter: getYScatterDataAndSeries
};

function multiChart(container, settings) {
    const multiTypes = settings.multiTypes || {primary: "line", alternate: "column"};

    const primarySeriesFn = seriesFunctions[multiTypes.primary];
    const altSeriesFn = seriesFunctions[multiTypes.alternate];
    const mixCharts = multiTypes.primary != multiTypes.alternate;

    const color = seriesColors(settings);
    const symbols = symbolType(settings);
    const {data, series, splitFn, xScaleFn} = primarySeriesFn(settings, color, symbols, {mixCharts});

    const legend = colorLegend()
        .settings(settings)
        .scale(color);

    const paddingStrategy = hardLimitZeroPadding()
        .pad([0.1, 0.1])
        .padUnit("percent");

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
        .paddingStrategy(paddingStrategy);

    // Check whether we've split some values into a second y-axis
    const splitter = axisSplitter(settings, data, splitFn)
        .color(color)
        .decorate((container, i) => {
            const multiSide = i === 0 ? "primary" : "alternate";
            seriesPicker()
                .current(multiTypes[multiSide])
                .onChange(newType => {
                    multiTypes[multiSide] = newType;
                    settings.multiTypes = multiTypes;
                    redrawChart(container);
                })(container);
        });

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
        const alt = altSeriesFn(settings, color, symbols, {mixCharts});
        const altData = alt.splitFn(alt.data, splitter.isOnAltAxis);
        splitter.altData(altData);

        // Create the y-axis data for the alt-axis
        const yAxis2 = yAxisFactory(splitter.altData());

        const altXScale = xAxis.scale.copy();
        alt.xScaleFn && alt.xScaleFn(altXScale);

        domainMatchOrigins(yAxis1.domain, yAxis2.domain);
        chart
            .yDomain(yAxis1.domain)
            .altAxis(yAxis2)
            .altXScale(altXScale)
            .altPlotArea(alt.series);

        zoomChart.altXScale(altXScale);

        // Give the tooltip the information (i.e. 2 datasets with different scales)
        toolTip.data(splitter.data()).altDataWithScale({yScale: yAxis2.scale, data: splitter.altData()});
        if (xScaleFn) toolTip.xScale(altXScale);
    }
    xScaleFn && xScaleFn(xAxis.scale);

    // render
    container.datum(splitter.data()).call(zoomChart);
    container.call(toolTip);
    container.call(legend);
}

multiChart.plugin = {
    type: "d3_y_multi",
    name: "Multi Chart",
    max_size: 25000
};

export default multiChart;

const redrawChart = selection => {
    const chartElement = getChartElement(selection.node());
    chartElement.remove();
    chartElement.draw();
};
