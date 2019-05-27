/******************************************************************************
 *
 * Copyright (c) 2017, the Perspective Authors.
 *
 * This file is part of the Perspective library, distributed under the terms of
 * the Apache License 2.0.  The full license can be found in the LICENSE file.
 *
 */

import * as d3 from "d3";
import {getOrCreateElement} from "../utils/utils";
import template from "../../html/zoom-controls.html";

export default () => {
    let chart = null;
    let settings = null;
    let xScale = null;
    let altXScale = null;
    let xCopy = null;
    let altXCopy = null;
    let yScale = null;
    let yCopy = null;
    let bound = false;
    let canvas = false;
    let onChange = () => {};

    function zoomableChart(selection) {
        const chartPlotArea = `d3fc-${canvas ? "canvas" : "svg"}.plot-area`;
        if (xScale || yScale) {
            const dateAxis = xCopy && xCopy.domain()[0] instanceof Date;
            const zoom = d3.zoom().on("zoom", () => {
                const {transform} = d3.event;
                settings.zoom = {
                    k: transform.k,
                    x: transform.x,
                    y: transform.y
                };

                applyTransform(transform);

                selection.call(chart);

                const noZoom = transform.k === 1 && transform.x === 0 && transform.y === 0;

                const zoomControls = getZoomControls(selection).style("display", noZoom ? "none" : "");
                zoomControls.select("#zoom-reset").on("click", () => selection.select(chartPlotArea).call(zoom.transform, d3.zoomIdentity));

                const oneYear = zoomControls.select("#one-year").style("display", dateAxis ? "" : "none");
                const sixMonths = zoomControls.select("#six-months").style("display", dateAxis ? "" : "none");
                const oneMonth = zoomControls.select("#one-month").style("display", dateAxis ? "" : "none");
                if (dateAxis) {
                    const dateClick = endCalculation => () => {
                        const start = new Date(xScale.domain()[0]);
                        const end = new Date(start);
                        endCalculation(start, end);

                        const xRange = xCopy.range();
                        const k = (xRange[1] - xRange[0]) / (xCopy(end) - xCopy(start));
                        const x = -xCopy(start) * k;
                        let y = 0;
                        if (yScale) {
                            const yMiddle = yScale.domain().reduce((a, b) => a + b) / 2;
                            y = -yCopy(yMiddle) * k + yScale(yMiddle);
                        }
                        selection.select(chartPlotArea).call(zoom.transform, d3.zoomIdentity.translate(x, y).scale(k));
                    };

                    oneYear.on("click", dateClick((start, end) => end.setYear(start.getFullYear() + 1)));
                    sixMonths.on("click", dateClick((start, end) => end.setMonth(start.getMonth() + 6)));
                    oneMonth.on("click", dateClick((start, end) => end.setMonth(start.getMonth() + 1)));
                }
            });

            const oldDecorate = chart.decorate();
            chart.decorate((sel, data) => {
                oldDecorate(sel, data);
                if (!bound) {
                    bound = true;
                    // add the zoom interaction on the enter selection
                    const plotArea = sel.select(chartPlotArea);

                    plotArea
                        .on("measure.zoom-range", () => {
                            if (xCopy) xCopy.range([0, d3.event.detail.width]);
                            if (altXCopy) altXCopy.range([0, d3.event.detail.width]);
                            if (yCopy) yCopy.range([0, d3.event.detail.height]);

                            if (settings.zoom) {
                                const initialTransform = d3.zoomIdentity.translate(settings.zoom.x, settings.zoom.y).scale(settings.zoom.k);
                                plotArea.call(zoom.transform, initialTransform);
                            }
                        })
                        .call(zoom);
                }
            });
        }

        selection.call(chart);
    }

    zoomableChart.chart = (...args) => {
        if (!args.length) {
            return chart;
        }
        chart = args[0];
        return zoomableChart;
    };

    zoomableChart.settings = (...args) => {
        if (!args.length) {
            return settings;
        }
        settings = args[0];
        return zoomableChart;
    };

    zoomableChart.xScale = (...args) => {
        if (!args.length) {
            return xScale;
        }
        xScale = zoomableScale(args[0]);
        xCopy = xScale ? xScale.copy() : null;
        return zoomableChart;
    };

    zoomableChart.altXScale = (...args) => {
        if (!args.length) {
            return xScale;
        }
        altXScale = args[0];
        altXCopy = altXScale ? altXScale.copy() : null;
        return zoomableChart;
    };

    zoomableChart.yScale = (...args) => {
        if (!args.length) {
            return yScale;
        }
        yScale = zoomableScale(args[0]);
        yCopy = yScale ? yScale.copy() : null;
        if (yCopy) {
            const yDomain = yCopy.domain();
            yCopy.domain([yDomain[1], yDomain[0]]);
        }
        return zoomableChart;
    };

    zoomableChart.canvas = (...args) => {
        if (!args.length) {
            return canvas;
        }
        canvas = args[0];
        return zoomableChart;
    };

    zoomableChart.onChange = (...args) => {
        if (!args.length) {
            return onChange;
        }
        onChange = args[0];
        return zoomableChart;
    };

    const applyTransform = transform => {
        const changeArgs = {...transform};
        if (xScale) {
            xScale.domain(transform.rescaleX(xCopy).domain());
            changeArgs.xDomain = xScale.domain();
        }
        if (altXScale) {
            altXScale.domain(transform.rescaleX(altXCopy).domain());
        }

        if (yScale) {
            const yZoomDomain = transform.rescaleY(yCopy).domain();
            yScale.domain([yZoomDomain[1], yZoomDomain[0]]);
            changeArgs.yDomain = yScale.domain();
        }

        onChange(changeArgs);
    };

    const getZoomControls = container =>
        getOrCreateElement(container, ".zoom-controls", () =>
            container
                .append("div")
                .attr("class", "zoom-controls")
                .style("display", "none")
                .html(template)
        );

    const zoomableScale = scale => {
        if (scale && scale.nice) {
            return scale;
        }
        return null;
    };

    return zoomableChart;
};
