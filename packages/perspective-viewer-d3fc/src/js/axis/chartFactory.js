/******************************************************************************
 *
 * Copyright (c) 2017, the Perspective Authors.
 *
 * This file is part of the Perspective library, distributed under the terms of
 * the Apache License 2.0.  The full license can be found in the LICENSE file.
 *
 */
import * as d3 from "d3";
import * as fc from "d3fc";

export const chartSvgFactory = (xAxis, yAxis) => chartFactory(xAxis, yAxis, fc.chartSvgCartesian, false);
export const chartCanvasFactory = (xAxis, yAxis) => chartFactory(xAxis, yAxis, fc.chartCanvasCartesian, true);

const chartFactory = (xAxis, yAxis, cartesian, canvas) => {
    let axisSplitter = null;
    let altAxis = null;
    let altPlotArea = null;
    let altXScale = null;

    const chart = cartesian({
        xScale: xAxis.scale,
        yScale: yAxis.scale,
        xAxis: xAxis.component,
        yAxis: yAxis.component
    })
        .xDomain(xAxis.domain)
        .xLabel(xAxis.label)
        .xAxisHeight(xAxis.size)
        .xDecorate(xAxis.decorate)
        .xTickFormat(xAxis.tickFormatFunction)
        .yDomain(yAxis.domain)
        .yLabel(yAxis.label)
        .yAxisWidth(yAxis.size)
        .yDecorate(yAxis.decorate)
        .yOrient("left")
        .yTickFormat(yAxis.tickFormatFunction);

    if (xAxis.decorate) chart.xDecorate(xAxis.decorate);
    if (yAxis.decorate) chart.yDecorate(yAxis.decorate);

    // Padding defaults can be overridden
    chart.xPaddingInner && chart.xPaddingInner(1);
    chart.xPaddingOuter && chart.xPaddingOuter(0.5);
    chart.yPaddingInner && chart.yPaddingInner(1);
    chart.yPaddingOuter && chart.yPaddingOuter(0.5);

    chart.axisSplitter = (...args) => {
        if (!args.length) {
            return axisSplitter;
        }
        axisSplitter = args[0];
        return chart;
    };

    chart.altAxis = (...args) => {
        if (!args.length) {
            return altAxis;
        }
        altAxis = args[0];
        return chart;
    };

    chart.altPlotArea = (...args) => {
        if (!args.length) {
            return altPlotArea;
        }
        altPlotArea = args[0];
        return chart;
    };

    chart.altXScale = (...args) => {
        if (!args.length) {
            return altXScale;
        }
        altXScale = args[0];
        return chart;
    };

    const oldDecorate = chart.decorate();
    chart.decorate((container, data) => {
        oldDecorate(container, data);
        if (!axisSplitter) return;

        if (axisSplitter.haveSplit()) {
            // Render a second axis on the right of the chart
            const altData = axisSplitter.altData();

            const y2AxisDataJoin = fc.dataJoin("d3fc-svg", "y2-axis").key(d => d);
            const ySeriesDataJoin = fc.dataJoin("g", "y-series").key(d => d);

            // Column 5 of the grid
            container
                .enter()
                .append("div")
                .attr("class", "y2-label-container")
                .style("grid-column", 5)
                .style("-ms-grid-column", 5)
                .style("grid-row", 3)
                .style("-ms-grid-row", 3)
                .style("width", altAxis.size || "1em")
                .style("display", "flex")
                .style("align-items", "center")
                .style("justify-content", "center")
                .append("div")
                .attr("class", "y-label")
                .style("transform", "rotate(-90deg)");

            const y2Scale = altAxis.scale.domain(altAxis.domain);
            const yAxisComponent = altAxis.component.right(y2Scale);
            yAxisComponent.tickFormat(altAxis.tickFormatFunction);
            if (altAxis.decorate) yAxisComponent.decorate(altAxis.decorate);

            // Render the axis
            y2AxisDataJoin(container, ["right"])
                .attr("class", d => `y-axis ${d}-axis`)
                .on("measure", (d, i, nodes) => {
                    const {width, height} = d3.event.detail;
                    if (d === "left") {
                        d3.select(nodes[i])
                            .select("svg")
                            .attr("viewBox", `${-width} 0 ${width} ${height}`);
                    }
                    y2Scale.range([height, 0]);
                })
                .on("draw", (d, i, nodes) => {
                    d3.select(nodes[i])
                        .select("svg")
                        .call(yAxisComponent);
                });

            // Render all the series using either the primary or alternate y-scales
            const plotAreas = [chart.plotArea(), altPlotArea || chart.plotArea()];
            const xScales = [xAxis.scale, altXScale || xAxis.scale];
            const yScales = [yAxis.scale, y2Scale];
            if (canvas) {
                const drawMultiCanvasSeries = selection => {
                    if (altXScale) altXScale.range(xAxis.scale.range());

                    [data, altData].forEach((d, i) => {
                        const canvasPlotArea = plotAreas[i];
                        canvasPlotArea
                            .context(selection.node().getContext("2d"))
                            .xScale(xScales[i])
                            .yScale(yScales[i]);
                        canvasPlotArea(d);
                    });
                };

                container.select("d3fc-canvas.plot-area").on("draw", (d, i, nodes) => {
                    drawMultiCanvasSeries(d3.select(nodes[i]).select("canvas"));
                });
            } else {
                const drawMultiSvgSeries = selection => {
                    if (altXScale) altXScale.range(xAxis.scale.range());

                    ySeriesDataJoin(selection, [data, altData]).each((d, i, nodes) => {
                        const svgPlotArea = plotAreas[i];
                        svgPlotArea.xScale(xScales[i]).yScale(yScales[i]);
                        d3.select(nodes[i])
                            .datum(d)
                            .call(svgPlotArea);
                    });
                };

                container.select("d3fc-svg.plot-area").on("draw", (d, i, nodes) => {
                    drawMultiSvgSeries(d3.select(nodes[i]).select("svg"));
                });
            }
        }

        // Render any UI elements the splitter component requires
        axisSplitter(container);
    });

    return chart;
};
