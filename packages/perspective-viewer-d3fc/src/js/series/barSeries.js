/******************************************************************************
 *
 * Copyright (c) 2017, the Perspective Authors.
 *
 * This file is part of the Perspective library, distributed under the terms of
 * the Apache License 2.0.  The full license can be found in the LICENSE file.
 *
 */
import * as fc from "d3fc";
import {tooltip} from "../tooltip/tooltip";

export function barSeries(settings, color, options = {mixCharts: false}) {
    let series = settings.mainValues.length > 1 ? fc.seriesSvgGrouped(fc.seriesSvgBar()) : fc.seriesSvgBar();

    series = series.decorate(selection => {
        if (!options.mixCharts) tooltip().settings(settings)(selection);
        selection.style("fill", d => color(d.key));
    });

    return fc
        .autoBandwidth(minBandwidth(series))
        .crossValue(d => d.crossValue)
        .mainValue(d => d.mainValue)
        .baseValue(d => d.baseValue);
}

const minBandwidth = adaptee => {
    const min = arg => {
        return adaptee(arg);
    };

    fc.rebindAll(min, adaptee);

    min.bandwidth = (...args) => {
        if (!args.length) {
            return adaptee.bandwidth();
        }
        adaptee.bandwidth(Math.max(args[0], 1));
        return min;
    };

    return min;
};
