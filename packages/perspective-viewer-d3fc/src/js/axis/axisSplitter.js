/******************************************************************************
 *
 * Copyright (c) 2017, the Perspective Authors.
 *
 * This file is part of the Perspective library, distributed under the terms of
 * the Apache License 2.0.  The full license can be found in the LICENSE file.
 *
 */
import {splitterLabels} from "./splitterLabels";

export const axisSplitter = (settings, sourceData, splitFn = dataSplitFunction) => {
    // splitMainValues is an array of main-value names to put into the alt-axis
    const splitMainValues = settings.splitMainValues || [];

    let color;
    let data;
    let altData;
    let decorate = () => {};

    // Renderer to show the special controls for moving between axes
    const splitter = selection => {
        if (settings["mainValues"].length !== 1) {
            const labelsInfo = settings["mainValues"].map((v, i) => ({
                index: i,
                name: v.name
            }));
            const mainLabels = labelsInfo.filter(v => !splitter.isOnAltAxis(v.name));
            const altLabels = labelsInfo.filter(v => splitter.isOnAltAxis(v.name));

            const labeller = () => splitterLabels(settings).color(color);

            selection.select(".y-label-container>.y-label").call(labeller().labels(mainLabels));
            selection.select(".y2-label-container>.y-label").call(
                labeller()
                    .labels(altLabels)
                    .alt(true)
            );
        }

        decorate(selection.select(".y-label-container"), 0);
        decorate(selection.select(".y2-label-container"), 1);
    };

    splitter.isOnAltAxis = name => {
        // Check whether this "aggregate" name should be on the alternate y-axis
        const split = name.split("|");
        return splitMainValues.includes(split[split.length - 1]);
    };

    const haveSplit = settings["mainValues"].some(m => splitter.isOnAltAxis(m.name));
    splitter.haveSplit = () => haveSplit;

    if (sourceData) {
        // Split the data into main and alt displays
        data = haveSplit ? splitFn(sourceData, key => !splitter.isOnAltAxis(key)) : sourceData;
        altData = haveSplit ? splitFn(sourceData, splitter.isOnAltAxis) : null;
    }

    splitter.color = (...args) => {
        if (!args.length) {
            return color;
        }
        color = args[0];
        return splitter;
    };

    splitter.data = (...args) => {
        if (!args.length) {
            return data;
        }
        data = args[0];
        return splitter;
    };
    splitter.altData = (...args) => {
        if (!args.length) {
            return altData;
        }
        altData = args[0];
        return splitter;
    };
    splitter.decorate = (...args) => {
        if (!args.length) {
            return decorate;
        }
        decorate = args[0];
        return splitter;
    };

    return splitter;
};

export const dataSplitFunction = (sourceData, isIncludedFn) => {
    return sourceData.map(d => d.filter(v => isIncludedFn(v.key)));
};

export const groupBlankFunction = (sourceData, isIncludedFn) => {
    return sourceData.map(series => {
        if (!isIncludedFn(series.key)) {
            // Blank this data
            return series.map(v => Object.assign({}, v, {mainValue: null}));
        }
        return series;
    });
};

export const multiGroupBlankFunction = (sourceData, isIncludedFn) => {
    return sourceData.map(group => groupBlankFunction(group, isIncludedFn));
};

export const groupRemoveFunction = (sourceData, isIncludedFn) => {
    return sourceData.filter(series => isIncludedFn(series.key));
};

export const multiGroupRemoveFunction = (sourceData, isIncludedFn) => {
    return sourceData.map(group => groupRemoveFunction(group, isIncludedFn));
};
