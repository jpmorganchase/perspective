/******************************************************************************
 *
 * Copyright (c) 2021, the Perspective Authors.
 *
 * This file is part of the Perspective library, distributed under the terms of
 * the Apache License 2.0.  The full license can be found in the LICENSE file.
 *
 */

module.exports = {
    presets: [
        [
            "@babel/preset-env",
            {
                targets: {
                    chrome: "70",
                    node: "8",
                    ios: "12",
                    safari: "12",
                    edge: "44"
                },
                modules: false,
                useBuiltIns: "usage",
                corejs: 3
            }
        ]
    ],
    sourceType: "unambiguous",
    plugins: [
        "lodash",
        ["@babel/plugin-proposal-decorators", {legacy: true}],
        "transform-custom-element-classes",
        "@babel/plugin-proposal-optional-chaining",
        "@babel/plugin-proposal-class-properties"
    ]
};
