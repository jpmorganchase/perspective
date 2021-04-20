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
                    node: "12",
                    ios: "13"
                },
                modules: process.env.BABEL_MODULE || false,
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
        "@babel/plugin-proposal-class-properties",
        "@babel/plugin-proposal-optional-chaining"
    ]
};
