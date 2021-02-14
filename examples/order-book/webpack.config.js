/******************************************************************************
 *
 * Copyright (c) 2017, the Perspective Authors.
 *
 * This file is part of the Perspective library, distributed under the terms of
 * the Apache License 2.0.  The full license can be found in the LICENSE file.
 *
 */

const PerspectivePlugin = require("@finos/perspective-webpack-plugin");
const HtmlWebPackPlugin = require("html-webpack-plugin");

module.exports = {
    mode: process.env.NODE_ENV || "development",
    entry: "./src/index.js",
    output: {
        filename: "index.js"
    },
    plugins: [
        new HtmlWebPackPlugin({
            title: "Workspace Example",
            template: "./src/index.html"
        }),
        new PerspectivePlugin()
    ],
    module: {
        rules: [
            {
                test: /\.less$/,
                use: [{loader: "style-loader"}, {loader: "css-loader"}, {loader: "less-loader"}]
            }
        ]
    },
    devtool: "source-map"
};
