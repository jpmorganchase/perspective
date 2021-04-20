/******************************************************************************
 *
 * Copyright (c) 2018, the Perspective Authors.
 *
 * This file is part of the Perspective library, distributed under the terms of
 * the Apache License 2.0.  The full license can be found in the LICENSE file.
 *
 */

const path = require("path");

module.exports = {
    mode: process.env.PSP_NO_MINIFY || process.env.PSP_DEBUG ? "development" : process.env.NODE_ENV || "production",
    entry: {
        index: "./src/js/index.js"
    },
    resolve: {
        extensions: [".js"],
        fallback: {
            path: false,
            fs: false,
            crypto: false
        }
    },
    performance: {
        hints: false,
        maxEntrypointSize: 512000,
        maxAssetSize: 512000
    },
    externals: [/^([a-z0-9]|@(?!finos\/perspective-viewer))/],
    stats: {modules: false, hash: false, version: false, builtAt: false, entrypoints: false},
    module: {
        rules: [
            {
                test: /\.less$/,
                use: [{loader: "style-loader"}, {loader: "css-loader"}, {loader: "less-loader"}]
            },
            {
                test: /\.css$/,
                exclude: /node_modules/,
                use: [{loader: "css-loader"}]
            },
            {
                test: /\.(html)$/,
                use: {
                    loader: "html-loader",
                    options: {}
                }
            },
            {
                test: /\.js$/,
                exclude: /node_modules/,
                loader: "babel-loader"
            },
            {
                test: /\.wasm$/,
                type: "javascript/auto",
                include: path.dirname(require.resolve("@finos/perspective-viewer")),
                loader: "arraybuffer-loader"
            }
        ]
    },
    externalsPresets: {web: false, webAsync: true},
    experiments: {
        syncWebAssembly: true
    },
    output: {
        filename: "[name].js",
        libraryTarget: "commonjs2",
        publicPath: "",
        path: path.resolve(__dirname, "../../dist")
    }
};
