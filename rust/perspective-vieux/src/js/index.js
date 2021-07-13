/******************************************************************************
 *
 * Copyright (c) 2018, the Perspective Authors.
 *
 * This file is part of the Perspective library, distributed under the terms
 * of the Apache License 2.0.  The full license can be found in the LICENSE
 * file.
 *
 */

const MODULE = import(
    /* webpackChunkName: "perspective-viewer.custom-element" */
    /* webpackMode: "eager" */
    "./vieux.js"
);

global.registerPlugin = async function registerPlugin(name) {
    const {wasm} = await MODULE;
    return wasm.then(x => x.register_plugin(name));
};

export async function _get_module() {
    return await MODULE;
}
