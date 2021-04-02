/******************************************************************************
 *
 * Copyright (c) 2017, the Perspective Authors.
 *
 * This file is part of the Perspective library, distributed under the terms of
 * the Apache License 2.0.  The full license can be found in the LICENSE file.
 *
 */
const {execute, docker, resolve, getarg, python_image} = require("./script_utils.js");

const IS_DOCKER = process.env.PSP_DOCKER;
const PYTHON = getarg("--python39") ? "python3.9" : getarg("--python38") ? "python3.8" : getarg("--python36") ? "python3.6" : "python3.7";

let IMAGE = "manylinux2014";

if (IS_DOCKER) {
    // defaults to 2010
    let MANYLINUX_VERSION = "manylinux2010";

    // switch to 2014 only on python3
    MANYLINUX_VERSION = getarg("--manylinux2010") ? "manylinux2010" : getarg("--manylinux2014") ? "manylinux2014" : "manylinux2014";

    IMAGE = python_image(MANYLINUX_VERSION, PYTHON);
}

const IS_FIX = getarg("--fix");

try {
    let cmd;
    let lint_cmd = `${PYTHON} -m flake8 perspective bench setup.py`;
    let fix_cmd = `black perspective bench setup.py --exclude tests`;

    if (process.env.PSP_DOCKER) {
        cmd = `cd python/perspective && ${IS_FIX ? fix_cmd : lint_cmd}`;
        execute`${docker(IMAGE)} bash -c "${cmd}"`;
    } else {
        const python_path = resolve`${__dirname}/../python/perspective`;
        execute`cd ${python_path} && ${IS_FIX ? fix_cmd : lint_cmd}`;
    }
} catch (e) {
    console.log(e);
    process.exit(1);
}
