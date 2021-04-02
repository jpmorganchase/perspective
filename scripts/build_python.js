/******************************************************************************
 *
 * Copyright (c) 2017, the Perspective Authors.
 *
 * This file is part of the Perspective library, distributed under the terms of
 * the Apache License 2.0.  The full license can be found in the LICENSE file.
 *
 */

const {execute, execute_throw, docker, resolve, getarg, bash, python_image} = require("./script_utils.js");
const fs = require("fs-extra");

const PYTHON = getarg("--python39") ? "python3.9" : getarg("--python38") ? "python3.8" : getarg("--python36") ? "python3.6" : "python3.7";
let IMAGE = "manylinux2010";
const IS_DOCKER = process.env.PSP_DOCKER;

if (IS_DOCKER) {
    // defaults to 2010
    let MANYLINUX_VERSION = "manylinux2010";

    // switch to 2014 only on python3
    (MANYLINUX_VERSION = getarg("--manylinux2010") ? "manylinux2010" : getarg("--manylinux2014") ? "manylinux2014" : "manylinux2010"), PYTHON;

    IMAGE = python_image(MANYLINUX_VERSION, PYTHON);
}

const IS_CI = getarg("--ci");
const SETUP_ONLY = getarg("--setup-only");
const IS_INSTALL = getarg("--install");

// Check that the `PYTHON` command is valid, else default to `python`.
try {
    execute_throw`${PYTHON} --version`;
} catch (e) {
    console.warn(`\`${PYTHON}\` not found - using \`python\` instead.`);
    PYTHON = "python";
}
try {
    const dist = resolve`${__dirname}/../python/perspective/dist`;
    const cpp = resolve`${__dirname}/../cpp/perspective/src`;
    const cmakelists = resolve`${__dirname}/../cpp/perspective/CMakeLists.txt`;
    const lic = resolve`${__dirname}/../LICENSE`;
    const cmake = resolve`${__dirname}/../cmake`;
    const dcmake = resolve`${dist}/cmake`;
    const dlic = resolve`${dist}/LICENSE`;

    fs.mkdirpSync(dist);
    fs.copySync(cmakelists, resolve`${dist}/CMakeLists.txt`, {preserveTimestamps: true});
    fs.copySync(cpp, resolve`${dist}/src`, {preserveTimestamps: true});
    fs.copySync(lic, dlic, {preserveTimestamps: true});
    fs.copySync(cmake, dcmake, {preserveTimestamps: true});

    if (SETUP_ONLY) {
        // don't execute any build steps, just copy
        // the C++ assets into the python folder
        return;
    }

    let cmd;
    if (IS_CI) {
        cmd = bash`${PYTHON} -m pip install -e .[dev] --no-clean &&`;

        // pip install in-place with --no-clean so that pep-518 assets stick
        // around for later wheel build (so cmake cache can stay in place)
        //
        // lint the folder with flake8
        //
        // pytest the client first (since we need to move the shared libraries out of place
        // temporarily to simulate them not being installed)
        //
        // then run the remaining test suite
        cmd =
            cmd +
            `${PYTHON} -m flake8 perspective && echo OK && \
            ${PYTHON} -m pytest -vvv --noconftest perspective/tests/client_mode && \
            ${PYTHON} -m pytest -vvv perspective \
            --ignore=perspective/tests/client_mode \
            --junitxml=python_junit.xml --cov-report=xml --cov-branch \
            --cov=perspective`;
        if (IMAGE == "python") {
            // test the sdist to make sure we dont
            // dist a non-functioning source dist
            cmd =
                cmd +
                `&& \
                ${PYTHON} setup.py sdist && \
                ${PYTHON} -m pip install -U dist/*.tar.gz`;
        }
    } else if (IS_INSTALL) {
        cmd = `${PYTHON} -m pip install .`;
    } else {
        cmd = bash`${PYTHON} setup.py build -v`;
    }

    if (IS_DOCKER) {
        execute`${docker(IMAGE)} bash -c "cd python/perspective && \
            ${cmd} "`;
    } else {
        const python_path = resolve`${__dirname}/../python/perspective`;
        execute`cd ${python_path} && ${cmd}`;
    }
} catch (e) {
    console.log(e.message);
    process.exit(1);
}
