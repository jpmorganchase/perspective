/******************************************************************************
 *
 * Copyright (c) 2017, the Perspective Authors.
 *
 * This file is part of the Perspective library, distributed under the terms of
 * the Apache License 2.0.  The full license can be found in the LICENSE file.
 *
 */

const {execute, docker, clean, resolve, getarg, bash, python_image} = require("./script_utils.js");
const fs = require("fs-extra");
const IS_DOCKER = process.env.PSP_DOCKER;
const IS_MACOS = getarg("--macos");
const PYTHON = getarg("--python39") ? "python3.9" : getarg("--python38") ? "python3.8" : getarg("--python36") ? "python3.6" : "python3.7";

let IMAGE = "manylinux2014";
let MANYLINUX_VERSION;

if (IS_DOCKER) {
    // defaults to 2010
    MANYLINUX_VERSION = "manylinux2010";

    // switch to 2014 only on python3
    MANYLINUX_VERSION = getarg("--manylinux2010") ? "manylinux2010" : getarg("--manylinux2014") ? "manylinux2014" : "manylinux2014";

    IMAGE = python_image(MANYLINUX_VERSION, PYTHON);
}

/**
 * Using Perspective's docker images, create a wheel built for the image
 * architecture and output it to the local filesystem. A thin wrapper around
 * `bdist_wheel`, except it also automatically calls `auditwheel` (Linux) or
 * `delocate` on Mac.
 */
try {
    console.log("Copying assets to `dist` folder");
    const dist = resolve`${__dirname}/../python/perspective/dist`;
    const cpp = resolve`${__dirname}/../cpp/perspective`;
    const lic = resolve`${__dirname}/../LICENSE`;
    const cmake = resolve`${__dirname}/../cmake`;
    const dcmake = resolve`${dist}/cmake`;
    const dlic = resolve`${dist}/LICENSE`;
    const obj = resolve`${dist}/obj`;

    fs.mkdirpSync(dist);
    fs.copySync(cpp, dist, {preserveTimestamps: true});
    fs.copySync(lic, dlic, {preserveTimestamps: true});
    fs.copySync(cmake, dcmake, {preserveTimestamps: true});
    clean(obj);

    let cmd = bash``;

    // Create a wheel
    if (MANYLINUX_VERSION) {
        // install deps

        // These are system deps that may only be in place from pep-517/518 so
        // lets reinstall them to be sure
        cmd += `${PYTHON} -m pip install 'numpy>=1.13.1' && `;

        // remove the build folder so we completely rebuild (and pick up the
        // libs we just installed above, since this build method won't use
        // pep-517/518)
        cmd += `rm -rf build/ &&`;

        // now build the wheel in place
        cmd += `${PYTHON} setup.py bdist_wheel`;

        // Use auditwheel on Linux - repaired wheels are in
        // `python/perspective/wheelhouse`.
        let PYTHON_INTERPRETER = PYTHON;
        cmd += `&& ${PYTHON_INTERPRETER} -m auditwheel -v show ./dist/*.whl && ${PYTHON_INTERPRETER} -m auditwheel -v repair -L .lib ./dist/*.whl`;
    } else if (IS_MACOS) {
        // Don't need to do any cleaning here since we will reuse the cmake
        // cache and numpy paths from the pep-517/518 build in build_python.js
        cmd += `${PYTHON} setup.py bdist_wheel`;
        cmd += " && mkdir -p ./wheelhouse && cp -v ./dist/*.whl ./wheelhouse";
    } else {
        cmd += `${PYTHON} setup.py bdist_wheel`;
    }

    // TODO: MacOS wheel processed with delocate segfaults on
    // `import perspective`.

    if (IS_DOCKER) {
        console.log(`Building wheel for \`perspective-python\` using image \`${IMAGE}\` in Docker`);
        execute`${docker(IMAGE)} bash -c "cd python/perspective && ${cmd}"`;
    } else {
        console.log(`Building wheel for \`perspective-python\``);
        const python_path = resolve`${__dirname}/../python/perspective`;
        execute`cd ${python_path} && ${cmd}`;
    }
} catch (e) {
    console.error(e.message);
    process.exit(1);
}
