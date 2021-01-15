/******************************************************************************
 *
 * Copyright (c) 2017, the Perspective Authors.
 *
 * This file is part of the Perspective library, distributed under the terms of
 * the Apache License 2.0.  The full license can be found in the LICENSE file.
 *
 */

const {execute, clean} = require("./script_utils.js");

const glob = require("glob");
const minimatch = require("minimatch");
const args = process.argv.slice(2);

const IS_SCREENSHOTS = args.indexOf("--screenshots") !== -1;

function clean_screenshots() {
    if (args.indexOf("--all") !== -1) {
        try {
            execute`lerna exec --scope="@finos/${process.env.PACKAGE}" -- yarn rimraf screenshots`;
        } catch (e) {}
    } else {
        execute`lerna exec --scope="@finos/*" -- mkdirp screenshots`;
        execute`lerna run clean:screenshots --ignore-missing --scope="@finos/${process.env.PACKAGE}"`;
    }
}

try {
    if (!process.env.PSP_PROJECT || args.indexOf("--deps") > -1) {
        clean`packages/perspective/build`;
    }

    if (process.env.PSP_PROJECT === "python") {
        clean(
            "python/perspective/dist",
            "python/perspective/build",
            "python/perspective/docs/build",
            "python/perspective/perspective_python.egg-info",
            "python/perspective/.coverage",
            "python/perspective/.pytest_cache",
            "python/perspective/python_junit.xml",
            "python/perspective/coverage.xml",
            ...glob.sync("python/perspective/**/*.pyc"),
            ...glob.sync("python/perspective/**/__pycache__")
        );
        return;
    }

    if (process.env.PSP_PROJECT === "rust") {
        clean("rust/perspective/arrow_accessor/target", "rust/perspective/arrow_accessor/pkg");
        return;
    }

    if (!IS_SCREENSHOTS && (!process.env.PACKAGE || minimatch("perspective", process.env.PACKAGE))) {
        clean`cpp/perspective/cppbuild`;
        const files = ["CMakeFiles", "build", "cmake_install.cmake", "CMakeCache.txt", "compile_commands.json", "libpsp.a", "Makefile"];
        clean(...files.map(x => `cpp/perspective/obj/${x}`));
    }
    if (!IS_SCREENSHOTS) {
        execute`lerna run clean --scope="@finos/${process.env.PACKAGE}"`;
        clean("docs/build", "docs/python", "docs/obj");
    }
    clean_screenshots();
} catch (e) {
    console.error(e.message);
    process.exit(1);
}
