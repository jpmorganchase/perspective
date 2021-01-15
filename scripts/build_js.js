/******************************************************************************
 *
 * Copyright (c) 2017, the Perspective Authors.
 *
 * This file is part of the Perspective library, distributed under the terms of
 * the Apache License 2.0.  The full license can be found in the LICENSE file.
 *
 */

const fs = require("fs");
const path = require("path");
const mkdirp = require("mkdirp");
const prettier = require("prettier");
const argv = require("minimist")(process.argv.slice(2));
const minimatch = require("minimatch");
const os = require("os");
const {getarg, bash, execute} = require("./script_utils.js");
const IS_CI = getarg("--ci");

//require("dotenv").config({path: "./.perspectiverc"});

/**
 * WASM Output Options
 */
const WEB_WASM_OPTIONS = {
    inputFile: "psp.async.js",
    inputWasmFile: "psp.async.wasm",
    format: false,
    packageName: "perspective",
    build: !!argv.wasm // flag as to whether to build
};

/**
 * Filter for the runtimes we should build
 */
const AVAILABLE_RUNTIMES = [WEB_WASM_OPTIONS];

// Select the runtimes - if no builds are specified then build everything
const RUNTIMES = AVAILABLE_RUNTIMES.filter(runtime => runtime.build).length ? AVAILABLE_RUNTIMES.filter(runtime => runtime.build) : AVAILABLE_RUNTIMES;

// Directory of Emscripten output

function compileRuntime({inputFile, inputWasmFile, format, packageName}) {
    console.log("-- Building %s", inputFile);

    const dir_name = process.env.PSP_DEBUG ? "debug" : "release";
    const base_dir = path.join(__dirname, "..", "packages", packageName, "build", dir_name);

    const output_dir = path.join(__dirname, "..", "packages", packageName);
    const build_dir = path.join(base_dir, "build");

    mkdirp.sync(path.join(output_dir, "dist", "obj"));
    mkdirp.sync(path.join(output_dir, "dist", "umd"));

    if (inputWasmFile) {
        console.log("-- Copying WASM file %s", inputWasmFile);
        fs.copyFileSync(path.join(build_dir, inputWasmFile), path.join(output_dir, "dist", "umd", inputWasmFile));
    }

    console.debug("-- Creating wrapped js runtime");
    const runtimeText = String(
        fs.readFileSync(path.join(build_dir, inputFile), {
            encoding: "utf8"
        })
    );

    let source = runtimeText;
    if (format) {
        console.debug("Formatting code");
        source = prettier.format(source, {
            printWidth: 200,
            tabWidth: 4,
            parser: "babylon"
        });
    }

    fs.writeFileSync(path.join(output_dir, "dist", "obj", inputFile), source);
}

function docker(image = "emsdk") {
    console.log("-- Creating emsdk docker image");
    let cmd = "docker run --rm ";
    if (!IS_CI) {
        cmd += "-it";
    }

    if (process.env.PSP_CPU_COUNT) {
        cmd += ` --cpus="${parseInt(process.env.PSP_CPU_COUNT)}.0"`;
    }
    cmd += ` -v ${process.cwd()}:/src -e PACKAGE=${process.env.PACKAGE} perspective/${image}`;
    return cmd;
}

function compileCPP(packageName) {
    const dir_name = process.env.PSP_DEBUG ? "debug" : "release";
    const base_dir = path.join(__dirname, "..", "packages", packageName, "build", dir_name);
    mkdirp.sync(base_dir);
    const cmd = bash`
        emcmake cmake ../../../../cpp/perspective -DCMAKE_BUILD_TYPE=${dir_name}
        && emmake make -j${process.env.PSP_CPU_COUNT || os.cpus().length}
    `;
    if (process.env.PSP_DOCKER) {
        execute`${docker()} bash -c "cd /src/packages/perspective/build/${dir_name} && ${cmd}"`;
    } else {
        execute`cd ${base_dir} && ${cmd}`;
    }
}

function lerna() {
    let cmd = `lerna run build --loglevel silent `;
    if (process.env.PACKAGE) {
        cmd += `--scope="@finos/${process.env.PACKAGE}" `;
    }
    execute(cmd);
}

try {
    if (!process.env.PACKAGE || minimatch("perspective", process.env.PACKAGE)) {
        compileCPP("perspective");
        RUNTIMES.map(compileRuntime);
    }

    lerna();
} catch (e) {
    console.log(e.message);
    process.exit(1);
}
