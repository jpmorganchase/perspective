/******************************************************************************
 *
 * Copyright (c) 2017, the Perspective Authors.
 *
 * This file is part of the Perspective library, distributed under the terms of
 * the Apache License 2.0.  The full license can be found in the LICENSE file.
 *
 */

const {bash, execute, getarg, docker} = require("./script_utils.js");
const minimatch = require("minimatch");
const execSync = require("child_process").execSync;
const fs = require("fs");

const IS_PUPPETEER = !!getarg("--private-puppeteer");
const IS_EMSDK = !!getarg("--private-emsdk");
const IS_WRITE = !!getarg("--write") || process.env.WRITE_TESTS;
const IS_DOCKER = !!getarg("--docker") || process.env.PSP_DOCKER;
const IS_LOCAL_PUPPETEER = fs.existsSync("node_modules/puppeteer");

const PACKAGE = process.env.PACKAGE;

if (IS_WRITE) {
    console.log("-- Running the test suite in Write mode");
}

if (getarg("--saturate")) {
    console.log("-- Running the test suite in saturate mode");
}

if (getarg("--debug")) {
    console.log("-- Running tests in debug mode - all console.log statements are preserved.");
}

function silent(x) {
    return bash`output=$(${x}); ret=$?; echo "\${output}"; exit $ret`;
}

function jest() {
    return bash`
        PSP_SATURATE=${!!getarg("--saturate")} 
        PSP_PAUSE_ON_FAILURE=${!!getarg("--interactive")}
        WRITE_TESTS=${IS_WRITE} 
        TZ=UTC 
        node_modules/.bin/jest 
        --rootDir=.
        --config=packages/perspective-test/jest.all.config.js 
        --color
        --verbose 
        --maxWorkers=50%
        ${getarg("--bail") && "--bail"}
        ${getarg("--debug") || "--silent 2>&1 --noStackTrace"} 
        --testNamePattern="${get_regex()}"`;
}

function emsdk() {
    console.log("-- Creating emsdk docker image");
    return bash`${docker("emsdk")} node scripts/test_js.js
        --private-emsdk ${getarg()}`;
}

function get_regex() {
    const regex = getarg`-t`;
    if (regex) {
        console.log(`-- Qualifying search '${regex}'`);
        return regex.replace(/ /g, ".");
    }
}

try {
    if (PACKAGE === "perspective-rust-internal" || !PACKAGE) {
        execute(`lerna exec --scope=@finos/perspective-rust-internal -- yarn test:rust`);
    }
    if (PACKAGE !== "perspective-rust-internal" || !PACKAGE) {
        if (!IS_PUPPETEER && !IS_LOCAL_PUPPETEER) {
            if (IS_DOCKER && !IS_EMSDK) {
                execute(emsdk());
            } else {
                execute`node_modules/.bin/lerna exec -- mkdir -p dist/umd`;
                execute`node_modules/.bin/lerna run test:build --stream
                --scope="@finos/${PACKAGE}"`;
            }
            if (!IS_EMSDK) {
                execute`yarn --silent clean --screenshots`;
                execute`${docker("puppeteer")} node scripts/test_js.js
                --private-puppeteer ${getarg()}`;
            }
        } else {
            if (IS_LOCAL_PUPPETEER) {
                execute`yarn --silent clean --screenshots`;
                execute`node_modules/.bin/lerna exec -- mkdir -p dist/umd`;
                execute`node_modules/.bin/lerna run test:build --stream
                --scope="@finos/${PACKAGE}"`;
            }
            if (getarg("--quiet")) {
                console.log("-- Running test suite in quiet mode");
                execute(silent(jest()));
            } else if (process.env.PACKAGE) {
                const debug = getarg("--debug") ? "" : "--silent";
                console.log("-- Running test suite in individual mode");
                execute`
                PSP_SATURATE=${!!getarg("--saturate")}
                PSP_PAUSE_ON_FAILURE=${!!getarg("--interactive")}
                WRITE_TESTS=${IS_WRITE}
                TZ=UTC 
                node_modules/.bin/lerna exec 
                --concurrency 1 
                --no-bail
                --scope="@finos/${PACKAGE}" 
                -- 
                yarn test:run
                ${debug}
                ${getarg("--interactive") && "--runInBand"}
                --testNamePattern="${get_regex()}"`;
            } else {
                console.log("-- Running test suite in fast mode");
                execute(jest());
            }
        }
    }
} catch (e) {
    console.log(e.message);
    process.exit(1);
}
