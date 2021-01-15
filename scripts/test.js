/******************************************************************************
 *
 * Copyright (c) 2017, the Perspective Authors.
 *
 * This file is part of the Perspective library, distributed under the terms of
 * the Apache License 2.0.  The full license can be found in the LICENSE file.
 *
 */

const fs = require("fs");
require("dotenv").config({path: "./.perspectiverc"});

if (!fs.existsSync("./.perspectiverc")) {
    console.error("No .perspectiverc, running setup");
    process.env.PSP_BUILD_IMMEDIATELY = 1;
    require("./setup");
} else if (process.env.PSP_PROJECT === "js") {
    require("./test_js");
} else if (process.env.PSP_PROJECT === "python") {
    require("./test_python");
} else if (process.env.PSP_PROJECT === "rust") {
    require("./test_rust");
} else if (process.env.PSP_PROJECT === "cpp") {
    require("./test_cpp");
} else if (process.env.PSP_PROJECT === "") {
    require("./test_js");
    require("./test_python");
    require("./test_cpp");
} else {
    console.error(`Invalid project "${process.env.PSP_PROJECT}" selected, running setup`);
    process.env.PSP_BUILD_IMMEDIATELY = 1;
    require("./setup");
}
