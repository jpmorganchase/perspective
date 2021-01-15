/******************************************************************************
 *
 * Copyright (c) 2017, the Perspective Authors.
 *
 * This file is part of the Perspective library, distributed under the terms of
 * the Apache License 2.0.  The full license can be found in the LICENSE file.
 *
 */

const execSync = require("child_process").execSync;
const execute = cmd => execSync(cmd, {stdio: "inherit"});

const inquirer = require("inquirer");
const fs = require("fs");

const CONFIG = new Proxy(
    new (class {
        constructor() {
            this.config = [];
            this._values = require("dotenv").config({path: "./.perspectiverc"}).parsed || {};
        }

        add(new_config) {
            for (const key in new_config) {
                const val = new_config[key];
                if (val !== "" && !!val) {
                    this._values[key] = val;
                    this.config.push(`${key}=${val}`);
                }
            }
        }

        write() {
            fs.writeFileSync("./.perspectiverc", this.config.join("\n"));
            if (process.env.PSP_BUILD_IMMEDIATELY) {
                execute("node scripts/build.js");
            }
        }
    })(),
    {
        set: function(target, name, val) {
            target.add({[name]: val});
        },
        get: function(target, name) {
            if (name in target._values) {
                return target._values[name];
            } else {
                return target[name];
            }
        }
    }
);

const PROMPT_DEBUG = {
    type: "confirm",
    name: "PSP_DEBUG",
    message: "Run debug build?",
    default: CONFIG["PSP_DEBUG"] || false
};

const PROMPT_DOCKER = {
    type: "confirm",
    name: "PSP_DOCKER",
    message: "Use docker for build env?",
    default: CONFIG["PSP_DOCKER"] || false
};

async function choose_docker() {
    const answers = await inquirer.prompt([PROMPT_DOCKER]);
    CONFIG.add(answers);
    CONFIG.write();
}

async function focus_package() {
    const new_config = await inquirer.prompt([
        {
            type: "expand",
            name: "PACKAGE",
            message: "Focus NPM package?",
            default: CONFIG["PACKAGE"] || "h",
            choices: [
                {
                    key: "a",
                    name: "(all)",
                    value: ""
                },
                {
                    key: "p",
                    name: "perspective",
                    value: "perspective"
                },
                {
                    key: "v",
                    name: "perspective-viewer",
                    value: "perspective-viewer"
                },
                {
                    key: "e",
                    name: "perspective-viewer-datagrid",
                    value: "perspective-viewer-datagrid"
                },

                {
                    key: "g",
                    name: "perspective-viewer-hypergrid",
                    value: "perspective-viewer-hypergrid"
                },
                {
                    key: "d",
                    name: "perspective-viewer-d3fc",
                    value: "perspective-viewer-d3fc"
                },
                {
                    key: "c",
                    name: "perspective-viewer-highcharts",
                    value: "perspective-viewer-highcharts"
                },
                {
                    key: "o",
                    name: "perspective-phosphor",
                    value: "perspective-phosphor"
                },
                {
                    key: "l",
                    name: "perspective-jupyterlab",
                    value: "perspective-jupyterlab"
                },
                {
                    key: "w",
                    name: "perspective-workspace",
                    value: "perspective-workspace"
                }
            ]
        }
    ]);
    CONFIG.add(new_config);
    await javascript_options();
}

async function javascript_options() {
    const new_config = await inquirer.prompt([
        PROMPT_DEBUG,
        PROMPT_DOCKER,
        {
            type: "confirm",
            name: "PSP_DOCKER_PUPPETEER",
            message: "Use docker for puppeteer tests?",
            default: !CONFIG["PSP_DOCKER_PUPPETEER"]
        }
    ]);
    new_config.PSP_DOCKER_PUPPETEER = !new_config.PSP_DOCKER_PUPPETEER;
    const local_puppeteer = fs.existsSync("node_modules/puppeteer");
    CONFIG.add(new_config);
    CONFIG.write();
    if (local_puppeteer !== new_config.PSP_DOCKER_PUPPETEER) {
        require("./toggle_puppeteer");
    }
}

async function options() {
    const new_config = await inquirer.prompt([PROMPT_DEBUG, PROMPT_DOCKER]);
    CONFIG.add(new_config);
    CONFIG.write();
}

async function choose_project() {
    const answers = await inquirer.prompt([
        {
            type: "expand",
            name: "PSP_PROJECT",
            message: "Focus (J)avascript, (P)ython, or (R)ust?",
            default: CONFIG["PSP_PROJECT"] || "js",
            choices: [
                {
                    key: "j",
                    name: "Javascript",
                    value: "js"
                },
                {
                    key: "p",
                    name: "Python",
                    value: "python"
                },
                {
                    key: "r",
                    name: "Rust",
                    value: "rust"
                }
            ]
        }
    ]);
    CONFIG.add(answers);
    CONFIG.write();
    switch (CONFIG.PSP_PROJECT) {
        case "js":
            {
                await focus_package();
            }
            break;
        case "python":
        case "rust":
            {
                await options();
            }
            break;
        default: {
            choose_docker();
        }
    }
}

choose_project();
