import babel from "@rollup/plugin-babel";
import filesize from "rollup-plugin-filesize";
import postcss from "rollup-plugin-postcss";
import sourcemaps from "rollup-plugin-sourcemaps";
import path from "path";
import fs from "fs";

export default () => {
    return [
        {
            input: `src/less/perspective-vieux.less`,
            output: {
                dir: "dist/css"
            },
            plugins: [
                postcss({
                    inject: false,
                    extract: path.resolve(`dist/css/perspective-vieux.css`),
                    minimize: {preset: "lite"}
                })
            ]
        },
        {
            input: `src/less/column-style.less`,
            output: {
                dir: "dist/css"
            },
            plugins: [
                postcss({
                    inject: false,
                    extract: path.resolve(`dist/css/column-style.css`),
                    minimize: {preset: "lite"}
                })
            ]
        },
        {
            input: `src/less/expression-editor.less`,
            output: {
                dir: "dist/css"
            },
            plugins: [
                postcss({
                    inject: false,
                    extract: path.resolve(`dist/css/expression-editor.css`),
                    minimize: {preset: "lite"}
                })
            ]
        },

        {
            input: "src/js/index.js",
            external: [/node_modules/, /pkg/, /monaco\-editor/],
            output: {
                sourcemap: true,
                dir: "dist/esm/"
            },
            plugins: [
                babel({
                    exclude: "node_modules/**",
                    babelHelpers: "bundled"
                }),
                filesize(),
                postcss({
                    inject: false,
                    sourceMap: true,
                    minimize: {mergeLonghand: false}
                }),
                sourcemaps()
            ].filter(x => x),
            watch: {
                clearScreen: false
            }
        },
        ...generate_themes()
    ];
};

const THEMES = fs.readdirSync(path.resolve(__dirname, "src", "themes"));

function generate_themes() {
    function reducer(key, val) {
        return {
            input: `${val}`,
            output: {
                dir: "dist/umd"
            },
            plugins: [
                {
                    name: "remove-js-after-hook",
                    resolveId(source) {
                        return null;
                    },
                    buildEnd: () => {
                        fs.rm(path.resolve(__dirname, "dist", "umd", `${key}.js`), () => {});
                    },
                    load(id) {
                        return null;
                    }
                },
                postcss({
                    inject: false,
                    extract: path.resolve(`dist/umd/${key}.css`),
                    sourceMap: false,
                    minimize: false
                })
            ]
        };
    }

    return THEMES.map(theme => reducer(theme.replace(".less", ""), path.resolve(__dirname, "src", "themes", theme)));
}
