/******************************************************************************
 *
 * Copyright (c) 2017, the Perspective Authors.
 *
 * This file is part of the Perspective library, distributed under the terms of
 * the Apache License 2.0.  The full license can be found in the LICENSE file.
 *
 */

const get_wppp = () => {
    // eslint-disable-next-line no-undef
    return __webpack_public_path__;
};

const set_wppp = x => {
    // eslint-disable-next-line no-undef
    __webpack_public_path__ = x;
};

/**
 * Executs a function `body()` within a scope with a modified
 * `__webpack_public_path__` var, so that assets imported lazily are
 * executed relative to their script, rather than the root script.
 * Necessary if oyou expect webpack'd WASM assets to work when imported from
 * e.g. a CDN.
 * @param {Function} body The function to execute in a modified
 * `__webpack_public_path__` scope.
 */
export function import_script_relative(body) {
    const url = new URL(document.currentScript.src);
    const old = get_wppp();
    set_wppp(url.origin + url.pathname.replace(/\/(?:.(?!\/))+?$/, "/"));
    const result = body();
    set_wppp(old);
    return result;
}

/**
 * Gets human-readable types for a column
 * @private
 * @returns {string}
 */
export function get_column_type(val) {
    if (val >= 1 && val <= 8) {
        return "integer";
    } else if (val === 19) {
        return "string";
    } else if (val === 10 || val === 9) {
        return "float";
    } else if (val === 11) {
        return "boolean";
    } else if (val === 12) {
        return "datetime";
    } else if (val === 13) {
        return "date";
    } else {
        console.warn(`Unknown type for value ${val} with JS type ${typeof val}`);
    }
}

/**
 * Bind all methods in a class to the class instance.  It is sad that this is
 * necessary.
 *
 * @export
 * @param {*} self
 */
export function bindall(self) {
    let obj = self;
    do {
        for (const key of Object.getOwnPropertyNames(obj)) {
            const value = self[key];
            if (key !== "constructor" && typeof value === "function") {
                self[key] = value.bind(self);
            }
        }
    } while ((obj = obj !== Object && Object.getPrototypeOf(obj)));
}

/**
 * Detect Node.js.
 *
 * Returns
 * -------
 * True if the current script is running in Node.js.
 */
export function detectNode() {
    return typeof window === "undefined";
}

/**
 * Detect Internet Explorer.
 *
 * Returns
 * -------
 * True if the current script is running in Internet Explorer.
 */
export const detectIE = require("detectie");

/**
 * Detect Chrome.
 *
 * Returns
 * -------
 * Detect if the current script is running in Chrome.
 */
export function detectChrome() {
    var isChromium = window.chrome,
        winNav = window.navigator,
        vendorName = winNav.vendor,
        isOpera = winNav.userAgent.indexOf("OPR") > -1,
        isIEedge = winNav.userAgent.indexOf("Edge") > -1,
        isIOSChrome = winNav.userAgent.match("CriOS");

    if (isIOSChrome) {
        return true;
    } else if (isChromium !== null && typeof isChromium !== "undefined" && vendorName === "Google Inc." && isOpera === false && isIEedge === false) {
        return true;
    } else {
        return false;
    }
}

// https://github.com/kripken/emscripten/issues/6042
export function detect_iphone() {
    return /iPad|iPhone|iPod/.test(navigator.userAgent) && !window.MSStream;
}

/**
 * String.includes() polyfill
 */
if (!String.prototype.includes) {
    String.prototype.includes = function(search, start) {
        if (typeof start !== "number") {
            start = 0;
        }

        if (start + search.length > this.length) {
            return false;
        } else {
            return this.indexOf(search, start) !== -1;
        }
    };
}

/* eslint-disable-next-line max-len */
// from https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/includes
if (!Array.prototype.includes) {
    Object.defineProperty(Array.prototype, "includes", {
        value: function(searchElement, fromIndex) {
            if (this == null) {
                throw new TypeError('"this" is null or not defined');
            }

            // 1. Let O be ? ToObject(this value).
            var o = Object(this);

            // 2. Let len be ? ToLength(? Get(O, "length")).
            var len = o.length >>> 0;

            // 3. If len is 0, return false.
            if (len === 0) {
                return false;
            }

            // 4. Let n be ? ToInteger(fromIndex). (If fromIndex is undefined,
            //    this step produces the value 0.)
            var n = fromIndex | 0;

            // 5. If n ≥ 0, then a. Let k be n.
            // 6. Else n < 0, a. Let k be len + n. b. If k < 0, let k be 0.
            var k = Math.max(n >= 0 ? n : len - Math.abs(n), 0);

            function sameValueZero(x, y) {
                return x === y || (typeof x === "number" && typeof y === "number" && isNaN(x) && isNaN(y));
            }

            // 7. Repeat, while k < len
            while (k < len) {
                // a. Let elementK be the result of ? Get(O, ! ToString(k)). b.
                // If SameValueZero(searchElement, elementK) is true, return
                // true.
                if (sameValueZero(o[k], searchElement)) {
                    return true;
                }
                // c. Increase k by 1.
                k++;
            }

            // 8. Return false
            return false;
        }
    });
}
