/******************************************************************************
 *
 * Copyright (c) 2021, the Perspective Authors.
 *
 * This file is part of the Perspective library, distributed under the terms of
 * the Apache License 2.0.  The full license can be found in the LICENSE file.
 *
 */
module.exports = async function() {
    await global.__BROWSER__.close();
};
