/******************************************************************************
 *
 * Copyright (c) 2017, the Perspective Authors.
 *
 * This file is part of the Perspective library, distributed under the terms of
 * the Apache License 2.0.  The full license can be found in the LICENSE file.
 *
 */

'use strict';

const TREE_COLUMN_INDEX = require('fin-hypergrid/src/behaviors/Behavior').prototype.treeColumnIndex;

module.exports = require('datasaur-local').extend('PerspectiveDataModel', {
    // Is the grid view a tree
    isTree: function() {
        return this.data[0] && this.data[0][TREE_COLUMN_INDEX] && this.data[0][TREE_COLUMN_INDEX].rowPath.length;
    },

    // Is this column the 'tree' column
    isTreeCol: function(x) {
        return x === TREE_COLUMN_INDEX && this.isTree();
    },

    // Return the value for a given cell based on (x,y) coordinates
    // All our column names are column indexes (including tree column's), so no need to indirect through schema.
    getValue: function(x, y) {
        var row = this.data[y];
        return row ? row[x] : null;
    },

    cellStyle: function(gridCellConfig, rendererName) {
        if (gridCellConfig.value === null || gridCellConfig.value === undefined) {
            gridCellConfig.value = '-';
        } else {
            var type = this.schema[gridCellConfig.dataCell.x].type;
            if (['number', 'float', 'integer'].indexOf(type) > -1) {
                if (gridCellConfig.value === 0) {
                    gridCellConfig.value = type === 'float' ? '0.00' : '0';
                } else if (isNaN(gridCellConfig.value)) {
                    gridCellConfig.value = '-';
                } else {
                    gridCellConfig.color = gridCellConfig.value >= 0
                        ? (gridCellConfig.columnHeaderBackgroundNumberPositive || 'rgb(160,207,255)')
                        : (gridCellConfig.columnHeaderBackgroundNumberNegative || 'rgb(255,136,136)');
                }
            } else if (type === 'boolean') {
                gridCellConfig.value = String(gridCellConfig.value);
            }
        }
    },

    // Return the cell renderer
    getCell: function(config, rendererName) {
        var nextRow, depthDelta;
        if (config.isUserDataArea) {
            this.cellStyle(config, rendererName);
        } else if (config.dataCell.x === TREE_COLUMN_INDEX) {
            nextRow = this.getRow(config.dataCell.y + 1);
            depthDelta = nextRow ? config.value.rowPath.length - nextRow[TREE_COLUMN_INDEX].rowPath.length : -1;
            config.last = depthDelta !== 0;
            config.expanded = depthDelta < 0;
        }
        return config.grid.cellRenderers.get(rendererName);
    },

    // Return the cell editor for a given (x,y) cell coordinate
    getCellEditorAt: function(x, y, declaredEditorName, cellEvent) {
        if (declaredEditorName) {
            var cellEditor = cellEvent.grid.cellEditors.create(declaredEditorName, cellEvent);
            if (declaredEditorName === 'combobox') {
                // cellEditor.modes[0].appendOptions = testingDropdownItems;
            }
            return cellEditor;
        }
        return declaredEditorName;
    }
});