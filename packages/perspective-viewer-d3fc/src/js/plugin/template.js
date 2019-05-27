/******************************************************************************
 *
 * Copyright (c) 2017, the Perspective Authors.
 *
 * This file is part of the Perspective library, distributed under the terms of
 * the Apache License 2.0.  The full license can be found in the LICENSE file.
 *
 */
import * as d3 from "d3";

import style from "../../less/chart.less";
import template from "../../html/d3fc-chart.html";
import {areArraysEqualSimple} from "../utils/utils";
import {initialiseStyles} from "../series/colorStyles";

import {bindTemplate} from "@finos/perspective-viewer/cjs/js/utils";

const styleWithD3FC = `${style}${getD3FCStyles()}`;

@bindTemplate(template, styleWithD3FC) // eslint-disable-next-line no-unused-vars
class D3FCChartElement extends HTMLElement {
    constructor() {
        super();
        this._chart = null;
        this._settings = null;
    }

    connectedCallback() {
        console.log("connected callback");
        this._container = this.shadowRoot.querySelector(".chart");
    }

    render(chart, settings) {
        this._chart = chart;
        this._settings = this._configureSettings(this._settings, settings);
        initialiseStyles(this._container, this._settings);

        if ((this._settings.data && this._settings.data.length > 0) || chart.plugin.type !== this._chart.plugin.type) {
            this.remove();
        }
        this.draw();

        if (window.navigator.userAgent.indexOf("Edge") > -1) {
            // Workaround for MS Edge issue that doesn't draw content in the
            // plot-area until the chart D3 object is redrawn
            setTimeout(() => this.draw());
        }
    }

    draw() {
        if (this._settings.data) {
            const containerDiv = d3.select(this._container);
            const chartClass = `chart ${this._chart.plugin.type}`;
            this._settings.size = this._container.getBoundingClientRect();

            if (this._settings.data.length > 0) {
                this._chart(containerDiv.attr("class", chartClass), this._settings);
            } else {
                containerDiv.attr("class", `${chartClass} disabled`);
            }
        }
    }

    resize() {
        this.draw();
    }

    remove() {
        this._container.innerHTML = "";
    }

    delete() {
        this.remove();
    }

    getContainer() {
        return this._container;
    }

    getSettings() {
        const excludeSettings = ["crossValues", "mainValues", "splitValues", "filter", "data", "size", "colorStyles"];
        const settings = {...this._settings};
        excludeSettings.forEach(s => {
            delete settings[s];
        });
        return settings;
    }

    setSettings(settings) {
        this._settings = {...this._settings, ...settings};
        this.draw();
    }

    _configureSettings(oldSettings, newSettings) {
        if (oldSettings) {
            if (!oldSettings.data) {
                // Combine with the restored settings
                return {...oldSettings, ...newSettings};
            }

            const oldValues = [oldSettings.crossValues, oldSettings.mainValues, oldSettings.splitValues];
            const newValues = [newSettings.crossValues, newSettings.mainValues, newSettings.splitValues];
            if (areArraysEqualSimple(oldValues, newValues)) return {...oldSettings, data: newSettings.data, colorStyles: null};
        }
        this.remove();

        // Some settings can be preserved even when the chart schema changes
        const preserved = oldSettings && {
            multiTypes: oldSettings.multiTypes,
            legend: oldSettings.legend && {left: oldSettings.legend.left, top: oldSettings.legend.top}
        };
        return {...preserved, ...newSettings};
    }
}

function getD3FCStyles() {
    const headerStyles = document.querySelector("head").querySelectorAll("style");
    const d3fcStyles = [];
    headerStyles.forEach(s => {
        if (s.innerText.indexOf("d3fc-") !== -1) {
            d3fcStyles.push(s.innerText);
        }
    });
    return d3fcStyles.join("");
}
