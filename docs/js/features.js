exports.EXAMPLES = [
    {
        name: "Default",
        config: {}
    },
    {
        name: "Row Pivots 1",
        config: {
            "row-pivots": ["Sub-Category"]
        },
        aggregates: "dominant"
    },
    {
        name: "Row Pivots 2",
        config: {
            "row-pivots": ["Category", "Sub-Category"]
        }
    },
    {
        name: "Column Pivots",
        config: {
            "column-pivots": ["Category"],
            columns: ["Sales", "Quantity", "Discount", "Profit"]
        }
    },
    {
        name: "Column Pivots 2",
        config: {
            "column-pivots": ["Category", "Sub-Category"],
            columns: ["Sales", "Quantity", "Discount", "Profit"]
        }
    },
    {
        name: "Both",
        config: {
            "row-pivots": ["Region"],
            "column-pivots": ["Category"],
            columns: ["Sales", "Quantity", "Discount", "Profit"]
        }
    },
    {
        name: "Both 2",
        config: {
            "row-pivots": ["Region", "State"],
            "column-pivots": ["Category", "Sub-Category"],
            columns: ["Sales", "Quantity", "Discount", "Profit"]
        }
    },
    {
        name: "Background Row And Column Pivots",
        config: {
            "row-pivots": ["State"],
            "column-pivots": ["Sub-Category"],
            columns: ["Profit"],
            sort: [["Profit", "col asc"]],
            plugin_config: {
                Profit: {
                    color_mode: "background"
                    // gradient: 600,
                }
            }
        }
    },
    {
        name: "Gradient Row And Column Pivots",
        config: {
            "row-pivots": ["State"],
            "column-pivots": ["Sub-Category"],
            columns: ["Profit"],
            sort: [["Profit", "col asc"]],
            plugin_config: {
                Profit: {
                    color_mode: "gradient",
                    gradient: 1600
                }
            }
        }
    },
    {
        name: "BackgBarround Row And Column Pivots",
        config: {
            "row-pivots": ["State"],
            "column-pivots": ["Sub-Category"],
            columns: ["Sales"],
            aggregates: {Sales: "avg"},
            plugin_config: {
                Sales: {
                    color_mode: "bar",
                    gradient: 600
                }
            }
        }
    },
    {
        name: "Foreground Colors",
        config: {
            columns: ["Category", "Sales", "Discount", "Profit", "Sub-Category", "Order Date"],
            plugin_config: {
                Profit: {
                    pos_color: "#32cd82",
                    neg_color: "#f50fed"
                },
                Sales: {
                    pos_color: "#780aff",
                    neg_color: "#f5ac0f"
                },
                Discount: {
                    pos_color: "#f5ac0f",
                    neg_color: "#780aff"
                }
            },
            sort: [["Sub-Category", "desc"]]
        }
    },
    {
        name: "Background Colors",
        config: {
            columns: ["Category", "Sales", "Discount", "Profit", "Sub-Category", "Order Date"],
            plugin_config: {
                Profit: {
                    color_mode: "background",
                    pos_color: "#32cd82",
                    neg_color: "#f50fed"
                },
                Sales: {
                    color_mode: "background",
                    pos_color: "#780aff",
                    neg_color: "#f5ac0f"
                },
                Discount: {
                    color_mode: "background",
                    pos_color: "#f5ac0f",
                    neg_color: "#780aff"
                }
            },
            sort: [["Sub-Category", "desc"]]
        }
    },

    {
        name: "Gradient Colors",
        config: {
            columns: ["Category", "Sales", "Discount", "Profit", "Sub-Category", "Order Date"],
            plugin_config: {
                Profit: {
                    color_mode: "gradient",
                    gradient: 600,
                    pos_color: "#32cd82",
                    neg_color: "#f50fed"
                },
                Sales: {
                    color_mode: "gradient",
                    gradient: 2268,
                    pos_color: "#780aff",
                    neg_color: "#f5ac0f"
                },
                Discount: {
                    color_mode: "gradient",
                    gradient: 0.8,
                    pos_color: "#f5ac0f",
                    neg_color: "#780aff"
                }
            },
            sort: [["Sub-Category", "desc"]]
        }
    },
    {
        name: "Bar Colors",
        config: {
            columns: ["Category", "Sales", "Discount", "Profit", "Sub-Category", "Order Date"],
            plugin_config: {
                Profit: {
                    color_mode: "bar",
                    gradient: 600,
                    pos_color: "#32cd82",
                    neg_color: "#f50fed"
                },
                Sales: {
                    color_mode: "bar",
                    gradient: 2268,
                    pos_color: "#780aff",
                    neg_color: "#f5ac0f"
                },
                Discount: {
                    color_mode: "bar",
                    gradient: 0.8,
                    pos_color: "#f5ac0f",
                    neg_color: "#780aff"
                }
            },
            sort: [["Sub-Category", "desc"]]
        }
    },
    {
        name: "Thermometer",
        config: {
            "row-pivots": ["State"],
            columns: ["Profit (-)", "Profit", "Profit (+)"],
            plugin_config: {
                "Profit (-)": {
                    color_mode: "bar",
                    gradient: 10000
                },
                Profit: {
                    color_mode: "gradient",
                    gradient: 10000
                },
                "Profit (+)": {
                    color_mode: "bar",
                    gradient: 10000
                }
            },
            expressions: [`//Profit (-)\nif("Profit"<0){"Profit"}else{0}`, `//Profit (+)\nif("Profit">0){"Profit"}else{0}`]
        }
    },

    // Y Bar

    {
        name: "Y Bar",
        config: {
            plugin: "Y Bar",
            "row-pivots": ["Sub-Category"],
            columns: ["Sales"]
        }
    },
    {
        name: "Y Bar, Sorted Desc By Y-Axis",
        config: {
            plugin: "Y Bar",
            "row-pivots": ["Sub-Category"],
            columns: ["Sales"],
            sort: [["Sales", "desc"]]
        }
    },
    {
        name: "Y Bar - Row And Column Pivots",
        config: {
            plugin: "Y Bar",
            "row-pivots": ["Sub-Category"],
            "column-pivots": ["Ship Mode"],
            columns: ["Sales"]
        }
    },

    {
        name: "Y Bar - Row Pivots 2 Sorted",
        config: {
            plugin: "Y Bar",
            "row-pivots": ["Category", "Sub-Category"],
            columns: ["Sales"],
            sort: [["Sales", "desc"]]
        }
    },
    {
        name: "Y Bar - Row Pivots 2 And Column Pivots Sorted",
        config: {
            plugin: "Y Bar",
            "row-pivots": ["Category", "Sub-Category"],
            "column-pivots": ["Category"],
            columns: ["Sales"],
            sort: [["Sales", "desc"]]
        }
    },
    {
        name: "Y Bar - Row Pivots 2 And 2 Column Pivots Sorted",
        config: {
            plugin: "Y Bar",
            "row-pivots": ["Category", "Sub-Category"],
            "column-pivots": ["Category", "Region"],
            columns: ["Sales"],
            sort: [["Sales", "desc"]]
        }
    },
    {
        name: "Y Bar - Row Pivots 2 And 2 Column Pivots Sorted 2",
        config: {
            plugin: "Y Bar",
            "row-pivots": ["State"],
            "column-pivots": ["Profit (-/+)"],
            columns: ["Profit"],
            expressions: [`//Profit (-/+)\nif("Profit"<0){1}else{0}`],
            sort: [["Profit", "desc"]]
        },
        viewport: {width: 600, height: 450}
    },

    // Y Bar Multi Axis

    {
        name: "Y Bar Multi Axis",
        config: {
            plugin: "Y Bar",
            "row-pivots": ["Sub-Category"],
            columns: ["Quantity", "Sales"],
            aggregates: {Sales: "avg"},
            sort: [["Sales", "desc"]]
        }
    },
    {
        name: "Y Bar Multi Axis - SPlit",
        config: {
            plugin: "Y Bar",
            "row-pivots": ["Sub-Category"],
            columns: ["Quantity", "Sales"],
            sort: [["Sales", "desc"]],
            aggregates: {Sales: "avg"},
            plugin_config: {splitMainValues: ["Sales"]}
        }
    },

    // X Bar

    {
        name: "X Bar",
        config: {
            plugin: "X Bar",
            "row-pivots": ["Sub-Category"],
            columns: ["Sales"]
        }
    },
    {
        name: "X Bar",
        config: {
            plugin: "X Bar",
            "row-pivots": ["Sub-Category"],
            columns: ["Quantity", "Profit"]
        }
    },
    {
        name: "X Bar, Sorted Desc By X-Axis",
        config: {
            plugin: "X Bar",
            "row-pivots": ["Sub-Category"],
            columns: ["Sales"],
            sort: [["Sales", "asc"]]
        }
    },
    {
        name: "X Bar - Row And Column Pivots",
        config: {
            plugin: "X Bar",
            "row-pivots": ["Sub-Category"],
            "column-pivots": ["Region"],
            columns: ["Sales"]
        }
    },
    {
        name: "X Bar - Row And Column Pivots",
        config: {
            plugin: "X Bar",
            "row-pivots": ["Sub-Category"],
            "column-pivots": ["Region"],
            columns: ["Sales"],
            sort: [["Sales", "asc"]]
        }
    },

    // Y Line

    {
        name: "Y Line - Datetime Axis",
        config: {
            plugin: "Y Line",
            "row-pivots": ["Order Date"],
            columns: ["Sales"]
        }
    },
    {
        name: "Y Line - Datetime Axis",
        config: {
            plugin: "Y Line",
            "row-pivots": ["Order Date"],
            "column-pivots": ["Segment"],
            columns: ["Sales"]
        }
    },
    {
        name: "Y Line - Datetime Axis Computed",
        config: {
            plugin: "Y Line",
            "row-pivots": ["bucket(\"Order Date\", 'M')"],
            expressions: ["bucket(\"Order Date\", 'M')"],
            columns: ["Sales"]
        }
    },
    {
        name: "Y Line - Datetime Axis",
        config: {
            plugin: "Y Line",
            "row-pivots": ["bucket(\"Order Date\", 'M')"],
            "column-pivots": ["bucket(\"Order Date\", 'Y')"],
            expressions: ["bucket(\"Order Date\", 'M')", "bucket(\"Order Date\", 'Y')"],
            columns: ["Sales"]
        }
    },
    {
        name: "Y Line - Datetime Axis And Column Pivots",
        config: {
            plugin: "Y Line",
            "row-pivots": ["bucket(\"Order Date\", 'M')"],
            "column-pivots": ["Region"],
            expressions: ["bucket(\"Order Date\", 'M')"],
            columns: ["Sales"]
        }
    },
    {
        name: "Y Line - Category Axis",
        config: {
            plugin: "Y Line",
            "row-pivots": ["State"],
            columns: ["Sales"]
        },
        viewport: {width: 600, height: 450}
    },
    {
        name: "Y Line - Category Axis",
        config: {
            plugin: "Y Line",
            "row-pivots": ["State"],
            columns: ["Sales"],
            sort: [["Sales", "desc"]]
        },
        viewport: {width: 600, height: 450}
    },
    {
        name: "Y Line - Row and Column Pivots",
        config: {
            plugin: "Y Line",
            "row-pivots": ["State"],
            "column-pivots": ["Segment"],
            columns: ["Sales"],
            sort: [["Sales", "desc"]]
        },
        viewport: {width: 600, height: 450}
    },
    {
        name: "Y Line - Multi Axis Split",
        config: {
            plugin: "Y Line",
            "row-pivots": ["State"],
            columns: ["Sales", "Profit"],
            plugin_config: {splitMainValues: ["Sales"]},
            sort: [["Sales", "desc"]]
        },
        viewport: {width: 600, height: 450}
    },
    {
        name: "Y Line - Multi Axis Split",
        config: {
            plugin: "Y Line",
            "row-pivots": ["State"],
            "column-pivots": ["Segment"],
            columns: ["Sales", "Profit"],
            plugin_config: {splitMainValues: ["Sales"]},
            sort: [["Sales", "desc"]]
        },
        viewport: {width: 600, height: 450}
    },

    // Y Area

    {
        name: "Y Area - Datetime Axis Computed",
        config: {
            plugin: "Y Area",
            "row-pivots": ["bucket(\"Order Date\", 'M')"],
            expressions: ["bucket(\"Order Date\", 'M')"],
            columns: ["Sales"]
        }
    },
    {
        name: "Y Area - Datetime Axis",
        config: {
            plugin: "Y Area",
            "row-pivots": ["bucket(\"Order Date\", 'M')"],
            "column-pivots": ["bucket(\"Order Date\", 'Y')"],
            expressions: ["bucket(\"Order Date\", 'M')", "bucket(\"Order Date\", 'Y')"],
            columns: ["Sales"]
        }
    },
    {
        name: "Y Area - Datetime Axis And Column Pivots",
        config: {
            plugin: "Y Area",
            "row-pivots": ["bucket(\"Order Date\", 'M')"],
            "column-pivots": ["Region"],
            expressions: ["bucket(\"Order Date\", 'M')"],
            columns: ["Sales"]
        }
    },
    {
        name: "Y Area - Category Axis",
        config: {
            plugin: "Y Area",
            "row-pivots": ["State"],
            columns: ["Sales"]
        },
        viewport: {width: 600, height: 450}
    },
    {
        name: "Y Area - Row and Column Pivots",
        config: {
            plugin: "Y Area",
            "row-pivots": ["State"],
            "column-pivots": ["Ship Mode"],
            columns: ["Sales"]
        },
        viewport: {width: 600, height: 450}
    },
    {
        name: "Y Area - 2 Row Pivots",
        config: {
            plugin: "Y Area",
            "row-pivots": ["Region", "State"],
            columns: ["Sales"],
            sort: [["Sales", "desc"]]
        },
        viewport: {width: 600, height: 450}
    },
    {
        name: "Y Area - Row and Column Pivots",
        config: {
            plugin: "Y Area",
            "row-pivots": ["Region", "State"],
            "column-pivots": ["Region"],
            columns: ["Sales"],
            sort: [["Sales", "desc"]]
        },
        viewport: {width: 600, height: 450}
    },
    {
        name: "Y Area - Row and Column Pivots 2",
        config: {
            plugin: "Y Area",
            "row-pivots": ["Region", "State"],
            "column-pivots": ["Ship Mode"],
            columns: ["Sales"],
            sort: [["Sales", "desc"]]
        },
        viewport: {width: 600, height: 450}
    },

    // X/Y Scatter

    {
        name: "X/Y Scatter",
        config: {
            plugin: "X/Y Scatter",
            "row-pivots": ["City"],
            columns: ["Sales", "Quantity"],
            aggregates: {Sales: "avg", Profit: "avg", Quantity: "avg"}
        }
    },
    {
        name: "X/Y Scatter - Column Pivots",
        config: {
            plugin: "X/Y Scatter",
            "row-pivots": ["City"],
            "column-pivots": ["Region"],
            columns: ["Sales", "Quantity"],
            aggregates: {Sales: "avg", Profit: "avg", Quantity: "avg"}
        }
    },
    {
        name: "X/Y Scatter - Color By Float",
        config: {
            plugin: "X/Y Scatter",
            "row-pivots": ["State"],
            columns: ["Sales", "Quantity", "Profit"],
            aggregates: {Sales: "avg", Profit: "avg", Quantity: "avg"},
            sort: [["Profit", "desc"]]
        }
    },
    {
        name: "X/Y Scatter - Bubble",
        config: {
            plugin: "X/Y Scatter",
            "row-pivots": ["Sub-Category"],
            columns: ["Sales", "Quantity", null, "Profit"],
            aggregates: {Sales: "avg", Profit: "avg", Quantity: "avg"}
        },
        viewport: {width: 600, height: 450}
    },
    {
        name: "X/Y Scatter - Bubble",
        config: {
            plugin: "X/Y Scatter",
            "row-pivots": ["Sub-Category"],
            "column-pivots": ["Category"],
            columns: ["Sales", "Quantity", null, "Profit"],
            aggregates: {Sales: "avg", Profit: "avg", Quantity: "avg"}
        },
        viewport: {width: 600, height: 450}
    },
    {
        name: "X/Y Scatter - Bubble",
        config: {
            plugin: "X/Y Scatter",
            "row-pivots": ["Sub-Category"],
            columns: ["Sales", "Quantity", "Profit", "Row ID"],
            sort: [["Profit", "desc"]],
            aggregates: {Sales: "avg", Profit: "sum", Quantity: "avg", "Row ID": "avg"}
        },
        viewport: {width: 600, height: 450}
    },

    {
        name: "X/Y Scatter - Category Y Axis",
        config: {
            plugin: "X/Y Scatter",
            columns: ["Profit", "State", null, "Quantity"],
            "row-pivots": ["City"],
            aggregates: {
                State: "dominant"
            }
        },
        viewport: {width: 600, height: 450}
    },

    {
        name: "X/Y Scatter - Category Y Axis And Size And Color",
        config: {
            plugin: "X/Y Scatter",
            columns: ["State", "Sub-Category", "Quantity", "Sales", null],
            "row-pivots": ["State", "Sub-Category"],
            aggregates: {
                State: "dominant",
                "Sub-Category": "dominant",
                Profit: "low"
            }
        },
        viewport: {width: 600, height: 450}
    },

    // X/Y Line

    {
        name: "X/Y Line",
        config: {
            columns: ["Sales", "Profit"],
            plugin: "X/Y Line",
            aggregates: {
                "Order Date": "dominant",
                Sales: "avg",
                Profit: "avg"
            },
            "row-pivots": ["State"],
            sort: [["Profit"], ["Sales"]],
            "column-pivots": ["Region"]
        }
    },

    // Treemap

    {
        name: "Treemap",
        config: {
            plugin: "Treemap",
            "row-pivots": ["Sub-Category"],
            columns: ["Sales"]
        }
    },
    {
        name: "Treemap - 2 Row Pivots",
        config: {
            plugin: "Treemap",
            "row-pivots": ["Category", "Sub-Category", "Segment"],
            columns: ["Sales"]
        }
    },
    {
        name: "Treemap - Float Color",
        config: {
            plugin: "Treemap",
            "row-pivots": ["Category", "Sub-Category"],
            columns: ["Sales", "Quantity"],
            sort: [["Quantity", "desc"]]
        }
    },
    {
        name: "Treemap - Category Color",
        config: {
            plugin: "Treemap",
            "row-pivots": ["Category", "Sub-Category"],
            columns: ["Sales", "Category"],
            aggregates: {Category: "dominant"}
        }
    },
    {
        name: "Treemap - Row And Column Pivots Float Color",
        config: {
            plugin: "Treemap",
            "row-pivots": ["Category", "Sub-Category"],
            "column-pivots": ["Region"],
            columns: ["Sales", "Quantity"],
            sort: [["Quantity", "desc"]]
        },
        viewport: {width: 800, height: 600}
    },
    {
        name: "Treemap - Row And Column Pivots Category Color",
        config: {
            plugin: "Treemap",
            "row-pivots": ["Category", "Sub-Category"],
            "column-pivots": ["Region"],
            columns: ["Sales", "Region"],
            aggregates: {Region: "dominant"}
        },
        viewport: {width: 800, height: 600}
    },
    {
        name: "Treemap - Row And Column Pivots Category Color 2",
        config: {
            plugin: "Treemap",
            "row-pivots": ["Category", "Sub-Category"],
            "column-pivots": ["Region"],
            columns: ["Sales", "Category"],
            aggregates: {Category: "dominant"}
        },
        viewport: {width: 800, height: 600}
    },
    // Sunburst

    {
        name: "Sunburst",
        config: {
            plugin: "Sunburst",
            "row-pivots": ["Sub-Category"],
            columns: ["Sales"]
        }
    },
    {
        name: "Sunburst - 2 Row Pivots",
        config: {
            plugin: "Sunburst",
            "row-pivots": ["Category", "Sub-Category", "Segment"],
            columns: ["Sales"]
        }
    },
    {
        name: "Sunburst - Float Color",
        config: {
            plugin: "Sunburst",
            "row-pivots": ["Category", "Sub-Category"],
            columns: ["Quantity", "Sales"],
            sort: [["Quantity", "desc"]]
        }
    },
    {
        name: "Sunburst - Category Color",
        config: {
            plugin: "Sunburst",
            "row-pivots": ["Category", "Sub-Category"],
            columns: ["Sales", "Category"],
            aggregates: {Category: "dominant"}
        }
    },
    {
        name: "Sunburst - Row And Column Pivots",
        config: {
            plugin: "Sunburst",
            "row-pivots": ["Category", "Sub-Category"],
            "column-pivots": ["Region"],
            columns: ["Sales"]
        },
        viewport: {width: 600, height: 450}
    },
    {
        name: "Sunburst - Row And Column Pivots Float Color",
        config: {
            plugin: "Sunburst",
            "row-pivots": ["Category", "Sub-Category"],
            "column-pivots": ["Region"],
            columns: ["Sales", "Quantity"]
        },
        viewport: {width: 600, height: 450}
    },
    {
        name: "Sunburst - Row And Column Pivots Category Color",
        config: {
            plugin: "Sunburst",
            "row-pivots": ["Category", "Sub-Category"],
            "column-pivots": ["Region"],
            columns: ["Sales", "Region"],
            aggregates: {Region: "dominant"}
        },
        viewport: {width: 600, height: 450}
    },
    {
        name: "Sunburst - Row And Column Pivots Category Color 2",
        config: {
            plugin: "Sunburst",
            "row-pivots": ["Category", "Sub-Category"],
            "column-pivots": ["Region"],
            columns: ["Sales", "Category"],
            aggregates: {Category: "dominant"}
        },
        viewport: {width: 600, height: 450}
    },

    // Heatmap

    {
        name: "Heatmap",
        config: {
            plugin: "Heatmap",
            "row-pivots": ["Sub-Category"],
            "column-pivots": ["Region"],
            columns: ["Profit"],
            sort: [
                ["Profit", "desc"],
                ["Profit", "col desc"]
            ]
            // aggregates: {Category: "dominant"}
        }
        // viewport: {width: 600, height: 200}
    },
    {
        name: "Heatmap 2",
        config: {
            plugin: "Heatmap",
            "row-pivots": ["State"],
            "column-pivots": ["Sub-Category"],
            columns: ["Profit"],
            sort: [
                ["Profit", "desc"],
                ["Profit", "col desc"]
            ]
            // aggregates: {Profit: "low"}
        },
        viewport: {width: 600, height: 450}
    },
    {
        name: "Heatmap 3",
        config: {
            columns: ["Discount"],
            plugin: "Heatmap",
            expressions: ["bucket(\"Order Date\", 'M')"],
            aggregates: {
                "Order Date": "dominant",
                Sales: "avg"
            },
            "column-pivots": ["Sub-Category"],
            "row-pivots": ["bucket(\"Order Date\", 'M')"],
            sort: [["Discount", "col asc"]]
        },
        viewport: {width: 600, height: 450}
    },
    {
        name: "Heatmap 4",
        config: {
            plugin: "Heatmap",
            columns: ["Profit"],
            expressions: ['bucket("Profit", 100)', "bucket(\"Order Date\", 'M')"],
            "row-pivots": ["bucket(\"Order Date\", 'M')"],
            "column-pivots": ['bucket("Profit", 100)'],
            plugin_config: {}
        },
        viewport: {width: 600, height: 450}
    }
];
