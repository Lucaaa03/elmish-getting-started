"use strict";

const path = require("node:path")

module.exports = {
    mode: "development",
    entry: "./build/App.js",

    output: {
        path: path.join(__dirname, "./dist"),
        filename: "main.js",
    },

    devServer: {
        static: path.join(__dirname, "./dist"),
        open: true,
    },
};
