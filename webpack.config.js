var path = require("path");

module.exports = {
    mode: "development",
    entry: "./src/Main.fs.js",
    output: {
        path: path.join(__dirname, "./public"),
        filename: "bundle.js",
    },
    devServer: {
        contentBase: "./public",
        port: 8080,
    }
}