var path = require("path");

module.exports = (env, argv) => {
    const mode = argv.mode || "none";
    console.log("Building application in " + mode + " mode");
    return {
        mode: mode,
        entry: "./src/App.fsproj",
        devServer: {
            contentBase: path.join(__dirname, "./dist")
        },
        module: {
            rules: [{
                test: /\.fs(x|proj)?$/,
                use: "fable-loader"
            }]
        }
    }
}