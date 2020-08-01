var path = require("path");

var babelOptions = {
    presets: [
        ["@babel/preset-env", {
            "targets": {
                "browsers": "last 2 Chrome versions, last 2 Firefox versions, last 2 Safari versions, last 2 iOS versions, last 2 Edge versions, last 2 Android versions"
                },
            "modules": false
        }]
    ],
    plugins: ["@babel/plugin-transform-runtime"]
};

module.exports = (env, argv) => {
    const mode = argv.mode || "none";
    console.log("Building application in " + mode + " mode");
    return {
        devtool: "source-map",
        mode: mode,
        entry: "./src/App.fsproj",
        devServer: {
            contentBase: path.join(__dirname, "./dist")
        },
        module: {
            rules: [{
                test: /\.fs(x|proj)?$/,
                loader: "fable-loader",
                options: { babel: babelOptions }
            }]
        },
        // optimization: {
        //     minimize: false
        // },
        resolve: {
            alias: {
                "react": "preact/compat",
                "react-dom": "preact/compat"
            }
        }
    }
}