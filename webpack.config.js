const path = require("path");

module.exports = {
  module: {
    rules: [
      {
        test: /\.html$/,
        exclude: /node_modules/,
        loader: "file-loader?name=[name].[ext]"
      },
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        loader: "elm-webpack-loader",
        options: {
          debug: true
        }
      },
      {
        test: /\.css$/,
        loader: ['style-loader', 'css-loader']
      }
    ]
  },

  devServer: {
    inline: true,
    overlay: true,
    compress: true,
    port: 3002,
    historyApiFallback: true,
    contentBase: path.join(__dirname, "public"),
    watchContentBase: true,
    publicPath: "/",
    watchOptions: {
      ignored: /node_modules/
    }
  }
};
