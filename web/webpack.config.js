const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');

module.exports = {
  entry: './src/index.js', 
  mode: 'development',
  output: {
	  filename: 'app.bundle.js',
  	path: path.resolve(__dirname, 'dist'),
  },
  module: {
    rules: [
      {
        test: /\.css$/,
        use: [ 'style-loader', 'css-loader' ],
        exclude: /node_modules/
      },
      {
        test: /\.(png|jpeg|jpg)$/,
        use: [ 'file-loader' ],
        exclude: /node_modules/
      }
    ]
  },
  plugins: [
    new HtmlWebpackPlugin({
      template: './src/index.html'
    })
  ]
};
