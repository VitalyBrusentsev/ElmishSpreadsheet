# Fable and Sass for ElmishSpreadsheet

## macOS install starts with [Homebrew](https://brew.sh)

### Install [node.js](https://nodejs.org)
The latest version of Node won’t work.
Version 14.15.0 will work.
```
# https://stackoverflow.com/a/67594618/1390116
brew uninstall node
brew install n
sudo ln -s /usr/local/Cellar/n/7.3.0 /usr/local/n
sudo chown -R $(whoami) /usr/local/n
n 14.15.0
```

### Install [Sass](https://sass-lang.com)
```
# https://stackoverflow.com/a/66298014/1390116
npm uninstall node-sass
npm i sass
```
