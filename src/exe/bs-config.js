
/*
 |-------------------------
 | Browser-sync config file
 |-------------------------
 |
 | For up-to-date information about the options:
 |   http://www.browsersync.io/docs/options/
 |
 */
module.exports = {
    "ui": {
        "port": 3001,
        "weinre": {
            "port": 8080
        }
    },
    "files": [
	"style.css",
        "index.html",
	"index-dev.html",
        "*.elm",
	"*/*.elm"
    ],
    "server": false,
    "proxy": "localhost:8000",
    "online": false,
    "open": false,
    "reloadOnRestart": true,
    "notify": false,
    "injectChanges": false,
};
