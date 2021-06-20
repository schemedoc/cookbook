#!/usr/bin/env node

var path = require('path');
var fs = require('fs');
var argv = require('@jcubic/lily')(process.argv.slice(2));

const { slugify, get } = require('.');


function get_api(argv) {
    const user = argv.u;
    const repo = argv.r;
    const base_path = '/repos/' + user + '/' + repo + '/issues';
    const query = {
        "per_page": 100
    };
    if (argv.t) {
        query['access_token'] = argv.t;
    }
    const re = /\[Recipe\]/;
    return get('https://api.github.com' + base_path, query).then(function(issues) {
        issues.forEach(function(issue) {
            const { title, url } = issue;
            if (title.match(re)) {
                const filename = './recipes/' + slugify(title.replace(re, '')) + '.md';
                get(url).then(function(issue) {
                    const { body } = issue;
                    fs.writeFileSync(filename, body.replace(/\r/g, ''));
                });
            }
        });
    });
}

if (argv.u && argv.r) {
     get_api(argv);
} else {
    var script = path.basename(process.argv[1]);
    console.log('usage: \n' + script + '-u <user> -r <repo> ' +
                '[-t]');
}


