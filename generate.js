#!/usr/bin/env node

var https = require('https');
var path = require('path');
var fs = require('fs');
var argv = require('@jcubic/lily')(process.argv.slice(2));
var request = require('request');

function get(url, query) {
    var options = {
        url: url,
        qs: query,
        headers: {
            'accept': 'application/vnd.github.VERSION.full+json',
            'User-Agent': 'Node.js'
        }
    };
    if (argv.auth) {
        var [user,pass] = argv.auth.split(':');
        options.auth = {
            user,
            pass
        };
    }
    return new Promise(function(resolve, reject) {
        request(options, function(error, res, body) {
            if (res.statusCode == 200) {
                resolve(JSON.parse(body));
            } else if (+res.headers['x-ratelimit-remaining'] == 0) {
                var date = new Date(+res.headers['x-ratelimit-reset']*1000);
                reject('Rate limit util ' + date);
            } else {
                reject('Error code ' + res.statusCode);
            }
        });
    });
}

// ref: https://gist.github.com/codeguy/6684588
function slugify(text) {
    return text
      .toString()
      .toLowerCase()
      .normalize('NFD')
      .trim()
      .replace(/\s+/g, '-')
      .replace(/[^\w\-]+/g, '')
      .replace(/\-\-+/g, '-');
}

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
                    fs.writeFileSync(filename, body);
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


