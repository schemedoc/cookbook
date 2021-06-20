var request = require('request');

exports.slugify = slugify;
exports.get = get;

function get(url, { query, auth = null } = {}) {
    var options = {
        url: url,
        qs: query,
        headers: {
            'accept': 'application/vnd.github.v3+json',
            'User-Agent': 'Node.js'
        }
    };
    if (auth) {
        options.headers['Authorization'] = auth;
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
