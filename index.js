var fetch = require('node-fetch');

exports.slugify = slugify;
exports.get = get;

function get(url, { query, auth = null } = {}) {
    var headers = {
        'accept': 'application/vnd.github.v3+json',
        'User-Agent': 'Node.js'
    };
    if (auth) {
        headers['Authorization'] = auth;
    }
    if (query) {
        var params = new URLSearchParams(query);
        url = url + '?' + params.toString();
    }
    return fetch(url, { headers: headers }).then(function(res) {
        if (res.status == 200) {
            return res.json();
        } else if (+res.headers.get('x-ratelimit-remaining') == 0) {
            var date = new Date(+res.headers.get('x-ratelimit-reset') * 1000);
            return Promise.reject('Rate limit util ' + date);
        } else {
            return Promise.reject('Error code ' + res.status);
        }
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
