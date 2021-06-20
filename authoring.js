#!/usr/bin/env node

const path = require('path');
const { readdir, readFile, writeFile }  = require('fs').promises;
const argv = require('@jcubic/lily')(process.argv.slice(2));

const { slugify, get } = require('.');
const dir = './recipes';

const cache = {};

function get_user_url(username) {
    if (cache[username]) {
        return Promise.resolve(cache[username]);
    }
    const query = {};
    if (argv.t) {
        query['access_token'] = argv.t;
    }
    return get(`https://api.github.com/users/${username}`, query).then(user => {
        const info = {
            username: username,
            url: user.blog || user.html_url,
            name: user.name || username
        };
        cache[username] = info;
        return info;
    });
}

readdir(dir).then(files => {
    files = files.filter(file => file.endsWith('.md'));
    files.forEach((filename) => {
        const fullname = path.join(dir, filename);
        readFile(fullname).then(async file => {
            file = file.toString();
            const re = /@([^\s]+)/g;
            let usernames = file.match(re);
            if (usernames) {
                usernames = usernames.map(name => name.substring(1));
                const users = await Promise.all(usernames.map(get_user_url));
                users.forEach(user => {
                    console.log(user);
                    file = file.replace('@' + user.username, function(_) {
                        return `[${user.name}](${user.url})`;
                    });
                });
                writeFile(fullname, file);
            }
        });
    });
});
