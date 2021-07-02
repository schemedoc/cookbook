#!/bin/sh
set -eu
cd "$(dirname "$0")"
curl --location --fail --silent --show-error -o www/style.css \
    https://www.staging.scheme.org/style.css
rsync -vr www/ alpha.servers.scheme.org:/production/cookbook/www/
