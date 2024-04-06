#!/bin/sh
set -eu
cd "$(dirname "$0")"
curl --location --fail --silent --show-error -o www/schemeorg.css \
    https://www.scheme.org/schemeorg.css
rsync -crv www/ tuonela.scheme.org:/production/cookbook/www/
