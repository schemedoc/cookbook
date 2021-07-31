#!/bin/sh
set -eu
cd "$(dirname "$0")"
exec csi -R r7rs -I . -script www.scm
