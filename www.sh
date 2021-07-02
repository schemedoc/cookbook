#!/bin/sh
set -eu
cd "$(dirname "$0")"
exec csi -R r7rs -script www.scm
