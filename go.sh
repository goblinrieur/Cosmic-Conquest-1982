#! /usr/bin/env bash
# -*- coding: UTF8 -*-
fsfile=cosmic_conquest_modern.fs
runner=$(which gforth-fast)
[ $(grep gforth-fast <<< ${runner}) ] && $runner $fsfile "$@" || ( echo "gforth not found" && exit 1 )
exit 0

