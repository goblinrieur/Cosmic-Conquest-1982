#! /usr/bin/env bash
# -*- coding: UTF8 -*-
fsfile=cosmic_conquest_modern.fs
runner=$(which gforth-fast)
[ $(grep gforth-fast <<< ${runner}) ] && $runner $fsfile "$@" 
exit 0
