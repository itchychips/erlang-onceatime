#!/bin/sh

if [ ! -f assets/.git ]; then
    git submodule init assets &&
    git submodule update assets
fi
