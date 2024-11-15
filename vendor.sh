#!/bin/bash

if [ -e _spec ]
then
    git -C _spec fetch --all
else
    git clone https://github.com/gesel-inc/gesel-spec _spec
fi
git -C _spec checkout master

rm -rf src/gesel
cp -r _spec/include/gesel src
