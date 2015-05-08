#!/bin/sh
DIRNAME=`dirname "$0"`
cd $DIRNAME

git fetch --all
git reset --hard origin/master

if [ -d treedata ] ; then
    cd treedata git fetch --all
    git reset --hard origin/master
else
    git clone https://github.com/nverno/treedata
fi

