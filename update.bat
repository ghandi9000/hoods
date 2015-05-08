rem Update repo
@ECHO off

git fetch --all
git reset --hard origin/master
cd treedata
git fetch --all
git reset --hard origin/master
cd ..
