#!/usr/bin/env bash

git checkout master
git merge -s ours gh-pages # Merge branches, but use our (=master) branch head
git checkout gh-pages
git merge master

cp _book docs
