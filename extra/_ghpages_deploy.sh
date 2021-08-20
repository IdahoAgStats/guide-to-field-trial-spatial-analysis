#!/usr/bin/env bash

# instructions for overwriting gh-pages with master branch and prepping it for gitbook deployment
# this overwrites some previous gh-pages commits, and that is okay
git checkout master 
git merge -s ours gh-pages # Merge branches, but use our (=master) branch head
git checkout gh-pages
git merge master
# need to make docs folder for github to read for gitbook deployment
cp _book docs
# remember to return to main branch!
git checkout master 
