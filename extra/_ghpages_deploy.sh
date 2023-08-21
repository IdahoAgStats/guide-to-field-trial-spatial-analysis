#!/usr/bin/env bash

git checkout gh-pages
git pull

# instructions for overwriting gh-pages with master branch and prepping it for gitbook deployment
# this overwrites some previous gh-pages commits, and that is okay
git checkout master 
git merge -s ours gh-pages -m 'update book' # Merge branches, but use our (=master) branch head
        # need merge message. to leave vim: esc, :w, :q!


# need to make docs folder for github to read for gitbook deployment
cp -r _book docs
git add . 
git commit -m '...'
git push --all origin
# remember to return to main branch!
git checkout master

