#!/bin/bash
# single
#git submodule update --remote site-lisp/sort-tab
# multiply submodules
git submodule foreach 'git pull --rebase origin master'

