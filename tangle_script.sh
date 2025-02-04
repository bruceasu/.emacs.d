#!/bin/bash

#../../emacs-29.2/bin/
emacs -Q --batch \
  --eval "(require 'ob-tangle)" \
  --eval "(setq org-confirm-babel-evaluate nil)" \
  --eval "(org-babel-tangle-file \"$1\")"
