#!/usr/bin/env bash
set -e
ORG_FILE=whitepaper.org
HTML_FILE=whitepaper.html
PDF_FILE=runtime-whitepaper.pdf
emacs --batch "$ORG_FILE" \
  --eval "(require 'ox-html)" \
  --eval "(org-html-export-to-html)"
weasyprint "$HTML_FILE" "$PDF_FILE"
echo "Generated $PDF_FILE"

# chmod +x build-whitepaper.sh
