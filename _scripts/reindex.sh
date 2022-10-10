#!/usr/bin/bash

# Generate index.
python mkindex.py .. > ../grammars.json

# Compute diff. Check in if there is new content.
git diff --quiet
if [ "$?" == "1" ]; then
  git config --local user.email "41898282+github-actions[bot]@users.noreply.github.com"
  git config --local user.name "github-actions[bot]"
  git add grammars.json
  git commit -m "generated"
fi
