#!/usr/bin/bash

# Generate index.
pwd
ls -l mkindex.py
echo "creating ../grammars.json"
python3 mkindex.py .. > ../grammars.json
echo "finished creating ../grammars.json"
ls -l ../grammars.json
exit 0
