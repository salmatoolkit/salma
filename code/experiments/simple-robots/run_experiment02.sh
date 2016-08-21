#!/usr/bin/env bash
echo "$DYLD_LIBRARY_PATH"
export PYTHONPATH=./src:../../SALMA
python3 src/simplerobots/experiment02.py $@