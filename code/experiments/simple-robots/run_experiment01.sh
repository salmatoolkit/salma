#!/usr/bin/env bash
echo "$DYLD_LIBRARY_PATH"
PYTHONPATH=./src:../../SALMA
python3 src/simplerobots/experiment01.py $@