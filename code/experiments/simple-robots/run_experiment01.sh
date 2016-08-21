#!/usr/bin/env bash
PYTHONPATH=$PYTHONPATH:./src:$SALMA_HOME
export PYTHONPATH
python3 src/simplerobots/experiment01.py $@