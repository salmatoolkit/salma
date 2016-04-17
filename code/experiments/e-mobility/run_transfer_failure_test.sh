#!/bin/bash
export PYTHONPATH=$PYTHONPATH:../../SALMA
export DYLD_LIBRARY_PATH=/opt/eclipseclp/lib/x86_64_macosx
python3.5 src/run_transfer_failure_test.py $1 $2
