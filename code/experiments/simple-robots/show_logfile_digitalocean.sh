#!/bin/bash
docker-machine ssh $1 "find /experiment_results -name experiment.log | xargs -I % cat %"