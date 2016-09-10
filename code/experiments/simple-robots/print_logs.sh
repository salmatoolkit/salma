#!/bin/bash
for (( i=1 ; $i <= 10 ; i=$i + 1 )) 
do
	echo "SIM$i:"
	docker-machine ssh sim$i \
		"find /experiment_results -name "experiment.log" | xargs -I % tail -n 4 %"
	echo "\n\n"
done