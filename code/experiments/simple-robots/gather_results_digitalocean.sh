#!/bin/bash
if [ -e remote_results ] ; then
    echo "Deleting old remote results"
    rm -rf remote_results
fi

mkdir remote_results

for (( i=1 ; $i <= 10 ; i=$i + 1 )) 
do
	NODE="sim$i"
    echo "Gathering results from $NODE"
	docker-machine scp -r $NODE:/experiment_results ./remote_results
done