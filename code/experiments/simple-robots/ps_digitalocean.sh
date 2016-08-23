#!/bin/bash
for (( i=1 ; $i <= 10 ; i=$i + 1 ))
do
	NODE="sim$i"
    echo "Status of $NODE:"
    echo "----------------"
	docker-machine ssh $NODE docker ps
	echo "\n\n"
done