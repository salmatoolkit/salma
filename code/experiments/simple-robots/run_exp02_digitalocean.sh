echo $1
for (( i=1 ; $i <= 10 ; i=$i + 1 )) 
do 
	echo "sim$i"
	NODE="sim$i"

	docker-machine create --driver digitalocean --digitalocean-access-token $DOTOKEN $NODE
	eval $(docker-machine env sim$i)
	docker run -d -v /experiment_results:/simulations/simple-robots/experiment_results \
		ckroiss/simple_robots --simulations_per_config 1 $1 $1
done