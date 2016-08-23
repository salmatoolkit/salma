echo $1
for (( i=1 ; $i <= 10 ; i=$i + 1 )) 
do 
	NODE="sim$i"
    echo "Killing node $NODE"
	docker-machine kill $NODE
	docker-machine rm $NODE
done