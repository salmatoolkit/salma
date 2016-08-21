if [ -d distrib ]; then
	rm -rf distrib
fi
mkdir distrib
cp -r ../../code/SALMA/salma distrib/salma
cp -r ../../code/SALMA/ecl-src distrib/ecl-src

docker build -t salma-basic .