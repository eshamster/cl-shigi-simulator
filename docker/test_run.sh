#!/bin/bash

set -eu

name=test-cl-shigi-simulator

docker rmi $(docker images | awk '/^<none>/ { print $3 }') || echo "ignore rmi error"
docker rm `docker ps -a -q` || echo "ignore rm error"

build_log=/tmp/build.log
echo "output build log to ${build_log}" 1>&2
docker build -t ${name} -t ${name} . > ${build_log} 2>&1
# docker run --name=shigi -p 80:8080 -it ${name}:latest /bin/sh
docker run --name=shigi-sim -p 80:8080 -d ${name}
