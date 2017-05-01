#!/bin/sh

set -eu

cd ${HOME}/.roswell/local-projects/cl-shigi-simulator
mkdir -p libs
cd libs
for tar_url in $(grep url ../qlfile.lock | awk '{print $2}'); do
    wget -O - $(echo ${tar_url} | sed -e 's/"//g') | tar zxf -
done

for dir in $(ls .); do
    new_name=$(echo ${dir} | sed -e 's/-[a-z0-9]\+$//')
    mv ${dir} ${new_name}
done
