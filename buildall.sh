#!/bin/bash

# Automatically build each documentation guide as html and copy to common location for upload.
# Expects one argument to specify the (existing) destination path for the copy command, e.g. `./buildall.sh build_dir`

for directory in $(ls -d */); do
    pushd $directory
    make html
    cp -r _build/html $1/${directory%?}
    popd
done
