#!/bin/bash

# Automatically build each documentation guide as html and copy to common location for upload.
# Expects one argument to specify the (existing) destination path for the copy command, e.g. `./buildall.sh build_dir`

set -euo pipefail
set -v

target="$(realpath "${1:-target}")"
mkdir -p "$target"

DIRECTORIES=(
	AcculturationGuide/
	AdminOpsGuide/
	MessageRecovery/
	MultiLangProgGuide/
	ProgGuide/
	ReleaseNotes/
)

# Copy the HTML files
for directory in "${DIRECTORIES[@]}"; do
    pushd $directory
    output="$target/${directory%?}"
    rsync --delete -lrtuv --exclude=\*.zip --exclude=.buildinfo --exclude=.nojekyll --exclude=_* . "$output"
    popd
done
