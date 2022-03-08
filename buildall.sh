#!/bin/bash
 ###############################################################
 #                                                             #
 # Copyright (c) 2019-2022 YottaDB LLC and/or its subsidiaries.#
 # All rights reserved.                                        #
 #                                                             #
 #     This source code contains the intellectual property     #
 #     of its copyright holder(s), and is made available       #
 #     under a license.  If you do not know the terms of       #
 #     the license, please stop and do not read further.       #
 #                                                             #
 ###############################################################

# Automatically build each documentation guide as html and copy to common location for upload.
# Expects one argument to specify the (existing) destination path for the copy command, e.g. `./buildall.sh build_dir`

set -euo pipefail
set -v
# for rsync; this expands *.c to nothing if there are no matches, rather than '*.c'
shopt -s nullglob

target="$(realpath "${1:-target}")"
octo="${2:-../YDBOcto}"
posix="${3:-../YDBPosix}"
mkdir -p "$target"

usage() {
	echo "usage: $0 [target [octo [posix]]]"
	exit 1
}

needs_clone() {
	if ! [ -d "$1" ]; then
		echo "error: $1 is not a directory or does not exist"
		echo "help: try running \`git clone $2\`"
		usage
	fi
}

needs_clone "$octo" https://gitlab.com/YottaDB/DBMS/YDBOcto
needs_clone "$posix" https://gitlab.com/YottaDB/Util/YDBPosix

DIRECTORIES=(
	AcculturationGuide/
	AdminOpsGuide/
	MessageRecovery/
	MultiLangProgGuide/
	ProgGuide/
	ReleaseNotes/
	StyleGuide/
	Plugins/
)

rsync() {
	command rsync --delete -lrtuv --exclude=\*.zip --exclude=.buildinfo --exclude=.nojekyll "$@"
}

# Copy the HTML files
for directory in "${DIRECTORIES[@]}"; do
	pushd $directory
	output="$target/${directory%?}"
	rsync _build/html/ "$output/"
	# Update the following line as additional languages are supported
	rsync --exclude=conf.py *.c *.m *.go *.pl *.py *.rs "$output/"
	popd
done
cp index.html "$target"

# Build the documentation from other repositories
pushd "$octo"/doc
make html
rsync _build/html/ "$target/Octo"
popd

pushd "$posix"/doc
make html
rsync _build/html/ "$target/YDBPosix"
popd

# The source directory for the programmer's guide does not have the same name
# as the page the index links to
if [ -d "$target/ProgrammersGuide" ]; then
	rm -r "$target/ProgrammersGuide"
fi
mv "$target"/ProgGuide/ "$target"/ProgrammersGuide/

# Remove unused fonts
echo "Removing unused fonts..."
find $target -iname 'lato*' -delete
find $target -iname 'roboto*' -delete
