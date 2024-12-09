#!/bin/bash
 ###############################################################
 #                                                             #
 # Copyright (c) 2019-2024 YottaDB LLC and/or its subsidiaries.#
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
# for rsync; this expands *.c to nothing if there are no matches, rather than '*.c'
shopt -s nullglob

target="$(realpath "${1:-public}")"
octo="${2:-../YDBOcto}"
mkdir -p "$target"

usage() {
	echo "usage: $0 [target [octo]]"
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

DIRECTORIES=(
	AcculturationGuide/
	AdminOpsGuide/
	ApplicationsManual/
	MessageRecovery/
	MultiLangProgGuide/
	ProgGuide/
	StyleGuide/
	Plugins/
)

rsync() {
	command rsync --delete -lrtu --exclude=\*.zip --exclude=.buildinfo --exclude=.nojekyll "$@"
}

# Copy the HTML files
for directory in "${DIRECTORIES[@]}"; do
	pushd $directory >/dev/null
	output="$target/${directory%?}"
	rsync _build/html/ "$output/"
	# Update the following line as additional languages are supported
	rsync --exclude=conf.py *.c *.m *.go *.pl *.py *.rs *.js *.lua "$output/"
	popd >/dev/null
done
cp index.html "$target"

# Build the documentation from other repositories
pushd "$octo"/doc >/dev/null
echo "Building Octo HTML docs..."
make html >/dev/null
rsync _build/html/ "$target/Octo"
popd >/dev/null

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

# Verify that there are no duplicate references. Every duplicate reference will be automatically named as a numbered link
# containing for example "#id1", "#id2" etc. by Sphinx so we look for that below. And expect NO such lines in "index.html".
# If we do find such lines, they need to be fixed to remove the duplicate reference thereby every reference will have a
# named link (which is a much better permalink than a numbered link). See YDBDoc#397 for more details.
outfile="public/duplicate_reference.out"
find public -name index.html -exec grep "href.*#id[0-9]" /dev/null {} \; >& $outfile
if [ -s $outfile ]; then
	echo "--------------------------------------------------------------------------"
	echo "Duplicate references found by buildall.sh. List follows. Fix those first."
	echo "--------------------------------------------------------------------------"
	cat $outfile
	echo "--------------------------------------------------------------------------"
	exit 1
fi
rm -f $outfile
