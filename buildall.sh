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
ydb="${3:-../YDB}"
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
needs_clone "$ydb" https://gitlab.com/YottaDB/DB/YDB

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

# This section of script responsible for detecting undocumented or incorrectly documented messages
# More details on https://gitlab.com/YottaDB/DB/YDBDoc/-/issues/409

# We wrap this with set -e and set -e because we need to retrieve list of all errors in YDB/sr_port/*.msg
# that doesn't exist in MessageRecovery/errors.rst. Otherwise, this script will exit immedietely.
# Script will still exit after checking finished and show list of errors.
set +e

# Check if every message documented in errors.rst is also documented with one error number
# (or more error numbers in rare cases) in errormsgref.rst

grep -A 1 '^---' MessageRecovery/errors.rst | grep '^[A-Z]' | grep -vwE "ILLEGALUSE|INVALIDGBL" > /tmp/err_errors.out
grep '^| [0-9]' MessageRecovery/errormsgref.rst | sed 's/^| [0-9][0-9]* | //;s/,.*//g;' | sort -u > /tmp/err_errormsgref.out
if ! diff /tmp/err_errors.out /tmp/err_errormsgref.out > err_diff.out; then
	echo "FATAL: some error message documented with error numbers/more error numbers in errormsgref.rst"
	echo "Please check diff output below:"
	cat err_diff.out
	exit 1
else
	rm err_diff.out
fi


# Check if Every message number recorded in errormsgref.rst is accurate
# (i.e. is aligned with the number in the YDB/sr_port/ydb*errors.h files in the YDB project
grep "#define" ../YDB/sr_port/ydb*errors.h | grep -v '#define CMI_REASON_' | grep -vE "CMI_BADPORT|CMI_NOTND|CMI_OVERRUN|CMI_NOSERVENT|CMI_BADIPADDRPORT|ERR_ACK|ERR_ASC2EBCDICCONV|ERR_BADTAG|ERR_DBGLDMISMATCH|ERR_DRVLONGJMP|ERR_ENQ|ERR_FAKENOSPCLEARED|ERR_FREEZEID|ERR_INVDBGLVL|ERR_JNLREQUIRED|ERR_JNLWRTNOWWRTR|ERR_JOBINTRRETHROW|ERR_JOBINTRRQST|ERR_KRNLKILL|ERR_LVMONBADVAL|ERR_MUDESTROYFAIL|ERR_MUDESTROYSUC|ERR_REPEATERROR|ERR_REPLONLNRLBK|ERR_TPRETRY|ERR_WILLEXPIRE|ERR_YDIRTSZ|ERR_ZDEFACTIVE|ERR_ZDEFOFLOW|ERR_ZLINKBYPASS|ERR_UNUSEDMSG" | sed 's/.*#define CMERR_// ; s/.*#define CMI_// ; s/.*#define ERR_// ; s/.*#define GDE_// ; ' | awk '{print $2,$1}' | sort > /tmp/err_ydberror.out
grep '| [0-9]' MessageRecovery/errormsgref.rst | grep -vEw "DBRNDWNBYPASS|SIGGORTNTIMEOUT|SIGACKTIMEOUT" | sed 's/,.*//;' | awk '{print $2,$4}' | sort > /tmp/err_errormsgref.out
if ! diff /tmp/err_ydberror.out /tmp/err_errormsgref.out > err_diff2.out; then
	echo "FATAL: message number recorded in errormsgref.rst is NOT accurate"
	echo "Please check diff output below:"
	cat err_diff2.out
	exit 1
else
	rm err_diff2.out
fi

# Check of 1-line message text in errors.rst for each message is
# in sync with the message text in YDB/sr_port/*.msg in the YDB project,
# and with the message text in errormsgref.rst.
ci/error_sync.py

set +e


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
# Duplicate references typically come from duplicate heading names when building the index, since Sphinx automatically
# creates reference names for each heading. cf. Hyperlink Targets:
#    https://docutils.sourceforge.io/docs/ref/rst/restructuredtext.html#explicit-hyperlink-targets
# To fix this, RENAME A HEADING, or give it an explicit reference. For example to give
# heading GDE the reference name GDE2, prefix it with this (surrounded by blank lines):
#
# .. _GDE2:
#
# ~~~~~~
# GDE
# ~~~~~~
outfile="$target/duplicate_reference.out"
find $target -name index.html -exec grep "href.*#id[0-9]" /dev/null {} \; >& $outfile
if [ -s $outfile ]; then
	echo "--------------------------------------------------------------------------"
	echo "Duplicate references found by buildall.sh. List follows. Fix those first."
	echo "--------------------------------------------------------------------------"
	cat $outfile
	echo "--------------------------------------------------------------------------"
	exit 1
else
	echo "No duplicate references found"
fi
rm -f $outfile
