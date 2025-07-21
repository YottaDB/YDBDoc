#!/bin/bash
 ###############################################################
 #                                                             #
 # Copyright (c) 2019-2025 YottaDB LLC and/or its subsidiaries.#
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

if [ $# -gt 1 ]; then
	echo "usage: $0 [build-dir]"
	exit 1
fi

rsync() {
        command rsync --delete -lrtu --exclude=\*.zip --exclude=.buildinfo --exclude=.nojekyll "$@"
}

target="${1:-public}"
mkdir -p "$target"
target=$(realpath "$target")

DIRECTORIES=(
        AcculturationGuide/
        AdminOpsGuide/
        ApplicationsManual/
        MessageRecovery/
        MultiLangProgGuide/
        ProgrammersGuide/
        StyleGuide/
        Plugins/
)

# sphinx has completely broken caching.
# In particular, updating the title of `ProgrammersGuide/index.rst` will not update any other subproject files, even though it's linked in e.g. `StyleGuide/index.html`.
# Overrule sphinx and force it to rebuild the headers.
rm -f public/*/index.html
# $CI is set by GitLab, per https://docs.gitlab.com/ci/variables/predefined_variables/
if [[ "${CI:-}" = "true" ]]; then
	# When running in the pipeline, fail the build if there are any warnings
	sphinx-build -W . public
else
	sphinx-build . public
fi
# Copy the HTML files
for directory in "${DIRECTORIES[@]}"; do
	pushd $directory >/dev/null
	rsync --exclude=conf.py *.c *.m *.go *.pl *.py *.rs *.js *.lua "$target/${directory%?}"
	popd >/dev/null
done

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
