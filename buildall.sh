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

# Automatically build each documentation guide as html for each release from r1.36 onwards and copy to common location for upload.
# Expects one argument to specify the (existing) destination path for the copy command, e.g. `./buildall.sh build_dir`

set -euo pipefail
set -v
# for rsync; this expands *.c to nothing if there are no matches, rather than '*.c'
shopt -s nullglob

target="$(realpath "${1:-target}")"
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
	MessageRecovery/
	MultiLangProgGuide/
	ProgGuide/
	StyleGuide/
	Plugins/
)

if ! git remote | grep -q upstream
then
    git remote add upstream https://gitlab.com/YottaDB/DB/YDBDoc/
fi
git fetch upstream --tags
tags=( `git tag --list` )
tags_len=${#tags[@]}

rsync() {
	command rsync --delete -lrtuv --exclude=\*.zip --exclude=.buildinfo --exclude=.nojekyll "$@"
}

copy_built_docs() {
    for directory in "${DIRECTORIES[@]}"; do
	output="$target/$1/${directory%?}"
	# Handling ProgGuide to ProgrammersGuide conversion
	if [ $directory = "ProgGuide/" ]
	then
	    output="$target/$1/ProgrammersGuide"
	fi
	mkdir $output
	pushd $directory
	rsync _build/html/ "$output/"
	# Update the following line as additional languages are supported
	rsync --exclude=conf.py *.c *.m *.go *.pl *.py *.rs *.js *.lua "$output/"
	# Handling AcculturationGuide VM file dummy file creation
	# for deadlinks job in the pipeline
	if [ $directory = "AcculturationGuide/" ]
	then
	    touch "$output"/Debian_yottadbworkshop.zip
	fi
	popd
    done
}

# Building and copying documentation for master branch (current)
mkdir $target/current
make html
copy_built_docs "current"
cp index.html "$target/current"
make clean

# The latest tag is checked out, its documentation is built and copied to the appropriate directories
for tag in ${tags[@]:7:$tags_len-7}; do
    dir_name=`echo $tag | sed -e 's/d/r/g' -e 's/\.//g'`
    mkdir $target/$dir_name
    git checkout -f $tag

    # A lot of changes were made to the documentation after r1.34, which is why a lot of fixes have
    # to be done manually in this script.
    # These are changes that need to be made pre-documentation build
    if [ $dir_name = "r134" ]
    then
	# The r1.34 Plugins manual has no templates, which are necessary to display the dropdown menu,
	# copy them over from Acculturation Guide.
	mkdir Plugins/_templates
	cp AcculturationGuide/_templates/footer.html Plugins/_templates/footer.html
	cp AcculturationGuide/_templates/searchbox.html Plugins/_templates/searchbox.html


        for directory in "${DIRECTORIES[@]}"; do
	    print_dir=$directory
	    if [ $directory = "ProgGuide/" ]
	    then
		print_dir="ProgrammersGuide/"
	    fi
	    # Update r134 conf.py files to add necessary code for RELEASE SPECIFIC DOCUMENTATION
	    # This includes creating and populating html_context
		cat <<-EOF >> ${directory%?}/conf.py
		from git import Repo
		repo = Repo( search_parent_directories=True )

		try:
		   html_context
		except NameError:
		   html_context = dict()

		html_context['releases'] = list()
		html_context['curr_rel'] = "r1.34"

		tags = sorted(repo.tags, key=lambda t: t.commit.committed_datetime)[7:]
		html_context['releases'].append(("current", "current/${print_dir%?}/"))
		for tag in tags:
		   current_tag = str(tag).replace("d","r")
		   html_context['releases'].append((current_tag, current_tag.replace(".","") + "/${print_dir%?}/"))
		EOF

		# Update searchbox.html template to add necessary code for RELEASE SPECIFIC DOCUMENTATION
		# This includes creating the dropdown menu.
		cat <<-EOF >> ${directory%?}/_templates/searchbox.html
		<br/>
		<div class="rst-releases">
		{% if releases %}
		<h5 class="release-dropdown">{{ _('Releases : ') }}</h5>
		<select class="release-dropdown" id="select-releases">
			{%- for tag,url in releases %}
			<option id="{{ tag }}" value="{{ url }}"{% if curr_rel == tag %} selected{% endif %}>{{ tag }}</option>
			{%- endfor %}
		</select>
		{% endif %}
		</div>
		EOF
        done
    fi

    make html

    copy_built_docs $dir_name
    cp index.html "$target/$dir_name"

    # These are post documentation build changes for r1.34
    if [ $dir_name = "r134" ]
    then
        for directory in "${DIRECTORIES[@]}"; do
	    if [ $directory = "ProgGuide/" ]
	    then
	        directory="ProgrammersGuide/"
	    fi
	    # Copy custom_script.js to r1.34 documentation and embedding the script in all HTML files
	    cp $target/current/${directory%?}/_static/js/custom_script.js $target/$dir_name/${directory%?}/_static/js/
	    for html_file in $target/$dir_name/${directory%?}/*.html; do
		sed -i 's#<script src="_static/js/theme\.js"></script>#<script src="_static/js/theme\.js"></script><script src="_static/js/custom_script\.js"></script>#g' $html_file
	    done

	    # Adjust alignment of dropdown menu, add to custom.css
	    cat <<-EOF >> $target/$dir_name/${directory%?}/_static/css/custom.css
		.release-dropdown {
			display: inline-block;
		}
		EOF
	done

	# Fix r134/AdminOpsGuide/dbmgmt.html for deadlinks job
	sed -i 's#href="/ProgrammersGuide#href="\.\./ProgrammersGuide#g' $target/r134/AdminOpsGuide/dbmgmt.html
	# Fix r134/index.html for deadlinks job
	sed -i 's#href="Octo/index\.html#href="\.\./Octo/index\.html#g' $target/r134/index.html

    fi

    make clean
done

# Build the documentation from other repositories
pushd "$octo"/doc
make html
rsync _build/html/ "$target/Octo"
popd

# Remove unused fonts
echo "Removing unused fonts..."
find $target -iname 'lato*' -delete
find $target -iname 'roboto*' -delete
