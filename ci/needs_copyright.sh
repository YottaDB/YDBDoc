#!/bin/sh

#################################################################
#								#
# Copyright (c) 2020-2024 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

# Determines whether a file should need a copyright by its name
# Returns 0 if it needs a copyright and 1 otherwise.
# Returns 2 if an error occurs.
set -eu

if ! [ $# = 1 ]; then
	echo "usage: $0 <filename>"
	exit 2
fi

file="$1"

# Don't require deleted files to have a copyright
if ! [ -e "$file" ]; then
       exit 1
fi

# .png, .gif and .svg are image format files
# .c, .m, .go, .pl, .py and .rs are example program files
skipextensions="png gif svg css c m go pl py rs zip" # List of extensions that cannot have copyrights.
if echo "$skipextensions" | grep -q -w "$(echo "$file" | awk -F . '{print $NF}')"; then
	exit 1
fi

# Below is a list of specific files that do not have a copyright or are from an
# external project with a specific copyright so ignore them
skiplist="COPYING README.md AcculturationGuide/sayhello* AcculturationGuide/xyz*"
for skipfile in $skiplist; do
	if [ "$file" = "$skipfile" ]; then
		exit 1
	fi
done
