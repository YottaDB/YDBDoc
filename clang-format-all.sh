#!/bin/bash
#################################################################
#								#
# Copyright (c) 2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

set -e # Fail script if any command fails
set -u # Enable detection of uninitialized variables
set -o pipefail	# this way $? is set to zero only if ALL commands in a pipeline succeed. Else only last command determines $?

if [ -x "$(which "$1")" ]; then
	CLANG_FORMAT="$1"
else
	CLANG_FORMAT=clang-format
fi

find . -name '*.c' -o -name '*.h' | xargs "$CLANG_FORMAT" -i
find . -regex '.*\.pyi?' | grep -v venv | grep -v "copyright.py" | xargs black -l 132
if ! [ $(git diff --stat | wc -l) = 0 ]; then
  echo " -> Formatting differences found!"
  git diff
  exit 1
fi
