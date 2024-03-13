#!/bin/sh

#################################################################
#								#
# Copyright (c) 2024 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################
# This script verifies that shared files get the copyright updated
# Return 0 if okay, 1 if needs attention
verbose=$1
set -eu

exitstatus=0

# This year
year=`date +%Y`

# Check all configuration files have the current year
conffiles="*/conf.py"
set +e
for file in $conffiles; do
	[ -n "$verbose" ] && echo -n "Checking $file: "
	grep -q "^copyright = .*$year" $file
	status=$?
	if [ -n "$verbose" ]; then
		[ $status -eq 0 ] && echo "Pass" || echo "Fail"
	fi
	[ $status -ne 0 ] && exitstatus=1
done
set -e

# Check templates and make sure they have the current year
templatefiles="shared/_templates/*"
set +e
for file in $templatefiles; do
	[ -n "$verbose" ] && echo -n "Checking $file: "
	grep -q "Copyright (c) .*$year" $file
	status=$?
	if [ -n "$verbose" ]; then
		[ $status -eq 0 ] && echo "Pass" || echo "Fail"
	fi
	[ $status -ne 0 ] && exitstatus=1
done
set -e

# Exit with status
exit $exitstatus
