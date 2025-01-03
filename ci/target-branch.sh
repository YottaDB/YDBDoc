#!/bin/sh
#################################################################
#                                                               #
# Copyright (c) 2025 YottaDB LLC and/or its subsidiaries.       #
# All rights reserved.                                          #
#                                                               #
#       This source code contains the intellectual property     #
#       of its copyright holder(s), and is made available       #
#       under a license.  If you do not know the terms of       #
#       the license, please stop and do not read further.       #
#                                                               #
#################################################################

# The master branch represents the latest *published* release of YDB.
# So the error messages it documents may be different between the latest release and the latest commit to YDB master.
# NOTE: this script hardcodes both the current development version and the latest published version of YDB.

CURRENT_DEV_VERSION=r2.04
LATEST_PUBLISHED_VERSION=r2.02

case $1 in
	ydb) ydb=1;;
	ydbdoc) ydb=0;;
	*) echo "unknown target repo: '$1'"; exit 1;;
esac

if ! git remote | grep -q upstream; then
	git remote add upstream https://gitlab.com/YottaDB/DB/YDBDoc.git
fi
git fetch upstream

# We depend on upstream/master not being a base of CURRENT_DEV_VERSION.
# If it is, we will get a false positive with `--is-ancestor` below and compare against the wrong branch.
# Error out with a useful message instead of silently doing the wrong thing.
if git merge-base --is-ancestor upstream/master upstream/$CURRENT_DEV_VERSION; then
	echo "ERROR: $CURRENT_DEV_VERSION should not be based off the master branch."
	echo "ERROR: this script will not work correctly until the situation is fixed."
	exit 1
fi >&2

git merge-base --is-ancestor upstream/$CURRENT_DEV_VERSION HEAD
head_is_ancestor_of_dev_version=$?
if [ "$ydb" = 1 ]; then
	# This code path is reached if the YDBDoc pipeline "pages" job is being run.
	# Caller script ci/error-check.sh wants to know which YDB commit to compare error messages of YDBDoc HEAD against.
	# Check if current commit YDBDoc HEAD is based off the YDBDoc r2.04 branch.
	if [ 0 = $head_is_ancestor_of_dev_version ]; then
		# HEAD is based off the YDBDoc r2.04 branch. In that case, we need to compare error messages against
		# YDB master. Therefore, return "master" for this case.
		echo master
	else
		# HEAD is based off the YDBDoc master branch. In that case, we need to compare error messages against
		# the latest production release of YDB. Therefore, return that.
		echo $LATEST_PUBLISHED_VERSION
	fi
else
	# This code path is reached if the YDBDoc pipeline "commit-verify" job is being run.
	# Caller script ci/commit-verify.sh wants to know what is the YDBDoc target branch to use as the base of the MR
	# and do copyright checks on all commits since that base commit.
	# Check if current commit YDBDoc HEAD is based off the YDBDoc r2.04 branch.
	if [ 0 = $head_is_ancestor_of_dev_version ]; then
		# HEAD is based off the YDBDoc r2.04 branch. In that case, we need to do copyright checks on all commits
		# since the YDBDoc r2.04 branch. So return r2.04 in this case.
		echo $CURRENT_DEV_VERSION
	else
		# HEAD is based off the YDBDoc master branch. In that case, we need to do copyright checks on all commits
		# since the YDBDoc master branch. So return master in this case.
		echo master
	fi
fi
