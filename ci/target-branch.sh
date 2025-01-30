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

set -e

# The master branch represents the latest *published* release of YDB.
# So the error messages it documents may be different between the latest release and the latest commit to YDB master.
# NOTE: this script hardcodes both the current development version and the latest published version of YDB.

YDB_CURPRO_TAG=r2.02	# the YDB project tag corresponding to the latest production release
YDBDOC_DEV_BRANCH=r2.04	# the YDBDoc branch name with documentation for the upcoming/next YDB release

case $1 in
	ydb) ydb=1;;
	ydbdoc) ydb=0;;
	*) echo "unknown target repo: '$1'"; exit 1;;
esac

if ! git remote | grep -q upstream; then
	git remote add upstream https://gitlab.com/YottaDB/DB/YDBDoc.git
fi
git fetch upstream

if ! git merge-base --is-ancestor upstream/$YDBDOC_DEV_BRANCH HEAD; then
	# If we reach here, it means HEAD is NOT based off the YDBDoc r2.04 branch.
	if ! git merge-base --is-ancestor upstream/master HEAD; then
		# If we reach here, it means HEAD is NOT based off the YDBDoc master branch either.
		echo "ERROR: HEAD should be based off [$YDBDOC_DEV_BRANCH] or [master] branch."
		echo "ERROR: This script will not work correctly until the situation is fixed."
		exit 1
	fi >&2
fi

if [ "$ydb" = 1 ]; then
	# This code path is reached if the YDBDoc pipeline "pages" job is being run.
	# Caller script ci/error-check.sh wants to know which YDB commit to compare error messages of YDBDoc HEAD against.
	# Check if current commit YDBDoc HEAD is based off the YDBDoc r2.04 branch.
	if git merge-base --is-ancestor upstream/$YDBDOC_DEV_BRANCH HEAD; then
		# HEAD is based off the YDBDoc r2.04 branch. In that case, we need to compare error messages against
		# YDB master. Therefore, return "master" for this case.
		echo upstream/master
	else
		# HEAD is based off the YDBDoc master branch. In that case, we need to compare error messages against
		# the latest production release of YDB. Therefore, return that.
		# NOTE: git does not allow tags to contain the prefix of the remote, so we just have to hope that there was no cached pre-existing tag.
		echo $YDB_CURPRO_TAG
	fi
else
	# This code path is reached if the YDBDoc pipeline "commit-verify" job is being run.
	# Caller script ci/commit-verify.sh wants to know what is the YDBDoc target branch to use as the base of the MR
	# and do copyright checks on all commits since that base commit.
	# Check if current commit YDBDoc HEAD is based off the YDBDoc r2.04 branch.
	if git merge-base --is-ancestor upstream/$YDBDOC_DEV_BRANCH HEAD; then
		# HEAD is based off the YDBDoc r2.04 branch. In that case, we need to do copyright checks on all commits
		# since the YDBDoc r2.04 branch. So return r2.04 in this case.
		echo $YDBDOC_DEV_BRANCH
	else
		# HEAD is based off the YDBDoc master branch. In that case, we need to do copyright checks on all commits
		# since the YDBDoc master branch. So return master in this case.
		echo master
	fi
fi
