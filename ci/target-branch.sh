#!/usr/bin/env bash
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

source ci/settings.sh

if [[ "$CI_COMMIT_BRANCH" = "" ]]; then
	# This script is running outside of the CI pipeline. In that case, the current YDBDoc branch name
	# must be looked up explicitly for use in target-branch.sh and subsequent logic below. So, set
	# CI_COMMIT_BRANCH to what it would be if this script were running in the pipeline.
	export CI_COMMIT_BRANCH=$(git rev-parse --abbrev-ref HEAD)
fi

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
		master_commits="$(git log --oneline upstream/master...HEAD | wc -l)"
		dev_commits="$(git log --oneline upstream/$YDBDOC_DEV_BRANCH...HEAD | wc -l)"
		if [ "$master_commits" -le "$dev_commits" ]; then
			merge_base=$(git merge-base upstream/master HEAD)
			echo -n "NOTE: HEAD is based off an old version [$merge_base] of [master]. "
			echo "Consider rebasing: 'git rebase upstream/master'"
		else
			merge_base=$(git merge-base upstream/$YDBDOC_DEV_BRANCH HEAD)
			echo -n "NOTE: HEAD is based off an old version [$merge_base] of [$YDBDOC_DEV_BRANCH]. "
			echo "Consider rebasing: 'git rebase upstream/$YDBDOC_DEV_BRANCH'"
		fi
		exit 1
	fi >&2
fi

if [ "$ydb" = 1 ]; then
	# This code path is reached if the YDBDoc pipeline "pages" job is being run.
	# Caller script ci/error-check.sh wants to know which YDB commit to compare error messages of YDBDoc HEAD against.
	# Check if current commit YDBDoc HEAD is based off the YDBDoc r2.04 branch.
	if git merge-base --is-ancestor upstream/$YDBDOC_DEV_BRANCH HEAD; then
		# HEAD is based off the YDBDoc r2.04 branch. In that case, we need to compare error messages against
		# YDB master. Therefore, return "master" for this case, unless the current branch corresponds to an
		# open upstream YDB merge request.
		target_branch=master
		# Check if there is an open upstream YDB MR corresponding to the current YDBDoc branch
		# 7957109 is the GitLab project ID the upstream YDB repository
		curl -s -k "https://gitlab.com/api/v4/projects/7957109/merge_requests?scope=all&state=opened" > ydb_open_mrs.json
		ydbdoc_branches=$(jq -r '.[] | "\(.source_branch) \(.iid)"' ydb_open_mrs.json)
		ydbdoc_mr=$(echo "$ydbdoc_branches" | grep "$CI_COMMIT_BRANCH" | cut -f 2 -d " ")
		if [[ $CI_COMMIT_BRANCH != "master" ]] && [[ "$ydbdoc_mr" != "" ]]; then
			# The YDBDoc commit branch matches the branch an upstream YDB MR.
			# In that case, return that branch as the YDB target branch.
			target_branch=$ydbdoc_mr
		fi
		echo $target_branch
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
