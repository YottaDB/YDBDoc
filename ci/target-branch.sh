#!/usr/bin/env bash
#################################################################
#								#
# Copyright (c) 2025-2026 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
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
	# If we reach here, it means HEAD is NOT based off the YDBDoc r2.08 branch.
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
	# Check if current commit YDBDoc HEAD is based off the YDBDoc r2.08 branch.
	if git merge-base --is-ancestor upstream/$YDBDOC_DEV_BRANCH HEAD; then
		# HEAD is based off the YDBDoc r2.08 branch. In that case, we need to compare error messages against
		# YDB master. Therefore, return "master" for this case, unless the current branch corresponds to an
		# open upstream YDB merge request.
		target_branch=master
		# Check if there is an open upstream YDB MR from a branch with the same name as the current YDBDoc branch.
		# 7957109 is the GitLab project ID of the upstream YDB repository. Ask for MRs from this exact source branch.
		# Listing all open YDB MRs instead returns only the first page (20) of them, and matching the branch name
		# against that list as a pattern also matches longer branch names that contain it.
		# A YDBDoc change for a YDB issue (its commit title names one, e.g. "[YDB#NNNN]") usually goes with a YDB MR
		# from a branch of the same name, and that YDB MR is often created a few seconds after this pipeline starts.
		# So, for such a change, keep retrying the lookup for a while before falling back to master. The wait messages
		# go to stderr because the caller reads the result of this script from stdout.
		ydb_mr=""
		if [[ $CI_COMMIT_BRANCH != "master" ]]; then
			commit_title=${CI_COMMIT_TITLE:-$(git log -1 --format=%s)}
			waited_secs=0
			max_wait_secs=60
			while true; do
				curl -s -k "https://gitlab.com/api/v4/projects/7957109/merge_requests?scope=all&state=opened&source_branch=${CI_COMMIT_BRANCH}" > ydb_open_mrs.json
				ydb_mr=$(jq -r --arg branch "$CI_COMMIT_BRANCH" '.[] | select(.source_branch == $branch) | .iid' ydb_open_mrs.json | head -1)
				if [[ "$ydb_mr" != "" ]] || [[ ! "$commit_title" =~ YDB#[0-9] ]] || [[ $waited_secs -ge $max_wait_secs ]]; then
					break
				fi
				echo "# Commit title names a YDB issue: wait 30 seconds for a YDB MR from branch [$CI_COMMIT_BRANCH] to be created (waited $waited_secs of $max_wait_secs seconds so far)" >&2
				sleep 30
				waited_secs=$((waited_secs + 30))
			done
		fi
		if [[ "$ydb_mr" != "" ]]; then
			# The YDBDoc commit branch matches the branch of an upstream YDB MR.
			# In that case, return that MR as the YDB target branch.
			target_branch=$ydb_mr
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
	# Check if current commit YDBDoc HEAD is based off the YDBDoc r2.08 branch.
	if git merge-base --is-ancestor upstream/$YDBDOC_DEV_BRANCH HEAD; then
		# HEAD is based off the YDBDoc r2.08 branch. In that case, we need to do copyright checks on all commits
		# since the YDBDoc r2.08 branch. So return r2.08 in this case.
		echo $YDBDOC_DEV_BRANCH
	else
		# HEAD is based off the YDBDoc master branch. In that case, we need to do copyright checks on all commits
		# since the YDBDoc master branch. So return master in this case.
		echo master
	fi
fi
