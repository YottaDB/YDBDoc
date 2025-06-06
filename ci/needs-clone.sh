#!/usr/bin/env bash

#################################################################
#
# Copyright (c) 2025 YottaDB LLC and/or its subsidiaries.
# All rights reserved.
#
#	This source code contains the intellectual property
#	of its copyright holder(s), and is made available
#	under a license.  If you do not know the terms of
#	the license, please stop and do not read further.
#
#################################################################

set -euo pipefail

usage() {
	echo "usage: $0 <remote url> [<target branch>] [workdir] [<local dir>]"
	exit 1
}

if [ $# = 0 ] || [ $# -gt 3 ]; then
	usage
fi

remote="$1"
branch_with_remote="${2:-upstream/HEAD}"
branch=$(echo "$branch_with_remote"| cut -d / -f 2-)
workdir=${3:-git-checkouts/$(echo "$remote" | sed 's#/$##; s#\.git$##' | rev | cut -d/ -f1 | rev | cut -d. -f1)}
workdir_path=$(realpath .)/$workdir
local_dir="${4:-../$(basename "$workdir")}"

mkdir -p "$(basename "$workdir")"

echo "Updating git checkout of $workdir" >&2

if [ ! -d "$workdir" ]; then \
	if [ -d "$local_dir" ]; then
		(
			cd "$local_dir"
			if ! git remote | grep -qFx upstream; then
				git remote add upstream "$remote"
			fi
			git fetch upstream "$branch"
			git remote set-head upstream -a
			git worktree add --force --detach "$workdir_path" "$branch_with_remote"
		)
	else
		git clone "$remote" "$workdir"
		cd "$workdir"
		git remote add upstream "$remote"
		git fetch upstream
		git remote set-head upstream -a
	fi
fi >&2

{
	cd "$workdir_path"
	git fetch --tags upstream "$branch"
	git checkout --detach "$branch_with_remote"
} >&2

echo "$workdir"
