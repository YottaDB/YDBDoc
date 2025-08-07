 ###############################################################
 #                                                             #
 # Copyright (c) 2017-2025 YottaDB LLC and/or its subsidiaries.#
 # All rights reserved.                                        #
 #                                                             #
 #     This source code contains the intellectual property     #
 #     of its copyright holder(s), and is made available       #
 #     under a license.  If you do not know the terms of       #
 #     the license, please stop and do not read further.       #
 #                                                             #
 ###############################################################

# Minimal makefile for Sphinx documentation
#

# You can set these variables from the command line.
SPHINXOPTS	= -W --keep-going -q
SPHINXBUILD	= sphinx-build
SOURCEDIRS	= AcculturationGuide AdminOpsGuide ApplicationsManual MessageRecovery MultiLangProgGuide ProgrammersGuide StyleGuide Plugins Octo
SOURCEDIR	= .
BUILDDIR	= public
OCTODIR		= $(shell ci/needs-clone.sh https://gitlab.com/YottaDB/DBMS/YDBOcto.git/)
# newline used in the foreach statements to make "make" stop at an erroring step.
define newline


endef

# Remove obsolete file to avoid error when run after building an old version in the same clone
$(shell rm -f MultiLangProgGuide/lua-yottadb-ydbdocs.rst)

# Put it first so that "make" without argument is like "make help".
help:
	@$(SPHINXBUILD) -M help "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)

.PHONY: help Makefile

# Clean target: remove all files under _build directory, as well as duplicated files.
# We don't use per-dir build directories anymore, but keep this around for a while so it removes cached artifacts.
# https://www.extrema.is/blog/2021/12/17/makefile-foreach-commands talks about why we use $(newline): to stop make at a specific step if it fails.
clean:
	@$(foreach dir, $(SOURCEDIRS), rm -rf "$(dir)/favicon.png" "$(dir)/_static" "$(dir)/_templates" "$(dir)/LICENSE.rst" "$(dir)/logo.png" $(newline))
	@rm -f MultiLangProgGuide/YDBLua-ydbdocs.rst YDBLua-ydbdocs.rst
	@rm -rf ./src/
	@rm -rf ./Octo/
	@rm -rf ./public/
	@rm -rf ./git-checkouts/
	@rm -f err_*.out

# Test target: Create the "tarball" directory and run deadlinks on it
# Relies on html, which gets passed to the % target below
test: html
	ci/error-check.sh
	@if [ ! -f ./deadlinks ]; then \
		echo "Downloading deadlinks..."; \
		DEADLINKS_VERSION=$$(wget -q -O - https://api.github.com/repos/deadlinks/cargo-deadlinks/releases/latest | jq --raw-output .tag_name); \
		echo $$DEADLINKS_VERSION; \
		wget https://github.com/deadlinks/cargo-deadlinks/releases/download/$$DEADLINKS_VERSION/deadlinks-linux -O deadlinks; \
		chmod +x ./deadlinks; \
	fi
	@touch public/AcculturationGuide/Debian-Hybrid_yottadbworkshop15.zip
	@./deadlinks -v public

# Set target-specific variable to not fail on warnings, since "table-row-spanning" warning occurs when making manpages
man: SPHINXOPTS=-q

# Fetch YDBLua-ydbdocs.rst from where YDBLua pipeline deployed it, so that Sphinx can import it.
MultiLangProgGuide/YDBLua-ydbdocs.rst:
	wget --no-verbose https://yottadb.gitlab.io/lang/ydblua/YDBLua-ydbdocs.rst --directory-prefix=MultiLangProgGuide/

# Fetch Octo documentation from YDBOcto repository and replicate file structure
# to allow build without modification to upstream YDBOcto.
.PHONY: octo
octo:
	mkdir -p ./Octo ./src/aux
	rsync -ar $(OCTODIR)/doc/ ./Octo
	cp $(OCTODIR)/src/aux/octo.conf.default ./src/aux/
	rm ./Octo/conf.py

html: MultiLangProgGuide/YDBLua-ydbdocs.rst octo
	+./buildall.sh

.PHONY: checklinks
checklinks: linkcheck
	# noop

# Catch-all target: route all unknown targets to Sphinx using the new
# "make mode" option.  $(O) is meant as a shortcut for $(SPHINXOPTS).
# All of the $(SOURCEDIRS) are built at once.
%: Makefile MultiLangProgGuide/YDBLua-ydbdocs.rst
	$(SPHINXBUILD) -M $@ "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)

# uses catch-all
linkcheck: html
