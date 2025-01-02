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
SPHINXOPTS    = -W -q
SPHINXBUILD   = sphinx-build
SPHINXPROJ    = GTMAdminOps
SOURCEDIRS    = AcculturationGuide AdminOpsGuide ApplicationsManual MessageRecovery MultiLangProgGuide ProgGuide StyleGuide Plugins
SOURCEDIR     = .
BUILDDIR      = _build
LINKCHECKDIR  = _build/linkcheck
# newline used in the foreach statements to make "make" stop at an erroring step.
define newline


endef


# Put it first so that "make" without argument is like "make help".
help:
	@$(SPHINXBUILD) -M help "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)

.PHONY: help Makefile

# Clean target: remove all files under _build directory, as well as duplicated files.
# The Sphinx command below functions the same way as that for the Catch-all target.
# Remove tarball diretory "public"
clean:
	@$(foreach dir, $(SOURCEDIRS), rm -rf "$(dir)/favicon.png" "$(dir)/_static" "$(dir)/_templates" "$(dir)/LICENSE.rst" "$(dir)/logo.png" $(newline))
	@$(foreach dir, $(SOURCEDIRS), $(SPHINXBUILD) -M $@ "$(dir)" "$(dir)/$(BUILDDIR)" $(SPHINXOPTS) $(O) $(newline))
	@rm -f MultiLangProgGuide/lua-yottadb-ydbdocs.rst
	@rm -rf ./public/
	@rm -f err_*.out

# Test target: Create the "tarball" directory and run deadlinks on it
# Relies on html, which gets passed to the % target below
test: html
	# TODO(jyn514): This check compares the error reference to YDB master. But YDBDoc master refers to the latest
	# published release of YDB, not the latest development commit. I tried disabling it in CI when making an MR against master,
	# but it is also causing issues when building locally. Disable the check for now.
	#ci/error-check.sh
	@+./buildall.sh
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

# Catch-all target: route all unknown targets to Sphinx using the new
# "make mode" option.  $(O) is meant as a shortcut for $(SPHINXOPTS).
# All of the $(SOURCEDIRS) are built at once.
# https://www.extrema.is/blog/2021/12/17/makefile-foreach-commands talks about why we use $(newline): to stop make at a specific step if it fails.
%: Makefile
	cp "$$(ci/needs-clone.sh https://github.com/anet-be/lua-yottadb.git)/docs/lua-yottadb-ydbdocs.rst" ./MultiLangProgGuide/
	@$(foreach dir, $(SOURCEDIRS), ln -sf ../shared/LICENSE.rst ../shared/_static  ../shared/_templates  ../shared/favicon.png ../shared/logo.png "$(dir)" $(newline))
	$(foreach dir, $(SOURCEDIRS), $(SPHINXBUILD) -M $@ "$(dir)" "$(dir)/$(BUILDDIR)" $(SPHINXOPTS) $(O) $(newline))

.PHONY: checklinks
checklinks: html
	$(foreach dir, $(SOURCEDIRS), $(SPHINXBUILD) -b linkcheck "$(dir)" "$(dir)/$(BUILDDIR)";)
