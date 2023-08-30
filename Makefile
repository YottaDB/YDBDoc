 ###############################################################
 #                                                             #
 # Copyright (c) 2017-2023 YottaDB LLC and/or its subsidiaries.#
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
SPHINXOPTS    = -W
SPHINXBUILD   = sphinx-build
SPHINXPROJ    = GTMAdminOps
SOURCEDIRS    = AcculturationGuide AdminOpsGuide ApplicationsManual MessageRecovery MultiLangProgGuide ProgGuide StyleGuide Plugins
SOURCEDIR     = .
BUILDDIR      = _build


# Put it first so that "make" without argument is like "make help".
help:
	@$(SPHINXBUILD) -M help "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)

.PHONY: help Makefile

# Clean target: remove all files under _build directory, as well as
# duplicated favicons.
# The Sphinx command below functions the same way as that for the
# Catch-all target.
clean:
	@$(foreach dir, $(SOURCEDIRS), rm -rf "$(dir)/favicon.png" "$(dir)/_static" "$(dir)/_templates" "$(dir)/LICENSE.rst" "$(dir)/logo.png";)
	@$(foreach dir, $(SOURCEDIRS), $(SPHINXBUILD) -M $@ "$(dir)" "$(dir)/$(BUILDDIR)" $(SPHINXOPTS) $(O);)

# Catch-all target: route all unknown targets to Sphinx using the new
# "make mode" option.  $(O) is meant as a shortcut for $(SPHINXOPTS).
# All of the $(SOURCEDIRS) are built at once.
%: Makefile
	@if [ ! -d ../lua-yottadb ]; then \
		git clone --depth 1 https://github.com/anet-be/lua-yottadb.git ../lua-yottadb; \
		else (cd ../lua-yottadb && git pull); \
	fi ;\
    	cp ../lua-yottadb/docs/lua-yottadb-ydbdocs.rst ./MultiLangProgGuide/
	@$(foreach dir, $(SOURCEDIRS), ln -s ../shared/LICENSE.rst ../shared/_static  ../shared/_templates  ../shared/favicon.png ../shared/logo.png "$(dir)";)
	@$(foreach dir, $(SOURCEDIRS), $(SPHINXBUILD) -M $@ "$(dir)" "$(dir)/$(BUILDDIR)" $(SPHINXOPTS) $(O);)
