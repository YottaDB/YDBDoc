 ###############################################################
 #                                                             #
 # Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.     #
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
SPHINXOPTS    = -W --keep-going
SPHINXBUILD   = sphinx-build
SPHINXPROJ    = GTMAdminOps
SOURCEDIRS    = AcculturationGuide AdminOpsGuide MessageRecovery MultiLangProgGuide ProgGuide ReleaseNotes StyleGuide
SOURCEDIR     = .
BUILDDIR      = _build
FONTDIR	      = html/_static/fonts

# Put it first so that "make" without argument is like "make help".
help:
	@$(SPHINXBUILD) -M help "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)

.PHONY: help Makefile

# Clean target: remove all files under _build directory.
# The Sphinx command below functions the same way as that for the
# Catch-all target.
clean:
	@$(foreach dir, $(SOURCEDIRS), $(SPHINXBUILD) -M $@ "$(dir)" "$(dir)/$(BUILDDIR)" $(SPHINXOPTS) $(O);)

# Catch-all target: route all unknown targets to Sphinx using the new
# "make mode" option.  $(O) is meant as a shortcut for $(SPHINXOPTS).
# All of the $(SOURCEDIRS) are built at once.
%: Makefile
	@$(foreach dir, $(SOURCEDIRS), $(SPHINXBUILD) -M $@ "$(dir)" "$(dir)/$(BUILDDIR)" $(SPHINXOPTS) $(O);)
	@$(foreach dir, $(SOURCEDIRS), cd $(dir) && $(SOURCEDIR)$(SOURCEDIR)/htmlpatch.csh && cd $(SOURCEDIR)$(SOURCEDIR);)
	$(foreach dir, $(SOURCEDIRS), cd $(dir) && rm $(BUILDDIR)/$(FONTDIR)/Lato-* $(BUILDDIR)/$(FONTDIR)/RobotoSlab-* -r $(BUILDDIR)/$(FONTDIR)/Lato/ $(BUILDDIR)/$(FONTDIR)/RobotoSlab/ && cd $(SOURCEDIR)$(SOURCEDIR);)
