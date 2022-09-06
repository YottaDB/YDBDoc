<!---
.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2022 YottaDB LLC and/or its subsidiaries.     #
.. # All rights reserved.                                        #
.. #                                                             #
.. #     This document contains the intellectual property        #
.. #     of its copyright holder(s), and is made available       #
.. #     under a license.  If you do not know the terms of       #
.. #     the license, please stop and do not read further.       #
.. #                                                             #
.. ###############################################################
-->

# YottaDB r1.00

# Release Note Revision History

Revision | Date | Summary
-|-|-
1.00 | July 4, 2017 | r1.00 - Initial release

# Contact Information

YottaDB LLC | Support
-|-
40 Lloyd Avenue, Suite 104 | Customers: Contact your YottaDB support channel
Malvern, PA 19355, USA | All others: See below

For free (to you) support from members of communities who run widely available applications on YottaDB, please use an application-specific list where appropriate.
* For issues specific to the use of YottaDB with [QewdJS](http://qewdjs.com) or [Enterprise Web Developer](http://ewdjs.com), post to the [Enterprise Web Developer community](http://groups.google.com/group/enterprise-web-developer-community).
* For issues specific to the use of YottaDB with [VistA](https://en.wikipedia.org/wiki/VistA) flavors, post to the [Hardhats](http://groups.google.com/group/hardhats) list.
* If you are not sure where to post, or for requests other than to the above communities, post an issue at https://github.com/YottaDB/YottaDB/issues and include the words "help wanted" in the summary.

# r1.00

## Overview

YottaDB r1.00 is the first YottaDB release, intended to validates the development processes of production grade releases of YottaDB LLC. Each YottaDB release is built on a GT.M release, and on the preceding YottaDB release (this first release of course has no preceding YottaDB release). Except for bugs and fixes, which by their nature are not upward compatible, our intent is for each YottaDB release to be functionally and operationally upward compatible with both its YottaDB and GT.M predecessors. The release notes call your attention to any changes that we think you should review with possible compatibility issues in mind.

## Platforms

A *platform* is a combination of a CPU architecture and an operating system. A platform is Supported, Supportable, or Unsupported. *Supported* means that we have the platform in our development environment and test each release on that platform. *Supportable* means that although we do not necessarily have such a platform in our environment, we have no reason to believe that the software will not run on it. All others are *Unsupported*.

Platform | Supported Version(s) | Notes
-|-|-
x86 GNU/Linux | Ubuntu 16.04 LTS | Recent releases of major Linux distributions (including Red Hat Enterprise Linux 7.x) with contemporary versions of kernels, glibc and ncurses are Supportable. Running on Arch Linux requires the ncurses5-compat-libs package to be installed.

## Installation

Follow the instructions for installing GT.M in the Installation Procedure section of Chapter 2 (Installing GT.M) in the [GT.M Administration and Operations Guide](http://tinco.pair.com/bhaskar/gtm/doc/books/ao/UNIX_manual/index.html).

## Upgrading to YottaDB r1.00

As YottaDB r1.00 is an upward compatible alternative to GT.M V6.3-001A, install YottaDB and follow the instructions in the [GT.M V6.3-001A Release Notes](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-001_Release_Notes.html).

# Change History

## r1.00

Id | Category | Summary
-|-|-
[YDB#2](https://gitlab.com/YottaDB/DB/YDB/-/issues/2) | Language | A way for application code to determine whether it is running on YottaDB or GT.M
[YDB#3](https://gitlab.com/YottaDB/DB/YDB/-/issues/3) | Language | Two argument $[z]length to use $[z]piece cache
[YDB#4](https://gitlab.com/YottaDB/DB/YDB/-/issues/4) | Other | Change default direct mode prompt to make it easier for users to know whether they're interacting with YottaDB or GT.M

# More Information

(Section blank for this release)

# Messages

(Section blank for this release)

# Legal Stuff

Copyright © 2017 YottaDB LLC

Permission is granted to copy, distribute and/or modify this document under the terms of the [GNU Free Documentation License, Version 1.3](http://www.gnu.org/licenses/fdl.txt) or any later version published by the Free Software Foundation; with no Invariant Sections, no Front-Cover Texts and no Back-Cover Texts.

YottaDB™ is a trademark of YottaDB LLC.
GT.M™ is a trademark of Fidelity National Information Services, Inc.
Other trademarks belong to their respective owners.

This document contains a description of YottaDB and the operating instructions pertaining to the various functions that comprise the software. This document does not contain any commitment of YottaDB LLC. YottaDB LLC believes the information in this publication is accurate as of its publication date; such information is subject to change without notice. YottaDB LLC is not responsible for any errors or defects.