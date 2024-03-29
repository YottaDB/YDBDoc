<!---
.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2017-2022 YottaDB LLC and/or its subsidiaries.#
.. # All rights reserved.                                        #
.. #                                                             #
.. # Portions Copyright (c) Fidelity National                    #
.. # Information Services, Inc. and/or its subsidiaries.         #
.. #                                                             #
.. #     This document contains the intellectual property        #
.. #     of its copyright holder(s), and is made available       #
.. #     under a license.  If you do not know the terms of       #
.. #     the license, please stop and do not read further.       #
.. #                                                             #
.. ###############################################################
-->

# YottaDB r1.10

# Release Note Revision History

Revision | Date | Summary
-|-|-
1.05 | October 27, 2017 | Updated tarballs with new ydbinstall.sh and configure scripts (address additional fallout because the installation automation could not be fully tested in advance), update sha256sums
1.04 | October 26, 2017 | Add example of installation with UTF-8 support
1.03 | October 26, 2017 | Update ydbinstall.sh for correct installation directory default (ydbinstall.sh could not be fully tested without an actual r1.10 release), rebuild tarballs, update sha256sums
1.02 | October 26, 2017 | Add sha256sum for ydbinstall.sh
1.01 | October 26, 2017 | Update tarballs, update sha256sums, correct date
1.00 | October 26, 2017 | r1.10 Initial release

# Contact Information

## YottaDB LLC

40 Lloyd Avenue, Suite 104
Malvern, PA 19355, USA
info@yottadb.com
+1 (610) 644-1898

## Support

### Customers

Contact your YottaDB support channel.

### Others

For free (to you) support from members of communities who run widely available applications on YottaDB, please use an application-specific list where appropriate.

* For issues specific to the use of YottaDB from node.js via [nodem](https://github.com/dlwicksell/nodem), [QewdJS](http://qewdjs.com) or [Enterprise Web Developer](http://ewdjs.com), post to the [Enterprise Web Developer community](http://groups.google.com/group/enterprise-web-developer-community).
* For issues specific to the use of YottaDB with [VistA](https://en.wikipedia.org/wiki/VistA) flavors, post to the [Hardhats](http://groups.google.com/group/hardhats) list.
* For issues specific to the use of YottaDB with M other than for applications above, post to the [comp.lang.mumps](http://groups.google.com/group/comp.lang.mumps) list.
* If you are not sure where to post, or for requests other than to the above communities, post an issue at https://github.com/YottaDB/YottaDB/issues and include the words "help wanted" in the summary.

# r1.10

## Overview

YottaDB r1.10 is a major release that adds support for Linux on ARM CPUs. Enhancements in this release include reverse $query() - $query(,-1) - as well as optimizations for certain $order() use cases. The `ydbinstall.sh` script automatically downloads and installs the current YottaDB release. As always, there as other enhancements and fixes, as noted below.

YottaDB r1.10 is built on (and except as noted, upward compatible with) both [YottaDB r1.00](https://github.com/YottaDB/YottaDB/releases/tag/r1.00) and [GT.M V6.3-002](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-002_Release_Notes.html#overview).

We would like to acknowledge and thank Steve Johnson (@sljohnson1) and Sam Habiel (@shabiel) for making the ARM port possible.

## Platforms

A *platform* is a combination of a CPU architecture and an operating system. A platform is Supported, Supportable, or Unsupported. *Supported* means that we have the platform in our development environment and test each release on that platform. *Supportable* means that although we do not necessarily have such a platform in our environment, we have no reason to believe that the software will not run on it. All others are *Unsupported*.

CPU Architecture | Supported OS Version(s) | Notes
-|-|-
64-bit x86 | Ubuntu 16.04 LTS; Red Hat Enterprise Linux 7.4 | Note that there are separate binary distributions for Ubuntu and Red Hat, owing to differences in library versions of those distributions.
Raspberry Pi 3 Model B; BeagleBone Black Wireless | Raspbian GNU/Linux 9.1; Stretch IoT (non GUI) |  While YottaDB r1.10 is Supportable on other ARMv7-A CPUs, owing to variations in the implementations of ARM microarchitectures, we recommend that you ensure the software runs correctly before committing to any specific hardware other than those Supported. Please contact info@yottadb.com if you want a specific combination of OS and CPU microarchitecture to be Supported.

Recent releases of major GNU/Linux distributions with contemporary kernels, glibc and ncurses are Supportable. Running on Arch Linux requires the ncurses5-compat-libs package to be installed.

## Installation

The simplest way to install the current release of YottaDB for your platform is the [ydbinstall.sh](https://github.com/YottaDB/YottaDB/blob/master/sr_unix/ydbinstall.sh) script.
 [Download it](https://raw.githubusercontent.com/YottaDB/YottaDB/master/sr_unix/ydbinstall.sh), make the file executable, and execute it:

-  `sudo ./ydbinstall.sh` downloads and installs the current YottaDB release with a reasonable set of defaults.
- `sudo ./ydbinstall.sh --utf8 default` downloads and installs the current YottaDB release with UTF-8 support, using the default ICU version on the operating system.
- `./ydbinstall.sh --help` prints a list of command line options.
- `sudo ./ydbinstall.sh --verbose` outputs information as it executes, for troubleshooting purposes (for example if Internet access is stalled by a proxy server, the script will print the `wget` command it is trying to execute and hang).

You can also download the YottaDB binary distribution tarball for your platform from the [release notes for the latest release](https://github.com/YottaDB/YottaDB/releases/latest), unpack it, change to the directory with the unpacked files, and (a) run the `ydbinstall` therein or (b) install the historical way per the instructions for installing GT.M in the Installation Procedure section of Chapter 2 (Installing GT.M) in the [GT.M Administration and Operations Guide](http://tinco.pair.com/bhaskar/gtm/doc/books/ao/UNIX_manual/index.html).

We **strongly recommend** that you install YottaDB r1.10 in a newly created directory, different from those of YottaDB r1.00 and any GT.M versions you may have installed on the system.

### Removing an installed YottaDB release

Assuming `$gtm_dist` points to the directory where YottaDB is installed:

- Cleanly shut down all application processes using that release.
- Execute `mupip rundown && mupip rundown -relinkctl`.
- Ensure that there are no`gtcm*` or `gtmsecshr` processes active.
- Use `sudo lsof | grep $gtm_dist` to ensure there are no open files.
- Delete the directory with `sudo rm -rf $gtm_dist`.

## Upgrading to YottaDB r1.10

As YottaDB r1.10 is upward compatible from both YottaDB r1.00 and GT.M V6.3-002, the minimal upgrade steps are:

- Install YottaDB r1.10.
- Recompile any object code, and recreate shared libraries where appropriate.
- If you are using encryption, compile and install the reference implementation plugin or your customized plugin.
- Cleanly shut down the application and ensure that the database files are shut down using mupip rundown from the prior release.
- Switch journal files with the new YottaDB release.
- Start using the new YottaDB release.

To upgrade from older GT.M releases, first upgrade to GT.M V6.0-000 or later and follow the steps above, or contact your YottaDB support channel for assistance and guidance.

A more sophisticated upgrade technique is:

- Install YottaDB r1.10.
- Create a new replicated instance of your application (on the same system or a different system).
- Assuming the existing instance is A, and the new instance is B, start replicating from A to B.
- Once B catches up, switchover so that B is in a primary role replicating to A.
- Once you are satisfied with B, remove (or upgrade) A.

# Change History

## r1.10

YottaDB r1.10 includes the following changes from [YottaDB r1.00](https://github.com/YottaDB/YottaDB/releases/tag/r1.00).

Id | Category | Summary
-|-|-
[YDB#8](https://gitlab.com/YottaDB/DB/YDB/-/issues/8) | Other | gtmgblstat.xc in binary distribution package
[YDB#10](https://gitlab.com/YottaDB/DB/YDB/-/issues/10) | Language | Support reverse $query for globals and locals
[YDB#11](https://gitlab.com/YottaDB/DB/YDB/-/issues/11) | Database | Speed up $order(gvn,-1) where gvn is of the form ^xxx("")
[YDB#12](https://gitlab.com/YottaDB/DB/YDB/-/issues/12) | Database | Repeated calls to $order(^xxx("")) run faster
[YDB#19](https://gitlab.com/YottaDB/DB/YDB/-/issues/19) | Other | gtmprofile sets "YDB>" as default prompt
[YDB#25](https://gitlab.com/YottaDB/DB/YDB/-/issues/25) | Language | Issue MAXSTRLEN error if length of $query(lvn) exceeds maximum string length
[YDB#30](https://gitlab.com/YottaDB/DB/YDB/-/issues/30) | Operations | Source server behaves correctly (no SIG-11) if started with an external filter followed by a deactivate and shutdown
[YDB#32](https://gitlab.com/YottaDB/DB/YDB/-/issues/32) | Language | Improve efficiency of C to M calls
[YDB#44](https://gitlab.com/YottaDB/DB/YDB/-/issues/44) | Utilities | %HD works on hexadecimal numbers with a 0x or 0X prefix
[YDB#47](https://gitlab.com/YottaDB/DB/YDB/-/issues/47) | Database | OFRZAUTOREL message includes full path of database file
[YDB#61](https://gitlab.com/YottaDB/DB/YDB/-/issues/61) | Other | Port to Linux on ARM
[YDB#66](https://gitlab.com/YottaDB/DB/YDB/-/issues/66) | Other | Make it easier to install YottaDB from a source or binary tarball

In addition to the changes described above, YottaDB r1.10 includes [changes released in GT.M V6.3-002](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-002_Release_Notes.html#GTMtaV63-002ble) with [one exception](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-002_Release_Notes.html#GTM-8726).

# More Information

(Section blank for this release)

# Messages

## OFRZAUTOREL

*This is an existing message with updated text. The Action remains unchanged.*

OFRZAUTOREL, Online Freeze automatically released for database file aaaa

Operator log Warning: A process needed to modify the database file aaaa, which had an Online Freeze, but with AutoRelease selected. The process continued normally, modifying the file.

Action: Discard any database copy or snapshot made after the Online Freeze, as its contents are suspect. Perform a MUPIP FREEZE -OFF to clean up the prior Online Freeze. If the AutoRelease behavior is not desired, try again with MUPIP FREEZE -ON -ONLINE -NOAUTORELEASE. If the cause of the AutoRelease is unclear, report this and the accompanying ERRCALL message to your YottaDB support channel.

## QUERY2

QUERY2, Invalid second argument to $QUERY. Must be -1 or 1

Run Time Error: This indicates that the second argument to a $QUERY function was not a 1 or -1, which are the only permitted values.

Action: Modify the application code to provide a correct value.

## From GT.M

In addition to the error messages added by YottaDB, [error messages from GT.M V6.3-002](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-002_Release_Notes.html#idp145262752) are also present in YottaDB r1.10.

# Legal Stuff

Copyright © 2017 YottaDB LLC

Permission is granted to copy, distribute and/or modify this document under the terms of the [GNU Free Documentation License, Version 1.3](http://www.gnu.org/licenses/fdl.txt) or any later version published by the Free Software Foundation; with no Invariant Sections, no Front-Cover Texts and no Back-Cover Texts.

YottaDB™ is a trademark of YottaDB LLC.
GT.M™ is a trademark of Fidelity National Information Services, Inc.
Other trademarks belong to their respective owners.

This document contains a description of YottaDB and the operating instructions pertaining to the various functions that comprise the software. This document does not contain any commitment of YottaDB LLC. YottaDB LLC believes the information in this publication is accurate as of its publication date; such information is subject to change without notice. YottaDB LLC is not responsible for any errors or defects.
