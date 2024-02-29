<!---
.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2019-2022 YottaDB LLC and/or its subsidiaries.#
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

# YottaDB r1.28

## Release Note Revision History

| Revision | Date | Summary |
| ---------| ---- | ------- |
| 1.00     | September 11, 2019 | r1.28 Initial Release
| 1.01     | March 31, 2020     | Added CentOS 8 Build |

## Contact Information

### YottaDB LLC

40 Lloyd Avenue, Suite 104
Malvern, PA 19355, USA
info@yottadb.com
+1 (610) 644-1898

### Support

#### Customers

Contact your YottaDB support channel.

#### Others

For free (to you) support from members of communities who run widely available applications on YottaDB, please use an application-specific list where appropriate.

* For requests other than to the communities below, post an Issue at [https://gitlab.com/YottaDB/DB/YDB/issues](https://gitlab.com/YottaDB/DB/YDB/issues) and include the words "Help Wanted" in the summary.

* For requests specific to the use of YottaDB from node.js via [nodem](https://github.com/dlwicksell/nodem), [QewdJS](http://qewdjs.com/) or [Enterprise Web Developer](http://ewdjs.com/), post to the [Enterprise Web Developer community](http://groups.google.com/group/enterprise-web-developer-community).

* For requests specific to the use of YottaDB with [VistA](https://en.wikipedia.org/wiki/VistA) flavors, post to the [Hardhats](http://groups.google.com/group/hardhats) list.

* For requests specific to the use of YottaDB with M other than for applications above, post to the [comp.lang.mumps](http://groups.google.com/group/comp.lang.mumps) list.

## r1.28

### Overview

For users of YottaDB using the [Go API](https://docs.yottadb.com/MultiLangProgGuide/goprogram.html), r1.28 is a major release because it is required for production grade access to YottaDB from [Go](https://golang.org). For others, it is a minor release with a small set of enhancements anf fixes as detailed in the [complete release notes](https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.28).

YottadB r1.28 is upward compatible with [YottaDB r1.26](https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.26), and thus upward compatible with [GT.M V6.3-006](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-006_Release_Notes.html), and [GT.M V6.3-007](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-007_Release_Notes.html).

### Platforms

A platform is a combination of a CPU architecture and an operating system. A platform is Supported, Supportable, or Unsupported. Supported means that we have the platform in our development environment and test each release on that platform. Supportable means that although we do not necessarily have such a platform in our environment, we have no reason to believe that the software will not run on it. All others are Unsupported.

| CPU Architecture                    | Supported OS Version(s)                                                      | Notes |
| ----------------------------------- | ---------------------------------------------------------------------------- | ----------------------------------------------------------------------------------- |
| 64-bit x86                          | Ubuntu 18.04 LTS; Red Hat Enterprise Linux 7.6; Debian GNU/Linux 10 (Buster) | There are separate binary distributions for each OS version, owing to differences in library versions of those distributions. |
| 64-bit ARM (Raspberry Pi 3 Model B) | Ubuntu 18.04 LTS                                                             | While YottaDB is Supportable on other [ARMv8-A CPUs](https://en.wikipedia.org/wiki/Comparison_of_ARMv8-A_cores), owing to variations in the implementations of ARM microarchitectures, we recommend that you ensure the software runs correctly before committing to any specific hardware other than those Supported. Please contact info@yottadb.com if you want a specific combination of OS and CPU microarchitecture to be Supported. |
| 32-bit ARM (Raspberry Pi 3 Model B) | Raspbian GNU/Linux 10 (Buster)                                | While YottaDB is Supportable on other [ARMv7-A CPUs](https://en.wikipedia.org/wiki/Comparison_of_ARMv7-A_cores), owing to variations in the implementations of ARM microarchitectures, we recommend that you ensure the software runs correctly before committing to any specific hardware other than those Supported. Please contact info@yottadb.com if you want a specific combination of OS and CPU microarchitecture to be Supported. |
| 32-bit ARM (Raspberry Pi Zero)      | Raspbian GNU/Linux 9.1                                                       | While YottaDB is Supportable on other ARMv6 CPUs, owing to variations in the implementations of ARM microarchitectures, we recommend that you ensure the software runs correctly before committing to any specific hardware other than those Supported. Please contact info@yottadb.com if you want a specific combination of OS and CPU microarchitecture to be Supported. |

Recent releases of major GNU/Linux distributions with contemporary kernels, glibc and ncurses are Supportable. Specific notes:

- On Ubuntu releases after 18.04 LTS, YottaDB needs the libtinfo5 package to be installed.
- On [Arch Linux](https://www.archlinux.org/) and other leading edge distributions, YottaDB may need to be recompiled from source code owing to library and tool chain versions significantly newer than those used in building the distribution.

### Installation

See our [Get Started page](https://yottadb.com/product/get-started) to use YottaDB.

We **strongly recommend** that you install YottaDB r1.28 in a newly created directory, different from those of YottaDB r1.26 and any GT.M versions you may have installed on the system.

### Removing an installed YottaDB release

Assuming $ydb\_dist points to the directory where YottaDB is installed:

* Cleanly shut down all application processes using that release.
* Execute `mupip rundown && mupip rundown -relinkctl`
* Ensure that there are no gtcm\* or gtmsecshr processes active.
* Use `sudo lsof | grep $ydb_dist` to ensure there are no open files.
* Delete the directory with `sudo rm -rf $ydb_dist`

## Upgrading to YottaDB r1.28

As YottaDB r1.28 is upward compatible from YottaDB r1.26, GT.M V6.3-006 and GT.M V6.3-007, the minimal upgrade steps are:

* Install YottaDB r1.28.
* Install plugins you use.
* Recompile object code, and recreate shared libraries where appropriate.
* If you are using encryption, compile and install the reference implementation plugin or your customized plugin.
* Cleanly shut down the application and ensure that the database files are shut down using mupip rundown from the prior release.
* Switch journal files with the new YottaDB release.
* Start using the new YottaDB release.

If the database has triggers defined with non-canonical numbers, or numbers specified as strings with any version prior to r1.26, or if you are unsure, extract the trigger definitions, delete existing triggers, and reload the trigger definitions. Issue [#430] from r1.26 has a series of steps you can copy and execute.

To upgrade from older GT.M releases, first upgrade to GT.M V6.0-000 or later and follow the steps above, or contact your YottaDB support channel for assistance and guidance.

A more sophisticated upgrade technique is:

* Install YottaDB r1.28.
* Create a new replicated instance of your application (on the same system or a different system).
* Assuming the existing instance is A, and the new instance is B, upgrade B to r1.28 and start replicating from A to B.
* Once B catches up, switchover so that B is in a primary role replicating to A.
* Once you are satisfied with B, remove (or upgrade) A.

## Change History

### r1.28

YottaDB r1.28 includes the following changes from [YottaDB r1.26](https://github.com/YottaDB/YottaDB/releases/tag/r1.26).

| ID           | Category                            | Summary                                                                                                                                |
| ------------ | ----------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------- |
| ([469](#x469))       | Language                            | $FNUMBER() formatting code `"."` facilitates number formatting                                                                         |
| ([471](#x471))       | Other                               | Sourcing ydb\_env\_set and ydb\_env\_unset provides more information in the event of an error                                          |
| ([472](#x472))       | Language                            | ydb\_data\_s()/ydb\_data\_st() return YDB\_DATA\_ERROR in case of error                                                                |
| ([475](#x475))       | Language                            | ydb\_lock\_incr\_s(), ydb\_lock\_decr\_s(), and ydb\_lock\_s() check for too many subscripts                                           |
| ([477](#x477))       | Language                            | NEW accepts $TEST as an argument                                                                                                       |
| ([480](#x480))       | Language                            | Incrementing a variable whose initial value is zero or undefined with a seven or more digit increment returns the correct string value |

### Database

### Language

* <a name="x469"></a> To facilitate application internationalization, $FNUMBER() accepts the following formatting code (i.e., as the second argument):

  - `"."`: inserts periods (`"."`) every third position to the left of the decimal within the number and uses a comma (`","`) as the decimal separator.

  [YDB#469](https://gitlab.com/YottaDB/DB/YDB/-/issues/469)

* <a name="x472"></a> When ydb\_data\_s() or ydb\_data\_st() get an error, the return value in *ret\_value is set to YDB\_DATA\_ERROR. Previously the return value was zero which is a valid return value. [YDB#472](https://gitlab.com/YottaDB/DB/YDB/-/issues/472)

* <a name="x475"></a> ydb\_lock\_incr\_s(), ydb\_lock\_decr\_s(), and ydb\_lock\_s() validate that the number of subscripts provided does not exceed the maximum. Previously specifying too many subscripts resulted in uninformative assert failures and buffer overflows. [YDB#475](https://gitlab.com/YottaDB/DB/YDB/-/issues/475)

* <a name="x477"></a> The NEW command accepts $TEST as an argument. Previously, it did not. YottaDB would like to thank [SP.ARM](https://sparm.com) for contributing this enhancement. [YDB#477](https://gitlab.com/YottaDB/DB/YDB/-/issues/477)

* <a name="x480"></a> Using $INCREMENT(), ydb\_incr\_s(), or ydb\_incr\_st() to increment a variable whose initial value is zero or undefined with an increment of seven digits or more returns the correct string value. Previously, while the numeric value of the result was correct, the string value was the empty string (`""`). Depending on how the result was used, it could have been wrong. [YDB#480](https://gitlab.com/YottaDB/DB/YDB/-/issues/480)

### System Administration

### Other

* <a name="x471"></a> With the %YDBENV program (invoked while sourcing the `ydb_env_set` and `ydb_env_unset` files), the error output includes the output of `ZSHOW "*"` in addition to the single line error message. Previously, it was only the single line error message, which provided less information when troubleshooting. [YDB#471](https://gitlab.com/YottaDB/DB/YDB/-/issues/471)

## More Information

## Error Messages

## Legal Stuff

Copyright © 2019 YottaDB LLC

Permission is granted to copy, distribute and/or modify this document under the terms of the [GNU Free Documentation License, Version 1.3](http://www.gnu.org/licenses/fdl.txt) or any later version published by the Free Software Foundation; with no Invariant Sections, no Front-Cover Texts and no Back-Cover Texts.

YottaDB® and Octo® are registered trademarks of YottaDB LLC.
GT.M™ is a trademark of Fidelity National Information Services, Inc.
Other trademarks belong to their respective owners.

This document contains a description of YottaDB and the operating instructions pertaining to the various functions that comprise the software. This document does not contain any commitment of YottaDB LLC. YottaDB LLC believes the information in this publication is accurate as of its publication date; such information is subject to change without notice. YottaDB LLC is not responsible for any errors or defects.
