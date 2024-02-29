<!---
.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2023 YottaDB LLC and/or its subsidiaries.     #
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

# YottaDB r1.38

### Release Note Revision History

| Revision  | Date              | Summary               |
| --------- | ----------------- | --------------------- |
| 1.00      |  April 3, 2023 | r1.38 Initial Release |

### Contact Information
#### YottaDB LLC
https://yottadb.com /  <info@yottadb.com>

#### Support
##### Customers
Contact YottaDB or your YottaDB support channel for support with assured service levels from knowledgeable staff.

##### Others
For free (to you) support from members of communities who run widely available applications on YottaDB, please use an application-specific list where appropriate.

 - For requests other than to the communities below, [post an Issue](https://gitlab.com/YottaDB/DB/YDB/-/issues) and include the words "Help Wanted" in the Title.
 - For access from node.js via [Nodem](https://github.com/dlwicksell/nodem), [post an Issue on the Nodem project](https://github.com/dlwicksell/nodem/issues/new/).
 - For access from [QewdJS](http://qewdjs.com/), or [EWD.js](https://github.com/robtweed/ewd.js), or from node.js via [mg-dbx](https://github.com/chrisemunt/mg-dbx) post to the [Enterprise Web Developer community](https://groups.google.com/forum/#!forum/enterprise-web-developer-community).
 - For requests specific to the use of YottaDB with [VistA](https://en.wikipedia.org/wiki/VistA) flavors, post to the [Hardhats](https://groups.google.com/forum/#!forum/hardhats) list.
 - For requests specific to the use of YottaDB with M other than for applications above, post to the [comp.lang.mumps](https://groups.google.com/forum/#!forum/comp.lang.mumps) list.

### r1.38

#### Overview
YottaDB r1.38 is a minor release that includes functionality needed by a customer on short notice.

* A MUPIP REPLICATE option provides for a replication stream to include updates made by triggers on the source instance. ([722](#x722))
* $ZPEEK() and \^%PEEKBYNAME() provide direct access to an additional process-private structure. [[986](#x986))
* The `-gui` option of `ydbinstall` / `ydbinstall.sh` installs the [YottaDB GUI](https://gitlab.com/YottaDB/UI/YDBGUI). ([988](#x988))
* Changes to the subscripts of \^ydbJNLFRECTBL and more meaningul SQL column names make %YDBJNLF more useful. ([990](#x990))

The %YDBJNLF utility program, which was released as field-test software in r1.36, is considered Supported for production use in r1.38. Also, the Supported level of Ubuntu on x86_64 moves up from 20.04 LTS to 22.04 LTS.

While all YottDB software is free to use under our free / open-source software licensing, r1.38 illustrates the value of being a YottaDB customer rather than a user: in addition to support with assured service levels, we will work with you to prioritize enhancements and fixes you need.

As with all YottaDB releases, there are a number of fixes and smaller enhancements, as described in the [Change History](#change-history).

#### Platforms
A platform is a combination of a CPU architecture and an operating system. A platform is Supported, Supportable, or Unsupported.

* Supported means that we have the platform in our development environment and test each release on that platform.
* Supportable means that although we do not necessarily have such a platform in our environment, we have no reason to believe that the software will not run on it.
* All others are Unsupported.

| CPU Architecture                    | Supported OS Version(s)                                                              | Notes                                                                                                                         |
| ----------------------------------- | ------------------------------------------------------------------------------------ | ----------------------------------------------------------------------------------------------------------------------------- |
| 64-bit x86                          | Ubuntu 22.04 LTS; Red Hat Enterprise Linux 8.x; SUSE Linux Enterprise 15.x; Debian GNU/Linux 11 (Bullseye) | There are separate binary distributions for each OS version, owing to differences in library versions of those distributions. |
| 64-bit ARM (Raspberry Pi 3 Model B) | Debian GNU/Linux 11 (Bullseye)                                                       | See below.                                                                                                                    |
| 32-bit ARM (Raspberry Pi Zero)      | Debian GNU/Linux 11 (Bullseye)                                                       | See below.                                                                                                                    |

Recent releases of major GNU/Linux distributions with contemporary kernels, glibc and ncurses are Supportable. Specific notes:

- [ydbinstall.sh](https://gitlab.com/YottaDB/DB/YDB/-/blob/master/sr_unix/ydbinstall.sh) recognizes [Rocky Linux](https://rockylinux.org/) 8 as equivalent to RHEL 8, and [OpenSUSE Leap](https://www.opensuse.org/#Leap) 15.x as equivalent to SUSE Linux Enterprise 15.x, and installs the corresponding versions. Note that Rocky Linux and OpenSUSE Leap are Supportable, not Supported.
- On [Arch Linux](https://www.archlinux.org/) and other leading edge distributions such as OpenSUSE Tumbleweed, YottaDB will likely need to be recompiled from source code owing to library and tool chain versions newer than those used in building Supported distributions. The `--from-source` option of [ydbinstall.sh](https://gitlab.com/YottaDB/DB/YDB/-/blob/master/sr_unix/ydbinstall.sh) simplifies the process. ([754](https://gitlab.com/YottaDB/DB/YDB/-/issues/754))
- While YottaDB is Supportable on other ARMv6, [ARMv7-A](https://en.wikipedia.org/wiki/Comparison_of_ARMv7-A_cores), and [ARMv8-A](https://en.wikipedia.org/wiki/Comparison_of_ARMv8-A_cores) CPUs, owing to variations in the implementations of ARM microarchitectures, we recommend that you ensure the software runs correctly before committing to any specific hardware other than those listed above. Please contact <info@yottadb.com> if you want a specific combination of OS and CPU microarchitecture to be Supported.

#### Installation
See our [Get Started page](https://yottadb.com/product/get-started) to use YottaDB.

We **strongly recommend** that you install YottaDB r1.38 in a newly created directory, different from those of prior YottaDB releases and any GT.M versions you may have installed on the system.

#### Removing an installed YottaDB release
Assuming `$ydb_dist` points to the directory where YottaDB is installed:

 - Cleanly shut down all application processes using that release.
 - Execute `mupip rundown && mupip rundown -relinkctl`
 - Ensure that there are no `gtcm*` or `gtmsecshr` processes active.
 - Use `sudo lsof | grep $ydb_dist` to ensure there are no open files.
 - Delete the directory with `sudo rm -rf $ydb_dist`

### Upgrading to YottaDB r1.38
As YottaDB r1.38 is upward compatible from YottaDB [r1.36](https://gitlab.com/YottaDB/DB/YDB/-/releases/r1.36). The minimal upgrade steps are:

* Install YottaDB r1.38. Use `ydbinstall` / `ydbinstall.sh` to install plugins you use.
* Install plugins you need in addition to those installed in the previous step.
* Recompile object code, and recreate shared libraries if your application uses shared libraries.
* If your application uses encryption, compile and install the reference implementation plugin (if not done by the `ydbinstall` / `ydbinstall.sh` script) or your customized plugin.
* Cleanly shut down the application and ensure that the database files are shut down using MUPIP RUNDOWN from the prior release.
* Switch journal files with the new YottaDB release.
* Start using the new YottaDB release.

If the database has triggers defined with non-canonical numbers, or numbers specified as strings with any version prior to [r1.28](https://gitlab.com/YottaDB/DB/YDB/-/releases/r1.28), or if you are unsure, extract the trigger definitions, delete existing triggers, and reload the trigger definitions. Issue [[#430](https://gitlab.com/YottaDB/DB/YDB/-/issues/430)] from [r1.28](https://gitlab.com/YottaDB/DB/YDB/-/releases/r1.28) has a series of steps you can copy and execute. There is no need to do this if upgrading from [r1.28](https://gitlab.com/YottaDB/DB/YDB/-/releases/r1.28) or later.

To upgrade from older GT.M releases, first upgrade to GT.M V6.0-000 or later and follow the steps above, or contact your YottaDB support channel for assistance and guidance.

A more sophisticated upgrade technique is:

* Install YottaDB r1.38.
* Create a new replicated instance of your application (on the same system or a different system).
* Assuming the existing instance is A, and the new instance is B, upgrade B to r1.38 and start replicating from A to B.
* Once B catches up, switchover so that B is in a primary role replicating to A.
* Once you are satisfied with B, remove (or upgrade) A.

### Change History

#### r1.38

YottaDB r1.38 includes the following enhancements and fixes beyond YottaDB [r1.36](https://gitlab.com/YottaDB/DB/YDB/-/releases/r1.36).

| ID             | Category              | Summary                                                                                             |
|----------------|-----------------------|-----------------------------------------------------------------------------------------------------|
| ([722](#x722)) | Database              | MUPIP REPLICATE option to replicate trigger updates                                                 |
| ([964](#x964)) | Database              | Fix bugs exposed by fuzz testing in YottaDB r1.38                                                   |
| ([966](#x966)) | Other                 | %YDBJNLF ingests journal extracts with transaction records as well as long records                  |
| ([969](#x969)) | System Administration | When `--utf8` is not specified `ydbinstall` does not check for `libicu.io`                          |
| ([979](#x979)) | Database              | MUPIP FTOK JNLPOOL correctly reports the FTOK Key and FileId columns for replication instance files |
| ([980](#x980)) | Other                 | %YDBJNLF ingests journal files whose database files do not exist                                    |
| ([985](#x985)) | Other                 | GT.CM works correctly in Ubuntu/Debian docker images                                                |
| ([986](#x986)) | System Administration | Add UDI (`unix_db_info` structure) access to $ZPEEK()/\^%PEEKBYNAME()                               |
| ([988](#x988)) | System Administration | `ydbinstall`/`ydbinstall.sh` option to install GUI                                                  |
| ([990](#x990)) | Other                 | More useful subscript for \^%ydbJNLFRECTBL and more meaningful SQL column name                      |
| ([995](#x995)) | Languages             | CINOENTRY error includes the path of the call-in table file                                         |

#### Database

* <a name="x722"></a> With the `-trigupdate` option of the [MUPIP REPLICATE SOURCE START](https://docs.yottadb.com/AdminOpsGuide/dbrepl.html#starting-the-source-server) command, database updates made by triggers are included in the replication stream. By default they are not. Previously, there was no mechanism to replicate database updates made by triggers. Also, the [-zerobacklog](https://docs.yottadb.com/AdminOpsGuide/dbrepl.html#zerobacklog) option requires the [-shutdown](https://docs.yottadb.com/AdminOpsGuide/dbrepl.html#shutdown) option to be specified, since the former is not meaningful without the latter, issuing a [CLIERR](https://docs.yottadb.com/MessageRecovery/errors.html#clierr) error otherwise. Previously, MUPIP REPLICATE accepted the `-zerobacklog` option without `-shutdown` which is meaningless.

  For more details, refer to the [Additional Information](#additional-information) section. [[#722](https://gitlab.com/YottaDB/DB/YDB/~/issues/722)]

* <a name="x964"></a> As described in [Fuzz Testing YottaDB](https://yottadb.com/fuzz-testing-yottadb/), [this Issue](#964) captures bugs found by Fuzz Testing and fixed in r1.38. [[#964](https://gitlab.com/YottaDB/DB/YDB/-/issues/964)]

* <a name="x979"></a> `mupip ftok -jnlpool` correctly reports the `FTOK Key` and `FileId` columns for replication instance files. Previously, in YottaDB r1.36, these columns used to hold incorrect values for those instance file names which were not in the current directory. Additionally, `mupip ftok` works correctly on database file names whose absolute path is longer than 255 characters. Previously, in YottaDB r1.36, it would use a truncated file name resulting in incorrect output. [[#979](https://gitlab.com/YottaDB/DB/YDB/~/issues/979)]

#### Languages

* <a name="x995"></a> The [CINOENTRY](https://docs.yottadb.com/MessageRecovery/errors.html#cinoentry) error message includes the full path of the [call-in table](https://docs.yottadb.com/ProgrammersGuide/extrout.html#call-in-table) file where the C function name was not found. Previously it only included the C function name which was not as helpful since it was not clear which `.ci` file YottaDB was using to access the function. [[#995](https://gitlab.com/YottaDB/DB/YDB/~/issues/995)]

#### System Administration

* <a name="x969"></a> When the `--utf8` command line option is not specified `ydbinstall` / `ydbinstall.sh` does not check to ensure that `libicu.io` is installed on the system. Effective r1.36, it checked, and if `libicuio.so` was not found, it reported an error and exited without installing YottaDB. [[#969](https://gitlab.com/YottaDB/DB/YDB/~/issues/969)]

* <a name="x986"></a> For [$ZPEEK()](https://docs.yottadb.com/ProgrammersGuide/functions.html#zpeek), there is a new mnemonic to access additional region related information:

  UDI[REG]:region - returns a value from the `unix_db_info` (process private) control block. Takes a case independent region name as an argument.

  See the `$ydb_dist/GTMDefinedTypesInit.m` for `unix_db_info` structure fields. Note the largest part of this structure is `s_addrs` which is the same as the `CSAREG:` mnemonic but there are fields in `s_addrs` that are unique, for example the `ftok_semid` field which contains the `semid` of the FTOK semaphore used only at process startup and rundown and which has the FTOK id of the database for key. There is also a semaphore which is unique to each database that has 0 for a `keyid`.

  For [$$\^%PEEKBYNAME()](https://docs.yottadb.com/ProgrammersGuide/utility.html#peekbyname), this adds fields `unix_db_info.*`.

  Previously, this information required parsing the output of [MUPIP FTOK](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#ftok). Note that \$ZPEEK() is likely to be of interest primarily to developers; most users will find $$\^%PEEKBYNAME() to be more useful. The most useful field, `$$^%PEEKBYNAME("unix_db_info.ftok_semid","DEFAULT")` returns the ftok semaphore id for the DEFAULT region. [[#986](https://gitlab.com/YottaDB/DB/YDB/~/issues/986)]

* <a name="x988"></a> The `--gui` option of the [ydbinstall.sh](https://download.yottadb.com/ydbinstall.sh) / ydbinstall script, downloads and installs the [YottaDB GUI](https://gitlab.com/YottaDB/UI/YDBGUI/). [[#988](https://gitlab.com/YottaDB/DB/YDB/~/issues/988)]

#### Other

* <a name="x966"></a> %YDBJNLF ingests journal extracts with transaction processing records as well as long records. Previously, it issued errors for TSTART records as well as for records longer than 32767 bytes. Note that journal files do not contain TSTART records to indicate the beginning of a transaction, but have TSET, TKILL, etc. records to indicate the first update of a transaction; MUPIP JOURNAL EXTRACT displays TSTART records to make the extracts easier for human beings to read. The ingested data reflects the actual journal record of the first journal record of a transaction, and not the TSTART markers in the MUPIP JOURNAL EXTRACT output. [[#966](https://gitlab.com/YottaDB/DB/YDB/~/issues/966)]

* <a name="x980"></a> %YDBJNLF sets [ydb\_extract\_nocol](https://docs.yottadb.com/AdminOpsGuide/basicops.html#ydb-extract-nocol) when ingesting journal files. Previously it terminated with an error when the corresponding database files did not exist on the system, a likely scenario in applications such as forensics. Refer to the Note in the [ydb\_extract\_nocol documentation](https://docs.yottadb.com/AdminOpsGuide/basicops.html#ydb-extract-nocol) if the database region uses custom collation and the database file is not accessible to MUPIP JOURNAL EXTRACT. [[#980](https://gitlab.com/YottaDB/DB/YDB/~/issues/980)]

* <a name="x985"></a> GT.CM works correctly in Ubuntu/Debian docker images. Previously, it did not. [[#985](https://gitlab.com/YottaDB/DB/YDB/~/issues/985)]

* <a name="x990"></a> The first subscript of \^%ydbJNLFRECTBL, which identifies the SQL table in which each journal record type appears, is the journal format (currently 44). Previously, it was the label from calls to INGEST(), which resulted in redundant table rows or trees. The SQL column name is `tbltype` which is more meaningful than the previous name `tbl`. Note that this change is not backward compatible; however, %YDBJNLF in r1.36 was released as field test software whose API was subject to improvement. [[#990](https://gitlab.com/YottaDB/DB/YDB/~/issues/990)]

### Additional Information

With the `-trigupdate` option of the [MUPIP REPLICATE SOURCE START](https://docs.yottadb.com/AdminOpsGuide/dbrepl.html#starting-the-source-server) command:

- An instance can have multiple Source Servers, some started with the `-trigupdate` option and some without it, depending on the type of replication needed by each connection.
- A Source Server started with the `-trigupdate` option does not replicate trigger definition changes made by [MUPIP TRIGGER](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#trigger) and [$ZTRIGGER()](https://docs.yottadb.com/ProgrammersGuide/functions.html#ztrigger). Having the same trigger definitions on both sides of a replication connection would at best result in duplication of updates, and in the worst case cause anomalies where updates on one side conflict with updates on the other side.
- While a Source Server started with the `-trigupdate` option replicates updates made by the [ZTRIGGER](https://docs.yottadb.com/ProgrammersGuide/commands.html#ztrigger) command, it does not replicate the record for the ZTRIGGER command itself, as it would otherwise cause triggers to execute on the replicating secondary instance.
- The `-trigupdate` option requires the [-secondary](https://docs.yottadb.com/AdminOpsGuide/dbrepl.html#secondary-hostname-port) option, i.e., only the initial startup of an active Source Server, or switching a passive Source Server to an active role support the `-trigupdate` option.
- When switching an instance from a primary role to a secondary role where it will receive database updates generated by triggers, trigger definitions should be removed (see below) before starting replication, to avoid duplication and anomalies. Conversely, when switching an instance without triggers from a secondary role to a primary role, triggers need to be installed before it comes up in a primary role.
- When creating a new replicating secondary instance from a backup of another instance, with the intention of having the new instance replicated to by a Source Server using the `-trigupdate` option, we recommend removing triggers before bringing it online, and at least, reviewing trigger definitions for those that can result in redundant or conflicting updates.
- To remove triggers, turn replication off, remove triggers, and turn replication back on. This will preserve the [Journal Sequence Number](https://docs.yottadb.com/AdminOpsGuide/dbrepl.html#journal-sequence-number) used by replication.
- The [$ZTWORMHOLE](https://docs.yottadb.com/ProgrammersGuide/isv.html#ztwormhole) intrinsic special variable can be used to pass information from an originating instance to a replicating instance and can be set inside a trigger. Depending on the requirement, this may be a simpler alternative to `-trigupdate`.

[[#722](https://gitlab.com/YottaDB/DB/YDB/~/issues/722)]

### Messages

The following message is modified to make it more helpful:

**CINOENTRY**, No entry specified for xxxx in the call-in table

Run Time Error: This indicates that the call-name invoked by the C program does not have a corresponding entry in the call-in table specified by ydb_ci environment variable.

Action: Add an entry to the call-in table for the call-name. Refer to the [External Calls section in the Programmer’s Guide](https://docs.yottadb.com/ProgrammersGuide/langfeat.html#ext-calls).

### Legal Stuff

Copyright © 2023 YottaDB LLC

Permission is granted to copy, distribute and/or modify this document under the terms of the GNU Free Documentation License, Version 1.3 or any later version published by the Free Software Foundation; with no Invariant Sections, no Front-Cover Texts and no Back-Cover Texts.

YottaDB® and Octo® are registered trademarks of YottaDB LLC. GT.M™ is a trademark of Fidelity National Information Services, Inc.
Other trademarks belong to their respective owners.

This document contains a description of YottaDB and the operating instructions pertaining to the various functions that comprise the software. This document does not contain any commitment of YottaDB LLC. YottaDB LLC believes the information in this publication is accurate as of its publication date; such information is subject to change without notice. YottaDB LLC is not responsible for any errors or defects.
