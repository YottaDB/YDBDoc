<!---
.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2024-2025 YottaDB LLC and/or its subsidiaries.#
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

# YottaDB r2.04

## Binary Distributions

| sha256sum | file |
|-----------|------|
|           |      |

## Release Note Revision History

| Revision | Date | Summary              |
|----------|------|----------------------|
| 1.0      |      | Initial Release Note |

## Contact Information

### YottaDB LLC

https://yottadb.com /  <info@yottadb.com>

### Support

#### Customers

Contact YottaDB or your YottaDB support channel for support with assured service levels on commercial terms from knowledgeable staff.

#### Others

For free (to you) support from members of communities who run widely available applications on YottaDB, please use an application-specific list where appropriate.

 - Join the discussion a the [YottaDB Discord channel](https://discord.gg/bFHmDdzcqY).
 - For issues specific to the use of YottaDB from node.js via [Nodem](https://github.com/dlwicksell/nodem), [post an Issue on the Nodem project](https://github.com/dlwicksell/nodem/issues/new/).
 - For issues specific to the use of YottaDB from [QewdJS](http://qewdjs.com/), or [EWD.js](https://github.com/robtweed/ewd.js), or from node.js via [mg-dbx](https://github.com/chrisemunt/mg-dbx) or [mg-dbx-napi](https://github.com/chrisemunt/mg-dbx-napi) post to the [Enterprise Web Developer community](https://groups.google.com/forum/#!forum/enterprise-web-developer-community).
 - For issues specific to the use of YottaDB with [VistA](https://en.wikipedia.org/wiki/VistA) flavors, post to the [Hardhats](https://groups.google.com/forum/#!forum/hardhats) list.
 - For issues specific to use of YottaDB with the [M Language](https://docs.yottadb.com/ProgrammersGuide/), post to the [Everything MUMPS](https://groups.google.com/g/everythingmumps) list.
 - For requests other than to the communities above [post an Issue](https://gitlab.com/YottaDB/DB/YDB/-/issues) and include the words "Help Wanted" in the Title.

## r2.04

### Overview

Effective r2.04, the 32-bit ARM platform is Supportable, and no longer Supported. If this is a problem for you, please [contact us](mailto:info@yottadb.com).

### Platforms

A platform is a combination of a CPU architecture and an operating system. A platform is Supported, Supportable, or Unsupported.

* Supported means that we have the platform in our development environment, test each release on that platform, and provide a binary distribution for it.
* Supportable means that although we may not have such a platform in our environment, we have no reason to believe that the software will not run on it.
* All others are Unsupported.

| CPU Architecture                    | Supported OS Version(s)                                                              | Notes                                                                                                                         |
| ----------------------------------- | ------------------------------------------------------------------------------------ | ----------------------------------------------------------------------------------------------------------------------------- |
| 64-bit x86                          | Ubuntu 22.04 LTS & 24.04 LTS; Red Hat Enterprise Linux 8.x & 9.x; SUSE Linux Enterprise 15.x; Debian GNU/Linux 12 (Bookworm) | Except for Ubuntu 22.04 LTS and 24.04 LTS, which share a binary distribution, there are separate binary distributions for each OS version, owing to differences in library versions of those distributions. |
| 64-bit ARM (Raspberry Pi 3 Model B) | Debian GNU/Linux 12 (Bookworm)                                                       | See below.                                                                                                                    |
Recent releases of major 64-bit GNU/Linux distributions with contemporary kernels, glibc and ncurses are Supportable; however you may have to build YottaDB from source. Specific notes:

- Supported filesystems are ext4 and xfs. f2fs is Supportable. btrfs, zfs, and NFS are Unsupported, and known to have issues.
- [ydbinstall.sh](https://gitlab.com/YottaDB/DB/YDB/-/blob/master/sr_unix/ydbinstall.sh) recognizes [Rocky Linux](https://rockylinux.org/) as equivalent to RHEL, and [OpenSUSE Leap](https://www.opensuse.org/#Leap) as equivalent to SUSE Linux Enterprise, installing the releases for the corresponding distributions. Note that Rocky Linux and OpenSUSE Leap are Supportable, not Supported.
- On [Arch Linux](https://www.archlinux.org/) and other leading edge distributions such as [OpenSUSE Tumbleweed](https://www.opensuse.org/#Tumbleweed), as well as new versions of Linux distributions, YottaDB will need to be recompiled from source code owing to library and tool chain versions newer than those used in building Supported distributions. The `--from-source` option of [ydbinstall.sh](https://gitlab.com/YottaDB/DB/YDB/-/blob/master/sr_unix/ydbinstall.sh) simplifies the process.
- While YottaDB is Supportable on other ARMv6, [ARMv7-A](https://en.wikipedia.org/wiki/Comparison_of_ARMv7-A_cores), and [ARMv8-A](https://en.wikipedia.org/wiki/Comparison_of_ARMv8-A_cores) CPUs, owing to variations in the implementations of ARM microarchitectures, we recommend that you ensure the software runs correctly before committing to any specific hardware other than those listed above. Please [contact us](mailto:info@yottadb.com) if you want a specific combination of OS and CPU microarchitecture to be Supported.

### Installation
See our [Get Started page](https://yottadb.com/product/get-started) to use YottaDB.

We **strongly recommend** that you install YottaDB r2.04 in a newly created directory, different from those of prior YottaDB releases and any GT.M versions you may have installed on the system.

### Removing an installed YottaDB release
Assuming `$ydb_dist` points to the directory where YottaDB is installed:

 - Cleanly shut down all application processes using that release.
 - Execute `mupip rundown && mupip rundown -relinkctl` and review the output to ensure successful completion.
 - Ensure that there are no `gtcm*` or `gtmsecshr` processes active.
 - Use `sudo lsof | grep $ydb_dist` to ensure there are no open files.
   - If there are links to files in `$ydb_dist`, e.g., from `/usr/local/bin/` or `/usr/local/etc/`, remove the links.
 - Delete the directory with `sudo rm -rf $ydb_dist`

## Upgrading to YottaDB r2.04

YottaDB r2.04 is upward compatible from YottaDB [r2.02](https://gitlab.com/YottaDB/DB/YDB/-/releases/r2.02), GT.M [V7.1-000](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-000_Release_Notes.html), GT.M [V7.1-001](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-001_Release_Notes.html), and (fill in additional GT.M versions here). The minimal upgrade steps are:

* Install YottaDB r2.04 using the [ydbinstall.sh](https://download.yottadb.com/ydbinstall.sh) script; or download the YottaDB distribution for your platform, and use the included `ydbinstall` script to install YottaDB. The scripts can install the YottaDB plugins that you use along with installing YottaDB, or you can install or upgrade them later. The `--help` option of the scripts lists their capabilities.
* Install plugins you need in addition to those installed in the previous step, e.g., non-YottaDB plugins or customized plugins.
* Cleanly shut down the application and ensure that the database files are shut down using [MUPIP RUNDOWN](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#rundown) from the prior release.
* Recompile object code, and recreate shared libraries if your application uses shared libraries.
* Switch journal files with the new YottaDB release.
* Start using the new YottaDB release.

If the database has triggers defined with non-canonical numbers, or numbers specified as strings with any version prior to [r1.28](https://gitlab.com/YottaDB/DB/YDB/-/releases/r1.28), or if you are unsure, extract the trigger definitions, delete existing triggers, and reload the trigger definitions. Issue [[#430](https://gitlab.com/YottaDB/DB/YDB/-/issues/430)] from [r1.28](https://gitlab.com/YottaDB/DB/YDB/-/releases/r1.28) has a series of steps you can copy and execute. There is no need to do this if upgrading from [r1.28](https://gitlab.com/YottaDB/DB/YDB/-/releases/r1.28) or later.

To upgrade from older GT.M releases, first upgrade to GT.M V6.0-000 or later and follow the steps above, or contact your YottaDB support channel for assistance and guidance.

A more sophisticated upgrade technique is:

* Install YottaDB r2.04.
* Create a new replicated instance of your application (on the same system or a different system).
* Assuming the existing instance is A, and the new instance is B, upgrade B to r2.04 and start replicating from A to B.
* Once B catches up, switchover so that B is in a primary role replicating to A.
* Once you are satisfied with B, remove (or upgrade) A.

### Upgrading Global Directories

Opening a global directory file with [GDE](https://docs.yottadb.com/AdminOpsGuide/gde.html) and exiting automatically upgrades it to the latest format. However, as there is no way to downgrade the format of a global directory file, we recommend taking a backup before you upgrade it. The [GDE SHOW COMMAND](https://docs.yottadb.com/AdminOpsGuide/gde.html#show) outputs a text version of the global directory that you can use to recreate it, and which you can also use for global directory version control.

### Upgrading Database Files

Database files created with prior r2.x releases of YottaDB do not need to be explicitly upgraded. Any database file created or used by r2.04, will remain usable by any subsequent r2.x release. YottaDB r2.04 fully supports and uses database files created with r1.x releases of YottaDB.

*Note: A database file can only be opened by processes of one YottaDB release at any given time.*

In the event you need database files that need the 16Gi block limit or the 11 tree levels of r2.x (r1.x database files have a 992Mi block limit and 7 tree levels), you have two options:

* After a [MUPIP FREEZE](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#mupip-freeze) of the database, use [MUPIP EXTRACT](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#extract) to extract the database nodes, and [MUPIP LOAD](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#load) to load them into a new database file created by [MUPIP CREATE](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#create). A binary format will give you the fastest extract and load times. Note that you should use a MUPIP FREEZE across all regions of a database to ensure consistency of the database. The MUPIP EXTRACT and MUPIP LOAD processes can run concurrently for the regions.

* After a [MUPIP INTEG](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#integ) to verify the structural integrity of database files, use [MUPIP UPGRADE](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#mupip-upgrade) to upgrade the database file header to the 2.x format, followed by a [MUPIP REORG UPGRADE](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#upgrade) to upgrade the individual blocks.

If you wish to use a database file created or used by r2.04 on a prior release of YottaDB or a GT.M version, contact your YottaDB support channel.

## Change History

### r2.04

YottaDB r2.04 includes the following enhancements and fixes beyond YottaDB [r2.02](https://gitlab.com/YottaDB/DB/YDB/-/releases/r2.02).

| ID               | Category  | Summary                                                                                           |
|------------------|-----------|---------------------------------------------------------------------------------------------------|
| ([873](#x873))   | Languages | ZSHOW "V" able to display variables at a specific stack levels                                    |
| ([1056](#x1056)) | Languages | Pre-allocation for call-out (IO, not just O) string parameters                                    |
| ([1112](#x1112)) | Other     | No GTMSECSHRSRVF and CRITSEMFAIL errors from ydb_env_set in certain rare cases                    |
| ([1133](#x1133)) | Languages | `-machine` compilation option                                                                     |
| ([1138](#x1138)) | Languages | $ZYCOMPILE() intrinsic function checks whether a string is a syntactically correct line of M code |
|                  |           |                                                                                                   |

<a name="gtmv71000"></a>
### GT.M V7.1-000

YottaDB r2.04 incorporates enhancements and fixes from [GT.M V7.0-000](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-000_Release_Notes.html).

| ID                              | Category              | Summary                                                                                                                          |
|---------------------------------|-----------------------|----------------------------------------------------------------------------------------------------------------------------------|
| ([GTM-DE325871](#GTM-DE325871)) | Other                 | Remove limit affecting sockets and improve error message where it still exists for GT.CM                                         |
| ([GTM-DE340906](#GTM-DE340906)) | Languages             | Attempting a LOCK with more identical arguments than YottaDB supports for the command generates an error                            |
| ([GTM-DE340950](#GTM-DE340950)) | Languages             | Exceeding the LOCK level limit for the same resource name generates a LOCKINCR2HIGH error                                        |
| ([GTM-DE376223](#GTM-DE376223)) | Languages             | $FNUMBER() handles fill requests up to close to the maximum string length                                                        |
| ([GTM-DE376224](#GTM-DE376224)) | Languages             | Modulo of non-canonical number by a divisor greater than 999,999 returns a canonical result                                      |
| ([GTM-DE376239](#GTM-DE376239)) | Languages             | When YottaDB inserts an implicit QUIT to prevent a possible error, it generates a FALLINTOFLST WARNING message                   |
| ([GTM-DE388565](#GTM-DE388565)) | Languages             | Avoid inappropriate NUMFLOW from a literal Boolean argument with exponential (E) form                                            |
| ([GTM-DE402020](#GTM-DE402020)) | Database              | Prevent Block SIG-11 splits under rare concurrency conditions involving empty string values                                      |
| ([GTM-DE408789](#GTM-DE408789)) | System Administration | MUPIP BACKUP DATABASE uses faster copy mechanism when available                                                                  |
| ([GTM-DE421008](#GTM-DE421008)) | System Administration | Triple MUPIP STOP within a minute similar to, but slightly better than kill -9                                                   |
| ([GTM-DE422089](#GTM-DE422089)) | Other                 | Improved detection and reporting of issues with utility command length and parsing                                               |
| ([GTM-DE493831](#GTM-DE493831)) | Languages             | Prevent rare deadlock while using JOB command                                                                                    |
| ([GTM-F135385](#GTM-F135385))   | System Administration | MUPIP RCTLDUMP reports the number of times a routine has been replaced (rtnsupersede) in the autorelink cache                    |
| ([GTM-F135427](#GTM-F135427))   | System Administration | Support in-place conversion from V6 to V7 database formats                                                                       |
| ([GTM-F221672](#GTM-F221672))   | Other                 | Additional context in SHMHUGETLB syslog message                                                                                  |

<a name="gtmv71001"></a>
### GT.M V7.1-001

YottaDB r2.04 incorporates enhancements and fixes from [GT.M V7.0-001](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-001_Release_Notes.html).

| ID                              | Category              | Summary                                                                                                                          |
|---------------------------------|-----------------------|----------------------------------------------------------------------------------------------------------------------------------|
| ([GTM-DE476408](#GTM-DE476408)) | Other                 | Prevent failing JOB command from damaging shared data belonging to the issuing process                                           |
| ([GTM-DE500856](#GTM-DE500856)) | Languages             | %RANDSTR() limits range arguments                                                                                                |
| ([GTM-DE500860](#GTM-DE500860)) | Languages             | The YottaDB compiler accepts files with extensions other than `.m`, or with no extension                                         |
| ([GTM-DE503394](#GTM-DE503394)) | Other                 | %YGBLSTAT() issues warnings for defective command lines                                                                          |
| ([GTM-DE506257](#GTM-DE506257)) | Languages             | Prevent errors in global variable subscript shifting, and prevent all shifting under EXTENDED_BOOLEAN                            |
| ([GTM-DE506361](#GTM-DE506361)) | Other                 | GTMSECSHR handles rare race condition at its startup and shutdown                                                                |
| ([GTM-DE507982](#GTM-DE507982)) | Languages             | Ensure zero (0) derived from multiplying a non-integer by zero equates to zero                                                   |
| ([GTM-DE508852](#GTM-DE508852)) | Languages             | Correctly report NUMOFLOW errors when evaluating operations on literals at compile time                                          |
| ([GTM-DE510902](#GTM-DE510902)) | Languages             | Prevent literal operation failures in XECUTE blocks from improperly affecting the surrounding execution environment              |
| ([GTM-DE511969](#GTM-DE511969)) | Languages             | Direct Mode command RECALL restored                                                                                              |
| ([GTM-DE512004](#GTM-DE512004)) | Languages             | SET @expr supports long exprs and %ZSHOWVTOLCL uses them for alias containers                                                    |
| ([GTM-DE513737](#GTM-DE513737)) | Languages             | Protect the truth-value of subscripted local variables in Boolean expressions from subsequent side effects when gtm_boolean >= 1 |
| ([GTM-DE513980](#GTM-DE513980)) | Languages             | $ZMAXTPTIME can interrupt a transaction holding a database critical section                                                      |
| ([GTM-DE519525](#GTM-DE519525)) | Languages             | $ZTIMEOUT deferred during a TP transaction                                                                                       |
| ([GTM-DE525624](#GTM-DE525624)) | Languages             | $ZTRANSLATE() does not issue a BADCHAR when operating on UTF-8 strings                                                           |
| ([GTM-DE530712](#GTM-DE530712)) | Database              | Proactive Block Split appropriately handles blocks with no records in them                                                       |
| ([GTM-DE531077](#GTM-DE531077)) | Database              | Prevent multi-region trigger load operations from possibly terminating with an GTMASSERT2 error                                  |
| ([GTM-DE531078](#GTM-DE531078)) | Database              | Online Rollback terminates active KILL In Progress (KIP) activity                                                                |
| ([GTM-DE532295](#GTM-DE532295)) | Database              | YottaDB disables proactive block splitting within TP transactions and by default                                                 |
| ([GTM-F135040](#GTM-F135040))   | Languages             | $VIEW("JNLPOOL") with multiple instances                                                                                         |
| ([GTM-F225097](#GTM-F225097))   | System Administration | Second phase of in-place conversion from V6 to V7 database formats supports operation with concurrent activity                   |

### Database

* <a name="xGTM-DE402020"></a>YottaDB deals appropriately with a concurrency issue encountered when splitting a block, the record triggering the split has a zero-length value, concurrent changes make the previous record appear identical to the one triggering the split, and YottaDB attempts to calculate a parent key to demarcate the split. This apparently longstanding issue was detected by a customer using a stress test with the default proactive block split setting. While more likely with proactive block splits, the issue is difficult to reproduce without using carefully constructed update patterns. We have no indication that it has ever been previously reported by a customer or detected in our testing. Previously, the condition caused a process to fail with a segmentation violation (SIG-11) but did not result in any database damage. [GTM-DE402020](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-000_Release_Notes.html#GTM-DE402020)

* <a name="GTM-DE530712"></a>Proactive block split appropriately handles blocks with no records in them. Previously, under certain circumstances, proactive block split could try to induce a block split when the block had no records in it. This was only seen during development testing and not reported by a user. [GTM-DE530712](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-001_Release_Notes.html#GTM-DE530712)

* <a name="GTM-DE531077"></a>Trigger load operations involving many regions gracefully restart when a concurrent [MUPIP JOURNAL ROLLBACK ONLINE](https://docs.yottadb.com/AdminOpsGuide/ydbjournal.html#on-line) forces the process to restart transaction processing in each region. Previously, in such a situation, YottaDB processes could terminate with a GTMASSERT2 error. This error was only seen in development and not reported by a user. [GTM-DE531077](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-001_Release_Notes.html#GTM-DE531077)

* <a name="GTM-DE531078"></a>YottaDB processes working on the second phase of a [KILL](https://docs.yottadb.com/ProgrammersGuide/commands.html#kill) operation (KILL-In-Progress), issue a [DBROLLEDBACK](https://docs.yottadb.com/MessageRecovery/errors.html#dbrolledback) error in the event a [MUPIP JOURNAL ROLLBACK ONLINE](https://docs.yottadb.com/AdminOpsGuide/ydbjournal.html#on-line) changes the state of the database. Previously, YottaDB processes would restart until the fourth retry acquiring the critical section on all participating regions before issuing such an error. This error was only seen in development and not reported by a user. [GTM-DE531078](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-001_Release_Notes.html#GTM-DE531078)

* <a name="GTM-DE532295"></a>YottaDB disables proactive block splitting by default as well as within a TP transaction. YottaDB made this change after observing that, under certain conditions, the setting could incur spurious restarts and split blocks which were not subject to contention. To enable the feature outside of transaction processing, use a [MUPIP SET -PROBLKSPLIT=n](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#problksplit), where n is the number of nodes at which YottaDB considers based on the number of restarts whether to split a block in advance of its becoming full. Previously, starting in r2.00, the default threshold for a proactive block split was 5 nodes and the feature applied to TP as well as non-TP. The performance issue was only ever observed in testing and not reported by a user; it was not associated with any correctness or integrity concerns. [GTM-DE532295](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-001_Release_Notes.html#GTM-DE532295)

### Languages

* <a name="x873"></a>The [ZSHOW Information Code](https://docs.yottadb.com/ProgrammersGuide/commands.html#zshow-information-codes) "V" can take an optional additional \<intexp> parameter to specify the stack level (corresponding to [$STACK](https://docs.yottadb.com/ProgrammersGuide/isv.html#stack)) for which the command is to output local variables. A value of -1 is an alias for $STACK. A negative value for the parameter, or one greater than the current value of $STACK raises a [STACKRANGE](https://docs.yottadb.com/MessageRecovery/errors.html#stackrange) error.

  ```
  YDB>zshow "v"::2
  a="The quick brown fox"
  c="jumps over the lazy dog"

  YDB>zshow "v"
  a=1
  b=2
  c="jumps over the lazy dog"

  YDB>set n=2  zshow "v":d:n  zwrite d
  d("V",1)="a=""The quick brown fox"""
  d("V",2)="c=""jumps over the lazy dog"""

  YDB>
  ```

  If the information codes of a ZSHOW command with a stack specification do not include "V", the stack specification is silently ignored. Other ZSHOW information codes ignore the stack specification, e.g., ZSHOW "vs"::2 (assuming $STACK>2) displays the local variables for $STACK level 2, as well as the current M stack. [#873](https://gitlab.com/YottaDB/DB/YDB/-/issues/873)

* <a name="x1056"></a>A pre-allocation size may be specified for input-output (IO) string parameters in the [external call-out table](https://docs.yottadb.com/ProgrammersGuide/extrout.html#using-external-calls-call-outs), e.g. `IO:ydb_buffer_t*[100]`. Pre-allocation for `ydb_char_t *`, `ydb_string_t *`, and `ydb_buffer_t *` types was previously available for output (O) parameters but with this enhancement may also be specified for input-output (IO) parameters. Previously, an external function could only populate IO parameter strings up to the size of the input string. With pre-allocation, a maximum size may optionally be specified in the external call table to allocate more return space than the input string size. Note that IO `ydb_char_t*` and `ydb_string_t*` parameters do not allow querying the pre-allocation size at runtime; For robust applications, we recommend using `ydb_buffer_t*` for external call-outs.

  - Pre-allocation for non-string parameters in the external call-out table produces a [ZCPREALLVALSTR](https://docs.yottadb.com/MessageRecovery/errors.html#zcpreallvalstr) error. Previously, specifying a pre-allocation size for non-string parameters was silently ignored by YDB. Note that specifying the allocated space for non-string parameters is meaningless, as sizes are determined by the platform.
  - For IO parameters of type `ydb_string_t*` and `ydb_buffer_t*`, YDB distinguishes the empty string from skipped arguments in external callouts, treating the empty string consistently with non-empty strings. Previously, passing an empty string would behave as if the parameter were omitted altogether.
	- Note that `ydb_char_t*` still represents skipped strings as a 0-length null-terminated string. We recommend using `ydb_buffer_t*` for IO parameters to avoid ambiguity.
  - For O parameters, YDB distinguishes skipped arguments from other kinds of arguments in callouts. Previously, there would be no indication that the argument had been skipped and the C code was forced to assume an M actual value was present.
  - YDB passes skipped `ydb_string_t*` parameters as a `ydb_string_t { .length = 0, .address = NULL }` pair, for both IO and O parameters. Previously it only did so for skipped IO parameters, whereas skipped O parameters were passed as an empty string, which did not match the documentation.

  See [Using External Calls: Call-Outs](https://docs.yottadb.com/ProgrammersGuide/extrout.html#using-external-calls-call-outs) for more detail about how pre-allocated IO parameters are represented in C. [#1056](https://gitlab.com/YottaDB/DB/YDB/-/issues/1056)

* <a name="x1133"></a>YottaDB compilation supports a `-machine` flag (see [Qualifiers for the yottadb command](https://docs.yottadb.com/ProgrammersGuide/devcycle.html#qualifiers-for-the-yottadb-command)) which produces a listing that shows the generated assembly code for each line of source code. Specifying `-machine` automatically turns on the [-list](https://docs.yottadb.com/ProgrammersGuide/devcycle.html#no-li-st-filename) option if the latter is not explicitly specified. [#1133](https://gitlab.com/YottaDB/DB/YDB/-/issues/1133)

* <a name="x1138"></a>The function $ZYCOMPILE(str) verifies whether or not `str` is valid M code. An empty string is returned if `str` is syntactically valid M code; otherwise a string of the form `POS,YDB_ERROR,Error` is returned where

  * POS - is the position in the line where parsing stopped
  * YDB_ERROR - YottaDB Standard Error Code Mnemonic
  * Error - Error description

  See [Examples](https://gitlab.com/YottaDB/DB/YDB/-/merge_requests/1656#examples-of-zycompile) for example usages.

  Note that the intrinsic function is very similar to the M Utility Function [$$^%ZMVALID()](https://docs.yottadb.com/ProgrammersGuide/utility.html#zmvalid) but the intrinsic function is a lot faster (in an experiment it was found to be 100,000 times faster) and does not create any `.m` and `.o` files. [#1138](https://gitlab.com/YottaDB/DB/YDB/-/issues/1138)

* <a name="GTM-DE340906"></a>YottaDB appropriately handles a command with multiple (more than 255) [LOCKs](https://docs.yottadb.com/ProgrammersGuide/commands.html#lock) with the same name. Previously, a YottaDB command that created more than 255 LOCKs with the same name caused a segmentation violation (SIG-11). [GTM-DE340906](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-000_Release_Notes.html#GTM-DE340906)

* <a name="GTM-DE340950"></a>An attempt by a process to incrementally [LOCK](https://docs.yottadb.com/ProgrammersGuide/commands.html#lock) the same resource name more than 511 times produces a [LOCKINCR2HIGH](https://docs.yottadb.com/MessageRecovery/errors.html#lockincr2high) with accurate context. Previously LOCK processing did not appropriately detect the limit or supply correct context. [GTM-DE340950](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-000_Release_Notes.html#GTM-DE340950)

* The following GT.M V7.1-00 issue was already addressed in YottaDB r1.34 and the following release note is included here for completeness.

  * <a name="GTM-DE376223"></a>[$FNUMBER()](https://docs.yottadb.com/ProgrammersGuide/functions.html#fnumber) reserves appropriate memory to handle a third expr that approaches the maximum string length (currently 1MiB). Note that this function and [$JUSTIFY()](https://docs.yottadb.com/ProgrammersGuide/functions.html#justify) reserve 65 bytes for their actual formatting. Previously, a large specification for this amount could cause a segmentation violation (SIG-11). [GTM-DE376223](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-000_Release_Notes.html#GTM-DE376223)

* The following GT.M V7.1-00 issue was already addressed in YottaDB r1.32 and the following release note is included here for completeness.

  * <a name="GTM-DE376224"></a>Modulo of non-canonical number by a divisor greater than 999,999 returns a canonical result. Previously, it might not. [GTM-DE376224](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-000_Release_Notes.html#GTM-DE376224)

* <a name="GTM-DE376239"></a>YottaDB reports a [FALLINTOFLST](https://docs.yottadb.com/MessageRecovery/errors.html#fallintoflst) error after an argumentless [DO](https://docs.yottadb.com/ProgrammersGuide/commands.html#do) embedded subroutine followed by a label with a formallist when no [QUIT](https://docs.yottadb.com/ProgrammersGuide/commands.html#quit) terminates the code after the DO block, except when there are no lines between the end of the embedded subroutine and the label with the formallist, in which case YottaDB inserts an implicit QUIT to separate them. When YottaDB inserts the implicit QUIT, it issues a FALLINTOFLST warning unless compilation has a NOWARNING qualifier. Previously, YottaDB inappropriately gave that error for cases of that combination under circumstances where the QUIT was on the same line as the argumentless DO rather than explicitly between the embedded subroutine and the label with the formallist. Note that this represents a different (and hopefully better) way of handling defect in application code than r2.02 did. [GTM-DE376239](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-000_Release_Notes.html#GTM-DE376239)

* The following GT.M V7.1-000 issue never occurred in any YottaDB release, and the following release note is included here for completeness.

  * <a name="GTM-DE388565"></a>YottaDB handles string literal operands to a Boolean string relational operator where the literal contains an exponential format appropriately. Previously such a combination inappropriately produced a [NUMOFLOW](https://docs.yottadb.com/MessageRecovery/errors.html#numoflow) error if the numeric evaluation would have produced an error. [GTM-DE388565](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-000_Release_Notes.html#GTM-DE388565)

* <a name="GTM-DE493831"></a>YottaDB properly handles interrupts while jobbing off a child process. Previously, in rare circumstances and related to timing, an interrupt could result in a deadlock. This was only seen in development and not reported by a user. [GTM-DE493831](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-000_Release_Notes.html#GTM-DE493831)

* <a name="GTM-DE500856"></a>[%RANDSTR()](https://docs.yottadb.com/ProgrammersGuide/utility.html#randstr) limits the range argument upper limit to the actual number of characters available in the current character set - 256 for M mode and 65,536 for UTF-8 mode. Previously, a missing or defective upper limit caused the routine to perform unproductive processing that could consume unreasonable amounts of time. The workaround was to avoid inappropriate range arguments. [GTM-DE500856](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-001_Release_Notes.html#GTM-DE500856)

* <a name="GTM-DE500860"></a>The YottaDB compiler accepts source files with arbitrary extensions, or even no extension. YottaDB recommends using the `.m` extension for source files as our testing of that is very extensive, however there may be cases where other extensions serve a purpose. Previously, the compiler enforced an explicit or implicit .m extension for source files. [GTM-DE500860](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-001_Release_Notes.html#GTM-DE500860)

  > [!note]
  > The GT.M enhancement allowed extensions other than `.m`. YottaDB further enhanced the compiler to allow filenames with no extension.

* The following YottaDB release note is adapted from the GT.M release note. YottaDB does not inplement the EXTENDED_BOOLEAN setting of [$ydb_boolean](https://docs.yottadb.com/AdminOpsGuide/basicops.html#ydb-boolean) / \$gtm\_boolean, as we could not find any cases where EXTENDED\_BOOLEAN gave different results from FULL\_BOOLEAN.

  * <a name="GTM-DE506257"></a>GT.M appropriately evaluates subscripts containing or depending on side effects in lock resource and global variable names. Previously, due to shifting of evaluations to avoid unnecessary global access while appropriately maintaining $REFERENCE, rare constructs with such subscripts could produce out-of-order side effects. [GTM-DE506257](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-001_Release_Notes.html#GTM-DE506257)

* <a name="GTM-DE507982"></a>YottaDB produces an appropriate result from an equality test between zero (0) and a multiplicand of zero value that is the result of multiplying a non-integer value with zero (0). Previously, this odd combination could inappropriately indicate inequality. [GTM-DE507982](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-001_Release_Notes.html#GTM-DE507982)

* <a name="GTM-DE508852"></a>YottaDB appropriately reports a [NUMOFLOW](https://docs.yottadb.com/MessageRecovery/errors.html#numoflow) error for literal values when those literals are operated on at compile-time and the run-time variable equivalent would trigger the same error. This affects the unaryoperators \', +, and -. The effect of these operators on variables is unchanged. Previously, these operators sometimes did not correctly report a NUMOFLOW when called for. In the case of the negation operator, this could lead to reporting an incorrect result. [GTM-DE508852](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-001_Release_Notes.html#GTM-DE508852)

* <a name="GTM-DE510902"></a>YottaDB prevents compile-time errors in operations on literals within an [XECUTE](https://docs.yottadb.com/ProgrammersGuide/commands.html#xecute) block from terminating the XECUTE without properly cleaning up the surrounding compilation environment. Previously, this could cause termination of compilation of the routine containing the XECUTE and failure to compile subsequent routines passed to the same YottaDB process [GTM-DE510902](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-001_Release_Notes.html#GTM-DE510902)

* <a name="GTM-DE511969"></a>The Direct Mode [RECALL](https://docs.yottadb.com/ProgrammersGuide/opdebug.html#command-recall) command functions as documented; starting in r2.02 it always gave an error. [GTM-DE511969](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-001_Release_Notes.html#GTM-DE511969)

  > [!note]
  > For a more powerful direct mode, YottaDB recommends setting [$ydb_readline](https://docs.yottadb.com/AdminOpsGuide/basicops.html#ydb-readline) to use [GNU Readline](https://www.gnu.org/software/readline/).

* <a name="GTM-DE512004"></a>SET @($ZWRTAC=expr) works for strings approaching 1MiB; previously such an indirect expression was limited to the maximum length of a source line. Also, the [%ZSHOWVTOLCL](https://docs.yottadb.com/ProgrammersGuide/utility.html#zshowvtolcl) utility routine now deals with more cases, such as such longer strings. [GTM-DE512004](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-001_Release_Notes.html#GTM-DE512004)

* <a name="GTM-DE513737"></a>YottaDB returns the correct value for a Boolean expression containing a subscripted local variable when that variable is affected by side-effects later in the expression and [$ydb_boolean](https://docs.yottadb.com/AdminOpsGuide/basicops.html#ydb-boolean)>=1. Previously, YottaDB evaluated the boolean using the value of the subscripted local after all side-effects. This issue did not affect unsubscripted local variables or globals of any kind. [GTM-DE513737](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-001_Release_Notes.html#GTM-DE513737)

* <a name="GTM-DE513980"></a>YottaDB correctly executes the [$ETRAP](https://docs.yottadb.com/ProgrammersGuide/isv.html#etrap)/[$ZTRAP](https://docs.yottadb.com/ProgrammersGuide/isv.html#ztrap) exception handler at the time of expiry of [$ZMAXTPTIME](https://docs.yottadb.com/ProgrammersGuide/isv.html#zmaxtptime) when the process holds a database critical section. Previously, due to a regression in r2.00, the $ZMAXTPTIME timer did not execute $ETRAP/$ZTRAP exception handler until the process released all database critical sections which could allow a transaction to materially exceed the specified $ZMAXTPTIME. [GTM-DE513980](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-001_Release_Notes.html#GTM-DE513980)

* <a name="GTM-DE519525"></a>YottaDB defers [$ZTIMEOUT](https://docs.yottadb.com/ProgrammersGuide/isv.html#ztimeout) when its expiry happens during a TP transaction. [$ZMAXTPTIME](https://docs.yottadb.com/ProgrammersGuide/isv.html#zmaxtptime) may interrupt a transaction, as optionally may [MUPIP INTRPT](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#intrpt) initiated [$ZINTERRUPT](https://docs.yottadb.com/ProgrammersGuide/isv.html#zinterrupt) processing, but $ZTIMEOUT acts after a [TROLLBACK](https://docs.yottadb.com/ProgrammersGuide/commands.html#trollback) or outermost [TCOMMIT](https://docs.yottadb.com/ProgrammersGuide/commands.html#tcommit). Previously, YottaDB allowed a $ZTIMEOUT expiry within a TP transaction. [GTM-DE519525](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-001_Release_Notes.html#GTM-DE519525)

* <a name="GTM-DE525624"></a>Because it is a byte-oriented function, [$ZTRANSLATE()](https://docs.yottadb.com/ProgrammersGuide/functions.html#ztranslate) does not issue a [BADCHAR](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#intrpt) error when operating on UTF-8 strings. Due to a regression in r1.36, this function previously incorrectly issued a BADCHAR error when the input string contained non-UTF-8 characters. There were two possible workarounds for this issue: 1) A targeted approach requiring code changes to enclose each use of $ZTRANSLATE() with [VIEW "NOBADCHAR"](https://docs.yottadb.com/ProgrammersGuide/commands.html#no-badchar) and VIEW "BADCHAR", which ensures all UTF-8 data is handled appropriately. 2) A broad approach of defining [$ydb_badchar](https://docs.yottadb.com/AdminOpsGuide/basicops.html#ydb-badchar) as zero or FALSE to disable BADCHAR checking through-out the application which disables detection of any improperly formatted UTF-8 strings. [GTM-DE525624](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-001_Release_Notes.html#GTM-DE525624)

* <a name="GTM-F135040"></a>Specifying a second expression for [$VIEW("JNLPOOL")](https://docs.yottadb.com/ProgrammersGuide/functions.html#argument-keywords-of-view) provides a means of iterating through active Journal Pools. If the second expression is an empty string, the function returns the replication instance file name associated with the instance first attached by the process or the string "\*" if the process has not previously engaged with any instance. If the file name specified in the second expression does not match the replication instance file name for any of the active Journal Pools the string "\*" is returned. Otherwise the file name of the Journal Pool attached after the Journal Pool with the file name given in the second expression is returned. Note the two argument form of $VIEW("JNLPOOL") does not change the current Replication Instance. [GTM-F135040](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-001_Release_Notes.html#GTM-F135040)

### System Administration

* <a name="GTM-DE408789"></a>[MUPIP BACKUP DATABASE](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#database) attempts to use a faster copy mechanism depending on the support by the kernel, and by source and destination filesystems. If the source and destination filesystems are different or the faster copy mechanisms are not available in the kernel, MUPIP BACKUP DATABASE uses the default copy mechanism (`/bin/cp`). Previously, YottaDB used faster copy mechanisms only on Linux Kernel 5.3 or above, and changes due to backporting in Linux kernels could cause MUPIP BACKUP to report an EXDEV error on filesystems where backups had earlier been supported.

  [MUPIP BACKUP ONLINE](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#mupip-backup-online) does not retry backup when it detects a concurrent rollback or on certain errors during the copy phase of BACKUP. Previously, MUPIP BACKUP ONLINE incorrectly retried backup when it encountered a concurrent rollback or an error in the first backup attempt; the workaround was to specify RETRY=0. [GTM-DE408789](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-000_Release_Notes.html#GTM-DE408789)

* <a name="xGTM-DE421008"></a>[MUPIP STOP](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#stop) three times within a minute acts like a kill -9 by stopping a process even if it might not be safe to do so, except that it may produce a core file. Three MUPIP STOPs issued over a period of more than one minute terminate the process when it is safe to do so. Previously any three MUPIP STOPs over the life of a process acted like a kill -9, whether or not the three MUPIP STOPs were sent within 1 minute, and whether or not the process was at a safe point to be terminated. [GTM-DE421008](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-000_Release_Notes.html#GTM-DE421008)

* <a name="xGTM-F135385"></a>[MUPIP RCTLDUMP](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#rctldump) reports the number of times a routine has been superseded (rtnsupersede) in the autorelink cache. Previously, MUPIP RTCLDUMP did not record this value, and only recorded the number of times a routine has been referenced. [GTM-F135385](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-000_Release_Notes.html#GTM-DE411385)

* <a name="xGTM-F135427"></a>YottaDB r2.04 provides the capability to upgrade a r1.x database to r2.x in-place. There is no ability to downgrade a r2.x database to r1.x in place. You can use [MUPIP EXTRACT](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#extract) on r2.x and [MUPIP LOAD](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#load) on r1.x as long as the data does not cause the r1.x database file to exceed the r1.x maximum limits, or revert to a prior version using a suitable combination of replicating instances. YottaDB r2.04 blocks all access to a r1.x database marked as not fully upgraded from the V4 format.

  YottaDB r2.x databases differ from r1.x in the following ways. Please refer to the Administration and Operations Guide for more details about these differences.

  * Starting Virtual Block Number (VBN) is 8193, or slightly more on upgraded files in r2.x, vs. 513 in r1.x.
  * Block numbers are 64-bit in r2.x, rather than 32-bit in r1.x.

  A YottaDB r2.x instance can originate BC/SI replication stream to or replicate from a r1.x BC/SI replication stream as long as the r2.x database remains within the maximum r1.x limits.

  The r1.x to r2.x database upgrade process is split into two phases intended to reduce the downtime necessary for a database upgrade. This process is considerably faster and consumes less disk space than a traditional extract, transfer and load cycle. Please refer to [Upgrading to YottaDB r2.04](releases#releases#upgrading-to-yottadb-r204) for more details. [GTM-F135427](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-000_Release_Notes.html#GTM-F135427)

* <a name="GTM-F225097"></a>[MUPIP REORG UPGRADE](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#upgrade), which completes the second phase of an r1.x (V6) to r2.x (V7) database transition, can run concurrently with other processing excepting other MUPIP REORG UPGRADE processes. MUPIP REORG UPGRADE can work either by region or by file allowing administrator to run concurrent upgrade operations on different regions/files. MUPIP DOWNGRADE -VERSION=V63000A allows the current YottaDB to downgrade a V6 database to the pre-V63000A EOF block format. Previously the downgrade function reported an unsupported error for a V7 versions. [GTM-F225097](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-001_Release_Notes.html#GTM-F225097)

### Other

* <a name="x1112"></a>[ydb_env_set](https://docs.yottadb.com/AdminOpsGuide/basicops.html#ydb-env-set) when run by multiple users from different environments all of which use the same YottaDB installation (i.e. [$ydb_dist](https://docs.yottadb.com/AdminOpsGuide/basicops.html#ydb-dist)) works correctly. Previously, it was possible in rare cases for `ydb_env_set` to issue [GTMSECSHRSRVF](https://docs.yottadb.com/MessageRecovery/errors.html#gtmsecshrsrvf) errors. [#1112](https://gitlab.com/YottaDB/DB/YDB/-/issues/1112)

* <a name="GTM-DE325871"></a>YottaDB processes can use sockets created when over 1021 files, pipes, fifos, sockets, and/or regions are already open. YottaDB issues an [FDSIZELMT](https://docs.yottadb.com/MessageRecovery/errors.html#fdsizelimit) error message when there are too many descriptors needed by GT.CM servers. Previously, sockets created when there were too many open descriptors caused an GTMASSERT2 error. [GTM-DE325871](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-000_Release_Notes.html#GTM-DE325871)

* <a name="GTM-DE422089"></a>The YottaDB command line parser correctly terminates input on a null byte. Previously, in rare cases, the parser appended random characters for a [PIPE](https://docs.yottadb.com/ProgrammersGuide/ioproc.html#using-pipe-devices) device usage where a [WRITE](https://docs.yottadb.com/ProgrammersGuide/commands.html#write) followed by the format control character "!" did not precede WRITE /EOF. This was seen only in development/testing and never reported by a user. Previously, YottaDB silently truncated shell arguments that exceeded these limits and did not produce an error when input to a utility prompt exceeded the allowed 33022 bytes. [GTM-DE422089](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-000_Release_Notes.html#GTM-DE422089)

* <a name="GTM-DE476408"></a>A process created by a [JOB](https://docs.yottadb.com/ProgrammersGuide/commands.html#job) command that for any reason exits before becoming fully functional and independent avoids interfering with any data it may share with the process issuing the JOB command. YottaDB also issues a [PIDMISMATCH](https://docs.yottadb.com/MessageRecovery/errors.html#pidmismatch) warning message to the operator log. Previously under rare conditions, such an "infant" process could inappropriately release resources belonging to its "parent," causing symptoms including damage to a statsdb database or to a routine repository. [GTM-DE476408](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-001_Release_Notes.html#GTM-DE476408)

* <a name="GTM-DE503394"></a>[%YGBLSTAT()](https://docs.yottadb.com/ProgrammersGuide/utility.html#ygblstat) warns about incorrect command line usage. Previously, the utility silently ignored command lines containing errors. [GTM-DE503394](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-001_Release_Notes.html#GTM-DE500860)

* <a name="GTM-DE506361"></a>[GTMSECSHR](https://docs.yottadb.com/AdminOpsGuide/ipcresource.html#gtmsecshr) appropriately handles a rare condition when two processes attempt to start a GTMSECSHR process at a coincident time. Previously, this could start more than one GTMSECSHR process, and, although a single GTMSECSHR process handled all the requests, their shutting down produced syslog error messages. [GTM-DE506361](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-001_Release_Notes.html#GTM-DE506361)

* <a name="GTM-F221672"></a>The [SHMHUGETLB]((https://docs.yottadb.com/MessageRecovery/errors.html#shmhugetlb) syslog warning message provides information about the operation of the calling process. Previously, SHMHUGETLB failure messages did not include operational information necessary to understand the reasons for such failures. [GTM-F221672](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-000_Release_Notes.html#GTM-F221672)

## Additional Information

(There is no Additional Information for r2.04.)

## Messages

### New Messages

**ARGTRUNC**, UUUU argument number CCCC truncated. Keep the size of total command line within NNNN bytes

DSE/LKE/MUPIP Warning: This warning appears when the YottaDB parser truncates an argument of a YottaDB Utility ([DSE](/AdminOpsGuide/dse.html), [LKE](https://docs.yottadb.com/AdminOpsGuide/mlocks.html), or [MUPIP](https://docs/yottadb.com/AdminOpsGuide/dbmgmt.html)) executable exceeding the allowed maximum of NNNN bytes. CCCC is the argument number with 1 being the first argument for the YottaDB Utility executable.

Action: Reduce the size of the argument number CCCC.

**DBFILERDONLY**, The database file ffff was opened as read-only (perms pppp)

All YottaDB Components Error: Database file ffff was opened read-only with permissions pppp, but the read-only status is inconsistent with application expectations.

Action: Use the error and any follow-on messages to assess whether or not the read-only status is correct or the rejection is appropriate.

**DBUPGRDREQ**, Database file DDDD is not fully upgraded (format FFFF) and cannot be used by this version of YottaDB. Please upgrade the database.

MUPIP Error: The database file DDDD with block format FFFF has the fully upgraded flag set to FALSE indicating that it holds a mix of block versions.

Action: While YottaDB r1.x can use database files with formats as old as GT.M V4.x, YottaDB starting with r2.x does not handle the V4 block format. Use YottaDB r1.x to fully upgrade the database file before using it.

**FDSIZELMT**, Too many nnnn descriptors needed by GT.CM server

GT.CM Error: A large number (nnnn) of regions accessed on behalf of [GT.CM ](https://docs.yottadb.com/AdminOpsGuide/gtcm.html) clients forced the file descriptor numerical value to its FD_SETSIZE limit. Under Linux as of this writing, this limit is fixed to 1024.

Action: Review the application, the database layout and the number of concurrent clients and adjust conditions to reduce the number of concurrent database files managed by the GT.CM server.

**FORCEDHALT2**, Receipt of 3 MUPIP STOP signals within xxxx seconds, process: yyyy shutting down with no exit handling like a kill -9

Run Time Fatal: This indicates that a YottaDB process recognized the receipt of three [MUPIP STOP ](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#stop) commands within approximately 60 seconds and is shutting down without normal clean up - very similar to a kill -9 signal. This event doesn't stop YottaDB processes in an orderly fashion, and might cause database damage if the target process is concurrently actively updating. Therefore use it only on processes that are deadlocked or otherwise stuck, say due to some type of FREEZE.

Action: Determine who initiated the MUPIP STOP and why they did so. Run [MUPIP INTEG ](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#integ) as appropriate.

**LINETOOLONG**, UUUU prompt input exceeds NNNN bytes

DSE/LKE/MUPIP Error: This error appears when the YottaDB parser detects the input to a YottaDB Utility ([DSE](/AdminOpsGuide/dse.html), [LKE](https://docs.yottadb.com/AdminOpsGuide/mlocks.html), or [MUPIP](https://docs/yottadb.com/AdminOpsGuide/dbmgmt.html)) prompt exceeds the allowed maximum of NNNN bytes.

Action: Reduce the size of input to the utility prompt. If input to the UUUU prompt is from a [PIPE device ](https://docs.yottadb.com/ProgrammersGuide/ioproc.html#using-pipe-devices), set the [RECORDSIZE deviceparameter ](https://docs.yottadb.com/ProgrammersGuide/ioproc.html#pipe-deviceparameter-summary) to a value less than NNNN bytes.

**ORLBKDBUPGRDREQ**, Region RRR (DDDD) is not fully upgraded. ONLINE ROLLBACK cannot continue

MUPIP Error: Region RRR pointing to database file DDDD has the fully upgraded flag set to FALSE and the database format is not r2.x indicating that there are GT.M V4 blocks in the database. [MUPIP JOURNAL ROLLBACK ONLINE ](https://docs.yottadb.com/AdminOpsGuide/ydbjournal.html#on-line) in YottaDB r2.x cannot process these database files.

Action: Because a MUPIP JOURNAL ROLLBACK ONLINE is not possible for this database, stop all access to the database files and perform a ROLLBACK with standalone access.

**PIDMISMATCH**, PID=qqqq has a mismatched internal process id value of pppp

Run Time Warning: pppp is the process_id shown by the internal state, qqqq is the operating system pid reported by [getpid()](https://www.man7.org/linux/man-pages/man2/getpid.2.html). If a child process forked off by a [JOB ](https://docs.yottadb.com/ProgrammersGuide/commands.html#job) command terminates abnormally and prematurely (i.e., before the jobbed off process setup is complete), this message indicates that it skipped exit handing in order to avoid potential damage to the parent processes' statsdb or routine shared memory.

Action: No action is required, unless there is a subsequent REQRUNDOWN or REQRLNKCTLRNDWN shortly afterwards.

**REORGUPCNFLCT**, MUPIP AAAA encountered a conflict due to OOOO (PID:PPPP)

MUPIP Error: MUPIP action AAAA encountered a conflict due to a concurrent operation OOOO run as process ID PPPP.

Action: MUPIP operations [REORG UPGRADE ](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#upgrade) and [ONLINE ROLLBACK ](https://docs.yottadb.com/AdminOpsGuide/ydbjournal.html#on-line) cannot run concurrently due to conflicting database changes. REORG UPGRADE exits if an ROLLBACK ONLINE is in progress or if it detects that an ROLLBACK ONLINE has started. ONLINE ROLLBACK pauses while waiting for the REORG UPGRADE to exit. ONLINE ROLLBACK has priority over REORG UPGRADE.

### Revised Messages

The following are updated messages:

**GTMSECSHRDMNSTARTED**, gtmsecshr daemon started for version vvvv from dddd using socket file ssss

GTMSECSHR Information: This message indicates that GTMSECSHR daemon was started for the version vvvv from the installation directory dddd and is expecting to receive client messages in the socket file whose full path is indicated by ssss. This message is immediately followed by a [GTMSECSHRTMPPATH](https://docs.yottadb.com/MessageRecovery/errors.html#gtmsecshrtmppath) message which records the value of the [ydb_tmp ](https://docs.yottadb.com/AdminOpsGuide/basicops.html#ydb-tmp) environment variable with which this daemon/server process started. In case a client process later encounters a [GTMSECSHRSRVF](https://docs.yottadb.com/MessageRecovery/errors.html#gtmsecshrsrvf) error, it would issue a GTMSECSHRTMPPATH message as well which records the value of the $ydb_tmp the client started with. Most often, the cause of the GTMSECSHRSRVF is a mismatch between the ydb_tmp env var values between the server and client processes. The GTMSECSHRTMPPATH messages of the server and client would help in identifying such a mismatch.

Action: N/A

**MUUPGRDNRDY**, Database xxxx has not been completely upgraded to yyyy format - still bbbb database blocks to upgrade

MUPIP Error: The named database file is in an older format than is in use by this YottaDB version and has not been certified as ready for use by this YottaDB version. There are still bbbb blocks in the older format that need to be upgraded.

Action: Run [MUPIP UPGRADE ](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#mupip-upgrade)  to complete the upgrade.

**SHMHUGETLB**, Could not back shared memory with huge pages, using base pages instead ffff

Run Time Warning: When the [ydb_hugetlb_shm ](https://docs.yottadb.com/AdminOpsGuide/basicops.html#ydb-hugetlb-shm) environment variable is defined and evaluates to a non-zero integer or any case-independent string or leading substring of "TRUE" or "YES" in a process creating shared memory, YottaDB attempts to back shared memory segments with hugepages, using the default hugepages size. If huge pages cannot be used, YottaDB outputs the SHMHUGETLB warning and tries to back the shared memory with base pages instead. The warning message specifies the operation of the caller along with the resource ffff for the process requesting shared memory. The warning message also includes either an ENOMEM or an EPERM error, depending on why the request for hugepages failed.

**ZCPREALLVALSTR**, Pre-allocation allowed only for output or input/output variables of type ydb_buffer_t*, ydb_string_t*, or ydb_char_t*

Compile Time Error: This indicates that the program specified a pre-allocation for non-string-type variable. In particular, pre-allocation is not allowed for :code:`ydb_char_t**`, which always has a fixed size.

Action: Remove the pre-allocation.

**ZSHOWSTACKRANGE**, Invalid stack level value nnnn for ZSHOW "V"

Run Time Error: This indicates that a ZSHOW argument specified an invalid stack level. The valid range is :code:`[0, $STACK]`, inclusive.

Action: Modify the argument to use a valid value.

### Obsolete messages

The following messages are no longer used:

* ASYNCIONOV4
* CRYPTNOV4
* MMNODYNDWNGRD
* MMNODYNUPGRD
* MUDWNGRDNRDY
* MUDWNGRDTN
* MUREENCRYPTV4NOALLOW
* MUTRUNCNOV4
* ORLBKNOV4BLK
* SNAPSHOTNOV4
* SSV4NOALLOW

## Legal Stuff

Copyright © 2025 YottaDB LLC. Portions Copyright Fidelity National Information Services, Inc. and/or its subsidiaries.

Permission is granted to copy, distribute and/or modify this document under the terms of the [GNU Free Documentation License, Version 1.3](https://www.gnu.org/licenses/fdl-1.3.txt) or any later version published by the [Free Software Foundation](https://www.fsf.org/); with no Invariant Sections, no Front-Cover Texts and no Back-Cover Texts.

YottaDB® and Octo® are registered trademarks, and Xider™ is trademark, of YottaDB LLC. GT.M™ is a trademark of Fidelity National Information Services, Inc. Other trademarks belong to their respective owners.

This document contains a description of YottaDB and the operating instructions pertaining to the various functions that comprise the software. This document does not contain any commitment of YottaDB LLC. YottaDB LLC believes the information in this publication is accurate as of its publication date; such information is subject to change without notice. YottaDB LLC is not responsible for any errors or defects.
