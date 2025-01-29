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

YottaDB r2.04 is upward compatible from YottaDB [r2.02](https://gitlab.com/YottaDB/DB/YDB/-/releases/r2.02), GT.M [V7.1-000](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-000_Release_Notes.html), and (fill in additional GT.M versions here). The minimal upgrade steps are:

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

| ID               | Category  | Summary                                                                        |
|------------------|-----------|--------------------------------------------------------------------------------|
| ([1056](#x1056)) | Languages | Pre-allocation for call-out (IO, not just O) string parameters                 |
| ([1112](#x1112)) | Other     | No GTMSECSHRSRVF and CRITSEMFAIL errors from ydb_env_set in certain rare cases |
|                  |           |                                                                                |

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
| ([GTM-DE402020](#GTM-DE402020)) | Database              | Prevent Block SIG-11 splits under rare concurrency conditions involving empry string values                                      |
| ([GTM-DE408789](#GTM-DE408789)) | System Administration | MUPIP BACKUP DATABASE uses faster copy mechanism when available                                                                  |
| ([GTM-DE421008](#GTM-DE421008)) | System Administration | Triple MUPIP STOP within a minute similar to, but slightly better than kill -9                                                   |
| ([GTM-DE422089](#GTM-DE422089)) | Other                 | Improved detection and reporting of issues with utility command length and parsing                                               |
| ([GTM-DE493831](#GTM-DE493831)) | Languages             | Prevent rare deadlock while using JOB command                                                                                    |
| ([GTM-F135385](#GTM-F135385))   | System Administration | MUPIP RCTLDUMP reports the number of times a routine has been replaced (rtnsupersede) in the autorelink cache                    |
| ([GTM-F135427](#GTM-F135427))   | System Administration | Support in-place conversion from V6 to V7 database formats                                                                       |
| ([GTM-F221672](#GTM-F221672))   | Other                 | Additional context in SHMHUGETLB syslog message                                                                                  |

### Database

* <a name="xGTM-DE402020"></a>YottaDB deals appropriately with a concurrency issue encountered when splitting a block, the record triggering the split has a zero-length value, concurrent changes make the previous record appear identical to the one triggering the split, and YottaDB attempts to calculate a parent key to demarcate the split. This apparently longstanding issue was detected by a customer using a stress test with the default proactive block split setting. While more likely with proactive block splits, the issue is difficult to reproduce without using carefully constructed update patterns. We have no indication that it has ever been previously reported by a customer or detected in our testing. Previously, the condition caused a process to fail with a segmentation violation (SIG-11) but did not result in any database damage. [GTM-DE402020](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-000_Release_Notes.html#GTM-DE402020)

### Languages

* <a name="x1056"></a>A pre-allocation size may be specified for input-output (IO) string parameters in the [external call-out table](https://docs.yottadb.com/ProgrammersGuide/extrout.html#using-external-calls-call-outs), e.g. `IO:ydb_buffer_t*[100]`. Pre-allocation for `ydb_char_t *`, `ydb_string_t *`, and `ydb_buffer_t *` types was previously available for output (O) parameters but with this enhancement may also be specified for input-output (IO) parameters. Previously, an external function could only populate IO parameter strings up to the size of the input string. With pre-allocation, a maximum size may optionally be specified in the external call table to allocate more return space than the input string size. Note that IO `ydb_char_t*` and `ydb_string_t*` parameters do not allow querying the pre-allocation size at runtime; For robust applications, we recommend using `ydb_buffer_t*` for external call-outs.

  - Pre-allocation for non-string parameters in the external call-out table produces a [ZCPREALLVALSTR](https://docs.yottadb.com/MessageRecovery/errors.html#zcpreallvalstr) error. Previously, specifying a pre-allocation size for non-string parameters was silently ignored by YDB. Note that specifying the allocated space for non-string parameters is meaningless, as sizes are determined by the platform.
  - For IO parameters of type `ydb_string_t*` and `ydb_buffer_t*`, YDB distinguishes the empty string from skipped arguments in external callouts, treating the empty string consistently with non-empty strings. Previously, passing an empty string would behave as if the parameter were omitted altogether.
	- Note that `ydb_char_t*` still represents skipped strings as a 0-length null-terminated string. We recommend using `ydb_buffer_t*` for IO parameters to avoid ambiguity.
  - For O parameters, YDB distinguishes skipped arguments from other kinds of arguments in callouts. Previously, there would be no indication that the argument had been skipped and the C code was forced to assume an M actual value was present.
  - YDB passes skipped `ydb_string_t*` parameters as a `ydb_string_t { .length = 0, .address = NULL }` pair, for both IO and O parameters. Previously it only did so for skipped IO parameters, whereas skipped O parameters were passed as an empty string, which did not match the documentation.

  See [Using External Calls: Call-Outs](https://docs.yottadb.com/ProgrammersGuide/extrout.html#using-external-calls-call-outs) for more detail about how pre-allocated IO parameters are represented in C. [#1056](https://gitlab.com/YottaDB/DB/YDB/-/issues/1056)

* <a name="GTM-DE340906"></a>YottaDB appropriately handles a command with multiple (more than 255) LOCKs with the same name. Previously, a YottaDB command that created more than 255 LOCKs with the same name caused a segmentation violation (SIG-11). [GTM-DE340906](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-000_Release_Notes.html#GTM-DE340906)

* <a name="GTM-DE340950"></a>An attempt by a process to incrementally LOCK the same resource name more than 511 times produces a [LOCKINCR2HIGH](https://docs.yottadb.com/MessageRecovery/errors.html#lockincr2high) with accurate context. Previously LOCK processing did not appropriately detect the limit or supply correct context. [GTM-DE340950](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-000_Release_Notes.html#GTM-DE340950)

* The following GT.M V7.1-00 issue was already addressed in YottaDB r1.34 and the following release note is included here for completeness.

  * <a name="GTM-DE376223"></a>$FNUMBER() reserves appropriate memory to handle a third expr that approaches the maximum string length (currently 1MiB). Note that this function and $JUSTIFY() reserve 65 bytes for their actual formatting. Previously, a large specification for this amount could cause a segmentation violation (SIG-11). [GTM-DE376223](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-000_Release_Notes.html#GTM-DE376223)

* The following GT.M V7.1-00 issue was already addressed in YottaDB r1.32 and the following release note is included here for completeness.

  * <a name="GTM-DE376224"></a>Modulo of non-canonical number by a divisor greater than 999,999 returns a canonical result. Previously, it might not. [GTM-DE376224](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-000_Release_Notes.html#GTM-DE376224)

* <a name="GTM-DE376239"></a>YottaDB reports a [FALLINTOFLST](https://docs.yottadb.com/MessageRecovery/errors.html#fallintoflst) error after an argumentless DO embedded subroutine followed by a label with a formallist when no QUIT terminates the code after the DO block, except when there are no lines between the end of the embedded subroutine and the label with the formallist, in which case YottaDB inserts an implicit QUIT to separate them. When YottaDB inserts the implicit QUIT, it issues a FALLINTOFLST warning unless compilation has a NOWARNING qualifier. Previously, YottaDB inappropriately gave that error for cases of that combination under circumstances where the QUIT was on the same line as the argumentless DO rather than explicitly between the embedded subroutine and the label with the formallist. Note that this represents a different (and hopefully better) way of handling defect in application code than r2.02 did. [GTM-DE376239](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-000_Release_Notes.html#GTM-DE376239)

* The following GT.M V7.1-000 issue never occurred in any YottaDB release, and the following release note is included here for completeness.

  * <a name="GTM-DE388565"></a>YottaDB handles string literal operands to a Boolean string relational operator where the literal contains an exponential format appropriately. Previously such a combination inappropriately produced a [NUMOFLOW](https://docs.yottadb.com/MessageRecovery/errors.html#numoflow) error if the numeric evaluation would have produced an error. [GTM-DE388565](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-000_Release_Notes.html#GTM-DE388565)

* <a name="xGTM-DE493831"></a>YottaDB properly handles interrupts while jobbing off of a child process. Previously, in rare circumstances and related to timing, an interrupt could result in a deadlock. This was only seen in development and not reported by a customer. [GTM-DE493831](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-000_Release_Notes.html#GTM-DE493831)

### System Administration

* <a name="GTM-DE408789"></a>MUPIP BACKUP DATABASE attempts to use a faster copy mechanism depending on the support by the kernel, and by source and destination filesystems. If the source and destination filesystems are different or the faster copy mechanisms are not available in the kernel, MUPIP BACKUP DATABASE uses the default copy mechanism (`/bin/cp`). Previously, YottaDB used faster copy mechanisms only on Linux Kernel 5.3 or above, and changes due to backporting in Linux kernels could cause MUPIP BACKUP to report an EXDEV error on filesystems where backups had earlier been supported.

  MUPIP BACKUP ONLINE does not retry backup when it detects a concurrent rollback or on certain errors during the copy phase of BACKUP. Previously, MUPIP BACKUP ONLINE incorrectly retried backup when it encountered a concurrent rollback or an error in the first backup attempt; the workaround was to specify RETRY=0. [GTM-DE408789](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-000_Release_Notes.html#GTM-DE408789)

* <a name="xGTM-DE421008"></a>MUPIP STOP three times within a minute acts like a kill -9 by stopping a process even if it might not be safe to do so, except that it may produce a core file. Three MUPIP STOPs issued over a period of more than one minute terminate the process when it is safe to do so. Previously any three MUPIP STOPs over the life of a process acted like a kill -9, whether or not the three MUPIP STOPs were sent within 1 minute, and whether or not the process was at a safe point to be terminated. [GTM-DE421008](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-000_Release_Notes.html#GTM-DE421008)

* <a name="xGTM-F135385"></a>MUPIP RCTLDUMP reports the number of times a routine has been superseded (rtnsupersede) in the autorelink cache. Previously, MUPIP RTCLDUMP did not record this value, and only recorded the number of times a routine has been referenced. [GTM-F135385](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-000_Release_Notes.html#GTM-DE411385)

* <a name="xGTM-F135427"></a>YottaDB r2.04 provides the capability to upgrade a r1.x database to r2.x in-place. There is no ability to downgrade a r2.x database to r1.x in place. You can use MUPIP EXTRACT on r2.x and MUPIP LOAD on r1.x as long as the data does not cause the r1.x database file to exceed the r1.x maximum limits or revert to a prior version using a suitable combination of replicating instances. YottaDB r2.04 blocks all access to a r1.x database marked as not fully upgraded from V4 format.

  YottaDB r2.x databases differ from r1.x in the following ways. Please refer to the Administration and Operations Guide for more details about these differences.

  * Starting Virtual Block Number (VBN) is 8193, or slightly more on upgraded files in r2.x vs. 513 in r1.x.
  * Block numbers are 64-bit in r2.x, rather than 32-bit in r1.x.

  A YottaDB r2.x instance can originate BC/SI replication stream to or replicate from a r1.x BC/SI replication stream as long as the r2.x database remains within the maximum r1.x limits.

  The r1.x to r2.x database upgrade process is split into two phases intended to reduce the downtime necessary for a database upgrade. This process is considerably faster and consumes less disk space than a traditional extract, transfer and load cycle. Please refer to [Upgrading to YottaDB r2.04](releases#releases#upgrading-to-yottadb-r204) for more details. [GTM-F135427](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-000_Release_Notes.html#GTM-F135427)

### Other

* <a name="x1112"></a>[ydb_env_set](https://docs.yottadb.com/AdminOpsGuide/basicops.html#ydb-env-set) when run by multiple users from different environments all of which use the same YottaDB installation (i.e. `$ydb_dist`) works correctly. Previously, it was possible in rare cases for `ydb_env_set` to issue [GTMSECSHRSRVF](https://docs.yottadb.com/MessageRecovery/errors.html#gtmsecshrsrvf) errors. [#1112](https://gitlab.com/YottaDB/DB/YDB/-/issues/1112)

* <a name="GTM-DE325871"></a>YottaDB processes can use sockets created when over 1021 files, pipes, fifos, sockets, and/or regions are already open. YottaDB issues an FDSIZELMT error message when there are too many descriptors needed by GT.CM servers. Previously, sockets created when there were too many open descriptors caused an GTMASSERT2 error. [GTM-DE325871](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-000_Release_Notes.html#GTM-DE325871)

* <a name="GTM-DE422089"></a>YottaDB command line parser correctly terminates input with a null byte. Previously, in rare cases, the parser appended random characters for a PIPE device usage where a WRITE followed by the format control character "!" did not precede WRITE /EOF. This was seen only in development/testing and never reported by a user. Previously, YottaDB silently truncated shell arguments that exceeded these limits and did not produce an error when input to a utility prompt exceeded the allowed 33022 bytes. [GTM-DE422089](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-000_Release_Notes.html#GTM-DE422089)

* <a name="xGTM-F221672"></a>The SHMHUGETLB syslog warning message provides information about the operation of the calling process. Previously, SHMHUGETLB failure messages did not include operational information necessary to understand the reasons for such failures. [GTM-F221672](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-000_Release_Notes.html#GTM-F221672)

## Additional Information

(There is no Additional Information for r2.04.)

## Messages

### New Messages

**ARGTRUNC**, UUUU argument number CCCC truncated. Keep the size of total command line within NNNN bytes

DSE/LKE/MUPIP Warning: This warning appears when the YottaDB parser truncates an argument of a YottaDB Utility (DSE, LKE, or MUPIP) executable exceeding the allowed maximum of NNNN bytes. CCCC is the argument number with 1 being the first argument for the YottaDB Utility executable.

Action: Reduce the size of the argument number CCCC.

**DBFILERDONLY**, The database file ffff was opened as read-only (perms pppp)

All YottaDB Components Error: Database file ffff was opened read-only with permissions pppp, but the read-only status is inconsistent with application expectations.

Action: Use the error and any follow-on messages to assess whether or not the read-only status is correct or the rejection is appropriate.

**DBUPGRDREQ**, Database file DDDD is not fully upgraded (format FFFF) and cannot be used by this version of YottaDB. Please upgrade the database.

MUPIP Error: The database file DDDD with block format FFFF has the fully upgraded flag set to FALSE indicating that it holds a mix of block versions.

Action: While YottaDB r1.x can use database files with formats as old as GT.M V4.x, YottaDB starting with r2.x does not handle the V4 block format. Use YottaDB r1.x to fully upgrade the database file before using it.

**FDSIZELMT**, Too many nnnn descriptors needed by GT.CM server

GT.CM Error: A large number (nnnn) of regions accessed on behalf of GT.CM clients forced the file descriptor numerical value to its FD_SETSIZE limit. Under Linux as of this writing, this limit is fixed to 1024.

Action: Review the application, the database layout and the number of concurrent clients and adjust conditions to reduce the number of concurrent database files managed by the GT.CM server.

**FORCEDHALT2**, Receipt of 3 MUPIP STOP signals within xxxx seconds, process: yyyy shutting down with no exit handling like a kill -9

Run Time Fatal: This indicates that a YottaDB process recognized the receipt of three `MUPIP STOP <../AdminOpsGuide/dbmgmt.html#stop>`_ commands within approximately 60 seconds and is shutting down without normal clean up - very similar to a kill -9 signal. This event doesn't stop YottaDB processes in an orderly fashion, and might cause database damage if the target process is concurrently actively updating. Therefore use it only on processes that are deadlocked or otherwise stuck, say due to some type of FREEZE.

Action: Determine who initiated the MUPIP STOP and why they did so. Run `MUPIP INTEG <../AdminOpsGuide/dbmgmt.html#integ>`_ as appropriate.

**LINETOOLONG**, UUUU prompt input exceeds NNNN bytes

DSE/LKE/MUPIP Error: This error appears when the YottaDB parser detects the input to a YottaDB Utility (DSE, LKE, or MUPIP) prompt exceeds the allowed maximum of NNNN bytes.

Action: Reduce the size of input to the utility prompt. If input to the UUUU prompt is from a `PIPE device <../ProgrammersGuide/ioproc.html#using-pipe-devices>`_, set the `RECORDSIZE deviceparameter <../ProgrammersGuide/ioproc.html#pipe-deviceparameter-summary>`_ to a value less than NNNN bytes.

**ORLBKDBUPGRDREQ**, Region RRR (DDDD) is not fully upgraded. ONLINE ROLLBACK cannot continue

MUPIP Error: Region RRR pointing to database file DDDD has the fully upgraded flag set to FALSE and the database format is not r2.x indicating that there are GT.M V4 blocks in the database. `MUPIP JOURNAL ROLLBACK ONLINE <../AdminOpsGuide/ydbjournal.html#on-line>`_ in YottaDB r2.* cannot process these database files.

Action: Because a MUPIP JOURNAL ROLLBACK ONLINE is not possible for this database, stop all access to the database files and perform a ROLLBACK with standalone access.

**REORGUPCNFLCT**, MUPIP AAAA encountered a conflict due to OOOO (PID:PPPP)

MUPIP Error: MUPIP action AAAA encountered a conflict due to a concurrent operation OOOO run as process ID PPPP.

Action: MUPIP operations REORG UPGRADE and ONLINE ROLLBACK cannot run concurrently due to conflicting database changes. REORG UPGRADE exits if an ONLINE ROLLBACK is in progress or if it detects that an ONLINE ROLLBACK has started. ONLINE ROLLBACK pauses while waiting for the REORG UPGRADE to exit. ONLINE ROLLBACK has priority over REORG UPGRADE.

### Revised Messages

The following are updated messages:

**MUUPGRDNRDY**, Database xxxx has not been completely upgraded to yyyy format - still bbbb database blocks to upgrade

MUPIP Error: The named database file is in an older format than is in use by this YottaDB version and has not been certified as ready for use by this YottaDB version. There are still bbbb blocks in the older format that need to be upgraded.

Action: Run `MUPIP UPGRADE <../AdminOpsGuide/dbmgmt.html#mupip-upgrade>`_  to complete the upgrade.

**SHMHUGETLB**, Could not back shared memory with huge pages, using base pages instead ffff

Run Time Warning: When the `ydb_hugetlb_shm <../AdminOpsGuide/basicops.html#ydb-hugetlb-shm>`_ environment variable is defined and evaluates to a non-zero integer or any case-independent string or leading substring of "TRUE" or "YES" in a process creating shared memory, YottaDB attempts to back shared memory segments with hugepages, using the default hugepages size. If huge pages cannot be used, YottaDB outputs the SHMHUGETLB warning and tries to back the shared memory with base pages instead. The warning message specifies the operation of the caller along with the resource ffff for the process requesting shared memory. The warning message also includes either an ENOMEM or an EPERM error, depending on why the request for hugepages failed.

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
