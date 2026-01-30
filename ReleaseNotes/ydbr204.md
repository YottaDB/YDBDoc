<!---
.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2024-2026 YottaDB LLC and/or its subsidiaries.#
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

| Revision | Date | Summary               |
|----------|------|-----------------------|
| 1.0      |      | Initial Release Notes |

## Contact Information

### YottaDB LLC

https://yottadb.com /  <info@yottadb.com>

### Support

#### Customers

Contact YottaDB or your YottaDB support channel for support with assured service levels on commercial terms from knowledgeable staff.

#### Others

For free (to you) support from members of communities who run widely available applications on YottaDB, please use an application-specific list where appropriate.

 - Join the discussion on the [YottaDB Discord channel](https://discord.gg/bFHmDdzcqY).
 - For issues specific to the use of YottaDB from node.js via [Nodem](https://github.com/dlwicksell/nodem), [post an Issue on the Nodem project](https://github.com/dlwicksell/nodem/issues/new/).
 - For issues specific to the use of YottaDB from [QewdJS](http://qewdjs.com/), [EWD.js](https://github.com/robtweed/ewd.js), node.js via [mg-dbx](https://github.com/chrisemunt/mg-dbx), or [mg-dbx-napi](https://github.com/chrisemunt/mg-dbx-napi) post to the [Enterprise Web Developer community](https://groups.google.com/forum/#!forum/enterprise-web-developer-community).
 - For issues specific to the use of YottaDB with [VistA](https://en.wikipedia.org/wiki/VistA) flavors, post to the [Hardhats](https://groups.google.com/forum/#!forum/hardhats) list.
 - For issues specific to use of YottaDB with the [M Language](https://docs.yottadb.com/ProgrammersGuide/), post to the [Everything MUMPS](https://groups.google.com/g/everythingmumps) list.
 - For requests other than to the communities above [post an Issue](https://gitlab.com/YottaDB/DB/YDB/-/issues) and include the words "Help Wanted" in the Title.

## r2.04

### Overview

r2.04 is YottaDB's biggest release to date with significant ehancements to both performance and functionality. Highlights include:

* Performance and throughput improvements. See ([665](#x665)), ([1145](#x1145)), ([1178](#x1178)), and ([1181](#x1181)), as well as our blog post [Critical Section Performance in r2.04](https://yottadb.com/critical-section-performance-in-r2-04/).
* C and M functions to export (sub)trees as JSON strings and import JSON strings to (sub)trees. The JSON encoding handles (sub)trees that have a value at the root, which is not supported by traditional JSON. Except for reordering (keys do not need to be ordered in JSON; exports from YottaDB have ordered keys), "round-tripping" preserves document and (sub)tree structure. YottaDB requires [Jansson](https://github.com/akheron/jansson) to support this functionality. See ([474](#x474)) and ([1152](#x1152)).
* Numerous enhancements for ease of developing and debugging applications, for example:
  * ZSHOW "V" able to display variables at a specific stack level. ([873](#x873))
  * CMDLINE keyword parameter of $ZGETJPI() provides complete command line of process. ([876](#x876))
  * SET $ZROUTINES supports globbing of shared library filenames. ([974](#x974))
  * $ZYCOMPILE() intrinsic function checks whether a string is a syntactically correct line of M code. ([1138](#x1138))
  * %RSEL reports actual locations of routines and SILENT label accepts case-insensitive "OBJ" and "SRC". ([1162](#x1162))
  * In Kubernetes pods, Source Server connects reliably with Receiver Server. ([1191](#x1191))
* GT.M versions V7.1-000 through V7.1-002 are merged into r2.04.
* As always there are fixes to bugs and misfeatures, including to issues we found in our independent testing of the upstream GT.M versions.

Details are in the [Change History](#changehistory) section.

> [!note]
> ([1164](#x1164)) is not an upward-compatible change. Although we normally go the extra mile to ensure upward compatibility, in this case our judgment is that the performance gain justifies the change.

### Platforms

A platform is a combination of a CPU architecture and an operating system. A platform is Supported, Supportable, or Unsupported.

* Supported means that we have the platform in our development environment, test each release on that platform, and provide a binary distribution for it.
* Supportable means that although we may not have such a platform in our environment, we have no reason to believe that the software will not run on it.
* All others are Unsupported.

Effective r2.04, the 32-bit ARM platform is no longer Supported. If this is a problem for you, please [contact us](mailto:info@yottadb.com?subject=ARM32%20Support).

| CPU Architecture      | Supported OS Version(s)              | Notes                                        |
|-----------------------|--------------------------------------|----------------------------------------------|
| 64-bit x86            | * Ubuntu 24.04 LTS<br>* Red Hat Enterprise Linux 9.x & 10.x<br>* SUSE Linux Enterprise 15.x<br>* Debian GNU/Linux 13 (Trixie) | There are separate binary distributions for each OS version, owing to differences in the library versions of the distributions. |
| AARCH64 (64-bit ARM   | * Ubuntu 24.04 LTS<br>* Debian GNU Linux 13 (Trixie) | See below. |

 Recent releases of major 64-bit GNU/Linux distributions with contemporary kernels, glibc and ncurses are Supportable; however you may have to build YottaDB from source using the `--from-source` option of `ydbinstall.sh`. Specific notes:

- Supported filesystems are ext4 and xfs. f2fs is Supportable. btrfs, zfs, and NFS are Unsupported, and known to have issues.
- [ydbinstall.sh](https://gitlab.com/YottaDB/DB/YDB/-/blob/master/sr_unix/ydbinstall.sh) recognizes [Rocky Linux](https://rockylinux.org/) as equivalent to RHEL, and [OpenSUSE Leap](https://www.opensuse.org/#Leap) as equivalent to SUSE Linux Enterprise, installing the releases for the corresponding Supported distributions. Note that Rocky Linux and OpenSUSE Leap are Supportable, not Supported.
- On [Arch Linux](https://www.archlinux.org/) and other leading edge distributions such as [OpenSUSE Tumbleweed](https://www.opensuse.org/#Tumbleweed), as well as new versions of Linux distributions, YottaDB will need to be recompiled from source code owing to library and tool chain versions newer than those used in building Supported distributions. The `--from-source` option of [ydbinstall.sh](https://gitlab.com/YottaDB/DB/YDB/-/blob/master/sr_unix/ydbinstall.sh) simplifies the process.
- Owing to variations in the implementations of [AARCH66](https://en.wikipedia.org/wiki/AArch64) microarchitectures, we recommend that you ensure the software runs correctly before committing to any specific hardware. Please [contact us](mailto:info@yottadb.com?subject=AARCH64%20CPU%20and%20OS%20support) if you want a specific combination of OS and CPU microarchitecture to be Supported.
- The 64-bit ARM Ubuntu 24.04 LTS distributions are built on [Ubuntu Asahi](https://ubuntuasahi.org/) on Apple Mac Mini M2 hardware. See our blog post [The Strong ARM of Apple](https://yottadb.com/the-strong-arm-of-apple/) for more information.

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

YottaDB r2.04 is upward compatible from YottaDB [r2.02](https://gitlab.com/YottaDB/DB/YDB/-/releases/r2.02), GT.M [V7.1-000](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-000_Release_Notes.html), GT.M [V7.1-001](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-001_Release_Notes.html), and GT.M [V7.1-002](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-002_Release_Notes.html). The minimal upgrade steps are:

* Install YottaDB r2.04 using the [ydbinstall.sh](https://download.yottadb.com/ydbinstall.sh) script; or download the YottaDB distribution for your platform, and use the included `ydbinstall` script to install YottaDB. The scripts can install the YottaDB plugins that you use, along with installing YottaDB, or you can install or upgrade them later. The `--help` option of the script lists its options.
* Install plugins you need in addition to those installed in the previous step, e.g., non-YottaDB plugins or customized plugins.
* Cleanly shut down the application, as well as replication servers if replication is in use. Ensure that the database files are shut down using [MUPIP RUNDOWN](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#rundown) and routine shared memory with [MUPIP RUNDOWN RELINKCTL](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#relinkctl) from the prior release.
* Recompile object code, and recreate shared libraries if your application uses shared libraries. If you are not using shared libraries, YottaDB will atomatically recompile routines and replace object code files compiled with prior releases.
* Switch journal files with the new YottaDB release.
* Start using the new YottaDB release. If the instance is replicated, remember to start a Source Server process first.

If the database has triggers defined with non-canonical numbers, or numbers specified as strings with any version prior to [r1.28](https://gitlab.com/YottaDB/DB/YDB/-/releases/r1.28), or if you are unsure, extract the trigger definitions, delete existing triggers, and reload the trigger definitions. Issue [[#430](https://gitlab.com/YottaDB/DB/YDB/-/work_items/430)] from [r1.28](https://gitlab.com/YottaDB/DB/YDB/-/releases/r1.28) has a series of steps you can copy and execute. There is no need to do this if upgrading from [r1.28](https://gitlab.com/YottaDB/DB/YDB/-/releases/r1.28) or later.

To upgrade from older GT.M releases, first upgrade to GT.M V6.0-000 or later and follow the steps above, or contact your YottaDB support channel for assistance and guidance.

Use a rolling technique to upgrade production installations to r2.04.

* Create a new replicated instance of your application (on the same system or a different system). Let's call the live production system A and the replicated instance B.
* Install YottaDB r2.04 on B, and upgrade the replica on B to r2.04. Resume replicating from A to B.
* Once B catches up, switchover so that B is in a primary role replicating to A.
* Once you are satisfied with B, remove (or upgrade) A.

### Upgrading Global Directories

Opening a global directory file with [GDE](https://docs.yottadb.com/AdminOpsGuide/gde.html) and exiting automatically upgrades it to the latest format. As there is no way to downgrade the format of a global directory file, we recommend taking a backup before you upgrade it. The [GDE SHOW COMMAND](https://docs.yottadb.com/AdminOpsGuide/gde.html#show) outputs a text version of the global directory that you can use to recreate it, and which you can also use for global directory version control.

### Upgrading Database Files

Database files created with prior r2.x releases of YottaDB do not need to be explicitly upgraded. YottaDB r2.04 fully supports and uses database files created with r1.x releases of YottaDB.

> [!note]
> A database file can only be opened by processes of one YottaDB release at any given time.

In the event you need database files that need the 16Gi block limit or the 11 tree levels of r2.x (r1.x database files have a 992Mi block limit and 7 tree levels), you have two options:

* After a [MUPIP FREEZE](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#mupip-freeze) of the database, use [MUPIP EXTRACT](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#extract) to extract the database nodes, and [MUPIP LOAD](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#load) to load them into a new database file created by [MUPIP CREATE](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#create). A binary format will give you the fastest extract and load times. Note that you should use a MUPIP FREEZE across all regions of a database to ensure consistency of the database. The MUPIP EXTRACT and MUPIP LOAD processes can run concurrently for the regions.

* After a [MUPIP INTEG](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#integ) to verify the structural integrity of database files, use [MUPIP UPGRADE](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#mupip-upgrade) to upgrade the database file header to the 2.x format, followed by a [MUPIP REORG UPGRADE](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#upgrade) to upgrade the individual blocks. MUPIP REORG UPGRADE can run concurrently with normal database usage.

If you wish to use a database file created or used by r2.04 on a prior release of YottaDB or a GT.M version, contact your YottaDB support channel.

<a name="changehistory"></a>
## Change History

### r2.04

YottaDB r2.04 includes the following enhancements and fixes beyond YottaDB [r2.02](https://gitlab.com/YottaDB/DB/YDB/-/releases/r2.02).

| ID               | Category              | Summary                                                                                                |
|------------------|-----------------------|--------------------------------------------------------------------------------------------------------|
| ([474](#x474))   | Languages             | C API to serialize/deserialize local or global variable subtrees                                       |
| ([665](#x665))   | Languages             | Generate faster object code using the naked reference template                                         |
| ([858](#x858))   | Other                 | Additional address information with "generated from" in syslog messages                                |
| ([873](#x873))   | Languages             | ZSHOW "V" able to display variables at a specific stack levels                                         |
| ([876](#x876))   | Languages             | CMDLINE keyword parameter of $ZGETJPI() provides complete command line of process                      |
| ([883](#x883))   | Other                 | Ctrl-C typed at GDE prompt returns to caller or to shell                                               |
| ([917](#x917))   | System Administration | MUPIP prompts "File:" when command line is missing a required file or region specification             |
| ([918](#x918))   | Languages             | For processes started with a JOB command, $ZYJOBPARENT has pid of parent process                       |
| ([971](#x971))   | Languages             | Default $ZROUTINES includes plugin shared libraries                                                    |
| ([933](#x933))   | Languages             | JOB command cmdline parameter works if cmdline starts with dash (-)                                    |
| ([974](#x974))   | Languages             | SET $ZROUTINES supports globbing of shared library filenames                                           |
| ([1002](#x1002)) | System Administration | Setting BLKS_TO_UPGRADE=0 with DSE SET FILEHEADER also sets FULLY_UPGRADED=FALSE                       |
| ([1034](#x1034)) | System Administration | All MUPIP operations are case-insensitive regarding region names                                       |
| ([1052](#x1052)) | System Administration | MUPIP DUMPFHEAD FLUSH reports BUFFLUFAILED error when it lacks write permission for a database file    |
| ([1056](#x1056)) | Languages             | Pre-allocation for call-out (IO, not just O) string parameters                                         |
| ([1102](#x1102)) | Other                 | GNU style command-line options for yottadb                                                             |
| ([1111](#x1111)) | Languages             | Warning if a variable appears more than once in a NEW                                                  |
| ([1112](#x1112)) | Other                 | No GTMSECSHRSRVF and CRITSEMFAIL errors from ydb\_env\_set in certain rare cases                       |
| ([1122](#x1122)) | System Administration | Minor corrections to ydbinstall.sh                                                                     |
| ([1125](#x1125)) | System Administration | Appropriate permissions for $ydb_tmp and where appropriate, parent directory                           |
| ([1128](#x1128)) | System Administration | When ydb_readline=1, MUPIP STOP does not terminate DSE/LKE/MUPIP if they hold a critical section       |
| ([1129](#x1129)) | Languages             | UTF-8 mode $TRANSLATE() works correctly with long search string with multi-byte characters             |
| ([1130](#x1130)) | System Administration | GDE reports VALTOOBIG error when journal allocation parameter exceeds autoswitchlimit                  |
| ([1133](#x1133)) | Languages             | `-machine` compilation option                                                                          |
| ([1136](#x1136)) | Languages             | WRITE /TLS does not set $TEST if no TIMEOUT was specified                                              |
| ([1137](#x1137)) | System Administration | MUPIP JOURNAL ROLLBACK ONLINE PARALLEL=N works correctly when N is more than 5                         |
| ([1138](#x1138)) | Languages             | $ZYCOMPILE() intrinsic function checks whether a string is a syntactically correct line of M code      |
| ([1142](#x1142)) | Languages             | Object file generated when M program has two or more FALLINTOFLIST compilation warnings                |
| ([1145](#x1145)) | Languages             | Garbage collection with no sorting can improve performance at the cost of memory usage                 |
| ([1150](#x1150)) | Other                 | Boolean environment variables only accept substrings of yes, no, true, and false, but not superstrings |
| ([1152](#x1152)) | Languages             | M commands to serialize/deserialize local or global variable subtree                                   |
| ([1153](#x1153)) | Database              | Critial section wait counters incremented only if a process has to wait                                |
| ([1154](#x1154)) | System Administration | MUPIP RUNDOWN runs down database files even when replication instance files do not exist               |
| ([1156](#x1156)) | Languages             | INDEXTRACHARS message includes indirection string with errors                                          |
| ([1161](#x1161)) | Languages             | $ydb_ci not required when calling a previously called M function                                       |
| ([1162](#x1162)) | Languages             | %RSEL reports actual locations of routines and SILENT label accepts case-insensitive "OBJ" and "SRC"   |
| ([1164](#x1164)) | Languages             | ydb\_get\_s()/ydb\_get\_st() do not treat YDB\_ERR\_GVUNDEF and YDB\_ERR\_LVUNDEF return as errors     |
| ([1169](#x1169)) | System Administration | Retire MUNOSTRMBKUP                                                                                    |
| ([1170](#x1170)) | System Administration | ydbinstall --linkexec creates a symbolic link to yottadb                                               |
| ([1171](#x1171)) | Languages             | Change random number generator from xoshiro256+ to xoshiro256++                                        |
| ([1172](#x1172)) | Database              | MUPIP LOAD accepts ZWR format extracts that contain large binary data                                  |
| ([1178](#x1178)) | Database              | Speed-up code that executes inside and outside critical sections                                       |
| ([1180](#x1180)) | Languages             | YottaDB call-in functions clear $ECODE                                                                 |
| ([1181](#x1181)) | Languages             | JOB command as much as five times faster                                                               |
| ([1182](#x1182)) | Languages             | --embed-source of large routines works correctly                                                       |
| ([1185](#x1185)) | System Administration | SIGTERM removes relinkctl file shared memory segment even in certain rare cases                        |
| ([1191](#x1191)) | Other                 | In Kubernetes pods, Source Server connects reliably with Receiver Server                               |
| ([1192](#x1192)) | System Administration | MUPIP REPLICATE SOURCE commands remove only their own Journal Pool semaphores                          |
| ([1195](#x1195)) | Languages             | WRITE /WAIT on socket device with multiple listening sockets works correctly                           |
| ([1198](#x1198)) | Other                 | Compiler specification for ydbinstall                                                                  |
| ([1202](#x1202)) | System Administration | MUPIP BACKUP ONLINE produces valid backup file if backup and database are on separate filesystems      |
| ([1203](#x1203)) | Languages             | M programs with more than 8Ki distinct local variable names run on AARCH64                             |
| ([1204](#x1204)) | System Administration | MUPIP BACKUP DATABASE uses faster copy mechanism when available                                        |
| ([1205](#x1205)) | Languages             | Ensure all handled signals are enabled                                                                 |

<a name="gtmv71000"></a>
### GT.M V7.1-000

YottaDB r2.04 incorporates enhancements and fixes from [GT.M V7.0-000](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-000_Release_Notes.html).

| ID                              | Category              | Summary                                                                                                        |
|---------------------------------|-----------------------|----------------------------------------------------------------------------------------------------------------|
| ([GTM-DE325871](#GTM-DE325871)) | Other                 | Remove limit affecting sockets and improve error message where it still exists for GT.CM                       |
| ([GTM-DE340906](#GTM-DE340906)) | Languages             | Attempting a LOCK with more identical arguments than YottaDB supports for the command generates an error       |
| ([GTM-DE340950](#GTM-DE340950)) | Languages             | Exceeding the LOCK level limit for the same resource name generates a LOCKINCR2HIGH error                      |
| ([GTM-DE376223](#GTM-DE376223)) | Languages             | $FNUMBER() handles fill requests up to close to the maximum string length                                      |
| ([GTM-DE376224](#GTM-DE376224)) | Languages             | Modulo of non-canonical number by a divisor greater than 999,999 returns a canonical result                    |
| ([GTM-DE376239](#GTM-DE376239)) | Languages             | When YottaDB inserts an implicit QUIT to prevent a possible error, it generates a FALLINTOFLST WARNING message |
| ([GTM-DE388565](#GTM-DE388565)) | Languages             | Avoid inappropriate NUMFLOW from a literal Boolean argument with exponential (E) form                          |
| ([GTM-DE402020](#GTM-DE402020)) | Database              | Prevent Block SIG-11 splits under rare concurrency conditions involving empty string values                    |
| ([GTM-DE408789](#GTM-DE408789)) | System Administration | MUPIP BACKUP DATABASE uses faster copy mechanism when available                                                |
| ([GTM-DE421008](#GTM-DE421008)) | System Administration | Triple MUPIP STOP within a minute similar to, but slightly better than kill -9                                 |
| ([GTM-DE422089](#GTM-DE422089)) | Other                 | Improved detection and reporting of issues with utility command length and parsing                             |
| ([GTM-DE493831](#GTM-DE493831)) | Languages             | Prevent rare deadlock while using JOB command                                                                  |
| ([GTM-F135385](#GTM-F135385))   | System Administration | MUPIP RCTLDUMP reports the number of times a routine has been replaced (rtnsupersede) in the autorelink cache  |
| ([GTM-F135427](#GTM-F135427))   | System Administration | Support in-place conversion from V6 to V7 database formats                                                     |
| ([GTM-F221672](#GTM-F221672))   | Other                 | Additional context in SHMHUGETLB syslog message                                                                |

<a name="gtmv71001"></a>
### GT.M V7.1-001

YottaDB r2.04 incorporates enhancements and fixes from [GT.M V7.1-001](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-001_Release_Notes.html).

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

<a name="gtmv71001"></a>
### GT.M V7.1-002

YottaDB r2.04 incorporates enhancements and fixes from [GT.M V7.1-002](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-002_Release_Notes.html).

| ID                              | Category  | Summary                                                                                                                                   |
|---------------------------------|-----------|-------------------------------------------------------------------------------------------------------------------------------------------|
| ([GTM-DE324947](#GTM-DE324947)) | Languages | Correct error message when ydbtls\_passwd\_TLSID is not set                                                                               |
| ([GTM-DE533918](#GTM-DE533918)) | Languages | Error on READ or WRITE to a SOCKET device with no active sockets                                                                          |
| ([GTM-DE534846](#GTM-DE534846)) | Languages | $ZTIMEOUT presents the time remaining value to microsecond resolution                                                                     |
| ([GTM-DE538928](#GTM-DE538928)) | Admin     | Update Process properly closes prior generation journal files                                                                             |
| ([GTM-DE549071](#GTM-DE549071)) | Admin     | REORG traverses the database correctly and accepts restrictions on which levels it processes                                              |
| ([GTM-DE549072](#GTM-DE549072)) | Admin     | REORG succeeds in splitting any blocks that require a split                                                                               |
| ([GTM-DE549073](#GTM-DE549073)) | Admin     | REORG no longer accepts a combination of reserved bytes and fill factor which together target an impossible block size                    |
| ([GTM-DE556365](#GTM-DE556365)) | Admin     | Receiver Server continues to accept connections after a TLSCONVSOCK error                                                                 |
| ([GTM-DE556760](#GTM-DE556760)) | Admin     | MUPIP UPGRADE appropriately processes r1.x database files that exceed the maximum tree depth (7 levels) associated with pre-r2.x versions |
| ([GTM-F135405](#GTM-F135405))   | Admin     | Syslog message on Replication state when Journaling turns off                                                                             |
| ([GTM-F167609](#GTM-F167609))   | Other     | SOCKET Devices support TLSv1.3 Post Handshake Authentication                                                                              |
| ([GTM-F167995](#GTM-F167995))   | Languages | The YottaDB TLS plugin library exposes an external call interface providing cipher suite and version information                          |
| ([GTM-F197635](#GTM-F197635))   | Admin     | YottaDB supports independent index and data reserved bytes values                                                                         |
| ([GTM-F217678](#GTM-F217678))   | DB        | Online Rollback syslogs change to database logical state                                                                                  |
| ([GTM-F229760](#GTM-F229760))   | Languages | $ZICUVER provide the ICU version if available                                                                                             |
| ([GTM-F235980](#GTM-F235980))   | Admin     | YottaDB supports increased user control of TCP buffer sizing in replication                                                               |

### Database

* <a name="1172"></a>[MUPIP LOAD](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#load) of ZWR format extract files works even if the extracted nodes contain large binary data (e.g. close to the maximum string length of 1MiB). Previously, YottaDB could issue an [EXTRFMT](https://docs.yottadb.com/MessageRecovery/errors.html#extrfmt) error if the ZWRITE format representation of a large string exceeded 1MiB. [#1172](https://gitlab.com/YottaDB/DB/YDB/-/work_items/1172)

* <a name="x1153"></a>The [ZSHOW "G"](https://docs.yottadb.com/ProgrammersGuide/commands.html#zshow-information-codes) / [$VIEW("GVSTAT")](https://docs.yottadb.com/ProgrammersGuide/functions.html#argument-keywords-of-view) critical section wait statistics DEXA, GLB, JNL, MLK, PRC, TRX, ZAD, JOPA, AFRA, BREA, MLBA & TRGA are incremented only if a process does not get a critical section on its first attempt. In YottaDB releases r2.00 and r2.02, these counters were incorrectly incremented even when processes did not have to wait. [#1153](https://gitlab.com/YottaDB/DB/YDB/-/work_items/1153)

* <a name-"x1178"></a>Database code that executes inside and outside [critical sections](https://en.wikipedia.org/wiki/Critical_section) has been sped up, and critical section management has been significantly rewritten, as described in the blog post [Critical Section Performance in r2.04](https://yottadb.com/critical-section-performance-in-r2-04/). By default, YottaDB uses an adaptive mutex type with heuristic based switching between a YottaDB implementation and pthread_mutex operations provided by Linux. We recommend that you use the adaptive method unless you have a specific need to restrict critical section management, e.g., if you determine that a workload performs better with critical section management restricted to one or the other method. Even single process applications benefit from these optimizations. See [Additional Information](#addlydb1178) for more information. [#1178](https://gitlab.com/YottaDB/DB/YDB/-/work_items/1178)

* <a name="xGTM-DE402020"></a>YottaDB deals appropriately with a concurrency issue encountered when splitting a block, the record triggering the split has a zero-length value, concurrent changes make the previous record appear identical to the one triggering the split, and YottaDB attempts to calculate a parent key to demarcate the split. This apparently longstanding issue was detected by a customer using a stress test with the default proactive block split setting. While more likely with proactive block splits, the issue is difficult to reproduce without using carefully constructed update patterns. We have no indication that it has ever been previously reported by a customer or detected in our testing. Previously, the condition caused a process to fail with a segmentation violation (SIG-11) but did not result in any database damage. [GTM-DE402020](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-000_Release_Notes.html#GTM-DE402020)

* <a name="GTM-DE530712"></a>Proactive block split appropriately handles blocks with no records in them. Previously, under certain circumstances, proactive block split could try to induce a block split when the block had no records in it. This was only seen during development testing and not reported by a user. [GTM-DE530712](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-001_Release_Notes.html#GTM-DE530712)

* <a name="GTM-DE531077"></a>Trigger load operations involving many regions gracefully restart when a concurrent [MUPIP JOURNAL ROLLBACK ONLINE](https://docs.yottadb.com/AdminOpsGuide/ydbjournal.html#on-line) forces the process to restart transaction processing in each region. Previously, in such a situation, YottaDB processes could terminate with a GTMASSERT2 error. This error was only seen in development and not reported by a user. [GTM-DE531077](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-001_Release_Notes.html#GTM-DE531077)

* <a name="GTM-DE531078"></a>YottaDB processes working on the second phase of a [KILL](https://docs.yottadb.com/ProgrammersGuide/commands.html#kill) operation (KILL-In-Progress), issue a [DBROLLEDBACK](https://docs.yottadb.com/MessageRecovery/errors.html#dbrolledback) error in the event a [MUPIP JOURNAL ROLLBACK ONLINE](https://docs.yottadb.com/AdminOpsGuide/ydbjournal.html#on-line) changes the state of the database. Previously, YottaDB processes would restart until the fourth retry acquiring the critical section on all participating regions before issuing such an error. This error was only seen in development and not reported by a user. [GTM-DE531078](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-001_Release_Notes.html#GTM-DE531078)

* <a name="GTM-DE532295"></a>YottaDB disables proactive block splitting by default as well as within a TP transaction. YottaDB made this change after observing that, under certain conditions, the setting could incur spurious restarts and split blocks which were not subject to contention. To enable the feature outside of transaction processing, use a [MUPIP SET -PROBLKSPLIT=n](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#problksplit), where n is the number of nodes at which YottaDB considers based on the number of restarts whether to split a block in advance of its becoming full. Previously, starting in r2.00, the default threshold for a proactive block split was 5 nodes and the feature applied to TP as well as non-TP. The performance issue was only ever observed in testing and not reported by a user; it was not associated with any correctness or integrity concerns. [GTM-DE532295](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-001_Release_Notes.html#GTM-DE532295)

* <a name="GTM-F217678"></a>When [MUPIP JOURNAL ROLLBACK ONLINE](https://docs.yottadb.com/AdminOpsGuide/ydbjournal.html#rollback-on-line-noo-nline) changes the logical state of a database it issues an [ORLBKROLLED](https://docs.yottadb.com/MessageRecovery/errors.html#orlbkrolled) message to the system log. Previously, MUPIP did not announce this state change in the system log. [GTM-F217678](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-002_Release_Notes.html#GTM-F217678)

### Languages

* <a name="x474"></a>The `ydb_encode_s()` / `ydb_encode_st()` functions serialize a (sub)tree into a JSON string (a serialized object or array) in an industry standard encoding suitable for communication or transmission to another application. The `ydb_decode_s()` and `ydb_decode_st()` parse a JSON string and store the result in a (sub)tree. See [Additional Information](#addlydb474) for more information. [#474](https://gitlab.com/YottaDB/DB/YDB/-/work_items/474)

* <a name="x665"></a>The compiler detects certain patterns of normal global variable references and generates the faster [naked reference](https://docs.yottadb.com/ProgrammersGuide/langfeat.html#naked-references) object code, including some cases where M syntax does not allow for naked references. Specifically:

  * Consecutive references where subsequent references could be replaced by a naked reference, as long as the subscripts are constants or unsubscripted local variables, and there is no intervening command that modifies any local variables, e.g., `SET ^X(y,1)=2 WRITE ^X(y,1)`.
  * References with a changing number of subscripts, e.g., `SET ^X(1)=1,^X(1,2)=2`.
  * Arbitrary expressions used as the last subscript, e.g., `SET ^X(y,1)=2,a=-2 WRITE ^X(1,@("a+3"))`.

  The generated code and runtime system ensure correct behavior of the application code even when there is an intervening command that transfers control between the initial and subsequent references, e.g., handling a [MUPIP INTRPT](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#intrpt) signal. Previously these patterns of global variable references generated normal object code for global variable references. [#665](https://gitlab.com/YottaDB/DB/YDB/-/work_items/665#note_3047778551)

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

  If the information codes of a ZSHOW command with a stack specification do not include "V", the stack specification is silently ignored. Other ZSHOW information codes ignore the stack specification, e.g., ZSHOW "vs"::2 (assuming $STACK>2) displays the local variables for $STACK level 2, as well as the current M stack. [#873](https://gitlab.com/YottaDB/DB/YDB/-/work_items/873)

* <a name="x876"></a>
A case-independent `"CMDLINE"` second parameter for the [$ZGETJPI()](https://docs.yottadb.com/ProgrammersGuide/functions.html#zgetjpi) function returns the command line of the process specified by the first parameter. YottaDB reads this information from `/proc/<pid>/cmdline` and if that file does not exist or the process calling $ZGETJPI() does not have permission to read that file, the function returns the empty string (`""`). [#876](https://gitlab.com/YottaDB/DB/YDB/-/work_items/876)

* <a name="x918"></a>For processes started with the [JOB](https://docs.yottadb.com/ProgrammersGuide/commands.html#job) command, the [$ZYJOBPARENT](https://docs.yottadb.com/ProgrammersGuide/isv.html#zyjobparent) intrinsic special variable provides the pid of the process which executed the JOB command. For processes not started with the JOB command, $ZYJOBPARENT is 0. [#918](https://gitlab.com/YottaDB/DB/YDB/-/work_items/918)

* <a name="x971"></a>The value YottaDB uses for `$ZROUTINES` if neither `ydb_routines` nor `gtmroutines` is set includes the shared libraries of plugins installed under the `$ydb_dist/plugins` subdirectory for the M/UTF-8 mode of the process. Previously, the default `$ZROUTINES` only included the `libyottadbutil.so` shared library for that mode. [#971](https://gitlab.com/YottaDB/DB/YDB/-/work_items/971)

* <a name="x933"></a>If the [cmdline](https://docs.yottadb.com/ProgrammersGuide/commands.html#cmd-line-strlit) parameter passed to the [JOB](https://docs.yottadb.com/ProgrammersGuide/commands.html#job) command starts with a dash, the jobbed process starts. Previously, it failed with a [CLIERR](https://docs.yottadb.com/MessageRecovery/errors.html#clierr) error. [#933](https://gitlab.com/YottaDB/DB/YDB/-/work_items/933)

* <a name="x974"></a>`SET $ZROUTINES` accepts [glob](https://man7.org/linux/man-pages/man7/glob.7.html) patterns for shared library filenames. The patterns must end in `.so`. Patterns are not supported for directories or object filenames. For example:

  ```
  YDB>write $zroutines
  /extra/usr/local/lib/yottadb/r203/libyottadbutil.so
  YDB>set $zroutines="$ydb_dist/plugin/o/*.so"_" "_$zroutines write $zroutines
  /extra/usr/local/lib/yottadb/r203/plugin/o/_ydbaim.so /extra/usr/local/lib/yottadb/r203/plugin/o/_ydbgui.so /extra/usr/local/lib/yottadb/r203/plugin/o/_ydbmwebserver.so /extra/usr/local/lib/yottadb/r203/plugin/o/_ydbocto.so /extra/usr/local/lib/yottadb/r203/plugin/o/_ydbposix.so /extra/usr/local/lib/yottadb/r203/plugin/o/_ydbsyslog.so /extra/usr/local/lib/yottadb/r203/libyottadbutil.so
  YDB>
  ```

  [#974](https://gitlab.com/YottaDB/DB/YDB/-/work_items/974)

* <a name="x1056"></a>A pre-allocation size may be specified for input-output (IO) string parameters in the [external call-out table](https://docs.yottadb.com/ProgrammersGuide/extrout.html#using-external-calls-call-outs), e.g. `IO:ydb_buffer_t*[100]`. Pre-allocation for `ydb_char_t *`, `ydb_string_t *`, and `ydb_buffer_t *` types was previously available for output (O) parameters but with this enhancement may also be specified for input-output (IO) parameters. Previously, an external function could only populate IO parameter strings up to the size of the input string. With pre-allocation, a maximum size may optionally be specified in the external call table to allocate more return space than the input string size. Note that IO `ydb_char_t*` and `ydb_string_t*` parameters do not allow querying the pre-allocation size at runtime; For robust applications, we recommend using `ydb_buffer_t*` for external calls.

  - Pre-allocation for non-string parameters in the external call-out table produces a [ZCPREALLVALSTR](https://docs.yottadb.com/MessageRecovery/errors.html#zcpreallvalstr) error. Previously, specifying a pre-allocation size for non-string parameters was silently ignored by YDB. Note that specifying the allocated space for non-string parameters is meaningless, as sizes are determined by the platform.
  - For IO parameters of type `ydb_string_t*` and `ydb_buffer_t*`, YDB distinguishes the empty string from skipped arguments in external callouts, treating the empty string consistently with non-empty strings. Previously, passing an empty string would behave as if the parameter were omitted altogether.
	- Note that `ydb_char_t*` still represents skipped strings as a 0-length null-terminated string. We recommend using `ydb_buffer_t*` for IO parameters to avoid ambiguity.
  - For O parameters, YDB distinguishes skipped arguments from other kinds of arguments in callouts. Previously, there would be no indication that the argument had been skipped and the C code was forced to assume an M actual value was present.
  - YDB passes skipped `ydb_string_t*` parameters as a `ydb_string_t { .length = 0, .address = NULL }` pair, for both IO and O parameters. Previously it only did so for skipped IO parameters, whereas skipped O parameters were passed as an empty string, which did not match the documentation.

  See [Using External Calls: Call-Outs](https://docs.yottadb.com/ProgrammersGuide/extrout.html#using-external-calls-call-outs) for more detail about how pre-allocated IO parameters are represented in C. [#1056](https://gitlab.com/YottaDB/DB/YDB/-/work_items/1056)

* <a name="x1111"></a>A variable that appears more than once in the same NEW command raises a compile-time [DUPVAR](https://docs.yottadb.com/MessageRecovery/errors.html#dupvar) warning. [#1111](https://gitlab.com/YottaDB/DB/YDB/-/work_items/1111)

* <a name="x1129"></a>[\$TRANSLATE()](https://docs.yottadb.com/ProgrammersGuide/functions.html#translate) works correctly in UTF-8 mode (i.e., [$ZCHSET](https://docs.yottadb.com/ProgrammersGuide/isv.html#zchset) is `"UTF-8"`) when the search string (second parameter) contains more than 256 characters and includes at least one non-ASCII character. Previously, effective YottaDB release r1.36, this would fail with a fatal [GTMASSERT2](https://docs.yottadb.com/MessageRecovery/errors.html#gtmassert2) error. [#1129](https://gitlab.com/YottaDB/DB/YDB/-/work_items/1129)

* <a name="x1133"></a>YottaDB compilation supports a `-machine` flag (see [Qualifiers for the yottadb command](https://docs.yottadb.com/ProgrammersGuide/devcycle.html#qualifiers-for-the-yottadb-command)) which produces a listing that shows the generated assembly code for each line of source code. Specifying `-machine` automatically turns on the [-list](https://docs.yottadb.com/ProgrammersGuide/devcycle.html#no-li-st-filename) option if the latter is not explicitly specified. [#1133](https://gitlab.com/YottaDB/DB/YDB/-/work_items/1133)

* <a name="x1136"></a>[WRITE /TLS](https://docs.yottadb.com/ProgrammersGuide/ioproc.html#write-command) does not set [$TEST](https://docs.yottadb.com/ProgrammersGuide/isv.html#test) in case no timeout was specified. In YottaDB releases r1.30 to r2.02, $TEST was set even in this case whereas it should have been set only if a timeout was specified. [#1136](https://gitlab.com/YottaDB/DB/YDB/-/work_items/1136)

* <a name="x1138"></a>The function $ZYCOMPILE(str) verifies whether or not `str` is valid M code. An empty string is returned if `str` is syntactically valid M code; otherwise a string of the form `POS,YDB_ERROR,Error` is returned where

  * POS - is the position in the line where parsing stopped
  * YDB_ERROR - YottaDB Standard Error Code Mnemonic
  * Error - Error description

    See [Examples](https://gitlab.com/YottaDB/DB/YDB/-/merge_requests/1656#examples-of-zycompile) for example usages.

    Note that the intrinsic function is very similar to the M Utility Function [$$^%ZMVALID()](https://docs.yottadb.com/ProgrammersGuide/utility.html#zmvalid) but the intrinsic function is a lot faster (in an experiment it was found to be 100,000 times faster) and does not create any `.m` and `.o` files. [#1138](https://gitlab.com/YottaDB/DB/YDB/-/work_items/1138)

* <a name="x1142"></a>An object file is generated when an M program has two or more FALLINTOFLIST compilation warnings. Previously, a program with two FALLINTOLIST compilation warnings would fail to generate an object file. [#1142](https://gitlab.com/YottaDB/DB/YDB/-/work_items/1142)

* <a name="x1145"></a>After executing a [VIEW "STP_GCOL_NOSORT":1](https://docs.yottadb.com/ProgrammersGuide/commands.html#keywords-in-view-command), subsequent garbage collections avoid sorting. VIEW "STP_GCOL_NOSORT":0 switches to garbage collections with sorting. This requires that VIEW "STP_GCOL" use the full name of the option; abbreviations such as VIEW "STP" no longer work.

    Avoiding sorting may improve garbage collection times, likely at the cost of increased memory usage. [$VIEW("SPSIZESORT")](https://docs.yottadb.com/ProgrammersGuide/functions.html#argument-keywords-of-view) performs a garbage collection, and returns a comma-separated list of 2 integers which indicate the memory usage (in bytes) of the stringpool with the unsorted and the sorted approaches respectively. If an application finds the unsorted value to be within its memory limits, it will likely benefit from the reduced runtime by switching to the unsorted approach (see https://gitlab.com/YottaDB/DB/YDB/-/work_items/1145#note_2507097811 for examples). \$VIEW("SPSIZE") must now write out the full name of the option; abbreviations such as \$VIEW("SPS") no longer work.

    The environment variable [ydb_stp_gcol_nosort](https://docs.yottadb.com/AdminOpsGuide/basicops.html#environment-variables) can be set to 0 or 1 (or any positive integer value) to initially choose the sorted or unsorted approach respectively by applications, defaulting to the sorted approach if the variable is not defined. Subsequent to startup, VIEW "STP_GCOL_NOSORT" commands can change the approach. [$VIEW("STP_GCOL_NOSORT")](https://docs.yottadb.com/ProgrammersGuide/functions.html#argument-keywords-of-view) returns a value of 0 if garbage collections use the sorted approach and 1 otherwise. Previously, garbage collections always used the sorted approach. [#1145](https://gitlab.com/YottaDB/DB/YDB/-/work_items/1145)

* <a name="x1172"></a>The ZYENCODE command serializes a (sub)tree into a JSON string (a serialized object or array) in an industry standard encoding suitable for communication or transmission to another application. The ZYDECODE command parses a JSON string and stores the result in a (sub)tree. See [Aditional Information](#addlydb1172) for more information. [#1152](https://gitlab.com/YottaDB/DB/YDB/-/work_items/1152)

* <a name="x1156"></a>The [INDEXTRACHARS](https://docs.yottadb.com/MessageRecovery/errors.html#indextrachars) error message includes the indirection string with the extra characters, which makes debugging easier. Previously it did not. [#1156](https://gitlab.com/YottaDB/DB/YDB/-/work_items/1156)

* <a name="x1161"></a>YottaDB does not require the environment variable [ydb_ci (GTMCI)](https://docs.yottadb.com/AdminOpsGuide/basicops.html#ydb-ci) to be set when the C API is used to call an M function previously called with [ydb\_cip()](https://docs.yottadb.com/ProgrammersGuide/extrout.html#ydb-cip) or [ydb\_cip\_t()](https://docs.yottadb.com/ProgrammersGuide/extrout.html#ydb-cip-t). Previously it issued a [CITABENV](https://docs.yottadb.com/MessageRecovery/errors.html#citabenv) error in this case. [#1161](https://gitlab.com/YottaDB/DB/YDB/-/work_items/1161)

* <a name="x1162"></a>The [\%RSEL](https://docs.yottadb.com/ProgrammersGuide/utility.html#rsel) utility program follows symbolic links to report the actual locations of routines. Also, the `label` parameter of SILENT^%RSEL() is case-insensitive. Previously, %RSEL did not follow symbolic links and `label` had to be upper case. [#1162](https://gitlab.com/YottaDB/DB/YDB/-/work_items/1162)

* <a name="x1164"></a>[ydb\_get\_s() / ydb\_get\_st()](https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#ydb-get-s-ydb-get-st) do not populate [$ZSTATUS](https://docs.yottadb.com/ProgrammersGuide/isv.html#zstatus) in case of [YDB\_ERR\_GVUNDEF](https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#ydb-err-gvundef) and [YDB\_ERR\_LVUNDEF](https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#ydb-err-lvundef) return codes. Previously they used to populate $ZSTATUS and this could be retrieved by a subsequent call to [ydb\_zstatus()](https://docs.yottadb.com/ProgrammersGuide/extrout.html#ydb-zstatus). While this is not an upward compatible change, a significant performance gain made it worthwhile, especially because in this case, application code would typically just check the return code, since a subsequent check of $ZSTATUS has no additional benefit. Note that this change affects all non-M language APIs, e.g., the Go [ValE()](https://docs.yottadb.com/MultiLangProgGuide/goprogram.html#go-vale) function. [#1164](https://gitlab.com/YottaDB/DB/YDB/-/work_items/1164)

* <a name="x1171"></a>The randomness of numbers generated by [$RANDOM()](https://docs.yottadb.com/ProgrammersGuide/functions.html#random) is enhanced by using the xoshiro256++ algorithm. Previously it used the xoshiro256+ algorithm (see [Scrambled Linear Pseudorandom Number Generators](https://vigna.di.unimi.it/ftp/papers/ScrambledLinear.pdf) for details). Note that random numbers generated by $RANDOM(), while possessing excellent statistical properties, are not suitable for cryptographic applications. [#1171](https://gitlab.com/YottaDB/DB/YDB/-/work_items/1171)

* <a name="x1180"></a>[ydb\_ci\_t()](https://docs.yottadb.com/ProgrammersGuide/extrout.html#ydb-ci-t) and related functions clear [$ECODE](https://docs.yottadb.com/ProgrammersGuide/isv.html#ecode). They never start in an error state and they clear any errors before returning to the caller. Functions affected are ydb\_ci(), ydb\_cip(), gtm\_ci(), gtm\_cip(), ydb_ci\_t(), and ydb\_cip\_t(). Previously, when when such a call set $ECODE, the process would remain in an error state, which could impact subsequent call-ins. The workaround was for the application to ensure that $ECODE was cleared. [#1180](https://gitlab.com/YottaDB/DB/YDB/-/work_items/1180)

* <a name="x1181"></a>The [JOB](https://docs.yottadb.com/ProgrammersGuide/commands.html#job) command is as much as five times faster than it was previously. This slowdown was an unintended side effect of changes to [GTM-9058](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-007_Release_Notes.html#GTM-9058) in GT.M V6.3-007 which was merged into YottaDB [r1.26](https://gitlab.com/YottaDB/DB/YDB/-/releases#r126-1). As a practical matter, you may not notice this unless you are launching hundreds, or even thousands, of processes in rapid succession. [#1181](https://gitlab.com/YottaDB/DB/YDB/-/work_items/1181)

* <a name="x1182"></a>The embedded source code of routines compiled in M mode with the [--embed-source](https://docs.yottadb.com/ProgrammersGuide/devcycle.html#no-embed-source) option are no longer garbled on rare occasions. In r2.02, they could be garbled owing to the garbage collector running during the compilation. As a practical matter, this was only rarely observed with unusually large routines, and never with normal routines. It was only observed in the development environment and never reported by a user. [#1182](https://gitlab.com/YottaDB/DB/YDB/-/work_items/1182)

* <a name="x1195"></a>[WRITE /WAIT(timeout)](https://docs.yottadb.com/ProgrammersGuide/ioproc.html#write-command) on a [SOCKET](https://docs.yottadb.com/ProgrammersGuide/ioproc.html#using-socket-devices) device with *n* (*n*>1) listening sockets works correctly. In r2.00 and r2.02, owing to a regression in GT.M V7.0-001, after clients have connected to all listening sockets, the first *n* WRITE /WAIT(timeout) commands would time out correctly, after which the next WRITE /WAIT(timeout) command would hang and consume 100% of a CPU. [#1195](https://gitlab.com/YottaDB/DB/YDB/-/work_items/1195)

* <a name="x1203"></a>M programs that contain more than 8Ki distinct local variable names now run on AARCH64 systems. Previously, on AARCH64, it was possible for such programs to terminate at run-time with various errors (SIG-11, LVUNDEF, SIGILLOPC etc.). YottaDB builds on x86_64 did not have this issue. [#1203](https://gitlab.com/YottaDB/DB/YDB/-/work_items/1203)

* <a name="x1205"></a>YottaDB processes ensure that all signals that they handle are enabled (not blocked in the signal mask). Previously, if a YottaDB process was started with one or more signals disabled (blocked), they were not re-enabled (cleared) by YottaDB. The specific case reported by a user was that of USR1 being disabled when a `yottadb` process was started by an [Apache web server](https://httpd.apache.org/) process. [#1205](https://gitlab.com/YottaDB/DB/YDB/-/work_items/1205)

* <a name="GTM-DE324947"></a>In the event of failure to acquire the TLS password, the YottaDB encryption plugin issues an appropriate error message. Previously, in some cases, failure to acquire the TLS password resulted in a garbled error message. [GTM-DE324947](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-002_Release_Notes.html#GTM-DE324947)

* <a name="GTM-DE340906"></a>YottaDB appropriately handles a command with multiple (more than 255) [LOCKs](https://docs.yottadb.com/ProgrammersGuide/commands.html#lock) with the same name. Previously, a YottaDB command that created more than 255 LOCKs with the same name caused a segmentation violation (SIG-11). [GTM-DE340906](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-000_Release_Notes.html#GTM-DE340906)

* <a name="GTM-DE340950"></a>An attempt by a process to incrementally [LOCK](https://docs.yottadb.com/ProgrammersGuide/commands.html#lock) the same resource name more than 511 times produces a [LOCKINCR2HIGH](https://docs.yottadb.com/MessageRecovery/errors.html#lockincr2high) with accurate context. Previously LOCK processing did not appropriately detect the limit or supply correct context. [GTM-DE340950](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-000_Release_Notes.html#GTM-DE340950)

* The following GT.M V7.1-00 issue was already addressed in YottaDB r1.34 and the following release note is included here for completeness.

  * <a name="GTM-DE376223"></a>[$FNUMBER()](https://docs.yottadb.com/ProgrammersGuide/functions.html#fnumber) reserves appropriate memory to handle a third expr that approaches the maximum string length (currently 1MiB). Note that this function and [$JUSTIFY()](https://docs.yottadb.com/ProgrammersGuide/functions.html#justify) reserve 65 bytes for their actual formatting. Previously, a large specification for this amount could cause a segmentation violation (SIG-11). [GTM-DE376223](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-000_Release_Notes.html#GTM-DE376223)

* The following GT.M V7.1-00 issue was already addressed in YottaDB r1.32 and the following release note is included here for completeness.

  * <a name="GTM-DE376224"></a>Modulo of non-canonical number by a divisor greater than 999,999 returns a canonical result. Previously, it might not. [GTM-DE376224](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-000_Release_Notes.html#GTM-DE376224)

* <a name="GTM-DE376239"></a>YottaDB reports a [FALLINTOFLST](https://docs.yottadb.com/MessageRecovery/errors.html#fallintoflst) error after an argumentless [DO](https://docs.yottadb.com/ProgrammersGuide/commands.html#do) embedded subroutine followed by a label with a formallist when no [QUIT](https://docs.yottadb.com/ProgrammersGuide/commands.html#quit) terminates the code after the DO block, except when there are no lines between the end of the embedded subroutine and the label with the formallist, in which case YottaDB inserts an implicit QUIT to separate them. When YottaDB inserts the implicit QUIT, it issues a FALLINTOFLST warning unless compilation has a NOWARNING qualifier. Previously, YottaDB inappropriately gave that error for cases of that combination under circumstances where the QUIT was on the same line as the argumentless DO rather than explicitly between the embedded subroutine and the label with the formallist. Note that this represents a different (and hopefully better) way of handling defects in application code than r2.02 did. [GTM-DE376239](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-000_Release_Notes.html#GTM-DE376239)

* The following GT.M V7.1-000 issue never occurred in any YottaDB release, and the following release note is included here for completeness.

  * <a name="GTM-DE388565"></a>YottaDB handles string literal operands to a Boolean string relational operator where the literal contains an exponential format appropriately. Previously such a combination inappropriately produced a [NUMOFLOW](https://docs.yottadb.com/MessageRecovery/errors.html#numoflow) error if the numeric evaluation would have produced an error. [GTM-DE388565](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-000_Release_Notes.html#GTM-DE388565)

* <a name="GTM-DE493831"></a>YottaDB properly handles interrupts while jobbing off a child process. Previously, in rare circumstances and related to timing, an interrupt could result in a deadlock. This was only seen in development and not reported by a user. [GTM-DE493831](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-000_Release_Notes.html#GTM-DE493831)

* <a name="GTM-DE500856"></a>[\%RANDSTR()](https://docs.yottadb.com/ProgrammersGuide/utility.html#randstr) limits the range argument upper limit to the actual number of characters available in the current character set - 256 for M mode and 65,536 for UTF-8 mode. Previously, a missing or defective upper limit caused the routine to perform unproductive processing that could consume unreasonable amounts of time. The workaround was to avoid inappropriate range arguments. [GTM-DE500856](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-001_Release_Notes.html#GTM-DE500856)

* <a name="GTM-DE500860"></a>The YottaDB compiler accepts source files with arbitrary extensions, or even no extension. YottaDB recommends using the `.m` extension for source files as our testing of that is very extensive, however there may be cases where other extensions serve a purpose. Previously, the compiler enforced an explicit or implicit .m extension for source files. [GTM-DE500860](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-001_Release_Notes.html#GTM-DE500860)

  > [!note]
  > The GT.M enhancement allowed extensions other than `.m`. YottaDB further enhanced the compiler to allow filenames with no extension.

* The following was fixed in r2.02, and the below release note appears here for completeness.

  * <a name="GTM-DE507982"></a>YottaDB produces an appropriate result from an equality test between zero (0) and a multiplicand of zero value that is the result of multiplying a non-integer value with zero (0). Previously, this odd combination could inappropriately indicate inequality. [GTM-DE507982](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-001_Release_Notes.html#GTM-DE507982)

* <a name="GTM-DE508852"></a>YottaDB appropriately reports a [NUMOFLOW](https://docs.yottadb.com/MessageRecovery/errors.html#numoflow) error for literal values when those literals are operated on at compile-time and the run-time variable equivalent would trigger the same error. This affects the unaryoperators \', +, and -. The effect of these operators on variables is unchanged. Previously, these operators sometimes did not correctly report a NUMOFLOW when called for. In the case of the negation operator, this could lead to reporting an incorrect result. [GTM-DE508852](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-001_Release_Notes.html#GTM-DE508852)

* <a name="GTM-DE510902"></a>YottaDB prevents compile-time errors in operations on literals within an [XECUTE](https://docs.yottadb.com/ProgrammersGuide/commands.html#xecute) block from terminating the XECUTE without properly cleaning up the surrounding compilation environment. Previously, this could cause termination of compilation of the routine containing the XECUTE and failure to compile subsequent routines passed to the same YottaDB process [GTM-DE510902](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-001_Release_Notes.html#GTM-DE510902)

* As the following defect in the GT.M code base was found and fixed by the YottaDB team when merging the V7.0-002 code base, the following was never an issue in YottaDB and appears here for completeness.

  * <a name="GTM-DE511969"></a>The Direct Mode [RECALL](https://docs.yottadb.com/ProgrammersGuide/opdebug.html#command-recall) command functions as documented; starting in V7.0-002 it always gave an error. [GTM-DE511969](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-001_Release_Notes.html#GTM-DE511969)

  > [!note]
  > For a more powerful direct mode, YottaDB recommends setting [$ydb_readline](https://docs.yottadb.com/AdminOpsGuide/basicops.html#ydb-readline) to use [GNU Readline](https://www.gnu.org/software/readline/).

* <a name="GTM-DE512004"></a>SET @($ZWRTAC=expr) works for strings approaching 1MiB; previously such an indirect expression was limited to the maximum length of a source line. Also, the [\%ZSHOWVTOLCL](https://docs.yottadb.com/ProgrammersGuide/utility.html#zshowvtolcl) utility routine now deals with more cases, such as such longer strings. [GTM-DE512004](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-001_Release_Notes.html#GTM-DE512004)

* <a name="GTM-DE513737"></a>YottaDB returns the correct value for a Boolean expression containing a subscripted local variable when that variable is affected by side-effects later in the expression and [$ydb_boolean](https://docs.yottadb.com/AdminOpsGuide/basicops.html#ydb-boolean)>=1. Previously, YottaDB evaluated the boolean using the value of the subscripted local after all side-effects. This issue did not affect unsubscripted local variables or globals of any kind. [GTM-DE513737](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-001_Release_Notes.html#GTM-DE513737)

* <a name="GTM-DE513980"></a>YottaDB correctly executes the [$ETRAP](https://docs.yottadb.com/ProgrammersGuide/isv.html#etrap)/[$ZTRAP](https://docs.yottadb.com/ProgrammersGuide/isv.html#ztrap) exception handler at the time of expiry of [$ZMAXTPTIME](https://docs.yottadb.com/ProgrammersGuide/isv.html#zmaxtptime) when the process holds a database critical section. Previously, due to a regression in r2.00, the $ZMAXTPTIME timer did not execute $ETRAP/$ZTRAP exception handler until the process released all database critical sections which could allow a transaction to materially exceed the specified $ZMAXTPTIME. [GTM-DE513980](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-001_Release_Notes.html#GTM-DE513980)

* <a name="GTM-DE519525"></a>YottaDB defers [$ZTIMEOUT](https://docs.yottadb.com/ProgrammersGuide/isv.html#ztimeout) when its expiry happens during a TP transaction. [$ZMAXTPTIME](https://docs.yottadb.com/ProgrammersGuide/isv.html#zmaxtptime) may interrupt a transaction, as optionally may [MUPIP INTRPT](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#intrpt) initiated [$ZINTERRUPT](https://docs.yottadb.com/ProgrammersGuide/isv.html#zinterrupt) processing, but $ZTIMEOUT acts after a [TROLLBACK](https://docs.yottadb.com/ProgrammersGuide/commands.html#trollback) or outermost [TCOMMIT](https://docs.yottadb.com/ProgrammersGuide/commands.html#tcommit). Previously, YottaDB allowed a $ZTIMEOUT expiry within a TP transaction. [GTM-DE519525](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-001_Release_Notes.html#GTM-DE519525)

* <a name="GTM-DE525624"></a>Because it is a byte-oriented function, [$ZTRANSLATE()](https://docs.yottadb.com/ProgrammersGuide/functions.html#ztranslate) does not issue a [BADCHAR](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#intrpt) error when operating on UTF-8 strings. Due to a regression in r1.36, this function previously incorrectly issued a BADCHAR error when the input string contained non-UTF-8 characters. There were two possible workarounds for this issue: 1) A targeted approach requiring code changes to enclose each use of $ZTRANSLATE() with [VIEW "NOBADCHAR"](https://docs.yottadb.com/ProgrammersGuide/commands.html#no-badchar) and VIEW "BADCHAR", which ensures all UTF-8 data is handled appropriately. 2) A broad approach of defining [$ydb_badchar](https://docs.yottadb.com/AdminOpsGuide/basicops.html#ydb-badchar) as zero or FALSE to disable BADCHAR checking through-out the application which disables detection of any improperly formatted UTF-8 strings. [GTM-DE525624](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-001_Release_Notes.html#GTM-DE525624)

* <a name="GTM-DE533918"></a>YottaDB issues a [NOSOCKETINDEV](https://docs.yottadb.com/MessageRecovery/errors.html#nosocketindev) error the first time a process in direct mode attempts to read from or write to a socket principal device with no socket descriptors, and issues a fatal [NOPRINCIO](https://docs.yottadb.com/MessageRecovery/errors.html#noprincio) error upon a subsequent attempt. Previously a process that entered direct mode with an empty socket principal device would loop indefinitely and consume CPU resources. [GTM-DE533918](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-002_Release_Notes.html#GTM-DE533918)

* <a name="GTM-DE534846"></a>$ZTIMEOUT presents the time remaining value to microsecond resolution; previously it only showed time with resolution in milliseconds or less. [GTM-DE534846](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-002_Release_Notes.html#GTM-DE534846)

* <a name="GTM-F135040"></a>Specifying a second expression for [$VIEW("JNLPOOL")](https://docs.yottadb.com/ProgrammersGuide/functions.html#argument-keywords-of-view) provides a means of iterating through active Journal Pools. If the second expression is an empty string, the function returns the replication instance file name associated with the instance first attached by the process or the string "\*" if the process has not previously engaged with any instance. If the file name specified in the second expression does not match the replication instance file name for any of the active Journal Pools the string "\*" is returned. Otherwise the file name of the Journal Pool attached after the Journal Pool with the file name given in the second expression is returned. Note the two argument form of $VIEW("JNLPOOL") does not change the current Replication Instance. [GTM-F135040](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-001_Release_Notes.html#GTM-F135040)

* <a name="GTM-F167995"></a>The [encryption plugin](https://docs.yottadb.com/AdminOpsGuide/encryption.html) exposes an external call interface providing ciphersuite and version information. The four new functions are:

  ```
  getversion:                     xc_long_t gtm_tls_get_version(O:xc_char_t*[2048])
  gettlslibsslversion:            xc_long_t gtm_tls_get_TLS_version(O:xc_char_t*[2048],I:xc_char_t*,O:xc_char_t*[2048])
  getdefaultciphers:              xc_long_t gtm_tls_get_defaultciphers(O:xc_char_t*[4096],I:xc_char_t*,O:xc_char_t*[2048])
  getciphers:                     xc_long_t gtm_tls_get_ciphers(O:xc_char_t*[4096],I:xc_char_t*,I:xc_char_t*,I:xc_char_t*,I:xc_char_t*,O:xc_char_t*[2048])
  ```

  The following entry points provide the supported cipher suite information. Except where noted, the [$ydb\_crypt\_conf](https://docs.yottadb.com/AdminOpsGuide/basicops.html#ydb-crypt-config) configuration file and [$ydb\_tls\_passwd\_<TLS_ID>](https://docs.yottadb.com/AdminOpsGuide/basicops.html#ydb-tls-passwd-label) are not required.

  * getdefaultciphers

	* 1st parameter contains the default list of ciphers based on the 2nd parameter
	* 2nd parameter directs the interface to report the OpenSSL default cipher suite for TLSv1.2 ("tls1\_2") or TLSv1.3 ("tls1\_3")
	* 3rd parameter is an error string (allocated by the external call interface).
    * The function returns negative as failure and positive for the number of colon delimited pieces in the return string.

  * getciphers

	* 1st parameter contains the list of available ciphers based on the 2nd parameter
	* 2nd parameter directs the interface to report the OpenSSL default cipher suite for TLSv1.2 ("tls1\_2") or TLSv1.3 ("tls1\_3")
	* 3rd parameter directs the interface to report the cipher suite using the cipher suite defaults for "SOCKET" Device or "REPLICATION" server
	* (optional) 4th parameter directs the interface to use the name TLS ID from the $gtmcrypt\_conf configuration file. Using the null string makes $gtmcrypt\_config optional. Using a TLS ID with certificates requires $ydb\_tls\_passwd\_<TLS ID>
	* (optional) 5th parameter directs the interface to use the supplied cipher suite string when determining supported ciphers
	* 6th parameter is an error string (allocated by the external call interface)
	* The function returns negative as failure and positive for the number of colon delimited pieces in the return string

  The following entry points provide version information.

  * getversion

	* 1st parameter contains the GT.M TLS plugin version as a string.
	* The function returns the GT.M TLS plugin version as a number.

  * gettlslibversion

	* 1st parameter contains the OpenSSL string
	* 2nd parameter directs the function to report the "run-time" or "compile-time" OpenSSL version
	* 3rd parameter is an error string (allocated by the external call interface)
	* The function returns the OpenSSL version number or negative on failure

  [GTM-F167995](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-002_Release_Notes.html#GTM-F167995)

* <a name="GTM-F229760"></a>The [$ZICUVER](https://docs.yottadb.com/ProgrammersGuide/isv.html#zicuver) Intrinsic Special Variable provides the current [International Components for Unicode (ICU)](https://icu.unicode.org/) version or an empty string if ICU is not available. YottaDB requires ICU to support UTF-8 operation. Previously, YottaDB did not make this information available to the application code. [GTM-F229760](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-002_Release_Notes.html#GTM-F229760)

### System Administration

* <a name="x917"></a>A [MUPIP](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html) command whose command line is missing a required file or region specification will prompt `File:`, and accept a database file name in response. Previously, it would prompt `File or Region:` but would only accept a database file name in response. [#917](https://gitlab.com/YottaDB/DB/YDB/-/work_items/917)

* <a name="x1052"></a>MUPIP DUMPFHEAD FLUSH reports a [BUFFLUFAILED](https://docs.yottadb.com/MessageRecovery/errors.html#bufflufailed) for any database file for which it does not have write permission and continue with the next region. Previously, it would fail with a [DBFILERR](https://docs.yottadb.com/MessageRecovery/errors.html#dbfilerr) or a [CRITSEMFAIL](https://docs.yottadb.com/MessageRecovery/errors.html#critsemfail) error and terminate. [#1052](https://gitlab.com/YottaDB/DB/YDB/-/work_items/1052)

* <a name="x1122"></a>`ydbinstall.sh --from-source` passes the `ydbinstall` script of YottaDB it builds with the `--nocopyenv`, `--nocopyexec`, `--nolinkenv`, `--nolinkexec`, `--nopkg-config` and `--support` options. Previously it did not, which meant they were ignored when the built YottaDB was installed. It also makes the default output a little less verbose. [#1122](https://gitlab.com/YottaDB/DB/YDB/-/work_items/1122)

* <a name="#x1125"></a>Several changes make sourcing `ydb_env_set` behave better in certain instances with multiple users of an instance and/or multiple releases of YottaDB.

  * %YDBENV, which is invoked by `ydb_env_set`, creates a directory for \$ydb\_tmp (defaulting to \$gtm\_tmp if that is defined, and `/tmp/yottadb/$ydb_rel` if neither is defined) if one does not exist. Previously, it created the directory based on \$ydb\_log/\$gtm\_log if they were defined, environment variables that YottaDB does not use.
  * %YDBENV attempts to set the group of \$ydb\_tmp to the group of \$ydb\_dist/libyottadb.so. Previously it did not, which was problematic: especially if the process creating \$ydb\_tmp was root, then subsequent ordinary processes would be unable to use it. It does this unconditionally (which makes the code marginally simpler) because it is important if YottaDB is installed with the `--group-restriction --group <group>` options of `ydbinstall`, and doesn't matter otherwise.
  * If the lowest level subdirectory of \$ydb\_tmp is \$ydb\_rel, %YDBENV assumes that the parent directory is intended for subdirectories of multiple YottaDB releases, and attempts to set the permissions of the parent directory to `drwxrwxrwt` to reduce the likelihood of processes of different YottaDB releases from stepping on one anothers' toes. Previously, it did not attempt such a distinction, and did not set the sticky bit.
  * If \$ydb\_log/\$gtm\_log are not defined, it sets them to \$ydb\_tmp. Although YottaDB does not use these environment variables, older scripting around GT.M/YottaDB might use them.

  [#1125](https://gitlab.com/YottaDB/DB/YDB/-/work_items/1125)

* <a name="x1128"></a>A [MUPIP STOP](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#stop) does not terminate DSE, LKE and MUPIP processes if they hold the critical section for a database file. Previously, if [$ydb_readline](https://docs.yottadb.com/AdminOpsGuide/basicops.html#ydb-readline) was `1` or an equivalent value, it would incorrectly terminate the process. Note that a sequence of three MUPIP STOP signals sent within one minute continue to terminate the process even if it holds a critical section. [#1128](https://gitlab.com/YottaDB/DB/YDB/-/work_items/1128)

* <a name="x1002"></a>When [DSE CHANGE FILEHEADER](https://docs.yottadb.com/AdminOpsGuide/dse.html#change) is used to reset the value of BLKS_TO_UPGRADE, the FULLY_UPGRADED field is set appropriately. If BLKS_TO_UPGRADE is set to 0, then FULLY_UPGRADED is set to TRUE. Previously FULLY_UPGRADED was ALWAYS set to FALSE. [#1002](https://gitlab.com/YottaDB/DB/YDB/-/work_items/1002)

* <a name="x1034"></a>All MUPIP operations accept regions names in a case-insensitive way. Previously some did (e.g., [MUPIP SET](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#set)) and some did not (e.g., [MUPIP EXTEND](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#extend)). [#1034](https://gitlab.com/YottaDB/DB/YDB/-/work_items/1034)

* <a name="x1130"></a>[GDE](https://docs.yottadb.com/AdminOpsGuide/gde.html) reports a [VALTOOBIG](https://docs.yottadb.com/MessageRecovery/errors.html#valtoobig) error when the value for the journal allocation parameter exceeds the autoswitchlimit. Previously, it reported an incorrect [CMD](https://docs.yottadb.com/MessageRecovery/errors.html#cmd) error. [1130](https://gitlab.com/YottaDB/DB/YDB/-/work_items/1130)

* <a name="x1137"></a>[MUPIP JOURNAL ROLLBACK ONLINE PARALLEL=N](https://docs.yottadb.com/AdminOpsGuide/ydbjournal.html#para-llel-n) works correctly when N is more than 5. Previously, it was possible in very rare cases for it to issue a fatal [MAXRTSERRDEPTH](https://docs.yottadb.com/MessageRecovery/errors.html#maxrtserrdepth) error and create a core file. [#1137](https://gitlab.com/YottaDB/DB/YDB/-/work_items/1137)

* <a name="x1154></a">If the replication instance file pointed to by [$ydb_repl_instance](https://docs.yottadb.com/AdminOpsGuide/basicops.html#ydb-repl-instance) / $gtm_repl_instance does not exist, [MUPIP RUNDOWN](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#rundown) nevertheless attempts to rundown database files. Previously, it would raise a [REPLINSTACC](https://docs.yottadb.com/MessageRecovery/errors.html#replinstacc) error, and not rundown the database files. [#1154](https://gitlab.com/YottaDB/DB/YDB/-/work_items/1154)

* <a name="x1169"></a>[MUPIP BACKUP BYTESTREAM](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#bytestream) is able to backup database regions with all block sizes. Previously it only backed up regions with block sizes up to 32256 bytes, and issued a [MUNOSTRMBKUP](https://docs.yottadb.com/MessageRecovery/errors.html#munostrmbkup) error for regions with larger block sizes. [MUPIP CREATE](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#create) no longer issues a MUNOSTRMBKUP warning when creating a region with block size greater than 32256 bytes, which it did previously. [#1169](https://gitlab.com/YottaDB/DB/YDB/-/work_items/1169)

* <a name="x1170"></a>The `--linkexec` option of `ydbinstall` / `ydbinstall.sh` creates a symbolic link from the specified (or default) directory to `$ydb_dist/yottadb`. Previously, while it created symbolic links to `ydb`, `ydbsh` and `gtm` it did not do so for `yottadb`. Note that if `$ydb_routines` / `$gtmroutines` is not set, `yottadb` sets [\$ZROUTINES](https://docs.yottadb.com/ProgrammersGuide/isv.html#zroutines) to a default value. [#1170](https://gitlab.com/YottaDB/DB/YDB/-/work_items/1170)

* <a name="x1185"></a>A SIGTERM (signal 15) sent to a YottaDB process (including MUPIP and DSE) runs down any [relinkctl file](https://docs.yottadb.com/AdminOpsGuide/basicops.html#ydb-linktmpdir) shared memory segments that the process is attached to or in the process of attaching to. Previously, it was possible in rare cases for a process to leave relinkctl file shared memory segments orphaned in the system in case it received a SIGTERM within a small window of time while trying to open a relinkctl file. This was only observed in the development environment and never reported by a user. [#1185](https://gitlab.com/YottaDB/DB/YDB/-/work_items/1185)

* <a name="x1192"></a>MUPIP REPLICATE SOURCE commands (e.g., [MUPIP REPLICATE SOURCE CHECKHEALTH](https://docs.yottadb.com/AdminOpsGuide/dbrepl.html#checkhealth)) do not remove any Journal Pool semaphores that they did not create. Previously, such a command could remove a semaphore created by a concurrent [MUPIP JOURNAL ROLLBACK](https://docs.yottadb.com/AdminOpsGuide/ydbjournal.html#rollback-on-line-noo-nline) command, causing subsequent MUPIP REPLICATE SOURCE commands to fail with a [REPLREQROLLBACK](https://docs.yottadb.com/MessageRecovery/errors.html#replreqrollback) error. [#1192](https://gitlab.com/YottaDB/DB/YDB/-/work_items/1192)

* <a name="x1202"></a>[MUPIP BACKUP ONLINE](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#online) produces valid backup files when the database and backup destination are on different filesystems, and there are concurrent updates. In r2.02, it was possible for the backup database files to have database errors ([DBTNTOOLG](https://docs.yottadb.com/MessageRecovery/errors.html#dbtntoolg), [DBMRKBUSY](https://docs.yottadb.com/MessageRecovery/errors.html#dbmrkbusy), etc.) in such circumstances. This was a regression introduced in [GT.M V7.0-004](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-004_Release_Notes.html#GTM-F166755), which was merged into r2.02, and fixed in [GT.M V7.1-000](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-000_Release_Notes.html#GTM-DE408789), which was merged into r2.04. The bug and fix were not mentioned in any GT.M release note. [#1202](https://gitlab.com/YottaDB/DB/YDB/-/work_items/1202#note_2991971090)

* <a name="x1204"></a>See [GTM-DE408789](#GTM-DE408789). The upstream GT.M functionality existed only on x86_64 systems. YottaDB extended it to also work on AARCH64 systems. [#1204](https://gitlab.com/YottaDB/DB/YDB/-/work_items/1204)

* <a name="GTM-DE408789"></a>[MUPIP BACKUP DATABASE](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#database) attempts to use a faster copy mechanism depending on the support by the kernel, and by source and destination filesystems. If the source and destination filesystems are different or the faster copy mechanisms are not available in the kernel, MUPIP BACKUP DATABASE uses the default copy mechanism (`/bin/cp`). Previously, YottaDB used faster copy mechanisms only on Linux Kernel 5.3 or above, and changes due to backporting in Linux kernels could cause MUPIP BACKUP to report an EXDEV error on filesystems where backups had earlier been supported. [MUPIP BACKUP ONLINE](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#mupip-backup-online) does not retry backup when it detects a concurrent rollback or on certain errors during the copy phase of BACKUP. Previously, MUPIP BACKUP ONLINE incorrectly retried backup when it encountered a concurrent rollback or an error in the first backup attempt; the workaround was to specify RETRY=0. [GTM-DE408789](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-000_Release_Notes.html#GTM-DE408789)

* <a name="GTM-DE421008"></a>[MUPIP STOP](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#stop) three times within a minute acts like a kill -9 by stopping a process even if it might not be safe to do so, except that it may produce a core file. Three MUPIP STOPs issued over a period of more than one minute terminate the process when it is safe to do so. Previously any three MUPIP STOPs over the life of a process acted like a kill -9, whether or not the three MUPIP STOPs were sent within 1 minute, and whether or not the process was at a safe point to be terminated. [GTM-DE421008](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-000_Release_Notes.html#GTM-DE421008)

* <a name="GTM-DE538928"></a>The Update Process properly closes prior generation journal files. Previously, due to a regression in r1.24, it was possible for the Update Process to miss closing a prior journal file when a [MUPIP SET](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#set) switched a journal file and the Update Process was waiting to receive updates from the Receiver Server. The workaround was to avoid explicitly switching journal files on the replicating instances. [GTM-DE538928](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-002_Release_Notes.html#GTM-DE538928)

* <a name="GTM-DE549071"></a>[MUPIP REORG](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#reorg) traverses all index blocks and achieves a compact and optimally-structured database with a single pass. Previously, MUPIP REORG failed to visit certain categories of blocks, including the root block and blocks it newly created or modified, and it required an indefinite number of passes to achieve optimal block structure. As a workaround for previous releases, users may consider repeating REORG operations until the command reports few blocks coalesced and split. In addition, REORG now recognizes a new qualifier, `-min_level=n`, which specifies the minimum level of block it may process. The default is `-min_level=0`. `-min_level=1` instructs reorg only to process index blocks and can be understood as the REORG equivalent of a [MUPIP INTEG FAST](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#fast). [GTM-DE549071](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-002_Release_Notes.html#GTM-DE549071)

* <a name="GTM-DE549072"></a>[MUPIP REORG](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#reorg) correctly handles block splits at the index level which are caused by a reorg-related block split at a lower-level index or data block. Previously, YottaDB would sometimes fail to split these blocks, which prevented the original lower data-level or index-level split from taking place. Together, these could prevent REORG from enforcing the provided fill factor and/or reserved bytes setting; a failure which persisted on all subsequent attempts, or until the database structure changed as a result of new updates. As a workaround, users of previous releases can address a subset of the failures-to-split by passing a slightly different fill factor, provided that reserved bytes are disabled. [GTM-DE549072](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-002_Release_Notes.html#GTM-DE549072)

* <a name="GTM-DE549073"></a>[MUPIP REORG](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#reorg) enforces a minimum target block size of the database block size minus the maximum reserved bytes. Previously, YottaDB failed to reject a combination of reserved bytes and [FILL FACTOR](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#fill-factor) which effectively reserved a space larger than the database block size and caused database damage. Users of previous releases should use either reserved bytes or fill factor, but not both, as a means of achieving REORG sparseness. [GTM-DE549073](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-002_Release_Notes.html#GTM-DE549073)

* <a name="GTM-DE556365"></a>When a Source Server fails to establish a TLS connection, the Receiver Server reports a [TLSCONVSOCK](https://docs.yottadb.com/MessageRecovery/errors.html#tlsconvsock) error message and terminates (see discussion at https://gitlab.com/YottaDB/DB/YDBTest/-/work_items/687#note_3109611733). The following GT.M change was not merged into r2.04.

    The Receiver Server, configured to use TLS, continues waiting for new connections when a Source Server fails to establish a TLS session (e.g. misconfiguration). Previously, a Receiver Server configured for TLS but without [PLAINTEXTFALLBACK](https://docs.yottadb.com/AdminOpsGuide/dbrepl.html#no-plaintextfallback) would exit with a [TLSCONVSOCK](https://docs.yottadb.com/MessageRecovery/errors.html#tlsconvsock) error message when it failed to establish a TLS session. [GTM-DE556365](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-002_Release_Notes.html#GTM-DE556365)

* <a name="GTM-DE556760"></a>[MUPIP UPGRADE](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#mupip-upgrade) correctly handles global variable trees with depths greater than supported by r1.x releases, but possible when running an r2.x release on a 1.x database. Previously, attempts to upgrade r1.x databases containing these tall global variable trees would result in a segmentation violation and the process would terminate early without upgrading the database. [GTM-DE556760](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-002_Release_Notes.html#GTM-DE556760)

* <a name="GTM-F135405"></a>YottaDB issues [JNLCLOSED](https://docs.yottadb.com/MessageRecovery/errors.html#jnlclosed) and [REPLSTATE](https://docs.yottadb.com/MessageRecovery/errors.html#replstate) syslog messages for a database file when journaling is closed and the replication status changes from ON to WAS\_ON. In the WAS\_ON state, replication continues until and unless replication requires records that are no longer available from the replication Journal Pool, as it can no longer use the journal files to find those records. While in this WAS\_ON state if you can resume journaling, you may be able to avoid having to refresh the associated Secondary Instances. The need to use journal files in replication is a function of the size of the replication Journal Pool, the rate of updates and operator actions that impact replication. Previously, YottaDB issued a [REPLJNLCLOSED](https://docs.yottadb.com/MessageRecovery/errors.html#repljnlclosed) message when journaling was disabled for a database file, but did not indicate that the replication status changed to WAS\_ON. [GTM-F135405](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-002_Release_Notes.html#GTM-F135405)

* <a name="GTM-F135385"></a>[MUPIP RCTLDUMP](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#rctldump) reports the number of times a routine has been superseded (rtnsupersede) in the autorelink cache. Previously, MUPIP RTCLDUMP did not record this value, and only recorded the number of times a routine has been referenced. [GTM-F135385](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-000_Release_Notes.html#GTM-F135385)

* <a name="xGTM-F135427"></a>YottaDB r2.04 provides the capability to upgrade a r1.x database to r2.x in-place. There is no ability to downgrade a r2.x database to r1.x in place. You can use [MUPIP EXTRACT](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#extract) on r2.x and [MUPIP LOAD](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#load) on r1.x as long as the data does not cause the r1.x database file to exceed the r1.x maximum limits, or revert to a prior version using a suitable combination of replicating instances. YottaDB r2.04 blocks all access to a r1.x database marked as not fully upgraded from the V4 format.

  YottaDB r2.x databases differ from r1.x in the following ways. Please refer to the Administration and Operations Guide for more details about these differences.

  * Starting Virtual Block Number (VBN) is 8193, or slightly more, on upgraded files in r2.x, vs. 513 in r1.x.
  * Block numbers are 64-bit in r2.x, rather than 32-bit in r1.x.

  A YottaDB r2.x instance can originate BC/SI replication stream to or replicate from a r1.x BC/SI replication stream as long as the r2.x database remains within the maximum r1.x limits.

  The r1.x to r2.x database upgrade process is split into two phases intended to reduce the downtime necessary for a database upgrade. This process is considerably faster and consumes less disk space than a traditional extract, transfer and load cycle. Please refer to [Upgrading to YottaDB r2.04](releases#releases#upgrading-to-yottadb-r204) for more details. [GTM-F135427](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-000_Release_Notes.html#GTM-F135427)

* <a name="GTM-F197635"></a>The INDEX\_RESERVED\_BYTES and DATA\_RESERVED\_BYTES qualifiers for [MUPIP SET](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#set) (and [DSE CHANGE FILEHEADER](https://docs.yottadb.com/AdminOpsGuide/dse.html#change-fileheader-qualifiers)) allow independent adjustment of reserved bytes for each block type. Previously YottaDB did not provide the flexibility to set these values independently. The RESERVED\_BYTES qualifier continues to adjust both types of block to the same value. When the command specifies RESERVED\_BYTES along with one of the more specific qualifiers, MUPIP applies the more general RESERVED\_BYTES value to the block type unspecified by the other qualifier. [MUPIP DUMPFHEAD](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#dumpfhead) reports the number of bytes reserved for each type of block (as does [DSE DUMP FILEHEADER](https://docs.yottadb.com/AdminOpsGuide/dse.html#f-ileheader)). Reserving additional bytes in index blocks can reduce the number of records in any given index block and may reduce invalidation and search restarts in some workloads. [GTM-F197635](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-002_Release_Notes.html#GTM-F197635)

* <a name="GTM-F225097"></a>[MUPIP REORG UPGRADE](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#upgrade), which completes the second phase of an r1.x (V6) to r2.x (V7) database transition, can run concurrently with other processing excepting other MUPIP REORG UPGRADE processes. MUPIP REORG UPGRADE can work either by region or by file allowing administrator to run concurrent upgrade operations on different regions/files. MUPIP DOWNGRADE -VERSION=V63000A allows the current YottaDB to downgrade a V6 database to the pre-V63000A EOF block format. Previously the downgrade function reported an unsupported error for V7 versions. [GTM-F225097](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-001_Release_Notes.html#GTM-F225097)

* <a name="GTM-F235980"></a>YottaDB recognizes the [NO]{SEND|RECV}BUFFSIZE=n qualifiers for use with MUPIP REPLICATE {SOURCE|RECEIVER} START invocations. These qualifiers affect the TCP send and receive buffers associated with the socket YottaDB uses for replication, rather than the Receive/Journal Pools. When invoked with {SEND|RECV}BUFFSIZE=n, where n is a positive decimal or hexadecimal (0x) integer, YottaDB attempts to increase the specified socket buffer size to match the provided value. If the specified buffer is already larger than the provided value, YottaDB does not attempt to reduce its size. By default, YottaDB attempts to increase the size of the send buffer (SO\_SNDBUF) or receive buffer (SO\_RCVBUF) if either is smaller than the internal target value specified below.

  |          | `SO_SNDBUF` | `SO_RCVBUF` |
  |----------|-------------|-------------|
  | Source   | 1MiB        | 1KiB        |
  | Receiver | 1KiB        | 1MiB        |

  If a user requests a size for either buffer smaller than necessary to support YottaDB replication, YottaDB will print a BUFFSIZETOOSMALL warning, act as if the minimum size had been specified instead, and continue. Due to a quirk in how the Linux kernel reports socket buffer sizes, users of YottaDB can expect YottaDB to report a final size approximately twice what is requested; the additional space is used internally by the kernel. When invoked with NO{SEND|RECV}BUFFSIZE, YottaDB leaves the management of the initial size of the specified buffer to the execution environment, including the system defaults, local configuration settings, and operating system. In some cases, such as when the operating system dynamically manages the size of the relevant buffers, this can may lead to better performance in conditions which call for larger sizes than the YottaDB default. Previously YottaDB enforced a fixed minimum size for each buffer and did not permit the explicit request of receive and send buffer sizes. In addition, YottaDB now correctly sets the buffer sizes, when warranted, before establishing a connection. Previously, YottaDB waited until after the source and receiver had established a connection to perform any modification of the buffer sizes, which prevented the TCP connection from taking full advantage of any increase in size. [GTM-F236066](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-002_Release_Notes.html#GTM-F236066) [GTM-F235980](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-002_Release_Notes.html#GTM-F235980)

### Other

* <a name="x858"></a>In a syslog message, the absolute location in the process that produced the message is followed by the base address of the shared library from which the code is executed followed by its offset within the library, For example:

  ```
  Mar 16 10:27:51 mypc YDB-MUPIP[44596]: %YDB-I-DBFREEZEON, Database file /tmp/test/r2.03_x86_64/g/yottadb.dat is FROZEN (NOOVERRIDE NOONLINE NOAUTOREL) -- generated from 0x00007FEDDDAC1D3B (0x00007FEDDD91A000+0x00000000001A7D3B).
  ```

  When feasible, the program will print a symbolic\_function\_name+offset; however this is not common. Previously, YottaDB printed only the absolute location. Also, previously, the absolute location could be incorrect. [#858](https://gitlab.com/YottaDB/DB/YDB/-/work_items/858())

* <a name="x883"></a>A Ctrl-C typed at a [GDE](https://docs.yottadb.com/AdminOpsGuide/gde.html) prompt to a session invoked directly from the shell (e.g., `yottadb -run GDE`) returns to the shell, and to one called from application code, returns to the caller. Previously, it would drop into the YottaDB direct mode. [#883](https://gitlab.com/YottaDB/DB/YDB/-/work_items/883)

* <a name="x1102"></a>The `yottadb` command accepts `--direct`, `--help`, `--run`, and `--version` as equivalent to `-direct`, `-help`, `-run` and `-version` respectively. [#1102](https://gitlab.com/YottaDB/DB/YDB/-/work_items/1102)

* <a name="x1112"></a>[ydb\_env\_set](https://docs.yottadb.com/AdminOpsGuide/basicops.html#ydb-env-set) when run by multiple users from different environments all of which use the same YottaDB installation (i.e. [$ydb\_dist](https://docs.yottadb.com/AdminOpsGuide/basicops.html#ydb-dist)) works correctly. Previously, it was possible in rare cases for `ydb_env_set` to issue [GTMSECSHRSRVF](https://docs.yottadb.com/MessageRecovery/errors.html#gtmsecshrsrvf) errors. [#1112](https://gitlab.com/YottaDB/DB/YDB/-/work_items/1112)

* <a name="x1150"></a>Boolean [environment variables](https://docs.yottadb.com/AdminOpsGuide/basicops.html#environment-variables) accept only leading case-insensitive substrings of `yes`, `no`, `true` or `false` as equivalent to specifying the corresponding full string values. Previously they would accept superstrings too (e.g. `yesx`, `yesxy`, `yesxyz` would all be treated as `yes`). See [YDBDoc#448](https://gitlab.com/YottaDB/DB/YDBDoc/-/work_items/448) for a list of environment variables that are affected by this change. [#1150](https://gitlab.com/YottaDB/DB/YDB/-/work_items/1150)

* <a name="x1191"></a>The Source Server connects reliably with the Receiver Server when they are running in Kubernetes pods. Previously it would occasionally hang forever. [#1191](https://gitlab.com/YottaDB/DB/YDB/-/work_items/1191)

* <a name="x1192"></a>The `--from-source` option of `ydbinstall` / `ydbinstall` takes an optional additional `--compiler` option with values `clang` and `gcc`. The compiler can also be specified with the `ydb_compiler` environment variable, with the command line option overriding the environment variable if both are specified. If unspecified, it defaults to `clang` on ARM architectures and `gcc` on x86_64 architectures. If a compiler is not specified, the `--plugins-only` option attempts to determine the compiler used to compile YottaDB from `$ydb_dist/libyottadb.so`. Compiler specifications are ignored with other command line options. [#1198](https://gitlab.com/YottaDB/DB/YDB/-/work_items/1198)

* <a name="GTM-DE325871"></a>YottaDB processes can use sockets created when over 1021 files, pipes, fifos, sockets, and/or regions are already open. YottaDB issues an [FDSIZELMT](https://docs.yottadb.com/MessageRecovery/errors.html#fdsizelimit) error message when there are too many descriptors needed by GT.CM servers. Previously, sockets created when there were too many open descriptors caused an GTMASSERT2 error. [GTM-DE325871](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-000_Release_Notes.html#GTM-DE325871)

* <a name="GTM-DE422089"></a>The YottaDB command line parser correctly terminates input on a null byte. Previously, in rare cases, the parser appended random characters for a [PIPE](https://docs.yottadb.com/ProgrammersGuide/ioproc.html#using-pipe-devices) device usage where a [WRITE](https://docs.yottadb.com/ProgrammersGuide/commands.html#write) followed by the format control character "!" did not precede WRITE /EOF. This was seen only in development/testing and never reported by a user. Previously, YottaDB silently truncated shell arguments that exceeded these limits and did not produce an error when input to a utility prompt exceeded the allowed 33022 bytes. [GTM-DE422089](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-000_Release_Notes.html#GTM-DE422089)

* <a name="GTM-DE476408"></a>A process created by a [JOB](https://docs.yottadb.com/ProgrammersGuide/commands.html#job) command that for any reason exits before becoming fully functional and independent avoids interfering with any data it may share with the process issuing the JOB command. YottaDB also issues a [PIDMISMATCH](https://docs.yottadb.com/MessageRecovery/errors.html#pidmismatch) warning message to the operator log. Previously under rare conditions, such an "infant" process could inappropriately release resources belonging to its "parent," causing symptoms including damage to a statsdb database or to a routine repository. [GTM-DE476408](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-001_Release_Notes.html#GTM-DE476408)

* <a name="GTM-DE503394"></a>[\%YGBLSTAT()](https://docs.yottadb.com/ProgrammersGuide/utility.html#ygblstat) warns about incorrect command line usage. Previously, the utility silently ignored command lines containing errors. [GTM-DE503394](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-001_Release_Notes.html#GTM-DE500860)

* <a name="GTM-DE506361"></a>[GTMSECSHR](https://docs.yottadb.com/AdminOpsGuide/ipcresource.html#gtmsecshr) appropriately handles a rare condition when two processes attempt to start a GTMSECSHR process at a coincident time. Previously, this could start more than one GTMSECSHR process, and, although a single GTMSECSHR process handled all the requests, their shutting down produced syslog error messages. [GTM-DE506361](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-001_Release_Notes.html#GTM-DE506361)

* <a name="GTM-F167609"></a>When using TLS configuration verify-mode option SSL\_VERIFY\_PEER, YottaDB enables TLSv1.3 Post Handshake Authentication (PHA) for client connections. When using TLS configuration verify-mode options SSL\_VERIFY\_PEER and SSL\_VERIFY\_POST\_HANDSHAKE, YottaDB enables TLSv1.3 PHA for server connections. By itself, SSL\_VERIFY\_POST_HANDSHAKE does not enable PHA. Previously, YottaDB did not support TLSv1.3's PHA capability. This could cause problems when connecting to some TLSv1.3 servers that require PHA. [GTM-F167609](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-002_Release_Notes.html#GTM-F167609)

* <a name="GTM-F221672"></a>The [SHMHUGETLB](https://docs.yottadb.com/MessageRecovery/errors.html#shmhugetlb) syslog warning message provides information about the operation of the calling process. Previously, SHMHUGETLB failure messages did not include operational information necessary to understand the reasons for such failures. [GTM-F221672](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.1-000_Release_Notes.html#GTM-F221672)

## Additional Information

<a name="addlydb474"></a>
### C API to serialize/deserialize local or global variable subtrees

The `ydb_encode_s()` and `ydb_encode_st()` functions serialize a (sub)tree into a JSON string (a serialized object or array) in an industry standard encoding suitable for communication or transmission to another application. The `varname` is the name of the local or global variable, `subs_used` is the count of subscripts already set up in `subsarray`, the subscripts used with `varname` to define the source array. The `format` points to a null-terminated string which has the case independent value `"JSON"`, and `ret_value` contains the JSON output string (a serialized object or array) .

Note that `ret_value` is an output-only parameter. Also note that you must `free()` the memory at the `address` member of `ret_value` after use to avoid memory leaks.

```c
int ydb_encode_s(const ydb_buffer_t *varname,
        int subs_used,
        const ydb_buffer_t *subsarray,
        const char *format,
        ydb_string_t *ret_value);

int ydb_encode_st(uint64_t tptoken,
        ydb_buffer_t *errstr,
        const ydb_buffer_t *varname,
        int subs_used,
        const ydb_buffer_t *subsarray,
        const char *format,
        ydb_string_t *ret_value);
```

Return values are:

- `YDB_OK` for a normal return;
- `YDB_ERR_GVUNDEF` or `YDB_ERR_LVUNDEF` as appropriate if no such (sub)tree exists;
- `YDB_ERR_PARAMINVALID` when `ret_value` is NULL; or `len_alloc` \< `len_used` for at least 1 subscript in `subsarray`; or the `len_used` is non-zero and `buf_addr` is NULL for at least one subscript in `subsarray`;
- `ERR_MINNRSUBSCRIPTS` or `ERR_MAXNRSUBSCRIPTS` if subscript counts are less than zero or greater than `YDB_MAX_SUBS`;
- `ERR_JANSSONDLERROR` if a function from the Jansson library fails to dynamically load;
- `ERR_JANSSONINVALIDJSON` if `ret_value->address` is NULL after the encoder finishes;
- `ERR_JANSSONENCODEERROR` if an error occurs when calling one of the Jansson encoding functions;
- Or another applicable [error return code](https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#error-return-code).

The `ydb_decode_s()` and `ydb_decode_st()` functions parse a JSON string (a serialized object or array) in `value`, using an industry standard encoding suitable for communication or transmission from another application, loading the value into the specified local or global variable (sub)tree. The `varname` is the name of the local or global variable, `subs_used` is the count of subscripts already set up in `subsarray`, the subscripts used with `varname` to define the destination array. Existing nodes in the (sub)tree whose subscripts are matched by a node in the input data are over-written; other nodes remain unaltered. The `format` points to a null-terminated string which has the case independent value `"JSON"`.

Note that `value` must be initialized with an `address` member that is not NULL and a `length` member that is not 0, before calling `ydb_decode_s()` or `ydb_decode_st()`. Also note that the `length` member of the `value` parameter, containing the length of the JSON string in the `address` member, should be set to the byte length not including the terminating NULL byte.

```c
int ydb_decode_s(const ydb_buffer_t *varname,
        int subs_used,
        const ydb_buffer_t *subsarray,
        const char *format,
        const ydb_string_t *value);

int ydb_decode_st(uint64_t tptoken,
        ydb_buffer_t *errstr,
        const ydb_buffer_t *varname,
        int subs_used,
        const ydb_buffer_t *subsarray,
        const char *format,
        const ydb_string_t *value);
```

Return values are:

- `YDB_OK` for a normal return;
- `YDB_ERR_PARAMINVALID` when `value` is NULL; or `value->address` is NULL; or `value->length` is zero; or when `len_alloc` \< `len_used` for at least 1 subscript in `subsarray`; or the `len_used` is non-zero and `buf_addr` is NULL for at least one subscript in `subsarray`; or `buf_addr` is too small for at least one key in JSON input; or byte length of the decoded string at a particular node is greater than:
  * 1 MiB in case the target is an `lvn`; or
  * the maximum record size of the region that the node maps to in case the target is a `gvn`;
- `ERR_MINNRSUBSCRIPTS` or `ERR_MAXNRSUBSCRIPTS` if subscript counts are less than zero or greater than `YDB_MAX_SUBS`;
- `ERR_JANSSONDLERROR` if a function from the Jansson library fails to dynamically load;
- `ERR_JANSSONINVALIDJSON` if there is a syntax error in the `value` string;
- Or another applicable [error return code](https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#error-return-code).

<a name="addlydb1152"></a>
### M commands to serialize/deserialize local or global variable subtree

#### ZYDECODE

The ZYDECODE command decodes (deserializes) a JSON string (a serialized object or array), stored in a variable (in a specific chunking format) into another variable, and its descendants, in a typical M tree format. ZYDECODE deletes neither the destination variable, nor any of its descendants.

The format of ZYDECODE command is:

```cos
ZYDE[CODE][:tvexpr] glvn1=glvn2[,...]
```

* The optional truth-valued expression immediately following the command is a command post conditional that controls whether or not YottaDB executes the command.
* When both glvn1 and glvn2 are local variables, the naked indicator does not change.
* If glvn2 is a global variable and glvn1 is a local variable, the naked indicator references glvn2.
* When both are global variables, the state of the naked indicator is unchanged if glvn2 is undefined ($DATA(glvn2)=0).
* In all other cases including $DATA(glvn2)=10, the naked indicator takes the same value that it would have if the SET command replaced the ZYDECODE command and glvn2 had a value.
* If glvn1 is a descendant of glvn2, or if glvn2 is a descendant of glvn1; YottaDB generates a [ZYDECODEDESC](https://docs.yottadb.com/MessageRecovery/errors.html#zydecodedesc) error.
* If $DATA(glvn2) is 0 then YottaDB issues a [ZYDECODEWRONGCNT](https://docs.yottadb.com/MessageRecovery/errors.html#zydecodewrongcnt) error.
* If the root node of glvn2 is not a positive integer then YottaDB issues a [ZYDECODEWRONGCNT](https://docs.yottadb.com/MessageRecovery/errors.html#zydecodewrongcnt) error.
* If any subscript in the sequence of positive integers, represented by the number at the root node is missing then YottaDB issues a [GVUNDEF](https://docs.yottadb.com/MessageRecovery/errors.html#gvundef) or [GVUNDEF](https://docs.yottadb.com/MessageRecovery/errors.html#gvundef) error.
* An indirection operator and an expression atom evaluating to a list of one or more ZYDECODE arguments form a legal argument for a ZYDECODE.
* JSON arrays are decoded into M variables starting with subscript 0.

> [!note]
>
> The chunking format that ZYDECODE takes as input is as follows. If the JSON-formatted string is shorter than or equal to the maximum string length for the current variable (1 MiB for local variables or the maximum record size of the global's region for global variables), store it in the variable's `(1)` subscript. If it is longer than the maximum string length, break it up into sub-strings that are shorter than or equal to the maximum string length, and store them in order in successive integer subscripts matching the pattern (1), (2), (3) and so on until the entire string has been stored. In all cases, store the number of subscripts that contain the JSON-formatted string, in the variable's root node.

ZYDECODE provides the ability to deserialize arbitrarily long JSON strings into decoded M arrays using a single command, offering a faster and simpler alternative to implementing a complex set of functions in M using other commands.

> [!important]
>
> ZYDECODE only supports the UTF-8 character set encoding. Use special care not to use characters which are illegal in UTF-8 when using it with a $ZCHSET of M. Real numbers in JSON use the IEEE 754 double-precision floating point standard, which uses a larger range, but with less precision within that range, than YottaDB's packed binary-coded decimal format; this may result in rounding errors when decoding certain real numbers from the JSON string.

##### Example of ZYDECODE

```cos
YDB>Set json(1)="[""array"", [""zero"", ""one"", {""key"": [""val1"", ""val2""]}], 3, ""1.5"", null, ""null"", true, ""true"", false, ""false""]"
YDB>Set json=1
YDB>ZYDEcode ^array("json")=json
YDB>Write $Reference
^array("json")
YDB>ZWRite ^array
^array("json",0)="array"
^array("json",1,0)="zero"
^array("json",1,1)="one"
^array("json",1,2,"key",0)="val1"
^array("json",1,2,"key",1)="val2"
^array("json",2)=3
^array("json",3)=$C(0)_"1.5"
^array("json",4)=$C(0)_"null"
^array("json",5)="null"
^array("json",6)=$C(0)_"true"
^array("json",7)="true"
^array("json",8)=$C(0)_"false"
^array("json",9)="false"
YDB>
```

This example illustrates how ZYDECODE decodes a JSON string into a global sub-tree. It shows how ZYDECODE encodes the JSON values of null, true, and false, which are not data types in M, as well as a string ("1.5") that would otherwise be coerced in to a number when stored in M, via M's canonical number rules.

#### ZYENCODE

The ZYENCODE command encodes (serializes) a variable, in a typical M tree format, into another variable (in a specific chunking format), formatted as a JSON string (a serialized object or array). ZYENCODE does not delete the destination variable, nor any of its descendants.

The format of ZYENCODE command is:

```cos
ZYEN[CODE][:tvexpr] glvn1=glvn2[,...]
```

* The optional truth-valued expression immediately following the command is a command post conditional that controls whether or not YottaDB executes the command.
* When both glvn1 and glvn2 are local variables, the naked indicator does not change.
* If glvn2 is a global variable and glvn1 is a local variable, the naked indicator references glvn2.
* When both are global variables, the state of the naked indicator is unchanged if glvn2 is undefined ($DATA(glvn2)=0).
* In all other cases including $DATA(glvn2)=10, the naked indicator takes the same value that it would have if the SET command replaced the ZYENCODE command and glvn2 had a value.
* If glvn1 is a descendant of glvn2, or if glvn2 is a descendant of glvn1; YottaDB generates a [ZYENCODEDESC](https://docs.yottadb.com/MessageRecovery/errors.html#zyencodedesc) error.
* If $DATA(glvn2) is 0 then YottaDB issues a [ZYENCODESRCUNDEF](https://docs.yottadb.com/MessageRecovery/errors.html#zyencodesrcundef) error.
* An indirection operator and an expression atom evaluating to a list of one or more ZYENCODE arguments form a legal argument for a ZYENCODE.
* M variables starting a subscript level with 0, with successive integer subscripts at the same level, with no other subscripts not matching that series, are encoded as JSON arrays, otherwise they are encoded as JSON objects.

> [!note]
>
> The chunking format that ZYENCODE returns as output is as follows. If the JSON-formatted string is shorter than or equal to the maximum string length for the current variable (1 MiB for local variables or the maximum record size of the global's region for global variables), it is stored in the variable's `(1)` subscript. If it is longer than the maximum string length, it is broken up into sub-strings that are equal to the maximum string length, and stores them in order in successive integer subscripts matching the pattern (1), (2), (3) and so on until the entire string has been stored. In all cases, the number of subscripts that contain the JSON-formatted string, is stored in the variable's root node.

ZYENCODE provides the ability to serialize arbitrarily large, and complex, M arrays into a JSON-encoded string using a single command, offering a faster and simpler alternative to implementing a complex set of functions in M using other commands.

> [!important]
>
> ZYENCODE only supports the UTF-8 character set encoding. Use special care not to use characters which are illegal in UTF-8 when using it with a $ZCHSET of M. Real numbers in JSON use the IEEE 754 double-precision floating point standard, which uses a larger range, but with less precision within that range, than YottaDB's packed binary-coded decimal format; this may result in rounding errors when encoding certain real numbers into the JSON string.

##### Example of ZYENCODE

```cos
YDB>Set array("json")="JSON test document"
YDB>Set array("json",0)="array"
YDB>Set array("json",1,0)="zero"
YDB>Set array("json",1,1)="one"
YDB>Set array("json",1,2,"key",0)="val1"
YDB>Set array("json",1,2,"key",1)="val2"
YDB>Set array("json",1,2,"key","string")="object"
YDB>Set array("json",2)=3
YDB>Set array("json",3)=$C(0)_"1.5"
YDB>Set array("json",4)=$C(0)_"null"
YDB>Set array("json",5)="null"
YDB>Set array("json",6)=$C(0)_"true"
YDB>Set array("json",7)="true"
YDB>Set array("json",8)=$C(0)_"false"
YDB>Set array("json",9)="false"
YDB>ZYEncode ^json=array("json")
YDB>Write $Reference
^json
YDB>ZWRite ^json
^json=1
^json(1)="{"""": ""JSON test document"", ""0"": ""array"", ""1"": [""zero"", ""one"" {""key"": {""0"": ""val1"", ""1"": ""val2"", ""string"": ""object""}}], ""2"": 3, ""3"": ""1.5"", ""4"": null, ""5"": ""null"", ""6"": true, ""7"": ""true"", ""8"": false, ""9"": ""false""}"
YDB>
```

This example illustrates how ZYENCODE encodes a global sub-tree into a JSON string. It shows how ZYENCODE encodes an M variable sub-tree with a subscript starting at 0, with successive positive integers at the same level, and no other subscripts not matching that pattern in the same level, as a JSON array. It also shows that when that pattern is broken (the series contains a string subscript), it encodes it as a JSON object. It also shows a node with a $DATA of 11, where the child sub-tree would otherwise have been encoded as a JSON array, but instead it was encoded as a JSON object, which then stored the data at the parent node in the empty string key.

<a name="addlydb1178"></a>
### Speed-up code that executes inside and outside critical sections

[MUPIP CREATE](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#create) creates database files with critical section management initialized to use the adaptive method. You can use the [MUPIP SET](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#set) command to restrict critical section management to use a specific method:

```
mupip set -mutex_type=ydb      # restricts use to the built-in YottaDB method
mupip set -mutex_type=pthread  # restricts use to Linux pthread_mutexes
mupip set -mutex_type=adaptive # reverts to switching between the two methods
```

The `-hard_spin_count` and `-soft_spin_counts` are meaningful only when the YottaDB method is in use for critical section management, either when explicitly specified, or chosen by the adaptive method heuristic.

[DSE DUMP FILEHEADER](https://docs.yottadb.com/AdminOpsGuide/dse.html#f-ileheader) output includes a `Mutex Manager Type` field with the following values:

- `ADAPTIVE (YDB)` - adaptive critical section management is active, and is currently using the YottaDB method.
- `ADAPTIVE (PTHREAD)` - adaptive critical section management is active, and is currently using pthread_mutexes.
- `YDB` - critical section management is restricted to use the YottaDB method.
- `PTHREAD` - critical section management is restricted to use pthread_mutexes.

[\%PEEKBYNAME](https://docs.yottadb.com/ProgrammersGuide/utility.html#peekbyname) can also be used to access the current critical section management method. `$$^%PEEKBYNAME("mutex_struct.curr_mutex_type",<region>)` returns the following values corresponding to the `Mutex Manager Type` values reported by DSE DUMP FILEHEADER:

- 0 for ADAPTIVE(YDB)
- 1 for ADAPTIVE (PTHREAD)
- 2 for YDB
- 3 for PTHREAD

The database statistics CQT and CYT reported by [ZSHOW "G"](https://docs.yottadb.com/ProgrammersGuide/commands.html#zshow) and [$VIEW("GVSTAT")](https://docs.yottadb.com/ProgrammersGuide/functions.html#view) are updated only when the YottaDB method is in use.

## Messages

### New Messages

* [ARGTRUNC](https://docs.yottadb.com/MessageRecovery/errors.html#argtrunc)
* [BSIZTOOLARGE](https://docs.yottadb.com/MessageRecovery/errors.html#bsiztoolarge)
* [BUFFSIZETOOSMALL](https://docs.yottadb.com/MessageRecovery/errors.html#buffsizetoosmall)
* [DBFILERDONLY](https://docs.yottadb.com/MessageRecovery/errors.html#dbfilerdonly)
* [DBUPGRDREQ](https://docs.yottadb.com/MessageRecovery/errors.html#dbupgrdreq)
* [EXCEEDRCTLRNDWN](https://docs.yottadb.com/MessageRecovery/errors.html#exceedrctlrndwn)
* [FDSIZELMT](https://docs.yottadb.com/MessageRecovery/errors.html#fdsizelmt)
* [FORCEDHALT2](https://docs.yottadb.com/MessageRecovery/errors.html#forcedhalt2)
* [LINETOOLONG](https://docs.yottadb.com/MessageRecovery/errors.html#linetoolong)
* [ORLBKDBUPGRDREQ](https://docs.yottadb.com/MessageRecovery/errors.html#orlbkdbupgrdreq)
* [ORLBKROLLED](https://docs.yottadb.com/MessageRecovery/errors.html#orlbkrolled)
* [PIDMISMATCH](https://docs.yottadb.com/MessageRecovery/errors.html#pidmismatch)
* [REORGUPCNFLCT](https://docs.yottadb.com/MessageRecovery/errors.html#reorgupcnflct)
* [RSVDBYTE2HIGH](https://docs.yottadb.com/MessageRecovery/errors.html#rsvdbyte2high)

### Revised Messages

The following are updated messages:

* [GTMSECSHRDMNSTARTED](https://docs.yottadb.com/MessageRecovery/errors.html#gtmsecshrdmnstarted)
* [MUUPGRDNRDY](https://docs.yottadb.com/MessageRecovery/errors.html#muupgrdnrdy)
* [SHMHUGETLB](https://docs.yottadb.com/MessageRecovery/errors.html#shmhugetlb)
* [ZCPREALLVALSTR](https://docs.yottadb.com/MessageRecovery/errors.html#zcpreallvalstr)
* [ZSHOWSTACKRANGE](https://docs.yottadb.com/MessageRecovery/errors.html#zshowstackrange)

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

Copyright © 2026 YottaDB LLC. Portions Copyright Fidelity National Information Services, Inc. and/or its subsidiaries.

Permission is granted to copy, distribute and/or modify this document under the terms of the [GNU Free Documentation License, Version 1.3](https://www.gnu.org/licenses/fdl-1.3.txt) or any later version published by the [Free Software Foundation](https://www.fsf.org/); with no Invariant Sections, no Front-Cover Texts and no Back-Cover Texts.

YottaDB®, Octo®, and Xider® are registered trademarks, of YottaDB LLC. GT.M™ is a trademark of Fidelity National Information Services, Inc. Other trademarks belong to their respective owners.

This document contains a description of YottaDB and the operating instructions pertaining to the various functions that comprise the software. This document does not contain any commitment of YottaDB LLC. YottaDB LLC believes the information in this publication is accurate as of its publication date; such information is subject to change without notice. YottaDB LLC is not responsible for any errors or defects.
