<!---
.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2024 YottaDB LLC and/or its subsidiaries.     #
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

# YottaDB r2.02

## Binary Distributions

| sha256sum | file |
|-----------|------|
|           |      |

## Release Note Revision History

| Revision | Date | Summary |
|----------|------|---------|
|          |      |         |

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

## r2.02
### Overview
YottaDB r2.02 is a (_overview to be filled in prior to release_).

In addition to enhancements and fixes made by YottaDB, r2.02 inherits numerous other enhancements and fixes from GT.M V7.0-002, V7.0-003, V7.0-004, and V7.0-005, as described in the [Change History](#change-history).

### Platforms
A platform is a combination of a CPU architecture and an operating system. A platform is Supported, Supportable, or Unsupported.

* Supported means that we have the platform in our development environment, test each release on that platform, and provide a binary distribution for it.
* Supportable means that although we do not necessarily have such a platform in our environment, we have no reason to believe that the software will not run on it.
* All others are Unsupported.

| CPU Architecture                    | Supported OS Version(s)                                                              | Notes                                                                                                                         |
| ----------------------------------- | ------------------------------------------------------------------------------------ | ----------------------------------------------------------------------------------------------------------------------------- |
| 64-bit x86                          | Ubuntu 22.04 LTS & 24.04 LTS; Red Hat Enterprise Linux 8.x & 9.x; SUSE Linux Enterprise 15.x; Debian GNU/Linux 12 (Bookworm) | There are separate binary distributions for each OS version, owing to differences in library versions of those distributions. |
| 64-bit ARM (Raspberry Pi 3 Model B) | Debian GNU/Linux 12 (Bookworm)                                                       | See below.                                                                                                                    |
| 32-bit ARM (Raspberry Pi Zero)      | Debian GNU/Linux 12 (Bookworm)                                                       | See below.                                                                                                                    |

Recent releases of major GNU/Linux distributions with contemporary kernels, glibc and ncurses are Supportable. Specific notes:

- [ydbinstall.sh](https://gitlab.com/YottaDB/DB/YDB/-/blob/master/sr_unix/ydbinstall.sh) recognizes [Rocky Linux](https://rockylinux.org/) as equivalent to RHEL, and [OpenSUSE Leap](https://www.opensuse.org/#Leap) as equivalent to SUSE Linux Enterprise, installing the releases for the corresponding distributions. Note that Rocky Linux and OpenSUSE Leap are Supportable, not Supported.
- On [Arch Linux](https://www.archlinux.org/) and other leading edge distributions such as [OpenSUSE Tumbleweed](https://www.opensuse.org/#Tumbleweed), as well as new versions of Linux distributions, YottaDB will need to be recompiled from source code owing to library and tool chain versions newer than those used in building Supported distributions. The `--from-source` option of [ydbinstall.sh](https://gitlab.com/YottaDB/DB/YDB/-/blob/master/sr_unix/ydbinstall.sh) simplifies the process.
- While YottaDB is Supportable on other ARMv6, [ARMv7-A](https://en.wikipedia.org/wiki/Comparison_of_ARMv7-A_cores), and [ARMv8-A](https://en.wikipedia.org/wiki/Comparison_of_ARMv8-A_cores) CPUs, owing to variations in the implementations of ARM microarchitectures, we recommend that you ensure the software runs correctly before committing to any specific hardware other than those listed above. Please [contact us](mailto:info@yottadb.com) if you want a specific combination of OS and CPU microarchitecture to be Supported.

### Installation
See our [Get Started page](https://yottadb.com/product/get-started) to use YottaDB.

We **strongly recommend** that you install YottaDB r2.02 in a newly created directory, different from those of prior YottaDB releases and any GT.M versions you may have installed on the system.

### Removing an installed YottaDB release
Assuming `$ydb_dist` points to the directory where YottaDB is installed:

 - Cleanly shut down all application processes using that release.
 - Execute `mupip rundown && mupip rundown -relinkctl` and review the output to ensure successful completion.
 - Ensure that there are no `gtcm*` or `gtmsecshr` processes active.
 - Use `sudo lsof | grep $ydb_dist` to ensure there are no open files.
   - If there are links to files in `$ydb_dist`, e.g., from `/usr/local/bin/` or `/usr/local/etc/`, remove the links.
 - Delete the directory with `sudo rm -rf $ydb_dist`

## Upgrading to YottaDB r2.02
YottaDB r2.02 is upward compatible from YottaDB [r2.00](https://gitlab.com/YottaDB/DB/YDB/-/releases/r2.00), GT.M [V7.0-002](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-002_Release_Notes.html), GT.M [V7.0-003](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-003_Release_Notes.html), and GT.M [V7.0-004](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-004_Release_Notes.html). The minimal upgrade steps are:

* Install YottaDB r2.02. Use `ydbinstall` / `ydbinstall.sh` to install YottaDB plugins that you use.
* Install plugins you need in addition to those installed in the previous step, e.g., non-YottaDB plugins.
* Recompile object code, and recreate shared libraries if your application uses shared libraries.
* If your application uses encryption, compile and install the reference implementation plugin (if not done by the `ydbinstall` / `ydbinstall.sh` script) or your customized plugin.
* Cleanly shut down the application and ensure that the database files are shut down using MUPIP RUNDOWN from the prior release.
* Switch journal files with the new YottaDB release.
* Start using the new YottaDB release.

If the database has triggers defined with non-canonical numbers, or numbers specified as strings with any version prior to [r1.28](https://gitlab.com/YottaDB/DB/YDB/-/releases/r1.28), or if you are unsure, extract the trigger definitions, delete existing triggers, and reload the trigger definitions. Issue [[#430](https://gitlab.com/YottaDB/DB/YDB/-/issues/430)] from [r1.28](https://gitlab.com/YottaDB/DB/YDB/-/releases/r1.28) has a series of steps you can copy and execute. There is no need to do this if upgrading from [r1.28](https://gitlab.com/YottaDB/DB/YDB/-/releases/r1.28) or later.

To upgrade from older GT.M releases, first upgrade to GT.M V6.0-000 or later and follow the steps above, or contact your YottaDB support channel for assistance and guidance.

A more sophisticated upgrade technique is:

* Install YottaDB r2.02.
* Create a new replicated instance of your application (on the same system or a different system).
* Assuming the existing instance is A, and the new instance is B, upgrade B to r2.02 and start replicating from A to B.
* Once B catches up, switchover so that B is in a primary role replicating to A.
* Once you are satisfied with B, remove (or upgrade) A.

In the event you need database files that need to grow beyond the 992Mi block limit of database files created with r1.x / GT.M V6.x, you should extract data from r1.x database files and load the extracted data into database files created using r2.02.

## Change History

### r2.02

YottaDB r2.02 includes the following enhancements and fixes beyond YottaDB [r2.00](https://gitlab.com/YottaDB/DB/YDB/-/releases/r2.00).

| ID | Category | Summary |
|----|----------|---------|
|    |          |         |

<a name="gtmv70002"></a>
### GT.M V7.0-002

YottaDB r2.02 incorporates enhancements and fixes from [GT.M V7.0-002](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-002_Release_Notes.html).

| ID                              | Category              | Summary                                                                                              |
|---------------------------------|-----------------------|------------------------------------------------------------------------------------------------------|
| ([GTM-F132372](#GTM-F132372))   | Database              | The replication Update process and associated helper process record database-related statistics      |
| ([GTM-F135292](#GTM-F135292))   | Languages             | Optional second argument to $ZJOBEXAM() to control its output                                        |
| ([GTM-F135393](#GTM-F135393))   | Languages             | $ZMALLOCLIM ISV sets up a trappable warning of high memory usage by a process                        |
| ([GTM-F135414](#GTM-F135414))   | Database              | Proactive Database Block Splitting                                                                   |
| ([GTM-F135415](#GTM-F135415))   | Other                 | Defer a TLS renegotiation when a prior TLS renegotiation is pending                                  |
| ([GTM-F135433](#GTM-F135433))   | Languages             | SET $ZTRAP produces appropriate result                                                               |
| ([GTM-F135435](#GTM-F135435))   | Other                 | Routine shared object integrity check & repair                                                       |
| ([GTM-F135842](#GTM-F135842))   | System Administration | MUPIP BACKUP respects user specified order                                                           |
| ([GTM-DE201305](#GTM-DE201305)) | System Administration | MUPIP BACKUP works if environment variables used in segment to database file mapping are not defined |
| ([GTM-DE201378](#GTM-DE201378)) | Languages             | Prevent $[Z]CHAR() representions from generationg results longer than the maximum string length      |
| ([GTM-DE201380](#GTM-DE201380)) | Languages             | SOCKET device commands defend against large deviceparameter arguments                                |
| ([GTM-DE201381](#GTM-DE201381)) | System Administration | MUPIP LOAD -FORMAT=BINARY uses only data length in checking for maximum length                       |
| ([GTM-DE201386](#GTM-DE201386)) | Languages             | Address issues which can result in a segmentation violation (SIG-11) or similar failure              |
| ([GTM-DE201388](#GTM-DE201388)) | Languages             | Better handling of literal arguments that cause numeric overflow                                     |
| ([GTM-DE201389](#GTM-DE201389)) | Languages             | Pattern match better guards against deep recursion                                                   |
| ([GTM-DE201390](#GTM-DE201390)) | Languages             | CTRAP only recognizes characters with ASCII codes 0-31 inclusive                                     |
| ([GTM-DE201392](#GTM-DE201392)) | Other                 | Routine Management asynchronous event protection                                                     |
| ([GTM-DE201393](#GTM-DE201393)) | Languages             | \@x\@y indirection ignores comments in x and handles empty string arguments appropriately            |
| ([GTM-DE201825](#GTM-DE201825)) | System Administration | ydbinstall removes semsstat2, ftok, and getuid                                                       |
| ([GTM-DE222430](#GTM-DE222430)) | Other                 | Prevent fatal errors from disconnect/hangup                                                          |

<a name="gtmv70003"></a>
### GT.M V7.0-003

YottaDB r2.02 incorporates enhancements and fixes from [GT.M V7.0-003](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-003_Release_Notes.html).

| ID                              | Category              | Summary                                                                                                                                                 |
|---------------------------------|-----------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------|
| ([GTM-F134692](#GTM-F134692))   | System Administration | MUPIP INTEG and MUPIP DUMPFHEAD support the user-specified region order                                                                                 |
| ([GTM-F134877](#GTM-F134877))   | Other                 | GT.M TLS reference encryption plugin provides support for ECDHE ciphers                                                                                 |
| ([GTM-F135169](#GTM-F135169))   | Languages             | Provide additional options for SOCKET devices                                                                                                           |
| ([GTM-F135258](#GTM-F135258))   | Other                 | GT.M TLS reference encryption plugin provides support for TLS v1.3                                                                                      |
| ([GTM-F135313](#GTM-F135313))   | Other                 | Retry interruptions on database file creation or extention with NODEFER_ALLOCATE                                                                        |
| ([GTM-F135334](#GTM-F135334))   | Other                 | Error reporting from the GT.M database and the TLS reference encryption plugins don't interfere with each other                                         |
| ([GTM-F135416](#GTM-F135416))   | System Administration | MUPIP FREEZE -ON does not hang while an online rollback is in progress                                                                                  |
| ([GTM-F135424](#GTM-F135424))   | Languages             | Empty string rather than UNDEF error in case of a lost FOR control variable in NOUNDEF mode                                                             |
| ([GTM-F135428](#GTM-F135428))   | Languages             | $ZSOCKET() returns an empty string when given an out of range index                                                                                     |
| ([GTM-F157495](#GTM-F157495))   | Languages             | $VIEW("DEVICE",<device-designation>) returns device status                                                                                              |
| ([GTM-DE257948](#GTM-DE257948)) | Languages             | Appropriate handling for square-bracket (nonstandard) extended reference syntax and NUMOFLOW in expressions made up of literals                         |
| ([GTM-DE276621](#GTM-DE276621)) | Languages             | Literal FALSE postconditionals don't cause failures when they suppress a command with a local variable in their argument that appears later in the line |
| ([GTM-DE294185](#GTM-DE294185)) | System Administration | Receiver Server continues to operate after an unexpected termination of a TLS connection                                                                |
| ([GTM-DE294187](#GTM-DE294187)) | Languages             | Protect $ZKey from damage during heap management                                                                                                        |

<a name="gtmv70004"></a>

YottaDB r2.02 incorporates enhancements and fixes from [GT.M V7.0-004](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-004_Release_Notes.html).

| ID                              | Category              | Summary                                                                                            |
|---------------------------------|-----------------------|----------------------------------------------------------------------------------------------------|
| ([GTM-F135380](#GTM-F135380))   | System Administration | Restrictions for LKE and LKE CLEAR                                                                 |
| ([GTM-F135381](#GTM-F135381))   | System Administration | Audit MUPIP facility and improved handling of network errors by the audit facility                 |
| ([GTM-F158404](#GTM-F158404))   | Other                 | Multiple Enhancements for OpenSSL 3.0 and TLSv1.3 compatibility                                    |
| ([GTM-F166755](#GTM-F166755))   | System Administration | MUPIP BACKUP retry normalization                                                                   |
| ([GTM-F167559](#GTM-F167559))   | System Administration | GT.M V7 versions support the ability to create database files compatible with GT.M V6.3-014        |
| ([GTM-DE201295](#GTM-DE201295)) | Languages             | Honor timeout value on initial socket connect                                                      |
| ([GTM-DE270421](#GTM-DE270421)) | Database              | Correct navigation when subscript-level mapping "hides" data                                       |
| ([GTM-DE297205](#GTM-DE297205)) | Languages             | Prevent assert from stopping a process that is in the midst of OPENing a SOCKET device             |
| ([GTM-DE304273](#GTM-DE304273)) | Languages             | Appropriate result for SET $ZTRAP under certain conditions                                         |
| ([GTM-DE305529](#GTM-DE305529)) | Languages             | Protect correct value of $ZTSLATE                                                                  |
| ([GTM-DE307442](#GTM-DE307442)) | Languages             | Handle error with USE LISTEN or CONNECT and IOERROR="T"                                            |
| ([GTM-DE308470](#GTM-DE308470)) | Languages             | Preserve appropriate left-to-right evaluation within $SELECT() when using FULL_BOOLEAN compilation |

<a name="gtmv70005"></a>

YottaDB r2.02 incorporates enhancements and fixes from [GT.M V7.0-005](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-005_Release_Notes.html).

| ID                              | Category              | Summary                                                                                       |
|---------------------------------|-----------------------|-----------------------------------------------------------------------------------------------|
| ([GTM-F134396](#GTM-F134396))   | Database              | More context for TPFAIL errors                                                                |
| ([GTM-F135278](#GTM-F135278))   | Languages             | GT.M performs certain cases of string concatenation more efficiently                          |
| ([GTM-F135288](#GTM-F135288))   | System Administration | Environment variables for huge pages and shared memory pinning                                |
| ([GTM-F135319](#GTM-F135319))   | Other                 | Improve handling of stack limit detection and reporting                                       |
| ([GTM-F135355](#GTM-F135355))   | Other                 | GT.M Internal signal DRVLONGJMP no longer exposed                                             |
| ([GTM-F135370](#GTM-F135370))   | System Administration | Audit LKE facility                                                                            |
| ([GTM-F135376](#GTM-F135376))   | System Administration | Automatically reestablish replication connection on detecting various network errors          |
| ([GTM-F135382](#GTM-F135382))   | System Administration | Audit GDE facility                                                                            |
| ([GTM-F135383](#GTM-F135383))   | System Administration | Audit DSE facility                                                                            |
| ([GTM-F135406](#GTM-F135406))   | Database              | Trigger statistics for ZSHOW and ^%YGBLSTAT                                                   |
| ([GTM-F135418](#GTM-F135418))   | System Administration | Extend restriction facility to ZLINK, ZRUPDATE & SET $ZROUTINES                               |
| ([GTM-F170998](#GTM-F170998))   | Languages             | $ZAUDITLOG() function for possible application audit logging and audit GDE facility           |
| ([GTM-F171004](#GTM-F171004))   | System Administration | GT.M audit logging puts standard input labeled with "tty=" and reports more error information |
| ([GTM-F188829](#GTM-F188829))   | System Administration | MUPIP shell works with MUPIP Audit facility                                                   |
| ([GTM-F188844](#GTM-F188844))   | Other                 | Auto-Relink performance Improvement                                                           |
| ([GTM-DE309281](#GTM-DE309281)) | Other                 | Protect getaddrinfo and freeaddrinfo calls from interrupts                                    |
| ([GTM-DE326986](#GTM-DE326986)) | System Administration | Improved management of shared memory associated with MUPIP INTEG -ONLINE                      |
| ([GTM-DE327593](#GTM-DE327593)) | Database              | Fix name-level $ORDER(gvn,-1)                                                                 |
| ([GTM-DE340860](#GTM-DE340860)) | System Administration | Clean up temporary backup files following multi-region backup copy errors                     |
| ([GTM-DE345399](#GTM-DE345399)) | Database              | Prevent inappropriate TPFAIL with forth retry code of X                                       |

### Database

* <a name="GTM-F132372"></a>The replication update process and its helper processes record statistics using the STATSDB facility for reporting using %YGBLSTAT. YottaDB reports the WRL, PRG, WFL, and WHE statistics as part of the %YGBLSTAT output. Previously, the replication update process and its helper processes did not record statistics for STATSDB. ([GTM-F132372](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-002_Release_Notes.html#GTM-DE197426456))

* <a name="GTM-F134396"></a>TPFAIL errors include context similar to that provided for GVXXXFAIL errors. Previously such errors did not supply context beyond the retry codes. ([GTM-F134396](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-005_Release_Notes.html#GTM-F134396))

* <a name="GTM-F135406"></a>YottaDB reports the STG, KTG and ZTG statistics as part of the ZSHOW and YGBLSTAT outputs. These accumulators provide additional details on invoked SET KILL and ZTRIGGER and triggers. Due to the nature of some statistics, their order of appearance in ZSHOW output may change. Previously, YottaDB did not record these statistics in ZSHOW and YGBLSTAT. ([GTM-F135406](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-005_Release_Notes.html#GTM-F135406))

* <a name="GTM-F135414"></a>By default, YottaDB proactively splits blocks when it detects significant restarts in an attempt to reduce overall restarts. MUPIP SET FILE <filename> PROBLKSPLIT=N where N is 0 disables proactive splitting, as do very large values of N. Values of N greater than 0 adjust the lower limit for the number of records below which YottaDB does not consider splitting a block. The default value of N is 5. While this is behavior is aimed at reducing restarts, because it reduces data density, it may also increase the number of blocks and even the tree depth. ([GTM-F135414](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-002_Release_Notes.html#GTM-DE197637426))

* <a name="GTM-DE270421"></a>YottaDB handles $ORDER(), $ZPREVIOUS(), $QUERY(), $NEXT(), and MERGE operations which traverse subscript-level mappings correctly. Previously, these operations could return gvns which should have been excluded by the global directory. In the case of MERGE, the target could contain data from excluded subtrees. ([GTM-DE270421](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-004_Release_Notes.html#GTM-DE270421))

* <a name="GTM-DE327593"></a>The issue reported here was never in a YottaDB release, and the following appears here for completeness:

  GT.M handles name-level $ORDER(gvn,-1)/$ZPREVIOUS(gvn) correctly when searching across subscript-level mappings in the global directory. In V7.0-004, $ORDER(gvn,-1), where gvn is an unsubscripted global, could return the same gvn instead of a previous global name or the null string. The workaround was to add global data to otherwise empty global maps between the specified gvn and its previous gvn, and optionally KILLing it afterwards, which leaves around sufficient residual information in the database to avoid the issue. ([GTM-DE327593](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-005_Release_Notes.html#GTM-DE327593))

* <a nane="GTM-DE345399"></a>TP transactions allocating blocks do not encounter TPFAIL errors with a fourth restart code of "X" due to a large number of such actions within a region. In r2.00, it would take a lifetime to encounter the issue but it has been completely eliminated. While user update patterns vary and we do not have the means to replicate them, our limited testing indicates the revision may result in a modest reduction in bit map related TPRESTARTs with code: a. In r1.24 through r1.38, such a TPFAIL could arise in a region with a high rate of TP induced block allocations and a long period of up time. The workaround was to monitor tp_hint and restart or reset the monitored value; contact your YottaDB support channel for additional information. ([GTM-DE345399](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-005_Release_Notes.html#GTM-DE345399))

### Languages

* <a name="GTM-F135278"></a>YottaDB avoids copying strings in some cases involving the concatenation operator. Certain concatenation patterns can see significant performance improvements. Previously, YottaDB sometimes unnecessarily recopied concatenation operands to the end of the heap (stringpool). ([GTM-F135278](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-005_Release_Notes.html#GTM-F135278))

* <a name="GTM-F135169"></a>The OPEN and USE commands for SOCKET devices support assigning characteristics maintained with the POSIX setsockopt() service using the OPTIONS deviceparameter for the newly created socket or the current socket.

  ```
  OPTIONS=expr  Applies to: SOC
  ```

  The argument (expr) is a string which contains a comma separated list of setsockopt options. If the option takes a value, it is given after an equal sign (=) following the option. The supported options are:
  
  ```
  KEEPALIVE   a positive value enables SO_KEEPALIVE.  A zero value disables SO_KEEPALIVE.    
  KEEPIDLE    sets the TCP_KEEPIDLE socket value.    
  KEEPCNT     sets the TCP_KEEPCNT socket value.    
  KEEPINTVL   sets the TCP_KEEPINTVL socket value.    
  SNDBUF      sets the size of the socket's network send buffer (SO_SNDBUF) in bytes.
  ```

  Examples:
  
  ```
  USE dev:OPTIONS="KEEPALIVE=1,KEEPIDLE=50"
  ```
  
  This enables SO_KEEPALIVE and set TCP_KEEPIDLE to 50 seconds.

  ```
  USE dev:OPTIONS="KEEPALIVE=0"
  ```

  This disables SO_KEEPALIVE.
  
  **Note**
  
    For more information on the use of these options, please review the man page for setsockopt . On Linux, "man 7 socket" and "man 7 tcp" provide additional information.

  The $ZSOCKET() function supports an "OPTIONS" keyword which takes an index argument and returns a string of the OPTIONS previously specified for the selected socket. The string may not exactly match the string originally specified but has the same meanings.

  The keywords "KEEPALIVE", "KEEPCNT", "KEEPIDLE", "KEEPINTVL", and "SNDBUF" return the individual items. If the system's current value for the item doesn't match the value previously specified with the OPTIONS device parameter, both values are returned separated by a semicolon (";"): "uservalue;systemvalue".

  The "ZIBFSIZE" keyword may return the system value for SO_RCVBUF in addition to the value from the ZIBFSIZE device parameter. Note that the operating system may modify the values specified for SO_RCVBUF and SO_SNDBUF so the returned values for those options obtained with POSIX getsockopt() service may be different than those specified using setsockopt().
  
  ([GTM-F135169](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-003_Release_Notes.html#GTM-F135169))

* <a name="GTM-F135292"></a>As YottaDB already implemented [this functionality in r1.30](https://gitlab.com/YottaDB/DB/YDB/-/issues/482), the following appears here for completeness.

  $ZJOBEXAM() recognizes an optional second argument of an expr that evaluates to a string as described for the argument of ZSHOW specifying one or more codes determining the nature of the information produced by the function. If the argument is missing or empty, YottaDB operates as if it was a "*" and produces the associated context. This provides a way to suppress content that might contain PNI. Previously, $ZJOBEXAM() always produced a full context. ([GTM-F135292](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-002_Release_Notes.html#GTM-DE197637151))

* <a name="GTM-F135393"></a>$ZMALL[OCLIM] provides a way for a process to limit its process memory.

  - When the value is zero (0), YottaDB imposes no limit, although the OS still does.
  - A positive value specifies a byte limit with a minimum of 2.5MB.
  - A value of minus one (-1) provides a value of half the system imposed limit if any.
  
  When a request for additional memory exceeds the limit, YottaDB does the expansion and then produces a trappable MALLOCCRIT warning. By default, some later request for memory is likely to produce a fatal MEMORY error, unless subsequent to MALLOCCRIT, a limit has been reestablished by SET $ZMALLOCLIM to the same or higher limit, but one not exceeding any system limit. Note that YottaDB allocates memory from the OS in large blocks so the interaction of $ZMALLOCLIM with memory growth is not exact. In the case of a MEMORY error, YottaDB makes an attempt to marshal available memory to enable as graceful a termination as possible. Note that independent of this mechanism, the OS may kill the process without recourse if it determines the greed of the process for memory jeopardizes the viability of the system. When the integer byte value specified in a SET $ZMALLOCLIN=intexpr or, at process startup, by the \$ydb_malloc_limit environment variable (\$gtm_malloc_limit, if it specified and $ydb_malloc_limit is not), specifies a positive value, YottaDB uses the smaller of that value and any OS defined amount for the value of $ZMALLOCLIM. YottaDB does not give errors or messages about its choice for $ZMALLOCLIM between a specified value and some other more appropriate value, so if the application needs to verify the outcome, it should examine the resulting ISV value. MEMORY errors are fatal and terminate the process. Previously, fatal MEMORY errors were not preceded by a trappable MALLOCCRIT warning. ([GTM-F135393](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-002_Release_Notes.html#GTM-DE197637393))

* <a name="GTM-F135424"></a>In NOUNDEF mode, YottaDB assigns the value of an empty string to an undefined FOR control variable. Previously, YottaDB provided an UNDEF error for this condition even when the mode was NOUNDEF. Note that KILL or NEW of a control variable is a questionnable programming practice that can result in an unintended infinite loop. ([GTM-F135424](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-003_Release_Notes.html#GTM-F135424))

* <a name="GTM-F135428"></a>If the index for the $ZSOCKET() function is outside the range of attached sockets for the specified SOCKET device, $ZSOCKET() returns an empty string. Previously, if prior actions on the SOCKET device removed one or more sockets, $ZSOCKET() could return stale or invalid information, or cause a segmentation violation (SIG-11). ([GTM-F135428](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-003_Release_Notes.html#GTM-F135428))

* <a name="GTM-F135433"></a>SET \$ZTRAP=\$ETRAP works correctly; previously it tended to result in $ZTRAP="" regardless of the prior value of $ETRAP, but could also produce garbage values, while less likely, other assignments to $ZTRAP could also go awry. The workaround was to NEW $ETRAP, assign a the value of $ETRAP to a temporary, the empty string to $ETRAP and $ZTRAP to the value of the temporary. ([GTM-F135433](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-002_Release_Notes.html#GTM-DE197637480))

* <a name="GTM-F157495"></a>The $VIEW() keyword "DEVICE" returns the type and status, if any, for the device named by the second argument. If no device of that name exists, the function returns an empty string. The device type is based on what ZSHOW "D" shows.

  Examples:

  ```
  WRITE $VIEW("DEVICE","0")
  TERMINAL:OPEN
  ```

  This indicates the $PRINCIPAL device is a terminal and it is open, which is usually the case for $PRINCIPAL. $ZPIN and $ZPOUT intrinsic special variables used as the second argument select the corresponding side of a split $PRINCIPAL device.

  ```
  OPEN "f.txt"
  CLOSE "f.txt":NODESTROY
  WRITE $VIEW("DEVICE","f.txt")
  RMS:CLOSED
  ```

  ([GTM-F157495](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-003_Release_Notes.html#GTM-F157495))

* <a name="GTM-F170998"></a>The $ZAUDITLOG() function establishes a connection via a socket and sends its argument to a logger/listener process. It requires setting the AZA_ENABLE audit logging facility in the `$ydb_dist/restrict.txt` file. The format for the $ZAUDITLOG() function is:

  ```
  ZAUDITLOG(expr)
  ```

  - expr specifies the string to send for audit logging.
  - $ZAUDITLOG() identifies its message with src=4, and like other YottaDB logging facilities, records the location of the YottaDB distribution, uid, euid, pid, tty, and the command / argument(s).
  - A return of: TRUE (1) indicates successful logging, FALSE (0) indicates logging is not enabled; a trappable RESTRICTEDOP error indicates logging is enabled but not working.
  - If LGDE is specified as an option for the AZA_ENABLE facility, GDE logs all commands. YottaDB ignores this option if specified with other A*_ENABLE audit logging facilities. When it fails to log a command, GDE issues a GDELOGFAIL error. The following table characterizes $ZAUDITLOG() and GDE audit logging behavior:

   $ZAUDITLOG() / GDE logging Characteristics
  | AZA_ENABLE | LGDE | Logging Success | GDE Audit Logging | $ZAUDITLOG() result |
  |------------|------|-----------------|-------------------|---------------------|
  | Yes        | Yes  | Yes             | Yes               | 1                   |
  | Yes        | No   | Yes             | No                | 1                   |
  | Yes        | Yes  | No              | GDELOGFAIL error  | RESTRICTEDOP error  |
  | Yes        | No   | no              | No                | RESTRICTEDOP error  |
  | No         | N/A  | N/A             | No                | 0                    |

  Previously, YottaDB did not support the $ZAUDITLOG() function. ([GTM-F170998](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-005_Release_Notes.html#GTM-F170998))

*<a name="GTM-DE201295"></a>SOCKET device OPEN better honors timeouts; similarly, USE for a SOCKET honors an implicit zero timeout. Note that very short timeouts, particularly zero (0), may be unsuitable for dealing with the timing of some network events, and thus may need adjustment to avoid timing out after this correction. Previously, these commands did not start their own timers to bound the operation, causing the CONNECT to only check for a timeout condition when some other process, or operating system, event interrupted the process. ([GTM-DE201295](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-004_Release_Notes.html#GTM-DE201295))

* <a name="GTM-DE201378"></a>Operations that replace non-graphic characters with $[Z]CHAR() representations give a MAXSTRLEN error when the result would exceed 1MiB. Previously such operations ensured they had adequate space for the result, but could pass strings to other operations that might have no provision for dealing with strings longer than 1MiB. ([GTM-DE201378](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-002_Release_Notes.html#GTM-DE197637455))

* <a name="GTM-DE201380"></a>USE SOCKET handles the ATTACH, DETACH, CONNECT, SOCKET and ZLISTEN deviceparameters appropriately; previously, certain arguments for these deviceparameters could cause a segmentation violation (SIG-11). ([GTM-DE201380](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-002_Release_Notes.html#GTM-DE197637457))

* <a name="GTM-DE201386"></a>Address issues that can result in a segmentation violation (SIG-11) or similar failure.

  - YottaDB appropriately detects divide by zero; previously there were some unusual combinations of calculations which produced a SIGINTDIV (SIG-8), which could not be trapped and therefore terminated the process.
  - The YottaDB compiler appropriately handles optimization of literal FALSE post conditionals; previously these could cause segmentation violations (SIG-11). The YottaDB compiler deals with argumentless BREAK and NEW commands combined with certain patterns of local variable use, and also with NEW commands having indirect arguments after a FOR command. Previously these could cause a segmentation violation (SIG-11).
  - As YottaDB protects itself from overly long strings, it does not limit the list of global names to an arbitrary 1024 bytes. The following appears here for completeness: _The VIEW command for "NOISOLATION" handles various edge cases and reports a VIEWARGTOOLONG error when a list of global names exceeds 1024 bytes. Previously, some malformed global names could cause a segmentation violation (SIG-11) and using a string over 1024 bytes would cause an assertion failure._
  - The ZSHOW command operates as expected when directing the output to a local variable. Previously repeatedly running the ZSHOW command in an indefinite loop with the output directed to a local variable could result in a segmentation violation (SIG-11).
  - The $FNUMBER() and $JUSTIFY() functions work as expected. Previously, results requiring long lengths could result in a segmentation violation (SIG-11).
  - The YottaDB compiler protects against an indirect argument combined with a missing closing parenthesis in a $INCREMENT() function; previously some syntax with that characteristic produced a segmentation violation (SIG-11).
  - $ZSYSLOG() ignores Format Ascii Output (FAO) directives. Previously, $ZSYSLOG() did not ignore such directives resulting in a segmentation violation (SIG-11).
  - The USE command works as expected when the indirect device name contains an ill formatted string. Previously certain strings could cause a segmentation violation (SIG-11).
  - YottaDB ignores unknown device class mnemonic spaces. Previously, if a mnemonic space other than "SOCKET" or "PIPE" was used, a subsequent OPEN resulted in a segmentation violation (SIG-11).

  ([GTM-DE201386](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-002_Release_Notes.html#GTM-DE197637465))

* <a name="GTM-DE201388"></a>$[Z]CHAR("1E48") and similar numeric literal overflows produce a NUMOFLOW error at compile time; previously an optimization inappropriately treated this as $[Z]CHAR(0). ([GTM-DE201388](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-002_Release_Notes.html#GTM-DE197637468))

* <a name="GTM-DE201389"></a>YottaDB pattern match reports a PATMAXLEN error for a deeply nested pattern containing many indefinite ranges (. symbols). While we think it unlikely, it is possible that existing complex patterns may now report this error. Previously such a deeply nested pattern terminated with a segmentation violation (SIG-11). ([GTM-DE201389](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-002_Release_Notes.html#GTM-DE197637469))

* <a name="GTM-DE201390"></a>CTRAP characters other than CTRL-x, where x corresponds to the non-graphic characters represented by \$CHAR(n) with n 0 to 31 inclusive, have no effect, and do not show up in ZSHOW "D" output. On the keyboard those characters correspond to <CTRL-@> to CTRL-_> with the bulk of the range being <CTRL-A> to <CTRL-Z>. Note that CTRAP=\$CHAR(3) has a different semantic than the other CTRAP characters, in that, when enabled and $PRINCIPAL is a terminal device type, <CTRL-C> can interrupt at any time, including when \$PRINCIPAL\'=\$IO. YottaDB only recognizes the other CTRAP characters when they appear in input to a READ command. Previously, characters outside the range with codes 0-31 showed up in ZSHOW "D" output with a modulo 32 $CHAR() representation and caused corresponding CTRAP recognition. ([GTM-DE201390](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-002_Release_Notes.html#GTM-DE197637470))

* <a name="GTM-DE201393"></a>Indirection of the form \@x@y ignores any comment at the end of the value of x; previously such a comment stopped the evaluation so the subscripts after the second @ were ignored. In addition, if the first (x) expression evaluates to the empty string, YottaDB produces a VAREXPECTED error; previously that scenario produced incorrect results, sub-optimal errors or, in the worst case damage to an item at the top of the heap. Also, if the second (y) expression is the empty string, it no longer has the potential to lead to incorrect results or sub-optimal error reports. ([GTM-DE201393](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-002_Release_Notes.html#GTM-DE197637474))

* <a name="GTM-DE257948"></a>YottaDB accepts square-bracket expressions as synonyms for the standard vertical bars. Previously, YottaDB handled square-bracket expressions inappropriately, which could cause a fatal error. In addition, YottaDB detects and recovers from certain numeric overflow errors. Previously, YottaDB inappropriately handled such overflow errors when optimizing arithmetic expressions containing literals, which caused fatal errors. ([GTM-DE257948](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-003_Release_Notes.html#GTM-DE257948))

* <a name="GTM-DE276621"></a>The issue reported here was never in a YottaDB release, and the following appears here for completeness:

  The YottaDB compiler correctly handles lines with literal FALSE postconditionals. Due to a regression in V7.0-002, the compiler could encounter a segmentation violation (SIG-11) error when having to fetch a variable that earlier in the same line was the argument of a command with a literal FALSE postconditional. ([GTM-DE276621](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-003_Release_Notes.html#GTM-DE276621))

* <a name="GTM-DE294187"></a>Reading from $ZKey works as expected. Previously under rare circumstances, an access of $ZKey could result in a segmentation violation (SIG-11). This was only seen in a development environment and never reported by a user. (GTM-DE294187)(http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-003_Release_Notes.html#GTM-DE294187)

* <a name="GTM-DE297205"></a>YottaDB processes shut down gracefully when they receive a MUPIP STOP while preforming an OPEN of a SOCKET device. Previously, these conditions could cause a YottaDB process to issue a GTMASSERT2 error. ([GTM-DE297205](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-004_Release_Notes.html#GTM-DE297205))

* <a name="GTM-DE304273"></a>SET $ZTRAP, for example \$ZTRAP=\$ETRAP, works correctly during XECUTE frame or from a non-base Direct Mode prompt, for example due to a BREAK command, when ydb_ztrap_new is TRUE. Previously, such a circumstance tended to produce a corrupted value for $ZTRAP. ([GTM-DE304273](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-004_Release_Notes.html#GTM-DE304273))

* <a name="GTM-DE305529"></a>YottaDB maintains the correct value of $ZTSLATE. Previously, garbage collection could mishandle the ISV resulting in values not consistent with proper application execution (([GTM-DE305529](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-004_Release_Notes.html#GTM-DE305529))

* <a name="GTM-DE307442"></a>SOCKET device USE handles errors associated with device parameters CONNECT or LISTEN and IOERROR="T" appropriately. Previously, such errors could cause odd behavior including segmentation violation (SIG-11). This was only seen in development and testing environments and never reported by a user. ([GTM-DE307442](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-004_Release_Notes.html#GTM-DE307442))

* <a name="GTM-DE308470"></a>Expratoms in a boolean expression are evaluated in left-to-right order in FULL_BOOLEAN compilation mode. Previously, side effects in a $SELECT() could affect earlier expratoms in a boolean expression. ([GTM-DE308470](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-004_Release_Notes.html#GTM-DE308470))

### System Administration

* <a name="GTM-F134692"></a>When MUPIP INTEG and MUPIP DUMPFHEAD arguments specify a region list, MUPIP processes regions in the listed order, or, for names expanded by wildcard ("*"), alphabetically. Previously, MUPIP DUMPFHEAD and MUPIP INTEG ignored any user-specified order of regions, and processed regions in FTOK order, which tends to change with changes in operational conditions within the underlying file system. ([GTM-F134692](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-003_Release_Notes.html#GTM-F134692))

* <a name="GTM-F135288"></a>When the gtm_pinshm environment variable is defined and evaluates to a non-zero integer or any case-independent string or leading substring of TRUE or YES in a process creating shared memory, YottaDB attempts to pin such memory used for database global buffers, replication buffers, and routine buffers into physical memory. Hugepages are implicitly locked in physical memory, so YottaDB does not attempt to pin shared memory buffers backed by hugepages. gtm_pinshm does not pin memory used by online INTEG (integ snapshot). Pinning may not succeed due to insufficient physical memory and/or OS configuration. When the gtm_hugetlb_shm environment variable is defined and evaluates to a non-zero integer or any case-independent string or leading substring of TRUE or YES in a process creating shared memory, YottaDB attempts to back all such shared memory segments with huge pages, using the default huge page size. If huge pages cannot be used, GT.M tries to back the shared memory with base pages instead, and attempts to pin the shared memory if requested with gtm_pinshm. GT.M issues a SHMHUGETLB or SHMLOCK warning message to the system log when the system is unable to back shared memory with huge pages or is unable to pin shared memory to physical memory, respectively. Previously, YottaDB did not support the gtm_pinshm option to pin memory, and gtm_hugetlb_shm replaces the use of libhugetlbfs for huge page functions, so YottaDB no longer evaluates libhugetlbfs environment variables, e.g. HUGETLB_SHM, HUGETLB_VERBOSE, Setting HUGETLB* environment variables continues to work, but YottaDB will not be aware - a non-issue in practice - that hugepages are in use. [Linux] ([GTM-F135288](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-005_Release_Notes.html#GTM-F135288))

* <a name="GTM-F135370"></a>When the restriction file contains a line specifying AL_ENABLE, LKE establishes a connection, a via socket, to a logger/listener process, and sends all commands, designating src=5, and using the socket to the listener for audit logging. If sending succeeds, LKE executes the command. If the connection or the send fail, LKE issues a RESTRICTEDOP error and does not execute the command. AL_ENABLE supports TCP, TLS or UNIX socket types. By default, LKE commands execute without logging. Previously, LKE did not provide an audit logging option. ([GTM-F135370](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-005_Release_Notes.html#GTM-F135370))

* <a name="GTM-F135376"></a>YottaDB Replication resets replication connection and attempts to automatically reestablish connection on detecting various network errors. Previously, YottaDB replication server processes could shut down on encountering some network errors. ([GTM-F135376](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-005_Release_Notes.html#GTM-F135376))

* <a name="GTM-F135416"></a>MUPIP FREEZE ON operates while an online rollback is in progress. YottaDB now checks for contention and ensures online rollback appropriately releases the required resources and other database accesses wait for this to occur. Previously in rare cases, it might hang because of shared resource contention. ([GTM-F135416](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-003_Release_Notes.html#GTM-F135416))

* <a name="GTM-F135842"></a>When MUPIP BACKUP arguments specify a list, the utility processes regions in the listed order, or, for names expanded by wildcard ("*"), alphabetically. Previously, MUPIP BACKUP ignored any user-specified order of regions, and processed regions in FTOK order, which tends to change with changes in operational conditions within the underlying file system.([GTM-F135842](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-002_Release_Notes.html#GTM-DE197637997))

* <a name="GTM-F135380"></a>The LKECLEAR keyword in the YottaDB restrictions file prevents the use of the LKE CLEAR command by unauthorized users, while the LKE keyword prevents any unauthorized use of the LKE utility. Previously the only way to restrict the use of LKE was by setting the authorizations on the executable; there was no way to allow the use of LKE while blocking the use of LKE CLEAR. ([GTM-F135380](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-004_Release_Notes.html#GTM-F135380))

* <a name="GTM-F135381"></a>When the Audit MUPIP facility (AM_ENABLE) is enabled, MUPIP establishes a connection (via a UNIX/TCP/TLS socket) to a logger/listener process, and sends any MUPIP shell command through the socket to the listener for logging. If sending is successful, MUPIP executes the command. If the connection is not successful or sending of the command fails, then MUPIP produces an error and does not execute the command. When this facility is enabled, all commands typed at the MUPIP prompt (MUPIP>) produce the RESTRICTEDOP error. When this facility is disabled, which it is by default, MUPIP commands execute as usual. In addition, the APD_ENABLE facility displays the appropriate network error messages and exits the process gracefully. Previously, certain network errors could result in a segmentation fault without reporting the reason. ([GTM-F135381](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-004_Release_Notes.html#GTM-F135381))

* <a name="GTM-F135382"></a>When LGDE is specified as an option for the AZA_ENABLE facility, GDE logs all commands. For example, an entry like the following in `$ydb_dist/restrict.txt` enables GDE logging via a local socket:

  ```
  AZA_ENABLE:LGDE:/path/to/sock/file/audit.sock
  ```
  
  The AZA_ENABLE facility enables the use of the $ZAUDITLOG() function which GDE uses for logging commands. Refer to [GTM-F170998](#GTM-F170998) for information on the $ZAUDITLOG() function and for other possible use in application audit logging. Previously, GDE did not provide an audit logging option. ([GTM-F135382](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-005_Release_Notes.html#GTM-F135382))

* <a name="GTM-F135418"></a>The YottaDB restrictions facility recognizes ZLINK, ZRUPDATE and SET $ZROUTINES. When an explicit ZLINK (not auto-zlink), ZRUPDATE, or SET $ZROUTINES restriction conditions are configured, the restricted command issues a RESTRICTEDOP error message. Previously, the restrictions facility did not support ZLINK, ZRUPDATE, or SET $ZROUTINES. ([GTM-F135418](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-005_Release_Notes.html#GTM-F135418))

* <a name="GTM-F135383"></a>When the restriction file specifies AD_ENABLE, DSE establishes a connection via a socket, to a logger/listener process, and sends all commands, designating src=6, and using the socket to the listener for audit logging. If sending succeeds, DSE executes the command. If the connection or the send fail, DSE issues a RESTRICTEDOP error and does not execute the command. AD_ENABLE supports TCP, TLS or UNIX socket types. By default, DSE commands execute without logging. Previously, DSE did not provide an audit logging option. ([GTM-F135383](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-005_Release_Notes.html#GTM-F135383))

* <a name="GTM-F166755"></a>By default, MUPIP BACKUP DATABASE retries file copy operation once if the first attempt fails for a retriable reason; this is equivalent to RETRY=1. RETRY=0 prevents any retries, so if the copy fails MUPIP immediately reports an error. Additionally, any copy failure error messages include region information when displaying OS service error codes. Previously, while the default was equivalent to RETRY=1 that incorrectly meant one BACKUP attempt, RETRY=0 did not perform a backup, and error reports did not identify the region associated with the failure. ([GTM-F166755](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-004_Release_Notes.html#GTM-F166755))

* <a name="GTM-F167559"></a>MUPIP CREATE creates GT.M V6.x (YottaDB r1.x) database files when the environment variable ydb_db_create_ver is defined as [V]6, or when the command line specifies `-v6`. The `-nov6` command line option allowing overrides a ydb_db_create_ver and creates a GT.M V7.x (YottaDB r2.x) database file. This means r2.02 can operate seamlessly with YottaDB r1.x (GT.M V6.x) databases. r2.00 did not have this support. ([GTM-F167559](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-004_Release_Notes.html#GTM-F167559))

* <a name="GTM-F171004"></a>YottaDB audit logging facilities use `tty` to label the standard input of the process. YottaDB places `tty=ttyname` before the command field in all audit log messages. If the standard input at process startup is not a terminal device, YottaDB logs `tty=0`. In addition, the audit facilities check for errors at the time of closing a socket / terminating a connection and report them with a YDB-E-SOCKCLOSE message to the operator log. The audit logger/listener sample programs (downloadable from the A&O Guide) switch their log files after receiving a SIGHUP signal. The switched log file has a suffix "_%Y%j%H%M%S" (yearjuliendayhoursminutesseconds) and the naming convention is similar to what YottaDB uses for switching journal files. YottaDB recommends periodically switching logger files. As deleting an active audit log file makes it lost to new processes, while existing processes continue to use it, YottaDB recommends against taking such a step. The sample programs have a Makefile. Previously, the audit log facilities did not provide tty information, did not check and report on errors during socket close, the logger/listener programs did not implement a log file switching mechanism, and those programs had no Makefile. ([GTM-F171004](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-005_Release_Notes.html#GTM-F171004))

* <a name="GTM-F188829"></a>The previous behavior reported here was never in a YottaDB release, and the following appears here for completeness:

  When the restriction file contains a line specifying AM_ENABLE, commands typed at the MUPIP prompt (MUPIP>) are audit logged and executed the same as MUPIP shell commands. Note that MUPIP returns to the shell after each command. Previously, when this facility was enabled, all commands typed at the MUPIP prompt (MUPIP>) produced the RESTRICTEDOP error. ([GTM-F188829](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-005_Release_Notes.html#GTM-F188829))

* <a name="GTM-DE201305"></a>When the Database segment to file mapping uses an environment variable, and the environment variable is not defined, MUPIP BACKUP uses the environment variable name itself in constructing the file name, as does MUPIP CREATE. Previously MUPIP CREATE created database with undefined environment variable name as the database name, but MUPIP BACKUP failed to backup that existing database. ([GTM-DE201305](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-002_Release_Notes.html#GTM-DE197637290))

* <a name="GTM-DE201381"></a>MUPIP LOAD FORMAT=BINARY checks the record length against the data for a record. YottaDB defines record length as the data in a node, but the utility in question still inappropriately included the key in its length check. ([GTM-DE201381](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-002_Release_Notes.html#GTM-DE197637458))

* <a name="GTM-DE201825"></a>The `ydbinstall` script checks removes any `semsstat2`, `ftok`, and `getuid` files if they exist from a prior installation. YottaDB releases starting from r1.36 no longer use these programs. Previously, `ydbinstall` did not remove these obsolete executable files. Note that YottaDB recommends installing each YottaDB release in its own directory, and not overwriting an existing release. Installing in an existing directory must be explicitly specified with the `--ovewrite-existing` command line option. ([GTM-DE201825](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-002_Release_Notes.html#GTM-DE197637999))

* <a name="GTM-DE294185"></a>The Receiver Server continues to operate after an unexpected termination of a TLS connection. Previously, and more frequently with OpenSSL 3.0, the Receiver Server would issue a TLSIOERR error message and terminate in response to network disruptions. ([GTM-DE294185](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-003_Release_Notes.html#GTM-DE294185))

* <a name="GTM-DE326986"></a>YottaDB processes detach from shared memory associated with MUPIP INTEG snapshots correctly. Previously, relatively idle YottaDB processes could remain attached to such shared memory segments on unjournaled regions or when the journal file switched while the snapshot was active, which prevented YottaDB process rundown from removing shared memory. The workaround was to use a subsequent MUPIP RUNDOWN. ([GTM-DE326986](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-005_Release_Notes.html#GTM-DE326986))

* <a name="GTM-DE340860"></a>MUPIP BACKUP does not leave temporary files when there in an error during the copy phase for a multi-region backup. Previously, MUPIP BACKUP incorrectly left temporary files when there was an error during the copy phase for any region except the first. ([GTM-DE340860](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-005_Release_Notes.html#GTM-DE340860))

### Other

* <a name="GTM-F134877"></a>As all platforms Supported by YottaDB r2.02 use OpenSSL 1.1.0 or later, and as support for ECDHE cipher support with OpenSSL 1.0.2 has been available since YottaDB r1.32, the following appears here for completness.

  The YottaDB TLS plugin library supports ECDHE ciphers with OpenSSL 1.0.2. OpenSSL 1.1.0 and above automatically support ECDHE ciphers. ([GTM-F134877](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-003_Release_Notes.html#GTM-F134877))

* <a name="GTM-F135258"></a>As support for TLS 1.3 has been available since YottaDB r1.32, the following appears here for completeness.

  The YottaDB TLS reference encryption plugin supports TLS v1.3 with OpenSSL 1.1.1 series and up. OpenSSL negotiates that highest level of TLS available between both sides of a connection. The implication is that just using OpenSSL 1.1.1 and up does not guarantee the use of TLSv1.3 for all TLS sessions. Previously the TLS reference encryption plugin supported only up to TLS v1.2.

  **Note**

    TLSv1.3 introduces a number of protocol changes. The concept of renegotiation was removed. YottaDB used TLSv1.2 renegotiation to ensure appropriate refresh of session keys (for rationale, see [Luykx, A. and K. Paterson, "Limits on Authenticated Encryption Use in TLS", 2016](https://eprint.iacr.org/2024/051.pdf)). As a result, YottaDB uses the term "renegotiation" for SOCKET devices where the mode is "server" and "renegotiation_interval" for the frequency of renegotiation (by the Source Server) during database replication. For TLSv1.3 sessions, the YottaDB Reference TLS plugin treats the "renegotiation" as a request to update the session keys.

  **Important**

    OpenSSL 3.0 by default does not allow client-side initiated TLSv1.2 renegotiation requests due to potential DoS attacks. Because of this, the YottaDB Reference TLS plugin built with a pre-OpenSSL 3.0 version does not support client-initiated renegotiation when communicating with a plugin build with OpenSSL 3.0. This limitation only affects database replication and not SOCKET devices.

  ([GTM-F135258](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-003_Release_Notes.html#GTM-F135258))

* <a name="GTM-F135313"></a>When an event interrupts a POSIX fallocate system call used for database file creation or extension with NODEFER_ALLOCATE, YottaDB retries the call. Previously, such an interruption resulted in a PREALLOCATEFAIL message. ([GTM-F135313](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-003_Release_Notes.html#GTM-F135313))

* <a name="GTM-F135319"></a>When a single operation exceeds both the STACKCRIT and STACKOFLOW limits, YottaDB issues a STACKCRIT message to the operator log, and exits with a STACKOFLOW error. Note this condition is fatal and not trappable. If this occurs, look into the ydb_mstack_crit and ydb_mstack_size environment variables. Previously, when this occurred, the process failed with a segmentation violation (SIG-11). ([GTM-F135319](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-005_Release_Notes.html#GTM-F135319))

* <a name="GTM-F135334"></a>The YottaDB reference encryption plugin correctly reports OpenSSL messages when a process reports an error message from either a TLS connection or database encryption. Previously processes leveraging the reference encryption plugin with OpenSSL could see incorrect (potentially empty) error messages. This was only seen in a development environment and never reported by a user. ([GTM-F135334](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-003_Release_Notes.html#GTM-F135334))

* <a name="GTM-F135355"></a>YottaDB prevents internal error ERR_DRVLONGJMP from becoming user visible after failure to open a statsDB. Previously, there were cases in which the error's mnemonic could be found in $ZSTATUS following attempts to open statsDBs in nonexistent directories. ([GTM-F135355](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-005_Release_Notes.html#GTM-F135355))

* <a name="GTM-F135415"></a>The Source Server defers a TLS renegotiation when a prior TLS renegotiation is pending. Previously, the Source Server logged the REPLWARN message, closed the replication connection, and attempted to re-establish the replication connection with the Receiver Server. ([GTM-F135415](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-002_Release_Notes.html#GTM-DE197637428))

* <a name="GTM-F158404"></a>When using OpenSSL 3.0+, the YottaDB encryption plugin uses automatic Diffie-Hellman parameters. This action is the recommended behavior from the OpenSSL project. Previously, the plugin allowed Diffie-Hellman parameter configuration files which required the user understand how those parameter configuration options affected the Diffie-Hellman key exchange.

  The YottaDB encryption plugin applies the configuration directed cipher suites, cipher-list, to TLSv1.3 sessions. Previously, the CipherSuites configuration only applied to pre-TLSv1.3 sessions.

  Replication encrypted using the YottaDB encryption plugin may perform TLSv1.2 re negotiations when the Receiver Server's plugin was compiled with OpenSSL 3.0+. Previously, these would fail with the error message "no renegotiation". ([GTM-F158404](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-004_Release_Notes.html#GTM-F158404))

* <a name="GTM-F188844"></a>When configured for auto-relink, YottaDB uses a lightweight operation to validate an existing relinkctl file. Previously, every YottaDB process using a relinkctl file would use a heavyweight operation to validate it, which added to routine ZLINK latency on the process' first lookup in the auto-relink directory. In particular, this may have resulted in visible latency when starting a large number of JOB processes if the JOB routine search included one or more auto-relink directories. ([GTM-F188844](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-005_Release_Notes.html#GTM-F188844))

* <a name="GTM-DE201392"></a>YottaDB provides protection from asynchronous events during routine management. Previously, asynchronous events like MUPIP STOP at inopportune times could cause a segmentation violation (SIG-11). This was only seen in a development environment and was never reported by a user. ([GTM-DE201392](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-002_Release_Notes.html#GTM-DE197637472))

* <a name="GTM-DE222430"></a>YottaDB handles SIGHUP appropriately when $PRINCIPAL has HUPENABLE set; in r2.00 owing to changes in deferred event handling, error handing could encounter a GTMASSERT2. In addition, a TERMHANGUP error implicitly sets the device to NOHUPENABLE, so should a process anticipate multiple disconnects/hangups, it should explicitly issue a USE $PRINCIPAL:HUPENABLE. Also, ZSHOW "D" displays the HUPENABLE state for $PRINCIPAL. ([GTM-DE222430](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-002_Release_Notes.html#GTM-DE197637998))

* <a name="GTM-DE309281"></a>YottaDB handles the interruption of certain socket operations correctly. Previously, asynchronous events like MUPIP STOP while creating a socket connection could cause a segmentation violation (SIG-11). This was only seen in a development environment and was never reported by a user. ([GTM-DE309281](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-005_Release_Notes.html#GTM-DE309281))

## Additional Information

## Messages

### New messages

**AUDCONNFAIL**, Audit XXXX facility failed to connect to audit logger

Run Time Error: The facility for logging activity generated by the use of XXXX is enabled, but is unable to form a connection with its configured logging program. This prevents a process from taking actions configured for logging when using XXXX.

Action: Check to make sure logger program is running and listening/accepting connections. If using a TCP or TLS-enabled logger, make sure the port number the logger is listening/accepting on matches the port number provided in the restriction file. Ensure the provided information (logger's connection info) in the restriction file is correct. Also make sure the line in restriction file is in correct format. If running a TLS-enabled logger, make sure the logger's TLS certificate is signed by a root Certificate Authority specified in the YottaDB TLS configuration file. Check syslog or additional messages for more information. After addressing identified issues, restart the XXXX process.

**AUDINITFAIL**, Audit XXXX facility failed to initialize audit information

Run Time Error: YottaDB was unable to process or initialize the provided information (e.g. IP, hostname, port number, UNIX domain socket file path, or TLS ID) from the restriction file. This prevents a process from taking actions configured for logging when using XXXX.

Action: Check the restriction file to make sure information is in proper format. After addressing identified issues, restart processes using XXXX.

**AUDLOGFAIL**, Audit XXXX facility failed to log activity

Run Time Error: YottaDB was unable to send the to-be-logged activitiy by XXXX to logger. This prevents a process from taking the action when using XXXX.

Action: Check to make sure that YottaDB is able to successfully connect to the logger program. Check syslog or additional messages for more information.

**GDELOGFAIL**, GDE failed to log command. Check operator log for more information

GDE Error: This message appears when LGDE is specified with the AZA_ENABLE facility and there is a problem with logging the GDE commands. This message also prevents the execution of the GDE command.

Action: Review the operator log for the exact reason of the failure to log the GDE command

**MALLOCCRIT**, Memory allocation critical due to request for bbbb bytes from aaaa

All YottaDB Components Warning: Indicates a YottaDB process exceeded the memory allocation threshold established with $ydb_malloc_limit / $ZMALLOCLIM with a request for bbbb bytes. The address aaaa gives a location in a YottaDB executable, likely only useful to your YottaDB support channel.

Action: Consider diagnosing the process behavior. For example, look for a resource leak, or a more resource efficient approach. The size of the request may be helpful in indicating how aggressively the process is growing. The MALLOCRIT invokes the error handler, and may need special handling to resume execution at the point it was detected. By default, some later request for memory is likely to produce a fatal MEMORY error, unless a subsequent set of $ZMALLOCLIM reestablishes the same or higher limit not exceeding any system limit. MEMORY errors are fatal and terminate the process. Independent of this mechanism, the OS may kill the process without recourse if it determines the greed of the process for memory jeopardizes the viability of the system.

**RLNKINTEGINFO**, Integrity check completed successfully: xxxx -- called from module yyyy at line zzzz

Run Time Information: Indicates relinkctl integrity check completed successfully by performing the action described by xxxx. YottaDB issues this message after RLNKRECNFL, otherwise, it means the relinkctl integrity check failed. This message was called from the yyyy module at zzzz line, and it goes to the operator log.

Action: If the integrity check result succeeds, the process can continue, otherwise, YottaDB generates a trappable error, right after this message. Contact your YottaDB support channel to discuss what might have caused the issue.

**RLNKRECNFL**, Conflict on relinkctl file rrrr for $ZROUTINES directory dddd, running an integrity check

Run Time Warning: Indicates a process encountered an issue attempting to attach to a routine object in dynamically linked library associated with directory dddd, and initiated an integrity check of the library control structures associated with relinkctl file rrrr. YottaDB sends this message to the operator log.

Action: If the check finds no problem or can correct any abnormality it finds (look for the RLNKINTEGINFO message in the operator log, to have more information about the integrity check result), the process can continue, otherwise, YottaDB generates a trappable error. Contact your YottaDB support channel to discuss what might have caused the issue.

As noted in ([GTM-DE201386](#GTM-DE201386)), YottaDB already protects itself from overly long arguments, and will never report this error. The following appears here for completeness.

**SOCKCLOSE**, Error closing socket: (errno = aaaa) xxxx

Operator log/All YottaDB Components Error: aaaa contains the OS error code and xxx indicates information about the error that occured while closing a socket connection for the process attempting to log a command to the audit logging facility.

Action: Review the message to determine whether the logger programs are running or whether a change is required in the configuration of your audit logging facility.

**VIEWARGTOOLONG**, The argument length LLLL to VIEW command vvvv exceeds the maximum mmmm

Run Time Error: An argument to the VIEW command vvvv has a length LLLL bytes that exceeds the maximum supported length of mmmm.

Action: Reduce the length of the VIEW command argument to no more than mmmm bytes.

### Revised messages

**STACKCRIT**, Stack space critical

Run Time Error: This indicates that the process has consumed almost all of the available stack space.

Action: If you do not take immediate action to reduce stack usage, YottaDB is likely to produce a STACKOFLOW error, which terminates the process. There are two common causes:

 * Infinite recursion. The most common cause of infinite recursion is a buggy error trap. If you do not take immediate action to reduce your stack, YottaDB is likely to produce a STACKOFLOW error, which terminates the process. Examine the stack with ZSHOW. Trim the stack using QUIT, ZGOTO, HALT or ZHALT. Note that if a single application call generates a stack frame larger than the stack space between the STACKCRIT and STACKOFLOW boundaries, the process puts a STACKCRIT message in the operator log, but processes the STACKOFLOW and terminates, so the application gets no chance to intervene and handle the error. Look into the ydb_mstack_crit_threshold and ydb_mstack_size environment variables.

 * Application level memory leak caused by NEW of local variables in a loop. Correct the application code to NEW variables outside any loop, and replace the NEW inside loops with KILL.

**STACKOFLOW**, Stack overflow

Run Time Fatal: This indicates that the process required more stack space than was available in memory.

Action: Reduce the stack when you get a STACKCRIT error. This error terminates the process. Note that if a single application call generates a stack frame larger than the stack space between the STACKCRIT and STACKOFLOW boundaries, the process puts a STACKCRIT message in the operator log, but processes the STACKOFLOW and terminates, so the application gets no chance to intervene and handle the error. Look into the ydb_mstack_crit_threshold and ydb_mstack_size environment variables.

**TPFAIL**, Transaction COMMIT failed

Run Time Error: This indicates that YottaDB attempted to process this transaction four times, but encountered an error every time. Additional accompanying messages indicate details of the failure.

Action: Report this database error to the group responsible for database integrity at your operation.

### Obsolete Messages

The following messages are no longer used:

## Legal Stuff

Copyright © 2024 YottaDB LLC. Portions Copyright Fidelity National Information Services, Inc. and/or its subsidiaries.

Permission is granted to copy, distribute and/or modify this document under the terms of the GNU Free Documentation License, Version 1.3 or any later version published by the Free Software Foundation; with no Invariant Sections, no Front-Cover Texts and no Back-Cover Texts.

YottaDB® and Octo® are registered trademarks of YottaDB LLC. GT.M™ is a trademark of Fidelity National Information Services, Inc. Other trademarks belong to their respective owners.

This document contains a description of YottaDB and the operating instructions pertaining to the various functions that comprise the software. This document does not contain any commitment of YottaDB LLC. YottaDB LLC believes the information in this publication is accurate as of its publication date; such information is subject to change without notice. YottaDB LLC is not responsible for any errors or defects.
