<!---
.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2024 YottaDB LLC and/or its subsidiaries.     #
.. # All rights reserved.                                        #
.. #                                                             #
.. # Portions copyright (c) 2021 Fidelity National Information   #
.. # Services, Inc. and/or its subsidiaries.                     #
.. # All rights reserved.                                        #
.. #                                                             #
.. #     This document contains the intellectual property        #
.. #     of its copyright holder(s), and is made available       #
.. #     under a license.  If you do not know the terms of       #
.. #     the license, please stop and do not read further.       #
.. #                                                             #
.. ###############################################################
.. Although this document was not created by FIS, it contains text
.. from FIS GT.M user documentation. Therefore, it carries an FIS
.. copyright.
-->

# YottaDB r2.00

## Binary Distributions

| sha256sum                                                          | file                                                                                                                     |
|--------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------|
| `45703de102b99d1fc2bd40e8d8436bf244d7a7bdaa08bbb77d2d8ceb49c799d2` | [yottadb_r200_aarch64_debian12_pro.tgz](/uploads/6e053c63d627671963a6ac9940613aef/yottadb_r200_aarch64_debian12_pro.tgz) |
| `6354378fe0ba74337ef40c2a59f43a7b43a7bee4ab4ded916dad2122ed0a8282` | [yottadb_r200_armv6l_debian12_pro.tgz](/uploads/da3ecebeea5de4d06b6b99ce214ff632/yottadb_r200_armv6l_debian12_pro.tgz)   |
| `4114ed640ed7cf8c60e05d3569001fa0277e43932e6fbdbe3502d0243b826147` | [yottadb_r200_x8664_debian12_pro.tgz](/uploads/170f0bf0932017d21d7aae0b0513be7d/yottadb_r200_x8664_debian12_pro.tgz)     |
| `7e793cb9215f9746e790c8025943c2213193cd34dd13b498f258b7690431ac79` | [yottadb_r200_x8664_rhel8_pro.tgz](/uploads/a601efd2a23b818313d6298c2cfff039/yottadb_r200_x8664_rhel8_pro.tgz)           |
| `767f7e03754680700150c75acf0a4d5cccce92ea90b9b2a1b4d42bf8f357b162` | [yottadb_r200_x8664_rhel9_pro.tgz](/uploads/aeffdc2e50c64f80f9ca8851cae4d92a/yottadb_r200_x8664_rhel9_pro.tgz)           |
| `1c164538c8f0d7167c41a8cdcc33d2aa618a76f80499abc07763fe566d9df8e6` | [yottadb_r200_x8664_sle15_pro.tgz](/uploads/39b5073e5ab016ccca4f6e84aa9cc1a7/yottadb_r200_x8664_sle15_pro.tgz)           |
| `41f970bd8350ab27d7eee364e3e0b3308545e6c480f15eba235e6dc79b93e56c` | [yottadb_r200_x8664_ubuntu2204_pro.tgz](/uploads/d92e847a97516185185b1c363034fd5c/yottadb_r200_x8664_ubuntu2204_pro.tgz) |

## Release Note Revision History

| Revision | Date              | Summary                                            |
|----------|-------------------|----------------------------------------------------|
| 1.00     | February 23, 2024 | r2.00 Initial Release                              |
| 1.01     | February 29, 2024 | Update information for non-blocking SOCKET devices |

## Contact Information
### YottaDB LLC
https://yottadb.com /  <info@yottadb.com>

### Support
#### Customers
Contact YottaDB or your YottaDB support channel for support with assured service levels from knowledgeable staff.

#### Others
For free (to you) support from members of communities who run widely available applications on YottaDB, please use an application-specific list where appropriate.

 - For access from node.js via [Nodem](https://github.com/dlwicksell/nodem), [post an Issue on the Nodem project](https://github.com/dlwicksell/nodem/issues/new/).
 - For access from [QewdJS](http://qewdjs.com/), or [EWD.js](https://github.com/robtweed/ewd.js), or from node.js via [mg-dbx](https://github.com/chrisemunt/mg-dbx) or [mg-dbx-napi](https://github.com/chrisemunt/mg-dbx-napi) post to the [Enterprise Web Developer community](https://groups.google.com/forum/#!forum/enterprise-web-developer-community).
 - For requests specific to the use of YottaDB with [VistA](https://en.wikipedia.org/wiki/VistA) flavors, post to the [Hardhats](https://groups.google.com/forum/#!forum/hardhats) list.
 - For requests specific to use of YottaDB with the [M Language](https://docs.yottadb.com/ProgrammersGuide/), post to the [Everything MUMPS](https://groups.google.com/g/everythingmumps) list.
 - For requests other than to the communities above:
   - [Join the YottaDB Discord server](https://discord.gg/bFHmDdzcqY) and post to one of the channels.
   - [Post an Issue](https://gitlab.com/YottaDB/DB/YDB/-/issues) and include the words "Help Wanted" in the Title.

## r2.00
### Overview
YottaDB r2.00 is a major new release with substantial new functionality and database format enhancements.

* Inherited from the upstream GT.M V7.0-000, YottaDB r2.00 creates database files of up to 16Gi blocks. For example, the maximum size of a database file with 4KiB blocks is 64TiB, which means you can use fewer regions for extremely large databases. With YottaDB r2.00, you can continue to use database files created by r1.x releases, except that the maximum size of a database file created with prior YottaDB releases remains unchanged.
* For direct mode, as well as utility programs, YottaDB can optionally use [GNU Readline](https://tiswww.case.edu/php/chet/readline/), if it is installed on the system. This includes the ability to access and use command history from prior sessions. ([YDB#88](#x88))
* Listening TCP sockets can be passed between processes. ([YDB#996](#x996))
* The `ydbinstall` / `ydbinstall.sh` script has multiple enhancements.

In addition to enhancements and fixes made by YottaDB, r2.00 inherits numerous other enhancements and fixes from GT.M V7.0-000 and V7.0-001, all described in the [Change History](#change-history).

### Platforms
A platform is a combination of a CPU architecture and an operating system. A platform is Supported, Supportable, or Unsupported.

* Supported means that we have the platform in our development environment, test each release on that platform, and provide a binary distribution for it.
* Supportable means that although we do not necessarily have such a platform in our environment, we have no reason to believe that the software will not run on it.
* All others are Unsupported.

| CPU Architecture                    | Supported OS Version(s)                                                              | Notes                                                                                                                         |
| ----------------------------------- | ------------------------------------------------------------------------------------ | ----------------------------------------------------------------------------------------------------------------------------- |
| 64-bit x86                          | Ubuntu 22.04 LTS; Red Hat Enterprise Linux 8.x; Red Hat Enterprise Linux 9.x; SUSE Linux Enterprise 15.x; Debian GNU/Linux 12 (Bookworm) | There are separate binary distributions for each OS version, owing to differences in library versions of those distributions. |
| 64-bit ARM (Raspberry Pi 3 Model B) | Debian GNU/Linux 12 (Bookworm)                                                       | See below.                                                                                                                    |
| 32-bit ARM (Raspberry Pi Zero)      | Debian GNU/Linux 12 (Bookworm)                                                       | See below.                                                                                                                    |

Recent releases of major GNU/Linux distributions with contemporary kernels, glibc and ncurses are Supportable. Specific notes:

- [ydbinstall.sh](https://gitlab.com/YottaDB/DB/YDB/-/blob/master/sr_unix/ydbinstall.sh) recognizes [Rocky Linux](https://rockylinux.org/) 8 as equivalent to RHEL 8, and [OpenSUSE Leap](https://www.opensuse.org/#Leap) 15.x as equivalent to SUSE Linux Enterprise 15.x, installing the releases for the corresponding distributions. Note that Rocky Linux and OpenSUSE Leap are Supportable, not Supported.
- On [Arch Linux](https://www.archlinux.org/) and other leading edge distributions such as [OpenSUSE Tumbleweed](https://www.opensuse.org/#Tumbleweed), and newer OS versions such as Red Hat Enterprise Linux 9.x, YottaDB will need to be recompiled from source code owing to library and tool chain versions newer than those used in building Supported distributions. The `--from-source` option of [ydbinstall.sh](https://gitlab.com/YottaDB/DB/YDB/-/blob/master/sr_unix/ydbinstall.sh) simplifies the process.
- While YottaDB is Supportable on other ARMv6, [ARMv7-A](https://en.wikipedia.org/wiki/Comparison_of_ARMv7-A_cores), and [ARMv8-A](https://en.wikipedia.org/wiki/Comparison_of_ARMv8-A_cores) CPUs, owing to variations in the implementations of ARM microarchitectures, we recommend that you ensure the software runs correctly before committing to any specific hardware other than those listed above. Please [contact us](mailto:info@yottadb.com) if you want a specific combination of OS and CPU microarchitecture to be Supported.

### Installation
See our [Get Started page](https://yottadb.com/product/get-started) to use YottaDB.

We **strongly recommend** that you install YottaDB r2.00 in a newly created directory, different from those of prior YottaDB releases and any GT.M versions you may have installed on the system.

### Removing an installed YottaDB release
Assuming `$ydb_dist` points to the directory where YottaDB is installed:

 - Cleanly shut down all application processes using that release.
 - Execute `mupip rundown && mupip rundown -relinkctl` and review the output to ensure successful completion.
 - Ensure that there are no `gtcm*` or `gtmsecshr` processes active.
 - Use `sudo lsof | grep $ydb_dist` to ensure there are no open files.
   - If there are links to files in `$ydb_dist`, e.g., from `/usr/local/bin/` or `/usr/local/etc/`, remove the links.
 - Delete the directory with `sudo rm -rf $ydb_dist`

## Upgrading to YottaDB r2.00
YottaDB r2.00 is upward compatible from YottaDB [r1.38](https://gitlab.com/YottaDB/DB/YDB/-/releases/r1.38), GT.M [V7.0-000](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-000_Release_Notes.html) and [V7.0-001](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-001_Release_Notes.html). The minimal upgrade steps are:

* Install YottaDB r2.00. Use `ydbinstall` / `ydbinstall.sh` to install plugins you use.
* Install plugins you need in addition to those installed in the previous step, e.g., non-YottaDB plugins that are not installed by  `ydbinstall` / `ydbinstall.sh`.
* Recompile object code, and recreate shared libraries if your application uses shared libraries.
* If your application uses encryption, compile and install the reference implementation plugin (if not done by the `ydbinstall` / `ydbinstall.sh` script) or your customized plugin.
* Cleanly shut down the application and ensure that the database files are shut down using MUPIP RUNDOWN from the prior release.
* Switch journal files with the new YottaDB release.
* Start using the new YottaDB release.

If the database has triggers defined with non-canonical numbers, or numbers specified as strings with any version prior to [r1.28](https://gitlab.com/YottaDB/DB/YDB/-/releases/r1.28), or if you are unsure, extract the trigger definitions, delete existing triggers, and reload the trigger definitions. Issue [[#430](https://gitlab.com/YottaDB/DB/YDB/-/issues/430)] from [r1.28](https://gitlab.com/YottaDB/DB/YDB/-/releases/r1.28) has a series of steps you can copy and execute. There is no need to do this if upgrading from [r1.28](https://gitlab.com/YottaDB/DB/YDB/-/releases/r1.28) or later.

To upgrade from older GT.M releases, first upgrade to GT.M V6.0-000 or later and follow the steps above, or contact your YottaDB support channel for assistance and guidance.

A more sophisticated upgrade technique is:

* Install YottaDB r2.00.
* Create a new replicated instance of your application (on the same system or a different system).
* Assuming the existing instance is A, and the new instance is B, upgrade B to r2.00 and start replicating from A to B.
* Once B catches up, switchover so that B is in a primary role replicating to A.
* Once you are satisfied with B, remove (or upgrade) A.

As discussed [below](#GTM-8913), in the event you need database files that need to grow beyond the 992Mi block limit of database files created with r1.x / GT.M V6.x, you should extract data from r1.x database files and load the extracted data into database files created using r2.00.


## Change History

### r2.00

YottaDB r2.00 includes the following enhancements and fixes beyond YottaDB [r1.38](https://gitlab.com/YottaDB/DB/YDB/-/releases/r1.38).

| ID               | Category              | Summary                                                                                                      |
|------------------|-----------------------|--------------------------------------------------------------------------------------------------------------|
| ([88](#x88))     | Other                 | GNU Readline for direct mode, DSE, LKE and MUPIP                                                             |
| ([427](#x427))   | Other                 | %GO outputs large global nodes in single lines                                                               |
| ([767](#x767))   | Other                 | Friendlier ydbinstall warnings about missing utilities                                                       |
| ([780](#x780))   | Other                 | YottaDB r2.00 includes fixes and enhancements from GT.M V7.0-000                                             |
| ([835](#x835))   | Other                 | YottaDB r2.00 includes fixes and enhancements from GT.M V7.0-001                                             |
| ([851](#x851))   | System Administration | MUPIP commands accept either space or "=" after "-region"                                                    |
| ([970](#x970))   | System Administration | %YDBSYSLOG for sophisticated analytics, forensics and troubleshooting of syslog data                         |
| ([994](#x994))   | Other                 | Fix bugs exposed by fuzz testing in YottaDB r2.00                                                            |
| ([996](#x996))   | Languages             | WRITE /PASS and WRITE /ACCEPT allow also LISTENING TCP sockets to be passed from one process to another      |
| ([997](#x997))   | Other                 | ydbinstall included in installed YottaDB directory; checks for dependencies; less chatty                     |
| ([998](#x998))   | Database              | TSTART does not open the default process global directory                                                    |
| ([1011](#x1011)) | Other                 | Please see [997](#x997)                                                                                      |
| ([1019](#x1019)) | Languages             | More robust signal handling for non-M main programs                                                          |
| ([1021](#x1021)) | System Administration | MUPIP SET JOURNAL switches older format journal files                                                        |
| ([1022](#x1022)) | Other                 | ydbinstall option to install %YDBSYSLOG                                                                      |
| ([1023](#x1023)) | Other                 | ydb_env_set works with zsh                                                                                   |
| ([1025](#x1025)) | Other                 | %YDBJNLF runs in a Docker container without /proc filesystem permissions                                     |
| ([1026](#x1026)) | Languages             | Output files with STREAM & NOWRAP deviceparameters do not require large WIDTH parameter to avoid line breaks |
| ([1028](#x1028)) | Other                 | ydbinstall / ydbinstall.sh flag --webserver installs YottaDB web server                                      |
| ([1033](#x1033)) | Languages             | $ZCMDLINE can be stacked with NEW and updated with SET                                                       |
| ([1037](#x1037)) | Languages             | Repeated invocation of a $ZTRAP error handler does not result in a STACKCRIT error                           |
| ([1040](#x1040)) | Other                 | ydb_env_set enables GNU Readline by default                                                                  |
| ([1041](#x1041)) | Database              | MUPIP REORG TRUNCATE operates correctly with concurrent database file extensions                             |
| ([1043](#x1043)) | Other                 | ydbinstall uses ld.gold to build libyottadbutil.so                                                           |
| ([1047](#x1047)) | System Administration | MUPIP INTEG STATS with VIEW "STATSHARE" and ydb_app_ensures_isolation works                                  |
| ([1049](#x1049)) | Langunages            | All 64 bits of int64/uint64 return values from external calls are returned                                   |
| ([1050](#x1050)) | Languages             | Type int* accepted as a return value or parameter by call-in table                                           |
| ([1051](#x1051)) | System Administration | MUPIP BACKUP DBG cleans up temporary files after error exit                                                  |
| ([1054](#x1054)) | Languages             | Call-in/out support 64-bit integer pointer types on 32-bit architectures                                     |
| ([1058](#x1058)) | Languages             | Raise error for illegal call-out parameter output (O) types                                                  |
| ([1062](#x1062)) | System Administration | MUPIP TRIGGER STDIN prints correct line numbers                                                              |
| ([1063](#x1063)) | Languages             | Improve accuracy of issuing EXTCALLBOUNDS error                                                              |

<a name="gtmv70000"></a>
### GT.M V7.0-000

YottaDB r2.00 incorporates enhancements and fixes from [GT.M V7.0-000](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-000_Release_Notes.html).

| ID                      | Category              | Summary                                                                                                                                                   |
|-------------------------|-----------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------|
| ([GTM-6952](#GTM-6952)) | System Administration | Utilites accept decimal or hexadecimal input in many places                                                                                               |
| ([GTM-8262](#GTM-8262)) | System Administration | The installation script prints proper error messages if UTF-8 dependencies are not met                                                                    |
| ([GTM-8263](#GTM-8263)) | System Administration | UTF8 installation simplifications                                                                                                                         |
| ([GTM-8288](#GTM-8288)) | System Administration | Install configuration leaves clean file authorizations                                                                                                    |
| ([GTM-8517](#GTM-8517)) | System Administration | YottaDB installation script records ownership, permissions, and OpenSSH SHA256 checksum values of files                                                   |
| ([GTM-8913](#GTM-8913)) | Database              | Revised database format supports up to 16Gi blocks in a region                                                                                            |
| ([GTM-9268](#GTM-9268)) | System Administration |JNLFLUSHLOG in the syslog if journal buffer flushing fails                                                                                                 |
| ([GTM-9302](#GTM-9302)) | System Administration | Acknowledged sequence number in MUPIP REPLICATE SOURCE SHOWBACKLOG and available with ^%PEEKBYNAME                                                        |
| ([GTM-9340](#GTM-9340)) | Languages             | VIEW and $VIEW() use the first 31 characters of overly long region names; and $VIEW() rejects multiple region names with a more appropriate error message |
| ([GTM-9343](#GTM-9343)) | System Administration | MUPIP FTOK JNLPOOL or RECVPOOL use the entire path for the replication instance file                                                                      |
| ([GTM-9344](#GTM-9344)) | System Administration | MUPIP FREEZE ONLINE with AIO freezes all regions at the same point in time                                                                                |
| ([GTM-9346](#GTM-9346)) | System Administration | Correct sleep period between soft tries connection attempts                                                                                               |
| ([GTM-9347](#GTM-9347)) | Languages             | Compiler optimization of out-of-range literals produces a NUMOFLOW error                                                                                  |
| ([GTM-9349](#GTM-9349)) | System Administration | YottaDB installation script does not attempt to install semstat2 and ftok utilities.                                                                      |
| ([GTM-9357](#GTM-9357)) | Other                 | %DSEWRAP removed from the distribution                                                                                                                    |
| ([GTM-9358](#GTM-9358)) | System Administration | MUPIP REPLICATE SOURCE ZEROBACKLOG SHUTDOWN appropriately cleans up IPC resources                                                                         |
| ([GTM-9361](#GTM-9361)) | System Administration | MUPIP REPLICATE ZEROBACKLOG uses sequence number from Receiver Server while determining the backlog                                                       |
| ([GTM-9362](#GTM-9362)) | Languages             | $TRANSLATE() with prior compilation errors does not terminate with segmentation violation                                                                 |
| ([GTM-9368](#GTM-9368)) | System Administration | A Source Server shutdown process exits with CTRL\_C                                                                                                       |
| ([GTM-9370](#GTM-9370)) | Languages             | All DIVZERO errors deferred to run-time                                                                                                                   |
| ([GTM-9371](#GTM-9371)) | Languages             | Ensure appropriate compilation of +<literal><nonrelational-Boolean-operator><gvn|@>                                                                       |

<a name="gtmv70001"></a>
### GT.M V7.0-001

| ID                      | Category              | Summary                                                                                                        |
|-------------------------|-----------------------|----------------------------------------------------------------------------------------------------------------|
| ([GTM-4272](#GTM-4272)) | System Administration | MUPIP BACKUP displays information in standard messages format                                                  |
| ([GTM-4814](#GTM-4814)) | Languages             | M-profiling (VIEW "TRACE") restored after ZSTEP                                                                |
| ([GTM-5148](#GTM-5148)) | System Administration | REPLAHEAD message for ROLLBACK                                                                                 |
| ([GTM-8010](#GTM-8010)) | Languages             | Appropriate handling of deviceparmeters on OPEN "dev/null" and EXCEPTION values in general                     |
| ([GTM-8681](#GTM-8681)) | System Administration | MUPIP BACKUP -RECORD stores the time of its start when it completes successfully                               |
| ([GTM-8843](#GTM-8843)) | Languages             | SOCKET devices support non blocking WRITEs                                                                     |
| ([GTM-9057](#GTM-9057)) | System Administration | MUPIP JOURNAL -EXTRACT to a FIFO device                                                                        |
| ([GTM-9131](#GTM-9131)) | Other                 | LOGTPRESTART appropriately identifies restarts due to less frequent reduced statsdb file extensions            |
| ([GTM-9213](#GTM-9213)) | Languages             | A process can SET the trailing portion of $SYSTEM                                                              |
| ([GTM-9324](#GTM-9234)) | Languages             | ZSTEP restored after MUPIP INTRPT or $ZTIMEOUT; ZSTEP subject to ZBREAK restriction                            |
| ([GTM-9333](#GTM-9333)) | Other                 | (See ([GTM-9333](#GTM-9333)) below)                                                                            |
| ([GTM-9363](#GTM-9363)) | System Administration | Source Server polling and TLS renegotiation improvements                                                       |
| ([GTM-9373](#GTM-9373)) | System Administration | Additional information from MUPIP REPLICATE SOURCE SHOWBACKLOG                                                 |
| ([GTM-9378](#GTM-9378)) | Languages             | Appropriate handling of pattern match in large compilations                                                    |
| ([GTM-9382](#GTM-9382)) | Other                 | Fix V7.0-000 issue with symbolic links and relative addresses for $gtm_dist (n.a. for YottaDB)                 |
| ([GTM-9388](#GTM-9388)) | Languages             | ZSHOW "B" shows the action                                                                                     |
| ([GTM-9392](#GTM-9392)) | Database              | NOISOLATION characteristics maintained correctly when databases and global directories are not aligned         |
| ([GTM-9400](#GTM-9400)) | System Administration | MUPIP REORG prevents MUPIP STOP from causing KILLABANDONED                                                     |
| ([GTM-9405](#GTM-9405)) | Other                 | Better diagnostic information from MUPIP JOURNAL for errors reading and writing flat files                     |
| ([GTM-9408](#GTM-9408)) | Languages             | Prevent an occasional indefinite HANG                                                                          |
| ([GTM-9409](#GTM-9409)) | Languages             | More context with certain JOBFAIL errors                                                                       |
| ([GTM-9410](#GTM-9410)) | System Administration | More robust performance when a global directory file is being updated                                          |
| ([GTM-9416](#GTM-9416)) | System Administration | Receiver server started with REUSE continues to operate across a connection reset                              |
| ([GTM-9422](#GTM-9422)) | Other                 | Counter statistics replace waiting state for critical code section waits                                       |
| ([GTM-9423](#GTM-9423)) | System Administration | MUPIP DUMPFHEAD recognizes the FLUSH qualifier                                                                 |
| ([GTM-9424](#GTM-9424)) | System Administration | Automatically select the best copy mechanism for MUPIP BACKUP                                                  |
| ([GTM-9425](#GTM-9425)) | Database              | $GTCM_<node-name> accepts "host-name:port-number"                                                              |
| ([GTM-9429](#GTM-9429)) | Languages             | $QLENGTH() and $QSUBSCRIPT() do tighter checking for canonic references                                        |
| ([GTM-9432](#GTM-9432)) | Languages             | ZTRIGGER with GT.CM results in REMOTEDBNOTRIG error                                                            |
| ([GTM-9434](#GTM-9434)) | Database              | Maximum tree depth of 11                                                                                       |
| ([GTM-9437](#GTM-9437)) | Languages             | Improvements to USE for SOCKET devices                                                                         |
| ([GTM-9441](#GTM-9441)) | System Administration | Cleaner build for the encrption plug-in on AIX (n.a. for YottaDB)                                              |
| ([GTM-9442](#GTM-9442)) | System Administration | Scripts no longer rely on /usr/bin/which (n.a. for YottaDB)                                                    |
| ([GTM-9443](#GTM-9443)) | System Administration | MUPIP SET JOURNAL more cautious with the journal file chain                                                    |
| ([GTM-9444](#GTM-9444)) | Languages             | Change in behavior when an indirect argument to a potentially argumentless command evalutes to an empty string |
| ([GTM-9451](#GTM-9451)) | Database              | TPNOACID releases database critical sections for LOCK operations that hang for lack of LOCKSPACE               |
| ([GTM-9452](#GTM-9452)) | Languages             | CLOSE deviceparameter REPLACE overwrites an existing file, which RENAME does not                               |

### Database

* <a name="x998"></a> [TSTART](https://docs.yottadb.com/ProgrammersGuide/commands.html#tstart) does not open the default process global directory. Previously, it did, causing problems when the process only accessed globals by extended reference or not accessing globals at all. [#998](https://gitlab.com/YottaDB/DB/YDB/-/issues/998)

* <a name="x1041"></a> [MUPIP REORG TRUNCATE](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#truncate) operates correctly with concurrent database file extensions. Previously, in such scenarios, it could in rare cases result in database damage. [#1041](https://gitlab.com/YottaDB/DB/YDB/-/issues/1041)

* <a name="GTM-8913"></a> r2.00 creates database files that can hold up to 16Gi blocks. This requires a new database format using 64-bit rather than 32-bit block numbers. The impact on storage efficiency is a function of the ratio of key size to block number size. Prior versions used 32-bit block numbers and recent r1.x versions supported a maximum of 992Mi blocks. YottaDB r2.00 supports database files created using any prior YottaDB releases. Migrating existing data from an r1.x database file to an r2.00 database file requires [MUPIP EXTRACT](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#extract) followed by a [MUPIP LOAD](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#load) of the extract into a database file created with r2.00.

  YottaDB recommends using a rolling upgrade process to maximize application availability. [GTM-8913](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-000_Release_Notes.html#GTM-8913)

* <a name="GTM-9392"></a> YottaDB correctly maintains [NOISOLATION](https://docs.yottadb.com/ProgrammersGuide/commands.html#noisolation-expr) characteristics for globals. Owing to a regression in r1.20, the application of NOISOLATION to globals may not have worked when there was a configuration difference between a region's maximum key size in the Global Directory and the database file header. The workaround was to ensure that the maximum key size settings were the same in the Global Directory and the database file header. [GTM-9392](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-001_Release_Notes.html#GTM-9392)

* <a name="GTM-9425"></a> As YottaDB already supports this syntax, the following GT.M release note is reproduced here for completeness.

  _The environment variable $GTCM_<node-name> accepts host-name and port-number in the form "host-name:port-number". Previously, host names were accepted only in the form [host-name]:port-number. [GTM-9425](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-001_Release_Notes.html#GTM-9425)_

* <a name="GTM-9434"></a> r2.00 creates databases with a maximum tree depth of 11 levels; previously the limit was 7. The workarounds were to use a large block size, or to map the data into multiple files. [GTM-9434](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-001_Release_Notes.html#GTM-9434)

* <a name="GTM-9451"></a> YottaDB issues a TPNOTACID message and releases all database critical sections it owns during any LOCK operation in the final retry that could result in an indefinite hang, e.g. LOCKSPACE full. Previously, LOCK operations with a timeout less than [$ydb_tpnotacidtime](https://docs.yottadb.com/AdminOpsGuide/basicops.html#ydb-tpnotacidtime) (or the default of 2 seconds), would not generate such an action. As a result, a process could hang in the final transaction processing retry. [GTM-9451](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-001_Release_Notes.html#GTM-9451)

### Languages

* <a name="x996"></a> WRITE /PASS and WRITE /ACCEPT allow also LISTENING TCP sockets to be passed from one process to another. Previously, only CONNECTED TCP sockets could be passed, and an attempt to pass a LISTENING TCP socket resulted in the WRITE /ACCEPT command raising a [GETSOCKNAMERR](https://docs.yottadb.com/MessageRecovery/errors.html#getsocknamerr) error. [#996](https://gitlab.com/YottaDB/DB/YDB/-/issues/996)

* <a name="x1019"></a> SIGINT/SIGTERM/SIGQUIT signals are specially handled in case of a non-YottaDB main application/program (e.g., a Simple API application):

  - The first call by the main program into YottaDB establishes YottaDB signal handlers for these signals overriding any signal handlers established by the main program.
  - On receipt of one of these signals, the YottaDB signal handler checks if the database engine is in a state where it can be safely interrupted.
	- If not safe, YottaDB defers handling the signal until the database engine is in a safe state.
	- If safe, the YottaDB signal handler invokes the signal handler, if any, defined by the main program. There are two special cases:
	  - If the main program has not established a signal handler for these signals, the default signal handler (`SIG_DFL`) would be in effect. Since these signals are considered fatal/terminating signals by YottaDB, it terminates the process.
	  - If the main program has explicitly set these signals to be ignored (`SIG_IGN`), then YottaDB signal does not drive any application signal handler for these signals.

  A consequence of this behavior for [Flask](https://palletsprojects.com/p/flask/) applications is that:

	- Ctrl-C on an interactive Flask application, that uses the YDBPython wrapper to make calls into YottaDB, stops the application because Flask sets up a signal handler for Ctrl-C (SIGINT) to terminate the process. Previously, repeated Ctrl-C did nothing to the application until the next YottaDB call at which point the process would terminate.
	- SIGTERM on a non-interactive Flask application, that uses the YDBPython wrapper to make calls into YottaDB, terminates the application because Flask sets the SIGTERM handler to `SIG_DFL` (the default signal handler). Previously, one had to send 3 SIGTERM signals to the Flask application to terminate it.

  Additionally, a non-M main program that uses non-threaded Simple API functions to make calls into YottaDB correctly handles all signals in the thread that receives the signal. Previously, many signals (including but not limited to SIGINT/SIGTERM/SIGQUIT) would be ignored) owing to an attempt to forward the signal to the thread that originally did the `ydb_init()` call, in case that thread no longer exists.

  [#1019](https://gitlab.com/YottaDB/DB/YDB/-/issues/1019)

* <a name="x1026"></a> An output file with the [STREAM](https://docs.yottadb.com/ProgrammersGuide/ioproc.html#stream) and [NOWRAP](https://docs.yottadb.com/ProgrammersGuide/ioproc.html#wrap) deviceparameters does not require a large [WIDTH](https://docs.yottadb.com/ProgrammersGuide/ioproc.html#width) deviceparameter to ensure that line breaks are not automatically inserted. Previously a large WIDTH deviceparameter was required. [#1026](https://gitlab.com/YottaDB/DB/YDB/-/issues/1026)

* <a name="x1033"></a> The [$ZCMDLINE](https://docs.yottadb.com/ProgrammersGuide/isv.html#zcmdline) environment variable can be stacked with the [NEW](https://docs.yottadb.com/ProgrammersGuide/commands.html#new) command and updated with the [SET](https://docs.yottadb.com/ProgrammersGuide/commands.html#set) command. [#1033](https://gitlab.com/YottaDB/DB/YDB/-/issues/1033)

* <a name="x1037"></a> Repeated invocation of the [$ZTRAP](https://docs.yottadb.com/ProgrammersGuide/isv.html#ztrap) error handler does not result in a [STACKCRIT](https://docs.yottadb.com/MessageRecovery/errors.html#stackcrit) error. Previously, it could. [#1037](https://gitlab.com/YottaDB/DB/YDB/-/issues/1037)

* <a name="x1050"></a> `int*` is accepted as a parameter to call-in table, and is equivalent to `ydb_int_t*`. [#1050](https://gitlab.com/YottaDB/DB/YDB/-/issues/1050)

* <a name="x1049"></a> All 64 bits of `int64` and `uint64` return values are returned to YottaDB from external calls. Previously they were truncated to 32 bits. Note that YottaDB itself is limited to 18 significant digits of numerical resolution. [#1049](https://gitlab.com/YottaDB/DB/YDB/-/issues/1049)

* <a name="x1054"></a> Call-ins and call-outs on 32-bit architectures support pointers to 64-bit integer parameters, i.e., `ydb_int64_t*` and `ydb_uint64_t*`. Note that `ydb_int64_t` and `ydb_uint64_t` remain supported only on 64-bit architectures and produce an error on 32-bit architectures. [#1054](https://gitlab.com/YottaDB/DB/YDB/-/issues/1054)

* <a name="x1058"></a> YottaDB raises an [UNIMPLOP](https://docs.yottadb.com/MessageRecovery/errors.html#unimplop) error for unsupported call-out parameter types in an [external call table](https://docs.yottadb.com/ProgrammersGuide/extrout.html#using-external-calls-call-outs).

  - Non-pointer types used as output types generate an error, since the C calling convention does not allow them to receive output data.
  - `ydb_pointertofunc_t`, `ydb_pointertofunc_t*` and `ydb_jbig_decimal_t` raise an error if declared as outputs since the first two types [make no sense as outputs](https://docs.yottadb.com/ProgrammersGuide/extrout.html#callback-functions), and `ydb_jbig_decimal_t` is [implemented as input-only](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTMJI_Technical_Bulletin.html#:~:text=BigDecimal-,gtm_jbig_decimal_t,-I).
  - `ydb_float_t`, `ydb_double_t`, `float`, and `double` produce an error in both directions. Use the pointer versions of of these types instead.

  A parameter of type `ydb_status_t` now raises a [ZCUNTYPE](https://docs.yottadb.com/MessageRecovery/errors.html#zcuntype) error. Previously, it raised a ZCMLTSTATUS error, a confusing error message which has been removed.

  [#1058](https://gitlab.com/YottaDB/DB/YDB/-/issues/1058)
  
* <a name="x1063"></a> The [call-out interface](https://docs.yottadb.com/ProgrammersGuide/extrout.html#using-external-calls-call-outs) allocates the appropriate amount of memory required to pass parameters. Previously, it allocated twice as much memory as required, which was wasteful, especially when large pre-allocation values are specified for strings. This also improves the detection of string overrun errors in the called external function which would previously have been silently permitted, provided they did not overrun the double space allocated. [#1063](https://gitlab.com/YottaDB/DB/YDB/-/issues/1063)

* <a name="GTM-4814"></a> YottaDB restores [VIEW "TRACE"](https://docs.yottadb.com/ProgrammersGuide/commands.html#trace-value-expr) operation (M-Profiling) after [ZSTEP](https://docs.yottadb.com/ProgrammersGuide/commands.html#zstep) operations. However, issuing a VIEW "[NO]TRACE" may interfere with ZSTEP operations. Note that using ZSTEP materially impacts M-Profiling times, so using these two facilities together may be problematic. Previously, ZSTEP usage usually turned off M-Profiling. [GTM-4814](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-001_Release_Notes.html#GTM-4814)

* <a name="GTM-8010"></a> `OPEN "/dev/null"` with deviceparameters works appropriately. The only deviceparameters YottaDB actually attempts to implement for `/dev/null` are [[NO]WRAP](https://docs.yottadb.com/ProgrammersGuide/ioproc.html#sequential-file-deviceparameter-summary) and [EXCEPTION](https://docs.yottadb.com/ProgrammersGuide/ioproc.html#sequential-file-deviceparameter-summary). At least in theory, the device should never give an exception. Previously, such an [OPEN](https://docs.yottadb.com/ProgrammersGuide/ioproc.html#open) handled deviceparameters inappropriately, which could cause unintended WRAP behavior or an attempt to instantiate an exception handler constructed out of garbage, which, in turn, could cause a segmentation violation (SIG-11). The workaround was to specify no deviceparmeters on an OPEN of `/dev/null`. In addition, YottaDB appropriately handles EXCEPTION values settings whose lengths are between 128 and 255 bytes long. Previously, YottaDB mishandled such settings potentially resulting in a segmentation violation (SIG-11). The second issue was never reported by a user, but was observed during development. [GTM-8010](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-001_Release_Notes.html#GTM-8010)

* <a name="GTM-8843"></a> SOCKET devices support non blocking WRITEs. For more information, refer to [Additional Information for GTM-8843](#addlgtm8843). [GTM-8843](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-001_Release_Notes.html#GTM-8843)

* <a name="GTM-9213"></a> YottaDB accepts [SET](https://docs.yottadb.com/ProgrammersGuide/commands.html#set) [$SYSTEM](https://docs.yottadb.com/ProgrammersGuide/isv.html#system)=`expr`, where `expr` appends to the initial value up to the length permitted for an initial value; an empty string removes any current added value. Initial values are determined by the `ydb_sysid` environment variable  preceded by `"47,"`. Previously, an attempt to SET $SYSTEM produced an [SVNOSET](https://docs.yottadb.com/MessageRecovery/errors.html#svnoset) error. [GTM-9213](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-001_Release_Notes.html#GTM-9213)

* <a name="GTM-9234"></a> YottaDB restores [ZSTEP](https://docs.yottadb.com/ProgrammersGuide/commands.html#zstep) operations after an asynchronous event such as [MUPIP INTRPT](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#intrpt) or [$ZTIMEOUT](https://docs.yottadb.com/ProgrammersGuide/isv.html#ztimeout); previously asynchronous events implicitly removed any pending ZSTEP operations. In addition, a [restriction](https://docs.yottadb.com/AdminOpsGuide/basicops.html#configuring-the-restriction-facility) configured for [ZBREAK](https://docs.yottadb.com/ProgrammersGuide/commands.html#zbreak) also applies to ZSTEP; previously it did not. [GTM-9324](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-001_Release_Notes.html#GTM-9324)

* <a name="GTM-9340"></a> [VIEW](https://docs.yottadb.com/ProgrammersGuide/commands.html#view) and [$VIEW()](https://docs.yottadb.com/ProgrammersGuide/functions.html#view) use the first 31 characters of overly long region names. Previously, such an invalid name could cause the process to fail. This behavior was seen only seen in in-house testing and was never reported by a user. Also, $VIEW() produces a [VIEWREGLIST](https://docs.yottadb.com/MessageRecovery/errors.html#viewreglist) error when it detects an attempt to specify more than one region, as $VIEW() does not accept region lists. Previously, it produced a [NOREGION](https://docs.yottadb.com/MessageRecovery/errors.html#noregion) error for the second region even if that region existed. [GTM-9340](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-000_Release_Notes.html#GTM-9340)

* <a name="GTM-9347"></a> During compilation, YottaDB now reports [NUMOFLOW](https://docs.yottadb.com/MessageRecovery/errors.html#numoflow) errors for constants that exceed its numeric range. Such errors typically arise from use of the exponential ("E") notation. Previously, YottaDB suppressed such errors, which could lead to incorrect results. [GTM-9347](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-000_Release_Notes.html#GTM-9347)

* <a name="GTM-9362"></a> This issue was previously addressed in YottaDB r1.28 as part of the V6.3-013 code merge, and the following release note is included here for completeness.

  _[$TRANSLATE()](https://docs.yottadb.com/ProgrammersGuide/functions.html#translate) appropriately handles UTF-8 mode processing. [GTM-9362](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-000_Release_Notes.html#GTM-9362)_

* <a name="GTM-9370"></a> As YottaDB already incorporated this behavior, the following appears here for completeness.

  _The GT.M compiler defers optimization of expressions containing modulo by zero (#0) or exponentiation of zero using a negative exponent (e.g. 0**-1) to run time, when, if executed, they produce DIVZERO errors. This is consistent with GT.M's treatment of divide by zero (/0) and integer divide by zero (\0). Note that except when it evaluates to match a recent and still cached object, XECUTE induces a mini-compilation with all detected errors deferred to run time in order to prevent double reporting. Starting in V6.3-001 the modulo divide by zero produced a segmentation violation (SIG11) when executed, and the negative exponentiation of zero typically failed in compilation with a GTMASSERT. [GTM-9370](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-000_Release_Notes.html#GTM-9370)_

* <a name="GTM-9371"></a> The following issue in the GT.M V7.0-000 release notes was previously addressed in YottaDB release r1.34 ([YDB#828](https://gitlab.com/YottaDB/DB/YDB/-/issues/828)). The following release note is included here for completeness.

  _GT.M appropriately compiles Boolean expressions of the form literal-operator-<2nd operand> where a plus-sign (+) precedes the literal, the operator is non-relational (AND/OR/NAND/NOR), and the second operand contains a global reference or indirection. The literal may not appear obvious because it may exist as an expression of literal elements, which the GT.M compiler reduces to a single literal; the global and/or indirection may appear anywhere in the right-hand operand or in later in an argument containing the Boolean. Since V6.3-001 such a construct tended to fail with a GTMASSERT, looping or out of memory during compilation; note that XECUTE might make the failure appear while executing at run time. [GTM-9371](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-000_Release_Notes.html#GTM-9371)_

* <a name="GTM-9378"></a> The M compiler appropriately manages the heap during compilation of [pattern match operations](https://docs.yottadb.com/ProgrammersGuide/langfeat.html#pattern-match-operator); previously it could produce a segmentation violation at compile time or an ASSERTPRO at runtime, typically with large patterns. [GTM-9378](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-001_Release_Notes.html#GTM-9378)

* <a name="GTM-9388"></a> [ZSHOW "B"](https://docs.yottadb.com/ProgrammersGuide/commands.html#zshow-information-codes) output consists of an entryref followed by any associated code with a right angle-bracket delimiter after the entryref. Previously the outbut only contained the entryref. [GTM-9388](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-001_Release_Notes.html#GTM-9388)

* <a name="GTM-9408"></a> The following issue in the GT.M V7.0-001 release notes was previously addressed in YottaDB release r1.24 as part of [YDB#333](https://gitlab.com/YottaDB/DB/YDB/-/issues/333). The following release note is included here for completeness.

  _The [HANG](https://docs.yottadb.com/ProgrammersGuide/commands.html#hang) command avoids a race condition where a non-zero duration could occasionally hang indefinitely. The change makes things, including [$HOLOROG](https://docs.yottadb.com/ProgrammersGuide/isv.html#horolog) and [$ZUT](https://docs.yottadb.com/ProgrammersGuide/isv.html#zut), that rely on the system clock more sensitive to changes which adjust that resource. The workaround for this was to wake the affected process with a SIGALRM, and change any HANG that exhibited the symptom to use a timed [READ](https://docs.yottadb.com/ProgrammersGuide/commands.html#read) of some non-responding device in place of the HANG. [GTM-9408](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-001_Release_Notes.html#GTM-9408)_

* <a name="GTM-9409"></a> The following issue in the GT.M V7.0-001 release notes was previously addressed in YottaDB release r1.24 when merging GT.M V6.3-005 code into YottaDB ([YDB!377](https://gitlab.com/YottaDB/DB/YDB/-/merge_requests/377)). The following release note is included here for completeness.

  _YottaDB appropriately reports the underlying cause of a [JOBFAIL](https://docs.yottadb.com/MessageRecovery/errors.html#jobfail) error due to an issue setting up a socketpair to transfer context to the new [JOB](https://docs.yottadb.com/ProgrammersGuide/commands.html#job) process. Previously, it did not supply this information. [GTM-9409](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-001_Release_Notes.html#GTM-9409)_

* <a name="GTM-9429"></a> [$QLENGTH()](https://docs.yottadb.com/ProgrammersGuide/functions.html#qlength) and [$QSUBSCRIPT()](https://docs.yottadb.com/ProgrammersGuide/functions.html#qsubscript) report errors when a literal portion of the namevalue argument contains a leading decimal point (.) or minus-sign (-) not followed by one or more numeric digits, or text matching the appearance of a [$[Z]CHAR()](https://docs.yottadb.com/ProgrammersGuide/functions.html#char) function. Previously these cases were not appropriately detected. [GTM-9429](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-001_Release_Notes.html#GTM-9429)

* <a name="GTM-9432"></a> A [ZTRIGGER](https://docs.yottadb.com/ProgrammersGuide/commands.html#ztrigger) involving a remote GT.CM database errors out with YDB-E-REMOTEDBNOTRIG. Previously, it resulted in a segmentation violation (SIG-11). [GTM-9432](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-001_Release_Notes.html#GTM-9432)

* <a name="GTM-9437"></a> Making current a socket in a [SOCKET](https://docs.yottadb.com/ProgrammersGuide/ioproc.html#using-socket-devices) device with [USE dev:SOCKET=handle](https://docs.yottadb.com/ProgrammersGuide/ioproc.html#socket) is now significantly faster when no other deviceparameters are specified. Previously, the operation was slowed down by preparations needed by other deviceparameters. In addition, [USE ATTACH](https://docs.yottadb.com/ProgrammersGuide/ioproc.html#attach-use) and [USE DETACH](https://docs.yottadb.com/ProgrammersGuide/ioproc.html#detach) issue an error if additional deviceparameters are specified. Previously, they silently ignored the extra deviceparameters. [GTM-9437](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-001_Release_Notes.html#GTM-9437)

* <a name="GTM-9444"></a> When a command has an argument in the form `@indir` where `indir` evaluates to the empty string, YottaDB treats it as equivalent to the argument being an empty string, which typically generates an error; the exceptions being [IF](https://docs.yottadb.com/ProgrammersGuide/commands.html#if), [QUIT](https://docs.yottadb.com/ProgrammersGuide/commands.html#quit), and [TROLLBACK](https://docs.yottadb.com/ProgrammersGuide/commands.html#trollback) where the indirect-derived argmentless form acts the same as an empty string argument, and also cases where the empty string acts as a no-op, namely: [HANG](https://docs.yottadb.com/ProgrammersGuide/commands.html#hang), [WRITE](https://docs.yottadb.com/ProgrammersGuide/commands.html#write), [XECUTE](https://docs.yottadb.com/ProgrammersGuide/commands.html#xecute) and [ZSHOW]. For example: `DO @x` where `""=x` now produces a [LABELEXPECTED](https://docs.yottadb.com/MessageRecovery/errors.html#labelexpected) error. Previously with this form, some potentially argumentless commands ([DO](https://docs.yottadb.com/ProgrammersGuide/commands.html#do), [KILL](https://docs.yottadb.com/ProgrammersGuide/commands.html#kill), [LOCK](https://docs.yottadb.com/ProgrammersGuide/commands.html#lock), [NEW](https://docs.yottadb.com/ProgrammersGuide/commands.html#new), and [ZDEALLOCATE](https://docs.yottadb.com/ProgrammersGuide/commands.html#zdeallocate)) behaved as if argumentless, and certain Z* commands behaved as no-ops. We believe this is more in conformance with the language definition and less likely to violate the principal of least surprise. [GTM-9444](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-001_Release_Notes.html#GTM-9444)

* <a name="GTM-9452"></a> [CLOSE](https://docs.yottadb.com/ProgrammersGuide/commands.html#close) accepts REPLACE=<file-name> as a deviceparameter, to overwrite an existing file. RENAME or REPLACE that specifies the original name simply closes the file. Previously YottaDB only provided RENAME on CLOSE which intentionally protected against accidental replacement, including of itself. [GTM-9452](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-001_Release_Notes.html#GTM-9452)

### System Administration

* <a name="x851"></a> MUPIP commands accept either a space or an equal sign (`=`) after the `-region` qualifier. Previously they were inconsistent (e.g., MUPIP INTEG expected a space, whereas MUPIP CREATE expected an equal). However, this additional consistency of use causes several edge cases to behave differently, e.g.

  * Region names must follow the `-region` qualifier. So, a command line such as `mupip integ -region -online DEFAULT` is no longer accepted.
  * Previously, if no region was specified in the command line, the user would be prompted for a region name, which allowed a region name to be specified in a pipe, e.g., `echo DEFAULT | mupip integ -region`. As region names must now be specified as part of the command, this command should be written as `mupip integ -region DEFAULT`.

  [#851](https://gitlab.com/YottaDB/DB/YDB/-/issues/851)

* <a name="x970"></a> The [YDBSyslog](https://docs.yottadb.com/Plugins/ydbsyslog.html#ydb-syslog) plugin captures syslog data in a YottaDB database, for more sophisticated analytics, forensics, and troubleshooting. [ydbinstall](https://docs.yottadb.com/AdminOpsGuide/installydb.html#ydbinstall-script) can be used to install the YDBSyslog plugin. [#970](https://gitlab.com/YottaDB/DB/YDB/-/issues/970)

* <a name="x1021"></a> [MUPIP SET JOURNAL](https://docs.yottadb.com/AdminOpsGuide/ydbjournal.html#set) switches journal files when the existing journal file was created by an older YottaDB release. Previously, it issued a [FILEEXISTS](https://docs.yottadb.com/MessageRecovery/errors.html#fileexists) error. The workaround was to rename or move the existing journal file before (re)issuing the command. [#1021](https://gitlab.com/YottaDB/DB/YDB/-/issues/1021)

* <a name="x1047"></a> [MUPIP INTEG STATS](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#stats) works correctly when the environment variable [ydb_statshare](https://docs.yottadb.com/AdminOpsGuide/basicops.html#ydb-statshare)is set to 1 and the [ydb_app_ensures_isolation](https://docs.yottadb.com/AdminOpsGuide/basicops.html#ydb-app-ensures-isolation) environment variable is set to a list of global names. Previously, this used to terminate abnormally with a SIG-11. Additionally, applications using the Simple API or Simple Threaded API correctly record [database statistics](https://docs.yottadb.com/ProgrammersGuide/commands.html#no-statshare-region-list). Previously, it was possible for child processes, created by a `fork()` call for example in a Simple API application, to record its database statistics as if it was done by the parent process. [#1047](https://gitlab.com/YottaDB/DB/YDB/-/issues/1047)

* <a name="x1051"></a> [MUPIP BACKUP DBG](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#dbg) performs appropriate cleanup before exiting the BACKUP command in case of errors. Previously, it was possible for it to leave temporary directories/files around in case of errors. The workaround was to not use the DBG qualifier as that did the proper cleanup, and to manually cleanup temporary files. [#1051](https://gitlab.com/YottaDB/DB/YDB/-/issues/1051)

* <a namme="x1062"></a> [MUPIP TRIGGER STDIN](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#id43) reports accurate line numbers. Previously, it was possible for reported line numbers to be incorrectly large, especially if MUPIP had to wait for input to be available. [#1062](https://gitlab.com/YottaDB/DB/YDB/-/issues/1062)

* <a name="GTM-4272"></a> [MUPIP BACKUP](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#backup) and [RESTORE](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#restore) display information in standard message format. The messages display the full path when they include a file context. Please refer to the [Messages](#messages) section of these release notes for details on the standard messages. Previously, certain MUPIP BACKUP and RESTORE messages did not follow the standard message format and/or did not display the full path when there was a file context. [GTM-4272](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-001_Release_Notes.html#GTM-4272)

* <a name="GTM-5148"></a> The Receiver Server reports a [REPLAHEAD](#replahead) warning when it detects the replicating instance is ahead of the originating instance. The message also includes the backlog count, and information on whether the Receiver Server requires a manual rollback. Previously, the Receiver Server recorded the REPL_ROLLBACK_FIRST message in its log file, which lacked clarity on the Receiver Server actions. [GTM-5148](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-001_Release_Notes.html#GTM-5148)

* <a name="GTM-6952"></a> All command line arguments across YottaDB utilities now accept hexadecimal numbers as inputs in addition to decimal numbers. Hexadecimal numbers must be prefixed with a `0x` or `0X` and the digits above nine are case insensitive. Furthermore, all the command line arguments across YottaDB utilities which previously accepted only hexadecimal number inputs continue to accept only hexadecimal number inputs. YottaDB is deprecating the use of HEX values without a leading `0x` or `0X` and may cease to support them in the future. [GTM-6952](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-000_Release_Notes.html#GTM-6952)

* <a name="GTM-8262"></a> The installation script checks dependencies if requested to run YottaDB in UTF-8 mode, prints proper error messages and exits if any of the dependencies are not met. Previously it did not provide explicit error messages if dependencies were not met. [GTM-8262](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-000_Release_Notes.html#GTM-8262)

* <a name="GTM-8263"></a> The `ydbinstall` / `ydbinstall.sh` script picks up the default ICU version of the system. Previously, it accepted ICU version specification with a `−−utf8` option. The manual ydbinstall script no longer prompts for non-default ICU versions. When requesting UTF-8 support, if the dependencies are not met, the installation exits with an appropriate error. Previously, the installer accepted and prompted for non-default ICU versions and continued installation without UTF-8 support if the dependencies had not been met. [GTM-8263](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-000_Release_Notes.html#GTM-8263)

* <a name="GTM-8288"></a> The `ydbinstall` / `ydbinstall.sh` script explicitly removes setgid, setuid and sticky bits from the target installation directory if it exists with those bits set. Previously, the sub-directories created by the installation script inappropriately carried the setgid settings. [GTM-8288](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-000_Release_Notes.html#GTM-8288)

* <a name="GTM-8517"></a> The `ydbinstall` / `ydbinstall.sh` script records ownership, permissions and SHA-256 checksum values of all installed files for future reference in `$ydb_dist/install_permissions.log` and `$ydb_dist/install_sha256_checksum.log`. [GTM-8517](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-000_Release_Notes.html#GTM-8517)

* <a name="GTM-9268"></a> This issue was separately addressed by YottaDB prior to merging GT.M V7.0-000 into the YottaDB code base. Effective r1.30, YottaDB issued the more informative [WCSFLUFAILED](https://docs.yottadb.com/MessageRecovery/errors.html#wcsflufailed) error message to the system log when the journal buffer flushing fails. Previously, it issued the less informative [BUFFLUFAILED](https://docs.yottadb.com/MessageRecovery/errors.html#bufflufailed) message. The following text from the GT.M V7.0-000 release note appears here for completeness. Note that there is an error in the release note, as there is no JNLFLUSHLOG message. The text presumably refers to the GT.M WCSFLUFAIL message.

  _GT.M issues a JNLFLUSHLOG error message to the system log when the journal buffer flushing fails. Previously, GT.M issued only the BUFFLUFAILED error message. [GTM-9268](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-000_Release_Notes.html#GTM-9268)_

* <a name="GTM-8681"></a> When a backup completes with no errors, [MUPIP BACKUP RECORD](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#record) provides a timestamp marking the start of backup. The timestamp, in the number of seconds since January 1, 1970 UTC, provides a backup timestamp which is meaningful to operators without regard to when a particular transaction might have occurred. [MUPIP DUMPFHEAD](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#dumpfhead) and [%PEEKBYNAME()](https://docs.yottadb.com/ProgrammersGuide/utility.html#peekbyname) provide the timestamp as `"sgmnt_data.last_start_backup"`, while [DSE DUMP FILEHEADER](https://docs.yottadb.com/AdminOpsGuide/dse.html#f-ileheader), labels it as `Last Record Backup Start`. If a database has never been backed up with MUPIP BACKUP RECORD, DSE DUMP FILEHEADER labels it as `Never` and `"sgmnt_data.last_start_backup"` provides a value of zero. Previously the RECORD option stored only a starting transaction number. [GTM-8681](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-001_Release_Notes.html#GTM-8681)

* <a name="GTM-9057"></a> [MUPIP JOURNAL EXTRACT](https://docs.yottadb.com/AdminOpsGuide/ydbjournal.html#extract-file-name-stdout) accepts a named pipe (FIFO) as its output device. A process needs to open one end of the FIFO (in read mode) and the device can then be passed as an extract output device. Previously, such extracts could not be written into a FIFO. [GTM-9057](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-001_Release_Notes.html#GTM-9057)

* <a name="GTM-9302"></a> [MUPIP REPLICATE SOURCE SHOWBACKLOG](https://docs.yottadb.com/AdminOpsGuide/dbrepl.html#reporting-the-current-backlog-of-journal-records) reports the sequence number acknowledged from the Receiver Server. Previously, MUPIP REPLICATE SOURCE SHOWBACKLOG reported information from only the Source Server, which did not include an acknowledgement of updates that have reached the Receiver Server. The acknowledged sequence number from the replicating (secondary) instance can also be accessed from the originating (primary) instance using the `"gtmsource_local_struct.heartbeat_jnl_seqno"` field of the [%PEEKBYNAME()](https://docs.yottadb.com/ProgrammersGuide/utility.html#peekbyname) utility function. For an example of the gtmsource_local_struct.heartbeat_jnl_seqno field, refer to [Additional Information for GTM-9302](#addlgtm9302) below. [GTM-9302](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-000_Release_Notes.html#GTM-9302)

* <a name="GTM-9343"></a> The following was fixed in r1.38 as part of [#979](https://gitlab.com/YottaDB/DB/YDB/-/issues/979). However, the following aspect of the fix was not captured in those release notes.

  _[MUPIP FTOK](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#ftok) JNLPOOL or RECVPOOL use the entire replication instance file path. Previously, they assumed the current working directory of the process. [GTM-9343](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-000_Release_Notes.html#GTM-9343)_

* <a name="GTM-9344"></a> [MUPIP FREEZE ONLINE](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#id7) institutes the FREEZE on all regions in parallel to produce a consistent snapshot for a multiple-region database when [ASYNCIO](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#asyncio) is ON. Previously, MUPIP FREEZE ONLINE with ASYNCIO froze each region serially which could result in an inconsistent snapshot with the later regions having sequence numbers higher than the earlier regions. [GTM-9344](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-000_Release_Notes.html#GTM-9344)

* <a name="GTM-9346"></a> During soft tries connection attempts, the Source Server waits for the specified soft tries period when it encounters a network error for the host name specified with the SECONDARY qualifier. Previously, the Source Server skipped waiting which caused the Source Server log to record such attempts in quick succession. [GTM-9346](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-000_Release_Notes.html#GTM-9346)

* <a name="GTM-9349"></a> YottaDB installation script does not attempt to install the deprecated semstat2 and ftok utilities. When using existing directories the installation script creates new soft links in utf8 directories if the destination files exist. Previously, creating such softlinks would result in a "File exists" error. When the installation fails, execute permissions of only the installed files are removed, previously execute permissions of even the directories were inappropriately removed. Except for installing or updating plugins, YottaDB strongly recommends installing YottaDB in a new directory rather than installing into an existing directory. [GTM-9349](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-000_Release_Notes.html#GTM-9349)

* <a name="GTM-9358"></a> [MUPIP REPLICATE SOURCE SHUTDOWN ZEROBACKLOG](https://docs.yottadb.com/AdminOpsGuide/dbrepl.html#zerobacklog) clears all IPC resources associated with the replication instance file. Also, MUPIP REPLICATE SOURCE ZEROBACKLOG SHUTDOWN TIMEOUT displays _Initiating ZEROBACKLOG shutdown operation. Waiting for up to N seconds for backlog to clear at the start of the command._ Previously, MUPIP REPLICATE SOURCE ZEROBACKLOG SHUTDOWN did not clear the IPC resources associated with the replication instance file and did not display the Initiating ZEROBACKLOG shutdown operation message. [GTM-9358](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-000_Release_Notes.html#GTM-9358)

* <a name="GTM-9361"></a> [MUPIP REPLICATE SOURCE SHUTDOWN ZEROBACKLOG](https://docs.yottadb.com/AdminOpsGuide/dbrepl.html#zerobacklog) accounts for the acknowledged sequence number received from the Receiver Server while determining whether the Source Server has a backlog. It returns the [REPL0BACKLOG](https://docs.yottadb.com/MessageRecovery/errors.html#repl0backlog) message when there is no backlog and the Receiver Server has acknowledged all sequence number updates. Alternatively, it returns [REPLBACKLOG](https://docs.yottadb.com/MessageRecovery/errors.html#replbacklog) when there is a backlog, unacknowledged updates, or the timeout expires. Previously it did not include the sequence number acknowledgement from the Receiver Server to confirm that there is no backlog, which could lead to in-flight updates that generate a lost transaction file during a switchover. To reduce the possibility of a generating a lost transaction file during switchover, YottaDB recommends using MUPIP REPLICATE SOURCE SHUTDOWN ZEROBACKLOG and a check for the REPL0BACKLOG message during a planned switchover to help confirm that there is no backlog, and all in-flight updates have reached the Receiver Pool. The maximum TIMEOUT specified with MUPIP REPICATE SOURCE SHUTDOWN (with or without ZEROBACKLOG) is 3600 seconds (1 hour). MUPIP produces the [INVSHUTDOWN](https://docs.yottadb.com/MessageRecovery/errors.html#invshutdown) error if the TIMEOUT is higher than the maximum shutdown timeout. If TIMEOUT is not specified, the default timeout for MUPIP REPLICATE SOURCE SHUTDOWN is 120 seconds. Previously, the maximum timeout for both these cases was 30 seconds and MUPIP automatically adjusted the timeout downwards to 30 seconds when a higher timeout was specified. [GTM-9361](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-000_Release_Notes.html#GTM-9361)

* <a name="GTM-9363"></a> The Source Server performs its polling activities in a more CPU-efficient manner. Also, the Source Server schedules a TLS renegotiation after an appropriate multiple of heartbeat events. This prevents a TLS renegotiation from interfering/overlapping with the normal replication message interchange mechanism and eliminates the need of a separate timer event for running periodic TLS renegotiation. Previously, the Source Server used a separate timer for TLS renegotiation which could cause a race condition. MUPIP produces the [MUPCLIERR](https://docs.yottadb.com/MessageRecovery/errors.html#mupclierr) error when the specified heartbeat interval (the fifth parameter of [CONNECTPARAMS](https://docs.yottadb.com/AdminOpsGuide/dbrepl.html#connectparams-hard-tries-hard-tries-period-soft-tries-period-alert-time-heartbeat-period-max-heartbeat-wait)) is larger than the TLS renegotiate interval. Previously, it did not report this condition as an error. The default value of the heartbeat max period (the sixth parameter) of CONNECTPARAMS is 300 seconds. Previously, the value was 60 seconds. The Receiver Server logs an [REPLCOMM](https://docs.yottadb.com/MessageRecovery/errors.html#replcomm) information message when the connection breaks during a message exchange. Previously, the Receiver Server reported this event in its log file but not as an REPLCOMM informational message. [GTM-9363](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-001_Release_Notes.html#GTM-9363)

* <a name="GTM-9368"></a> A Source Server process responding to a [MUPIP REPLICATE SOURCE SHUTDOWN](https://docs.yottadb.com/AdminOpsGuide/dbrepl.html#shutdown) exits gracefully in response to a CTRL-C. Previously the Source Server did not respond to CTRL-C during the shutdown process. [GTM-9368](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-000_Release_Notes.html#GTM-9368)

* <a name="GTM-9373"></a> [MUPIP REPLICATE SOURCE SHOWBACKLOG](https://docs.yottadb.com/AdminOpsGuide/dbrepl.html#showbacklog) considers a transaction as backlogged until it is acknowledged from the Receiver Server. The [SRCBACKLOGSTATUS](#srcbacklogstatus) message reports whether a Receiver Server is behind, ahead, or has not yet acknowledged the transactions. The [LASTTRANS](#lasttrans) message reports the state (posted, sent, or acknowledged) of the Source Server transactions under replication. Previously, MUPIP REPLICATE SOURCE SHOWBACKLOG did not display the SRCBACKLOGSTATUS and LASTTRANS messages, did not consider in-flight transactions as a backlog and did not report when the replicating instance was ahead during conditions such as online rollback. [GTM-9373](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-001_Release_Notes.html#GTM-9373)

* <a name="GTM-9400"></a> When YottaDB incorporated the following change from GT.M V7.0-001, we discovered that the fix in the GT.M code base was incomplete. YottaDB r2.00 implements a complete fix.

  _[MUPIP REORG](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#reorg) defers acting on [MUPIP STOP](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#stop) while performing bit map adjustments after an action that frees a block. This prevents such events from possibly causing [KILLABANDONED](https://docs.yottadb.com/MessageRecovery/errors.html#killabandoned) and associated errors. Previously REORG did not perform such a deferral. The workaround was to stop the REORG with a CTRL-C. [GTM-9400](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-001_Release_Notes.html#GTM-9400)_

* <a name="GTM-9410"></a> When YottaDB incorporated the following change from GT.M, it was observed that the change slowed down global directory creation in the most common code path. The YottaDB fix avoids the slowness in the most common code path (an existing global directory), and reduces it considerably when there is not an existing global directory.

  _[GDE](https://docs.yottadb.com/AdminOpsGuide/gde.html) attempts to avoid inappropriately creating a global directory by retrying its opening of an existing file a number of times; other components that read a global directory use the same technique to ensure a missing global directory is not a transient condition. These additional attempts take a fraction of a second, but one may perceive the additional time. Writing a revised global directory has a short gap between the removal of the prior file and the replacement by the new/revised file, during which another process might find the global directory missing; previously this was unlikely but has been encountered. [GTM-9410](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-001_Release_Notes.html#GTM-9410)_

* <a name="GTM-9416"></a> A Receiver Server of an SI replication instance started with the [REUSE](https://docs.yottadb.com/AdminOpsGuide/dbrepl.html#reuse-instname) qualifier continues to run normally on a connection reset. Previously on a connection reset, the receiver server exited with [REUSEINSTNAME](https://docs.yottadb.com/MessageRecovery/errors.html#reuseinstname). [GTM-9416](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-001_Release_Notes.html#GTM-9416)

* <a name="GTM-9423"></a> [MUPIP DUMPFHEAD FLUSH REGION](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#dumpfhead) performs a database file header flush for the specified region(s) before displaying the database file header fields. If the database file header flush fails, MUPIP DUMPFHEAD FLUSH produces the [BUFFLUFAILED](https://docs.yottadb.com/MessageRecovery/errors.html#bufflufailed) warning. The qualifier makes the command considerably more heavy weight, and, in most cases, does not provide material benefit, but there may be cases where it addresses a need. Previously, MUPIP DUMPFHEAD provided no option to flush the database file header fields. [GTM-9423](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-001_Release_Notes.html#GTM-9423)

* <a name="GTM-9424"></a> YottaDB includes the following enhancement from GT.M, with an additional optimization for large database files with sparse ranges. The GT.M version of the following code uses an unnecessary amount of disk space and takes a long time for large, sparse database files.

  _[MUPIP BACKUP DATABASE](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#database) uses what seems to be the best copy mechanism available in the kernel to create a backup copy of the database. If the copy mechanism supports monitoring, MUPIP BACKUP SHOWPROGRESS periodically displays the transfer progress, estimated time left, speed, and the number of transaction applied to the database during backup. Pressing CTRL_C performs the appropriate cleanup and exits the BACKUP command. Previously, MUPIP BACKUP used `cp` or `pax` for the copy phase and did not have a progress monitoring mechanism. On kernels where the copy mechanism supports robust error handling, MUPIP BACKUP handles error conditions such as ENOMEM, ENOSPC, EIO, and so on with a more descriptive action message._

  _MUPIP BACKUP displays the [BKUPFILPERM](#bkupfileperm) message when it finds that there is no write permission for the backup file. MUPIP BACKUP performs this check before starting BACKUP. Previously, MUPIP BACKUP reported this condition after performing the copy._

  _The RETRY=n qualifier of MUPIP BACKUP DATABASE makes n number of attempts to retry performing BACKUP if the backup fails. If RETRY is not specified, YottaDB defaults RETRY to zero (0). In case of an error, retry attempts are always based on `cp` or `pax`. Previously, the RETRY qualifier was not available. [GTM-9424](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-001_Release_Notes.html#GTM-9424)_

* <a name="GTM-9441"></a> The following GT.M release note does not apply to YottaDB, for which AIX is an Unsupported platform. It appears here for completeness.

  _The GT.M encryption plugin build looks for libraries in `/opt/freeware`. Previously, the build could generate plugins based on AIX RPM/YUM encryption packages which obtain incompatible library dependencies at run time from elsewhere (e.g. `/usr/local/lib`). [AIX] [GTM-9441](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-001_Release_Notes.html#GTM-9441)_

* <a name="GTM-9442"></a> The following GT.M release note does not apply to YottaDB, which does not use the `gtminstall` / `gtminstall.sh` script. It appears here for completeness.

  _The `gtminstall` script wrapper (`gtminstall.sh`) and the reference encryption plugin scripts no longer rely on the presence of the utility `/usr/bin/which`. [GTM-9442](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-001_Release_Notes.html#GTM-9442)_

* <a name="GTM-9443"></a> When [MUPIP SET JOURNAL](https://docs.yottadb.com/AdminOpsGuide/ydbjournal.html#journal) encounters a temporary journal file that is an artifact of the renaming process which YottaDB uses to create a chain of journal files, it only deletes it after concluding that has been abandoned by seeing that it persists for an interval longer than the renaming process should take. Previously when MUPIP encountered a temporary journal file, it assumed the file was an abandoned artifact and immediately deleted it potentially inappropriately breaking the chain. [GTM-9443](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-001_Release_Notes.html#GTM-9443)

### Other

* <a name="x88"></a> YottaDB can optionally use [GNU Readline](https://tiswww.case.edu/php/chet/readline/) if it is installed on the system. It is enabled by setting the environment variable `ydb_readline` to a nonzero numeric value (typically 1) or a case-independent string which is, or whose leading substring is, `YES` or `TRUE`. See [Additional Information for YDB#88](#addlydb88)

* <a name="x427"></a> The [%GO](https://docs.yottadb.com/ProgrammersGuide/utility.html#go) utility program outputs large global variable nodes without inserting spurious line breaks. Previously it inserted spurious line breaks. [#427](https://gitlab.com/YottaDB/DB/YDB/-/issues/427)

* <a name="x767"></a> `ydbinstall` / `ydbinstall.sh` checks for dependencies needed to run the script, terminating with an error, and returning an error status of 1 if any are missing. If all dependencies to run the script are found, it checks for dependencies required for the installation options specified in the command line, reporting all missing dependencies and terminating with an error if any are not found. Previously, it checked for and reported dependencies individually, terminating with an error after the first missing one was reported, which was less friendly. As before, if all dependencies are found, it proceeds with the installation. [#767](https://gitlab.com/YottaDB/DB/YDB/-/issues/767)

* <a name="x780"></a> YottaDB r2.00 includes fixes and enhancements from [GT.M V7.0-000](#gtmv70000).[#780](https://gitlab.com/YottaDB/DB/YDB/-/issues/780)

* <a name="x835"></a> YottaDB r2.00 includes fixes and enhancements from [GT.M V7.0-001](#gtmv70001).[#785](https://gitlab.com/YottaDB/DB/YDB/-/issues/835)

* <a name="x994"></a> This Issue captures fixes to bugs found while [Fuzz Testing](https://yottadb.com/fuzz-testing-yottadb/) YottaDB and Octo, and fixed in r2.00. They are all edge cases unlikely to be found during normal use and none of them with the potential of causing database damage. [#994](https://gitlab.com/YottaDB/DB/YDB/-/issues/994)

* <a name="x997"></a> Unless the `--verbose` option is given, the `ydbinstall` / `ydbinstall.sh` script is not overly chatty when installing plugins. Previously, while it was not chatty when installing YottaDB, installing plugins emitted many lines of output. The new behavior:

  ```
  $ sudo ./ydbinstall --utf8 --allplugins
  YottaDB version r1.39 installed successfully at /usr/local/lib/yottadb/r139
  YottaDB pkg-config file installed successfully at /usr/share/pkgconfig/yottadb.pc
  Now installing YDBAIM
  Now installing YDBEncrypt
  Now installing YDBGUI
  Now installing YDBOcto
  Now installing YDBPosix
  Now installing YDBSodium
  Now installing YDBSyslog
  Now installing YDBZlib
  $
  ```

  `ydbinstall` is included in the YottaDB installation directory. Since the `--plugins-only` option of `ydbinstall` provides for installation of new or updated plugins into an existing YDB installation directory, including `ydbinstall` in the YDB installation directory avoids the need to download `ydbinstall.sh` afresh to install plugins, which was required previously.

  Included with this update are the following:

	- The `--sodium` command line option, or a value of "Y" for $ydb_sodium in the environment instruct `ydbinstall` / `ydbinstall.sh` to install the [YDBSodium](https://gitlab.com/YottaDB/Util/YDBSodium) plugin.
	- The `--allplugins` command line option, or a value of "Y" for $ydb_all in the environment instruct `ydbinstall` / `ydbinstall.sh` to install all plugins that it knows about.
	- It checks for header files required to install plugins.
	- The script terminates immediately if any dependencies for processing the command line are not met, and after processing the command line if any dependencies for processing the rest of the script are not met. Then it checks for dependencies to run YottaDB as well for all the required installations and terminates after reporting all those missing dependencies.
	- The files intended to be sourced, `ydb_env_set` and `ydb_env_unset`, do not have execute permissions. They did previously, which could result in their being inadvertently executed.
	- The `--help` option reports options on alphabetic order.
	- If `openssl` is not installed on the system, it uses the standard GNU utility `sha256sum` to compute SHA256 sums of installation files.
	- The script has been significantly restructured, with efficiency and maintainability improvements.

  [#997](https://gitlab.com/YottaDB/DB/YDB/-/issues/997)

* <a name="x1011"></a> Please see [997](#x997). [#1011](https://gitlab.com/YottaDB/DB/YDB/-/issues/1011)

* <a name="x1022"></a> The `--ydbsyslog` option of `ydbinstall` / `ydbinstall.sh` installs the [%YDBSYSLOG](https://gitlab.com/YottaDB/Util/YDBSyslog) plugin. Previously, it had to be installed separately. [#1022](https://gitlab.com/YottaDB/DB/YDB/-/issues/1022)

* <a name="x1023"></a> Sourcing `ydb_env_set` works with [zsh](https://www.zsh.org/). Previously it generated an error. Note that zsh is a Supportable shell but not a Supported shell. [#1023](https://gitlab.com/YottaDB/DB/YDB/-/issues/1023)

* <a name="x1025"></a> [%YDBJNLF](https://docs.yottadb.com/ProgrammersGuide/utility.html#ydbjnlf) runs correctly as a non-root user in a Docker container, and other environments where a process does not have access to its stderr under the `/proc` filesystem. [#1025](https://gitlab.com/YottaDB/DB/YDB/-/issues/1025)

* <a name="x1028"></a> The `--webserver` option of `ydbinstall` / `ydbinstall.sh` installs the [YottaDB web server](YottaDB/Util/YDB-Web-Server/) [#1028](https://gitlab.com/YottaDB/DB/YDB/-/issues/1028)

* <a name="x1040"></a> Sourcing `ydb_env_set` sets the environment variable `ydb_readline=1`, enabling the use of [GNU Readline](https://tiswww.case.edu/php/chet/readline/rluserman.html) by default, if it is installed. Previously the environment variable had to be explicitly set. As GNU Readline is more functional than RECALL, especially as it provides command recall across sessions, we believe users will prefer it by default. See [GNU Readline for direct mode, DSE, LKE and MUPIP](#88) for details. [#1040](https://gitlab.com/YottaDB/DB/YDB/-/issues/1040)

* <a name="x1043"></a> On all architectures `ydbinstall` / `ydbnstall.sh` use `ld.gold` to build `libyottadbutil.so`, the shared library of M routines. `ld.gold` is more likely to be found by default on ARM architectures than `cc`, which it previously used. Also, previously, on ARM machines without `cc` it would fail silently; now on machines without `ld.gold` it reports an error. [#1043](https://gitlab.com/YottaDB/DB/YDB/-/issues/1043)

* <a name="GTM-9131"></a> [VIEW "LOGTPRESTART"](https://docs.yottadb.com/ProgrammersGuide/commands.html#no-logt-prestart-intexpr) appropriately identifies restarts associated with extensions of [statistics sharing databases](https://docs.yottadb.com/ProgrammersGuide/utility.html#ygblstat). Previously, it inappropriately identified these as caused by a BITMAP conflict. YottaDB doubles the block count with each statistics database size increase; Previously, it used a fixed extension size of 2050 blocks. YottaDB saves the database block count after each extension and uses it as the initial size for any subsequent recreation of the statistics database during the life of the associated database file; Previously, YottaDB always created a statistics database with 2050 blocks, which is still the initial size for such a database file when the corresponding database is first created. [GTM-9131](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-001_Release_Notes.html#GTM-9131)

* <a name="GTM-9333"></a> YottaDB testing found that the GT.M code did not appropriately implement the behavior described in the release note. We reported this to the upstream GT.M team. The following appears here for completeness.

  _At any point GT.M recognizes only one request for any type of asynchronous processing, including CTRL-C, CTRAP, MUPIP INTRPT, $ZMAXTPTIME, and $ZTIMEOUT. Note that MUPIP INTRPT (SIGUSR1) and untrapped CTRL-C can interrupt other asynchronous events, and an untrapped CTRL-C cancels any other pending asynchronous processing. Previously, GT.M could inappropriately attempt to handle multiple requests for a single type of asynchronous operation, which caused unintended behavior, most likely a stack overflow. The workaround was to avoid rapid interrupting. [GTM-9333](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-001_Release_Notes.html#GTM-9333)_

* <a name="GTM-9357"></a> The previously deprecated %DSEWRAP utility program, is removed from the distribution. Use [MUPIP DUMPFHEAD](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#dumpfhead), [%PEEKBYNAME()](https://docs.yottadb.com/ProgrammersGuide/utility.html#peekbyname), or [DSE DUMP FILEHEADER](https://docs.yottadb.com/AdminOpsGuide/dse.html#f-ileheader) instead. [GTM-9357](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-000_Release_Notes.html#GTM-9357)

* <a name="GTM-9382"></a> As the behavior addressed by this issue would not have occurred in any YottaDB release, the following text from the GT.M V7.0-001 release notes is reproduced here for completeness

  _GT.M appropriately handles symbolic links and relative paths in the $gtm_dist path. In V7.0-000, GT.M issued a GTMSECSHRPERM or SYSCALL error when the first process attempting access to a previously quiescent database file had read-only permissions to the file. Other actions that require gtmsecshr (e.g., when dealing with processes or resources created or owned by a different user, such as waking or checking for the existence, and removing or instantiating a resource, such as semaphores, shared memory or socket files) could result in such error messages as well. The workaround was to avoid symbolic links and relative paths for V7.0-000 [GTM-9382](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-001_Release_Notes.html#GTM-9382)_

* <a name="GTM-9405"></a> [MUPIP JOURNAL](https://docs.yottadb.com/AdminOpsGuide/ydbjournal.html#mupip-journal) reports error conditions related to file reads and file writes both for sequential devices and non-database product-specific files more accurately. Previously, MUPIP always displayed `"ENO1 : operation not permitted"` for such error conditions. [GTM-9405](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-001_Release_Notes.html#GTM-9405)

* <a name="GTM-9422"></a> YottaDB provides counter statistics designated DEXA, GLB, JNL, MLK, PRC, TRX, ZAD, JOPA, AFRA, BREA, MLBA & TRGA, which accumulate counts when a process waits for a critical code section in various cases. These counters are documented in the [ZSHOW](https://docs.yottadb.com/ProgrammersGuide/commands.html#zshow) section of the [M Programmers Guide](https://docs.yottadb.com/ProgrammersGuide/). Previously, YottaDB provided toggle statistics to indicate when a process was waiting for a critical code section. [GTM-9422](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-001_Release_Notes.html#GTM-9422)

## Additional Information

<a name="addlydb88"></a>
### Additional Information for YDB#88 - GNU Readline

YottaDB can optionally use [GNU Readline](https://tiswww.case.edu/php/chet/readline/) if it is installed on the system. It is enabled by setting the environment variable `ydb_readline` to a nonzero numeric value (typically 1) or a case-independent string which is, or whose leading substring is, `YES` or `TRUE`.

When enabled, interactive use of YottaDB [Direct Mode](https://docs.yottadb.com/ProgrammersGuide/opdebug.html), [LKE](https://docs.yottadb.com/AdminOpsGuide/mlocks.html), [DSE](https://docs.yottadb.com/AdminOpsGuide/dse.html), and [MUPIP](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html) use the Readline library to read input, enabling many features of Unix shells (including saving & recalling history). Implemented features include:

  * Line editing for direct mode, DSE, LKE, and MUPIP.
  * Command history is saved in `$HOME/.ydb_{YottaDB,DSE,LKE,MUPIP}_history`. When Readline is enabled:

	* Direct mode [RECALL](https://docs.yottadb.com/ProgrammersGuide/opdebug.html#command-recall) displays Readline history when entered at the beginning of the line.
	* Recalling and editing prior commands with `^` or `!` work when they are entered at the beginning of the line. Ending the line with `:p` prints the recalled and edited command, instead of executing it.
	* Recalling and editing commands also executes them, unlike the RECALL command implemented by YottaDB.
	* Entering UTF-8 mode characters works even in M mode.
	* Settings are read from `$HOME/.inputrc`, whose location can be overridden by the `INPUTRC` environment variable. The application name for use in `$if` statements in the settings file is `YottaDB`.
	* If `history-size` is not set, it defaults to 1,000. The history file on disk is always limited to 1,000 entries, no matter the setting of history-size.
	* Signals are handled by YottaDB and not by Readline.

Examples of history expansion:

  * `!!`: Recall last command
  * `!$`: Last argument of last command
  * `!nnn`: Execute line in history number nnn
  * `!nnn:p`: Print line (but don't execute) nnn, and add it to the history to the end. You can press up arrow to recall that command for editing.
  * `!?xxxx`: Execute line containing text xxxx. BE CAREFUL WITH THIS ONE. It can lead to unexpected items getting executed.
  * `^string1^string2^`: In the last command, replace string1 with string2, and execute it.
  * `!nnn:s/old/new/`: In history item nnn, replace old with new and execute it.

Refer to the [GNU Readline user documentation](https://tiswww.case.edu/php/chet/readline/rluserman.html) for more information on the functionality of Readline.

Limitations include:

  * DSE/LKE/MUPIP

	* There is no history listing (equivalent to the direct mode RECALL command).
	* History expansion module works only in direct mode.

  * Direct mode

	* Only traditional characters terminate input lines (CR, LF, FF, and their UTF-8 variants); alternate terminators are not supported. (YottaDB direct mode has the ability to terminate input using the [TERMINATOR](https://docs.yottadb.com/ProgrammersGuide/ioproc.html#terminator) deviceparameter.)
	* Wrap on device width (set using [WIDTH](https://docs.yottadb.com/ProgrammersGuide/ioproc.html#width) deviceparameter) is not supported.
	* [MUPIP INTRPT](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#intrpt) (SIGUSR1) turns off line editing on the line being entered. You can still enter more characters or cancel the line using CTRL-C.

  * Readline is not supported for the [READ](https://docs.yottadb.com/ProgrammersGuide/commands.html#read) command.

[#88](https://gitlab.com/YottaDB/DB/YDB/-/issues/88)

<a name="addlgtm8843"></a>
### Additional Information for GTM-8843 - Non-blocking WRITE to SOCKET devices

[SOCKET](https://docs.yottadb.com/ProgrammersGuide/ioproc.html#using-socket-devices) devices default to blocking [WRITE](https://docs.yottadb.com/ProgrammersGuide/ioproc.html#write)s. ``WRITE /BLOCK("OFF")`` enables non blocking WRITEs for the current socket of the current SOCKET device. A socket must be enabled for non blocking WRITEs before enabling it for TLS if both features are desired. For non blocking sockets, YottaDB retries a WRITE that blocks up to the number of times specified by the value of the environment variable `ydb_non-blocked_write_retries`, and if that is not set, `gtm_non_blocked_write_retries`, with a 100 millisecond delay between each retry, defaulting to 10 attempts.

If WRITE remains blocked after the specified retries, the WRITE sets [$DEVICE](https://docs.yottadb.com/ProgrammersGuide/ioproc.html#device) to `"1,Resource temporarily unavailable"` and issues an error if deviceparameter [IOERROR](https://docs.yottadb.com/ProgrammersGuide/ioproc.html#deviceparameter-summary-table) is `"TRAP"`. If IOERROR is not `"TRAP"`, $DEVICE must be checked after each WRITE. An attempt to WRITE to a socket after it has been blocked is an error which sets $DEVICE to `"1,Non blocking WRITE blocked - no further WRITEs allowed"`. Thus the only operation permitted on a blocked socket is to [CLOSE](https://docs.yottadb.com/ProgrammersGuide/ioproc.html#close) it.

``WRITE /W[AIT][(timeout[,[what][,handle]])]`` where:

- ``timeout`` is a numeric expression that specifies how long in seconds the WRITE command waits for a condition to be met in the current SOCKET device.

- The optional ``what`` specifies the condition that the command waits for.

  - if ``what`` is omitted, the command waits for an incoming connection at a listening socket.

  - if ``what`` is just ``"WRITE"``, the command waits till a socket is not blocked for a WRITE.

  - if ``what`` is just ``"READ"``, the command waits till data is available to read.

  - if what includes both ``"READ"`` and ``"WRITE"`` substrings, the command waits till either data becomes available to read, or a socket is not blocked for a WRITE.

- If ``handle`` is specified, the command waits for the condition to occur at a specific socket in the current SOCKET device. Otherwise, it waits till the condition is satisfied for any socket in tne current SOCKET device.

A WRITE to a non-blocking socket, which is not enabled for TLS, may terminate early on the following events: <CTRL-C>; exceeding [$ZMAXTPTIME](https://docs.yottadb.com/YDBDoc/ProgGuide/_build/html/isv.html#zmaxtptime), or [$ZTIMEOUT](https://docs.yottadb.com/YDBDoc/ProgGuide/_build/html/isv.html#ztimeout) expiring. These events result in a transfer to the interrupt vector or error handler at the next execution boundary as described in [Interrupt Handling](https://docs.yottadb.com/YDBDoc/ProgGuide/_build/html/langfeat.html#interrupt-handling).

When non-blocking WRITEs are enabled for a socket, WRITE /WAIT can check if that socket would not block on WRITE and/or a READ. The optional second argument can be a string containing ``"READ"`` and/or ``"WRITE"``.

- If the second argument is omitted or specifies both ``"READ"`` and ``"WRITE"`` (e.g., ``"READWRITE"`` or ``"WRITEREAD"``) and the socket selected by WRITE /WAIT is ready for both READ and WRITE, $KEY contains ``READWRITE|<socket handle>|<address>``.

  - If the socket selected by a WRITE /WAIT implicitly or explicitly requests the state for writing would block on a READ but not block on WRITE, $KEY contains ``WRITE|<socket handle>|<address>``. Note that a WRITE may still be unable to complete if it tries to write more bytes than the system is ready to accept.

  - If the socket selected by WRITE /WAIT which implicitly or explicitly requests the state for reading would not block on a READ but would block on a WRITE, $KEY contains ``READ|<socket handle>|<address>``.

- If the second argument is omitted or contains ``"WRITE"`` but not ``"READ"``, WRITE /WAIT checks for readiness for WRITE on non-blocking sockets, but never checks readiness to WRITE on blocking sockets, even if explicitly requested.

[$ZKEY](https://docs.yottadb.com/YDBDoc/ProgGuide/_build/html/isv.html#zkey) after a prior WRITE /WAIT will contain a piece of the form ``"WRITE|sockethandle|ipaddress"`` if a non-blocking socket was considered writable, which we expect to be typical. If a socket was also readable, there will be two pieces in $ZKEY for the socket, one for WRITE and the other for READ.

An application can determine whether a socket is enabled for non-blocking WRITEs with ``$ZSOCKET(device,"BLOCKING",index)`` which returns either 1 (TRUE) for blocking, or 0 (FALSE) for non-blocking.

**Notes**

- In most circumstances, WRITE /WAIT(timeout[,"WRITE"]) for non-blocking sockets returns immediately, because non-blocking sockets are usually ready for writing. See WRITE /BLOCK("OFF") below.

- If the current Socket Device is $PRINCIPAL and input and output are different SOCKETs, WRITE /WAIT applies to the input side of the device.

- Multi-argument WRITEs are equivalent to a series of single-argument WRITEs. YottaDB turns unparenthesized concatenation within a WRITE argument into multiple arguments. Format control characters such as "!" and "#" are each considered as an argument.

- A significant delay between bytes for any reason, including blocking, especially within a multibyte character when CHSET is UTF-8, may be considered an error by the receiving end of a connection. If the application is unable to handle such a delay, it may result in an application error.A WRITE to a non blocking socket, which is not enabled for TLS, may terminate early on the following events:

<a name="addlgtm9302"></a>
### Additional Information for GTM-9302 - gtmsource_local_struct.heartbeat_jnl_seqno

Here are some examples of the `"gtmsource_local_struct.heartbeat_jnl_seqno"` %PEEKBYNAME() field. This routine returns the replication speed, that is the number of seqno updates per second acknowledged by the replicating instance during heartbeat intervals.

  ```
  replspeed
	; This routine returns the replication speed, that is the number of seqno updates per second acknowledged by the replicating instance during the heartbeat intervals.
	; Usage: $gtm_exe/mumps -r ^replspeed INSTANCE3 20
	; The second parameter is the sampling size
	set $etrap="write ""REPLSPEED-E-ACKSMPL : unable to fetch sampling data due to "",$zstatus  halt  "
	set $ztimeout="300:write ""Timeout occurred out after 5 minutes"",! zwrite  halt"
	new hrtbtperiod,instance,samplingsize,hrtbts,slot,i,hrtbtdiffs,diff,dump
	set instance=$piece($zcmdline," ",1),slot=0
	set samplingsize=$piece($zcmdline," ",2),hrtbtdiffs=0
	set:$length(samplingsize)=0 samplingsize=10
	if '($length(instance)) write "REPLSPEED-E-ARGS : ",$zcmdline," was specified. This routine requires specifying an instance name.",! halt
	for i=0:1:15 set instname=$$^%PEEKBYNAME("gtmsource_local_struct.secondary_instname",i),instname=$piece(instname,$char(0),1) set:(instname=instance) slot=i
	; capture heartbeat_jnl_seqno samplingsize times. Wait for hrtbtperiod after every capture of heartbeat_jnl_seqno
	set hrtbtperiod=$piece($$^%PEEKBYNAME("gtmsource_local_struct.connect_parms",slot),",",5)
	for j=1:1:samplingsize do
	. set hrtbts(j)=$$^%PEEKBYNAME("gtmsource_local_struct.heartbeat_jnl_seqno",slot,"I")
	. do:(j>1)
	. . set diff=hrtbts(j)-hrtbts(j-1)
	. . do:(diff<0)
	. . . write "REPLSPEED-E-ACKSEQNO : acknowledgement sequence number received is lower than previous acknowledgement seqno."
	. . . set dump="replspeed.dump" open dump use dump zwrite hrtbts  close dump
	. . . halt
	. . set hrtbtdiffs=hrtbtdiffs+diff
	. hang hrtbtperiod
	set hrtbtdiffs=hrtbtdiffs/samplingsize
	write $justify(hrtbtdiffs/hrtbtperiod,0,0)
	quit
  ```

Use the following example to confirm that an update corresponding to a Source Server sequence number has reached the Receiver Pool. This can be used as a tool to help confirm that there are no in-flight updates.

  ```
  waitforhrtbt
	; This routine returns 0 when the acknowledged seqno from the specified instance matches or exceeds the specified seqno
	; If there is no confirmation (network issues etc) from the Receiver Server for 300 seconds, this routines returns 1.
	; Usage: $gtm_exe/mumps -r ^waitforhrtbt <instname> <seqnocheckpoint>
	set $etrap="write ""WAITFORHRTBT-E-ERROR, Error occurred while waiting for ackseqno confirmation due to "",$zstatus  halt  "
	new heartbeatseqno,hangduration,instance,checkseqno,i,instname,slot
	; set $ztimeout to align with the REPLALERT threshold for the test system
	set $ztimeout="900:write 1 halt"
	set hangduration=1,slot=""
	set instance=$piece($zcmdline," ",1)
	set checkseqno=$piece($zcmdline," ",2)
	if '($length(instance)&$length(checkseqno)) write "WAITFORHRTBT-E-ARGS : ",$zcmdline," was specified. This routine requires specifying instance name and seqno.",! halt
	for i=0:1:15 set instname=$$^%PEEKBYNAME("gtmsource_local_struct.secondary_instname",i),instname=$piece(instname,$char(0),1) set:(instname=instance) slot=i
	if '($length(slot)) write "WAITFORHRTBT-E-INSTNOMATCH : No matching instance name for ",instance,! halt
	for  do
	. set heartbeatseqno=$$^%PEEKBYNAME("gtmsource_local_struct.heartbeat_jnl_seqno",slot,"I")-1
	. if (heartbeatseqno>=checkseqno) write 0 halt
	. hang hangduration
	quit
  ```

and

  ```
  waitforseqnosync
	; This routine returns 0 when there the sequence numbers of the Source Server and the acknowledged sequence number from the Receiver Server is the same.
	; It returns a non-zero value when there is no confirmation of the receipt of the latest seqno from the secondary even when there is no backlog.
	; If there is no confirmation (network issues etc) from the Receiver Server for 150 seconds, this routines returns 1.
	; Usage: $gtm_exe/mumps -r ^waitforseqnosync <instname>
	set $etrap="write ""WAITFORSEQNOSYNC-E-SRCBACKLOG : unable to get current Source Server backlog and seqno updates status due to "",$zstatus  halt  "
	new readseqno,heartbeatseqno,instance,i,instname,slot,hrtbtperiod
	set $ztimeout="150:write 1 halt"
	set slot=""
	; hrtbtperiod: the heartbeat period (the fifth parameter of -CONNECTPARAMS)
	set instance=$zcmdline
	if '$length(instance) write "WAITFORSEQNOSYNC-E-ARGS : This routine requires specifying an instance name.",! halt
	for i=0:1:15 set instname=$$^%PEEKBYNAME("gtmsource_local_struct.secondary_instname",i),instname=$piece(instname,$char(0),1) set:(instname=instance) slot=i
	if '($length(slot)) write "WAITFORSEQNOSYNC-E-INSTNOMATCH : No matching instance name for ",instance halt
	set hrtbtperiod=$piece($$^%PEEKBYNAME("gtmsource_local_struct.connect_parms",slot),",",5)
	for  do
	. set seqno=$$^%PEEKBYNAME("jnlpool_ctl_struct.jnl_seqno","","I")
	. set readseqno=$$^%PEEKBYNAME("gtmsource_local_struct.read_jnl_seqno",i,"I")
	. set heartbeatseqno=$$^%PEEKBYNAME("gtmsource_local_struct.heartbeat_jnl_seqno",i,"I")
	. if (seqno=readseqno=heartbeatseqno) write 0 halt
	. hang hrtbtperiod
	quit
  ```

## Messages

### New messages

The following new messages have been added:

**BACKUPDBFILE**, DB file dddd backed up in file bbbb

MUPIP Information: This message indicates MUPIP BACKUP successfully backed up database file dddd to file bbbb.

Action: None required.

**BACKUPFAIL**, MUPIP cannot start backup with the above errors

MUPIP Error: This message indicates MUPIP BACKUP was unable to complete the backup.

Action: Review accompanying messages for action guidance.

**BACKUPREPL**, Replication Instance file iiii backed up in file rrrr

MUPIP Information: This message indicates that MUPIP BACKUP was successful in backing up replication instance file iiii to file rrrr.

Action: None required.

**BACKUPSEQNO**, Journal Seqnos up to 0xhhhh are backed up

MUPIP Information: This message indicates MUPIP BACKUP REPLINSTANCE backed up journal sequence numbers up to 0xhhhh to the specified replication instance file.

Action: None required.

**BACKUPSUCCESS**, Backup completed successfully

MUPIP Information: This message indicates the backup actions specified with MUPIP BACKUP command were successful. MUPIP does not display this message until all actions are complete.

Action: None required.

**BACKUPTN**, Transactions from 0xbbbb to 0xeeee are backed up

MUPIP Information: This information message indicates MUPIP BACKUP backed up transactions from 0xbbbb to 0xeeee.

Action: None required.

<a name="bkupfileperm"></a>
**BKUPFILEPERM**, Backup file dddd does not have write permission

MUPIP Information: MUPIP BACKUP encountered an authorization issue with the target location while preparing to perform the BACKUP.

Action: Ensure the target location has appropriate authorization and the appropriate user is properly configured.

**BKUPPROGRESS**, Transfer : cccc / tttt (pppp%) ; Speed : zzzz MiB/sec ; Transactions : nnnn ; Estimated left : tt minutes

MUPIP Information: MUPIP BACKUP -SHOWPROGRESS displays this message when the kernel supports copy progress monitoring. cccc is the size (MiB or GiB) of the copied database file and tttt is the total size of the database file. pppp is the progress percentage. Speed is always in MiB/sec and can vary based on the resources available for copy. Transactions includes the number of transaction (increments of current tn), applied to the region during MUPIP BACKUP. If the kernel does not support progress monitoring, MUPIP BACKUP -SHOWPROGRESS does not report this message. This message is expected to appear after about 25% completion of copy. Note that YottaDB instructs the kernel to copy as much data as possible in one go. If the kernel has available resources and the database file size is relatively small, you may only see one BKUPPROGRESS message followed by the BACKUP COMPLETED message.

Action: None required.

**BKUPRETRY**, Retrying MUPIP BACKUP for region: rrrr (database file: dddd). Attempt: #nnnn of mmmm

MUPIP Information: This message appears when MUPIP BACKUP initiates a retry attempt because a prior backup attempt failed. #nnnn is the current retry attempt count and mmmm is the maximum number of retry attempts.

Action: None required.

**CMDERR**, Error running command : cccc

MUPIP Error: This message indicates MUPIP BACKUP received an error trying to execute the shell command cccc.

Action: Look at the error message and preceding messages. Check for errors in paths, authorizations and/or other possibilities related to the specified MUPIP BACKUP actions.

**DIRACCESS**, Do not have full access to directory for temporary files: pppp

MUPIP Error: The message indicates that MUPIP BACKUP does not have appropriate access to the temporary directory pppp.

Action: Check the path and the directory permissions for the temporary directory. You can also set the gtm_baktmpdir environment variable to specify the location of the temporary directory.

**GTMCURUNSUPP**, The requested operation is unsupported in this version of YottaDB

All YottaDB Components Error: YottaDB tried to perform an operation that is unsupported in the current version. Currently this is only thrown by YottaDB when trying to perform an upgrade/downgrade operation.

Action: YottaDB does not support upgrade/downgrade between r1.x and r2.x databases.

**HEX64ERR**, Error: cannot convert VVVV value to 64 bit hexadecimal number

All YottaDB Components Error: The entered value does not correspond to a valid hexadecimal representation of a 64-bit number.

Action: Enter an appropriate hexadecimal value starting with 0X.

**HEXERR**, Error: cannot convert VVVV value to hexadecimal number

All YottaDB Components Error: The entered value does not correspond to a valid hexadecimal number.

Action: Enter an appropriate hexadecimal value starting with 0X.

**INVSHUTDOWN**, Shutdown timeout should be from 0 to 3600 seconds

MUPIP Error: This error appears when the TIMEOUT specified with SOURCE SHUTDOWN exceeds 3600 seconds (1 hour).

Action: Specify TIMEOUT between 0 to 3600 seconds

<a name="lasttrans"></a>
**LASTTRANS**, Last transaction sequence number SSSS : NNNN

MUPIP Information: This message appears with the output of MUPIP REPLICATE -SOURCE -SHOWBACKLOG. SSSS denotes the three states the latest transaction sequence number - posted, sent, and acknowledged. NNNN denotes the associated count for each state. A transaction is first "posted" on the Journal Pool, "sent" to the Receiver Server, and finally "acknowledged" once the Source Server receives confirmation that it has reached the Receiver Server.

**NORTN**, Routine name missing

Run Time Error: This indicates the specification used to locate a routine for compilation and / or zlinking was missing the name.

Action: Correct the routine specification.

**NUM64ERR**, Error: cannot convert VVVV value to 64 bit decimal or hexadecimal number

All YottaDB Components Error: The entered value does not correspond to a valid decimal or hexadecimal representation of a 64-bit number.

Action: Enter an appropriate decimal value or hexadecimal value starting with 0X.

**NUMERR**, Error: cannot convert VVVV value to 64 bit decimal or hexadecimal number

All YottaDB Components Error: The entered value does not correspond to a valid decimal number or hexadecimal number.

Action: Enter an appropriate decimal value or hexadecimal value starting with 0X.

**READLINEFILEPERM**, Readline history file ffff could not be created

Run Time Warning: When using Readline for managing command line history, the process does not have the needed permissions to create history file ffff. The process continues without a history file.

Action: Check the path and directory permissions needed for the process to create file ffff.

**READLINELONGLINE**, Entered line is greater than 32Kb long and will be truncated

Run Time Warning: Readline provided YottaDB with a command line longer than 32KB. YottaDB ignores the entire command line.

Action: Provide YottaDB with a shorter command line.

**REPL0BACKLOG**, Total backlog for the specified replicating instance(s) is 0

MUPIP Success: This message indicates a successful ZEROBACKLOG SHUTDOWN. It means that there was no backlog for the specified replicating instance(s), no inflight updates, and all updates were successfully acknowledged by the Receiver Server.

Action: None.

<a name="replahead"></a>
**REPLAHEAD**, Replicating instance is ahead of the originating instance. aaaa

MUPIP Error: The message appears on the Source and Receiver Server log files when the Receiver Server is ahead due to a possible rollback on the Source Server side. aaaa contains additional information or action that the user may have to perform.

Action: Action: Acknowledge or perform the appropriate action suggested with aaaa.

**REPLBACKLOG**, Timeout occurred while there was a backlog

MUPIP Error: This error occurs when the TIMEOUT specified with SOURCE ZEROBACKLOG SHUTDOWN expires and there is a either a backlog and/or there was a failure to receive an acknowledgement of the latest sequence number on the Source Server by the Receiver Server. If REPLNORESP also accompanies this error, it means that the Source Server did not receive a response from the Receiver Server acknowledging sequence number confirmation.

Action: This error means that the ZEROBACKLOG checks did not pass. Restart the Source Server to clear any backlog. The presence of a REPL0BACKLOG success message for ZEROBACKLOG SHUTDOWN confirms that there are no inflight updates and all updates are acknowledged by the Receiver Server.

**REPLNORESP**, No sequence number confirmation from the replicating instance xxxx after waiting for nnnn second(s)

MUPIP Warning: This message appears when the Source Server fails to receive a response from the Receiver Server during a ZEROBACKLOG SHUTDOWN. The presence of a REPLNORESP indicates that a ZEROBACKLOG SHUTDOWN check failed. This warning is accompanied by the REPLBACKLOG error message.

Action: This warning means that the ZEROBACKLOG checks did not pass. Restart the Source Server to clear any backlog. The presence of a REPL0BACKLOG success message for ZEROBACKLOG SHUTDOWN confirms that there are no inflight updates and all updates are acknowledged by the Receiver Server.

**RESTORESUCCESS**, Restore completed successfully

MUPIP Information: This message indicates MUPIP RESTORE successfully completed all specified with command actions.

Action: None required.

**SHUT2QUICK**, Shutdown timeout ssss shorter than the heartbeat period SSSS; cannot confirm the backlog at the replicating instance iiii

MUPIP Warning: This warning appears when the TIMEOUT=ssss specified with ZEROBACKLOG is less than the heartbeat period SSSS (the fifth parameter of CONNECTPARAMS). If TIMEOUT is less than the heartbeat period, ZEROBACKLOG cannot confirm that there is zero backlog as it cannot obtain the acknowledgement of the latest sequence number from the Receiver Server of instance iiii in such a short time.

Action: Specify TIMEOUT that is larger or equal to the heartbeat period.

**SOCKBLOCKERR**, WRITE /BLOCK error: dddd

Run Time Error: This indicates a format or usage error in a WRITE /BLOCK command. Specific details are provided by dddd.

Action: Correct the format or usage of the WRITE /BLOCK command.

**SOCKWAITARG**, nnnn argument to WRITE /WAIT xxxx

Run Time Error: This indicates an error with argument number nnnn of a WRITE /WAIT as described by xxxx.

Action: Correct the specified argument.

<a name="srcbacklogstatus"></a>
**SRCBACKLOGSTATUS**, Instance RRRR SSSS NNNN transaction(s)

MUPIP Information: This message appears with the output of MUPIP REPLICATE -SOURCE -SHOWBACKLOG. RRRR specifies the name of the replicating instance. SSSS denotes three possible stages of the replicating instance in relation to the originating instance - "is behind by", "has not acknowledged" and "is ahead by". A replicating instance is behind by the originating instance when there is a backlog of unacknowledged transactions. A replicating instance is ahead by the originating instance when the Receiver Server is performing an online rollback. An instance has not yet acknowledged transaction when the originating instance has not received a response from the replicating instance. NNNN is the number of transactions. There are no in-flight updates when SRCBACKLOGSTAUS reports that the replicating instance is behind by 0 transactions and the LASTTRANS messages for "posted", "sent", and "acknowledged" have the same number of transaction count.

Action: Use this message as an operational aid to determine the status of the replicating instance in relation to the originating instance.

**UNUM64ERR**, Error: cannot convert VVVV value to 64 bit unsigned decimal or hexadecimal number

All YottaDB Components Error: The entered value does not correspond to a valid unsigned decimal or hexadecimal representation of a 64-bit number.

Action: Enter an appropriate decimal value or hexadecimal value starting with 0X.

**VIEWREGLIST**, $VIEW() only handles the first region subparameter

Run Time Warning: $VIEW() with a region subparameter only operates on a single region. This differs from the VIEW command which has similar arguments and accepts region-lists for regions. This error is a warning and the function attempts to act on the first region.

Action: If the requirement is for multiple regions, use multiple $VIEW() invocations, perhaps in a loop.

**WCSFLUFAIL**, Error flushing buffers -- called from module MMMM at line LLLL

All YottaDB Components Error: This indicates that an attempt to flush a buffer to disk failed.

Action: For a BG database file, this means that a process attempting to flush modified global buffers to disk encountered an error. EEEE is the error it encountered, for database file DDDD when attempting to flush the blocks for database transaction number TTTT. This is usually accompanied by other messages that can together help provide more information and context. If you need further assistance and have purchased support, contact your YottaDB support channel.

Action: Refer to the description of error EEEE and take appropriate action.

### Revised messages

The following messages have been modified for correctness and/or clarity:

**BOVTMGTEOVTM**, Journal file xxxx has beginning timestamp aaaa greater than end timestamp bbbb

MUPIP Error: This indicates that the beginning time stamp aaaa of the journal file xxxx is greater than the ending timestamp bbbb. This could be due to something that changed the system time,such as a daylight savings time change or a testing time reset, while YottaDB was journaling. YottaDB recommends against changing system time during YottaDB Run-time as a matter of course, as this disruption is not heavily tested.

Action: Changing system time during YottaDB run-time is not allowed. Contact your YottaDB support channel for further assistance.

**CRYPTNOV4**, ffff is an encrypted database. Cannot downgrade(to V4) with Encryption option enabled

MUPIP Error: An attempt to downgrade ffff which is an encrypted database to the V4 (GT.M version 4) format failed because the V4 format does not support encrypted database files.

Action: Use the database in the current format. If a V4 format is required, extract the data in unencrypted ZWRite format with MUPIP EXTRACT and load it into a newly created V4 database.

**DBADDRANGE**, Database file rrrr element location aaaa: control vvvv was outside qqqq range bbbb to tttt

Run Time Information: This indicates that a process was abnormally terminated while updating the database. Database control structures may be damaged.

Action: This typically indicates a process terminated abnormally while updating the database. YottaDB often fixes such an error unless there is a serious problem causing this error. If YottaDB cannot correct the issue, the accompanying messages should expand on the situation. You are advised to report such a database error to the group responsible for database integrity at your operation.

**DBADDRANGE8**, Database file rrrr element location aaaa: control vvvv was outside qqqq range bbbb to tttt

Run Time Error: This indicates a database control structure for database region rrrr at memory location aaaa contains a value vvvv outside range bbbb to tttt for quantity qqqq.This message is the same as a DBADDRANGE message except that vvvv, bbbb and tttt are 8-byte quantities (as opposed to 4-byte quantitites in DBADDRANGE).

Action: This typically indicates a process terminated abnormally while updating the database. YottaDB often fixes such an error unless there is a serious problem causing this error. If YottaDB cannot correct the issue, the accompanying messages should expand on the situation; and you should report such database error to the group responsible for database integrity at your operation.

**DBMISALIGN**, Database file xxxx has yyyy blocks which does not match alignment rules. Reconstruct the database from a backup or extend it by at least zzzz blocks.

MUPIP Error: This error appears when YottaDB detects a mismatch between the total block count in the file header and the expected block count based on the database file size reported by the file system. This error may appear when you perform a MUPIP INTEG FILE after a YottaDB upgrade on a database file which has not yet been opened by a process using a normal database access which performs an automatic database file header upgrade.

Action: If there are prior messages, address them first. Extend the database by at least one block, perform at least one $GET() operation or run MUPIP INTEG -REGION. If the error persists, reconstruct the database from a backup.

**DLLCHSETM**, Routine XXX in library YYY was compiled with CHSET=M which is different from $ZCHSET.

Run Time Error: This error is triggered when a UTF-8 mode process attempts to execute a shared library's routine that was compiled in M-mode.

Action: Change ydb_routines to include the routine compiled in UTF-8 mode. For example, in UTF-8 mode, `$ydb_routines <../AdminOpsGuide/basicops.html#ydb-routines>`_ would typically include the shared library :code:`$ydb_dist/utf8/libyottadbutil.so`. If instead it includes the shared library :code:`$ydb_dist/libyottadbutil.so`, the process would get a DLLCHSETM error when attempt to execute code from the latter.

**DLLCHSETUTF8**, Routine XXX in library YYY was compiled with CHSET=UTF-8 which is different from $ZCHSET.

Run Time Error: This error is triggered when an M mode process attempts to execute a shared library's routine that was compiled in UTF-8 mode.

Action: Change ydb_routines to include the routine compiled in M mode. For example, in M mode, `$ydb_routines <../AdminOpsGuide/basicops.html#ydb-routines>`_ would typically include the shared library :code:`$ydb_dist/libyottadbutil.so`. If instead it includes the shared library :code:`$ydb_dist/utf8/libyottadbutil.so`, the process would get a DLLCHSETUTF8 error when attempt to execute code from the latter.

**REPLCOMM**, Replication subsystem communication failure

MUPIP Error: This is a generic error or message indicating that there has been a communication error between the two systems performing replication.

Action: Review the accompanying message(s) for more information about the cause of this error. When REPLCOMM has an error severity, it accompanys a shut down of the replicating server. When REPLCOMM has an information severity, it indicates a temporary pause in replication due to a situation described by accompanying messages.

### Obsolete Messages

The following messages are no longer used:

- DBCBADFILE
- DBCCMDFAIL
- DBCDBCERTIFIED
- DBCDBNOCERTIFY
- DBCINTEGERR
- DBCKILLIP
- DBCMODBLK2BIG
- DBCNOEXTND
 -DBCNOFINISH
- DBCNOTSAMEDB
- DBCREC2BIG
- DBCREC2BIGINBLK
- DBCSCNNOTCMPLT
- DBMXRSEXCMIN
- DEFEREVENT
- SECONDAHEAD
- SRCLOCUNKNOWN
- VARRECBLKSZ
- ZCMLTSTATUS

## Legal Stuff

Copyright © 2024 YottaDB LLC

Permission is granted to copy, distribute and/or modify this document under the terms of the GNU Free Documentation License, Version 1.3 or any later version published by the Free Software Foundation; with no Invariant Sections, no Front-Cover Texts and no Back-Cover Texts.

YottaDB® and Octo® are registered trademarks of YottaDB LLC. GT.M™ is a trademark of Fidelity National Information Services, Inc.
Other trademarks belong to their respective owners.

This document contains a description of YottaDB and the operating instructions pertaining to the various functions that comprise the software. This document does not contain any commitment of YottaDB LLC. YottaDB LLC believes the information in this publication is accurate as of its publication date; such information is subject to change without notice. YottaDB LLC is not responsible for any errors or defects.
