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

# YottaDB r1.36

### Release Note Revision History

| Revision  | Date              | Summary               |
| --------- | ----------------- | --------------------- |
| 1.00      |  2022 | r1.36 Initial Release |

### Contact Information
#### YottaDB LLC
https://yottadb.com /  <info@yottadb.com>

#### Support
##### Customers
Contact your YottaDB support channel.

##### Others
For free (to you) support from members of communities who run widely available applications on YottaDB, please use an application-specific list where appropriate.

 - For requests other than to the communities below, [post an Issue](https://gitlab.com/YottaDB/DB/YDB/-/issues) and include the words "Help Wanted" in the Title.
 - For access from node.js via [Nodem](https://github.com/dlwicksell/nodem), [post an Issue on the Nodem project](https://github.com/dlwicksell/nodem/issues/new/).
 - For access from [QewdJS](http://qewdjs.com/), or [EWD.js](https://github.com/robtweed/ewd.js), or from node.js via [mg-dbx](https://github.com/chrisemunt/mg-dbx) post to the [Enterprise Web Developer community](https://groups.google.com/forum/#!forum/enterprise-web-developer-community).
 - For requests specific to the use of YottaDB with [VistA](https://en.wikipedia.org/wiki/VistA) flavors, post to the [Hardhats](https://groups.google.com/forum/#!forum/hardhats) list.
 - For requests specific to the use of YottaDB with M other than for applications above, post to the [comp.lang.mumps](https://groups.google.com/forum/#!forum/comp.lang.mumps) list.

### r1.36

#### Overview
YottaDB r1.36 is a major release, not because it has a common theme, but because of the number of enhancements that it includes.

* On x86_64 architectures, SUSE Enterprise Linux 15.x is a Supported platform, and OpenSUSE Leap 15.x is Supportable. As OpenSUSE Tumbleweed is a rolling release, you may need to build the release from source code.
* There is new functionality for diagnostics, troubleshooting, and forensics, e.g.
     * $zstatus after INVVARNAME identifies the offending variable name in $ZWRITE format ([181](#x181))
     * Easier use of journal extract data for troubleshooting and forensics ([922](#x922))
     * %YDBPROCSTUCKEXEC captures DSE file headers ([937](#x937))
     * The SYSCALL message identifies file name if $ZROUTINES has Permission denied error ([954](#x954))
* YottaDB is friendlier to Kubernetes.
     * YottaDB runs in environments where the OS reports the terminal name as an empty string ([491](#x491))
* There are enhancements to make programming easier, e.g.
     * ydb_buffer_t can now be used for passing string values between C and M ([565](#x565))
     * The COMMAND deviceparameter limit for PIPE devices is 1MiB ([708](#x708))
     * $ZGETJPI() time measurements for processes other than the current process ([908](#x908))
     * The %ZMVALID() utility function checks whether a string is a syntactically correct line of M code ([919](#x919))
     * SET $ZGBLDIR sets ydb_cur_gbldir env var to new value of $ZGBLDIR ([941](#x941))
     * VIEW "GBLDIRLOAD" refreshes cached copies of global directories ([956](#x956))

r1.36 also inherits enhancements and fixes from GT.M V6.3-012, V6.3-013, and V6.3-014. Note that the %JSWRITE utility released with V6.3-012 is not publicly available, and is presumably therefore not available under a FOSS license. As there are numerous options to import and export global variables in JSON, please contact YottaDB to discuss what options might best fit your needs.

As with all YottaDB releases, there are a number of fixes and smaller enhancements, as described in the [Change History](#change-history).

#### Platforms
A platform is a combination of a CPU architecture and an operating system. A platform is Supported, Supportable, or Unsupported.

* Supported means that we have the platform in our development environment and test each release on that platform.
* Supportable means that although we do not necessarily have such a platform in our environment, we have no reason to believe that the software will not run on it.
* All others are Unsupported.

| CPU Architecture                    | Supported OS Version(s)                                                              | Notes                                                                                                                         |
| ----------------------------------- | ------------------------------------------------------------------------------------ | ----------------------------------------------------------------------------------------------------------------------------- |
| 64-bit x86                          | Ubuntu 20.04 LTS; Red Hat Enterprise Linux 8.x; SUSE Linux Enterprise 15.x; Debian GNU/Linux 11 (Bullseye) | There are separate binary distributions for each OS version, owing to differences in library versions of those distributions. |
| 64-bit ARM (Raspberry Pi 3 Model B) | Debian GNU/Linux 11 (Bullseye)                                                       | See below.                                                                                                                    |
| 32-bit ARM (Raspberry Pi Zero)      | Debian GNU/Linux 11 (Bullseye)                                                       | See below.                                                                                                                    |

Recent releases of major GNU/Linux distributions with contemporary kernels, glibc and ncurses are Supportable. Specific notes:

- Future releases of YottaDB will require 22.04 LTS as the minimum required Ubuntu version. Please contact your YottaDB support channel if this raises any concerns.
- [ydbinstall.sh](https://gitlab.com/YottaDB/DB/YDB/-/blob/master/sr_unix/ydbinstall.sh) recognizes [Rocky Linux](https://rockylinux.org/) 8 as equivalent to RHEL 8, and [OpenSUSE Leap](https://www.opensuse.org/#Leap) 15.x as equivalent to SUSE Linux Enterprise 15.x, and installs the corresponding versions. Note that Rocky Linux and OpenSUSE Leap are Supportable, not Supported.
- On [Arch Linux](https://www.archlinux.org/) and other leading edge distributions such as OpenSUSE Tumbleweed, YottaDB will likely need to be recompiled from source code owing to library and tool chain versions newer than those used in building Supported distributions. The `--from-source` option of [ydbinstall.sh](https://gitlab.com/YottaDB/DB/YDB/-/blob/master/sr_unix/ydbinstall.sh) simplifies the process. ([754](https://gitlab.com/YottaDB/DB/YDB/-/issues/754))
- While YottaDB is Supportable on other ARMv6, [ARMv7-A](https://en.wikipedia.org/wiki/Comparison_of_ARMv7-A_cores), and [ARMv8-A](https://en.wikipedia.org/wiki/Comparison_of_ARMv8-A_cores) CPUs, owing to variations in the implementations of ARM microarchitectures, we recommend that you ensure the software runs correctly before committing to any specific hardware other than those listed above. Please contact <info@yottadb.com> if you want a specific combination of OS and CPU microarchitecture to be Supported.

#### Installation
See our [Get Started page](https://yottadb.com/product/get-started) to use YottaDB.

We **strongly recommend** that you install YottaDB r1.36 in a newly created directory, different from those of prior YottaDB releases and any GT.M versions you may have installed on the system.

#### Removing an installed YottaDB release
Assuming `$ydb_dist` points to the directory where YottaDB is installed:

 - Cleanly shut down all application processes using that release.
 - Execute `mupip rundown && mupip rundown -relinkctl`
 - Ensure that there are no `gtcm*` or `gtmsecshr` processes active.
 - Use `sudo lsof | grep $ydb_dist` to ensure there are no open files.
 - Delete the directory with `sudo rm -rf $ydb_dist`

### Upgrading to YottaDB r1.36
As YottaDB r1.36 is upward compatible from YottaDB [r1.34](https://gitlab.com/YottaDB/DB/YDB/-/releases/r1.34), [GT.M V6.3-012](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-012_Release_Notes.html), [GT.M V6.3-013](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-013_Release_Notes.html), and [GT.M V6.3-014](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-014_Release_Notes.html). The minimal upgrade steps are:

* Install YottaDB r1.36. Use `ydbinstall` / `ydbinstall.sh` to install plugins you use.
* Install plugins you need in addition to those installed in the previous step.
* Recompile object code, and recreate shared libraries if your application uses shared libraries.
* If your application uses encryption, compile and install the reference implementation plugin (if not done by the `ydbinstall` / `ydbinstall.sh` script) or your customized plugin.
* Cleanly shut down the application and ensure that the database files are shut down using MUPIP RUNDOWN from the prior release.
* Switch journal files with the new YottaDB release.
* Start using the new YottaDB release.

If the database has triggers defined with non-canonical numbers, or numbers specified as strings with any version prior to [r1.28](https://gitlab.com/YottaDB/DB/YDB/-/releases/r1.28), or if you are unsure, extract the trigger definitions, delete existing triggers, and reload the trigger definitions. Issue [[#430](https://gitlab.com/YottaDB/DB/YDB/-/issues/430)] from [r1.28](https://gitlab.com/YottaDB/DB/YDB/-/releases/r1.28) has a series of steps you can copy and execute. There is no need to do this if upgrading from [r1.28](https://gitlab.com/YottaDB/DB/YDB/-/releases/r1.28) or later.

To upgrade from older GT.M releases, first upgrade to GT.M V6.0-000 or later and follow the steps above, or contact your YottaDB support channel for assistance and guidance.

A more sophisticated upgrade technique is:

* Install YottaDB r1.36.
* Create a new replicated instance of your application (on the same system or a different system).
* Assuming the existing instance is A, and the new instance is B, upgrade B to r1.36 and start replicating from A to B.
* Once B catches up, switchover so that B is in a primary role replicating to A.
* Once you are satisfied with B, remove (or upgrade) A.

### Change History

#### r1.36

YottaDB r1.36 includes the following enhancements and fixes beyond YottaDB [r1.34](https://gitlab.com/YottaDB/DB/YDB/-/releases/r1.34).

| ID             | Category              | Summary                                                                                                                |
|----------------|-----------------------|------------------------------------------------------------------------------------------------------------------------|
| ([181](#x181)) | Database              | $zstatus after INVVARNAME identifies the offending variable name in $ZWRITE format                                     |
| ([266](#x266)) | Database              | Argumentless MUPIP RUNDOWN syslog messages record removed Journal/Receive Pool ipcs  (semid/shmid)                     |
| ([459](#x459)) | Database              | MUPIP RUNDOWN REGION reports REPLINSTACC when instance file is missing                                                 |
| ([491](#x491)) | Other                 | YottaDB runs in environments where the OS reports the terminal name as an empty string                                 |
| ([564](#x564)) | Languages             | Copy only whatever can be safely copied for ydb_string_t structures                                                    |
| ([565](#x565)) | Languages             | Structure ydb_buffer_t for passing string values between C and M                                                       |
| ([708](#x708)) | Languages             | COMMAND deviceparameter limit for PIPE devices is 1MiB                                                                 |
| ([716](#x716)) | Database              | MUPIP CREATE with explicit region specification creates database files for AutoDB regions                              |
| ([729](#x729)) | Languages             | ACTLSTTOOLONG message includes line and column numbers                                                                 |
| ([763](#x763)) | System Administration | ydbinstall / ydbinstall.sh explicitly checks for libelf.so                                                             |
| ([765](#x765)) | System Administration | ydbinstall correctly reports failure to install UTF-8 mode                                                             |
| ([789](#x789)) | System Administration | ./ydbinstall --help correctly reports its name                                                                         |
| ([791](#x791)) | Languages             | Simple API calls with inappropriate variables passed as parameters raise meaningful errors                             |
| ([830](#x830)) | Languages             | SET lvn=$FNUMBER() leaves an undefined lvn undefined if $FNUMBER() encounters an error                                 |
| ([850](#x850)) | System Administration | Scripting does not rely on pkg-config to ascertain libicu version                                                      |
| ([855](#x855)) | System Administration | Multiple concurrent invocations of ydbinstall / ydbinstall.sh work correctly                                           |
| ([861](#x861)) | Languages             | $ZATRANSFORM() returns correct results in an edge case                                                                 |
| ([869](#x869)) | Languages             | Large numeric values used in Boolean expressions raise the NUMOFLOW error                                              |
| ([871](#x871)) | System Administration | ydbinstall.sh looks at ID_LIKE field of /etc/os-release if it does not recognize the ID field                          |
| ([872](#x872)) | Other                 | Large numbers of short-lived processes that access M code using auto-relink work correctly                             |
| ([877](#x877)) | Languages             | $VIEW("JOBPID") reports setting of VIEW "JOBPID"                                                                       |
| ([880](#x880)) | System Administration | ydbinstall options --linkexec and --linkenv update existing links                                                      |
| ([886](#x886)) | Languages             | Values assigned to $ZSTRPLLIM are always at least the minimum value                                                    |
| ([891](#x891)) | Other                 | SUSE Linux Enterprise on x86_64 is a Supported platform                                                                |
| ([893](#x893)) | Languages             | Minimum stringpool limit ($ZSTRPLLIM) is 150,000 bytes                                                                 |
| ([894](#x894)) | System Administration | Option for ydbinstall.sh to not update pkg-config                                                                      |
| ([901](#x901)) | Languages             | Code containing NEW or BREAK commands compiled with -noline_entry runs correctly                                       |
| ([908](#x908)) | Languages             | $ZGETJPI() time measurements for processes other than the current process                                              |
| ([919](#x919)) | Languages             | $$^%ZMVALID() function checks whether a string is a syntactically correct line of M code                               |
| ([922](#x922)) | Other                 | Easier use of journal extract data for troubleshooting and forensics                                                   |
| ([923](#x923)) | System Administration | ydb_env_set works on Debian systems where ICU version is not explicitly specified                                      |
| ([924](#x924)) | System Administration | ydbinstall.sh terminates after processing command line if not run as root, unless --dry-run is specified               |
| ([927](#x927)) | Database              | Remove database file initialization semaphore in case an error prevents use of that database file                      |
| ([934](#x934)) | Languages             | Error in $ZTIMEOUT in direct mode reports error and returns control                                                    |
| ([935](#x935)) | Languages             | Multi-threaded application that ensures single-threading of calls to unthreaded Simple API calls runs correctly        |
| ([937](#x937)) | Database              | %YDBPROCSTUCKEXEC captures DSE file headers                                                                            |
| ([940](#x940)) | Languages             | Flush terminal $PRINCIPAL on call-out from M                                                                           |
| ([941](#x941)) | Languages             | SET $ZGBLDIR sets ydb_cur_gbldir env var to new value of $ZGBLDIR                                                      |
| ([943](#x943)) | System Administration | ydbinstall installs encryption plugin with execute permissions                                                         |
| ([944](#x944)) | Database              | Transactions that allocate blocks to commit their updates no longer encounter TPFAIL errors with a failure code of eeee|
| ([945](#x945)) | Database              | ydb_env_set returns non-zero status on MUPIP CREATE errors                                                             |
| ([948](#x948)) | Languages             | Bug fixed that could cause abnormal process termination when compiling M code                                          |
| ([949](#x949)) | System Administration | getfields^%DUMPFHEAD() & MUPIP DUMPFHEAD report better error when file is not a database file                          |
| ([951](#x951)) | Languages             | OPEN command on /dev/null incorrectly assumes non-specified device parameters as having been specified                 |
| ([954](#x954)) | Languages             | SYSCALL message identifies file name if $ZROUTINES has Permission denied error                                         |
| ([956](#x956)) | Database              | VIEW "GBLDIRLOAD" refreshes cached copies of global directories                                                        |
| ([961](#x961)) | Other                 | Bug fixed that could cause cached code for $ZTIMEOUT to be recompiled                                                  |

#### GT.M V6.3-012
YottaDB r1.36 incorporates enhancements and fixes from [GT.M V6.3-012](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-012_Release_Notes.html).

| ID                      | Category              | Summary                                                                                        |
|-------------------------|-----------------------|------------------------------------------------------------------------------------------------|
| ([GTM-5381](#GTM-5381)) | System Administration | New -FULLBLKWRT qualifier for MUPIP SET and GDE                                                |
| ([GTM-7759](#GTM-7759)) | System Administration | Logging when YottaDB denies access to a process                                                |
| ([GTM-9136](#GTM-9136)) | Languages             | Optional standard behavior for WRITE #                                                         |
| ([GTM-9157](#GTM-9157)) | System Administration | Source Server gives more information and persists when name resolution fails                   |
| ([GTM-9182](#GTM-9182)) | System Administration | Appropriate edit checking on path/file length for MUPIP BACKUP                                 |
| ([GTM-9238](#GTM-9238)) | Other                 | Handle $ZSTPPLLIM settings and STPCRIT errors so as to avoid apparent loss of heap space       |
| ([GTM-9247](#GTM-9247)) | Languages             | $SELECT() handles truly tortured argument lists                                                |
| ([GTM-9248](#GTM-9248)) | Other                 | Correct "generated from" in messages                                                           |
| ([GTM-9249](#GTM-9249)) | System Administration | Receiver Server handles cross-endian decompression errors appropriately                        |
| ([GTM-9255](#GTM-9255)) | Database              | Prevent harmless segmentation violation (SIG-11) at process exit when using qdbrundown         |
| ([GTM-9260](#GTM-9260)) | Database              | MUPIP RUNDOWN removes abandoned memory associated with LOCK_SPACE                              |
| ([GTM-9269](#GTM-9269)) | Languages             | The -NOWARNING compilation qualifier applies to DONOBLOCK warnings                             |
| ([GTM-9270](#GTM-9270)) | Languages             | Improve alignment of $HOROLOG, $ZHORLOG, and $ZUT                                              |
| ([GTM-9273](#GTM-9273)) | Database              | dbcertify no longer maintained for this and future versions                                    |

#### GT.M V6.3-013
YottaDB r1.36 incorporates enhancements and fixes from [GT.M V6.3-013](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-013_Release_Notes.html).

| ID                      | Category              | Summary                                                                                                                  |
|-------------------------|-----------------------|--------------------------------------------------------------------------------------------------------------------------|
| ([GTM-7759](#GTM-7759)) | System Administration | Logging of access denials                                                                                                |
| ([GTM-7987](#GTM-7987)) | System Administration | Replication servers retry on some potentially transient network errors                                                   |
| ([GTM-8772](#GTM-8772)) | System Administration | MUPIP SET accepts actions that would not end FREEZE -ONLINE                                                              |
| ([GTM-8784](#GTM-8784)) | System Administration | MUPIP SET rejects actions that would end FREEZE -ONLINE                                                                  |
| ([GTM-8793](#GTM-8793)) | System Administration | More suitable error on return from an internally driven script                                                           |
| ([GTM-8838](#GTM-8838)) | Database              | Additional statistics                                                                                                    |
| ([GTM-8878](#GTM-8878)) | Other                 | Better ordering of journal record time stamps under a particular circumstance                                            |
| ([GTM-9147](#GTM-9147)) | System Administration | Larger maximum journal buffer size                                                                                       |
| ([GTM-9230](#GTM-9230)) | Database              | VIEW "NOIOSLATION" has the described impact on no-TP updates as on TP updates                                            |
| ([GTM-9252](#GTM-9252)) | System Administration | Prevent inappropriate IPC_RMID syslog messages caused by prior use of %PEEKBYNAME or ZHELP                               |
| ([GTM-9259](#GTM-9259)) | Languages             | More careful keyword parsing                                                                                             |
| ([GTM-9275](#GTM-9275)) | System Administration | MUPIP LOAD provides appropriate error message for some poorly formatted extract input                                    |
| ([GTM-9276](#GTM-9276)) | Languages             | Improved bounds check for M strings                                                                                      |
| ([GTM-9277](#GTM-9277)) | Languages             | In `ydb_side_effects` mode, fix handling of integer function arguments involving side effects                            |
| ([GTM-9278](#GTM-9278)) | Other                 | Better errors for unusual conditions                                                                                     |
| ([GTM-9287](#GTM-9287)) | Languages             | Fix syntax error message line number reporting for large M files                                                         |
| ([GTM-9288](#GTM-9288)) | Other                 | Fix issue with over-long M filenames                                                                                     |
| ([GTM-9289](#GTM-9289)) | System Administration | Appropriate behavior for MUPIP REPLIC -SOURCE -SHUTDOWN in the face of network issues                                    |
| ([GTM-9291](#GTM-9291)) | Languages             | Fix handling of $TEST as the first term of a Boolean that includes side effects                                          |
| ([GTM-9292](#GTM-9292)) | Languages             | Fix handling of side effect processing turned off during Boolean processing for $SELECT()                                |
| ([GTM-9293](#GTM-9293)) | Languages             | Where appropriate, accept an empty string result from argument indirection                                               |
| ([GTM-9294](#GTM-9294)) | Languages             | Fix handling of $QUERY(lvn) where the lvn has no subscripts or does not exist                                            |
| ([GTM-9295](#GTM-9295)) | Languages             | Various $[Z]TRANSLATE() fixes                                                                                            |
| ([GTM-9296](#GTM-9296)) | Languages             | Continue compilation after a BADCHAR error                                                                               |
| ([GTM-9311](#GTM-9311)) | Other                 | %YGBLSTAT NEWs local variables d and x to protect the user environment                                                   |
| ([GTM-9313](#GTM-9313)) | Languages             | $ORDER() issue with 1st argument subscripts containing both a gvn and a Boolean expression,and 2nd argument is a literal |

#### GT.M V6.3-014
YottaDB r1.36 incorporates enhancements and fixes from [GT.M V6.3-014](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-014_Release_Notes.html).

| ID                      | Category              | Summary                                                                                                     |
|-------------------------|-----------------------|-------------------------------------------------------------------------------------------------------------|
| ([GTM-7628](#GTM-7628)) | System Administration | Up to 64GiB for Source and receiver buffers                                                                 |
| ([GTM-8800](#GTM-8800)) | System Administration | MUPIP FTOK enhancements and MUPIP SEMAPHORE replace separate utilities                                      |
| ([GTM-8863](#GTM-8863)) | Database              | Provide YottaDB statistics indicating use of various critical sections                                      |
| ([GTM-9102](#GTM-9102)) | System Administration | MUPIP FREEZE is consistent across regions except for AIO                                                    |
| ([GTM-9266](#GTM-9266)) | System Administration | Correction to replication connection failure message                                                        |
| ([GTM-9285](#GTM-9285)) | System Administration | MUPIP REPLICATE -SOURCE -CONNECTPARAMS changes                                                              |
| ([GTM-9308](#GTM-9308)) | System Administration | Better messages from MUPIP RUNDOWN when in FREEZE -ONLINE                                                   |
| ([GTM-9320](#GTM-9320)) | System Administration | Suppress inappropriate EN022 syslog messages associated with use of READ_ONLY databases                     |
| ([GTM-9321](#GTM-9321)) | Languages             | Correct $REFERENCE maintenance in $ORDER(\<indirection>,\<literal>)                                         |
| ([GTM-9322](#GTM-9322)) | Database              | More care to release the replication journal pool when not doing so might cause a deadlock                  |
| ([GTM-9328](#GTM-9328)) | Languages             | No interrupting an already interrupted process                                                              |
| ([GTM-9329](#GTM-9329)) | Languages             | $ZTIMEOUT issue corrections                                                                                 |
| ([GTM-9331](#GTM-9331)) | Languages             | Prevent signal interference from external calls                                                             |
| ([GTM-9332](#GTM-9332)) | Database              | Prevent high LOCK activity over a region instantiation from causing moment of LOCK "confusion"              |
| ([GTM-9335](#GTM-9335)) | Database              | Prevent unusual case in which an abnormal process exit might cause an apparent database hang when using AIO |
| ([GTM-9336](#GTM-9336)) | System Administration | Correct an issue with MUPIP SIZE occasionally failing with a damaged stack                                  |
| ([GTM-9337](#GTM-9337)) | Database              | Prevent spurious error in a process that contributed to the snapshot facility used by MUPIP INTEG           |
| ([GTM-9338](#GTM-9338)) | System Administration | Allow the $ydb_tmp environment variable to control the placement of files used in trigger compilation       |

#### Database

* <a name="x181"></a> After a call with multiple variable names, such as ``ydb_lock_s()``, that reports an INVVARNAME error, the intrinsic special variable ``$zstatus`` acquired with a subsequent call to ``ydb_get_s()`` provides details on which variable name was invalid. [[#181](https://gitlab.com/YottaDB/DB/YDB/-/issues/181)]

* <a name="x266"></a> In SEMREMOVED/SHMREMOVED messages in the syslog, MUPIP RUNDOWN reports the ids of orphaned semaphore and shared memory segments (corresponding to the Journal and Receive Pools) that it removes, aiding troubleshooting. Previously it recorded this information only for database file ipcs and not for Journal/Receive pool ipcs. [[#266](https://gitlab.com/YottaDB/DB/YDB/-/issues/266)]

* <a name="x459"></a> When the environment variable [ydb_repl_instance](https://docs.yottadb.com/AdminOpsGuide/basicops.html#ydb-repl-instance) (or gtm_repl_instance) is defined, but the replication instance file it references does not exist, MUPIP RUNDOWN REGION reports a REPLINSTACC error. Previously it reported a misleading DBFILERR error. [[#459](https://gitlab.com/YottaDB/DB/YDB/-/issues/459)]

* <a name="x716"></a> A [MUPIP CREATE](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#create) command that explicitly specifies a region which is tagged in the global direction as [AutoDB](https://docs.yottadb.com/AdminOpsGuide/gde.html#no-au-todb) creates the database file for that region if it does not exist. Previously, it would not create the database file for such a region. A MUPIP CREATE command that does not specify any region, continues to not create database files for AutoDB regions. [[#716](https://gitlab.com/YottaDB/DB/YDB/-/issues/716)]

* <a name="x927"></a> YottaDB processes remove a semaphore, that is created when opening a database file, if they encounter an error that prevents use of that database file. Previously, in this situation, they could leave behind an orphaned semaphore. This was only encountered in the development environment, and was never reported by a user. [[#575](https://gitlab.com/YottaDB/DB/YDB/-/issues/575)] [[#927](https://gitlab.com/YottaDB/DB/YDB/-/issues/927)]

* <a name="x937"></a> To improve troubleshooting capabilities, the [%YDBPROCSTUCKEXEC](https://docs.yottadb.com/ProgrammersGuide/utility.html#ydbprocstuckexec) utility also captures file header data. It also logs its invocation in the syslog. Previously it did not. Also, some messages have been improved. Note that the global directory used for capturing the database file header data is $ydb_gbldir of the blocked process, rather than either that of the blocking process or $ZGBLDIR of the blocked process. [[#937](https://gitlab.com/YottaDB/DB/YDB/-/issues/937)]

* <a name="x944"></a> Transactions that allocate blocks to commit their updates no longer encounter TPFAIL errors with a failure code of eeee when the total number of blocks allocated since opening the database file exceeds 2**31. Previously, they did. The workaround was to monitor $$^%PEEKBYNAME("node_local.tp_hint",reg) and to shutdown all processes accessing that database file and then to restart the processes. [MUPIP RUNDOWN](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#rundown) can be used to verify that there are no processes accessing a database file. [[#944](https://gitlab.com/YottaDB/DB/YDB/-/issues/944)]

* <a name="x945"></a> When MUPIP CREATE reports errors, sourcing `ydb_env_set` returns a non-zero status to the shell, and reports the location of the MUPIP CREATE error output. It also forces NOASYNCIO in `gdedefaults` as the default database block size of 2048 bytes is incompatible with ASYNCIO in filesystems with 4096 byte blocks. Previously, it silently ignored MUPIP CREATE errors and reported a zero status. This issue was only encountered in our development and test environment, as was never reported by a user. [[#945](https://gitlab.com/YottaDB/DB/YDB/-/issues/945)]

* <a name="x956"></a> The VIEW "GBLDIRLOAD":str command instructs YottaDB to read the global directory file specified by `str`, cache its contents in memory, and switch to it as the current global directory ($ZGBLDIR). If the global directory file was previously read and its contents cached (e.g., by a SET $ZGBLDIR, or from the environment variables `ydb_gbldir` / `gtmgbldir` at process startup), YottaDB replaces its cached copy with a fresh copy. If `str` is not a valid global directory, YottaDB issues a [GDINVALID](https://docs.yottadb.com/MessageRecovery/errors.html#gdinvalid) error. If `str` is the empty string (`""`), YottaDB re-reads and again caches the global directory file specified by `ydb_gbldir` / `gtmgbldir`. If `str` is omitted, a `VIEWARGCNT` error is issued. As each VIEW "GBLDIRLOAD" command causes the process to permanently consume a small amount of memory, use it only when a global directory file has changed. Previously, once a YottaDB process read a global directory file, it would never refresh its cached copy. [[#956](https://gitlab.com/YottaDB/DB/YDB/-/issues/956)]

* <a name="GTM-8838"></a> YottaDB reports execution statistics WFR, BUS and BTS in the result from $VIEW("GVSTAT",\<region>). These statistics are described in the "ZSHOW Information Codes" section of the "YottaDB Programmer's Guide". MUPIP -DUMPFHEAD reports these statistics as "n_wait_for_read", "n_buffer_scarce" and "nt_bt_scarce" respectively. Previously, MUPIP -DUMPFHEAD reported these statistics with the names "t_qread_ripsleep_cnt_cntr", "db_csh_get_too_many_loops_cntr" and "bt_put"flush_dirty_cntr" along with the last transaction associated with the statistic with the same prefixes, but with the suffix "_tn" replacing "_cntr"; DUMPFHEAD no longer reports the transaction information. $VIEW() did not report these counters at all. These statistics are also available through the %PEEKBYNAME() interface under same names as used by DUMPFHEAD, and on a systemwide basis as WFR, BUS and BTS through the utility program ^%YGBLSTAT for those processes which have opted to share statistics. ([GTM-8838](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-013_Release_Notes.html#GTM-8838))

* <a name="GTM-8863"></a> YottaDB provides toggle statistics to indicate when a process is in a critical code section, and the general characterization of that critical section. When a process enters a critical section, it sets the appropriate toggle statistic to '1' and when it leaves the critical section, sets it back to '0'. YottaDB provides toggle statistics designated DEXA, GLB, JNL, MLK, PRC, TRX, ZAD, JOPA, AFRA, BREA, MLBA & TRGA, which are documented in the "ZSHOW" section of the YottaDB Programmers Guide. All of the standard YottaDB tools and facilities for examining statistics operate on the toggle statistics, and the only operational difference between the toggle statistics and the counter statistics previously provided by YottaDB is that, since toggle statistics provide a near real-time report on YottaDB process status rather than a cumulative state, they are neither saved on process exit nor loaded on process initialization. Note that we are still considering feedback on these added statistics, so they may be the subject of future changes. ([GTM-8863](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-014_Release_Notes.html#GTM-8863))

* <a name="GTM-9230"></a> YottaDB mini-transaction (non-TP) database SETs recognize the NOISOLATION characteristic; previously they did not. ([GTM-9230](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-013_Release_Notes.html#GTM-9230))

* <a name="GTM-9255"></a> YottaDB manages the release of the rendezvous (ftok) semaphore appropriately on process exit when using qdbrundown; previously when processes bypassed certain logic, they could encounter a SIG-11, which had no additional impact. ([GTM-9255](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-012_Release_Notes.html#GTM-9255))

* <a name="GTM-9260"></a> MUPIP RUNDOWN removes shared memory associated with LOCK hashing when removing database shared memory, as needed. Previously, in the rare case that YottaDB created an additional shared memory segment to hold the LOCK hash table and the database required a manual MUPIP RUNDOWN/RECOVER/ROLLBACK to run down the database, MUPIP did not remove the additional shared memory segment. ([GTM-9260](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-012_Release_Notes.html#GTM-9260))

* <a name="GTM-9273"></a> The dbcertify software to upgrade V4 GT.M databases to YottaDB is deprecated and no longer shipped as a separate package. Please use a prior YottaDB version to upgrade a V4 database. ([GTM-9273](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-012_Release_Notes.html#GTM-9273))

* <a name="GTM-9322"></a> YottaDB processes appropriately handle releasing the replication journal pool resource; previously in rare circumstances, processes might not release the resource as intended leading to a dead-lock that can only be cleared by stopping the process inappropriately continuing to hold the resource. ([GTM-9322](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-014_Release_Notes.html#GTM-9322))

* <a name="GTM-9332"></a> YottaDB deals appropriately with large numbers of LOCK releases. One aspect of this change eliminates counting LOCK pending removals, mostly from timeouts, as wake ups in internal counters. Previously if the releases reached 2**32 in the active life of a region (between shutdowns), processes could, for some typically brief period, receive an incorrect indication they had received the LOCK, when they had not. ([GTM-9332](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-014_Release_Notes.html#GTM-9332))

* <a name="GTM-9335"></a> YottaDB deals with the loss of a process involved with AIO database writes more promptly; previously it could take material time for it to recognize and deal with the issue, which manifested as a database hang. This behavior was seen in in-house testing and was never reported by a customer. ([GTM-9335](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-014_Release_Notes.html#GTM-9335))

* <a name="GTM-9337"></a> YottaDB process disengaging from the snapshot mechanism used by MUPIP INTEG ignores an error indicating the associated shared memory is already gone. Previously, such a process could occasionally and inappropriately get a SYSCALL error associated with shmdt and ENO22. ([GTM-9337](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-014_Release_Notes.html#GTM-9337))

#### Languages

* <a name="x564"></a> For `ydb_string *` O and IO [call-in parameters](https://docs.yottadb.com/ProgrammersGuide/extrout.html#call-in-table), YottaDB will copy a maximum of `length` bytes. Previously, it would raise an INVSTRLEN error if the number of bytes to be copied was greater than `length`. YottaDB recommends using the `ydb_buffer_t` structure to pass strings and binary data between application code and the YottaDB runtime system. [[#564](https://gitlab.com/YottaDB/DB/YDB/-/issues/564)]

* <a name="x565"></a> `ydb_buffer_t` is a structure for passing string values between C and M, and is recommended as `ydb_string_t` does not provide a way to distinguish between allocated storage and used storage. We would like to thank @pkoper for this suggested enhancement. [[#565](https://gitlab.com/YottaDB/DB/YDB/-/issues/565)]

* <a name="x708"></a> The COMMAND deviceparameter limit for PIPE devices is the YottaDB string maximum length (currently 1MiB). Previously it was 255 bytes. Note that literal string arguments are still limited to 255 bytes: concatenate them or use a variable for longer arguments. [[#708](https://gitlab.com/YottaDB/DB/YDB/-/issues/708)]

* <a name="x729"></a> The [ACTLSTTOOLONG](https://docs.yottadb.com/MessageRecovery/errors.html#actlsttoolong) M compilation error message includes the line and column numbers of the error. Previously it did not, which made debugging harder. [[#729](https://gitlab.com/YottaDB/DB/YDB/-/issues/729)]

* <a name="x791"></a> Simple API calls with inappropriate variable names passed as parameters raise meaningful errors in the following cases:

   - Calls to [Simple API](https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#simple-api) functions that specify [intrinsic special variables](https://docs.yottadb.com/ProgrammersGuide/isv.html#) with subscripts raise the ISVSUBSCRIPTED error as there are no subscripted intrinsic special variables. Previously such subscripts were ignored.
   - An intrinsic special variable passed as a parameter to a function where it is meaningless (e.g., [$trestart](https://docs.yottadb.com/ProgrammersGuide/isv.html#trestart) passed to [ydb_delete_excl_s()](https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#ydb-delete-excl-s-ydb-delete-excl-st)) raises the ISVUNSUPPORTED error. Previously, such usage raised [INVVARNAME](https://docs.yottadb.com/MessageRecovery/errors.html#invvarname) or [UNIMPLOP](https://docs.yottadb.com/MessageRecovery/errors.html#unimplop) errors.
   - A global variable name passed as a parameter to a function where it is meaningless, e.g., `ydb_delete_excl_s()` raises the GVNUNSUPPORTED error. Previously, it raised the INVVARNAME error.

  These uses were only detected in in-house development and test environments, and never reported by a user. [[#791](https://gitlab.com/YottaDB/DB/YDB/-/issues/791)]

* <a name="x830"></a> An undefined local variable that is the target of an assignment of a [$FNUMBER()](https://docs.yottadb.com/ProgrammersGuide/functions.html#fnumber) (e.g., SET X=$FNUMBER(y)) which encounters a runtime error leaves the local variable undefined. Previously, the local variable was assigned the value 0. This was only encountered in the development environment, and was never reported by a user. [[#830](https://gitlab.com/YottaDB/DB/YDB/-/issues/830)]

* <a name="x861"></a> [$ZATRANSFORM()](https://docs.yottadb.com/ProgrammersGuide/functions.html#zatransform) returns correct results when a numeric value that has no internal string representation is the first argument and the third argument is 2 or -2, specifying a return value of either the next or previous character in the collation order of the first character of the input value. Previously, this could either return the wrong value or get a SIGSEGV (SIG-11) failure generating a core. This edge case was encountered in our development and test environments and was never reported by a user. [[#861](https://gitlab.com/YottaDB/DB/YDB/-/issues/861)]

* <a name="x869"></a> Large numeric values (e.g., 1E47) used in Boolean expressions raise the [NUMOFLOW](https://docs.yottadb.com/MessageRecovery/errors.html#numoflow) error. Previously, they could result in segmentation violation (SIG-11) and abnormal process termination. This edge condition was only encountered in development and test environments and was never reported by a user. [[#869](https://gitlab.com/YottaDB/DB/YDB/-/issues/869)]

* <a name="x877"></a> The case-insensitive `"JOBPID"` parameter of $VIEW() returns the current setting of VIEW "JOBPID". Previously, there was not a way for M code to test the value of this setting. [[#877](https://gitlab.com/YottaDB/DB/YDB/-/issues/877)]

* <a name="x886"></a> An attempt to assign a value to [$ZSTRPLLIM](https://docs.yottadb.com/ProgrammersGuide/isv.html#zstrpllim) that is smaller than the minimum (currently 150,000 bytes) assigns the minimum value. Previously, an initial assignment to $ZSTRPLLIM was not checked against the minimum, and a subsequent assignment added the minimum value to the requested value. Also, previously, the minimum was 100,000 bytes. [[#886](https://gitlab.com/YottaDB/DB/YDB/-/issues/886)]

* <a name="x893"></a> The minimum value of [$ZSTRPLLIM](https://docs.yottadb.com/ProgrammersGuide/isv.html#zstrpllim) is 150,000 bytes. Previously, it was 100,000 bytes, which in practice was too small for just about any command, resulting in an immediate [STPCRIT](https://docs.yottadb.com/MessageRecovery/errors.html#stpcrit) error. [[#893](https://gitlab.com/YottaDB/DB/YDB/-/issues/893)]

* <a name="x901"></a> Certain cases of M code containing [NEW](https://docs.yottadb.com/ProgrammersGuide/commands.html#new) or [BREAK](https://docs.yottadb.com/ProgrammersGuide/commands.html#break) run when compiled with the [-noline_entry](https://docs.yottadb.com/ProgrammersGuide/devcycle.html#noline-entry) option, whether specified on a shell command line or through [$ZCOMPILE](https://docs.yottadb.com/ProgrammersGuide/isv.html#zcompile). Previously the code would crash with a segmentation violation (SIGSEGV / signal 11). [[#901](https://gitlab.com/YottaDB/DB/YDB/-/issues/901)]

* <a name="x908"></a> $ZGETJPI() retrieves process time measurements (CPUTIM, CSTIME, CUTIME, STIME, and UTIME) for the requested process. Previously, regardless of the process id specified, it returned data for the current process. If the information is not available for the specified process, e.g., the process does not exist, the function returns -1. If the process id specified is 0, the current process time measurements are returned. [[#908](https://gitlab.com/YottaDB/DB/YDB/-/issues/908)]

* <a name="x919"></a> $$^%ZMVALID(str) creates a temporary file containing the string, compiles it, and returns the result of the compilation as its output. If the parameter `str` is a valid line of M code, the output is the empty string. Note:

   - As the utility program prefixes `str` with a space, the column in which an error is reported will be one greater than the actual byte position in the string.
   - The output string will contain linefeed characters. When using M code to write the output to a file or terminal, set $X to 0 after writing each linefeed, in order to prevent unexpected line wrapping.

  [[#919](https://gitlab.com/YottaDB/DB/YDB/-/issues/919)]

* <a name="x934"></a> An error in the [$ZTIMEOUT](https://docs.yottadb.com/ProgrammersGuide/isv.html#ztimeout) intrinsic special variable by a direct mode process reports the error once and returns control to direct mode. Previously, it reported the error repeatedly in an infinite loop. [[#934](https://gitlab.com/YottaDB/DB/YDB/-/issues/934)]

* <a name="x935"></a> A multi-threaded application that ensures single-threading of calls to unthreaded Simple API calls (i.e., ensures that calls to the YottaDB engine happen only from one thread at any point in time) runs correctly. An example application is one that uses Flask web framework with the YDBPython wrapper. Previously, such an application could intermittently get `%YDB-E-SYSCALL, Error received from system call timer_create()` errors. [[#935](https://gitlab.com/YottaDB/DB/YDB/-/issues/935)]

* <a name="x940"></a> YottaDB flushes the output buffer of a terminal $PRINCIPAL device on call-out from M. Previously, it did not, which resulted in potentially unpredictable flushed output after the call. [[#940](https://gitlab.com/YottaDB/DB/YDB/-/issues/940)]

* <a name="x941"></a> SET $ZGBLDIR also sets the environment variable `ydb_cur_gbldir` to the new value of $ZGBLDIR. This allows a child process (PIPE device; ZSYSTEM or JOB command; $ydb_procstuckexec script) to decide whether to use the current global directory of the parent or the $ydb_gbldir / $gtm_gbldir at parent process startup. If $ydb_cur_gbldir is not set, it means the parent has not set a value to $ZGBLDIR. [[#941](https://gitlab.com/YottaDB/DB/YDB/-/issues/941)]

* <a name="x948"></a> A stack use after return bug that could cause abnormal process termination when compiling M code has been fixed. It is not feasible to characterize what sort of M code would trip the bug in normal use. The bug could not cause structural damage to the database. This issue was encountered during stress testing in the YottaDB development environment, and was never reported by a user. [[#948](https://gitlab.com/YottaDB/DB/YDB/-/issues/948)]

* <a name="x951"></a> An OPEN of `/dev/null` works correctly. Previously, it could treat unspecified deviceparameters as having been specified, with incorrect or invalid values. For example in a direct mode use case, a garbage value of the EXCEPTION deviceparameter caused a later READ command to issue an [INVCMD](https://docs.yottadb.com/MessageRecovery/errors.html#invcmd) error when compiling the EXCEPTION string, instead of issuing an expected [IOEOF](https://docs.yottadb.com/MessageRecovery/errors.html#ioeof) error. This issue was only encountered in our development and test environment, and was never reported by a user. [[#951](https://gitlab.com/YottaDB/DB/YDB/-/issues/951)]

* <a name="x954"></a> A `%YDB-E-SYSCALL` message followed by a `%SYSTEM-E-ENO13, Permission denied` error, while processing `$ZROUTINES`, identifies the file that resulted in the error, making it easier to troubleshoot the problem. Previously, the file name was not included in the error. [[#954](https://gitlab.com/YottaDB/DB/YDB/-/issues/954)]

* <a name="GTM-9136"></a> The [NO]FFLF deviceparameter controls whether WRITE # produces only a form-feed (\<FF>) or a form-feed and line-feed (\<FF>\<LF>). Previously, YottaDB used \<FF>\<LF> which deviated from the standard, however, out of concern for existing practice the default remains \<FF>\<LF>. Additionally, the "ydb_nofflf" environment variable controls the default WRITE # behavior of YottaDB. If it is unset or set to 0, N[O] or F[ALSE], the default behavior is unchanged. If it is set to 1, Y[ES] or T[RUE], the default behavior of WRITE # is changed to produce only a form-feed (\<FF>), though M programs can still control behavior by specifying the FFLF deviceparameter. ([GTM-9136](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-012_Release_Notes.html#GTM-9136))

* <a name="GTM-9247"></a> $SELECT() correctly handles a range of arguments that include literals that YottaDB optimizes at compile time and extensive nesting of the function. Previously, $SELECT() with some argument patterns failed, typically with a YottaDB fatal error, but in a few cases with an incorrect result. ([GTM-9247](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-012_Release_Notes.html#GTM-9247))

* <a name="GTM-9259"></a> The YottaDB compiler checks Z* keywords except for deviceparameters for the length provided by the source code and accepts a leading subset, with a small set of legacy abbreviations. This means that if you use a short form that's not unique, YottaDB evaluates it as the alphabetically first keyword or legacy abbreviation. YottaDB recommends using at least four characters in stored code, but ZBIT* functions and deviceparameters with a leading "NO" may need more. For standard keywords, the compiler requires correct spelling for the full keyword through up to eight characters and recognizes standard abbreviations. Previously the compiler ignored characters following matches in its internal tables which caused surprise if there were similar keywords or typos - in particular it may reject misspellings it accepted in prior releases. ([GTM-9259](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-013_Release_Notes.html#GTM-9259))

* <a name="GTM-9269"></a> The -NOWARNING compilation qualifier suppresses DONOBLOCK warnings; previously it did not. ([GTM-9269](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-012_Release_Notes.html#GTM-9269))

* <a name="GTM-9270"></a> $HOROLOG uses the same system service as $ZUT. Previously, when $HOROLOG was fetched first and scaled to microseconds, it could be less than a subsequently fetched $ZUT. ([GTM-9270](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-012_Release_Notes.html#GTM-9270))

* <a name="GTM-9276"></a> The Call-in interface uses the length from supplied size of `ydb_string_t` types to effectively bounds check the returned string. Programs using YottaDB call-ins need to reset the length of the `ydb_string_t` before invoking call-ins. Previously YottaDB wrote whatever the M routine returned into the caller's buffers. Users of `ydb_char_t` type should allocate at least 1 MiB of space to ensure that the M routine, which can potentially write up to 1MiB, does not overflow the callers buffer. ([GTM-9276](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-013_Release_Notes.html#GTM-9276))

* <a name="GTM-9277"></a> The YottaDB compiler, when using `ydb_side_effects`, deals appropriately cases when the last argument in a Boolean expression has a side effect and the result of the entire expression is evaluated as an integer. Most integer evaluations are for intrinsic function arguments. The side effects include extrinsic expressions, $INCREMENT() and indirect evaluations. Subsequent to the introduction of the `ydb_side_effects` mode, such evaluations could fail with an ASSERTPRO fatal error, segmentation violation (SIG-11), or rarely an incorrect result. ([GTM-9277](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-013_Release_Notes.html#GTM-9277))

* <a name="GTM-9287"></a> YottaDB correctly reports line numbers in syntax errors for M files  longer than 64K-1 lines. Previously, it reported error messages with line numbers in excess of that size incorrectly (modulo 64Ki). ([GTM-9287](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-013_Release_Notes.html#GTM-9287))

* <a name="GTM-9291"></a> The YottaDB compiler appropriately handles cases where $TEXT appears as the first term in a Boolean expression that includes side effects. Previously this pattern could cause fatal compilation errors or, infrequently, incorrect results. ([GTM-9291](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-013_Release_Notes.html#GTM-9291))

* <a name="GTM-9292"></a> The YottaDB compiler appropriately handles cases where the compiler detects a side effect within a $SELECT() argument, but subsequently determines that the side effect would not come into play at runtime. Starting in r1.34 $SELECT() changes created the possibility of this causing abnormal process termination. ([GTM-9292](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-013_Release_Notes.html#GTM-9292))

* <a name="GTM-9293"></a> YottaDB accepts empty string results from argument indirection when the command would accept such an argument; previously it rejected such a result as an invalid expression. ([GTM-9293](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-013_Release_Notes.html#GTM-9293))

* <a name="GTM-9294"></a> YottaDB appropriately handles $QUERY() of local variables that have no subscripts; previously such a pattern could cause a segmentation violation (SIG-11). ([GTM-9294](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-013_Release_Notes.html#GTM-9294))

* <a name="GTM-9295"></a> The $(Z)TRANSLATE() functions appropriately handle the case where the function appears more than once in a line and the function that evaluates first is within an arithmetic or Boolean expression. Subsequent to the optimization of $[Z]TRANSLATE() in V6.3-005 (GTM-8947) such a pattern could give an incorrect result. Also, $[Z]TRANSLATE() appropriately handles cases where UTF8 mode lengths exceeded expectations; previously these situations typically caused a segmentation violation (SIG-11). In addition, UTF-8 mode $TRANSLATE() evaluations with NOBADCHAR checking give uniform results, typically matching the $ZTRANSLATE() result with the same arguments. Previously the results depended on the code path followed to create the string containing the bad character(s). Note that YottaDB recommends use of BADCHAR checking when using standard M functions. ([GTM-9295](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-013_Release_Notes.html#GTM-9295))

* <a name="GTM-9296"></a> The YottaDB compiler appropriately handles a BADCHAR error in a UTF-8 source file as a syntax error; previously it stopped the compilation and did not produce an object file. ([GTM-9296](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-013_Release_Notes.html#GTM-9296))

* <a name="GTM-9313"></a> Prevent possible incorrect result when the first $ORDER() argument contains subscripts with both a Boolean expression and a global reference, and the second argument is a literal. ([GTM-9313](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-013_Release_Notes.html#GTM-9313))

* <a name="GTM-9321"></a> Name-level $ORDER(\<indirection>,\<literal>), where the indirection contains a subscripted or unsubscripted gvn, correctly maintains $REFERENCE; a change in V6.3-013 caused this case to incorrectly maintain $REFERENCE, giving it the value of the last prior global reference rather than that within the indirection. ([GTM-9321](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-014_Release_Notes.html#GTM-9321))

* <a name="GTM-9328"></a> When handling a MUPIP INTRPT, YottaDB discards any other such requests. Starting in V6.3-006, YottaDB could inappropriately accept multiple invocations of $ZINTERRUPT causing the process to get a fatal STACKOFLOW. ([GTM-9328](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-014_Release_Notes.html#GTM-9328))

* <a name="GTM-9329"></a> YottaDB handles an edge case for $ZTIMEOUT properly. Previously, cancelling a timer by setting $ZTIMEOUT to -1 before completely processing the $ZTIMEOUT action for an expired timeout could cause the $ZTIMEOUT action to repeat indefinitely. YottaDB retains the vector in $ZTIMEOUT properly. Previously, attempting to assign $ZTIMEOUT an incorrect vector routine, caused YottaDB to inappropriately clear the prior vector value. These behaviors were seen in in-house testing and were never reported by a customer. ([GTM-9329](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-014_Release_Notes.html#GTM-9329))

* <a name="GTM-9331"></a> The YottaDB external call and timer logic ensure that YottaDB recovers from external calls that ignore YottaDB timers or disrupt YottaDB signal handling. Note that YottaDB exposes a timer mechanism for use by external calls that integrates external timer needs with those of YottaDB. Note also that YottaDB external calls have a configuration option that claims an external call does not change the YottaDB signal handling; this option makes signal handling more efficient. As a result of changes in V6.3-006 associated with GTM-7952, an external call could suspend YottaDB's timer mechanism for a process, which caused timed operations (HANG, JOB, LOCK, OPEN, READ, and some Z* operations) to suspend indefinitely. The workaround was to signal such a process with a SIGALARM. ([GTM-9331](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-014_Release_Notes.html#GTM-9331))

#### System Administration

* <a name="x763"></a> `ydbinstall` / `ydbinstall.sh` warns users if `libelf.so` is not present, as it is required. Previously, the script failed in an obscure way. This was only detected in in-house development and test environments, and was never reported by a user. [[#763](https://gitlab.com/YottaDB/DB/YDB/-/issues/763)]

* <a name="x765"></a> `ydbinstall` reports failure when a requested UTF-8 mode installation fails. Previously it reported success, even when it failed. This was only encountered in the development environment, and was never reported by a user. [[#765](https://gitlab.com/YottaDB/DB/YDB/-/issues/765)]

* <a name="x789"></a> `ydbinstall --help` correctly shows its commands, e.g., `./ydbinstall --utf8 default`. Previously it added a `.sh` suffix to the commands, e.g., `./ydbinstall.sh --utf8 default`. Note that the same script when packaged with a YottaDB distribution is named `ydbinstall` and `ydbinstall.sh` when [downloaded from GitLab](https://gitlab.com/YottaDB/DB/YDB/-/blob/master/sr_unix/ydbinstall.sh) and executed stand-alone. [[#789](https://gitlab.com/YottaDB/DB/YDB/-/issues/789)]

* <a name="x850"></a> Scripting (`ydbinstall` / `ydbinstall.sh` and `ydb_env_set`) uses alternative methods to ascertain the version of libicu. Previously, it relied on pkg-config, which was not always installed. `ydbinstall` / `ydbinstall.sh` continues to install a `yottadb.pc` file whether or not pkg-config is installed. [[#850](https://gitlab.com/YottaDB/DB/YDB/-/issues/850)]

* <a name="x855"></a> Multiple concurrent invocations of `ydbinstall` / `ydbinstall.sh` work correctly. Previously, owing to a race condition, this could result in unpredictable results. This edge condition was only detected in in-house development and test environments, and was never reported by a user. [[#855](https://gitlab.com/YottaDB/DB/YDB/-/issues/855)]

* <a name="x871"></a> If `ydbinstall` does not recognize the Linux distribution from the ID field of `/etc/os-release`, it looks at the first part of the ID_LIKE field and uses the YottaDB distribution for that Linux distribution, if there is one. For example, Linux Mint does not have a recognizable ID field, but with an ID_LIKE `"ubuntu debian"`, `ydbinstall` fetches and installs the YottaDB distribution for Ubuntu. [[#871](https://gitlab.com/YottaDB/DB/YDB/-/issues/871)]

* <a name="x880"></a> The `--linkexec` and `--linkenv` options of the `ydbinstall` / `ydbinstall.sh` script correctly update existing links to point to the newly installed YottaDB. Previously, while the script would create the links if none existed, it would not update existing links. [[#880](https://gitlab.com/YottaDB/DB/YDB/-/issues/880)]

* <a name="x894"></a> With the `--nopkg-config` option, `ydbinstall` / `ydbinstall.sh` neither creates a `yottadb.pc` file for `pkg-config` to use, nor updates an existing file. This allows one to try a YottaDB release affecting applications that are using an installed release. [[#894](https://gitlab.com/YottaDB/DB/YDB/-/issues/894)]

* <a name="x923"></a> Sourcing `ydb_env_set` now works on Debian GNU/Linux systems on which YottaDB is installed with UTF-8 support, and `ydb_icu_version` or `gtm_icu_version` is not explicitly set. Previously, it failed with an error that it could not find the ICU version. The workaround was to explicitly set `ydb_icu_version` or `gtm_icu_version`, or to include /sbin in the the PATH environment variable. This was only encountered in the development environment, and was never reported by a user. [[#923](https://gitlab.com/YottaDB/DB/YDB/-/issues/923)]

* <a name="x924"></a> `ydbinstall` / `ydbinstall.sh` terminates after processing command line if not run as root, unless `--dry-run` is specified. `ydbinstall.sh` users on all platforms, and `ydbinstall` users on slow platforms may see slightly improved performance; others may not. [[#924](https://gitlab.com/YottaDB/DB/YDB/-/issues/924)]

* <a name="x943"></a> `ydbinstall` installs the encryption plugin with execute permissions under `$ydb_dist/plugin/ydbcrypt`. In YottaDB, owing to a regression, they were installed without execute permissions. A workaround in r1.34 was to run `sudo chmod +x $ydb_dist/plugin/ydbcrypt/*.sh` manually. This was only encountered in the development environment, and was never reported by a user. [[#943](https://gitlab.com/YottaDB/DB/YDB/-/issues/943)]

* <a name="x949"></a> If the file on which they are invoked is not a database file, getfields^%DUMPFHEAD() and MUPIP DUMPFHEAD report that the file is not a database file. MUPIP DUMPFHEAD returns a status of 10 to the shell in this case. Previously, they printed less helpful messages, such as `The file "<filename>" had error:` or `GETFIELDS: Error - unable to read fileheader from <filename>`. [[#949](https://gitlab.com/YottaDB/DB/YDB/-/issues/949)]

* <a name="GTM-5381"></a> GDE and MUPIP SET recognize the FULLBLKWRT database segment/file characteristic to determine whether YottaDB writes only valid database blocks contents, or a full block including meaningless trailing content. Full block writes are more efficient with some secondary storage because they avoid read-before-write. The format of the FULLBLKWRT qualifier is:
   -FU[LLBLKWRT]={0|1|2}

    - When -FULLBLKWRT=2, a process writes all newly allocated database blocks in their entirety regardless of their actual valid contents. This relieves some file systems from tracking as much unallocated space and thus reduces file system metadata maintenance.
    - When -FULLBLKWRT=1, a process writes entire file system blocks in their entirety regardless of their actual valid contents, on some file systems, this avoids reading in advance of most writes and thus reduces file system load and increases response time.
    - When -FULLBLKWRT=0 (the default), a process writes only valid data.

  Note that when the file system block size and the database block size are the same there is no difference between the settings of 1 and 2.

  Previously the `ydb_fullblockwrites` environment variable of the first process to access the database (or its absence) determined this characteristic. With this change, YottaDB no longer recognizes the enviroment variable `ydb_fullblockwrites`. ([GTM-5381](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-012_Release_Notes.html#GTM-5381))

* <a name="GTM-7628"></a>If there is enough free shared memory available, the MUPIP REPLICATE qualifier -BUFFSIZE accepts values of up to 64 GiB for source and receiver servers. Previously, this maximum was 4GiB. ([GTM-7628](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-014_Release_Notes.html#GTM-7628))

* <a name="GTM-7759"></a> By default YottaDB uses the the syslog() facility to log a number of errors related to permissions and access. Previously, it only noted these messages on the console or, if output was redirected, in a YottaDB process's error output. The YottaDB restriction LOGDENIALS provides a facility for disabling this logging on a Unix group basis. If the restriction mechanism is not used, the logging takes place for all YottaDB processes. If the restriction is used logging takes place for specified groups only.

  To support restriction based configuration of LOGDENIALS, the YottaDB restriction handling code now supports group names using the POSIX Portable Filename Character Set:

        3.282 Portable Filename Character Set

        The set of characters from which portable filenames are constructed.

        A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
        a b c d e f g h i j k l m n o p q r s t u v w x y z
        0 1 2 3 4 5 6 7 8 9 . _ -

  In practice, this means the characters "-" and "." are now accepted by YottaDB in group names specified in the restrict.txt file whereas previously they were not. ([GTM-7759](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-013_Release_Notes.html#GTM-7759))

* <a name="GTM-7987"></a> Replication servers try to reconnect for some errors from the TLS/SSL layers; previously, these errors caused the servers to terminate with a TLSIOERROR. ([GTM-7987](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-013_Release_Notes.html#GTM-7987))

* <a name="GTM-8772"></a> When FREEZE -ONLINE is in place, MUPIP SET fails for any command that requires a write to a frozen database file, and does so with the OFRZACTIVE warning message. In such circumstances, the operator must release the freeze before performing a standalone operation. The purpose of the FREEZE -ONLINE is to provide a consistent state for a storage-based backup without impacting on-line users. ([GTM-8772](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-013_Release_Notes.html#GTM-8772))

* <a name="GTM-8784"></a> MUPIP SET flushes only the database file header for the following non-standalone operations: Epoch taper, Flush time, Hard spin count, Inst freeze on error, Sleep spin count and Spin sleep mask. During FREEZE -ONLINE, MUPIP SET rejects non-standalone operations and issues GTM-W-OFRZACTIVE. In such circumstances, the operator must release the freeze before performing the operation. Previously, MUPIP SET flushed all buffers for non-standalone operations and implicitly released the freeze during FREEZE -ONLINE. ([GTM-8784](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-013_Release_Notes.html#GTM-8784))

* <a name="GTM-8793"></a> YottaDB provides an appropriate error message which indicates a non-zero exit status returned from the script specified by `ydb_procstuckexec` (or other YottaDB driven script). Previously, YottaDB issued a system call error with an inappropriate errno. ([GTM-8793](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-013_Release_Notes.html#GTM-8793))

* <a name="GTM-8800"></a> MUPIP FTOK recognizes the -ID=\<integer> qualifier to specify the signature use for the ftok; if unspecified, it defaults to the signature for a YottaDB database file. MUPIP FTOK also recognizes the -ONLY qualifier to restrict the output to only the file's ftok; if a file is not valid and accessible, FTOK reports -1 values. By default, the utility provides additional information about the file, including its file ID which provides the basis for the FTOK. In addition, MUPIP FTOK recognizes the -[NO]HEADER qualifier; -ONLY output has no header, but by default other forms include a header that -NOHEADER suppresses. Any existing parsing of the output require adjustments to deal with the additional information and revised spacing. If the database file is unavailable, the utility defaults to the -ONLY behavior. In addition, the utility accepts a space (\<SP>) delimited list of files, such as that provided by the use of the * and ? shell wildcard characters. With either the -JNLPOOL or -RECVPOOL qualifier, MUPIP FTOK ignores any files in the list. Note that to minimize ftok collisions, YottaDB uses its own ftok generation algorithm. This added functionality in MUPIP FTOK replaces the standalone ftok utility provided in the YottaDB distribution; that executable has been removed from the release package. Previously, MUPIP FTOK did not recognize these qualifiers, only processed a single file, and did not show the FTOK in its output. MUPIP SEMAPHORE reports the details of the a space delimited list of semaphores IDs. This replaces the undocumented standalone semstat2 utility in the YottaDB distribution; that executable has been removed from the release package. Note the output format differs slightly from that of semstat2. Previously MUPIP did not support the SEMPHORE command. ([GTM-8800](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-014_Release_Notes.html#GTM-8800))

* <a name="GTM-9102"></a> MUPIP FREEZE -ONLINE institutes the FREEZE on all regions in parallel to produce a consistent snapshot for a multiple-region database. FREEZE -ONLINE momentarily suspends updates to all regions while instituting the freeze. This action could slow regular processing until all regions are frozen. When AIO is ON, FREEZE -ONLINE institutes the FREEZE serially. Previously, MUPIP FREEZE -ONLINE froze each region serially which could result in an inconsistent snapshot with the final regions have sequence numbers higher than the first regions. ([GTM-9102](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-014_Release_Notes.html#GTM-9102))

* <a name="GTM-9147"></a> MUPIP SET -JOURNAL -BUFFSIZE accepts values up to 1Mi 512-byte blocks, corresponding to 16GiB; previously the maximum 32Ki, corresponding to 16 Mib. ([GTM-9147](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-013_Release_Notes.html#GTM-9147))

* <a name="GTM-9157"></a> When the Source Server fails to resolve a secondary host name to an IP address, it reports the secondary host name with the GETADDRINFO message in the Source Server log file and continues attempts to resolve the host name to an IP address as per the -CONNECTPARAMS settings. Previously, the Source Server did not display the host name in the GETADDRINFO message and terminated the Source Server process. This change increases the replication resilience in events like network reconfiguration and cluster management. ([GTM-9157](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-012_Release_Notes.html#GTM-9157))

* <a name="GTM-9182"></a> MUPIP BACKUP issues FILENAMETOOLONG when the backup file path size exceeds the maximum allowed by YottaDB, 255 bytes. Previously, MUPIP BACKUP could at least partially execute in spite of file paths that exceeded the maximum length allowed by YottaDB. ([GTM-9182](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-012_Release_Notes.html#GTM-9182))

* <a name="GTM-9249"></a> Receiver Server connected to a cross endian Source Server handles decompression errors appropriately. Previously when there was an issue with zlib uncompress, and the Source Server has the opposite endian, the Receiver Server would exit with a KILLBYSIGSINFO1 error. This was only encountered in the YottaDB development environment, and was never reported by a user. ([GTM-9249](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-012_Release_Notes.html#GTM-9249))

* <a name="GTM-9252"></a> Processes exit appropriately after using a -READ_ONLY database such as $ydb_dist/ydbhelp.dat; previously they could produce a harmless, but concerning, SYSCALL syslog entry mentioning semctl (IPC_RMID, ...) ([GTM-9352](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-013_Release_Notes.html#GTM-9252))

* <a name="GTM-9266"></a> The replication source server includes the correct error text when logging failed connection attempts. Previously, the wrong error text may have been included in the message. This was found during internal testing and has never been reported in the field. ([GTM-9266](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-014_Release_Notes.html#GTM-9266))

* <a name="GTM-9275"></a> MUPIP LOAD provides appropriate error messages when the record after the header of a FORMAT=GO or ZWR file is missing or invalid; previously, in such cases MUPIP issued GTM-E-MAXSTRLEN. ([GTM-9275](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-013_Release_Notes.html#GTM-9275))

* <a name="GTM-9285"></a> The -CONNECTPARAMS qualifier MUPIP REPLICATE -SOURCE -START includes the following fixes and enhancements:

    - -CONNECTPARAMS takes the fourth comma-delimited value as the approximate alert-time in seconds after which the Source Server records the REPLAERT message in the Source Server log file when it fails to establish a replication connection during soft connection attempts. Specify 0 if you want to disable the REPLALERT logging behavior. By default, the logging of the REPLALERT message for soft connection attempt failures is disabled. Previously, MUPIP only reported the REPLWARN message for such failures with an inaccurate time.
    - The Source Server sleeps for hard-tries-period when it fails to resolve the network address specified with the -SECONDARY qualifier. Also, the Source Server allows users to bypass hard connection attempts by specifying zero (0) as the hard-connection-count for more timely logging of the REPLALERT messages as the Source Server bypasses hard connection attempts. Previously, it was not possible to bypass hard connection attempts and the Source Server did not sleep for hard-tries-period when the network address specified with -SECONDARY resulted in a network resolution failure.
    - YottaDB automatically adjusts the values for soft-tries-period and hard-tries-period to about half of the YottaDB determined shutdown wait time. YottaDB logs these adjustments in the Source Server log file. This helps prevent inadvertent timeouts of a Source Server shutdown due to high soft-tries-period and/or hard-tries-period and prevents out-of-design conditions. The Source Server log file includes the units for hard-tries-period in milliseconds and the units of soft-tries-period and alert-time in seconds. Previously, the Source Server log file did not include the time measurement units.
    - MUPIP reports a BADCONNECTPARAM if the specification is incorrect or the user specifies a value that may lead to an out-of-design situation. MUPIP also allows users to specify up to six parameters with -CONNECTPARAMS and uses the defaults for the parameters not specified. Previously, MUPIP displayed the same "Error parsing or invalid parameter in CONNECTPARAMS" message for parsing error in any -CONNECTPARAMS parameter and required all six parameters.

  Please refer to the "Starting the Source Server" section of the Administration and Operations Guide for complete information on -CONNECTPARAMS. ([GTM-9285](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-014_Release_Notes.html#GTM-9285))

* <a name="GTM-9289"></a> MUPIP REPLIC -SOURCE -SHUTDOWN performs a shutdown for a Source Server that keeps retrying to establish a connection with the Receiver Server, but keeps failing due to a network address resolution error. Also, the Source Server provides more timely recognition and recovery from transient network problems. Previously, MUPIP REPLIC -SOURCE -SHUTDOWN timed out in such a situation. ([GTM-9289](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-013_Release_Notes.html#GTM-9289))

* <a name="GTM-9308"></a> When MUPIP RUNDOWN encounters a region with an active FREEZE -ONLINE, it skips running down the region and issues an OFRZACTIVE warning message. Previously under these circumstances, MUPIP issued less appropriate messages. ([GTM-9308](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-014_Release_Notes.html#GTM-9308))

* <a name="GTM-9320"></a> YottaDB does not issue warning messages to the syslog when processes close READ_ONLY databases; previously it did, which was noticeable when using ^%PEEKBYNAME. ([GTM-9320](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-014_Release_Notes.html#GTM-9320))

* <a name="GTM-9336"></a> When MUPIP SIZE encounters an apparently invalid key, it either ignores it or reports a database integrity error, depending on the nature of the issue; previously some such keys could cause the utility to fail due to a damaged stack. ([GTM-9336](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-014_Release_Notes.html#GTM-9336))

* <a name="GTM-9338"></a> Trigger compilation responds to the location specified by the `ydb_tmp` environment variable; previously it always used the default for `ydb_tmp`, which is /tmp. ([GTM-9338](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-014_Release_Notes.html#GTM-9338))

#### Other


* <a name="x491"></a> YottaDB runs in environments where the operating system reports that a terminal exists, and that its name is the empty string (`""`). Previously, it would terminate with a Segmentation Violation (SIG-11) under such conditions. [[#491](https://gitlab.com/YottaDB/DB/YDB/-/issues/491)]

* <a name="x872"></a> Large numbers of short-lived processes that access M code using [auto-relink](https://docs.yottadb.com/ProgrammersGuide/commands.html#auto-relink-setup) work correctly. Previously, this could result in processes terminating abnormally with GTMASSERT2 errors. Such a workload might be encountered with web-server CGI processes on a fast machine under load. This issue was detected in development and test environments and was never reported by a user. [[#872](https://gitlab.com/YottaDB/DB/YDB/-/issues/872)]

* <a name="x891"></a> Effective r1.36, SUSE Linux Enterprise is a Supported platform. Previously it was Supportable. [[#891](https://gitlab.com/YottaDB/DB/YDB/-/issues/891)]

* <a name="x922"></a> %YDBJNLF is new functionality to simplify analytics and forensics using journal files.

  %YDBJNLF is available in r1.36 as field-test software. As we normally strive to ensure upward compatibility of functionality from release to release, the field-test designation indicates that, based on user input, we may improve the API in a way that is not upward compatible. Please use %YDBJNLF and give us feedback.

  For detailed explanation refer to the [Additional Information](#additional-information) section. [[#922](https://gitlab.com/YottaDB/DB/YDB/-/issues/922)]

* <a name="x961"></a> A heap use after free error that could cause code cached for $ZTIMEOUT to be discarded, forcing recompilation, has been fixed. In very rare scenarios, it could theoretically cause abnormal process termination. The bug could not cause structural damage to the database. The issue was encountered during stress testing in the YottaDB development environment, and was never reported by a user. [[#961](https://gitlab.com/YottaDB/DB/YDB/-/issues/961)]

* <a name="GTM-8878"></a> YottaDB attempts to correctly time stamp journal files in cases where system load delays the exiting logic of an exiting process. Previously such cases could cause slightly out-of-order time stamps. ([GTM-8878](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-013_Release_Notes.html#GTM-8878))

* <a name="GTM-9238"></a> YottaDB manages detection of STPCRIT errors to minimize the chance that repeats of those errors indicate decreasing amounts of available space; previously repeated STPCRIT errors inappropriately indicated a loss of string pool space. Also, an attempt to set the limit lower than the miminum string pool size adds the requested limit to the minimum size; previously, YottaDB silently ignored such attempts. ([GTM-9238](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-012_Release_Notes.html#GTM-9238))

* <a name="GTM-9248"></a> YottaDB includes the correct value for "generated from" in system log messages. Since V60002 the value indicated the wrong location. ([GTM-9248](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-012_Release_Notes.html#GTM-9248))

* <a name="GTM-9278"></a> YottaDB Utility programs (DSE, LKE, MUPIP) output more details when there is a failure to communicated with gtmsecshr and when an unexpected errors causes YottaDB to open a database file as read-only. ([GTM-9278](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-013_Release_Notes.html#GTM-9278))

* <a name="GTM-9288"></a> YottaDB handles long M filenames appropriately; previously filenames longer than 32 bytes (not including the path or the .m extension) caused a buffer overflow, possibly resulting in improper timer operation or a segmentation violation (SIG-11). ([GTM-9288](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-013_Release_Notes.html#GTM-9288))

* <a name="GTM-9311"></a> %YGBLSTAT NEWs local variables d and x to protect the user environment; previously it did not. ([GTM-9311](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-013_Release_Notes.html#GTM-9311))

### Additional Information

#### Overview

Since journal files capture database state changes, they are invaluable for troubleshooting and forensics.

While [MUPIP JOURNAL EXTRACT](https://docs.yottadb.com/AdminOpsGuide/ydbjournal.html#extract-file-name-stdout) provides that information in a human readable form, given the large volume of data in even a single journal file from a production system, automation facilitates analysis. The %YDBJNLF utility routine loads journal extracts into global variables, allowing software to answer questions such as which process(es) updated a certain global, in what sequence and when; that global variable updates a process made; etc.

Using the [Octo SQL Engine](https://docs.yottadb.com/Octo/) makes it even easier to query journal records ingested by %YDBJNLF. This further enables those who are not software developers or system administrators, such as business analysts and auditors, to query and analyze the data. For example:

```
OCTO> select op,horologdate,horologtime,pid from YDBJNLFTYPE1 where gvref = '^x(5)' order by horologdate,horologtime;
OP|HOROLOGDATE|HOROLOGTIME|PID
SET|66385|59173|8937
KILL|66386|54903|20312
SET|66386|55166|20312
(3 rows)
OCTO>
```

#### Ingestion

INGEST^%YDBJNLF(jnlfile[,label]) uses [MUPIP JOURNAL EXTRACT FORWARD SHOW=ALL FENCES=NONE DETAIL FULL NOVERIFY](https://docs.yottadb.com/AdminOpsGuide/ydbjournal.html#journal-selection-qualifiers) to extract journal file jnlfile into global variables as described below. Since troubleshooting and forensics may need damaged journal files to be ingested, %YDBJNLF uses the NOVERIFY option.

- If `label` is specified, it is used to identify the extract; otherwise the journal file name `jnlfile` is the identifying label.
- INGEST deletes any existing `^%ydbJNLF*(label)` global variables. Use a unique label for each call to INGEST if the journal file name is not unique, e.g., current generation journal files like `yottadb.mjl`, because they will be renamed when the journal file is switched, and ingesting a subsequent file with same name will replace previously ingested data. For prior generation journal files, e.g., `yottadb.mjl_2022341165323` using a label is less important as the file name is permanent. If you are ingesting a journal file from another machine, e.g., on a machine designated for forensics, then a label is required to distinguish journal files from different machines.

#### Global Variables

Global variables store both journal extract records as well as metadata. The general format of global variables used to store journal extract records is:

- a prefix of `^%ydbJNLF` and a suffix of the record type, as discussed below, e.g., `^%ydbJNLFTYPE1`;
- a first subscript, which is the label;
- a second subscript, which is the offset in the journal file at which the record begins; and
- a third subscript, which is the size of the record, in bytes.

Since journal files are written sequentially, the offset states definitively whether, for example, a SET was executed before or after a KILL.

The data at each node is a `"\"` separated record, as documented in [MUPIP JOURNAL EXTRACT formats](https://docs.yottadb.com/AdminOpsGuide/ydbjournal.html#journal-extract-formats) with the following changes to expedite analysis:

- The first piece, preceding the record, is the record type, KILL, PBLK, PINI, SET, etc.
- The [$HOROLOG](https://docs.yottadb.com/ProgrammersGuide/isv.html#horolog) timestamp of the record is stored in two separate pieces, date and time.
- `node` fields in extract records are stored as two pieces, the global variable (e.g., `^x`) and the complete global reference (e.g., `^x(2)`).
- `node=sarg` fields in extract records are stored as three pieces, the global variable, the global reference, and the value. So `^x(2)=ABC` woudl be stored as `…\^x\^x(2)\"ABC"`.
- Extracts for SET and KILL type records are all stored in the same `^%ydbJNLFTYPE1` global variable, except that the KILL type records do not have the last piece, which stores the value for SET type records.

For example, the MUPIP JOURNAL EXTRACT format for SET records is:
```
time\tnum\chksum\pid\clntpid\token_seq\strm_num\strm_seq\updnum\nodeflags\node=sarg
```
^%ydbJNLFTYPE1 nodes store SET records as
```
op\horologdate\horologtime\tnum\chksum\pid\clntpid\token_seq\strm_num\strm_seq\updnum\nodeflags\gvname\gvref\nodeval
```
where `op` is `SET`.

Metadata for the journal extract data is stored in global variables as follows:

- `^%ydbJNLF`, with records:

  - `^%ydbJNLF(label,"extrfmt")` contains the format of the journal extract, e.g., `"YDBJDX09 UTF-8"`.

  - `^%ydbJNLF(label)` contains metadata about the extract, in semicolon (`;`) separated pieces, with strings quoted. Fields of interest are:

    - The journal file name, e.g., `"/tmp/test/r1.36_x86_64/g/yottadb.mjl"`.
	- The journal file format, e.g., `"YDBJNL44"`.
	- The database file name, e.g., `"/tmp/test/r1.36_x86_64/g/yottadb.dat"`.
	- The prior generation journal file name, if any, e.g., `"/tmp/test/r1.36_x86_64/g/yottadb.mjl_2022277114311"`.
	- A subsequent journal file name if any. This field is typically blank, except for journal files involved in a MUPIP JOURNAL BACKWARD RECOVER/ROLLBACK.
	- If before-image journaling is enabled, the string `"ENABLED"`.
	- The remaining fields are primarily of interest to system administrators. Consult your YottaDB support channel or the source code for more information.

- `^%ydbJNLFACTIVE(label,counter)` has records of processes that had the journal file open at the time of the EXTRACT, where counter is simply an incrementing integer. Semicolon separated pieces are as follows:

  - The process pid.
  - The node name of the computer, e.g., `mylaptop`.
  - The user name of the process, e.g., `ydbuser`.
  - The terminal or pseudo-terminal of the session, if any, e.g., `3`.
  - The $HOROLOG date when the process opened the journal file, e.g., `66386`.
  - The $HOROLOG time of day when the process opened the journal file, e.g., `42207`.

- `^%ydbJNLFCOMPLETE(label,counter)` has records of processes that previously had the journal file open, but no longer do. The fields are the same as for `^%ydbJNLFACTIVE`, except that the pid may not be unique, in the event the operating system recycled pids while the journal file was active.

- `^%ydbJNLFOPCOUNT(label,op)` has count of each opcode, e.g., if label is `"TEST1"`, the node `^%ydbJNLFOPCOUNT("TEST1","SET")` reports the number of SET records ingested.

- Since the number of record formats is smaller than the number of opcodes, e.g., all SET and KILL opcodes use the same record type, `^%ydbJNLFRECTBL(label,op)` specifies the record type for each opode. For example, `^%ydbJNLFRECTBL("TEST1","SET")="TYPE1"` means that global variables nodes that store ingested SET records are `^%ydbJNLFTYPE1("TEST1",offset,recsize)`.

#### Octo DDL

`OCTODDL^%YDBJNLF([rectype])` outputs CREATE TABLE statements in a format suitable for Octo, which then allows the journal file data identified by label to be queried through Octo using SQL.
If rectype is omitted, OCTODDL, outputs CREATE TABLE statements for all record types and all metadata, e.g.,

```
YDB>do OCTODDL^%YDBJNLF
DROP TABLE IF EXISTS YDBJNLF KEEPDATA;
CREATE TABLE YDBJNLF -- Metadata for ingested journal files
(label VARCHAR,
 jnlfilename VARCHAR, -- Journal file name
…
 openprocjpvtime INTEGER,
 PRIMARY KEY (label))
Delim ";"
Global "^%ydbJNLF";
…
DROP TABLE IF EXISTS YDBJNLFRECTYPE KEEPDATA;
CREATE TABLE YDBJNLFRECTYPE -- Table for record types
(label VARCHAR,
 rectype VARCHAR,
 tbl VARCHAR,
 PRIMARY KEY (label, rectype))
GLOBAL "^%ydbJNLFRECTBL";

YDB>
```

If `rectype` is one or more opcodes, OCTODDL outputs the CREATE TABLE statements for those record types, e.g.,

```
YDB>do OCTODDL^%YDBJNLF("SET,AIMG,KILL")
DROP TABLE IF EXISTS YDBJNLFTYPE1 KEEPDATA;
CREATE TABLE YDBJNLFTYPE1 -- FKILL,FSET,FZKILL,GKILL,GSET,GZKILL,KILL,SET,TKILL,TSET,TZKILL,TZTRIG,UKILL,USET,UZKILL,UZTRIG,ZKILL,ZTRIG
(label VARCHAR,
 offset INTEGER,
 recsize INTEGER,
 op VARCHAR,
 horologdate INTEGER,
 horologtime INTEGER,
 tnum INTEGER,
 chksum INTEGER,
 pid INTEGER,
 clntpid INTEGER,
 token_seq INTEGER,
 strm_num INTEGER,
 strm_seq INTEGER,
 updnum INTEGER,
 nodeflags INTEGER,
 gvname VARCHAR,
 gvref VARCHAR,
 nodeval VARCHAR,
 PRIMARY KEY (label, offset, recsize))
Delim "\"
Global "^%ydbJNLFTYPE1";
DROP TABLE IF EXISTS YDBJNLFTYPE5 KEEPDATA;
CREATE TABLE YDBJNLFTYPE5 -- AIMG,PBLK
(label VARCHAR,
 offset INTEGER,
 recsize INTEGER,
 op VARCHAR,
 horologdate INTEGER,
 horologtime INTEGER,
 tnum INTEGER,
 chksum INTEGER,
 pid INTEGER,
 clntpid INTEGER,
 blknum INTEGER,
 bsiz INTEGER,
 blkhdrtn INTEGER,
 ondskbver INTEGER,
 dsecmdline VARCHAR,
 PRIMARY KEY (label, offset, recsize))
Delim "\"
Global "^%ydbJNLFTYPE5";

YDB>
```

#### Purging Data

`PURGE^%YDBJNLF(label)` purges all ingested %YDBJNLF data with the specified `label`. If `label` is omitted, it purges all ingested %YDBJNLF data.

#### Operational

If a YDBJNLF region does not exist in the global directory, INGEST creates an [AutoDB](https://docs.yottadb.com/AdminOpsGuide/gde.html#no-au-todb) region that uses the MM access method, mapped to database file `%ydbjnlf.dat` in the same directory as the DEFAULT region (`$ydb_dir/$ydb_rel/g` if the global directory was created by sourcing `ydb_env_set`). Global variables of the form `^%ydbJNLF` with all case combinations of `JNLF` are mapped to the YDBJNLF region.

To avoid impacting response times in production environments, we recommend analyzing elsewhere the large journal files generated by production environments.

Although we have attempted to make INGEST as efficient as we can, ingesting large journal files is inherently not a fast operation and the time taken will be comparable to the time taken for the underlying MUPIP JOURNAL EXTRACT operation.

When ingesting journal files on systems where the corresponding database file is not present:

- If the database file does not use custom collation, set the [ydb_extract_nocol](https://docs.yottadb.com/AdminOpsGuide/basicops.html#ydb-extract-nocol) environment variable.
- If the database file does use custom collation, create an empty database file with the same filename as the original database, and with the same custom collation.

[[#922](https://gitlab.com/YottaDB/DB/YDB/-/issues/922)]

### Messages
The following messages are no longer is use:

- GVNEXTARG
- INVLNPAIRLIST
- STRUCTNOTALLOCD

These following messages are either modified or newly added:

#### YottaDB

**GVNUNSUPPORTED**, Global variable name xxxx not supported in yyyy call

Run Time Error: This indicates that the global variable xxxx was passed as a parameter to function yyyy where it is not meaningful, e.g., ^ABC passed as a parameter to ydb_delete_excl_s().

Action: Fix the application code bug.

**ISVSUBSCRIPTED**, ISV variable name xxxx specified with a non-zero subscript count of nnnn

Run Time Error: Intrinsic special variable xxxx was passed as a parameter to a Simple API function with nnnn subscripts. YottaDB has no subscripted intrinsic special variables.

Action: Fix the applicated code bug.

**ISVUNSUPPORTED**, ISV variable name xxxx not supported in yyyy call

Run Time Error: This indicates that the intrinsic special variable xxxx was passed as a parameter to function yyyy where it is not meaningful, e.g., $TRESTART passed as a parameter to ydb_delete_excl_s().

Action: Fix the application code bug.

**ZBRKCNTNEGATIVE**, Count xxx, of transits through a ZBREAK breakpoint before activating it, cannot be negative

Run Time Information: This indicates that a ZBREAK command specified a negative count of process transits through the breakpoint before the breakpoint action takes effect. xxx is the specified negative count.

Action: Specify a positive count. Note that a count of 0 is treated as if a count of 1 was specified (i.e. the breakpoint action takes effect the first time the breakpoint is reached).

#### From GT.M V6.3-012

**DBFILNOFULLWRT**, Disabling fullblock writes. iiii tttt: bbbb

MUPIP Warning: Indicates full block writes were not successfully enabled. iiii describes the issue, tttt describes the type and bbbb is a block size.

Action: Consider planning to choose a blocksize better aligned with the file system blocksize at the next opportunity.

#### From GT.M V6.3-013

**EXITSTATUS**, Unexpected process exit (xxxx), exit status aaaa -- called from module yyyy at line zzzz

Run Time Error: Indicates a non-zero exit status aaaa returned from a process started in the context of xxxx. The following are common values (other values are possible depending on the script called) and descriptions for the exit status: 1-"Catchall for general errors", 2-"Misuse of shell builtins", 126-"Command invoked cannot execute", 127-"Command not found", 128-"Invalid argument to exit" and 130-"Script terminated by Control-C".

Action: Use the exit status aaaa to adjust the script causing the unexpected exit.

**EXTRFMT**, Extract error: invalid record format - no records found.

MUPIP Error: This indicates that LOAD could not process the sequential output file because the record after the header is invalid.

Action: Verify the file has a valid format and actually contains records.

#### From GT.M V6.3-014

**BADCONNECTPARAM**, Error parsing or invalid parameter. [XXXX]

MUPIP Error: MUPIP produces this message when there in an error in any connection parameter specified with -CONNECTPARAMS. XXXX contain a brief description of the parameter and its a valid value range.

Action: Specify valid values for the -CONNECTPARAM parameter. Refer to the -CONNECTPARAM documentation in the Administration and Operations Guide for more information.

**BADPARAMCOUNT**, -CONNECTPARAMS accepts one to six parameter values

MUPIP Error: MUPIP produces this message when there are more than six parameters specified for -CONNECTPARAMS.

Action: Specify one to six parameters or omit -CONNECTPARAMS from the MUPIP REPLICATE -SOURCE -START command to use the default connection parameters.

**REPLALERT**, Source Server could not connect to replicating instance [XXXX] for [NNNN] seconds

MUPIP Warning: The Source Server records this warning message when the Source Server fails to establish a replication connection with the secondary instance [XXXX] for [NNNN] seconds. The frequency of recording this warning message can be adjusted with the soft connection attempt period (the fourth -CONNECTPARAM).

Action: Use the REPLALERT message as an mechanism to alert operations about replication network issues. Specify 0 as the REPLALERT period parameter (the fourth -CONNECTPARAM) to disable logging this message. The REPLALERT messages are disabled by default (that is, without specifying -CONNECTPARAM).

### Legal Stuff

Copyright © 2022 YottaDB LLC

Permission is granted to copy, distribute and/or modify this document under the terms of the GNU Free Documentation License, Version 1.3 or any later version published by the Free Software Foundation; with no Invariant Sections, no Front-Cover Texts and no Back-Cover Texts.

YottaDB® and Octo® are registered trademarks of YottaDB LLC. GT.M™ is a trademark of Fidelity National Information Services, Inc.
Other trademarks belong to their respective owners.

This document contains a description of YottaDB and the operating instructions pertaining to the various functions that comprise the software. This document does not contain any commitment of YottaDB LLC. YottaDB LLC believes the information in this publication is accurate as of its publication date; such information is subject to change without notice. YottaDB LLC is not responsible for any errors or defects.