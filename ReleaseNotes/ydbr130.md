<!---
.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2020-2025 YottaDB LLC and/or its subsidiaries.#
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

# YottaDB r1.30

## Release Note Revision History

| Revision  | Date | Summary               |
| --------- | ---- | --------------------- |
| 1.00      | August 14, 2020 | r1.30 Initial Release |
| 1.01      | May 23, 2021 | Change supported Debian release from Buster to Bullseye |
| 1.02      | May 8, 2025 | Change "smart" quotes to simple double quotes |

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

* For requests other than to the communities below, [post an Issue](https://gitlab.com/YottaDB/DB/YDB/issues) and include the words "Help Wanted" in the summary.
* For requests specific to the use of YottaDB from node.js via [nodem](https://github.com/dlwicksell/nodem), [post an Issue](https://github.com/dlwicksell/nodem/issues/new/).
* For access from [QewdJS](http://qewdjs.com/), or [EWD.js](https://github.com/robtweed/ewd.js), post to the [Enterprise Web Developer community](https://groups.google.com/forum/#!forum/enterprise-web-developer-community).
* For requests specific to the use of YottaDB with [VistA](https://en.wikipedia.org/wiki/VistA) flavors, post to the [Hardhats](https://groups.google.com/forum/#!forum/hardhats) list.
* For requests specific to the use of YottaDB with M other than for applications above, post to the [comp.lang.mumps](https://groups.google.com/forum/#!forum/comp.lang.mumps) list.

## r1.30

### Overview

YottaDB r1.30 is a major release with important new functionality that is required by the [Octo](https://gitlab.com/YottaDB/DBMS/YDBOcto) 1.0 production release and the production release of the [Rust wrapper](https://gitlab.com/YottaDB/Lang/YDBRust). While some releases add major new functionality, r1.30 is a major release by the sheer volume of new functionality.

The most important new functionality, required by Octo to distinguish between the empty string (`""`) and the absence of data (NULL), is the intrinsic special variable, $ZYSQLNULL, which can be used as a local variable subscript or node value. ([484](#x484))

Other highlights include:

* The `ydbinstall` / `ydbinstall.sh` scripts include `--enclugin`, `--octo`, and `--posix` command line options to install respectively, the encryption plugin, Octo, and the POSIX plugin. ([458](#x458)), ([457](#x457)),  ([521](#x521))
* The %YDBPROCSTUCKEXEC routine provides a standard action that can be invoked by `ydb_procstuckexec`. ([579](#x579))
* The call to $ZJOBEXAM() allows for specification of the data to dumped. So, for example, the dumping of local variables can be suppressed unless required, as local variables can contain confidential information. ([482](#x482))
* `yottadb -version` provides a detailed report on the YottaDB build. ([595](#x595))
* For faster numeric base conversion, $ZCONVERT() converts between decimal and hexadecimal. ([485](#x485))
* $ZYHASH() returns the 128-bit [MurmurHash3](https://en.wikipedia.org/wiki/MurmurHash#MurmurHash3) hash of a string. ([390](#x390))
* Simple API functions use nanosecond timers internally. ([388](#x388))
* A new implementation of $RANDOM(). ([384](#x384))
* To facilitate migration to YottaDB from big endian versions of the upstream code base, YottaDB can automatically convert global directories from big to little endian formats. Note that all YottaDB platforms are little endian. ([524](#x524))
* [CentOS 8](https://wiki.centos.org/Manuals/ReleaseNotes/CentOS8.1911) and [Ubuntu 20.04 LTS](http://www.releases.ubuntu.com/20.04/) on x86_64 are Supported platforms with their own binary distributions.

YottadB r1.30 is upward compatible with [YottaDB r1.28](https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.28), as well as with [GT.M V6.3-008](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-008_Release_Notes.html).

There are significant internal changes to signal handling to make the [Go wrapper](https://gitlab.com/YottaDB/Lang/YDBGo) more friendly to application developers, but access to YottaDB from other languages is unaltered.

As with any YottaDB release, there are numerous other enhancements and fixes, some specific to YottaDB (including fixes to issues discovered in the upstream code base), and others inherited from the upstream [GT.M V6.3-008](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-008_Release_Notes.html). See the [Change History](#change-history).

### Platforms

A platform is a combination of a CPU architecture and an operating system. A platform is Supported, Supportable, or Unsupported.

* Supported means that we have the platform in our development environment and test each release on that platform.
* Supportable means that although we do not necessarily have such a platform in our environment, we have no reason to believe that the software will not run on it.
* All others are Unsupported.

| CPU Architecture                    | Supported OS Version(s)                                                                           | Notes                                                                                                                         |
| ----------------------------------- | ------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------- |
| 64-bit x86                          | Ubuntu 18.04 & 20.04 LTS; Red Hat Enterprise Linux/CentOS 7.x & 8.x; Debian GNU/Linux 11 (Bullseye) | There are separate binary distributions for each OS version, owing to differences in library versions of those distributions. |
| 64-bit ARM (Raspberry Pi 3 Model B) | Ubuntu 18.04 LTS                                                                                  | See below.                                                                                                                    |
| 32-bit ARM (Raspberry Pi 3 Model B) | Raspbian GNU/Linux 10 (Buster)                                                                    | See below.                                                                                                                    |
| 32-bit ARM (Raspberry Pi Zero)      | Raspbian GNU/Linux 10 (Buster)                                                                    | See below.                                                                                                                    |

Recent releases of major GNU/Linux distributions with contemporary kernels, glibc and ncurses are Supportable. Specific notes:

- For YottaDB releases after January 1, 2021, we intend to drop support for Ubuntu 18.04 LTS on x86_64. We have not made a decision about 64-bit ARM at this time.
- On [Arch Linux](https://www.archlinux.org/) and other leading edge distributions, YottaDB may need to be recompiled from source code owing to library and tool chain versions significantly newer than those used in building the distribution.
- While YottaDB is Supportable on other ARMv6, [ARMv7-A](https://en.wikipedia.org/wiki/Comparison_of_ARMv7-A_cores), and [ARMv8-A](https://en.wikipedia.org/wiki/Comparison_of_ARMv8-A_cores) CPUs, owing to variations in the implementations of ARM microarchitectures, we recommend that you ensure the software runs correctly before committing to any specific hardware other than those Supported. Please contact info@yottadb.com if you want a specific combination of OS and CPU microarchitecture to be Supported.
- Owing to Go language versions required by the YottaDB Go wrapper, the supported Debian release is Bullseye (10) which is currently (May 23, 2021) Testing. `ydbinstall` / `ydbinstall.sh` requires `--force-install` to install on Debian Bullseye.

### Installation

See our [Get Started page](https://yottadb.com/product/get-started) to use YottaDB.

We **strongly recommend** that you install YottaDB r1.30 in a newly created directory, different from those of prior YottaDB releases and any GT.M versions you may have installed on the system.

### Removing an installed YottaDB release

Assuming `$ydb_dist` points to the directory where YottaDB is installed:

* Cleanly shut down all application processes using that release.
* Execute `mupip rundown && mupip rundown -relinkctl`
* Ensure that there are no gtcm\* or gtmsecshr processes active.
* Use `sudo lsof | grep $ydb_dist` to ensure there are no open files.
* Delete the directory with `sudo rm -rf $ydb_dist`

## Upgrading to YottaDB r1.30

As YottaDB r1.30 is upward compatible from YottaDB r1.28 and GT.M V6.3-008, the minimal upgrade steps are:

* Install YottaDB r1.30.
* Install plugins you use. Note that the `ydbinstall` / `ydbinstall.sh` shell scripts can install the encryption and POSIX plugins after installing YottaDB. ([457](#x457)) and ([458](#x458))
* Recompile object code, and recreate shared libraries where appropriate.
* If you are using encryption, compile and install the reference implementation plugin (if not done by the `ydbinstall` / `ydbinstall.sh` script) or your customized plugin.
* Cleanly shut down the application and ensure that the database files are shut down using MUPIP RUNDOWN from the prior release.
* Switch journal files with the new YottaDB release.
* Start using the new YottaDB release.

If the database has triggers defined with non-canonical numbers, or numbers specified as strings with any version prior to r1.28, or if you are unsure, extract the trigger definitions, delete existing triggers, and reload the trigger definitions. Issue [#430] from r1.28 has a series of steps you can copy and execute. There is no need to do this if upgrading from r1.28.

To upgrade from older GT.M releases, first upgrade to GT.M V6.0-000 or later and follow the steps above, or contact your YottaDB support channel for assistance and guidance.

A more sophisticated upgrade technique is:

* Install YottaDB r1.30.
* Create a new replicated instance of your application (on the same system or a different system).
* Assuming the existing instance is A, and the new instance is B, upgrade B to r1.30 and start replicating from A to B.
* Once B catches up, switchover so that B is in a primary role replicating to A.
* Once you are satisfied with B, remove (or upgrade) A.

## Change History

### r1.30

YottaDB r1.30 includes the following enhancements and fixes beyond [YottaDB r1.28](https://github.com/YottaDB/YottaDB/releases/tag/r1.28).

| ID             | Category                            | Summary                                                                                                                                               |
| -------------- | ----------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------- |
| ([384](#x384)) | Languages                           | Reimplement $RANDOM()                                                                                                                                 |
| ([388](#x388)) | Other                               | Nanosecond resolution timer for Simple API times                                                                                                      |
| ([390](#x390)) | Languages                           | $ZYHASH(string\[,salt\]) function                                                                                                                     |
| ([457](#x457)) | Other                               | `ydbinstall` option to install POSIX plugin after installing YottaDB                                                                                  |
| ([458](#x458)) | Other                               | `ydbinstall` option to install encryption plugin after installing YottaDB                                                                             |
| ([470](#x470)) | Other                               | $ydb\_dist copied to gtm\_dist during initialization                                                                                                  |
| ([476](#x476)) | Languages                           | $ZSIGPROC() accepts signal names                                                                                                                      |
| ([482](#x482)) | Languages                           | Control contents of process dump generated by $ZJOBEXAM()                                                                                             |
| ([484](#x484)) | Languages                           | $ZYSQLNULL intrinsic special variable and $ZYISSQLNULL() function                                                                                     |
| ([485](#x485)) | Languages                           | $ZCONVERT() converts integers between decimal and hexadecimal                                                                                         |
| ([488](#x488)) | Languages                           | Environment translation for extended references; global directory translation                                                                         |
| ([492](#x492)) | Languages                           | UTF-8 mode $TRANSLATE() with multi-byte string literals as the second or third parameter works correctly when executed from shared library            |
| ([493](#x493)) | System Administration               | More informative default first line from MUPIP EXTRACT in GO and ZWR formats                                                                          |
| ([494](#x494)) | System Administration               | Spaces in labels in GO and ZWR format MUPIP EXTRACT                                                                                                   |
| ([495](#x495)) | Other                               | Option to recompile only if .m file is newer than corresponding .o file                                                                               |
| ([496](#x496)) | Other                               | Object code files linkable into shared libraries by ld version 2.33 and later                                                                         |
| ([503](#x503)) | Database                            | DRD not incremented for MM access method                                                                                                              |
| ([504](#x504)) | Database                            | On ARMv6 and ARMv7-A platforms, syslog messages recording database update failures work correctly                                                     |
| ([511](#x511)) | Languages                           | $\[Z\]TRANSLATE() correctly reports error when second and third arguments are undefined                                                               |
| ([513](#x513)) | Languages                           | $VIEW("REGION","^*") returns name of default region                                                                                                   |
| ([515](#x515)) | Languages                           | ydb\_zstatus() has a return value                                                                                                                     |
| ([518](#x518)) | Languages                           | Support for 64-bit integer types on 64-bit platforms when calling in to M                                                                             |
| ([519](#x519)) | Languages                           | SOCKET device timeouts happen at specified times                                                                                                      |
| ([520](#x520)) | Languages                           | Repeatedly switching between $ZTRAP and $ETRAP works correctly                                                                                        |
| ([521](#x521)) | Other                               | `ydbinstall` script option to install Octo as a YottaDB plugin                                                                                        |
| ([523](#x523)) | Languages                           | IF '@ works when operand is a constant or glvn evaluating to a constant                                                                               |
| ([524](#x524)) | Database                            | Auto convert global directory to endianness of current architecture                                                                                   |
| ([525](#x525)) | Languages                           | SILENT^%RSEL restores value of $IO                                                                                                                    |
| ([532](#x532)) | Other                               | `ydbinstall` with `--encplugin --utf8 default` installs encryption plugin                                                                             |
| ([534](#x534)) | Languages                           | Process-terminating signals handled correctly in Simple API multi-threaded functions                                                                  |
| ([545](#x545)) | Languages                           | LOCK command works correctly after a prior LOCK command interrupted by an error                                                                       |
| ([547](#x547)) | Languages                           | Extrinsic function calls where the caller and label are hundreds of thousands or more lines apart work correctly on 32-bit ARMv7 CPUs                 |
| ([549](#x549)) | Languages                           | Environment variables are expanded in $ZROUTINES                                                                                                      |
| ([550](#x550)) | Languages                           | On successful commit of outer transactions, only updates of inner transactions that return status other than YDB_OK or YDB_TP_RESTART are rolled back |
| ([553](#x553)) | Languages                           | Boolean expressions involving the NOT operator (') and side effects compile when ydb\_boolean=1                                                       |
| ([554](#x554)) | Languages                           | Boolean expressions with side effects compile when used in a context requiring an integer and ydb\_boolean or ydb\_side\_effects are 1                |
| ([560](#x560)) | Languages                           | Processes accessing YottaDB terminate at a safe point if they are sent a termination signal                                                           |
| ([562](#x562)) | Languages                           | Setting $ZROUTINES to "..."  or "...." issues a ZROSYNTAX error                                                                                       |
| ([566](#x566)) | Languages                           | Comments in Call-In and External call tables                                                                                                          |
| ([567](#x567)) | Languages                           | HANG 0 interrupted by MUPIP INTRPT returns forthwith                                                                                                  |
| ([568](#x568)) | System Administration               | MUPIP commands that output to a pipe leave terminal settings untouched                                                                                |
| ([569](#x569)) | System Administration               | MUPIP LOAD option to ignore mode of MUPIP EXTRACT that created ZWR extract                                                                            |
| ([576](#x576)) | Languages                           | $PIECE() in a database trigger returns correct results when invoked from SimpleAPI                                                                    |
| ([577](#x577)) | Languages                           | Normal termination on receipt of SIGTERM when inside a transaction                                                                                    |
| ([578](#x578)) | Languages                           | $ZSTATUS for LVUNDEF error quotes string subscripts                                                                                                   |
| ([579](#x579)) | System Administration               | Standard action for $ydb_procstuckexec to invoke                                                                                                      |
| ([580](#x580)) | Languages                           | `timer_id` parameter of `ydb_timer_cancel()`, `ydb_timer_cancel_t()`, and `ydb_timer_start_t()` is type `intptr_t`                                    |
| ([583](#x583)) | Languages                           | CITABOPN error details why `fopen()` call failed                                                                                                      |
| ([584](#x584)) | Languages                           | $ZSTATUS after OPEN command errors identifies the file name                                                                                           |
| ([586](#x586)) | Database                            | No SYSCALL error in rare cases when MUPIP STOP sent to YottaDB process                                                                                |
| ([587](#x587)) | Languages                           | Environment variable ydb_dollartest provides an initial value of $TEST                                                                                |
| ([589](#x589)) | Languages                           | Buffered IO writes from external calls are flushed by YottaDB when it closes IO devices                                                               |
| ([590](#x590)) | Languages                           | Identify source checkin commit hash in $ZRELDATE                                                                                                      |
| ([592](#x592)) | System Administration               | JOB'd processes have same name as parent                                                                                                              |
| ([594](#x594)) | Languages                           | Robust results from `ydb_incr_s()`/`ydb_incr_st()` and language wrapper APIs                                                                          |
| ([595](#x595)) | Other                               | `yottadb -version` provides a detailed report on the YottaDB build                                                                                    |
| ([596](#x596)) | Database                            | $ZTDELIM returns piece separator for KILL and ZKILL triggers                                                                                          |
| ([597](#x597)) | Other                               | `ydbinstall` saves plugin build directories when building a plugin fails                                                                              |
| ([607](#x607)) | Database                            | Upgrading a database from r1.22 or r1.24 upgrades the `flush_trigger_top` field                                                                       |
| ([609](#x609)) | Database                            | MUPIP STOP can cause DBMBPINCFL or DBMBMINCFRE errors in database files of frozen instances                                                           |
| ([617](#x617)) | Database                            | MUPIP REORG UPGRADE/DOWNGRADE completes successfully even when concurrent processes terminated with SIGKILL                                           |
| ([619](#x619)) | Languages                           | ydb_ci*() functions return YDB_TP_RESTART to RESTART an enclosing transaction                                                                         |

### GT.M V6.3-008

YottaDB r1.30 incorporates enhancements and fixes from [GT.M V6.3-008](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-008_Release_Notes.html). In some cases, fixes from the GT.M release notes are not shown here because those issues may have been identified and already been fixed in earlier releases of YottaDB, or they were independently fixed by the YottaDB team (for example, ([511](#x511)) is a more complete fix than [(GTM-9093)](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-008_Release_Notes.html#GTM-9093), the latter being only a partial fix for the same issue).

| ID                      | Category              | Summary                                                                                             |
| ----------------------- | --------------------- | --------------------------------------------------------------------------------------------------- |
| ([GTM-8911](#GTM-8911)) | Other                 | Defer allocation of memory for TP processing until the region participcates in a TP transaction     |
| ([GTM-9000](#GTM-9000)) | Other                 | ^%PEEKBYNAME accepts an optional argument to specify a global directory                             |
| ([GTM-9012](#GTM-9012)) | Database              | Processes automatically release online freeze only if they do not find enough global buffers.       |
| ([GTM-9067](#GTM-9067)) | Database              | Avoid WCBLOCKED caused by delayed I/O                                                               |
| ([GTM-9077](#GTM-9077)) | Languages             | SOCKET Devices properly report TLS errors                                                           |
| ([GTM-9078](#GTM-9078)) | Database              | Avoid potential deadlock caused by delay I/O with Instance Freeze                                   |
| ([GTM-9079](#GTM-9079)) | Languages             | Prevent inappropriate error status from compile of certain literal arguments to an XECUTE           |
| ([GTM-9082](#GTM-9082)) | Other                 | Fix to auto-upgrade for database file header error in V6.3-007                                      |
| ([GTM-9083](#GTM-9083)) | Database              | \[NON]TPRESTART messages present numeric codes with a hexadecimal representation                    |
| ([GTM-9084](#GTM-9084)) | System Administration | TLS reference implementation partially supports OpenSSL 1.1.1                                       |
| ([GTM-9089](#GTM-9089)) | Database              | NULSUBSC error provides more accurate context                                                       |
| ([GTM-9092](#GTM-9092)) | Other                 | IN^%YGBLSTAT function checks for process connection to GT.M; also an enhancement and a fix          |
| ([GTM-9097](#GTM-9097)) | Other                 | Prevent Source Server hang when processing corrupted journal files                                  |
| ([GTM-9098](#GTM-9098)) | System Administration | MUPIP JOURNAL reports the correct numbers when issuing a continuity check failure                   |
| ([GTM-9099](#GTM-9099)) | Languages             | The compiler warns of argumentless DOs without a valid block                                        |
| ([GTM-9100](#GTM-9100)) | Languages             | Resume from Out-Of-Band Operations during XECUTEd indirect extrinsics nesting interruptable actions |
| ([GTM-9110](#GTM-9110)) | Other                 | limit GT.M commands from the shell to 32KiB                                                         |

### Database

* <a name="x503"></a>The DRD count stored in the database fileheader, and reported by `ZSHOW "G"` and `$VIEW("GVSTAT",<region>)` is not incremented for database accesses when using the MM access method. Previously, it could be incremented, even though DRD is not meaningful for the MM access method. [YDB#503](https://gitlab.com/YottaDB/DB/YDB/-/issues/503)

* <a name="x504"></a>Syslog messages recording failures in certain database update operations work correctly. Previously such failures could cause segmentation violations (SIGSEGV). This issue was restricted to ARMv6 and ARMv7-A platforms and was only observed in the development environment; it was never reported by a user. [YDB#504](https://gitlab.com/YottaDB/DB/YDB/-/issues/504)

* <a name="x524"></a>When GDE opens a global directory file, it converts the global directory file to the endianness of the current architecture. Previously, this would result in an ZGBLDIRACC error. After conversion, if the global directory needs upgrading to the current release of YottaDB, existing logic performs such a conversion. Although all YottaDB platforms are little endian, this facilitates upgrading to YottaDB from versions of the upstream software on big endian platforms. [YDB#524](https://gitlab.com/YottaDB/DB/YDB/-/issues/524)

* <a name="x586"></a>MUPIP STOP (SIGTERM/SIG-15 signal) of a YottaDB process that has updated one or more database files works correctly. Previously it was possible in very rare cases for the process to terminate with a SYSCALL error. [YDB#586](https://gitlab.com/YottaDB/DB/YDB/-/issues/586)

* <a name="x596"></a>Within a KILL or ZKILL trigger the intrinsic special variable $ZTDELIM returns the piece separator (specified by `-delim` in the trigger definition). Previously, $ZTDELIM was maintained only for SET triggers. [YDB#596](https://gitlab.com/YottaDB/DB/YDB/-/issues/596)

* <a name="x607"></a>The `flush_trigger_top` field is set correctly when a database is auto-upgraded from YottaDB r1.22 or r1.24. [YDB#607](https://gitlab.com/YottaDB/DB/YDB/-/issues/607)

* <a name="x609"></a>MUPIP STOP (i.e. SIGTERM / kill -15) of a YottaDB process while it has a database file open that is part of a [frozen instance](https://docs.yottadb.net/AdminOpsGuide/dbrepl.html#instance-freeze) leaves the database in a clean state. Previously, it was possible in rare cases for such database files to get [DBMBPINCFL](https://docs.yottadb.net/MessageRecovery/errors.html#dbmbpincfl) or [DBMBMINCFRE](https://docs.yottadb.net/MessageRecovery/errors.html#dbmbmincfre) structural integrity errors. [YDB#609](https://gitlab.com/YottaDB/DB/YDB/-/issues/609)

* <a name="x617"></a>MUPIP REORG UPGRADE/DOWNGRADE completes the requested operation successfully and returns a success return code even when a concurrent process accessing the database is terminated with SIGKILL (kill -9). Previously it could occasionally issue a `%YDB-E-BUFFLUFAILED` message and return with a non-zero exit status. Note that:

  - YottaDB strongly recommends against using SIGKILL to terminate processes that have open database files.
  - MUPIP REORG UPGRADE/DOWNGRADE only applies when migrating databases to YottaDB from very old versions of the upstream code base, or from YottaDB to those very old versions of the upstream code base. Through YottaDB r1.30, no database created in any prior release of YottaDB needs to be upgraded.

  [YDB#617](https://gitlab.com/YottaDB/DB/YDB/-/issues/617)

* <a name="GTM-9012"></a>When initiating a MUPIP FREEZE -ONLINE -AUTORELEASE, processes release the freeze if they cannot find global buffers to do their work. Previously, they occasionally released the freeze prematurely. [(GTM-9012)](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-008_Release_Notes.html#GTM-9012)

* <a name="GTM-9067"></a>Processes handle very long delays in database and journal file writes appropriately. Previously, they could incorrectly issue a WCBLOCKED message in such cases, resulting a variety of incorrect behavior, including a segmentation violation (SIG-11). [(GTM-9067)](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-008_Release_Notes.html#GTM-9067)

* <a name="GTM-9078"></a>Processes avoid a possible deadlock between a database critical resource and a database buffer flush wait caused by an extended period of significantly delayed I/O when using an Instance Freeze for certain journaling conditions. [(GTM-9078)](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-008_Release_Notes.html#GTM-9078)

* <a name="GTM-9083"></a>TPRESTART and NONTPRESTART messages report non-graphic codes as "0xnn". The numeric codes are associated with restarts caused by implementation details, rather than database conflicts and are normally infrequent. Previously they reported these as octal values or as periods (.) depending on the configuration of the device presenting the report. [(GTM-9083)](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-008_Release_Notes.html#GTM-9083)

* <a name="GTM-9089"></a>The NULSUBSC error now provides more accurate context; previously, it always indicated an update. [(GTM-9089)](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-008_Release_Notes.html#GTM-9089)

### Languages

* <a name="x384"></a>The underlying implementation of $RANDOM() has been changed with no change to the API. Note that $RANDOM() should never be used when cryptographic quality random numbers are needed. [YDB#384](https://gitlab.com/YottaDB/DB/YDB/-/issues/384)

* <a name="x390"></a>The M function `$ZYHASH(string[,salt])` returns the 128-bit MurmurHash3 of `string` as a hexadecimal string prefixed with `"0x"`. For `salt` values of 0 (the default) through 2,147,483,647 (2**31-1) this is equivalent to calling the C API function `ydb_mmrhash128()` and passing its return value to `ydb_mmrhash128_hex()`. [YDB#390](https://gitlab.com/YottaDB/DB/YDB/-/issues/390)

* <a name="x476"></a>In $ZSIGPROC(expr1,expr2), expr2 can optionally be a signal name, such as `"USR1"`, optionally prefixed by `"SIG"`, e.g., `"SIGUSR1"`. The strings are case-insensitive. Previously $ZSIGPROC() required numeric signal values. [YDB#476](https://gitlab.com/YottaDB/DB/YDB/-/issues/476)

* <a name="x482"></a>Defaulting to `"*"`, $ZJOBEXAM() takes a second parameter whose string value specifies the ZSHOW output codes as a string to be included in the process dump. For example, $ZJOBEXAM("isvsonly.txt","I") dumps only the intrinsic special variables, and nothing else, to the output file. Previously, the $ZJOBEXAM() process dump was always equivalent to `ZSHOW "*"`. [YDB#482](https://gitlab.com/YottaDB/DB/YDB/-/issues/482)

* <a name="x484"></a>The read-only $\[ZYSQ\]LNULL intrinsic special variable is conceptually equivalent to a logical value of unknown, and can be assigned as the value or used as a subscript of a local variable.

  When $ZYSQLNULL is an operand, the results are as follows:

  | Operator(s)                             | Result                                                                                              |
  | --------------------------------------- | --------------------------------------------------------------------------------------------------- |
  | Binary `!`                              | If the other operand evaluates to true (1), the result is true, otherwise the result is $ZYSQLNULL  |
  | Binary `&`                              | If the other operand evalutes to false (0), the result is false, otherwise the result is $ZYSQLNULL |
  | All binary operators except `!` and `&` | Regardless of the value of the other operand, the result is $ZYSQLNULL                              |
  | Unary `+`                               | $ZYSQLNULL                                                                                          |
  | Unary `-`                               | $ZYSQLNULL                                                                                          |
  | Unary `'`                               | $ZYSQLNULL                                                                                          |

  $TEST continues to have only 2 values : false (0) and true (1). An IF statement whose condition evaluates to $ZYSQLNULL sets $TEST to 0 and does not execute the rest the line. Commands with postconditionals that evaluate to $ZYSQLNULL do not execute the command.

  ZSHOW and ZWRITE of $ZYSQLNULL values show a value of $ZYSQLNULL. WRITE does not show any value for $ZYSQLNULL just as it does with `""`.

  $ZYSQLNULL can be a subscript of a local variable. In that case, it collates after all other subscripts, i.e., $ORDER() and $QUERY() return that subscript at the very end.

  The function $ZYISSQLNULL() returns 1 if its sole argument has a value $ZYSQLNULL, and 0 otherwise.

  Using $ZYSQLNULL as a subscript or assigning it as the value of a global variable (including implicitly with a MERGE), using it as a subscript in a LOCK/ZALLOCATE/ZDEALLOCATE command, or in a context that expects an integer or a numeric value raises the `ZYSQLNULLNOTVALID` error.
  Other than usage as an operand as discussed above, $ZYSQLNULL in a context that expects a string, e.g. `$ASCII($ZYSQLNULL,1)`, is treated like the empty string  `""`. [YDB#484](https://gitlab.com/YottaDB/DB/YDB/-/issues/484)

* <a name="x485"></a>For fast numeric conversion between decimal and hexadecimal, the second and third parameters of $ZCONVERT() can be case-insensitive string values `"HEX"` or `"DEC"`, the second parameter being the base of the string or number to be converted and the third the base of the output.

  - Unsigned numbers in the range 0 through 0xFFFFFFFFFFFFFFFF can be converted. Decimal return values greater than 999999999999999999 (YottaDB's maximum numeric size) are returned as strings.
  - Hexadecimal numbers are always converted to positive decimal numbers.
  - As conversion from hexadecimal numbers preceded by `"-"` to decimal is not considered meaningful, if the number to be converted is a "negative" hexadecimal number (e.g., `"-F"`), the result is 0.
  - Conversion from negative decimal numbers to hexadecimal returns the hexadecimal value of the 2's complement of the number, e.g., the value of `$ZCONVERT(-23,"DEC","HEX")` is `"E9"`.

  The ^%DH() and ^%HD() utility programs use fast $ZCONVERT() conversion when the values supplied are in the range that $ZCONVERT() supports; they otherwise revert to existing conversion logic.

  $ZCONVERT() raises the INVCONVERT error for unsupported conversions (e.g., a Unicode conversion in M mode, or converting from DEC to DEC or HEX to HEX) and the INVVALUE error when the input number is outside the supported range. [YDB#485](https://gitlab.com/YottaDB/DB/YDB/-/issues/485)

* <a name="x488"></a>The environment variable `ydb_env_translate` can identify a shared library containing a routine to provide an optional [environment translation for extended references](https://docs.yottadb.com/ProgrammersGuide/langfeat.html#optional-yottadb-environment-translation-facility). If `$ydb_env_translate` is undefined, YottaDB can use a shared library identified by `$gtm_env_translate`; defaulting to no environment translation. Previously, YottaDB only sought a value of the `gtm_env_translate` environment variable.

  Global directories defined by `$zgbldir` can also be similarly translated. When `$zgbldir` is set, if the environment variable `ydb_gbldir_translate` identifies a shared library with the entry point `ydb_gbldir_xlate()`, the global directory used is the value assigned to `$zgbldir` as translated by the routine. `ydb_gbldir_xlate()` has the same signature as the `ydb_env_xlate()` routine used for environment translation:

  ```C
  int ydb_gbldir_xlate(ydb_string_t *in1, ydb_string_t *in2, ydb_string_t *in3, ydb_string_t *out);
  ```

  where:
  * `in1` references the value being assigned to `$zgbldir`.
  * `in2` is the NULL string - the parameter exists only so that the signature matches that of `ydb_env_translate()`.
  * `in3` references `$zdirectory` the current directory of the process.
  * `out` is a return value that references the actual global directory file to be used.

  A return value other than zero (0) indicates an error in translation, and is reported as a YottaDB error.

  We thank Thomas Morstein (@ztmr) for working with us on this enhancement. [YDB#488](https://gitlab.com/YottaDB/DB/YDB/-/issues/488)

* <a name="x492"></a>When executed from a routine in a shared library, $TRANSLATE() in UTF-8 mode works correctly when the second or third arguments are multi-byte literals. In YottaDB r1.26 and r1.28, this could result in abnormal process termination with a segmentation violation (SIGSEGV). [YDB#492](https://gitlab.com/YottaDB/DB/YDB/-/issues/492)

* <a name="x511"></a>A first call to $\[Z\]TRANSLATE() where the first argument is defined but the second and third are undefined results in an error. Previously, this caused abnormal process termination with a segmentation fault (SIG-11) in UTF-8 mode, and no output in M mode. [YDB#511](https://gitlab.com/YottaDB/DB/YDB/-/issues/511)

* <a name="x513"></a>`$VIEW("REGION","^*")` returns the name of the region in the global directory to which the `*` namespace maps (as seen in a `$ydb_dist/yottadb -run GDE SHOW -NAME` output). Previously there was no easy way to determine the default region name. [YDB#513](https://gitlab.com/YottaDB/DB/YDB/-/issues/513)

* <a name="x515"></a>`ydb_zstatus()` has an `int` return value with a value of YDB_ERR_INVSTRLEN if the buffer supplied is not large enough to hold the message and YDB_OK otherwise. As with other YDB_ERR_INVSTRLEN return codes, `ydb_zstatus()` copies what can be copied to the buffer (including a null terminator byte) if the length is non-zero. Previously, `ydb_zstatus()` would attempt to copy the entire message to the buffer even if the `buf_len` parameter was zero. [YDB#515](https://gitlab.com/YottaDB/DB/YDB/-/issues/515)

* <a name="x518"></a>On 64-bit platforms, `ydb_ci()`,  `ydb_cip()`, `ydb_ci_t()`, and `ydb_cip_t()` support `ydb_int64_t` and `ydb_uint64_t` data types. These 64-bit types are [not yet supported on 32-bit platforms](https://gitlab.com/YottaDB/DB/YDB/-/issues/544). [YDB#518](https://gitlab.com/YottaDB/DB/YDB/-/issues/518)

* <a name="x519"></a>Opening a SOCKET device with a timeout times out as specified. Previously the timeout could happen too early or much later than requested. Note that while system load and overhead can always cause a timeout to happen later (which was not the case here), a timeout should never happen sooner. [YDB#519](https://gitlab.com/YottaDB/DB/YDB/-/issues/519)

* <a name="x520"></a>Switching error trapping back and forth by alternating assignments to $ETRAP and $ZTRAP works correctly. Previously YottaDB could lose the error trap assignment, resulting in an empty error trap. [YDB#520](https://gitlab.com/YottaDB/DB/YDB/-/issues/520)

* <a name="x523"></a>`IF '@` syntax works when the operand is a constant or a glvn that evaluates to a constant (e.g. `IF '@"0"`, `IF '@x`). Previously this issued a VAREXPECTED error. [YDB#523](https://gitlab.com/YottaDB/DB/YDB/-/issues/523)

* <a name="x525"></a>`SILENT^%RSEL` returns with the current M IO device (`$IO`) restored to what it was at the time of the call. Previously, it would set it to $PRINCIPAL. [YDB#525](https://gitlab.com/YottaDB/DB/YDB/-/issues/525)

* <a name="x534"></a>Signals that terminate a YottaDB process are handled correctly in multi-threaded functions of the Simple API. For example, invoking the `abort()` function (which raises a SIGABRT signal) inside a TP callback function correctly generates a core file and terminates the process immediately. Previously such signals could cause the YottaDB process to hang. [YDB#534](https://gitlab.com/YottaDB/DB/YDB/-/issues/534)

* <a name="x545"></a>A LOCK command after a prior LOCK command interrupted by an error works correctly. Previously, it would result in a BADLOCKNEST error. [YDB#545](https://gitlab.com/YottaDB/DB/YDB/-/issues/545)

* <a name="x547"></a>Extrinsic function calls where the calling M line is hundreds of thousands or more M lines away from the called M line work correctly on the ARMv7 architecture (32-bit ARM). Previously, the process would terminate abnormally with a SYSTEM-E-UNKNOWN and/or SIG-11 error. We thank Steve Johnson (@sljohnson1) for this fix. [YDB#547](https://gitlab.com/YottaDB/DB/YDB/-/issues/547)

* <a name="x549"></a>Any environment variables in a string used to set `$ZROUTINES`. either in an environment variable at process startup or explicitly during execution, are expanded into absolute path names, eliminating the need for application code using `$ZROUTINES` to do the expansion. Previously, environment variables were not expanded. [YDB#549](https://gitlab.com/YottaDB/DB/YDB/-/issues/549)

* <a name="x550"></a>In applications using the C API (and other language APIs that wrap the C API), updates from outer transactions that invoke nested inner transactions which return YDB\_TP\_ROLLBACK are committed upon a successful transaction commit; updates of inner transactions whose callback functions return any status to `ydb_tp_s()` / `ydb_tp_st()` other than YDB\_OK or YDB\_TP\_RESTART are rolled back. Previously, updates from the enclosing transaction were also rolled back, whereas only the updates of the inner transactions should have been rolled back. The workaround was to return YDB\_TP\_ROLLBACK through all transaction levels to the top application code level outside a transaction, and for that code to re-invoke YottaDB transaction logic. [YDB#550](https://gitlab.com/YottaDB/DB/YDB/-/issues/550)

* <a name="x553"></a>Boolean expressions involving the NOT operator (`'`) and side effects (`$increment()`, extrinsic function calls, etc.) compile when the `ydb_boolean` or `ydb_side_effects` environment variables are set to 1. Previously, the compilation would hang (loop for ever). [YDB#553](https://gitlab.com/YottaDB/DB/YDB/-/issues/553)

* <a name="x554"></a>Boolean expressions with side effects compile when used in a context that requires an integer (e.g. as an argument to `$RANDOM()`) and when the `ydb_boolean` or `ydb_side_effects` environment variables are set to 1. Previously terminated the process with a fatal GTMASSERT2 error. [YDB#554](https://gitlab.com/YottaDB/DB/YDB/-/issues/554)

* <a name="x560"></a>Processes accessing YottaDB terminate at a safe point if they are sent a termination signal (e.g. MUPIP STOP/SIGTERM/SIG-15, Ctrl-C/SIGINT/SIG-2, SIGALRM/SIG-14, etc.). Previously, it was possible in very rare cases for such processes to hang.

  C application code calling system functions (e.g, `accept()`) that can return EINTR should call the `ydb_eintr_handler()` / `ydb_eintr_handler_t()` if the system function returns EINTR.

  [YDB#560](https://gitlab.com/YottaDB/DB/YDB/-/issues/560)

* <a name="x562"></a>Setting `$ZROUTINES` to `"..."` or `"...."` either with a SET command or from `$ydb_routines` or `$gtmroutines` at process startup issues a ZROSYNTAX error. Previously those meaningless directory specifications were accepted. [YDB#562](https://gitlab.com/YottaDB/DB/YDB/-/issues/562)

* <a name="x566"></a>Call-in tables for calling M routines from other languages, and external-call tables to call out from M code to non-M routines support comments. YottaDB ignores text from a double slash (`//`) on a line to the end of the line. Previously, there was no support for comments in these tables. [YDB#566](https://gitlab.com/YottaDB/DB/YDB/-/issues/566)

* <a name="x567"></a>A HANG 0 that is interrupted by a MUPIP INTRPT returns forthwith. Previously, in very rare cases, this could hang for an arbitrarily long time. This was only observed in the development / test environment, and that too only on a DBG build, and was never reported by a user. It is documented here as it is theoretically possible for it to happen in production builds. [YDB#567](https://gitlab.com/YottaDB/DB/YDB/-/issues/567)

* <a name="x578"></a>$ZSTATUS for an LVUNDEF error message quotes string subscripts. Previously, it did not quote string subscripts. [YDB#578](https://gitlab.com/YottaDB/DB/YDB/-/issues/578)

* <a name="x576"></a>$PIECE() in trigger logic invoked by `ydb_set_s()`/`ydb_set_st()`/`ydb_incr_s()`/`ydb_incr_st()` works correctly. Previously, such a $PIECE() invocation could return incorrect results. This was observed in the development / test environment and was never reported by a user. [YDB#576](https://gitlab.com/YottaDB/DB/YDB/-/issues/)576

* <a name="x577"></a>A YottaDB process that does transaction processing (using TSTART / TCOMMIT in M, `ydb_tp_s()`/`ydb_tp_st()` in C and languages that use the C API, etc.) terminates normally when it receives a SIGTERM signal (SIG-15 or MUPIP STOP). Previously it could in rare case terminate with a TPLOCK error. This was only observed in in development / test environments, and was never reported by a user. [YDB#577](https://gitlab.com/YottaDB/DB/YDB/-/issues/577)

* <a name="x580"></a>The `timer_id` parameter of `ydb_timer_start_t()`, `ydb_timer_cancel_t()` and `ydb_timer_cancel()` is of type `intptr_t`. Previously this was of type `int` which was inadequate for a 64-bit platform. [YDB#580](https://gitlab.com/YottaDB/DB/YDB/-/issues/580)

* <a name="x583"></a>The CITABOPN error has additional detail on why the `fopen()` of the call-in table file failed (e.g., `%SYSTEM-E-ENO2, No such file or directory`). Previously this was not provided, making it hard to troubleshoot this error. [YDB#583](https://gitlab.com/YottaDB/DB/YDB/-/issues/583)

* <a name="x584"></a>$ZSTATUS, after an OPEN command that encounters an error, identifies the associated file name in addition to the error (e.g. `%YDB-E-DEVOPENFAIL, Error opening /tmp/x.txt, %SYSTEM-E-ENO13, Permission denied`). Previously it only identified the error without the associated file name (e.g. `%SYSTEM-E-ENO13, Permission denied`). [YDB#584](https://gitlab.com/YottaDB/DB/YDB/-/issues/584)

* <a name="x587"></a>The environment variable `ydb_dollartest`, if set to 0 or 1, provides an initial value for $TEST. If undefined, the default value is 1. [YDB#587](https://gitlab.com/YottaDB/DB/YDB/-/issues/587)

* <a name="x589"></a>YottaDB flushes all buffered writes done outside of its runtime logic (e.g., a `puts()` from C code called by a base M program) as part of closing open IO devices. Previously it was possible for some non-M buffered output to be lost when the output was piped to other programs, for example when the output of `yottadb -run` was piped to another program. [YDB#589](https://gitlab.com/YottaDB/DB/YDB/-/issues/589)

* <a name="x590"></a>The third piece of $ZRELDATE is the hexadecimal hash assigned by the version control system to identify the last source code commit. Previously, $ZRELDATE only had two pieces. [YDB#590](https://gitlab.com/YottaDB/DB/YDB/-/issues/590)

* <a name="x594"></a>`ydb_incr_s()`/`ydb_incr_st()` and language wrappers that use those functions, such as `IncrE()` implemented by the Go wrapper, work correctly on local variable nodes. Previously, it was possible in rare cases for a subsequent operation accessing such a local variable node (e.g. `ydb_get_s()`) to abnormally terminate with a SIG-11 or to report incorrect results. [YDB#594](https://gitlab.com/YottaDB/DB/YDB/-/issues/594)

* <a name="x619"></a>`ydb_ci()`, `ydb_cip()`, `ydb_ci_t()`, and `ydb_cip_t()` return YDB_TP_RESTART to the caller to indicate that an enclosing transaction should be restarted. Previously, they returned ERR_TPRETRY, even though restarting a transaction is not an error. Also, errors are returned as negative values, making the functions consistent with other Simple API functions. Previously, they returned positive values consistent with the `gtm_ci()` and `gtm_cip()` functions of the upstream code base. For example, the functions will return YDB_ERR_CALLINTCOMMIT  (actual value -151027858) where previously they would have returned ERR_CALLINTCOMMIT (actual value 151027858). Please note that:

  - Although this change is not backward compatible, it makes the YottaDB API more consistent and hence more robust by making application programming errors less likely.
  - With this change `ydb_ci()` is not synonymous with `gtm_ci()` and `ydb_cip()` is not synonymous with `gtm_cip()`. *There are no changes to `gtm_ci()` or `gtm_cip()` and YottaDB remains drop-in upward compatible with the upstream GT. M for applications.*

  [YDB#619](https://gitlab.com/YottaDB/DB/YDB/-/issues/619)

* <a name="GTM-9077"></a>SOCKET Devices properly report TLS errors. Since GT.M V6.3-003 ([YottaDB r1.20](https://gitlab.com/YottaDB/DB/YDB/-/releases/r1.20)) TLS related errors from SOCKET devices could result in a segmentation violation (SIG-11). [(GTM-9077)](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-008_Release_Notes.html#GTM-9077)

* <a name="GTM-9079"></a>YottaDB appropriately maintains the exit status for modules with literal arguments to XECUTE containing GOTO, NEW, QUIT, (nested) XECUTE and indirection. In GT.M V6.3-007 ([YottaDB r1.28](https://gitlab.com/YottaDB/DB/YDB/-/releases/r1.28)), such XECUTE arguments could cause incorrect $ZCSTATUS from ZCOMPILE and ZLINK, or a non-zero $status in the shell immediately after a MUMPS command performing a compilation. [(GTM-9079)](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-008_Release_Notes.html#GTM-9079)

* <a name="GTM-9099"></a>The compiler reports a warning when an argumentless DO has no subsequent block of the appropriate level; previously it silently ignored such inoperable DO commands. [(GTM-9099)](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-008_Release_Notes.html#GTM-9099)

* <a name="GTM-9100"></a>Execution resumes correctly after out-of-band actions, such as responding to MUPIP INTRPT, \<CTRL-C\>, and $ZTIMER. Previously when XECUTE'ng an operation that could run a long time (for example, LOCK) with an indirect argument that used an extrinsic, and the code invoked by the extrinsic performed a FOR or one of the set of potentially long commands (JOB, LOCK, MERGE, READ, ZATTACH, ZEDIT, ZPRINT, ZSHOW, ZSYSTEM, ZWRITE, or a format control mnemonic), when resuming from the out-of-band action, the process could terminate abnormally with a segmentation violation (SIG-11) or GTMASSERT2 fatal error. [(GTM-9100)](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-008_Release_Notes.html#GTM-9100)

### System Administration

* <a name="x493"></a>The default first output line of a GO or ZWR format MUPIP EXTRACT command is `YottaDB MUPIP EXTRACT` followed by the command line used to generate the extract, including the full path to the `mupip` executable, followed by `UTF-8` if the process ran in UTF-8 mode. Previously, the default first line was `GT.M MUPIP EXTRACT`. [YDB#493](https://gitlab.com/YottaDB/DB/YDB/-/issues/493)

* <a name="x494"></a>The entire text of a string specified for the `-label` command line parameter for GO and ZWR format MUPIP EXTRACT commands is reproduced on the first line of the extract. Previously only text up to the first space was reproduced. Note that quote characters should not be specified in the label, e.g. `-label="hello \"world"` is not valid. [YDB#494](https://gitlab.com/YottaDB/DB/YDB/-/issues/494)

* <a name="x568"></a>MUPIP commands that output to a pipe, such as `mupip extract -stdout | less` leave terminal settings untouched. Previously MUPIP modified and restored terminal settings even when its output was to a pipe. As a program such as `less` will modify and restore terminal settings (since it expects to control terminal output), the previous behavior could result in terminal settings left in an unfriendly state in the event the command was terminated, e.g., by pressing Ctrl-C at the terminal, requiring an `stty sane` to restore terminal settings. [YDB#568](https://gitlab.com/YottaDB/DB/YDB/-/issues/568)

* <a name="x569"></a>The `-ignorechset` command line option of MUPIP LOAD tells MUPIP to load the extract even if it was created by a MUPIP process in another mode (UTF-8 mode vs. M mode). As using `-ignorechset` bypasses YottaDB checks, use it only if you are sure that the extract file can be loaded correctly. [YDB#569](https://gitlab.com/YottaDB/DB/YDB/-/issues/569)

* <a name="x579"></a>%YDBPROCSTUCKEXEC is a standard utility program to capture diagnostics when invoked by the [ydb_procstuckexec](https://docs.yottadb.com/AdminOpsGuide/basicops.html#ydb-procstuckexec) mechanism. To use it, set the `ydb_procstuckexec` environment variable to `$ydb_dist/yottadb -run %YDBPROCSTUCKEXEC` with `$ydb_dist` expanded to the actual directory where YottaDB is installed, or to `yottadb -run %YDBPROCSTUCKEXEC` when `yottadb` is located by `$PATH`. Also `$ydb_dist/libyottadbutil.so` must be in the routine search path defined by `$ydb_routines` at process startup, or in the $ZROUTINES intrinsic special variable. When invoked by `yottadb -run %YDBPROCSTUCKEXEC <msg> <callingpid> <blockingpid> <count>` as described in the ydb_procstuckexec documentation, it creates in the directory specified by `$ydb_log`, `$gtm_log`, `$ydb_tmp`, or `$gtm_tmp` (defaulting to `/tmp`) a file whose name is `%YDBPROCSTUCKEXEC_<date>,<time>_<msg>_<callingpid>_<blockingpid>_<count>_<%YDBPROCSTUCEXECpid>.out` where the <date>,<time> timestamp is generated by [$ZDATE()](https://docs.yottadb.com/ProgrammersGuide/functions.html#zdate) with a format string of `"YEAR.MM.DD,24.60.SS"`, e.g., `%YDBPROCSTUCKEXEC_2020.05.22,17.58.38_ABCD_91998_91987_1_92045.out`. The file contains:

  - A summary of the invocation, e.g., `Invoked on 91987 by 91998 for 1st time; reason: MUTEXLCKALERT`.
  - The command line of the blocking process.
  - The environment of the blocking process.
  - The value of `/proc/sys/kernel/yama/ptrace_scope` (if non-zero, gdb may not be able to capture the blocking process for a snapshot of its state).
  - A snapshot of the state of the blocking process, captured by gdb.
  - The value returned by [$ZSIGPROC()](https://docs.yottadb.com/ProgrammersGuide/functions.html#zsigproc) used to send SIGUSR1 (a [MUPIP INTRPT](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#intrpt)) to the blocking pid. If appropriately configured (with [ydb_zinterrupt](https://docs.yottadb.com/AdminOpsGuide/basicops.html#ydb-zinterrupt) / [$ZINTERRUPT](https://docs.yottadb.com/ProgrammersGuide/isv.html#zinterrupt), the blocking process will create a dump file of its process state.

  If the calling pid and blocking pid are of different uids, the functionality of %YDBPROCSTUCKEXEC is reduced. Depending on security and system administration considerations of the system, it may be appropriate in such cases to create a small setuid root shell script to invoke %YDBPROCSTUCKEXEC. Depending your specific requirements, it may be appropriate to copy and adapt the standard routine for your environment.

  Previously, YottaDB provided the `ydb_procstuckexec` mechanism for a blocked process to invoke a monitoring script, but not a specific tool. [YDB#579](https://gitlab.com/YottaDB/DB/YDB/-/issues/579)

* <a name="x592"></a>JOB'd processes have the same process name (displayed by `ps`) as the parent process. Previously, it was `mumps` even if the parent process was `yottadb`. [YDB#592](https://gitlab.com/YottaDB/DB/YDB/-/issues/592)

* <a name="GTM-9084"></a>The TLS reference implementation plugin supports the use of OpenSSL 1.1.1, but not TLSv1.3 certificates. The configuration file pointed to by $ydb\_crypt\_config $gtmcrypt\_config or passed on the WRITE /TLS command recognizes SSL\_OP\_NO\_TLSv1\_3, in the ssl-options list. The reference encryption plugin currently sets this option by default. Note that even without using TLSv1.3 certificates, current versions of OpenSSL report some error conditions differently than prior versions of OpenSSL. Previously, the differences introduced by the TLSv1.3 certificates preferred by OpenSSL 1.1.1 caused YottaDB to report OpenSSL errors. [(GTM-9084)](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-008_Release_Notes.html#GTM-9084)

* <a name="GTM-9098"></a>YottaDB correctly prints 64-bit sequence numbers in error messages; previously sequence numbers only represented the lower 32 bits of the appropriate value. [(GTM-9098)](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-008_Release_Notes.html#GTM-9098)

### Other

* <a name="x388"></a>Simple API functions such as [ydb_lock_s()](https://docs.yottadb.com/MultiLangProgGuide/MultiLangProgGuide.html#ydb-lock-s) internally use nanosecond timers. Previously, although the API specified time in nanoseconds, the underlying timer was a millisecond timer. Note that even if timer resolution is in nanoseconds, the accuracy is always determined by the underlying hardware and operating system, as well as factors such as system load. [YDB#388](https://gitlab.com/YottaDB/DB/YDB/-/issues/388)

* <a name="x457"></a>The `--posix` option of the `ydbinstall` script downloads and installs the POSIX plugin after first successfully installing YottaDB. Note that this option requires Internet access, and also that `gcc` already be installed. [YDB#457](https://gitlab.com/YottaDB/DB/YDB/-/issues/457)

* <a name="x458"></a>The `--encplugin` option of the `ydbinstall` script downloads and installs the encryption plugin after first successfully installing YottaDB. Note that this option requires Internet access, and also that `gcc` and supported encryption libraries already be installed, as YottaDB includes no cryptographic software. [YDB#458](https://gitlab.com/YottaDB/DB/YDB/-/issues/458)

* <a name="x470"></a>When YottaDB is initialized by a non-M process using either call-ins, the Simple API, or by calling `ydb_init()` explicitly, the `gtm_dist` environment variable is set to `$ydb_dist`. Previously, it was possible under these circumstances for `$gtm_dist` to be not set, or to point to a directory other than `$ydb_dist`. [YDB#470](https://gitlab.com/YottaDB/DB/YDB/-/issues/470)

* <a name="x495"></a>With the environment variable `ydb_recompile_newer_src` set to 1, t\[rue\], or y\[es\], a ZLINK/DO/GOTO/ZBREAK/ZGOTO/ZPRINT/$TEXT recompile the `.m` file only if it has a newer modification time than the corresponding `.o` file. The default behavior is for the `.m` file to be recompiled if its modification time is later than OR equal to that of the corresponding `.o` file. [YDB#495](https://gitlab.com/YottaDB/DB/YDB/-/issues/495)

* <a name="x496"></a>Object code files generated by YottaDB can be linked into shared libraries by `ld` version 2.33 and later. Previously `ld` version 2.33 and later, found on distributions with newer toolchains, such as Ubuntu 19.10 & up, and Arch Linux, would not link YottaDB object code files into shared libraries. [YDB#496](https://gitlab.com/YottaDB/DB/YDB/-/issues/496)

* <a name="x521"></a>The `--octo` command line option of the `ydbinstall` script downloads and installs [Octo](https://gitlab.com/YottaDB/DBMS/YDBOcto), a YottaDB plugin for SQL access to application data in YottaDB global variables. [YDB#521](https://gitlab.com/YottaDB/DB/YDB/-/issues/521)

* <a name="x532"></a>With the `--utf8 default` and `--encplugin` options, the `ydbinstall` script successfully installs the encryption plugin. Previously, when installing the encryption plugin, it failed with an ICUSYMNOTFOUND error. [YDB#532](https://gitlab.com/YottaDB/DB/YDB/-/issues/532)

* <a name="x595"></a>`yottadb -version` provides a detailed report on the YottaDB build, e.g.,

  ```
  $ yottadb -version
  YottaDB release:         r1.30
  Upstream base version:   GT.M V6.3-008
  Platform:                Linux x86_64
  Build date/time:         2020-07-17 16:58
  Build commit SHA:        5f0e5c9b368c18405e2b4a34dc85f4901cfde9ae
  $
  ```

  [YDB#595](https://gitlab.com/YottaDB/DB/YDB/-/issues/595)

* <a name="x597"></a>If the build of any plugin (those installed by the `--encplugin`, `--octo`, `--posix`, and `--zlib` command line options) does not succeed, the `ydbinstall` script retains the directory where it built the plugin. Previously, it deleted directories where it built plugins, making troubleshooting problematic. [YDB#597](https://gitlab.com/YottaDB/DB/YDB/-/issues/597)

* <a name="GTM-8911"></a>YottaDB defers allocation of transaction processing structures for a particular region until that region is directly part of a TP transaction. This prevents allocation of structures that may not be needed by the application. Previously, YottaDB allocated transaction processing structures for all open regions at the first transaction in any region, and from then on, when opening any new region. [(GTM-8911)](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-008_Release_Notes.html#GTM-8911)

* <a name="GTM-9000"></a>%PEEKBYNAME() accepts the path to the directory where `gtmhelp.gld` and `gtmhelp.dat` reside, as an optional fourth parameter gldpath. These files contain the data required for %PEEKBYNAME() to execute properly. If not provided, %PEEKBYNAME() picks up the data from $ydb\_dist, the default location. Previously, %PEEKBYNAME() did not accept this option. [(GTM-9000)](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-008_Release_Notes.html#GTM-9000)

* <a name="GTM-9082"></a>The auto-upgrade processes performs on database file headers works appropriately. In GT.M V6.3-007 ([YottaDB r1.28](https://gitlab.com/YottaDB/DB/YDB/-/releases/r1.28)) due to GTM-9052, it failed to upgrade the flush\_trigger\_top field, which could require a MUPIP SET -TRIGGER\_FLUSH=*n* if flushing with that version seems inappropriate, where *n* is a number of dirty buffers at which processes should start flushing more aggressively. [(GTM-9082)](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-008_Release_Notes.html#GTM-9082)

* <a name="GTM-9092"></a>$$IN^%YGBLSTAT(\<pid\>,\<global directory\>,\<region\>) returns a TRUE (1) value if the process is sharing statistics in the region, a FALSE (0) if it is not, and an empty string if the pid is invalid or there is no sharing for a region of the specified name. If region is empty or an asterisk, the extrinsic returns a TRUE if the process is sharing statistics in any region, and a FALSE otherwise. If the global directory is not empty the function attempts to use it, but if it is unavailable the function fails into the invoking environment's specified $ETRAP or $ZTRAP. There are other ways of getting the same kind of indication but they require more code. Previously ^%YGBLSTAT did not provide this extrinsic function. Also, ^%YGBLSTAT returns an empty string for any process that has not shared statistics on any region sought by the invoking arguments. The most interesting class of such processes are probably non-YottaDB processes, but it also includes YottaDB processes that are not sharing. Previously the utility returned an empty string only for nonexistent processes. In addition, ^%YGBLSTAT deals appropriately with the current global directory; previously an explicit or implicit invocation of the STAT extrinsic with no global directory argument inappropriately used the original global directory of the reporting process rather than its current global directory. [(GTM-9092)](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-008_Release_Notes.html#GTM-9092)

* <a name="GTM-9097"></a>The Source Server issues a JNLFILEREAD error when processing corrupted journal files; previously, journal files corrupted by external actions could cause the Source Server to hang indefinitely. [(GTM-9097)](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-008_Release_Notes.html#GTM-9097)

* <a name="GTM-9110"></a>YottaDB issues an error, typically CLISTRTOOLONG or CLIERR, for a command line that exceeds 32KiB; in V6.3-007 ([YottaDB r1.28](https://gitlab.com/YottaDB/DB/YDB/-/releases/r1.28)) a regression allowed this condition to cause a segmentation violation (SIG-11), This issue was identified in the development environment and was reported by a user. [(GTM-9110)](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-008_Release_Notes.html#GTM-9110)

## Messages

**AIOQUEUESTUCK**, Waited mmmm minutes for AIO work queue to complete (cr = rrrr)

All Components Run Time Error: A process is taking over mmmm minutes for asynchronous IO (AIO) activity to complete on cache record rrrr. A zero value for rrrr indicates that the process is waiting for all AIO activity to complete.

Action: Check for trouble in the I/O subsystem. The process continues to wait for AIO activity to complete.

**BOOLEXPRTOODEEP**,Boolean expression depth exceeds maximum supported limit of 2047

Compile / Run Time Error: The nesting depth of a Boolean expression exceeds 2047, the YottaDB limit.

Action: Fix the coding issue. Exceeding the nesting depth limit of 2047 is a pathological error that is most likely to occur in generated code.

**BUFSPCDELAY**, Request for bbbb blocks in region rrrr delayed

All Components Run Time Warning: A process is taking longer than expected to obtain bbbb free database buffers for region rrrr. If bbbb is zero, then the process was attempting to free one particular buffer.

Action: Check for trouble in the I/O subsystem. The process continues its attempt to obtain the free buffer(s).

**COMMITWAITSTUCK** Pid wwww waited tttt minute(s) for nnnn concurrent YottaDB process(es) to finish commits in database file dddd

Run Time Error: This error message indicates that a process could not finish a database transaction commit and timed out waiting for other concurrent processes to finish. The process will continue to wait.

Action: Check the operator log for accompanying COMMITWAITPID messages. Every concurrent YottaDB process reporting COMMITWAITSTUCK messages would have accompanying COMMITWAITPID message(s). If so, review those messages. If not, report to your YottaDB support channel with system log and operator log information.

**DONOBLOCK**, Argumentless DO not followed by a block

Compile Time Warning: This indicates the compiler detected an argumentless DO with no subsequent block with an appropriate level, and optimized it away.

Action: This indicates a coding issue where the block is missing or has the wrong level indication. This may occur in code under development where the block is yet to be coded, or code being debugged where the block has been commented out. Otherwise, it likely indicates a logic bug where a programmer intended to provide a block of code but did not provide one. Correct as appropriate.

**GTMSECSHRPERM**, The gtmsecshr module in ydb_dist (DDDD) does not have the correct permission and uid (permission: PPPP, and UID: UUUU)

Run Time Error: This indicates that a client did not start GTMSECSHR, installed in DDDD, because the executable was not owned by root (UUUU is the actual owner) and/or did not have setuid and/or execute permissions (actual permissions are PPPP).

Action: Arrange to provide the GTMSECSHR executable with the proper characteristics. The executable must be SETUID root with execute permissions for the current user.

**INDRCOMPFAIL**, Compilation of indirection failed

Run Time Error: This indicates that an indirection or XECUTE command failed due to syntax errors.

Action: Review the code and make sure the indirection or XECUTE string has valid syntax and contains no non-graphic characters. Consider using $ZWRITE to identify any such characters.

**INVMAINLANG**, Invalid main routine language id specified: xxxx

Run Time Error: This indicates that an internal feature of YottaDB, an alternative signal handling mechanism, is used by an unsupported language.

Action: If this error occurs for a language wrapper provided by YottaDB, contact your YottaDB support channel. Otherwise, contact the developer of the language wrapper the application is using.

**INVVALUE**, VVVV is invalid DEC value for $ZCONVERT(). Range is -9223372036854775808 to 18446744073709551615\
**INVVALUE**, VVVV is invalid HEX value for $ZCONVERT(). Range is 1 to 16 unsigned hexadecimal digits

Run Time Error: This message has two forms both of which indicate that the value VVVV is not valid input for $ZCONVERT() in the specified base.

Action: If the input value is expected to be within the range supported by $ZCONVERT(), the actual results from a coding issue or from input that is not validated, correct the issue. If input values are legitimately expected outside the range supported by $ZCONVERT() use the [%DH](https://docs.yottadb.com/ProgrammersGuide/utility.html#dh) and [%HD](https://docs.yottadb.com/ProgrammersGuide/utility.html#hd) utility programs.

**INVZCONVERT**, Translation supported only between DEC/HEX OR between UTF-8/UTF-16/UTF-16LE/UTF-16BE

Run Time Error: This indicates that the base from which a number is to be converted is the same as that to which it is to be coverted, or for a string that conversion between the requested character sets is not supported. Numeric conversion is not meaningful when the bases are the same.

Action: Review the code, and modify as necessary to ensure that $ZCONVERT() is being used correctly.

**JNLQIOSALVAGE**, Journal IO lock for database file dddd salvaged from dead process pppp

Run Time Information: An active process salvaged the critical resource for a journal flush of database file dddd marked as belonging to process pppp, which no longer exists.

Action: The system automatically returns the critical resource to normal operation and continues execution. If this message continues to occur, please investigate why the process holding the crit abnormally exited.

**LOADINVCHSET**, Extract file CHSET xxx is incompatible with ydb\_chset/gtm\_chset.

MUPIP Information: This indicates that a MUPIP LOAD operation did not take place because the value of the environment variable ydb\_chset or gtm\_chset at the time of creating the extract file was not the same as the current value of ydb\_chset or gtm\_chset.

Action: Determine whether to change the current character set or retry the EXTRACT with a different character set. If you know that the MUPIP LOAD operation is correct even with the mismatch, use the `--ignorechset` option to instruct it to proceed with the LOAD despite the mismatch.

**NULSUBSC**, XXXX Null subscripts are not allowed for database file: YYYY

Run Time/MUPIP Error: This indicates that a global variable specified a null subscript in a database file YYYY, which does not accept null subscripts. The leading context (XXXX) identifies more about the event or the location of the issue.

Action: Look for the source of the null subscript(s) and consider whether they are appropriate or due to a coding error. If they are appropriate, use MUPIP SET -NULL\_SUBSCRIPTS, and remember to make the same adjustment with GDE CHANGE REGION -NULL\_SUBSCRIPTS to ensure the next time you recreate a database that the characteristic persists.

**REPLJNLCLOSED**, Replication in jeopardy as journaling got closed for database file dddd. Current region seqno is xxxx[XXXX] and system seqno is yyyy[YYYY]

Run Time Warning: This message indicates that YottaDB turned OFF journaling and switched replication from ON to WAS_ON on the specified database. Other preceding messages identify the cause (for example, lack of disk space while writing to journal file, permissions issue while auto-switching to new journal files, and so on). The message also displays the region (xxxx decimal and XXXX hexadecimal) and journal (yyyy/YYYY) sequence numbers. From this point, replicating updates on the primary to the secondary might, or might not, work depending on the backlog on the primary until replication/journaling gets turned back ON.

Action: First, correct the cause (lack of disk space, permission issues, and so on) that turned journaling OFF.

Execute the MUPIP SET REPLICATION=ON or MUPIP BACKUP REPLICATION=ON command to turn replication (and journaling) ON and switch to a new set of journal files. This command can work while processes are concurrently updating the database and causes YottaDB to journal subsequent updates in both the journal file and journal pool (rather than only in the journal pool as it does when replication is in the WAS_ON state).

Execute the MUPIP REPLIC -SOURCE -SHOWBACKLOG command. Note down the value of "sequence number of last transaction written to journal pool".

Execute the above command at regular intervals and note down the value of "sequence number of last transaction sent by source server."

If the "sequence number of last transaction sent by source server" is greater than "sequence number of last transaction written to journal pool", it means that the source server successfully sent all journal records during the time interval when journaling was turned OFF. In this case, no further action is required.

On the other hand, if the "sequence number of last transaction sent by source server" is less than "sequence number of last transaction written to journal pool" and reports the same value across repeated SHOWBACKLOG commands, then check the source server log file for any error messages - most likely a NOPREVLINK error from the source server. This means the source server could not locate the corresponding journal records required from the journal files to replicate a particular sequence number and therefore, it failed to synchronize the primary and secondary. In this case, take an online backup of the primary, restore it on the secondary and start the secondary with the UPDATERESYNC qualifier to synchronize the secondary with the primary.

**TPCALLBACKINVRETVAL**, Invalid return type for TP callback function

Run Time Error: This is not an error that YottaDB generates, but one generated by a language wrapper for a dynamically typed language such as [Python](https://www.python.org/). When an application function that implements transaction logic returns an inappropriately typed value to the wrapper, it raises the TPCALLBACKINVRETVAL error.

Action: Examine application logic and ensure that transaction logic returns an appropriately typed return value to the wrapper.

**WCSFLUFAILED**, EEEE error while flushing buffers at transaction number TTTT for database file DDDD

Run Time Error: For a BG database file, this means that a process attempting to flush modified global buffers to disk encountered an error. EEEE is the error it encountered, for database file DDDD when attempting to flush the blocks for database transaction number TTTT. This is usually accompanied by other messages that can together help provide more information and context. If you need further assistance and have purchased support, contact your YottaDB support channel.

Action: Refer to the description of error EEEE and take appropriate action.

**ZYSQLNULLNOTVALID**, $ZYSQLNULL cannot be used as an integer, numeric, gvn subscript/value or lock subscript

Compile / Run Time Error: $ZYSQLNULL can only be used in expressions, as a local variable subscript, or the value of a local variable node.

Action: Correct the coding issue resulting in the incorrect $ZYSQLNULL usage.


## Legal Stuff

Copyright © 2020 YottaDB LLC

Permission is granted to copy, distribute and/or modify this document under the terms of the [GNU Free Documentation License, Version 1.3](http://www.gnu.org/licenses/fdl.txt) or any later version published by the Free Software Foundation; with no Invariant Sections, no Front-Cover Texts and no Back-Cover Texts.

YottaDB® and Octo® are registered trademarks of YottaDB LLC.
GT.M™ is a trademark of Fidelity National Information Services, Inc.
Other trademarks belong to their respective owners.

This document contains a description of YottaDB and the operating instructions pertaining to the various functions that comprise the software. This document does not contain any commitment of YottaDB LLC. YottaDB LLC believes the information in this publication is accurate as of its publication date; such information is subject to change without notice. YottaDB LLC is not responsible for any errors or defects.
