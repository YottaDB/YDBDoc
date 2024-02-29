<!---
.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2021-2022 YottaDB LLC and/or its subsidiaries.#
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

# YottaDB r1.32

## Release Note Revision History

| Revision | Date | Summary |
| ---------| ---- | ------- |
| 1.00     | July 13, 2021 | r1.32 Initial Release |
| 1.01     | July 13, 2021 | Updated Title and Description for #724 |
| 1.02     | July 22, 2021 | Added TRANSREPLJNL1GB ([749](#x749)), WORDEXPFAILED, and ZCCONVERT messages |

## Contact Information

### YottaDB LLC

40 Lloyd Avenue, Suite 104 Malvern, PA 19355, USA <info@yottadb.com> +1 (610) 644-1898

### Support

#### Customers

Contact your YottaDB support channel.

#### Others

For free (to you) support from members of communities who run widely available applications on YottaDB, please use an application-specific list where appropriate.

* For requests other than to the communities below, [post an Issue](https://gitlab.com/YottaDB/DB/YDB/issues) and include the words "Help Wanted" in the Title.
* For requests specific to the use of YottaDB from node.js via [Nodem](https://github.com/dlwicksell/nodem), [post an Issue on the Nodem project](https://github.com/dlwicksell/nodem/issues/new/).
* For access from [QewdJS](http://qewdjs.com/), or [EWD.js](https://github.com/robtweed/ewd.js), post to the [Enterprise Web Developer community](https://groups.google.com/forum/#!forum/enterprise-web-developer-community).
* For requests specific to the use of YottaDB with [VistA](https://en.wikipedia.org/wiki/VistA) flavors, post to the [Hardhats](https://groups.google.com/forum/#!forum/hardhats) list.
* For requests specific to the use of YottaDB with M other than for applications above, post to the [comp.lang.mumps](https://groups.google.com/forum/#!forum/comp.lang.mumps) list.

## r1.32

### Overview

Although there is no single theme to YottaDB r1.32, it qualifies as a major release because it includes a significant number of enhancements, including several to enable the [Application Independent Metadata](https://gitlab.com/YottaDB/Util/YDBAIM) plugin. The plugin provides functionality for applications to push responsibility for maintaining cross references and statistics to YottaDB triggers, thereby reducing the code that applications must maintain. Enhancements include:

* The $ZYSUFFIX() function enables applications to create variable and routine names that are guaranteed for all practical purposes to be unique, as it uses the 128-bit [MurmurHash3](https://en.wikipedia.org/wiki/MurmurHash#MurmurHash3) non-cryptographic hash. ([391](#x391))
* A $ZPARSE() option to follow symbolic links. ([581](#x581))
* Sourcing `ydb_env_set` creates and manages a three-region database, and defaults to UTF-8 mode. ([661](#x661))
* An option to allow $ZINTERRUPT to be invoked for the USR2 signal. ([678](#x678))
* The `--aim` option of `ydbinstall` / `ydbinstall.sh` installs the [Application Independent Metadata](https://gitlab.com/YottaDB/Util/YDBAIM) plugin.
* Shell-like word expansion with $VIEW("WORDEXP"). ([731](#x731))
* Propagation downstream of context set by triggers. ([727](#x727))
* `ydb_ci_*()` functions return the ZHALT argument for C code that calls M code which terminates with a ZHALT. ([742](#x742))

From the upstream [GT.M V6.3-009](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-009_Release_Notes.html) and [GT.M V6.3-010](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-010_Release_Notes.html), which are included in the r1.32 code base, there are enhancements to operational functionality.

There are numerous smaller enhancements that make system administration and operations (DevOps) friendlier, and easier to automate. For example, %PEEKBYNAME() has an option to query global directory segments without opening the corresponding database files ([730](#x730)), orphaned relinkctl files are automatically cleaned up ([695](#x695)), and the `--octo` option of `ydbinstall` / `ydbinstall.sh` installs Octo such that `octo --version` reports the git commit hash of the build ([634](#x634)).

As with any YottaDB release, there are numerous other enhancements and fixes, some specific to YottaDB (including fixes to issues discovered while merging and testing the upstream code), and others inherited from the upstream code.

YottadB r1.32 is upward compatible with [YottaDB r1.30](https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.30), [GT.M V6.3-009](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-009_Release_Notes.html), and [GT.M V6.3-010](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-010_Release_Notes.html).

### Platforms

A platform is a combination of a CPU architecture and an operating system. A platform is Supported, Supportable, or Unsupported.

* Supported means that we have the platform in our development environment and test each release on that platform.
* Supportable means that although we do not necessarily have such a platform in our environment, we have no reason to believe that the software will not run on it.
* All others are Unsupported.

| CPU Architecture                    | Supported OS Version(s)                                                              | Notes                                                                                                                         |
| ----------------------------------- | ------------------------------------------------------------------------------------ | ----------------------------------------------------------------------------------------------------------------------------- |
| 64-bit x86                          | Ubuntu 20.04 LTS; Red Hat Enterprise Linux 7.x & 8.x; Debian GNU/Linux 11 (Bullseye) | There are separate binary distributions for each OS version, owing to differences in library versions of those distributions. |
| 64-bit ARM (Raspberry Pi 3 Model B) | Ubuntu 20.04 LTS; Debian GNU/Linux 11 (Bullseye)                                     | See below.                                                                                                                    |
| 32-bit ARM (Raspberry Pi Zero)      | Debian GNU/Linux 11 (Bullseye)                                                       | See below.                                                                                                                    |

Recent releases of major GNU/Linux distributions with contemporary kernels, glibc and ncurses are Supportable. Specific notes:

- We intend to drop support for Red Hat Enterprise Linux 7.x for future YottaDB releases. Please contact us if this a concern.
- On [Arch Linux](https://www.archlinux.org/) and other leading edge distributions, YottaDB may need to be recompiled from source code owing to library and tool chain versions newer than those used in building Supported distributions.
- While YottaDB is Supportable on other ARMv6, [ARMv7-A](https://en.wikipedia.org/wiki/Comparison_of_ARMv7-A_cores), and [ARMv8-A](https://en.wikipedia.org/wiki/Comparison_of_ARMv8-A_cores) CPUs, owing to variations in the implementations of ARM microarchitectures, we recommend that you ensure the software runs correctly before committing to any specific hardware other than those listed above. Please contact <info@yottadb.com> if you want a specific combination of OS and CPU microarchitecture to be Supported.

### Installation

See our [Get Started page](https://yottadb.com/product/get-started) to use YottaDB.

We **strongly recommend** that you install YottaDB r1.32 in a newly created directory, different from those of prior YottaDB releases and any GT.M versions you may have installed on the system.

### Removing an installed YottaDB release

Assuming `$ydb_dist` points to the directory where YottaDB is installed:

* Cleanly shut down all application processes using that release.
* Execute `mupip rundown && mupip rundown -relinkctl`
* Ensure that there are no `gtcm*` or `gtmsecshr` processes active.
* Use `sudo lsof | grep $ydb_dist` to ensure there are no open files.
* Delete the directory with `sudo rm -rf $ydb_dist`

## Upgrading to YottaDB r1.32

As YottaDB r1.32 is upward compatible from YottaDB r1.30, GT.M V6.3-009 and GT.M V6.3-010, the minimal upgrade steps are:

* Install YottaDB r1.32. Use `ydbinstall` / `ydbinstall.sh` to install plugins you use.
* Install plugins you need in addition to those installed in the previous step.
* Recompile object code, and recreate shared libraries where appropriate.
* If you are using encryption, compile and install the reference implementation plugin (if not done by the `ydbinstall` / `ydbinstall.sh` script) or your customized plugin.
* Cleanly shut down the application and ensure that the database files are shut down using MUPIP RUNDOWN from the prior release.
* Switch journal files with the new YottaDB release.
* Start using the new YottaDB release.

If the database has triggers defined with non-canonical numbers, or numbers specified as strings with any version prior to r1.28, or if you are unsure, extract the trigger definitions, delete existing triggers, and reload the trigger definitions. Issue [#430] from r1.28 has a series of steps you can copy and execute. There is no need to do this if upgrading from r1.28 or later.

To upgrade from older GT.M releases, first upgrade to GT.M V6.0-000 or later and follow the steps above, or contact your YottaDB support channel for assistance and guidance.

A more sophisticated upgrade technique is:

* Install YottaDB r1.32.
* Create a new replicated instance of your application (on the same system or a different system).
* Assuming the existing instance is A, and the new instance is B, upgrade B to r1.32 and start replicating from A to B.
* Once B catches up, switchover so that B is in a primary role replicating to A.
* Once you are satisfied with B, remove (or upgrade) A.

## Change History

### r1.32

YottaDB r1.32 includes the following enhancements and fixes beyond [YottaDB r1.30](https://github.com/YottaDB/YottaDB/releases/tag/r1.30).

| ID             | Category              | Summary                                                                                                                                    |
|----------------|-----------------------|--------------------------------------------------------------------------------------------------------------------------------------------|
| ([391](#x391)) | Languages             | $ZYSUFFIX() returns 128-bit hash as 22-character alphanumeric string                                                                       |
| ([441](#x441)) | Languages             | Auto-zlink works properly when linking a newer version of an M routine after changing $ZROUTINES                                           |
| ([497](#x497)) | System Administration | ydbinstall installs successful build on the same platform without --force-install |
| ([505](#x505)) | Languages             | More helpful error message for `-object` qualifier without a value                                                                           |
| ([551](#x551)) | Languages             | $ZSYSLOG() logs its parameter string verbatim                                                                                              |
| ([558](#x558)) | Languages             | ZSHOW "*" is equivalent to ZSHOW "VIBDLGR"                                                                                                 |
| ([581](#x581)) | Languages             | $ZPARSE() option to follow symbolic links                                                                                                  |
| ([612](#x612)) | Languages             | Suspended direct mode correctly restores terminal settings when resumed                                                                    |
| ([629](#x629)) | Languages             | Unary plus on $ZYSQLNULL value set from $ORDER() has a value of $ZYSQLNULL                                                                 |
| ([630](#x630)) | Languages             | $ZSYSLOG() correctly identifies executable name in SYSLOG\_IDENTIFIER                                                                      |
| ([632](#x632)) | Languages             | ydb\_tp\_st() returns YDB\_ERR\_CALLINAFTEREXIT faster if a fatal signal is caught by a different thread                                   |
| ([633](#x633)) | Languages             | Removed a rare case of SIGSEGV from ydb\_incr\_s() / ydb\_incr\_st()                                                                       |
| ([634](#x634)) | Other                 | Octo installed by `ydbinstall` / `ydbinstall.sh` `--octo` reports git commit hash                                                          |
| ([635](#x635)) | System Administration | MUPIP INTEG (no options) responds to Ctrl-C (SIGINT)                                                                                       |
| ([651](#x651)) | Database              | Removed a rare case of SIGSEGV (SIG-11) for MM databases                                                                                   |
| ([653](#x653)) | Languages             | READ with timeout on a socket device always times out                                                                                      |
| ([657](#x657)) | Other                 | TLS 1.3 support re-enabled in the encryption plugin reference implementation                                                               |
| ([661](#x661)) | Database              | Sourcing `ydb_env_set` creates a three-region database and defaults to UTF-8 mode                                                          |
| ([663](#x663)) | System Administration | Loading a binary extract containing null subscripts from a region configured for nonstandard null collation into another such region works |
| ([664](#x664)) | Languages             | VIEW "ZTRIGGER\_OUTPUT" controls $ZTRIGGER() output                                                                                          |
| ([671](#x671)) | System Administration | MUPIP TRIGGER can accept input from stdin and send output to stdout                                                                        |
| ([673](#x673)) | System Administration | Lock space improvements |
| ([676](#x676)) | System Administration | Receiver Server and MUPIP JOURNAL ROLLBACK FETCHRESYNC reset connection on bad input                                                       |
| ([678](#x678)) | Languages             | Optionally invoke $ZINTERRUPT for USR2 signal                                                                                              |
| ([679](#x679)) | Other                 | Healthy, active process terminates gracefully on receipt of process-terminating signal                                                     |
| ([680](#x680)) | Languages             | Eliminate rare case of SIGSEGV (SIG-11) during M lock acquisitions                                                                         |
| ([682](#x682)) | Other                 | %HO accepts hexadecimal numbers prefixed by 0x or 0X                                                                                       |
| ([683](#x683)) | Database              | More robust ASYNCIO                                                                                                                        |
| ([685](#x685)) | Other                 | %OH and %OD utility programs convert until first invalid character                                                                         |
| ([688](#x688)) | Languages             | ZWRITE of M global variables using pattern match operators from database region with null subscripts works                                 |
| ([692](#x692)) | Languages             | Modulo operator (#) returns a purely numeric value if dividend has numeric portion followed by non-numeric portion                         |
| ([695](#x695)) | System Administration | Automatically clean up orphaned relinkctl files                                                                                            |
| ([700](#x700)) | Languages             | Multi-line `-xecute` in $ZTRIGGER() accepts trailing "`>>`" and subsequent `-pieces` or `-delim` following `-xecute`                       |
| ([704](#x704)) | System Administration | yottadb executable works with valgrind  |
| ([709](#x709)) | Languages             | $VIEW("GVFILE",\<region>) does not create database file for AutoDB regions |
| ([711](#x711)) | Languages             | $INCREMENT(gvn) works correctly within a transaction with NOISOLATION enabled for gvn                                                      |
| ([712](#x712)) | Languages             | Appropriate reporting of ERRWZTIMEOUT and ERRWZINTR errors in direct mode |
| ([715](#x715)) | Languages             | MUPIP JOURNAL RECOVER/ROLLBACK BACKWARD completes successfully if an AutoDB region's database file does not exist |
| ([718](#x718)) | System Administration | `ydbinstall` / `ydbinstall.sh` `--aim` installs Application Independent Metadata plugin                                                    |
| ([719](#x719)) | Other                 | `ydb_env_set` always sets `ydb_dist` to the parent directory                                                                               |
| ([721](#x721)) | System Administration | LKE SHOW displays all information for an M lock on one line                                                                                |
| ([724](#x724)) | Languages             | Correct treatment of numeric first argument by $ZATRANSFORM()                                                                      |
| ([725](#x725)) | Languages             | $GET(gvn) returns correct result in all cases on ARM platforms for database regions using the MM access method                             |
| ([727](#x727)) | Languages             | Propagation downstream of $ZTWORMHOLE set by trigger logic |
| ([730](#x730)) | Other                 | %PEEKBYNAME() provides a way to examine global directory segments                                                                          |
| ([731](#x731)) | Languages             | $VIEW("WORDEXP") provides wordexp() functionality for shell like word expansion                                                            |
| ([737](#x737)) | Languages             | For M code invoked from non-M code, $QUIT specifies whether the caller expects a return value |
| ([738](#x738)) | System Administration | MUPIP FREEZE ONLINE ensures database and journal file integrity in a rare case                                                             |
| ([739](#x739)) | System Administration | MUPIP INTEG SUBSCRIPT handles database regions with null subscripts when no end key is specified                                           |
| ([741](#x741)) | System Administration | DSE REMOVE RECORD works on blocks with DBCOMPTOOLRG errors                                                                                 |
| ([742](#x742)) | Languages             | ydb\_ci\_*() returns ZHALT argument                                                                                                           |
| ([743](#x743)) | System Administration | MUPIP INTEG ONLINE on a journaled database runs correctly in a rare edge case                                                              |
| ([744](#x744)) | Languages             | %CDS^%H correctly converts $HOROLOG format dates greater than 94657  |
| ([749](#x749)) | Languages             | Transactions generating >1GiB logical update journal records abort with TRANSREPLJNL1GB error |
| ([755](#x755)) | Languages | Very rare case of MUPIP RECOVER TRUNCATE concurrent with internal recovery code no longer causes database damage |

### GT.M V6.3-009

YottaDB r1.32 incorporates enhancements and fixes from [GT.M V6.3-009](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-009_Release_Notes.html).

| ID                      | Category              | Summary                                                                                                             |
| ----------------------- | --------------------- | ------------------------------------------------------------------------------------------------------------------- |
| ([GTM-6631](#GTM-6631)) | System Administration | MUPIP REORG NOCOALESCE, NOSPLIT and NOSWAP to selectively disable its actions                                       |
| ([GTM-8203](#GTM-8203)) | System Administration | MUPIP REORG TRUNCATE support for KEEP=\|blocks\|percent%\|                                                          |
| ([GTM-8706](#GTM-8706)) | System Administration | MUPIP REPLICATE STOPRECEIVERFILTER disables any active receiver filter                                              |
| ([GTM-8901](#GTM-8901)) | System Administration | MUPIP JOURNAL EXTRACT GVPATFILE specifies a patterns list to restrict the extract output to matching SET records   |
| ([GTM-8921](#GTM-8921)) | System Administration | YottaDB cleans up IPC semaphores for READ\_ONLY (MM) database files at the first opportunity                        |
| ([GTM-9037](#GTM-9037)) | System Administration | Additional reporting of unusual journaling issues to the operator log                                               |
| ([GTM-9044](#GTM-9044)) | System Administration | Enhanced message in case of gtmsecshr misconfiguration                                                              |
| ([GTM-9113](#GTM-9113)) | Languages             | Fix XECUTE of a literal FOR with a control variable termination value                                               |
| ([GTM-9114](#GTM-9114)) | Languages             | Prevent GTMCHECK from auto-zlink of a renamed object                                                               |
| ([GTM-9115](#GTM-9115)) | Other                 | Minor performance improvements to the radix conversion utilities                                                    |
| ([GTM-9116](#GTM-9116)) | System Administration | Installation script explicitly applies permissions to `libgtmutil.so`                                               |
| ([GTM-9123](#GTM-9123)) | Languages             | Fix an odd case of a potential incorrect result with ydb\_side\_effects compilation                               |
| ([GTM-9126](#GTM-9126)) | Languages             | ZSTEP OVER stops after returning from a line containing an XECUTE or indirection                                    |
| ([GTM-9134](#GTM-9134)) | System Administration | Protect a Receiver Server against spurious messages while waiting for a connection                                  |
| ([GTM-9142](#GTM-9142)) | System Administration | Please see [GTM-6631](#GTM-6631).                                                                                   |
| ([GTM-9144](#GTM-9144)) | System Administration | Fix to detection of GTM-E-DBDUPNULCOL error                                                                         |
| ([GTM-9145](#GTM-9145)) | Other                 | ^%RI and ^%RO handle longer lines                                                                                   |
| ([GTM-9149](#GTM-9149)) | Languages             | YottaDB protections against invalid returns from external calls ($&)                                                |
| ([GTM-9152](#GTM-9152)) | Languages             | Fix error compiler handling of invalid literal code in an XECUTE argument                                           |
| ([GTM-9155](#GTM-9155)) | Languages             | Fix $SELECT() handling of certain nestings that included extrinsics                                                 |

### GT.M V6.3-010

YottaDB r1.32 also incorporates enhancements and fixes from [GT.M V6.3-010](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-010_Release_Notes.html).

| ID                      | Category              | Summary                                                                                                             |
| ----------------------- | --------------------- | ------------------------------------------------------------------------------------------------------------------- |
| ([GTM-1044](#GTM-1044)) | Languages             | Next and Previous character values from $ZATRANSFORM()                                                              |
| ([GTM-5333](#GTM-5333)) | System Administration | See [GTM-5626](#GTM-5626)                                                                                           |
| ([GTM-5626](#GTM-5626)) | System Administration | Replication retries indefinitely looking for a connection                                                           |
| ([GTM-7113](#GTM-7113)) | System Administration | MUPIP ROLLBACK FETCHRESYNC tries to reconnect in case of a lost connection                                          |
| ([GTM-8322](#GTM-8322)) | System Administration | MUPIP SIZE accepts SUBSCRIPT=\<subscript> and other clean up of SIZE and INTEG                                      |
| ([GTM-8747](#GTM-8747)) | System Administration | MUPIP JOURNAL EXTRACT CORRUPTDB for journal extract without using the database file header(s)                       |
| ([GTM-8848](#GTM-8848)) | System Administration | Error "nature" classification added to many MUPIP INTEG reports                                                     |
| ([GTM-9076](#GTM-9076)) | System Administration | Better error messaging for file paths that exceed the maximum supported                                             |
| ([GTM-9166](#GTM-9166)) | System Administration | JNLPROCSTUCK does not produce a JNLFLUSH message                                                                    |
| ([GTM-9178](#GTM-9178)) | Languages             | Fix for faulty ZTIMEOUT vector code trying to execute in Direct Mode                                                |
| ([GTM-9180](#GTM-9180)) | System Administration | Slight change in DSE output for bit maps                                                                            |
| ([GTM-9181](#GTM-9181)) | Languages             | Fix for Boolean literals prior to, or within, $SELECT()                                                             |
| ([GTM-9183](#GTM-9183)) | Languages             | Fix indirect exclusive NEW after a FOR                                                                              |
| ([GTM-9185](#GTM-9185)) | Languages             | Align ZLINK behavior with `yottadb` and ZCOMPILE commands                                                           |
| ([GTM-9188](#GTM-9188)) | Other                 | Ignore spaces between the qualifier introducer (-) and the qualifier keyword for a `yottadb` command                |
| ([GTM-9190](#GTM-9190)) | Other                 | Object files conform to current ELF standards                                                                       |
| ([GTM-9193](#GTM-9193)) | System Administration | Eliminate spurious message from MUPIP RECOVER                                                                       |
| ([GTM-9206](#GTM-9206)) | System Administration | Eliminate inappropriate MUPIP LOAD limit at 2\**32 records                                                           |

### Database

* <a name="x651"></a>Updates to database files that use the MM access method work correctly when there are concurrent database file extensions and termination of processes accessing the database using SIGKILL (SIG-9). Previously, processes updating database files under these conditions could in rare cases terminate with a SIGSEGV (SIG-11). Note that YottaDB strongly recommends against the use of SIGKILL to terminate processes, except as a last resort. This was only encountered in a development environment and not reported by a user. [YDB#651](https://gitlab.com/YottaDB/DB/YDB/-/issues/651)

* <a name="x661"></a>Sourcing `ydb_env_set` manages a three region database:

  1. A DEFAULT region in which empty string (`""`) subscripts are disabled. Except for global variables mapped to the YDBOCTO and YDBAIM regions, the properties of this region are unchanged.

  1. A YDBOCTO region, intended to be used by [the Octo SQL plugin](https://gitlab.com/YottaDB/DBMS/YDBOcto/) with the following properties:

	 -  Empty string subscripts are enabled.

	 - `^%ydbOcto*` global variables (with all combinations of capitalization of `"Octo"`) are mapped to YDBOCTO.

	 - The key size is 1019 bytes and the record size is 1MiB.

	 - The default database filename is `$ydb_dir/$ydb_rel/g/%ydbocto.dat` and the default journal file is `$ydb_dir/$ydb_rel/g/%ydbocto.mjl`.

	 - The block size is 2KiB, with an initial allocation of 10000 blocks, extended by 20000 blocks.

	 - 2000 global buffers.

     Except for these differences, the properties of the YDBOCTO region are the same as those of the DEFAULT region.

  1. A YDBAIM region, intended to be used by [the Application Independent Metadata plugin](https://gitlab.com/YottaDB/Util/YDBAIM) with the following properties:

	 - Empty string subscripts are enabled.

	 - `^%ydbAIM*` global variables (with all combinations of capitalization of `"AIM"`) are mapped to YDBAIM.

	 - The key size is 992 bytes and the record size is 1008 bytes.

	 -  The default database filename is `$ydb_dir/$ydb_rel/g/%ydbaim.dat`. Journaling is not enabled by default, as Application Independent Metadata can be (re)created from application data at any time, on demand.

	 - The block size for YDBAIM is 1KiB, with an initial allocation of 20000 blocks, extended by 40000 blocks.

	 - The YDBAIM region uses the [MM access method](https://docs.yottadb.com/AdminOpsGuide/gde.html#ac-cess-method-code).

	 - Sourcing `ydb_env_set` does not create the database file. The YDBAIM region has [AutoDB](https://docs.yottadb.com/AdminOpsGuide/gde.html#no-au-todb) set in the global directory and the first access to a global variable mapped to the YDBAIM region automatically creates the database file.

	 - Sourcing `ydb_env_set` when recovering from an unclean shutdown (such as when coming back up from a system crash) deletes the YDBAIM region, whereas it performs RECOVER / ROLLBACK BACKWARD on the DEFAULT and YDBOCTO regions. To make YDBAIM region recoverable, [change the access method to BG](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#access-method), and [enable and turn on before-image journaling](https://docs.yottadb.com/AdminOpsGuide/ydbjournal.html#set-action-qualifiers).

   Previously, the default database had just the DEFAULT region, and initialized YottaDB in M mode. To continue using M mode (e.g., if your application code requires it), set the environment variable `ydb_chset` to `"M"` and export it before sourcing `ydb_env_set`.

   Additionally:

   - The default mode is UTF-8 if YottaDB was installed with UTF-8 support. Previously, it was M mode by default even if UTF-8 mode support was installed.

   - For UTF-8 mode, sourcing `ydb_env_set` checks whether a locale is set in the LC_ALL or LC_CTYPE environment variables. If not, it uses the first UTF-8 locale in the `locale -a` output, and terminates with an error if one is not found. Previously, UTF-8 mode would result in an error if a locale was not set.

   - In case of error, the location of the error file is output. Previously, its contents were dumped on the shell session.

   Sourcing `ydb_env_set` handles the case where replication is turned on. [YDB#661](https://gitlab.com/YottaDB/DB/YDB/-/issues/661)

* <a name="x663"></a>When a database region is configured for [nonstandard null subscript collation](https://docs.yottadb.com/AdminOpsGuide/gde.html#no-std-nullcoll), a MUPIP LOAD of a binary extract containing nodes with null subscripts created with MUPIP EXTRACT from another region so-configured loads the data. Previously it would incorrectly report a YDB-E-DBDUPNULCOL error. Note that YottaDB recommends against use of nonstandard null subscript collation; use it only for historical applications that require it. This was only encountered in a development environment and not reported by a user. [YDB#663](https://gitlab.com/YottaDB/DB/YDB/-/issues/663)

* <a name="x683"></a>ASYNCIO can be enabled for database files without issues. Owing to issues with the fix for [YDB#560](https://gitlab.com/YottaDB/DB/YDB/-/issues/560) in [r1.30](https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.30), it was possible in very rare cases for processes to hang and/or crash abnormally owing to multiple threads incorrectly running the YottaDB engine (which is not multi-thread safe) at the same time. This was only encountered in a development environment and not reported by a user. [YDB#683](https://gitlab.com/YottaDB/DB/YDB/-/issues/683)

* <a name="x725"></a>$GET(*gvn*) returns correct results on ARM platforms for database regions using the MM access method. Previously, it was possible in very rare cases for it to return an incorrect result. This was only encountered in a development environment and not reported by a user. [YDB#725](https://gitlab.com/YottaDB/DB/YDB/-/issues/725)

* <a name="x738"></a>[MUPIP FREEZE ONLINE](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#id4) ensures database and journal file integrity. Previously, in a rare case when the MUPIP FREEZE ONLINE process timed out waiting to get access, it was possible for the process to update database and/or journal files without an exclusive lock, which resulted in database/journal file structural damage. This was only encountered in a development environment and not reported by a user. [YDB#738](https://gitlab.com/YottaDB/DB/YDB/-/issues/738)

* <a name="x739"></a>MUPIP INTEG SUBSCRIPT handles database regions containing nodes with null (empty string) subscripts, when no end key is specified. Previously, it failed with a segmentation violation (SIG-11). This was only encountered in a development environment and not reported by a user. [YDB#739](https://gitlab.com/YottaDB/DB/YDB/-/issues/739)

* <a name="x743"></a>MUPIP INTEG ONLINE runs correctly on a journaled database. Starting with r1.26, it was possible in very rare cases for the MUPIP INTEG to terminate with a `%SYSTEM-E-ENO22, Invalid argument` error from a `shmdt()` call. This was only encountered in a development environment and not reported by a user. [YDB#743](https://gitlab.com/YottaDB/DB/YDB/-/issues/743)

### Languages

* <a name="x391"></a>The M function $ZYSU\[FFIX\]() returns a 128-bit [MurmurHash3](https://en.wikipedia.org/wiki/MurmurHash#MurmurHash3) of its string argument rendered as a 22 character alphanumeric (i.e., 0-9, a-z, A-Z) sequence suitable for concatenation to an application identifier (e.g., `"^%MyApp"`) to generate names for global variables, local variables, and routines that are unique for all practical purposes.

  Note that (a) YottaDB supports names that are unique in the first 31 characters, and (b) the function may return different sequences for the same argument string on different platforms. [YDB#391](https://gitlab.com/YottaDB/DB/YDB/-/issues/391)

* <a name="x558"></a>ZSHOW "*" is equivalent to ZSHOW "VIBDLGR". Previously it was equivalent to ZSHOW "IVBDLGR" which meant that when the output was directed to a local variable, that local variable included the potentially confusing ISV values that had already been stored in it. Note that this will result in slightly different output which may be more noticeable when the output is directed to a file or device rather than to a variable. [YDB#558](https://gitlab.com/YottaDB/DB/YDB/-/issues/558)

* <a name="x581"></a>A value of `"SYMLINK"` for the optional fifth parameter of $ZPARSE() specifies that if a valid file name that $ZPARSE() would otherwise return is a symbolic link, $ZPARSE() follows the link or a chain of links, and returns the file name of the actual file. [YDB#581](https://gitlab.com/YottaDB/DB/YDB/-/issues/581)

* <a name="x629"></a>The unary plus (+) operator on a local variable node whose $ZYSQLNULL value was set from a $ORDER() return has a value of $ZYSQLNULL. Previously a unary plus on a node value of $ZYSQLNULL set from a $ORDER() return had a value of zero (0). This was encountered in the development environment and was never reported by a user. [YDB#629](https://gitlab.com/YottaDB/DB/YDB/-/issues/629)

* <a name="x630"></a>Syslog entries logged by $ZSYSLOG() show `"YDB-"` followed by the process executable (`proc/<pid>/comm`) converted to upper case as the SYSLOG_IDENTIFIER field. Previously, this was `"YDB-MUMPS"`. [YDB#630](https://gitlab.com/YottaDB/DB/YDB/-/issues/630)

* <a name="x632"></a>If a multi-threaded application sets its own signal handlers for a fatal signal (SIGABRT, SIGBUS, SIGFPE, SIGILL, SIGINT, SIGIOT, SIGQUIT, SIGSEGV, or SIGTERM), and during the execution of a TP callback function (see [Transaction Processing](https://docs.yottadb.com/MultiLangProgGuide/MultiLangProgGuide.html#transaction-processing)) another thread receives and handles a fatal signal, [ydb_tp_st()](https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#ydb-tp-s-ydb-tp-st) returns a [[CALLINA]YDB_ERR_CALLIINAFTEREXIT](https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#ydb-err-callinafterxit) error to its caller. The same behavior occurred previously, but as the detection of this condition took longer, it was more likely that the process terminated before the caller of ydb_tp_st() received the return value, whereas now it is more likely to get the return value prior to process termination. Note that this condition is more likely to occur in processes with large numbers of threads and with runtime systems that set their own signal handlers. In particular, this is most likely to happen with Go applications. [YDB#632](https://gitlab.com/YottaDB/DB/YDB/-/issues/632)

* <a name="x633"></a>`ydb_incr_s()`/`ydb_incr_st()` work correctly when operating on a local variable node. In [r1.30](https://gitlab.com/YottaDB/DB/YDB/-/milestones/7), it was possible in rare cases for them to fail with a segmentation violation (SIGSEGV / SIG-11) owing to a regression in the code for [YDB#594](https://gitlab.com/YottaDB/DB/YDB/-/issues/). This was only encountered in a development environment and not reported by a user. [YDB#633](https://gitlab.com/YottaDB/DB/YDB/-/issues/633)

* <a name="x664"></a>VIEW "ZTRIGGER_OUTPUT":0 disables output from the $ZTRIGGER() function, while VIEW "ZTRIGGER_OUTPUT" or VIEW "ZTRIGGER_OUTPUT":1 enable this output. Processes start with the default behavior of the output enabled. The function $VIEW("ZTRIGGER_OUTPUT") returns 1 if this setting is enabled and 0 if disabled. [YDB#664](https://gitlab.com/YottaDB/DB/YDB/-/issues/664)

* <a name="x671"></a>MUPIP TRIGGER supports the following command line options:

   - `-stdin` reads input triggers to be set from stdin. `-stdin` is incompatible with any other option.
   - `-stdout` reports on triggers from the database to stdout. `-stdout` is only valid for `-select` and therefore is incompatible with the `-stdin`, `-triggerfile`, and `-upgrade` options.

   Previously, reading input triggers from stdin required a `-triggerfile=/proc/self/fd/0` command line option, and `cat /dev/null | mupip trigger -select` could be used to direct the output to stdout. [YDB#671](https://gitlab.com/YottaDB/DB/YDB/-/issues/671)

* <a name="x680"></a>A rare case of M lock acquisitions failing with SIGSEGV (SIG-11) has been eliminated. This was only encountered in a development environment and not reported by a user. [YDB#680](https://gitlab.com/YottaDB/DB/YDB/-/issues/680)

* <a name="x682"></a>The %HO utility program to convert strings representing hexadecimal numbers to strings representing octal numbers accepts strings starting with a case-independent `"0x"`. [YDB#682](https://gitlab.com/YottaDB/DB/YDB/-/issues/682)

* <a name="x685"></a>All base conversion utility programs convert input until the first invalid character and then ignore remaining characters. Previously, %OH and %OD would either treat invalid characters as if they were zero (0), and return either an incorrect conversion or an empty string. [YDB#685](https://gitlab.com/YottaDB/DB/YDB/-/issues/685)

* <a name="x688"></a>ZWRITE of M global variable nodes selected with pattern match operators from a database region with null (empty string) subscripts enabled works. Previously, it failed with the error message: `%YDB-E-LVUNDEF, Undefined local variable:`. This was only encountered in a development environment and not reported by a user. [YDB#688](https://gitlab.com/YottaDB/DB/YDB/-/issues/688)

* <a name="x700"></a>Multi-line triggers specified in the `-xecute` option of the [$ZTRIGGER](https://docs.yottadb.com/ProgrammersGuide/functions.html#ztrigger)("ITEM",\<trigspec\>) function accept a final line with `">>"`, with or without a trailing newline, making them consistent with multi-line triggers specified in files and loaded with [MUPIP TRIGGER](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#trigger) or $ZTRIGGER("FILE",<filespec\>). Also, "ITEM" trigger specifications allow `-piece` and `-delim` options to follow the `xecute` option. Previously, multi-line triggers specified using the "ITEM" specification required `-xecute` to be the last option in the trigger specification, and required the trigger specification to end with a newline. The workaround was to place the `-piece` and `-delim` specifications before the `-xecute` specification and to end the trigger specification with a newline. This was only encountered in a development environment and not reported by a user. [YDB#700](https://gitlab.com/YottaDB/DB/YDB/-/issues/700)

* <a name="x709"></a>The function $VIEW("GVFILE",\<region>) does not have the side effect of creating the database file for AutoDB regions. Previously, if AutoDB was set for a region, simply querying the file name of such a region with $VIEW() had the side effect of creating the database file where one did not exist. This was not the case if AutoDB was not set for the region. This was only encountered in a development environment and not reported by a user. [YDB#709](https://gitlab.com/YottaDB/DB/YDB/-/issues/709)

* <a name="x711"></a> $INCREMENT(*gvn*) works correctly within a transaction even if *gvn* has the [NOISOLATION](https://docs.yottadb.com/ProgrammersGuide/commands.html#noisolation-expr) feature turned on. Previously, if multiple processes attempted the $INCREMENT() at the same time, it was possible for the post increment value to be incorrectly set to `""` (i.e. the empty string), as the result of a regression introduced by the code for [YDB#353] in [r1.24](https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.24). This was only encountered in a development environment and not reported by a user. [YDB#711](https://gitlab.com/YottaDB/DB/YDB/-/issues/711)

* <a name="x712"></a> ERRWZTIMEOUT and ERRWZINTR errors that occur when a process is in direct mode are reported appropriately. Previously the error reporting was potentially confusing and misleading. Note that since $ZTIMEOUT and $ZINTERRUPT code (errors in whose execution lead to ERRWZTIMEOUT and ERRWZINTR respectively) is executed asynchronously, such an error in direct mode may require an ENTER to be pressed to see the direct mode prompt. This was only encountered in a development environment and not reported by a user. [YDB#712](https://gitlab.com/YottaDB/DB/YDB/-/issues/712)

* <a name="x715"></a>MUPIP JOURNAL ROLLBACK/RECOVER BACKWARD completes operation successfully if a database includes an [AutoDB](https://docs.yottadb.com/AdminOpsGuide/gde.html#no-au-todb) region, and a database file for that region does not exist, e.g., because application logic has not yet accessed a global variable mapped to that region. Previously, it would terminate with an error. The workaround was to create a region before running MUPIP JOURNAL ROLLBACK/RECOVER BACKWARD (and then delete it after the command), or to omit the region name for a MUPIP JOURNAL RECOVER BACKWARD. Note that MUPIP continues to report an error if all regions are AutoDB regions and no database files exist. [YDB#715](https://gitlab.com/YottaDB/DB/YDB/-/issues/715)

* <a name="x721"></a>LKE SHOW displays all information for an M lock on one line. Previously it split the information for locks with long resource names across two lines, while using one line for locks with short resource names. [YDB#721](https://gitlab.com/YottaDB/DB/YDB/-/issues/721)

* <a name="x724"></a>$ZATRANSFORM() determines whether or not the first argument is a string based on its value, without consideration of the fourth argument. Previously, it used the fourth argument to determine whether to treat the first argument as a string or not and thus whether it went through transformation or not. Also, $ZATRANSFORM() correctly transforms a numeric first argument regardless of whether the value is a literal value or a computed value. Previously, computed numeric values could be mishandled returning incorrect results. [YDB#724](https://gitlab.com/YottaDB/DB/YDB/-/issues/724)

* <a name="x727"></a>If trigger logic on an originating instance sets the [$ZTWORMHOLE intrinsic special variable](https://docs.yottadb.com/ProgrammersGuide/isv.html#ztwormhole), that value is propagated in the replication stream to receiving instances. Previously, any value $ZTWORMHOLE value set within a trigger was ignored. Note that use of $ZTWORMHOLE requires some care, as it is intended for specialized use cases. In general, applications should use global variables to propagate updates down a replication stream.

- As before, if application code sets $ZTWORMHOLE before a trigger is invoked, it is propagated downstream only if trigger logic references it.

- If there are values assigned to $ZTWORMHOLE at multiple times in trigger logic, or in both the application and the trigger, the value at the time the trigger completes execution is propagated.

- Use of this advanced functionality requires a little additional logic in trigger design. A trigger will need to distinguish between running on an originating primary instance, where it would set $ZTWORMHOLE vs. on a replicating secondary instance, where it would use the provided $ZTWORMHOLE. An efficient way to accomplish this is for applications to set different values for the environment variable [ydb_sysid](https://docs.yottadb.com/AdminOpsGuide/basicops.html#ydb-sysid) on originating and replicating instances, and for the trigger code to check the value of the [$SYSTEM intrinsic special variable](https://docs.yottadb.com/ProgrammersGuide/isv.html#system) whose second piece is set at process startup from `$ydb_sysid`. [YDB#727](https://gitlab.com/YottaDB/DB/YDB/-/issues/727)

* <a name="x731"></a>$VIEW("WORDEXP") uses [wordexp()](https://linux.die.net/man/3/wordexp) functionality for shell-like expansion of strings. For example `$VIEW("WORDEXP","~ $SHELL")` returns the string `/home/ydbuser /bin/bash` in case the process has `ydbuser` as the userid with a home directory of `/home/ydbuser` and `/bin/bash` as its login shell. [YDB#731](https://gitlab.com/YottaDB/DB/YDB/-/issues/731)

* <a name="x737"></a>Matching its behavior in M code invoked from M code, [$QUIT](https://docs.yottadb.com/ProgrammersGuide/isv.html#quit) in M code invoked from non-M code evaluates to 1 if the caller expects a value to be returned, and 0 if it does not. Previously, $QUIT was always 0 for M code invoked from non-M code. [YDB#737](https://gitlab.com/YottaDB/DB/YDB/-/issues/737)

* <a name="x742"></a>A [ZHALT](https://docs.yottadb.com/ProgrammersGuide/commands.html#zhalt) in M code called by C code using a `ydb_ci*()` function causes the `ydb_ci*()` have as its status (the `ydb_status_t` return value of the `ydb_ci*()` function) the ZHALT argument. A [HALT](https://docs.yottadb.com/ProgrammersGuide/commands.html#halt) causes `ydb_ci*()` to return a status of zero (0). Previously, the ZHALT argument would show up as the return value of the corresponding function specified in the [call-in table](https://docs.yottadb.com/ProgrammersGuide/extrout.html#call-in-table), if the function provided for a return value in the call-in table, and a NOTEXTRINSIC error otherwise, and also initialized return/output parameters of the C function. Note that:

   - Wrappers for other languages, such as Go, Perl, Python, and Rust all use `ydb_ci*()` if they call M code.

   - When invoked through `ydb_ci*()`, a ZHALT argument can be a 4-byte integer with the entire 4-byte value returned. As before, a ZHALT that returns to the shell (i.e., M code directly invoked from the shell) has a 0-255 value as its status.

   - When `ydb_ci*()` returns a status of a non-zero value, any return/output type parameters should be assumed uninitialized, whereas they are set for zero return values.
   [YDB#742](https://gitlab.com/YottaDB/DB/YDB/-/issues/742)

* <a name="x744"></a>%CDS^%H correctly converts $HOROLOG dates greater than 94657. Previously, it returned an empty string in %DAT. This was only encountered in a development environment and not reported by a user. [YDB#744](https://gitlab.com/YottaDB/DB/YDB/-/issues/744)

* <a name="x749"></a>Transactions that generate more than 1GiB of logical update journal records, abort with a TRANSREPLJNL1GB error. Previously such transactions would terminate the process with a %YDB-F-MEMORY, %YDB-F-GTMASSERT2, or raise errors such as %YDB-E-JNLCTRL or %YDB-E-REPLJNLCLOSED. That last error would turn off replication in a replicated instance, requiring a new replicating secondary instance to be created from a backup of another instance. Since an ACID transaction is normally a conceptually integral, atomic unit of work, a transaction that generates more than 1GiB of logical update records may be buggy or inappropriately designed. The most plausible application scenario where a transaction is genuinely likely to generate such large volumes of journal update records is one that sets multiple nodes with large values. For such applications, consider compressing the data stored in the global variable nodes. M applications can use the [zlib plugin](https://gitlab.com/YottaDB/Util/YDBZlib) to compress data; other languages can call compression libraries directly. [YDB#749](https://gitlab.com/YottaDB/DB/YDB/-/issues/749)

* <a name="x755"></a>A rare case involving a MUPIP REORG TRUNCATE concurrent with internal code that is recovering from unclean process terminations, such as a `kill -KILL`, three consecutive MUPIP STOPs issued to a process, an abnormal termination, or manually running DSE CACHE RECOVER does not cause structural database damage. This was only observed in special stress tests in the development environment and was never reported by a user. Note that YottaDB strongly recommends against manually aborting processes, and DSE CACHE RECOVER should only be used by an expert under guidance from your YottaDB support channel. [YDB#755](https://gitlab.com/YottaDB/DB/YDB/-/issues/755)

* <a name="GTM-1044"></a>ZATRANSFORM() recognizes 2 (two) and -2 (minus two) as optional third arguments. 2 specifies the return of the character which collates immediately after the first character of the first argument, or the empty string if no character does. -2 specifies the return of the character which collates immediately before the first character of the first argument, or the empty string if no character does. These specifiers work in M mode for the 'M' collation (collation 0), or any user defined collation which supplies the necessary plugin functionality. In UTF-8 mode, these argument values produce a ZATRANSCOL error. If the plugin for the specified collation does not support this "next character" functionality, the function produces a COLLATIONUNDEF error in response to an attempt to invoke it. The previously supported specifiers 0 & 1 continue to work as documented. This change should not affect existing code unless it relies on the undocumented behavior that any non-zero value, and in specific 2 or -2, given as optional argument three invoked the reverse subscript collation operation. 1 is the value documented to do so. If an external collation library is used and encounters a gtm\_ac\_xutil() failure, ZATRANSFORM() produces an ERR\_ZATRANSCOL. If an external collation library is used and does not supply a gtm\_ac\_xutil() function, ZATRANSFORM() produces an ERR\_COLLATIONUNDEF when operations -2 or 2 are specified. To use these operations with external collation libraries, the libraries must supply a function called 'gtm\_ac\_xutil()' with the following signature and characteristics:

```
long gtm_ac_xutil (gtm32_descriptor *in, int level, gtm32_descriptor *out, int *outlen, int op, int honor_numeric)
  The parameter in - Supplies the input string, the first character of which is considered
  The parameter level - Currently unused and should not be examined or changed
  The parameter out - Supplies the one (1) character result string produced by applying the collation operation if a result was possible
  The parameter outlen - Supplies to the caller the length of the returned string, which will be 0 or 1.
  The parameter op - Specifies the collation operation returned:
    0 - collation value of the given character
    1 - character collating before the given character if it exists
    2 - character collating after the given character if it exists
  The parameter honor_numeric - Boolean specifying:
    TRUE  - standard YottaDB collation of digits before any other character
    FALSE - digits should be treated the same as all other characters
  The gtm_ac_xutil function returns 0 on success and -1 on failure.
```
   ([GTM-1044](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-010_Release_Notes.html#GTM-1044))

* <a name="GTM-9113"></a>After XECUTE of a literal argument containing a FOR with a termination value for a control variable argument, YottaDB processes the rest of the line. Previously, YottaDB ignored anything on the line after such a literal argument. ([GTM-9113](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-009_Release_Notes.html#GTM-9133))

* <a name="GTM-9123"></a>YottaDB produces a correct result with ydb\_side\_effect set to one (1) or two (2) for a Boolean expression with a superfluous leading plus (+) or minus (-) sign on a parenthetical sub-expression containing at least one non-relational Boolean operator, and an extrinsic someplace other than the first element in the Boolean expression. Previously, YottaDB releases r1.20 and up could produce an incorrect result. ([GTM-9123](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-009_Release_Notes.html#GTM-9123))

* <a name="GTM-9126"></a>ZSTEP OVER stops after returning from a line containing an XECUTE or indirection; previously a ZSTEP OVER in such a situation acted like a ZSTEP OUTOF. ([GTM-9126](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-009_Release_Notes.html#GTM-9126))

* <a name="GTM-9149"></a>To protect the process, YottaDB turns any return values containing a null pointer to an empty string value and, for the first occurrence in a process, sends one XCRETNULLREF syslog message. Previously, in releases r1.28 and up, external calls that returned null pointers produced a XCRETNULLREF error. If an external call sets a ydb\_string\_t length to a negative value, to protect the process, YottaDB turns any return with a negative length to an empty string value and, for the first occurrence in a process, sends one XCCONVERT syslog message. Previously, such negative string lengths could lead to out-of-design conditionals like a segmentation violation (SIG-11). ([GTM-9149](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-009_Release_Notes.html#GTM-9149))

* <a name="GTM-9152"></a>The YottaDB compiler manages incorrect literal code in an XECUTE argument appropriately. Previously, in r1.30, owing to a regression caused by [GTM-9079](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-008_Release_Notes.html#GTM-9079), such a syntax could either cause a hang or a segmentation violation (SIG-11). ([GTM-9152](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-009_Release_Notes.html#GTM-9152))

* <a name="GTM-9155"></a>$SELECT() appropriately handles certain unusual cases involving extrinsics and nesting; previously these could cause a GTMASSERT2, a segmentation violation (SIG-11), or even an incorrect result. ([GTM-9155](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-009_Release_Notes.html#GTM-9155))

* <a name="GTM-9178"></a>YottaDB issues ERRWZTIMEOUT, along with the accompanying error description, in case of a run-time error in the ztimeout vector at the direct prompt. Previously, it issued GTMASSERT2 and created a core file. ([GTM-9178](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-010_Release_Notes.html#GTM-9178))

* <a name="GTM-9181"></a>The YottaDB compiler appropriately optimizes cases with one or more literal Boolean values prior to, or within, a $SELECT() function. The workaround was to resolve literal expressions when producing code, which could be a challenge if they are produced by a code generator. Previously, these cases, typically involving nested $SELECT() functions, could generate incorrect results or produce a GTMASSERT. ([GTM-9181](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-010_Release_Notes.html#GTM-9181))

* <a name="GTM-9183"></a>YottaDB supports indirect arguments that specify an exclusive operation to a NEW command on a line following a FOR command; previously such arguments produced an unsatisfactory result - typically a segmentation violation (SIG-11), but possibly some other YottaDB error. ([GTM-9183](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-010_Release_Notes.html#GTM-9183))

* <a name="GTM-9185"></a>ZLINK and auto-ZLINK consistently observe $ZCOMPILE qualifiers except when they are overridden by explicit ZLINK command qualifiers, and they consequently always report an error when the routine name is invalid. Previously they observed some $ZCOMPILE factors some of the time, and under some circumstances they eventually reported the routine could not be found. Also, explicit ZLINK observes $ZROUTINES conventions except when the command argument or $ZCOMPILE specifies otherwise; previously it placed the object in the current working directory. In addition, some messages associated with ZLINKFILE error contain additional context. ([GTM-9185](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-010_Release_Notes.html#GTM-9185))

### System Administration

* <a name="x497"></a>On a Supportable platform, following a successful build, the `ydbinstall` script in the built distribution installs YottaDB without requiring the `--force-install` option. Previously, on a Supportable platform, e.g., Arch Linux on x86_64, even after a successful build, `ydbinstall` required the `--force-install` option to be specified. Note that on platforms that are Supportable but not Supported, even a successfully built YottaDB is not necessarily tested prior to release. If you want to use YottaDB in production on a platform that is Supportable but not Supported, please ensure that it is adequately tested. Contact YottaDB to discuss making a Supportable platform Supported. [YDB#497](https://gitlab.com/YottaDB/DB/YDB/-/issues/497)

* <a name="x505"></a>Compiling M code with an `-object` qualifier that has no value specified reports a helpful `%YDB-E-CLIERR, Option : OBJECT needs value` error. Previously it reported the less helpful `%YDB-E-NOTMNAME` error. [YDB#505](https://gitlab.com/YottaDB/DB/YDB/-/issues/505)

* <a name="x551"></a>$ZSYSLOG() logs its parameter string verbatim in the syslog. Previously it would incorrectly attempt to process certain tags used within YottaDB message templates. [YDB#551](https://gitlab.com/YottaDB/DB/YDB/-/issues/551)

* <a name="x612"></a>A YottaDB process that is suspended when in direct (interactive) mode (e.g., with a Ctrl-Z) correctly restores terminal settings when resumed (e.g., with an `fg` shell directive). Previously it did not, potentially resulting in incorrect terminal handling. This was encountered in the development environment and was never reported by a user. [YDB#612](https://gitlab.com/YottaDB/DB/YDB/-/issues/612)

* <a name="x653"></a>READ with a timeout on a socket device always times out. In YottaDB [r1.30](https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.30), owing to a regression introduced by the code for [YDB#560], in certain rare cases it could hang indefinitely and not time out. This was only encountered in a development environment and not reported by a user. [YDB#653](https://gitlab.com/YottaDB/DB/YDB/-/issues/653)

* <a name="x657"></a>The reference implementation of the encryption plugin again supports TLS 1.3. In YottaDB [r1.30](https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.30) this was inadvertently disabled, resulting in TLS 1.2 being used even though OpenSSL 1.1.1 supports TLS 1.3. [YDB#657](https://gitlab.com/YottaDB/DB/YDB/-/issues/657)

* <a name="x673"></a>Owing to a more efficient implementation, locks use more shared memory. If the peak lock space usage of an application was previously within 10% of the allocated lock space for a region, you should increase the amount using the [MUPIP SET LOCK_SPACE](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#lock-space) command, which requires stand-alone access to database files. Remember to also change the setting in global directories with the [GDE LOCK_SPACE segment qualifier](https://docs.yottadb.com/AdminOpsGuide/gde.html#l-ock-space-integer) so that the next time the global directory is used by [MUPIP CREATE](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#create) to create new database files, they are created with the appropriate lock space allocation. As part of this change, the default lock space allocated by GDE is 220× 512-byte pages. Previously, the default allocation was 40× pages. [YDB#673](https://gitlab.com/YottaDB/DB/YDB/-/issues/673)

* <a name="x676"></a>When a replication Receiver Server or a MUPIP JOURNAL ROLLBACK FETCHRESYNC process waiting for a connection detects bad input, it resets the connection. Previously depending on the bad input it could fail, causing a core file, or loop producing a continuous stream of receiver server log messages taking significant amounts of file space. This was only encountered in a development environment and not reported by a user. [YDB#676](https://gitlab.com/YottaDB/DB/YDB/-/issues/676)

* <a name="x704"></a>The `yottadb` executable works with [valgrind](https://www.valgrind.org/). Previously it issued a `YDBDISTUNVERIF` error. Note that valgrind is intended for use in development and testing, rather than production. [YDB#704](https://gitlab.com/YottaDB/DB/YDB/-/issues/704)

* <a name="x718"></a>The `--aim` option of `ydbinstall` / `ydbinstall.sh` installs the [Application Independent Metadata](https://gitlab.com/YottaDB/Util/YDBAIM) plugin. [YDB#718](https://gitlab.com/YottaDB/DB/YDB/-/issues/718)

* <a name="x741"></a>`DSE REMOVE RECORD` works on blocks with `DBCOMPTOOLRG` integrity errors. Previously, it terminated abnormally with `SIGSEGV (SIG-11)` / `KILLBYSIGSINFO1` errors. This was only encountered in a development environment and not reported by a user. [YDB#741](https://gitlab.com/YottaDB/DB/YDB/-/issues/741)

* <a name="GTM-5333"></a>See [GTM-5626](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-010_Release_Notes.html#GTM-5626). ([GTM-5333](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-010_Release_Notes.html#GTM-5333))

* <a name="GTM-5626"></a>YottaDB replication indefinitely tries to reconnect after a communication error and issues syslog messages about any errors in its attempts. To stop the retries, explicitly stop replication. Previously, the replication servers exited without sending information about the connection issue to syslog. YottaDB prints additional connection information in case of communication errors, for easier debugging. ([GTM-5626](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-010_Release_Notes.html#GTM-5626))

* <a name="GTM-6631"></a>MUPIP REORG recognizes the NOCOALESCE, NOSPLIT and NOSWAP qualifiers, which respectively disable: increases in block density, decreases in block density, and improvements to physical adjacency. These are new options. ([GTM-6631](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-009_Release_Notes.html#GTM-6631))

* <a name="GTM-7113"></a>MUPIP ROLLBACK FETCHRESYNC tries to reconnect in case of a lost connection. Previously, under this condition, the rollback exited with a REPLCOMM error. ([GTM-7113](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-010_Release_Notes.html#GTM-7113))

* <a name="GTM-8203"></a>MUPIP REORG TRUNCATE recognizes the KEEP=|blocks|percent%| qualifier where the argument to KEEP specifies either a number of database blocks or a percentage (0-99) followed by a percent-sign (%) of the starting total blocks to exclude from release to the operating system. Previously, a truncation returned all of the available free space. ([GTM-8203](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-009_Release_Notes.html#GTM-8203))

* <a name="GTM-8322"></a>MUPIP SIZE recognizes the SU[BSCRIPT]=subscript qualifier to specify a global or a range of keys to estimate. The global key may be enclosed in quotation marks (""). Identify a range by separating two subscripts with a colon (:). The SE[LECT]=global-name-list and SU[BSCRIPT]=subscript qualifiers are incompatible. Scan heuristic for MUPIP SIZE shows information of all levels down through the target level. Previously, it showed details only for the target level. For all heuristics, the adjacency shown is of current level. Previously, the adjacency shown was for the next lower level. In addition, MUPIP INTEG correctly handles the case of NULL subscripts, when no end key is specified; previously, it failed with a segmentation violation (SIG-11). ([GTM-8322](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-010_Release_Notes.html#GTM-8322))

* <a name="GTM-8706"></a>MUPIP REPLICATE RECEIVER STOPRECEIVERFILTER turns off any active filter on the Receiver Server without turning off the Receiver Server; previously this qualifier was not supported. STOPRECEIVERFILTER is not compatible with any other RECEIVER qualifier. Using STOPRECEIVERFILTER, when no filter is active, returns a non-success return code. ([GTM-8706](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-009_Release_Notes.html#GTM-8706))

* <a name="GTM-8747"></a>The new CORRUPTDB qualifier for MUPIP JOURNAL EXTRACT extracts journal records into a single file even if the database is corrupt or missing. Always specify a journal file name when you are using CORRUPTDB. CORRUPTDB does not recognize the wildcard character "*" for journal file name and is incompatible with the FENCES, LOST, and BROKEN qualifiers. ([GTM-8747](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-010_Release_Notes.html#GTM-8747))

* <a name="GTM-8848"></a>Many MUPIP INTEG error reports include a "nature" that gives some guidance on the urgency of the error corresponding to documentation in the Maintaining Database Integrity chapter of the Administration and Operations Guide. YottaDB recommends addressing even "Benign" errors. Previously, INTEG did not report this information. ([GTM-8848](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-010_Release_Notes.html#GTM-8848))

* <a name="GTM-8901"></a>The GVPATFILE qualifier for MUPIP JOURNAL EXTRACT specifies the location of a pattern file containing a list of patterns for all types of SET journal records that MUPIP JOURNAL EXTRACT should include in, or exclude from, its output. Use this qualifier to restrict the output of a journal extract by global node content (value) in any SET record types). The format of the GVPATFILE qualifier is:

  ```
  GVPATFILE=path-to-pattern-file
  ```

  The following details the syntax of the pattern file and examples of how MUPIP JOURNAL EXTRACT responds:

	 - When a pattern entry starts with a tilde (~), GVPATFILE excludes the matching global node values from the JOURNAL EXTRACT file; for example: ~(not this value) excludes all global SETs that exactly match "not this value"
	 - When the pattern does not start a tilde (~) or contain an asterisk (*), MUPIP JOURNAL EXTRACT reports only those global SET values that exactly match the pattern. For example: " match this value"
	 - When a pattern contains an asterisk (\*), MUPIP JOURNAL EXTRACT expands it and tries to match multiple characters; for example: "\*a\*b*" matches values like "ab", "..ab", "ab.. ", "a..b", " aaabbabb", and so on but does not match values like "ba", "aaa", "bbb", and so on
	 - When a pattern contains a percentage (%), MUPIP JOURNAL EXTRACT matches it for one character; for example: "a%b%" matches values like "a1b1" but does not match values like "ab", "aabbc", and so on
	 - A pattern can be enclosed within parentheses "()" for readability
	 - When you use any of the following characters in the pattern, you can escape them by preceding the character with "\\"; for example: " a\\**b" matches values like "a\*..b" but not "a..b".:
		-  "(" and "~" at the beginning
		-  ")" at the end
		-  "\", "*" and "%" occurring anywhere within the pattern
	 - In UTF-8 mode, the contents of the pattern file can include Unicode characters
	 - If a pattern file does not exist, MUPIP JOURNAL EXTRACT produces the FILEOPENFAIL error and returns a non-zero exit code to the shell

  You can specify multi-line entries in a pattern file. With multiple lines, MUPIP JOURNAL EXTRACT produces those SET records that match any one of the pattern lines with the exception of exclusion patterns (those starting with ~) which take precedence over other non-exclusion patterns.

  Here are a few examples of the pattern file, and how MUPIP JOURNAL EXTRACT matches the pattern file values:

  ```
  $ cat matchA_notAA.txt
  ~(*AA*)
  *A*
  $ $ydb_dist/mupip journal -extract -gvpatfile=matchA_notAA.txt -forward "*"
  Extracts global values that contain at least one "A", but not "AA".

  $ cat ending22.txt
  *notmatching*
  *22
  $ $ydb_dist/mupip journal -extract -gvpatfile=ending22.txt -forward "*"
  Extracts global values ending with "22", even when there are no globals containing "notmatching".

  $ cat startswithsplchars.txt
  \**
  \~*
  $ $ydb_dist/mupip journal -extract -gvpatfile=matchA_notAA.txt -forward "*"
  Extracts global values that start with a "*" or a "~".
  ```

  ([GTM-8901](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-009_Release_Notes.html#GTM-8901))

* <a name="GTM-8921"></a>YottaDB releases the semaphore IPCs associated with the region "ftok" and any associated statsDB when closing a file with the READ ONLY characteristic; previously it did not. Remember the READ\_ONLY characteristic only applies to MM access method regions. Note that because of the way YottaDB handles semaphores for READ\_ONLY database files it does not enforce standalone access for setting the characteristic from READ\_ONLY to NOREAD\_ONLY. However, making a change from READ\_ONLY to NOREAD\_ONLY without standalone access will likely cause problems, such as errors on termination and failure to release IPC resources that would require additional MUPIP commands to clean up. Therefore, YottaDB recommends using other means such as the following based on `fuser` return of one (1) to verify there are no processes are accessing the file:

  ```
  fuser <db-filename> | awk -F: '{if(length($NF))exit(1)}' && mupip set -noread_only -file <db-filename>
  ```

  In addition, argumentless MUPIP RUNDOWN sends fewer cautions when encountering conditions created by use of READ\_ONLY database files. ([GTM-8921](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-009_Release_Notes.html#GTM-8921))

* <a name="GTM-9037"></a>When YottaDB encounters certain problems with journaling that generate errors, and possibly cause an Instance Freeze in lieu of shutting down journaling, it ensures there is a message about the error in the syslog; previously there were cases when it did not do so. ([GTM-9037](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-009_Release_Notes.html#GTM-9037))

* <a name="GTM-9044"></a>When `gtmsecshr` is misconfigured, an attempt by a process to start it produces a message with some guidance. Note that security policy indicates that because `gtmsecshr` is a secured component detailed information is not appropriate. Previously the message was less helpful. ([GTM-9044](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-009_Release_Notes.html#GTM-9044))

* <a name="GTM-9076"></a>YottaDB produces a FILEPATHTOOLONG error when it is unable to create a new (or temporary) file because of the file name (including the path) exceeding 255 chars. FILEPATHTOOLONG replaces PARBUFSM where ever it was issued but uses the same message ID. Also, the text for FNTRANSERROR now reads: `Filename including path exceeded 255 chars while trying to resolve filename ffff`, where ffff is the file name. Previously, YottaDB did not issue PARBUFSM errors when file/path exceeded 255 characters while creating journal files or global directories. ([GTM-9076](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-010_Release_Notes.html#GTM-9076))

* <a name="GTM-9116"></a>The YottaDB installation script explicitly provides read-execute permissions to `libgtmutil.so`. Previously permissions were not explicitly provided to `libgtmutil.so`, and so the permissions depended on umask settings. ([GTM-9116](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-009_Release_Notes.html#GTM-9116))

* <a name="GTM-9142"></a>Please see [GTM-6631](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-009_Release_Notes.html#GTM-6631). ([GTM-9142](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-009_Release_Notes.html#GTM-9142))

* <a name="GTM-9144"></a>Loading a binary extract in which duplicate null-subscript globals exist with both YottaDB and standard null subscript collation triggers a YDB-E-DBDUPNULCOL error. Previously, performing this action could cause the YottaDB process to terminate with a segmentation violation (SIG-11). This issue was only observed in the development environment, and was never reported by a user. ([GTM-9144](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-009_Release_Notes.html#GTM-9144))

* <a name="GTM-9166"></a>JNLPROCSTUCK warnings go to the syslog to indicate that journal file writes are taking longer than anticipated. These warnings do not represent a YottaDB issue nor a failure per se. Previously, they were accompanied by a JNLFLUSH message, which, because YottaDB uses it to report other conditions, remains available as a custom error indication for initiating an Instance Freeze. ([GTM-9166](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-010_Release_Notes.html#GTM-9166))

* <a name="GTM-9180"></a>In order to support a change in an upcoming release DSE internally handles block numbers as 64-bit entities. For safety, it gives an error if it encounters a block number larger than currently supported. Previously the field for the block number when displaying a bitmap, was not as wide. ([GTM-9180](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-010_Release_Notes.html#GTM-9180))

* <a name="GTM-9193"></a>YottaDB issues appropriate error messages during an interrupted MUPIP RECOVER. Previously, in rare instances, YottaDB could issue an error indicating it could not delete a file that had already been deleted. This issue was only observed in the YottaDB development environment, and was never reported by a user. ([GTM-9193](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-010_Release_Notes.html#GTM-9193))

* <a name="GTM-9206"></a>MUPIP LOAD can handle a maximum of 2\*\*64 records including the header records. BIN format has one header record and GO and ZWR formats each have two. The GO format uses two records per node and ZWR uses one, but BIN format typically carries many nodes per record. Previously, MUPIP LOAD terminated without error when it reached a record count of 4294967293 even when the LOAD was incomplete because there were more records in the extract. The workaround was to limit the extract to less than 2\*\*32 records (including the header) by selecting globals and/or using temporary global directories, and/or modifying the extracts. ([GTM-9206](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-010_Release_Notes.html#GTM-9206))

### Other

* <a name="x441"></a>[Auto-ZLINK](https://docs.yottadb.com/ProgrammersGuide/commands.html#auto-zlink) works properly when linking a newer version of an M routine after changing [$ZROUTINES](https://docs.yottadb.com/ProgrammersGuide/isv.html#zroutines). Previously, it could in rare cases terminate the process abnormally with a fatal GTMCHECK error and a core dump. This was encountered in the development environment and was never reported by a user. [YDB#441](https://gitlab.com/YottaDB/DB/YDB/-/issues/441)

* <a name="x634"></a>When Octo is installed by `ydbinstall` / `ydbinstall.sh` using its `--octo` option, `octo --version` for that installed Octo reports the git commit hash of the Octo build. Previously, it reported `Git commit: GIT-NOTFOUND`. [YDB#634](https://gitlab.com/YottaDB/DB/YDB/-/issues/634)

* <a name="x635"></a>MUPIP INTEG with no options terminates in response to Ctrl-C (SIGINT). Previously, it would ignore the signal, and termination required it to be suspended to put it in the background, followed by a `kill` (SIGTERM) to terminate it. [YDB#635](https://gitlab.com/YottaDB/DB/YDB/-/issues/635)

* <a name="x678"></a>The environment variable `ydb_treat_sigusr2_like_sigusr1`, when set to a non-zero value, "yes" or "TRUE" (case insensitive), or a leading substring of "yes" or "true", causes a YottaDB process to treat a USR2 signal just as it would a SIGUSR1 (by invoking $ZINTERRUPT mechanism). The default behavior is to ignore SIGUSR2. When executing $ZINTERRUPT (i.e. if $ZININTERRUPT is 1), the read-only intrinsic special variable $ZYINTRSIG is `"SIGUSR1"` or `"SIGUSR2"` depending on the signal that caused it to be executed. If $ZININTERRUPT is 0, $ZYINTRSIG is the empty string `""`. [YDB#678](https://gitlab.com/YottaDB/DB/YDB/-/issues/678)

* <a name="x679"></a>YottaDB gracefully shuts down when an otherwise healthy, active process receives a process-terminating signal. Previously, under certain rare conditions, the process could terminate with a SYSCALL and/or SIGSEGV (SIG-11) failure, or hang. This was only encountered in a development environment and not reported by a user. [YDB#679](https://gitlab.com/YottaDB/DB/YDB/-/issues/679)

* <a name="x692"></a>The modulo operator (#) returns a purely numeric value in case the dividend is a numeric portion followed by a non-numeric portion and the divisor is greater than the numeric portion of the string. Previously it would incorrectly return the result with the non-numeric portion appended. [YDB#692](https://gitlab.com/YottaDB/DB/YDB/-/issues/692)

* <a name="x695"></a>Orphaned relinkctl files associated with an object directory (see [Auto-Relink Setup](https://docs.yottadb.com/ProgrammersGuide/commands.html#auto-relink-setup)) are automatically cleaned up on a subsequent reuse of that object directory. Previously this required a [MUPIP RUNDOWN RELINKCTL](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#relinkctl) command, or explicit deletion of the orphaned files. An orphaned relinkctl file exists if the last YottaDB process using it does not shutdown cleanly, typically the result of a SIGKILL, but also possible with SIGINT (sent by Ctrl-C) or SIGTERM (sent by [MUPIP STOP](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#stop)). [YDB#695](https://gitlab.com/YottaDB/DB/YDB/-/issues/695)

* <a name="x719"></a>Sourcing `ydb_env_set` always sets the `ydb_dist` environment variable to the same (parent) directory whether or not YDB is configured for UTF-8 mode or M mode. Previously it was set to the `utf8/` subdirectory for UTF-8 mode, and the parent directory for M mode. [YDB#719](https://gitlab.com/YottaDB/DB/YDB/-/issues/719)

* <a name="x730"></a>[%PEEKBYNAME()](https://docs.yottadb.com/ProgrammersGuide/utility.html#peekbyname) provides a way to examine segment fields in the global directory using the `gd_segment` structure. For example, `$ZPIECE($$^%PEEKBYNAME("gd_segment.fname","AREG"),$ZCHAR(0),1)` returns the database file name corresponding to `AREG` region in the global directory. This functionality differs from that of [$VIEW("GVFILE")](https://docs.yottadb.com/ProgrammersGuide/functions.html#argument-keywords-of-view) as the latter opens the database file whereas %PEEKBYNAME() only examines the global directory. Since the database file name stored in the global directory can contain environment variables (e.g. `$ydb_rel/$ydb_dir/g/yottadb.dat` for a global directory created by sourcing `ydb_env_set`), %PEEKBYNAME() returns the string with the unexpanded environment variable names. [YDB#730](https://gitlab.com/YottaDB/DB/YDB/-/issues/730)

* <a name="GTM-9114"></a>Auto-zlink gives an error when the invocation name and the compilation name of the object don't match. Owing to a flaw in GTM-8178 in r1.28, auto-zlinking of an object module copied or moved to another name caused a GTMCHECK. ([GTM-9114](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-009_Release_Notes.html#GTM-9114))

* <a name="GTM-9115"></a>The code invoked for longer radix is faster, and the other radix conversion routines (%DH, %DO, %HD, %HO, %OD, %OH) have spelled out keywords. Those other routines now have a single working code block in an attempt to make them as fast as possible, given their documented behavior. Note that if your application limits the input(s) and performance is important, you can get some speed improvement by eliminating behavior on which your code does not rely. While the conversions work for very long values, the performance degrades with length and may be impractical beyond some point. Previously, the longer conversion code had more abstraction, and the other routines used two code blocks, rather than one. Also, if [$ZCONVERT()](https://docs.yottadb.com/ProgrammersGuide/functions.html#zconvert) meets your requirements, it provides the fastest conversion. ([GTM-9115](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-009_Release_Notes.html#GTM-9115))

* <a name="GTM-9134"></a>When a replication Receiver Server waiting for a connection detects bad input, it resets the connection. Previously depending on the bad input it could fail, causing a core file or loop producing a continuous stream of receiver server log messages taking significant amounts of file space. ([GTM-9134](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-009_Release_Notes.html#GTM-9134))

* <a name="GTM-9145"></a>^%RO and ^%RI handle lines of up to 1MiB. Note that the current supported maximum code line length is 8KiB and the YottaDB compiler automatically breaks longer lines up and issues warnings when lines exceed 8KiB. Previously ^%RO and ^%RI limited lines to 2044 bytes. ([GTM-9145](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-009_Release_Notes.html#GTM-9145))

* <a name="GTM-9188"></a>A `yottadb` command invocation ignores extra spaces between a hyphen (-) and subsequent qualifier name for `-run` or `-direct` when determining the value of $ZCMDLINE. Note, as before, if you use both `-direct` and `-run` on the same command, YottaDB always attempts to enter Direct Mode, and $ZCMDLINE may have interesting contents. Previously spaces in between the hyphen and the keyword caused the qualifier name to appear in $ZCMDLINE. ([GTM-9188](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-010_Release_Notes.html#GTM-9188))

* <a name="GTM-9190"></a>YottaDB produces object files consistent with ELF standards. Previously, ELF tools such as the linker could emit warnings or errors when operating on YottaDB-produced object files. ([GTM-9190](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-010_Release_Notes.html#GTM-9190))

## Messages

### From GT.M V6.3-009
**INVGVPATQUAL**, Invalid Global Value Pattern file qualifier value

MUPIP Error: This indicates that GVPATFILE did not specify a valid file name. The maximum file name length is 256.

Action: Specify a valid file name with the appropriate path.

**MLKREHASH**, LOCK hash table rebuilt for region rrrr (seed = ssss)

Run Time Information: YottaDB has detected an issue with the LOCK hash table for region rrrr and regenerated it using a new seed value ssss.

Action: This information message confirms the sucess of the rehash operation. No further action is necessary unless it is issued repeatedly or with a large seed value.

**MUKEEPNODEC**, Expected decimal integer input for keep

MUPIP Error: The value for the MUPIP REORG -keep qualifier does not have the appropriate syntax.

Action: Revise the argument for -keep= to be a decimal integer number of blocks, or a 0-99 percentage followed by a percent sign (%).

**MUKEEPNOTRUNC**, Keep issued without -truncate

MUPIP Error: The -keep qualifier for MUPIP REORG only applies when used with -truncate.

Action: Adjust the MUPIP REORG command qualifiers to provide a valid combination.

**MUKEEPPERCENT**, Keep threshold percentage should be from 0 to 99

MUPIP Error: The MUPIP REORG KEEP= qualifier can accept either a number of blocks or a percentage from 0% to 99%.

Action: If you wish to specify a number of blocks, remove the trailing %; if you wish to use a percentage, ensure it is within range.

**MUTRUNCNOSPKEEP**, Region rrrr has insufficient space to meet truncate target percentage of pppp with keep at bbbb blocks

MUPIP Information: MUPIP REORGE KEEP for region rrrr could not meet the specified percentage pppp so it left all the available blocks bbbb.

Action: None required, other than evaluating the space situation for the region and file system to ensure that it is wholesome and does not require additional intervention.

**NOJNLPOOL**, No journal pool info found in the replication instance of xxxx

Run Time/MUPIP Error: This indicates that YottaDB / MUPIP did not get replication information from the instance file specified. Replication instance file was not initialized because replication did not start, or some other process reset the replication instance file.

Action: Start the Source Server if it was not started. Note that the first Source Server process creates the Journal Pool. Subsequent Source Server processes use the Journal Pool that the first Source Server process creates. If the source server was running, stop the server and perform an optimum recovery using MUPIP JOURNAL -ROLLBACK -BACKWARD "\*" and restart the Source Server. If optimum recovery command fails, perform a MUPIP RUNDOWN (or a MUPIP RUNDOWN -REGION "*"), and then restart the Source Server.

**NULLPATTERN**, Empty line found in the Pattern file

MUPIP Warning: MUPIP JOURNAL EXTRACT pattern file contained an empty line, which generates this message.

Action: Remove the empty line

### From GT.M V6.3-010

**DSEINVALBLKID**, Trying to edit DB with 64-bit block IDs using pre-V7 DSE

DSE Error: Indicates a mismatch between the version of DSE and the version of the target database

Action: Use the version of DSE that matches the database.

**FNTRANSERROR**, Filename including path exceeded 255 chars while trying to resolve filename FFFF

All GT.M Components Error: While creating a database, resolving environment variables in a database path exceeded the maximum supported file name size.

Action: Reduce the path size by altering base components of the path or database name and/or the values of the environment variables to create a shorter overall filename and retry.

**VERMISMATCH**, Attempt to access xxxx with version yyyy, while already using zzzz

Run Time Error: The database file xxxx is currently marked as in use by version ($ZVERSION) zzzz, but the process attempted to access the database file with version yyyy. YottaDB prevents this as different versions typically have some incompatibility in their use of the database file. This error may also occur when you make a copy of the database without performing a RUNDOWN and attempt to access it with a newer GT.M version.

Action: If you encounter this error while upgrading a database file, perform the appropriate MUPIP action using the prior YottaDB version to ensure that there are no processes/semaphores attached to the database file.

**ZATRANSCOL**, The collation requested has no implementation for the requested operation

Run Time Error: The $ZATRANSFORM() function requested a next or previous collation character, but the collation transform module involved does not implement that functionality.

Action: Enhance the collation transform plug-in in question to support the desired functionality

### From YottaDB

**FILEPATHTOOLONG**, Filename including the path cannot be longer than 255 characters

 Run Time Error: This indicates that the filename provided was longer than 255 characters.

 Action: Update the filename with length no longer than 255 characters.

 *FILEPATHTOOLONG replaces PARBUFSM*

**TRANSREPLJNL1GB**, Transaction can use at most 1GiB of replicated journal records across all journaled regions

 Run Time Error: This indicates that a transaction attempted to create more than 1GiB of journal update records, which are replicated by Source Server processes, across all the regions updated by the transaction.

 Action: Transactions are intended to be small, conceptually integral units of work. A transaction that generates 1GiB of journal update records is indicative of an application flaw. Correct the application accordingly.

**WORDEXPFAILED**, wordexp() call for string SSSS returned TTTT error. See wordexp() man pages for details

 Run Time Error: This indicates that the command $VIEW("WORDEXP",TTTT), received the TTTT error code from the OS wordexp() function which it called.

 Action: Refer to the OS wordexp() documentation to identify the error and correct the application accordingly.

**ZCCONVERT**, External call: error converting output argument from external call

 Run Time Error: This indicates that an external call failed because an output argument supplied by the external routine did not match the corresponding output description in the external call table.

 Action: Change the external call table or the called routine so that they correspond.

## Legal Stuff

Copyright © 2021 YottaDB LLC

Permission is granted to copy, distribute and/or modify this document under the terms of the [GNU Free Documentation License, Version 1.3](http://www.gnu.org/licenses/fdl.txt) or any later version published by the Free Software Foundation; with no Invariant Sections, no Front-Cover Texts and no Back-Cover Texts.

YottaDB® and Octo® are registered trademarks of YottaDB LLC.
GT.M™ is a trademark of Fidelity National Information Services, Inc.
Other trademarks belong to their respective owners.

This document contains a description of YottaDB and the operating instructions pertaining to the various functions that comprise the software. This document does not contain any commitment of YottaDB LLC. YottaDB LLC believes the information in this publication is accurate as of its publication date; such information is subject to change without notice. YottaDB LLC is not responsible for any errors or defects.
