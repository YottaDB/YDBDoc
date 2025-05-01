<!---
.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2022-2025 YottaDB LLC and/or its subsidiaries.#
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

# YottaDB r1.34

### Release Note Revision History

| Revision  | Date              | Summary               |
| --------- | ----------------- | --------------------- |
| 1.00      | February 25, 2022 | r1.34 Initial Release |
| 1.01      | May 8, 2025 | Change "smart" quotes to simple double quotes |

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

### r1.34

#### Overview

While YottaDB r1.34 is an otherwise modest successor to [r1.32](https://gitlab.com/YottaDB/DB/YDB/-/releases/r1.32), internal changes allow the popular programming language [Python](https://www.python.org/) to be fully Supported. We are excited about making YottaDB available to the large Python user community. Here is a "Hello, World" Python program that updates the database and illustrates YottaDB's Unicode support:

```
	import yottadb

	if __name__ == "__main__":
		yottadb.set("^hello", ("Python",), value="नमस्ते दुनिया")
```

The Python wrapper can be installed with `pip install yottadb`. Full details of the API are in the [Python wrapper user documentation](https://docs.yottadb.com/MultiLangProgGuide/pythonprogram.html). The current [Debian Docker image at Docker Hub](https://hub.docker.com/r/yottadb/yottadb-debian) includes the Python wrapper. We thank Peter Goss (@gossrock) for his contributions to the Python wrapper.

Python joins C, Go, M, node.js, Perl, and Rust as languages with APIs to access YottaDB.

*Owing to an internal change required to support the Python wrapper, application code written in Go and Rust will need to be compiled with new versions of the Go and Rust wrappers. We anticipate no regressions, and apologize for the inconvenience.*

As discussed in our blog post [Fuzz Testing YottaDB](https://yottadb.com/fuzz-testing-yottadb/), adding a new type of testing exposed bugs previously neither encountered in our development environment nor reported by a user. Although fuzz testing generates syntactically correct but semantically questionable, contorted code that is unlikely to be part of any real application, the bugs are nevertheless defects. r1.34 includes the first tranche of fixes. As we are dedicating hardware to continuous fuzz testing, future YottaDB releases will include fixes for bugs found by fuzzing. We thank Zachary Minneker of [Security Innovation](https://www.securityinnovation.com/) for Fuzz Testing YottaDB and bringing its benefits to our attention.

In addition to fixes for issues, whether found by fuzz testing ([828](#x828)) or otherwise, YottaDB r1.34 has enhancements that make it faster and more friendly, e.g.,

- Faster stringpool garbage collection, thanks to Alexander Sergeev and Konstantin Aristov (@littlecat). ([786](#x786))
- HOME and END keys work in direct mode for READ, thanks to Sergey Kamenev (@inetstar). ([803](#x803))
- Multiple improvements to `ydbinstall` / `ydbinstall.sh`. ([705](#x705)) ([754](#x754)) ([764](#x764)) ([766](#x766)) ([829](#x829)) ([838](#x838))
- Enhancements to `ydb_env_set` to improve performance under some conditions and to be compatible with existing environments created without `ydb_env_set`. ([787](#x787)) ([842](#x842))
- Enhancements to the %RSEL utility program. ([772](#x772)) ([781](#x781))

YottaDB r1.34 also inherits enhancements and fixes from [GT.M V6.3-011](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-011_Release_Notes.html).

Details are in the [Change History](#change-history).

#### Platforms

A platform is a combination of a CPU architecture and an operating system. A platform is Supported, Supportable, or Unsupported.

* Supported means that we have the platform in our development environment and test each release on that platform.
* Supportable means that although we do not necessarily have such a platform in our environment, we have no reason to believe that the software will not run on it.
* All others are Unsupported.

| CPU Architecture                    | Supported OS Version(s)                                                              | Notes                                                                                                                         |
| ----------------------------------- | ------------------------------------------------------------------------------------ | ----------------------------------------------------------------------------------------------------------------------------- |
| 64-bit x86                          | Ubuntu 20.04 LTS; Red Hat Enterprise Linux 7.x & 8.x; Debian GNU/Linux 11 (Bullseye) | There are separate binary distributions for each OS version, owing to differences in library versions of those distributions. |
| 64-bit ARM (Raspberry Pi 3 Model B) | Debian GNU/Linux 11 (Bullseye)                                                       | See below.                                                                                                                    |
| 32-bit ARM (Raspberry Pi Zero)      | Debian GNU/Linux 11 (Bullseye)                                                       | See below.                                                                                                                    |

Recent releases of major GNU/Linux distributions with contemporary kernels, glibc and ncurses are Supportable. Specific notes:

- We intend to drop support for Red Hat Enterprise Linux 7.x for new YottaDB releases after June 2022. Of course, existing releases will continue to work. Please contact us if this a concern.
- [ydbinstall.sh](https://gitlab.com/YottaDB/DB/YDB/-/blob/master/sr_unix/ydbinstall.sh) recognizes [Rocky Linux](https://rockylinux.org/) 8 as equivalent to RHEL 8 and installs the RHEL 8 YottaDB build. Note that Rocky Linux is Supportable, not Supported.
- On [Arch Linux](https://www.archlinux.org/) and other leading edge distributions, YottaDB will likely need to be recompiled from source code owing to library and tool chain versions newer than those used in building Supported distributions. The `--from-source` option of [ydbinstall.sh](https://gitlab.com/YottaDB/DB/YDB/-/blob/master/sr_unix/ydbinstall.sh) simplifies the process. ([754](#x754))
- While YottaDB is Supportable on other ARMv6, [ARMv7-A](https://en.wikipedia.org/wiki/Comparison_of_ARMv7-A_cores), and [ARMv8-A](https://en.wikipedia.org/wiki/Comparison_of_ARMv8-A_cores) CPUs, owing to variations in the implementations of ARM microarchitectures, we recommend that you ensure the software runs correctly before committing to any specific hardware other than those listed above. Please contact <info@yottadb.com> if you want a specific combination of OS and CPU microarchitecture to be Supported.

#### Installation
See our [Get Started page](https://yottadb.com/product/get-started) to use YottaDB.

We **strongly recommend** that you install YottaDB r1.34 in a newly created directory, different from those of prior YottaDB releases and any GT.M versions you may have installed on the system.

Note that with this release [ydbinstall.sh](https://gitlab.com/YottaDB/DB/YDB/-/blob/master/sr_unix/ydbinstall.sh) can install plugins into an installed YottaDB directory. ([705](#x705))

#### Removing an installed YottaDB release
Assuming `$ydb_dist` points to the directory where YottaDB is installed:

 - Cleanly shut down all application processes using that release.
 - Execute `mupip rundown && mupip rundown -relinkctl`
 - Ensure that there are no `gtcm*` or `gtmsecshr` processes active.
 - Use `sudo lsof | grep $ydb_dist` to ensure there are no open files.
 - Delete the directory with `sudo rm -rf $ydb_dist`

### Upgrading to YottaDB r1.34

*Owing to an internal change required to support the Python wrapper, applications code written in Go and Rust will need to be compiled with new versions of the Go and Rust wrappers. We anticipate no regressions, and apologize for the inconvenience.*

As YottaDB r1.34 is upward compatible from YottaDB [r1.32](https://gitlab.com/YottaDB/DB/YDB/-/releases/r1.32) and GT.M V6.3-011, the minimal upgrade steps are:

* Install YottaDB r1.34. Use `ydbinstall` / `ydbinstall.sh` to install plugins you use.
* Install plugins you need in addition to those installed in the previous step.
* Recompile object code, and recreate shared libraries if your application uses shared libraries.
* If your application uses encryption, compile and install the reference implementation plugin (if not done by the `ydbinstall` / `ydbinstall.sh` script) or your customized plugin.
* Cleanly shut down the application and ensure that the database files are shut down using MUPIP RUNDOWN from the prior release.
* Switch journal files with the new YottaDB release.
* Start using the new YottaDB release.

If the database has triggers defined with non-canonical numbers, or numbers specified as strings with any version prior to [r1.28](https://gitlab.com/YottaDB/DB/YDB/-/releases/r1.28), or if you are unsure, extract the trigger definitions, delete existing triggers, and reload the trigger definitions. Issue [#430] from [r1.28](https://gitlab.com/YottaDB/DB/YDB/-/releases/r1.28) has a series of steps you can copy and execute. There is no need to do this if upgrading from [r1.28](https://gitlab.com/YottaDB/DB/YDB/-/releases/r1.28) or later.

To upgrade from older GT.M releases, first upgrade to GT.M V6.0-000 or later and follow the steps above, or contact your YottaDB support channel for assistance and guidance.

A more sophisticated upgrade technique is:

* Install YottaDB r1.34.
* Create a new replicated instance of your application (on the same system or a different system).
* Assuming the existing instance is A, and the new instance is B, upgrade B to r1.34 and start replicating from A to B.
* Once B catches up, switchover so that B is in a primary role replicating to A.
* Once you are satisfied with B, remove (or upgrade) A.

For well configured environments of existing YottaDB releases, after cleanly shutting down an application environment with the prior release (confirm with MUPIP RUNDOWN), sourcing the r1.34 `ydb_env_set` is designed to upgrade the global directory, database, and to switch journal files. ([842](#x842))

*We recommend always taking a backup of the environment with the prior YottaDB release or GT.M version before upgrading.*

### Change History

#### r1.34

YottaDB r1.34 includes the following enhancements and fixes beyond YottaDB [r1.32](https://gitlab.com/YottaDB/DB/YDB/-/releases/r1.32).

| ID             | Category              | Summary                                                                                                                |
|----------------|-----------------------|------------------------------------------------------------------------------------------------------------------------|
| ([546](#x546)) | Languages             | Certain pathological nested $SELECT() statements do not cause abnormal process termination                             |
| ([555](#x555)) | Languages             | Boolean expressions with $SELECT() that includes a global reference do not cause abnormal process termination          |
| ([557](#x557)) | Languages             | Naked indicator maintained correctly when $SELECT() is used in boolean expression                                      |
| ([705](#x705)) | System Administration | Add or update plugins in an existing YottaDB installation with `ydbinstall` / `ydbinstall.sh`                          |
| ([734](#x734)) | Other                 | Option to use address sanitizer when building YottaDB                                                                  |
| ([754](#x754)) | System Administration | `ydbinstall` / `ydbinstall.sh` option to build and install YottaDB from a git repository                               |
| ([757](#x757)) | Languages             | SET X=$ZYHASH(X) works correctly                                                                                       |
| ([758](#x758)) | System Administration | Report and reset maximum number of processes concurrently accessing a database region                                  |
| ([764](#x764)) | System Administration | `ydbinstall` / `ydbinstall.sh` creates `/usr/share/pkgconfig/` if it does not exist                                    |
| ([766](#x766)) | System Administration | `ydbinstall` / `ydbinstall.sh` choices pertaining to RemoveIPC option on systemd systems                               |
| ([772](#x772)) | Languages             | Utility label $$SRCDIR^%RSEL returns space separated list of source code directories                                   |
| ([775](#x775)) | Languages             | LOCKs acquired inside TSTART/TCOMMIT released when restart occurs in a nested transaction                              |
| ([781](#x781)) | Languages             | ^%RSEL finds and reports routines in shared library files                                                              |
| ([782](#x782)) | Languages             | Child processes after a `fork()` by a lock-owning parent process do not incorrectly assume ownership of parent locks   |
| ([783](#x783)) | Languages             | $ZROUTINES defaults to `$ydb_dist/utf8/libyottadbutil.so` for UTF-8 mode processes                                     |
| ([785](#x785)) | Languages             | `errstr` contains complete error string when M code called from other languages raises an error                        |
| ([786](#x786)) | Other                 | Faster string pool garbage collection                                                                                  |
| ([787](#x787)) | Other                 | Performance improvement to sourcing `ydb_env_set` under some conditions                                                |
| ([793](#x793)) | Languages             | Comments supported at the beginning of external call tables                                                            |
| ([803](#x803)) | Other                 | HOME and END keys work in direct mode for READ mode                                                                    |
| ([806](#x806)) | Languages             | RECALL \<number> subsequent to an erroneous RECALL \<number> command works correctly                                   |
| ([818](#x818)) | Languages             | Correctly load triggers for global variables that span multiple regions                                                |
| ([828](#x828)) | Languages             | Fix bugs exposed by fuzz testing                                                                                       |
| ([829](#x829)) | System Administration | On RHEL 7.x and equivalent, `ydbinstall.sh --octo` installs Octo                                                       |
| ([838](#x838)) | System Administration | `ydbinstall.sh` installs RHEL8 build on Rocky Linux 8                                                                  |
| ([840](#x840)) | Languages             | $ZATRANSFORM() issues an LVUNDEF error when the first argument is an undefined local variable                          |
| ([842](#x842)) | Database              | `ydb_env_set` respects existing $ydb\_gbldir & $ydb\_routines and creates YDBOCTO & YDBAIM regions if they don't exist |
| ([843](#x843)) | Languages             | `ydb_get_s()` / `ydb_get_st()` issue LVUNDEF error if specified local variable does not exist                          |
| ([845](#x845)) | System Administration | Correct LKE SHOW output for LOCK resource names with long subscripts or many subscripts                                |
| ([846](#x846)) | System Administration | DSE DUMP ZWR/GLO correctly outputs records longer than maximum record size for region                                  |

#### GT.M V6.3-011
YottaDB r1.34 incorporates enhancements and fixes from [GT.M V6.3-011](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-011_Release_Notes.html).

| ID                      | Category              | Summary                                                                                                      |
|-------------------------|-----------------------|--------------------------------------------------------------------------------------------------------------|
| ([GTM-3035](#GTM-3035)) | Languages             | Additional information in NOPRINCIO message and related behavior normalized                                  |
| ([GTM-4041](#GTM-4041)) | Languages             | Please see [GTM-3035](#GTM_3035)                                                                             |
| ([GTM-8398](#GTM-8398)) | System Administration | Support larger database extensions                                                                           |
| ([GTM-9036](#GTM-9036)) | Languages             | Recognition of terminal disconnects of a PRINCIPAL device                                                    |
| ([GTM-9136](#GTM-9136)) | Languages             | Available standard behavior for WRITE #                                                                      |
| ([GTM-9215](#GTM-9215)) | Languages             | Reading an ISV does not result in a NUMOFLOW                                                                 |
| ([GTM-9222](#GTM-9222)) | Languages             | $TEXT() of a literal designating the routine it's in OK as 1st value in $SELECT() and with -DYNAMIC_LITERALS |
| ([GTM-9223](#GTM-9223)) | Languages             | Remove conflict between TLS SOCKETs and PIPE Devices                                                         |
| ([GTM-9225](#GTM-9225)) | System Administration | `gtmprofile` uses `pkg-config` in preference to `icu-config` to determine the icu version number             |
| ([GTM-9226](#GTM-9226)) | Languages             | Fix $[Z]TRANSLATE() result length when manipulating UTF-8 strings with NOBADCHAR                             |

#### Database

* <a name="x842"></a> Enhancements to `ydb_env_set` make it friendlier to existing environments.

  - Sourcing `ydb_env_set` respects existing values of `ydb_gbldir` and `ydb_routines`, and adjusts `$ydb_routines` to point to `libyottadbutil.so` of the current YottaDB release, in the parent directory or the UTF-8 subdirectory, depending on the desired mode. Previously, it replaced those values with standard values pointing to an environment under `$ydb_dir`, defaulting to `$HOME/.yottadb` if not defined. As before, sourcing `ydb_env_unset` restores the prior values. Enhancements include:

	* `ydb_routines` now automatically includes the plugins in the `$ydb_dist/plugin` directory. Environment variables to call exported C routines in plugins (e.g., `ydb_xc_gtmzlib` to call the [Zlib plugin](https://docs.yottadb.com/Plugins/ydbzlib.html)) are also created.

	* In the event the environment is coming up after an unclean shutdown, it recovers the environment using the existing `$ydb_gbldir` using [MUPIP JOURNAL RECOVER](https://docs.yottadb.com/AdminOpsGuide/ydbjournal.html#recover) or [MUPIP JOURNAL ROLLBACK](https://docs.yottadb.com/AdminOpsGuide/ydbjournal.html#rollback-on-line-noo-nline) depending on the replication setting. Note that existing regions must have before-image journaling enabled, and the YDBAIM region is deleted, since it is automatically created on demand.

	* Global directory files are automatically upgraded to the format of the current YottaDB release.

	* If YDBOCTO and YDBAIM regions do not exist in the global directory, it creates them. The database and journal file for the YDBOCTO region are created in the same directories as the database and journal files of the DEFAULT region, with the same journal settings as the DEFAULT region's database file. Since YDBAIM is an [AutoDB](https://docs.yottadb.com/AdminOpsGuide/gde.html#no-au-todb) region, the database file is not created, but when created on demand, it will be created in the same directory as the DEFAULT region.

  - If `ydb_chset` is not set, inference of M vs. UTF-8 mode is more reliable than it was previously. We recommend setting `ydb_chset` to the preferred mode instead of having `ydb_env_set` infer it.

  - Sourcing `ydb_env_set` sets `$status`, whereas previously it did not:

	* 0 is normal exit, as expected
	* 1 means that `$ydb_dist` does not match the location of `ydb_env_set`
	* 2 means that `$gtm_dist` does not match the location of `ydb_env_set`
	* 3 means that neither `$ydb_dist` nor `$gtm_dist` match the location of `ydb_env_set`
	* Other non-zero error codes are as returned by running `set^%YDBENV`

  Note that:

  - While `ydb_env_set` attempts to be more user-friendly, it assumes well-configured environment, and misconfigured environments (for example a `$ydb_routines` of `$ydb_dist/libyottadbutil.so $ydb_dist/utf8/libyottadbutil.so`) can make it misbehave: the `$ydb_dist/utf8/libyottadbutil.so` will make `ydb_env_set` infer a UTF-8 environment, however the prior `$ydb_dist/libyottadbutil.so` means that YottaDB will find M mode object code for utility routines.

  - If upgrading from an older release of YottaDB, the environment must be cleanly shut down with the prior release.

  - Although upgrading and using GT.M environments should work, this has not been tested.

  [YDB#842](https://gitlab.com/YottaDB/DB/YDB/-/issues/842)

#### Languages

* <a name="x546"></a> Certain pathological cases of nested [$SELECT()](https://docs.yottadb.com/ProgrammersGuide/functions.html#select) expressions do not cause process termination with GTMASSERT2 failures. Previously they did. This was discovered in the YottaDB development environment and was never reported by a user. [YDB#546](https://gitlab.com/YottaDB/DB/YDB/-/issues/546)

* <a name="x555"></a> Boolean expressions with a [$SELECT()](https://docs.yottadb.com/ProgrammersGuide/functions.html#select) that includes a global reference (e.g., `set false=0,^true=1 write 1!$select(false:1&0,^true:1)`) do not cause abnormal process termination with a GTMASSERT2 message. Previously they did. This was discovered in the YottaDB development environment and never reported by a user. [YDB#555](https://gitlab.com/YottaDB/DB/YDB/-/issues/555)

* <a name="x557"></a> The naked indicator is maintained correctly after executing [$SELECT()](https://docs.yottadb.com/ProgrammersGuide/functions.html#select) functions that are part of Boolean expressions. Previously, the naked indicator could incorrectly indicate a global variable that was referenced in an expression within the [$SELECT()](https://docs.yottadb.com/ProgrammersGuide/functions.html#select) after the expression that provided the [$SELECT()](https://docs.yottadb.com/ProgrammersGuide/functions.html#select) function its value. This was discovered in the YottaDB development environment and never reported by a user. [YDB#557](https://gitlab.com/YottaDB/DB/YDB/-/issues/557)

* <a name="x757"></a> The code `SET X=$ZYHASH(X)` where `X` is a local variable assigns the result of the function to the variable. Previously it made X undefined. The workaround was to use an intermediate variable, e.g., `SET Y=$ZYHASH(X),X=Y`. This was discovered in the YottaDB development environment and never reported by a user. [YDB#757](https://gitlab.com/YottaDB/DB/YDB/-/issues/757)

* <a name="x772"></a> The utility label $$SRCDIR^%RSEL takes no parameters and returns as its value a space separated list of directories in $ZROUTINES that can contain source code. Typically, the first source directory is the location where code generators should place generated source code. If there are no source directories, for example, if $ZROUTINES contains only shared libraries, $$SRCDIR^%RSEL returns an empty string (`""`). [YDB#772](https://gitlab.com/YottaDB/DB/YDB/-/issues/772)

* <a name="x775"></a> LOCKs acquired inside a transaction (`TSTART`/`TCOMMIT` fence) are correctly released in case the transaction restarts. Previously, a LOCK acquired at the outermost level (i.e. when `$TLEVEL` is 1) would not be released if the transaction restart happened in a nested transaction (i.e. when `$TLEVEL` is greater than 1). [YDB#775](https://gitlab.com/YottaDB/DB/YDB/-/issues/775)

* <a name="x781"></a> Labels in [%RSEL](https://docs.yottadb.com/ProgrammersGuide/utility.html#rsel) that report object code locations (e.g., OBJ^%RSEL) also report object code in shared library files. Previously it only found and reported object code in individual files in directories. Selecting routines in shared libraries can either be done with ranges or wildcards, but not both. For example:

  ```
	YDB>do SILENT^%RSEL("%X:%Y","OBJ") zwrite %ZR
	%ZR=1
	%ZR("%XCMD")="/usr/local/lib/yottadb/r134/utf8/libyottadbutil.so"

	YDB>
  ```

  [YDB#781](https://gitlab.com/YottaDB/DB/YDB/-/issues/781)

* <a name="x782"></a> Applications that use the YottaDB Simple API to maintain locks (using `ydb_lock_incr_s()` / `ydb_lock_decr_s()` / `ydb_lock_s()` calls and their equivalent functions for multi-threaded applications) work correctly when they spawn processes (e.g., using the `fork()` system call) from a parent process that has acquired locks using the Simple API. Previously, child processes incorrectly assumed they had ownership of locks acquired by the parent process at the time of the `fork()`. This was discovered in the YottaDB development environment and never reported by a user. [YDB#782](https://gitlab.com/YottaDB/DB/YDB/-/issues/782)

* <a name="x783"></a> If $ZROUTINES is not set, `yottadb` defaults it to `$ydb_dist/libyottadbutil.so` in M mode and `$ydb_dist/utf8/libyottadbutil.so` in UTF-8 mode. Previously it was unconditionally set to `$ydb_dist/libyottadbutil.so` which was inappropriate for UTF-8 mode processes. This was discovered in the YottaDB development environment and never reported by a user. [YDB#783](https://gitlab.com/YottaDB/DB/YDB/-/issues/783)

* <a name="x785"></a> When an error occurs in M code called from other languages, the `errstr` error string, the [second parameter in the Simple API for multi-threaded applications](https://docs.yottadb.com/MultiLangProgGuide/programmingnotes.html#threads), is filled in appropriately. Previously, it was either the empty string (`""`) for C and most languages, or the template for the error message without the fields filled in for Go. This was discovered in the YottaDB development environment and never reported by a user. [YDB#785](https://gitlab.com/YottaDB/DB/YDB/-/issues/785)

* <a name="x793"></a> Comments are supported at the beginning of [external call tables](https://docs.yottadb.com/ProgrammersGuide/extrout.html#using-external-calls). Previously, comments prior to the shareable library name were interpreted as the name of the shareable library causing an external call using the table to fail with a %YDB-I-DLLNOOPEN. This was discovered in the development environment, and never reported by a user. [YDB#793](https://gitlab.com/YottaDB/DB/YDB/-/issues/793)

* <a name="x806"></a> A `RECALL <number>` command subsequent to a `RECALL <number>` command that reported a `Recall Error : Number exceeds limit` error works correctly. Previously it returned the wrong command. We thank Sergey Kamenev (@inetstar) for reporting the bug and contributing the fix. [YDB#806](https://gitlab.com/YottaDB/DB/YDB/-/issues/806)

* <a name="x818"></a> [$ZTRIGGER()](https://docs.yottadb.com/ProgrammersGuide/functions.html#ztrigger) and [MUPIP TRIGGER](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#trigger) correctly load triggers for global variables that span multiple regions. Previously, in random cases, it was possible for triggers with a duplicate name to be incorrectly loaded, instead of issuing an error. This was discovered in the YottaDB development environment and never reported by a user. [YDB#818](https://gitlab.com/YottaDB/DB/YDB/-/issues/818)

* <a name="x828"></a> As described in [Fuzz Testing YottaDB](https://yottadb.com/fuzz-testing-yottadb/), implementing a new type of testing in YottaDB exposed bugs that were neither previously discovered in development environments, nor reported by users. This Issue captures 40 such bugs fixed in the r1.34 release. We intend to create a separate Issue for each YottaDB release that includes fixes for bugs exposed by fuzz testing. [YDB#828](https://gitlab.com/YottaDB/DB/YDB/-/issues/828)

* <a name="x840"></a> [$ZATRANSFORM()](https://docs.yottadb.com/ProgrammersGuide/functions.html#zatransform) issues an LVUNDEF error when the first argument is an undefined local variable. In YottaDB [r1.32](https://gitlab.com/YottaDB/DB/YDB/-/releases/r1.32), due to a regression in fixing [YDB#724](https://gitlab.com/YottaDB/DB/YDB/-/issues/724), this could cause the process to abnormally terminate with a SIG-11. [YDB#840](https://gitlab.com/YottaDB/DB/YDB/-/issues/840)

* <a name="x843"></a> `ydb_get_s()` / `ydb_get_st()` return a YDB\_ERR\_LVUNDEF error on a non-existent local variable node specified with a large number of subscripts or very large subscripts, and sets $ZSTATUS, which can subsequently be accessed with `ydb_zstatus()` which will contain the first 512 bytes of the ZWRITE representation of the local variable node. In releases starting with [r1.26](%r1.26), the process in rare cases (if the ZWRITE format representation of the local variable node approached or exceeded 512 bytes) terminated with a GTMASSERT2 error. This was discovered in the YottaDB development environment and never reported by a user.[YDB#843](https://gitlab.com/YottaDB/DB/YDB/-/issues/843)

* <a name="GTM-3035"></a> Loss of the PRINCIPAL device ([$PRINCIPAL](https://docs.yottadb.com/ProgrammersGuide/isv.html#principal) in base Direct Mode is silent, this includes completion of HEREDOC input from a script; previously the behavior differed by device. This requires that when you desire a non-zero error status return from a HEREDOC, you must use [ZHALT](https://docs.yottadb.com/ProgrammersGuide/commands.html#zhalt) with an appropriate argument. SYSLOG NOPRINCIO errors, which indicate the loss of the PRINCIPAL device include information on the device, and, when available, the error status from [$ZSTATUS](https://docs.yottadb.com/ProgrammersGuide/isv.html#zstatus); previously they did not. Note that NOPRINCIO errors are fatal, so when YottaDB detects one due to a second unsuccessful attempt to [READ](https://docs.yottadb.com/ProgrammersGuide/commands.html#read) or [WRITE](https://docs.yottadb.com/ProgrammersGuide/commands.html#write) to the device, it terminates the process immediately, possibly leaving an application state that is out of design. Therefore, the application should detect (at least) IOEOF, TERMHANGUP and TERMWRITE errors when they first occur, and shutdown appropriately. You should address any NOPRINCIO errors in the syslog as evidence of potential application integrity problems. In addition, the following M utility programs handling of [$PRINCIPAL](https://docs.yottadb.com/ProgrammersGuide/isv.html#principal) conforms to NOPRINCIO conventions: [^%GC](https://docs.yottadb.com/ProgrammersGuide/utility.html#gc), [^%GO](https://docs.yottadb.com/ProgrammersGuide/utility.html#go), [^%RCE](https://docs.yottadb.com/ProgrammersGuide/utility.html#rce), [^%RI](https://docs.yottadb.com/ProgrammersGuide/utility.html#ri), [^%RO](https://docs.yottadb.com/ProgrammersGuide/utility.html#ro), [^%RSEL](https://docs.yottadb.com/ProgrammersGuide/utility.html#rsel), and [^%YGBLSTAT](https://docs.yottadb.com/ProgrammersGuide/utility.html#ygblstat); previously it may not have, particularly because of the issues described earlier in this release note. (GTM-4041)([GTM-3035](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-011_Release_Notes.html#GTM-3035))

* <a name="GTM-4041"></a> Please see [GTM-3035](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-011_Release_Notes.html#GTM-3035). ([GTM-4041](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-011_Release_Notes.html#GTM-4041))

* <a name="GTM-9036"></a> [USE](https://docs.yottadb.com/ProgrammersGuide/commands.html#use) for a terminal device recognizes NOHUPENABLE and HUPENABLE deviceparameters to determine whether to recognize a terminal disconnect on [$PRINCIPAL](https://docs.yottadb.com/ProgrammersGuide/isv.html#principal) as an error. The `ydb_hupenable` environment variable, if defined to a one (1) T(rue) or Y(es), determines the initial setting for the process as HUPENABLE; if undefined or defined to zero (0) F(alse) or N(o), it specifies the initial setting as NOHUPENABLE. By default, YottaDB ignores terminal disconnects which can be handy for breaking and resuming connections with long-running processes. Previously, there was no way to change the default behavior. ([GTM-9036](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-011_Release_Notes.html#GTM-9036))

* <a name="GTM-9136"></a> The [NO]FFLF deviceparameter controls whether WRITE # produces only a form-feed (\<FF>) or a form-feed and line-feed (\<FF>\<LF>). Previously, YottaDB used \<FF>\<LF> which deviated from the ISO/IEC 11756:1999 standard, however, out of concern for existing practice the default remains \<FF>\<LF> Additionally, the `ydb_nofflf` environment variable controls the default WRITE # behavior of YottaDB. If it is unset or set to 0, N[O] or F[ALSE], the default behavior is unchanged. If it is set to 1, Y[ES] or T[RUE], the default behavior of [WRITE #](https://docs.yottadb.com/ProgrammersGuide/commands.html#write) is changed to produce only a form-feed (\<FF>), though M programs can still control behavior by specifying the FFLF deviceparameter. ([GTM-9136](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-011_Release_Notes.html#GTM-9136))

* <a name="GTM-9215"></a> YottaDB retrieves values from Intrinsic Special Variables (ISVs) appropriately; previously the YottaDB compiler produced NUMOFLOW error for values that matched the ISO/IEC 11756:1999 standard exponential form (nEm) and exceeded the YottaDB numeric size limit. While [ZSHOW "I"](https://docs.yottadb.com/ProgrammersGuide/commands.html#zshow-information-codes) could display such a value, the only workaround was to ensure an ISV did not acquire a value with both the exponential form and a large value when evaluated as numeric, for example by a convention of prefixing such a value with a non-numeric character. ([GTM-9215](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-011_Release_Notes.html#GTM-9215))

* <a name="GTM-9222"></a> YottaDB appropriately handles the case of the DYNAMIC\_LITERALS compiler option for [$TEXT()](https://docs.yottadb.com/ProgrammersGuide/functions.html#text) with a literal argument referring to the routine in which it appears; previously this caused a segmentation violation (SIG-11). YottaDB also appropriately handles the same type of [$TEXT()](https://docs.yottadb.com/ProgrammersGuide/functions.html#text) as result value in the first argument of a [$SELECT()](https://docs.yottadb.com/ProgrammersGuide/functions.html#select) when a literal follows the [$TEXT()](https://docs.yottadb.com/ProgrammersGuide/functions.html#text) in the list of possible results. Previously, YottaDB performed compiler optimizations for literal arguments to [$TEXT()](https://docs.yottadb.com/ProgrammersGuide/functions.html#text) in a way that conflicted with other literal management when combined with the [$SELECT()](https://docs.yottadb.com/ProgrammersGuide/functions.html#select) management of possible result values such that a subsequent result literal selected by a constant (typically 1) overlaid a not-chosen [$TEXT()](https://docs.yottadb.com/ProgrammersGuide/functions.html#text) value. The wrong [$TEXT()](https://docs.yottadb.com/ProgrammersGuide/functions.html#text) value then persisted for the process in executing all code within the routine where this occurred. ([GTM-9222](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-011_Release_Notes.html#GTM-9222))

* <a name="GTM-9223"></a> YottaDB handles the combination of PIPE devices and TLS SOCKETs correctly. Previously, a process opening a PIPE device had the side effect of disconnecting any TLS sockets. ([GTM-9223](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-011_Release_Notes.html#GTM-9223))

* <a name="GTM-9226"></a> [$TRANSLATE](https://docs.yottadb.com/ProgrammersGuide/functions.html#translate) appropriately handles the character length of its results when it is used in UTF-8 mode to construct characters. Previously it could be used to create a string with the wrong character length, which could cause issues in subsequent code, including a segmentation violation (SIG-11). In the cases we are aware of, BADCHAR detection was disabled. ([GTM-9226](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-011_Release_Notes.html#GTM-9226))

#### System Administration

* <a name="x705"></a> The `--plugins-only` option of `ydbinstall` / `ydbinstall.sh` adds or updates specified plugins to an existing YottaDB installation. Previously `ydbinstall` / `ydbinstall.sh` would always install YottaDB as well. Note that the YottaDB installation must already exist. [YDB#705](https://gitlab.com/YottaDB/DB/YDB/-/issues/705)

* <a name="x754"></a> With the `--fromsource <repo>` option, the `ydbinstall` / `ydbinstall.sh` script clones the repository specified by `<repo>` using `git clone <repo>` in the current directory, and changes to the `YDB/` subdirectory. If `--branch <branch>` is specified, it executes `git checkout -B <branch>` to specify a branch other than the default. Then it builds YottaDB, and if successful, installs the built YottaDB using `sudo ydbinstall` of the `ydbinstall` script of the built YottaDB, passing it all command line options except the `--fromsource` and `--branch` options. `sudo ydbinstall` prompts for a password as required. For example, `ydbinstall --from-source https://gitlab.com/ydbuser/YDB.git --branch working --utf8 default --aim --install-directory /usr/local/lib/yottadb/devel_$(date +%Y%m%d)` will checkout, build, and install the `working` branch of YottaDB from the YDB repository of GitLab user `ydbuser` in a date-stamped directory, along with the [Application Independent Metadata plugin](https://gitlab.com/YottaDB/Util/YDBAIM/). [YDB#754](https://gitlab.com/YottaDB/DB/YDB/-/issues/754)

* <a name="x758"></a> `$$^%PEEKBYNAME("node_local.max_procs",<region>)` returns a string consisting of two comma separated integers. The first is a count of the maximum number of processes that concurrently accessed that database region, and the second is the number of seconds since January 1, 1970 00:00:00 UTC (i.e., the value of [$ZUT](https://docs.yottadb.com/ProgrammersGuide/isv.html#zut)) at which that number of processes was recorded. The output of the [DSE DUMP FILEHEADER](https://docs.yottadb.com/AdminOpsGuide/dse.html#f-ileheader) command includes the field `Max Concurrent processes` that reports the maximum number of concurrent processes, and the field `Max conc proc time` which reports the time at which that value was recorded. The `-reset_max_procs` option of the [DSE CHANGE FILEHEADER](https://docs.yottadb.com/AdminOpsGuide/dse.html#f-ileheader) command sets the maximum number of concurrent processes to zero, and the time to the time that the [DSE CHANGE FILEHEADER](https://docs.yottadb.com/AdminOpsGuide/dse.html#f-ileheader) command was run. [YDB#758](https://gitlab.com/YottaDB/DB/YDB/-/issues/758)

* <a name="x764"></a> `ydbinstall` / `ydbinstall.sh` creates `/usr/share/pkgconfig` (the directory where `pkg-config` by default tracks packages) if it does not exist. [YDB#764](https://gitlab.com/YottaDB/DB/YDB/-/issues/764)

* <a name="x766"></a> On Linux systems using [systemd](https://systemd.io/), correct operation of YottaDB requires the `RemoveIPC=no` setting in `/etc/systemd/logind.conf`. The `ydbinstall` / `ydbinstall.sh` script checks this setting.

  - If `RemoveIPC` is set to `no`, it proceeds with the installation.

  - If `RemoveIPC` is set to `yes`, the script checks the command line option `--preserveRemoveIPC`.

	* If `--preserveRemoveIPC` is set to `no`, the script changes the setting in `/etc/systemd/logind.conf`, the script outputs a message that it has made the change, and a restart of `systemd-logind` is required to complete the installation.
	* If `--preserveRemoveIPC` is set to `yes` the script informs the user that they need to change `RemoveIPC` to `no`.

  Restarting `systemd-logind` may terminate graphical desktop sessions, such as Gnome or Plasma.

  Previously the `ydbinstall` / `ydbinstall.sh` script would silently restart `systemd-logind` causing graphical desktop sessions to terminate. [YDB#766](https://gitlab.com/YottaDB/DB/YDB/-/issues/766)

* <a name="x829"></a> On Red Hat Enterprise Linux 7.x and clones such as CentOS 7.x, The `--octo` option of `ydbinstall.sh` correctly installs Octo. Previously, it would terminate with a `fatal: The remote end hung up unexpectedly` error, owing to an older git on that platform. More current Linux distributions Supported by YottaDB were not affected. [YDB#829](https://gitlab.com/YottaDB/DB/YDB/-/issues/829)

* <a name="x838"></a> When run on [Rocky Linux](https://rockylinux.org/), [ydbinstall.sh](https://gitlab.com/YottaDB/DB/YDB/-/blob/master/sr_unix/ydbinstall.sh) installs the YottaDB build for [Red Hat Enterprise Linux](https://www.redhat.com/en/technologies/linux-platforms/enterprise-linux) 8. Note that Rocky Linux is Supportable but not Supported. [YDB#838](https://gitlab.com/YottaDB/DB/YDB/-/issues/838)

* <a name="x845"></a> [LKE SHOW](https://docs.yottadb.com/AdminOpsGuide/mlocks.html#show) output is correct for LOCK resource names with long subscripts or many subscripts. Previously, the output was garbled for long resource names and truncated for resource names with many subscripts. This was discovered in the YottaDB development environment and never reported by a user. [YDB#845](https://gitlab.com/YottaDB/DB/YDB/-/issues/845)

* <a name="x846"></a> [DSE DUMP](https://docs.yottadb.com/AdminOpsGuide/dse.html#dump) ZWR/GLO correctly outputs records longer than the maximum record size for the region. Previously, it truncated them. This was discovered in the YottaDB development environment and never reported by a user. [YDB#846](https://gitlab.com/YottaDB/DB/YDB/-/issues/846)

* <a name="GTM-8398"></a> [GDE](https://docs.yottadb.com/AdminOpsGuide/gde.html) and [MUPIP](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#mupip) support database file extension sizes of up to 1,048,575 blocks; previously the maximum was 65,355 blocks. While GDE upgrades a global directory simply by opening it and EXITing, remember to save copies or commands if you need the ability to return to prior versions. ([GTM-8398](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-011_Release_Notes.html#GTM-8398))

* <a name="GTM-9225"></a> To obtain the default ICU version number, `gtmprofile` queries `pkg-config` and only falls back to `icu-config` if `pkg-config` is missing. `icu-config` has been deprecated and not installed by default starting with ICU 63.1. Previously, `gtmprofile` relied solely on the result from `icu-config`. ([GTM-9225](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-011_Release_Notes.html#GTM-9225))

#### Other

* <a name="x734"></a> The file `CmakeLists.txt` used to build YottaDB has an option to enable the [address sanitizer](https://en.wikipedia.org/wiki/AddressSanitizer) (ASAN; the `-fsanitize=address` compiler option). This option is for those developing and testing YottaDB; it is not used to build production distributions. [YDB#734](https://gitlab.com/YottaDB/DB/YDB/-/issues/734)

* <a name="x786"></a> YottaDB string pool garbage collection performance has been improved by using a faster sort algorithm. We have seen improvement of about 25% in [specific tests](https://gitlab.com/YottaDB/DB/YDB/-/merge_requests/1038#note_691523847). Your mileage may vary. We would like to thank Konstantin Aristov (@littlecat) and Alexander Sergeev for this enhancement. [YDB#786](https://gitlab.com/YottaDB/DB/YDB/-/issues/786)

* <a name="x787"></a> Sourcing `ydb_env_set` is more robust as it uses PIPE devices rather than the [ZSYSTEM](https://docs.yottadb.com/ProgrammersGuide/commands.html#zsystem) command. PIPE devices are more efficient and give YottaDB more control. Previously, depending on the environment in which `ydb_env_set` was sourced, it could execute slowly. [YDB#787](https://gitlab.com/YottaDB/DB/YDB/-/issues/787)

* <a name="x803"></a> The HOME and END keys work as expected in direct mode and [READ](https://docs.yottadb.com/ProgrammersGuide/commands.html#read) mode. Previously they did not. YottaDB thanks Sergey Kamenev (@inetstar) for this contribution to make direct mode more user-friendly. [YDB#803](https://gitlab.com/YottaDB/DB/YDB/-/issues/803)

### Messages

These are messages which are either modified or newly added.

#### YottaDB

**DEVPARPARSE**, Error parsing device parameter specification

Compile Time Error: While parsing deviceparameters, YottaDB encountered syntax errors.

Action: Correct the code with the deviceparameter syntax.

**UTF8NOTINSTALLED**, $ydb_dist does not have utf8 folder installed. Please use M mode or re-install YottaDB with UTF-8 support

Run Time Error: The environment variable `ydb_chset` (or if it is not set, the environment variable `gtm_chset`) has the case-independent value `"UTF-8"` instructing the YottaDB process to run in UTF-8 mode. However, YottaDB at `$ydb_dist` was not installed with UTF-8 support.

Action: Reinstall YottaDB with UTF-8 support and try again.

**ZDIRTOOLONG**, $ZDIR value specified is xxxx bytes long which is greater than the allowed maximum of yyyy bytes

Compile / Run Time Error: The value to be assigned to [$ZDIRECTORY](https://docs.yottadb.com/ProgrammersGuide/isv.html#zdirectory) is xxxx bytes long, but the maximum supported string length for $ZDIRECTORY is yyyy bytes.

Action: Correct the code that attempts to set $ZDIRECTORY to an excessively long value.

#### From GT.M V6.3-011

**NOPRINCIO**, Unable to dddd principal device: DDDD at LLLL due to: SSSS

Run Time Fatal: This indicates that YottaDB attempted to but could not READ from or WRITE to (direction indicated by dddd), the PRINCIPAL device and therefore attempted to issue an appropriate error, for example, an IOEOF, TERMHANGUP, or TERMWRITE at location LLLL, with a status of SSSS. However, if the error handling does not prevent any and all subsequent READs and WRITEs to the no longer available PRINCIPAL device, the next I/O error shuts down the process immediately with a NOPRINCIO to prevent mysteriously lost output, or worse, an indefinite loop.

Action: The NOPRINCIO error message is FATAL which does not drive device or trap handlers and terminates the process. This termination does not allow any application level orderly shutdown and, depending on the application, may lead to out-of-design application state. Therefore we recommend appropriate application level error handling that recognizes the error and performs an orderly shutdown without issuing any additional READ or WRITE to the principal device. The most common causes for the principal device to cease to exist involve terminal sessions or socket connections (including those from processes started by inetd/xinetd). When the remote client terminates the connection, the underlying PRINCIPAL device becomes inaccessible making any subsequent attempt to READ from, or WRITE to, it hopeless. In the case of terminals, a user closing the window of a session without cleanly exiting from the YottaDB process sets up the case that can drive this error. YottaDB does not issue NOPRINCIO errors from Direct Mode, because it is a developer tool, or at the completion a HEREDOC in a shell script. However, this means a HEREDOC must use ZHALT to return a specific status to the shell, and that a process in direct mode as the result of a $ETRAP terminates without evidence.

**TERMHANGUP**, Terminal has disconnected

Run Time Error: This indicates that the terminal serving as the PRINCIPAL device has disconnected. By default, YottaDB ignores terminal "hang-ups," which can allow the terminal to reconnect at a later time to a process that does not need the terminal to continue work. You can enable recognition of Principal device disconnects with USE $PRINCIPAL:HUPENABLE or by starting the process with the gtm_hupenable set to 1, TRUE or YES, or disable them with USE $PRINCIPAL:NOHUPENABLE.

Action: When a process receives this error it must avoid any further READs from, or WRITEs to, $PRINCIPAL, typically by shutting down in a wholesome fashion. Failure to do so causes YottaDB to terminate the process with a NOPRINCIO message to the operator log.

**TRANS2BIG**, Transaction exceeded available buffer space for region rrrr

Run Time Error: This indicates that a transaction updated more blocks than the global buffers could hold (half - 2) for a particular region rrrr or accessed more than the single transaction limit of 64Ki blocks.

Action: Look for missing TCOMMIT commands; modify the code to reduce the total content or change content of the transaction. If the transaction is as intended and the issue is the number of updates, increase the GLOBAL_BUFFERS for the region using MUPIP SET, or modify the Global Directory to redistribute the relevant globals to more regions. If this occurs on a replicating instance it may indicate either a difference in configuration between the originating and replicating instances, which probably should be addressed, or a transaction that was borderline on the originating instance, but failed on the replicating instance because of difference in the database layout. In the later case, consider examining the application code to see if it's possible to reduce the size of the transaction, or alternatively increase the global buffers on both the instances.

### Legal Stuff

Copyright © 2022 YottaDB LLC

Permission is granted to copy, distribute and/or modify this document under the terms of the GNU Free Documentation License, Version 1.3 or any later version published by the Free Software Foundation; with no Invariant Sections, no Front-Cover Texts and no Back-Cover Texts.

YottaDB® and Octo® are registered trademarks of YottaDB LLC. GT.M™ is a trademark of Fidelity National Information Services, Inc.
Other trademarks belong to their respective owners.

This document contains a description of YottaDB and the operating instructions pertaining to the various functions that comprise the software. This document does not contain any commitment of YottaDB LLC. YottaDB LLC believes the information in this publication is accurate as of its publication date; such information is subject to change without notice. YottaDB LLC is not responsible for any errors or defects.
