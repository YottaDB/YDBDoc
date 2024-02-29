<!---
.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2019-2023 YottaDB LLC and/or its subsidiaries.#
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

# YottaDB r1.26

## Release Note Revision History

| Revision | Date | Summary |
| ---------| ---- | ------- |
| 1.00     |  June 27, 2019    | r1.26 Initial Release |

## Contact Information

### YottaDB LLC

40 Lloyd Avenue, Suite 104

Malvern, PA 19355, USA

info@yottadb.com

+1 (610) 644-1898

### Support

**Customers**

Contact your YottaDB support channel.

**Others**

For free (to you) support from members of communities who run widely available applications on YottaDB, please use an application-specific list where appropriate.

* For issues specific to the use of YottaDB from node.js via [nodem](https://github.com/dlwicksell/nodem), [QewdJS](http://qewdjs.com/) or [Enterprise Web Developer](http://ewdjs.com/), post to the [Enterprise Web Developer community](http://groups.google.com/group/enterprise-web-developer-community).

* For issues specific to the use of YottaDB with [VistA](https://en.wikipedia.org/wiki/VistA) flavors, post to the [Hardhats](http://groups.google.com/group/hardhats) list.

* For issues specific to the use of YottaDB with M other than for applications above, post to the [comp.lang.mumps](http://groups.google.com/group/comp.lang.mumps) list.

* If you are not sure where to post, or for requests other than to the above communities, post an issue at https://gitlab.com/YottaDB/DB/YDB/issues and include the words "help wanted" in the summary.

## r1.26

### Overview

r1.26 is a major release that brings important new functionality to YottaDB.

Functions of the [C Simple API](https://docs.yottadb.com/MultiLangProgGuide/cprogram.html) to support multi-threaded applications, which were considered field test grade in [r1.24](https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.24) are considered production grade and fully Supported in r1.26. Their performance is also now [comparable to functions for single-threaded applications](#420).

There are numerous other enhancements, including:

* Sourcing `ydb_env_set` addresses an [expanded set of use cases](#429), including setting up and automatically recovering a database (e.g., when coming up after a crash, for example). This brings to applications in all languages functionality that was previously available to M applications through the `ydb` script in a more limited form. `ydb_env_set` also automatically sets environment variables to access YottaDB plugins that comply with the plugin architecture standard.

* More seamless integration between C and M code.

* Debian GNU/Linux 10 (Buster) on x86\_64 is a Supported platform.

* The $ZTIMEOUT intrinsic special variable allows an application to create and manage software [watchdog timers](https://en.wikipedia.org/wiki/Watchdog_timer).

As with any YottaDB release, there are numerous additional enhancements for functionality, performance, usability, and robustness, as well as fixes. These are detailed in the [complete release notes](https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.26).

YottadB r1.26 is upward compatible with both [YottaDB r1.24](https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.24), [GT.M V6.3-006](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-006_Release_Notes.html), and [GT.M V6.3-007](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-007_Release_Notes.html).

### Platforms

A platform is a combination of a CPU architecture and an operating system. A platform is Supported, Supportable, or Unsupported. Supported means that we have the platform in our development environment and test each release on that platform. Supportable means that although we do not necessarily have such a platform in our environment, we have no reason to believe that the software will not run on it. All others are Unsupported.

| CPU Architecture                                               | Supported OS Version(s)                            | Notes                                                                               |
| -----------------------------------------------         | -------------------------------------------------  | ----------------------------------------------------------------------------------- |
| 64-bit x86                                                     | Ubuntu 18.04 LTS; Red Hat Enterprise Linux 7.6; Debian GNU/Linux 10 (Buster) | There are separate binary distributions for each OS version , owing to differences in library versions of those distributions. |
| 64-bit ARM (Raspberry Pi 3 Model B)                            | Ubuntu 18.04 LTS                                   | While YottaDB is Supportable on other [ARMv8-A CPUs](https://en.wikipedia.org/wiki/Comparison_of_ARMv8-A_cores), owing to variations in the implementations of ARM microarchitectures, we recommend that you ensure the software runs correctly before committing to any specific hardware other than those Supported. Please contact info@yottadb.com if you want a specific combination of OS and CPU microarchitecture to be Supported.                                                  |
| 32-bit ARM (Raspberry Pi Zero)                                 | Raspbian GNU/Linux 9.1                             | While YottaDB is Supportable on other ARMv6 CPUs, owing to variations in the implementations of ARM microarchitectures, we recommend that you ensure the software runs correctly before committing to any specific hardware other than those Supported. Please contact info@yottadb.com if you want a specific combination of OS and CPU microarchitecture to be Supported.                                         |

Recent releases of major GNU/Linux distributions with contemporary kernels, glibc and ncurses are Supportable. Specific notes:

- On Ubuntu releases after 18.04 LTS, YottaDB needs the libtinfo5 package to be installed.
- On [Arch Linux](https://www.archlinux.org/)) and other leading edge distributions, YottaDB may need to be recompiled from source code owing to library and tool chain versions significantly more recent than those used in building the distribution.

### Installation

See our [Get Started page](https://yottadb.com/product/get-started) to use YottaDB.

We **strongly recommend** that you install YottaDB r1.26 in a newly created directory, different from those of YottaDB r1.24 and any GT.M versions you may have installed on the system.

### Removing an installed YottaDB release

Assuming $ydb\_dist points to the directory where YottaDB is installed:

* Cleanly shut down all application processes using that release.
* Execute mupip rundown && mupip rundown -relinkctl.
* Ensure that there are no gtcm\* or gtmsecshr processes active.
* Use sudo lsof | grep $ydb\_dist to ensure there are no open files.
* Delete the directory with sudo rm -rf $ydb\_dist.

## Upgrading to YottaDB r1.26

As YottaDB r1.26 is upward compatible from YottaDB r1.24, GT.M V6.3-006 and GT.M V6.3-007, the minimal upgrade steps are:

* Install YottaDB r1.26.
* Recompile any object code, and recreate shared libraries where appropriate.
* If you are using encryption, compile and install the reference implementation plugin or your customized plugin.
* Cleanly shut down the application and ensure that the database files are shut down using mupip rundown from the prior release.
* Switch journal files with the new YottaDB release.
* Start using the new YottaDB release.

If the database has triggers defined with non-canonical numbers, or numbers specified as strings (Issue [#430]), or if you are unsure, you should extract the trigger definitions, delete existing triggers, and reload the trigger definitions. The Issue [#430] has a series of steps you can copy and execute.

To upgrade from older GT.M releases, first upgrade to GT.M V6.0-000 or later and follow the steps above, or contact your YottaDB support channel for assistance and guidance.

A more sophisticated upgrade technique is:

* Install YottaDB r1.26.
* Create a new replicated instance of your application (on the same system or a different system).
* Assuming the existing instance is A, and the new instance is B, upgrade B to r1.26 and start replicating from A to B.
* Once B catches up, switchover so that B is in a primary role replicating to A.
* Once you are satisfied with B, remove (or upgrade) A.

## Change History

### r1.26

YottaDB r1.26 includes the following changes from [YottaDB r1.24](https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.24).

| ID           | Category                            | Summary                                                                      |
| ----------   | ----------------------------------  | ---------------------------------------------------------------------------- |
| ([#111](#x111))       | DB                                  | MUPIP JOURNAL ROLLBACK and MUPIP JOURNAL RECOVER BACKWARD correctly rundown database files after abnormal process terminations |
| ([#370](#x370))       | Language                            | Provide a mechanism to set the C→M call-in table                             |
| ([#382](#x382))       | Other                               | Clear locks abandoned by processes terminating abnormally                    |
| ([#405](#x405))       | Other                               | Debian 10 is a Supported platform                                            |
| ([#416](#x416))       | Admin                               | ydb\_env\_set in M mode when $ydb\_dist is a symbolic link sets ydb\_dist to the fully resolved real path |
| ([#417](#x417))       | Language                            | ydb\_subscript\_next\_s(), ydb\_subscript\_next\_st(), ydb\_subscript\_previous\_s(), and ydb\_subscript\_previous\_st() return YDB\_ERR\_NODEEND when there is no successor / predecessor subscripted local variable node |
| ([#418](#x418))       | Language                            | ydb\_file\_id\_free()/ydb\_file\_is\_identical()/ydb\_file\_name\_to\_id() and their \_t variants should issue PARAMINVALID error if input filename/fileid pointer is NULL |
| ([#419](#x419))       | DB                                  | Source Server processes do not help flush journal buffers for a frozen instance |
| ([#420](#x420))       | Language                            | Performance of Simple API functions for multi-threaded applications comparable to those for single-threaded applications; both faster |
| ([#421](#x421))       | Admin                               | Automatically manage environment variables for plugins that meet the standard |
| ([#423](#x423))       | DB                                  | On AARCH64/ARM64 platform, MUPIP INTRPT works correctly                       |
| ([#424](#x424))       | Language                            | $ZTRIGGER("ITEM",...) works correctly in workload with significant concurrent updates |
| ([#425](#x425))       | Language                            | ydb\_message() and ydb\_message\_t() return YDB\_ERR\_PARMINVALID when second parameter is NULL |
| ([#428](#x428))       | Language                            | ydb\_timer\_start() timer\_id parameter type is intptr\_t                     |
| ([#429](#x429))       | Admin                               | ydb\_env\_set and ydb\_env\_unset handle an expanded set of "out of the box" use cases |
| ([#430](#x430))       | DB                                  | $ZTRIGGER() and MUPIP TRIGGER accept subscripts with decimal points and treat as identical different descriptions of the same subscript |
| ([#431](#x431))       | Language                            | LVUNDEF error in SimpleAPI reports variable name; also string subscripts are appropriately quoted |
| ([#434](#x434))       | Language                            | ydb\_exit() reports INVDBEXIT error when called from C code invoked from M code |
| ([#435](#x435))       | Admin                               | source command using ydbinstall script leaves user in deleted directory         |
| ([#440](#x440))       | Language                            | Line in a routine with XECUTE of a literal works correctly |
| ([#446](#x446))       | Language                            | Call-ins where the return value is a string checked for overflow |
| ([#447](#x447))       | Language                            | \*ret\_subs\_used is 0 when returning YDB\_ERR\_NODEEND |
| ([#449](#x449))       | Language                            | $order(gvn,-1), $zprevious(gvn), and $query(gvn,-1) work correctly with concurrent use of $increment() |
| ([#450](#x450))       | Language                            | A large number of calls to $QUERY(lvn,-1) does not lead to a STACKCRIT error                |
| ([#452](#x452))       | Language                            | M routines returning no value and passed strings by invoking C code return execute correctly |
| ([#453](#x453))       | Other                               | yottadb is the main executable to which mumps is a symbolic link                |
| ([#454](#x454))       | Other                               | Default value of ydb\_routines if not set on yottadb/mumps process startup      |
| ([#455](#x455))       | Other                               | ydbcrypt\_interface.h and ydb\_tls\_interface.h available $ydb\_dist                              |
| ([#456](#x456))       | Language                            | ZWRITE of a global issues DBFILERR when specified database file does not exist |
| ([#460](#x460))       | Language                            | Maximum M source code line length is 32,766 bytes |
| ([#462](#x462))       | Admin                               | MUPIP STOP of processes with top level code written in a language other than M terminates processes cleanly |
| ([#463](#x463))       | Language                            | Subsequent calls to YottaDB from threads spawned by a TP callback function work correctly |
| ([#464](#x464))       | DB                                  | SIGINT (Ctrl-C) of an application whose top level is a language other than M terminates the process at the earliest safe point |
| ([#467](#x467))       | Language                            | Processes whose top level is a language other than M report a GTMSECSHRPERM |

### Database

* <a name="x111"></a>MUPIP JOURNAL ROLLBACK and MUPIP JOURNAL RECOVER BACKWARD correctly rundown database files after abnormal process terminations (e.g., resulting from a `kill -9`, or an OS out-of-memory process termination) that leave shared memory segments in place. Previously, there was a small window of a few instructions in the terminated process that could cause the subsequent MUPIP JOURNAL process to hang indefinitely. Note that YottaDB strongly recommends against using `kill -9` to terminate processes, and to ensure adequate system resources. ([YDB#111](https://gitlab.com/YottaDB/DB/YDB/-/issues/111))

* <a name=""></a>During an instance freeze (indicated by `mupip replicate -source -freeze`), Source Server processes do not flush journal buffers to journal files, as they do when an instance freeze is not on. Previously, Source Servers could attempt to flush journal buffers, and hang until the freeze was released, which in turn could result in a replication backlog not being cleared during that time.([YDB#419](https://gitlab.com/YottaDB/DB/YDB/-/issues/))

* <a name="x423"></a>On the AARCH64/ARM64 platform, MUPIP INTRPT works correctly. Previously, it was possible for the M program receiving the interrupt to issue random errors. ([YDB#423](https://gitlab.com/YottaDB/DB/YDB/-/issues/423))

* <a name="x430"></a>$ZTRIGGER() and MUPIP TRIGGER load triggers for numeric subscripts with a decimal point. Previously, they raised parse errors that "." was an invalid character in the subscript. Additionally, trigger definitions for subscripted global variable references are better checked for equality. For example, trigger definitions for ^x(2), ^x(2.0) and ^x("2") are treated the same, as all 3 specifications map to the same node in the database file. Previously such specifications caused multiple triggers to be created, resulting in a single update of ^x(2) invoking multiple triggers, a potentially unintended consequence.

  If you suspect that an existing application database has triggers defined with non-canonical numbers, numbers specified as strings or if you are not sure, extract, delete, and reload all triggers after upgrading to r1.26. For example:

  ```
  mupip trigger -select /tmp/triggers.define # extract the current trigger definitions
  echo "-*" >/tmp/triggers.delete # create trigger file to delete all triggers
  mupip trigger -triggerfile=/tmp/triggers.delete # delete triggers
  mupip trigger -triggerfile=/tmp/triggers.define # reload trigger definitions
  ```

  ([YDB#430](https://gitlab.com/YottaDB/DB/YDB/-/issues/430))

* <a name="x464"></a>A SIGINT (Ctrl-C) of an application whose top level is a language other than M, terminates the process at the earliest safe point. Previously, a premature process termination at an unsafe point could result in structural damage to database files being updated by the process. ([YDB#464](https://gitlab.com/YottaDB/DB/YDB/-/issues/464))

### Language

* <a name="x370"></a>The Simple API Utility Functions [ydb\_ci\_tab\_open() / ydb\_ci\_tab\_open\_t()](https://docs.yottadb.com/ProgrammersGuide/extrout.html#call-in-intf) and [ydb\_ci\_switch() / ydb\_ci\_switch\_t()](https://docs.yottadb.com/ProgrammersGuide/extrout.html#call-in-intf) provide a mechanism for a process to switch between different call-in tables. This allows for more modular applications by allowing each plugin or library to have its own call-in table; previously, a process had a single call-in table. The call-in table specified by the environment variable `ydb_ci` is available at process startup without the need to explicitly open it (i.e., its behavior is unchanged). ([YDB#370](https://gitlab.com/YottaDB/DB/YDB/-/issues/370))

* <a name="x417"></a>`ydb_subscript_next_s()`, `ydb_subscript_next_st()`, `ydb_subscript_previous_s()`, and `ydb_subscript_previous_st()` return YDB\_ERR\_NODEEND and leave `*ret_value` unmodified when provided with a subscripted local variable as input, and there is no successor or predecessor node. In YottaDB r1.24, the functions could sometimes return YDB\_ERR\_LVUNDEF incorrectly or YDB\_OK, and could set `*ret_value` to the empty string.([YDB#417](https://gitlab.com/YottaDB/DB/YDB/-/issues/417))

* <a name="x418"></a>`ydb_file_id_free()`/`ydb_file_id_free_t()`, `ydb_file_is_identical()`/`ydb_file_is_identical_t()`, and `ydb_file_name_to_id()`/`ydb_file_name_to_id_t()` issue a PARAMINVALID error in case the input file name or file id parameter is NULL. Previously, they could cause the process to terminate abnormally with a SIG-11 if the file id parameter was NULL, and returned YDB\_OK in case the file name parameter was NULL. ([YDB#418](https://gitlab.com/YottaDB/DB/YDB/-/issues/418))

* <a name="x420"></a>The performance of Simple API functions to support multi-threaded applications (those whose names end in `_st` e.g,. `ydb_get_st()`) is comparable to those that support single-threaded applications. Previously, they were markedly (on the order of a half order of magnitude) slower. Functions to support single-threaded applications are also faster. ([YDB#420](https://gitlab.com/YottaDB/DB/YDB/-/issues/420))

* <a name="x424"></a>$ZTRIGGER("ITEM",expr2) works correctly in a workload with significant concurrent updates to database triggers from multiple processes. Previously, it could incorrectly return a parse error in the input string `expr2` even if no such error existed. ([YDB#424](https://gitlab.com/YottaDB/DB/YDB/-/issues/424))

* <a name="x425"></a>If the `*msg_buff` parameter is NULL, `ydb_message()` and `ydb_message_t()` return YDB\_ERR\_PARAMINVALID. Previously, this would result in process termination with a segmentation fault. Additionally, YDB\_ERR\_INVSTRLEN errors returned by `ydb_message()` and `ydb_message_t()` result in a subsequent `ydb_status()` returning the full YDB-E-INVSTRLEN error string. Previously, it returned an additional SYSTEM-W-UNKNOWN string. ([YDB#425](https://gitlab.com/YottaDB/DB/YDB/-/issues/425))

* <a name="x428"></a>The `timer_id` parameter of `ydb_timer_start()` is of type `intptr_t`. Previously, this was of type `int` which was inadequate for a 64-bit platform. ([YDB#428](https://gitlab.com/YottaDB/DB/YDB/-/issues/428))

* <a name="x431"></a>A YDB\_ERR\_LVUNDEF return status from `ydb_get_s()`/`ydb_get_st()` records the non-existent local variable name and any specified subscripts in the error string obtained by a subsequent `ydb_zstatus()` call. Previously `ydb_zstatus()` returned an error string that did not contain the local variable name but contained the subscripts. Additionally, string subscripts are reported within double-quotes. Previously they were reported without the surrounding double-quotes. ([YDB#431](https://gitlab.com/YottaDB/DB/YDB/-/issues/431))

* <a name="x434"></a>`ydb_exit()` returns with an error YDB\_ERR\_INVYDBEXIT when called from C code was invoked from M code. Previously, this abnormally terminated the process with a SIG-11. ([YDB#434](https://gitlab.com/YottaDB/DB/YDB/-/issues/434))

* <a name="x440"></a>Routine lines with an XECUTE of a literal string followed by one or more commands (i.e., the XECUTE is not the last command in the line) work correctly. Previously, an optimization of XECUTE of literal strings in routines caused such lines to execute incorrectly. For example, the line `W !,"start" X "F J=1:1:0 W !,J" W !,"stop"` in a routine works correctly, whereas previously it would print `"start"` but not `"stop"`. The workaround was to assign the literal to a variable. Direct mode was unaffected by this issue. ([YDB#440](https://gitlab.com/YottaDB/DB/YDB/-/issues/440))

* <a name="x446"></a>Call-ins where the return value is a string are checked for overflow (where possible) and return an INVSTRLEN error if the return area is not large enough, where previously it did not. Note that for string parameters, use of the `ydb_string_t` type is highly recommended as it facilitates checking for buffer overflows. A `char *` type does not facilitate such checks and is best avoided. ([YDB#446](https://gitlab.com/YottaDB/DB/YDB/-/issues/446))

* <a name="x447"></a>The parameter `*ret_subs_used`, which is intended to return results, is set to zero (0) when a call to `ydb_node_next_s()`, `ydb_node_next_st()`, `ydb_node_prev_s()`, or `ydb_node_prev_st()` has a return code of YDB\_ERR\_NODEEND. Previously, this value remained untouched under this case. ([YDB#447](https://gitlab.com/YottaDB/DB/YDB/-/issues/447))

* <a name="x449"></a>`$order(gvn,-1)`, `$zprevious(gvn)`, and `$query(gvn,-1)` work correctly with `$increment()` used by concurrent processes. Previously, it was possible in such an environment for processes to fail on rare occasions with a SIG-11 or to return incorrect results. ([YDB#449](https://gitlab.com/YottaDB/DB/YDB/-/issues/449))

* <a name="x450"></a>A sequence of a large number of calls to $QUERY(lvn,-1) does not lead to a STACKCRIT error. Previously, it could. ([YDB#450](https://gitlab.com/YottaDB/DB/YDB/-/issues/450))

* <a name="x452"></a>M routines that are called from C code with string (`ydb_string_t *` type) parameters and which return no value (i.e., the call-in table has a `void` return type) execute correctly. Previously, $[Z]PIECE() and $[Z]LENGTH() functions using these parameters could occasionally return incorrect results, resulting in incorrect execution of the M routines. ([YDB#452](https://gitlab.com/YottaDB/DB/YDB/-/issues/452))

* <a name="x456"></a>When preceded by a name level $ORDER(), $ZPREVIOUS(), or DO ^%GD, ZWRITE of a global issues a DBFILERR error when the database file for the global does not exist. Previously, it was possible for this to abnormally terminate the process with a KILLBYSIGSINFO1. ([YDB#456](https://gitlab.com/YottaDB/DB/YDB/-/issues/456))

* <a name="x460"></a>The maximum length for lines of M source code, and the maximum length of a single line of output when ZSHOW directs its output to a local variable, is 32,766 bytes. Previously, it was 8192 bytes. ([YDB#460](https://gitlab.com/YottaDB/DB/YDB/-/issues/460))

* <a name="x463"></a>Subsequent calls to YottaDB (e.g., `ydb_set_st()`) from threads spawned by a TP callback function (invoked from `ydb_tp_st()`) work correctly. Previously, this use case could cause the process to terminate abnormally with a SIG-11. ([YDB#463](https://gitlab.com/YottaDB/DB/YDB/-/issues/463))

* <a name="x467"></a>Processes whose top level is a language other than M report a GTMSECSHRPERM error if `$ydb_dist/gtmsecshr` is not owned by root or does not have the setuid bit set. Previously, such calls terminated the process abnormally with a SIG-11. ([YDB#467](https://gitlab.com/YottaDB/DB/YDB/-/issues/467))

### System Administration

* <a name="x416"></a>Sourcing `$ydb_dist/ydb_env_set` in M mode sets the `ydb_dist` environment variable to the fully resolved real path when `$ydb_dist` is a symbolic link, bringing M mode behavior in line with that of UTF-8 mode. Previously, it left that environment variable unaltered in M mode. ([YDB#416](https://gitlab.com/YottaDB/DB/YDB/-/issues/416))

* <a name="x421"></a>The `ydb_env_set` file when sourced also defines environment variables `ydb_xc_* / GTMXC_*` for `$ydb_dist/plugin/*.xc` files with M to C call-out tables. Previously, `ydb_env_set` ignored the call-out table files. ([YDB#421](https://gitlab.com/YottaDB/DB/YDB/-/issues/421))

* <a name="x429"></a>Under the directory specified by `$ydb_dir` (defaulting to $HOME/.yottadb), the `ydb_env_set` file when sourced:

  * Ensures a standard environment for YottaDB, creating one should an environment not exist.
  * Creates new database files for any regions that do not have database files, under the assumption that missing database files correspond to temporary regions with scratch globals.
  * If the database was not shut down cleanly (e.g., when a system is rebooted after a crash), recovers the database using MUPIP JOURNAL RECOVER BACKWARD if the database does not have replication turned on and MUPIP JOURNAL ROLLBACK BACKWARD if replication is turned on. All database regions to be recovered must have before image journaling enabled and on at the time of the crash.
  * Sets reasonable values for `ydb_*` environment variables and their `gtm*` counterparts (the latter so that application code and scripts that query the latter continue to work correctly).
  * Defines environment variables `ydb_xc_* / GTMXC_*` for `$ydb_dist/plugin/*.xc` files with M to C call-out tables.
  * Adds `$ydb_dist` to `$PATH`, as well as `$ydb_dist/plugin/bin` if it exists.
  * Sourcing `ydb_env_set` saves environment variables that it sets so that sourcing `ydb_env_unset` subsequently restores those variables to their prior values.

  Previously:

  * Sourcing `ydb_env_set` created an environment if one did not exist, and provided values to environment variables that were not set, letting the `ydb` script perform any needed recovery. However, that only served those using YottaDB using the M language, since those using YottaDB using other Supported languages would not use the `ydb` script.
  * Environment variables had to be explicitly added for M-to-C call-out tables in `$ydb_dist/plugin/*.xc` files.
  * Sourcing `ydb_env_set` created aliases for `mupip` and `mumps` (`gde` continues to be an alias).

  While the behavior of the combination of `ydb_env_set` and `ydb_env_unset` is not upward compatible (for example, it sets `ydb_routines` and `gtmroutines` to the environment under `$ydb_dir` where previously it made consistent existing values of `$ydb_routines` and `$gtmroutines`), they are intended to be upward compatible for common "out of the box" usage; with the expectation that more complex scenarios would use application-specific scripting. In addition to improved support for non-M users, they handle a larger number of situations that previously would have required application-specific scripting. ([YDB#429](https://gitlab.com/YottaDB/DB/YDB/-/issues/429))

* <a name="x435"></a>Sourcing the `ydbinstall` / `ydbinstall.sh` script reports an error. Previously, it completed the installation, but then left the shell in a directory that had been deleted. Note that the file is designed to be run as its own process, not sourced by an existing shell process. ([YDB#435](https://gitlab.com/YottaDB/DB/YDB/-/issues/435))

* <a name="x462"></a>MUPIP STOP of processes with top level code written in a language other than M terminates processes cleanly. Previously, a CALLINTROLLBACK error could be issued if the process was in a transaction (e.g., C functions `ydb_tp_s()` / `ydb_tp_st()`) or in called M code (e.g., using C functions `ydb_ci()`, `ydb_cip()`, `ydb_ci_t()` and `ydb_cip_t()`).  ([YDB#462](https://gitlab.com/YottaDB/DB/YDB/-/issues/462))

### Other

* <a name="x382"></a>The `LKE CLNUP` command clears the lock space of locks abandoned by processes that exited abnormally. In addition to the standard `-region` and `-all` flags, the command has two optional command line flags:

  * While the command by default runs once and terminates, the `-periodic=n` qualifier instructs LKE CLNUP to run in a loop, performing a cleanup every n seconds, a lighter weight operation than invoking the LKE command every n seconds from a shell script.
  * The `-integ` option instructs the command to also validate the data structures in the lock space for structural integrity.

  Note that processes release their locks on normal exit. Also, there is technically no need to run LKE CLNUP because a process whose attempt to acquire a lock is blocked by an abandoned lock will clean up the blocking abandoned locks. ([YDB#382](https://gitlab.com/YottaDB/DB/YDB/-/issues/382))

* <a name="x405"></a>Debian 10 (Buster) on x86\_64 is a Supported platform for YottaDB. Previously, it was Supportable, requiring manual installation or use of the `--force-install` flag for the `ydbinstall` script. This was previously reported as [Issue 354 on GitHub](https://github.com/YottaDB/YDB/issues/354). ([YDB#405](https://gitlab.com/YottaDB/DB/YDB/-/issues/405))

* <a name="x453"></a>The main executable of YottaDB is called `yottadb` to which `mumps` is a symbolic link. Either name can therefore be used. ([YDB#453](https://gitlab.com/YottaDB/DB/YDB/-/issues/453))

* <a name="x454"></a>If the environment variable `ydb_routines` is not set when the `yottadb` / `mumps` process starts, or if it is set to the empty string (`""`), YottaDB sets it in the environment to `$ydb_dist/libyottadbutil.so` if it exists, and to `$ydb_dist` if it does not, and then uses that value. Previously, it used a value of `"."`. While this is not backward compatible, the new behavior better protects against inadvertent execution of the wrong routine. Please let us know if this change to an edge case affects you. ([YDB#454](https://gitlab.com/YottaDB/DB/YDB/-/issues/454))

* <a name="x455"></a>To facilitate the compilation of YottaDB plugins, the files `ydbcrypt_interface.h` and `ydb_tls_interface.h` are made available both in `$ydb_dist` as well as in the tarball of the reference implementation of the encryption plugin at `$ydb_dist/plugin/source.tar`, with symbolic links `gtmcrypt_interface.h` and `gtm_tls_interface.h` respectively. Previously the files, named `gtmcrypt_interface.h` and `gtm_tls_interface.h`, were only provided in the tarball of the reference implementation of the encryption plugin. ([YDB#455](https://gitlab.com/YottaDB/DB/YDB/-/issues/455))

### GT.M V6.3006

| ID           | Category                            | Summary                                                                       |
| ----------   | ----------------------------------  | ----------------------------------------------------------------------------- |
| ([GTM-4263](#GTM-4263))   | Language                            | MUMPS commands accept prompted names in response to "What file: "             |
| ([GTM-6135](#GTM-6135))   | Language                            | $ZTIMEOUT manages a process wide timed interrupt                              |
| ([GTM-7952](#GTM-7952))   | Language                            | Improve memory and signal management for external calls                       |
| ([GTM-8017](#GTM-8017))   | Other                               | ^%TRIM accepts characters other than <SP> and <TAB> to trim                   |
| ([GTM-8178](#GTM-8178))   | Language                            | Normalize YottaDB compiler invocations from ZCOMPILE, ZLINK, auto-ZLINK and the MUMPS command |
| ([GTM-8518](#GTM-8518))   | Admin                               | MUPIP REPLICATE -EDITINSTANCE requires standalone access and supports -CLEANSLOTS |
| ([GTM-8933](#GTM-8933))   | DB                                  | YottaDB limits the number of errors from processes attempting to open a statsDB |
| ([GTM-8947](#GTM-8947))   | Language                            | Performance enhancement for $TRANSLATE() when arguments two and three are literals |
| ([GTM-8993](#GTM-8993))   | DB                                  | A process that fails to open a statsDB does not establish the location it used |
| ([GTM-8998](#GTM-8998))   | Language                            | External calls can return all available types                                  |
| ([GTM-9005](#GTM-9005))   | Admin                               | Appropriate exit status from MUPIP LOAD                                       |
| ([GTM-9011](#GTM-9011))   | Admin                               | MUPIP SET accepts -KEY\_SIZE or -RESERVED\_BYTES in the same command as -RECORD\_SIZE |
| ([GTM-9017](#GTM-9017))   | Other                               | Prevent segmentation violation when invoking $ydb\_procstuckexec                 |
| ([GTM-9024](#GTM-9024))   | DB                                  | Improve LOWSPC reporting                                                        |
| ([GTM-9025](#GTM-9025))   | Other                               | Restore conversion performance in percent routines for smaller numbers          |
| ([GTM-9031](#GTM-9031))   | Other                               | Update Cmake build scripts to be compatible with current Cmake releases         |
| ([GTM-9038](#GTM-9038))   | Other                               | YottaDB excludes TLS 1.0/1.1 unless configured to use older SSL/TLS protocols   |

### Database

* <a name="GTM-8933"></a>If misconfigured processes get the same error when opening a statsDB, YottaDB throttles the messages such that every hundredth message goes to the operator log; previously every process reported the issue. ([GTM-8933](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-006_Release_Notes.html#GTM-8933) )

* <a name="GTM-8993"></a>A process which cannot open a statsDB disables itself from maintaining the shared statistics, but does not disable subsequently starting processes, which better enables changes to an incorrectly configured environment. Previously, an initializing process that could not access a statsDB could also effectively require all processes using that database to restart in order to enable statistics sharing for the region. ([GTM-8993](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-006_Release_Notes.html#GTM-8993))

* <a name="GTM-9024"></a>When the database reaches the 88% size threshold, and for every 1% increase in size and beyond, YottaDB reports the blocks used in the LOWSPC warning as the sum of the data blocks and the local bit map blocks. Previously, YottaDB attempted to report the total blocks used as just the data blocks to match the 'total' field output by `MUPIP INTEG`. Additionally, YottaDB prints an accurate message about the percent usage when one of these threshold sizes is reached. ([GTM-9024](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-006_Release_Notes.html#GTM-9024))

### Language

* <a name="GTM-4263"></a>MUMPS commands without an argument accept an appropriate response after the "What file: " prompt. The "What file: " prompt may be more appropriate to a `mumps -run` than for compilation only. Previously, YottaDB appeared to ignore all input at this prompt and optionally allowed MUPIP to accept file or region on a separate line. ([GTM-4263](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-006_Release_Notes.html#GTM-4263))

* <a name="GTM-"></a>The `$ZTIMeout=([timeout]\[:labelref])` Intrinsic Special Variable (ISV) controls a single process wide timer. The optional timeout in seconds specifies with millisecond accuracy how long from the current time the timer interrupts the process. If the specified timeout is negative, YottaDB cancels the timer. If the timeout is zero, YottaDB treats it as it would a DO of the vector. The optional labelref specifies a code vector defining a fragment of M code to which YottaDB transfers control as if with a DO when the timeout expires. If the timeout is missing, the assignment must start with a colon and only changes the vector, and in this case, if the vector is the empty string, YottaDB removes any current vector. Note that YottaDB only recognizes interrupts, such as those from `$ZTIMEOUT` at points where it can properly resume operation, for example, at the beginning of a line, when waiting on a command with a timeout, or when starting a FOR iteration. When a ztimeout occurs, if the last assignment specified no vector, YottaDB uses the current `$ETRAP` or `$ZTRAP` with a status warning of `ZTIMEOUT`. YottaDB rejects an attempted KILL of `$ZTIMeout` with an error of `%YDB-E-VAREXPECTED`, and an attempted NEW of `$ZTIMeout` with an error of `%YDB-E-SVNONEW`. ([GTM-6135](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-006_Release_Notes.html#GTM-6135))

Example:

```
   YDB>zprint ^ztimeout
   ztimeout
     ; Display $ztimeout
       write !,$ztimeout               ; display $ZTIMeout - in this case the initial value -1
     ; set with a vector (do ^TIMEOUT)
       set $ztimeout="60:do ^TIMEOUT"  ; timeout of 1 minute. After timeout expires, XECUTEs do ^TIMEOUT
       write !,$ztimeout               ; displays the remaining time:vector until timeout
     ; set without a vector
       set $ztimeout=120               ; set the timeout to 2 minutes without changing the vector
       set $ztimeout="1234do ^TIMEOUT" ; missing colon creates a timeout for 1234 seconds
       set $ztimeout="10:"             ; set the timeout to 10 seconds and vector to current etrap or ztrap
       set $ztimeout=-1                ; set cancels the timeout
     ; Note that set to 0 triggers an immediate timeout
       set $ztimeout=0                 ; triggers the current vector
       set $ztimeout="0:DO FOO"        ; this has the same effect as DO FOO

   YDB>
```

* <a name="GTM-7433"></a>Name-level `$ORDER(,-1)` and `$ZPREVIOUS()` return an empty string when they reach the trigger definitions (stored in ^#t) as it is not a normally accessible global. Since the introduction of triggers, if there were trigger definitions, these functions could return ^#t. ([GTM-7433](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-006_Release_Notes.html#GTM-7433))

* <a name="GTM-7952"></a>YottaDB protects buffers used for external calls and produces an `EXTCALLBOUNDS` error if the external call attempts to exceed the space requested by the call table definition. Previously, YottaDB did not provide this protection and used a less efficient strategy for managing the space. Additionally, when an external call exceeds its specified preallocation (`ydb_string_t *` or `ydb_char_t *` output), YottaDB produces an `EXCEEDSPREALLOC` error. Previously YottaDB did not immediately detect this condition, which could cause subsequent hard to diagnose failures.

  YottaDB supports call-specific options in external call tables by appending a colon to the end of the line followed by zero or more space separated, case-insensitive keywords. The SIGSAFE keyword attests that the specific call does not create its own signal handlers, which allows YottaDB to avoid burdensome signal handler coordination for the external call. Previously, and by default, YottaDB saves and restores signal setups for external calls. ([GTM-7952](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-006_Release_Notes.html#GTM-7952))

* <a name="GTM-8178"></a>When the name of a source file is not a valid routine name, YottaDB issues a `NOTMNAME` error and does not produce an object file. Previously, YottaDB eventually gave an error when attempting use the routine. The `-OBJECT` compilation qualifier used without the `-NAMEOFRTN` qualifier implicitly names the first routine to match the name of the object qualifier. Note that, as before, listing files take on the name of the source rather than the name of the routine, and the `-NAMEOFRTN` or `-OBJECT` qualifiers in `$ZCOMPILE` are problematic to use with ZLINK commands as they apply to every ZLINK argument. Previously the qualifier applied to all files specified by the same MUMPS or ZCOMPILE command such that all sources received the same object name, which meant the last file was the only one that endured. Also, ZCOMPILE, as documented, accepts qualifiers in its argument prior to the routine list; previously it did not. ZCOMPILE with a wildcard works reliably; previously it stopped compiling routines after encountering a large source file. In addition, the MUMPS and ZCOMPILE commands default file specifications without a .m file extension to have one and they only compile files with a .m extension; previously they did not, although some other facilities did require a .m file extension for source files. As before, explicit ZLINK of a source (.m) file always places the object in the same directory as the specified source. While we are not aware of customers with a practice of using non .m extensions or module names that are not valid M names at compilation, but subsequently rename the object modules, this change requires revision of such practices. ([GTM-8178](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-006_Release_Notes.html#GTM-8178))

* <a name="GTM-8947"></a>When the second and third argument of `$TRANSLATE` are literals, the YottaDB compiler calculates the tables used by the translation. Previously, the tables were always prepared at run-time. ([GTM-8947](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-006_Release_Notes.html#GTM-8947))

* <a name="GTM-8998"></a>YottaDB can return any of the types documented in the external calls API. For C external calls, to prevent memory leaks when returning any pointer types, YottaDB requires the application to allocate returns of these types using `ydb_malloc`. Note that using the standard malloc for these types (or not explicitly allocating at all) produces a `YDB-F-ASSERT`. For Java external calls, the plugin manages any necessary allocations. Previously, YottaDB had the ability to return any of the documented types, however the Programmer's Guide did not document the ability to return anything other than an integer status or a long (for Java calls). Additionally, even though YottaDB had the ability to return a range of types, it did not perform the appropriate check for available space in the string pool when returning `ydb_char_t*`, `ydb_char_t**`, or `ydb_string_t*`, which could lead to unpleasant symptoms including a segmentation violation (SIG-11) or incorrect results. Also, attempting to return null values from a C external call results in a `%YDB-E-XCRETNULLREF` error, and attempting to return null from a Java external call results in a `%YDB-E-JNI` error. ([GTM-8998](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-006_Release_Notes.html#GTM-8998))

### System Administration

* <a name="GTM-8518"></a>`MUPIP REPLICATE -EDITINSTANCE` supports a `-CLEANSLOTS` qualifier. When specified, MUPIP goes through all slots (currently 16) in the replication instance file, identifies the slots that are inactive, and clears them to make them available for reuse. Also, except in the case where an originating primary instance has crashed, `MUPIP REPLICATE` grabs a "standalone" resource lock for processing `-EDITINSTANCE`. Previously, MUPIP did not use a resource lock when acting on an `-EDITINSTANCE`. ([GTM-8518](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-006_Release_Notes.html#GTM-8518))

* <a name="GTM-9005"></a>`MUPIP LOAD` returns non-zero exit status for load errors. Previously, in some cases, it inappropriately returned a 0 (Zero) exit status when it had been unable to load one or more records. ([GTM-9005](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-006_Release_Notes.html#GTM-9005))

* <a name="GTM-9011"></a>`MUPIP SET` accepts `-KEY_SIZE` or `-RESERVED_BYTES` and `-RECORD_SIZE` in the same command; in previous versions, they were not incompatible, but `MUPIP SET` continued to give an error when they were combined. ([GTM-9011](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-006_Release_Notes.html#GTM-9011))

### Other

* <a name="GTM-8017"></a>The `^%TRIM()` utility allows the specification of what character(s) to trim from either the left and/or right hand side of a given string. The default trim characters are $CHAR(32,9) (`<SP>` and `<TAB>`), these can be overridden by passing a string consisting of the desired characters in the optional second parameter. This functionality has existed for some time but was undocumented and not regularly tested. ([GTM-8017](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-006_Release_Notes.html#GTM-8017))

* <a name="GTM-9017"></a>YottaDB appropriately invokes `$ydb_procstuckexec` when it encounters a situation, such as `BUFOWNERSTUCK`, when another process abnormally holds some resource for too long. Previously, under a rare sequence of events the invocation of `ydb_procstuckexec` could result in a segmentation violation (SIG-11). ([GTM-9017](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-006_Release_Notes.html#GTM-9017))

* <a name="GTM-9025"></a>The following utilities: %DH, %DO, %HD %OD and %UTF2HEX have optimizations for the sizes most likely to be used in the YottaDB environment. Previously, GTM-5574 extended the maximum size supported by the conversion utilities but that caused a performance reduction for the most common cases. ([GTM-9025](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-006_Release_Notes.html#GTM-9025))

* <a name="GTM-9031"></a>The YottaDB Cmake build scripts now work with Cmake v3; previously they used a feature (the debug property) deprecated in that Cmake release. ([GTM-9031](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-006_Release_Notes.html#GTM-9031))

* <a name="GTM-9038"></a>YottaDB TLS encrypted sockets disallow TLS 1.0 and TLS 1.1 protocols. Previously, YottaDB disallowed only SSLv2 and SSLv3 protocols. If you need to selectively re-enable these protocols, please refer to the `ssl_options` configuration option in "[Creating a TLS Configuration File](https://docs.yottadb.com/AdminOpsGuide/tls.html#creating-a-tls-configuration-file)" ([GTM-9038](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-006_Release_Notes.html#GTM-9038))

## GT.M V6.3-007

| ID           | Category                            | Summary                                                                       |
| ----------   | ----------------------------------  | ----------------------------------------------------------------------------- |
| ([GTM-4796](#GTM-4796))   | Language                            | Reserved name for socket pool                                                 |
| ([GTM-7318](#GTM-7318))   | Admin                               | Audit Direct Mode facility                                                    |
| ([GTM-8130](#GTM-8130))   | Other                               | Modify %GSEL to deal appropriately with names and wildcards                   |
| ([GTM-8626](#GTM-8626))   | Other                               | MUPIP JOURNAL requires different names for different output files             |
| ([GTM-8653](#GTM-8653))   | Admin                               | Prevent potential deadlock between MUPIP JOURNAL ROLLBACK and MUPIP REPLICATE -SOURCE -FREEZE=OFF |
| ([GTM-8665](#GTM-8665))   | Admin                               | Improve reporting on an interrupted MUPIP JOURNAL -RECOVER/-ROLLBACK          |
| ([GTM-8729](#GTM-8729))   | Other                               | MUPIP JOURNAL support for -NOLOSTTRANS and -NOBROKENTRANS                     |
| ([GTM-8871](#GTM-8871))   | DB                                  | Processes survive a StatDB out-of-space, and maintain statistics when switching between private and shared |
| ([GTM-8872](#GTM-8872))   | Other                               | Please see [GTM-8871](#GTM-8871) |
| ([GTM-8904](#GTM-8904))   | Other                               | Revise ^%RCE to not lose files when changing routines on multiple file systems |
| ([GTM-9003](#GTM-9003))   | DB                                  | Critical Resource Management Changes                                           |
| ([GTM-9042](#GTM-9042))   | Delete                              | Resolve some issues with $ZTIMEOUT                                             |
| ([GTM-9043](#GTM-9043))   | Language                            | The compiler detects too much concatenation in an expression in a way that does not prevent production of an object file |
| ([GTM-9047](#GTM-9047))   | Language                            | Improved $ZCSTATUS presentation of compilation results                         |
| ([GTM-9049](#GTM-9049))   | Language                            | Adjust WRITE of a concatenation expression in non side effect mode for consistency |
| ([GTM-9053](#GTM-9053))   | Admin                               | Resource management fixes for a couple of unusual cases                        |
| ([GTM-9056](#GTM-9056))   | Admin                               | MUPIP SET for -WRITES\_PER\_FLUSH and -TRIGGER\_FLUSH, both of which persist   |
| ([GTM-9058](#GTM-9058))   | Language                            | JOB error handling changes                                                     |
| ([GTM-9061](#GTM-9061))   | Other                               | ^%YGBLSTAT returns an empty string when directed to report on a nonexistent process |
| ([GTM-9065](#GTM-9065))   | Other                               | GDE treats canonic numeric subscripts as numerics rather than strings          |
| ([GTM-9071](#GTM-9071))   | Language                            | Fix ZMESSAGE to allow Boolean expressions in its argument                      |
| ([GTM-9072](#GTM-9072))   | Admin                               | GBLOFLOW message identifies the database file rather than a global             |
| ([GTM-9073](#GTM-9073))   | Other                               | Relationship between Maximum Key Size and Maximum Reserved Bytes               |
| ([GTM-9074](#GTM-9074))   | Other                               | GDE accepts values between 2048 and 8388607 for JOURNAL ALLOCATION             |
| ([GTM-9075](#GTM-9075))   | Other                               | GDE accepts values within quotes and exits on first out of bounds error        |

## Database

* <a name="GTM-8871"></a>If a YottaDB process receives a SIGBUS (SIG-7) signal when attempting to register itself in a statistics database, it prints a `STATSDBMEMERR` message in the system log, indicating the need to provide sufficient space for the StatsDB file to expand. The process then reverts to process-private statistics collection. After the cause of the SIGBUS has been addressed, the process may turn statistics database logging back on by doing a `VIEW "STATSHARE":"REGION"`, at which point the process returns to doing updates to the corresponding statistics database. Previously, a YottaDB process that received a SIGBUS while trying to do statistics database logging terminated, producing a core file. Additionally, when a YottaDB process changes between private and shared statistics collection, it copies the current statistics into the appropriate statistics location so collected statistics persist. Previously, YottaDB did not properly copy over process-private statistics, meaning that switching over to a statistics database lost any previously collected statistics. ([GTM-8871](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-007_Release_Notes.html#GTM-8871))

* <a name="GTM-9003"></a>YottaDB uses facilities provided by the operating system to protect critical database and journal pool resources on Linux. Previously, YottaDB used a combination of multiple operating system facilities and its own logic to provide this protection. Several ZSHOW "G" mnemonics behave differently with this change. CQS, CQT, CYS, and CYT are not maintained and contain zeros. CFT and CFE are maintained, but are only incremented a single time for each observed instance of contention, whereas previously it counted the number of low-level synchronization operations performed, which would typically have been significantly larger. CFS is incremented a single time along with CFT (as the square of one is one.) CAT is maintained as before. `$VIEW("PROBECRIT")` returns valid CPT and CAT fields, but zero for all other fields. ([GTM-9003](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-007_Release_Notes.html#GTM-9003))

## Language

* <a name="GTM-4796"></a>SOCKET devices use "YGTMSOCKETPOOL" to identify the socket pool; an attempt to OPEN a device of that name produces a `DEVNAMERESERVED` error. Note this change requires adjustment of any code explicitly referencing the socket pool. Previously, SOCKET devices used the name "socketpool" to designate the socket pool and an OPEN of a device with that name prevented the use of the pool or access to any devices sockets in it. ([GTM-4796](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-007_Release_Notes.html#GTM-4796))

* <a name="GTM-9043"></a>YottaDB detects the case of more concatenation operands in a row than it can handle when parsing the source code; previously, it detected this at code generation, which meant it always failed to create an object file in this case. ([GTM-9043](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-007_Release_Notes.html#GTM-9043))

* <a name="GTM-9047"></a>$ZCSTATUS holds an indication of the result of the last ZCOMPILE, ZLINK, $ZTRIGGER() or auto-zlink compilation. One (1) indicates a clean compilation, a positive number greater than one is an error code you can turn into text with $ZMESSAGE(), and a negative number is a negated error code that indicates that YottaDB was not able to produce an object file. The error details appear in the compilation output, so $ZCSTATUS typically contains the code for ERRORSUMMARY. Previously, $ZSTATUS almost always indicated a one (1) except when object file creation failed. $ZTRIGGER() and MUPIP TRIGGER don't install trigger definitions with XECUTE strings that do not compile without error; previously they did. In addition, the value for $ZCSTATUS provided by ZSHOW "I" matches that provided by `WRITE $ZCSTATUS`; previously ZSHOW provided a zero (0) when it should have provided a one (1). ([GTM-9047](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-007_Release_Notes.html#GTM-9047))

* <a name="GTM-9049"></a>WRITE does not turn an expression starting with a concatenation operation into separate arguments if the expression is within parentheses. WRITE compilation turns a concatenated sequence into separate arguments which, when not processing in side effect mode, can affect the evaluation of side effects. Therefore the documentation contains the following: "The YottaDB compiler breaks a concatenated WRITE argument into a series of WRITE arguments to eliminate the overhead of the concatenation. If circumstances provide a reason for a single WRITE, perform the concatenation prior to the WRITE." Previously protecting the concatenation with parentheses in non-side effect mode did not suppress this optimization, which made the result inconsistent with separate evaluation. ([GTM-9049](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-007_Release_Notes.html#GTM-9049))

* <a name="GTM-9058"></a>YottaDB handles JOB errors differently. Errors associated with the specified routine, label, or offset appear in the error file of the JOBbed process in detail in addition to the `JOBFAIL` error received by the original process. Previously, YottaDB did not report the more specific errors and did not start the JOBbed process. YottaDB detects and reports `JOBLVN2LONG` errors in the original process; YottaDB does not start the JOBbed process in this case. Previously, the JOBbed process would report `JOBLVN2LONG` to its error file. ([GTM-9058](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-007_Release_Notes.html#GTM-9058))

* <a name="GTM-9071"></a>The ZMESSAGE command appropriately handles a Boolean expression within an argument; previously, such an argument tended to cause a segmentation violation (SIG-11). ([GTM-9071](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-007_Release_Notes.html#GTM-9071))

## System Administration

* <a name="GTM-7318"></a>YottaDB supports the ability to log actions initiated from a principal device including MUMPS commands typed interactively, or piped in by a script or redirect, from the principal device ($PRINCIPAL) and/or any information entered in response to a READ from $PRINCIPAL. An action initiated from $PRINCIPAL executes as usual when the Audit Principal Device is disabled, which it is by default. However, when the Audit Principal Device is enabled, YottaDB attempts to send the action out for logging before acting on it. Additionally, the `$ZAUDIT` Intrinsic Special Variable (ISV) provides a Boolean value that indicates whether or not the Audit Principal Device is enabled. Please see the **Additional information for GTM-7318 - Audit Principal Device** in this document for details. ([GTM-7318](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-007_Release_Notes.html#GTM-7318))

* <a name="GTM-8653"></a>`MUPIP JOURNAL -ROLLBACK -ONLINE -BACKWARD`, on encountering a frozen region when Instance Freeze is ON, releases all its resources and retries the rollback from the start. Previously, this could cause a potential deadlock with `MUPIP REPLICATE -SOURCE -FREEZE=OFF`. This issue was only observed in the development environment, and was never reported by a user. ([GTM-8653](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-007_Release_Notes.html#GTM-8653))

* <a name="GTM-8665"></a>MUPIP INTEG reports an interrupted `MUPIP JOURNAL -RECOVER/-ROLLBACK` operation on the database. Previously, a MUPIP INTEG on such a database did not report an interrupted recovery. Note: The `MUPIP dumpfhead` command already provided this information. YottaDB reports the "Recover interrupted" field with `DSE DUMP -FILEHEADER` even when journaling is turned off. Previously, YottaDB reported the "Recovery interrupted" field only with `DSE DUMP -FILEHEADER -ALL` and only when journaling was turned ON. ([GTM-8665](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-007_Release_Notes.html#GTM-8665))

* <a name="GTM-9053"></a>YottaDB does additional error checking when managing shared resources associated with relinkctl and replication update operations. Previously rare error conditions could leave the resources in an unintended status, resulting in core dumps and interfering with future relinkctl or replication update operations. This issue was only observed in the development environment, and was never reported by a user. ([GTM-9053](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-007_Release_Notes.html#GTM-9053))

* <a name="GTM-9056"></a>`MUPIP SET {-FILE|-REGION}` accepts `-TRIGGER_FLUSH=n` and `-WRITES_PER_FLUSH=n` qualifiers; previously, only DSE supported these qualifiers. Also, the `trigger_flush` value appears in `MUPIP DUMPFHEAD` as `trigger_flush_top` and acts as a stable limit; previously, YottaDB tended to lose any user supplied value as it made adjustments intended to improve performance. ([GTM-9056](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-007_Release_Notes.html#GTM-9056))

* <a name="GTM-9072"></a>`GBLOFLOW` messages identify the database file that is full; previously, they identified the global node the process was updating when it found no room, and in the case of a TP transaction, the report was for the last update in the transaction, which was not necessarily in the full database file. ([GTM-9072](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-007_Release_Notes.html#GTM-9072))

## Other

* <a name="GTM-8130"></a>The `%GSEL` utility now silently ignores any subscript in the search string and throws a non-fatal error if the input contains an invalid character. Previously, `%GSEL` would remove invalid characters and then perform the search. This could cause problems if a subscript was present because the utility removed "(" and ")" from the search pattern but not what was between them. This change also applies to `%GCE`, `%GD`, `%GO`, and `%GSE` which use `GD^%GSEL` to search for globals. In addition, when used interactively, it attempts to preserve the original I/O state of the caller; previously, it tended to leave that state disrupted. ([GTM-8130](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-007_Release_Notes.html#GTM-8130))

* <a name="GTM-8626"></a>MUPIP JOURNAL does not allow any two of `-EXTRACT`, `-LOSTTRANS` or `-BROKENTRANS` to specify the same file name unless they are special files (-stdout or /dev/null). Previously, MUPIP JOURNAL allowed overlapping file specifications, which lead to unexpected behavior including missing files and the specified file was not created. ([GTM-8626](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-007_Release_Notes.html#GTM-8626))

* <a name="GTM-8729"></a>MUPIP JOURNAL recognizes `-NOLOSTTRANS` and `-NOBROKENTRANS` as optional qualifiers which cause it to discard any lost or broken transactions rather than record them in files. Previously, MUPIP JOURNAL always produced files containing any lost and/or broken transactions as there was no such option to discard them. ([GTM-8729](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-007_Release_Notes.html#GTM-9072))

* <a name="GTM-8871"></a>Please see [GTM-8871](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-007_Release_Notes.html#GTM-8871). ([GTM-8872](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-007_Release_Notes.html#GTM-8872))

* <a name="GTM-8904"></a>The `^%RCE` utility deals appropriately with the case where $ZROUTINES lists source directories where the target directory is on a different file system than the default (first) source directory. Previously, the utility would delete the original, but then fail to move the modified copy to the correct directory. In addition, when used interactively, it attempts to preserve the original I/O state of the caller; previously, it tended to leave that state disrupted. ([GTM-8904](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-007_Release_Notes.html#GTM-8904))

* <a name="GTM-9061"></a>`^%YGBLSTAT` skips non-existent processes and returns an empty string when it is sent after an nonexistent PID. Previously, it could inappropriately return information on the reporting process or defunct processes. In addition, when used interactively, it attempts to preserve the original I/O state of the caller; previously, it tended to leave that state disrupted. Also, the utility versions its local working storage where previously it did not. ([GTM-9061](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-007_Release_Notes.html#GTM-9061))

* <a name="GTM-9065"></a>GDE treats canonic numeric string subscripts as numeric subscripts, in line with other YottaDB utilities. Previously, GDE treated them as string subscripts. In addition, `GDE SHOW -NAME <NAME>` prints the name-region mapping of all subscripts of the specified name. Previously, GDE only printed the name-region mapping of the unsubscripted name. ([GTM-9065](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-007_Release_Notes.html#GTM-9065))

* <a name="GTM-9073"></a>GDE and MUPIP maintain a consistent relationship between Maximum Key Size and Maximum Reserved Bytes. It adheres to the equation:

  `Maximum Reserved Bytes = Block Size - Key Size - (Size of Block Header + Size of Record Header + Size of Block id + B-star Record Size)`

  which is equivalent to

  `Block Size - Key Size - 32`

  Previously, they used inconsistent calculations and allowed inconsistent Maximum Key Size. ([GTM-9073](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-007_Release_Notes.html#GTM-9073))

* <a name="GTM-9074"></a>GDE accepts values between 2048 and 8388607 for JOURNAL ALLOCATION, with 2048 as the default value. Previously, GDE incorrectly accepted values between 200 and 16777216 with 200 as the default value. ([GTM-9074](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-007_Release_Notes.html#GTM-9074))

* <a name="GTM-9075"></a>For `-ACCESS_METHOD` and `-NULL_SUBSCRIPTS`, GDE accepts valid values enclosed within double quotes. Previously, GDE rejected valid values when enclosed within double quotes. If a value of any qualifier is out of bounds, GDE prints VALTOOSMALL/VALTOOBIG and exits immediately without further processing. Previously GDE continued to process that value and the values of other qualifiers and printed potentially confusing messages. ([GTM-9075](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-007_Release_Notes.html#GTM-9075))

## More Information

### Additional information for GTM-7318 - Audit Principal Device

Direct Mode receives a command line, YottaDB first checks if direct mode auditing or Audit Principal Device (APD) is enabled. If it is disabled, which it is by default, the command executes as usual. If it is enabled, YottaDB establishes a connection (via a UNIX/TCP/TLS socket) to a logger/listener process, and sends the to-be-logged command through the socket to the listener for logging. If sending is successful, YottaDB executes the logged command. If the connection is not successful or sending of the command fails, then YottaDB produces an error and does NOT execute the command.

**Enabling Audit Principal Device (APD)**

The `APD_ENABLE` entry in a restrictions definition file turns on APD and enables the logging of all code entered from Direct Mode and optionally any input entered on the principal device ($PRINCIPAL). To enable APD, add a line with the following format to the restriction file:

`APD_ENABLE:[comma-separated-list-of-options]:{path-to-sock-file|host:port}[:tls-id]`

* The optional "comma-separated-list-of-options" can consist of zero or more of these options:

  * TLS - Enables TLS connectivity between YottaDB and the logger; this option requires the host information (e.g. IP/port or hostname/port)
  * RD - Enables logging of all responses READ from $PRINCIPAL in addition to that entered at the Direct Mode prompt. This option is more comprehensive and captures input that might be XECUTEd, but depending on your application architecture may significantly increase the amount of logged information.

* The "path-to-sock-file" is the absolute path of the UNIX domain socket file for connecting to the logger.

* The "host" is the hostname or numeric IPv4/IPv6 address of the logger; numeric IP addresses must be enclosed in square brackets (i.e. '[' and ']').

* The "port" is the port number the logger listens on.

* The optional "tls-id" is the label of the section within the YottaDB configuration file that contains TLS options and/or certificates for YottaDB to use; APD ignores any "tls-id" if the "TLS" option is not specified.

If parsing the `APD_ENABLE` line in restriction file or initializing logger information fails, YottaDB enforces all restrictions (default restriction file behavior).

**Examples**

`APD_ENABLE::/path/to/sock/file/audit.sock`

Adding this line to the restriction file enables APD. YottaDB connects with the logger via UNIX domain socket using the domain socket file "/path/to/sock/file/audit.sock" and sends all Direct Mode activity from $PRINCIPAL to logger.

`APD_ENABLE:RD:[123.456.789.100]:12345`

Adding this line to the restriction file enables APD. YottaDB connects with the logger (listening on port 12345 at the IPv4 address 1enable23.456.789.100) via TCP socket and sends all Direct Mode and READ activities from $PRINCIPAL to logger.

`APD_ENABLE::loggerhost:56789`

Adding this line to the restriction file enables APD. YottaDB connects with the logger (listening on port 56789 at the hostname "loggerhost") using a TCP socket and sends all Direct Mode activities from $PRINCIPAL to logger.

`APD_ENABLE:TLS,RD:[1234:5678:910a:bcde::f:]:12345:clicert`

Adding this line to the restriction file enables APD. YottaDB connects with the logger (listening on port 12345 at the IPv6 address 1234:5678:910a:bcde::f:) via TLS socket. YottaDB configures its TLS options for APD based on the contents within the section of the configuration file labeled "clicert". YottaDB sends all Direct Mode and READ activities from $PRINCIPAL to logger.

**Logging**

The "logger" is a separate server-like program responsible for receiving the to-be-logged information from YottaDB and logging it. This separate program must be introduced by the user, either running in foreground or background, in order for logging to actually work. YottaDB distributions include basic example logger programs.

The six fields in the message, separated by semicolons (';'), contain information on the to-be-logged activity. Each to-be-logged message sent to the logger from YottaDB has the following format:

`dist=<path>; src={0|1|2}; uid=<uid>; euid=<euid>; pid=<pid>; command=<text>`

* The "dist" field, shows the path to location of the sender/user's `$ydb_dist` (YottaDB executables).
* The "src" field shows zero (0) for input from unknown source, one (1) for Direct Mode input, or two (2) for READ input from $PRINCIPAL.
* The next three fields ("uid", "euid", and "pid") show (respectively) decimal representations of the user ID, effective user ID, and process ID of the process that sent the message.
* The "command" field is the input provided on the YottaDB side.

**Examples**

`dist=/path/to/ydb_dist; src=1; uid=112233445; euid=112233445; pid=987654; command=write "Hello world",!`

`dist=/usr/library/r126/dbg; src=2; uid=998877665; euid=998877665; pid=123456; command=set a=789`

## Error Messages

## From GT.M V6.3-006

**ERRWZTIMEOUT**, Error while processing $ZTIMEOUT

Run Time Error: This indicates a problem invoking the current $ZTIMEOUT vector and usually accompanies other error messages

Action: Examine and correct the code vector specified by $ZTIMEOUT, or if there is none, examine the current value for $ETRAP or $ZTRAP. Unlike $ETRAP and code values for $ZTRAP, which are evaluated when they are assigned, compilation of $ZTIMEOUT vectors occurs when the vector is invoked by the expiration of the specified time.

**EXCEEDSPREALLOC**, Preallocated size ssss for M external call label LLLL exceeded by string of length SSSS.

Call out Error: The code invoked as external routine name LLLL returned a string of length SSSS, but the call table specified a maximum length of ssss for the return.

Action: Revise the external routine to abide by the call table size or change the call table to preallocate a suitably larger size.

**EXTCALLBOUNDS**, Wrote outside bounds of external call buffer. M label: LLLL.

Call out Fatal: The code invoked as external routine name LLLL violated the bounds of its allocated buffers.

Action: Ensure the non-YottaDB code uses appropriate allocations, pointer management logic and bounds checking.

**FILEEXISTS**, File xxxx already exists.

*This is an existing message with updated text.*

MUPIP Error: This indicates that MUPIP discovered a file with the filename xxxx already existing and did not overwrite it while executing the specified command(s). In many cases, this is an expected outcome when the action has an explicit or implicit target of multiple database files which may be in differing states.

Action: If appropriate, rename the already existing file xxxx and reissue the MUPIP command(s), or modify the MUPIP command to name (explicitly/implicitly) a file different from xxxx. If you encountered this error with MUPIP BACKUP, use the -REPLACE qualifier if you want to replace the existing backup files.

**FILERENAME**, File xxxx is renamed to yyyy.

*This is an existing message with updated text.*

Run Time Information: This indicates that an existing file xxxx has been renamed to yyyy so that a new file created with the original name does not overwrite the existing one. YottaDB renames files during an automatic journal switch in case no explicit journal file name is specified, in which case the message is sent to the operator log. The utilities (MUPIP, GT.CM) rename files while opening log files or journal extract files and they send the message to the terminal. YottaDB or utilities rename files only if the new file name specified already exists.

Action: This information message confirms the success of the file rename operation. No futher action is necessary unless there are other warning, fatal, and/or error category messages.

**GTMSECSHRPERM**, The gtmsecshr module in $ydb\_dist (DDDD) does not have the correct permission and UID (permission: PPPP, and UID: UUUU).

*This is an existing message with updated text.*

Run Time Error: This indicates that a client did not start the GTMSECSHR, installed to DDDD, because the executable was not owned by root (UUUU is the actual owner) and/or did not have setuid and/or execute permissions (actual permissions are PPPP).

Action: Arrange to provide the GTMSECSHR executable with the proper characteristics. The executable must be SETUID root with execute permissions for the current user.

**JNLCREATE**, Journal file xxxx created for \<database/region\> yyyy with aaaa.

*This is an existing message with updated text.*

MUPIP Information: This indicates that a journal file xxxx is created for database/region yyyy with `NOBEFORE_IMAGES` or `BEFORE_IMAGES` journaling option (aaaa).

Action: This informational message confirms the success of the new journal file creation operation for a region. No futher action is necessary unless there are other warning, fatal, and/or error category messages.

**JNLFILOPN**, Error opening journal file jjjj for database file dddd.

Run Time/MUPIP Error: This indicates that YottaDB was unable to open journal file jjjj for the specified database file dddd. The Source Server exits with a `JNLFILOPN` message after six failed attempts to open journal files.

Action: Check the authorizations for the user of the process and the health of the file system holding the journal file.

**JNLSPACELOW**, Journal file jjjj nearing maximum size, nnnn blocks to go.

*This is an existing message with updated text.*

Run Time Information: Depending on your settings for `ALLOCATION`, `AUTOSWITCHLIMIT`, and `EXTENSION` journaling options, you may see one to three `JNLSPACELOW` messages for each generation of a journal file. When the difference between `AUTOSWITCHLIMIT` and `ALLOCATION` is an exact multiple of `EXTENSION`, YottaDB attempts to write the `JNLSPACELOW` message to the operator log three times as a journal file reaches its maximum size. The first `JNLSPACELOW` message appears in the operator log when the available free space (blocks) in a journal file is equal to twice the `EXTENSION`, the second appears when the available free space is equal to `EXTENSION`, and the third appears when the journal file reaches the maximum size (`AUTOSWITCHLIMIT`). With `EXTENSION=0` or `EXTENSION=AUTOSWITCHLIMIT`, YottaDB logs the `JNLSPACELOW` message only once per journal file to the operator log.

Action: The `JNLSPACELOW` message is an information message and requires no action. However, you can use the `JNLSPACELOW` messages as part of monitoring journaling space requirements or as an operational practice to a trigger to intervene in journal file management. Use the frequency of `JNLSPACELOW` messages to proactively monitor how fast a journal file grows and as part of a monitoring alorithm that helps predict how soon the disk is likely to hit a quota limit.

**JNLSTATE**, Journaling state for \<database/region\> xxxx is now yyyy.

*This is an existing message with updated text.*

MUPIP Information: This indicates that journal state for the database/region xxxx is now yyyy.

Action: This information message confirms the success of journal state change operation. No futher action is necessary unless there are other warning, fatal, and/or error category messages.

**JNLSWITCHSZCHG**, Journal AUTOSWITCHLIMIT [aaaa blocks] is rounded down to [bbbb blocks] to equal the sum of journal ALLOCATION [cccc blocks] and a multiple of journal EXTENSION [dddd blocks].

*This is an existing message with updated text.*

MUPIP Information: This indicates that the specified `AUTOSWITCHLIMIT` value was rounded down as little as possible to make it aligned to the `ALLOCATION` + a multiple of `EXTENSION`. Any subsequently created journal file will use this value for `AUTOSWITCHLIMIT`.

Action: If the automatically rounded value for `AUTOSWITCHLIMIT` is inappropriate, specify an appropriate value for `ALIGNSIZE`, `ALLOCATION`, and/or `EXTENSION`.

**JNLSWITCHTOOSM**, Journal AUTOSWITCHLIMIT [aaaa blocks] is less than journal ALLOCATION [bbbb blocks] for database file dddd.

*This is an existing message with updated text.*

MUPIP Error: This indicates that the specified value or the automatically calculated value for `AUTOSWITCHLIMIT` from a `MUPIP SET JOURNAL` command is less than the default or specified value of `ALLOCATION`. This error also indicates that the `AUTOSWITCHLIMIT` value specified was greater or equal to the `ALLOCATION` but in turn got rounded down, and this rounded down value is less than the `ALLOCATION`.

Action: Specify a higher value of `AUTOSWITCHLIMIT` or specify an `ALLOCATION` value that is less than the `AUTOSWITCHLIMIT`.

**LOCKCRITOWNER**, LOCK crit is held by: PPPP.

Run Time/LKE Information: This shows any current owner of the resource managing M LOCKs.

Action: If a process persists in this state investigate what it's doing and, if appropriate, consider terminating it.

**LOCKSPACEINFO**, Region: rrrr: processes on queue: pppp/qqqq; LOCK slots in use: llll/kkkk; SUBSCRIPT slot bytes in use: ssss/tttt.

*This is an existing message with updated text.*

Run Time Error: This indicates that the environment attempted more concurrent M LOCKs than the configured `LOCK_SPACE` for region rrrr can support. pppp processes are waiting on a lock. llll locks are in use. qqqq and kkkk indicate maximum number of process queue entries, and maximum number of locks respectively.

Action: Analyze the LOCK protocol for efficiency. Use `mupip set -region -lock_space=size "rrrr"` to increase the lock space for region rrrr. To avoid the same problem the next time you recreate the database, use GDE to make the analogous change to lock\_space for the segment mapped to the ffff file in the global directory used to MUPIP CREATE this region.

**LOWSPC**, WARNING: Database DDDD has less than PPPP% of the total block space remaining. Blocks Used: UUUU Total Blocks Available: AAAA.

*This is an existing message with updated text.*

Operator log Information: The database has UUUU block in use and is appoaching its current limit of AAAA blocks. When the database reaches the 88% size threshold, and for every 1% increase in size and beyond, YottaDB reports the blocks used in the LOWSPC warning as the sum of the data blocks and the local bit map blocks.

Action: Purge data if possible. Consider a MUPIP REORG to compact the remaining data. Investigate whether migrating to a database created by a current version has a higher limit. Move some data to another, possibly new, region and delete it from this one.

**MLKCLEANED**, LOCK garbage collection freed aaaa lock slots for region rrrr.

LKE Information: `LKE CLNUP` was able to free lock slots when requested.

Action: No action required.

**MLKHASHRESIZE**, LOCK hash table increased in size from aaaa to bbbb and placed in shared memory (id = mmmm).

Operator log Information: YottaDB needed to expand a hash table used for managing LOCK information.

Action: No user action is required, but shared memory monitoring will show an additional shared memory segment with id mmmm.

**MLKHASHRESIZEFAIL**, Failed to increase LOCK hash table size from aaaa to bbbb. Will retry with larger size.

Operator log Warning: YottaDB needed to expand a hash table used for managing LOCK information needed to be expanded, but the initial attempt failed, necessitating a retry.

Action: A subsequent `MLKHASHRESIZE` indicates that the retry succeeded and no user action is required.

**MLKHASHTABERR**, A LOCK control structure is damaged and could not be corrected. Lock entry for LLLL is invalid.

LKE Error: `LKE CLNUP -INTEG` encountered an out-of-design situation for LOCK LLLL and was unable to repair it automatically.

Action: Immediately report the entire incident context with information from the operator log and any other relevant information to your support channel.

**MLKHASHWRONG**, A LOCK control structure has an invalid state; LOCK table failed integrity check TTTT.

LKE Error: `MLK CLNUP -INTEG` encountered damage to the data structures related to LOCK management. The text in TTTT describes whether LKE was able to correct the error or not.

Action: If LKE was not able to correct the error, immediately report the entire incident context with information from the operator log and any other relevant information to your support channel as soon as possible.

**MUNOACTION**, MUPIP unable to perform requested action.

*This is an existing message with updated text.*

MUPIP Error: This indicates that MUPIP encountered an error, which prevented the requested action.

Action: Review the accompanying message(s) to identify the cause that prevented MUPIP from performing the requested operation.

**NOTMNAME**, XXXX is not a valid M name.

Compile Time Error: M names must be ASCII, start with a "%" or an alpha and thereafter contain only alphanumeric characters. In YottaDB M, names are currently functionally limited to 31 characters, in most cases, by truncation.

Action: Correct the (typically) routine name to comply with the supported format. Names are also used for labels and both global and local variables. Note that YottaDB usually truncates names longer than its supported maximum, which YottaDB recommends against having as, while it can provide embedded information, can lead to ambiguity or other unintended behavior.

**RESRCWAIT**, Waiting briefly for the tttt semaphore for region rrrr (ffff) was held by PID pppp (Sem. ID: ssss).

Run Time Information: A process started a three (3) second wait for an FTOK or access control semaphore. If the process with PID pppp does not release the semaphore before the timeout expires, the waiting process bypasses acquiring the semaphore. tttt identifies the semaphore type: "FTOK" or "access control"; rrrr is the region; ffff is the database file corresponding to region rrrr; ssss is the semaphore ID.

Action: None required.

**XCRETNULLREF**, Returned null reference from external call LLLL.

Call out Error: The code invoked as external routine name LLLL returned a NULL pointer. While YottaDB accepts returns of a zero (0) value or an empty string, it does not support the return of a NULL pointer.

Action: Revise the external call code to return a pointer to an appropriate value.

**ZTIMEOUT**, ZTIMEOUT Time expired.

Run Time Warning: This warning message appears when $ZTIMEOUT expires and there were no vectors defined. If no error handlers are defined, YottaDB invokes the default trap which puts the control to Direct Mode.

Action: Check the message(s) for more information on where the timer expired in the current process. If needed, set an appropriate error handler to specify an action associated with $ZTIMEOUT expiry or define a $ZTIMEOUT with a vector.

## From GT.M V6.3-007

**APDCONNFAIL**, Audit Principal Device failed to connect to audit logger

Run Time Error: The facility for logging activity on principal devices is enabled, but is unable to form a connection with its configured logging program. This prevents a process from taking actions configured for logging initiated on its principal device ($PRINCIPAL).

Action: Check to make sure logger program is running and listening/accepting connections. If using a TCP or TLS-enabled logger, make sure the port number the logger is listening/accepting on matches the port number provided in the restriction file. Ensure the provided information (logger's connection info) in the restriction file is correct. Also make sure the line in restriction file is in correct format. If running a TLS-enabled logger, make sure the logger's TLS certificate is signed by a root CA that YottaDB is aware of through the TLS configuration file. Check syslog for more information on the error. After addressing identified issues, restart all processes subject to APD.

**APDINITFAIL**, Audit Principal Device failed to initialize audit information

Run Time Error: YottaDB was unable to process or initialize the provided information (e.g. IP, hostname, port number, UNIX domain socket file path, or TLS ID) from the restriction file. This prevents a process from taking actions configured for logging initiated on its principal device ($PRINCIPAL).

Action: Check the restriction file to make sure information is in proper format. After addressing identified issues, restart all processes subject to APD.

**APDLOGFAIL**, Audit Principal Device failed to log activity

Run Time Error: YottaDB was unable to send the to-be-logged activitiy to logger. This prevents a process from taking the action initiated on its principal device ($PRINCIPAL).

Action: Check to make sure that YottaDB is able to successfully connect to the logger program. Check syslog for more information on error.

**DEVNAMERESERVED**, Cannot use NNNN as device name. Reserved for YottaDB internal usage.

Run Time Error: This error appears when there is an attempt to OPEN a device with the name YGTMSOCKETPOOL. YottaDB internally reserves the name YGTMSOCKETPOOL to identify the socket pool and prevents any other device from using it.

Action: Use a different name for the SOCKET device.

**GBLOFLOW**, Database file FFFF is full

*This is an existing message with updated text.*

Run Time/MUPIP Error: This indicates that an error was encountered while extending database file FFFF.

Action: Examine the accompanying message(s) for the cause of the error. If the error is due to insufficient authorization, address that. If the error is due to TOTALBLKMAX (refer to the explaination of that message) or a lack of enough free space on the disk to fit the size of a database file, try performing a KILL of some nodes in the database to get free blocks in the existing allocated space (you may need to KILL several subscripted nodes before you can KILL a name node).

**ILLEGALUSE**, Illegal use of the special character "?" in %GSEL

Utility Error: This is an illegal use of the special character "?" in %GSEL. The special character "?" is not valid as the first character of a global name search pattern. "?" only valid as the first character of a search pattern when invoking the commands "?D" or "?d".

Action: Review and re-enter a valid search pattern.

**INVALIDGBL**, Search pattern is invalid

Utility Error: The search pattern used is invalid due to either using invalid characters or improper formatting.

Action: Review and re-enter a valid search pattern.

**ORLBKREL**, ONLINE ROLLBACK releasing all locking resources to allow a freeze OFF to proceed

MUPIP Information: MUPIP ROLLBACK -ONLINE encountered an Instance Freeze and must release its resources and restart to prevent a possible deadlock.

Action: None required as this is an informational message.

**ORLBKRESTART**, ONLINE ROLLBACK restarted on instance iiii corresponding to rrrr

MUPIP Information: MUPIP ROLLBACK -ONLINE is restarting on the instance iiii with replication journal pool rrrr.

Action: None required for this informational message.

**STATSDBMEMERR**, Process attempted to create stats block in statistics database SSSS and received SIGBUS--invalid physical address. Check file system space.

Run Time Error: A process attempted to enable shared statistics collection for the region associated with SSSS, but was unable to find room to add its records, so it cannot contribute to sharing. This message goes to the operator log facility rather than the process as an error, but the process continues without shared statistics.

Action: Adjust the environment so that SSSS can expand, and then, if possible, have the process again attempt to enable sharing.

**TOTALBLKMAX**, Extension exceeds maximum total blocks, not extending

*This is an existing message with updated text.*

Run Time Error: This indicates that the database file extension specified implicitly or explicitly (using MUPIP EXTEND) would cause the GDS file to exceed its maximum size. Please refer to the most recent Release Notes for maximum database sizes.

Action: Modify the extension to use a smaller size. This may indicate that you should move some contents of the database file to another file.

**TRANS2BIG**, Transaction exceeded available buffer space for region rrrr

*This is an existing message with updated text.*

Run Time Error: This indicates that a transaction updated more blocks than the global buffer could hold for a particular region rrrr or accessed more than the single transaction limit of 64K blocks.

Action: Look for missing TCOMMIT commands; modify the code to reduce the total content or change content of the transaction. If the transaction is as intended and the issue is the number of updates, increase the GLOBAL\_BUFFERS for the region using MUPIP SET, or modify the Global Directory to redistribute the relevant globals to more regions. If this occurs on a replicating instance it may indicate either a difference in configuration between the originating and replicating instances, which probably should be addressed, or a transaction that was borderline on the originating instance, but failed on the replicating instance because of difference in the database layout. In the later case, consider examining the application code to see if it's possible to reduce the size of the transaction, or alternatively increase the global buffers on both the instances.

**UNIQNAME**, Cannot provide same file name (nnnn) for ffff and FFFF

MUPIP Error: The command species the same name, nnnn for both output ffff and output FFFF.

Action: Revise the command to use unique names for different outputs.

## Legal Stuff

Copyright © 2019 YottaDB LLC

Permission is granted to copy, distribute and/or modify this document under the terms of the [GNU Free Documentation License, Version 1.3](http://www.gnu.org/licenses/fdl.txt) or any later version published by the Free Software Foundation; with no Invariant Sections, no Front-Cover Texts and no Back-Cover Texts.

YottaDB® is a registered trademark of YottaDB LLC.
GT.M™ is a trademark of Fidelity National Information Services, Inc.
Other trademarks belong to their respective owners.

This document contains a description of YottaDB and the operating instructions pertaining to the various functions that comprise the software. This document does not contain any commitment of YottaDB LLC. YottaDB LLC believes the information in this publication is accurate as of its publication date; such information is subject to change without notice. YottaDB LLC is not responsible for any errors or defects.
