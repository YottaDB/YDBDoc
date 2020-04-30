.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.     #
.. # All rights reserved.                                        #
.. #                                                             #
.. #     This source code contains the intellectual property     #
.. #     of its copyright holder(s), and is made available       #
.. #     under a license.  If you do not know the terms of       #
.. #     the license, please stop and do not read further.       #
.. #                                                             #
.. ###############################################################

=================
YottaDB r1.22
=================

.. contents::
   :depth: 2

------------------------------
Release Note Revision History
------------------------------

+-------------------------------+---------------------------------------+----------------------------------------------------------------------+
| Revision                      | Date                                  | Summary                                                              |
+===============================+=======================================+======================================================================+
| 1.00                          |                                       | r1.22 Initial Release                                                |
+-------------------------------+---------------------------------------+----------------------------------------------------------------------+

-----------------------------
Contact Information
-----------------------------

++++++++++++
YottaDB LLC
++++++++++++

40 Lloyd Avenue, Suite 104

Malvern, PA 19355, USA

info@yottadb.com

+1 (610) 644-1898

++++++++++++
Support
++++++++++++

**Customers**

Contact your YottaDB support channel.

**Others**

For free (to you) support from members of communities who run widely available applications on YottaDB, please use an application-specific list where appropriate.

* For issues specific to the use of YottaDB from node.js via `nodem <https://github.com/dlwicksell/nodem>`_, `QewdJS <http://qewdjs.com/>`_ or `Enterprise Web Developer <http://ewdjs.com/>`_, post to the `Enterprise Web Developer community <http://groups.google.com/group/enterprise-web-developer-community>`_.

* For issues specific to the use of YottaDB with `VistA <https://en.wikipedia.org/wiki/VistA>`_ flavors, post to the `Hardhats <http://groups.google.com/group/hardhats>`_ list.

* For issues specific to the use of YottaDB with M other than for applications above, post to the `comp.lang.mumps <http://groups.google.com/group/comp.lang.mumps>`_ list.

* If you are not sure where to post, or for requests other than to the above communities, post an issue at https://github.com/YottaDB/YottaDB/issues and include the words "help wanted" in the summary.


----------------------
r1.22
----------------------

+++++++++++++
Overview
+++++++++++++

YottaDB r1.22 is a minor release, primarily intended to make available in a YottaDB release the enhancements and fixes in the upstream GT.M release.

YottaDB r1.22 is built on (and except as noted, upward compatible with) both `YottaDB r1.20 <https://github.com/YottaDB/YottaDB/releases/tag/r1.20>`_ and `GT.M V6.3-004 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html>`_.

As always, there as other enhancements and fixes, as noted below. See our `Get Started <https://yottadb.com/product/get-started/>`_ page to use YottaDB.

++++++++++++++++++++++
Platforms
++++++++++++++++++++++

A platform is a combination of a CPU architecture and an operating system. A platform is Supported, Supportable, or Unsupported. Supported means that we have the platform in our development environment and test each release on that platform. Supportable means that although we do not necessarily have such a platform in our environment, we have no reason to believe that the software will not run on it. All others are Unsupported.

+---------------------------------------------------------+----------------------------------------------------+-------------------------------------------------------------------------------------+
| CPU Architecture                                        | Supported OS Version(s)                            | Notes                                                                               |
+=========================================================+====================================================+=====================================================================================+
| 64-bit x86                                              | Ubuntu 16.04 LTS; Red Hat Enterprise Linux 7.4     | Note that there are separate binary distributions for Ubuntu and Red Hat, owing to  |
|                                                         |                                                    | differences in library versions of those distributions.                             |
+---------------------------------------------------------+----------------------------------------------------+-------------------------------------------------------------------------------------+
| Raspberry Pi 3 Model B; BeagleBone Black Wireless       | Raspbian GNU/Linux 9.1; Stretch IoT (non GUI)      | While YottaDB r1.22 is Supportable on other ARMv7-A CPUs, owing to variations in the|
|                                                         |                                                    | implementations of ARM microarchitectures, we recommend that you ensure the software|
|                                                         |                                                    | runs correctly before committing to any specific hardware other than those Supported|
|                                                         |                                                    | Please contact info@yottadb.com if you want a specific combination of OS and CPU    |
|                                                         |                                                    | microarchitecture to be Supported.                                                  |
+---------------------------------------------------------+----------------------------------------------------+-------------------------------------------------------------------------------------+
| Raspberry Pi Zero                                       | Raspbian GNU/Linux 9.1                             | While YottaDB r1.22 is Supportable on other ARMv6 CPUs, owing to variations in the  |
|                                                         |                                                    | implementations of ARM microarchitectures, we recommend that you ensure the software|
|                                                         |                                                    | runs correctly before committing to any specific hardware other than those          |
|                                                         |                                                    | Supported. Please contact info@yottadb.com if you want a specific combination of OS |
|                                                         |                                                    | and CPU microarchitecture to be Supported.                                          |
+---------------------------------------------------------+----------------------------------------------------+-------------------------------------------------------------------------------------+

Recent releases of major GNU/Linux distributions with contemporary kernels, glibc and ncurses are Supportable. Running on Arch Linux requires the ncurses5-compat-libs package to be installed.


+++++++++++++++
Getting Started
+++++++++++++++

See our `Get Started <https://yottadb.com/product/get-started/>`_ page to use YottaDB.

We **strongly recommend** that you install YottaDB r1.22 in a newly created directory, different from those of YottaDB r1.20 and any GT.M versions you may have installed on the system.

+++++++++++++++++++++++++++++++++++++
Removing an installed YottaDB release
+++++++++++++++++++++++++++++++++++++

Assuming $ydb_dist points to the directory where YottaDB is installed:

* Cleanly shut down all application processes using that release.
* Execute mupip rundown && mupip rundown -relinkctl.
* Ensure that there are no gtcm* or gtmsecshr processes active.
* Use sudo lsof | grep $ydb_dist to ensure there are no open files.
* Delete the directory with sudo rm -rf $ydb_dist.

----------------------------
Upgrading to YottaDB r1.22
----------------------------

As YottaDB r1.22 is upward compatible from both YottaDB r1.20 and GT.M V6.3-004, the minimal upgrade steps are:

* Install YottaDB r1.22.
* Recompile any object code, and recreate shared libraries where appropriate.
* If you are using encryption, compile and install the reference implementation plugin or your customized plugin.
* Cleanly shut down the application and ensure that the database files are shut down using mupip rundown from the prior release.
* Switch journal files with the new YottaDB release.
* Start using the new YottaDB release.

To upgrade from older GT.M releases, first upgrade to GT.M V6.0-000 or later and follow the steps above, or contact your YottaDB support channel for assistance and guidance.

A more sophisticated upgrade technique is:

* Install YottaDB r1.22.
* Create a new replicated instance of your application (on the same system or a different system).
* Assuming the existing instance is A, and the new instance is B, start replicating from A to B.
* Once B catches up, switchover so that B is in a primary role replicating to A.
* Once you are satisfied with B, remove (or upgrade) A.

------------------------
Change History
------------------------

++++++++
r1.22
++++++++

YottaDB r1.22 includes the following changes from `YottaDB r1.20 <https://github.com/YottaDB/YottaDB/releases/tag/r1.20>`_.

+---------------------------------------------------------+-------------------------+----------------------------------------------------------------------------------------------------------------------+
| ID                                                      | Category                | Summary                                                                                                              |
+=========================================================+=========================+======================================================================================================================+
| `#102 <https://gitlab.com/YottaDB/DB/YDB/issues/102>`_  | Other                   | ydb_coredump_filter to control information in core dumps                                                             |
+---------------------------------------------------------+-------------------------+----------------------------------------------------------------------------------------------------------------------+
| `#159 <https://github.com/YottaDB/YottaDB/issues/159>`_ | Language                | $TEXT returns source code lines with both carriage return and newline line terminators removed                       |
+---------------------------------------------------------+-------------------------+----------------------------------------------------------------------------------------------------------------------+
| `#193 <https://github.com/YottaDB/YottaDB/issues/193>`_ | Language                | GDE @ command reports success after executing good command files                                                     |
+---------------------------------------------------------+-------------------------+----------------------------------------------------------------------------------------------------------------------+
| `#197 <https://github.com/YottaDB/YottaDB/issues/197>`_ | Language                | ydb_env_set preserves ydb_gbldir/gtmgbldir and ydb_routines/gtmroutines                                              |
+---------------------------------------------------------+-------------------------+----------------------------------------------------------------------------------------------------------------------+
| `#207 <https://github.com/YottaDB/YottaDB/issues/207>`_ | DB                      | TPRESTART syslog messages identify correct global name and message type                                              |
+---------------------------------------------------------+-------------------------+----------------------------------------------------------------------------------------------------------------------+
| `#209 <https://github.com/YottaDB/YottaDB/issues/209>`_ | Other                   | Complete set of ydb\_\* environment variables corresponding to all gtm\*/GTM\* environment variables                 |
+---------------------------------------------------------+-------------------------+----------------------------------------------------------------------------------------------------------------------+
| `#210 <https://github.com/YottaDB/YottaDB/issues/210>`_ | Admin                   | Source Server errors always appear in the Source Server log file                                                     |
+---------------------------------------------------------+-------------------------+----------------------------------------------------------------------------------------------------------------------+
| `#215 <https://github.com/YottaDB/YottaDB/issues/215>`_ | Admin                   | Ctrl-Z (the suspend signal) is honored in all cases                                                                  |
+---------------------------------------------------------+-------------------------+----------------------------------------------------------------------------------------------------------------------+
| `#217 <https://github.com/YottaDB/YottaDB/issues/217>`_ | DB                      | SET and $INCREMENT() work correctly without abnormal process termination in a very rare case                         |
+---------------------------------------------------------+-------------------------+----------------------------------------------------------------------------------------------------------------------+
| `#218 <https://github.com/YottaDB/YottaDB/issues/218>`_ | DB                      | ydb\_app\_ensures\_isolation provides initial setting for global variables where application design ensures Isolation|
+---------------------------------------------------------+-------------------------+----------------------------------------------------------------------------------------------------------------------+
| `#220 <https://github.com/YottaDB/YottaDB/issues/220>`_ | Other                   | mumps -nowarning does not display BLKTOODEEP messages                                                                |
+---------------------------------------------------------+-------------------------+----------------------------------------------------------------------------------------------------------------------+
| `#221 <https://github.com/YottaDB/YottaDB/issues/221>`_ | Admin                   | ydb_* environment variables to drive operation of ydbinstall.sh script                                               |
+---------------------------------------------------------+-------------------------+----------------------------------------------------------------------------------------------------------------------+
| `#224 <https://github.com/YottaDB/YottaDB/issues/224>`_ | Language                | Certain edge cases of VIEW and $VIEW() work correctly without terminating the process                                |
+---------------------------------------------------------+-------------------------+----------------------------------------------------------------------------------------------------------------------+
| `#228 <https://github.com/YottaDB/YottaDB/issues/228>`_ | Language                | $ZSEARCH() calls in a loop work correctly in certain edge cases                                                      |
+---------------------------------------------------------+-------------------------+----------------------------------------------------------------------------------------------------------------------+
| `#233 <https://github.com/YottaDB/YottaDB/issues/233>`_ | DB                      | Option to reduce IO impact of MUPIP REORG                                                                            |
+---------------------------------------------------------+-------------------------+----------------------------------------------------------------------------------------------------------------------+
| `#235 <https://github.com/YottaDB/YottaDB/issues/235>`_ | Other                   | Source server fails rarely with JNLSWITCHRETRY error when instance is frozen                                         |
+---------------------------------------------------------+-------------------------+----------------------------------------------------------------------------------------------------------------------+
| `#237 <https://github.com/YottaDB/YottaDB/issues/237>`_ | Other                   | Establish replication connections more efficiently in an edge case                                                   |
+---------------------------------------------------------+-------------------------+----------------------------------------------------------------------------------------------------------------------+
| `#247 <https://github.com/YottaDB/YottaDB/issues/247>`_ | DB                      | Epoch tapering works correctly without process termination from signal 8 in a rare case                              |
+---------------------------------------------------------+-------------------------+----------------------------------------------------------------------------------------------------------------------+

+++++++++++++++++
Database
+++++++++++++++++

* Type 0 and type 3 TPRESTART messages in the syslog (enabled by turning on TP restart logging) correctly report the global variable causing the restart. Furthermore, type 3 messages correctly identify themselves as type 3 messages. Previously, they could report an incorrect global variable reference, and type 3 messages were sometimes incorrectly identifed as type 2. (`#207 <https://github.com/YottaDB/YottaDB/issues/207>`_)

* SET and $INCREMENT() operations on a global variable work correctly without abnormal termination in a very rare case. Previously, it was possible a process doing the operation to terminate abnormally with a SIG-11. This was only observed in internal testing, and there was no risk of database damage from this issue. (`#217 <https://github.com/YottaDB/YottaDB/issues/217>`_)

* A string value of a comma-separated list of global variable names in the environment variable ydb\_app\_ensures\_isolation informs YottaDB that application design ensures the transaction property of Isolation for global variables in that list and YottaDB need not do so, potentially increasing application throughput by reducing random, accidental TP restarts. The effect is functionally equivalent to executing the M language command view "noisolation":"\<list\>" where "\<list\>" is the value of the environment variable. For example, export ydb\_app\_ensures\_isolation="^TMP,^XREF" is equivalent to a process executing view "noisolation":"^TMP,^XREF" before it makes any database accesses. Note that using the environment variable ydb\_app\_ensures\_isolation requires the environment variable ydb\_gbldir to be set to a valid global directory. The setting applies to the global variables mapped by that global directory. (`#218 <https://github.com/YottaDB/YottaDB/issues/218>`_)

* MUPIP SET -REORG_SLEEP_NSEC= specifies the number of nanoseconds that a MUPIP REORG process operating between blocks that it processes, with default value of 0 and a maximum of 999999999 (i.e. 999,999,999, or 1 nanosecond less than 1 second). Using non-zero values reduces the IO impact of MUPIP REORG, at the cost of increasing the duration of the operation. Note that the existing environment variable ydb_poollimit / gtm_poollimit is the appropriate technique to limit the impact of MUPIP REORG on global buffers; the -reorg_sleep_nsec can be used to limit the impact on the IO subsystem. (`#233 <https://github.com/YottaDB/YottaDB/issues/233>`_)

* Epoch tapering works correctly. Previously, on very rare occasions, epoch tapering could result in process termination with a signal 8 resulting from a divide-by-zero error. This issue was only observed in the development environment, and was never reported by a user. (`#247 <https://github.com/YottaDB/YottaDB/issues/247>`_)

+++++++++++++++++++++
Language
+++++++++++++++++++++

* $TEXT() returns source code lines with both the carriage return and newline characters removed from routine files that have lines ending with them. Previously, only the newline was removed. (`#159 <https://github.com/YottaDB/YottaDB/issues/159>`)

* The GDE @ command reports success after executing good command files. In r1.20, it would issue a YDB-E-IOEOF error even after sucessfully executing the commands in the file specfied. A workaround was to set the environment variable ydb_msgprefix to "GTM" for the GDE process. (`#193 <https://github.com/YottaDB/YottaDB/issues/193>`_)

* Values of environment variables ydb_gbldir/gtmgbldir and ydb_routines/gtmroutines set before sourcing ydb_env_set are preserved. The ydb_* versions of the environment variables is used if both ydb_* and gtm* versions are defined.  (`#197 <https://github.com/YottaDB/YottaDB/issues/197>`_)

* VIEW commands and $VIEW functions work correctly. Previously, edge cases in YottaDB, and newly introduced in GT.M V6.3-004, in commands like VIEW "NOISOLATION", functions like $VIEW("GVFILE"), etc., could terminate the process abnormally with a SIG-11, SIG-6, GTMASSERT2 error. (`#224 <https://github.com/YottaDB/YottaDB/issues/224>`_)

* $ZSEARCH() calls in a loop using the same stream and the same pattern work by ignoring edge cases of changes in the underlying files (such as a permission change of a directory that makes a file inaccessible to the process) and pathological cases such as an infinite symbolic link loop. Previously, ZSEARCH() calls in these edge case could incorrectly issue an error (SYSTEM-E-ENO40, SYSTEM-E-ENO13, etc.), with some difference in behavior between Ubuntu 18.04 LTS and prior releases resulting from a change in the underlying API. (`#228 <https://github.com/YottaDB/YottaDB/issues/228>`_)

++++++++++++++++++++++
System Administration
++++++++++++++++++++++

* Replication Source Server startup errors always appear in the Source Server log file. In previous versions of YottaDB, if the Source Server was started from the terminal, and the Source Server startup command failed or the Source Server successfully started but later failed (e.g. with errors like NULLCOLLDIFF, REPLOFFJNLON, REPLINSTNOHIST, SECNOTSUPPLEMENTARY etc.) this error message did not show up in the Source Server log file or on the terminal where it was started (assuming that terminal still exists at the time of the error). The release notes of GT.M V6.3-004 mention that this issue has been fixed by GTM-8576 but we found the issue exists even in GT.M V6.3-004. (`#210 <https://github.com/YottaDB/YottaDB/issues/210>`_)

* Ctrl-Z (the suspend signal) is honored by YottaDB processes in all cases. Previously, if the signal was delivered while the process was executing a section of code where it was not safe to suspend it, the signal was ignored and the user had to retry the Ctrl-Z. Note that as handling the signal is deferred when the process is in sensitive areas of code, it is possible for the response to be momentarily delayed. (`#215 <https://github.com/YottaDB/YottaDB/issues/215>`_)

* The ydbinstall.sh script uses environment variables ydb_buildtype, ydb_dryrun, ydb_gtm, ydb_group_restriction, ydb_keep_obj, ydb_lcase_utils, ydb_overwrite_existing, ydb_prompt_for_group, and ydb_verbose to drive its operation, corresponding to the environment variables gtm_buildtype, gtm_dryrun, gtm_gtm, gtm_group_restriction, gtm_keep_obj, gtm_lcase_utils, gtm_overwrite_existing, gtm_prompt_for_group, and gtm_verbose. The latter remain supported, with the proviso that if a ydb\_ prefixed environment and the corresponding gtm\_ prefixed environment variable are both defined, the former takes precedence. (`#221 <https://github.com/YottaDB/YottaDB/issues/221>`_)


+++++++++++++++++++++
Other
+++++++++++++++++++++

* A value of "0xXX" where XX are case-insensitive hexadecimal digits in the environment variable ydb_coredump_filter, sets the corresponding value to /proc/<pid>/coredump_filter (see man 5 core) at process startup without explicitly setting a value if unspecified. This controls the contents of core dumps generated by the process. A YottaDB process that terminates or is terminated abnormally (terminates with any signal except SIGKILL, SIGQUIT, or SIGTERM) generates a core file, if generating core files is enabled in the system and for that process. As core files can contain confidential / proprietary information, please limit the content of core files to that required for diagnostic purposes, and generate them with appropriate protection. If ydb_coredump_filter is not specified, but gtm_coredump_filter is, the latter environment variable is used. If both are specified, the former takes precedence. Previously, only the environment variable gtm_coredump_filter was recognized, with a default value of "0x73". (`#102 <https://gitlab.com/YottaDB/DB/YDB/issues/102>`_)

* YottaDB has a complete set of environment variables with "ydb\_" prefixes for environment variables with "gtm" and "GTM" prefixes. For example, ydb_routines is the equivalent of gtmroutines, ydb_chset is the equivalent of gtm_chset, ydb_repl_instance is the equivalent of gtm_repl_instance etc. YottaDB continues to support the gtm* environment variable if the corresponding ydb_* environment variable is not defined. If both variables are defined, the ydb_* definition takes precedence. The full list of environment variables is in the `Environment Variables section of the Administration and Operations Guide <https://docs.yottadb.com/AdminOpsGuide/basicops.html#environment-variables>`_. (`#209 <https://github.com/YottaDB/YottaDB/issues/209>`_)

* mumps -nowarning does not display BLKTOODEEP messages. Previously, mumps -nowarning displayed YDB-W-BLKTOODEEP errors even though it did not display other types of errors (e.g. YDB-E-EXPR). (`#220 <https://github.com/YottaDB/YottaDB/issues/220>`_)

* The replication source server works correctly even if the replication instance is frozen due to an error while switching journal files. In prior versions of YottaDB (and GT.M versions V6.3-001A and above), the source server could terminate with a JNLSWITCHRETRY error in this scenario in rare cases. (`#235 <https://github.com/YottaDB/YottaDB/issues/235>`_)

* Initiating replication connections between Source and Receiver Servers is more efficient. Previously, in rare cases, the Source Server unnecessarily disconnected the connection and reconnected. This is very similar to (`#136 <https://github.com/YottaDB/YottaDB/issues/136>`_) but under slightly different circumstances. (`#237 <https://github.com/YottaDB/YottaDB/issues/237>`_)

+++++++++++++++
GT.M V6.3-004
+++++++++++++++

+-------------------------------------------------------------------------------------------------------+------------------------------------+------------------------------------------------------------------+
| ID                                                                                                    | Category                           | Summary                                                          |
+=======================================================================================================+====================================+==================================================================+
| `GTM-1042 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-1042>`_ | Other                              | The gtm\_mstack environment variable can control the M stack size|
+-------------------------------------------------------------------------------------------------------+------------------------------------+------------------------------------------------------------------+
| `GTM-3146 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-3146>`_ | Other                              | MUPIP BACKUP command ignores aliases                             |
+-------------------------------------------------------------------------------------------------------+------------------------------------+------------------------------------------------------------------+
| `GTM-5730 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-5730>`_ | Admin                              | Update Process logs show record type descriptions                |
+-------------------------------------------------------------------------------------------------------+------------------------------------+------------------------------------------------------------------+
| `GTM-6747 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-6747>`_ | Admin                              | MUPIP SET -JOURNAL does not adjust sequence numbers on           |
|                                                                                                       |                                    | regions                                                          |
+-------------------------------------------------------------------------------------------------------+------------------------------------+------------------------------------------------------------------+
| `GTM-7483 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-7483>`_ | Other                              | Improve message from a MUPIP INTEG when a Directory Tree issue is|
|                                                                                                       |                                    | detected                                                         |
+-------------------------------------------------------------------------------------------------------+------------------------------------+------------------------------------------------------------------+
| `GTM-7872 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-7872>`_ | Admin                              | Improve message from a MUPIP INTEG directed to an non-existent   |
|                                                                                                       |                                    | database file                                                    |
+-------------------------------------------------------------------------------------------------------+------------------------------------+------------------------------------------------------------------+
| `GTM-7915 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-7915>`_ | DB                                 | Trigger key size not limited by database key size                |
+-------------------------------------------------------------------------------------------------------+------------------------------------+------------------------------------------------------------------+
| `GTM-8202 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-8202>`_ | Admin                              | Journal extract for specific sequence number(s)                  |
+-------------------------------------------------------------------------------------------------------+------------------------------------+------------------------------------------------------------------+
| `GTM-8576 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-8576>`_ | Admin                              | Source Server logs errors to its log file rather than stderr     |
+-------------------------------------------------------------------------------------------------------+------------------------------------+------------------------------------------------------------------+
| `GTM-8643 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-8643>`_ | Language                           | SOCKET device listen queue depth determined by OS configuration; |
|                                                                                                       |                                    | better retry management for local connects                       |
+-------------------------------------------------------------------------------------------------------+------------------------------------+------------------------------------------------------------------+
| `GTM-8699 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-8699>`_ | Language                           | Optional region argument for $VIEW("STATSHARE")                  |
+-------------------------------------------------------------------------------------------------------+------------------------------------+------------------------------------------------------------------+
| `GTM-8777 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-8777>`_ | Other                              | %GCE, %GSE, %RCE, and %RSE support QUIET entrypoint. %RCE and    |
|                                                                                                       |                                    | %RSE support QCALL entrypoint                                    |
+-------------------------------------------------------------------------------------------------------+------------------------------------+------------------------------------------------------------------+
| `GTM-8791 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-8791>`_ | Other                              | Prevent certain control character inputs to LKE from causing a   |
|                                                                                                       |                                    | SIG-11                                                           |
+-------------------------------------------------------------------------------------------------------+------------------------------------+------------------------------------------------------------------+
| `GTM-8859 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-8859>`_ | Admin                              | Correct turn-around point calculation for MUPIP JOUNAL -ROLLBACK |
|                                                                                                       |                                    | when dealing with "idle" regions                                 |
+-------------------------------------------------------------------------------------------------------+------------------------------------+------------------------------------------------------------------+
| `GTM-8860 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-8860>`_ | Admin                              | Prevent multiple slashes (/) from appearing in MUPIP JOURNAL     |
|                                                                                                       |                                    | -EXTRACT output                                                  |
+-------------------------------------------------------------------------------------------------------+------------------------------------+------------------------------------------------------------------+
| `GTM-8870 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-8870>`_ | Other                              | Clean up some rough edges                                        |
+-------------------------------------------------------------------------------------------------------+------------------------------------+------------------------------------------------------------------+
| `GTM-8874 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-8874>`_ | Language                           | VIEW "[NO]STATSHARE" accepts an optional region-list             |
+-------------------------------------------------------------------------------------------------------+------------------------------------+------------------------------------------------------------------+
| `GTM-8883 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-8883>`_ | DB                                 | Online Freeze/Journal switch cleanup                             |
+-------------------------------------------------------------------------------------------------------+------------------------------------+------------------------------------------------------------------+
| `GTM-8891 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-8891>`_ | Language                           | Prevent process failure from a certain pattern of $SELECT()      |
|                                                                                                       |                                    | errors                                                           |
+-------------------------------------------------------------------------------------------------------+------------------------------------+------------------------------------------------------------------+
| `GTM-8894 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-8894>`_ | Language                           | $ZRELDATE ISV provides the UTC data and time of the YottaDB build|
+-------------------------------------------------------------------------------------------------------+------------------------------------+------------------------------------------------------------------+
| `GTM-8895 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-8895>`_ | Other                              | %PEEKBYNAME protects the str variable from inappropriate         |
|                                                                                                       |                                    | modification                                                     |
+-------------------------------------------------------------------------------------------------------+------------------------------------+------------------------------------------------------------------+
| `GTM-8899 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-8899>`_ | Other                              | Work-around for bugs in the GnuPG agent                          |
+-------------------------------------------------------------------------------------------------------+------------------------------------+------------------------------------------------------------------+
| `GTM-8900 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-8900>`_ | Admin                              | MUPIP SET -NOENCRYPTABLE works without valid encryption setup    |
+-------------------------------------------------------------------------------------------------------+------------------------------------+------------------------------------------------------------------+
| `GTM-8903 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-8903>`_ | Language                           | Prevent process failure from a certain pattern of $SELECT() usage|
+-------------------------------------------------------------------------------------------------------+------------------------------------+------------------------------------------------------------------+
| `GTM-8906 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-8906>`_ | Admin                              | MUPIP JOURNAL -ROLLBACK and -RECOVER handle a larger number of   |
|                                                                                                       |                                    | records                                                          |
+-------------------------------------------------------------------------------------------------------+------------------------------------+------------------------------------------------------------------+
| `GTM-8909 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-8909>`_ | Other                              | Online help does not report an inappropriate error when exiting  |
|                                                                                                       |                                    | after the user typed <CTRL-C>                                    |
+-------------------------------------------------------------------------------------------------------+------------------------------------+------------------------------------------------------------------+
| `GTM-8914 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-8914>`_ | Language                           | $VIEW("GVSTATS"), ZSHOW "G" and ZSHOW "T" flag results that are  |
|                                                                                                       |                                    | probably not current                                             |
+-------------------------------------------------------------------------------------------------------+------------------------------------+------------------------------------------------------------------+
| `GTM-8919 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-8919>`_ | DB                                 | MUPIP REORG -ENCRYPT does not induce CRYPTOPFAILED errors in     |
|                                                                                                       |                                    | concurrent processes                                             |
+-------------------------------------------------------------------------------------------------------+------------------------------------+------------------------------------------------------------------+
| `GTM-8922 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-8922>`_ | Language                           | VIEW region subarguments can be a list of regions                |
+-------------------------------------------------------------------------------------------------------+------------------------------------+------------------------------------------------------------------+
| `GTM-8923 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-8923>`_ | Language                           | UTF-16 READ * and WRITE * Fixes                                  |
+-------------------------------------------------------------------------------------------------------+------------------------------------+------------------------------------------------------------------+
| `GTM-8924 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-8924>`_ | Language                           | Fix for unusual conditions with a $PRINCIPAL SOCKET device for a |
|                                                                                                       |                                    | JOB'd process                                                    |
+-------------------------------------------------------------------------------------------------------+------------------------------------+------------------------------------------------------------------+
| `GTM-8926 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-8926>`_ | Other                              | Flush Timer Deferred During External Call                        |
+-------------------------------------------------------------------------------------------------------+------------------------------------+------------------------------------------------------------------+
| `GTM-8927 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-8927>`_ | DB                                 | Prevent inappropriate JNLCTRL errors                             |
+-------------------------------------------------------------------------------------------------------+------------------------------------+------------------------------------------------------------------+

+++++++++++++
Database
+++++++++++++

* YottaDB limits the key size of a trigger to 1019 bytes. Previously, it limited the key size to the configured limit of regions that used the trigger, which caused a need to increase the maximum key limit and/or change the name of the trigger. This change also allows YottaDB to store triggers more compactly in cases where regions have a small record size limit. (`GTM-7915 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-7915>`_)

* YottaDB handles journal switch errors encountered during a MUPIP FREEZE -ONLINE -ON correctly. Previously it could leave the specified region with a very large Epoch Interval, which could cause a replication source server to issue SRVLCKWT2LNG errors. In addition, YottaDB only sends a PREVJNLLINKCUT message to the system log when the links have been cut. Previously the message could be reported in rare cases where a journal switch was deferred, then later switched without cutting links. Also, the source server reports the SRVLCKWT2LNG message correctly. Previously the values for PID and the wait time were reversed and the message reported minutes instead of seconds as the unit of time. (`GTM-8883 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-8883>`_)

* YottaDB processes work correctly when a concurrent MUPIP REORG -ENCRYPT changes the encryption key. Previously YottaDB processes could fail with CRYPTOPFAILED errors unaccompanied by other errors explaining the reason for the failure. This issue was only observed in the development environment, and was never reported by a user. MUPIP RESTORE issues an encryption setup error when attempting to restore an encrypted backup without the correct encryption setup. Previously MUPIP RESTORE issued a CRYPTOPFAILED when the restore instance was not configured for encryption. (`GTM-8919 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-8919>`_)

* YottaDB avoids issuing JNLCNTRL errors inappropriately. This issue was only observed in the development environment, and was never reported by a user. (`GTM-8927 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-8927>`_)

++++++++++++++++
Language
++++++++++++++++

* The listen queue depth specified with WRITE/LISTEN for a listening socket is limited only by the OS; previously, YottaDB enforced an artificial limit of 5. A SOCKET device retries connection attempts on local sockets that fail due to possible transient issues on the other end of the attempted connection, (for example an insufficiently large listen queue). Previously such attempts could sometimes appear to succeed, when they actually did not, leading to subsequent errors from READ and WRITE on the socket. (`GTM-8643 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-8643>`_)

* $VIEW("STATSHARE",\<region>) returns TRUE (1) if the process is currently sharing database statistics for the region and FALSE (0) if it is not. For the process to be sharing the database must be enabled for sharing and the process must have opted in to share. $VIEW("STATSHARE") with no region argument indicates the process has enabled sharing. (`GTM-8699 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-8699>`_)

* VIEW "[NO]STATSHARE"[:\<region-list>] enables or disables database statistics sharing for listed regions which permit such sharing. Without the region-list, the command acts on all regions enabled for sharing. When a targeted region has sharing disabled, STATSHARE has no effect. Note: a VIEW "[NO]STATSHARE" with no region sub-argument opens any unopened mapped regions and any enabled associated statsDB regions; the $gtm\_statshare environment variable applies to databases as the application first uses them. When the last VIEW "[NO]STATSHARE" had no region sub-argument, regions implicitly share when the process first references them, but after a VIEW specifies selective sharing, regions don't implicitly share as they open. Previously the VIEW command only supported enabling or disabling sharing for all enabled regions. (`GTM-8874 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-8874>`_)

* A \<side-effect-expression><\pure-Boolean-operator\>$SELECT(0:side-effect-expression)) sequence produces a SELECTFALSE run-time error; a regression introduced with the literal optimizations in V6.3-002 caused this combination to produce a SIG-11 (segmentation violation) at compilation. (`GTM-8891 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-8891>`_)

* The $ZRELDATE Intrinsic Special Variable provides the UTC date/time of the YottaDB build in the form YYYYMMDD 24:60 (using $ZDATE() notation). While $ZVERSION and $ZYRELEASE are probably better identifiers for most uses, $ZRELDATE may be a helpful alternative for those testing pre-release builds. (`GTM-8894 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-8894>`_)

* $SELECT() deals with cases where the first value is evaluated to a literal TRUE (1) and later arguments to the function contain one or more global references. Due to a regression in V6.3-002 associated with compiler optimizations caused this combination to produce a SIG-11 (segmentation violation) at compilation. (`GTM-8903 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-8903>`_)

* $VIEW("GVSTATS",<region>), ZSHOW "G" and ZSHOW "T" return a question-mark (?) at the end of their output strings when the process does not have access to the current shared statistics; they did not do this previously. Please adjust your scripting as needed to allow for this format enhancement. (`GTM-8914 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-8914>`_)

* VIEW commands accepting a region sub-argument also accept a comma (,) delimited string listing of regions. As part of deadlock prevention, YottaDB sorts the regions in an internal order, eliminating any duplicates from the list. Note: a VIEW with no region sub-argument opens any unopened mapped regions in the current global directory, while one with a list only opens the listed regions. If the VIEW argument has a corresponding environment variable to set the default state, the state applies to databases as the application implicitly opens them with references. Previously such commands accepted either one argument or an asterisk ("\*") for all regions, but if supplied with a region string tended to do more than the specified region. (`GTM-8922 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-8922>`_)

* READ * and WRITE * operate correctly on files and sockets with a CHSET of UTF-16, UTF-16BE, or UTF-16LE. Previously READ * returned an incorrect codepoint and WRITE * produced an incorrect character or a BADCHAR error. (`GTM-8923 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-8923>`_)

* YottaDB handles socket errors on JOB process startup correctly. Previously if the INPUT and/or OUTPUT for a JOB were in a bad state (e.g. LISTEN) the JOB process could terminate with a KILLBYSIGSINFO1 (signal 11) and a core dump or hang indefinitely. (`GTM-8924 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-8924>`_)

++++++++++++++++++++++
System Administration
++++++++++++++++++++++

* The Update Process logs record types with their corresponding type description; previously it only logged the integer type value. Note that it is possible, the change could disrupt code that parses the modified results. (`GTM-5730 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-5730>`_)

* MUPIP SET -JOURNAL in a replicated environment respects existing region sequence numbers, which aligns with the behavior of MUPIP BACKUP, which can also create new journal files. Previously, MUPIP SET -JOURNAL updated the region sequence numbers to the maximum of the sequence numbers for all regions specified for the command. (`GTM-6747 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-6747>`_)

* MUPIP INTEG produces a MUNOACTION error when the command specifies non-existent database; previously it produced an INTEGERRS message in this situation. (`GTM-7872 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-7872>`_)

* MUPIP JOURNAL -EXTRACT recognizes the -SEQNO=\<sequence\_number\_list> qualifier. \<sequence\_number\_list> is a comma separated list of sequence number(s) in decimal form. It specifies a list of sequence numbers to include or exclude in the journal extract. When a sequence number has a (~) prefix, -SEQNO excludes it from the journal extract. For replicated regions, EXTRACT -SEQNO uses replication sequence numbers, which may select records from multiple regions. For unreplicated regions, EXTRACT uses database transaction numbers, but specifying sequence number selection with more than one regions produces a JNLEXTRCTSEQNO error. When the sequence number list contains a sequence number involved in a TP transaction, EXTRACT reports it in a broken transaction file when the result does not contain all regions, which is commonly the case without replication, and may be the case with replication when not all regions are available to the utility. Previously EXTRACT did not support record selection by sequence number. (`GTM-8202 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-8202>`_)

* The Source Server directs errors to the Source Server log file. Be sure to check the Source Server log; previously it directed errors to stderr, which meant that if it was started from a terminal and the terminal session closed, a subsequent error logged an inappropriate NOPRINCIO error and terminated the Server. (`GTM-8576 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-8576>`_)

* MUPIP JOURNAL -ROLLBACK ignores Idle regions (those that received their last updates a long time previously). GT.M V6.3-000 introduced a regression whereby idle regions were not appropriately excluded from determining the ROLLBACK turn-around point. The result was that ROLLBACK could adjust time so far back that the journal files are not present (say due to periodic removal to conserve space) which terminated the ROLLBACK. The operational work-around for this issue was to cut new journal files when deleting old journal files. (`GTM-8859 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-8859>`_)

* YottaDB appropriately removes extra slashes (/) from journal files and output files provided to the MUPIP JOURNAL -EXTRACT command. Previously, when extracting journal files, YottaDB kept any extra slashes in the journal file name or output file name, causing multi-slash paths to appear in the output of the extract. The workaround was to avoid using journal files and output files with successive slashes in their paths. (`GTM-8860 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-8860>`_)

* MUPIP SET -NOENCRYPTABLE can set the database encryptable flag to FALSE. Previously, after invoking MUPIP SET -ENCRYPTABLE attempting to undo that action failed unless the GNUPGHOME pointed to a valid directory and gtm\_passwd was defined in the environment. This issue was only observed in the development environment, and was never reported by a user. MUPIP SET -ENCRYPTABLE now performs some basic encryption setup checks. To execute this command, GNUPGHOME must point to a valid directory and gtm\_passwd must be defined in the environment. Previously, the command did not require these environment variables. (`GTM-8900 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-8900>`_)

* MUPIP JOURNAL -ROLLBACK and -RECOVER handle large amounts of journal data; while there are still limits, previously when they attempted to handle around 55 million updates or more they inappropriately failed with a GTM-F-MEMORY error. (`GTM-8906 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-8906>`_)


+++++++++++++
Other
+++++++++++++

* YottaDB supports setting the M stack size using the gtm\_mstack\_size environment variable. This specifies the size of the M stack in KiB. No setting or a setting of 0 uses the default (272KiB). The minimum supported size is 25 KiB; YottaDB reverts values smaller than this to 25 KiB. The maximum supported size is 10000 KiB; YottaDB reverts values larger than this to 10000 KiB. (`GTM-1042 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-1042>`_)

* The MUPIP BACKUP command ignores aliases when executing system commands (cp, mv etc.) under the covers. Previously, aliases for cp, mv etc. (in one's .bashrc/.cshrc etc.) could cause the backup to use those alias versions of the cp/mv commands which might not work as desired. (`GTM-3146 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-3146>`_)

* MUPIP INTEG issues a DBKEYMX error in case of a long key name stored in the DT (Directory Tree); previously, it issued a INVSPECREC with no context. This issue was only observed in the development environment, and was never reported by a user. (`GTM-7483 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-7483>`_)

* When invoked at their QUIET or QCALL entrypoints, the %GCE, %GSE, %RCE, and %RSE utility routines report only globals or routines in which they find a match, for example: do QUIET^%GCE or do QCALL^%RSE. The QCALL entryref only exists in %RCE and %RSE. Previously these routines always reported every item they processed regardless of whether it contained a match. (`GTM-8777 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-8777>`_)

* LKE handles certain control characters appropriately; previously, for example, <CTRL-Z> caused a segmentation violation (SIG-11). (`GTM-8791 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-8791>`_)

* YottaDB addresses some issues identified by static analysis tools which appear to be obscure, and, in most cases, harmless. Previously an error with a source file could leak a file descriptor, and, in odd circumstances, trigger management could fail with a segmentation violation. These issues were only observed in the development environment, and were never reported by a user. (`GTM-8870 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-8870>`_)

* %PEEKBYNAME protects the variable str against inappropriate modification in the user symbol space; previously an error in %PEEKBYNAME, possibly caused by user input, would change the value of this variable. (`GTM-8895 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-8895>`_)

* The YottaDB reference encryption plugin library retries a failing decryption request once to mask a bug in GnuPG that caused spurious CRYPTKEYFETCHFAILED errors during process startup or re-encryption. GnuPG version 2.2.4 fixes the underlying bug, so upgrading to GnuPG 2.2.4 and libgcrypt 1.8.2 fixes the underlying issue. These problems were only seen in development and not reported by a customer. (`GTM-8899 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-8899>`_)

* The help facility for the utility programs MUPIP, DSE and LKE ignores control-C; previously if a user pressed \<CTRL-C> while using help, the facility could exit with a harmless, but inappropriate, ENO256 error. (`GTM-8909 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-8909>`_)

* MUMPS processes which attempt to process a flush timer while executing an external call defer the timer processing until after the external call is complete. Previously, flush timers could interfere with non-reentrant routines used in the external call with undesired results. In particular, memory allocation operations in the external call can interfere with ASYNCIO setup in the flush timer, resulting in process hangs. (`GTM-8926 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-004_Release_Notes.html#GTM-8926>`_)

--------------------------
More Information
--------------------------

(Section Blank for this Release)


-----------------------
Messages
-----------------------

+++++++++++++++
From GT.M
+++++++++++++++

**CRYPTJNLMISMATCH**, Encryption settings mismatch between journal file jjjj and corresponding database file dddd

*This is an existing message with updated text.*

All Components Error: Encryption settings in the header of database file dddd do not match those stored in the header of journal file jjjj. This prevents access to the database. The most likely cause is inappropriate operator action such as replacing the current journal file with an older journal file.

Action: Correct the error that caused the incorrect journal file to be pointed to by the database file. If the correct journal file has been inadvertently deleted, create new journal files with the -noprevjnl switch. Take a backup as soon as possible thereafter. Depending on your situation, you may need to refresh secondary instances.

**ENCRYPTCONFLT2**, Message: A concurrent MUPIP REORG -ENCRYPT changed the encryption key for RRRR before the process could initialize it

Run Time Warning: Due to a concurrent MUPIP REORG -ENCRYPT, a process was forced to defer encryption key initialization for region RRRR.

Action: None. This information message is only important when followed by other encryption errors.

**INVSEQNOQUAL**, Invalid SEQNO qualifier value xxxx

MUPIP Error: This error indicates that MUPIP JOURNAL -EXTRACT -SEQNO command could not extract a journal file because an invalid SEQNO format was specified.

Action: Enter a comma separated list of valid sequence numbers ('0' or positive integers) as value for the SEQNO qualifier. The format of the -SEQNO qualifier is -SEQNO=seqno1[,seqno2,seqno3.....] where seqno is the region sequence number in decimal format.

**JNLACCESS**, Error accessing journal file jjjj

*This is an existing message with updated text.*

Run Time Error: YottaDB sends this message to the system log followed by other messages detailing the failure. jjjj is the file-specification for the inaccessible journal. In most situations, this error occurs when the journal file storage runs out of disk space.

Action: Review the accompanying message(s) for additional information. This means an error while trying to write to the journal file.

**JNLBADRECFMT**, Journal Record Format Error encountered for file jjjj at disk address yyyy

*This is an existing message with updated text.*

MUPIP/Run Time Error: This indicates that an attempt to open a journal file encountered an invalid record.

Action: Report the entire incident context to your YottaDB support channel.

**JNLCYCLE**, Journal file jjjj causes cycle in the journal file generations of database file dddd

*This is an existing message with updated text.*

MUPIP Error: This indicates that MUPIP encountered journal file jjjj causing cycle in the journal file generations of database file dddd; that is following the back-pointers in the journal files can wind up repeatedly finding the same journal file.

Action: Contact your YottaDB support channel with appropriate log messages.

**JNLDBERR**, Journal file jjjj does not correspond to database dddd

*This is an existing message with updated text.*

Run Time Error: This indicates that YottaDB could not open journal file jjjj for database file dddd because the journal file header identifies itself as belonging to a different database file that does not exist in the system.

Action: Use a MUPIP SET command with the qualifier JOURNAL to create a journal file that matches the database.

**JNLDISABLE**, Specified journal option(s) cannot take effect as journaling is DISABLED on database file dddd

*This is an existing message with updated text.*

MUPIP Warning: This indicates that none of the specified journal option(s) in MUPIP SET -JOURNAL or MUPIP BACKUP command took effect, because journaling was found DISABLED on database file dddd.

Action: Revise the selection qualification to exclude the DISABLED region(s) or, if appropriate, enable journaling on those regions.

**JNLEXTEND**, Journal file extension error for file jjjj.

*This is an existing message with updated text.*

Run Time/MUPIP Error: Journal file jjjj failed to extend. If the environment is not configured for instance freeze, this causes journaling to be turned off for the region.

Action: Review the accompanying message(s) and take appropriate action. If the environment is not configured for instance freeze, perform a MUPIP BACKUP, that turns journaling on again, to reestablish durability.

**JNLEXTR**, Error writing journal extract file: xxxx

*This is an existing message with updated text.*

MUPIP Error: This indicates that an error was encountered while trying to write to either the JNL EXTRACT file or lost-transaction file or broken-transaction file as part of a MUPIP JOURNAL command.

Action: Review the accompanying message(s) for additional information.

**JNLEXTRCTSEQNO**, Journal Extracts based on sequence numbers are restricted to a single region when replication is OFF

MUPIP Error: When replication is enabled YottaDB applies a uniform set of sequence numbers across regions, but when it is not in use each region has its own set of sequence numbers, and, in that case, MUPIP only works on a region at a time.

Action: If you need cross region sequence numbers, start replication with at least a passive Source Server; otherwise use one MUPIP JOURNAL -EXTRACT command for each region when using the -SEQNO qualifier.

**JNLFILOPN**, Error opening journal file jjjj for database file dddd

*This is an existing message with updated text.*

Run Time/MUPIP Error: This indicates that YottaDB was unable to open journal file jjjj for the specified database file dddd. The Source Server exits with a JNLFILOPN message after six failed attempts to open journal files.

Action: Check the authorizations for the user of the process and the health of the file system holding the journal file.

**JNLFLUSHNOPROG**, No progress while attempting to flush journal file jjjj

*This is an existing message with updated text.*

Run Time Warning: Indicates processes needing space in the journal buffers were unable to write journal jjjj because even though multiple processes have controlled the resource, this process has not been able to flush records. JNLPROCSTUCK means one process is hogging, while this message means more than one process has tried but none have succeeded. Might indicate a clogged disk subsystem on which journal file JJJJ resides.

Action: Check the log file for other journaling related messages. Consider balancing disk subsystem load.

**JNLFSYNCLSTCK**, Journaling fsync lock is stuck in journal file jjjj

*This is an existing message with updated text.*

Run Time Error: A resource controlling journal file actions has remained unavailable for a long period.

Action: Check on the condition of the process identified in the associated messages.

**JNLINVALID**, jjjj is not a valid journal file Region: rrrr

*This is an existing message with updated text.*

MUPIP Error: This indicates that YottaDB could not open journal file jjjj, due to an error that is detailed in the accompanying previous message(s). While trying to create a new journal file for the same region it encountered errors. rrrr is the region name associated with the journal.

Action: Review the accompanying error message(s) to determine the cause of the failure of the new journal file creation. After the cause is resolved, to reestablish durability, perform a MUPIP BACKUP that turns journaling back on.

**JNLNAMLEN**, Journal file jjjj: for database file dddd exceeds maximum of MMMM

*This is an existing message with updated text.*

MUPIP Error: This indicates that the file-specification jjjj of the journal for database file dddd exceeds the maximum supported length of MMMM.

Action: Modify the journal file-specification to adhere to the file length restrictions.

**JNLNOCREATE**, Journal file jjjj not created

*This is an existing message with updated text.*

MUPIP/Run Time Error: This indicates that YottaDB could not create journal file jjjj.

Action: Review the accompanying message(s) for additional information.

**JNLORDBFLU**, Error flushing database blocks to dddd. See related messages in the operator log.

*This is an existing message with updated text.*

MUPIP Error: This message indicates that hardening journal or database records could not be completed due to an error. The operator log should contain one or more accompanying messages indicating the cause of the error.

Action: Verify the normal state of the file system and appropriate permissions of the database and journal files. Report the entire incident context to your YottaDB support channel along with any operator log messages within the same time frame.

**JNLREADEOF**, End of journal file encountered for jjjj

*This is an existing message with updated text.*

MUPIP/Run Time Error: This indicates that MUPIP JOURNAL or a run-time journal operation encountered the end-of-file for the journal file jjjj, before it completed processing.

Action: This error indicates an improperly closed journal file. Restart journaling with a MUPIP BACKUP -NEWJNLFILES or a MUPIP SET -JOURNAL and report all available circumstance to those responsible for supporting your database operations.

**JNLRECFMT**, Journal file record format error encountered

*This is an existing message with updated text.*

MUPIP Error: This indicates that MUPIP JOURNAL encountered an invalid record in the journal file.

Action: In the event of YottaDB issuing this error message, use MUPIP BACKUP to ensure durability by creating a fresh set of journals consistent with the database. Else, to resume operation, restore the database from the last backup and play forward the updates using the appropriate MUPIP JOURNAL command. As soon as possible, report the entire incident context with information from the operator log and any other relevant information to your YottaDB support channel.

**JNLSPACELOW**, Journal file jjjj nearing maximum size, nnnn blocks to go

*This is an existing message with updated text.*

Run Time Information: This indicates that the journal file jjjj is approaching the maximum size specified for it. The system creates a new journal file when the limit is reached.

Action: None required except as part of monitoring journaling space requirements or when operational practice uses this as a trigger to intervene in journal file management.

**JNLSWITCHSZCHG**, Journal AUTOSWITCHLIMIT [aaaa blocks] is rounded down to [bbbb blocks] to equal the sum of journal ALLOCATION

*This is an existing message with updated text.*

MUPIP Information: This indicates that the specified AUTOSWITCHLIMIT value was rounded down as little as possible to make it aligned to the ALLOCATION + a multiple of EXTENSION. Any subsequently created journal file will use this value for AUTOSWITCHLIMIT.

Action: If the rounded value is inappropriate examine the alignsize, allocation and extension values and choose a more suitable value.

**JNLSWITCHTOOSM**, Journal AUTOSWITCHLIMIT [aaaa blocks] is less than journal ALLOCATION [bbbb blocks] for database file dddd

*This is an existing message with updated text.*

MUPIP Error: This indicates that the value of AUTOSWITCHLIMIT specified in a MUPIP SET JOURNAL command is less than the default or specified value of ALLOCATION. This error also indicates that the AUTOSWITCHLIMIT value specified was greater or equal to the ALLOCATION but in turn got rounded down, and this rounded down value is lesser than the ALLOCATION.

Action: Specify a higher value of AUTOSWITCHLIMIT.

**JNLWRERR**, Error writing journal file jjjj. Unable to update header Region: yyyy

*This is an existing message with updated text.*

Run Time/MUPIP Error: This indicates that YottaDB encountered an error while updating the journal file header as part of trying to open the journal file.

Action: Review the accompanying message(s) for detail on the cause of the error. YottaDB automatically closes the current journal file and creates a new one. To reestablish durability, perform MUPIP BACKUP to create a fresh set of journals consistent with the database.

**MSTACKSZNA**, User-specified M stack size of SSSS KiB not appropriate; must be between LLLL KiB and MMMM KiB; reverting to VVVV KiB

Run Time Information: The gtm\_mstack environment variable species an M stack size outside the range GT.M supports, where LLLL and MMMM are the lower and upper bounds respectively; VVVV is the value actually used.

Action: None required immediately as the process operates with the reported size M stack, however it would be preferable to eliminate such messages by setting gtm\_mstack to a value in the supported range.

**PREMATEOF**, Premature end of file detected

*This is an existing message with updated text.*

MUPIP/Run Time Error: A file read or write detected an end-of-file when it was expecting additional records.

Action: Analyze accompanying messages for the type of file on which the operation failed. If the operation was a MUPIP LOAD, refer to the `About this Manual section on MUPIP LOAD errors <https://docs.yottadb.com/MessageRecovery/about.html#mupip-load-errors>`_. If the circumstances warrant, contact the group responsible for database integrity at your operation with all the diagnostic context you can gather.


**SYSUTILCONF**, Error determining the path for system utility. tttt

Run Time Error: tttt represents text describing details of an issue finding a POSIX function that it needed.

Action: Check for aliases or environment variables related to paths that might be interfering with YottaDB's ability to invoke functions.

------------------
Tarball Hashes
------------------

+--------------------------------------------------------------------+----------------------------------------------------+
| sha256sum                                                          | File                                               |
+====================================================================+====================================================+
| dab80777a1d4d5be52f7c5d0749d8701f383aa8601eb9037884b14512d2b854d   |  yottadb_r122_linux_armv6l_pro.tgz                 |
+--------------------------------------------------------------------+----------------------------------------------------+
| 62dce638fee3fbac38de2c0f495730ab52abd05bf622d6ab7c89c98718dd5185   |  yottadb_r122_linux_armv7l_pro.tgz                 |
+--------------------------------------------------------------------+----------------------------------------------------+
| 7c187371924429e30d3254cd17fc1b9d9fb3ac3792254880bec250be728ef056   |  yottadb_r122_linux_x8664_pro.tgz                  |
+--------------------------------------------------------------------+----------------------------------------------------+
| ac4cc25b1d1de4e3571aa8a5b448a584931095f9e7f1a9208c034ecc42d6f7f5   |  yottadb_r122_rhel7_x8664_pro.tgz                  |
+--------------------------------------------------------------------+----------------------------------------------------+
| 44678b658fe6d1396b02250c837826c97aaf2706d9963a70aac6d0ea78085c9b   |  yottadb_r122_src.tgz                              |
+--------------------------------------------------------------------+----------------------------------------------------+

-----------------
Legal Stuff
-----------------

Copyright © 2018 YottaDB LLC

Permission is granted to copy, distribute and/or modify this document under the terms of the `GNU Free Documentation License, Version 1.3 <http://www.gnu.org/licenses/fdl.txt>`_ or any later version published by the Free Software Foundation; with no Invariant Sections, no Front-Cover Texts and no Back-Cover Texts.

YottaDB™ is a trademark of YottaDB LLC.
GT.M™ is a trademark of Fidelity National Information Services, Inc.
Other trademarks belong to their respective owners.

This document contains a description of YottaDB and the operating instructions pertaining to the various functions that comprise the software. This document does not contain any commitment of YottaDB LLC. YottaDB LLC believes the information in this publication is accurate as of its publication date; such information is subject to change without notice. YottaDB LLC is not responsible for any errors or defects.


