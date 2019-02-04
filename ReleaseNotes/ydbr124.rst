=================
YottaDB r1.24
=================

.. contents::
   :depth: 2

------------------------------
Release Note Revision History
------------------------------

+-------------------------------+---------------------------------------+----------------------------------------------------------------------+
| Revision                      | Date                                  | Summary                                                              |
+===============================+=======================================+======================================================================+
| 1.00                          | January 31, 2019                      | r1.24 Initial Release                                                |
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
r1.24
----------------------

+++++++++++++
Overview
+++++++++++++

r1.24 is a major release that brings much new functionality to YottaDB.

`pkg-config <https://www.freedesktop.org/wiki/Software/pkg-config/>`_ is a standard tool on Linux systems for managing package versions, for example, to query the location and latest installed version of a package. YottaDB is now part of this ecosystem: the `ydbinstall script <https://gitlab.com/YottaDB/DB/YDB/blob/master/sr_unix/ydbinstall.sh>`_ creates a pkg-config file for YottaDB at :code:`/usr/share/pkgconfig/yottadb.pc`. In turn, this means that an application can write a script to use YottaDB without the script needing to be updated when a new YottaDB release is installed on a system, or if a YottaDB release is installed in different directories on different systems, e.g.

.. parsed-literal::
   source $(pkg-config --variable=prefix yottadb)/ydb_env_set # setup environment
   gcc -I $ydb_dist -L $ydb_dist -o myprog myprog.c -lyottadb
   ./myprog


or

.. parsed-literal::
   source $(pkg-config --variable=prefix yottadb)/ydb_env_set # setup environment
   mumps -run mypro

Fitting into the standard ecosystem becomes increasingly important as we release YottaDB wrappers providing standard APIs for a variety of languages starting with `Go <https://golang.org>`_.

Released as field test grade functionality are `Simple API functions that support multi-threaded applications <https://gitlab.com/YottaDB/DB/YDB/issues/351>`_. Although the YottaDB engine is itself single-threaded, it supports multi-threaded applications as described in our blog post `YottaDB Support for Multi-threaded Applications <https://yottadb.com/yottadb-support-for-multi-threaded-applications/>`_ and in the `Simple API section of the Multi-Language Programmers Guide <https://docs.yottadb.com/MultiLangProgGuide/MultiLangProgGuide.html#simple-api>`_. An API with support for threads allows YottaDB to create wrappers for inherently multi-threaded language implementations such as `Go <https://golang.org>`_, as described in the `Programming in Go section of the Multi-Language Programmers Guide <https://docs.yottadb.com/MultiLangProgGuide/MultiLangProgGuide.html#programming-in-go>`_. Note that r1.24 is otherwise considered a production release. This means that we consider YottaDB r1.24 to be suitable for use in production except for functionality to support multi-thread applications which are suitable for use in development and testing, but not in production. In the next release, if not sooner, we intend for that functionality to be suitable for production use, like the rest of YottaDB.

64-bit ARM (aarch64) is a supported CPU architecture, running Ubuntu 18.04 LTS, tested on Raspberry Pi 3. If you need another distribution of Linux to be Supported, or testing on another hardware implementation of the architecture, please `contact us <https://yottadb.com/about-us/contact/>`_. YottaDB thanks community member `Steve Johnson <https://gitlab.com/sljohnson1>`_ for contributing the port.

r1.24 comes with numerous & noteworthy functional & performance enhancements, fixes (including `fixes to issues in the upstream GT.M code base <https://gitlab.com/YottaDB/DB/YDB/issues?scope=all&utf8=%E2%9C%93&state=closed&milestone_title=r124&label_name[]=upstream%20issue>`_), as described below with `full details on Gitlab <https://gitlab.com/YottaDB/DB/YDB/issues?milestone_title=r124&state=closed>`_.

YottadB r1.24 is upward compatible with both `YottaDB r1.22 <https://github.com/YottaDB/YottaDB/releases/tag/r1.22>`_ and `GT.M V6.3-005 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html>`_.

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

Recent releases of major GNU/Linux distributions with contemporary kernels, glibc and ncurses are Supportable. Specific notes:

- On Ubuntu releases after 18.04 LTS, YottaDB needs the libtinfo5 package to be installed.
- On `Arch Linux <https://www.archlinux.org/>`_ and possibly other leading edge distributions, YottaDB may need to be recompiled from source code owing to library and tool chain versions significantly more recent than those used in building the distribution.


+++++++++++++++
Getting Started
+++++++++++++++

See our `Get Started <https://yottadb.com/product/get-started/>`_ page to use YottaDB.

We **Strongly recommend** that you install YottaDB r1.24 in a newly created directory, different from those of YottaDB r1.22 and any GT.M versions you may have installed on the system.

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

As YottaDB r1.24 is upward compatible from both YottaDB r1.22 and GT.M V6.3-004, the minimal upgrade steps are:

* Install YottaDB r1.24.
* Recompile any object code, and recreate shared libraries where appropriate.
* If you are using encryption, compile and install the reference implementation plugin or your customized plugin.
* Cleanly shut down the application and ensure that the database files are shut down using mupip rundown from the prior release.
* Switch journal files with the new YottaDB release.
* Start using the new YottaDB release.

To upgrade from older GT.M releases, first upgrade to GT.M V6.0-000 or later and follow the steps above, or contact your YottaDB support channel for assistance and guidance.

A more sophisticated upgrade technique is:

* Install YottaDB r1.24.
* Create a new replicated instance of your application (on the same system or a different system).
* Assuming the existing instance is A, and the new instance is B, start replicating from A to B.
* Once B catches up, switchover so that B is in a primary role replicating to A.
* Once you are satisfied with B, remove (or upgrade) A.

------------------------
Change History
------------------------

+++++++++++
r1.24
+++++++++++

YottaDB r1.24 includes the following changes from `r1.22 <https://github.com/YottaDB/YottaDB/releases/tag/r1.22>`_.

+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| ID                                                     | Category           | Summary                                                                                   |
+========================================================+====================+===========================================================================================+
| `#95 <https://gitlab.com/YottaDB/DB/YDB/issues/95>`_   | DB                 | MUPIP LOAD reports 0 records loaded on an empty ZWR or GO format extract file             |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#96 <https://gitlab.com/YottaDB/DB/YDB/issues/96>`_   | DB                 | AIMG records include DSE CHANGE command effecting the change                              |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#113 <https://gitlab.com/YottaDB/DB/YDB/issues/113>`_ | Admin              | ydb_lct_stdnull environment variable                                                      |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#114 <https://gitlab.com/YottaDB/DB/YDB/issues/114>`_ | Admin              | GDE defaults to standard null collation                                                   |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#183 <https://gitlab.com/YottaDB/DB/YDB/issues/183>`_ | Language           | ydb_incr_s() accepts NULL ret_value parameter                                             |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#191 <https://gitlab.com/YottaDB/DB/YDB/issues/191>`_ | DB                 | Force access through GNP server even when client and server are on the same host          |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#230 <https://gitlab.com/YottaDB/DB/YDB/issues/230>`_ | Language           | $ZSEARCH() context of -1 is always new                                                    |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#253 <https://gitlab.com/YottaDB/DB/YDB/issues/253>`_ | Language           | Attempting to update a database mapped through multiple global directories and for which  |
|                                                        |                    | a process does not have write access issues a DBPRIVERR                                   |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#255 <https://gitlab.com/YottaDB/DB/YDB/issues/255>`_ | Admin              | ydbinstall script checks for Supported or known Supportable platform and issues a warning |
|                                                        |                    | otherwise                                                                                 |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#264 <https://gitlab.com/YottaDB/DB/YDB/issues/264>`_ | Other              | Source Server issues FILTERNOTALIVE when external filter program terminates abnormally    |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#267 <https://gitlab.com/YottaDB/DB/YDB/issues/267>`_ | Language           | MUPIP BACKUP issues a FILENAMETOOLONG error when the absolute path length of the target   |
|                                                        |                    | backup filename is too long                                                               |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#272 <https://gitlab.com/YottaDB/DB/YDB/issues/272>`_ | Admin              | MUMPS, DSE, LKE, MUPIP BACKUP etc. honor the `ydb_poollimit` environment variable         |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#275 <https://gitlab.com/YottaDB/DB/YDB/issues/275>`_ | Other              | LISTENING sockets can be passed between processes                                         |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#277 <https://gitlab.com/YottaDB/DB/YDB/issues/277>`_ | Language           | $$set^%PATCODE works in UTF-8 mode                                                        |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#280 <https://gitlab.com/YottaDB/DB/YDB/issues/280>`_ | Other              | WRITE /WAIT on a SOCKET device with no sockets returns immediately                        |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#282 <https://gitlab.com/YottaDB/DB/YDB/issues/282>`_ | DB                 | Source Server clears as much backlog as possible when reading from journal files of       |
|                                                        |                    | instance frozen because of errors in journal file switching                               |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#284 <https://gitlab.com/YottaDB/DB/YDB/issues/284>`_ | DB                 | MUPIP JOURNAL -EXTRACT on concurrently updated journal file works                         |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#290 <https://gitlab.com/YottaDB/DB/YDB/issues/290>`_ | DB                 | During MUPIP FREEZE -ONLINE, exiting YottaDB processes detach from database files without |
|                                                        |                    | hanging                                                                                   |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#293 <https://gitlab.com/YottaDB/DB/YDB/issues/293>`_ | Admin              | SET $ZGBLDIR inside a Update Process trigger correctly switches global directory          |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#295 <https://gitlab.com/YottaDB/DB/YDB/issues/295>`_ | Language           | ZSTEP inside trigger ignored completely if TRIGGER_MOD restriction is in place            |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#297 <https://gitlab.com/YottaDB/DB/YDB/issues/297>`_ | DB                 | LOCK commands work correctly in a rare case of multiple hash collisions                   |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#309 <https://gitlab.com/YottaDB/DB/YDB/issues/309>`_ | Admin              | ydb_env_set leaves gtm_prompt untouched                                                   |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#310 <https://gitlab.com/YottaDB/DB/YDB/issues/310>`_ | Other              | DEVOPENFAIL messages in the syslog correctly identify the device name and system error    |
|                                                        |                    | number in case of errors during OPEN of a PIPE device                                     |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#312 <https://gitlab.com/YottaDB/DB/YDB/issues/312>`_ | DB                 | Fixes to issues affecting processes part of multiple replication instances                |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#313 <https://gitlab.com/YottaDB/DB/YDB/issues/313>`_ | DB                 | MUPIP FTOK -JNLPOOL and -RECVPOOL recognize instance-file-name on command line            |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#315 <https://gitlab.com/YottaDB/DB/YDB/issues/315>`_ | Language           | ZCOMPILE omits warnings during compilation if $ZCOMPILE includes "-nowarning"             |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#318 <https://gitlab.com/YottaDB/DB/YDB/issues/318>`_ | Other              | Changes to ZSYSTEM_FILTER/PIPE_FILTER in restrict.txt accepted even when within one second|
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#321 <https://gitlab.com/YottaDB/DB/YDB/issues/321>`_ | Language           | Journal records fed to replication filters include timestamps                             |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#324 <https://gitlab.com/YottaDB/DB/YDB/issues/324>`_ | Language           | Errors inside indirection usage while in the direct mode using $ETRAP return control to   |
|                                                        |                    | the direct mode prompt                                                                    |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#329 <https://gitlab.com/YottaDB/DB/YDB/issues/329>`_ | Language           | Compiling M program with literal optimization in UTF-8 mode works correctly               |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#333 <https://gitlab.com/YottaDB/DB/YDB/issues/333>`_ | Other              | $VIEW("PROBECRIT") CPT time resolution is nanoseconds                                     |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#338 <https://gitlab.com/YottaDB/DB/YDB/issues/338>`_ | Admin              | Improvements to the ydbinstall.sh script                                                  |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#341 <https://gitlab.com/YottaDB/DB/YDB/issues/341>`_ | DB                 | Journal updates separated in time by more than an epoch interval guaranteed to have an    |
|                                                        |                    | EPOCH journal record between them                                                         |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#344 <https://gitlab.com/YottaDB/DB/YDB/issues/344>`_ | Language           | Simple API calls subsequent to a ydb_zwr2str_s() call work correctly                      |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#345 <https://gitlab.com/YottaDB/DB/YDB/issues/345>`_ | Other              | OPEN of a PIPE device with the STDERR deviceparameter set to the name of an already-open  |
|                                                        |                    | device issues a STDERRALREADYOPEN error                                                   |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#346 <https://gitlab.com/YottaDB/DB/YDB/issues/346>`_ | Other              | Certain cases of invalid collation specification in directory tree records handled        |
|                                                        |                    | correctly                                                                                 |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#347 <https://gitlab.com/YottaDB/DB/YDB/issues/347>`_ | Admin              | ydbinstall.sh checks at startup for all utilities it needs                                |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#348 <https://gitlab.com/YottaDB/DB/YDB/issues/348>`_ | DB                 | OPEN of SOCKET device previously closed after TPTIMEOUT works correctly                   |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#349 <https://gitlab.com/YottaDB/DB/YDB/issues/349>`_ | DB                 | MUPIP REORG on a database file with non-zero RESERVED_BYTES retains structural            |
|                                                        |                    | integrity of file                                                                         |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#350 <https://gitlab.com/YottaDB/DB/YDB/issues/350>`_ | Admin              | Change terminal characteristics only during READ or Direct Mode                           |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#351 <https://gitlab.com/YottaDB/DB/YDB/issues/351>`_ | Language           | API for Multi-threaded applications to use the single-threaded YottaDB engine             |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#352 <https://gitlab.com/YottaDB/DB/YDB/issues/352>`_ | Other              | Unhandled error from C→M call-in ireturned to caller after Simple API call that sets a    |
|                                                        |                    | spanning node                                                                             |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#353 <https://gitlab.com/YottaDB/DB/YDB/issues/353>`_ | Language           | $INCREMENT() supported for a global that has NOISOLATION turned on                        |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#354 <https://gitlab.com/YottaDB/DB/YDB/issues/354>`_ | Language           | $ZSTATUS reports correct actual and maximum string length following ERR_INVSTRLEN return  |
|                                                        |                    | from Simple API call                                                                      |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#356 <https://gitlab.com/YottaDB/DB/YDB/issues/356>`_ | Language           | Extended global reference correctly reports NETDBOPNERR error when $zgbldir/$ydb_gbldir   |
|                                                        |                    | not set                                                                                   |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#357 <https://gitlab.com/YottaDB/DB/YDB/issues/357>`_ | Admin              | SIGINT/SIGQUIT (kill -2 or kill -3) handled when sent to a mumps process waiting for      |
|                                                        |                    | spawned process to complete                                                               |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#358 <https://gitlab.com/YottaDB/DB/YDB/issues/358>`_ | DB                 | Avoid DBIOERR error in forked child process when using simpleAPI in database file using   |
|                                                        |                    | ASYNCIO                                                                                   |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#360 <https://gitlab.com/YottaDB/DB/YDB/issues/360>`_ | Language           | $ZEDITOR reflects exit status of the last ZEDIT                                           |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#361 <https://gitlab.com/YottaDB/DB/YDB/issues/361>`_ | Other              | Avoid SI replication REPLINSTNOHIST after Receiver Server shutdown before any data is     |
|                                                        |                    | replicated                                                                                |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#362 <https://gitlab.com/YottaDB/DB/YDB/issues/362>`_ | DB                 | Ensure logical consistency between primary and secondary instances after a switchover     |
|                                                        |                    | following kill -9 of processes on secondary                                               |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#363 <https://gitlab.com/YottaDB/DB/YDB/issues/363>`_ | Language           | Issue NUMOFLOW for overlarge numeric expressions encountered during literal optimization  |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#364 <https://gitlab.com/YottaDB/DB/YDB/issues/364>`_ | DB                 | For a frozen instance Source Server shutdown reports whether or not it deleted the Journal|
|                                                        |                    | Pool ipcs                                                                                 |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#365 <https://gitlab.com/YottaDB/DB/YDB/issues/365>`_ | DB                 | Use prior journal type in MUPIP SET JOURNAL; default depending on access method           |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#369 <https://gitlab.com/YottaDB/DB/YDB/issues/369>`_ | Admin              | VIEW "SETENV" and VIEW "UNSETENV" commands to set and unset environment variables in M    |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#371 <https://gitlab.com/YottaDB/DB/YDB/issues/371>`_ | Language           | $SELECT() stops evaluating tvexprs or exprs after the first true tvexpr is found          |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#372 <https://gitlab.com/YottaDB/DB/YDB/issues/372>`_ | Language           | Report syntax error for SET command where first setleft is an invalid ISV and later       |
|                                                        |                    | setleft uses indirection                                                                  |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#374 <https://gitlab.com/YottaDB/DB/YDB/issues/374>`_ | Language           | Ensure $TEXT() strips line terminators for object code compiled with -EMBED_SOURCE flag   |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#375 <https://gitlab.com/YottaDB/DB/YDB/issues/375>`_ | Other              | Drop support for SSL 2.0, SSL 3.0 and TLS1.0 in the reference TLS implementation shipped  |
|                                                        |                    | with YottaDB                                                                              |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#376 <https://gitlab.com/YottaDB/DB/YDB/issues/376>`_ | Other              | Add support for TLS 1.3 in the reference TLS implementation that ships with the           |
|                                                        |                    | encryption plugin                                                                         |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#383 <https://gitlab.com/YottaDB/DB/YDB/issues/383>`_ | Language           | ydb_tp_s() returns negative GBLOFLOW error code                                           |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#385 <https://gitlab.com/YottaDB/DB/YDB/issues/385>`_ | Admin              | Recompile .m file if .o has same timestamp                                                |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#392 <https://gitlab.com/YottaDB/DB/YDB/issues/392>`_ | Admin              | Environment variables ydb_linktmpdir/gtm_linktmpdir default to ydb_tmp/gtm_tmp            |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#393 <https://gitlab.com/YottaDB/DB/YDB/issues/393>`_ | Admin              | YDB_RELEASE in libyottadb.h specifies YottaDB release                                     |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#394 <https://gitlab.com/YottaDB/DB/YDB/issues/394>`_ | Language           | ydb_subscript_next_s() for nonexistent local returns next subscript "0"                   |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#395 <https://gitlab.com/YottaDB/DB/YDB/issues/395>`_ | Admin              | /tmp/yottadb/$ydb_ver has read-write-execute permissions for all users permitted to run   |
|                                                        |                    | YottaDB                                                                                   |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#396 <https://gitlab.com/YottaDB/DB/YDB/issues/396>`_ | Language           | ydb_node_next_s() and ydb_node_previous_s() report PARMINVALID when output ydb_buffer_t   |
|                                                        |                    | structure has NULL buf_addr                                                               |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#397 <https://gitlab.com/YottaDB/DB/YDB/issues/397>`_ | Admin              | Access through SimpleAPI issues ZGBLDIRACC if ydb_app_ensures_isolation is set to a       |
|                                                        |                    | non-null value but global directory does not exist                                        |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#401 <https://gitlab.com/YottaDB/DB/YDB/issues/401>`_ | Language           | SIG-11 and/or non-deterministic behavior when a call-in invokes an external call that     |
|                                                        |                    | uses Simple API                                                                           |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+
| `#402 <https://gitlab.com/YottaDB/DB/YDB/issues/402>`_ | Language           | gtm_init(), gtm_ci() and gtm_cip() entry points needed in YottaDB for backward            |
|                                                        |                    | compatibility with GT.M                                                                   |
+--------------------------------------------------------+--------------------+-------------------------------------------------------------------------------------------+


+++++++++++++++++
Database
+++++++++++++++++

* MUPIP LOAD reports 0 records loaded on an empty ZWR or GO format extract file. Previously, it reported MAXSTRLEN and LDBINFMT errors. [`#95 <https://gitlab.com/YottaDB/DB/YDB/issues/95>`_]

* AIMG records (After-Images of database blocks) reported by MUPIP JOURNAL -EXTRACT -DETAIL include the DSE CHANGE command that caused the block state to change. Previously they did not. [`#96 <https://gitlab.com/YottaDB/DB/YDB/issues/96>`_]

* :code:`GDE CHANGE -SEGMENT -FILE` and :code:`GDE ADD -SEGMENT -FILE` accept a database file name specification of the form @: where  is the local host name or a remote host name where the GT.CM GNP server runs. Any database access to such a region/segment by a client process now goes through the GNP server even if  is the current host. The : syntax where a database access goes through a GNP server only if  is not the current host remains supported. The optional "@" prefix allows a client to force every database access to go through a GNP server, thereby allowing client processes running different releases of YottaDB to access a database file. [`#191 <https://gitlab.com/YottaDB/DB/YDB/issues/191>`_]

* When an instance is frozen due to an error switching journal files (for example, file system permissions) a Source Server clears the backlog as much as possible and then waits for the instance freeze to clear. Previously, the Source Server could terminate with a FILEDELFAIL or RENAMEFAIL. [`#282 <https://gitlab.com/YottaDB/DB/YDB/issues/282>`_]

* :code:`MUPIP JOURNAL -EXTRACT` on a journal file that is being concurrently updated creates an extract file reflecting the state of the journal file when the command started. Previously, the extract command could terminate abnormally with JNLBADRECFMT, JNLUNXPCTERR, and, rarely, GTMASSERT2 errors, sometimes with a core file. [`#284 <https://gitlab.com/YottaDB/DB/YDB/issues/284>`_]

* When a :code:`MUPIP FREEZE -ONLINE` is active, exiting YottaDB processes detach from database files without hanging. Previously, they could hang. [`#290 <https://gitlab.com/YottaDB/DB/YDB/issues/290>`_]

* LOCK commands work correctly when more than 31 subscripts at a given lock name subscript level (across all processes concurrently using a particular database region) hash to the same value. :code:`$$^%PEEKBYNAME("sgmnt_data.lock_hash_bucket_full_cntr",<region>)` now returns the number of times such a rare event was seen (and handled correctly) in . Previously, effective YottaDB r1.20, it was possible for LOCK commands in this rare case to spin and incorrectly seize ownership of the lock resource from a concurrently holding process. [`#297 <https://gitlab.com/YottaDB/DB/YDB/issues/297>`_]

* Database file updates to a process that is part of multiple replication instances works correctly in several cases. Previously it was possible for the process to incorrectly

  - issue REPLINSTMISMTCH errors;
  - terminate with a fatal GTMASSERT2 error;
  - terminate with a fatal KILLBYSIGSINFO1/SIGMAPERR/SIG-11 error;
  - freeze the wrong instance;
  - cause a memory leak when using PEEKBYNAME on the Journal Pool; or
  - attach to a Journal Pool even though no update was attempted on a corresponding database file.

  [`#312 <https://gitlab.com/YottaDB/DB/YDB/issues/312>`_]

* MUPIP FTOK -JNLPOOL  and MUPIP FTOK -RECVPOOL  operate on the specified instance-file-name. Previously they ignored the input instance-file-name and always operated on the instance file name specified by the ydb_repl_instance or gtm_repl_instance environment variable. [`#313 <https://gitlab.com/YottaDB/DB/YDB/issues/313>`_]

* Two updates to the journal file are separated by n seconds where n is greater than the epoch_interval for that region, then are guaranteed to have at least at least one intervening EPOCH record in the journal file. Previously, this setting was not honored in some cases (e.g. when an idle/free epoch was written) resulting in delaying the EPOCH record by as much as 6 seconds. [`#341 <https://gitlab.com/YottaDB/DB/YDB/issues/341>`_]

* OPEN of a SOCKET device that was previously closed after a TPTIMEOUT error while in a READ command works correctly. Previously this would abnormally terminate the process with a GTMASSERT2 fatal error. [`#348 <https://gitlab.com/YottaDB/DB/YDB/issues/348>`_]

* MUPIP REORG on a database file with non-zero RESERVED_BYTES retains the integrity of the database file. Previously, it was possible for cause a structurally damaged database file (for example DBINVGBL integrity error). [`#349 <https://gitlab.com/YottaDB/DB/YDB/issues/349>`_]

* A child process forked from a parent using the Simple API to update a database file that has ASYNCIO turned on can successfully update that database file. Previously, such a child process raised a DBIOERR. [`#358 <https://gitlab.com/YottaDB/DB/YDB/issues/358>`_]

* MUPIP JOURNAL -ROLLBACK -BACKWARD -FETCHRESYNC keeps user data in sync between primary and secondary sides of a replication connection when processes on the secondary are terminated with `kill -9` followed by a switchover. Previously, this could cause primary and secondary database instances to have different content. Note that YottaDB strongly recommends against use of `kill -9`. [`#362 <https://gitlab.com/YottaDB/DB/YDB/issues/362>`_]

* MUPIP REPLICATE -SOURCE -SHUTDOWN reports whether or not it deleted the Journal Pool shared memory and semaphores even if the instance is frozen. Previously, it reported this information only if the instance was not frozen. [`#364 <https://gitlab.com/YottaDB/DB/YDB/issues/364>`_]

* The -[NO]BEFORE_IMAGES option of the MUPIP SET JOURNAL command is optional. If neither option is specified and journaling is currently disabled in the database file header (i.e. :code:`DSE DUMP -FILEHEADER` lists "Journal State" as "DISABLED"), MUPIP SET -JOURNAL defaults to BEFORE_IMAGES for databases with the BG access method, and to NOBEFORE_IMAGES for databases with the MM access method. If neither option is specified and journaling is not disabled in the database file header (i.e. "Journal State" is not "DISABLED"), the prior journal type from the database file header is used for the new journal file. Previously, one of BEFORE_IMAGES or NOBEFORE_IMAGES option was required. [`#365 <https://gitlab.com/YottaDB/DB/YDB/issues/365>`_]

+++++++++++++++++++++++++
Language
+++++++++++++++++++++++++

* ydb_incr_s() accepts a NULL ret_value parameter, slightly simplifying application programs and marginally improving performance for applications that need to increment a node, but do not need to use the value. [`#183 <https://gitlab.com/YottaDB/DB/YDB/issues/183>`_]

* A value of -1 as the optional intexpr second parameter of `$ZSEARCH() <https://docs.yottadb.com/ProgrammersGuide/functions.html?highlight=stream#id14>`_ always returns the first file name matched by the pattern searched for, i.e., it does not use a stream. Previously, a value of -1 returned a ZSRCHSTRMCT error. [`#230 <https://gitlab.com/YottaDB/DB/YDB/issues/230>`_]

* Attempting to update a database file for which a process does not have write access, and which is mapped through multiple global directories issues a DBPRIVERR error. In prior versions, such an attempted update would abnormally terminate the process with a KILLBYSIGINFO1 error (SIG-11). [`#253 <https://gitlab.com/YottaDB/DB/YDB/issues/253>`_]

* MUPIP BACKUP issues a FILENAMETOOLONG error when the absolute path length of the target backup filename is too long (approximately 255 bytes). Previously, this resulted in an abnormal termination of the backup process with a KILLBYSIGSINFO1 fatal error (signal 6/SIGABRT). [`#267 <https://gitlab.com/YottaDB/DB/YDB/issues/267>`_]

* $$set^%PATCODE loads user defined pattern tables in UTF-8 mode, returning 1 in case of a successful load. Previously it always failed in UTF-8 mode, returning 0. [`#277 <https://gitlab.com/YottaDB/DB/YDB/issues/277>`_]

* When the `TRIGGER_MOD <https://docs.yottadb.com/AdminOpsGuide/basicops.html?highlight=trigger_mod#configuring-the-restriction-facility>`_ restriction is in place, ZSTEP is completely ignored. Previously it would be executed on completion of the trigger code. [`#295 <https://gitlab.com/YottaDB/DB/YDB/issues/295>`_]

* ZCOMPILE omits warnings during compilation if $ZCOMPILE includes :code:`-nowarning`. Previously, the compilation incorrectly issued warnings even with this setting. Note that an application can set $ZCOMPILE directly or through the environment variables :code:`ydb_compile`/:code:`gtmcompile`. [`#315 <https://gitlab.com/YottaDB/DB/YDB/issues/315>`_]

* Journal update records in MUPIP JOURNAL EXTRACT format fed to replication filters include time stamps. Previously the time stamps were zero. [`#321 <https://gitlab.com/YottaDB/DB/YDB/issues/321>`_]

* Errors inside indirection usage (@ syntax) while in the direct mode using $ETRAP return control to the direct mode prompt after executing the $ETRAP error handler. Previously, such errors caused the mumps process to abruptly terminate. [`#324 <https://gitlab.com/YottaDB/DB/YDB/issues/324>`_]

* Compiling M programs with $select() containing invalid $char() usages in UTF-8 mode correctly issues a INVDLRCVAL error. Previously, in some cases where the M line could be optimized (due to literal usage in the $CHAR() function which corresponded to an invalid codepoint), the compilation would abnormally terminate with a GTMASSERT2 fatal error. [`#329 <https://gitlab.com/YottaDB/DB/YDB/issues/329>`_]

* Simple API calls subsequent to a ydb_zwr2str_s() call work. Previously, Simple API calls subsequent to a ydb_zwr2str_s() call would fail with a SIMPLEAPINEST error. [`#344 <https://gitlab.com/YottaDB/DB/YDB/issues/344>`_]

* Even though the YottaDB data management engine is single-threaded, YottaDB has Simple API functions to support multi-threaded applications. An overview is in the blog post `YottaDB Support for Multi-threaded Applications <https://yottadb.com/yottadb-support-for-multi-threaded-applications/>`_ and details are in the `Multi-Language Programmers Guide <https://docs.yottadb.com/MultiLangProgGuide/>`_. [`#351 <https://gitlab.com/YottaDB/DB/YDB/issues/351>`_]

* $INCREMENT() is supported for global variables that have NOISOLATION turned on. Previously this reported GVINCRISOLATION errors, albeit not consistently. Note that for variables that have NOISOLATION turned on YottaDB expects application logic to ensure Isolation within a transaction without requiring YottDB to do so. [`#353 <https://gitlab.com/YottaDB/DB/YDB/issues/353>`_]

* $ZSTATUS reports the correct actual and maximum string length values in all cases following a YDB_ERR_INVSTRLEN error returned by a Simple API function call e.g. :code:`ydb_get_s()`. Previously, $ZSTATUS could contain a 0 string length and max length value in the INVSTRLEN. [`#354 <https://gitlab.com/YottaDB/DB/YDB/issues/354>`_]

* An extended reference correctly issues a NETDBOPNERR error if it points to a remote database file that cannot be opened (for example, the GNP server is not started on the remote host). Previously, if :code:`$zgbldir` was not set (or the :code:`ydb_gbldir` / :code:`gtmgbldir` environment variables were not set), a similar extended reference would terminate with a KILLBYSIGSINFO1 fatal error (SIG-11). [`#356 <https://gitlab.com/YottaDB/DB/YDB/issues/356>`_]

* $ZEDITOR reports the exit status of the last edit session invoked by a ZEDIT command. Previously, it reported 0 even if the editor invocation exited with a non-zero status. [`#360 <https://gitlab.com/YottaDB/DB/YDB/issues/360>`_]

* YottaDB issues NUMOFLOW errors for literal expressions which contain large numeric values stored as strings. Previously, in some cases (e.g., `0!("1E47"*10)`)YottaDB would return potentially incorrect values rather than issue a NUMOFLOW error. [`#363 <https://gitlab.com/YottaDB/DB/YDB/issues/363>`_]

* $SELECT stops evaluating tvexprs or exprs once a true tvexpr is encountered even if evaluating later tvexprs or exprs would result in a NUMOFLOW or INVDLRCVAL errors. Previously a NUMOFLOW or INVDLRCVAL runtime error would be incorrectly issued (e.g., `if $select(1:1,1:1E47)`). [`#371 <https://gitlab.com/YottaDB/DB/YDB/issues/371>`_]

* Indirection in a setleft correctly generates a compilation error if it is preceded by a setleft which has an invalid ISV usage (e.g. `set ($iv,@y(2))=1`). Previously such a usage would cause process termination with a fatal GTMASSERT2 error during compilation. [`#372 <https://gitlab.com/YottaDB/DB/YDB/issues/372>`_]

* $TEXT() returns source lines without line terminators. Previously, if a .m source file had line terminators and was compiled with the -EMBED_SOURCE compilation flag, $TEXT() returned source lines with line terminators in some cases and without them in others. [`#374 <https://gitlab.com/YottaDB/DB/YDB/issues/374>`_]

* :code:`ydb_tp_s()` returns a negative error code (YDB_ERR_GBLOFLOW) in case of a GBLOFLOW error. Previously, it incorrectly returned the positive value of this error code (i.e. -YDB_ERR_GBLOFLOW). [`#383 <https://gitlab.com/YottaDB/DB/YDB/issues/383>`_]

* :code:`ydb_subscript_next_s()` and :code:`ydb_subscript_previous_s()` for a nonexistent local variable return the null string (i.e. :code:`ret_value.len_used` is 0) as the next subscript. Previously, it used to incorrectly return the next subscript as the string literal "0" (i.e. :code:`ret_value.len_used` is 1). [`#394 <https://gitlab.com/YottaDB/DB/YDB/issues/394>`_]

* :code:`ydb_node_next_s()` and :code:`ydb_node_previous_s()` report a PARAMINVALID error when a :code:`ydb_buffer_t` structure passed via the :code:`*ret_subsarray` parameter has a NULL :code:`buf_addr` member. In this case, :code:`*ret_subs_used` is the index into the :code:`*ret_subsarray` array where the NULL :code:`buf_addr` was encountered. Previously this usage caused the process to terminate with a fatal KILLBYSIGSINFO1 error (SIG-11). [`#396 <https://gitlab.com/YottaDB/DB/YDB/issues/396>`_]

* Application code that calls in to M code (using :code:`ydb_ci()` / :code:`ydb_cip()`) which in turn calls out to code that in turn calls the Simple API works. Previously, such usage could result in errors and otherwise behave incorrectly. [`#401 <https://gitlab.com/YottaDB/DB/YDB/issues/401>`_]

* Routines :code:`gtm_ci()`, :code:`gtm_cip()`, and :code:`gtm_init()` are available for C code to call M code providing upward compatibility to allow non-M applications using `FIS GT.M <https://fis-gtm.com>`_ to run unchanged with YottaDB. Previously, while YottaDB provided upward compatibility for M applications, non-M applications had to replace the calls with :code:`ydb_ci()`, :code:`ydb_cip()`, and :code:`ydb_init()` respectively. [`#402 <https://gitlab.com/YottaDB/DB/YDB/issues/402>`_]


+++++++++++++++++++++
Other
+++++++++++++++++++++

* The Source Server issues a FILTERNOTALIVE error in case an external filter (invoked with the -FILTER qualifier at process startup) terminates abnormally. Previously, the Source Server could loop indefinitely waiting for a response from the terminated filter process. [`#264 <https://gitlab.com/YottaDB/DB/YDB/issues/264>`_]

* Sockets in LISTENING state can now be passed to other processes using a JOB command or the WRITE /PASS with WRITE /ACCEPT. Previously this used to hang. [`#275 <https://gitlab.com/YottaDB/DB/YDB/issues/275>`_]

* WRITE /WAIT on a SOCKET device with no sockets returns immediately. Previously it would loop until the command timed out (indefinitely if there was no timeout). [`#280 <https://gitlab.com/YottaDB/DB/YDB/issues/280>`_]

* DEVOPENFAIL messages in the syslog correctly identify the device name and system error number in case of errors during OPEN of a PIPE device. Previously, reported incorrect (garbage) device name and/or system error number/text in some cases. [`#310 <https://gitlab.com/YottaDB/DB/YDB/issues/310>`_]

* With GT.M V6.3-005, one can specify M code to run a filter that decides whether to allow a restriction or not and to control what commands actually get run when a user specifies ZSYSTEM or a PIPE OPEN command (see `GTM-8877 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8877>`_ for details). While testing this feature out, we noticed that if the $ydb_dist/restrict.txt or $gtm_dist/restrict.txt file is updated multiple times within the same second, where the first update included one M labelref for ZSYSTEM_FILTER/PIPE_FILTER and the second update included a different M labelref, the second (and later) M labelref did not take effect when a ZSYSTEM or PIPE OPEN command ran afterwards. The first M labelref still took effect incorrectly. [`#318 <https://gitlab.com/YottaDB/DB/YDB/issues/318>`_]

* $VIEW("PROBECRIT") CPT time resolution is nanoseconds. Previously, it was reported in nanoseconds, but it was a multiple of 1000, effectively making it microsecond resolution. [`#333 <https://gitlab.com/YottaDB/DB/YDB/issues/333>`_]

* OPEN of a PIPE device with the STDERR deviceparameter set to the name of an already-open device issues a STDERRALREADYOPEN error. Previously such usages resulted in unexpected device state and behavior (e.g. unexpected errors). [`#345 <https://gitlab.com/YottaDB/DB/YDB/issues/345>`_]

* When a directory tree record has an invalid collation specification (a structural error), MUPIP INTEG issues an INVSPECREC error with appropriate context (block number and offset where the error was noticed), while DSE and application programs assume no collation and proceed. Previously, in some cases of an INVSPECREC error, it was possible for all the above commands to loop for ever. [`#346 <https://gitlab.com/YottaDB/DB/YDB/issues/346>`_]

* A C→M call-in that encounters an error which is not handled by the error trap handler (:code:`$ETRAP` or :code:`$ZTRAP`) returns the error code back to the calling C code. Previously, in this situation, the process could terminate with a GTMASSERT2 fatal error if the call-in was preceded by a :code:`ydb_set_s()` Simple API call that set a node in the database spanning more than one database block. [`#352 <https://gitlab.com/YottaDB/DB/YDB/issues/352>`_]

* A Receiver Server on a Supplementary Instance starts replicating after a prior Receiver Server was shutdown after it connected to a Source Server for the first time but before it actually received any data. Previously, a receiver startup in this case could incorrectly terminate with a REPLINSTNOHIST error. [`#361 <https://gitlab.com/YottaDB/DB/YDB/issues/361>`_]

* The encryption plugin shipped with YottaDB, which uses OpenSSL for TLS for replication and SOCKET devices) does not support SSL 2.0, SSL 3.0 and TLS 1.0 as these are vulnerable to cryptographic attacks (e.g., `POODLE <https://en.wikipedia.org/wiki/POODLE>`_). The plugin continues to support the TLS 1.1 and TLS 1.2 protocols. [`#375 <https://gitlab.com/YottaDB/DB/YDB/issues/375>`_]

* The reference implementation of the encryption plugin supports TLS 1.3 (which in turn is supported by `OpenSSL <https://www.openssl.org/>`_ 1.1.1). [`#376 <https://gitlab.com/YottaDB/DB/YDB/issues/376>`_]

++++++++++++++++++++++++++
System Administration
++++++++++++++++++++++++++

* If the environment variable :code:`ydb_lct_stdnull` environment variable has the value 1, YottaDB uses standard collation for empty string (null) subscripts of local variables, i.e., the empty string collates first, before all numeric values. If set to 0, YottaDB uses the legacy collation, where the empty string collates between numeric and non-empty string subscripts. If the environment variable is not set, YottaDB uses the :code:`gtm_lct_stdnull` environment variable, and if neither is set, YottaDB uses standard collation. Previously, the :code:`ydb_lct_stdnull` environment variable was not recognized, and the default of legacy collation (which is not used by any application we know of) was an opportunity for an application to have an obscure, hard to troubleshoot, bug. **YottaDB strongly recommends against use of legacy collation**. [`#113 <https://gitlab.com/YottaDB/DB/YDB/issues/113>`_]

* The default value for the GDE region parameter STDNULLCOLL is STDNULLCOLL. Previously it was NOSTDNULLCOLL. As we are aware of no application that uses empty string (null) subscripts and NOSTDNULLCOLL, changing the default is an opportunity to keep an application from having obscure, hard-to-troubleshoot bugs as a consequence of inadvertently using NOSTDNULLCOLL. Note that `YottaDB strongly recommends against use of legacy collation <https://docs.yottadb.com/MultiLangProgGuide/MultiLangProgGuide.html#id7>`_. (`#114 <https://gitlab.com/YottaDB/DB/YDB/issues/114>`_)

* The :code:`ydbinstall` script checks for existence and contents of :code:`/etc/os-release`, and aborts the install of YottaDB if it detects an unsupported platform. Rerunning it with :code:`--force-install` overrides this behavior and goes ahead with the install even if it is an unsupported platform. [`#255 <https://gitlab.com/YottaDB/DB/YDB/issues/255>`_]

* MUMPS, DSE, LKE, MUPIP BACKUP etc. honor the :code:`ydb_poollimit` environment variable and set the poollimit value at process startup. Previously only MUPIP REORG honored this environment variable. In MUMPS, VIEW "POOLLIMIT" continues to be honored and overrides the poollimit setting initially set from the environment variable. [`#272 <https://gitlab.com/YottaDB/DB/YDB/issues/272>`_]

* The Update Process operates correctly when a trigger executes a SET $ZGBLDIR to switch the current global directory. Previously it switched the global directory correctly the first time the trigger was invoked but later invocations of the trigger did not switch the global directory resulting in any updates in the trigger code unintentionally happening in the primary global directory with which the Update process was started. A workaround in prior versions was to do a NEW $ZGBLDIR before the SET $ZGBLDIR in the trigger code. [`#293 <https://gitlab.com/YottaDB/DB/YDB/issues/293>`_]

* When sourced, the :code:`ydb_env_set` file does not clear the value of the environment variable :code:`gtm_prompt`. Previously it did. The workaround was to use the :code:`ydb_prompt` environment variable. As the :code:`gtm_prompt` environment variable is deprecated, YottaDB recommends using :code:`ydb_prompt` except in cases where compatibility with GT.M must be maintained. This also preserves ANSI escape sequences included in the :code:`ydb_prompt`/:code:`gtm_prompt` environment variables (for example one can use this to color the direct mode prompt). Previously such usages could cause a "bad variable name" error (reported as Issue 348 on GitHub). [`#309 <https://gitlab.com/YottaDB/DB/YDB/issues/309>`_]

* The ydbinstall.sh script now issues an error when an option has been specified with no value following it. For example, :code:`ydbinstall.sh --installdir --utf8` default. In this case, :code:`--installdir` needs to be followed by a  directory name but that was missed out in the specification. Previously, :code:`--utf8` (because that parameter immediately follows :code:`--installdir` in the command line) was assumed to be the directory where the install was desired and this led to confusing results. Additionally, if a relative path is specified as the install directory name, the script now creates a subdirectory under the current directory at the time when the script was invoked and installs YottaDB there. Previously, it used to install YottaDB in a subdirectory under :code:`/tmp`, later remove the entire directory and yet confusingly display a message indicating the install was successful. Finally, the :code:`geteuid` executable is no longer part of the YottaDB install. Its sole purpose was to get the effective user id at the time of installing YottaDB and this is easily obtained without a standalone executable. If you have any scripts that use :code:`getuid`, please replace it with :code:`id -un`. [`#338 <https://gitlab.com/YottaDB/DB/YDB/issues/338>`_]

* The ydbinstall.sh script checks at the outset for the existence of all utilities that it relies on (e.g. tar, gzip, grep etc.) and exits with a clear error message if any of them do not exist. Previously, it reported hard to diagnose errors at the first point of failure from a missing utility program. [`#347 <https://gitlab.com/YottaDB/DB/YDB/issues/347>`_]

* YottaDB changes terminal characteristics internally only for the duration of a READ command or while interacting in Direct Mode. Previously, YottaDB changed terminal characteristics at process startup and restored them at process exit, which meant that non-M code reading from a terminal used by M code might not behave as expected by the non-M code. [`#350 <https://gitlab.com/YottaDB/DB/YDB/issues/350>`_]

* SIGQUIT/SIGINT signals (generated by :code:`kill -3` and :code:`kill -2` respectively) are correctly handled by YottaDB processes. Previously, these signals were ignored if the target process had spawned another process (for example using the :code:`$ydb_procstuckexec`) and was waiting for it to finish. [`#357 <https://gitlab.com/YottaDB/DB/YDB/issues/357>`_]

* Environment variables can be set and unset inside M using VIEW commands. VIEW "SETENV":"envvar":value sets the environment variable named :code:`envvar` to value and VIEW "UNSETENV":"envvar" unsets the environment variable :code:`envvar`. [`#369 <https://gitlab.com/YottaDB/DB/YDB/issues/369>`_]

* YottaDB recompiles a :code:`.m` source file to generate a new :code:`.o` object file if a source file and its corresponding object file (as matched by :code:`$zroutines`) have the same time stamp. Previously, YottaDB recompiled only if the timestamp of the matching object file was older than than of the source file. To avoid unnecessary recompilation, YottaDB ensures that a generated object file has a later timestamp than its source file. Previously, it was possible for both to have the same timestamp. [`#385 <https://gitlab.com/YottaDB/DB/YDB/issues/385>`_]

* If the environment variables :code:`ydb_linktmpdir` and :code:`gtm_linktmpdir` are unspecified, but :code:`ydb_tmp` or :code:`gtm_tmp` are, YottaDB uses the latter for the location of the relinkctl control files for auto-relink enabled directories. Previously, owing to a regression effective GT.M V6.3-002 (and in turn YottaDB r1.10), in this case, the relinkctl control files were created in /tmp. [`#392 <https://gitlab.com/YottaDB/DB/YDB/issues/392>`_]

* :code:`YDB_RELEASE` is a numeric constant defined in :code:`libyottadb.h` (e.g., 124 for YottaDB r1.24) that C code accessing YottaDB can use for conditional compilation. [`#393 <https://gitlab.com/YottaDB/DB/YDB/issues/393>`_]

* The permissions of the temporary directory :code:`/tmp/yottadb/$ydb_ver`  (e.g., :code:`/tmp/yottadb/r1.24_x86_64`)  created by :code:`ydb_env_set` are read-write-execute for user, group and world if YottaDB is installed to be world-executable, and read-write-execute for the user, and the group permitted to run YottaDB, if YottaDB is installed to allow only members of a group to execute it. Previously it was read-write-execute only for the user, which while appropriate for a single-user environment, is not well suited to multi-user systems. [`#395 <https://gitlab.com/YottaDB/DB/YDB/issues/395>`_]

* Accessing through SimpleAPI functions issues a ZGBLDIRACC error if the :code:`ydb_app_ensures_isolation` environment variable is set to a non-null value but the global directory file does not exist. Previously, this abnormally terminated the process with a KILLBYSIGSINFO1 error (SIG-11). [`#397 <https://gitlab.com/YottaDB/DB/YDB/issues/397>`_]

++++++++++++++++
GT.M V6.3005
++++++++++++++++

+-------------------------------------------------------------------------------------------------------+-----------------------------+--------------------------------------------------------------------------------------------------+
| ID                                                                                                    | Category                    | Summary                                                                                          |
+=======================================================================================================+=============================+==================================================================================================+
| `GTM-3659 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-3659>`_ | Admin                       | Improved MUPIP LOAD error messaging                                                              |
+-------------------------------------------------------------------------------------------------------+-----------------------------+--------------------------------------------------------------------------------------------------+
| `GTM-4647 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-4647>`_ | Admin                       | The ydb_mstack_size environment variable specifies M stack size in KiB                           |
+-------------------------------------------------------------------------------------------------------+-----------------------------+--------------------------------------------------------------------------------------------------+
| `GTM-5059 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-5059>`_ | Admin                       | ydb_mstack_crit optionally specifies the percentage of the stack at which YottaDB should produce |
|                                                                                                       |                             | a STACKCRIT                                                                                      |
+-------------------------------------------------------------------------------------------------------+-----------------------------+--------------------------------------------------------------------------------------------------+
| `GTM-5574 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-5574>`_ | Other                       | Percent conversion routines handle larger numbers                                                |
+-------------------------------------------------------------------------------------------------------+-----------------------------+--------------------------------------------------------------------------------------------------+
| `GTM-7960 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-7960>`_ | Admin                       | Warnings of approach to maximum database file size                                               |
+-------------------------------------------------------------------------------------------------------+-----------------------------+--------------------------------------------------------------------------------------------------+
| `GTM-8836 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8836>`_ | Admin                       | Account for MUPIP journal rollback multi-threading in hash table condition handler               |
+-------------------------------------------------------------------------------------------------------+-----------------------------+--------------------------------------------------------------------------------------------------+
| `GTM-8875 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8875>`_ | Other                       | More complete detection of STACKOFLOW                                                            |
+-------------------------------------------------------------------------------------------------------+-----------------------------+--------------------------------------------------------------------------------------------------+
| `GTM-8877 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8877>`_ | Admin                       | Allow user to create M filters for the commands passed to zsystem and PIPE                       |
+-------------------------------------------------------------------------------------------------------+-----------------------------+--------------------------------------------------------------------------------------------------+
| `GTM-8910 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8910>`_ | DB                          | Prevent disruption of $ORDER(gvn,1) by MUPIP REORG                                               |
+-------------------------------------------------------------------------------------------------------+-----------------------------+--------------------------------------------------------------------------------------------------+
| `GTM-8930 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8930>`_ | Language                    | Improvement to return values from $VIEW("JNLPOOL")                                               |
+-------------------------------------------------------------------------------------------------------+-----------------------------+--------------------------------------------------------------------------------------------------+
| `GTM-8940 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8940>`_ | DB                          | Performance improvement for ftok semaphores                                                      |
+-------------------------------------------------------------------------------------------------------+-----------------------------+--------------------------------------------------------------------------------------------------+
| `GTM-8941 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8941>`_ | Admin                       | LKE recognizes the full keyword for the -CRITICAL qualifier                                      |
+-------------------------------------------------------------------------------------------------------+-----------------------------+--------------------------------------------------------------------------------------------------+
| `GTM-8942 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8942>`_ | Other                       | Reduce impact of signal management                                                               |
+-------------------------------------------------------------------------------------------------------+-----------------------------+--------------------------------------------------------------------------------------------------+
| `GTM-8943 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8943>`_ | Language                    | ZGOTO 0 in a call-in returns to the invoking C code                                              |
+-------------------------------------------------------------------------------------------------------+-----------------------------+--------------------------------------------------------------------------------------------------+
| `GTM-8945 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8945>`_ | Admin                       | Prevent Receiver hang after killing an Update Process                                            |
+-------------------------------------------------------------------------------------------------------+-----------------------------+--------------------------------------------------------------------------------------------------+
| `GTM-8949 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8949>`_ | Other                       | Prevent recursive calls to system memory allocator                                               |
+-------------------------------------------------------------------------------------------------------+-----------------------------+--------------------------------------------------------------------------------------------------+
| `GTM-8951 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8951>`_ | Language                    | $TEXT() and ZPRINT use auto-relink when their argument includes a routinename                    |
+-------------------------------------------------------------------------------------------------------+-----------------------------+--------------------------------------------------------------------------------------------------+
| `GTM-8953 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8953>`_ | Admin                       | ROLLBACK FORWARD with -VERIFY properly applies all valid updates                                 |
+-------------------------------------------------------------------------------------------------------+-----------------------------+--------------------------------------------------------------------------------------------------+
| `GTM-8954 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8954>`_ | Admin                       | Source Server sends all errors to the log file                                                   |
+-------------------------------------------------------------------------------------------------------+-----------------------------+--------------------------------------------------------------------------------------------------+
| `GTM-8955 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8955>`_ | Other                       | Relink Locking Speedup                                                                           |
+-------------------------------------------------------------------------------------------------------+-----------------------------+--------------------------------------------------------------------------------------------------+
| `GTM-8956 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8956>`_ | Other                       | -NOWARNING compiler qualifier suppresses more messages                                           |
+-------------------------------------------------------------------------------------------------------+-----------------------------+--------------------------------------------------------------------------------------------------+
| `GTM-8957 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8957>`_ | Admin                       | MUPIP SET -NOREADONLY -ACC=BG works in one command                                               |
+-------------------------------------------------------------------------------------------------------+-----------------------------+--------------------------------------------------------------------------------------------------+
| `GTM-8958 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8958>`_ | Admin                       | TLS reference implementation plug-in disables SSLv3 by default                                   |
+-------------------------------------------------------------------------------------------------------+-----------------------------+--------------------------------------------------------------------------------------------------+
| `GTM-8959 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8959>`_ | Language                    | For 64-bit Linux, fix ZGOTO 0 within a call-in                                                   |
+-------------------------------------------------------------------------------------------------------+-----------------------------+--------------------------------------------------------------------------------------------------+
| `GTM-8962 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8962>`_ | Language                    | Clean up ZSHOW "i" output                                                                        |
+-------------------------------------------------------------------------------------------------------+-----------------------------+--------------------------------------------------------------------------------------------------+
| `GTM-8964 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8964>`_ | Admin                       | MUPIP respects -READONLY (MM) database state                                                     |
+-------------------------------------------------------------------------------------------------------+-----------------------------+--------------------------------------------------------------------------------------------------+
| `GTM-8965 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8965>`_ | Language                    | Alternation pattern with a large match produces a PATALTER2LARGE error                           |
+-------------------------------------------------------------------------------------------------------+-----------------------------+--------------------------------------------------------------------------------------------------+
| `GTM-8967 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8967>`_ | DB                          | Correct reporting of globals reported by TPRESTART                                               |
+-------------------------------------------------------------------------------------------------------+-----------------------------+--------------------------------------------------------------------------------------------------+
| `GTM-8969 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8969>`_ | DB                          | When encrypting for the first time, prevent spurious CRYPTOPFAILED errors                        |
+-------------------------------------------------------------------------------------------------------+-----------------------------+--------------------------------------------------------------------------------------------------+
| `GTM-8973 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8973>`_ | Other                       | External call table loading more careful about M labelrefs                                       |
+-------------------------------------------------------------------------------------------------------+-----------------------------+--------------------------------------------------------------------------------------------------+
| `GTM-8974 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8974>`_ | DB                          | Improved handling of significant IO pauses in Database or Journal I/O                            |
+-------------------------------------------------------------------------------------------------------+-----------------------------+--------------------------------------------------------------------------------------------------+
| `GTM-8980 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8980>`_ | Language                    | VIEW and $VIEW() fixes, particularly related to region arguments                                 |
+-------------------------------------------------------------------------------------------------------+-----------------------------+--------------------------------------------------------------------------------------------------+
| `GTM-8981 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8981>`_ | Language                    | Fix to IF @\<literal\>                                                                           |
+-------------------------------------------------------------------------------------------------------+-----------------------------+--------------------------------------------------------------------------------------------------+
| `GTM-8985 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8985>`_ | Language                    | Prevent possible incorrect result from a function with all literal arguments in a Boolean        |
|                                                                                                       |                             | expression                                                                                       |
+-------------------------------------------------------------------------------------------------------+-----------------------------+--------------------------------------------------------------------------------------------------+
| `GTM-8988 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8988>`_ | Admin                       | MUPIP RESTORE handling of out-of-space                                                           |
+-------------------------------------------------------------------------------------------------------+-----------------------------+--------------------------------------------------------------------------------------------------+
| `GTM-8989 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8989>`_ | Admin                       | Prevent unusual MUPIP RUNDOWN hang                                                               |
+-------------------------------------------------------------------------------------------------------+-----------------------------+--------------------------------------------------------------------------------------------------+
| `GTM-8990 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8990>`_ | Language                    | ZRUPDATE treats a cycle of symbolic links like a simple missing routine                          |
+-------------------------------------------------------------------------------------------------------+-----------------------------+--------------------------------------------------------------------------------------------------+
| `GTM-8992 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8992>`_ | DB                          | YottaDB never decrements database statistics for $DATA(), $GET(), $ORDER(), or $QUERY()          |
+-------------------------------------------------------------------------------------------------------+-----------------------------+--------------------------------------------------------------------------------------------------+
| `GTM-8996 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8996>`_ | Admin                       | YottaDB issues an fsync on the database file after a region freeze goes into effect              |
+-------------------------------------------------------------------------------------------------------+-----------------------------+--------------------------------------------------------------------------------------------------+
| `GTM-9001 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-9001>`_ | Other                       | Improved bounds checking in DSE and MUPIP                                                        |
+-------------------------------------------------------------------------------------------------------+-----------------------------+--------------------------------------------------------------------------------------------------+

+++++++++++++++++++++++
Database
+++++++++++++++++++++++

* :code:`$ORDER(gvn,-1)` and :code:`$ZPREVIOUS(gvn)` appropriately handle the concurrent move of the global variable tree root block of the gvn by MUPIP REORG; previously, this concurrence could occasionally produce a GVORDERFAIL with a status code of tSSS. (`GTM-8910 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8910>`_)

* YottaDB avoids superfluous public (ftok) semaphore control operations on database startup or rundown. (`GTM-8940 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8940>`_)

* TPRESTART messages correctly identify the global causing the conflict; previously they could report an incorrect global name. (`GTM-8967 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8967>`_)

* YottaDB processes work correctly when a concurrent :code:`MUPIP REORG -ENCRYPT` encrypts a previously unencrypted database file. Previously YottaDB processes could fail with CRYPTOPFAILED errors unaccompanied by other errors explaining the reason for the failure. This issue was only observed in the development environment, and was never reported by a user. (`GTM-8969 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8969>`_)

* YottaDB processes attempting to acquire a shared database resource and encountering long delays continue to wait if the process holding the resource is still active. Previously they intervened prematurely which could cause DBDANGER errors and accompanying database damage. YottaDB processes attempting to acquire a shared resource required to perform journal writes and encountering an extended delay in acquiring the resource, issue JNLSENDOPER/JNLFLUSH/JNLPROCSTUCK messages to the system log, and, if so configured, freeze the instance as a result, sending REPLINSTFROZEN/REPLINSTFREEZECOMMENT messages to the system log. They continue to attempt the journal writes, removing the instance freeze if they eventually succeed. Previously, such processes received an error, and the instance required a operator action to release the freeze. (`GTM-8974 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8974>`_)

* YottaDB does not decrement database statistics for $DATA(), $GET(), $ORDER(), or $QUERY(). Note that, to minimize performance impact, YottaDB updates most database statistics without concurrency controls, so the statistics are approximate and may have brief fluctuations. Previously YottaDB made explicit adjustments which reporting was more likely to detect. MERGE operations increment DATA and GET database statistics to more accurately reflect database activity. Previously, Merge suppressed increments to those statistics under certain conditions.(`GTM-8992 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8992>`_)

++++++++++++++++++++++++
Language
++++++++++++++++++++++++

* If the process has access to an instance file designation, $VIEW("JNLPOOL") returns its name; if the process has not yet opened the pool, the return contains an asterisk after the name. If the process does not have sufficient information to determine a replication journal instance file, the function returns "No replication instance defined." Previously, the function with this argument returned an empty string if the process had not opened the journal pool. (`GTM-8930 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8930>`_)

* A ZGOTO 0 in case of call-in, unwinds all the M stack frames and returns to the invoking C routine. In case of a non-CI invocation, this terminates the process. A :code:`ydb_ci` invocation without calling :code:`ydb_init()` first, proceeds with the CI invocation. Previously, this produced a segmentation violation (SIG-11) error. This issue was only observed in the development environment, and was never reported by a user. (`GTM-8943 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8943>`_)

* With auto-relink enabled, $TEXT() and ZPRINT use auto-relink when their argument includes a routinename. Note that ZBREAK does not use auto-relink because its intended purpose is as a debugging facility and if the user intends the ZBREAK for a new version they can explicitly request it with a ZLINK. Previously, $TEXT() and ZPRINT used the version most recently linked by the process even if a new version had become available in an auto-relink enabled environment. (`GTM-8951 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8951>`_)

* ZGOTO 0 in a call-in returns to the invoking program. Previously on x86-64 Linux systems with glibc 2.24 or later, this generated a segmentation violation (SIG-11).(`GTM-8959 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8959>`_)

* :code:`ZSHOW "I"` outputs $ZPIN and $ZPOUT even if they are the same as $PRINCIPAL and no longer displays $ZPROCESS, as it was only meaningful on OpenVMS; also, $ZTNAME appears in proper alphabetic sequence. Previously if $ZPIN or $ZPOUT matched $PRINCIPAL, :code:`ZSHOW "I"` omitted them, $ZPROCESS appeared as an empty string and $ZTNAME was not ordered appropriately. (`GTM-8962 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8962>`_)

* Pattern match of a string with an alternation match exceeding what YottaDB can handle produces a PATALTER2LARGE error; previously this condition produced a segmentation violation (SIG-11). (`GTM-8965 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8965>`_)

* VIEW and $VIEW() with a empty string or inappropriate region-list works appropriately; previously, these could cause inappropriate results, including a segmentation violation (SIG-11). :code:`$VIEW("statshare")` returns a 0 when the process has sharing disabled, a 1 when it has sharing enabled and a 2 when sharing is selectively enabled. Previously, it did not differentiate between the all and selective cases and returned 1 when sharing was disabled and selective disabling was also specified. :code:`$VIEW("statshare","<region>")` works appropriately even if the region had been selectively disabled when full sharing is disabled and the region had not been opened. In r1.22, this set of conditions produced a segmentation violation (SIG-11). The error messages when invalid parameters are passed to VIEW/$VIEW() print the name of the parameter; previously such error messages did not have the name of the parameter. (`GTM-8980 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8980>`_)

* IF :code:`@<literal>` works correctly when the literal evaluates to FALSE; previously, it tended to fail with an inappropriate INDEXTRACHARS error or a segmentation violation (SIG-11). (`GTM-8981 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8981>`_)

* The compiler appropriately handles possible string returns into Boolean expressions from functions with all literal arguments; previously, it could produce an incorrect result. The workaround was to avoid all literal arguments for $CHAR(), $EXTRACT(), $PIECE(), their $Z\*() variants and $SELECT() when they appeared in Boolean expressions. (`GTM-8985 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8985>`_)

* When ZRUPDATE encounters a cycle of symbolic links without finding a specified routine, it treats the cycle the same as a simple routine not found and ignores the missing routine. Previously, the command issued an error with text about "too many levels of symbolic links". This issue was observed in the development environment after upgrading to newer Linux distributions. (`GTM-8990 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8990>`_)

+++++++++++++++++++++++
System Administration
+++++++++++++++++++++++

* MUPIP LOAD reports ranges of records not loaded due to missing database files; previously it reported an error for every such record. (`GTM-3659 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-3659>`_)

* YottaDB supports specifying the M stack size in KiB with the :code:`ydb_mstack_size` environment variable. No setting or a setting of 0 uses the default (272KiB). The minimum supported size is 25 KiB; YottaDB reverts values smaller than this to 25 KiB. The maximum supported size is 10000 KiB; YottaDB reverts values larger than this to 10000 KiB. (`GTM-4647 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-4647>`_)

* YottaDB recognizes setting the environment variable :code:`ydb_mstack_crit_threshold` to specify an integer between 15 and 95 defining the percentage of the stack which should be used before YottaDB emits a STACKCRIT warning. If the value is below the minimum or above the maximum YottaDB uses the minimum or maximum respectively. The default is 90. (`GTM-5059 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-5059>`_)

* An automatic database file extension, MUPIP EXTEND, INTEG and SIZE all put a message in the system logs when the database reaches 88% of its maximum size. Beyond this 88% threshold, manual and automatic extends only report at 1% intervals, but INTEG and SIZE report at every subsequent invocation while the condition persists. All but the automatic extension also produce the message for the operator. Previously, YottaDB gave no such warnings, which made it necessary to proactively check on database size. (`GTM-7960 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-7960>`_)

* When a multi-threaded instance of :code:`mupip journal -rollback` runs out of memory during a rehashing operation, child threads transfer error-handling to the parent thread and terminate. Previously, child threads occasionally failed to report the error, causing MUPIP to halt without a descriptive error message. This issue was only observed in the development environment, and was never reported by a user. (`GTM-8836 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8836>`_)

* 
  The YottaDB restriction mechanism recognizes the following lines:

  ::

       ZSYSTEM_FILTER[:M labelref]
       PIPE_FILTER[:M labelref]

  The labelref must include a routine name. If a process is restricted by a ZSYSTEM or PIPE_OPEN line in the restrictions file that restriction takes precedence over the corresponding filter restriction. Otherwise when a process is subject to these restrictions, YottaDB inserts an invocation of the labelref prior to the restricted command, passing a string containing the argument to the ZSYSTEM command or the command deviceparameter of the PIPE OPEN. The path to the filter routine must be included in $zroutines. YottaDB recommends that the filter routine is placed in a location with restricted access such as $ydb_dist. If the filter invocation return is -1, YottaDB produces a RESTRICTEDOP error, otherwise it executes the command using the returned string via output parameters as a, possibly identical, replacement for the original string. Since YottaDB uses the call-ins mechanism to execute the filters, a filter invocation inside a TP transaction in call-ins produces a CITPNESTED error. Note that because ZSYSTEM and OPEN are not Isolated actions YottaDB recommends against their use within a TP transaction. Filters also increment the nested level of call-ins. A recursive filter invocation produces a NOFILTERNEST error. YottaDB reports all filter errors to the operator log accompanied by a COMMFILTERERR.

  The M labelref is mandatory. If it is not specified a syntax error is assumed (which in turn enables all restrictions unconditionally).

  An example restrict file for this:

  ::
     
       cat $ydb_dist/restrict.txt

       ZSYSTEM_FILTER:^filterzsy
       PIPE_FILTER:^filterzsy

  The actual filter routine:

  ::
 
       filterzsy(inarg,outarg);
       if ""=inarg set outarg="-1;must provide a command" quit
       for i=1:1 set arg=$piece(inarg,";",i) quit:""=arg  do  quit:$data(outarg)
       . for  quit:$zchar(9,32)'\[$extract(arg)  set arg=$extract(arg,2,9999)
       . set cmd=$piece(arg," ")
       . for restrict="sudo","cd" if cmd=restrict set outarg="-1;command "_restrict_" not permitted" quit
       . quit:$data(outarg)
       . if "echo"=cmd set $piece(arg," ")="echo #",$piece(inarg,";",i)=arg    ;example of modification
       set:'$data(outarg) outarg=inarg
       quit +outarg

  Filter execution starts with $STACK=1 ($ZLEVEL=2).

  Following are the YottaDB commands, Intrinsic Special Variables, and functions whose behavior changes in the context of a filter invocation.

  ZGOTO 0 (zero) returns to the processing of the restricted command as does ZGOTO 1 (one) with no entryref, while ZGOTO 1:entryref replaces the originally invoked filter and continues filter execution.

  $ZTRAP/$ETRAP NEW'd at level 1.

  $ZLEVEL initializes to one (1) in GTM$CI, and increments for every new stack level.

  $STACK initializes to zero (0) in GTM$CI frame, and increments for every new stack level.

  $ESTACK NEW'd at level one (1) in GTM$CI frame.

  $ECODE/$STACK() initialized to the empty string at level one (1) in GTM$CI frame.

  After the filter completes, YottaDB restores the above to their values at the invocation of the filter. (`GTM-8877 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8877>`_)

* LKE recognizes the full keyword for the -CRITICAL qualifier; previously it only accepted -CRIT. (`GTM-8941 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8941>`_)

* The Receiver Server process recovers after its Update process was terminated with a signal while idle. Previously, on Linux systems with glibc 2.25 or newer, the Receiver Server process could hang indefinitely, requiring manual cleanup of the process and shared memory. This issue was only observed in the development environment, and was never reported by a user. (`GTM-8945 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8945>`_)

* :code:`MUPIP JOURNAL -ROLLBACK -FORWARD` when executed with -VERIFY properly applies all updates. Previously updates that should have been applied to the database were instead sent to the lost transaction file. The workaround was to execute :code:`MUPIP JOURNAL -VERIFY` independent of the FOWARD ROLLBACK. (`GTM-8953 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8953>`_)

* The Source Server directs errors to the Source Server log file. A previous fix with GTM-8576 was incomplete and could result in TLS initialization error messages not logged to the server log file. (`GTM-8954 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8954>`_)

* Executing :code:`MUPIP SET -NOREAD_ONLY -ACC=BG` on a read-only database sets the access mode to BG and turns off read-only. Previously, this action would result in a READONLYNOBG error message and no changes to the file. (`GTM-8957 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8957>`_)

* The TLS reference implementation plug-in disables SSLv3 by default. Previously, customers wishing to disable SSLv3 needed to add the configuration option ssl-options: :code:`"SSL_OP_NO_SSLv3";` to the "tls" namespace in the $ydbcrypt_config configuration file. (`GTM-8958 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8958>`_)

* MUPIP does not modify -READONLY (MM) database files; previously various MUPIP commands could inappropriately update state information in such database files, causing errors when subsequently using the database. (`GTM-8964 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8964>`_)

* MUPIP RESTORE exits with an error when it encounters an out-of-space condition. Previously, if MUPIP RESTORE encountered an out-of-space condition, it crashed with a segmentation violation (SIG-11). This issue was only observed in the development environment, and was never reported by a user. (`GTM-8988 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8988>`_)

* MUPIP RUNDOWN works as expected. In rare situations, processes killed during transaction commits could leave the transaction information in an inconsistent state that caused MUPIP RUNDOWN to hang. Note that YottaDB strongly recommends against kill -9 of a process accessing the database as it can induce database damage. This issue was only observed in the development environment, and was never reported by a user. (`GTM-8989 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8989>`_)

* YottaDB issues an fsync on the database file after a region freeze goes into effect, which forces the underlying file system to harden changes to secondary storage. (`GTM-8996 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8996>`_)

++++++++++++++++
Other
++++++++++++++++

* The %DH,%HD,%OH,%HO,%OD, and %DO routines now handle conversions with numbers up to YottaDB's maximum string length in size. Additionally, %DH and %DO routines now handle the conversion of negative decimal numbers properly, even when the specified length is not long enough to represent that fully converted number. Previously, the %DH,%HD,%OH,%HO,%OD, and %DO routines could only deal with numbers up to the equivalent of the maximum 18-digit decimal number. Attempting to convert a number larger than this would cause roundoff in the final result, which still occurs if they are used for arithmetic. Moreover, specifying a length for the %DH and %DO routines that was not long enough to hold the fully converted result would cause the routines to produce incorrect values. The workaround for both these issues was to avoid using these percent routines with numbers greater than the maximum 18-digit decimal number and to always specify an appropriate length for the %DH and %DO routines. (`GTM-5574 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-5574>`_)

* When a stack frame exceeds the M virtual machine stack pointer, YottaDB issues a STACKOFLOW error and produces a YDB\_FATAL_\* context dump, but no core file, as this an application issue, rather than a YottaDB problem. A module with a very large number of variables and/or dynamic literals can cause this problem. Note that the $ydb\_mstack environment variable can, within limits, set the size of the M virtual machine stack size. Previously if the overflow was sufficient to make the stack pointer negative, the process exited with a segmentation violation (SIG-11). (`GTM-8875 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8875>`_)

* YottaDB avoids superfluous signal management operations. Previously system traces showed more sigprocmask system calls than desirable, particularly on Linux systems with Meltdown/Spectre mitigation in place. (`GTM-8942 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8942>`_)

* YottaDB prevents recursive calls to the system memory allocator. Previously, the system memory allocator could be called recursively which resulted in a process hanging. This issue was only observed in the development environment, and was never reported by a user. (`GTM-8949 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8949>`_)

* YottaDB uses a lighter weight locking mechanism to protect the relink control file. Previously, a large number of concurrent relink control operations such as those done on process shutdown could cause the system to use an excessive amount of CPU. (`GTM-8955 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8955>`_)

* The YottaDB compiler -NOWARNING qualifier for the MUMPS command and $ZCOMPILE suppresses warning messages for BADCHAR, BLKTOODEEP, and LITNONGRAPH; previously it did not. (`GTM-8956 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8956>`_)

* YottaDB external call loading is more discriminating about M labelrefs; previously it did not detect all invalid labelrefs, which deferred error detection and made diagnosis more challenging. (`GTM-8973 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-8973>`_)

* YottaDB appropriately bounds checks variable length input parameters to MUPIP and DSE. Previously it did not detect an off-by-one buffer overrun. Also, MUPIP EXTRACT issues the ICUNOTENABLED warning message when used with the -OCHSET qualifier while in M mode. Previously, attempting to use -OCHSET while in M mode resulted in a SIG-11. These issues were only observed in the development environment, and was never reported by a user. (`GTM-9001 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-005_Release_Notes.html#GTM-9001>`_)

---------------------
More Information
---------------------

-------------------
Messages
-------------------

++++++++++++++++
From YottaDB
++++++++++++++++

**INVSPECREC**, pppp Invalid global modifier record

MUPIP Error: This indicates that MUPIP INTEG found a corrupt 4-byte collation record was found for a global variable (that is the 1st of the 4 bytes is not 1). pppp identifies the path in the directory tree (each element of the path consisting of a block#/offset) leading to the error.

Action: Use DSE to examine the corrupt record and fix it. Report the entire incident context to your YottaDB support channel.

**RECLOAD**, Error loading record number: nnnn

MUPIP Error: This message identifies nnnn, a record or a range of records, that MUPIP could not LOAD and follows a message about the cause. If this message is Fatal, which it can be for BIN format, it produces a core file for diagnostic analysis.

Action: Address the cause or, for GO and ZWR format input files, examine the record or range of records with a text editor for possible correction or alternate action and for BIN format if fixing the cause does not resolve the error switch to ZWR format EXTRACT.

**REGSSFAIL**, Process pppp encountered error eeee contributing to the snapshot for region rrrr - the snapshot is no longer valid.

MUPIP Error: A YottaDB process encountered failure while opening snapshot file or attaching to shared memory or writing a block to the snapshot file, any of which invalidate the snapshot file. The original error eeee that process pppp encountered follows the REGSSFAIL error message and can also be found in the syslog (search for messages from process pppp).

Action: Examine the syslog for messages issued by process pppp to obtain details of the failure and take action, possibly by modifying file access characteristics or user roles, to address the problem.

**SETENVFAIL**, VIEW "SETENV":"eeee" failed in setenv() system call

Run Time Error: This indicates that a setenv() system call failed for the environment variable named eeee.

Action: Examine the accompanying SYSCALL error message which has more detail on the error returned by the setenv() call.

**SIMPLEAPINOTALLOWED**, Process cannot switch to using Simple API while already using threaded API.

Run Time Error: This indicates a process has started using the threaded Simple API functions (e.g. ydb_set_st()) and is now trying to use the Simple API functions (e.g. ydb_set_s()).

Action: A process can only use either Simple API functions or threaded Simple API functions, not both.

**STAPIFORKEXEC**, Calls to YottaDB are not supported after a fork() if threaded Simple API functions were in use in parent. Call exec() first

Run Time Error: This indicates a process that has already used at least one threaded Simple API function did a fork() to create a child process and is trying to use YottaDB (e.g. Simple API functions like ydb_set_s(), or threaded Simple API functions like ydb_set_st()).

Action: Once a process that has used threaded Simple API functions or threaded Simple API functions does a fork(), the child process has to do an exec() before it can call again into YottaDB (using Simple API functions or threaded Simple API functions).

**STDERRALREADYOPEN**, STDERR deviceparameter specifies an already open device xxxx

Run Time Error: This indicates that the STDERR deviceparameter in the OPEN command of a PIPE device specifies a device name xxxx that is already open in the process.

Action: Specify a device name that is not already an open device in the process.

**THREADEDAPINOTALLOWED**, Process cannot switch to using threaded Simple API while already using Simple API

Run Time Error: This indicates a process has started using the Simple API functions (e.g. ydb_set_s()) and is now trying to use the threaded Simple API functions (e.g. ydb_set_st()).

Action: A process can only use either Simple API functions or threaded Simple API functions, not both. If the base program corresponding to this process is an M program, it can only use Simple API functions.

**UNSETENVFAIL**, VIEW "UNSETENV":"eeee" failed in unsetenv() system call

Run Time Error: This indicates that a unsetenv() system call failed for the environment variable named eeee.

Action: Examine the accompanying SYSCALL error message which has more detail on the error returned by the unsetenv() call.

++++++++++++++++
From GT.M
++++++++++++++++

**COLLDATAEXISTS**, Collation type cannot be changed while xxxx data exists

Run Time Error: This indicates that an attempt was made to change the collation type while xxxx was either a subscripted local for a process collation change or a gvn name for global variable collation.

Action: KILL or NEW the local variables before you change the local collation type, or KILL a gvn before changing its collation.

**COMMFILTERERR**, Error executing the command filter for FFFF DDDD

Run Time Error: Reports a problem in filter code where FFFF describes the nature of the filter and DDDD some thing about the nature of the issue. There may be associated/related messages. Because filters are a potential security tool, these errors tend are generally reported to the operator log.

Action: Analyze the filter code in light of the messages and revise accordingly.

**EXTRINTEGRITY**, Database ffff potentially contains spanning nodes or data encrypted with two different keys

MUPIP Error: MUPIP EXTRACT cannot run because the database file ffff might contain spanning nodes or be partially encrypted with a particular key. Proceeding on a live database in such situation could result in data corruption.

Action: If you encounter this error while running MUPIP EXTRACT with -FORMAT="BINARY", re-run the command with the -FREEZE qualifier. MUPIP EXTRACT requires -FREEZE to acquire stand-alone access to produce a consistent copy of the data. However, not using -FREEZE when you request a MUPIP EXTRACT may produce a loadable, if inconsistent output. If you encountered this error while running MUPIP EXTRACT with ZWR or GO format, it is likely that your database is encrypted with more than one key; with BINARY output it may be multiple keys or spanning node data. If the issue is a key change, run MUPIP REORG -ENCRYPT to complete the encryption of the database. As a final resort, you may use an -OVERRIDE qualifier to proceed on a live database that either contains spanning nodes or is undergoing (re)encryption. Although EXTRACT -OVERRIDE may produce text for analysis, the result is not suitable as input for MUPIP LOAD and YottaDB highly discourages using -OVERRIDE.

**FAILEDRECCOUNT**, LOAD unable to process MMMM records

MUPIP Error: MUPIP LOAD was unable to load MMMM records from the specified input extract.

Action: Examine prior RECLOAD error messages for causes for the failed records and address them.

**ICUNOTENABLED**, ICU libraries not loaded

Run Time Warning: The operation required the library containing support for International Components for Unicode (ICU) but YottaDB could not find libicu. There may be other messages.

Action: If you require UTF-8 support, install an appropriate ICU library - see the YottaDB `Administration and Operations Guide <https://docs.yottadb.com/AdminOpsGuide/index.html>`_ for information on ICU setup.

**LOADRECCNT**, Last EXTRACT record processed by LOAD: RRRR

MUPIP Information: This message indicates number of records (RRRR) MUPIP LOAD processed. The number of records represents the sum of header records, successfully loaded data records, and failed records. Note: LOAD may have stopped processing due to a record limit in the command or a \<CTRL-C\>.

Action: Ensure the identified stopping point corresponds with your intentions.

**LOWSPC**, WARNING: Database DDDD has less than PPPP% of the total block space remaining. Blocks Used: UUUU Total Blocks Available: AAAA

Operator log Information: The database has UUUU block in use and is appoaching its current limit of AAAA blocks. When the database reaches the 88% size threshold, and for every 1% increase in size and beyond, YottaDB reports the blocks used in the LOWSPC warning as the sum of the data blocks and the local bit map blocks.

Action: Purge data if possible. Consider a MUPIP REORG to compact the remaining data. Investigate whether migrating to a database created by a current version has a higher limit. Move some data to another, possibly new, region and delete it from this one.

**MSTACKCRIT**, User-specified M stack size critical threshold of xxxx not appropriate; must be between mmmm and nnnn; reverting to kkkk

Run Time Error: The environment variable ydb_mstack_crit_threshold was set to an invalid value, either too large, in which case YottaDB uses the largest acceptable value or too low, in which case YottaDB uses the smallest acceptable value.

Action: If the adjusted value is unacceptable, revise or unset the environment variable.

**NOFILTERNEST**, Filter nesting not allowed

Run Time Error: Filter code must not invoke other code that requires a filter.

**PATALTER2LARGE**, Pattern match alternation exceeded the LLLL repetition limit on prospective matches

Run Time Error: An alternation pattern applied to a long occurrence of that pattern reached a YottaDB limit (LLLL) on tracking the match.

Action: Revise the logic to reduce the size of the string being matched or to otherwise break up the match into smaller parts.

**REGFILENOTFOUND**, Database file DDDD corresponding to region RRRR cannot be found

MUPIP Error: This indicates MUPIP cannot locate the database file DDDD mapped to region RRRR.

Action: Ensure that the current global directory is the one intended and that it maps the file intended. If the path is relative or includes environment variables, ensure that the current working directory and any environment variables are appropriate. Also ensure the file exists and has authorizations, including its path, that make it available to the user attempting to access it. If the MUPIP command involves a statsDB (for example MUPIP INTEG -STATS), ensure that the appropriate regions have STATS enabled, that the $ydb_statsdir environment variable has been properly defined, and that other processes are using shared statistics, as MUPIP by itself does not create new statsDB databases. Note that MUPIP INTEG does not create statsDB and reports any that it skips with an informational message, but exits with a normal status after such skips.

**VIEWCMD**, View parameter pppp is not valid with the VIEW command

Run Time Error: This indicates that the VIEW command has an argument pppp that is only valid with the $VIEW() function.

Action: Modify the argument.

**VIEWFN**, View parameter pppp is not valid with the VIEW command

Run Time Error: This indicates that the $VIEW() function has an argument pppp that is only valid with the VIEW command.

Action: Modify the argument.

------------------------
Tarball Hashes
------------------------

+----------------------------------------------------------------------------------------------+------------------------------------------------+
| sha256sum                                                                                    | File                                           |
+==============================================================================================+================================================+
| 7e405a94e0be03ac24be5d6c2741f0195414c875b0a72fe8c6fdfb6d9693b375                             | yottadb_r124_linux_aarch64_pro.tgz             |
+----------------------------------------------------------------------------------------------+------------------------------------------------+
| e01dd5cebe5c407d0f32419eab08429765f653df24e7a2b857485d386abaed54                             | yottadb_r124_linux_armv6l_pro.tgz              |
+----------------------------------------------------------------------------------------------+------------------------------------------------+
| 01c6f70216a5e07df0cba611c5d549c6caefc13fec43e481d8efe7a4c4679731                             | yottadb_r124_linux_armv7l_pro.tgz              |
+----------------------------------------------------------------------------------------------+------------------------------------------------+
| 4010f898fc190dce8e3fe3b73e6e26b76461dd8352c372cc22731d85b0d6c943                             | yottadb_r124_linux_x8664_pro.tgz               |
+----------------------------------------------------------------------------------------------+------------------------------------------------+
| 8e125474aed4b1aa0e0a66c527ad5f558da48353bd4b2b82bfd29e58fe1d26dd                             | yottadb_r124_rhel7_x8664_pro.tgz               |
+----------------------------------------------------------------------------------------------+------------------------------------------------+

-------------------------
Legal Stuff
-------------------------

Copyright © 2019 YottaDB LLC

Permission is granted to copy, distribute and/or modify this document under the terms of the `GNU Free Documentation License, Version 1.3 <http://www.gnu.org/licenses/fdl.txt>`_ or any later version published by the Free Software Foundation; with no Invariant Sections, no Front-Cover Texts and no Back-Cover Texts.

YottaDB® is a registered trademark of YottaDB LLC.
GT.M™ is a trademark of Fidelity National Information Services, Inc.
Other trademarks belong to their respective owners.

This document contains a description of YottaDB and the operating instructions pertaining to the various functions that comprise the software. This document does not contain any commitment of YottaDB LLC. YottaDB LLC believes the information in this publication is accurate as of its publication date; such information is subject to change without notice. YottaDB LLC is not responsible for any errors or defects.
