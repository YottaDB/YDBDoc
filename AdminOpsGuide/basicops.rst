.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2017-2022 YottaDB LLC and/or its subsidiaries.#
.. # All rights reserved.                                        #
.. #                                                             #
.. #     This document contains the intellectual property        #
.. #     of its copyright holder(s), and is made available       #
.. #     under a license.  If you do not know the terms of       #
.. #     the license, please stop and do not read further.       #
.. #                                                             #
.. ###############################################################

.. index:: Basic Operations

=======================
3. Basic Operations
=======================

.. contents::
   :depth: 5

------------------------------
YottaDB Environment Setup
------------------------------

Several environment variables control the operation of YottaDB. Some of them are required for normal operation, whereas for others, YottaDB assumes a default value if they are not set. The three most important ones are:

* **ydb_dist**: Points to the directory where YottaDB is installed. If it is not set, the :code:`yottadb` program sets it to the directory from which it is executed, and child processes inherit it.

* **ydb_gbldir**: Points to a global directory file. A global directory file maps global variables (the hierarchical key-value relationships of the YottaDB database) to database files in the filesystem. This is not required by processes that do not access any global variables. There is no default.

* **ydb_routines**: Tells a process where to find application code written in M. It is not required by applications that do not invoke M code. If it is not set, the :code:`yottadb` program sets it to :code:`$ydb_dist/libyottadbutil.so` where YottaDB's own utility routines (such as GDE) are available.

Your YottaDB distribution comes with many scripts that set up a default YottaDB environment for the shell of your choice. These scripts are as follows:

* **ydb_env_set** (gtmprofile): uses reasonable defaults to set up a YottaDB application development environment that includes plugins conforming to the YottaDB standard (such as the POSIX plugin and Octo). The ydb_env_set file when sourced sets default values for the environment variables ydb_dir, ydb_dist, ydb_etrap, ydb_gbldir, ydb_log, ydb_rel, ydb_repl_instance, ydb_retention, ydb_routines, ydb_tmp, gtmdir, gtm_dist, gtm_etrap, gtmgbldir, gtm_log, gtm_repl_instance, gtm_retention, gtmroutines, gtm_tmp, and gtmver. Additionally, if $ydb_chset is set to a case independent "UTF-8", sourcing the file also sets ydb_icu_version and gtm_icu_version. The default environment is created under $ydb_dir, defaulting to $HOME/.yottadb if ydb_dir is undefined. Sourcing ydb_env_set saves environment variables that it sets such that subsequently sourcing ydb_env_unset restores the saved values.

* **gtmcshrc**: sets up a default YottaDB environment for C-shell compatible shells. It sets up default values for ydb_dist, ydb_gbldir, ydb_chset and ydb_routines. It also creates aliases so you can execute YottaDB and its utilities without typing the full path.

* **gtmbase**: detects the shell type and adds :code:`ydb_env_set` to :code:`.profile` or :code:`gtmcshrc` to :code:`.cshrc` so the shell automatically sources :code:`ydb_env_set` or :code:`gtmcshrc` on a subsequent login operation. YottaDB does not recommend using :code:`gtmbase` as is - use it as an example for you to develop a suitable script for your systems. :code:`gtmbase` is deprecated, and not actively maintained or tested.

* **ydb** (gtm): starts YottaDB in direct mode on POSIX shells. The ydb script sources ydb_env_set. It also deletes prior generation journal and temporary files older than the number of days specified by the environment variable gtm_retention. It attempts to automatically recover the database when it runs and as such is suitable for "out of the box" usage of YottaDB. Although it will work for large multi-user environments, you may want to modify or replace it with more efficient scripting.

* **gdedefaults**: a GDE command file that specifies the default values for database characteristics defined by GDE.

These scripts are designed to give you a friendly out-of-the-box YottaDB experience. Even though you can set up an environment for normal YottaDB operation without using these scripts, it is important to go through these scripts to understand how to configure environments.


++++++++++++
ydb_env_set
++++++++++++

On POSIX shells, ydb_env_set helps you set an environment for single-user, non-replicated use of YottaDB.

ydb_env_set sets reasonable defaults for the following environment variables for normal YottaDB operation:

.. code-block:: none

   ydb_dir, ydb_dist, ydb_etrap, ydb_gbldir, ydb_log, ydb_rel, ydb_repl_instance, ydb_retention, ydb_routines, ydb_tmp, gtmdir, gtm_dist, gtm_etrap, gtmgbldir, gtm_log, gtm_repl_instance, gtm_retention, gtmroutines, gtm_tmp, gtmver

You can set the following environment variables before sourcing ydb_env_set or running the ydb script;

* **ydb_chset** - set this to "UTF-8" to run YottaDB in UTF-8 mode; it defaults to M mode. As UTF-8 mode requires a UTF-8 locale to be set in LC_CTYPE or LC_ALL, if a locale is not specified, ydb_env_set also attempts to set a UTF-8 locale. Since YottaDB in UTF-8 mode often requires ydb_icu_version to be set, if it is not set, ydb_env_set attempts to determine the ICU version on the system and set it. This requires the icu-config program to be installed and executable by ydb_env_set.

* **gtmdir** - set this to define a directory for the environment set by ydb_env_set.

The following shell variables are used by the script and left unset at its completion:

.. code-block:: none

   old_ydb_dist, old_ydb_routines, old_gtmver, tmp_ydb_tmp, tmp_passwd.

The $ydb_routines value set by the ydb_env_set script enables auto-relink by default for object files in the $gtmdir/$ydb_rel/o directory in M mode and $gtmdir/$ydb_rel/o/utf8 in UTF-8 mode. Auto-relink requires shared memory resources and limits beyond those for database operation. If your system has inadequate shared memory configured, YottaDB displays messages along the lines of:

.. code-block:: bash

   %YDB-E-SYSCALL, Error received from system call shmget() failed

Refer to your OS documentation to configure shared memory limits (for example, on common Linux systems, the kernel.shmmax parameter in /etc/sysctl.conf).

The ydb_env_set (and ydb) scripts are idempotent by design, so calling them repeatedly is safe. The YottaDB installation process ensures that ydb_env_set always sets ydb_dist correctly. Idempotency is implemented by checking the value of $ydb_dist and skipping all changes to environment variables if ydb_dist is already defined.

When ydb sources ydb_env_set, it provides a default execution environment (global directory and a default database (with BEFORE_IMAGE journaling)) if none exists. By default, it creates the database in $HOME/.yottadb with a structure like the following; note that this directory structure has different locations for YottaDB routines (r), object files (o), and database-related files (g):

.. code-block:: bash

   .yottadb
   |-- r
   |-- r1.10
   |   |-- g
   |   |   |-- yottadb.dat
   |   |   |-- yottadb.gld
   |   |   `-- yottadb.mjl
   |   |-- o
   |   |   `-- utf8
   |   `-- r
   `-- r1.20
       |-- g
       |   |-- yottadb.dat
       |   |-- yottadb.gld
       |   `-- yottadb.mjl
       |-- o
       |   `-- utf8
       `-- r


where r1.20 represents the current release and platform information and r1.10 represents a previously used YottaDB release.

On 64-bit platforms in M mode, ydb_env_set sets the environment variable ydb_routines to something like the following (where $ydb_dist and $ydb_rel are as discussed above):

.. code-block:: bash

   $gtmdir/$ydb_rel/o*($gtmdir/$ydb_rel/r $gtmdir/r) $ydb_dist/plugin/o($ydb_dist/plugin/r) $ydb_dist/libgtmutil.so $ydb_dist

$gtmdir/$ydb_rel/o*($gtmdir/$ydb_rel/r $gtmdir/r) specifies that YottaDB searches for routines in $gtmdir/$ydb_rel/r, then $gtmdir/r using $gtmdir/$ydb_rel/o for object code, then for routines in the plugin subdirectory of $ydb_dist, then in $ydb_dist, looking first for a shared library of routines distributed with YottaDB and then for other routines subsequently installed there. The * -suffix after the object directory enables the auto-relink facility.

For a comprehensive discussion of YottaDB source and object file management, refer to the `$ZROUTINES section in the Programmer's Guide <../ProgrammersGuide/isv.html#zroutines-isv>`_.

When $ydb_chset is set to UTF-8, ydb_env_set sets ydb_routines to something like this:

.. code-block:: bash

   $gtmdir/$ydb_rel/o/utf8*($gtmdir/$ydb_rel/r $gtmdir/r) $ydb_dist/plugin/o/utf8($ydb_dist/plugin/r) $ydb_dist/libgtmutil.so $ydb_dist

Note that ydb_env_set sets $ydb_dist in UTF-8 mode to the utf8 subdirectory of the YottaDB installation directory. If you have installed any plugins that include shared libraries, the ydb_env_set script includes those. For example, with the POSIX and ZLIB plugins installed on a 64-bit platform, gtmdir set to /home/jdoe1 and YottaDB installed in /opt/yottadb/r120, ydb_env_set would set ydb_routines to:

.. code-block:: bash

   /home/jdoe1/.yottadb/r1.20/o*(/home/jdoe1/.yottadb/r1.20/r /home/jdoe1/.yottadb/r) /usr/local/lib/yottadb/r120/plugin/o/_POSIX.so /usr/local/lib/yottadb/r120/plugin/o/_ZLIB.so /usr/local/lib/yottadb/r120/plugin/o(/usr/local/lib/yottadb/r120/plugin/r) /usr/local/lib/yottadb/r120/libgtmutil.so /usr/local/lib/yottadb/r120

.. note::
   This scenario of sourcing ydb_env_set is only for the sake of example. Consult your system administrator before implementing ydb_env_set for a multi-user environment.

ydb_env_set creates the following aliases:

.. code-block:: bash

   alias dse="$ydb_dist/dse"
   alias gde="$ydb_dist/yottadb -run GDE"
   alias ydb="$ydb_dist/ydb"
   alias lke="$ydb_dist/lke"
   alias mupip="$ydb_dist/mupip"

If /var/log/yottadb/$ydb_rel directory exists, ydb_env_set sets it as the value for $ydb_log. If ydb_env_set does not find /var/log/yottadb/$ydb_rel, it uses $ydb_tmp to set the value of $ydb_log.

Sourcing :code:`ydb_env_set` manages a three region database:

#. A DEFAULT region in which empty string (:code:`""`) subscripts are disabled. Except for global variables mapped to the YDBOCTO and YDBAIM regions, the properties of this region are unchanged.

#. A YDBOCTO region, intended to be used by the `Octo SQL plugin <https://gitlab.com/YottaDB/DBMS/YDBOcto/>`_ with the following properties:

    * Empty string subscripts are enabled.

    * :code:`^%ydbOcto*` global variables (with all combinations of capitalization of :code:`"Octo"`) are mapped to YDBOCTO.

    * The key size is 1019 bytes and the record size is 1MiB.

    * The default database filename is :code:`$ydb_dir/$ydb_rel/g/%ydbocto.dat` and the default journal file is :code:`$ydb_dir/$ydb_rel/g/%ydbocto.mjl`.

    * The block size is 2KiB, with an initial allocation of 10000 blocks, extended by 20000 blocks.

    * 2000 global buffers.

    Except for these differences, the properties of the YDBOCTO region are the same as those of the DEFAULT region.

#. A YDBAIM region, intended to be used by the `Application Independent Metadata plugin <https://gitlab.com/YottaDB/Util/YDBAIM>`_ with the following properties:

    * Empty string subscripts are enabled.

    * :code:`^%ydbAIM*` global variables (with all combinations of capitalization of :code:`"AIM"`) are mapped to YDBAIM.

    * The key size is 992 bytes and the record size is 1008 bytes.

    * The default database filename is :code:`$ydb_dir/$ydb_rel/g/%ydbaim.dat`. Journaling is not enabled by default, as Application Independent Metadata can be (re)created from application data at any time, on demand.

    * The block size for YDBAIM is 1KiB, with an initial allocation of 20000 blocks, extended by 40000 blocks.

    * The YDBAIM region uses the :ref:`MM access method <segment-access-method>`.

    * Sourcing :code:`ydb_env_set` does not create the database file. The YDBAIM region has :ref:`AutoDB <region-no-autodb>` set in the global directory and the first access to a global variable mapped to the YDBAIM region automatically creates the database file.

    * Sourcing :code:`ydb_env_set` when recovering from an unclean shutdown (such as when coming back up from a system crash) deletes the YDBAIM region, whereas it performs RECOVER / ROLLBACK BACKWARD on the DEFAULT and YDBOCTO regions. To make YDBAIM region recoverable, :ref:`change the access method to BG <set-access-method>`, and :ref:`enable and turn on before-image journaling <set-action-qualifiers>`.

Additionally:

- The default mode is UTF-8 if YottaDB was installed with UTF-8 support.

- For UTF-8 mode, sourcing :code:`ydb_env_set` checks whether a locale is set in the LC_ALL or LC_CTYPE environment variables. If not, it uses the first UTF-8 locale in the :code:`locale -a` output, and terminates with an error if one is not found.

- In case of error, the location of the error file is output.

Sourcing :code:`ydb_env_set` handles the case where replication is turned on.

:code:`ydb_env_set` was modified in YottaDB effective release `r1.32 <https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.32>`_, to create a three region database.

Sourcing :code:`ydb_env_set` respects existing values of :code:`ydb_gbldir` and :code:`ydb_routines`.
Some more features include:

* :code:`ydb_routines` automatically includes the plugins in the :code:`$ydb_dist/plugin` directory. Environment variables to call exported C routines in plugins are also created.
* In the event the instance is coming up after an unclean shutdown, it recovers the instance using the existing :code:`$ydb_gbldir` using MUPIP JOURNAL RECOVER or MUPIP JOURNAL ROLLBACK depending on the replication setting.
* Global directory files are automatically upgraded to the format of the current YottaDB release.

Sourcing :code:`ydb_env_set` sets :code:`$?`:

* 0 is normal exit
* 1 means that :code:`$ydb_dist` does not match the location of :code:`ydb_env_set`
* 2 means that :code:`$gtm_dist` does not match the location of :code:`ydb_env_set`
* 3 means that neither :code:`$ydb_dist` nor :code:`$gtm_dist` match the location of :code:`ydb_env_set`
* Other non-zero codes are as returned by :code:`set^%YDBENV`

:code:`ydb_env_set` was modified to set :code:`$status` in YottaDB effective release `r1.34 <https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.34>`_.

++++++++++
gtmcshrc
++++++++++

.. note::

   :code:`gtmcshrc` is deprecated and no longer maintained or tested.

.. note::

   Sets a default YottaDB environment for C type shell. It sets the $ydb_dist, $ydb_gbldir, $ydb_chset, $ydb_routines, and adds $ydb_dist to the system environment variable PATH.

To source the gtmcshrc script, type:

.. code-block:: bash

   $ source <path_to_YottaDB_installation_directory>/gtmcshrc

You can also run the gtmbase script which places the above command in the .cshrc file so the script will get automatically sourced the next time you log in.

gtmcshrc also creates the following aliases.

.. code-block:: bash

   alias ydb '$ydb_dist/yottadb -direct'
   alias mupip '$ydb_dist/mupip'
   alias lke '$ydb_dist/lke'
   alias gde '$ydb_dist/yottadb -r ^GDE'
   alias dse '$ydb_dist/dse'

Now you can run YottaDB and its utilities without specifying a full path to the directory in which YottaDB was installed.

++++++++++++++++
 gtmbase
++++++++++++++++

Adds the following line to the .profile or .cshrc file depending on the shell.

In the POSIX shell, gtmbase adds the following line to .profile:

.. code-block:: bash

   . <ydb_dist pathname>/ydb_env_set

In the C shell, adds the following line to .cshrc:

.. code-block:: bash

   source <ydb_dist pathname>/gtmcshrc

+++++++++++++
 gdedefaults
+++++++++++++

Specifies default or template values for database characteristics defined by GDE.

+++
ydb
+++

The ydb script starts with #!/bin/sh so it can run with any shell. Also, you can use it to both run a program and run in direct mode. It sources ydb_env_set and sets up default YottaDB database files with BEFORE_IMAGE journaling. It automatically recovers the database on startup. This script sets up everything you need to run YottaDB for a simple out-of-the-box experience.

For multi-user multi-environment systems, modify or replace the ydb script for your configuration.

The ydb script deletes all prior generation journal files (\*_<time and date stamp> files) older than $gtm_retention days from the directory that contains the global directory (as pointed to by $ydb_gbldir) and any subdirectories of that directory. By default, $gtm_retention is 42. However, you might want to align it with the backup frequency of your database.

Note that the removal of prior generation journal files is not specific to the database/journal files indicated by the current $ydb_gbldir but the directory from where you run the ydb script.

If you plan to use YottaDB in UTF-8 mode, set $ydb_chset to UTF-8 and LC_CTYPE to a UTF-8 locale and then run the ydb script.

If you intend to use Database Encryption, set the ydb_passwd and ydb_crypt_config environment variables first and then run the ydb script.

**To run the ydb script, type:**

.. code-block:: bash

   $ <path to your YottaDB Distribution>/ydb

**To invoke the help to assist first-time users, type:**

 .. code-block:: bash

    $ <path to your YottaDB Distribution>/ydb -help
     ydb -dir[ect] to enter direct mode (halt returns to shell)
    ydb -run <entryref> to start executing at an entryref
    ydb -help / ydb -h / ydb -? to display this text


.. _env-vars:

------------------------
 Environment Variables
------------------------

(Last updated: `r1.36 <https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.36>`_)

YottaDB supports both ydb_* environment variables and GT.M environment variables (referred to here as gtm*, though some are upper case). If the ydb* environment variable is not defined, but the gtm* environment variable is defined, the ydb* environment variable is also defined to have the same value as the gtm* environment variable the first time the gtm* environment variable is read. If the ydb* environment variable and the gtm* environment variable are both defined, the ydb* environment variable value takes precedence.

Environment variables of the form ydb_xc_<package> are used to point to the call-out tables for external calls, and the GT.M names of these variables are of the form GTMXC_<package>.

A comprehensive list of environment variables that are directly or indirectly used by YottaDB follows:

+++++++++
EDITOR
+++++++++
**EDITOR** is a standard system environment variable that specifies the full path to the editor to be invoked by YottaDB in response to the ZEDit command (defaults to vi, if $EDITOR is not set).

.. _lc-ctype-env-var:

+++++++++++
LC_CTYPE
+++++++++++
**LC_CTYPE** is a standard system environment variable used to specify a locale. When $ydb_chset has the value "UTF-8", $LC_CTYPE must specify a UTF-8 locale (e.g., "en_US.utf8").

+++++++++
LC_ALL
+++++++++
**LC_ALL** is a standard system environment variable used to select a locale with UTF-8 support. LC_ALL is an alternative to LC_TYPE, which overrides LC_TYPE and has a more pervasive effect on other aspects of the environment beyond YottaDB.

++++++++++++++++++
LD_LIBRARY_PATH
++++++++++++++++++
**LD_LIBRARY_PATH** is a standard system environment variable used to modify the default library search path. Use this extension when YottaDB relies on custom compiled libraries that do not reside in the default library search path. For example ICU, GPG, OpenSSL and/or zlib libraries.

+++++
TZ
+++++
**TZ** is a standard system environment variable that specifies the timezone to be used by a YottaDB process, if they are not to use the default system timezone. YottaDB uses the system clock for journal time stamps on the assumption it reflects UTC time.

++++++++++++++++++++
ydb_aio_nr_events
++++++++++++++++++++
**ydb_aio_nr_events (gtm_aio_nr_events)**: For Linux x86_64, the ydb_aio_nr_events environment variable controls the number of structures a process has per global directory to manage asynchronous writes, and therefore determines the number of concurrent writes a process can manage across all regions within a global directory. If not specified, the value controlled by ydb_aio_nr_events defaults to 128. If a process encounters a situation where it needs to perform an asynchronous write, but has no available slots with which to manage an additional one, it either falls back to synchronous writing if the write is blocking other actions, or defers the write until a slot becomes available as other writes complete. Linux allocates the structures on a system-wide basis with the setting of /proc/sys/fs/aio-max-nr. Therefore, you should configure this parameter to account for the needs (as determined by ydb_aio_nr_events or the default) of all processes using asynchronous I/O. When processes use multiple global directories with asynchronous I/O, their need for system resources increases accordingly. For example, if an environment runs 10,000 processes, each of which open two global directories and /proc/sys/fs/aio-max-nr is set to a value of 200,000 then ydb_aio_nr_events needs to be set to a value <= 200,000 / (10,000 * 2) = 10. Conversely if ydb_aio_nr_events is set to a value of 20, then aio-max-nr needs to be bumped up to (10,000 * 2 * 20) = 400,000. YottaDB captures the number of errors encountered when attempting to write database blocks for a region, and, barring problems with the storage subsystem, hitting an asynchronous write limit would constitute a primary (probably only) contribution to that value, which you can access with $$^%PEEKBYNAME("sgmnt_data.wcs_wterror_invoked_cntr",<region>)

++++++++++++++++++++++++
ydb_autorelink_ctlmax
++++++++++++++++++++++++
**ydb_autorelink_ctlmax (gtm_autorelink_ctlmax)** specifies the maximum number of entries for unique routine names in the relink control file created by a process for any directory, with a minimum of 1,000, a maximum of 16,000,000 and a default of 50,000 if unspecified. If a specified value is above or below the allowed range, the process logs the errors ARCTLMAXHIGH or ARCTLMAXLOW respectively in the syslog, and uses the nearest acceptable limit instead. MUPIP RCTLDUMP and ZSHOW "A" outputs include the maximum number of unique routine names available in a relink control file.

+++++++++++++++++++++++++
ydb_autorelink_keeprtn
+++++++++++++++++++++++++
**ydb_autorelink_keeprtn (gtm_autorelink_keeprtn)**: When ydb_autorelink_keeprtn is set to 1, t[rue], or y[es] , exiting processes leave auto-relinked routines in shared memory. When the environment variable ydb_autorelink_keeprtn is undefined, 0, f[alse] or n[o], exiting processes purge auto-relinked routines in shared memory if no other processes are using them. Regardless of the value of ydb_autorelink_keeprtn, the Operating System removes an auto-relink shared memory repository when there are no processes accessing it.

All values are case-independent. When ydb_autorelink_keeprtn is defined and TRUE:

* Processes do less work on exiting, with some performance gain - faster process termination - likely only observable when a large number of processes exit concurrently.

* In a production environment, an application that frequently invokes YottaDB routines in short running processes (such as YottaDB routines invoked by web servers using interfaces such as CGI) may give better performance when setting ydb_autorelink_keeprtn or using at least one long running auto-relink process that remains attached to the shared memory to keep routines available in shared memory for use when short running processes need them.

+++++++++++++++++++++
ydb_autorelink_shm
+++++++++++++++++++++
**ydb_autorelink_shm (gtm_autorelink_shm)** specifies the size (in MiB) of an initial Rtnobj shared memory segment used by the auto-relink facility. If the value of ydb_autorelink_shm is not a power of two, YottaDB rounds the value to the next higher integer power of two. If the first object (.o) file does not fit in a new Rtnobj segment, YottaDB rounds the allocation up to the smallest integer power of two required to make it fit. When YottaDB needs room for object files and existing Rtnobj segments have insufficient free space, it creates an additional shared memory segment, double the size of the last. Note that when hugepages are enabled, the actual Rtnobj shared memory size might be more than that requested implicitly or explicitly through $ydb_autorelink_shm.

++++++++++++++
ydb_badchar
++++++++++++++
**ydb_badchar (gtm_badchar)** specifies the initial setting that determines whether YottaDB should raise an error when it encounters an illegal UTF-8 character sequence. This setting can be changed with a VIEW "[NO]BADCHAR" command, and is ignored for I/O processing and in M mode.

++++++++++++++++
ydb_baktmpdir
++++++++++++++++
**ydb_baktmpdir (gtm_baktmpdir)** specifies the directory where MUPIP BACKUP creates temporary files. If $ydb_baktmpdir is not defined, YottaDB uses the deprecated $GTM_BAKTMPDIR environment variable if defined, and otherwise the temporary files are created as follows:

  * MUPIP BACKUP DATABASE uses the directory of the backup destination for creating temporary files.

  * MUPIP BACKUP BYTESTREAM uses /tmp

All processes performing updates during an online IBACKUP must use the same directory and have write access to it.

.. _ydb-boolean-env-var:

++++++++++++++
ydb_boolean
++++++++++++++
**ydb_boolean (gtm_boolean)** specifies the initial setting that determines how YottaDB compiles Boolean expression evaluation (expressions evaluated as a logical TRUE or FALSE). If ydb_boolean is undefined or evaluates to an integer zero (0), YottaDB behaves as it would after a VIEW "NOFULL_BOOLEAN" and compiles such that it stops evaluating a Boolean expression as soon as it establishes a definitive result . Note that:

*  :ref:`ydb-side-effects-env-var` has an analogous impact on function argument evaluation order and implies "FULLBOOLEAN" compilation, so VIEW "NOFULLBOOLEAN" produces an error when :ref:`ydb-side-effects-env-var` is on.

* If ydb_boolean evaluates to an integer one (1), YottaDB enables VIEW "FULL_BOOLEAN" compilation, which means that YottaDB ensures that within a Boolean expression, all side effect expression atoms, extrinsic functions ($$), external functions ($&), and $INCREMENT() execute in left-to-right order.

* If ydb_boolean evaluates to an integer two (2), YottaDB enables VIEW "FULL_BOOLWARN" behavior, which means that YottaDB not only evaluates Boolean expressions like "FULL_BOOLEAN" but produces a BOOLSIDEFFECT warning when it encounters Boolean expressions that may induce side-effects; that is: expressions with side effects after the first Boolean operator - extrinsic functions, external calls, and $INCREMENT().

* Boolean expressions without side effects will continue to be short-circuited whether or not ydb_boolean is 1 or 0. Error messages that could result if an expression were fully evaluated may not occur even with this setting enabled.

.. _ydb-chset-env-var:

++++++++++++
ydb_chset
++++++++++++
**ydb_chset (gtm_chset)** determines the mode in which YottaDB compiles and operates. If it has a value of "UTF-8", YottaDB assumes that strings are encoded in UTF-8. In response to a value of "M" (or indeed anything other than "UTF-8"), YottaDB treats all 256 combinations of the 8 bits in a byte as a single character.

+++++++++
ydb_ci
+++++++++
**ydb_ci (GTMCI)** specifies the call-in table for function calls from C code to M code.

++++++++++++++++++++
ydb_cm_<node-name>
++++++++++++++++++++
**ydb_cm_<node-name>** is used by a GT.CM client process to locate the GT.CM server. <node-name> is an alphanumeric, which is used as a prefix for the GT.CM database segment file by the Global Directory of the client process. For detailed usage refer to the :ref:`GT.CM Client <gt-cm-client>` section.

++++++++++++++++
ydb_collate_n
++++++++++++++++
**ydb_collate_n (gtm_collate_n)** specifies the shared library holding an alternative sequencing routine when using non-M standard (ASCII) collation. The syntax is ydb_collate_n=pathname where n is an integer from 1 to 255 that identifies the collation sequence, and pathname identifies the shared library containing the routines for that collation sequence.

++++++++++++++
ydb_compile
++++++++++++++
**ydb_compile (gtmcompile)** specifies the initial value of the $ZCOmpile ISV. The SET command can alter the value of $ZCOMPILE in an active process.

++++++++++++++++++++++
ydb_coredump_filter
++++++++++++++++++++++
**ydb_coredump_filter (gtm_coredump_filter)** contains case-insensitive hexadecimal digits that sets the corresponding value to /proc/<pid>/coredump_filter (see :code:`man 5 core`) at process startup without explicitly setting a value if unspecified. This controls the contents of core dumps generated by the process. If ydb_coredump_filter is not specified, but gtm_coredump_filter is, the latter environment variable is used. If both are specified, the former takes precedence.

.. note::
   Setting :code:`ydb_coredump_filter` to -1 disables writing to :code:`/proc/<pid>/coredump_filter`

+++++++++++++++++++
ydb_crypt_config
+++++++++++++++++++
**ydb_crypt_config (gtmcrypt_config)** specifies the location of the configuration file required for database encryption, Sequential file, PIPE, and FIFO device encryption and/or TLS support. A configuration file is divided into two sections: the database encryption section and the TLS section. The database encryption section contains a list of database files and their corresponding key files. You do not need to add a database encryption section if you are not using an encrypted database, or a TLS section if you are not using TLS for replication or sockets. The TLS section provides information needed for OpenSSL (in the reference plugin implementation) or other encryption package, such as the location of the root certification authority certificate in PEM format and leaf-level certificates with their corresponding private key files. Note that the use of the ydb_crypt_config environment variable requires prior installation of the libconfig package.

+++++++++++++++++
ydb_crypt_fips
+++++++++++++++++
**ydb_crypt_fips (gtmcrypt_FIPS)** specifies whether the plugin reference implementation should attempt to use either OpenSSL or Libgcrypt to provide database encryption that complies with FIPS 140-2. When the environment variable $ydb_crypt_FIPS is set to 1 (or evaluates to a non-zero integer or any case-independent string or leading substring of "TRUE" or "YES"), the plugin reference implementation attempts to use libgcrypt (from GnuPG) and libcrypto (OpenSSL) in "FIPS mode." Note that to comply with FIPS 140-2 you should be knowledgeable with that standard and take many steps beyond setting this environment variable. By default YottaDB does not enforce "FIPS mode.

+++++++++++++++++++
ydb_crypt_plugin
+++++++++++++++++++
**ydb_crypt_plugin (gtm_crypt_plugin)**: If the environment variable ydb_crypt_plugin is defined and provides the path to a shared library relative to $ydb_dist/plugin, YottaDB uses $ydb_dist/plugin/$ydb_crypt_plugin as the shared library providing the plugin. If $ydb_crypt_plugin is not defined, YottaDB expects $ydb_dist/plugin/libgtmcrypt.so to be a symbolic link to a shared library providing the plugin. The expected name of the actual shared library is libgtmcrypt_cryptlib_CIPHER.so (depending on your platform, the actual extension may differ from .so), for example, libgtmcrypt_openssl_AESCFB. YottaDB cannot and does not ensure that the cipher is actually AES CFB as implemented by OpenSSL.

++++++++++++++++++++
ydb_custom_errors
++++++++++++++++++++
**ydb_custom_errors (gtm_custom_errors)** specifies the complete path to the file that contains a list of errors that should automatically stop all updates on those region(s) of an instance which have the Instance Freeze mechanism enabled.

+++++++++++++++++++
ydb_cur_gbldir
+++++++++++++++++++
**ydb_cur_gbldir** specifies the current value of $ZGBLDIR set by the parent process using SET $ZGBLDIR. If ydb_cur_gbldir is not set it means the parent has not set a value to ZGBLDIR, and is using the value set from the environment at process startup.

ydb_cur_gbldir was added to YottaDB effective release `r1.36 <https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.36>`_.

++++++++++++++++++++++++++++++
ydb_dbfilext_syslog_disable
++++++++++++++++++++++++++++++
**ydb_dbfilext_syslog_disable (gtm_dbfilext_syslog_disable)** Controls whether database file extensions are logged in the syslog or not. If the environment variable is set to a non-zero numeric value or case-independent string or leading substrings of TRUE or YES, database file extensions are not logged to the syslog.

+++++++++++++
ydb_dbglvl
+++++++++++++
**ydb_dbglvl (gtmdbglvl)** specifies the YottaDB debug levels. The defined values can be added together to turn on multiple features at the same time. Note that the cumulative value specified in the logical or environment variable must currently be specified in decimal.

.. note::
   Use of ydb_dbglvl is intended for debugging under the guidance of your YottaDB support channel. If you set ydb_dbglvl to a non-zero value, be aware that there will be a performance impact. We do not recommend its use in production.

+------------------------------+--------------------------------------------+--------------------------------------------------------------------------------------------+
| Level                        | Value                                      | Notes                                                                                      |
+==============================+============================================+============================================================================================+
| GDL_None                     | 0x00000000                                 | No debugging                                                                               |
+------------------------------+--------------------------------------------+--------------------------------------------------------------------------------------------+
| GDL_Simple                   | 0x00000001                                 | Regular assert checking, no special checks                                                 |
+------------------------------+--------------------------------------------+--------------------------------------------------------------------------------------------+
| GDL_SmStats                  | 0x00000002                                 | Print usage statistics at end of process                                                   |
+------------------------------+--------------------------------------------+--------------------------------------------------------------------------------------------+
| GDL_SmTrace                  | 0x00000004                                 | Trace each malloc/free (output to stderr)                                                  |
+------------------------------+--------------------------------------------+--------------------------------------------------------------------------------------------+
| GDL_SmDumpTrace              | 0x00000008                                 | Dump malloc/free trace information on exit                                                 |
+------------------------------+--------------------------------------------+--------------------------------------------------------------------------------------------+
| GDL_SmAllocVerf              | 0x00000010                                 | Perform verification of allocated storage chain for each call                              |
+------------------------------+--------------------------------------------+--------------------------------------------------------------------------------------------+
| GDL_SmFreeVerf               | 0x00000020                                 | Perform simple verification of free storage chain for each call                            |
+------------------------------+--------------------------------------------+--------------------------------------------------------------------------------------------+
| GDL_SmBackfill               | 0x00000040                                 | Backfill unused storage (cause exceptions if released storage is used)                     |
+------------------------------+--------------------------------------------+--------------------------------------------------------------------------------------------+
| GDL_SmChkAllocBackfill       | 0x00000080                                 | Verify backfilled storage in GDL_AllocVerf while verifying each individual queue entry     |
+------------------------------+--------------------------------------------+--------------------------------------------------------------------------------------------+
| GDL_SmChkFreeBackfill        | 0x00000100                                 | Verify backfilled storage in GDL_FreeVerf while verifying each individual queue entry      |
+------------------------------+--------------------------------------------+--------------------------------------------------------------------------------------------+
| GDL_SmStorHog                | 0x00000200                                 | Each piece of storage allocated is allocated in an element twice the desired size to       |
|                              |                                            | provide glorious amounts of backfill for overrun checking.                                 |
+------------------------------+--------------------------------------------+--------------------------------------------------------------------------------------------+
| GDL_DumpOnStackOFlow         | 0x00000400                                 | When get a stack overflow or out-of-memory error, generate a core                          |
+------------------------------+--------------------------------------------+--------------------------------------------------------------------------------------------+
| GDL_ZSHOWDumpOnSignal        | 0x00000800                                 | Don't supress YDB_FATAL file creation when get a signal                                    |
+------------------------------+--------------------------------------------+--------------------------------------------------------------------------------------------+
| GDL_PrintIndCacheStats       | 0x00001000                                 | Print indirect cacheing stats                                                              |
+------------------------------+--------------------------------------------+--------------------------------------------------------------------------------------------+
| GDL_PrintCacheStats          | 0x00002000                                 | Print stats on $Piece and UTF8 cacheing (debug only)                                       |
+------------------------------+--------------------------------------------+--------------------------------------------------------------------------------------------+
| GDL_DebugCompiler            | 0x00004000                                 | Turn on compiler debugging                                                                 |
+------------------------------+--------------------------------------------+--------------------------------------------------------------------------------------------+
| GDL_SmDump                   | 0x00008000                                 | Do full blown storage dump -- only useful in debug mode                                    |
+------------------------------+--------------------------------------------+--------------------------------------------------------------------------------------------+
| GDL_PrintEntryPoints         | 0x00010000                                 | Print address of entry points when they are loaded/resolved                                |
+------------------------------+--------------------------------------------+--------------------------------------------------------------------------------------------+
| GDL_PrintSockIntStats        | 0x00020000                                 | Print Socket interrupt stats on exit                                                       |
+------------------------------+--------------------------------------------+--------------------------------------------------------------------------------------------+
| GDL_SmInitAlloc              | 0x00040000                                 | Initialize all storage allocated or deallocated with 0xdeadbeef                            |
+------------------------------+--------------------------------------------+--------------------------------------------------------------------------------------------+
| GDL_PrintPipeIntStats        | 0x00080000                                 | Print Pipe/Fifo(rm) interrupt stats on exit                                                |
+------------------------------+--------------------------------------------+--------------------------------------------------------------------------------------------+
| GDL_IgnoreAvailSpace         | 0x00100000                                 | Allow gdsfilext/mu_cre_file (UNIX) to ignore available space                               |
+------------------------------+--------------------------------------------+--------------------------------------------------------------------------------------------+
| GDL_PrintPMAPStats           | 0x00200000                                 | Print process memory map on exit (using pmap or procmap utility)                           |
+------------------------------+--------------------------------------------+--------------------------------------------------------------------------------------------+
| GDL_AllowLargeMemcpy         | 0x00400000                                 | Bypass the 1GB sanity check in gtm_memcpy_validate_and_execute()                           |
+------------------------------+--------------------------------------------+--------------------------------------------------------------------------------------------+
| define GDL_UseSystemMalloc   | 0x80000000                                 | Use the system's malloc(), disabling all the above GDL_Sm options                          |
+------------------------------+--------------------------------------------+--------------------------------------------------------------------------------------------+

++++++++++++++++++++++++++
ydb_db_startup_max_wait
++++++++++++++++++++++++++
**ydb_db_startup_max_wait (gtm_db_startup_max_wait)** specifies how long to wait for a resolution of any resource conflict when they first access a database file. YottaDB uses semaphores maintained using UNIX Inter-Process Communication (IPC) services to ensure orderly initialization and shutdown of database files and associated shared memory. Normally, the IPC resources are held in an exclusive state only for very brief intervals. However, under unusual circumstances that might include extremely large numbers of simultaneous database initializations, a long-running MUPIP operation involving standalone access (like INTEG -FILE or RESTORE), an OS overload or an unpredicted process failure, the resources might remain unavailable for an unanticipated length of time. $ydb_db_startup_max_wait specifies how long to wait for the resources to become available:

* -1 - Indefinite wait until the resource becomes available; the waiting process uses the :ref:`ydb-procstuckexec-env-var` mechanism at approximately 48 and 96 seconds.

* 0 - No wait - if the resource is not immediately available, give a DBFILERR error with an associated SEMWT2LONG

* > 0 - Seconds to wait - rounded to the nearest multiple of eight (8); if the specification is 96 or more seconds, the waiting process uses the :ref:`ydb-procstuckexec-env-var` mechanism at one half the wait and at the end of the wait; if the resource remains unavailable, the process issues DBFILERR error with an associated SEMWT2LONG

+++++++++++
ydb_dist
+++++++++++
**ydb_dist (gtm_dist)** specifies the path to the directory containing the YottaDB system distribution. ydb_dist must be defined for each user. If you are not using the ydb script or sourcing ydb_env_set, consider defining ydb_dist in the login file or as part of the default system environment. In UTF-8 mode, the ydb_dist environment variable specifies the path to the directory containing the YottaDB system distribution for Unicode. The distribution for Unicode is located in subdirectory utf8 under the YottaDB distribution directory. For example, if the YottaDB distribution is in /usr/local/lib/yottadb/r120, set ydb_dist to point to /usr/local/lib/yottadb/r120/utf8 for UTF-8 mode. Correct operation of YottaDB executable programs requires ydb_dist to be set correctly. Effective release `r1.30 <https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.30>`_, at process initialization YottaDB ensures that gtm_dist is set to $ydb_dist.

+++++++++++++
ydb_dmterm
+++++++++++++
**ydb_dmterm (gtm_dmterm)** specifies a [NO]DMTERM state at process initiation where application setting applied to $PRINCIPAL also apply to direct mode interactions; a case-insensitive value of "1", "yes", or "true" establishes a DMTERM state at process initiation where direct mode uses default terminal characteristics and ignores application settings for $PRINCIPAL; all other values, including no value, result in the default VIEW "NODMTERM" behavior.

++++++++++++++++
ydb_dollartest
++++++++++++++++

**ydb_dollartest** provides an initial value for $TEST. When ydb_dollartest is set to 0 or 1, the value of $TEST will be set to 0 or 1 respectively. If ydb_dollartest is undefined then the value of $TEST will be set to 1.

.. _ydb-env-translate-env-var:

++++++++++++++++++++
ydb_env_translate
++++++++++++++++++++
**ydb_env_translate (gtm_env_translate)** specifies the path to a shared library to implement the optional YottaDB `environment translation facility <../ProgrammersGuide/langfeat.html#opt-ydb-env-xltn-fac>`_ to aid application portability across platforms by translating strings into global directory references.

+++++++++++++++++++++++++++++
ydb_error_on_jnl_file_lost
+++++++++++++++++++++++++++++
**ydb_error_on_jnl_file_lost (gtm_error_on_jnl_file_lost)** causes a runtime error when set to 1 in case of problems with journaling (disk space issues etc.). Setting this environment variable to 0 (or having it undefined) is the default behavior which is to turn off journaling in case of problems.

++++++++++++
ydb_etrap
++++++++++++
**ydb_etrap (gtm_etrap)** specifies an initial value of $ETRAP to override the default value of "B" for $ZTRAP as the base level error handler. The ydb_env_set script sets ydb_etrap to "Write:(0=$STACK) ""Error occurred: "",$ZStatus,!" which you can customize to suit your needs.

.. _ydb-extract-nocol:

++++++++++++++++++++
ydb_extract_nocol
++++++++++++++++++++
**ydb_extract_nocol (gtm_extract_nocol)** specifies whether a MUPIP JOURNAL -EXTRACT (when used without -RECOVER or -ROLLBACK) on a database with custom collation should use the default collation if it is not able to read the database file. In a situation where the database file is inaccessible or the replication instance is frozen with a critical section required for the access held by another process and the environment variable ydb_extract_nocol is defined and evaluates to a non-zero integer or any case-independent string or leading substrings of "TRUE" or "YES", MUPIP JOURNAL -EXTRACT issues the DBCOLLREQ warning and proceeds with the extract using the default collation. If ydb_extract_nocol is not set or evaluates to a value other than a positive integer or any case-independent string or leading substrings of "FALSE" or "NO", MUPIP JOURNAL -EXTRACT exits with the SETEXTRENV error.

.. note::
    If default collation is used for a database with custom collation, the subscripts reported by MUPIP JOURNAL -EXTRACT are those stored in the database, which may differ from those used by application logic.

++++++++++++++++++++++
ydb_flushoncallout
++++++++++++++++++++++
**ydb_flushoncallout** specifies whether the process should startup with `VIEW FLUSHONCALLOUT <../ProgrammersGuide/commands.html#view-flushoncallout>`_. If set to a non-zero numeric value. "yes" or "TRUE" (case-insensitive), or a leading substring thereof, causes the process to startup with VIEW FLUSHONCALLOUT. Any other value, or no value causes the process to startup with VIEW NOFLUSHONCALLOUT.

ydb_flushoncallout was added to YottaDB effective release `r1.36 <https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.36>`_.

++++++++++++++++++++++
ydb_fullblockwrites
++++++++++++++++++++++
**ydb_fullblockwrites (gtm_fullblockwrites)** specifies whether a YottaDB process should write a full filesystem, or full database block, worth of bytes when writing a database block that is not full. Depending on your IO subsystem, writing a full block worth of bytes (even when there are unused garbage bytes at the end) may result in better database IO performance by replacing a low level read-modify-read IO operation with a single write operation.

ydb_fullblockwrites is deprecated in YottaDB effective release r1.36 and no longer maintained or tested.

+++++++++++++
ydb_gbldir
+++++++++++++
**ydb_gbldir (gtmgbldir)** specifies the initial value of the $ZGBLDIR ISV. $ZGBLDIR identifies the global directory. A global directory maps global variables to physical database files, and is required to access M global variables. Users who maintain multiple global directories use this environment variable to conveniently choose one to use from the time of process startup. To automate this definition, define ydb_gbldir in the user's login file. The SET command can alter the value of $ZGBLDIR in an active process.

+++++++++++++++++++++++
ydb_gbldir_translate
+++++++++++++++++++++++
**ydb_gbldir_translate** provides the path to a shared library to allow a set of $ZGBLDIR to be transformed for application portability across platforms, using the optional YottaDB `global directory translation facility <../ProgrammersGuide/langfeat.html#opt-ydb-gbldir-xltn-fac>`_. This is similar to the the optional YottaDB environment translation facility provided by :ref:`ydb-env-translate-env-var` above. ydb_gbldir_translate was added effective release `r1.30 <https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.30>`_.

++++++++++++++
ydb_gdscert
++++++++++++++
**ydb_gdscert (gtm_gdscert)** specifies the initial setting that controls whether YottaDB processes should test updated database blocks for structural damage. If it is defined, and evaluates to a non-zero integer or any case-independent string or leading substrings of "TRUE" or "YES", YottaDB performs a block-level integrity check on every block as a process commits it. Within a running process, VIEW "GDSCERT":value controls this setting. By default, YottaDB does not check database blocks for structural damage, because the impact on performance is usually unwarranted.

+++++++++++++++
ydb_hupenable
+++++++++++++++
**ydb_hupenable (gtm_hupenable)** specifies the initial value that determines whether a YottaDB process should recognize a disconnect signal from a PRINCIPAL device that is a terminal. If it is defined, and evaluates to a non-zero integer or any case-independent string or leading substrings of "TRUE" or "YES", the process receives a TERMHANGUP error if the OS signals that the terminal assigned to the process as the PRINCIPAL device has disconnected. Within a running process, USE $PRINCIPAL:[NO]HUP[ENABLE] controls this behavior. By default, YottaDB ignores such a signal, but a process that ignores the signal may subsequently receive an IOEOF or a TERMWRITE error from an attempt to respectively READ from, or WRITE to the missing device. YottaDB terminates a process that ignores more than one of these messages and, if the process is not in Direct Mode, sends a NOPRINCIO message to the operator log.

ydb_hupenable was added to YottaDB effective release `r1.34 <https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.34>`_.

++++++++++++++++++
ydb_icu_version
++++++++++++++++++
**ydb_icu_version (gtm_icu_version)** specifies the MAJOR VERSION and MINOR VERSION numbers of the desired ICU. For example "3.6" denotes ICU-3.6. If $ydb_chset has the value "UTF-8", YottaDB requires libicu with version 3.6 or higher. If you must chose between multiple versions of libicu or if libicu has been compiled with symbol renaming enabled, YottaDB requires ydb_icu_version to be explicitly set. Please see the section on :ref:`config-op-ydb-unicode` for more information.

++++++++++++++++
ydb_ipv4_only
++++++++++++++++
**ydb_ipv4_only (gtm_ipv4_only)** specifies whether a Source Server should establish only IPv4 connections with a Receiver Server or sockets associated with a SOCKET device. If it is defined, and evaluates to a non-zero integer, or any case-independent string or leading substring of "TRUE" or "YES", the Source Server establishes only IPv4 connections with the Receiver Server. ydb_ipv4_only is useful for environments where different server names are not used for IPv4 and IPv6 addresses.

++++++++++++++++++++++++++
ydb_jnl_release_timeout
++++++++++++++++++++++++++
**ydb_jnl_release_timeout (gtm_jnl_release_timeout)** specifies the number of seconds that a replicating Source Server waits when there is no activity on an open journal file before closing it. The default wait period is 300 seconds (5 minutes). If $ydb_jnl_release_timeout specifies 0, the Source Server keeps the current journal files open until shutdown. The maximum value for $ydb_jnl_release_timeout is 2147483 seconds.

+++++++++++++++
ydb_keep_obj
+++++++++++++++
**ydb_keep_obj (gtm_keep_obj)** specifies whether the ydbinstall script should delete the object files from the YottaDB installation directory. If ydb_keep_obj is set to "Y", the ydbinstall script leaves object files; by default, ydbinstall deletes object files after archiving them in a shared library.

++++++++++++++++++
ydb_lct_stdnull
++++++++++++++++++
**ydb_lct_stdnull (gtm_lct_stdnull)** specifies whether a YottaDB process should use standard collation for local variables with null subscripts or `historical null collation <../ProgrammersGuide/langfeat.html#null-subs-colltn>`_.

+++++++++++
ydb_link
+++++++++++
**ydb_link (gtm_link)** specifies the initial setting that determines whether YottaDB permits multiple versions of the same routine to be active at different stack levels of the M virtual machine. The VIEW "LINK":"[NO]RECURSIVE" command modifies this in an active process. If ydb_link is set to "RECURSIVE", auto-relink and explicit ZLINK commands links a newer object even when a routine with the same name is active and available in the current stack. When a process links a routine with the same name as an existing routine, future calls use the new routine. Prior versions of that routine referenced by the stack remain tied to the stack until they QUIT, at which point they become inaccessible. This provides a mechanism to patch long-running processes. If ydb_link is undefined or set to NORECURSIVE, or any value other than "RECURSIVE", auto-zlink defers replacing older routines until they no longer have an invoking use by the process and a ZLINK command produces a LOADRUNNING error when it attempts to relink an active routine on the YottaDB invocation stack.

+++++++++++++++++
ydb_linktmpdir
+++++++++++++++++
**ydb_linktmpdir (gtm_linktmpdir)** identifies a directory (defaulting to $ydb_tmp, which in turn defaults to /tmp, if unspecified) where YottaDB creates a small control file (Relinkctl), for each auto-relink enabled directory which a YottaDB process accesses while searching through $ZROUTINES. The names of these files are of the form ydb-relinkctl-<murmur> where <murmur> is a hash of the realpath() to an auto-relink directory; for example: /tmp/ydb-relinkctl-f0938d18ab001a7ef09c2bfba946f002). With each Relinkctl file, YottaDB creates and associates a block of shared memory that contains associated control structures. Among the structures is a cycle number corresponding to each routine found in the routine directory; a change in the cycle number informs a process that it may need to determine whether there is a new version of a routine. Although YottaDB only creates relinkctl records for routines that actually exist on disk, it may increment cycle numbers for existing relinkctl records even if they no longer exist on disk.

+++++++++++++
ydb_locale
+++++++++++++
**ydb_locale (gtm_locale)** specifies a locale to use (:ref:`lc-ctype-env-var` would be set to this value) if the :ref:`ydb-chset-env-var` environment variable is set to UTF-8. If not set, the current value of :ref:`lc-ctype-env-var` is used.  This environment variable is ignored if :ref:`ydb-chset-env-var` is not set to UTF-8.

++++++++++++++++++++
ydb_local_collate
++++++++++++++++++++
**ydb_local_collate (gtm_local_collate)** specifies an alternative collation sequence for local variables.

++++++++++
ydb_log
++++++++++
**ydb_log (gtm_log)** specifies a directory where the gtm_secshr_log file is stored. The gtm_secshr_log file stores information gathered in the gtmsecshr process. YottaDB recommends that a system-wide default be established for ydb_log so that gtmsecshr always logs its information in the same directory, regardless of which user's YottaDB process invokes gtmsecshr. In conformance with the Filesystem Hierarchy Standard, YottaDB recommends /var/log/yottadb/$ydb_rel as the value for $ydb_log unless you are installing the same version of YottaDB in multiple directories. Note that $ydb_rel can be in the form of the current YottaDB release and platform. If you do not set $ydb_log, YottaDB creates log files in a directory in /tmp. However, this is not recommended because it makes YottaDB log files vulnerable to the retention policy of a temporary directory.

.. note::
   In the latest versions, gtmsecshr logs its messages in the system log and the environment variable ydb_log is ignored.

+++++++++++++++++
ydb_lvnullsubs
+++++++++++++++++
**ydb_lvnullsubs (gtm_lvnullsubs)** specifies the initialization of [NEVER][NO]LVNULLSUBS at process startup. The value of the environment variable can be 0 which is equivalent to VIEW “NOLVNULLSUBS”, 1 (the default) which is equivalent to VIEW “LVNULLSUBS” or 2, which is equivalent to VIEW “NEVERLVNULLSUBS”. These settings disallow, partially disallow, or allow local arrays to have empty string subscripts.

++++++++++++++++
ydb_maxtptime
++++++++++++++++
**ydb_maxtptime (gtm_zmaxtptime)** specifies the initial value of the $ZMAXTPTIME Intrinsic Special Variable, which controls whether and when YottaDB issues a TPTIMEOUT error for a TP transaction that runs too long. ydb_maxtptime specifies time in seconds and the default is 0, which indicates "no timeout" (unlimited time). The maximum value of ydb_maxtptime is 60 seconds and the minimum is 0; YottaDB ignores ydb_maxtptime if it contains a value outside of this recognized range. This range check does not apply to SET $ZMAXTPTIME.

+++++++++++++++++++++++++++++++++++++++++++++++++++
ydb_max_indrcache_count/ydb_max_indrcache_memory
+++++++++++++++++++++++++++++++++++++++++++++++++++
**ydb_max_indrcache_count (gtm_max_indrcache_count)** and **ydb_max_indrcache_memory (gtm_max_indrcache_memory)** control the cache of compiled code for indirection/execute. ydb_max_indrcache_count is the maximum number of entries in the cache (defaulting to 128) and ydb_max_indrcache_memory is maximum memory (in KiB, defaulting to 128). When the number of cache entries exceeds $ydb_max_indrcache_count, or the memory exceeds $ydb_max_indrcache_memory KiB, YottaDB discards the entire cache and starts over.

++++++++++++++++++
ydb_max_sockets
++++++++++++++++++
**ydb_max_sockets (gtm_max_sockets)** specifies the maximum number of client connections for socket devices. The default is 64. While it must be large enough to accommodate the actual need, each reservation requires some memory in socket structures, so setting this number unnecessarily high causes requires a bit of additional memory for no benefit.

++++++++++++++++++++
ydb_max_storalloc
++++++++++++++++++++
**ydb_max_storalloc (gtm_max_storalloc)** limits the amount of memory (units in bytes) a YottaDB process is allowed to allocate before issuing a MEMORY (and MALLOCMAXUNIX) error. This helps in tracking memory allocation issues in the application.

+++++++++++++++++++++
ydb_memory_reserve
+++++++++++++++++++++
**ydb_memory_reserve (gtm_memory_reserve)** specifies the size in kilobytes of the reserve memory that YottaDB should use in handling and reporting an out-of-memory condition. The default is 64 (KiB). Setting this too low can impede investigations of memory issues, but YottaDB only uses this reserve when a process runs out of memory so it almost never requires actual memory, only address space.

++++++++++++++++
ydb_msgprefix
++++++++++++++++
**ydb_msgprefix** specifies a prefix for YottaDB messages generated by a process, with the prefix defaulting to "YDB", e.g., YDB-I-DBFILEXT. Previously, the prefix was always "GTM". A value of "GTM" retains the previous format.

++++++++++++++++++
ydb_mstack_crit
++++++++++++++++++
**ydb_mstack_crit (gtm_mstack_crit)** specifies an integer between 15 and 95 defining the percentage of the stack which should be used before YottaDB emits a STACKCRIT warning. If the value is below the minimum or above the maximum, YottaDB uses the minimum or maximum respectively. The default is 90.

++++++++++++++++++
ydb_mstack_size
++++++++++++++++++
**ydb_mstack_size (gtm_stack_size)** specifies the M stack size (in KiB). If ydb_mstack_size is not set or set to 0, YottaDB uses the default M stack size (that is, 272KiB). The minimum supported size is 25 KiB; YottaDB reverts values smaller than this to 25 KiB. The maximum supported size is 10000 KiB; YottaDB reverts values larger than this to 10000 KiB.

.. _ydb-mupjnl-parallel:

++++++++++++++++++++++
ydb_mupjnl_parallel
++++++++++++++++++++++
**ydb_mupjnl_parallel (gtm_mupjnl_parallel)** defines the number of processes or threads used by MUPIP JOURNAL -RECOVER/-ROLLBACK when the invoking command does not have a -PARALLEL qualifier. When defined with no value, it specifies one process or thread per region. When undefined or defined to one (1), it specifies MUPIP should process all regions without using additional processes or threads. When defined with an integer value greater than one (1), it specifies the maximum number of processes or threads for MUPIP to use. If the value is greater than the number of regions, MUPIP never uses more processes or threads than there are regions. If it is less than the number of regions, MUPIP allocates work to the additional processes or threads based on the time stamps in the journal files.

++++++++++++++++
ydb_nocenable
++++++++++++++++
**ydb_nocenable (gtm_nocenable)** specifies whether the $principal terminal device should ignore <CTRL-C> or use <CTRL-C> as a signal to place the process into direct mode; a USE command can modify this device characteristic. If ydb_nocenable is defined and evaluates to a non-zero integer or any case-independent string or leading substrings of "TRUE" or "YES", $principal ignores <CTRL-C>. If ydb_nocenable is not set or evaluates to a value other than a positive integer or any case-independent string or leading substrings of "FALSE" or "NO", <CTRL-C> on $principal places the process into direct mode at the next opportunity (usually at a point corresponding to the beginning of the next source line).

+++++++++++++
ydb_nofflf
+++++++++++++
**ydb_nofflf (gtm_nofflf)** specifies the default WRITE # behavior for STREAM and VARIABLE format sequential files. If it is set to 1, Y[ES] or T[RUE], WRITE # writes only a form-feed <FF> character in conformance to the M standard. If it is not defined or set to 0, N[O] or F[ALSE], WRITE # writes <FF><LF> characters. The [NO]FFLF deviceparameter for USE and OPEN commands takes precedence over any value of ydb_nofflf.

ydb_nofflf was added to YottaDB effective release `r1.34 <https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.34>`_.

++++++++++++++++++++++++++++++++
ydb_non_blocked_write_retries
++++++++++++++++++++++++++++++++
**ydb_non_blocked_write_retries (gtm_non_blocked_write_retries)** modifies FIFO or PIPE write behavior. A WRITE which would block is retried up to the number specified with a 100 milliseconds delay between each retry. The default value is 10 times. If all retries block, the WRITE command issues a %SYSTEM-E-ENO11 (EAGAIN) error. For more details, refer to `PIPE Device Examples <https://docs.yottadb.com/ProgrammersGuide/ioproc.html#pipe-device-ex>`_ in the Programmers Guide.

.. _ydb-nontprestart-log-delta-env-var:

+++++++++++++++++++++++++++++
ydb_nontprestart_log_delta
+++++++++++++++++++++++++++++
**ydb_nontprestart_log_delta (gtm_nontprestart_log_delta)** specifies the number of non-transaction restarts for which YottaDB should wait before reporting a non-transaction restart to the operator logging facility. If ydb_nontprestart_log_delta is not defined, YottaDB initializes ydb_nontprestart_log_delta to 0.

+++++++++++++++++++++++++++++
ydb_nontprestart_log_first
+++++++++++++++++++++++++++++
**ydb_nontprestart_log_first (gtm_nontprestart_log_first)** specifies the initial number of non-transaction restarts which YottaDB should report before placing non-transaction restart reports to the operator logging facility using the :ref:`ydb-nontprestart-log-delta-env-var` value. If :ref:`ydb-nontprestart-log-delta-env-var` is defined and ydb_nontprestart_log_first is not defined, YottaDB initializes ydb_nontprestart_log_first to 1.

++++++++++++++
ydb_noundef
++++++++++++++
**ydb_noundef (gtm_noundef)** specifies the initial setting that controls whether a YottaDB process should treat undefined global or local variables as having an implicit value of an empty string. If it is defined, and evaluates to a non-zero integer or any case-independent string or leading substring of "TRUE" or "YES", then YottaDB treats undefined variables as having an implicit value of an empty string. The VIEW "[NO]UNDEF" command can alter this behavior in an active process. By default, YottaDB signals an error on an attempt to use the value of an undefined variable.

++++++++++++++++++++++
ydb_obfuscation_key
++++++++++++++++++++++
**ydb_obfuscation_key (gtm_obfuscation_key)** : If $ydb_obfuscation_key specifies the name of the file readable by the process, the encryption reference plug-in uses a cryptographic hash of the file's contents as the XOR mask for the obfuscated password in the environment variable :ref:`ydb-passwd-env-var`. When ydb_obfuscation_key does not point to a readable file, the plugin computes a cryptographic hash using a mask based on the value of $USER and the inode of the yottadb executable to use as a mask. $ydb_passwd set with a $ydb_obfuscation_key allows access to all users who have the same $ydb_obfuscation_key defined in their environments. However, $ydb_passwd set without $ydb_obfuscation_key can be used only by the same $USER using the same YottaDB distribution.

.. _ydb-passwd-env-var:

+++++++++++++
ydb_passwd
+++++++++++++
**ydb_passwd (gtm_passwd)** specifies the obfuscated (not encrypted) password of the GNU Privacy Guard key ring. When the environment variable $ydb_passwd is set to "", YottaDB invokes the default GTMCRYPT passphrase prompt defined in the reference implementation of the plugin to obtain a passphrase at process startup and uses that value as $ydb_passwd for the duration of the process.

+++++++++++++++++
ydb_patnumeric
+++++++++++++++++
**ydb_patnumeric (gtm_patnumeric)** specifies the value of the read-only ISV $ZPATNUMERIC that determines how YottaDB interprets the patcode "N" used in the pattern match operator. The SET command can alter the value of $ZPATNUMERIC in an active process.

+++++++++++++++++++++++++++++++++++++
ydb_pattern_file/ydb_pattern_table
+++++++++++++++++++++++++++++++++++++
**ydb_pattern_file (gtm_pattern_file)** and **ydb_pattern_table (gtm_pattern_table)** specify alternative patterns for the pattern (?) syntax. Refer to the `Internationalization chapter in the Programmer's Guide <../ProgrammersGuide/internatn.html>`_ for additional information.

++++++++++++++++
ydb_poollimit
++++++++++++++++
**ydb_poollimit (gtm_poollimit)** restricts the number of global buffers a process uses in order to limit the potential impact on other processes. It is intended for use by MUPIP REORG, since it has the potential to "churn" global buffers; the value is of the form n[%]. When it ends with a per-cent sign (%), the number is taken as a percentage of the configured global buffers and otherwise as an ordinal number of preferred buffers; standard M parsing and integer conversions apply. Note that this environment variable applies to all regions accessed by a process; the VIEW command for this feature allows finer grained control. MUPIP REORG uses this facility to limit its buffers with a default of 64 if ydb_poollimit is not specified. Note that this may slightly slow a standalone REORG but can be overridden by defining ydb_poollimit as 0 or "100%".

++++++++++++++++
ydb_principal
++++++++++++++++
**ydb_principal (gtm_principal)** specifies the value for $PRINCIPAL, which designates an alternative name (synonym) for the principal $IO device.

.. _ydb-principal-editing-env-var:

++++++++++++++++++++++++
ydb_principal_editing
++++++++++++++++++++++++
**ydb_principal_editing (gtm_principal_editing)** specifies the initial settings for $PRINCIPAL for the following colon-delimited deviceparameters: [NO]EDITING, [NO]EMPTERM and [NO]INSERT; in an active process the USE command can modify these device characteristics.

.. note::
   The YottaDB direct mode commands have a more extensive capability in this regard, independent of the value of this environment variable.

.. _ydb-procstuckexec-env-var:

++++++++++++++++++++
ydb_procstuckexec
++++++++++++++++++++
**ydb_procstuckexec (gtm_procstuckexec)** specifies a shell command or a script to execute when any of the following conditions occur:

* A one minute wait on a region due to an explicit MUPIP FREEZE or an implicit freeze, such as BACKUP, INTEG -ONLINE, and so on.

* MUPIP actions find kill_in_prog (KILLs in progress) to be non-zero after a one minute wait on a region. Note that YottaDB internally maintains a list of PIDs (up to a maximum of 8 PIDs) currently doing a KILL operation.

* A process encounters conditions that produce the following operator log messages: BUFOWNERSTUCK, INTERLOCK_FAIL, JNLPROCSTUCK, SHUTDOWN, WRITERSTUCK, MAXJNLQIOLOCKWAIT, MUTEXLCKALERT, SEMWT2LONG, and COMMITWAITPID.

You can use this as a monitoring facility for processes holding a resource for an unexpected amount of time. Typically, for the shell script or command pointed to by ydb_procstuckexec, you would write corrective actions or obtain the stack trace of the troublesome processes (using their PIDs). YottaDB passes arguments to the shell command/script in the order specified as follows:

* *condition* is the name of the condition. For example, BUFOWNERSTUCK, INTERLOCK_FAIL, and so on.

* *waiting_pid* is the PID of the process reporting the condition.

* *blocking_pid* is the PID of the process holding a resource.

* *count* is the number of times the script has been invoked for the current condition (1 for the first occurrence).

Each invocation generates an operator log message and if the invocation fails, an error message to the operator log. The shell script should start with a line beginning with #! that designates the shell.

.. note::
   Make sure that user processes have sufficient space and permissions to run the shell command/script. For example - for the script to invoke the debugger, the process must be of the same group or have a way to elevate privileges.

+++++++++++++
ydb_prompt
+++++++++++++
**ydb_prompt (gtm_prompt)** specifies the initial value of the ISV $ZPROMPT, which controls the YottaDB direct mode prompt. The SET command can alter the value of $ZPROMPT in an active process. By default, the direct mode prompt is "YDB>".

+++++++++++++++++
ydb_quiet_halt
+++++++++++++++++
**ydb_quiet_halt (gtm_quiet_halt)** specifies whether YottaDB should disable the FORCEDHALT message when the process is stopped via MUPIP STOP or by a SIGTERM signal (as sent by some web servers).

++++++++++++++++++++++++++
ydb_recompile_newer_src
++++++++++++++++++++++++++
**ydb_recompile_newer_src** when set to 1, t[rue], or y[es], specifies that a ZLINK/DO/GOTO/ZBREAK/ZGOTO/ZPRINT/$TEXT should recompile the :code:`.m` file only if it has a newer modification time than the corresponding :code:`.o` file. The default behavior is for the :code:`.m` file to be recompiled if its modification time is later than OR equal to that of the corresponding :code:`.o` file. ydb_recompile_newer_src was added effective release `r1.30 <https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.30>`_.

++++++++++
ydb_rel
++++++++++
**ydb_rel (gtmversion)** (not used by YottaDB directly) - The current YottaDB version. The ydb_env_set script uses $ydb_rel to set other environment variables.

++++++++++++++++++++++++++
ydb_repl_filter_timeout
++++++++++++++++++++++++++
**ydb_repl_filter_timeout (gtm_repl_filter_timeout)** can be set to an integer value indicating the timeout (in seconds) that the replication source server sets for a response from the external filter program. A value less than 32 would be treated as if 32 was specified. A value greater than 131072 (2**17) would be treated as if 131072 was specified. The default value of the timeout (if env var is not specified) is 64 seconds. This provides the user a way to avoid seeing FILTERTIMEDOUT errors from the source server on relatively slower systems.

.. _ydb-repl-instance-env-var:

++++++++++++++++++++
ydb_repl_instance
++++++++++++++++++++
**ydb_repl_instance (gtm_repl_instance)** specifies the location of the replication instance file when database replication is in use.

++++++++++++++++++++
ydb_repl_instname
++++++++++++++++++++
**ydb_repl_instname (gtm_repl_instname)** specifies a replication instance name that uniquely identifies an instance. The replication instance name is immutable. The maximum length of a replication instance name is 15 bytes. Note that the instance name is not the same as the name of the replication instance file (:ref:`ydb-repl-instance-env-var`). You need to specify a replication instance name at the time of creating a replication instance file. If you do not define ydb_repl_instname, you need to specify an instance name using -NAME=<instance_name> with MUPIP REPLICATE -INSTANCE_CREATE.

+++++++++++++++++++++++++
ydb_repl_instsecondary
+++++++++++++++++++++++++
**ydb_repl_instsecondary (gtm_repl_instsecondary)** specifies the name of the replicating instance in the current environment. YottaDB uses $ydb_repl_instsecondary if the -instsecondary qualifer is not specified.

++++++++++++++++
ydb_retention
++++++++++++++++
**ydb_retention (gtm_retention)** (not used by YottaDB directly) - Journal files and temporary files older than the number of days specified by :code:`ydb_retention` (:code:`gtm_retention` if not specified; defaulting to 42 days), are deleted by sourcing the :code:`ydb_env_set` file, which can be invoked explicitly, or as part of executing the :code:`ydb` script.

+++++++++++++++
ydb_routines
+++++++++++++++
**ydb_routines (gtmroutines)** specifies the initial value of the $ZROutines ISV, which specifies where to find object and source code. The SET command can alter the value of $ZROUTINES in an active process.

.. _ydb-side-effects-env-var:

+++++++++++++++++++
ydb_side_effects
+++++++++++++++++++
**ydb_side_effects (gtm_side_effects)**: When the environment variable ydb_side_effects is set to one (1) at process startup, YottaDB generates code that performs left to right evaluation of actual list arguments, function arguments, operands for non-Boolean binary operators, SET arguments where the target destination is an indirect subscripted glvn, and variable subscripts. When the environment variable is not set or set to zero (0), YottaDB retains its traditional behavior, which re-orders the evaluation of operands using rules intended to improve computational efficiency. This reordering assumes that functions have no side effects, and may generate unexpected behavior (x+$increment(x) is a pathological example). When ydb_side_effects is set to two (2), YottaDB generates code with the left-to-right behavior, and also generates SIDEEFFECTEVAL warning messages for each construct that potentially generates different results depending on the order of evaluation. As extrinsic functions and external calls are opaque to the compiler at the point of their invocation, it cannot statically determine whether there is a real interaction. Therefore, SIDEEFFECTEVAL warnings may be much more frequent than actual side effect interactions and the warning mode may be most useful as a diagnostic tool to investigate problematic or unexpected behavior in targeted code rather than for an audit of an entire application. Note that a string of concatenations in the same expression may generate more warnings than the code warrants. Other values of the environment variable are reserved for potential future use by YottaDB. It is important to note that ydb_side_effects affects the generated code, and must be in effect when code is compiled - the value when that compiled code is executed is irrelevant. Note also that XECUTE and auto-ZLINK, explicit ZLINK and ZCOMPILE all perform run-time compilation subject to the characteristics selected when the process started. Please be aware it is an unsafe programming practice when one term of an expression changes a prior term in the same expression. The environment variable :ref:`ydb-boolean-env-var` may separately control short-circuit evaluation of Boolean expressions but a setting of 1 (or 2) for ydb_side_effects causes the same boolean evaluations as setting :ref:`ydb-boolean-env-var` to 1 (or 2). Note that warning reports for the two features are separately controlled by setting their values to 2. The differences in the compilation modes may include not only differences in results, but differences in flow of control when the code relies on side effect behavior.

+++++++++++++++++
ydb_snaptmpdir
+++++++++++++++++
**ydb_snaptmpdir (gtm_snaptmpdir)** specifies the location to place the temporary "snapshot" file created by facilities such as MUPIP INTEG ONLINE. If $ydb_snaptmpdir is not defined, YottaDB uses the deprecated $GTM_BAKTMPDIR environment variable if defined, and otherwise uses the current working directory. All processes performing updates during an online INTEG must use the same directory and have write access to it.

++++++++++++++++++++++++
ydb_string_pool_limit
++++++++++++++++++++++++
**ydb_string_pool_limit (gtm_string_pool_limit)** is used for the initial value of $ZSTRPLLIM, when it specifies a positive value.

+++++++++++++++
ydb_statsdir
+++++++++++++++
**ydb_statsdir (gtm_statsdir)** specifies the directory for database files into which processes that have opted-in to sharing global statistics place their statistics as binary data. If you do not explicitly define this environment variable for a process, YottaDB defines this to the evaluation of $ydb_tmp, which defaults to /tmp. All processes that share statistics MUST use the same value for $ydb_statsdir. YottaDB suggests that you point ydb_statsdir at a tmpfs or ramfs. These database files have a name derived from the user defined database file name and a .gst extension. They are not usable as normal database files by application code, except to read statistics. YottaDB automatically creates and deletes these database files as needed. Under normal operation, applications do not need to manage them explicitly. The mapping of ^%YGS to statistics database files is managed by YottaDB transparently to applications with global directories. The ^%YGBLSTAT utility program gathers and reports statistics from nodes of ^%YGS(region,pid).

++++++++++++++++
ydb_statshare
++++++++++++++++
**ydb_statshare (gtm_statshare)** specifies an initial value for the `VIEW "[NO]STATSHARE" <../ProgrammersGuide/commands.html#view-nostatshare>`_ setting.

+++++++++++++++
ydb_stdxkill
+++++++++++++++
**ydb_stdxkill (gtm_stdxkill)** enables the standard-compliant behavior to kill local variables in the exclusion list if they had an alias that was not in the exclusion list. By default, this behavior is disabled.

++++++++++++
ydb_sysid
++++++++++++
**ydb_sysid (gtm_sysid)** specifies the value for the second piece of the $SYSTEM ISV. $SYSTEM contains a string that identifies the executing M instance. The value of $SYSTEM is a string that starts with a unique numeric code that identifies the manufacturer. Codes were originally assigned by the MDC (M Development Committee). $SYSTEM in YottaDB starts with "47" followed by a comma and $ydb_sysid.

+++++++++++++++++++++++++
ydb_tls_passwd_<label>
+++++++++++++++++++++++++
**ydb_tls_passwd_<label> (gtmtls_passwd_<label>)** specifies the obfuscated password of the encrypted private key pair. You can obfuscate passwords using the 'maskpass' utility provided along with the encryption plugin. If you choose to use unencrypted private keys, set the ydb_tls_passwd_<label> environment variable to a non-null dummy value; this prevents inappropriate prompting for a password.

++++++++++
ydb_tmp
++++++++++
**ydb_tmp (gtm_tmp)** specifies a directory where socket files used for communication between gtmsecshr and YottaDB processes are stored. All processes using the same YottaDB installation (i.e., the same :code:`ydb_dist`) should have the same $ydb_tmp. YottaDB recommends setting ydb_tmp to a location:

* which is unique to a YottaDB version, e.g., /tmp/r1.34_x86_64; and
* where operation procedures and policies can ensure that the contents are not removed when there are active YottaDB processes.

If appropriate, set ydb_tmp to a directory location in tmpfs or ramfs on Linux.

If ydb_tmp is not defined, YottaDB uses the /tmp directory which may disrupt active gtmsecshr operations when /tmp is either cleared manually or by the retention policies of the operating system.

++++++++++++++++++++
ydb_tpnotacidtime
++++++++++++++++++++
**ydb_tpnotacidtime (gtm_tpnotacidtime)** specifies the maximum time that a YottaDB process waits for a non-isolated timed command (HANG, JOB, LOCK, OPEN, READ, WRITE /* or ZALLOCATE) running within a transaction to complete before it releases all critical sections it owns and sends a TPNOTACID information message to the system log. A YottaDB process owns critical sections on all or some of the regions participating in a transaction, only during final retry attempts (when $TRETRY>2). ydb_tpnotacidtime specifies time in seconds to millisecond precision (three decimal places); the default is 2 seconds. The maximum value of ydb_tpnotacidtime is 30 and the minimum is 0. If ydb_tpnotacidtime specifies a time outside of this range, YottaDB uses the default value. YottaDB releases critical sections in a final retry attempt to provide protection from certain risky coding patterns which, because they are not isolated, can cause deadlocks (in the worst case) and long hangs (in the best case). As ZSYSTEM and BREAK are neither isolated nor timed, YottaDB initiates TPNOTACID behavior for them immediately as it encounters them during execution in a final retry attempt (independent of ydb_tpnotacidtime). Rapidly repeating TPNOTACID messages are likely associated with live-lock, which means that a process is consuming critical resources repeatedly within a transaction, and is unable to commit because the transaction duration is too long to commit while maintaining ACID transaction properties.

.. _ydb-tprestart-log-delta-env-var:

++++++++++++++++++++++++++
ydb_tprestart_log_delta
++++++++++++++++++++++++++
**ydb_tprestart_log_delta (gtm_tprestart_log_delta)** specifies the number of transaction restarts for which YottaDB should wait before reporting a transaction restart to the operator logging facility. If ydb_tprestart_log_delta is not defined, YottaDB initializes ydb_tprestart_log_delta to 0.

++++++++++++++++++++++++++
ydb_tprestart_log_first
++++++++++++++++++++++++++
**ydb_tprestart_log_first (gtm_tprestart_log_first)** specifies the initial number of transaction restarts which YottaDB should report before pacing transaction restart reports to the operator logging facility using the :ref:`ydb-tprestart-log-delta-env-var` value. If :ref:`ydb-tprestart-log-delta-env-var` is defined and ydb_tprestart_log_first is not defined, YottaDB initializes ydb_tprestart_log_first to 1.

+++++++++++++++++++++
ydb_trace_gbl_name
+++++++++++++++++++++
**ydb_trace_gbl_name (gtm_trace_gbl_name)** enables YottaDB tracing at process startup. Setting ydb_trace_gbl_name to a valid global variable name instructs YottaDB to report the data in the specified global when a VIEW command disables the tracing, or implicitly at process termination. This setting behaves as if the process issued a VIEW "TRACE" command at process startup. However, ydb_trace_gbl_name has a capability not available with the VIEW command, such that if the environment variable is defined but evaluates to zero (0) or to the empty string, YottaDB collects the M-profiling data in memory and discards it when the process terminates (this feature is mainly used for in-house testing). Note that having this feature activated for processes that otherwise don't open a database file (such as GDE) can cause them to encounter an error.

++++++++++++++++++++++++++++++++
ydb_treat_sigusr2_like_sigusr1
++++++++++++++++++++++++++++++++
**ydb_treat_sigusr2_like_sigusr1** when set to a non-zero numeric value, "yes" or "TRUE" (case-insensitive), or a leading substring of "yes" or "true", causes a YottaDB process to treat a USR2 signal just as it would a SIGUSR1 (by invoking $ZINTERRUPT mechanism). The default behavior is to ignore SIGUSR2.

ydb_treat_sigusr2_like_sigusr1 was added to YottaDB release `r1.32 <https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.32>`_.

++++++++++++++++++++
ydb_trigger_etrap
++++++++++++++++++++
**ydb_trigger_etrap (gtm_trigger_etrap)** provides the initial value for $ETRAP in trigger context; can be used to set trigger error traps for trigger operations in both yottadb and MUPIP processes.

++++++++++++++++++
ydb_xc_gpgagent
++++++++++++++++++
**ydb_xc_gpgagent (GTMXC_gpgagent)** specifies the location of gpgagent.tab. By default, YottaDB places gpgagent.tab in the $ydb_dist/plugin/ directory. ydb_xc_gpgagent is used by pinentry-gtm.sh and is meaningful only if you are using Gnu Privacy Guard version 2.

+++++++++++++++++
ydb_zdate_form
+++++++++++++++++
**ydb_zdate_form (gtm_zdate_form)** specifies the initial value for the $ZDATE ISV. The SET command can alter the value of $ZDATE in an active process.

.. _ydb-zinterrupt-env-var:

+++++++++++++++++
ydb_zinterrupt
+++++++++++++++++
**ydb_zinterrupt (gtm_zinterrupt)** specifies the initial value of the ISV $ZINTERRUPT which holds the code that YottaDB executes (as if it is the argument for an XECUTE command) when a process receives a signal from a MUPIP INTRPT command. The SET command can alter the value of $ZINTERRUPT in an active process.

+++++++++++++++++++++
ydb_zlib_cmp_level
+++++++++++++++++++++
**ydb_zlib_cmp_level (gtm_zlib_cmp_level)** specifies the zlib compression level used in the replication stream by the source and receiver servers. By default, replication does not use compression.

+++++++++++++++++++
ydb_zquit_anyway
+++++++++++++++++++
**ydb_zquit_anyway (gtm_zquit_anyway)** specifies whether the code of the form QUIT <expr> execute as if it were SET <tmp>=<expr> QUIT:$QUIT tmp QUIT, where <tmp> is a temporary local variable in the YottaDB runtime system that is not visible to application code. This setting is a run-time setting, rather than a compiler-time setting. If ydb_zquit_anyway is defined and evaluates to 1 or any case-independent string or leading substrings of "TRUE" or "YES", code of the form QUIT <expr> executes as if it were SET <tmp>=<expr> QUIT:$QUIT tmp QUIT. If ydb_zquit_anyway is not defined or evaluates to 0 or any case-independent string or leading substrings of "FALSE" or "NO", YottaDB executes QUIT <expr> as specified by the standard.

++++++++++++
ydb_zstep
++++++++++++
**ydb_zstep (gtm_zstep)** specifies the initial value of $ZSTEP, which defines the ZSTEP action; if ydb_zstep is not defined, $ZSTEP defaults to "B".

+++++++++++++++++++++++++++++
ydb_ztrap_form/ydb_zyerror
+++++++++++++++++++++++++++++
**ydb_ztrap_form (gtm_ztrap_form)** and **ydb_zyerror (gtm_zyerror)** specify the behavior of error handling specified by $ZTRAP as described in the `Error Processing chapter of the Programmer's Guide <../ProgrammersGuide/errproc.html>`_.

++++++++++++++++
ydb_ztrap_new
++++++++++++++++
**ydb_ztrap_new (gtm_ztrap_new)** specifies whether a SET $ZTRAP also implicitly performs a NEW $ZTRAP before the SET.

The ydb_env_set and gtmcshrc scripts sets the following environment variables. YottaDB recommends using the ydb_env_set script (or the ydb script which sources ydb_env_set) to set up an environment for YottaDB.

+------------------------------------------------+--------------------------------------------------------+
| Environment Variables                          | Set up by YottaDB shell scripts                        |
+================================================+========================================================+
| LC_CTYPE                                       | ydb_env_set                                            |
+------------------------------------------------+--------------------------------------------------------+
| ydb_gbldir*                                    | ydb_env_set, gtmcshrc                                  |
+------------------------------------------------+--------------------------------------------------------+
| ydb_routines*                                  | ydb_env_set, gtmcshrc                                  |
+------------------------------------------------+--------------------------------------------------------+
| ydb_rel                                        | ydb_env_set                                            |
+------------------------------------------------+--------------------------------------------------------+
| ydb_dist*                                      | ydb_env_set, gtmcshrc                                  |
+------------------------------------------------+--------------------------------------------------------+
| ydb_icu_version                                | ydb_env_set                                            |
+------------------------------------------------+--------------------------------------------------------+
| ydb_log                                        | ydb_env_set                                            |
+------------------------------------------------+--------------------------------------------------------+
| ydb_prompt                                     | ydb_env_set                                            |
+------------------------------------------------+--------------------------------------------------------+
| ydb_repl_instance                              | ydb_env_set                                            |
+------------------------------------------------+--------------------------------------------------------+
| ydb_retention                                  | ydb_env_set                                            |
+------------------------------------------------+--------------------------------------------------------+
| ydb_tmp                                        | ydb_env_set                                            |
+------------------------------------------------+--------------------------------------------------------+

\* denotes environment variables that must be defined for normal YottaDB operation.

While creating an environment for multiple processes accessing the same version of YottaDB, bear in mind the following important points:

* A YottaDB version has an associated gtmsecshr (located by $ydb_dist). If multiple processes are accessing the same YottaDB version, each process must use the same combination of $ydb_tmp and $ydb_log.

* In conformance with the Filesystem Hierarchy Standard, YottaDB recommends /var/log/yottadb/$ydb_rel as the value for $ydb_log. Note that $ydb_rel can be in the form of the current YottaDB release and platform information.

* YottaDB recommends setting $ydb_tmp to a temporary directory. The ydb_env_set script sets $ydb_tmp to /tmp/yottadb/$ydb_rel.

* YottaDB recommends setting $ydb_log to a temporary directory. The ydb_env_set script sets $ydb_log to /tmp/yottadb/$ydb_rel.

Always set the same value of $ydb_tmp for all processes using the same YottaDB version. Having different $ydb_tmp for multiple processes accessing the same YottaDB version may prevent processes from being able to communicate with gtmsecshr and cause performance issues.

.. _config-op-ydb-unicode:

-------------------------------------------------------------------
 Configuring and operating YottaDB with Unicode™ support (optional)
-------------------------------------------------------------------

The configure script provides the option to install YottaDB with or without Unicode™ support for encoding international character sets. This section describes the system environment required to install and operate YottaDB with Unicode™ support. Users who handle data in ASCII or other single-byte character sets such as one of the ISO-8859 representations and do not foresee any use of character sets beyond single byte character sets, may proceed to the next section.

++++++++++++++++++++++++++++++++
M mode and UTF-8 mode
++++++++++++++++++++++++++++++++

A YottaDB process can operate in either M mode or UTF-8 mode. In certain circumstances, both M mode and UTF-8 mode may concurrently access the same database.

$ydb_chset determines the mode in which a process operates. If it has a value of M, YottaDB treats all 256 combinations of the 8 bits in a byte as a character, which is suitable for many single-language applications.

If $ydb_chset has a value of UTF-8, YottaDB (at process startup) interprets strings as being encoded in UTF-8. In this mode, all functionality related to Unicode™ becomes available and standard string-oriented operations operate with UTF-8 encoding. In this mode, YottaDB detects character boundaries (since the size of a character is variable length), calculates glyph display width, and performs string conversion between UTF-8 and UTF-16.

If you install YottaDB with Unicode™ support, all YottaDB components related to the M mode reside in your YottaDB distribution directory and Unicode™-related components reside in the utf8 subdirectory of your YottaDB distribution. For processes in UTF-8 mode, in addition to ydb_chset, ensure that $ydb_dist points to the utf8 subdirectory, that $ydb_routines includes the utf8 subdirectory (or the libgtmutil.so therein) rather than its parent directory.

In addition to $ydb_chset, recent versions use $ydb_icu_version to choose an ICU library version other than the default. For ICU libraries built with symbol renaming enabled, $ydb_icu_version becomes a required setting.

$ydb_icu_version specifies the ICU version that YottaDB should use for Unicode operations. It is in the form of MajorVersion.MinorVersion where MajorVersion and MinorVersion specify the desired major version and minor version of ICU. For example, 3.6 refers to ICU version 3.6. If $ydb_icu_version is defined, YottaDB works regardless of whether or not symbols are renamed in ICU. If $ydb_icu_version is not defined or does not evaluate to an installed ICU version, YottaDB looks for non-renamed symbols in the default ICU version. Note that display widths for a few characters are different starting in ICU 4.0.

.. note::
   The ydb_env_set script defines $ydb_icu_version as necessary.

+++++++++++++++++
Compiling ICU
+++++++++++++++++

YottaDB uses ICU 3.6 (or above) to perform Unicode™-related operations. YottaDB generates the distribution for Unicode™ only if ICU 3.6 (or above) is installed on the system. Therefore, install an appropriate ICU version before installing YottaDB to perform functionality related to Unicode™.

Note that the ICU installation instructions may not be the same for every platform. If libicu has been compiled with symbol renaming enabled, YottaDB requires $ydb_icu_version be explicitly set. Please see the above section for more information.

After installing ICU 3.6 (or above), you also need to set the following environment variables to an appropriate value.

1. LC_CTYPE
2. LC_ALL
3. LD_LIBRARY_PATH
4. TERM

------------------------------
Starting YottaDB
------------------------------

++++++++++++++++++++++++++++++++++++
To start YottaDB from a POSIX shell:
++++++++++++++++++++++++++++++++++++

Execute ydb from your shell prompt:

.. code-block:: bash

   $ <path_to_ydb_installation_directory>/ydb

++++++++++++++++++++++++++++++++++++++++++++++++++
To start YottaDB in UTF-8 mode from a POSIX shell:
++++++++++++++++++++++++++++++++++++++++++++++++++

First, set $ydb_chset to UTF-8 and LC_CTYPE or LC_ALL to any usable UTF-8 locale.

.. code-block:: bash

   $ export ydb_chset="UTF-8"
   $ export LC_CTYPE="en_US.utf8"

Execute the ydb script.

.. code-block:: bash

   $ <path_to_ydb_installation_directory>/ydb

+++++++++++++++++++++++++++++++++++++
To start YottaDB from a C-type shell:
+++++++++++++++++++++++++++++++++++++

First source the gtmcshrc script to set up a default YottaDB environment. At your shell prompt, type:

.. code-block:: bash

   $ source <path_to_ydb_installation_directory>/gtmcshrc

Run the ydb alias to start YottaDB in direct mode.

.. code-block:: bash

   $ ydb

+++++++++++++++++++++++++++++++++++++++++++++++++++
To start YottaDB in UTF-8 mode from a C-type shell:
+++++++++++++++++++++++++++++++++++++++++++++++++++

Set the environment variable ydb_chset to UTF-8 and LC_CTYPE or LC_ALL to any usable UTF-8 locale.

.. code-block:: bash

   $ setenv ydb_chset UTF-8
   $ setenv LC_CTYPE en_US.utf8

Source the gtmcshrc script to set up default YottaDB Unicode environment.

.. code-block:: bash

   $ source <path_to_ydb_installation_directory>/gtmcshrc

Run the ydb alias to start YottaDB in direct mode.

.. code-block:: bash

   $ ydb

++++++++++++++++++++++++++++++++++++++++++
To start YottaDB without using any script:
++++++++++++++++++++++++++++++++++++++++++

* Define ydb_dist, ydb_log, ydb_tmp, ydb_gbldir, and ydb_routines. Ensure that ydb_dist points to the location of your YottaDB distribution.

* Add ydb_dist to the system environment variable PATH.

* Ensure that you have set an appropriate value for TERM.

* Consider adding these environment variables in your login file so you do not have to create them again the next time you start your shell.

* Set up the following aliases to run YottaDB and its utilities.

 .. code-block:: bash

    alias dse="$ydb_dist/dse"
    alias gde="$ydb_dist/yottadb -run ^GDE"
    alias ydb="$ydb_dist/yottadb -direct"
    alias lke="$ydb_dist/lke"
    alias mupip="$ydb_dist/mupip"

* Run the ydb alias to start YottaDB in direct mode.

  .. code-block:: bash

     $ ydb

++++++++++++++++++++++++++++++++++++++++++++++++++++++++
To start YottaDB in UTF-8 mode without using any script:
++++++++++++++++++++++++++++++++++++++++++++++++++++++++

* Define ydb_dist, ydb_log, ydb_gbldir, and ydb_routines. Ensure that ydb_dist points to the uft8 subdirectory of your YottaDB distribution.

* Set ydb_routines to include the utf8 subdirectory of your YottaDB distribution. Note that the utf8 subdirectory includes all Unicode-related YottaDB functionality.

* Ensure that you have installed ICU 3.6 (or above) and have LC_CTYPE or LC_ALL set to a usable UTF-8 locale.

* Set LD_LIBRARY_PATH and TERM to appropriate values.

* If you have built ICU with symbol renaming enabled, set ydb_icu_version to an appropriate ICU version.

* Add ydb_dist to the system environment variable PATH.

* Set ydb_chset to UTF-8.

* Consider adding these environment variables to your login file so you do not have to create them again the next time you start your shell.

* Set up the following aliases to run YottaDB and its utilities.

  .. code-block:: bash

     alias dse="$ydb_dist/dse"
     alias gde="$ydb_dist/yottadb -run ^GDE"
     alias ydb="$ydb_dist/yottadb -direct"
     alias lke="$ydb_dist/lke"
     alias mupip="$ydb_dist/mupip"

* Type the following command to start YottaDB in direct mode.

  .. code-block:: bash

     $ ydb

* At the YottaDB prompt, type the following command.

  .. code-block:: bash

    YDB>w $ZCHSET
    UTF-8 ; the output confirms UTF-8 mode.

.. note::
    If you are configuring a YottaDB environment without using the ydb_env_set script (or the ydb script which sources ydb_env_set), bear in mind the following recommendation: All YottaDB processes should use the same settings for ydb_log and ydb_tmp, especially for production environments. This is because gtmsecshr inherits these values from whichever YottaDB process first uses its services. If there are multiple YottaDB versions active on a system, YottaDB recommends different sets of ydb_log and ydb_tmp values for each version as using the same values for different distributions can cause significant performance issues.

YottaDB has three invocation modes: compiler, direct, and auto-start. To invoke YottaDB in these modes, provide the following arguments to the ydb script or the yottadb command.

* **-direct**: Invokes YottaDB in direct mode where you can enter M commands interactively.

* **<list of M source files>**: Invokes YottaDB in compiler mode, invoke YottaDB by entering a list of file names to compile as an argument. YottaDB then compiles the specified programs into .o files, UNIX shell globbing to resolve wild-cards (* and ?) in names.

* **-run ^routine_name**: -r invokes YottaDB in auto-start mode. The second argument is taken to be an M entryref, and that routine is automatically executed, bypassing direct mode. Depending on your shell, you may need to put the entryref in quotes.

When executing M programs, YottaDB incrementally links any called programs. For example, the command YDB> do ^TEST links the object file TEST.o and executes it; if the TEST.m program calls other M routines, those are automatically compiled and linked.

.. note::
   When possible, YottaDB verifies that M, MUPIP, DSE and LKE reside in $ydb_dist. If the path to the executable and the path to $ydb_dist do not match, each executable issues an error. In cases where the executable path could not be determined, each executable defers issuing an error until it is required.

--------------------------------------------------
 Configuring huge pages for YottaDB on Linux
--------------------------------------------------

Huge pages are a Linux feature that may improve the performance of YottaDB applications in production. Huge pages create a single page table entry for a large block (typically 2MiB) of memory in place of hundreds of entries for many smaller (typically 4KiB) blocks. This reduction of memory used for page tables frees up memory for other uses, such as file system caches, and increases the probability of TLB (translation lookaside buffer) matches - both of which can improve performance. The performance improvement related to reducing the page table size becomes evident when many processes share memory as they do for global buffers, journal buffers, and replication journal pools. Configuring huge pages on Linux for x86 or x86_64 CPU architectures help improve:

* YottaDB shared memory performance: When your YottaDB database uses journaling, replication, and the BG access method.

* YottaDB process memory performance: For your process working space and dynamically linked code.

  .. note::

     At this time, huge pages have no effect for MM databases; the text, data, or bss segments for each process; or for process stack.

While YottaDB recommends you configure huge pages for shared memory, you need to evaluate whether or not configuring huge pages for process-private memory is appropriate for your application. Having insufficient huge pages available during certain commands (for example, a JOB command - see complete list below) can result in a process terminating with a SIGBUS error. This is a current limitation of Linux. Before you use huge pages for process-private memory on production systems, YottaDB recommends that you perform appropriate peak load tests on your application and ensure that you have an adequate number of huge pages configured for your peak workloads or that your application is configured to perform robustly when processes terminate with SIGBUS errors.

The following YottaDB features fork processes and may generate SIGBUS errors when huge pages are not available - JOB, OPEN a PIPE device, ZSYSTEM, interprocess signaling that requires the services of gtmsecshr when gtmsecshr is not already running, SPAWN commands in DSE, GDE, and LKE, argumentless MUPIP RUNDOWN, and replication-related MUPIP commands that start server processes and/or helper processes. As increasing the available huge pages may require a reboot, an interim workaround is to unset the environment variable HUGETLB_MORECORE for YottaDB processes until you are able to reboot or otherwise make available an adequate supply of huge pages.

Consider the following example of a memory map report of a Source Server process running at peak load:

.. code-block:: bash

   $ pmap -d 18839
   18839: /usr/lib/yottadb/r120/mupip replicate -source -start -buffsize=1048576 -secondary=melbourne:1235 -log=/var/log/.yottadb/mal2mel.log -instsecondary=melbourne
   Address   Kbytes Mode Offset   Device Mapping
   --- lines removed for brevity -----
   mapped: 61604K writeable/private: 3592K shared: 33532K
   $

Process id 18839 uses a large amount of shared memory (33535K) and can benefit from configuring huge pages for shared memory. Configuring huge pages for shared memory does not cause a SIGBUS error when a process does a fork. For information on configuring huge pages for shared memory, refer to the "Using huge pages" and "Using huge pages for shared memory" sections. SIGBUS errors only occur when you configure huge pages for process-private memory; these errors indicate you have not configured your system with an adequate number of huge pages. To prevent SIGBUS errors, you should perform peak load tests on your application to determine the number of required huge pages. For information on configuring huge pages for process-private memory, refer to the "Using huge pages" and "Using huge pages for process working space" sections.

As application response time can be adversely affected if processes and database shared memory segments are paged out, YottaDB recommends configuring systems for use in production with sufficient RAM so as to not require swap space or a swap file. While you must configure an adequate number of huge pages for your application needs as empirically determined by benchmarking/testing and there is little downside to a generous configuration to ensure a buffer of huge pages available for workload spikes, an excessive allocation of huge pages may affect system throughput by reserving memory for huge pages that could otherwise be used by applications that cannot use huge pages.


++++++++++++++++++++++++++++++++++
Using huge pages
++++++++++++++++++++++++++++++++++

+----------------------------------------------------------------------+---------------------------------------------------------------------------------------------------------------------------------------+
| Prerequisites                                                        | Notes                                                                                                                                 |
+======================================================================+=======================================================================================================================================+
| A 32- or 64-bit x86 CPU running a Linux kernel with huge pages       | All currently Supported Linux distributions appear to support huge pages; to confirm, use the command: grep hugetlbfs                 |
| enabled                                                              | /proc/filesystems which should report: nodev hugetlbfs                                                                                |
+----------------------------------------------------------------------+---------------------------------------------------------------------------------------------------------------------------------------+
| libhugetlbfs.so                                                      | Use your Linux system's package manager to install the libhugetlbfs.so library in a standard location. Note that libhugetlbfs is not  |
|                                                                      | in certain repositories and must be manually installed.                                                                               |
+----------------------------------------------------------------------+---------------------------------------------------------------------------------------------------------------------------------------+
| Have sufficient number of huge pages available.                      | To reserve Huge Pages boot Linux with the hugepages=num_pages kernel boot parameter; or, shortly after bootup when unfragmented       |
|                                                                      | memory is still available, with the command: hugeadm --pool-pages-min DEFAULT:num_pages                                               |
|                                                                      | For subsequent on-demand allocation of Huge Pages, use: hugeadm --pool-pages-max DEFAULT:num_pages                                    |
|                                                                      | These delayed (from boot) actions do not guarantee availability of the requested number of huge pages; however, they are safe as, if a|
|                                                                      | sufficient number of huge pages are not available, Linux simply uses traditional sized pages.                                         |
+----------------------------------------------------------------------+---------------------------------------------------------------------------------------------------------------------------------------+

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Using Huge Pages for Shared Memory
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To use huge pages for shared memory (journal buffers, replication journal pool and global buffers):

* Permit the group used by YottaDB processes to use huge pages with the following command, which requires root privileges:

    .. code-block:: bash

       echo <gid> >/proc/sys/vm/hugetlb_shm_group

.. note::
   The :code:`/proc/sys/vm/hugetlb_shm_group` setting needs to be preserved on reboot, e.g., in :code:`/etc/sysctl.conf` or a startup script.

* Set the environment variable HUGETLB_SHM for each process to "yes".

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Using huge pages for YottaDB process working space
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To use huge pages for process working space and dynamically linked code:

* Set the environment variable HUGETLB_MORECORE for each process to "yes".

Although not required to use huge pages, your application is also likely to benefit from including the path to libhugetlbfs.so in the LD_PRELOAD environment variable.

If you enable huge pages for all applications (by setting HUGETLB_MORECORE, HUGETLB_SHM, and LD_PRELOAD as discussed above in /etc/profile and/or /etc/csh.login), you may find it convenient to suppress warning messages from common applications that are not configured to take advantage of huge pages by also setting the environment variable HUGETLB_VERBOSE to zero (0).

Refer to the documentation of your Linux distribution for details. Other sources of information are:

* http://www.kernel.org/doc/Documentation/vm/hugetlbpage.txt

* http://lwn.net/Articles/374424/

* https://www.ibm.com/developerworks/community/blogs/fe313521-2e95-46f2-817d-44a4f27eba32/entry/backing_guests_with_hugepages?lang=en

* the HOWTO guide that comes with libhugetlbfs (http://sourceforge.net/projects/libhugetlbfs/files/)

.. note::
   Since the memory allocated by Linux for shared memory segments mapped with huge pages is rounded up to the next multiple of huge pages, there is potentially unused memory in each such shared memory segment. You can therefore increase any or all of the number of global buffers, journal buffers, and lock space to make use of this otherwise unused space. You can make this determination by looking at the size of shared memory segments using ipcs. Contact YottaDB support for a sample program to help you automate the estimate. Transparent huge pages may further improve virtual memory page table efficiency. Some supported releases automatically set transparent_hugepages to "always"; others may require it to be set at or shortly after boot-up. Consult your Linux distribution's documentation.

.. _configuring-restriction-facility:

-------------------------------------
Configuring the Restriction Facility
-------------------------------------

Post installation, a system administrator can optionally add a restrict.txt file in $ydb_dist to restrict the use of certain YottaDB facilities to a group-name. The owner and group for $ydb_dist/restrict.txt can be different from those used to install YottaDB. The file may contain zero or more of the following case-insensitive lines in any order:

.. code-block:: none

   APD_ENABLE:[comma-separated-list-of-options]:{path-to-sock-file|host:port}[:tls-id]
   BREAK[:<group-name>]
   CENABLE[:<group-name>]
   DIRECT_MODE[:<group-name>]
   DSE[:<group-name>]
   HALT[:<group-name>]
   LOGDENIALS[:<group-name>]
   PIPE_OPEN[:<group-name>]
   TRIGGER_MOD[:<group-name>]
   ZBREAK[:<group-name>]
   ZCMDLINE[:<group-name>]
   ZEDIT[:<group-name>]
   ZSYSTEM[:<group-name>]

If the file $ydb_dist/restrict.txt does not exist, YottaDB does not restrict any facilities.

Any non-empty lines that do not match the above format cause processes with read-only permissions to behave as if they could not read the file, and YottaDB enforces all restrictions.

Restrictions apply as follows:

+---------------------------------------------------------+----------------------------------------------------------------------------------------------------+
| YottaDB Facility                                        | Behavior                                                                                           |
+=========================================================+====================================================================================================+
| APD_ENABLE                                              | YottaDB supports the ability to log actions initiated from a principal device including M          |
|                                                         | commands typed interactively, or piped in by a script or redirect, from the principal device       |
|                                                         | ($PRINCIPAL) and/or any information entered in response to a READ from $PRINCIPAL. An action       |
|                                                         | initiated from $PRINCIPAL executes as usual when Audit Principal Device is disabled, which it is   |
|                                                         | by default. However, when Audit Principal Device is enabled, YottaDB attempts to send the action   |
|                                                         | out for logging before acting on it. Additionally, the $ZAUDIT Intrinsic Special Variable (ISV)    |
|                                                         | provides a Boolean value that indicates whether Audit Principal Device is enabled. See the Audit   |
|                                                         | Principal Device section below for details.                                                        |
+---------------------------------------------------------+----------------------------------------------------------------------------------------------------+
| BREAK                                                   | YottaDB ignores any break command                                                                  |
+---------------------------------------------------------+----------------------------------------------------------------------------------------------------+
| CENABLE                                                 | the process acts like $ydb_nocenable is TRUE and ignores any CENABLE deviceparameter               |
+---------------------------------------------------------+----------------------------------------------------------------------------------------------------+
| DIRECT_MODE                                             | yottadb -direct terminates immediately with a RESTRICTEDOP error                                   |
+---------------------------------------------------------+----------------------------------------------------------------------------------------------------+
| DSE                                                     | terminates immediately with a RESTRICTEDOP error                                                   |
+---------------------------------------------------------+----------------------------------------------------------------------------------------------------+
| HALT                                                    | any HALT produces a RESTRICTEDOP error                                                             |
+---------------------------------------------------------+----------------------------------------------------------------------------------------------------+
| LOGDENIALS                                              | restrict logging when YottaDB denies access to a process to the named groups                       |
|                                                         |                                                                                                    |
|                                                         | YottaDB supports logging a number of errors related to permissions and access using the syslog()   |
|                                                         | facility. The YottaDB restriction LOGDENIALS provides a facility for disabling this logging on a   |
|                                                         | Unix group basis. If this mechanism is not used, the logging takes place for all YottaDB processes |
|                                                         | If the restriction is used, logging takes place for specified groups only.                         |
+---------------------------------------------------------+----------------------------------------------------------------------------------------------------+
| ZBREAK                                                  | any ZBREAK produces a RESTRICTEDOP error                                                           |
+---------------------------------------------------------+----------------------------------------------------------------------------------------------------+
| ZCMDLINE                                                | YottaDB returns an empty string for all references to $ZCMDLINE                                    |
+---------------------------------------------------------+----------------------------------------------------------------------------------------------------+
| ZEDIT                                                   | any ZEDIT produces a RESTRICTEDOP error                                                            |
+---------------------------------------------------------+----------------------------------------------------------------------------------------------------+
| ZHALT                                                   | any ZHALT produces a RESTRICTEDOP error                                                            |
+---------------------------------------------------------+----------------------------------------------------------------------------------------------------+
| ZSYSTEM                                                 | any ZSYSTEM produces a RESTRICTEDOP error                                                          |
+---------------------------------------------------------+----------------------------------------------------------------------------------------------------+
| PIPE_OPEN                                               | any OPEN of a PIPE device produces a RESTRICTEDOP error                                            |
+---------------------------------------------------------+----------------------------------------------------------------------------------------------------+
| TRIGGER_MOD                                             | any $ZTRIGGER() or MUPIP TRIGGER that attempts a change or delete produces a RESTRICTEDOP error;   |
|                                                         | in addition, while executing code within a trigger, ZBREAK results in a RESTRICTEDOP error, and    |
|                                                         | both ZBREAK and ZSTEP actions are ignored                                                          |
+---------------------------------------------------------+----------------------------------------------------------------------------------------------------+

If the file exists, a process:

* that has write authorization to restrict.txt has no restrictions

* that has no read access to restrict.txt is restricted from all facilities for which YottaDB supports a restriction (currently the above list)

* that has read-only access to restrict.txt is restricted from any listed facility unless it is a member of the group specified in the optional group-id following the facility name

Note that restricting $ZCMDLINE prevents things like: yottadb -run %XCMD 'for read x xecute x' which can act as substitutes for Direct Mode.

In order to limit pathological looping from restricted HALT or ZHALT, if a YottaDB process issues a second occurrence of the restricted command within half a second, the process terminates after sending a fatal error to both the principal device and the syslog, and also produces a YDB_FATAL* context file, but no core file. With these restrictions in place, a process should terminate with, for example: ZGOTO 0. Note that with or without a restriction, executing these commands as part triggered logic on a replicating instance may cause the Update Server to terminate and thereby stop replication.

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
ZSYSTEM and PIPE OPEN command restriction facility
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

The YottaDB restriction mechanism recognizes the following lines:

.. code-block:: none

    ZSYSTEM_FILTER[:M labelref]
    PIPE_FILTER[:M labelref]

The labelref must include a routine name. If a process is restricted by a ZSYSTEM or PIPE_OPEN line in the restrictions file, that restriction takes precedence over the corresponding filter restriction. Otherwise when a process is subject to these restrictions, YottaDB inserts an invocation of the labelref prior to the restricted command, passing a string containing the argument to the ZSYSTEM command or the command deviceparameter of the PIPE OPEN. The path to the filter routine must be included in $zroutines. YottaDB recommends that the filter routine be placed in a location with restricted access such as $ydb_dist. If the filter invocation return is -1, YottaDB produces a RESTRICTEDOP error, otherwise, it executes the command using the returned string via output parameters as a possibly identical replacement for the original string. Note that because ZSYSTEM and OPEN are not Isolated actions, YottaDB recommends against their use within a TP transaction. Filters will also increment the nested level of call-ins. A recursive filter invocation produces a NOFILTERNEST error. YottaDB reports all filter errors to the operator log accompanied by a COMMFILTERERR.

An example restrict file for this:

.. code-block:: bash

   cat $ydb_dist/restrict.txt
   ZSYSTEM_FILTER:^filterzsy
   PIPE_FILTER:^filterzsy

The actual filter routine:

.. code-block:: none

   filterzsy(inarg,outarg);
   if ""=inarg set outarg="-1;must provide a command" quit
   for i=1:1 set arg=$piece(inarg,";",i) quit:""=arg  do  quit:$data(outarg)
   . for  quit:$zchar(9,32)'[$extract(arg)  set arg=$extract(arg,2,9999)
   . set cmd=$piece(arg," ")
   . for restrict="sudo","cd" if cmd=restrict set outarg="-1;command "_restrict_" not permitted" quit
   . quit:$data(outarg)
   . if "echo"=cmd set $piece(arg," ")="echo #",$piece(inarg,";",i)=arg    ;example of modification
   set:'$data(outarg) outarg=inarg
   quit +outarg

 Filter execution starts with $STACK=1 ($ZLEVEL=2).

The commands, Intrinsic Special Variables, and functions whose behavior changes in the context of a filter invocation are below:

* ZGOTO 0 (zero) returns to the processing of the restricted command as does ZGOTO 1 (one) with no entryref, while ZGOTO 1:entryref replaces the originally invoked filter and continues filter execution.
* $ZTRAP/$ETRAP NEW'd at level 1.
* $ZLEVEL initializes to one (1) in GTM$CI, and increments for every new stack level.
* $STACK initializes to zero (0) in GTM$CI frame, and increments for every new stack level.
* $ESTACK NEW'd at level one (1) in GTM$CI frame.
* $ECODE/$STACK() initialized to the empty string at level one (1) in GTM$CI frame.

After the filter completes, YottaDB restores the above to their values at the invocation of the filter.

++++++++++++++++++++++++++++++++++++++++++++
Audit Principal Device restriction facility
++++++++++++++++++++++++++++++++++++++++++++

The "APD_ENABLE" entry in a restrictions definition file turns on APD and enables the logging of all code entered from Direct Mode and optionally any input entered on the principal device ($PRINCIPAL). To enable APD, add a line with the following format to the restriction file:

.. code-block:: none

   APD_ENABLE:[comma-separated-list-of-options]:{path-to-sock-file|host:port}[:tls-id]

* The optional "comma-separated-list-of-options" can consist of zero or more of these options:

  * TLS - Enables TLS connectivity between YottaDB and the logger; this option requires the host information (e.g. IP/port or hostname/port)
  * RD - Enables logging of all responses READ from $PRINCIPAL in addition to that entered at the Direct Mode prompt. This option is more comprehensive and captures input that might be XECUTEd, but depending on your application architecture may significantly increase the amount of logged information.

* The "path-to-sock-file" is the absolute path of the UNIX domain socket file for connecting to the logger.

* The "host" is the hostname or numeric IPv4/IPv6 address of the logger; numeric IP addresses must be enclosed in square brackets (i.e. '[' and ']').

* The "port" is the port number the logger listens on.

* The optional "tls-id" is the label of the section within the YottaDB configuration file that contains TLS options and/or certificates for YottaDB to use; APD ignores any "tls-id" if the "TLS" option is not specified.

If parsing the "APD_ENABLE" line in restriction file or initializing logger information fails, YottaDB enforces all restrictions (default restriction file behavior).

Examples:

.. code-block:: none

   APD_ENABLE::/path/to/sock/file/audit.sock

Adding this line to the restriction file enables APD. YottaDB connects with the logger via UNIX domain socket using the domain socket file "/path/to/sock/file/audit.sock" and sends all Direct Mode activity from $PRINCIPAL to logger.

.. code-block:: none

   APD_ENABLE:RD:[123.456.789.100]:12345

Adding this line to the restriction file enables APD. YottaDB connects with the logger (listening on port 12345 at the IPv4 address 1enable23.456.789.100) via TCP socket and sends all Direct Mode and READ activities from $PRINCIPAL to logger.

.. code-block:: none

   APD_ENABLE::loggerhost:56789

Adding this line to the restriction file enables APD. YottaDB connects with the logger (listening on port 56789 at the hostname "loggerhost") using a TCP socket and sends all Direct Mode activities from $PRINCIPAL to logger.

.. code-block:: none

   APD_ENABLE:TLS,RD:[1234:5678:910a:bcde::f:]:12345:clicert

Adding this line to the restriction file enables APD. YottaDB connects with the logger (listening on port 12345 at the IPv6 address 1234:5678:910a:bcde::f:) via TLS socket. YottaDB configures its TLS options for APD based on the contents within the section of the configuration file labeled "clicert". YottaDB sends all Direct Mode and READ activities from $PRINCIPAL to logger.

~~~~~~~~~~~~~~~~~~~~~~~
Logging
~~~~~~~~~~~~~~~~~~~~~~~

The "logger" is a separate server-like program responsible for receiving the to-be-logged information from YottaDB and logging it. This separate program must be introduced by the user, either running in foreground or background, in order for logging to actually work. YottaDB distributions include basic example logger programs.

The six fields in the message, separated by semicolons (';'), contain information on the to-be-logged activity. Each to-be-logged message sent to the logger from YottaDB has the following format:

.. code-block:: none

   dist=<path>; src={0|1|2}; uid=<uid>; euid=<euid>; pid=<pid>; command=<text>


* The "dist" field, shows the path to location of the sender/user's $ydb_dist (YottaDB executables).
* The "src" field shows zero (0) for input from unknown source, one (1) for Direct Mode input, or two (2) for READ input from $PRINCIPAL.
* The next three fields ("uid", "euid", and "pid") show (respectively) decimal representations of the user ID, effective user ID, and process ID of the process that sent the message.
* The "command" field is the input provided on the YottaDB side.

Examples:

.. code-block:: none

   dist=/path/to/ydb_dist; src=1; uid=112233445; euid=112233445; pid=987654; command=write "Hello world",!
   dist=/usr/local/lib/yottadb/r132; src=2; uid=998877665; euid=998877665; pid=123456; command=set a=789

Click `here <https://gitlab.com/YottaDB/DB/YDBDoc/blob/master/AdminOpsGuide/dm_audit_listener.zip>`_ to download sample listener programs.
