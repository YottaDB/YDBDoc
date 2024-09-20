.. ###############################################################
.. #								 #
.. # Copyright (c) 2023-2024 YottaDB LLC and/or its subsidiaries.#
.. # All rights reserved.					 #
.. #								 #
.. #	 This document contains the intellectual property	 #
.. #	 of its copyright holder(s), and is made available	 #
.. #	 under a license.  If you do not know the terms of	 #
.. #	 the license, please stop and do not read further.	 #
.. #								 #
.. ###############################################################

==============
YDB Syslog
==============

.. contents::
   :depth: 3

--------------
Overview
--------------

YDBSyslog is a YottaDB plugin to capture syslog data in a YottaDB database, to allow for more sophisticated analytics, forensics, and troubleshooting, for example by using Octo. Furthermore, by consolidating the syslogs of several systems in a single database, queries can run on data that cuts across multiple systems, e.g., to investigate concurrent events.

It operates in two modes to ingest data in the :code:`journalctl --output=export` format:

* By running :code:`journalctl --follow` in a PIPE device, YDBSyslog can continuously ingest syslog entries in real time.

* Reading a :code:`journalctl` export from stdin. Reading from :code:`journalctl --output=export --follow` in a pipe is effectively the same as reading from a PIPE device using the :code:`--follow` option.

YDBSyslog can output a DDL which Octo will accept, allow the syslog databaase to be queried using SQL.

.. _quickstartydbsyslog:

--------------
Quickstart
--------------

As a YottaDB plugin, YDBSyslog requires YottaDB. You can install YottaDB and YDBSyslog together:

.. code:: bash

   mkdir /tmp/tmp ; wget https://gitlab.com/YottaDB/DB/YDB/raw/master/sr_unix/ydbinstall.sh
   cd /tmp/tmp ; chmod +x ydbinstall.sh
   sudo ./ydbinstall.sh --utf8 --syslog

Although you can omit the ``--utf8`` option if you do not want UTF-8 support installed, we recommend installing UTF-8 support as syslogs can include UTF-8 characters. If you already have YottaDB installed, use ``sudo $ydb_dist/ydbinstall --syslog --plugins-only --overwrite-existing`` to install or reinstall the YDBSyslog plugin without reinstalling YottaDB.

--------------
Installation
--------------

If you don't use the :ref:`quickstart` method, you can install YDBSyslog from source. In addition to YottaDB and its requirements, YDBSyslog requires ``cmake``, ``git``, ``make``, and ``pkg-config``. Clone the YDBSyslog repository, and then install the plugin, using the following commands:

.. code:: bash

   git clone https://gitlab.com/YottaDB/Util/YDBSyslog.git YDBSyslog-master
   cd YDBSyslog-master
   mkdir build && cd build
   cmake ..
   make && sudo make install

--------------
Usage
--------------

The most common usage of YDBSyslog is to run %YDBSYSLOG from the shell.

.. code:: bash

   yottadb -run %YDBSYSLOG op [options]


Where :code:`op` and :code:`[options]` are:

* :code:`help` - Output options to use this program.

* :code:`ingestjnlctlcmd [options]` - Run the :code:`journalctl --output=export` command in a PIPE. Options are as follows; all options may be omitted.

  * :code:`--boot [value]` - :code:`--boot` is mutually exclusive with :code:`--follow`. There are several cases of :code:`value`:

    #. If omitted, the :code:`--boot` parameter is omitted when invoking :code:`journalctl`. This ingests the syslog from the current boot.
    #. If a hex string prefixed with :code:`0x`, the string sans prefix is passed to :code:`journalctl --boot`.
    #. If a decimal number, it is passed unaltered to :code:`journalctl --boot`.
    #. If a case-independent :code:`all`, that option is passed to :code:`journalctl --boot`.

  * :code:`--follow` is mutually exclusive with :code:`--boot`. The :code:`--follow` option is used to invoke :code:`journalctl --follow`, and results in %YDBSYSLOG running as a daemon to continuously ingest the syslog exported by :code:`journalctl`.

  * :code:`--moreopt` indicates that the rest of the command line should be passed verbatim to the :code:`journalctl` command as additional options. See the Linux command :code:`man journalctl` for details. YDBSyslog does no error checking of these additional options.

* :code:`ingestjnlctlfile` – read :code:`journalctl --output=export` formatted data from stdin.

* :code:`octoddl` - output an Octo DDL to allow analysis of syslog data using SQL. If the database combines syslog data from multiple systems, Octo SQL queries can span systems.

The following M entryrefs can called directly from programs.

* :code:`INGESTJNLCTLCMD^%YDBSYSLOG(boot,follow,moreopt)` runs :code:`journalctl --output=export` in a PIPE device. Parameters are:

  * :code:`boot` is the parameter for the :code:`--boot` command line option of :code:`journalctl`. There are several cases:

    #. If unspecified or the empty string, the :code:`--boot` option is omitted.
    #. If a hex string prefixed with :code:`"0x"`, the string sans prefix is passed to :code:`journalctl` as the value.
    #. If a decimal number, it is passed unaltered to :code:`journalctl`.
    #. If a case-independent :code:`"all"`, that option is passed to :code:`journalctl`.

  * If :code:`follow` is non-zero, INGESTJNLCTLCMD follows journalctl, continuously logging syslog output in the database. :code:`boot` and :code:`follow` are mutuially exclusive.

  * :code:`moreopt` is a string intended to be passed verbatim to the journalctl command. See the Linux command :code:`man journalctl` for details. INGESTJNLCTMCMD does no error checking of these additional options.

* :code:`INGESTJNLCTLFILE^%YDBSYSLOG` reads :code:`jnlctl --output=export` formatted data from stdin.

* :code:`OCTODDL^%YDBSYSLOG([scanflag])` generates the DDL that can be fed to Octo to query the ingested syslog data using SQL. If :code:`scanflag` evaluates to 1, the routine scans the database for additional fields beyond those indentified in the code.

Data are stored in nodes of :code:`^%ydbSYSLOG` with the following subscripts, which are reverse engineered from the :code:`__CURSOR` field of the :code:`journalctl` export format. While :code:`__CURSOR` is documented as opaque, reverse engineering provides a more compact database and faster access:

* :code:`Cs` – a UUID for a large number of syslog records.
* :code:`Cb` – evidently a boot UUID.
* :code:`Ci` - evidently the record number in a syslog.
* :code:`Ct` - evidently the number of microseconds since the UNIX epoch.
* :code:`Cm` – evidently a monolithic timestamp since boot.
* :code:`Cx` - a UUID that is unique to each syslog entry.

Fields that :code:`journalctl` has been found to flag as binary, e.g., :code:`"MESSAGE"` and :code:`"SYSLOG_RAW"` have an additional, seventh, subscript, the tag for the field.

Note that since querying syslog entries is content based (e.g., the USER_ID field) and not by the subscripts, if the reverse engineering of :code:`__CURSOR` is imperfect, or if a future :code:`systemd-journald` changes the fields, it will not affect the correctness of queries; it will only incrementally increase database size and consequently access speed (smaller databases are marginally faster).

The numerous fields exported by :code:`journalctl` are not well documented. `Systemd Journal Export Formats <https://systemd.io/JOURNAL_EXPORT_FORMATS/>`_ is helpful, as is `man systemd.journal-fields <https://www.freedesktop.org/software/systemd/man/systemd.journal-fields.html>`_. However, outside the source code, there does not appear to be a comprehensive list of all fields. The fields listed in the :code:`_YDBSYSLOG.m` source code were captured from a couple dozen Linux systems running releases and derivatives of Arch Linux, Debian GNU/Linux, Red Hat Enterprise Linux, SUSE Linux Enterprise, and Ubuntu. Even if :code:`journalctl` exports additional fields not identified, %YDBSYSLOG captures them, and generates reasonable DDL entries for them.

Should you find additional entries not identified by the :code:`_YDBSYSLOG.m` source code, please create an Issue or a Merge Request `in the YottaDB project <https://gitlab.com/YottaDB/DB>`_.

++++++++++++++++++++++++++++++
Syslog from multiple systems
++++++++++++++++++++++++++++++

Although there are many ways to script gathering data from multiple systems using %YDBSYSLOG, the program UseYDBSyslog is a sample script you can use. After reading the comments in the file `UseYDBSyslog.txt <https://gitlab.com/YottaDB/Util/YDBSyslog/-/raw/master/UseYDBSyslog.txt>`_:

#. Edit the file :code:`UseYDBSyslog.txt` to replace the sample loghost name, server names, and starting TCP port with the specific values for your environment.
#. Save the file as :code:`UseYDBSyslog.m` on the loghost and on each server in a location where YottaDB can execute it.
#. To use it, first start it on the loghost, and then on each server, and confirm that the two port numbers reported by the loghost for each server match those the server reports.
#. To collect all syslogs from all servers, intially, start it with :code:`yottadb -run %XCMD 'do ^UseYDBSyslog(1)'`. Subsequently, a simple :code:`yottadb -run UseYDBSyslog` suffices to capture syslogs from the current boot.
#. To collect all syslogs from all servers starting at a specific time, pass the time as the third parameter, e.g., :code:`yottadb -run %XCMD 'do ^UseYDBSyslog(,,,"--since=""2023-08-13 14:04""")'`.

The default configuration of UseYDBSyslog creates an unjournaled database that uses the MM access method. If you use journaling for recoverability, remember to monitor space used by prior generation journal files, and to delete those old journal files when they are no longer needed.


.. raw:: html

    <img referrerpolicy="no-referrer-when-downgrade" src="https://download.yottadb.com/plugins.png" />
