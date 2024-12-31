.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2024-2025 YottaDB LLC and/or its subsidiaries.#
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

============
YDB Support
============

.. contents::
   :depth: 5

--------
Overview
--------

When you contact your YottaDB support channel, they will often ask for metadata to establish context around the issue for which you want support. The YDBSupport plugin is a shell script that captures general metadata about the system and database that you can submit to your YottaDB support channel along with your support request. Submitting it with your support request may get an answer sooner since your support channel team may not need to come back with a request for metadata. The script works with both YottaDB as well as the upstream GT.M.

- If `$ydb_dist <../AdminOpsGuide/basicops.html#ydb-dist>`_, or failing that, $gtm_dist, is set, it uses that as the location of the YottaDB/GT.M installation. If neither are set, it checks whether the script is installed in the ``plugin`` subdirectory of a YottaDB/GT.M installation. If that is also not the case, it looks for a ``/usr/share/pkgconfig/yottadb.pc`` file. If it cannot find a YottaDB/GT.M installation, it reports system metadata that does not rely on access to YottaDB/GT.M.
- With a YottaDB installation, it reports both `$ZYRELEASE <../ProgrammersGuide/isv.html#zyrelease-isv>`_ and `$ZVERSION <../ProgrammersGuide/isv.html#zversion>`_. With a GT.M installation, it only reports $ZVERSION.
- If `$ydb_gbldir <../AdminOpsGuide/basicops.html#ydb-gbldir>`_, or failing that, $gtmgbldir, is set it reports metadata about the database. If neither is set, it reports metadata about the system and the YottaDB release / GT.M version.
- If run as root, the system metadata includes the output of the ``dmesg`` command; if run as a normal user, this is not available. Running as root also provides more diagnostic information about processes and core files.
- In the environment variables that it captures and reports, it omits any environment variable whose name includes the case-insensitive substrings ``key``, ``passp``, or ``passw``, in order to avoid inadvertently reporting environment variables that may contain sensitive information.

The preferred way to run the script is ``sudo -E <path_to_script>/ydb_support.sh``. The ``-E`` command line option passes the environment variables to the ``sudo`` script. Without ``sudo``, you can run it as ``<path_to_script>/ydb_support.sh`` to get metadata without ``dmesg`` output. You can use the ``-h`` / ``--help`` option for a full list of command line options:

.. code:: bash

   $ $ydb_dist/plugin/ydb_support.sh -h
   Usage:
   ./ydb_support.sh [-f|--force] [-o|--outdir OUTPUT DIRECTORY]
   sudo -E ./ydb_support.sh [-f|--force] [-o|--outdir OUTPUT DIRECTORY]
     [-p|--pid "PID OR CORE FILE"] [-h|--help]
     [-l|--logs-since "JOURNALCTL TIME FORMAT"]
     [-u|--logs-until "JOURNALCTL TIME FORMAT"]
     [-n|--no-logs]

   where:
     -f|--force removes the output directory if it exists before starting, else an error will be emitted
     -o|--outdir <directory> the output directory to store files in before compressing
	   DEFAULT ydb_support
     -p|--pid <pid or core> the PID or core file of a YDB/GTM process to get information from
     -h|--help displays this message
     -l|--logs-since <time spec> passed to journalctl (if present) to control starting time of logs
	 DEFAULT: 2 hours ago
     -u|--logs-until <time spec> passed to journalctl (if present) to control topping time of logs
	 DEFAULT: now
     -n|--no-logs if present, no information from the system beyond what is required for processing -p is collected

   Running as root with sudo also captures dmesg output; running as a normal user does not. When
   running with sudo, the -E option is important to capture environment variables.
   $ 


Here is an example of running it, as a normal user:

.. code:: none

   $ $ydb_dist/plugin/ydb_support.sh -p 33386 -o /tmp/support
   ## Gathering system information
   ## Gathering environment variables, omitting keys, passphrases, and passwords
   ## Gathering system logs
   ## YottaDB/GT.M distribution is at /extra/usr/local/lib/yottadb/r201
   ## Gathering information about the database
   ## Getting filesystem information
   ## Analyzing active process
   ## Done getting information, packing tarball
   tar: Removing leading `/' from member names
   ## Done! Please review the files in /tmp/support to make sure that they only contain metadata
   ## that can be sent. If not, please edit them as needed, and run the command
   ##   tar -czf /tmp/support.tar.gz /tmp/support
   ## to recreate /tmp/support.tar.gz. Send /tmp/support.tar.gz and a description of your problem to your
   ## YottaDB support channel, as well as the severity (impact), scope, and timeframes. You can
   ## remove the directory /tmp/support afterwards. Thank you.
   $

The directory ``/tmp/support`` contains several files:

.. code:: bash

   $ ls -l /tmp/support
   total 288
   -rw-r--r-- 1 ydbuser ydb    457 May  7 12:07 df.txt
   -rw-r--r-- 1 ydbuser ydb     43 May  7 12:07 dmesg.log
   -rw-r--r-- 1 ydbuser ydb  32761 May  7 12:07 dse_all_dump_all.txt
   -rw-r--r-- 1 ydbuser ydb   7225 May  7 12:07 env.txt
   -rw-r--r-- 1 ydbuser ydb   2337 May  7 12:07 gdb_33386.txt
   -rw-r--r-- 1 ydbuser ydb   3992 May  7 12:07 gde_show_command.txt
   -rw-r--r-- 1 ydbuser ydb     88 May  7 12:07 global_dir.txt
   -rw-r--r-- 1 ydbuser ydb  47043 May  7 12:07 journalctl.log
   -rw-r--r-- 1 ydbuser ydb    745 May  7 12:07 lsblk.txt
   -rw-r--r-- 1 ydbuser ydb    144 May  7 12:07 lsb_release.txt
   -rw-r--r-- 1 ydbuser ydb   3132 May  7 12:07 lscpu.txt
   -rw-r--r-- 1 ydbuser ydb    320 May  7 12:07 lsmem.txt
   -rw-r--r-- 1 ydbuser ydb    641 May  7 12:07 mtab.txt
   -rw-r--r-- 1 ydbuser ydb 143461 May  7 12:07 mupip_dumpfhead.txt
   -rw-r--r-- 1 ydbuser ydb    469 May  7 12:07 os-release
   -rw-r--r-- 1 ydbuser ydb    125 May  7 12:07 uname.txt
   -rw-r--r-- 1 ydbuser ydb    105 May  7 12:07 zversion.txt
   -rw-r--r-- 1 ydbuser ydb    106 May  7 12:07 zyrelease.txt
   $ 

The files are as follows:

- ``df.txt`` contains the ``df`` output for filesystems other than loop devices. A not-uncommon reason for issues is unmonitored filesystems running out of space.
- ``dmesg.log`` would have contained the ``dmesg`` output had the script been run as root. In this case, the file notes that it requires root to run, and reports the userid used to run the script.
- Both ``dse_all_dump_all.txt`` and ``mupip_dumpfhead.txt`` contain database fileheader information. The information in both files is the same, but the former is in human-readable format whereas the latter is more easily consumed by software.
- ``env.txt`` reports the envionment variables, except those potentially containing keys, passphrases, and passwords, as noted above.
- ``gdb_33386.txt`` shows the output from running ``gdb`` on process ``33386``. When run as a non-root userid, ``gdb``  may show less output than when run as root.
- ``global_dir.txt`` is the output of `GDE SHOW COMMAND <../AdminOpsGuide/gde.html#show>`_; importantly, how the globals are distributed across database regions.
- ``journalctl.log`` is ``journalctl`` output , showing syslog entries.
- ``lsblk.txt``, ``lscpu.txt`` and ``lsmem.txt`` are the outputs of the ``lsblk``, ``lscpu`` and ``lsmem`` commands, with information about system block devices, CPUs, and RAM.
- ``lsb_release.txt``, ``os-release`` and ``uname.txt`` provide information about the operating system distribution and version.
- ``mtab.txt`` is the output ``/etc/mtab`` on mounted filesystems except loop devices.
- ``zversion.txt`` is the output of $ZVERSION, and for YottaDB releases, ``zyrelease.txt`` is the outpyt of $ZYRELEASE.

We suggest that you download ``ydb_support.sh`` and run it to ensure that you are comfortable with the metadata it is collecting. If not, please adapt it as befits your situation.

.. raw:: html

    <img referrerpolicy="no-referrer-when-downgrade" src="https://download.yottadb.com/plugins.png" />
