.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.     #
.. # All rights reserved.                                        #
.. #                                                             #
.. #     This source code contains the intellectual property     #
.. #     of its copyright holder(s), and is made available       #
.. #     under a license.  If you do not know the terms of       #
.. #     the license, please stop and do not read further.       #
.. #                                                             #
.. ###############################################################

.. index::
   MUPIP BACKUP

=================
5.1 MUPIP BACKUP
=================

.. contents::
   :depth: 3

Saves the contents of the database. It provides a consistent application snapshot across all database regions involved in the backup operation.
The format of the MUPIP BACKUP command is:

.. code-block:: none

   B[ACKUP]
   [
    -BK[UPDBJNL]={DISABLE|OFF}]
    -B[YTESTREAM] [-NET[TIMEOUT]]
    -DA[TABASE]
    -[NO]NEWJNLFILES[=[NO]PREVLINK],[NO]S[YNC_IO]]
    -O[NLINE]
    -REC[ORD]
    -REPL[ACE]
    -REPLINSTANCE=target_location
    -S[INCE]={DATABASE|BYTESTREAM|RECORD}
    -T[RANSACTION]=hexadecimal_transaction_number
   ] region-list[,...] destination-list

.. note::
   MUPIP BACKUP does a more comprehensive job of managing backup activities than other backup techniques such as a SAN backup, disk mirroring, or a file system snapshot because it integrates journal management, instance file management, and records timestamps in the database file headers. To use other techniques, you must first freeze all regions concurrently with a command such as MUPIP FREEZE -ON "*" in order to ensure a consistent copy of files with internal structural integrity. YottaDB neither endorses nor tests any third party products for backing up a YottaDB database.

* MUPIP BACKUP supports two methods of database backup: -BYTESTREAM and -DATABASE. MUPIP BACKUP -BYTESTREAM directs the output to a broad range of devices, including disks, TCP sockets, and pipes. MUPIP BACKUP -DATABASE directs the output to random access devices (i.e., disks).

* [NO]ONLINE qualifier determines whether MUPIP BACKUP should suspend updates to regions. For example, MUPIP BACKUP -NOONLINE suspends updates to all regions from the time it starts the first region until it finishes the last region. However, it does not suspend processes that only read from the database.

* By default, MUPIP BACKUP is -DATABASE -ONLINE.

* If any region name does not map to an existing accessible file, or if any element of the destination list is invalid, BACKUP rejects the command with an error.

* region-list may specify more than one region of the current global directory in a list. Regions are case-insensitive, separated by a comma, and wildcards can be used to specify them. Any region-name may include the wildcard characters \* and % (remember to escape them to protect them from inappropriate expansion by the shell). Any region name expansion occurs in M (ASCII) collation order.

* Depending on the type of backup, destination-list may be a single directory, or a comma separated list of destinations including files, piped commands, or a TCP socket address (a combination of IPv4 or IPV6 hostname and a port number).

* Region-list and destination-list items are matched in order - the first region is mapped to the first destination, the second to the second destination, and so on. If YottaDB encounters a region mapped to a directory, YottaDB treats that directory as the destination for all subsequent regions in the region-list.

* YottaDB implicitly timestamps both BYTESTREAM and DATABASE backups using relative timestamps (transaction numbers). You can also explicitly specify a RECORD timestamp for custom-control (SANS or mirrored disk) backup protocol. You might want to use these timestamps as reference points for subsequent backups.

* It takes approximately one (1) minute (per region) for BACKUP -ONLINE to give up and bypass a KILLs in progress; backup does not wait for Abandoned Kills to clear.

* The environment variable ydb_baktmpdir specifies the directory where mupip backup creates temporary files. If ydb_baktmpdir is not defined, YottaDB uses the deprecated GTM_BAKTMPDIR environment variable, if defined, and otherwise uses the current working directory.

* When you restrict access to a database file, YottaDB propagates those restrictions to shared resources associated with the database file, such as semaphores, shared memory, journals and temporary files used in the course of MUPIP BACKUP.

* YottaDB supports only one concurrent -ONLINE backup on a database. MUPIP BACKUP displays the BKUPRUNNING message if started when there is an already running BACKUP.

* MUPIP BACKUP protects against overwriting of existing destination files. However, it cannot protect other destinations, for example, if the destination is a pipe into a shell command that overwrites a file.

------------------------------
Before Starting a MUPIP Backup
------------------------------

Perform the following tasks before you begin a database backup.

* Ensure adequate disk space for target location and temporary files. Set the environment variable ydb_baktmpdir to specify the directory where MUPIP BACKUP creates temporary files. If ydb_baktmpdir is not defined, YottaDB uses the deprecated GTM_BAKTMPDIR environment variable if defined, and otherwise uses the current working directory. Do not place temporary files in the current directory for large databases in production environments.

* When using replication, ensure that the Source/Receiver process is alive (MUPIP REPLIC -SOURCE/-RECEIVER -CHECKHEALTH). Always backup the replicating instance file with the database (BACKUP -REPLINST).

* If you intend to use a -DATABASE backup at the same time in the same computer system as the source database, be sure to disable journaling in the backed up database with -BKUPDBJNL=DISABLE.

* When doing a complete backup, switch journal files as part of the backup command using -NEWJNLFILES=NOPREVLINK. This aligns the journal files with the backup and simplifies journal file retention. Use the NOPREVLINK option for this qualifier with caution if the original database is used for replication. If the link to the previous generation journal file is cut, then the source server cannot supply transactions from the prior generation journal files.

* If you follow separate procedures for backup and archival (moving to secondary storage), you can save time by starting archival as soon as MUPIP BACKUP completes the process of creating a backup database file for a region. You do not need to wait for MUPIP BACKUP to complete processing for all regions before starting archival. For example, a message like:

.. code-block:: bash

   DB file /home/jdoe/.yottadb/r1.10/g/ydb.dat backed up in file /backup/ydb.dat
   Transactions up to 0x0000000000E92E04 are backed up.

confirms that ydb.dat is backed up correctly and is ready for archival.

* Determine an appropriate frequency, timing, and backup method (-BYTESTREAM or -DATABASE) based on the situation.

* Ensure the user issuing backup commands has appropriate permissions before starting the backup. Backup files have the ownership of the user running MUPIP BACKUP.

* There is one circumstance under which a MUPIP BACKUP is not advised.  When your operational procedures call for taking backups of unmodified databases and journal files when rebooting a system after a crash, use an underlying operating system command (cp, cpio, gzip, tar, and so on) which will open the files read-only.  Note that for ordinary system crashes where the system simply stops writing to open files at power down, you can use MUPIP JOURNAL to recover journaled database files, and taking backups on reboot should not be required.  However, for system crashes with the possibility of damage to files already written to disk (for example, if the crash involved an IO controller with the potential for having written random data to disk immediately prior to power down), such backups on reboot are appropriate.

Example:

.. code-block:: bash

   $ mupip backup "*" /ydb/bkup

This example creates ready-to-run database backup of all regions.

------------------
BACKUP Qualifiers
------------------

.. _mupip-backup-bkupdbjnl:

++++++++++
-BKupdbjnl
++++++++++

A backup database shares the same journaling characteristics of the source database. However, with BKUPDBJNL you can disable or turn off journaling in the backup database. Use this qualifier if you intend to open your backup database at the same time in the same environment as the source database.

The format of the BKUPDBJNL qualifier is:

.. code-block:: none

   -BK[UPDBJNL]={DISABLE|OFF}

* Specify DISABLE to disable journaling in the backup database.

* Specify OFF to turn off journaling in the backup database.

* Only one of the qualifiers DISABLE or OFF can be specified at any given point.

+++++++++++
-Bytestream
+++++++++++

Transfers MUPIP BACKUP output to a TCP connection, file (or a backup directory), or a pipe. If there are multiple .dat files, BYTESTREAM transfers output to a comma separated list of TCP connections, incremental backup files and/or directories, or pipes. When used with -SINCE or -TRANSACTION, MUPIP BACKUP allows incremental backup, that is, includes database blocks that have changed since a prior point specified by the -SINCE or -TRANSACTION.

.. note::
   MUPIP BACKUP output to a TCP connection saves disk I/O bandwidth on the current system.

All bytestream backups needs to be restored to a random access file (with MUPIP RESTORE) before being used as a database file. -BYTESTREAM can also send the output directly to a listening MUPIP RESTORE process via a TCP/IP connection or a pipe.

The format of the BYTESTREAM qualifier is:

.. code-block:: none

   -B[YTESTREAM]

* -BYTESTREAM is compatible with -SINCE and -TRANSACTION.

* -INCREMENTAL is deprecated in favor of -BYTESTREAM. For upward compatibility, MUPIP temporarily continues to support the deprecated -INCREMENTAL

++++++++++
-Database
++++++++++

Creates a disk-to-disk backup copy of the files of all selected regions. DATABASE backup copy is a ready-to-use YottaDB database unlike BYTESREAM backup which is required to be restored to a random access file.

The format of the DATABASE qualifier is:

.. code-block:: none

   -D[ATABASE]

* By default, MUPIP BACKUP uses -DATABASE.

* The DATABASE qualifier is only compatible with the -[NO]NEW[JNLFILES], -ONLINE, and -RECORD qualifiers.

* -COMPREHENSIVE is deprecated in favor of -DATABASE. For upward compatibility, MUPIP temporarily continues to support the deprecated -COMPREHENSIVE.

++++++++++++
-NETtimeout
++++++++++++

Specifies the timeout period when a bytestream BACKUP data is sent over a TCP/IP connection. The format of the NETTIMEOUT qualifier is:

.. code-block:: none

   NET[TIMEOUT]=seconds

* The default value is 30 seconds.

* Use only with -BYTESTREAM and RESTORE.

+++++++++++++
-NEWJNLFILES
+++++++++++++

Determines the journaling characteristics of the database files being backed-up. All the established journaling characteristics apply to new journal files. This qualifier is effective only for an ONLINE backup (the default), when the database has journaling enabled.

The format of the NEWJNLFILES qualifier is:

.. code-block:: none

   -[NO]NEWJNLFILES[=[NO]PREVLINK], [NO]S[YNC_IO]]

* -NEWJNLFILES can take the following three values:

  * PREVLINK: Back links new journal files with the journal files of the prior generation. This is the default value.
  * NOPREVLINK: Indicates that there should be no back link between the newly created journals and the journal files of the prior generation.
  * SYNC_IO: Specifies that every WRITE to a journal file is to be committed directly to disk. On high-end disk subsystems (for example, those that include a non-volatile cache and consider the data to be committed when it reaches this cache), this might result in a better performance than the NOSYNC_IO option. (NOSYNC_IO turns off this option).

* -NONEWJNLFILES causes journaling to continue with the current journal files. It does not accept any arguments.

* The default is -NEWJNLFILES=PREVLINK.

++++++++
-Online
++++++++

Specifies that while a MUPIP BACKUP operation is active, other processes can update the database without affecting the result of the backup. The format of the ONLINE qualifier is:

.. code-block:: none

   -[NO]O[NLINE]

* MUPIP BACKUP -ONLINE creates a backup of the database as of the moment the backup starts. If running processes subsequently update the database, the backup does not reflect those updates.

* MUPIP BACKUP -ONLINE on region(s) waits for up to one minute so any concurrent KILL or MUPIP REORG operations can complete. If the KILL or MUPIP REORG operations do not complete within one minute, MUPIP BACKUP -ONLINE starts the backup with a warning that the backup may contain incorrectly marked busy blocks. Such blocks waste space and can desensitize operators to much more dangerous errors, but otherwise don't affect database integrity. If you get such an error, it may be better to stop the backup and restart it when KILL or MUPIP REORG operations are less likely to interfere. Performing MUPIP STOP on a process performing a KILL or MUPIP REORG operation may leave the database with incorrectly marked busy blocks. In this situation, YottaDB converts the ongoing KILLs flag to an Abandoned KILLs flag. If MUPIP BACKUP -ONLINE encounters ADANDONED_KILLS, it gives a message and then starts the backup. An ABANDONED_KILLS error means that both the original database and the backup database possibly have incorrectly busy blocks which should be corrected promptly.

* By default, MUPIP BACKUP is -ONLINE.

++++++++
-Record
++++++++

Timestamps (in the form of a transaction number) a database file to mark a reference point for subsequent bytestream, database, or custom backup (SANS or disk mirror) protocols. Even though -DATABASE and -BYTESTREAM both mark their own relative timestamps, -RECORD provides an additional timestamp option. MUPIP FREEZE also provides the -RECORD qualifier because a FREEZE may be used to set the database up for a SAN or disk-mirror based backup mechanism.

The format of the RECORD qualifier is:

.. code-block:: none

   -R[ECORD]

* Use -RECORD (with the hyphen) to timestamp a reference point and use RECORD as a keyword (as in -SINCE=RECORD) to specify the starting point for a MUPIP BACKUP operation.

* -RECORD replaces the previously RECORDed transaction identifier for the database file.

+++++++++
-REPlace
+++++++++

Overwrites the existing destination files.

The format of the REPLACE qualifier is:

.. code-block:: none

   -[REPL]ACE

* By default, MUPIP BACKUP protects against overwriting the destination files. -REPLACE disables this default behavior.

* -REPLACE is compatible only with -DATABASE.

++++++++++++++
-REPLInstance
++++++++++++++

Specifies the target location to place the backup of the replication instance file.

.. note::
   The replication instance file should always be backed up with the database file. The source server for the instance must be started at least once before backing up the replication instance file.

The format of the REPLINSTANCE qualifier is:

.. code-block:: none

   -REPLI[NSTANCE]=<target_location>

+++++++
-Since
+++++++

Includes blocks changed since the last specified backup. The format of the SINCE qualifier is:

.. code-block:: none

   -S[ince]={Database|Bytestream|Record}

* D[atabase] - Backup all changes since the last MUPIP BACKUP DATABASE.

* B[ytestream] - Backup all changes since the last MUPIP BACKUP BYTESTREAM.

* R[ecord] - Backup all changes since the last MUPIP BACKUP RECORD.

By default, MUPIP BACKUP BYTESTREAM operates as :code:`-since=database`.

Incompatible with: TRANSACTION.

++++++++++++
-Transaction
++++++++++++

Specifies the transaction number of a starting transaction that causes BACKUP -BYTESTREAM to copy all blocks that have been changed by that transaction and all subsequent transactions. The format of the TRANSACTION qualifier is:

.. code-block:: none

   -T[RANSACTION]=transaction-number

* A Transaction number is always a 16 digit hexadecimal number. It appears in a DSE DUMP -FILEHEADER with the label "Current transaction".

* If the transaction number is invalid, MUPIP BACKUP reports an error and rejects the command.

* It may be faster than a DATABASE backup, if the database is mostly empty.

* Incompatible with: -DATABASE, -SINCE.

.. note::
   A point in time that is consistent from an application perspective, is unlikely to have the same transaction number in all database regions. Therefore, except for -TRANSACTION=1, this qualifier is not likely to be useful for any backup involving multiple regions.

-------------------------
Examples for MUPIP BACKUP
-------------------------

Example:

.. code-block:: bash

   $ mupip backup -bytestream MAMMALS,CRUSTACEANS bkup

Suppose that the environment variable ydb_gbldir has regions MAMMALS and CRUSTACEANS that map to files called LINNAEUS.DAT and BRUNNICH.DAT (no matter which directory or directories the files reside in). Then the above example creates bytestream backup files MAMMALS.DAT and CRUSTACEANS.DAT in the bkup directory since the last DATABASE backup.

Example:

.. code-block:: bash

   $ mupip backup -bkupdbjnl="OFF" "*"

This command turns off journaling in the backup database.

Example:

.. code-block:: bash

   $ mupip backup -bytestream "*" tcp://philadelphia:7883,tcp://tokyo:8892

Assuming a Global Directory with two regions pointing to ACN.DAT and HIST.DAT, this example creates a backup of ACN.DAT to a possible MUPIP RESTORE process listening at port 7883 on server philadelphia and HIST.DAT to a possible MUPIP RESTORE process listening at port 8893 on server tokyo.

Always specify the <machine name> and <port> even if both backup and restore are on the same system, and ensure that the MUPIP RESTORE process is started before the MUPIP BACKUP process.

Example:

.. code-block:: bash

   $ mupip backup -database -noonline "*" bkup
   DB file /home/ydbnode1/yottadbuser1/yottadb.dat backed up in file bkup/yottadb.dat
   Transactions up to 0x00000000000F42C3 are backed up.
   BACKUP COMPLETED.

This command creates a disk-to-disk backup copy of all regions of the current database in directory bkup. YottaDB freezes all the regions during the backup operation.

Example:

.. code-block:: bash

   $ mupip backup -bytestream -nettimeout=420 DEFAULT tcp://${org_host}:6200

This command creates a backup copy of the DEFAULT region with timeout of 420 seconds.

Example:

.. code-block:: bash

   $ mupip backup -bytestream DEFAULT '"| gzip -c > online5pipe.inc.gz"'

This command sends (via a pipe) the backup of the DEFAULT region to a gzip command.

Example:

.. code-block:: bash

   $ mupip backup -online DEFAULT bkup
   DB file /ydbnode1/yottadbuser1/yottadb.dat backed up in file bkup/yottadb.dat
   Transactions up to 0x00000000483F807C are backed up.
   BACKUP COMPLETED.

This command creates a backup copy of the DEFAULT region of the current database in directory bkup. During the backup operation, other processes can read and update the database.

Example:

.. code-block:: bash

   $ mupip backup -record DEFAULT bkup

This command sets a reference point and creates a backup copy of the DEFAULT region of the current database in directory bkup.

Example:

.. code-block:: bash

   $ mupip backup -online -record DEFAULT bkup1921
   DB file /home/mammals/yottadb.dat backed up in file bkup1921/yottadb.dat
   Transactions up to 0x00000000000F4351 are backed up.

Example:

.. code-block:: bash

   $ mupip backup -bytestream -since=record DEFAULT bkup1921onwards
   MUPIP backup of database file /home/mammals/yottadb.dat to bkup1921onwards/yottadb.dat
   DB file /home/mammals/yottadb.dat incrementally backed up in file bkup1921onwards/yottadb.dat
   6 blocks saved.
   Transactions from 0x00000000000F4351 to 0x00000000000F4352 are backed up.
   BACKUP COMPLETED.

The first command sets a reference point and creates a backup copy of the DEFAULT region of the current database in directory bkup1921. The second command completes a bytestream backup starting from the reference point set by the first command.

Example:

.. code-block:: bash

   $ mupip backup -bytestream -transaction=1 DEFAULT bkup_dir
   MUPIP backup of database file /ydbnode1/yottadbuser1/yottadb.dat to bkup_dir/yotttadb.dat
   DB file /ydbnode1/yottadbuser1/yottadb.dat incrementally backed up in file bkup/yottadb.dat
   5 blocks saved.
   Transactions from 0x0000000000000001 to 0x0000000000000003 are backed up.
   BACKUP COMPLETED.

This command copies all in-use blocks of the DEFAULT region of the current database to directory bkup_dir.

Example:

.. code-block:: bash

   $ mupip backup -newjnlfiles=noprevlink,sync_io "*" backupdir

This example creates new journal files for the current regions, cuts the previous journal file link for all regions in the global directory, enables the SYNC_IO option and takes a backup of all databases in the directory backupdir.
