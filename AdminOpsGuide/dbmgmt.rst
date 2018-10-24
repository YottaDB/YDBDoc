
.. index:: 
    Database Management

===============================
5. General Database Management
===============================

.. contents::
   :depth: 5

----------------------
Introduction
----------------------

This chapter describes common database management operations such as creating database files, modifying database characteristics, database backup and restore, routine integrity checks, extracting or loading data, and optimizing performance.

YottaDB uses M Peripheral Interchange Program (MUPIP) for database management, database journaling, and logical multisite replication (LMS). This chapter summarizes the MUPIP commands pertaining to YottaDB database management and serves as a foundation for more advanced YottaDB functionality described for Journaling and LMS.

For MUPIP commands pertaining to database journaling, refer to `Chapter 6: “YottaDB Journaling” <https://docs.yottadb.com/AdminOpsGuide/ydbjournal.html>`_.

For MUPIP commands pertaining to multisite database replication, refer to `Chapter 7: “Database Replication” <https://docs.yottadb.com/AdminOpsGuide/dbrepl.html>`_. 

.. note::
   Two MUPIP operations - INTRPT and STOP - perform process management functions. All other MUPIP operations relate to the operation of the database.

The YottaDB installation procedure places the MUPIP utility program in a directory specified by $ydb_dist.

Invoke MUPIP by executing the mupip program at the shell prompt. If this does not work, consult your system manager (MUPIP requires that the $ydb_dist point to the directory containing the MUPIP executable image).

.. parsed-literal::
   $ydb_dist/mupip
   MUPIP>

MUPIP asks for commands, with the MUPIP> prompt. Enter the EXIT command at the MUPIP> prompt to stop the utility. MUPIP performs one operation at a time, and automatically terminates after most operations.

When additional information appears on the command line after the mupip program name, MUPIP processes the additional information as its command, for example:

.. parsed-literal::
   $ydb_dist/mupip stop 1158 

This starts MUPIP and stops the process with Process ID (PID) 1158.

Some MUPIP commands require information contained in the global directory. Therefore, a process must have access to a valid global directory before using any MUPIP commands other than EXIT, INTRPT, JOURNAL, RESTORE, STOP and the -file option for any command that has that option.

The environment variable ydb_gbldir specifies the active global directory.

A ydb_gbldir value of mumps.gld tells MUPIP to look for a global directory file mumps.gld in the current directory. For more information on the global directory, refer to `“Global Directory Editor” <https://docs.yottadb.com/AdminOpsGuide/gde.html>`_.

.. note::
   YottaDB recommends against running YottaDB components as root. When run as root, YottaDB components use the owner and group of the database file as the owner and group of newly created journal files, backup files, snapshot files, shared memory, and semaphores. In addition, they set the permissions on the resulting files, shared memory, and semaphores, as if running as the owner of the database file and as a member of the database file group.

.. note::
   You can perform read operations on a YottaDB database residing on a read-only mounted filesystem. However, the filesystem must remain read-only for the duration of any process that opens a database file resident on it. If a read-only file system is switched to read-write while YottaDB processes have database files open on it, and other processes update those databases, the read-only processes are likely to read incorrect or corrupt data. When the filesystem is read-only, the shared memory resources - which are typically shared among multiple processes - become private to each process instead, so memory resource use increases with each additional concurrent process. M locks mapped to regions that map to database files on read-only filesystems are visible only to the process that owns the locks, and are invisible to other processes.

+++++++++++++++++++++++++++++++++++++++++++++++
Operations - Standalone and Concurrent Access 
+++++++++++++++++++++++++++++++++++++++++++++++

While most MUPIP operations can be performed when YottaDB processes are actively accessing database files, some operations require stand-alone access. When using standalone access, no other process can access the database file(s). When using concurrent access, other processes can read or update the database file(s) while MUPIP accesses them. A few operations permit concurrent access to read database files, but not to update them. All MUPIP operations can be performed with stand-alone access - there is never a requirement for another process to be accessing database files when MUPIP operates on them.

Most MUPIP operations require write access to the database files with which they interact. The exceptions are INTRPT and STOP, which do not require database access, but may require other privileges; EXTRACT, which requires read access; and INTEG, which may require write access, depending on the circumstances it encounters and the qualifiers with which it is invoked. The following table displays some of the MUPIP operations and their database access requirements.

+-------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------------+
| Operations                                                        | MUPIP Command                         | Database Access Requirements                                                        |
+===================================================================+=======================================+=====================================================================================+
| Backup database files                                             | MUPIP BACKUP                          | Backup never requires standalone access and concurrent write access is controlled by|
|                                                                   |                                       | -[NO]ONLINE.                                                                        |
+-------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------------+
| Create and initialize database files                              | MUPIP CREATE                          | Standalone Access                                                                   |
+-------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------------+
| Convert a database file from one endian format to the other       | MUPIP ENDIANCVT                       | Standalone Access                                                                   |
| (BIG to LITTLE or LITTLE to BIG)                                  |                                       |                                                                                     |
+-------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------------+
| Recover database files (for example, after a system crash) and    | MUPIP JOURNAL                         | Standalone Access                                                                   |
| extract journal records                                           |                                       |                                                                                     |
+-------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------------+
| Restore databases from bytestream backup files                    | MUPIP RESTORE                         | Standalone access                                                                   |
+-------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------------+
| Properly close database files when processes terminate abnormally.| MUPIP RUNDOWN                         | Standalone access                                                                   |
+-------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------------+
| Modify database and/or journal file characteristics               | MUPIP SET                             | Standalone access is required if the MUPIP SET command specifies -ACCESS_METHOD,    |
|                                                                   |                                       | -GLOBAL_BUFFERS, -MUTEX_SLOTS, -LOCK_SPACE or -NOJOURNAL, or if any of the -JOURNAL |
|                                                                   |                                       | options ENABLE, DISABLE, or BUFFER_SIZE are specified.                              |
+-------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------------+
| Grow the size of BG database files                                | MUPIP EXTEND                          | Concurrent Access                                                                   |
+-------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------------+
| Export data from database files into sequential (flat) or binary  | MUPIP EXTRACT                         | Although MUPIP EXTRACT command works with concurrent access, it implicitly freezes  |
| files                                                             |                                       | the database to prevent updates. Therefore, from an application standpoint, you     |
|                                                                   |                                       | might plan for a standalone access during a MUPIP EXTRACT operation.                |
+-------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------------+
| Prevent updates to database files                                 | MUPIP FREEZE                          | Standalone access.                                                                  |
+-------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------------+
| Check the integrity of GDS databases                              | MUPIP INTEG                           | Concurrent access. However, standalone access is required if MUPIP INTEG specifies  |
|                                                                   |                                       | -FILE                                                                               |
+-------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------------+
| Import data into databases                                        | MUPIP LOAD                            | Although MUPIP LOAD works with concurrent access, you should always assess the      |
|                                                                   |                                       | significance of performing a MUPIP LOAD operation when an application is running    |
|                                                                   |                                       | because it may result in an inconsistent application state for the database.        |
+-------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------------+
| Defragment database files to improve performance                  | MUPIP REORG                           | Concurrent access.                                                                  |
+-------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------------+
| Send an asynchronous signal to a YottaDB process                  | MUPIP INTRPT                          | Non-database access.                                                                |
+-------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------------+
| Reports information related to relinkctl files and their          | MUPIP RCTLDUMP                        | Non-database access.                                                                |
| associated shared memory segments.                                |                                       |                                                                                     |
+-------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------------+
| Stop YottaDB processes                                            | MUPIP STOP                            | Non-database access.                                                                |
+-------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------------+


.. note::
   MUPIP commands that need standalone access issue a MUUSERLBK error on a crashed replication-enabled database and MUUSERECOV error in case of a non-replicated-but-journaled database.

+++++++++++
MUPIP
+++++++++++

The general format of MUPIP commands is:

.. parsed-literal::
   mupip command [-qualifier[...]] [object[,...]] [destination] 

MUPIP allows the abbreviation of commands and qualifiers. In each section describing a command or qualifier, the abbreviation is also shown (for example, B[ACKUP]). The abbreviated version of B[ACKUP] you can use on the command line is B. To avoid future compatibility problems and improve readability, specify at least four characters when using MUPIP commands in scripts.

Although you can enter commands in both upper and lower case (the mupip program name itself must be in lower case on UNIX/Linux), the typographical convention used in this chapter is all small letters for commands. Another convention is in the presentation of command syntax. If the full format of the command is too long for a single line of print, the presentation wraps around into additional lines. 

.. parsed-literal::
   $ mupip backup -bytestream -transaction=1 accounts,history,tables,miscellaneous /var/production/backup/

When you enter a MUPIP command, one of its variable arguments is the region-list. region-list identifies the target of the command and may include the UNIX wildcards "?" and "*". Region-lists containing UNIX wildcard characters must always be quoted, for example, "*" to prevent inappropriate expansion by the UNIX shell. Similarly, for file and directory names you might want to avoid non-graphic characters and most punctuations except underbars (_), not because of YottaDB conventions but because of inappropriate expansion by UNIX shells.

MUPIP qualifier values are restricted only by the maximum size of the command input line, which is 4KB on some systems and upto 64KB on others.

--------------------------
Commands and Qualifiers
--------------------------

The MUPIP commands described in this section are used for common database operations and serves as the foundation for more advanced functionality like `Journaling <https://docs.yottadb.com/AdminOpsGuide/ydbjournal.html>`_ and `Replication <https://docs.yottadb.com/AdminOpsGuide/dbrepl.html>`_.

++++++++++++
BACKUP
++++++++++++

Saves the contents of the database. It provides a consistent application snapshot across all database regions involved in the backup operation.
The format of the MUPIP BACKUP command is: 

.. parsed-literal::
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
   MUPIP BACKUP does a more comprehensive job of managing backup activities than other backup techniques such as a SAN backup, breaking a disk mirror, or a file system snapshot because it integrates journal management, instance file management, and records timestamps in the database file headers. To use other techniques, you must first freeze all regions concurrently with a command such as MUPIP FREEZE -ON "*" in order to ensure a consistent copy of files with internal structural integrity. YottaDB neither endorses nor tests any third party products for backing up a YottaDB database.

* MUPIP BACKUP supports two methods of database backup: -BYTESTREAM and -DATABASE. MUPIP BACKUP -BYTESTREAM directs the output to a broad range of devices, including disks, TCP sockets, and pipes. MUPIP BACKUP -DATABASE directs the output to random access devices (i.e., disks).

* [NO]ONLINE qualifier determines whether MUPIP BACKUP should suspend updates to regions. For example, MUPIP BACKUP -NOONLINE suspends updates to all regions from the time it starts the first region until it finishes the last region. However, it does not suspend processes that only read from the database.

* By default, MUPIP BACKUP is -DATABASE -ONLINE.

* If any region name does not map to an existing accessible file, or if any element of the destination list is invalid, BACKUP rejects the command with an error.

* region-list may specify more than one region of the current global directory in a list. Regions are case insensitive, separated by a comma, and wildcards can be used to specify them. Any region-name may include the wildcard characters \* and % (remember to escape them to protect them from inappropriate expansion by the shell). Any region name expansion occurs in M (ASCII) collation order.

* Depending on the type of backup, destination-list may be a single directory, or a comma separated list of destinations including files, piped commands, or a TCP socket address (a combination of IPv4 or IPV6 hostname and a port number).

* Region-list and destination-list items are matched in order - the first region is mapped to the first destination, the second to the second destination, and so on. If YottaDB encounters a region mapped to a directory, YottaDB treats that directory as the destination for all subsequent regions in the region-list.

* YottaDB implicitly timestamps both BYTESTREAM and DATABASE backups using relative timestamps (transaction numbers). You can also explicitly specify a RECORD timestamp for custom-control (SANS or mirrored disk) backup protocol. You might want to use these timestamps as reference points for subsequent backups.

* It takes approximately one (1) minute (per region) for BACKUP -ONLINE to give up and bypass a KILLs in progress; backup does not wait for Abandoned Kills to clear.

* The environment variable ydb_baktmpdir specifies the directory where mupip backup creates temporary files. If ydb_baktmpdir is not defined, YottaDB uses the deprecated GTM_BAKTMPDIR environment variable, if defined, and otherwise uses the current working directory.

* When you restrict access to a database file, YottaDB propagates those restrictions to shared resources associated with the database file, such as semaphores, shared memory, journals and temporary files used in the course of MUPIP BACKUP.

* YottaDB supports only one concurrent -ONLINE backup on a database. MUPIP BACKUP displays the BKUPRUNNING message if started when there is an already running BACKUP.

* MUPIP BACKUP protects against overwriting of existing destination files. However, it cannot protect other destinations, for example, if the destination is a pipe into a shell command that overwrites a file.

**Before Starting a MUPIP Backup**

Perform the following tasks before you begin a database backup. 

* Ensure adequate disk space for target location and temporary files. Set the environment variable ydb_baktmpdir to specify the directory where MUPIP BACKUP creates temporary files. If ydb_baktmpdir is not defined, YottaDB uses the deprecated GTM_BAKTMPDIR environment variable if defined, and otherwise uses the current working directory. Do not place temporary files in the current directory for large databases in production environments.

* When using replication, ensure that the Source/Receiver process is alive (MUPIP REPLIC -SOURCE/-RECEIVER -CHECKHEALTH). Always backup the replicating instance file with the database (BACKUP -REPLINST).

* If you intend to use a -DATABASE backup at the same time in the same computer system as the source database, be sure to disable journaling in the backed up database with -BKUPDBJNL=DISABLE.

* When doing a complete backup, switch journal files as part of the backup command using -NEWJNLFILES=NOPREVLINK. This aligns the journal files with the backup and simplifies journal file retention.

* If you follow separate procedures for backup and archival (moving to secondary storage), you can save time by starting archival as soon as MUPIP BACKUP completes the process of creating a backup database file for a region. You do not need to wait for MUPIP BACKUP to complete processing for all regions before starting archival. For example, a message like:

.. parsed-literal::
   DB file /home/jdoe/.yottadb/r1.10/g/ydb.dat backed up in file /backup/ydb.dat
   Transactions up to 0x0000000000E92E04 are backed up.

confirms that ydb.dat is backed up correctly and is ready for archival.

* Determine an appropriate frequency, timing, and backup method (-BYTESTREAM or -DATABASE) based on the situation. 

* Ensure the user issuing backup commands has appropriate permissions before starting the backup. Backup files have the ownership of the user running MUPIP BACKUP. 

* There is one circumstance under which a MUPIP BACKUP is not advised.  When your operational procedures call for taking backups of unmodified databases and journal files when rebooting a system after a crash, use an underlying operating system command (cp, cpio, gzip, tar, and so on) which will open the files read-only.  Note that for ordinary system crashes where the system simply stops writing to open files at power down, you can use MUPIP JOURNAL to recover journaled database files, and taking backups on reboot should not be required.  However, for system crashes with the possibility of damage to files already written to disk (for example, if the crash involved an IO controller with the potential for having written random data to disk immediately prior to power down), such backups on reboot are appropriate.

Example:

.. parsed-literal::
   $ mupip backup "*" /ydb/bkup

This example creates ready-to-run database backup of all regions.

~~~~~~~~~~
-BKupdbjnl
~~~~~~~~~~

A backup database shares the same journaling characteristics of the source database. However, with BKUPDBJNL you can disable or turn off journaling in the backup database. Use this qualifier if you intend to open your backup database at the same time in the same environment as the source database.

The format of the BKUPDBJNL qualifier is:

.. parsed-literal::
   -BK[UPDBJNL]={DISABLE|OFF}

* Specify DISABLE to disable journaling in the backup database.

* Specify OFF to turn off journaling in the backup database.

* Only one of the qualifiers DISABLE or OFF can be specified at any given point.

~~~~~~~~~~~
-Bytestream
~~~~~~~~~~~

Transfers MUPIP BACKUP output to a TCP connection, file (or a backup directory), or a pipe. If there are multiple .dat files, BYTESTREAM transfers output to a comma separated list of TCP connections, incremental backup files and/or directories, or pipes. When used with -SINCE or -TRANSACTION, MUPIP BACKUP allows incremental backup, that is, includes database blocks that have changed since a prior point specified by the -SINCE or -TRANSACTION.

.. note::
   MUPIP BACKUP output to a TCP connection saves disk I/O bandwidth on the current system. 

All bytestream backups needs to be restored to a random access file (with MUPIP RESTORE) before being used as a database file. -BYTESTREAM can also send the output directly to a listening MUPIP RESTORE process via a TCP/IP connection or a pipe.

The format of the BYTESTREAM qualifier is:

.. parsed-literal::
   -B[YTESTREAM]

* -BYTESTREAM is compatible with -SINCE and -TRANSACTION.

* -INCREMENTAL is deprecated in favor of -BYTESTREAM. For upward compatibility, MUPIP temporarily continues to support the deprecated -INCREMENTAL

~~~~~~~~~~  
-Database
~~~~~~~~~~

Creates a disk-to-disk backup copy of the files of all selected regions. DATABASE backup copy is a ready-to-use YottaDB database unlike BYTESREAM backup which is required to be restored to a random access file.

.. note::
   The DATABASE qualifier does not support backup to magnetic tape.

The format of the DATABASE qualifier is:

.. parsed-literal::
   -D[ATABASE]

* By default, MUPIP BACKUP uses -DATABASE.

* The DATABASE qualifier is only compatible with the -[NO]NEW[JNLFILES], -ONLINE, and -RECORD qualifiers.

* -COMPREHENSIVE is deprecated in favor of -DATABASE. For upward compatibility, MUPIP temporarily continues to support the deprecated -COMPREHENSIVE.

~~~~~~~~~~~~
-NETtimeout
~~~~~~~~~~~~

Specifies the timeout period when a bytestream BACKUP data is sent over a TCP/IP connection. The format of the NETTIMEOUT qualifier is:

.. parsed-literal::
   NET[TIMEOUT]=seconds

* The default value is 30 seconds.

* Use only with -BYTESTREAM

~~~~~~~~~~~~~
-NEWJNLFILES
~~~~~~~~~~~~~

Determines the journaling characteristics of the database files being backed-up. All the established journaling characteristics apply to new journal files. This qualifier is effective only for an ONLINE backup (the default), when the database has journaling enabled.

The format of the NEWJNLFILES qualifier is:

.. parsed-literal::
   -[NO]NEWJNLFILES[=[NO]PREVLINK], [NO]S[YNC_IO]]

* -NEWJNLFILES can take the following three values:
  
  * PREVLINK: Back links new journal files with the journal files of the prior generation. This is the default value.
  * NOPREVLINK: Indicates that there should be no back link between the newly created journals and the journal files of the prior generation.
  * SYNC_IO: Specifies that every WRITE to a journal file is to be committed directly to disk. On high-end disk subsystems (for example, those that include a non-volatile cache and consider the data to be committed when it reaches this cache), this might result in a better performance than the NOSYNC_IO option. (NOSYNC_IO turns off this option). 

* -NONEWJNLFILES causes journaling to continue with the current journal files. It does not accept any arguments.

* The default is -NEWJNLFILES=PREVLINK. 

~~~~~~~~~  
-Online
~~~~~~~~~

Specifies that while a MUPIP BACKUP operation is active, other processes can update the database without affecting the result of the backup. The format of the ONLINE qualifier is:

.. parsed-literal::
   -[NO]O[NLINE]

* MUPIP BACKUP -ONLINE creates a backup of the database as of the moment the backup starts. If running processes subsequently update the database, the backup does not reflect those updates.

* MUPIP BACKUP -ONLINE on region(s) waits for up to one minute so any concurrent KILL or MUPIP REORG operations can complete. If the KILL or MUPIP REORG operations do not complete within one minute, MUPIP BACKUP -ONLINE starts the backup with a warning that the backup may contain incorrectly marked busy blocks. Such blocks waste space and can desensitize operators to much more dangerous errors, but otherwise don't affect database integrity. If you get such an error, it may be better to stop the backup and restart it when KILL or MUPIP REORG operations are less likely to interfere. Performing MUPIP STOP on a process performing a KILL or MUPIP REORG operation may leave the database with incorrectly marked busy blocks. In this situation, YottaDB converts the ongoing KILLs flag to an Abandoned KILLs flag. If MUPIP BACKUP -ONLINE encounters ADANDONED_KILLS, it gives a message and then starts the backup. An ABANDONED_KILLS error means that both the original database and the backup database possibly have incorrectly busy blocks which should be corrected promptly.

* By default, MUPIP BACKUP is -ONLINE.

~~~~~~~~  
-Record
~~~~~~~~

Timestamps (in the form of a transaction number) a database file to mark a reference point for subsequent bytestream, database, or custom backup (SANS or disk mirror) protocols. Even though -DATABASE and -BYTESTREAM both mark their own relative timestamps, -RECORD provides an additional timestamp option. MUPIP FREEZE also provides the -RECORD qualifier because a FREEZE may be used to set the database up for a SAN or disk-mirror based backup mechanism.

The format of the RECORD qualifier is:

.. parsed-literal::
   -R[ECORD]

* Use -RECORD (with the hyphen) to timestamp a reference point and use RECORD as a keyword (as in -SINCE=RECORD) to specify the starting point for a MUPIP BACKUP operation.

* -RECORD replaces the previously RECORDed transaction identifier for the database file.

~~~~~~~~~  
-Replace
~~~~~~~~~

Overwrites the existing destination files. 

The format of the REPLACE qualifier is:

.. parsed-literal::
   -[REPL]ACE

* By default, MUPIP BACKUP protects against overwriting the destination files. -REPLACE disables this default behavior.

* -REPLACE is compatible only with -DATABASE.

~~~~~~~~~~~~~~  
-REPLinstance
~~~~~~~~~~~~~~

Specifies the target location to place the backup of the replication instance file.

.. note::
   The replication instance file should always be backed up with the database file. The source server for the instance must be started at least once before backing up the replication instance file.

The format of the REPLINSTANCE qualifier is:

.. parsed-literal::
   -REPLI[NSTANCE]=<target_location>

~~~~~~~
-Since
~~~~~~~

Includes blocks changed since the last specified backup. The format of the SINCE qualifier is:

.. parsed-literal::
   -S[INCE]={DATABASE|BYTESTREAM|RECORD}

* D[ATABASE] - Backup all changes since the last MUPIP BACKUP -DATABASE.

* B[YTESTREAM] - Backup all changes since the last MUPIP BACKUP -BYTESTREAM.

* R[ECORD] - Backup all changes since the last MUPIP BACKUP -RECORD.

By default, MUPIP BACKUP -BYTESTREAM operates as -SINCE=DATABASE.

Incompatible with: -TRANSACTION.

~~~~~~~~~~~~
-Transaction
~~~~~~~~~~~~

Specifies the transaction number of a starting transaction that causes BACKUP -BYTESTREAM to copy all blocks that have been changed by that transaction and all subsequent transactions. The format of the TRANSACTION qualifier is:

.. parsed-literal::
   -T[RANSACTION]=transaction-number

* A Transaction number is always a 16 digit hexadecimal number. It appears in a DSE DUMP -FILEHEADER with the label "Current transaction".

* If the transaction number is invalid, MUPIP BACKUP reports an error and rejects the command.

* It may be faster than a DATABASE backup, if the database is mostly empty.

* Incompatible with: -DATABASE, -SINCE.

.. note::
   A point in time that is consistent from an application perspective, is unlikely to have the same transaction number in all database regions. Therefore, except for -TRANSACTION=1, this qualifier is not likely to be useful for any backup involving multiple regions. 

**Examples for MUPIP BACKUP**

Example:

.. parsed-literal::
   $ mupip backup -bytestream MAMMALS,CRUSTACEANS bkup

Suppose that the environment variable ydb_gbldir has regions MAMMALS and CRUSTACEANS that map to files called LINNAEUS.DAT and BRUNNICH.DAT (no matter which directory or directories the files reside in). Then the above example creates bytestream backup files MAMMALS.DAT and CRUSTACEANS.DAT in the bkup directory since the last DATABASE backup.

Example:

.. parsed-literal::
   $ mupip backup -bkupdbjnl="OFF" "*"

This command turns off journaling in the backup database.

Example:

.. parsed-literal::
   $ mupip backup -bytestream "*" tcp://philadelphia:7883,tcp://tokyo:8892

Assuming a Global Directory with two regions pointing to ACN.DAT and HIST.DAT, this example creates a backup of ACN.DAT to a possible MUPIP RESTORE process listening at port 7883 on server philadelphia and HIST.DAT to a possible MUPIP RESTORE process listening at port 8893 on server tokyo.

Always specify the <machine name> and <port> even if both backup and restore are on the same system, and ensure that the MUPIP RESTORE process is started before the MUPIP BACKUP process.

Example:

.. parsed-literal::
   $ mupip backup -database -noonline "*" bkup
   DB file /home/ydbnode1/yottadbuser1/mumps.dat backed up in file bkup/mumps.dat
   Transactions up to 0x00000000000F42C3 are backed up.
   BACKUP COMPLETED.

This command creates a disk-to-disk backup copy of all regions of the current database in directory bkup. YottaDB freezes all the regions during the backup operation.

Example:

.. parsed-literal::
   $ mupip backup -bytestream -nettimeout=420 DEFAULT tcp://${org_host}:6200

This command creates a backup copy of the DEFAULT region with timeout of 420 seconds.

Example:

.. parsed-literal::
   $ mupip backup -bytestream DEFAULT '"| gzip -c > online5pipe.inc.gz"'

This command sends (via a pipe) the backup of the DEFAULT region to a gzip command.

Example:

.. parsed-literal::
   $ mupip backup -online DEFAULT bkup
   DB file /ydbnode1/yottadbuser1/mumps.dat backed up in file bkup/mumps.dat
   Transactions up to 0x00000000483F807C are backed up.
   BACKUP COMPLETED.

This command creates a backup copy of the DEFAULT region of the current database in directory bkup. During the backup operation, other processes can read and update the database.

Example:

.. parsed-literal::
   $ mupip backup -record DEFAULT bkup

This command sets a reference point and creates a backup copy of the DEFAULT region of the current database in directory bkup.

Example:

.. parsed-literal::
   $ mupip backup -online -record DEFAULT bkup1921
   DB file /home/mammals/mumps.dat backed up in file bkup1921/mumps.dat
   Transactions up to 0x00000000000F4351 are backed up.

Example:

.. parsed-literal::
   $ mupip backup -bytestream -since=record DEFAULT bkup1921onwards
   MUPIP backup of database file /home/mammals/mumps.dat to bkup1921onwards/mumps.dat
   DB file /home/mammals/mumps.dat incrementally backed up in file bkup1921onwards/mumps.dat
   6 blocks saved.
   Transactions from 0x00000000000F4351 to 0x00000000000F4352 are backed up.
   BACKUP COMPLETED.

The first command sets a reference point and creates a backup copy of the DEFAULT region of the current database in directory bkup1921. The second command completes a bytestream backup starting from the reference point set by the first command.

Example:

.. parsed-literal::
   $ mupip backup -bytestream -transaction=1 DEFAULT bkup_dir
   MUPIP backup of database file /ydbnode1/yottadbuser1/mumps.dat to bkup_dir/mumps.dat
   DB file /ydbnode1/yottadbuser1/mumps.dat incrementally backed up in file bkup/mumps.dat
   5 blocks saved.
   Transactions from 0x0000000000000001 to 0x0000000000000003 are backed up.
   BACKUP COMPLETED.

This command copies all in-use blocks of the DEFAULT region of the current database to directory bkup_dir.

Example:

.. parsed-literal::
   $ mupip backup -newjnlfiles=noprevlink,sync_io "*" backupdir

This example creates new journal files for the current regions, cuts the previous journal file link for all regions in the global directory, enables the SYNC_IO option and takes a backup of all databases in the directory backupdir. 

++++++++++++++++
CREATE
++++++++++++++++

Creates and initializes database files using the information in a Global Directory file. If a file already exists for any segment, MUPIP CREATE takes no action for that segment.

The format of the CREATE command is:

.. parsed-literal::
   CR[EATE] [-R[EGION]=region-name]

The single optional -REGION qualifier specifies a region for which to create a database file.

Note that one YottaDB database file grows to a maximum size of 1,040,187,392(992Mi) blocks. This means, for example, that with an 8KB block size, the maximum single database file size is 1,792GB (8KB*224M). Note that this is the size of one database file -- a logical database (an M global variable namespace) can consist of an arbitrary number of database files. 

~~~~~~~~~~
-Region
~~~~~~~~~~

Specifies a single region for creation of a database file. By default, MUPIP CREATE creates database files for all regions in the current Global Directory that do not already have a database file.

The format of the REGION qualifier is:

.. parsed-literal::
   -R[EGION]=region-name

**Examples for MUPIP CREATE**

Example:

.. parsed-literal::
   $ mupip create -region=MAMMALS

This command creates the database file specified by the Global Directory (named by the Global Directory environment variable) for region MAMMALS.

++++++++++
DOWNGRADE
++++++++++

The MUPIP DOWNGRADE command changes the file header format to a previous version number. The format of the MUPIP DOWNGRADE command is:

.. parsed-literal::
   D[OWNGRADE] -V[ERSION]={r1.10\|r1.20} file-name

.. note::
   You must perform a database integrity check using the -noonline parameter prior to downgrading a database. The integrity check verifies and clears database header fields required for an orderly downgrade. If an integrity check is not possible due to time constraints, please rely on a rolling upgrade scheme using replication and/or take a backup prior to upgrading the database.

~~~~~~~~~~~~~~~~~~~
-VERSION={version}
~~~~~~~~~~~~~~~~~~~

For more information on the downgrade criteria for your database, refer to the release notes document of your current YottaDB version.

**Examples for MUPIP DOWNGRADE**

Example:

.. parsed-literal::
   $ mupip downgrade mumps.dat

This command changes the file-header of mumps.dat to the format in the previous version.

+++++++++++++++++++++
DUMPFHEAD
+++++++++++++++++++++

The MUPIP DUMPFHEAD command displays information about one or more database files. The format of the MUPIP DUMPFHEAD command is:

.. parsed-literal::
   DU[MPFHEAD]  {-FI[LE] file-name \|-REG[ION] region-list} 

~~~~~~~~~~
-FILE=file
~~~~~~~~~~

Specifies the name of the database file for the MUPIP DUMPFHEAD operation. -FILE does not require a Global Directory. The format of the FILE qualifier is:

.. parsed-literal::
   -FI[LE] database-file-name

* The database filename must include the absolute or relative path.

* The -FILE qualifier is incompatible with the -REGION qualifier.

~~~~~~~~~~~~~~
-REGION=region
~~~~~~~~~~~~~~

Specifies that the INTEG parameter identifies one or more regions rather than a database file. The format of the REGION qualifier is:

.. parsed-literal::
   -R[EGION] region-list

* The region-list identifies the target of DUMPFHEAD. region-list may specify more than one region of the current global directory in a list. Regions are case-insensitive, separated by a comma, and wildcards can be used to specify them. Any region-name may include the wildcard characters * and ? (remember to escape them to protect them from inappropriate expansion by the shell). Any region name expansion occurs in M (ASCII) collation order.

* The region-list argument may specify more than one region of the current Global Directory in a list separated with commas. DUMPFHEAD -REGION requires the environment variable ydb_gbldir to specify a valid Global Directory. For more information on defining ydb_gbldir, refer to `Chapter 4: “Global Directory Editor” <https://docs.yottadb.com/AdminOpsGuide/gde.html>`_.

* The -REGION qualifier is incompatible with the -FILE qualifier.

**Examples for MUPIP DUMPFHEAD**

Example:

.. parsed-literal::
   $ mupip dumpfhead -file mumps.dat

This command lists information about the database file mumps.dat in the current working directory.

.. parsed-literal::
   $ mupip dumpfhead -region "*"

This command lists information about all the database files mapped by the global directory specified by $ydb_gbldir.

+++++++++++++++
ENDIANCVT
+++++++++++++++

Converts a database file from one endian format to the other (BIG to LITTLE or LITTLE to BIG). The format of the MUPIP ENDIANCVT command is:

.. parsed-literal::
   ENDIANCVT [-OUTDB=<outdb-file>] -OV[ERRIDE] <db-file>

* <db-file> is the source database for endian conversion. By default ENDIANCVT converts <db-file> in place.

* outdb writes the converted output to <outdb-file>. In this case, ENDIANCVT does not modify the source database <db-file>.

* ENDIANCVT produces a <outdb-file> of exactly the same size as <db-file>.

.. note::
   Ensure adequate storage for <outdb-file> to complete the endian conversion successfully.

* ENDIANCVT requires standalone access to the database.

* YottaDB displays a confirmation request with the "from" and "to" endian formats to perform the conversion. Conversion begins only upon receiving positive confirmation, which is a case-insensitive "yes".

* In a multi-site replication configuration, the receiver server automatically detects the endian format of an incoming replication stream and converts it into the native endian format. See the `Database Replication chapter <https://docs.yottadb.com/AdminOpsGuide/dbrepl.html>`_ for more information.

* Encrypted database files converted with ENDIANCVT require the same key and the same cipher that were used to encrypt them. 

.. note::
   YottaDB on a big endian platform can convert a little endian database into big endian and vice versa; as can YottaDB on a little endian platform. YottaDB (run-time and utilities other than MUPIP ENDIANCVT) on a given endian platform opens and processes only those databases that are in the same endian format. An attempt to open a database of a format other than the native endian format produces an error.

~~~~~~~~~~
-OVERRIDE
~~~~~~~~~~

Enables MUPIP ENDIANCVT to continue operations even if YottaDB encounters the following errors:

* "minor database format is not the current version"

* "kills in progress"

* "a GT.CM server is accessing the database"

Note that the OVERRIDE qualifier does not override critical errors (database integrity errors, and so on) that prevent a successful endian format conversion.

**Examples for MUPIP ENDIANCVT**

.. parsed-literal::
   $ mupip endiancvt mumps.dat -outdb=mumps_cvt.dat
   Converting database file mumps.dat from LITTLE endian to BIG endian on a LITTLE endian system
   Converting to new file mumps_cvt.dat
   Proceed [yes/no] ?

This command detects the endian format of mumps.dat and converts it to the other endian format if you type yes to confirm. 

++++++++++
EXIT
++++++++++

Stops a MUPIP process and returns control to the process from which MUPIP was invoked.

The format of the MUPIP EXIT command is:

.. parsed-literal::
   EXI[T]

The EXIT command does not accept any qualifiers.

++++++++++++
EXTEND
++++++++++++

Increases the size of a database file. By default, YottaDB automatically extends a database file when there is available space.

The format of the MUPIP EXTEND command is:

.. parsed-literal::
   EXTE[ND] [-BLOCKS=<data-blocks-to-add>] region-name

* The only qualifier for MUPIP EXTEND is BLOCKS.

* The required region-name parameter specifies the name of the region to expand.

* EXTEND uses the Global Directory to map the region to the dynamic segment and the segment to the file.

~~~~~~~~  
-Blocks
~~~~~~~~

Specifies the number of GDS database blocks by which MUPIP should extend the file. GDS files use additional blocks for bitmaps. MUPIP EXTEND adds the specified number of blocks plus the bitmap blocks required as overhead. For more information about bitmaps, refer to `Chapter 9: “YottaDB Database Structure(GDS)” <https://docs.yottadb.com/AdminOpsGuide/gds.html>`_.

The format of the BLOCK qualifier is:

.. parsed-literal::
   -BLOCKS=data-blocks-to-add

By default, EXTEND uses the extension value in the file header as the number of GDS blocks by which to extend the database file. You can specify as many blocks as needed as long as you are within the maximum total blocks limit (which could be as high as 224 million GDS blocks).

**Examples for MUPIP EXTEND**

.. parsed-literal::
   $ mupip extend DEFAULT -blocks=400

This command adds 400 GDE database blocks to region DEFAULT.

Example:

.. parsed-literal::
   $ mupip extend MAMMALS -blocks=100

This command adds 100 GDE database blocks to the region MAMMALS.

++++++++++++++++++
EXTRACT
++++++++++++++++++

Backs up certain globals or extracts data from the database for use by another system. The MUPIP EXTRACT command copies globals from the current database to a sequential output file in one of three formats - GO, BINARY, or ZWR. The format of the MUPIP EXTRACT command is:

.. parsed-literal::
   EXTR[ACT] 
   [
    -FO[RMAT]={GO|B[INARY]|Z[WR]}
    -FR[EEZE]
    -LA[BEL]=text
    -[NO]L[OG]
    -R[EGION]=region-list
    -S[ELECT]=global-name-list]
   ]
   {-ST[DOUT]|file-name}

* By default, MUPIP EXTRACT uses -FORMAT=ZWR.

* MUPIP EXTRACT uses the Global Directory to determine which database files to use.

* MUPIP EXTRACT supports user collation routines. When used without the -FREEZE qualifier, EXTRACT may operate concurrently with normal YottaDB database access.

* To ensure that MUPIP EXTRACT reflects a consistent application state, suspend the database updates to all regions involved in the extract, typically with the FREEZE qualifier, or backup the database with the ONLINE qualifier and extract files from the backup.

* EXTRACT places its output in the file defined by the file-name. EXTRACT may output to a UNIX file on any device that supports such files, including magnetic tapes.

* In UTF-8 mode, MUPIP EXTRACT writes a sequential output file in the UTF-8 character encoding. Ensure that the MUPIP EXTRACT commands and corresponding MUPIP LOAD commands execute with the same setting for the environment variable ydb_chset.

* The GO format is not supported for UTF-8 mode. Use BINARY or ZWR formats in UTF-8 mode. 

For information on extracting globals with the %GO utility, refer to the `"Utility Routines" chapter of the Programmer's Guide <https://docs.yottadb.com/ProgrammersGuide/utility.html>`_. MUPIP EXTRACT is typically faster, but %GO can be customized.

The following sections describe the qualifiers of MUPIP EXTRACT command.

~~~~~~~
-FORMAT
~~~~~~~

Specifies the format of the output file. The format of the FORMAT qualifier is:

.. parsed-literal::
   -FO[RMAT]=format_code

The format code is any one of the following:

1. B[INARY] - Binary format, used for database reorganization or short term backups. MUPIP EXTRACT -FORMAT=BINARY works much faster than MUPIP EXTRACT -FORMAT=GO and MUPIP EXTRACT -FORMAT=ZWR. Note: There is no defined standard to transport binary data from one YottaDB implementation to another. Furthermore, YottaDB reserves the right to modify the binary format in new versions. The first record of a BINARY format data file contains the header label. The header label is 87 characters long. The following table illustrates the components of the header label. 

   +----------------------------+-------------------------------------------------------------------------------------------+
   | Characters                 | Explanation                                                                               |
   +============================+===========================================================================================+
   | 1-2                        | Hexadecimal representation of the length of the label (by default 64 - decimal 100).      |
   +----------------------------+-------------------------------------------------------------------------------------------+
   | 3-28                       | Fixed-length ASCII text containing:                                                       |
   |                            | * "GDS BINARY EXTRACT LEVEL 6": when no region is encrypted.                              |
   |                            | * "GDS BINARY EXTRACT LEVEL 8": when one more regions are encrypted using null IVs.       |
   |                            | * "GDS BINARY EXTRACT LEVEL 9": when one or regions are encrypted using non-null IVs.     |
   +----------------------------+-------------------------------------------------------------------------------------------+
   | 29-41                      | Fixed-length ASCII text: Date and time of extract in the $ZDATE() format: "YEARMMDD2460SS"|
   +----------------------------+-------------------------------------------------------------------------------------------+
   | 42-48                      | Fixed-length ASCII text: Decimal maximum block size of the union of each region from which|
   |                            | data was extracted                                                                        |
   +----------------------------+-------------------------------------------------------------------------------------------+
   | 49-55                      | Fixed-length ASCII text: Decimal maximum record size of the union of each region from     |
   |                            | which data is extracted                                                                   |
   +----------------------------+-------------------------------------------------------------------------------------------+
   | 56-62                      | Fixed-length ASCII text:Decimal maximum key size of the union of each region from which   |
   |                            | data is extracted                                                                         |
   +----------------------------+-------------------------------------------------------------------------------------------+
   | 63-69                      | Fixed-length ASCII text:Boolean indicator of Standard NULL collation (1) or YottaDB       |
   |                            | legacy collation (0).                                                                     |
   +----------------------------+-------------------------------------------------------------------------------------------+
   | 70-100                     | Fixed-length ASCII text: Space-padded label specified by the -LABEL qualifier; the default|
   |                            | LABEL is "MUPIP EXTRACT"                                                                  |
   |                            | For extracts in UTF-8 mode, YottaDB prefixes UTF-8 and a space to -LABEL.                 |
   +----------------------------+-------------------------------------------------------------------------------------------+

2. GO - Global Output format, used for files to transport or archive. -FORMAT=GO stores the data in record pairs. Each global node produces one record for the key and one for the data. MUPIP EXTRACT -FORMAT=GO has two header records - the first is a test label (refer to the LABEL qualifier) and the second contains data, and time. 

3. ZWR - ZWRITE format, used for files to transport or archive that may contain non-graphical information. Each global node produces one record with both a key and data. MUPIP EXTRACT -FORMAT=ZWR has two header records, which are the same as for FORMAT=GO, except that the second record ends with the text " ZWR". 

~~~~~~~
-FREEZE
~~~~~~~

Prevents database updates to all database files from which the MUPIP EXTRACT command is copying records. FREEZE ensures that a MUPIP EXTRACT operation captures a "sharp" image of the globals, rather than one "blurred" by updates occurring while the copy is in progress.

The format of the FREEZE qualifier is:

.. parsed-literal::
   -FR[EEZE]

By default, MUPIP EXTRACT does not "freeze" regions during operation. 

~~~~~~~~
-LABEL
~~~~~~~~

Specifies the text string that becomes the first record in the output file. MUPIP EXTRACT -FORMAT=BINARY truncates the label text to 32 characters. The format of the LABEL qualifier is:

.. parsed-literal::
   -LA[BEL]=text

* By default, EXTRACT uses the label "MUPIP EXTRACT."

* For more detailed information about the -FORMAT=BINARY header label, refer to the description of EXTRACT -FORMAT=BINARY. 

~~~~~~  
-LOG
~~~~~~

Displays a message on stdout for each global extracted with the MUPIP EXTRACT command. The message displays the number of global nodes, the maximum subscript length and maximum data length for each global. The format of the LOG qualifier is:

.. parsed-literal::
   -[NO]LO[G]

By default, EXTRACT operates -LOG. 

~~~~~~~~~
-NULL_IV
~~~~~~~~~

Creates an encrypted binary extract with null IVs from a database with non-null IVs, which can be restored to a version that does not support non-null IVs. The format of the -NULL_IV qualifier is:

.. parsed-literal::
   -[N]ULL_IV

* Older versions of YottaDB used empty (all zeros or "NULL_IV") initialization vectors(IVs) to encrypt or decrypt -FORMAT="BINARY" extracts.

* The current and later versions use non-zero IVs.

* Use the NULL_IV qualifier only on encrypted databases to create an encrypted binary extract in GDS BINARY EXTRACT LEVEL 8 format. This format can load data on any encrypted YottaDB database created with an older version.

* The default is -NONULL_IV which produces a binary extract in GDS BINARY EXTRACT LEVEL 9 format.

~~~~~~~
-REGION
~~~~~~~

Restricts MUPIP EXTRACT to a set of regions. The format of the REGION qualifier is:

.. parsed-literal::
   -R[EGION]=region-list 

region-list may specify more than one region of the current global directory in a list. Regions are case-insensitive, separated by a comma, and wildcards can be used to specify them. Any region-name may include the wildcard characters * and % (remember to escape them to protect them from inappropriate expansion by the shell). Any region name expansion occurs in M (ASCII) collation order.

~~~~~~~~
-SELECT
~~~~~~~~

Specifies globals for a MUPIP EXTRACT operation. The format of the SELECT qualifier is:

.. parsed-literal::
   -S[ELECT]= global-specification

* By default, EXTRACT selects all globals, as if it had the qualifier -SELECT=*

* The caret symbol (^) in the specification of the global name is optional. 

The global-specification can be:

* A global name, such as MEF. In this case, MUPIP EXTRACT selects only global ^MEF.
* A range of global names, such as A7:B6. In this case, MUPIP EXTRACT selects all global names between ^A7 and ^B6, inclusive.
* A list, such as A,B,C. In this case, MUPIP EXTRACT selects globals ^A, ^B, and ^C.
* A suffix with a global name. For example, PIGEON* selects all global names from ^PIGEON through ^PIGEONzzzzz. You can use suffixes with a global name or a list. 
  
.. note::
   If the rules for selection are complex, it may be easier to construct an ad hoc Global Directory that maps the global variables to be extracted to the database file. This may not be permissible if the database file is part of a replicated instance. If this is the case, work with a backup of the database.

~~~~~~~~~
-STDOUT
~~~~~~~~~

Redirects the database extract to the standard output stream. The format of the STDOUT qualifier is:

.. parsed-literal::
   -ST[DOUT]

**Examples for MUPIP EXTRACT**

Example:

.. parsed-literal::
   $ mupip extract -format=go -freeze big.glo

This command prevents database updates during a MUPIP EXTRACT operation.

Example:

.. parsed-literal::
   $ mupip extract -format=GO mumps_i.go

This command creates an extract file called mumps_i.go in "Global Output" format. Use this format to transport or archive files. The first record of a GO format file contains the header label, "MUPIP EXTRACT," as text.

Example:

.. parsed-literal::
   $ mupip extract -format=BINARY v5.bin

This command creates an extract file called v5.bin in Binary format. Use this format for reorganizing a database or for short-term backups.

Example:

.. parsed-literal::
   $ mupip extract -format=ZWR -LABEL=My_Label My_Extract_File

This example extracts all globals from the current database to file My_Extract_File (in ZWRITE format) with label My_Label.

Example:

.. parsed-literal::
   $ mupip extract -nolog FL.GLO

This command creates a global output file, FL.GLO, (which consists of all global variables in the database) without displaying statistics on a global-by-global basis. As there is no label specified, the first record in FL.GLO contains the text string "MUPIP EXTRACT."

Example:

.. parsed-literal::
   $ mupip extract -select=Tyrannosaurus /dev/tty

This command instructs EXTRACT to dump the global ^Tyrannosaurus to the device (file-name) /dev/tty. 

++++++++++++++
FREEZE
++++++++++++++

Temporarily suspends (freezes) updates to the database. If you prefer a non-YottaDB utility to perform a backup or reorganization, you might use this facility to provide standalone access to your YottaDB database. You might use MUPIP FREEZE to suspend (and later resume) database updates for creating mirrored disk configuration or re-integrating a mirror.

BACKUP, INTEG, and REORG operations may implicitly freeze and unfreeze database regions. However, for most operations, this freeze/unfreeze happens internally and is transparent to the application.

The format of the MUPIP FREEZE command is:

.. parsed-literal::
   F[REEZE] {-OF[F] [-OV[ERRIDE]]|-ON [[-ONL[INE] [-[NO]AUTORELEASE]] | [-NOONL[INE]] [-R[ECORD]]]} region-list

* The region-list identifies the target of the FREEZE. region-list may specify more than one region of the current global directory in a list. Regions are case-insensitive, separated by a comma, and wildcards can be used to specify them. Any region-name may include the wildcard characters * and % (remember to escape them to protect them from inappropriate expansion by the shell). Any region name expansion occurs in M (ASCII) collation order.

* MUPIP FREEZE waits for up to one minute so that concurrent KILL or MUPIP REORG operations can complete. If the KILL or MUPIP REORG commands do not complete within one minute, MUPIP FREEZE unfreezes any regions it had previously marked as frozen and terminates with an error.

* To ensure that a copy or reorganized version of a database file contains a consistent set of records, concurrent MUPIP utilities, such as BACKUP (without the ONLINE qualifier) and EXTRACT, include mechanisms to ensure that the database does not change while the MUPIP utility is performing an action. YottaDB recommends the use of the -ONLINE qualifier with BACKUP.

* A MUPIP FREEZE can be removed only by the user who sets the FREEZE or by using -OVERRIDE.

* A MUPIP FREEZE -ON can specify either -NOONLINE, the default, or -ONLINE, and if -ONLINE, can specify either -AUTORELEASE, the default, or -NOAUTORELEASE.

* A FREEZE specifying -ONLINE attempts to minimize the impact of the FREEZE on concurrently updating processes.

* A FREEZE specifying -ONLINE -AUTORELEASE allows updates to continue immediately when YottaDB needs to update the database file.

* After MUPIP FREEZE -ON -NOONLINE, processes that are attempting updates "hang" until the FREEZE is removed by the MUPIP FREEZE -OFF command or DSE. Make sure that procedures for using MUPIP FREEZE, whether manual or automated, include provisions for removing the FREEZE in all appropriate cases, including when errors disrupt the normal flow.

* MUPIP FREEZE sends a DBFREEZEON/DBFREEZEOFF message to the system log for each region whose freeze state is changed.

* A -RECOVER/-ROLLBACK for a database reverts to a prior database update state. Therefore, a -RECOVER/-ROLLBACK immediately after a MUPIP FREEZE -ON removes the freeze. However, -RECOVER/-ROLLBACK does not succeed if there are processes attached (for example when a process attempts a database update immediately after a MUPIP FREEZE -ON) to the database.

FREEZE must include one of the qualifiers:

.. parsed-literal::
   -OF[F]
   -ON

The optional qualifiers are:

.. parsed-literal::
   -[NO]A[UTORELEASE] - only valid with -ONLINE
   -ON[LINE] - only valid with -ON
   -OV[ERRIDE]
   -R[ECORD] - only valid with -ON

~~~~~
-OFF
~~~~~

Clears a freeze set by another process with the same userid.

The format of the OFF qualifier is:

.. parsed-literal::
   OF[F]

* A FREEZE -OFF which turns off a FREEZE -ONLINE -AUTORELEASE produces a OFRZNOTHELD warning to indicate that the freeze was automatically released and therefore did not protect whatever concurrent actions it was intended to guard.

* When used with -OVERRIDE, -OFF stops a freeze operation set by a process with a different userid.

* Incompatible with: -ON, -RECORD

~~~~  
-ON
~~~~

Specifies the start of a MUPIP FREEZE operation. The format of the ON qualifier is:

.. parsed-literal::
   -ON

Incompatible with: -OFF, -OVERRIDE

~~~~~~~~~~~~~~~~~~
-[NO]A[UTORELEASE
~~~~~~~~~~~~~~~~~~

Controls the behavior of a FREEZE specified with -ONLINE when YottaDB must write to a database file. The format of the AUTORELEASE qualifier is:

.. parsed-literal::
   -[NO]A[UTORELEASE]

* -AUTORELEASE, the default, causes YottaDB to release the freeze if it needs to update the file before a FREEZE -OFF.

* -NOAUTORELEASE causes YottaDB to hold off actions that need to update the database file until someone issues a MUPIP FREEZE -OFF.

* -The actions that require YottaDB to write to the database file are:

  * Insufficient global buffers to hold updates - YottaDB must flush buffers to make space to do any additional updates

  * Insufficient space in the database to hold updates - YottaDB must extend the file

  * The journal file reaches its maximum size or someone issues a MUPIP SET -JOURNAL command - YottaDB must create a new journal file

  * An epoch comes due - YottaDB must create a checkpoint

  * Someone issues a MUPIP BACKUP command - YottaDB must record state information to mark the beginning of the backup

* When an -AUTORELEASE abandons a FREEZE, any actions that depend on the stability of the database file on secondary storage, such as a database copy, lose that protection and are not reliable, so they likely need to be repeated at a time when an -AUTORELEASE is less likely or when -NOONLINE is more appropriate.

* An -AUTORELEASE action produces an OFRZAUTOREL message in the operator log.

* An -AUTORELEASE action requires a FREEZE -OFF to reestablish a normal database state.

* Incompatible with: -OFF, -NOONLINE 

~~~~~~~~ 
-ONLINE
~~~~~~~~

Controls the potential impact of a FREEZE on concurrently updating processes. The format of the ONLINE qualifier is:

.. parsed-literal::
   -[NO]ONL[INE]

* ON -NOONLINE, the default, causes the freeze to last until OFF, and makes management of the FREEZE straightforward.

* ON -ONLINE, causes YottaDB to attempt to minimize the impact of the FREEZE on concurrently updating processes by taking a number of actions, as appropriate:
  
  * Switching journal files to provide maximum space
  * Performing an epoch to provide maximum time to the next epoch
  * Flushing the global buffers to make all available to hold updates
  * Incompatible with: -AUTORELEASE, -OFF 

* After performing these preparations, -ONLINE allows updating processes to make updates to global buffers but defer flushing them to the database file.

* -ONLINE cannot apply to MM databases, so a FREEZE -ONLINE skips any MM regions it encounters.

* Refer to -AUTORELEASE above for additional information.

* Incompatible with: -OFF

.. note::
   If a database is nearly full, and want to use MUPIP FREEZE -ON -ONLINE, you may want to use MUPIP EXTEND first as a database file extension to either AUTORELEASE or "harden" the -ONLINE freeze effectively into a -NOONLINE freeze.

~~~~~~~~~~
-OVERRIDE
~~~~~~~~~~

Release a freeze set by a process with a different userid. YottaDB provides OVERRIDE to allow error recovery in case a procedure with a freeze fails to release. The format of the OVERRIDE qualifier is:

.. parsed-literal::
   -OV[ERRIDE]

* OVERRIDE should not be necessary (and may even be dangerous) in most schemes.

* Incompatible with: -AUTORELEASE, -ON, -ONLINE, -RECORD

~~~~~~~~  
-RECORD
~~~~~~~~

Specifies that a MUPIP FREEZE operation should record an event as a reference point. You might use MUPIP FREEZE to set up your database for a custom-backup mechanism (SAN or mirror-based).

The format of the RECORD qualifier is:

.. parsed-literal::
   -R[ECORD]

* You might use -RECORD to integrate MUPIP BACKUP -BYTESTREAM with an external backup mechanism.

* -RECORD replaces the previously RECORDed transaction identifier for the database file.

* Incompatiable with: -OFF and -OVERRIDE.

**Examples for MUPIP FREEZE**

Example:

.. parsed-literal::
   $ mupip freeze -off DEFAULT

This command stops an ongoing MUPIP FREEZE operation on the region DEFAULT.

Example:

.. parsed-literal::
   $ mupip freeze -on "*"

This command prevents updates to all regions in the current Global Directory.

Example:

.. parsed-literal::
   $ set +e
   $ mupip freeze -on -record "*"
   $ tar cvf /dev/tape /prod/appl/\*.dat
   $ mupip freeze -off
   $ set -e

The set +e command instructs the shell to attempt all commands in the sequence , regardless of errors encountered by any command. This ensures that the freeze -off is processed even if the tar command fails. FREEZE prevents updates to all database files identified by the current Global Directory. The -record qualifier specifies that the current transaction in each database be stored in the RECORD portion of the database file header. The tar command creates a tape archive file on the device /dev/tape, containing all the files from /prod/app that have an extension of .dat. Presumably all database files in the current Global Directory are stored in that directory, with that extension. The second FREEZE command re-enables updates that were suspended by the first FREEZE. The set -e command re-enables normal error handling by the shell.

Example:

.. parsed-literal::
   $ mupip freeze -override -off DEFAULT

This command unfreezes the DEFAULT region even if the freeze was set by a process with a different userid. 

+++++++++
FTOK
+++++++++

Produces the "public" (system generated) IPC Keys (essentially hash values) of a given file.

The format of the MUPIP FTOK command is:

.. parsed-literal::
   FT[OK] [-DB] [-JNLPOOL] [-RECVPOOL] file-name

~~~
-DB
~~~

Specifies that the file-name is a database file. By default, MUPIP FTOK uses -DB.

~~~~~~~~~
-JNLPOOL
~~~~~~~~~

Specifies that the reported key is for the Journal Pool of the instance created by the current Global Directory.

~~~~~~~~~
-RECVPOOL
~~~~~~~~~

Specifies that the reported key is for the Receive Pool of the instance created by the current Global Directory.

+++++++++++++++
HASH
+++++++++++++++

Uses a 128 bit hash based on the MurmurHash3 algorithm to provide the hash of source files from the command line.

The format of the MUPIP HASH command is:

.. parsed-literal::
   MUPIP HASH <file-names> 

++++++++++++
INTEG
++++++++++++

Performs an integrity check on a YottaDB database file. You can perform structural integrity checks on one or more regions in the current Global Directory without bringing down (suspending database updates) your application. However, a MUPIP INTEG on a single file database requires standalone access but does not need a Global Directory. The order in which the MUPIP INTEG command selects database regions is a function of the file system layout and may vary as files are moved or created. Execute MUPIP INTEG operations one database file at a time to generate an report where the output always lists database files in a predictable sequence. For example, to compare output with a reference file, run INTEG on one file at a time.

Always use MUPIP INTEG in the following conditions:

* Periodically - to ensure ongoing integrity of the database(s); regular INTEGs help detect any integrity problems before they spread and extensively damage the database file.

* After a crash - to ensure the database was not corrupted. (Note: When using before-image journaling, when the database is recovered from the journal file after a crash, an integ is not required).

* When database errors are reported - to troubleshoot the problem.

Improving the logical and physical adjacency of global nodes may result in faster disk I/O. A global node is logically adjacent when it is stored within a span of contiguous serial block numbers. A global node is physically adjacent when it resides on adjacent hard disk sectors in a way that a single seek operation can access it. Database updates (SETs/KILLs) over time affect the logical adjacency of global nodes. A MUPIP INTEG reports the logical adjacency of your global nodes which may indicate whether a MUPIP REORG could improve the database performance. A native file system defragmentation improves physical adjacency.

.. note::
   Most modern SAN and I/O devices often mask the performance impact of the adjustments in logical and physical adjacency. If achieving a particular performance benchmark is your goal, increasing the logical and physical adjacency should be only one of many steps that you might undertake. While designing the database, try to ensure that the logical adjacency is close to the number of blocks that can physically reside on your hard disk's cylinder. You can also choose two or three cylinders, with the assumption that short seeks are fast.

The format of the MUPIP INTEG command is:

.. parsed-literal::
   I[NTEG] 
   [
    -A[DJACENCY]=integer
    -BL[OCK]=hexa;block-number
    -BR[IEF]
    -FA[ST]
    -FU[LL]
    -[NO]K[EYRANGES]
    -[NO]MAP[=integer]
    -[NO]MAXK[EYSIZE][=integer]
    -[NO]O[NLINE]
    -S[UBSCRIPT]=subscript] 
    -TN[_RESET]
    -[NO]TR[ANSACTION][=integer]
   ]
   {[-FILE] file-name|-REG[ION] region-list}



* MUPIP INTEG requires specification of either file(s) or region(s).

* Press <CTRL-C> to stop MUPIP INTEG before the process completes.

* The file-name identifies the database file for a MUPIP INTEG operation. The region-list identifies one or more regions that, in turn, identify database files through the current Global Directory.

* MUPIP INTEG operation keeps track of the number of blocks that do not have the current block version during a non-fast integ (default or full) and matches this value against the blocks to upgrade counters in the file-header. It issues an error if the values are unmatched and corrects the count in the file header if there are no other integrity errors.

.. note::
   Promptly analyze and fix all errors that MUPIP INTEG reports. Some errors may be benign while others may be signs of corruption or compromised database integrity. If operations continue without fixes to serious errors, the following problems may occur: Invalid application operation due to missing or incorrect data, Process errors (including inappropriate indefinite looping when a database access encounters an error), and degrading application level consistency as a result of incomplete update sequences caused by pre-existing database integrity issues. 

YottaDB strongly recommends fixing the following errors as soon as they are discovered:

* Blocks incorrectly marked free - these may cause accelerating damage when processes make updates to any part of the database region.

* Integrity errors in an index block - these may cause accelerating damage when processes make updates to that area of the database region using the faulty index. For more information, refer to `Chapter 11: “Maintaining Database Integrity” <https://docs.yottadb.com/AdminOpsGuide/integrity.html>`_.
  
MUPIP INTEG -FAST and the "regular" INTEG both report these errors (These qualifiers are described later in this section). Other database errors do not pose the threat of rapidly spreading problems in GDS files. After the YottaDB database repair, assess the type of damage, the risk of continued operations, and the disruption in normal operation caused by the time spent repairing the database. For information on analyzing and correcting database errors, refer to `Chapter 11: “Maintaining Database Integrity” <https://docs.yottadb.com/AdminOpsGuide/integrity.html>`_. Contact your YottaDB support channel for help assessing INTEG errors.

The following sections describe the qualifiers of the INTEG command.

~~~~~~~~~~~~
-ADJACENCY
~~~~~~~~~~~~

Specifies the logical adjacency of data blocks that MUPIP INTEG should assume while diagnosing the database. By default, MUPIP INTEG operates with -ADJACENCY=10 and reports the logical adjacency in the "Adjacent" column of the MUPIP INTEG report. 

* The complexity of contemporary disk controllers and the native file system may render this report superfluous. But when it is meaningful, this report measures the logical adjacency of data.

* A MUPIP REORG improves logical adjacency and a native file system defragmentation improves physical adjacency.

The format of the ADJACENCY qualifier is:

.. parsed-literal::
   -AD[JACENCY]=integer

~~~~~~~
-BLOCK
~~~~~~~

Specifies the block for MUPIP INTEG command to start checking a sub-tree of the database. MUPIP INTEG -BLOCK cannot detect "incorrectly marked busy errors".

The format of the BLOCK qualifier is:

.. parsed-literal::
   -BL[OCK]=block-number

* Block numbers are displayed in an INTEG error report or by using DSE.

* Incompatible with: -SUBSCRIPT and -TN_RESET

~~~~~~~
-BRIEF
~~~~~~~

Displays a single summary report by database file of the total number of directory, index and data blocks. The format of the BRIEF qualifier is:

.. parsed-literal::
   -BR[IEF]

* By default, MUPIP INTEG uses the BRIEF qualifier.

* Incompatible with: -FULL

~~~~~  
-FAST
~~~~~

Checks only index blocks. FAST does not check data blocks.

The format of the FAST qualifier is:

.. parsed-literal::
   -FA[ST]


* -FAST produces results significantly faster than a full INTEG because the majority of blocks in a typical database are data blocks.

* While INTEG -FAST is not a replacement for a full INTEG, it very quickly detects those errors that must be repaired immediately to prevent accelerating database damage.

* By default, INTEG checks all active index and data blocks in the database.

* -FAST reports include adjacency information.

* Incompatible with: -TN_RESET.

~~~~~~
-FILE
~~~~~~

Specifies the name of the database file for the MUPIP INTEG operation. FILE requires exclusive (stand-alone) access to a database file and does not require a Global Directory. The format of the FILE qualifier is:

.. parsed-literal::
   -FI[LE]

* With stand-alone access to the file, MUPIP INTEG -FILE is able to check whether the reference count is zero. A non-zero reference count indicates prior abnormal termination of the database.

* The -FILE qualifier is incompatible with the -REGION qualifier.

* By default, INTEG operates on -FILE.

~~~~~~  
-FULL
~~~~~~

Displays an expanded report for a MUPIP INTEG operation. With -FULL specified, MUPIP INTEG displays the number of index and data blocks in the directory tree and in each global variable tree as well as the total number of directory, index and data blocks. The format of the FULL qualifier is:

.. parsed-literal::
   -FU[LL]


* The -FULL qualifier is incompatible with the -BRIEF qualifier.

* By default, INTEG reports are -BRIEF.

* Use -FULL to have INTEG report all global names in a region or list of regions.

~~~~~~~~~~~  
-KEYRANGES
~~~~~~~~~~~

Specify whether the MUPIP INTEG report includes key ranges that it detects which identify the data suspected of problems. The format of the KEYRANGES qualifier is:

.. parsed-literal::
   -[NO]K[EYRANGES]

By default, INTEG displays -KEYRANGES.

~~~~~
-MAP
~~~~~

Specifies the maximum number of "incorrectly marked busy errors" that MUPIP INTEG reports. The format of the MAP qualifier is:

.. parsed-literal::
   -[NO]MAP[=max_imb_errors]


* <max_imb_errors> specifies the threshold limit for the number of incorrectly marked busy errors.

* -NOMAP automatically sets a high threshold limit of 1000000 (1 million) incorrectly marked busy errors (-MAP=1000000).

* By default, INTEG reports a maximum of 10 map errors (-MAP=10).

.. note::
   MUPIP INTEG reports all "incorrectly marked free" errors as they require prompt action. MAP does not restrict their reports.

An error in an index block prevents INTEG from processing potentially large areas of the database. A single "primary" error may cause large numbers of "secondary" incorrectly marked busy errors, which are actually useful in identifying valid blocks that have no valid index pointer. Because "real" or primary incorrectly marked busy errors only make "empty" blocks unavailable to the system, they are low impact and do not require immediate repair.

.. note::
   After a database recovery with -RECOVER (for example, using -BEFORE_TIME) or -ROLLBACK (for example, using -FETCHRESYNC), the database may contain incorrectly marked busy errors. Although these errors are benign, they consume available space. Schedule repairs on the next opportunity.

~~~~~~~~~~~~~
-MAXKEYSIZE
~~~~~~~~~~~~~

Specifies the maximum number of "key size too large" errors that a MUPIP INTEG operation reports. The format of the MAXKEYSIZE qualifier is:

.. parsed-literal::
   -[NO]MAX[KEYSIZE][=integer]

* By default, INTEG reports a maximum of 10 key size errors (-MAXKEYSIZE=10).

* -NOMAXKEYSIZE removes limits on key size reporting so that INTEG reports all "key size too large" errors.

* -NOMAXKEYSIZE does not accept assignment of an argument.

* "Key size too large" errors normally only occur if a DSE CHANGE -FILEHEADER -KEY_MAX_SIZE command reduces the maximum key size.

~~~~~~~~  
-ONLINE
~~~~~~~~

Specifies that while a MUPIP INTEG operation is active, other processes can update the database without affecting the result of the backup. Allows checking database structural integrity to run concurrently with database updates. The format of the ONLINE qualifier is:

.. parsed-literal::
   -[NO]O[NLINE]

* -NOONLINE specifies that the database should be frozen during MUPIP INTEG.

* By default, MUPIP INTEG is online. 

* Since MUPIP INTEG -ONLINE does not freeze database updates, it cannot safely correct errors in the "blocks to upgrade" and "free blocks" fields in the file header, while MUPIP INTEG -NOONLINE can correct these fields.

* As it checks each database file, MUPIP INTEG -ONLINE creates a sparse file of the same size as the database. As each YottaDB process updates the database, it places a copy of the old block in the sparse file before updating the database. For any database blocks with a newer transaction number than the start of the INTEG, MUPIP uses the copy in the sparse file. Thus, analogous with MUPIP BACKUP -ONLINE, INTEG reports on the state of the database as of when it starts, not when it completes. Note: a command such as ls -l shows sparse files at their full size, but does not show actual disk usage. Use a command such as du -sh to see actual disk usage.

* The environment variable ydb_snaptmpdir can be used to indicate a directory where MUPIP should place the snapshot files (used by MUPIP INTEG -ONLINE). If ydb_snaptmpdir does not exist, INTEG uses the location specified by ydb_baktmpdir and if neither of those environment variables is defined, INTEG places the snapshot files in the current directory at the time you issue the INTEG command. MUPIP and YottaDB processes automatically clean up these temporary snapshot files under a wide variety of conditions.

* Temporary directory security settings must allow write access by the MUPIP process and by all processes updating the database. MUPIP creates the temporary file with the same access as the database file so processes updating the database can write to the temporary file. If the database is encrypted, the updating processes write encrypted blocks to the snapshot file and the MUPIP INTEG process must start with access to appropriate key information as it does even -NOONLINE.

*  MUPIP INTEG -NOONLINE [-FAST] {-REGION|-FILE} clears the KILLs in progress and the Abandoned Kills flags if the run includes the entire database and there are no incorrectly marked busy blocks.

* Only one online integ can be active per database region. If an online integ is already active, a subsequent one issues an error and immediately terminates. If an online integ does not successfully complete, YottaDB cleans it up in one of the following ways: 

  * A subsequent online integ detects that an earlier one did not successfully complete and releases the resources held by the prior online integ before proceeding.
  * If a MUPIP STOP was issued to the online integ process, the process cleans up any resources it held. Note: since the process was stopped the results of the integ may not be valid.
  * subsequent MUPIP RUNDOWN ensures the release of resources held by prior unsuccessful online integs for the specified regions.
  * For every 64K transactions after the online integ initiation, online integ checks YottaDB's health for improperly abandoned online integs and releases resources held by any it finds.

* Incompatible with: -FILE, -TN_RESET (there should be no need to use -TN_RESET on a YottaDB database).

~~~~~~~~
-REGION
~~~~~~~~

Specifies that the INTEG parameter identifies one or more regions rather than a database file. The format of the REGION qualifier is:

.. parsed-literal::
   -R[EGION]=region-list

* The region-list identifies the target of INTEG. region-list may specify more than one region of the current global directory in a list. Regions are case-insensitive, separated by a comma, and wildcards can be used to specify them. Any region-name may include the wildcard characters * and ? (remember to escape them to protect them from inappropriate expansion by the shell). Any region name expansion occurs in M (ASCII) collation order.

* The region-list argument may specify more than one region of the current Global Directory in a list separated with commas. INTEG -REGION requires the environment variable ydb_gbldir to specify a valid Global Directory. For more information on defining ydb_gbldir, refer to `Chapter 4: “Global Directory Editor” <https://docs.yottadb.com/AdminOpsGuide/gde.html>`_

* Because a KILL may briefly defer marking the blocks it releases "free" in the bit maps, INTEG -REGION may report spurious "block incorrectly marked busy" errors. These errors are benign. If these errors occur in conjunction with a "Kill in progress" error, resolve the errors after the "Kill in progress" error is no longer present.

* By default, INTEG operates -FILE.

* Incompatible with: -FILE, -TN_RESET

~~~~~~~~~~~
-SUBSCRIPT
~~~~~~~~~~~

Specifies a global or a range of keys to INTEG. The global key may be enclosed in quotation marks (" "). Identify a range by separating two subscripts with a colon (:). -SUBSCRIPT cannot detect incorrectly marked busy errors. The format of the SUBSCRIPT qualifier is:

.. parsed-literal::
   -SU[BSCRIPT]=subscript

Specify SUBSCRIPT only if the path to the keys in the subscript is not damaged. If the path is questionable or known to be damaged, use DSE to find the block(s) and INTEG -BLOCK.

Incompatible with: -BLOCK, -TN_RESET

~~~~~~~
-STATS
~~~~~~~

Specifies INTEG to check any active statistics database associated with the region(s) specified for the command. The format of the STATS qualifier is:

.. parsed-literal::
   -[NO]ST[ATS]

Specify STATS only if you have reason to understand that statistics reporting is failing with database errors or reporting incorrect results. Because -FILE requires standalone access and statistic databases are automatically created and removed it is incompatible with -STATS. The default is NOSTATS.

Incompatible with: -BLOCK, -FILE, -TN_RESET

~~~~~~~~~~
-TN_RESET
~~~~~~~~~~

Resets block transaction numbers and backup event recorded transaction numbers to one (1), and the current transaction number to two (2) which makes the backup event recorded transaction numbers more meaningful and useful. It also issues an advisory message to perform a backup.

The format of the TN_RESET qualifier is:

.. parsed-literal::
   -TN[_RESET]

* Transaction numbers can go up to 18,446,744,073,709,551,615. This means that a transaction processing application that runs flat out at a non-stop rate of 1,000,000 updates per second would need a TN reset approximately every 584,554 years.

* The -TN_RESET qualifier rewrites all blocks holding data. If the transaction overflow resets to zero (0) database operation is disrupted.

* The -TN_RESET qualifier is a protective mechanism that prevents the transaction overflow from resetting to 0.

* By default, INTEG does not modify the block transaction numbers.

.. note::
   There should never be a need for a -TN_RESET on the database, even when cleaning up after a runaway process.

* The -TN_RESET qualifier is incompatible with the -FAST, -BLOCK, -REGION, and -SUBSCRIPT qualifiers.

.. note::
   Any time a YottaDB update opens a database file that was not properly closed, YottaDB increments the transaction number by 1000. This automatic increment prevents problems induced by abnormal database closes, but users must always consider this factor in their operational procedures. The rate at which YottaDB "uses up" transaction numbers is a function of operational procedures and real database updates.

~~~~~~~~~~~~~
-TRANSACTION
~~~~~~~~~~~~~

Specifies the maximum number of block transaction-number-too-large errors that MUPIP INTEG reports. The format of the TRANSACTION qualifier is:

.. parsed-literal::
   -[NO]TR[ANSACTION][=integer]

* -NOTRANSACTION removes limits on transaction reporting so MUPIP INTEG reports all transaction number errors.

* -NOTRANSACTION does not accept assignment of an argument.

* A system crash may generate many "block transaction number too large" errors. These errors can cause problems for BACKUP -INCREMENTAL and for transaction processing. Normally, the automatic increment of 1000 blocks that YottaDB adds when a database is reopened averts these errors. If a problem still exists after the database is reopened, users can use a value in the DSE CHANGE -FILEHEADER -CURRENT_TN= command to quickly fix "block transaction number too large" errors.

* By default, INTEG reports a maximum of 10 block transaction errors (-TRANSACTION=10).

**Examples for MUPIP INTEG**

Example:

.. parsed-literal::
   $ mupip integ -block=4 mumps.dat

This command performs a MUPIP INTEG operation on BLOCK 4 of mumps.dat.

Example:

.. parsed-literal::
   $ mupip integ -adjacency=20

A sample output from the above command follows:

.. parsed-literal::
   Type           Blocks         Records          % Used      Adjacent
   Directory           2             110          25.732            NA
   Index            1170          341639          88.298             6
   Data           340578          519489          99.268        337888
   Free             6809              NA              NA            NA
   Total          348559          861238              NA        337894
   [Spanning Nodes:3329 ; Blocks:341403]

This example performs a MUPIP INTEG operation assuming that logically related data occupies 20 data blocks in the current database. The sample output shows that out of 1137 data blocks, 1030 data blocks are adjacent to each other. One may be able to improve the performance of a database if all blocks are as adjacent as possible. "% Used" is the amount of space occupied across the in-use blocks divided by the space available in the in-use blocks, and thus represents the packing density for the in-use blocks (excluding local bit maps). Higher "% Used" may actually be undesirable from a performance perspective as they indicate a higher likelihood of block splits with upcoming updates.

Example:

.. parsed-literal::
   $ mupip integ -brief mumps.dat

This command performs a MUPIP INTEG operation on the database mumps.dat. A sample output from the above command follows:

.. parsed-literal::
   No errors detected by integ.
   Type           Blocks         Records          % Used      Adjacent
   Directory           2             110          25.732            NA
   Index            1170          341639          88.298             4
   Data           340578          519489          99.268        337617
   Free             6809              NA              NA            NA
   Total          348559          861238              NA        337621
   [Spanning Nodes:3329 ; Blocks:341403]

Example:

.. parsed-literal::
   $ mupip integ -fast mumps.dat

This command performs a MUPIP INTEG operation only on the index block of the database file mumps.dat. A sample output from the above command follows:

.. parsed-literal::
   No errors detected by fast integ.
    
  Type           Blocks         Records          % Used      Adjacent
  Directory           2             110          25.732            NA
  Index            1170          341639          88.298             4
  Data           340578              NA              NA        337617
  Free             6809              NA              NA            NA
  Total          348559              NA              NA        337621

Note the NA entries for Data type. It means that the MUPIP INTEG -FAST operation checked only index blocks.

.. parsed-literal::
   $ mupip integ -full mumps.dat

The sample output from the above command follows:

.. parsed-literal::
   
   Directory tree
   Level          Blocks         Records          % Used      Adjacent
      1               1               1           0.585           NA
      0               1             109          50.878           NA
   Global variable ^#t
   Level          Blocks         Records          % Used      Adjacent
      1               1               1           0.585             0
      0               1              80          49.609             1
   Global variable ^versionContent
   Level          Blocks         Records          % Used      Adjacent
      1               1               1           0.585             0
      0               1               1          94.018             0
   Global variable ^x
   Level          Blocks         Records          % Used      Adjacent
      1               1               2           1.464             0
      0               2             109          52.551             1
  
Example:

.. parsed-literal::
   $ mupip integ -map=20 -maxkeysize=20 -transaction=2 mumps.dat

This command performs a MUPIP INTEG operation and restricts the maximum number of "key size too large" errors to 20.

Example:

.. parsed-literal::
   $ mupip integ -map=20 -transaction=2 mumps.dat

This command performs a MUPIP INTEG operation and restricts the maximum number of "block transaction number too large" errors to 2.

.. parsed-literal::
   $ mupip integ -file mumps.dat -tn_reset

This command resets the transaction number to one in every database block.

Example:

.. parsed-literal::
   $ mupip integ -subscript="^Parrots" mumps.dat

This example performs a MUPIP INTEG operation on the global variable ^Parrots in the database file mumps.dat.

Example:

.. parsed-literal::
   $ mupip integ -subscript="^Amsterdam(100)":"^Bolivia(""Chimes"")" -region DEFAULT

This example performs a MUPIP INTEG operation all global variables greater than or equal to ^Amsterdam (100) and less than or equal to ^Bolivia("Chimes") in the default region(s).

.. note::
   To specify a literal in the command string, use two double quotation marks for example, ^b(""c""). 

+++++++
INTRPT
+++++++

.. parsed-literal::
   INTRPT process-id

.. note::
   Ensure that signal SIGUSR1 is not be used by any C external function calls or any (initially non-YottaDB) processes that use call-in support, as it is interpreted by YottaDB as a signal to trigger the $ZINTERRUPT mechanism.

* To INTRPT a process belonging to its own account, a process requires no UNIX privileges.

* To INTRPT a process belonging to its own GROUP, a process requires UNIX membership in the user group of the target process privilege. To INTRPT a process belonging to an account outside its own GROUP, a process requires the UNIX superuser privilege.

++++++++++
JOURNAL
++++++++++

Analyzes, extracts, reports, and recovers data using journal files. For a description of the JOURNAL command, refer to `Chapter 6: “YottaDB Journaling” <https://docs.yottadb.com/AdminOpsGuide/ydbjournal.html>`_.

+++++++
LOAD
+++++++

Puts the global variable names and their corresponding data values into a YottaDB database from a sequential file.

The format of the LOAD command is:

.. parsed-literal::
   L[OAD] 
   [-BE[GIN]=integer -E[ND]=integer 
   -FI[LLFACTOR]=integer 
   -FO[RMAT]={GO|B[INARY]|Z[WR]]} 
   -[O]NERROR={STOP|PROCEED|INTERACTIVE} 
   -S[TDIN]] file-name

.. note::
   From an application perspective, performing a MUPIP LOAD operation while an application is running may result in an inconsistent application state for the database.

* MUPIP LOAD uses the Global Directory to determine which database files to use.

* LOAD supports user collation routines.

* LOAD takes its input from the file defined by the file-name, which may be a UNIX file on any device that supports such files.

* LOAD accepts files with DOS style termination.

* MUPIP LOAD command considers a sequential file as encoded in UTF-8 if the environment variable ydb_chset is set to UTF-8. Ensure that MUPIP EXTRACT commands and the corresponding MUPIP LOAD commands execute with the same setting for the environment variable ydb_chset.

* For information on loading with an M "percent utility," refer to the `%GI section of the "M Utility Routines" chapter in the Programmer's Guide <https://docs.yottadb.com/ProgrammersGuide/utility.html#gi>`_. LOAD is typically faster, but the %GI utility can be customized.

* Press <CTRL-C> to produce a status message from LOAD. Entering <CTRL-C> twice in quick succession stops LOAD. A LOAD that is manually stopped or stops because of an internal error is incomplete and may lack application level integrity, but will not adversely affect the database structure unless terminated with a kill -9.

.. note::
   The MUPIP EXTRACT or MUPIP LOAD procedure for large databases is time consuming due to the volume of data that has to be converted from binary to ZWR format (on source) and vice versa (on target). One must also consider the fact that the extracted file can be very large for a large database. Users must ensure there is adequate storage space to support the size of the extract file and the space occupied by the source and target databases. In order to reduce the total time and space it takes to transfer database content from one endian platform to another, it is efficient to convert the endian format in-place for a database and transfer the converted database. See MUPIP ENDIANCVT for more information on converting the endian format of a database file. 

The following sections describe the optional qualifiers of the MUPIP LOAD command.

~~~~~~~~
-FORMAT
~~~~~~~~

Specifies the format of the input file. If the format of the input file is not specified, MUPIP LOAD automatically detects the file format (BINARY/ZWR/GO) based on the file header of the input file. If the format is specified, it must match the actual format of the input file for LOAD to proceed.

The format codes are:

.. parsed-literal::
   B[INARY] - Binary format
   GO - Global Output format
   Z[WR] - ZWRITE format

* MUPIP LOAD detects the file format (BINARY/ZWR/GO) based on the file header of the extracted files from MUPIP EXTRACT, ^%GO and DSE.

* -FORMAT=BINARY only applies to GDS files. A BINARY format file loads significantly faster than a GO or ZWR format file. -FORMAT=BINARY works with data in a proprietary format. -FORMAT=BINARY has one header record, therefore LOAD -FORMAT=BINARY starts active work with record number two (2).

* -FORMAT={ZWR|GO} applies to text files produced by tools such as MUPIP EXTRACT or %GO.

* For FORMAT={ZWR|GO} UTF-8 files not produced by MUPIP EXTRACT or %GO, the first line of the label must contain the case insensitive text "UTF-8".

* For all -FORMAT={ZWR|GO} files not produced by MUPIP EXTRACT or %GO, the second line should contain the case insensitive test "ZWR" for zwr format or "GLO" for GO format and the two label lines must contain a total of more than 10 characters.

* -FORMAT=GO expects the data in record pairs. Each global node requires one record for the key and one for the data.

* -FORMAT=ZWR expects the data for each global node in a single record.

~~~~~~~
-BEGIN
~~~~~~~

Specifies the record number of the input file with which LOAD should begin. Directing LOAD to begin at a point other than the beginning of a valid key causes an error. The format of the BEGIN qualifier is:

.. parsed-literal::
   -BE[GIN]=integer

.. note::
   Always consider the number of header records for choosing a -BEGIN point. See FORMAT qualifier for more information.


* For -FORMAT=GO input, the value is usually an odd number. As -FORMAT=BINARY requires important information from the header, this type of load requires an intact file header regardless of the -BEGIN value.

* For -FORMAT = ZWR input, each record contains a complete set of reference and data information. The beginning values are not restricted, except to allow two records for the header.

* By default, LOAD starts at the beginning of the input file.

~~~~~  
-END
~~~~~

Specifies the record number of the input file at which LOAD should stop. -END=integer must be greater than the -BEGIN=integer for LOAD to operate. LOAD terminates after processing the record of the number specified by -END or reaching the end of the input file. The format of the END qualifier is:

.. parsed-literal::
   -E[ND]=integer

The value of -FORMAT=GO input should normally be an even number. By default, LOAD continues to the end of the input file.

~~~~~~~~~~~~
-FILLFACTOR
~~~~~~~~~~~~

Specifies the quantity of data stored in a database block. Subsequent run-time updates to the block fill the remaining available space reserved by the FILL_FACTOR. Blocks that avoid block splits operate more efficiently. The format of the FILL_FACTOR qualifier is:

.. parsed-literal::
   -FI[LL_FACTOR]=integer

* Reserves room and avoids unnecessary block splits to accommodate the forecasted growth in a global variable that may experience significant rate of additions over a period of time.

* Users having database performance issues or a high rate of database updates must examine the defined FILL_FACTORs. Unless the application only uses uniform records, which is not typical for most applications, FILL_FACTORs do not work precisely.

* By default, LOAD uses -FILL_FACTOR=100 for maximum data density.

.. note::
   FILL_FACTOR is useful when updates add or grow records reasonably uniformly across a broad key range. If updates are at ever-ascending or ever-descending keys, or if the record set and record sizes are relatively static in the face of updates, FILL_FACTOR does not provide much benefit.

~~~~~~~~~
-ONERROR
~~~~~~~~~

Determines the MUPIP LOAD behavior when it encounters an error. The format of the ONERROR qualifier is:

.. parsed-literal::
   -[O]NERROR={STOP|PROCEED|INTERACTIVE}

- STOP causes MUPIP LOAD to exit immediately.

- PROCEED proceeds to the next record.

- INTERACTIVE prompts to continue or stop.

By default MUPIP LOAD exits on encountering an error. 

~~~~~~~
-STDIN
~~~~~~~

Specifies that MUPIP LOAD takes input from standard input (stdin). The format of the STDIN qualifier is:

.. parsed-literal::
   -S[TDIN]

**Examples for MUPIP LOAD**

Example:

.. parsed-literal::
   $ mupip load ex_file.go

This command loads the content of the extract file ex_file.go to the current database.

Example:

.. parsed-literal::
   $ mupip load -format=go big.glo

This command loads an extract file big.glo in the current database.

Example:

.. parsed-literal::
   $ mupip load -begin=5 -end=10 rs.glo

This command begins the MUPIP LOAD operation from record number 5 and ends at record number 10. Note that the value for BEGIN is an odd number. A sample output from the above command follows:

.. parsed-literal::
   MUPIP EXTRACT
   02-MAR-2017 18:25:47 ZWR
   Beginning LOAD at record number: 5
   LOAD TOTAL Key Cnt: 6 
   Max Subsc Len: 7 
   Max Data Len: 1
   Last LOAD record number: 10

Example:

.. parsed-literal::
   $ mupip load -fill_factor=5 reobs.glo

This command sets the FILL_FACTOR to 5 for loading an extract file in the current database.

Example:

.. parsed-literal::
   $cat big.glo | mupip load -stdin
   $mupip load -stdin < big.glo

These commands loads the extract file big.glo using -stdin.

++++++++
RCTLDUMP
++++++++

Reports information related to relinkctl files and their associated shared memory segments. The format of the MUPIP RCTLDUMP command is:

.. parsed-literal::
   MUPIP RCTLDUMP [dir1]

If the optional parameter dir1 is not specified, MUPIP RCTLDUMP dumps information on all its active auto-relink-enabled directories (those with with a \*-suffix) identified by $ydb_routines. With a directory path specified for dir1, MUPIP RCTLDUMP reports information on that directory. An example output follows. It lists the full path of the Object directory; its corresponding relinkctl file name; the number of routines currently loaded in this relinkctl file; the number of processes including the reporting MUPIP process that have this relinkctl file open; the shared memory id and length of the relinkctl shared memory segment; one or more rtnobj shared memory segment(s); and a listing of all the routine names loaded in this file (lines starting with rec#...).

* The Rtnobj shared memory line : All the length fields are displayed in hexadecimal. shmlen is the length of the allocated shared memory segment in bytes. shmused is the length that is currently used. shmfree is the length available for use. objlen is the total length of all the objects currently loaded in this shared memory. As YottaDB allocates blocks of memory with sizes rounded-up to an integer power of two bytes, shmused is always greater than objlen; for example with an objlen of 0x1c0, the shmused is 0x200.

* Lines of the form rec#... indicate the record number in the relinkctl file. Each relinkctl file can store a maximum of 1,000,000 records, i.e., the maximum number of routines in a directory with auto-relink enabled is one million. Each record stores a routine name (rtnname:), the current cycle for this object file record entry (cycle:) which gets bumped on every ZLINK or ZRUPDATE command, the hash of the object file last loaded for this routine name (objhash:), the number of different versions of object files loaded in the Rtnobj shared memory segments with this routine name (numvers:), the total byte-length of the one or more versions of object files currently loaded with this routine name (objlen:), the total length used up in shared memory for these object files where YottaDB allocates each object file a rounded-up perfect 2-power block of memory (shmlen:).

Given a relinkctl file name, one can find the corresponding directory path using the Unix "strings" command on the Relinkctl file. For example, "strings /tmp/ydb-relinkctl-f0938d18ab001a7ef09c2bfba946f002", corresponding to the above MUPIP RCTLDUMP output example, would output "/obj" the corresponding directory name.

Example:

.. parsed-literal::
   $ mupip rctldump .
   Object Directory         : /tmp
   Relinkctl filename       : /tmp/yottadb/r1.20_x86_64/ydb-relinkctl-61f9eb418212a24a75327f53106c1656
   # of routines            : 1
   # of attached processes  : 2
   Relinkctl shared memory  : shmid: 11534344 shmlen: 0x57c6000
   Rtnobj shared memory # 1 : shmid: 11567113 shmlen: 0x100000 shmused: 0x200 shmfree: 0xffe00 objlen: 0x1c0
   rec#1: rtnname: abcd cycle: 1 objhash: 0xedbfac8c7f7ca357 numvers: 1 objlen: 0x1c0 shmlen: 0x200


+++++
REORG
+++++

Improves database performance by defragmenting and reorganizing database files and attempts to reduce the size of the database file. MUPIP REORG runs concurrently with other database activity, including updates. Competing activity generally increases the time required to perform a REORG, as well as that of the competing operations.

MUPIP REORG can also encrypt a database and/or change the encryption keys for database files "on the fly" while the database is in use.

The format of the MUPIP REORG command is:

.. parsed-literal::
   REO[RG] 
   [
    -D[OWNGRADE]
    -ENCR[YPT]=key
    -E[XCLUDE]=global-name-list
    -FI[LL_FACTOR]=integer
    -I[NDEX_FILL_FACTOR]=integer
    -R[ESUME]
    -S[ELECT]=global-name-list
    -T[RUNCATE][=percentage]
    -UP[GRADE]
    -REG[ION] region-list
   ]

.. note::
   While REORG optimizes the GDS structure of database files, it does not handle native file system file fragmentation. In most cases, fragmentation at the native file system level is more likely than fragmentation at the GDS structure level. Therefore, YottaDB recommends users create files with appropriate allocations and extensions, on disks with large amounts of contiguous free space. Use native utilities and MUPIP utilities (depending on operational procedures) to eliminate file fragmentation when database files have been extended more than a dozen times.

* As REORG is IO intensive, running a REORG concurrently with normal database access may impact the operation of normal processes. As the YottaDB database engine has a daemonless architecture, attempts to reduce the impact by reducing the priority of REORG can (perhaps counter-intuitively) exacerbate rather than alleviate the impact. To reduce the impact REORG has on other processes, use the ydb_poollimit environment variable to limit the number of global buffers used by the REORG.

* MUPIP REORG does not change the logical contents of the database, and can run on either the originating instance or replicating instance of an LMS application. In such cases, resuming REORGs in process should be part of the batch restart. See the `"Database Replication" chapter <https://docs.yottadb.com/AdminOpsGuide/dbrepl.html>`_ for more information about running REORG on a dual site application.

* Use MUPIP STOP (or <Ctrl-C> for an interactive REORG) to terminate a REORG process. Unless terminated with a kill -9, a REORG terminated by operator action or error is incomplete but does not adversely affect the database.

.. note::
   REORG focuses on optimum adjacency and a change to even a single block can cause it to perform a large number of updates with only marginal benefit. Therefore, YottaDB recommends not running successive REORGs close together in time, as much as can provide minimal benefit for a significant increase in database and journal activity. For the same reason, YottaDB recommends careful research and planning before using the -RESUME qualifier or complex uses of -EXCLUDE and -SELECT.

Assume two scenarios of putting values of ^x(1) to ^x(10000). In the first scenarios, fill values in a sequential manner. In the second scenario, enter values for odd subscripts and then enter values for the even subscripts.

Scenario 1:

At the prompt, execute the following command sequence:

.. parsed-literal::
   YDB>for i=1:1:10000 set ^x(i)=$justify(i,200)

Then, execute the following MUPIP INTEG command.

.. parsed-literal::
   $ mupip integ -region "*"

This command produces an output like the following:

.. parsed-literal::
   Integ of region DEFAULT
   No errors detected by integ.
   Type           Blocks         Records          % Used      Adjacent
   Directory           2               2           2.490            NA
   Index              29            2528          95.999             1
   Data             2500           10000          82.811          2499
   Free               69              NA              NA            NA
   Total            2600           12530              NA          2500

Note the high density (percent used) for index and data blocks from the report.

Scenario 2:

At the YottaDB prompt, execute the following command sequence:

.. parsed-literal::
   YDB>for i=1:2:10000 s ^x(i)=$justify(i,200)
   YDB>for i=2:2:10000 set ^x(i)=$justify(i,200)

Then, execute the following command:

.. parsed-literal::
   $ mupip integ -region "*"

This command produces an output like the following:

.. parsed-literal::
   Integ of region DEFAULT
   No errors detected by integ.
   Type           Blocks         Records          % Used      Adjacent
   Directory           2               2           2.490            NA
   Index             153            3902          29.211            57
   Data             3750           10000          55.856          1250
   Free               95              NA              NA            NA
   Total            4000           13904              NA          1307

Note that there are more and less dense index and data blocks used than in scenario 1. MUPIP REORG addresses such issues and makes the database (depending on the FILL_FACTOR) more compact.

The optional qualifiers for MUPIP REORG are:

~~~~~~~~~
-ENCRYPT
~~~~~~~~~

Encrypts an unencrypted database or changes the encryption key of a database while the database continues to be used by applications. Whether or not the prior encryption uses non-zero initialization vectors (IVs), database blocks encrypted with the new key use non-zero IVs. The format of the ENCRYPT qualifier is:

.. parsed-literal::
   -ENCR[YPT]=<key>

MUPIP provides <key> to the encryption plugin. The reference implementation of the plugin expects a key with the specified name in the encryption configuration file identified by $ydb_crypt_config. The configuration file must contain an entry in the database section for each database file mapping to a region specified in <region-list> that names the specified key as its key. The -ENCRYPT flag is incompatible with all other command line flags of MUPIP REORG except -REGION, and performs no operation other than changing the encryption key. If the specified key is already the encryption key of a database region, MUPIP REORG -ENCRYPT moves on to the next region after displaying a message (on stderr, where MUPIP operations send their output).

As MUPIP REORG -ENCRYPT reads, re-encrypts, and writes every in-use block in each database file, its operations take a material amount of time on the databases of typical applications, and furthermore add an additional IO load to the system on which it runs. You can use the environment variable ydb_poollimit to ameliorate, but not eliminate the impact, at the cost of extending execution times. To minimize impact on production instances, YottaDB recommends running this operation on replicating secondary instances, rather than on originating primary instances.

-ENCRYPT switches the journal file for each database region when it begins operating on it, and again when it completes, and also records messages in the syslog for both events.

As is the case under normal operation when MUPIP REORG -ENCRYPT is not active, journaled databases are protected against system crashes when MUPIP REORG -ENCRYPT is in operation: MUPIP JOURNAL -ROLLBACK / -RECOVER recovers journaled database regions (databases that use NOBEFORE journaling continue to require -RECOVER / -ROLLBACK -FORWARD).

Because a database file utilizes two keys while MUPIP REORG -ENCRYPT is underway, the database section of the configuration file provides for a single database file entry to specify multiple keys. For example, if the keys of database regions CUST and HIST, mapping to database files cust.dat and hist.dat in directory /var/myApp/prod, are to be changed from key1 to key2 using the command:

.. parsed-literal::
   MUPIP REORG -ENCRYPT=key2 -REGION CUST,HIST

then the database section of the configuration file must at least have the following entries:

.. parsed-literal::
    database: {
        keys: ({
                dat: "/var/myApp/cust.dat";
                key: "key1";
               },{
                  dat: "/var/myApp/cust.dat";
                  key: "key2";
                 },{
                    dat: "/var/myApp/hist.dat";
                    key: "key1";
                   },{
                      dat: "/var/myApp/hist.dat";
                      key: "key2";
                     })
              };


In other words, each database file entry can have multiple keys, and a key can be associated with multiple database files. With a configuration file that has multiple keys associated with the same database file, MUPIP CREATE uses the last entry. Other database operations use whichever key associated with the database file has a hash matching one in the database file header, reporting an error if no key matches. To improve efficiency when opening databases, you can delete entries for keys that are no longer used from the configuration file.

MUPIP REORG -ENCR[YPT] can encrypt an unencrypted database only if the following command:

.. parsed-literal::
   MUPIP SET -ENCRYPTABLE -REGION <region-list>

has previously marked the database "encryptable".

The command requires standalone access to the database.  It performs some basic encryption setup checks and requires the ydb_passwd environment variable to be defined and the GNUPGHOME environment variable to point to a valid directory in the environment. Just as encrypted databases use global buffers in pairs (for encrypted and unencrypted versions of blocks), a database marked as encryptable has global buffers allocated in pairs (i.e., the actual number of global buffers is twice the number reported by DSE DUMP -FILEHEADER) and requires correspondingly larger shared memory segments. To revert unencrypted but encryptable databases back to the "unencryptable" state, use the command:

.. parsed-literal::
   MUPIP SET -NOENCRYPTABLE -REGION <region-list>

The above command also requires standalone access, and the result depends on the state of the database. It:

* is a no-op if the database is encrypted;

* is disallowed if the database is partially (re)encrypted; and

* prohibits encryption if the database is not encrypted.

Under normal operation, a database file has only one key. Upon starting a MUPIP REORG -ENCRYPT to change the key, there are two keys, both of whose hashes YottaDB stores in the database file header. With a MUPIP REORG -ENCRYPT operation underway to change the key, normal database operations can continue, except for another MUPIP REORG -ENCRYPT or MUPIP EXTRACT in binary format. Other MUPIP operations, such as MUPIP BACKUP and MUPIP EXTRACT in ZWR format can occur. A MUPIP REORG -ENCRYPT operation can resume after an interruption, either unintentional, such as after a system crash and recovery, or intentional, i.e., an explicit MUPIP STOP of the MUPIP REORG -ENCRYPT process. To resume the REORG operation, reissue the original command, including the key parameter. (Note that supplying a key other than the one used in the original command produces an error.)

After the MUPIP REORG -ENCRYPT process completes, subsequent MUPIP REORG -ENCRYPT operations on the same region(s) are disallowed until the following command is run:

.. parsed-literal::
   MUPIP SET -ENCRYPTIONCOMPLETE -REGION <region-list>

Blocking subsequent MUPIP REORG -ENCRYPT operations after one completes, provides time for a backup of the entire database before enabling further key changes. MUPIP SET -ENCRYPTIONCOMPLETE reports an error for any database region for which MUPIP REORG -ENCRYPT has not completed.

.. note::
   MUPIP REORG -ENCRYPT does not enable switching between encryption algorithms. To migrate databases from Blowfish CFB to AES CFB requires that the data be extracted and loaded into newly created database files. To minimize the time your application is unavailable, you can deploy your application in a Logical Multi-Site (LMS) configuration, and migrate using a rolling upgrade technique. Refer to the `Chapter 7: “Database Replication” <https://docs.yottadb.com/AdminOpsGuide/dbrepl.html>`_ for more complete documentation. 

~~~~~~~~~
-EXCLUDE
~~~~~~~~~

Specifies that REORG not handle blocks that contain information about the globals in the associated list–this means they are neither reorganized nor swapped in the course of reorganizing other globals; -EXCLUDE can reduce the efficiency of REORG because it complicates and interferes with the block swapping actions that try to improve adjacency.

The format of the EXCLUDE qualifier is:

.. parsed-literal::
   -E[XCLUDE]=global-name-list

* Assume that a single MUPIP command organizes a subset of the globals in a database or region. If a second MUPIP REORG command selects the remaining globals, it may tend to disrupt the results of the first REORG by de-optimizing the previously organized blocks. This is because there is no information passed from the previous MUPIP REORG command to the next command. The EXCLUDE qualifier allows users to list the name of the previously REORGed globals, so that the MUPIP REORG bypasses the GDS blocks containing these globals.

* If global-name-list contains globals that do not exist, REORG issues a message to the terminal and continues to process any specified globals that exist. If REORG is unable to process any globals, it terminates with an error.

* Global-name-list can be an individual global name, a range of global names, or a list of names and prefixes followed by the wildcard symbol. For example:
   
   * A global name, such as ACN.

   * A range of global names, such as A7:B7.

   * A list, such as A,B,C.

   * Global names with the same prefix such as TMP*.

   In the first case, REORG only excludes global ^ACN. In the second case, REORG excludes all global names in the collating sequence A7 to B7. For the third case, REORG excludes A, B, and C. In the last case, REORG excludes all globals prefixed with TMP.

* Enclose wildcards in double-quotes ("") to prevent inappropriate expansion by the shell. The caret symbol (^) in the specification of the global is optional.

* By default, REORG does not EXCLUDE any globals.

* In case any global appears in the argument lists of both -SELECT and -EXCLUDE, REORG terminates with an error. 

~~~~~~~~~~~~~
-FILL_FACTOR
~~~~~~~~~~~~~

Specifies how full you want each database block to be. This is a target number. Individual blocks may be more or less full than the fill factor. The format of the FILL_FACTOR qualifier is:

.. parsed-literal::
   F[ILL_FACTOR]=integer

* The arguments for the FILL_FACTOR qualifier must be integers from 30 to 100. These integers represent the percentage of the data block that REORG can fill. By default, the FILL_FACTOR value is 100 for maximum data density.

* Users who come upon database performance issues or a high rate of database updates must examine the defined FILL_FACTORs. Unless the application uses entirely uniform records, which is not typical for most applications, FILL_FACTORs do not work precisely.

* The FILL_FACTOR for data that is relatively static, or grows by the addition of new nodes that collate before or after pre-existing nodes, should be 100 percent. The FILL_FACTOR for data that is growing by additions to existing nodes may be chosen to leave room in the typical node for the forecast growth for some period. Generally, this is the time between the LOAD and first REORG, or between two REORGs. This is also true for additions of nodes that are internal to the existing collating sequence.

~~~~~~~~~~~~~~~~~~~
-INDEX_FILL_FACTOR
~~~~~~~~~~~~~~~~~~~

Directs REORG to leave free space within index blocks for future updates. Arguments to this qualifier must be integers from 30 to 100 that represent the percentage of the index block that REORG can fill. REORG uses this number to decide whether to place more information in an index block, or create space by moving data to another block. The format of the INDEX_FILL_FACTOR qualifier is:

.. parsed-literal::
   -I[NDEX_FILL_FACTOR]=integer

Under certain conditions, especially with large database block sizes, it may be possible to achieve faster throughput by using a smaller fill factor for index blocks than for data blocks. By default, the INDEX_FILL_FACTOR is the value of FILL_FACTOR regardless of whether that value is explicitly specified or implicitly obtained by default.

~~~~~~~~~
-RESUME
~~~~~~~~~

For an interrupted REORG operation, -RESUME allows the user to resume the REORG operation from the point where the operation stopped. REORG stores the last key value in the database file header. The format of the RESUME qualifier is:

.. parsed-literal::
   -R[ESUME]

* With RESUME specified, the program retrieves the last key value, from the database file header, and restarts operations from that key. 

~~~~~~~~  
-REGION
~~~~~~~~

Specifies that REORG operate in the regions in the associated list and restricts REORG to the globals in those regions that are mapped by the current global directory; it does not have the same interactions as -EXCLUDE and -SELECT, but it does not mitigate those interactions when combined with them.

The format of the REGION qualifier is:

.. parsed-literal::
   -R[EGION] region-list

region-list may specify more than one region of the current global directory in a list. Regions are case-insensitive, separated by a comma, and wildcards can be used to specify them. Any region-name may include the wildcard characters * and % (remember to escape them to protect them from inappropriate expansion by the shell). Any region name expansion occurs in M (ASCII) collation order.

~~~~~~~~~
-SELECT
~~~~~~~~~

Specifies that REORG reorganizes only the globals in the associated list; globals not on the list may be modified by block swaps with selected globals unless they are named with -EXCLUDE; -SELECT can be difficult to use efficiently because it tends to de-optimize unselected globals unless they are named in an -EXCLUDE list (which introduces inefficiency).

The format of the SELECT qualifier is:

.. parsed-literal::
   -S[ELECT]=global-name-list

* By default, REORG operates on all globals in all database files identified by the current Global Directory for the process executing the MUPIP command.

* One of the functions performed by REORG is to logically order the globals on which it operates within the file. Unless the EXCLUDE and SELECT qualifiers are properly used in tandem, repeating the command with different selections in the same file wastes work and leaves only the last selection well organized.

* If you enter the REORG -SELECT=global-name-list command and the specified globals do not exist, REORG issues a message to the screen and continues to process any specified globals that exist. If REORG is unable to process any globals, it terminates with an error.

* Arguments for this qualifier may be an individual global name, a range of global names, or a list of names and prefixes followed by the wildcard symbol. The caret symbol (^) in the specification of the global is optional.

* The global name can be:
  
  * A global name, such as ACN
  * A range of global names, such as A7:B7
  * A list, such as A,B,C.
  * Global names with the same prefix such as TMP*. 

* In the first case, REORG only includes global ^ACN. In the second case, REORG includes all global names in the collating sequence A7 to B7. For the third case, REORG includes A, B, and C. In the last case, REORG includes all globals prefixed with TMP.

* By default, REORG selects all globals.

~~~~~~~~~~  
-TRUNCATE
~~~~~~~~~~

Specifies that REORG, after it has rearranged some or all of a region's contents, should attempt to reduce the size of the database file and return free space to the file system. The format of the TRUNCATE qualifier is:

.. parsed-literal::
   -T[RUNCATE][=percentage]

The optional percentage (0-99) provides a minimum amount for the reclamation; in other words, REORG won't bother performing a file truncate unless it can give back at least this percentage of the file; the default (0) has it give back anything it can. TRUNCATE always returns space aligned with bit map boundaries, which fall at 512 database block intervals. TRUNCATE analyses the bit maps, and if appropriate, produces before image journal records as needed for recycled (formerly used) blocks; The journal extract of a truncated database file may contain INCTN records having the inctn opcode value 9 indicating that the specific block was marked from recycled to free by truncate.

.. note::
   TRUNCATE does not complete if there is a concurrent online BACKUP or use of the snapshot mechanism, for example by INTEG.

**Examples for MUPIP REORG**

Example:

.. parsed-literal::
   $ mupip reorg 
   Fill Factor:: Index blocks 100%: Data blocks 100%
      
   Global: CUST (region DEFAULT)
   Blocks processed    : 667340 
   Blocks coalesced    : 601487 
   Blocks split        : 0 
   Blocks swapped      : 319211 
   Blocks freed        : 646964 
   Blocks reused       : 298814 
   Blocks extended     : 0 
   
   Global: HIST (region HIST)
   %YDB-I-FILERENAME, File /var/myApp/prod/journals/hist.mjl is renamed to /var/myApp/prod/journals/hist.mjl_2015289165050
   Blocks processed    : 337069 
   Blocks coalesced    : 12888 
   Blocks split        : 0 
   Blocks swapped      : 329410 
   Blocks freed        : 315998 
   Blocks reused       : 308337
   Levels Eliminated   : 1 
   Blocks extended     : 0 
   $

In this output:

* Blocks processed - the number of blocks originally used by the global variable

* Blocks coalesced - the number of blocks that were sufficiently compacted enough to free a block

* Blocks split - the number of blocks expanded enough to require the allocation of a new block

* Blocks swapped - the number of blocks moved to improve adjacency; this can exceed the number of blocks processed as a consequence of the movement of blocks

* Blocks freed - the number of blocks formerly used that were released by a combination of swaps and coalesces

* Blocks reused - blocks freed, and then reused

* Levels eliminated - reduction in the depth of the global variable tree

* Blocks extended - the number of blocks the database grew during the reorg

Note also that owing the database update activity of REORG, the hist.mjl journal file reached its limit, requiring MUPIP to switch the journal file.

Example:

.. parsed-literal::
   $ mupip reorg -exclude="^b2a,^A4gsEQ2e:^A93"

This example performs a MUPIP REORG operation on all globals excluding ^b2a and all globals ranging from ^A4gsEQ2e to ^A93.

Example:

If the forecasted growth of a global is 5% per month from relatively uniformly distributed updates, and REORGs are scheduled every quarter, the FILL_FACTOR for both the original LOAD and subsequent REORGs might be 80 percent 100 - ((3 months + 1 month "safety" margin) * five percent per month). The REORG command based on the above assumptions is as follows:

.. parsed-literal::
   $ mupip reorg -fill_factor=80 

Example:

The following example uses recorg -encrypt to encrypt a database "on the fly". This is a simple example created for demonstration purposes. It is NOT recommended for production use. Consult your YottaDB support channel for specific instructions on encrypting an unencrypted database.

Create an empty default unencrypted database. 

.. parsed-literal::
   $ydb_dist/mumps -r ^GDE exit
   $ydb_dist/mupip create

Setup the GNUPG home directory.

.. parsed-literal::
   export GNUPGHOME=$PWD/.helengnupg3    
   mkdir $GNUPGHOME # Ensure that you protect this directory with appropriate permissions.  
   chmod go-rwx $GNUPGHOME

Create a new key. Enter demo values. Accept default values. Choose a strong passphrase.

.. parsed-literal::
   gpg --gen-key  

Edit the key to add a new sub-key:

.. parsed-literal::
   gpg --edit-key helen.keymaster@yottadb

Type addkey, select option 6 RSA (encrypt only), and accept default values and execute the following commands:

.. parsed-literal::
   gpg --gen-random 2 32 | gpg --encrypt --default-recipient-self --sign --armor > ydb_workshop_key.txt
   gpg --decrypt < ./ydb_workshop_key.txt | gpg --encrypt --armor --default-recipient-self --output ydb.key

Refer to the 'man gpg; a description on the qualifiers for gpg.

Create a gtmcrypt_config file as following:

.. parsed-literal::
   $ cat config
     database: {
        keys:   (
                  {
                   dat: "/path/to/mumps.dat" ;
                   key: "/path/to/ydb.key" ;
                  }
               );
  }

Set the environment variable gtmcrypt_config to point to this config file.

.. parsed-literal::
   export gtmcrypt_config=$PWD/config 

Set the environment variable ydb_passwd. 

.. parsed-literal::
   echo -n "Enter passphrase for ydb.key: " ; export ydb_passwd=`$ydb_dist/plugin/gtmcrypt/maskpass|cut -f 3 -d " "`

Execute the following commands:

.. parsed-literal::
   $ mupip set -encryptable -region DEFAULT
   $ mupip reorg -encrypt="ydb.key" -region DEFAULT
   mupip reorg -encrypt="ydb.key" -region DEFAULT
   Region DEFAULT : MUPIP REORG ENCRYPT started
   Region DEFAULT : Database is now FULLY ENCRYPTED with the following key: ydb.key
   Region DEFAULT : MUPIP REORG ENCRYPT finished

Execute the following command when encryption completes. 

.. parsed-literal::
   $ mupip set -encryptioncomplete -region DEFAULT
   Database file /home/gtc_twinata/staff/nitin/tr11/mumps.dat now has encryption marked complete

Always keep the keys in a secured location. Always set gtmcrypt_config and ydb_passwd to access the encrypted database.

+++++++++++++++++++
USER_DEFINED_REORG
+++++++++++++++++++

The major REORG operations are COALESCE, SPLIT and SWAP, in terms of how database files are defragmented and reorganized.

As defined above:

* Blocks coalesced - the number of blocks that were sufficiently compacted enough to free a block
* Blocks split - the number of blocks expanded enough to require the allocation of a new block
* Blocks swapped - the number of blocks moved to improve adjacency; this can exceed the number of blocks processed as a consequence of the movement of blocks

USER_DEFINED_REORG gives the user more control over which operations are performed during the REORG process.

As REORG is IO intensive, an organization may opt to skip swapping or splitting files, especially as these operations may only yield a minimal benefit while also impacting the operation of normal processes.

Examples of USER_DEFINED_REORG are as follows:

.. parsed-literal::
   mupip reorg -user_defined_reorg="DETAIL"

This example records every block and every operation (coalesce/swap/split) and gives detailed information about the REORG.

.. parsed-literal::
   mupip reorg -user_defined_reorg="NOCOALESCE"

This example carries out the REORG but does not coalesce (combine) any blocks.

.. parsed-literal::
   mupip reorg -user_defined_reorg="NOSPLIT"

This example carries out the REORG but does not split any blocks.

.. parsed-literal::
   mupip reorg -user_defined_reorg="NOSWAP"

This example carries out the REORG but does not swap any blocks.

.. parsed-literal::
   mupip reorg -user_defined_reorg="NOSWAP,NOCOALESCE"

This example combines two specifications, and carries out the REORG without swapping or coalescing any blocks.

++++++++++++
REPLICATE
++++++++++++

Control the logical multi-site operation of YottaDB. For more information on the qualifiers of the MUPIP REPLICATE command, refer to `Chapter 7: “Database Replication” <https://docs.yottadb.com/AdminOpsGuide/dbrepl.html>`_ .

+++++++++
RESTORE
+++++++++

Integrates one or more BACKUP -INCREMENTAL files into a corresponding database. The transaction number in the first incremental backup must be one more than the current transaction number of the database. Otherwise, MUPIP RESTORE terminates with an error.

The format of the RESTORE command is:

.. parsed-literal::
   RE[STORE] [-[NO]E[XTEND]] file-name file-list


* file-name identifies the name of the database file that RESTORE uses as a starting point.

* file-list specifies one or more files produced by BACKUP -INCREMENTAL to RESTORE into the database. The file-names are separated by commas (,) and must be in sequential order, from the oldest transaction number to the most recent transaction number. RESTORE may take its input from a UNIX file on any device that supports such files.

* The current transaction number in the database must match the starting transaction number of each successive input to the RESTORE.

* If the BACKUP -INCREMENTAL was created using -TRANSACTION=1, create a new database with MUPIP CREATE and do not access it, except the standalone MUPIP commands INTEG -FILE, EXTEND, and SET before initiating the RESTORE.

~~~~~~~~~  
-EXTEND
~~~~~~~~~

Specifies whether a MUPIP RESTORE operation should extend the database file automatically if it is smaller than the size required to load the data.

The format of the EXTEND qualifier is:

.. parsed-literal::
   -[NO]E[XTEND]

M activity between backups may automatically extend a database file. Therefore, the database file specified as the starting point for a RESTORE may require an extension before the RESTORE. If the database needs an extension and the command specifies -NOEXTEND, MUPIP displays a message and terminates. The message provides the sizes of the input and output database files and the number of blocks by which to extend the database. If the RESTORE specifies more than one incremental backup with a file list, the database file may require more than one extension.

By default, RESTORE automatically extends the database file.

**Examples for MUPIP RESTORE**

.. parsed-literal::
   $ mupip restore backup.dat $backup_dir/backup.bk1, $backup_dir/backup.bk2, $backup_dir/backup.bk3

This command restores backup.dat from incremental backups stored in directory specified by the environment variable backup_dir.

.. parsed-literal::
   $ mupip restore ydb.dat '"gzip -d -c online5pipe.inc.gz \|"'

This command uses a pipe to restore ydb.dat since its last DATABASE backup from the bytestream backup stored in online5pipe.inc.gz.

++++++++++++++++
RUNDOWN
++++++++++++++++

When database access has not been properly terminated, RUNDOWN properly closes currently inactive databases, removes abandoned YottaDB database semaphores, and releases any IPC resources used. Under normal operations, the last process to close a database file performs the RUNDOWN actions, and a MUPIP RUNDOWN is not required. If a database file is already properly rundown, a MUPIP RUNDOWN has no effect. If in doubt, it is always to safe to perform a rundown. YottaDB recommends the following method to shutdown a YottaDB application or the system:

* Terminate all YottaDB processes, and
* Rundown any and all database files that may be active.

MUPIP RUNDOWN checks for version mismatch. If there is a mismatch, it skips the region and continues with the next region. This makes it easier for multiple (non-interacting) YottaDB versions to co-exist on the same machine. Note that YottaDB does not support concurrent access to the same database file by multiple versions of the software.

The format of the MUPIP RUNDOWN command is:

.. parsed-literal::
   RU[NDOWN] {-FILE file-name|-REGION region-list|-RELINKCTL [dir]|-OVERRIDE}

MUPIP RUNDOWN clears certain fields in a file that is already closed. This facilitates recovery from a system crash or any other operational anomaly.

Use RUNDOWN after a system crash or after the last process accessing a database terminates abnormally. RUNDOWN ensures that open databases are properly closed and ready for subsequent use. RUNDOWN has no effect on any database that is actively being accessed at the time the RUNDOWN is issued.

A successful MUPIP RUNDOWN of a database region removes any current MUPIP FREEZE.

RUNDOWN -FILE can be directed to a statistics database file and works even if the corresponding actual database file does not exist.

To ensure database integrity, all system shutdown algorithms should include scripts that stop at YottaDB processes and perform RUNDOWN on all database files.

The RUNDOWN command may include one of the following qualifiers:

.. parsed-literal::
   -F[ile] 
   -R[egion]=region-list
   -RELinkctl [dir1] 
   -Override

If the RUNDOWN command does not specify either -File or -Region, it checks all the IPC resources (shared memory) on the system and if they are associated with a YottaDB database, attempts to rundown that file. MUPIP RUNDOWN with no argument removes any statistics database file resources associated with actual database file resources it can remove.

~~~~~~
-FILE
~~~~~~

Specifies that the argument is a file-name for a single database file. The -FILE qualifier is incompatible with the REGION qualifier. If the rundown parameter consists of a list of files, the command only operates on the first item on the list.

Incompatible with: -REGION

~~~~~~~~~
-OVERRIDE
~~~~~~~~~

Overrides the protection that prevents MUPIP RUNDOWN from performing a rundown of a replication-enabled (with BEFORE_IMAGE) database or a non-replicated NOBEFORE-journaled database that was abnormally shutdown. The protection involves issuing the MUUSERLBK error for a previously crashed replication-enabled (with BEFORE IMAGE journaling) database and the MUUSERECOV error for a non-replicated or NOBEFORE-journaled database. Both these errors prevent complications related to data recovery from a journal file or a replication-enabled database.

~~~~~~~~
-REGION
~~~~~~~~

The region-list identifies the target of the RUNDOWN. region-list may specify more than one region of the current global directory in a list. Regions are case-insensitive, separated by a comma, and wildcards can be used to specify them. Any region-name may include the wildcard characters * and % (remember to escape them to protect them from inappropriate expansion by the shell). Any region-name expansion occurs in M (ASCII) collation order.

Use the wildcard "*" to rundown all inactive regions in a global directory.

Incompatible with: -FILE

When MUPIP RUNDOWN has no qualifier, it performs rundown on all inactive database memory sections on the node. Because this form has no explicit list of databases, it does not perform any clean up on regions that have no abandoned memory segments but may not have been shutdown in a crash.

~~~~~~~~~~~
-RELINKCTL
~~~~~~~~~~~

Cleans up orphaned Relinkctl files. YottaDB strongly recommends avoiding actions that tend to make such cleanup necessary - for example, kill -9 of YottaDB processes or ipcrm -m of active Relinkctl and/or Rtnobj shared memory segments.

If the optional dir1 is not specified, MUPIP RUNDOWN -RELINKCTL examines the environment variable $ydb_routines, attempts to verify and correct their attach counts and runs down all its inactive auto-relink-enabled directories (those with with a \*-suffix). Alternatively, one can specify a directory path for the parameter dir1 and MUPIP RUNDOWN -RELINKCTL treats it as an auto-relink-enabled directory and runs down the resources associated with this directory. It prints a RLNKCTLRNDWNSUC message on a successful rundown and a RLNKCTLRNDWNFL message on a failure (usually because live processes are still accessing the Relinkctl file).

+++++++
SET
+++++++

Modifies certain database characteristics. MUPIP SET operates on either regions or files.

The format of the SET command is:

.. parsed-literal::
   SE[T] {-FI[LE] file-name|-JN[LFILE] journal-file-name|-REG[ION] region-list|-REP[LICATION]={ON|OFF}}  
    -AC[CESS_METHOD]={BG|MM}
    -[NO]AS[YNCIO]
    -[NO]DE[FER_TIME][=seconds]
    -[NO]DEFER_ALLOCATE
    -EPOCHTAPER 
    -E[XTENSION_COUNT]=integer(no of blocks)
    -F[LUSH_TIME]=integer
    -G[LOBAL_BUFFERS]=integer
    -H[ARD_SPIN_COUNT]=integer
    -[NO]INST[_FREEZE_ON_ERROR]
    -JN[LFILE]journal-file-name
    -K[EY_SIZE]=bytes
    -L[OCK_SPACE]=integer
    -M[UTEX_SLOTS]=integer
    -N[ULL_SUBSCRIPTS]=value
    -[NO]LCK_SHARES_DB_CRIT
    -PA[RTIAL_RECOV_BYPASS]
    -[NO]Q[DBRUNDOWN]
    -[NO]REA[D_ONLY]
    -REC[ORD_SIZE]=bytes
    -REG[ION] region-list
    -REORG_SLEEP_NSEC=integer
    -REP[LICATION]={ON|OFF}
    -RES[ERVED_BYTES]=integer]
    -SL[EEP_SPIN_COUNT]=integer
    -SPIN_SLEEP_M[ASK]=hex_mask
    -STAN[DALONENOT]
    -[NO]STAT[S]
    -NO]STD[NULLCOLL]
    -V[ERSION]={V4|V6}
    -W[AIT_DISK]=integer 


* Exclusive access to the database is required if the MUPIP SET command specifies -ACCESS_METHOD, -GLOBAL_BUFFERS, -LOCK_SPACE or -NOJOURNAL, or if any of the -JOURNAL options ENABLE, DISABLE, or BUFFER_SIZE are specified.

* The file-name, journal_file_name, region-list or -REPLICATION qualifier identify the target of the SET.

* The SET command must include one of the following target qualifiers which determine whether the argument to the SET is a file-name or a region-list.

~~~~~~  
-FILE
~~~~~~

Specifies that the argument is a file-name for a single database file. The format of the FILE qualifier is:

.. parsed-literal::
   -F[ILE]

Incompatible with: -JNLFILE, -REGION and -REPLICATION

~~~~~~~~~
-JNLFILE
~~~~~~~~~

Specifies that the argument is a journal-file-name. The format of the JNLFILE qualifier is:

.. parsed-literal::
   -JNLF[ILE] journal-file-name

Incompatible with: -FILE, -REGION and -REPLICATION

~~~~~~~~
-REGION
~~~~~~~~

Specifies that the argument is a region-list which identifies database file(s) mapped by the current Global Directory. The format of the REGION qualifier is:

.. parsed-literal::
   -R[EGION] region-list

The region-list identifies the target of SET. region-list may specify more than one region of the current global directory in a list. Regions are case-insensitive, separated by a comma, and wild-cards can be used to specify them. Any region-name may include the wild-card characters * and % (remember to escape them to protect them from inappropriate expansion by the shell). Any region name expansion occurs in M (ASCII) collation order.

Incompatible with: -FILE, -JNLFILE and -REPLICATION

~~~~~~~~~~~~~
-REPLICATION
~~~~~~~~~~~~~

Specifies whether replication is on or off. The format of the REPLICATION qualifier is:

.. parsed-literal::
   -REP[LICATION]={ON|OFF}

Incompatible with: -FILE, -JNLFILE and -REGION

The following sections describe the action qualifiers of the MUPIP SET command exclusive of the details related to journaling and replication, which are described in `Chapter 6: “YottaDB Journaling” <https://docs.yottadb.com/AdminOpsGuide/ydbjournal.html>`_ and `Chapter 7: “Database Replication” <https://docs.yottadb.com/AdminOpsGuide/dbrepl.html>`_. All of these qualifiers are incompatible with the -JNLFILE and -REPLICATION qualifiers.

~~~~~~~~~~~~~~~
-ACCESS_METHOD
~~~~~~~~~~~~~~~

Specifies the access method (YottaDB buffering strategy) for storing and retrieving data from the global database file. The format of the ACCESS_METHOD qualifier is:

.. parsed-literal::
   -AC[CESS_METHOD]=code

For more information on specifying the ACCESS_METHOD,refer to `“Segment Qualifiers” <https://docs.yottadb.com/AdminOpsGuide/gde.html#segment-qualifiers>`_.

~~~~~~~~~~
-ASYNCIO
~~~~~~~~~~

Specifies whether to use asynchronous I/O for an access method BG database, rather than using synchronous I/O through the file system cache. ASYNCIO is incompatible with the MM access method and an attempt to combine the two with MUPIP SET produces a ASYNCIONOMM error. The format of the ASYNCIO qualifier is:

.. parsed-literal::
   -[NO]AS[YNCIO]

For more information on specifying ASYNCIO,refer to `“Segment Qualifiers” <https://docs.yottadb.com/AdminOpsGuide/gde.html#segment-qualifiers>`_.

~~~~~~~~~~~~
-DEFER_TIME
~~~~~~~~~~~~

Specifies, in MM access mode, the multiplying factor applied to the flush time to produce a wait after an update before ensuring a journal buffer write to disk; the default is 1. A value of 2 produces a wait of double the flush time. -NODEFER_TIME or a value of -1 turns off timed journal writing, leaving the journal, under light update conditions, to potentially get as stale as the epoch time. Note that, in MM mode without the sync_io option set, absent a VIEW("JNLFLUSH") from the application, YottaDB only fsyncs the journal at the epoch. The format of the DEFER_TIME qualifier is:

.. parsed-literal::
   -[NO]D[efer_time][=seconds]

~~~~~~~~~~~~~~~~
-DEFER_ALLOCATE
~~~~~~~~~~~~~~~~

Provides a mechanism to control YottaDB behavior when subsequently extending existing database files, whether using MUPIP EXTEND or auto-extend. To switch an existing database file so it immediately preallocates all blocks, first use MUPIP SET -NODEFER_ALLOCATE to set the switch in the database file header, followed by MUPIP EXTEND -BLOCKS=n, where n >= 0. Failures to preallocate space produce a PREALLOCATEFAIL error. The format of the DEFER_ALLOCATE qualifier is: 

.. parsed-literal::
   -[NO]DEFER_ALLOCATE

~~~~~~~~~~~~
-EPOCHTAPER
~~~~~~~~~~~~

Tries to minimize epoch duration by reducing the number of buffers to flush by YottaDB and the file system (via an fsync()) as the epoch (time-based or due to journal file auto-switch) approaches. The format of the -EPOCHTAPER qualifier is:

.. parsed-literal::
   -[NO]EPOCHTAPER

~~~~~~~~~~~~~~~~~
-EXTENSION_COUNT
~~~~~~~~~~~~~~~~~

Specifies the number of GDS blocks by which an existing database file extends. A file or region name is required. This qualifier requires standalone access. The format of the EXTENSION_COUNT qualifier is:

.. parsed-literal::
   -E[XTENSION_COUNT]=integer 

For more information on specifying the EXTENSION_COUNT, refer to `“Segment Qualifiers” <https://docs.yottadb.com/AdminOpsGuide/gde.html#segment-qualifiers>`_.

~~~~~~~~~~~~
-FLUSH_TIME
~~~~~~~~~~~~

Specifies the amount of time between deferred writes of stale cache buffers. The default value is 1 second and the maximum value is 1 hour. -FLUSH_TIME requires standalone access. The format of the FLUSH_TIME qualifier is:

.. parsed-literal::
   -F[LUSH_TIME]=[[[HOURS:]MINUTES:]SECONDS:]CENTISECONDS

~~~~~~~~~~~~~~~~
-GLOBAL_BUFFERS
~~~~~~~~~~~~~~~~

Specifies the number of cache buffers for a BG database. This qualifier requires standalone access.The format of the GLOBAL_BUFFERS qualifier is:

.. parsed-literal::
   -G[LOBAL_BUFFERS]=integer

For more information on ways to determine good working sizes for GLOBAL_BUFFERS, refer to `“Segment Qualifiers” <https://docs.yottadb.com/AdminOpsGuide/gde.html#segment-qualifiers>`_.

In general, increasing the number of global buffers improves performance by smoothing the peaks of I/O load on the system. However, increasing the number of global buffers also increases the memory requirements of the system, and a larger number of global buffers on memory constrained systems can increase the probability of the buffers getting swapped out. If global buffers are swapped out, any performance gain from increasing the number of global buffers will be more than offset by the performance impact of swapping global buffers. Most applications use from 1,000 to 4,000 global buffers for database regions that are heavily used. YottaDB does not recommend using fewer than 256 buffers except under special circumstances.

The minimum is 64 buffers and the maximum is 65536 buffers. By default, MUPIP CREATE establishes GLOBAL_BUFFERS using information entered in the Global Directory.

On many UNIX systems, default kernel parameters may be inadequate for YottaDB global buffers, and may need to be adjusted by a system administrator.

~~~~~~~~~~~~~~~~~
-HARD_SPIN_COUNT
~~~~~~~~~~~~~~~~~

The mutex hard spin count specifies the number of attempts to grab the mutex lock before initiating a less CPU-intensive wait period. The format of -HARD_SPIN_COUNT is:

.. parsed-literal::
   -HARD_SPIN_COUNT=integer

The default value is 128. Except on the advice of your YottaDB support channel, YottaDB recommends leaving the default values unchanged in production environments, until and unless, you have data from testing and benchmarking that demonstrates a benefit from a change.

~~~~~~~~~~~~~~~~~~~~~~~
-INST_FREEZE_ON_ERROR
~~~~~~~~~~~~~~~~~~~~~~~

Enables or disables custom errors in a region to automatically cause an Instance Freeze. This flag modifies the "Inst Freeze on Error" file header flag. The format of the INST_FREEZE_ON_ERROR qualifier is:

.. parsed-literal::
   -[NO]INST[_FREEZE_ON_ERROR]

For more information on creating a list of custom errors that automatically cause an Instance Freeze, refer to `Instance Freeze <https://docs.yottadb.com/AdminOpsGuide/dbrepl.html#instance-freeze>`_.

For more information on promptly setting or clearing an Instance Freeze on an instance irrespective of whether any region is enabled for Instance, refer to the `Starting the Source Server <https://docs.yottadb.com/AdminOpsGuide/dbrepl.html#starting-the-source-server>`_ section of the Database Replication chapter.

~~~~~~~~
-JOURNAL
~~~~~~~~

Specifies whether the database allows journaling and, if it does, characteristics for the journal file.

.. note::
   In regions that have journaling enabled and on, users can switch journal files without either requiring standalone access or freezing updates.

The format of the JOURNAL qualifier is:

.. parsed-literal::
   -[NO]J[OURNAL][=journal-option-list]

* -NOJOURNAL specifies that the database does not allow journaling. And also it does not accept an argument assignment.

* -JOURNAL specifies journaling is allowed. It takes one or more arguments in a journal-option-list.

For detailed description of the all JOURNAL qualifiers and its keywords, refer to `SET -JOURNAL Options <https://docs.yottadb.com/AdminOpsGuide/ydbjournal.html#set-action-qualifiers>`_. 

~~~~~~~~~~
-KEY_SIZE
~~~~~~~~~~

Specifies the maximum key size in bytes for storing and retrieving data from the global database file. The maximum supported size is 1019 bytes. The format of the KEY_SIZE qualifier is:

.. parsed-literal::
   -K[EY_SIZE]=bytes

For more information on KEY_SIZE, refer to `“Region Qualifiers” <https://docs.yottadb.com/AdminOpsGuide/gde.html#region-qualifiers>`_.

~~~~~~~~~~~~
-LOCK_SPACE
~~~~~~~~~~~~

Specifies the number of pages allocated to the management of M locks associated with the database. The size of a page is always 512 bytes. The format of the LOCK_SPACE qualifier is:

.. parsed-literal::
   -L[OCK]_SPACE=integer

* The maximum LOCK_SPACE is 262144 pages.

* The minimum LOCK_SPACE is 10 pages.

* The default LOCK_SPACE is 40 pages.

* For more information on LOCK_SPACE, refer to `“Segment Qualifiers” <https://docs.yottadb.com/AdminOpsGuide/gde.html#segment-qualifiers>`_.

* This qualifier requires standalone access.

~~~~~~~~~~~~~  
-MUTEX_SLOTS
~~~~~~~~~~~~~

Sets the size of a structure that YottaDB uses to manage contention for the principal critical section for a database. Performance issues may occur when there are many processes contending for database access and if this structure cannot accommodate all waiting processes. Therefore, YottaDB recommends setting this value to a minimum of slightly more than the maximum number of concurrent processes you expect to access the database.

The minimum value is 64 and the maximum value is 32768. The default value is 1024. The format of the MUTEX_SLOTS qualifier is:

.. parsed-literal::
   -M[UTEX_SLOTS]=integer

~~~~~~~~~~~~~~~~~
-NULL_SUBSCRIPTS
~~~~~~~~~~~~~~~~~

Controls whether YottaDB accepts null subscripts in database keys.

Usage:

.. parsed-literal::
   -N[ULL_SUBSCRIPTS]=value

* value can either be T[RUE], F[ALSE], ALWAYS, NEVER, or EXISTING. See GDE chapter for more information on these values of null_subscript.

* Prohibiting null subscripts can restrict access to existing data and cause YottaDB to report errors.

* The default value is never.

~~~~~~~~~~~~~~~~~~~~  
-LCK_SHARES_DB_CRIT
~~~~~~~~~~~~~~~~~~~~

Specifies whether LOCK actions share the same resource and management as the database or use a separate resource and management. The format of the LCK_SHARES_DB_CRIT qualifier is:

.. parsed-literal::
    -[NO]LC[K_SHARES_DB_CRIT]

The default is Sep(arate)/FALSE.

For more information, refer to `“Region Qualifiers” <https://docs.yottadb.com/AdminOpsGuide/gde.html#region-qualifiers>`_.

~~~~~~~~~~~~
-QDBRUNDOWN
~~~~~~~~~~~~

Shortens normal process shutdown when a large number of processes accessing a database file need to shutdown almost simultaneously, for example, in benchmarking scenarios or emergencies. The format of the QDBRUNDOWN qualifier is:

.. parsed-literal::
   -[NO]Q[DBRUNDOWN]

When a terminating YottaDB process observes that a large number of processes are attached to a database file and QDBRUNDOWN is enabled, it bypasses checking whether it is the last process accessing the database. Such a check occurs in a critical section and bypassing it also bypasses the usual RUNDOWN actions which accelerates process shutdown removing a possible impediment to process startup. By default, QDBRUNDOWN is disabled.

Note that with QDBRUNDOWN there is a possibility that the last process to exit might leave the database shared memory and IPC resources in need of cleanup. Except after the number of concurrent processes exceeds 32Ki, QDBRUNDOWN minimizes the possibility of abandoned resources, but it cannot eliminate it. When using QDBRUNDOWN, use an explicit MUPIP command such as RUNDOWN or JOURNAL -RECOVER or -ROLLBACK of the database file after the last process exits, to ensure the cleanup of database shared memory and IPC resources; not doing so risks database damage.

When a database has QDBRUNDOWN enabled, if the number of attached processes ever exceeds 32Ki, YottaDB stops tracking the number of attached processes, which means that it cannot recognize when the number reaches zero (0) and the shared resources can be released. The process that detects this event issues a NOMORESEMCNT in the system log. This means that an orderly, safe shutdown requires a MUPIP JOURNAL -ROLLBACK -BACKWARD for replicated databases, a MUPIP JOURNAL -RECOVER -BACKWARD for unreplicated journaled databases and a MUPIP RUNDOWN for journal-free databases. 

~~~~~~~~~~~~~~~~~~~~~~
-PARTIAL_RECOV_BYPASS
~~~~~~~~~~~~~~~~~~~~~~

Sets the CORRUPT_FILE flag in the database file header to FALSE. The CORRUPT_FILE flag indicates whether a region completed a successful recovery. The format of the PARTIAL_RECOV_BYPASS qualifier is:

.. parsed-literal::
   -PA[RTIAL_RECOV_BYPASS]

For more information, refer to the CORRUPT_FILE qualifier in `“CHANGE -FILEHEADER Qualifiers” <https://docs.yottadb.com/AdminOpsGuide/dse.html#change>`_. 

~~~~~~~~~~~
-READ_ONLY
~~~~~~~~~~~

Indicates whether YottaDB should treat an MM access method segment as read only for all users, including root. This designation augments UNIX authorizations and prevents any state updates that normally might require an operational action for a database with no current accessing (attached) processes. MUPIP emits an error on attempts to set -READ_ONLY on databases with the BG access method, or to set the access method to BG on databases with -READ_ONLY set. The YottaDB help databases have -READ_ONLY set by default. The format of the READ_ONLY qualifier is:

.. parsed-literal::
   -[NO]REA[D_ONLY]

.. note::
   When the first process connects to a database, it creates a access-control semaphore as part of the management of the shared resource. However, when processes connect to a -READ_ONLY database , each creates a private copy of the in-memory structures for the database and thus a private semaphore. 

~~~~~~~~~~~~~
-RECORD_SIZE
~~~~~~~~~~~~~

Specifies the maximum record size in bytes for storing and retrieving data from the global database file. The maximum supported size is 1MiB bytes. The format of the RECORD_SIZE qualifier is:

.. parsed-literal::
   -REC[ORD_SIZE]=bytes

For more information on KEY_SIZE, refer to `“Region Qualifiers” <https://docs.yottadb.com/AdminOpsGuide/gde.html#region-qualifiers>`_.

~~~~~~~~~~~~~~~~~~
-REORG_SLEEP_NSEC
~~~~~~~~~~~~~~~~~~

Specifies the number of nanoseconds that a MUPIP REORG process operating between blocks takes to process, with default value of 0 and a maximum of 999999999 (i.e. 999,999,999, or 1 nanosecond less than 1 second). Using non-zero values reduces the IO impact of MUPIP REORG, at the cost of increasing the duration of the operation. Note that the existing environment variable ydb_poollimit is the appropriate technique to limit the impact of MUPIP REORG on global buffers; the -reorg_sleep_nsec can be used to limit the impact on the IO subsystem.

~~~~~~~~~~~~~~~~
-RESERVED_BYTES
~~~~~~~~~~~~~~~~

Specifies the size to be reserved in each database block. RESERVED_BYTES is generally used to reserve room for compatibility with other implementations of M or to observe communications protocol restrictions. The format of the RESERVED_BYTES qualifier is:

.. parsed-literal::
   -RES[ERVED_BYTES]=size


* RESERVED_BYTES may also be used as a user-managed fill factor.

* The minimum RESERVED_BYTES is 0 bytes. The maximum RESERVED_BYTES is the block size minus the size of the block header which is 7 or 8 depending on your platform. Realistic determinations of this amount should leave room for at least one record of maximum size.

~~~~~~~~~~~~~~~~~~  
-SLEEP_SPIN_COUNT
~~~~~~~~~~~~~~~~~~

Specifies the number of times a process suspends its activity while waiting to obtain critical sections for shared resources, principally those involving databases. The format of the -SLEEP_SPIN_COUNT qualifier is:

.. parsed-literal::
   -SLEEP_SPIN_COUNT=integer

* integer is the number of times the process yields to the OS scheduler or sleeps (depending in the SPIN_SLEEP_LIMIT) after exhausting its hard spin count and before enqueuing itself to be awakened by another process releasing the shared resource mutex.

* The default is 128.

* Except on the advice of your YottaDB support channel, YottaDB recommends leaving the default values unchanged in production environments, until and unless you have data from testing and benchmarking that demonstrates benefits from a change.

~~~~~~~~~~~~~~~~~  
-SPIN_SLEEP_MASK
~~~~~~~~~~~~~~~~~

Specifies the maximum number of nanoseconds for processes to sleep while waiting to obtain critical sections for shared resources, principally those involving databases. The format of the -SPIN_SLEEP_MASK qualifier is:

.. parsed-literal::
   -SPIN_SLEEP_MASK=hex_mask

* hex_mask is a hexadecimal mask that controls the maximum time (in nanoseconds) the process sleeps on a sleep spin.

* The default is zero (0) which causes the process to return control to the UNIX kernel to be rescheduled with no explicit delay. When the value is non-zero, the process waits for a random value between zero (0) and the maximum value permitted by the mask.

* Except on the advice of your YottaDB support channel, YottaDB recommends leaving the default values unchanged in production environments, until and unless you have data from testing and benchmarking that demonstrates a benefit from a change.

~~~~~~~  
-STATS
~~~~~~~

Specifies whether YottaDB should permit statistics sharing for this region. This characteristic permits operational exclusion of statistics sharing for a region. The format of the STATS qualifier is:

.. parsed-literal::
   -[NO]STAT[S]

At database creation, GDE controls this characteristic, which by default is specified as STATS (on). When on, this characteristic causes YottaDB to create a small MM database for the associated region to hold the shared statistics.

~~~~~~~~~~~~~~
-STDNULLCOLL
~~~~~~~~~~~~~~

Specifies whether YottaDB uses standard MUMPS collation or YottaDB collation for null-subscripted keys. YottaDB strongly recommends that you use STDNULLCOLL and not the non-standard null collation, which is the default for historical reasons. The format of the STDNULLCOLL qualifier is:

.. parsed-literal::
   -[NO]STD[NULLCOLL]

~~~~~~~~~~
-VERSION
~~~~~~~~~~

Sets the block format version (Desired DB Format field in the file header) for all subsequent new blocks. The format of the VERSION qualifier is:

.. parsed-literal::
   -V[ERSION]={version}

* MUPIP UPGRADE and MUPIP REORG -UPGRADE set the Desired DB Format field in the database file header to the latest version while MUPIP REORG -DOWNGRADE sets it to the previous version.

For more information on the upgrading or downgrading your database, refer to the release notes document of your current YottaDB version(s).

~~~~~~~~~~~
-WAIT_DISK
~~~~~~~~~~~

Specifies the seconds to wait for disk space before giving up on a database block write, where zero (0) means to give an error immediately without waiting. The format of the WAIT_DISK qualifier is:

.. parsed-literal::
   -W[AIT_DISK]=seconds

**Examples for MUPIP SET**

Example:

.. parsed-literal::
   $ mupip set -journal=on,nobefore -region "*"

This example enables NOBEFORE image journaling and turns on journaling for all regions.

.. parsed-literal::
   $ mupip set -version=r120 -file mumps.dat
   Database file mumps.dat now has desired DB format r120

This example sets the block format to r1.20 for all subsequent new blocks in r1.10 database file mumps.dat.

Example:

.. parsed-literal::
   $ mupip set -version=r110 -file mumps.dat
   Database file mumps.dat now has desired DB format r110

This example sets the block format to r1.10 for all subsequent new blocks in r1.00 database file mumps.dat.

Example:

.. parsed-literal::
   mupip set -flush_time=01:00:00:00 -region DEFAULT

This example sets flush time to 1 hour. You can also specify flush time in any combination of [[[HOURS:]MINUTES:]SECONDS:]CENTISECONDS. MUPIP interprets -FLUSH_TIME=360000 or -FLUSH_TIME=00:60:00:00 as -FLUSH_TIME=01:00:00:00.

Example:

.. parsed-literal::
   $ mupip set -region MAMMALS -inst_freeze_on_error

This example enables custom errors in region MAMMALS to cause an Instance Freeze.

++++++++++++
SIZE
++++++++++++

Estimates and reports the size of global variables using a format that is similar to the one that appears at the end of the MUPIP INTEG -FULL report. In comparison with MUPIP INTEG -FAST -FULL, MUPIP SIZE provides the option of choosing any one of the three estimation techniques to estimate the size of global variables in a database file. These techniques vary in measurement speed and estimate accuracy. The format of the MUPIP SIZE command is:

.. parsed-literal::
   MUPIP SI[ZE] [-h[euristic]=estimation_technique] [-s[elect]=global-name-list] [-r[egion]=region-list] [-a[djacency]=integer]

The optional qualifiers of MUPIP SIZE are:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-HEURISTIC=estimation_technique
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Specifies the estimation technique that MUPIP SIZE should use to estimate the size of global variables. The format of the -HEURISTIC qualifier is:

.. parsed-literal::
   -h[euristic]={sc[an][,level=<lvl>] | a[rsample][,samples=<smpls>] | i[mpsample][,samples=<smpls>]}

* smpls is the number of samples and must be greater than zero (0)
* lvl is a positive or negative tree level designation and -(level of the root block) <= lvl <= (level of the root block)

estimation-technique is one of the following: 

* scan,level=<lvl> : Traverses the global variable tree and counts the actual number of records and blocks at levels from the root down to the level specified by lvl (default is 0, the data blocks). If the given level is non-negative, it is the lowest block level of the global for which the count is requested. So, 0 means all blocks, 1 means all index blocks, 2 means all index blocks of level 2 and above, and so on. SCAN counts a negative level from the root of the global tree where -1 means children of the root. The technique reports the results for levels other than 0 and shows the adjacency for the next lower (one less) level.

* arsample,samples=<smpls> : Uses acceptance/rejection sampling of random tree traversals to estimate the number of blocks at each level. It continues until the specified number of samples (default is 1,000) is accepted.

* impsample,samples=<smpls> : Uses importance sampling of random tree traversals to weight each sample of the specified number of samples (default is 1,000) in order to estimate the size of the tree at each level.

* If -HEURISTIC is not specified, MUPIP SIZE uses the ARSAMPLE,SAMPLE=1000 estimation technique.

The 2 sigma column for the two sampling techniques shows the dispersion of the samples (in blocks) and the probability (rounded to a whole percentage) that the actual value falls farther away from the reported value by more than two sigma. With the scan method the "sample" is "complete," so any inaccuracy comes from concurrent updates.

.. note::
   For large databases, MUPIP SIZE is faster than MUPIP INTEG -FAST -FULL. IMPSAMPLE is expected to be the fastest estimation technique, followed by ARSAMPLE and then SCAN. In terms of accuracy, MUPIP INTEG -FAST -FULL is the most accurate.

~~~~~~~~~~~~~~~~~~~
-ADJACENCY=integer
~~~~~~~~~~~~~~~~~~~

Specifies the logical adjacency of data blocks that MUPIP SIZE should assume during estimation. By default, MUPIP SIZE assumes -ADJACENCY=10 and reports the logical adjacency in the "Adjacent" column of the MUPIP SIZE report. Note that adjacency is only a proxy for database organization and its usefulness may be limited by the technology and configuration of your secondary storage. See the `INTEG <https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#integ>`_ section of this chapter for additional comments on adjacency.

~~~~~~~~
-SELECT
~~~~~~~~

Specifies the global variables on which MUPIP SIZE runs. If -SELECT is not specified, MUPIP SIZE selects all global variables.

The format of the SELECT qualifier is: 

.. parsed-literal::
   -s[elect]=global-name-list

global-name-list can be:

* A comma separated list of global variables.

* A range of global variables denoted by start:end syntax. For example, -select="g1:g4".

* A global variable with wildcards, for example, "g*" (the name must be escaped to avoid shell filename expansion)

* "\*" to select all global variables.

~~~~~~~~  
-REGION
~~~~~~~~

Specifies the region on which MUPIP SIZE runs. If REGION is not specified, MUPIP SIZE selects all regions. The format of the REGION qualifier is: 

.. parsed-literal::
   -R[EGION]=region-list

**Examples for MUPIP SIZE**

.. parsed-literal::
   $ mupip size -heuristic="impsample,samples=2000" -select="y*" -region="AREG"

This example estimates the size of all global variable starting with "y". It uses importance sampling with 2000 samples on the region AREG.

.. parsed-literal::
   $ mupip size -heuristic="scan,level=-1"

This example counts the number of blocks and records at 1 level below the root of the database tree.

.. parsed-literal::
   $ mupip size -heuristic="arsample" -select="g1:g3"

This example estimates the size of global variables g1, g2 and g3 using accept/reject sampling with the default number of samples regardless of the region in which they reside.

.. note::
   Apart from randomness caused by sampling heuristics, MUPIP SIZE also has randomness from concurrent updates because it does not use the snapshot technique that MUPIP INTEG uses.

++++++++++
STOP
++++++++++

Terminates a YottaDB image. The image executes an orderly disengagement from all databases that are currently open by the process, and then exits. A MUPIP STOP performs a kill -15 and therefore may also be used to stop non-YottaDB images.

The format of the STOP command is:

.. parsed-literal::
   MUPIP ST[OP] process-id

* Use the shell command ps to display a list of active process names and process identifiers (PIDs).

* To STOP a process belonging to its own account, a process requires no privileges. To STOP a process belonging to another account, MUPIP STOP must execute as root.

.. note::
   On receipt of a MUPIP STOP signal, a YottaDB process cleans up its participation in managing the database before shutting down. On receipt of three MUPIP STOP signals in a row, a YottaDB process shuts down forthwith without cleaning up - the equivalent of a kill -9 signal. This can result in structural database damage, because YottaDB does not have sufficient control of what happens in response to an immediate process termination to protect against database damage under all circumstances. In all cases, on receipt of a MUPIP STOP, a process will eventually terminate once it gets the resources needed to clean up. Use three MUPIP STOPs in a row only as a last resort, and when you do, perform a MUPIP INTEG at your earliest opportunity thereafter to check for database structural damage, and repair any damage following the procedures in `Chapter 11 (Maintaining Database Integrity) <https://docs.yottadb.com/AdminOpsGuide/integrity.html>`_.You may never have to perform a MUPIP STOP if your application is designed in a way that it reduces or eliminates the probability of a process getting in the final try of a transaction. For more information, refer to the `Programmers Guide <https://docs.yottadb.com/ProgrammersGuide/index.html>`_.

++++++++++++++++
TRIGGER
++++++++++++++++

Examines or loads trigger definitions. The format of the MUPIP TRIGGER command is:

.. parsed-literal::
   TRIGGER {-TRIG[GERFILE]=<trigger_definitions_file> 
   [-NOPR[OMPT]]|[-SELE[CT][=name-list|*][<select-output-file>]|-UPGRADE}

Before you run the MUPIP TRIGGER command:

* Set the value of the environment variable ydb_gbldir: to specify the value of a current global directory.

* Ensure that the key size, record size, block size of your database is sufficient for storing all planned trigger definitions. You may have to set the key and record sizes larger than the database content would otherwise require.

The qualifiers of the MUPIP TRIGGER command are as follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-TRIGGERFILE=<trigger_definitions_file>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Loads a trigger definition file to the database. The format of the TRIGGERFILE qualifier is:

.. parsed-literal::
   -TRIG[GERFILE]=<trigger_definitions_file> [-NOPR[OMPT]]

* For information on the syntax and usage of a trigger definition file, refer to the `Triggers <https://docs.yottadb.com/ProgrammersGuide/triggers.html>`_ chapter and the `$ZTRIGGER() section in the Functions chapter of the Programmer's Guide <https://docs.yottadb.com/ProgrammersGuide/functions.html#ztrigger>`_.

* A MUPIP TRIGGER -TRIGGERFILE operation occurs within a transaction boundary, therefore, if even one trigger from the trigger definition file fails to parse correctly, MUPIP TRIGGER rolls back the entire trigger definition file load. Trigger maintenance operations reserve their output until the transaction commits at which time they deliver the entire output in a consistent way. MUPIP TRIGGER operations have an implicit timeout of zero (0), meaning the read must succeed on the first try or the command will act as if it received no input.

* MUPIP TRIGGER -TRIGGERFILE ignores blank lines and extra whitespace within lines. It treats lines with a semi-colon in the first position as comments and ignores their content.

* MUPIP TRIGGER compiles the XECUTE action string and rejects the load if the compilation has errors.

* Always specify the same value for the environment variable ydb_chset during loading and executing triggers. If you specify different values of ydb_chset during loading and executing triggers, MUPIP TRIGGER generates a run-time error (TRIGINVCHSET). YottaDB does not prevent a process from updating different nodes with triggers using a different character set, however, YottaDB prevents a process from updating the same triggering node with different character sets. Your coding practice, for all database updates, should be to ensure that you provide the same value for ydb_chset during load compilation and run-time compilation.

* MUPIP TRIGGER replicate trigger definitions as logical actions from an originating/primary instance to a replicating/secondary instance based on LGTRIG journal records. This permits the instances to have different sets of triggers and differing database layouts (for example, different # of regions, different block sizes, different maximum-record-size, and so on).

* MUPIP TRIGGER error messages associated with loading triggers limit trigger expression source lines to 80 characters including a trailing ellipsis to indicate there was more text, and they also replace any non-graphic characters with a dot (.)

* YottaDB triggers apply to spanning regions. When $ZTRIGGER() or MUPIP TRIGGER define triggers that apply to globals spanning multiple regions, each of the spanned regions install a definition.

* Incompatible with: -SELECT

.. note::
   The trigger update summary reports count not only names and option changes as "modified" but also cases where a -COMMANDS list changed, even though those are functionally additions or deletions of separate trigger definitions.

~~~~~~~~~~~~~~~~~~
-SELECT=name-list
~~~~~~~~~~~~~~~~~~

Provides a facility to examine the current trigger definition. SELECT produces a list of the current triggers for a comma-separate list of global variables or trigger names. The format of the SELECT qualifier is:

.. parsed-literal::
   -SELE[CT][=name-list*][ <select-output-file>]

* Name-list can include global names, delimited with a leading caret (^), and/or trigger names (user-defined or auto-generated) with no leading caret. You can specify a trailing asterisk(*) with either.

* With no arguments specified, YottaDB treats -SELECT as -SELECT="*" and extracts a list of all current triggers.

* Optionally, you can specify a file name to redirect the output of the command. If you do not specify a file name, MUPIP TRIGGER prompts for a file name. If you respond with an empty string (RETURN), MUPIP TRIGGER directs the output to STDOUT.

* MUPIP TRIGGER -SELECT displays all output including errors on STDOUT.

* For Trigger definition reporting operations, $ZTRIGGER("SELECT") and MUPIP TRIGGER -SELECT, return a non-zero exit status when their selection criteria encounter an error in the select.

* MUPIP TRIGGER -SELECT works even if a multi-line XECUTE string does not terminate with a newline character. For more information on multi-line XECUTE strings, refer to the -xecute="\|<<strlit1"\|>> section under Trigger Definition File in the `Triggers chapter <https://docs.yottadb.com/ProgrammersGuide/triggers.html>`_ and the `$ZTRIGGER() section in the Functions chapter of the Programmer's Guide <https://docs.yottadb.com/ProgrammersGuide/functions.html#ztrigger>`_.

.. note::
   The output from the MUPIP TRIGGER -SELECT command may not be identical to your trigger definition file. This is because YottaDB converts semantically identical syntax into a single internal representation; while -SELECT output may not be identical to the -TRIGGERFILE input, it has the same meaning. Additionally, MUPIP TRIGGER -SELECT displays a field called "Cycle" as part of a comment. Cycle is the number of trigger definition updates (addition, modification, or deletion) performed on a global node. MUPIP TRIGGER treats the deletion of a non-existent trigger as a success; if that is the only operation, or one of a set of successful operations, it returns success 0 to the shell. Also, MUPIP TRIGGER returns failure in case of trigger selection using trigger names where the number after the pound-sign (#) starts with a 0 (which is an impossible auto-generated trigger name).

~~~~~~~~~
-UPGRADE
~~~~~~~~~

Upgrades older trigger definitions into current format.

The format of the UPGRADE qualifier is:

.. parsed-literal::
   -UPGRADE

If YottaDB encounters an old trigger definition it produces a NEEDTRIGUPGRD message. To preserve the possibility of a straightforward downgrade to an earlier version, perform a select "*" action with MUPIP TRIGGER (or $ZTRIGGER() and save the result. Note that TRIGGER -UPGRADE assumes that the existing trigger definitions are properly defined; if the prior release has produced defective triggers, delete them with a wild-card ("*"), and redefine the triggers in the new release. In the event of a downgrade, delete "*" all triggers before the downgrade and insert the saved version from before the upgrade. Attempting to perform a MUPIP TRIGGER -UPGRADE on a database without write authorization to the database produces a TRIGMODREGNOTRW error. The -UPGRADE qualifier is not compatible with any other MUPIP TRIGGER qualifier. Trigger upgrades from older versions may produce journal records based on the prior format that a MUPIP JOURNAL -RECOVER cannot process correctly, therefore, YottaDB recommends you do them with journaling off, and start with a backup and fresh journal files after the trigger upgrade.

**Examples for MUPIP TRIGGER**

This section provides step-by-step instructions for creating, modifying, and deleting triggers. Triggers affect all processes updating a database unlike, for example, environment variables such as $ydb_routines which work on a per process basis. Therefore, YottaDB recommends that you should always have carefully planned procedures for changing triggers in your production environment.

*To create a new trigger for global node ^Acct("ID")*:

Using your editor, create a trigger definition file called triggers.trg with the following entry:

.. parsed-literal::
   +^Acct("ID") -name=ValidateAccount -commands=S -xecute="Write ""Hello Earth!"""

Execute a command like the following:

.. parsed-literal::
   $ mupip trigger -triggerfile=triggers.trg

This command adds a trigger for ^Acct("ID"). On successful trigger load, this command displays an output like the following:

.. parsed-literal::
   File triggers.trg, Line 1: ^Acct trigger added with index 1
   =========================================
   1 triggers added
   0 triggers deleted
   0 trigger file entries not changed
   0 triggers modified
   ========================================= 

Now, every S[et] operation on the global node ^Acct("ID") executes the trigger.

Execute a command like the following:

.. parsed-literal::
   $ mupip trigger -select="^Acct*"

This command displays the triggers. A sample output looks like the following:

.. parsed-literal::
   ;trigger name: ValidateAccount# cycle: 1
   +^Acct("ID") -name=ValidateAccount -commands=S -xecute="Write ""Hello Earth!""" 

*To modify an existing trigger for global node ^Acct("ID")*:

You cannot directly replace an existing trigger definition with a new one. With the exception of -NAME and -OPTIONS, to change an existing trigger, you have to delete the existing trigger definition and then add the modified trigger definition as a new trigger. Note that YottaDB performs two different trigger comparisons to match trigger definitions depending on whether or not S[ET] is the trigger invocation command. If there is a S[ET], then the comparison is based on the global name and subscripts, PIECES, [Z]DELIM, and XECUTE. If there is no SET, YottaDB compares only the global node with subscripts and the -XECUTE code value.

Begin by executing the following command:

.. parsed-literal::
   $ mupip trigger -select="^Acct*"Output file: 

Specify trigger_mod.trg as the output file. This file contains entries like the following:

.. parsed-literal::
   ;trigger name: ValidateAccount# cycle: 1
   +^Acct("ID") -name=ValidateAccount -commands=S -xecute="Write ""Hello Earth!"""

Using your editor, open trigger_mod.trg and change + (plus) to - (minus) for the trigger definition entry for ValidateAccount and add a new trigger definition for ^Acct("ID"). To avoid inconsistent application behavior, it is important to replace an old trigger with a new one in the same transaction (Atomic). The trigger_mod.trg file should have entries like:

.. parsed-literal::
   ;trigger name: ValidateAccount# cycle: 1
   -^Acct("ID") -name=ValidateAccount -commands=Set -xecute="Write ""Hello Earth!"""
   ;trigger name: ValidateAccount#
   +^Acct("ID") -name=ValidateAccount -commands=Set -xecute="Write ""Hello Mars!""" 

Execute a command like the following:

.. parsed-literal::
   $ mupip trigger -triggerfile=trigger_mod.trg

This command displays an output like the following:

.. parsed-literal::
   File trigger_mod.trg, Line 1: ^Acct trigger deleted
   File trigger_mod.trg, Line 3: ^Acct trigger added with index 1
   =========================================
   1 triggers added
   1 triggers deleted
   0 trigger file entries not changed
   0 triggers modified
   =========================================

Congratulations! You have successfully modified the xecute string of ValidateAccount with the new one.

*To delete an existing trigger for global node ^Acct("ID")*:

Begin by executing the following command:

.. parsed-literal::
   $ mupip trigger -select="^Acct*"Output file:

Specify trigger_delete.trg as the output file. This file contains entries like the following:

.. parsed-literal::
   ;trigger name: ValidateAccount# cycle: 3
   +^Acct("ID") -name=ValidateAccount -commands=S -xecute="Write ""Hello Mars!"""

Using your editor, change + (plus) to - (minus) for the trigger definition entry for ValidateAccount. Alternatively, you can create a file with an entry like -ValidateAccount.

Now, execute a command like the following:

.. parsed-literal::
   $ mupip trigger -triggerfile=trigger_delete.trg

This command displays an output like the following:

.. parsed-literal::
    File trigger_delete.trg, Line 2: ^Acct trigger deleted
    =========================================
    0 triggers added
    1 triggers deleted
    0 trigger file entries not changed
    0 triggers modified
    =========================================

You have successfully deleted trigger "ValidateAccount".

*To change a trigger name for global node ^Acct("ID")*:

Using your editor, create a new file called trigger_rename.trg and add a trigger definition entry for ValidateAcct with the same trigger signature as ValidateAccount. Your trigger definition would look something like:

.. parsed-literal::
   +^Acct("ID") -name=ValidateAcct -commands=S -xecute="Write ""Hello Mars!"""

Verify that the ValidateAccount trigger exists by executing the following command:

.. parsed-literal::
   $ mupip trigger -select="^Acct*"Output file:

Respond with an empty string (Press Enter). Confirm that the trigger summary report contains an entry like the following:

.. parsed-literal::
   ;trigger name: ValidateAccount# cycle: 3
   +^Acct("ID") -name=ValidateAccount -commands=S -xecute="Write ""Hello Mars!"""

Now, execute a command like the following:

.. parsed-literal::
   $ mupip trigger -triggerfile=trigger_rename.trg

This command displays an output like the following:

.. parsed-literal::
   =========================================
   0 triggers added
   0 triggers deleted
   0 trigger file entries not changed
   1 triggers modified
   =========================================

You have successfully changed the trigger name ValidateAccount to ValidateAcct.

+++++++++
UPGRADE
+++++++++

Upgrades the file-header of a database. The format of the MUPIP UPGRADE command is:

.. parsed-literal::
   UP[GRADE]

* It increases the size from 4 bytes to 8 bytes of file-header fields such as current transaction number (CTN), maximum TN and others that contain transaction numbers.
* It resets the various trace counters and changes the database format to the most recent version. This change does not upgrade the individual database blocks but sets the database format flag to the most recent version.
* It also initializes a counter of the current blocks that are still in the previous version format. It decrements this counter each time an older version format block is converted to the new format. When the counter is 0, the entire database gets converted.

**Example for MUPIP UPGRADE**

.. parsed-literal::
   $ mupip upgrade mumps.dat

This example upgrades the file-header of mumps.dat to the latest version format.

-----------------------------
MUPIP Command Summary
-----------------------------

+--------------------------------------+---------------------------------------------+------------------------------------------------------------------------------------------+
| Command                              | Objects                                     | Main Qualifier                                                                           |
+======================================+=============================================+==========================================================================================+
| B[ACKUP]                             | region-name                                 | * -BK[UPDBJNL]=DISABLE | OFF                                                             |
|                                      | file-name                                   | * -B[YTESTREAM] -NET[TIMEOUT]=seconds                                                    |
|                                      |                                             | * -C[OMPREHENSIVE]                                                                       |
|                                      |                                             | * -DA[TABASE] -REPLA[CE]                                                                 |
|                                      |                                             | * -DBG                                                                                   |
|                                      |                                             | * -I[NCREMENTAL]                                                                         |
|                                      |                                             | * -[NO]J[OURNAL][=journal-options-list]                                                  |
|                                      |                                             | * -NETTIMEOUT                                                                            |
|                                      |                                             | * -[NO]NEWJNLFILES[=[NO]PREVLINK],[NO]S[YNC_IO]                                          |
|                                      |                                             | * -O[NLINE]                                                                              |
|                                      |                                             | * -RECORD                                                                                |
|                                      |                                             | * -REPLI[NSTANCE]=OFF | ON                                                               |
|                                      |                                             | * -S[INCE]={DATABASE|BYTESTREAM|RECORD}                                                  |
|                                      |                                             | * -T[RANSACTION=hexa;transaction_number]                                                 |
+--------------------------------------+---------------------------------------------+------------------------------------------------------------------------------------------+
| CR[EATE]                             | \-                                          | * -R[EGION]=region-name                                                                  |
+--------------------------------------+---------------------------------------------+------------------------------------------------------------------------------------------+
| DO[WNGRADE]                          | file-name                                   | * -V[ERSION]={V4|V5}                                                                     |
+--------------------------------------+---------------------------------------------+------------------------------------------------------------------------------------------+
| DU[MPFHEAD]                          | file-name or region-list                    | * \-                                                                                     |
+--------------------------------------+---------------------------------------------+------------------------------------------------------------------------------------------+
| EN[DIANCVT]                          | file-name                                   | * -OUTDB=<outdb-file>                                                                    |
|                                      |                                             | * -OV[ERRIDE]                                                                            |
+--------------------------------------+---------------------------------------------+------------------------------------------------------------------------------------------+
| EXI[T]                               | \-                                          | * \-                                                                                     |
+--------------------------------------+---------------------------------------------+------------------------------------------------------------------------------------------+
| EXTE[ND]                             | region-name                                 | * -B[LOCKS]=blocks                                                                       |
+--------------------------------------+---------------------------------------------+------------------------------------------------------------------------------------------+
| EXTR[ACT]                            | \-                                          | * -FO[RMAT]=GO|B[INARY]|Z[WR]                                                            |
|                                      |                                             | * -FR[EEZE]                                                                              |
|                                      |                                             | * -LA[BEL]=text                                                                          |
|                                      |                                             | * -[NO]L[OG]                                                                             |
|                                      |                                             | * -S[ELECT]=global-name-list                                                             |
|                                      |                                             | * -O[CHSET]=character-set                                                                |
|                                      |                                             | * -R[EGION]=region-list                                                                  |
+--------------------------------------+---------------------------------------------+------------------------------------------------------------------------------------------+
| F[REEZE]                             | region-list                                 | * -DBG                                                                                   |
|                                      |                                             | * -OF[F] [-OV[ERRIDE]]                                                                   |
|                                      |                                             | * -ON [-[NO]ONL[INE] [-[NO]A[UTORELEASE]] [-R[ECORD]]]                                   |
+--------------------------------------+---------------------------------------------+------------------------------------------------------------------------------------------+
| FT[OK]                               | File-name                                   | * -D[B]                                                                                  |
|                                      |                                             | * -J[NLPOOL]                                                                             |
|                                      |                                             | * -R[ECVPOOL]                                                                            |
+--------------------------------------+---------------------------------------------+------------------------------------------------------------------------------------------+
| H[ELP]                               | command-option                              | * \-                                                                                     |
+--------------------------------------+---------------------------------------------+------------------------------------------------------------------------------------------+
| I[NTEG]                              | File-name or region-list                    | * -A[DJACENCY]=integer                                                                   |
|                                      |                                             | * -BL[OCK]=hexa;block-number]                                                            |
|                                      |                                             | * -BR[IEF]                                                                               |
|                                      |                                             | * -FA[ST]                                                                                |
|                                      |                                             | * -FI[LE]                                                                                |
|                                      |                                             | * -FU[LL]                                                                                |
|                                      |                                             | * -[NO]K[EYRANGES]                                                                       |
|                                      |                                             | * -[NO][MAP]=integer                                                                     |
|                                      |                                             | * -[NO]MAXK[EYSIZE]=integer                                                              |
|                                      |                                             | * -R[EGION]                                                                              |
|                                      |                                             | * -[NO]O[NLINE]
|                                      |                                             | * -[NO]ST[ATS]                                                                           |
|                                      |                                             | * -SU[BSCRIPT]=subscript                                                                 |
|                                      |                                             | * -TN[_RESET]                                                                            |
|                                      |                                             | * -[NO]TR[ANSACTION][=integer]                                                           |
+--------------------------------------+---------------------------------------------+------------------------------------------------------------------------------------------+
| J[OURNAL]                            | file-name                                   | * -EX[TRACT][=file-specification|-stdout]                                                |
|                                      |                                             | * -REC[OVER] | -RO[LLBACK]                                                               |
|                                      |                                             | * -SH[OW][=show-option-list]                                                             |
|                                      |                                             | * -[NO]V[ERIFY]                                                                          |
|                                      |                                             | * -BA[CKWARD] | -FO[RWARD]                                                               |
+--------------------------------------+---------------------------------------------+------------------------------------------------------------------------------------------+
| L[OAD]                               | file-name                                   | * -BE[GIN]=integer                                                                       |
|                                      |                                             | * -BLOCK_DENSITY                                                                         |
|                                      |                                             | * -E[ND]=integer                                                                         |
|                                      |                                             | * -FI[LLFACTOR]=integer                                                                  |
|                                      |                                             | * -FO[RMAT]=GO|B[INARY]|Z[WR]                                                            |
|                                      |                                             | * -S[TDIN]                                                                               |
|                                      |                                             | * -O[NERROR]                                                                             |
+--------------------------------------+---------------------------------------------+------------------------------------------------------------------------------------------+
| REO[RG]                              | \-                                          | * -ENCR[YPT]=key                                                                         |
|                                      |                                             | * -E[XCLUDE]=global-name-list                                                            |
|                                      |                                             | * -FI[LL_FACTOR]=integer                                                                 |
|                                      |                                             | * -I[NDEX_FILL_FACTOR]=integer                                                           |
|                                      |                                             | * -REG[ION]                                                                              |
|                                      |                                             | * -R[ESUME]                                                                              |
|                                      |                                             | * -S[ELECT]=global-name-list                                                             |
|                                      |                                             | * -T[RUNCATE][=percentage]                                                               |
|                                      |                                             | * -UP[GRADE]                                                                             |
|                                      |                                             | * -REG[ION] region-list                                                                  |
+--------------------------------------+---------------------------------------------+------------------------------------------------------------------------------------------+
| REP[LICATE]                          | file-name                                   | * -E[DITINSTANCE]                                                                        |
|                                      |                                             | * -I[NSTANCE_CREATE]                                                                     |
|                                      |                                             | * -R[ECEIVER]                                                                            |
|                                      |                                             | * -S[OURCE]                                                                              |
|                                      |                                             | * -UPDA[TEPROC]                                                                          |
+--------------------------------------+---------------------------------------------+------------------------------------------------------------------------------------------+
| RE[STORE]                            | file-name or file-list                      | * -[NO]E[XTEND]                                                                          |
+--------------------------------------+---------------------------------------------+------------------------------------------------------------------------------------------+
| RU[NDOWN]                            | file-name or region-name                    | * -F[ILE]                                                                                |
|                                      |                                             | * -R[EGION]                                                                              |
|                                      |                                             | * -RELINKCTL [dir]                                                                       |
|                                      |                                             | * -OVERRIDE                                                                              |
+--------------------------------------+---------------------------------------------+------------------------------------------------------------------------------------------+
| SE[T]                                | file-name or region-name                    | * SE[T] {-FI[LE] file-name|-JN[LFILE] journal-file-name|-REG[ION] region-list|-          |
|                                      |                                             |   REP[LICATION]={ON|OFF}}                                                                |
|                                      |                                             | * -AC[CESS_METHOD]={BG|MM}                                                               |
|                                      |                                             | * -[NO]AS[YNCIO]                                                                         |
|                                      |                                             | * -[NO]DE[FER_TIME][=seconds]                                                            |
|                                      |                                             | * -[NO]DEFER_ALLOCATE                                                                    |
|                                      |                                             | * -E[XTENSION_COUNT]=integer(no of blocks)                                               |
|                                      |                                             | * -F[LUSH_TIME]=integer                                                                  |
|                                      |                                             | * -G[LOBAL_BUFFERS]=integer                                                              |
|                                      |                                             | * -[NO]INST[_FREEZE_ON_ERROR]                                                            |
|                                      |                                             | * -JN[LFILE]journal-file-name                                                            |
|                                      |                                             | * -K[EY_SIZE]=bytes                                                                      |
|                                      |                                             | * -L[OCK_SPACE]=integer                                                                  |
|                                      |                                             | * -M[UTEX_SLOTS]=integer                                                                 |
|                                      |                                             | * -[NO]LCK_SHARES_DB_CRIT                                                                |
|                                      |                                             | * -PA[RTIAL_RECOV_BYPASS]                                                                |
|                                      |                                             | * -[NO]Q[DBRUNDOWN]                                                                      |
|                                      |                                             | * -REC[ORD_SIZE]=bytes                                                                   |
|                                      |                                             | * -REG[ION] region-list                                                                  |
|                                      |                                             | * -REP[LICATION]={ON|OFF}                                                                |
|                                      |                                             | * -RES[ERVED_BYTES]=integer]                                                             |
|                                      |                                             | * -SLEE[P_SPIN_COUNT]=integer                                                            |
|                                      |                                             | * -SPIN[_SLEEP_LIMIT]=nanoseconds                                                        |
|                                      |                                             | * -STAN[DALONENOT]                                                                       |
|                                      |                                             | * -[NO]STAT[S]                                                                           |
|                                      |                                             | * -V[ERSION]={V4|V6}                                                                     |
|                                      |                                             | * -W[AIT_DISK]=integer                                                                   |
+--------------------------------------+---------------------------------------------+------------------------------------------------------------------------------------------+
| TRIGGER                              | \-                                          | * -TRIG[GERFILE]=<trigger_definitions_file>                                              |
|                                      |                                             | * -NOPR[OMPT]                                                                            |
|                                      |                                             | * -SELE[CT][=name-list|*][<select-output-file>]                                          |
|                                      |                                             | * -UPGRADE                                                                               |
+--------------------------------------+---------------------------------------------+------------------------------------------------------------------------------------------+
| ST[OP]                               | process-id                                  | * process-id                                                                             |
+--------------------------------------+---------------------------------------------+------------------------------------------------------------------------------------------+
| UP[GRADE]                            | file-name                                   | * \-                                                                                     |
+--------------------------------------+---------------------------------------------+------------------------------------------------------------------------------------------+

The following table summarizes the qualifiers.

+------------------------------------------------------------------------+----------------------------------------------------------+----------------------------------------------------------------------+
| Main Qualifier                                                         |  MUPIP Command                                           | Options/Qualifiers                                                   |
+========================================================================+==========================================================+======================================================================+
| -EDITINSTANCE                                                          | REPLICATE                                                | * -CHANGE                                                            |
|                                                                        |                                                          | * -DETAIL                                                            |
|                                                                        |                                                          | * -OFFSET=hexa                                                       |
|                                                                        |                                                          | * -VALUE=hexa                                                        |
|                                                                        |                                                          | * -SIZE=hexa                                                         |
|                                                                        |                                                          | * -[NO]QDBRUNDOWN                                                    |
+------------------------------------------------------------------------+----------------------------------------------------------+----------------------------------------------------------------------+
| -FENCES=<fence-options-list>                                           | JOURNAL-RECOVER-ROLLBACK                                 | * ALWAYS                                                             |
|                                                                        |                                                          | * NONE                                                               |
|                                                                        |                                                          | * PROCESS                                                            |
+------------------------------------------------------------------------+----------------------------------------------------------+----------------------------------------------------------------------+
| -OFF                                                                   | FREEZE                                                   | * -OVERRIDE                                                          |
|                                                                        |                                                          | * -RECORD                                                            |
+------------------------------------------------------------------------+----------------------------------------------------------+----------------------------------------------------------------------+
| -ON                                                                    | FREEZE                                                   | * -[NO]ONLINE                                                        |
|                                                                        |                                                          | * -[NO]AUTORELEASE                                                   |
+------------------------------------------------------------------------+----------------------------------------------------------+----------------------------------------------------------------------+
| -INSTANCE_CREATE                                                       | REPLICATE                                                | * -NAME                                                              |
|                                                                        |                                                          | * -NOREPLACE                                                         |
|                                                                        |                                                          | * -SUPPLEMENTARY                                                     |
|                                                                        |                                                          | * -[NO]QDBRUNDOWN                                                    |
+------------------------------------------------------------------------+----------------------------------------------------------+----------------------------------------------------------------------+
| -JOURNAL=<journal-options-list>                                        | BACKUP and SET                                           | * ALIGNSIZE=integer                                                  |
|                                                                        |                                                          | * ALLOCATION=integer                                                 |
|                                                                        |                                                          | * AUTOSWITCHLIMIT=integer                                            |
|                                                                        |                                                          | * BEFORE_IMAGES                                                      |
|                                                                        |                                                          | * BUFFER_SIZE=integer                                                |
|                                                                        |                                                          | * DISABLE                                                            |
|                                                                        |                                                          | * ENABLE                                                             |
|                                                                        |                                                          | * EPOCH_INTERVAL=integer                                             |
|                                                                        |                                                          | * EXTENSION=integer                                                  |
|                                                                        |                                                          | * FILENAME=file_name                                                 |
|                                                                        |                                                          | * OFF                                                                |
|                                                                        |                                                          | * ON                                                                 |
|                                                                        |                                                          | * SYNC_IO                                                            |
|                                                                        |                                                          | * YIELD_LIMIT=integer                                                |
+------------------------------------------------------------------------+----------------------------------------------------------+----------------------------------------------------------------------+
| -LOOKBACK_LIMIT=lookback-option-list                                   | -RECOVER                                                 | * TIME="time"                                                        |
|                                                                        | -ROLLBACK                                                | * OPERATIONS=integer                                                 |
+------------------------------------------------------------------------+----------------------------------------------------------+----------------------------------------------------------------------+
| -RECEIVER                                                              | REPLICATE                                                | * -BUFFSIZE=integer                                                  |
|                                                                        |                                                          | * -CHANGELOG                                                         |
|                                                                        |                                                          | * -CHECKHEALTH                                                       |
|                                                                        |                                                          | * -CMPLVL=integer                                                    |
|                                                                        |                                                          | * -FILTER=filter_name                                                |
|                                                                        |                                                          | * -he[lpers]=[m[,n]]                                                 |
|                                                                        |                                                          | * -INITIALIZE                                                        |
|                                                                        |                                                          | * -CMPLVL=n                                                          |
|                                                                        |                                                          | * -LISTENPORT=integer                                                |
|                                                                        |                                                          | * -LOG=logfile                                                       |
|                                                                        |                                                          | * -LOG_INTERVAL=integer                                              |
|                                                                        |                                                          | * -NORESYNC                                                          |
|                                                                        |                                                          | * -RESUME=strm_num                                                   |
|                                                                        |                                                          | * -REUSE=instname                                                    |
|                                                                        |                                                          | * -SHOWBACKLOG                                                       |
|                                                                        |                                                          | * -SHUTDOWN                                                          |
|                                                                        |                                                          | * -START                                                             |
|                                                                        |                                                          | * -STATSLOG=[ON|OFF]                                                 |
|                                                                        |                                                          | * -STOPSOURCEFILTER                                                  |
|                                                                        |                                                          | * TIMEOUT=seconds                                                    |
|                                                                        |                                                          | * TLSID=label                                                        |
|                                                                        |                                                          | * -UPDATEONLY                                                        |
|                                                                        |                                                          | * -UPDATERESYNC=/path/to/bkup-orig-inst                              |
+------------------------------------------------------------------------+----------------------------------------------------------+----------------------------------------------------------------------+
| -RECOVER                                                               | JOURNAL                                                  | * -AFTER=time                                                        |
|                                                                        |                                                          | * -APPLY_AFTER_IMAGE                                                 |
|                                                                        |                                                          | * -BACKWARD                                                          |
|                                                                        |                                                          | * -BEFORE=time                                                       |
|                                                                        |                                                          | * -BROKENTRANS=file                                                  |
|                                                                        |                                                          | * -CHAIN                                                             |
|                                                                        |                                                          | * -CHECKTN                                                           |
|                                                                        |                                                          | * -[NO]ER[ROR_LIMIT][=integer]                                       |
|                                                                        |                                                          | * -FENCES=fence-option-list                                          |
|                                                                        |                                                          | * -FORWARD                                                           |
|                                                                        |                                                          | * -FULL                                                              |
|                                                                        |                                                          | * -GLOBAL=<global_list>                                              |
|                                                                        |                                                          | * -ID=<pid_list>                                                     |
|                                                                        |                                                          | * -INTERACTIVE                                                       |
|                                                                        |                                                          | * -LOOKBACK_LIMIT=<lookback_limit_options>                           |
|                                                                        |                                                          | * -LOSTTRANS[=file]                                                  |
|                                                                        |                                                          | * -RED[IRECT]=file-pair-list                                         |
|                                                                        |                                                          | * -SINCE=time                                                        |
|                                                                        |                                                          | * -VERBOSE                                                           |
|                                                                        |                                                          | * -VERIFY                                                            |
+------------------------------------------------------------------------+----------------------------------------------------------+----------------------------------------------------------------------+
| -EXTRACT                                                               | JOURNAL                                                  | * -AFTER=time                                                        |
|                                                                        |                                                          | * -BEFORE=time                                                       |
|                                                                        |                                                          | * -BROKENTRANS=file                                                  |
|                                                                        |                                                          | * -CHAIN                                                             |
|                                                                        |                                                          | * -CHECKTN                                                           |
|                                                                        |                                                          | * -[NO]ER[ROR_LIMIT]=integer]                                        |
|                                                                        |                                                          | * -FENCES=fence-option-list                                          |
|                                                                        |                                                          | * -FULL                                                              |
|                                                                        |                                                          | * -GLOBAL=<global_list>                                              |
|                                                                        |                                                          | * -ID=<pid_list>                                                     |
|                                                                        |                                                          | * -INTERACTIVE                                                       |
|                                                                        |                                                          | * -LOOKBACK_LIMIT=<lookback_limit_options>                           |
|                                                                        |                                                          | * -LOSTTRANS[=file]                                                  |
|                                                                        |                                                          | * -REGION                                                            |
|                                                                        |                                                          | * -SINCE=time                                                        |
|                                                                        |                                                          | * -VERBOSE                                                           |
|                                                                        |                                                          | * -VERIFY                                                            |
+------------------------------------------------------------------------+----------------------------------------------------------+----------------------------------------------------------------------+
| -ROLLBACK                                                              | JOURNAL                                                  | * -APPLY_AFTER_IMAGE                                                 |
|                                                                        |                                                          | * -BACKWARD                                                          |
|                                                                        |                                                          | * -BEFORE=time                                                       |
|                                                                        |                                                          | * -BROKENTRANS=file                                                  |
|                                                                        |                                                          | * -[NO]ER[ROR_LIMIT][=integer]                                       |
|                                                                        |                                                          | * -FENCES=fence-option-list                                          |
|                                                                        |                                                          | * -FETCHRESYNC                                                       |
|                                                                        |                                                          | * -LOOKBACK_LIMIT=<lookback_limit_options>                           |
|                                                                        |                                                          | * -LOSTTRANS[=file]                                                  |
|                                                                        |                                                          | * -RES[YNC]=hexa;journal_sequence_number                             |
|                                                                        |                                                          | * -VERBOSE                                                           |
|                                                                        |                                                          | * -VERIFY                                                            |
+------------------------------------------------------------------------+----------------------------------------------------------+----------------------------------------------------------------------+
| -SHOW=<show-option-list>                                               | JOURNAL                                                  | * -ACTIVE_PROCESSES                                                  |
|                                                                        |                                                          | * -ALL                                                               |
|                                                                        |                                                          | * -BROKEN_TRANSACTIONS                                               |
|                                                                        |                                                          | * -HEADER                                                            |
|                                                                        |                                                          | * -PROCESSES                                                         |
|                                                                        |                                                          | * -STATISTICS                                                        |
|                                                                        |                                                          | * -AFTER=time                                                        |
|                                                                        |                                                          | * -USER=user-list                                                    |
|                                                                        |                                                          | * -TRANSACTION=[KILL|SET]                                            |
|                                                                        |                                                          | * -INTERACTIVE                                                       |
|                                                                        |                                                          | * -GLOBAL=<global_list>                                              |
|                                                                        |                                                          | * -ID=<pid_list>                                                     |
|                                                                        |                                                          | * -INTERACTIVE                                                       |
+------------------------------------------------------------------------+----------------------------------------------------------+----------------------------------------------------------------------+
| -SINCE                                                                 | BACKUP                                                   | * -BYTESTREAM                                                        |
|                                                                        |                                                          | * -COMPREHENSIVE                                                     |
|                                                                        |                                                          | * -DATABASE                                                          |
|                                                                        |                                                          | * -INCREMENTAL                                                       |
|                                                                        |                                                          | * -RECORD                                                            |
+------------------------------------------------------------------------+----------------------------------------------------------+----------------------------------------------------------------------+
| -SO[URCE]                                                              | REPLICATE                                                | * -ACTIVATE                                                          |
|                                                                        |                                                          | * -BUFFSIZE=Buffer_size                                              |
|                                                                        |                                                          | * -CHANGELOG                                                         |
|                                                                        |                                                          | * -CHECKHEALTH                                                       |
|                                                                        |                                                          | * -CMPLVL=integer                                                    |
|                                                                        |                                                          | * -CONNECTPARAMS=connection_options                                  |
|                                                                        |                                                          | * -DEACTIVATE                                                        |
|                                                                        |                                                          | * -DETAIL                                                            |
|                                                                        |                                                          | * -FILTER=filter_name                                                |
|                                                                        |                                                          | * -FREEZE=on/off                                                     |
|                                                                        |                                                          | * -[NO]COMMENT=string                                                |
|                                                                        |                                                          | * -INSTSECONDARY=secondary_instance name                             |
|                                                                        |                                                          | * -NOJNLFILEONLY                                                     |
|                                                                        |                                                          | * -JNLPOOL-LOG=log_file                                              |
|                                                                        |                                                          | * -LOG_INTERVAL=integer                                              |
|                                                                        |                                                          | * -LOSTTNCOMPLETE                                                    |
|                                                                        |                                                          | * -NEEDRESTART                                                       |
|                                                                        |                                                          | * -PASSIVE                                                           |
|                                                                        |                                                          | * -[NO]PLAINTEXTFALLBACK                                             |
|                                                                        |                                                          | * -PROPAGATEPRIMARY                                                  |
|                                                                        |                                                          | * -RENEGOTIATE_INTERVAL=minutes                                      |
|                                                                        |                                                          | * -ROOTPRIMARY                                                       |
|                                                                        |                                                          | * -SECONDARY=secondary_instance_name                                 |
|                                                                        |                                                          | * -SHOWBACKLOG                                                       |
|                                                                        |                                                          | * -SHUTDOWN                                                          |
|                                                                        |                                                          | * -START                                                             |
|                                                                        |                                                          | * -STATSLOG                                                          |
|                                                                        |                                                          | * -STOPSOURCEFILTER                                                  |
|                                                                        |                                                          | * -TIMEOUT=seconds                                                   |
|                                                                        |                                                          | * -TLSID=label                                                       |
|                                                                        |                                                          | * -UPDOK                                                             |
|                                                                        |                                                          | * -UPDNOTOK                                                          |
|                                                                        |                                                          | * -ZEROBACKLOG                                                       |
+------------------------------------------------------------------------+----------------------------------------------------------+----------------------------------------------------------------------+
| -VERSION={V4|V5}                                                       | DOWNGRADE and UPGRADE                                    | * file-name                                                          |
+------------------------------------------------------------------------+----------------------------------------------------------+----------------------------------------------------------------------+


