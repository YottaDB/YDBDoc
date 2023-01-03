.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2017-2023 YottaDB LLC and/or its subsidiaries.#
.. # All rights reserved.                                        #
.. #                                                             #
.. #     This document contains the intellectual property        #
.. #     of its copyright holder(s), and is made available       #
.. #     under a license.  If you do not know the terms of       #
.. #     the license, please stop and do not read further.       #
.. #                                                             #
.. ###############################################################

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

For MUPIP commands pertaining to database journaling, refer to `Chapter 6: “YottaDB Journaling” <./ydbjournal.html>`_.

For MUPIP commands pertaining to multisite database replication, refer to `Chapter 7: “Database Replication” <./dbrepl.html>`_.

.. note::
   Two MUPIP operations - INTRPT and STOP - perform process management functions. All other MUPIP operations relate to the operation of the database.

The YottaDB installation procedure places the MUPIP utility program in a directory specified by $ydb_dist.

Invoke MUPIP by executing the mupip program at the shell prompt. If this does not work, consult your system manager (MUPIP requires that the $ydb_dist point to the directory containing the MUPIP executable image).

.. code-block:: bash

   $ydb_dist/mupip
   MUPIP>

MUPIP asks for commands, with the MUPIP> prompt. Enter the EXIT command at the MUPIP> prompt to stop the utility. MUPIP performs one operation at a time, and automatically terminates after most operations.

When additional information appears on the command line after the mupip program name, MUPIP processes the additional information as its command, for example:

.. code-block:: bash

   $ydb_dist/mupip stop 1158

This starts MUPIP and stops the process with Process ID (PID) 1158.

Some MUPIP commands require information contained in the global directory. Therefore, a process must have access to a valid global directory before using any MUPIP commands other than EXIT, INTRPT, JOURNAL, RESTORE, STOP and the -file option for any command that has that option.

The environment variable ydb_gbldir specifies the active global directory.

A ydb_gbldir value of yottadb.gld tells MUPIP to look for a global directory file yottadb.gld in the current directory. For more information on the global directory, refer to `“Global Directory Editor” <./gde.html>`_.

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
|                                                                   |                                       | [NO]ONLINE.                                                                         |
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
| Modify database and/or journal file characteristics               | MUPIP SET                             | Standalone access is required if the MUPIP SET command specifies ACCESS_METHOD,     |
|                                                                   |                                       | GLOBAL_BUFFERS, MUTEX_SLOTS, LOCK_SPACE or NOJOURNAL, or if any of the JOURNAL      |
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
|                                                                   |                                       | FILE                                                                                |
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

.. _mupip:

+++++++++++
MUPIP
+++++++++++

The general format of MUPIP commands is:

.. code-block:: bash

   mupip command [-qualifier[...]] [object[,...]] [destination]

MUPIP allows the abbreviation of commands and qualifiers. In each section describing a command or qualifier, the abbreviation is also shown (for example, B[ACKUP]). The abbreviated version of B[ACKUP] you can use on the command line is B. To avoid future compatibility problems and improve readability, specify at least four characters when using MUPIP commands in scripts.

Although you can enter commands in both upper and lower case (the mupip program name itself must be in lower case on UNIX/Linux), the typographical convention used in this chapter is all small letters for commands. Another convention is in the presentation of command syntax. If the full format of the command is too long for a single line of print, the presentation wraps around into additional lines.

.. code-block:: bash

   $ mupip backup -bytestream -transaction=1 accounts,history,tables,miscellaneous /var/production/backup/

When you enter a MUPIP command, one of its variable arguments is the region-list. region-list identifies the target of the command and may include the UNIX wildcards "?" and "*". Region-lists containing UNIX wildcard characters must always be quoted, for example, "*" to prevent inappropriate expansion by the UNIX shell. Similarly, for file and directory names you might want to avoid non-graphic characters and most punctuations except underscores (_), not because of YottaDB conventions but because of inappropriate expansion by UNIX shells.

MUPIP qualifier values are restricted only by the maximum size of the command input line, which is 4KB on some systems and upto 64KB on others.

.. note::
   MUPIP sends its output to stderr not stdout. On shells such as :code:`bash` stderr can be redirected to stdout by `specifying 2>&1 on the command line <https://www.gnu.org/software/bash/manual/bash.html#Redirecting-Standard-Output-and-Standard-Error>`_.

--------------------------
Commands and Qualifiers
--------------------------

The MUPIP commands described in this section are used for common database operations and serves as the foundation for more advanced functionality like `Journaling <./ydbjournal.html>`_ and `Replication <./dbrepl.html>`_.

.. _mupip-backup:

++++++++++++
BACKUP
++++++++++++

.. include:: mupipbackup.inc

.. _mupip-create:

++++++++++++++++
CREATE
++++++++++++++++

Creates and initializes database files using the information in a Global Directory file. If a file already exists for any segment, MUPIP CREATE takes no action for that segment.

The format of the CREATE command is:

.. code-block:: none

   CR[EATE] [-R[EGION]=region-name]

The single optional REGION qualifier specifies a region for which to create a database file.

Note that one YottaDB database file grows to a maximum size of 1,040,187,392(992Mi) blocks. This means, for example, that with an 8KiB block size, the maximum single database file size is 7,936GiB (8KiB*992Mi). Also, this is the size of one database file -- a logical database (an M global variable namespace) can consist of an arbitrary number of database files.

Note that a MUPIP CREATE command that explicitly specifies a region which is tagged as :ref:`AutoDB <region-no-autodb>`, creates the database file for that region if it does not exist.

~~~~~~~~~~
-Region
~~~~~~~~~~

Specifies a single region for creation of a database file. By default, MUPIP CREATE creates database files for all regions in the current Global Directory that do not already have a database file.

The format of the REGION qualifier is:

.. code-block:: none

   -R[EGION]=region-name

The region-name is case-insensitive. The specified region name is converted into upper case before processing.

~~~~~~~~~~~~~~~~~~~~~~~~~
Examples for MUPIP CREATE
~~~~~~~~~~~~~~~~~~~~~~~~~

Example:

.. code-block:: bash

   $ mupip create -region=MAMMALS

This command creates the database file specified by the Global Directory (named by the Global Directory environment variable) for region MAMMALS.

.. _mupip-downgrade:

++++++++++
DOWNGRADE
++++++++++

The MUPIP DOWNGRADE command changes the file header format to a previous version number. The format of the MUPIP DOWNGRADE command is:

.. code-block:: none

   D[OWNGRADE] -V[ERSION]={r1.10|r1.20} file-name

.. note::
   You must perform a database integrity check using the -noonline parameter prior to downgrading a database. The integrity check verifies and clears database header fields required for an orderly downgrade. If an integrity check is not possible due to time constraints, please rely on a rolling upgrade scheme using replication and/or take a backup prior to upgrading the database.

~~~~~~~~~~~~~~~~~~~
-VERSION={version}
~~~~~~~~~~~~~~~~~~~

For more information on the downgrade criteria for your database, refer to the release notes document of your current YottaDB version.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Examples for MUPIP DOWNGRADE
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Example:

.. code-block:: bash

   $ mupip downgrade yottadb.dat

This command changes the file-header of yottadb.dat to the format in the previous version.

+++++++++++++++++++++
DUMPFHEAD
+++++++++++++++++++++

The MUPIP DUMPFHEAD command displays information about one or more database files. The format of the MUPIP DUMPFHEAD command is:

.. code-block:: none

   DU[MPFHEAD] {-F[ILE] file-name | -R[EGION] region-list}


~~~~~~~~~~~~~~~~
-FILE file-name
~~~~~~~~~~~~~~~~

Specifies the name of the database file for the MUPIP DUMPFHEAD operation. FILE does not require a Global Directory. The format of the FILE qualifier is:

.. code-block:: none

   -F[ILE] file-name

* The database filename must include the absolute or relative path.

* The FILE qualifier is incompatible with the REGION qualifier.

~~~~~~~~~~~~~~~~~~~~
-REGION region-list
~~~~~~~~~~~~~~~~~~~~

Specifies that the INTEG parameter identifies one or more regions rather than a database file. The format of the REGION qualifier is:

.. code-block:: none

   -R[EGION] region-list

* The region-list identifies the target of DUMPFHEAD. region-list may specify more than one region of the current global directory in a list. Regions are case-insensitive, separated by a comma, and wildcards can be used to specify them. Any region-name may include the wildcard characters * and ? (remember to escape them to protect them from inappropriate expansion by the shell). Any region name expansion occurs in M (ASCII) collation order.

* The region-list argument may specify more than one region of the current Global Directory in a list separated with commas. DUMPFHEAD REGION requires the environment variable ydb_gbldir to specify a valid Global Directory. For more information on defining ydb_gbldir, refer to `Chapter 4: “Global Directory Editor” <./gde.html>`_.

* The REGION qualifier is incompatible with the FILE qualifier.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Examples for MUPIP DUMPFHEAD
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Example:

.. code-block:: bash

   $ mupip dumpfhead -file yottadb.dat

This command lists information about the database file yottadb.dat in the current working directory.

.. code-block:: bash

   $ mupip dumpfhead -region "*"

This command lists information about all the database files mapped by the global directory specified by $ydb_gbldir.

+++++++++++++++
ENDIANCVT
+++++++++++++++

Converts a database file from one endian format to the other (BIG to LITTLE or LITTLE to BIG). The format of the MUPIP ENDIANCVT command is:

.. code-block:: none

   ENDIANCVT [-OUTDB=<outdb-file>] -OV[ERRIDE] <db-file>

* <db-file> is the source database for endian conversion. By default ENDIANCVT converts <db-file> in place.

* outdb writes the converted output to <outdb-file>. In this case, ENDIANCVT does not modify the source database <db-file>.

* ENDIANCVT produces a <outdb-file> of exactly the same size as <db-file>.

.. note::
   Ensure adequate storage for <outdb-file> to complete the endian conversion successfully.

* ENDIANCVT requires standalone access to the database.

* YottaDB displays a confirmation request with the "from" and "to" endian formats to perform the conversion. Conversion begins only upon receiving positive confirmation, which is a case-insensitive "yes".

* In a multi-site replication configuration, the receiver server automatically detects the endian format of an incoming replication stream and converts it into the native endian format. See the `Database Replication chapter <./dbrepl.html>`_ for more information.

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

~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Examples for MUPIP ENDIANCVT
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: bash

   $ mupip endiancvt yottadb.dat -outdb=yottadb_cvt.dat
   Converting database file yottadb.dat from LITTLE endian to BIG endian on a LITTLE endian system
   Converting to new file yottadb_cvt.dat
   Proceed [yes/no] ?

This command detects the endian format of yottadb.dat and converts it to the other endian format if you type yes to confirm.

++++++++++
EXIT
++++++++++

Stops a MUPIP process and returns control to the process from which MUPIP was invoked.

The format of the MUPIP EXIT command is:

.. code-block:: none

   EXI[T]

The EXIT command does not accept any qualifiers.

++++++++++++
EXTEND
++++++++++++

Increases the size of a database file. By default, YottaDB automatically extends a database file when there is available space.

The format of the MUPIP EXTEND command is:

.. code-block:: none

   EXTE[ND] [-BLOCKS=<data-blocks-to-add>] region-name

* The only qualifier for MUPIP EXTEND is BLOCKS.

* The required region-name parameter specifies the name of the region to expand.

* EXTEND uses the Global Directory to map the region to the dynamic segment and the segment to the file.

~~~~~~~~
-Blocks
~~~~~~~~

Specifies the number of GDS database blocks by which MUPIP should extend the file. GDS files use additional blocks for bitmaps. MUPIP EXTEND adds the specified number of blocks plus the bitmap blocks required as overhead. For more information about bitmaps, refer to `Chapter 9: “YottaDB Database Structure(GDS)” <./gds.html>`_.

The format of the BLOCK qualifier is:

.. code-block:: none

   -BLOCKS=data-blocks-to-add

By default, EXTEND uses the extension value in the file header as the number of GDS blocks by which to extend the database file. You can specify as many blocks as needed as long as you are within the maximum total blocks limit (which could be as high as 224 million GDS blocks).

~~~~~~~~~~~~~~~~~~~~~~~~~
Examples for MUPIP EXTEND
~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: bash

   $ mupip extend DEFAULT -blocks=400

This command adds 400 GDS database blocks to region DEFAULT.

Example:

.. code-block:: bash

   $ mupip extend MAMMALS -blocks=100

This command adds 100 GDS database blocks to the region MAMMALS.

++++++++++++++++++
EXTRACT
++++++++++++++++++

Backs up certain globals or extracts data from the database for use by another system. The MUPIP EXTRACT command copies globals from the current database to a sequential output file in one of three formats - GO, BINARY, or ZWR. The format of the MUPIP EXTRACT command is:

.. code-block:: none

   EXTR[ACT]
   [
    -FO[RMAT]={GO|B[INARY]|Z[WR]}
    -FR[EEZE]
    -LA[BEL]=text
    -[NO]L[OG]
    -[NO]NULL_IV
    -R[EGION]=region-list
    -SE[LECT]=global-name-list]
   ]
   {-ST[DOUT]|file-name}

* By default, the FORMAT of MUPIP EXTRACT is ZWR.

* MUPIP EXTRACT uses the Global Directory to determine which database files to use.

* MUPIP EXTRACT supports user collation routines. When used without the FREEZE qualifier, EXTRACT may operate concurrently with normal YottaDB database access.

* To ensure that MUPIP EXTRACT reflects a consistent application state, suspend the database updates to all regions involved in the extract, typically with the FREEZE qualifier, or backup the database with the ONLINE qualifier and extract files from the backup.

* EXTRACT places its output in the file defined by the file-name.

* In UTF-8 mode, MUPIP EXTRACT writes a sequential output file in the UTF-8 character encoding. Ensure that the MUPIP EXTRACT commands and corresponding MUPIP LOAD commands execute with the same setting for the environment variable ydb_chset.

* The GO format is not supported for UTF-8 mode. Use BINARY or ZWR formats in UTF-8 mode.

For information on extracting globals with the %GO utility, refer to the `"Utility Routines" chapter of the Programmer's Guide <../ProgrammersGuide/utility.html>`_. MUPIP EXTRACT is typically faster, but %GO can be customized.

The following sections describe the qualifiers of MUPIP EXTRACT command.

~~~~~~~
-FORMAT
~~~~~~~

Specifies the format of the output file. The format of the FORMAT qualifier is:

.. code-block:: none

   -FO[RMAT]=format_code

The format code is any one of the following:

1. B[INARY] - Binary format, used for database reorganization or short term backups. MUPIP EXTRACT FORMAT=BINARY works much faster than MUPIP EXTRACT FORMAT=GO and MUPIP EXTRACT FORMAT=ZWR. Note: There is no defined standard to transport binary data from one YottaDB implementation to another. Furthermore, YottaDB reserves the right to modify the binary format in new versions. The first record of a BINARY format data file contains the header label. The header label is 102 characters long. The following table illustrates the components of the header label.

   +----------------------------+----------------------------------------------------------------------------------------------------------------------+
   | Characters                 | Explanation                                                                                                          |
   +============================+======================================================================================================================+
   | 1-2                        | Hexadecimal representation of the length of the label (by default 64 - decimal 100).                                 |
   +----------------------------+----------------------------------------------------------------------------------------------------------------------+
   | 3-28                       | Fixed-length (26 bytes) ASCII text containing:                                                                       |
   |                            |                                                                                                                      |
   |                            |   * "GDS BINARY EXTRACT LEVEL 6": when no region is encrypted.                                                       |
   |                            |   * "GDS BINARY EXTRACT LEVEL 8": when one more regions are encrypted using null IVs.                                |
   |                            |   * "GDS BINARY EXTRACT LEVEL 9": when one or regions are encrypted using non-null IVs.                              |
   +----------------------------+----------------------------------------------------------------------------------------------------------------------+
   | 29-42                      | Fixed-length (14 bytes) ASCII text: Date and time of extract in the $ZDATE() format: "YEARMMDD2460SS"                |
   +----------------------------+----------------------------------------------------------------------------------------------------------------------+
   | 43-49                      | Fixed-length (7 bytes) ASCII text: Decimal maximum block size of each region from which data was extracted           |
   +----------------------------+----------------------------------------------------------------------------------------------------------------------+
   | 50-56                      | Fixed-length (7 bytes) ASCII text: Decimal maximum record size of each region from which data is extracted           |
   +----------------------------+----------------------------------------------------------------------------------------------------------------------+
   | 57-63                      | Fixed-length (7 bytes) ASCII text: Decimal maximum key size of each region from which data is extracted              |
   +----------------------------+----------------------------------------------------------------------------------------------------------------------+
   | 64-70                      | Fixed-length (7 bytes) ASCII text:Boolean indicator of Standard NULL collation (1) or                                |
   |                            | `historical null collation <../ProgrammersGuide/langfeat.html#null-subs-colltn>`_ (0).                               |
   +----------------------------+----------------------------------------------------------------------------------------------------------------------+
   | 71-102                     | Fixed-length (32 bytes) ASCII text: Space-padded label specified by the -LABEL qualifier; the default LABEL is       |
   |                            | "MUPIP EXTRACT". For extracts in UTF-8 mode, YottaDB prefixes UTF-8 and a space to -LABEL.                           |
   +----------------------------+----------------------------------------------------------------------------------------------------------------------+

2. GO - Global Output format, used for files to transport or archive. FORMAT=GO stores the data in record pairs. Each global node produces two records - the first contains the key and the second contains the value. GO format is only supported in M mode.

3. ZWR - ZWRITE format, used for files to transport or archive that may contain non-graphical information. Each global node produces one record with both a key and data. Note that for non-ASCII data, M mode and UTF-8 mode extracts can differ, as the definition of printable characters differs.

GO and ZWR format output files have two header records. The first is a text label (refer to the :ref:`LABEL qualifier <mupip-extract-label>`), defaulting to: :code:`"YottaDB MUPIP EXTRACT"` followed by the command line used to generate the extract, including the full path to the mupip executable, followed by UTF-8 if the process ran in UTF-8 mode; the second is the date and time of extract in $ZDATE() format DD-MON-YEAR 24:60:SS, and, for ZWR extracts, the text :code:`"ZWR"`.

.. note::
   ZWR format is suitable for all data. Use GO format for data that contains only printable characters and spaces, as some characters (such as linefeed) can corrupt the output file format.

The GO and ZWR format output header was enhanced in release `r1.30. <https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.30>`_

.. _mupip-extract-freeze:

~~~~~~~
-FREEZE
~~~~~~~

Prevents database updates to all database files from which the MUPIP EXTRACT command is copying records. FREEZE ensures that a MUPIP EXTRACT operation captures a "sharp" image of the globals, rather than one "blurred" by updates occurring while the copy is in progress.

The format of the FREEZE qualifier is:

.. code-block:: none

   -FR[EEZE]

By default, MUPIP EXTRACT does not "freeze" regions during operation.

.. _mupip-extract-label:

~~~~~~~~
-LABEL
~~~~~~~~

Specifies the text string that becomes the first record in the output file. MUPIP EXTRACT FORMAT=BINARY truncates the label text to 32 characters. The format of the LABEL qualifier is:

.. code-block:: none

   -LA[BEL]=text

* By default, EXTRACT uses the label "MUPIP EXTRACT."

* For more detailed information about the FORMAT=BINARY header label, refer to the description of EXTRACT FORMAT=BINARY.

~~~~~~
-LOG
~~~~~~

Displays a message on stdout for each global extracted with the MUPIP EXTRACT command. The message displays the number of global nodes, the maximum subscript length and maximum data length for each global. The format of the LOG qualifier is:

.. code-block:: none

   -[NO]LO[G]

By default, EXTRACT operates -LOG.

~~~~~~~~~
-NULL_IV
~~~~~~~~~

Creates an encrypted binary extract with null IVs from a database with non-null IVs, which can be restored to a version that does not support non-null IVs. The format of the NULL_IV qualifier is:

.. code-block:: none

   -[NO]NULL_IV

* Older versions of YottaDB used empty (all zeros or "NULL_IV") initialization vectors(IVs) to encrypt or decrypt FORMAT="BINARY" extracts.

* The current and later versions use non-zero IVs.

* Use the NULL_IV qualifier only on encrypted databases to create an encrypted binary extract in GDS BINARY EXTRACT LEVEL 8 format. This format can load data on any encrypted YottaDB database created with an older version.

* The default is NONULL_IV which produces a binary extract in GDS BINARY EXTRACT LEVEL 9 format.

~~~~~~~
-REGION
~~~~~~~

Restricts MUPIP EXTRACT to a set of regions. The format of the REGION qualifier is:

.. code-block:: none

   -R[EGION]=region-list

region-list may specify more than one region of the current global directory in a list. Regions are case-insensitive, separated by a comma, and wildcards can be used to specify them. Any region-name may include the wildcard characters * and % (remember to escape them to protect them from inappropriate expansion by the shell). Any region name expansion occurs in M (ASCII) collation order.

~~~~~~~~
-SELECT
~~~~~~~~

Specifies globals for a MUPIP EXTRACT operation. The format of the SELECT qualifier is:

.. code-block:: none

   -S[ELECT]= global-specification

* By default, EXTRACT selects all globals, as if it had the qualifier SELECT=*

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

.. code-block:: none

   -ST[DOUT]

~~~~~~~~~~~~~~~~~~~~~~~~~~
Examples for MUPIP EXTRACT
~~~~~~~~~~~~~~~~~~~~~~~~~~

Example:

.. code-block:: bash

   $ mupip extract -format=go -freeze big.glo

This command prevents database updates during a MUPIP EXTRACT operation.

Example:

.. code-block:: bash

   $ mupip extract -format=GO yottadb_i.go

This command creates an extract file called yottadb_i.go in "Global Output" format. Use this format to transport or archive files. The first record of a GO format file contains the header label, "MUPIP EXTRACT," as text.

Example:

.. code-block:: bash

   $ mupip extract -format=BINARY v5.bin

This command creates an extract file called v5.bin in Binary format. Use this format for reorganizing a database or for short-term backups.

Example:

.. code-block:: bash

   $ mupip extract -format=ZWR -LABEL=My_Label My_Extract_File

This example extracts all globals from the current database to file My_Extract_File (in ZWRITE format) with label My_Label.

Example:

.. code-block:: bash

   $ mupip extract -nolog FL.GLO

This command creates a global output file, FL.GLO, (which consists of all global variables in the database) without displaying statistics on a global-by-global basis. As there is no label specified, the first record in FL.GLO contains the text string "MUPIP EXTRACT."

Example:

.. code-block:: bash

   $ mupip extract -select=Tyrannosaurus /dev/tty

This command instructs EXTRACT to dump the global ^Tyrannosaurus to the device (file-name) /dev/tty.

.. _mupip-freeze:

++++++++++++++
FREEZE
++++++++++++++

Temporarily suspends (freezes) updates to the database after ensuring a consistent state between memory and secondary storage, which, with ACCESS_METHOD=BG, means after flushing global buffers. If you prefer a non-YottaDB utility to perform a backup or reorganization, you might use this facility to provide standalone access to your YottaDB database. You might use MUPIP FREEZE to suspend (and later resume) database updates for creating mirrored disk configuration or re-integrating a mirror.

BACKUP, INTEG, and REORG operations may implicitly freeze and unfreeze database regions. However, for most operations, this freeze/unfreeze happens internally and is transparent to the application.

The format of the MUPIP FREEZE command is:

.. code-block:: none

   F[REEZE] {-OF[F] [-OV[ERRIDE]]|-ON [[-ONL[INE] [-[NO]AUTORELEASE]] | [-NOONL[INE]] [-R[ECORD]]]} region-list

* The region-list identifies the target of the FREEZE. region-list may specify more than one region of the current global directory in a list. Regions are case-insensitive, separated by a comma, and wildcards can be used to specify them. Any region-name may include the wildcard characters * and % (remember to escape them to protect them from inappropriate expansion by the shell). Any region name expansion occurs in M (ASCII) collation order.

* MUPIP FREEZE waits for up to one minute so that concurrent KILL or MUPIP REORG operations can complete. If the KILL or MUPIP REORG commands do not complete within one minute, MUPIP FREEZE unfreezes any regions it had previously marked as frozen and terminates with an error.

* To ensure that a copy or reorganized version of a database file contains a consistent set of records, concurrent MUPIP utilities, such as BACKUP (without the ONLINE qualifier) and EXTRACT, include mechanisms to ensure that the database does not change while the MUPIP utility is performing an action. YottaDB recommends the use of the ONLINE qualifier with BACKUP.

* A MUPIP FREEZE can be removed only by the user who sets the FREEZE or by using OVERRIDE.

* A MUPIP FREEZE ON can specify either NOONLINE, the default, or ONLINE, and if ONLINE, can specify either AUTORELEASE, the default, or NOAUTORELEASE.

* A FREEZE specifying ONLINE attempts to minimize the impact of the FREEZE on concurrently updating processes.

* A FREEZE specifying ONLINE AUTORELEASE allows updates to continue immediately when YottaDB needs to update the database file. The processes release the freeze if they cannot find global buffers to do their work.

* After MUPIP FREEZE ON NOONLINE, processes that are attempting updates "hang" until the FREEZE is removed by the MUPIP FREEZE OFF command or DSE. Make sure that procedures for using MUPIP FREEZE, whether manual or automated, include provisions for removing the FREEZE in all appropriate cases, including when errors disrupt the normal flow.

* MUPIP FREEZE sends a DBFREEZEON/DBFREEZEOFF message to the system log for each region whose freeze state is changed.

* A RECOVER/ROLLBACK for a database reverts to a prior database update state. Therefore, a RECOVER/ROLLBACK immediately after a MUPIP FREEZE ON removes the freeze. However, RECOVER/ROLLBACK does not succeed if there are processes attached (for example when a process attempts a database update immediately after a MUPIP FREEZE ON) to the database.

FREEZE must include one of the qualifiers:

.. code-block:: none

   -OF[F]
   -ON

The optional qualifiers are:

.. code-block:: none

   -[NO]A[UTORELEASE] - only valid with -ONLINE
   -ON[LINE] - only valid with -ON
   -OV[ERRIDE]
   -R[ECORD] - only valid with -ON

~~~~~
-OFF
~~~~~

Clears a freeze set by another process with the same userid.

The format of the OFF qualifier is:

.. code-block:: none

   OF[F]

* A FREEZE OFF which turns off a FREEZE ONLINE AUTORELEASE produces a OFRZNOTHELD warning to indicate that the freeze was automatically released and therefore did not protect whatever concurrent actions it was intended to guard.

* When used with OVERRIDE, OFF stops a freeze operation set by a process with a different userid.

* Incompatible with: ON, RECORD

~~~~
-ON
~~~~

Specifies the start of a MUPIP FREEZE operation. The format of the ON qualifier is:

.. code-block:: none

   -ON

Incompatible with: OFF, OVERRIDE

~~~~~~~~~~~~~~~~~~
-[NO]A[UTORELEASE]
~~~~~~~~~~~~~~~~~~

Controls the behavior of a FREEZE specified with -ONLINE when YottaDB must write to a database file. The format of the AUTORELEASE qualifier is:

.. code-block:: none

   -[NO]A[UTORELEASE]

* AUTORELEASE, the default, causes YottaDB to release the freeze if it needs to update the file before a FREEZE OFF.

* When FREEZE -ONLINE is in effect, MUPIP rejects all SET actions and produces the OFRZACTIVE warning; if you need to perform any such SET actions, you must release the freeze first.

* NOAUTORELEASE causes YottaDB to hold off actions that need to update the database file until someone issues a MUPIP FREEZE OFF.

* The actions that require YottaDB to write to the database file are:

  * Insufficient global buffers to hold updates - YottaDB must flush buffers to make space to do any additional updates

  * Insufficient space in the database to hold updates - YottaDB must extend the file

  * The journal file reaches its maximum size or someone issues a MUPIP SET JOURNAL command - YottaDB must create a new journal file

  * An epoch comes due - YottaDB must create a checkpoint

  * Someone issues a MUPIP BACKUP command - YottaDB must record state information to mark the beginning of the backup

* When an AUTORELEASE abandons a FREEZE, any actions that depend on the stability of the database file on secondary storage, such as a database copy, lose that protection and are not reliable, so they likely need to be repeated at a time when an AUTORELEASE is less likely or when NOONLINE is more appropriate.

* An AUTORELEASE action produces an OFRZAUTOREL message in the operator log.

* An AUTORELEASE action requires a FREEZE OFF to reestablish a normal database state.

* Incompatible with: OFF, NOONLINE

~~~~~~~~
-ONLINE
~~~~~~~~

Controls the potential impact of a FREEZE on concurrently updating processes. The format of the ONLINE qualifier is:

.. code-block:: none

   -[NO]ONL[INE]

* ON NOONLINE, the default, causes the freeze to last until OFF, and makes management of the FREEZE straightforward.

* ON ONLINE, causes YottaDB to attempt to minimize the impact of the FREEZE on concurrently updating processes by taking a number of actions, as appropriate:

  * Switching journal files to provide maximum space
  * Performing an epoch to provide maximum time to the next epoch
  * Flushing the global buffers to make all available to hold updates
  * Incompatible with: AUTORELEASE, OFF

* After performing these preparations, ONLINE allows updating processes to make updates to global buffers but defer flushing them to the database file.

* ONLINE cannot apply to MM databases, so a FREEZE ONLINE skips any MM regions it encounters.

* Refer to AUTORELEASE above for additional information.

* Incompatible with: OFF

.. note::
   If any database region is nearly full, run MUPIP EXTEND before attempting a MUPIP FREEZE ON ONLINE. Otherwise, should a database file extension become necessary after the MUPIP FREEZE takes effect, the freeze will be released (if AUTORELEASE was specified or implicitly assumed by default) or all updates will be blocked (if NOAUTORELEASE was specified) effectively turning the ONLINE freeze into a NOONLINE freeze.

~~~~~~~~~~
-OVERRIDE
~~~~~~~~~~

Release a freeze set by a process with a different userid. YottaDB provides OVERRIDE to allow error recovery in case a procedure with a freeze fails to release. The format of the OVERRIDE qualifier is:

.. code-block:: none

   -OV[ERRIDE]

* OVERRIDE should not be necessary (and may even be dangerous) in most schemes.

* Incompatible with: AUTORELEASE, ON, ONLINE, RECORD

~~~~~~~~
-RECORD
~~~~~~~~

Specifies that a MUPIP FREEZE operation should record an event as a reference point. You might use MUPIP FREEZE to set up your database for a custom-backup mechanism (SAN or mirror-based).

The format of the RECORD qualifier is:

.. code-block:: none

   -R[ECORD]

* You might use RECORD to integrate MUPIP BACKUP BYTESTREAM with an external backup mechanism.

* RECORD replaces the previously RECORDed transaction identifier for the database file.

* Incompatiable with: OFF and OVERRIDE.

~~~~~~~~~~~~~~~~~~~~~~~~~
Examples for MUPIP FREEZE
~~~~~~~~~~~~~~~~~~~~~~~~~

Example:

.. code-block:: bash

   $ mupip freeze -off DEFAULT

This command stops an ongoing MUPIP FREEZE operation on the region DEFAULT.

Example:

.. code-block:: bash

   $ mupip freeze -on "*"

This command prevents updates to all regions in the current Global Directory.

Example:

.. code-block:: bash

   $ set +e
   $ mupip freeze -on -record "*"
   $ tar cvf /dev/tape /prod/appl/*.dat
   $ mupip freeze -off
   $ set -e

The set +e command instructs the shell to attempt all commands in the sequence , regardless of errors encountered by any command. This ensures that the FREEZE OFF is processed even if the tar command fails. FREEZE prevents updates to all database files identified by the current Global Directory. The RECORD qualifier specifies that the current transaction in each database be stored in the RECORD portion of the database file header. The tar command creates a tape archive file on the device :code:`/dev/tape`, containing all the files from :code:`/prod/app` that have an extension of .dat. Presumably all database files in the current Global Directory are stored in that directory, with that extension. The second FREEZE command re-enables updates that were suspended by the first FREEZE. The :code:`set -e` command re-enables normal error handling by the shell.

Example:

.. code-block:: bash

   $ mupip freeze -override -off DEFAULT

This command unfreezes the DEFAULT region even if the freeze was set by a process with a different userid.

+++++++++
FTOK
+++++++++

Produces the "public" (system generated) IPC Keys (essentially hash values) of a given file-list or the journal/receiver pool in a table form.

The format of the MUPIP FTOK command is:

.. code-block:: none

   FT[OK] [-DB] [-JNLPOOL] [-RECVPOOL] [-ID] [-ONLY] [-[NO]HEADER] file-name

where,

* file-list a space delimited list of files, such as that provided by the use of the * and ? shell wildcard characters.

* With -JNLPOOL or -RECVPOOL, MUPIP FTOK ignores any files in the list.

~~~
-DB
~~~

Specifies that the file-name is a database file. By default, MUPIP FTOK uses DB.

~~~~~~~~~
-JNLPOOL
~~~~~~~~~

Specifies that the reported key is for the Journal Pool of the instance created by the current Global Directory.

~~~~~~~~~
-RECVPOOL
~~~~~~~~~

Specifies that the reported key is for the Receive Pool of the instance created by the current Global Directory.

~~~~~
-ID
~~~~~

Specified the signature use for the FTOK; if unspecified, it defaults to the signature for a YottaDB database file.

ID qualifier was added to YottaDB effective release r1.36.

~~~~~~~
-ONLY
~~~~~~~

Restricts the output to only the FTOK key; if a file is not valid and accessible, FTOK reports -1 values. ONLY does not report the table header even when HEADER is specified. If the database file is unavailable, the utility defaults to the ONLY behavior.

ONLY qualifier was added to YottaDB effective release r1.36.

~~~~~~~~~
-HEADER
~~~~~~~~~

Displays or omits the header of the table. By default, MUPIP FTOK displays the header.

Example:

.. code-block:: none

   $ mupip ftok -id mumps.dat
		  File ::            Semaphore Id ::        Shared Memory Id ::                FTOK Key ::                             FileId
   -------------------------------------------------------------------------------------------------------------------------------------------
              mumps.dat :: 1044382209 [0x3e400201] ::  613417274 [0x2490013a] ::  725039842 [0x2b373ae2] :: 0x00000000000e09818000002900000001

   $ mupip ftok -id -only mumps.dat
	      mumps.dat  ::   725039842  [ 0x2b373ae2 ]
   $ mupip ftok -id -noheader mumps.dat
              mumps.dat :: 1044382209 [0x3e400201] ::  613417274 [0x2490013a] ::  725039842 [0x2b373ae2] :: 0x00000000000e09818000002900000001

   $ mupip ftok -jnlpool
                   File ::            Semaphore Id ::        Shared Memory Id ::                FTOK Key ::                             FileId
   -------------------------------------------------------------------------------------------------------------------------------------------
             multi.repl ::         -1 [0xffffffff] ::         -1 [0xffffffff] ::  743218009 [0x2c4c9b59] :: 0x00000000000e09838000002900000001
                jnlpool ::  636486196 [0x25f00234] ::  578814256 [0x22800130]

HEADER qualifier was added to YottaDB effective release r1.36.

+++++++++++++++
HASH
+++++++++++++++

Uses a 128 bit hash based on the MurmurHash3 algorithm to provide the hash of source files from the command line.

The format of the MUPIP HASH command is:

.. code-block:: none

   MUPIP HASH <file-names>

.. _mupip-integ:

++++++++++++
INTEG
++++++++++++

.. include:: mupipinteg.inc

.. _mupip-intrpt:

+++++++
INTRPT
+++++++

Sends an interrupt signal [POSIX] SIGUSR1 to the specified process, whose signal handler determines how it is handled. M processes execute the `$ZINTERRUPT <../ProgrammersGuide/isv.html#zinterrupt-isv>`_ intrinsic special variable. The format of the MUPIP INTRPT command is:

.. code-block:: none

   INTRPT process-id

.. note::
   Ensure that signal SIGUSR1 is not be used by any C external function calls or any (initially non-YottaDB) processes that use call-in support, as it is interpreted by YottaDB as a signal to trigger the $ZINTERRUPT mechanism.

* To INTRPT a process belonging to its own account, a process requires no UNIX privileges.

* The implementation of INTRPT uses signals, and on Linux a non-root process can only send signals to other processes of the same userid. Superuser privilege is required to send signals to processes of other userids, regardless of group membership.

++++++++++
JOURNAL
++++++++++

Analyzes, extracts, reports, and recovers data using journal files. For a description of the JOURNAL command, refer to `Chapter 6: “YottaDB Journaling” <./ydbjournal.html>`_.

+++++++
LOAD
+++++++

Puts the global variable names and their corresponding data values into a YottaDB database from a sequential file.

The format of the LOAD command is:

.. code-block:: none

   L[OAD]
   [-BE[GIN]=integer -E[ND]=integer
   -FI[LLFACTOR]=integer
   -FO[RMAT]={GO|B[INARY]|Z[WR]]}
   -I[GNORECHSET]
   -O[NERROR]={STOP|PROCEED|INTERACTIVE}
   -S[TDIN]] file-name

.. note::
   From an application perspective, performing a MUPIP LOAD operation while an application is running may result in an inconsistent application state for the database.

* MUPIP LOAD uses the Global Directory to determine which database files to use.

* LOAD supports user collation routines.

* LOAD takes its input from the file defined by the file-name, which may be a UNIX file on any device that supports such files.

* LOAD accepts files with DOS style termination.

* MUPIP LOAD command considers a sequential file as encoded in UTF-8 if the environment variable ydb_chset is set to UTF-8. Ensure that MUPIP EXTRACT commands and the corresponding MUPIP LOAD commands execute with the same setting for the environment variable ydb_chset.

* For information on loading with an M "percent utility," refer to the `%GI section of the "M Utility Routines" chapter in the Programmer's Guide <../ProgrammersGuide/utility.html#gi-util>`_. LOAD is typically faster, but the %GI utility can be customized.

* Press <CTRL-C> to produce a status message from LOAD. Entering <CTRL-C> twice in quick succession stops LOAD. A LOAD that is manually stopped or stops because of an internal error is incomplete and may lack application level integrity, but will not adversely affect the database structure unless terminated with a :code:`kill -9`.

.. note::
   The MUPIP EXTRACT or MUPIP LOAD procedure for large databases is time consuming due to the volume of data that has to be converted from binary to ZWR format (on source) and vice versa (on target). One must also consider the fact that the extracted file can be very large for a large database. Users must ensure there is adequate storage space to support the size of the extract file and the space occupied by the source and target databases. In order to reduce the total time and space it takes to transfer database content from one endian platform to another, it is efficient to convert the endian format in-place for a database and transfer the converted database. See MUPIP ENDIANCVT for more information on converting the endian format of a database file.

The following sections describe the optional qualifiers of the MUPIP LOAD command.

~~~~~~~~
-FORMAT
~~~~~~~~

Specifies the format of the input file. If the format of the input file is not specified, MUPIP LOAD automatically detects the file format (BINARY/ZWR/GO) based on the file header or (when the header is absent) the key and value information in the file. If the format is specified, it must match the actual format of the input file for LOAD to proceed.

The format codes are:

.. code-block:: none

   B[INARY] - Binary format
   GO - Global Output format
   Z[WR] - ZWRITE format

* MUPIP LOAD detects the file format (BINARY/ZWR/GO) based on the file header of the extracted files from MUPIP EXTRACT, ^%GO and DSE.

* :code:`-format=binary` only applies to GDS files. A BINARY format file loads significantly faster than a GO or ZWR format file. FORMAT=BINARY works with data in a proprietary format. FORMAT=BINARY has one header record, therefore LOAD FORMAT=BINARY starts active work with record number two (2).

* FORMAT={ZWR|GO} applies to text files produced by tools such as MUPIP EXTRACT or %GO.

* For FORMAT={ZWR|GO} UTF-8 files not produced by MUPIP EXTRACT or %GO, the first line of the label must contain the case insensitive text "UTF-8".

* For all FORMAT={ZWR|GO} files not produced by MUPIP EXTRACT or %GO, the second line should contain the case insensitive text "ZWR" for zwr format or "GLO" for GO format and the two label lines must contain a total of more than 10 characters.

* FORMAT=GO expects the data in record pairs. Each global node requires one record for the key and one for the data.

* FORMAT=ZWR expects the data for each global node in a single record.

~~~~~~~
-BEGIN
~~~~~~~

Specifies the record number of the input file with which LOAD should begin. Directing LOAD to begin at a point other than the beginning of a valid key causes an error. The format of the BEGIN qualifier is:

.. code-block:: none

   -BE[GIN]=integer

.. note::
   Always consider the number of header records for choosing a BEGIN point. See FORMAT qualifier for more information.


* For FORMAT=GO input, the value is usually an odd number. As FORMAT=BINARY requires important information from the header, this type of load requires an intact file header regardless of the BEGIN value.

* For FORMAT=ZWR input, each record contains a complete set of reference and data information. The beginning values are not restricted, except to allow two records for the header.

* By default, LOAD starts at the beginning of the input file.

~~~~~
-END
~~~~~

Specifies the record number of the input file at which LOAD should stop. END integer value must be greater than the BEGIN integer value for LOAD to operate. LOAD terminates after processing the record of the number specified by END or reaching the end of the input file. The format of the END qualifier is:

.. code-block:: none

   -E[ND]=integer

The value of FORMAT=GO input should normally be an even number. By default, LOAD continues to the end of the input file.

~~~~~~~~~~~~
-FILLFACTOR
~~~~~~~~~~~~

Specifies the quantity of data stored in a database block. Subsequent run-time updates to the block fill the remaining available space reserved by the FILL_FACTOR. Blocks that avoid block splits operate more efficiently. The format of the FILL_FACTOR qualifier is:

.. code-block:: none

   -FI[LL_FACTOR]=integer

* Reserves room and avoids unnecessary block splits to accommodate the forecasted growth in a global variable that may experience significant rate of additions over a period of time.

* Users having database performance issues or a high rate of database updates must examine the defined FILL_FACTORs. Unless the application only uses uniform records, which is not typical for most applications, FILL_FACTORs do not work precisely.

* By default, LOAD uses FILL_FACTOR=100 for maximum data density.

.. note::
   FILL_FACTOR is useful when updates add or grow records reasonably uniformly across a broad key range. If updates are at ever-ascending or ever-descending keys, or if the record set and record sizes are relatively static in the face of updates, FILL_FACTOR does not provide much benefit.

~~~~~~~~~~~~~~
-IGNORECHSET
~~~~~~~~~~~~~~

Notifies MUPIP LOAD to load the extract file even if it was created by a MUPIP process in another mode (UTF-8 mode vs. M mode). The format of the IGNORECHSET qualifier is:

.. code-block:: none

   -I[GNORECHSET]

.. note::

   As using IGNORECHSET bypasses YottaDB checks, use it only if you are sure that the extract file can be loaded correctly.

IGNORECHSET was added to YottaDB effective release `r1.30 <https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.30>`_.

~~~~~~~~~
-ONERROR
~~~~~~~~~

Determines the MUPIP LOAD behavior when it encounters an error. The format of the ONERROR qualifier is:

.. code-block:: none

   -O[NERROR]={STOP|PROCEED|INTERACTIVE}

- STOP causes MUPIP LOAD to exit immediately.

- PROCEED proceeds to the next record.

- INTERACTIVE prompts to continue or stop.

By default MUPIP LOAD exits on encountering an error.

~~~~~~~
-STDIN
~~~~~~~

Specifies that MUPIP LOAD takes input from standard input (stdin). The format of the STDIN qualifier is:

.. code-block:: none

   -S[TDIN]

~~~~~~~~~~~~~~~~~~~~~~~
Examples for MUPIP LOAD
~~~~~~~~~~~~~~~~~~~~~~~

Example:

.. code-block:: bash

   $ mupip load ex_file.go

This command loads the content of the extract file ex_file.go to the current database.

Example:

.. code-block:: bash

   $ mupip load -format=go big.glo

This command loads an extract file big.glo in the current database.

Example:

.. code-block:: bash

   $ mupip load -begin=5 -end=10 rs.glo

This command begins the MUPIP LOAD operation from record number 5 and ends at record number 10. Note that the value for BEGIN is an odd number. A sample output from the above command follows:

.. code-block:: bash

   MUPIP EXTRACT
   02-MAR-2017 18:25:47 ZWR
   Beginning LOAD at record number: 5
   LOAD TOTAL Key Cnt: 6
   Max Subsc Len: 7
   Max Data Len: 1
   Last LOAD record number: 10

Example:

.. code-block:: bash

   $ mupip load -fill_factor=5 reobs.glo

This command sets the FILL_FACTOR to 5 for loading an extract file in the current database.

Example:

.. code-block:: bash

   $cat big.glo | mupip load -stdin
   $mupip load -stdin < big.glo

These commands loads the extract file big.glo using -stdin.

++++++++
RCTLDUMP
++++++++

Reports information related to relinkctl files and their associated shared memory segments. The format of the MUPIP RCTLDUMP command is:

.. code-block:: none

   MUPIP RCTLDUMP [dir1]

If the optional parameter dir1 is not specified, MUPIP RCTLDUMP dumps information on all its active auto-relink-enabled directories (those with with a \*-suffix) identified by $ydb_routines. With a directory path specified for dir1, MUPIP RCTLDUMP reports information on that directory. An example output follows. It lists the full path of the Object directory; its corresponding relinkctl file name; the number of routines currently loaded in this relinkctl file; the number of processes including the reporting MUPIP process that have this relinkctl file open; the shared memory id and length of the relinkctl shared memory segment; one or more rtnobj shared memory segment(s); and a listing of all the routine names loaded in this file (lines starting with rec#...).

* The Rtnobj shared memory line : All the length fields are displayed in hexadecimal. shmlen is the length of the allocated shared memory segment in bytes. shmused is the length that is currently used. shmfree is the length available for use. objlen is the total length of all the objects currently loaded in this shared memory. As YottaDB allocates blocks of memory with sizes rounded-up to an integer power of two bytes, shmused is always greater than objlen; for example with an objlen of 0x1c0, the shmused is 0x200.

* Lines of the form rec#... indicate the record number in the relinkctl file. Each relinkctl file can store a maximum of 1,000,000 records, i.e., the maximum number of routines in a directory with auto-relink enabled is one million. Each record stores a routine name (rtnname:), the current cycle for this object file record entry (cycle:) which gets bumped on every ZLINK or ZRUPDATE command, the hash of the object file last loaded for this routine name (objhash:), the number of different versions of object files loaded in the Rtnobj shared memory segments with this routine name (numvers:), the total byte-length of the one or more versions of object files currently loaded with this routine name (objlen:), the total length used up in shared memory for these object files where YottaDB allocates each object file a rounded-up perfect 2-power block of memory (shmlen:).

Given a relinkctl file name, one can find the corresponding directory path using the Unix "strings" command on the Relinkctl file. For example, :code:`strings /tmp/ydb-relinkctl-f0938d18ab001a7ef09c2bfba946f002`, corresponding to the above MUPIP RCTLDUMP output example, would output :code:`/obj` the corresponding directory name.

Example:

.. code-block:: bash

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

.. code-block:: none

   REO[RG]
   [
    -D[OWNGRADE]
    -ENCR[YPT]=key
    -E[XCLUDE]=global-name-list
    -FI[LL_FACTOR]=integer
    -I[NDEX_FILL_FACTOR]=integer
    -NOCO[ALESCE]
    -NOSP[LIT]
    -NOSW[AP]
    -R[ESUME]
    -S[ELECT]=global-name-list
    -T[RUNCATE][=percentage]
    -UP[GRADE]
    -REG[ION] region-list
   ]

.. note::
   While REORG optimizes the GDS structure of database files, it does not handle native file system file fragmentation. In most cases, fragmentation at the native file system level is more likely than fragmentation at the GDS structure level. Therefore, YottaDB recommends users create files with appropriate allocations and extensions, on disks with large amounts of contiguous free space. Use native utilities and MUPIP utilities (depending on operational procedures) to eliminate file fragmentation when database files have been extended more than a dozen times.

* As REORG is IO intensive, running a REORG concurrently with normal database access may impact the operation of normal processes. As the YottaDB database engine has a daemonless architecture, attempts to reduce the impact by reducing the priority of REORG can (perhaps counter-intuitively) exacerbate rather than alleviate the impact. To reduce the impact REORG has on other processes, use the ydb_poollimit environment variable to limit the number of global buffers used by the REORG.

* MUPIP REORG does not change the logical contents of the database, and can run on either the originating instance or replicating instance of an LMS application. In such cases, resuming REORGs in process should be part of the batch restart. See the `"Database Replication" chapter <./dbrepl.html>`_ for more information about running REORG on a dual site application.

* Use MUPIP STOP (or <Ctrl-C> for an interactive REORG) to terminate a REORG process. Unless terminated with a :code:`kill -9`, a REORG terminated by operator action or error is incomplete but does not adversely affect the database.

.. note::
   REORG focuses on optimum adjacency and a change to even a single block can cause it to perform a large number of updates with only marginal benefit. Therefore, YottaDB recommends not running successive REORGs close together in time, as much as can provide minimal benefit for a significant increase in database and journal activity. For the same reason, YottaDB recommends careful research and planning before using the RESUME qualifier or complex uses of EXCLUDE and SELECT.

Assume two scenarios of putting values of ^x(1) to ^x(10000). In the first scenarios, fill values in a sequential manner. In the second scenario, enter values for odd subscripts and then enter values for the even subscripts.

Scenario 1:

At the prompt, execute the following command sequence:

.. code-block:: bash

   YDB>for i=1:1:10000 set ^x(i)=$justify(i,200)

Then, execute the following MUPIP INTEG command.

.. code-block:: bash

   $ mupip integ -region "*"

This command produces an output like the following:

.. code-block:: bash

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

.. code-block:: bash

   YDB>for i=1:2:10000 s ^x(i)=$justify(i,200)
   YDB>for i=2:2:10000 set ^x(i)=$justify(i,200)

Then, execute the following command:

.. code-block:: bash

   $ mupip integ -region "*"

This command produces an output like the following:

.. code-block:: bash

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

~~~~~~~~~~~~
-DOWNGRADE
~~~~~~~~~~~~

Changes the database blocks to V4 format of the upstream code base. The transaction number fields are reduced to 32 bits. In the event the CTN is too large to fit in 32 bits, a MUPIP INTEG TN_RESET will need to be performed prior to performing the MUPIP REORG DOWNGRADE. The blks_to_upgrd counter increases to signify a downgrading database.

.. code-block:: bash

   MUPIP> reorg -downgrade -reg DEFAULT

   Region DEFAULT : MUPIP REORG DOWNGRADE started
   Region DEFAULT : Desired DB Format set to V4 by MUPIP REORG DOWNGRADE
   Region DEFAULT : Started processing from block number [0x00000000]
   Region DEFAULT : Stopped processing at block number [0x00000839]
   Region DEFAULT : Statistics : Blocks Read From Disk (Bitmap)     : 0x00000005
   Region DEFAULT : Statistics : Blocks Skipped (Free/Recycled)     : 0x00000012
   Region DEFAULT : Statistics : Blocks Read From Disk (Non-Bitmap) : 0x00000822
   Region DEFAULT : Statistics : Blocks Skipped (new fmt in disk)   : 0x00000000
   Region DEFAULT : Statistics : Blocks Skipped (new fmt in cache)  : 0x00000000
   Region DEFAULT : Statistics : Blocks Converted (Bitmap)          : 0x00000005
   Region DEFAULT : Statistics : Blocks Converted (Non-Bitmap)      : 0x00000822
   Region DEFAULT : Total Blocks = [0x00000839] : Free Blocks = [0x00000012] : Blocks to upgrade = [0x00000827]
   Region DEFAULT : MUPIP REORG DOWNGRADE finished

.. note::

   The V4 format applies only to very old versions of the upstream code base. The ability to downgrade is inherited from the upstream code base and is not tested or supported by YottaDB. This feature is deprecated in YottaDB and will be removed from the YottaDB code base when it is removed from the upstream code base.

.. _mupip-reorg-encrypt:

~~~~~~~~~
-ENCRYPT
~~~~~~~~~

Encrypts an unencrypted database or changes the encryption key of a database while the database continues to be used by applications. Whether or not the prior encryption uses non-zero initialization vectors (IVs), database blocks encrypted with the new key use non-zero IVs. The format of the ENCRYPT qualifier is:

.. code-block:: none

   -ENCR[YPT]=<key>

MUPIP provides <key> to the encryption plugin. The reference implementation of the plugin expects a key with the specified name in the encryption configuration file identified by $ydb_crypt_config. The configuration file must contain an entry in the database section for each database file mapping to a region specified in <region-list> that names the specified key as its key. The ENCRYPT flag is incompatible with all other command line flags of MUPIP REORG except REGION, and performs no operation other than changing the encryption key. If the specified key is already the encryption key of a database region, MUPIP REORG ENCRYPT moves on to the next region after displaying a message (on stderr, where MUPIP operations send their output).

As MUPIP REORG ENCRYPT reads, re-encrypts, and writes every in-use block in each database file, its operations take a material amount of time on the databases of typical applications, and furthermore add an additional IO load to the system on which it runs. You can use the environment variable ydb_poollimit to ameliorate, but not eliminate the impact, at the cost of extending execution times. To minimize impact on production instances, YottaDB recommends running this operation on replicating secondary instances, rather than on originating primary instances.

ENCRYPT switches the journal file for each database region when it begins operating on it, and again when it completes, and also records messages in the syslog for both events.

As is the case under normal operation when MUPIP REORG ENCRYPT is not active, journaled databases are protected against system crashes when MUPIP REORG ENCRYPT is in operation: MUPIP JOURNAL ROLLBACK/RECOVER recovers journaled database regions (databases that use NOBEFORE journaling continue to require RECOVER/ROLLBACK FORWARD).

Because a database file utilizes two keys while MUPIP REORG ENCRYPT is underway, the database section of the configuration file provides for a single database file entry to specify multiple keys. For example, if the keys of database regions CUST and HIST, mapping to database files cust.dat and hist.dat in directory /var/myApp/prod, are to be changed from key1 to key2 using the command:

.. code-block:: none

   MUPIP REORG -ENCRYPT=key2 -REGION CUST,HIST

then the database section of the configuration file must at least have the following entries:

.. code-block:: none

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

MUPIP REORG ENCR[YPT] can encrypt an unencrypted database only if the following command:

.. code-block:: none

   MUPIP SET -ENCRYPTABLE -REGION <region-list>

has previously marked the database "encryptable".

The command requires standalone access to the database.  It performs some basic encryption setup checks and requires the ydb_passwd environment variable to be defined and the GNUPGHOME environment variable to point to a valid directory in the environment. Just as encrypted databases use global buffers in pairs (for encrypted and unencrypted versions of blocks), a database marked as encryptable has global buffers allocated in pairs (i.e., the actual number of global buffers is twice the number reported by DSE DUMP FILEHEADER) and requires correspondingly larger shared memory segments. To revert unencrypted but encryptable databases back to the "unencryptable" state, use the command:

.. code-block:: none

   MUPIP SET -NOENCRYPTABLE -REGION <region-list>

The above command also requires standalone access, and the result depends on the state of the database. It:

* is a no-op if the database is encrypted;

* is disallowed if the database is partially (re)encrypted; and

* prohibits encryption if the database is not encrypted.

Under normal operation, a database file has only one key. Upon starting a MUPIP REORG ENCRYPT to change the key, there are two keys, both of whose hashes YottaDB stores in the database file header. With a MUPIP REORG ENCRYPT operation underway to change the key, normal database operations can continue, except for another MUPIP REORG ENCRYPT or MUPIP EXTRACT in binary format. Other MUPIP operations, such as MUPIP BACKUP and MUPIP EXTRACT in ZWR format can occur. A MUPIP REORG ENCRYPT operation can resume after an interruption, either unintentional, such as after a system crash and recovery, or intentional, i.e., an explicit MUPIP STOP of the MUPIP REORG ENCRYPT process. To resume the REORG operation, reissue the original command, including the key parameter. (Note that supplying a key other than the one used in the original command produces an error.)

After the MUPIP REORG ENCRYPT process completes, subsequent MUPIP REORG ENCRYPT operations on the same region(s) are disallowed until the following command is run:

.. code-block:: none

   MUPIP SET -ENCRYPTIONCOMPLETE -REGION <region-list>

Blocking subsequent MUPIP REORG ENCRYPT operations after one completes, provides time for a backup of the entire database before enabling further key changes. MUPIP SET ENCRYPTIONCOMPLETE reports an error for any database region for which MUPIP REORG ENCRYPT has not completed.

.. note::
   YottaDB recommends rotating (changing) the encryption key of the database for better security. The frequency of encryption key rotation depends on your security requirements and policies.

.. note::
   MUPIP REORG ENCRYPT does not enable switching between encryption algorithms. To migrate databases from Blowfish CFB to AES CFB requires that the data be extracted and loaded into newly created database files. To minimize the time your application is unavailable, you can deploy your application in a Logical Multi-Site (LMS) configuration, and migrate using a rolling upgrade technique. Refer to the `Chapter 7: “Database Replication” <./dbrepl.html>`_ for more complete documentation.

~~~~~~~~~
-EXCLUDE
~~~~~~~~~

Specifies that REORG not handle blocks that contain information about the globals in the associated list–this means they are neither reorganized nor swapped in the course of reorganizing other globals; EXCLUDE can reduce the efficiency of REORG because it complicates and interferes with the block swapping actions that try to improve adjacency.

The format of the EXCLUDE qualifier is:

.. code-block:: none

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

* In case any global appears in the argument lists of both SELECT and EXCLUDE, REORG terminates with an error.

~~~~~~~~~~~~~
-FILL_FACTOR
~~~~~~~~~~~~~

Specifies how full you want each database block to be. This is a target number. Individual blocks may be more or less full than the fill factor. The format of the FILL_FACTOR qualifier is:

.. code-block:: none

   F[ILL_FACTOR]=integer

* The arguments for the FILL_FACTOR qualifier must be integers from 30 to 100. These integers represent the percentage of the data block that REORG can fill. By default, the FILL_FACTOR value is 100 for maximum data density.

* Users who come upon database performance issues or a high rate of database updates must examine the defined FILL_FACTORs. Unless the application uses entirely uniform records, which is not typical for most applications, FILL_FACTORs do not work precisely.

* The FILL_FACTOR for data that is relatively static, or grows by the addition of new nodes that collate before or after pre-existing nodes, should be 100 percent. The FILL_FACTOR for data that is growing by additions to existing nodes may be chosen to leave room in the typical node for the forecast growth for some period. Generally, this is the time between the LOAD and first REORG, or between two REORGs. This is also true for additions of nodes that are internal to the existing collating sequence.

~~~~~~~~~~~~~~~~~~~
-INDEX_FILL_FACTOR
~~~~~~~~~~~~~~~~~~~

Directs REORG to leave free space within index blocks for future updates. Arguments to this qualifier must be integers from 30 to 100 that represent the percentage of the index block that REORG can fill. REORG uses this number to decide whether to place more information in an index block, or create space by moving data to another block. The format of the INDEX_FILL_FACTOR qualifier is:

.. code-block:: none

   -I[NDEX_FILL_FACTOR]=integer

Under certain conditions, especially with large database block sizes, it may be possible to achieve faster throughput by using a smaller fill factor for index blocks than for data blocks. By default, the INDEX_FILL_FACTOR is the value of FILL_FACTOR regardless of whether that value is explicitly specified or implicitly obtained by default.

~~~~~~~~~~~~~
-NOCOALESCE
~~~~~~~~~~~~~

-NOCOALESCE specifies to MUPIP REORG to skip actions that increase block density. The format of the NOCOALESCE qualifier is :

.. code-block:: none

   -NOCO[ALESCE]

By default, MUPIP REORG attempts to rearrange blocks which are below the specified fill-factor to pack data more densely. More tightly packed data can increase block I/O and reduce the storage requirements.

~~~~~~~~~~
-NOSPLIT
~~~~~~~~~~

NOSPLIT specifies to MUPIP REORG to skip actions that decrease block density. The format of the NOSPLIT qualifier is:

.. code-block:: none

   -NOSP[LIT]

By default, MUPIP REORG attempts to rearrange blocks which are above the specified fill-factor to pack data less densely. Having empty space in blocks can reduce the need for block splits going forward, which may improve performance.

~~~~~~~~~
-NOSWAP
~~~~~~~~~

NOSWAP specifies to MUPIP REORG to skip actions that increase block adjacency. The format of the NOSWAP qualifier is:

.. code-block:: none

   -NOSW[AP]

By default, MUPIP REORG attempts to rearrange blocks so that logically related blocks have adjacent block numbers. On rotating storage, this tends to improve performance; on solid state storage the results are device specific and may increase device wear. Swap activities tend to generate a lot of journal file volume.

~~~~~~~~~
-RESUME
~~~~~~~~~

For an interrupted REORG operation, RESUME allows the user to resume the REORG operation from the point where the operation stopped. REORG stores the last key value in the database file header. The format of the RESUME qualifier is:

.. code-block:: none

   -R[ESUME]

* With RESUME specified, the program retrieves the last key value, from the database file header, and restarts operations from that key.

~~~~~~~~
-REGION
~~~~~~~~

Specifies that REORG operate in the regions in the associated list and restricts REORG to the globals in those regions that are mapped by the current global directory; it does not have the same interactions as EXCLUDE and SELECT, but it does not mitigate those interactions when combined with them.

The format of the REGION qualifier is:

.. code-block:: none

   -R[EGION] region-list

region-list may specify more than one region of the current global directory in a list. Regions are case-insensitive, separated by a comma, and wildcards can be used to specify them. Any region-name may include the wildcard characters * and % (remember to escape them to protect them from inappropriate expansion by the shell). Any region name expansion occurs in M (ASCII) collation order.

~~~~~~~~~
-SELECT
~~~~~~~~~

Specifies that REORG reorganizes only the globals in the associated list; globals not on the list may be modified by block swaps with selected globals unless they are named with EXCLUDE; SELECT can be difficult to use efficiently because it tends to de-optimize unselected globals unless they are named in an EXCLUDE list (which introduces inefficiency).

The format of the SELECT qualifier is:

.. code-block:: none

   -S[ELECT]=global-name-list

* By default, REORG operates on all globals in all database files identified by the current Global Directory for the process executing the MUPIP command.

* One of the functions performed by REORG is to logically order the globals on which it operates within the file. Unless the EXCLUDE and SELECT qualifiers are properly used in tandem, repeating the command with different selections in the same file wastes work and leaves only the last selection well organized.

* If you enter the REORG SELECT=global-name-list command and the specified globals do not exist, REORG issues a message to the screen and continues to process any specified globals that exist. If REORG is unable to process any globals, it terminates with an error.

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

.. code-block:: none

   -T[RUNCATE][=percentage]

The optional percentage (0-99) provides a minimum amount for the reclamation; in other words, REORG won't bother performing a file truncate unless it can give back at least this percentage of the file; the default (0) has it give back anything it can. TRUNCATE always returns space aligned with bit map boundaries, which fall at 512 database block intervals. TRUNCATE analyses the bit maps, and if appropriate, produces before image journal records as needed for recycled (formerly used) blocks; The journal extract of a truncated database file may contain INCTN records having the inctn opcode value 9 indicating that the specific block was marked from recycled to free by truncate.

MUPIP REORG TRUNCATE recognizes the KEEP qualifier. The format of the KEEP qualifier is:

.. code-block:: none

   [-KEEP=blocks|percent%]

The argument to KEEP specifies either a number of database blocks or a percentage (0-99), followed by a percent-sign (%), of the starting total blocks to exclude from truncation.

.. note::
   TRUNCATE does not complete if there is a concurrent online BACKUP or use of the snapshot mechanism, for example by INTEG.

~~~~~~~~~~~~~~~~~~~~~~~~
Examples for MUPIP REORG
~~~~~~~~~~~~~~~~~~~~~~~~

Example:

.. code-block:: bash

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

.. code-block:: bash

   $ mupip reorg -exclude="^b2a,^A4gsEQ2e:^A93"

This example performs a MUPIP REORG operation on all globals excluding ^b2a and all globals ranging from ^A4gsEQ2e to ^A93.

Example:

If the forecasted growth of a global is 5% per month from relatively uniformly distributed updates, and REORGs are scheduled every quarter, the FILL_FACTOR for both the original LOAD and subsequent REORGs might be 80 percent 100 - ((3 months + 1 month "safety" margin) * five percent per month). The REORG command based on the above assumptions is as follows:

.. code-block:: bash

   $ mupip reorg -fill_factor=80

Example:

The following example uses REORG ENCRYPT to encrypt a database "on the fly". This is a simple example created for demonstration purposes. It is NOT recommended for production use. Consult your YottaDB support channel for specific instructions on encrypting an unencrypted database.

Create an empty default unencrypted database.

.. code-block:: bash

   $ydb_dist/yottadb -r ^GDE exit
   $ydb_dist/mupip create

Setup the GNUPG home directory.

.. code-block:: bash

   export GNUPGHOME=$PWD/.helengnupg3
   mkdir $GNUPGHOME # Ensure that you protect this directory with appropriate permissions.
   chmod go-rwx $GNUPGHOME

Create a new key. Enter demo values. Accept default values. Choose a strong passphrase.

.. code-block:: bash

   gpg --gen-key

Edit the key to add a new sub-key:

.. code-block:: bash

   gpg --edit-key helen.keymaster@yottadb

Type addkey, select option 6 RSA (encrypt only), and accept default values and execute the following commands:

.. code-block:: bash

   gpg --gen-random 2 32 | gpg --encrypt --default-recipient-self --sign --armor > ydb_workshop_key.txt
   gpg --decrypt < ./ydb_workshop_key.txt | gpg --encrypt --armor --default-recipient-self --output ydb.key

Refer to the 'man gpg; a description on the qualifiers for gpg.

Create a gtmcrypt_config file as following:

.. code-block:: bash

   $ cat config
     database: {
        keys:   (
                  {
                   dat: "/path/to/yottadb.dat" ;
                   key: "/path/to/ydb.key" ;
                  }
               );
  }

Set the environment variable gtmcrypt_config to point to this config file.

.. code-block:: bash

   export gtmcrypt_config=$PWD/config

Set the environment variable ydb_passwd.

.. code-block:: bash

   echo -n "Enter passphrase for ydb.key: " ; export ydb_passwd=`$ydb_dist/plugin/gtmcrypt/maskpass|cut -f 3 -d " "`

Execute the following commands:

.. code-block:: bash

   $ mupip set -encryptable -region DEFAULT
   $ mupip reorg -encrypt="ydb.key" -region DEFAULT
   mupip reorg -encrypt="ydb.key" -region DEFAULT
   Region DEFAULT : MUPIP REORG ENCRYPT started
   Region DEFAULT : Database is now FULLY ENCRYPTED with the following key: ydb.key
   Region DEFAULT : MUPIP REORG ENCRYPT finished

Execute the following command when encryption completes.

.. code-block:: bash

   $ mupip set -encryptioncomplete -region DEFAULT
   Database file /home/gtc_twinata/staff/nitin/tr11/yottadb.dat now has encryption marked complete

Always keep the keys in a secured location. Always set gtmcrypt_config and ydb_passwd to access the encrypted database.

~~~~~~~~~~
-UPGRADE
~~~~~~~~~~

Upgrades the GDS blocks that are still in V4 format, after completion of mupip upgrade. This variant of the mupip reorg command runs through the entire database (or until blocks-to-convert = 0) checking the block format of each block. If the block format is V4, it is updated to V5 format and rewritten.

A block is considered to be too long to be upgraded from V4 format to V5 format if the records occupy more than blocksize-16 bytes. The presence of even one such block will prevent a database from being upgraded.

After a database has been certified, such "too long" blocks can occur if the Reserved Bytes field is reduced with a DSE CHANGE FILEHEADER command or if a MUPIP JOURNAL RECOVER or MUPIP JOURNAL ROLLBACK command changes the state of the of the database to before the dbcertify certify operation.

This condition can be detected by MUPIP REORG UPGRADE or during normal V5.0-000 operation when blocks are upgraded from V4 to V5 format. The only way to recover from this condition is to downgrade the database to V4 format and re-run both phases of dbcertify.

This condition can be avoided by not changing Reserved Bytes after any dbcertify step and before the MUPIP UPGRADE, and, in the event, a database has a MUPIP JOURNAL RECOVER or MUPIP JOURNAL ROLLBACK performed on it, then to repeat the dbcertify steps before the MUPIP UPGRADE.

^^^^^^^^^^
-nosafejnl
^^^^^^^^^^

If before image journaling is active, MUPIP REORG UPGRADE will generate before image records for these block changes even though there is no change to the data in the block. This is to ensure that backwards recovery can recover the database correctly, in the event of a system crash or power outage. In the event that a system has a battery-backed IO subsystem, or a SAN, it is unlikely that there will be incomplete writes to the journal files. As long as any pending write to the journal file is completed in the event of a system crash or power outage, the before image records are not required to recover the database. In the event your hardware guarantees that there will not be an incomplete IO write operation, you can reduce IO load on your system by suppressing the generation of these before images with the use of the NOSAFEJNL option. If your hardware does not provide such a guarantee, then YottaDB strongly recommends the use of the default behavior, which is to generate the before image records.

.. code-block:: bash

   MUPIP> reorg -upgrade -reg DEFAULT

   Region DEFAULT : MUPIP REORG UPGRADE started
   Region DEFAULT : Desired DB Format remains at V5 after MUPIP REORG UPGRADE
   Region DEFAULT : Started processing from block number [0x00000000]
   Region DEFAULT : Stopped processing at block number [0x00000800]
   Region DEFAULT : Statistics : Blocks Read From Disk (Bitmap)     : 0x00000005
   Region DEFAULT : Statistics : Blocks Skipped (Free/Recycled)     : 0x00000000
   Region DEFAULT : Statistics : Blocks Read From Disk (Non-Bitmap) : 0x000007FC
   Region DEFAULT : Statistics : Blocks Skipped (new fmt in disk)   : 0x00000017
   Region DEFAULT : Statistics : Blocks Skipped (new fmt in cache)  : 0x00000000
   Region DEFAULT : Statistics : Blocks Converted (Bitmap)          : 0x00000003
   Region DEFAULT : Statistics : Blocks Converted (Non-Bitmap)      : 0x000007E6
   Region DEFAULT : Total Blocks = [0x00000839] : Free Blocks = [0x00000012] : Blocks to upgrade = [0x00000000]
   Region DEFAULT : MUPIP REORG UPGRADE finished

.. note::

   As YottaDB does not use the V4 format of the upstream code base, this feature exists only to migrate old databases from the upstream code base to YottaDB.

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

.. code-block:: bash

   mupip reorg -user_defined_reorg="DETAIL"

This example records every block and every operation (coalesce/swap/split) and gives detailed information about the REORG.

.. code-block:: bash

   mupip reorg -user_defined_reorg="NOCOALESCE"

This example carries out the REORG but does not coalesce (combine) any blocks.

.. code-block:: bash

   mupip reorg -user_defined_reorg="NOSPLIT"

This example carries out the REORG but does not split any blocks.

.. code-block:: bash

   mupip reorg -user_defined_reorg="NOSWAP"

This example carries out the REORG but does not swap any blocks.

.. code-block:: bash

   mupip reorg -user_defined_reorg="NOSWAP,NOCOALESCE"

This example combines two specifications, and carries out the REORG without swapping or coalescing any blocks.

.. _mupip-replicate:

++++++++++++
REPLICATE
++++++++++++

Control the logical multi-site operation of YottaDB. For more information on the qualifiers of the MUPIP REPLICATE command, refer to `Chapter 7: “Database Replication” <./dbrepl.html>`_ .

+++++++++
RESTORE
+++++++++

Integrates one or more BACKUP INCREMENTAL files into a corresponding database. The transaction number in the first incremental backup must be one more than the current transaction number of the database. Otherwise, MUPIP RESTORE terminates with an error.

The format of the RESTORE command is:

.. code-block:: none

   RE[STORE] [-NET[TIMEOUT]] [-[NO]E[XTEND]] file-name bytestrm-bkup-list


* file-name identifies the name of the database file that RESTORE uses as a starting point.

* bytestrm-bkup-list specifies one or more bytestream backup files (generated with BACKUP BYTESTREAM) to RESTORE into the database file specified with file-name. The file-list are separated by commas (,) and must be in sequential order, from the oldest transaction number to the most recent transaction number. RESTORE may take its input from a UNIX file on any device that supports such files.

* bytestrm-bkup-list may also include a comma separated list of pipe commands or TCP socket addresses (a combination of IPv4 or IPV6 hostname and a port number) from where MUPIP RESTORE receives bytestream backup files (produced with BACKUP BYTESTREAM).

* The current transaction number in the database must match the starting transaction number of each successive input to the RESTORE.

* If the BACKUP BYTESTREAM was created using TRANSACTION=1, create a new database with MUPIP CREATE and do not access it, except the standalone MUPIP commands INTEG FILE, EXTEND, and SET before initiating the RESTORE.

~~~~~~~~~
-EXTEND
~~~~~~~~~

Specifies whether a MUPIP RESTORE operation should extend the database file automatically if it is smaller than the size required to load the data.

The format of the EXTEND qualifier is:

.. code-block:: none

   -[NO]E[XTEND]

M activity between backups may automatically extend a database file. Therefore, the database file specified as the starting point for a RESTORE may require an extension before the RESTORE. If the database needs an extension and the command specifies NOEXTEND, MUPIP displays a message and terminates. The message provides the sizes of the input and output database files and the number of blocks by which to extend the database. If the RESTORE specifies more than one incremental backup with a file list, the database file may require more than one extension.

By default, RESTORE automatically extends the database file.

~~~~~~~~~~~~~~~~~~~~~~~~~~
Examples for MUPIP RESTORE
~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: bash

   $ mupip restore backup.dat $backup_dir/backup.bk1, $backup_dir/backup.bk2, $backup_dir/backup.bk3

This command restores backup.dat from incremental backups stored in directory specified by the environment variable backup_dir.

.. code-block:: bash

   $ mupip restore ydb.dat '"gzip -d -c online5pipe.inc.gz |"'

This command uses a pipe to restore ydb.dat since its last DATABASE backup from the bytestream backup stored in online5pipe.inc.gz.

.. _mupip-rundown:

++++++++++++++++
RUNDOWN
++++++++++++++++

When database access has not been properly terminated, RUNDOWN properly closes currently inactive databases, removes abandoned YottaDB database semaphores, and releases any IPC resources used. Under normal operations, the last process to close a database file performs the RUNDOWN actions, and a MUPIP RUNDOWN is not required. If a database file is already properly rundown, a MUPIP RUNDOWN has no effect. If in doubt, it is always to safe to perform a rundown. YottaDB recommends the following method to shutdown a YottaDB application or the system:

* Terminate all YottaDB processes, and
* Rundown any and all database files that may be active.

MUPIP RUNDOWN checks for version mismatch. If there is a mismatch, it skips the region and continues with the next region. This makes it easier for multiple (non-interacting) YottaDB versions to co-exist on the same machine. Note that YottaDB does not support concurrent access to the same database file by multiple versions of the software.

The format of the MUPIP RUNDOWN command is:

.. code-block:: none

   RU[NDOWN] {-FILE file-name|-REGION region-list|-RELINKCTL [dir]|-OVERRIDE}

MUPIP RUNDOWN clears certain fields in a file that is already closed. This facilitates recovery from a system crash or any other operational anomaly.

Use RUNDOWN after a system crash or after the last process accessing a database terminates abnormally. RUNDOWN ensures that open databases are properly closed and ready for subsequent use. RUNDOWN has no effect on any database that is actively being accessed at the time the RUNDOWN is issued.

A successful MUPIP RUNDOWN of a database region removes any current MUPIP FREEZE.

RUNDOWN FILE can be directed to a statistics database file and works even if the corresponding actual database file does not exist.

To ensure database integrity, all system shutdown algorithms should include scripts that stop at YottaDB processes and perform RUNDOWN on all database files.

The RUNDOWN command may include one of the following qualifiers:

.. code-block:: none

   -F[ile]
   -R[egion]=region-list
   -RELinkctl [dir1]
   -Override

If the RUNDOWN command does not specify either FILE or REGION, it checks all the IPC resources (shared memory) on the system and if they are associated with a YottaDB database, attempts to rundown that file. MUPIP RUNDOWN with no argument removes any statistics database file resources associated with actual database file resources it can remove.

~~~~~~
-FILE
~~~~~~

Specifies that the argument is a file-name for a single database file. The FILE qualifier is incompatible with the REGION qualifier. If the rundown parameter consists of a list of files, the command only operates on the first item on the list.

Incompatible with: REGION

~~~~~~~~~
-OVERRIDE
~~~~~~~~~

Overrides the protection that prevents MUPIP RUNDOWN from performing a rundown of a replication-enabled (with BEFORE_IMAGE) database or a non-replicated NOBEFORE-journaled database that was abnormally shutdown. The protection involves issuing the MUUSERLBK error for a previously crashed replication-enabled (with BEFORE IMAGE journaling) database and the MUUSERECOV error for a non-replicated or NOBEFORE-journaled database. Both these errors prevent complications related to data recovery from a journal file or a replication-enabled database.

~~~~~~~~
-REGION
~~~~~~~~

The region-list identifies the target of the RUNDOWN. region-list may specify more than one region of the current global directory in a list. Regions are case-insensitive, separated by a comma, and wildcards can be used to specify them. Any region-name may include the wildcard characters * and % (remember to escape them to protect them from inappropriate expansion by the shell). Any region-name expansion occurs in M (ASCII) collation order.

Use the wildcard "*" to rundown all inactive regions in a global directory.

Incompatible with: FILE

When MUPIP RUNDOWN has no qualifier, it performs rundown on all inactive database memory sections on the node. Because this form has no explicit list of databases, it does not perform any clean up on regions that have no abandoned memory segments but may not have been shutdown in a crash.

~~~~~~~~~~~
-RELINKCTL
~~~~~~~~~~~

Cleans up orphaned Relinkctl files. YottaDB strongly recommends avoiding actions that tend to make such cleanup necessary - for example, :code:`kill -9` of YottaDB processes or :code:`ipcrm -m` of active Relinkctl and/or Rtnobj shared memory segments.

If the optional dir1 is not specified, MUPIP RUNDOWN RELINKCTL examines the environment variable $ydb_routines, attempts to verify and correct their attach counts and runs down all its inactive auto-relink-enabled directories (those with with a \*-suffix). Alternatively, one can specify a directory path for the parameter dir1 and MUPIP RUNDOWN RELINKCTL treats it as an auto-relink-enabled directory and runs down the resources associated with this directory. It prints a RLNKCTLRNDWNSUC message on a successful rundown and a RLNKCTLRNDWNFL message on a failure (usually because live processes are still accessing the Relinkctl file).

+++++++++++
SEMAPHORE
+++++++++++

Reports the details of a space delimited list of semaphore IDs.

The format of the MUPIP SEMAPHORE command is:

.. code-block:: none

   MUPIP SEMAPHORE <sem-ids-list>

SEMAPHORE command was added to YottaDB effective release r1.36.

.. _mupip-set:

+++++++
SET
+++++++

.. include:: mupipset.inc

++++++++++++
SIZE
++++++++++++

Estimates and reports the size of global variables using a format that is similar to the one that appears at the end of the MUPIP INTEG FULL report. In comparison with MUPIP INTEG FAST FULL, MUPIP SIZE provides the option of choosing any one of the three estimation techniques to estimate the size of global variables in a database file. These techniques vary in measurement speed and estimate accuracy. The format of the MUPIP SIZE command is:

.. code-block:: none

   MUPIP SI[ZE] [-h[euristic]=estimation_technique] [-s[elect]=global-name-list] [-r[egion]=region-list] [-a[djacency]=integer] [-su[bscript]]=global-list

The optional qualifiers of MUPIP SIZE are:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-HEURISTIC=estimation_technique
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Specifies the estimation technique that MUPIP SIZE should use to estimate the size of global variables. The format of the HEURISTIC qualifier is:

.. code-block:: none

   -h[euristic]={sc[an][,level=<lvl>] | a[rsample][,samples=<smpls>] | i[mpsample][,samples=<smpls>]}

* smpls is the number of samples and must be greater than zero (0)
* lvl is a positive or negative tree level designation and -(level of the root block) <= lvl <= (level of the root block)

estimation-technique is one of the following:

* scan,level=<lvl> : Traverses the global variable tree and counts the actual number of records and blocks at levels from the root down to the level specified by lvl (default is 0, the data blocks). If the given level is non-negative, it is the lowest block level of the global for which the count is requested. So, 0 means all blocks, 1 means all index blocks, 2 means all index blocks of level 2 and above, and so on. SCAN counts a negative level from the root of the global tree where -1 means children of the root. The technique reports the results for levels other than 0 and shows the adjacency for the next lower (one less) level.

* arsample,samples=<smpls> : Uses acceptance/rejection sampling of random tree traversals to estimate the number of blocks at each level. It continues until the specified number of samples (default is 1,000) is accepted.

* impsample,samples=<smpls> : Uses importance sampling of random tree traversals to weight each sample of the specified number of samples (default is 1,000) in order to estimate the size of the tree at each level.

* If HEURISTIC is not specified, MUPIP SIZE uses the ARSAMPLE,SAMPLE=1000 estimation technique.

The 2 sigma column for the two sampling techniques shows the dispersion of the samples (in blocks) and the probability (rounded to a whole percentage) that the actual value falls farther away from the reported value by more than two sigma. With the scan method the "sample" is "complete," so any inaccuracy comes from concurrent updates.

.. note::
   For large databases, MUPIP SIZE is faster than MUPIP INTEG FAST FULL. IMPSAMPLE is expected to be the fastest estimation technique, followed by ARSAMPLE and then SCAN. In terms of accuracy, MUPIP INTEG FAST FULL is the most accurate.

~~~~~~~~~~~~~~~~~~~
-ADJACENCY=integer
~~~~~~~~~~~~~~~~~~~

Specifies the logical adjacency of data blocks that MUPIP SIZE should assume during estimation. By default, MUPIP SIZE assumes ADJACENCY=10 and reports the logical adjacency in the "Adjacent" column of the MUPIP SIZE report. Note that adjacency is only a proxy for database organization and its usefulness may be limited by the technology and configuration of your secondary storage. See the :ref:`mupip-integ` section of this chapter for additional comments on adjacency.

~~~~~~~~
-SELECT
~~~~~~~~

Specifies the global variables on which MUPIP SIZE runs. SELECT is incompatible with SUBSCRIPT. If neither SELECT nor SUBSCRIPT is specified, MUPIP SIZE selects all global variables in the specified region(s).

The format of the SELECT qualifier is:

.. code-block:: none

   -s[elect]=global-name-list

global-name-list can be:

* A comma separated list of global variables.

* A range of global variables denoted by start:end syntax. For example, :code:`-select="g1:g4"`.

* A global variable with wildcards, for example, "g*" (the name must be escaped to avoid shell filename expansion)

* "\*" to select all global variables.

~~~~~~~~
-REGION
~~~~~~~~

Specifies the region on which MUPIP SIZE runs. If REGION is not specified, MUPIP SIZE selects all regions. The format of the REGION qualifier is:

.. code-block:: none

   -R[EGION]=region-list

The regions in the region-list are case-insensitive. The specified region-list is converted into upper case before processing.

~~~~~~~~~~~~
-SUBSCRIPT
~~~~~~~~~~~~

Specifies the subscripted/non-subscripted global variables on which MUPIP SIZE runs. SUBSCRIPT is incompatible with SELECT.

The format of the SUBSCRIPT qualifier is:

.. code-block:: none

   -SU[BSCRIPT]=global-list

*global-list* can be:

    * A range of global variables denoted by start:end syntax. For example, :code:`-subscript="^g1:^g4"`.

    * A range of subscripted global variables denoted by start:end syntax. For example, :code:`-subscript="^g(1):^g(4)"`

.. note::

   Apart from randomness caused by sampling heuristics, MUPIP SIZE also has randomness from concurrent updates because it does not use the snapshot technique that MUPIP INTEG uses.

SUBSCRIPT qualifier was added to YottaDB effective release `r1.32 <https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.32>`_.

~~~~~~~~~~~~~~~~~~~~~~~
Examples for MUPIP SIZE
~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: bash

   $ mupip size -heuristic="impsample,samples=2000" -select="y*" -region="AREG"

This example estimates the size of all global variable starting with "y". It uses importance sampling with 2000 samples on the region AREG.

.. code-block:: bash

   $ mupip size -heuristic="scan,level=-1"

This example counts the number of blocks and records at 1 level below the root of the database tree.

.. code-block:: bash

   $ mupip size -heuristic="arsample" -select="g1:g3"

This example estimates the size of global variables g1, g2 and g3 using accept/reject sampling with the default number of samples regardless of the region in which they reside.

.. note::
   Apart from randomness caused by sampling heuristics, MUPIP SIZE also has randomness from concurrent updates because it does not use the snapshot technique that MUPIP INTEG uses.

.. _mupip-stop:

++++++++++
STOP
++++++++++

Terminates a YottaDB image. The image executes an orderly disengagement from all databases that are currently open by the process, and then exits. A MUPIP STOP performs a :code:`kill -15` and therefore may also be used to stop non-YottaDB images.

The format of the STOP command is:

.. code-block:: none

   MUPIP ST[OP] process-id

* Use the shell command ps to display a list of active process names and process identifiers (PIDs).

* To STOP a process belonging to its own account, a process requires no privileges. To STOP a process belonging to another account, MUPIP STOP must execute as root.

.. note::
   On receipt of a MUPIP STOP signal, a YottaDB process cleans up its participation in managing the database before shutting down. On receipt of three MUPIP STOP signals in a row, a YottaDB process shuts down forthwith without cleaning up - the equivalent of a :code:`kill -9` signal. This can result in structural database damage, because YottaDB does not have sufficient control of what happens in response to an immediate process termination to protect against database damage under all circumstances. In all cases, on receipt of a MUPIP STOP, a process will eventually terminate once it gets the resources needed to clean up. Use three MUPIP STOPs in a row only as a last resort, and when you do, perform a MUPIP INTEG at your earliest opportunity thereafter to check for database structural damage, and repair any damage following the procedures in `Chapter 11 (Maintaining Database Integrity) <./integrity.html>`_.You may never have to perform a MUPIP STOP if your application is designed in a way that it reduces or eliminates the probability of a process getting in the final try of a transaction. For more information, refer to the `Programmers Guide <../ProgrammersGuide/index.html>`_.

.. _mupip-trigger:

++++++++++++++++
TRIGGER
++++++++++++++++

Examines or loads trigger definitions. The format of the MUPIP TRIGGER command is:

.. code-block:: none

   TRIGGER {-STDIN|-TRIG[GERFILE]=<trigger_definitions_file>
   [-NOPR[OMPT]]|[-SELE[CT][=name-list|*][-STDOUT|<select-output-file>]|-UPGRADE}

Before you run the MUPIP TRIGGER command:

* Set the value of the environment variable ydb_gbldir: to specify the value of a current global directory.

* Ensure that the key size, record size, block size of your database is sufficient for storing all planned trigger definitions. You may have to set the key and record sizes larger than the database content would otherwise require.

The qualifiers of the MUPIP TRIGGER command are as follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-TRIGGERFILE=<trigger_definitions_file>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Loads a trigger definition file to the database. The format of the TRIGGERFILE qualifier is:

.. code-block:: none

   -TRIG[GERFILE]=<trigger_definitions_file> [-NOPR[OMPT]]

* For information on the syntax and usage of a trigger definition file, refer to the `Triggers <../ProgrammersGuide/triggers.html>`_ chapter and the `$ZTRIGGER() section in the Functions chapter of the Programmer's Guide <../ProgrammersGuide/functions.html#ztrigger-function>`_.

* A MUPIP TRIGGER TRIGGERFILE operation occurs within a transaction boundary, therefore, if even one trigger from the trigger definition file fails to parse correctly, MUPIP TRIGGER rolls back the entire trigger definition file load. Trigger maintenance operations reserve their output until the transaction commits at which time they deliver the entire output in a consistent way. MUPIP TRIGGER operations have an implicit timeout of zero (0), meaning the read must succeed on the first try or the command will act as if it received no input.

* MUPIP TRIGGER TRIGGERFILE ignores blank lines and extra whitespace within lines. It treats lines with a semi-colon in the first position as comments and ignores their content.

* If the NOPROMPT option is specified and the trigger definition file contains a :code:`-*` line, then YottaDB will delete all triggers without user confirmation.

* MUPIP TRIGGER compiles the XECUTE action string and rejects the load if the compilation has errors.

* Always specify the same value for the environment variable ydb_chset during loading and executing triggers. If you specify different values of ydb_chset during loading and executing triggers, MUPIP TRIGGER generates a run-time error (TRIGINVCHSET). YottaDB does not prevent a process from updating different nodes with triggers using a different character set, however, YottaDB prevents a process from updating the same triggering node with different character sets. Your coding practice, for all database updates, should be to ensure that you provide the same value for ydb_chset during load compilation and run-time compilation.

* MUPIP TRIGGER replicate trigger definitions as logical actions from an originating/primary instance to a replicating/secondary instance based on LGTRIG journal records. This permits the instances to have different sets of triggers and differing database layouts (for example, different # of regions, different block sizes, different maximum-record-size, and so on).

* MUPIP TRIGGER error messages associated with loading triggers limit trigger expression source lines to 80 characters including a trailing ellipsis to indicate there was more text, and they also replace any non-graphic characters with a dot (.)

* YottaDB triggers apply to spanning regions. When $ZTRIGGER() or MUPIP TRIGGER define triggers that apply to globals spanning multiple regions, each of the spanned regions install a definition.

* Incompatible with: SELECT

.. note::
   The trigger update summary reports count not only names and option changes as "modified" but also cases where a COMMANDS list changed, even though those are functionally additions or deletions of separate trigger definitions.

~~~~~~~~~~~~~~~~~~
-SELECT=name-list
~~~~~~~~~~~~~~~~~~

Provides a facility to examine the current trigger definition. SELECT produces a list of the current triggers for a comma-separate list of global variables or trigger names. The format of the SELECT qualifier is:

.. code-block:: none

   -SELE[CT][=name-list*][ <select-output-file>]

* Name-list can include global names, delimited with a leading caret (^), and/or trigger names (user-defined or auto-generated) with no leading caret. You can specify a trailing asterisk(*) with either.

* With no arguments specified, YottaDB treats SELECT as :code:`-select="*"` and extracts a list of all current triggers.

* Optionally, you can specify a file name to redirect the output of the command. If you do not specify a file name, MUPIP TRIGGER prompts for a file name. If you respond with an empty string (RETURN), MUPIP TRIGGER directs the output to STDOUT.

* MUPIP TRIGGER SELECT displays all output including errors on STDOUT.

* For Trigger definition reporting operations, $ZTRIGGER("SELECT") and MUPIP TRIGGER SELECT, return a non-zero exit status when their selection criteria encounter an error in the select.

* MUPIP TRIGGER SELECT works even if a multi-line XECUTE string does not terminate with a newline character. For more information on multi-line XECUTE strings, refer to the -xecute="\|<<strlit1"\|>> section under Trigger Definition File in the `Triggers chapter <../ProgrammersGuide/triggers.html>`_ and the `$ZTRIGGER() section in the Functions chapter of the Programmer's Guide <../ProgrammersGuide/functions.html#ztrigger-function>`_.

.. note::
   The output from the MUPIP TRIGGER SELECT command may not be identical to your trigger definition file. This is because YottaDB converts semantically identical syntax into a single internal representation; while SELECT output may not be identical to the TRIGGERFILE input, it has the same meaning. Additionally, MUPIP TRIGGER SELECT displays a field called "Cycle" as part of a comment. Cycle is the number of trigger definition updates (addition, modification, or deletion) performed on a global node. MUPIP TRIGGER treats the deletion of a non-existent trigger as a success; if that is the only operation, or one of a set of successful operations, it returns success 0 to the shell. Also, MUPIP TRIGGER returns failure in case of trigger selection using trigger names where the number after the pound-sign (#) starts with a 0 (which is an impossible auto-generated trigger name).

~~~~~~~~~
-UPGRADE
~~~~~~~~~

Upgrades older trigger definitions into current format.

The format of the UPGRADE qualifier is:

.. code-block:: none

   -UPGRADE

If YottaDB encounters an old trigger definition it produces a NEEDTRIGUPGRD message. To preserve the possibility of a straightforward downgrade to an earlier version, perform a select "*" action with MUPIP TRIGGER (or $ZTRIGGER() and save the result. Note that TRIGGER UPGRADE assumes that the existing trigger definitions are properly defined; if the prior release has produced defective triggers, delete them with a wild-card ("*"), and redefine the triggers in the new release. In the event of a downgrade, delete "*" all triggers before the downgrade and insert the saved version from before the upgrade. Attempting to perform a MUPIP TRIGGER UPGRADE on a database without write authorization to the database produces a TRIGMODREGNOTRW error. The UPGRADE qualifier is not compatible with any other MUPIP TRIGGER qualifier. Trigger upgrades from older versions may produce journal records based on the prior format that a MUPIP JOURNAL RECOVER cannot process correctly, therefore, YottaDB recommends you do them with journaling off, and start with a backup and fresh journal files after the trigger upgrade.

~~~~~~~~
-STDIN
~~~~~~~~

Reads input triggers to be set from stdin.

The format of the STDIN qualifier is:

.. code-block:: none

   -STDIN

STDIN is incompatible with any other option.

STDIN qualifier was added to YottaDB effective release `r1.32 <https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.32>`_.

~~~~~~~~~
-STDOUT
~~~~~~~~~

Reports on triggers from the database to stdout.

The format of the STDOUT qualifier is:

.. code-block:: none

   -STDOUT

STDOUT is only valid for SELECT and therefore is incompatible with the STDIN, TRIGGERFILE, and UPGRADE options.

STDOUT qualifier was added to YottaDB effective release `r1.32 <https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.32>`_.

~~~~~~~~~~~~~~~~~~~~~~~~~~
Examples for MUPIP TRIGGER
~~~~~~~~~~~~~~~~~~~~~~~~~~

This section provides step-by-step instructions for creating, modifying, and deleting triggers. Triggers affect all processes updating a database unlike, for example, environment variables such as $ydb_routines which work on a per process basis. Therefore, YottaDB recommends that you should always have carefully planned procedures for changing triggers in your production environment.

*To create a new trigger for global node ^Acct("ID")*:

Using your editor, create a trigger definition file called triggers.trg with the following entry:

.. code-block:: none

   +^Acct("ID") -name=ValidateAccount -commands=S -xecute="Write ""Hello Earth!"""

Execute a command like the following:

.. code-block:: bash

   $ mupip trigger -triggerfile=triggers.trg

This command adds a trigger for ^Acct("ID"). On successful trigger load, this command displays an output like the following:

.. code-block:: bash

   File triggers.trg, Line 1: ^Acct trigger added with index 1
   =========================================
   1 triggers added
   0 triggers deleted
   0 trigger file entries not changed
   0 triggers modified
   =========================================

Now, every S[et] operation on the global node ^Acct("ID") executes the trigger.

Execute a command like the following:

.. code-block:: bash

   $ mupip trigger -select="^Acct*"

This command displays the triggers. A sample output looks like the following:

.. code-block:: bash

   ;trigger name: ValidateAccount# cycle: 1
   +^Acct("ID") -name=ValidateAccount -commands=S -xecute="Write ""Hello Earth!"""

*To modify an existing trigger for global node ^Acct("ID")*:

You cannot directly replace an existing trigger definition with a new one. With the exception of NAME and OPTIONS, to change an existing trigger, you have to delete the existing trigger definition and then add the modified trigger definition as a new trigger. Note that YottaDB performs two different trigger comparisons to match trigger definitions depending on whether or not S[ET] is the trigger invocation command. If there is a S[ET], then the comparison is based on the global name and subscripts, PIECES, [Z]DELIM, and XECUTE. If there is no SET, YottaDB compares only the global node with subscripts and the XECUTE code value.

Begin by executing the following command:

.. code-block:: bash

   $ mupip trigger -select="^Acct*"Output file:

Specify trigger_mod.trg as the output file. This file contains entries like the following:

.. code-block:: bash

   ;trigger name: ValidateAccount# cycle: 1
   +^Acct("ID") -name=ValidateAccount -commands=S -xecute="Write ""Hello Earth!"""

Using your editor, open trigger_mod.trg and change + (plus) to - (minus) for the trigger definition entry for ValidateAccount and add a new trigger definition for ^Acct("ID"). To avoid inconsistent application behavior, it is important to replace an old trigger with a new one in the same transaction (Atomic). The trigger_mod.trg file should have entries like:

.. code-block:: bash

   ;trigger name: ValidateAccount# cycle: 1
   -^Acct("ID") -name=ValidateAccount -commands=Set -xecute="Write ""Hello Earth!"""
   ;trigger name: ValidateAccount#
   +^Acct("ID") -name=ValidateAccount -commands=Set -xecute="Write ""Hello Mars!"""

Execute a command like the following:

.. code-block:: bash

   $ mupip trigger -triggerfile=trigger_mod.trg

This command displays an output like the following:

.. code-block:: bash

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

.. code-block:: bash

   $ mupip trigger -select="^Acct*"Output file:

Specify trigger_delete.trg as the output file. This file contains entries like the following:

.. code-block:: bash

   ;trigger name: ValidateAccount# cycle: 3
   +^Acct("ID") -name=ValidateAccount -commands=S -xecute="Write ""Hello Mars!"""

Using your editor, change + (plus) to - (minus) for the trigger definition entry for ValidateAccount. Alternatively, you can create a file with an entry like -ValidateAccount.

Now, execute a command like the following:

.. code-block:: bash

   $ mupip trigger -triggerfile=trigger_delete.trg

This command displays an output like the following:

.. code-block:: bash

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

.. code-block:: bash

   +^Acct("ID") -name=ValidateAcct -commands=S -xecute="Write ""Hello Mars!"""

Verify that the ValidateAccount trigger exists by executing the following command:

.. code-block:: bash

   $ mupip trigger -select="^Acct*"Output file:

Respond with an empty string (Press Enter). Confirm that the trigger summary report contains an entry like the following:

.. code-block:: bash

   ;trigger name: ValidateAccount# cycle: 3
   +^Acct("ID") -name=ValidateAccount -commands=S -xecute="Write ""Hello Mars!"""

Now, execute a command like the following:

.. code-block:: bash

   $ mupip trigger -triggerfile=trigger_rename.trg

This command displays an output like the following:

.. code-block:: bash

   =========================================
   0 triggers added
   0 triggers deleted
   0 trigger file entries not changed
   1 triggers modified
   =========================================

You have successfully changed the trigger name ValidateAccount to ValidateAcct.

.. _mupip-upgrade:

+++++++++
UPGRADE
+++++++++

Upgrades the file-header of a database. The format of the MUPIP UPGRADE command is:

.. code-block:: none

   UP[GRADE]

* It increases the size from 4 bytes to 8 bytes of file-header fields such as current transaction number (CTN), maximum TN and others that contain transaction numbers.
* It resets the various trace counters and changes the database format to the most recent version. This change does not upgrade the individual database blocks but sets the database format flag to the most recent version.
* It also initializes a counter of the current blocks that are still in the previous version format. It decrements this counter each time an older version format block is converted to the new format. When the counter is 0, the entire database gets converted.

~~~~~~~~~~~~~~~~~~~~~~~~~
Example for MUPIP UPGRADE
~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: bash

   $ mupip upgrade yottadb.dat

This example upgrades the file-header of yottadb.dat to the latest version format.

-----------------------------
MUPIP Command Summary
-----------------------------

+--------------------------------------+---------------------------------------------+------------------------------------------------------------------------------------------+-------------------+
| Command                              | Objects                                     | Main Qualifier                                                                           | Standalone Access |
+======================================+=============================================+==========================================================================================+===================+
| B[ACKUP]                             | region-name file-name                       | * BK[UPDBJNL]=DISABLE | OFF                                                              | N                 |
|                                      |                                             | * BY[TESTREAM] NET[TIMEOUT]=seconds                                                      | N                 |
|                                      |                                             | * C[OMPREHENSIVE]                                                                        | N                 |
|                                      |                                             | * DA[TABASE] REPLA[CE]                                                                   | N                 |
|                                      |                                             | * DBG                                                                                    | N                 |
|                                      |                                             | * I[NCREMENTAL]                                                                          | N                 |
|                                      |                                             | * [NO]J[OURNAL][=journal-options-list]                                                   | N                 |
|                                      |                                             | * NETTIMEOUT                                                                             | N                 |
|                                      |                                             | * [NO]NEWJNLFILES[=[NO]PREVLINK],[NO]S[YNC_IO]                                           | N                 |
|                                      |                                             | * O[NLINE]                                                                               | N                 |
|                                      |                                             | * RECORD                                                                                 | N                 |
|                                      |                                             | * REPLI[NSTANCE]=OFF | ON                                                                | N                 |
|                                      |                                             | * S[INCE]={DATABASE|BYTESTREAM|RECORD}                                                   | N                 |
|                                      |                                             | * T[RANSACTION=hexa;transaction_number]                                                  | N                 |
+--------------------------------------+---------------------------------------------+------------------------------------------------------------------------------------------+-------------------+
| CR[EATE]                             | \-                                          | * R[EGION]=region-name                                                                   | N.A.              |
+--------------------------------------+---------------------------------------------+------------------------------------------------------------------------------------------+-------------------+
| DO[WNGRADE]                          | file-name                                   | * V[ERSION]={V4|V5}                                                                      | Y                 |
+--------------------------------------+---------------------------------------------+------------------------------------------------------------------------------------------+-------------------+
| DU[MPFHEAD]                          | file-name or region-list                    | * \-                                                                                     | N                 |
+--------------------------------------+---------------------------------------------+------------------------------------------------------------------------------------------+-------------------+
| EN[DIANCVT]                          | file-name                                   | * OUTDB=<outdb-file>                                                                     | Y                 |
|                                      |                                             | * OV[ERRIDE]                                                                             | Y                 |
+--------------------------------------+---------------------------------------------+------------------------------------------------------------------------------------------+-------------------+
| EXI[T]                               | \-                                          | * \-                                                                                     | N.A.              |
+--------------------------------------+---------------------------------------------+------------------------------------------------------------------------------------------+-------------------+
| EXTE[ND]                             | region-name                                 | * B[LOCKS]=blocks                                                                        | N                 |
+--------------------------------------+---------------------------------------------+------------------------------------------------------------------------------------------+-------------------+
| EXTR[ACT]                            | \-                                          | * FO[RMAT]=GO|B[INARY]|Z[WR]                                                             | N                 |
|                                      |                                             | * FR[EEZE]                                                                               | N                 |
|                                      |                                             | * LA[BEL]=text                                                                           | N                 |
|                                      |                                             | * [NO]L[OG]                                                                              | N                 |
|                                      |                                             | * NU[LL_IV]                                                                              | N                 |
|                                      |                                             | * S[ELECT]=global-name-list                                                              | N                 |
|                                      |                                             | * O[CHSET]=character-set                                                                 | N                 |
|                                      |                                             | * R[EGION]=region-list                                                                   | N                 |
+--------------------------------------+---------------------------------------------+------------------------------------------------------------------------------------------+-------------------+
| F[REEZE]                             | region-list                                 | * DBG                                                                                    | N                 |
|                                      |                                             | * OF[F] [OV[ERRIDE]]                                                                     | N                 |
|                                      |                                             | * ON [[NO]ONL[INE] [[NO]A[UTORELEASE]] [R[ECORD]]]                                       | N                 |
+--------------------------------------+---------------------------------------------+------------------------------------------------------------------------------------------+-------------------+
| FT[OK]                               | File-name                                   | * D[B]                                                                                   | N                 |
|                                      |                                             | * J[NLPOOL]                                                                              | N                 |
|                                      |                                             | * R[ECVPOOL]                                                                             | N                 |
+--------------------------------------+---------------------------------------------+------------------------------------------------------------------------------------------+-------------------+
| H[ELP]                               | command-option                              | * \-                                                                                     | N                 |
+--------------------------------------+---------------------------------------------+------------------------------------------------------------------------------------------+-------------------+
| I[NTEG]                              | File-name or region-list                    | * A[DJACENCY]=integer                                                                    | N                 |
|                                      |                                             | * BL[OCK]=hexa;block-number                                                              | N                 |
|                                      |                                             | * BR[IEF]                                                                                | N                 |
|                                      |                                             | * FA[ST]                                                                                 | N                 |
|                                      |                                             | * FI[LE]                                                                                 | Y                 |
|                                      |                                             | * FU[LL]                                                                                 | N                 |
|                                      |                                             | * [NO]K[EYRANGES]                                                                        | N                 |
|                                      |                                             | * [NO][MAP]=integer                                                                      | N                 |
|                                      |                                             | * [NO]MAXK[EYSIZE]=integer                                                               | N                 |
|                                      |                                             | * R[EGION]                                                                               | N                 |
|                                      |                                             | * [NO]ST[ATS]                                                                            | N                 |
|                                      |                                             | * SU[BSCRIPT]=subscript                                                                  | N                 |
|                                      |                                             | * TN[_RESET]                                                                             | Y                 |
|                                      |                                             | * [NO]TR[ANSACTION][=integer]                                                            | N                 |
+--------------------------------------+---------------------------------------------+------------------------------------------------------------------------------------------+-------------------+
| IN[TRPT]                             | process id                                  |                                                                                          | N.A.              |
+--------------------------------------+---------------------------------------------+------------------------------------------------------------------------------------------+-------------------+
| J[OURNAL]                            | file-name                                   | * EX[TRACT][=file-specification|-stdout]                                                 | N                 |
|                                      |                                             | * REC[OVER] | RO[LLBACK]                                                                 | N                 |
|                                      |                                             | * SH[OW][=show-option-list]                                                              | N                 |
|                                      |                                             | * [NO]V[ERIFY]                                                                           | N                 |
|                                      |                                             | * BA[CKWARD] | FO[RWARD]                                                                 | N                 |
+--------------------------------------+---------------------------------------------+------------------------------------------------------------------------------------------+-------------------+
| L[OAD]                               | file-name                                   | * BE[GIN]=integer                                                                        | N                 |
|                                      |                                             | * BLOCK_DENSITY                                                                          | N                 |
|                                      |                                             | * E[ND]=integer                                                                          | N                 |
|                                      |                                             | * FI[LLFACTOR]=integer                                                                   | N                 |
|                                      |                                             | * FO[RMAT]=GO|B[INARY]|Z[WR]                                                             | N                 |
|                                      |                                             | * S[TDIN]                                                                                | N                 |
|                                      |                                             | * O[NERROR]                                                                              | N                 |
+--------------------------------------+---------------------------------------------+------------------------------------------------------------------------------------------+-------------------+
| REO[RG]                              | \-                                          | * D[OWNGRADE]                                                                            | N                 |
|                                      |                                             | * ENCR[YPT]=key                                                                          | N                 |
|                                      |                                             | * E[XCLUDE]=global-name-list                                                             | N                 |
|                                      |                                             | * FI[LL_FACTOR]=integer                                                                  | N                 |
|                                      |                                             | * I[NDEX_FILL_FACTOR]=integer                                                            | N                 |
|                                      |                                             | * NOCO[ALESCE]                                                                           | N                 |
|                                      |                                             | * NOSP[LIT]                                                                              | N                 |
|                                      |                                             | * NOSW[AP]                                                                               | N                 |
|                                      |                                             | * REG[ION]                                                                               | N                 |
|                                      |                                             | * R[ESUME]                                                                               | N                 |
|                                      |                                             | * S[ELECT]=global-name-list                                                              | N                 |
|                                      |                                             | * T[RUNCATE][=percentage]                                                                | N                 |
|                                      |                                             | * UP[GRADE]                                                                              | N                 |
|                                      |                                             | * REG[ION] region-list                                                                   | N                 |
+--------------------------------------+---------------------------------------------+------------------------------------------------------------------------------------------+-------------------+
| REP[LICATE]                          | file-name                                   | * E[DITINSTANCE]                                                                         | N                 |
|                                      |                                             | * I[NSTANCE_CREATE]                                                                      | N                 |
|                                      |                                             | * R[ECEIVER]                                                                             | N                 |
|                                      |                                             | * S[OURCE]                                                                               | N                 |
|                                      |                                             | * UPDA[TEPROC]                                                                           | N                 |
+--------------------------------------+---------------------------------------------+------------------------------------------------------------------------------------------+-------------------+
| RE[STORE]                            | file-name or file-list                      | * [NO]E[XTEND]                                                                           | Y                 |
+--------------------------------------+---------------------------------------------+------------------------------------------------------------------------------------------+-------------------+
| RU[NDOWN]                            | file-name or region-name                    | * F[ILE]                                                                                 | Y                 |
|                                      |                                             | * R[EGION]                                                                               | N                 |
|                                      |                                             | * RELINKCTL [dir]                                                                        | N                 |
|                                      |                                             | * OVERRIDE                                                                               | N                 |
+--------------------------------------+---------------------------------------------+------------------------------------------------------------------------------------------+-------------------+
| SE[T]                                | file-name or region-name                    | * SE[T] {FI[LE] file-name|JN[LFILE] journal-file-name|REG[ION] region-list|              | N                 |
|                                      |                                             |   \REP[LICATION]={ON|OFF}}                                                               | N                 |
|                                      |                                             | * AC[CESS_METHOD]={BG|MM}                                                                | Y                 |
|                                      |                                             | * [NO]AS[YNCIO]                                                                          | Y                 |
|                                      |                                             | * [NO]DE[FER_TIME][=seconds]                                                             | Y                 |
|                                      |                                             | * [NO]DEFER_ALLOCATE                                                                     | N                 |
|                                      |                                             | * [NO]EP[OCHTAPER]                                                                       | N                 |
|                                      |                                             | * [NO]ENCRYPTABLE                                                                        | Y                 |
|                                      |                                             | * E[XTENSION_COUNT]=integer(no of blocks)                                                | N                 |
|                                      |                                             | * F[LUSH_TIME]=integer                                                                   | N                 |
|                                      |                                             | * F[ULLBLKWRT]={0|1|2}                                                                   | Y                 |
|                                      |                                             | * G[LOBAL_BUFFERS]=integer                                                               | Y                 |
|                                      |                                             | * H[ARD_SPIN_COUNT]=integer                                                              | N                 |
|                                      |                                             | * [NO]INST[_FREEZE_ON_ERROR]                                                             | N                 |
|                                      |                                             | * JN[LFILE] journal-file-name                                                            | Y                 |
|                                      |                                             | * K[EY_SIZE]=bytes                                                                       | Y                 |
|                                      |                                             | * [NO]LCK_SHARES_DB_CRIT                                                                 | Y                 |
|                                      |                                             | * L[OCK_SPACE]=integer                                                                   | Y                 |
|                                      |                                             | * M[UTEX_SLOTS]=integer                                                                  | Y                 |
|                                      |                                             | * N[ULL_SUBSCRIPTS]=value                                                                | Y                 |
|                                      |                                             | * PA[RTIAL_RECOV_BYPASS]                                                                 | Y                 |
|                                      |                                             | * [NO]Q[DBRUNDOWN]                                                                       | Y                 |
|                                      |                                             | * [NO]REA[D_ONLY]                                                                        | Y                 |
|                                      |                                             | * REC[ORD_SIZE]=bytes                                                                    | Y                 |
|                                      |                                             | * REG[ION] region-list                                                                   | N                 |
|                                      |                                             | * REP[LICATION]={ON|OFF}                                                                 | Y                 |
|                                      |                                             | * RES[ERVED_BYTES]=integer                                                               | Y                 |
|                                      |                                             | * SLEE[P_SPIN_COUNT]=integer                                                             | N                 |
|                                      |                                             | * SPIN[_SLEEP_MASK]=hexa_mask                                                            | N                 |
|                                      |                                             | * STAN[DALONENOT]                                                                        | Y                 |
|                                      |                                             | * [NO]STD[NULLCOLL]                                                                      | Y                 |
|                                      |                                             | * [NO]STAT[S]                                                                            | Y                 |
|                                      |                                             | * V[ERSION]={V4|V6}                                                                      | N                 |
|                                      |                                             | * W[AIT_DISK]=integer                                                                    | Y                 |
+--------------------------------------+---------------------------------------------+------------------------------------------------------------------------------------------+-------------------+
| SI[ZE]                               | global-name-list region-list                | * H[EURISTIC]=estimation_technique                                                       | N                 |
|                                      |                                             | * S[ELECT]=global-name-list                                                              | N                 |
|                                      |                                             | * R[EGION]=region-list                                                                   | N                 |
+--------------------------------------+---------------------------------------------+------------------------------------------------------------------------------------------+-------------------+
| ST[OP]                               | process-id                                  | * process-id                                                                             | N.A.              |
+--------------------------------------+---------------------------------------------+------------------------------------------------------------------------------------------+-------------------+
| TRIGGER                              | \-                                          | * TRIG[GERFILE]=<trigger_definitions_file>                                               | N                 |
|                                      |                                             | * NOPR[OMPT]                                                                             | N                 |
|                                      |                                             | * SELE[CT][=name-list|*][<select-output-file>]                                           | N                 |
|                                      |                                             | * UPGRADE                                                                                | N                 |
+--------------------------------------+---------------------------------------------+------------------------------------------------------------------------------------------+-------------------+
| UP[GRADE]                            | file-name                                   | * \-                                                                                     | Y                 |
+--------------------------------------+---------------------------------------------+------------------------------------------------------------------------------------------+-------------------+

The following table summarizes the qualifiers.

+----------------------------------------------+------------------------------------------------------------------------------+----------------------------------------------------------------------+
| Main Qualifier                               |  MUPIP Command                                                               | Options/Qualifiers                                                   |
+==============================================+==============================================================================+======================================================================+
| EDITINSTANCE                                 | :ref:`mupip-replicate`                                                       | * CHANGE                                                             |
|                                              |                                                                              | * DETAIL                                                             |
|                                              |                                                                              | * OFFSET=hexa                                                        |
|                                              |                                                                              | * VALUE=hexa                                                         |
|                                              |                                                                              | * SIZE=hexa                                                          |
|                                              |                                                                              | * [NO]QDBRUNDOWN                                                     |
+----------------------------------------------+------------------------------------------------------------------------------+----------------------------------------------------------------------+
| FENCES=<fence-options-list>                  | :ref:`mupip-journal`                                                         | * ALWAYS                                                             |
|                                              |                                                                              | * NONE                                                               |
|                                              | -RECOVER                                                                     | * PROCESS                                                            |
|                                              |                                                                              |                                                                      |
|                                              | -ROLLBACK                                                                    |                                                                      |
+----------------------------------------------+------------------------------------------------------------------------------+----------------------------------------------------------------------+
| OFF                                          | :ref:`mupip-freeze`                                                          | * OVERRIDE                                                           |
|                                              |                                                                              | * RECORD                                                             |
+----------------------------------------------+------------------------------------------------------------------------------+----------------------------------------------------------------------+
| ON                                           | :ref:`mupip-freeze`                                                          | * [NO]ONLINE                                                         |
|                                              |                                                                              | * [NO]AUTORELEASE                                                    |
+----------------------------------------------+------------------------------------------------------------------------------+----------------------------------------------------------------------+
| INSTANCE_CREATE                              | :ref:`mupip-replicate`                                                       | * NAME                                                               |
|                                              |                                                                              | * NOREPLACE                                                          |
|                                              |                                                                              | * SUPPLEMENTARY                                                      |
|                                              |                                                                              | * [NO]QDBRUNDOWN                                                     |
+----------------------------------------------+------------------------------------------------------------------------------+----------------------------------------------------------------------+
| JOURNAL=<journal-options-list>               | :ref:`mupip-backup` and                                                      | * ALIGNSIZE=integer                                                  |
|                                              | :ref:`mupip-set`                                                             | * ALLOCATION=integer                                                 |
|                                              |                                                                              | * AUTOSWITCHLIMIT=integer                                            |
|                                              |                                                                              | * BEFORE_IMAGES                                                      |
|                                              |                                                                              | * BUFFER_SIZE=integer                                                |
|                                              |                                                                              | * DISABLE                                                            |
|                                              |                                                                              | * ENABLE                                                             |
|                                              |                                                                              | * EPOCH_INTERVAL=integer                                             |
|                                              |                                                                              | * EXTENSION=integer                                                  |
|                                              |                                                                              | * FILENAME=file_name                                                 |
|                                              |                                                                              | * OFF                                                                |
|                                              |                                                                              | * ON                                                                 |
|                                              |                                                                              | * SYNC_IO                                                            |
|                                              |                                                                              | * YIELD_LIMIT=integer                                                |
+----------------------------------------------+------------------------------------------------------------------------------+----------------------------------------------------------------------+
| LOOKBACK_LIMIT=lookback-option-list          | -RECOVER                                                                     | * TIME="time"                                                        |
|                                              |                                                                              |                                                                      |
|                                              | -ROLLBACK                                                                    | * OPERATIONS=integer                                                 |
+----------------------------------------------+------------------------------------------------------------------------------+----------------------------------------------------------------------+
| RECEIVER                                     | :ref:`mupip-replicate`                                                       | * BUFFSIZE=integer                                                   |
|                                              |                                                                              | * CHANGELOG                                                          |
|                                              |                                                                              | * CHECKHEALTH                                                        |
|                                              |                                                                              | * CMPLVL=integer                                                     |
|                                              |                                                                              | * FILTER=filter_name                                                 |
|                                              |                                                                              | * he[lpers]=[m[,n]]                                                  |
|                                              |                                                                              | * INITIALIZE                                                         |
|                                              |                                                                              | * CMPLVL=n                                                           |
|                                              |                                                                              | * LISTENPORT=integer                                                 |
|                                              |                                                                              | * LOG=logfile                                                        |
|                                              |                                                                              | * LOG_INTERVAL=integer                                               |
|                                              |                                                                              | * NORESYNC                                                           |
|                                              |                                                                              | * RESUME=strm_num                                                    |
|                                              |                                                                              | * REUSE=instname                                                     |
|                                              |                                                                              | * SHOWBACKLOG                                                        |
|                                              |                                                                              | * SHUTDOWN                                                           |
|                                              |                                                                              | * START                                                              |
|                                              |                                                                              | * STATSLOG=[ON|OFF]                                                  |
|                                              |                                                                              | * STOPSOURCEFILTER                                                   |
|                                              |                                                                              | * TIMEOUT=seconds                                                    |
|                                              |                                                                              | * TLSID=label                                                        |
|                                              |                                                                              | * UPDATEONLY                                                         |
|                                              |                                                                              | * UPDATERESYNC=/path/to/bkup-orig-inst                               |
+----------------------------------------------+------------------------------------------------------------------------------+----------------------------------------------------------------------+
| RECOVER                                      | :ref:`mupip-journal`                                                         | * AFTER=time                                                         |
|                                              |                                                                              | * APPLY_AFTER_IMAGE                                                  |
|                                              |                                                                              | * BACKWARD                                                           |
|                                              |                                                                              | * BEFORE=time                                                        |
|                                              |                                                                              | * [NO]BROKENTRANS=file                                               |
|                                              |                                                                              | * CHAIN                                                              |
|                                              |                                                                              | * CHECKTN                                                            |
|                                              |                                                                              | * [NO]ER[ROR_LIMIT][=integer]                                        |
|                                              |                                                                              | * FENCES=fence-option-list                                           |
|                                              |                                                                              | * FORWARD                                                            |
|                                              |                                                                              | * FULL                                                               |
|                                              |                                                                              | * GLOBAL=<global_list>                                               |
|                                              |                                                                              | * ID=<pid_list>                                                      |
|                                              |                                                                              | * INTERACTIVE                                                        |
|                                              |                                                                              | * LOOKBACK_LIMIT=<lookback_limit_options>                            |
|                                              |                                                                              | * [NO]LOSTTRANS[=file]                                               |
|                                              |                                                                              | * RED[IRECT]=file-pair-list                                          |
|                                              |                                                                              | * SINCE=time                                                         |
|                                              |                                                                              | * VERBOSE                                                            |
|                                              |                                                                              | * VERIFY                                                             |
+----------------------------------------------+------------------------------------------------------------------------------+----------------------------------------------------------------------+
| EXTRACT                                      | :ref:`mupip-journal`                                                         | * AFTER=time                                                         |
|                                              |                                                                              | * BEFORE=time                                                        |
|                                              |                                                                              | * [NO]BROKENTRANS=file                                               |
|                                              |                                                                              | * CHAIN                                                              |
|                                              |                                                                              | * CHECKTN                                                            |
|                                              |                                                                              | * [NO]ER[ROR_LIMIT]=integer]                                         |
|                                              |                                                                              | * FENCES=fence-option-list                                           |
|                                              |                                                                              | * FULL                                                               |
|                                              |                                                                              | * GLOBAL=<global_list>                                               |
|                                              |                                                                              | * ID=<pid_list>                                                      |
|                                              |                                                                              | * INTERACTIVE                                                        |
|                                              |                                                                              | * LOOKBACK_LIMIT=<lookback_limit_options>                            |
|                                              |                                                                              | * [NO]LOSTTRANS[=file]                                               |
|                                              |                                                                              | * REGION                                                             |
|                                              |                                                                              | * SINCE=time                                                         |
|                                              |                                                                              | * VERBOSE                                                            |
|                                              |                                                                              | * VERIFY                                                             |
+----------------------------------------------+------------------------------------------------------------------------------+----------------------------------------------------------------------+
| ROLLBACK                                     | :ref:`mupip-journal`                                                         | * APPLY_AFTER_IMAGE                                                  |
|                                              |                                                                              | * BACKWARD                                                           |
|                                              |                                                                              | * BEFORE=time                                                        |
|                                              |                                                                              | * [NO]BROKENTRANS=file                                               |
|                                              |                                                                              | * [NO]ER[ROR_LIMIT][=integer]                                        |
|                                              |                                                                              | * FENCES=fence-option-list                                           |
|                                              |                                                                              | * FETCHRESYNC                                                        |
|                                              |                                                                              | * LOOKBACK_LIMIT=<lookback_limit_options>                            |
|                                              |                                                                              | * [NO]LOSTTRANS[=file]                                               |
|                                              |                                                                              | * RES[YNC]=hexa;journal_sequence_number                              |
|                                              |                                                                              | * VERBOSE                                                            |
|                                              |                                                                              | * VERIFY                                                             |
+----------------------------------------------+------------------------------------------------------------------------------+----------------------------------------------------------------------+
| SHOW=<show-option-list>                      | :ref:`mupip-journal`                                                         | * ACTIVE_PROCESSES                                                   |
|                                              |                                                                              | * ALL                                                                |
|                                              |                                                                              | * BROKEN_TRANSACTIONS                                                |
|                                              |                                                                              | * HEADER                                                             |
|                                              |                                                                              | * PROCESSES                                                          |
|                                              |                                                                              | * STATISTICS                                                         |
|                                              |                                                                              | * AFTER=time                                                         |
|                                              |                                                                              | * USER=user-list                                                     |
|                                              |                                                                              | * TRANSACTION=[KILL|SET]                                             |
|                                              |                                                                              | * INTERACTIVE                                                        |
|                                              |                                                                              | * GLOBAL=<global_list>                                               |
|                                              |                                                                              | * ID=<pid_list>                                                      |
|                                              |                                                                              | * INTERACTIVE                                                        |
+----------------------------------------------+------------------------------------------------------------------------------+----------------------------------------------------------------------+
| SINCE                                        | :ref:`mupip-backup`                                                          | * BYTESTREAM                                                         |
|                                              |                                                                              | * COMPREHENSIVE                                                      |
|                                              |                                                                              | * DATABASE                                                           |
|                                              |                                                                              | * INCREMENTAL                                                        |
|                                              |                                                                              | * RECORD                                                             |
+----------------------------------------------+------------------------------------------------------------------------------+----------------------------------------------------------------------+
| SO[URCE]                                     | :ref:`mupip-replicate`                                                       | * ACTIVATE                                                           |
|                                              |                                                                              | * BUFFSIZE=Buffer_size                                               |
|                                              |                                                                              | * CHANGELOG                                                          |
|                                              |                                                                              | * CHECKHEALTH                                                        |
|                                              |                                                                              | * CMPLVL=integer                                                     |
|                                              |                                                                              | * CONNECTPARAMS=connection_options                                   |
|                                              |                                                                              | * DEACTIVATE                                                         |
|                                              |                                                                              | * DETAIL                                                             |
|                                              |                                                                              | * FILTER=filter_name                                                 |
|                                              |                                                                              | * FREEZE=on/off                                                      |
|                                              |                                                                              | * [NO]COMMENT=string                                                 |
|                                              |                                                                              | * INSTSECONDARY=secondary_instance name                              |
|                                              |                                                                              | * NOJNLFILEONLY                                                      |
|                                              |                                                                              | * JNLPOOL-LOG=log_file                                               |
|                                              |                                                                              | * LOG_INTERVAL=integer                                               |
|                                              |                                                                              | * LOSTTNCOMPLETE                                                     |
|                                              |                                                                              | * NEEDRESTART                                                        |
|                                              |                                                                              | * PASSIVE                                                            |
|                                              |                                                                              | * [NO]PLAINTEXTFALLBACK                                              |
|                                              |                                                                              | * PROPAGATEPRIMARY                                                   |
|                                              |                                                                              | * RENEGOTIATE_INTERVAL=minutes                                       |
|                                              |                                                                              | * ROOTPRIMARY                                                        |
|                                              |                                                                              | * SECONDARY=secondary_instance_name                                  |
|                                              |                                                                              | * SHOWBACKLOG                                                        |
|                                              |                                                                              | * SHUTDOWN                                                           |
|                                              |                                                                              | * START                                                              |
|                                              |                                                                              | * STATSLOG                                                           |
|                                              |                                                                              | * STOPSOURCEFILTER                                                   |
|                                              |                                                                              | * TIMEOUT=seconds                                                    |
|                                              |                                                                              | * TLSID=label                                                        |
|                                              |                                                                              | * UPDOK                                                              |
|                                              |                                                                              | * UPDNOTOK                                                           |
|                                              |                                                                              | * ZEROBACKLOG                                                        |
+----------------------------------------------+------------------------------------------------------------------------------+----------------------------------------------------------------------+
| VERSION={V4|V5}                              | :ref:`mupip-downgrade`                                                       | * file-name                                                          |
|                                              | and :ref:`mupip-upgrade`                                                     |                                                                      |
+----------------------------------------------+------------------------------------------------------------------------------+----------------------------------------------------------------------+
