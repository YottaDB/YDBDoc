.. ################################################################
.. #                                                              #
.. # Copyright (c) 2021-2025 YottaDB LLC and/or its subsidiaries. #
.. # All rights reserved.                                         #
.. #                                                              #
.. #     This source code contains the intellectual property      #
.. #     of its copyright holder(s), and is made available        #
.. #     under a license.  If you do not know the terms of        #
.. #     the license, please stop and do not read further.        #
.. #                                                              #
.. ################################################################

Performs an integrity check on a YottaDB database file. You can perform structural integrity checks on one or more regions in the current Global Directory without bringing down (suspending database updates) your application. However, a MUPIP INTEG on a single file database requires standalone access but does not need a Global Directory. The order in which the MUPIP INTEG command selects database regions is a function of the file system layout and may vary as files are moved or created. Execute MUPIP INTEG operations one database file at a time to generate an report where the output always lists database files in a predictable sequence. For example, to compare output with a reference file, run INTEG on one file at a time.

Always use MUPIP INTEG in the following conditions:

* Periodically - to ensure ongoing integrity of the database(s); regular INTEGs help detect any integrity problems before they spread and extensively damage the database file.

* After a crash - to ensure the database was not corrupted. (Note: When using before-image journaling, when the database is recovered from the journal file after a crash, an integ is not required).

* When database errors are reported - to troubleshoot the problem.

Improving the logical and physical adjacency of global nodes may result in faster disk I/O. A global node is logically adjacent when it is stored within a span of contiguous serial block numbers. A global node is physically adjacent when it resides on adjacent hard disk sectors in a way that a single seek operation can access it. Database updates (SETs/KILLs) over time affect the logical adjacency of global nodes. A MUPIP INTEG reports the logical adjacency of your global nodes which may indicate whether a MUPIP REORG could improve the database performance. A native file system defragmentation improves physical adjacency.

.. note::
   Most modern SAN and I/O devices often mask the performance impact of the adjustments in logical and physical adjacency. If achieving a particular performance benchmark is your goal, increasing the logical and physical adjacency should be only one of many steps that you might undertake. While designing the database, try to ensure that the logical adjacency is close to the number of blocks that can physically reside on your hard disk's cylinder. You can also choose two or three cylinders, with the assumption that short seeks are fast.

The format of the MUPIP INTEG command is:

.. code-block:: none

   I[NTEG]
   [
    [-]-A[DJACENCY]=integer
    [-]-BL[OCK]=hexa;block-number
    [-]-BR[IEF]
    [-]-DBG
    [-]-DMAX[BLOCKSIZE][=integer]
    [-]-FA[ST]
    [-]-FU[LL]
    [-]-IMAX[BLOCKSIZE][=integer]
    [-]-[NO]K[EYRANGES]
    [-]-[NO]MAP[=integer]
    [-]-[NO]MAXK[EYSIZE][=integer]
    [-]-[NO]O[NLINE]
    [-]-S[UBSCRIPT]=subscript]
    [-]-TN[_RESET]
    [-]-[NO]TR[ANSACTION][=integer]
    [-]-VERBOSE
   ]
   {[[-]-FILE] file-name | [-]-REG[ION] region-list}



* MUPIP INTEG requires specification of either file(s) or region(s).

* Press <CTRL-C> to stop MUPIP INTEG before the process completes.

* The file-name identifies the database file for a MUPIP INTEG operation. The region-list identifies one or more regions that, in turn, identify database files through the current Global Directory.

* MUPIP INTEG operation keeps track of the number of blocks that do not have the current block version during a non-fast integ (default or full) and matches this value against the blocks to upgrade counters in the file-header. It issues an error if the values are unmatched and corrects the count in the file header if there are no other integrity errors.

.. note::
   Promptly analyze and fix all errors that MUPIP INTEG reports. Some errors may be benign while others may be signs of corruption or compromised database integrity. If operations continue without fixes to serious errors, the following problems may occur: Invalid application operation due to missing or incorrect data, Process errors (including inappropriate indefinite looping when a database access encounters an error), and degrading application level consistency as a result of incomplete update sequences caused by pre-existing database integrity issues.

YottaDB strongly recommends fixing the following errors as soon as they are discovered:

* Blocks incorrectly marked free - these may cause accelerating damage when processes make updates to any part of the database region.

* Integrity errors in an index block - these may cause accelerating damage when processes make updates to that area of the database region using the faulty index. For more information, refer to `Chapter 11: "Maintaining Database Integrity" <./integrity.html>`_.

MUPIP INTEG FAST and the "regular" INTEG both report these errors (These qualifiers are described later in this section). Other database errors do not pose the threat of rapidly spreading problems in GDS files. After the YottaDB database repair, assess the type of damage, the risk of continued operations, and the disruption in normal operation caused by the time spent repairing the database. For information on analyzing and correcting database errors, refer to `Chapter 11: "Maintaining Database Integrity" <./integrity.html>`_. Contact your YottaDB support channel for help assessing INTEG errors.

~~~~~~~~~~~~~~~~~
INTEG Qualifiers
~~~~~~~~~~~~~~~~~

The following sections describe the qualifiers of the INTEG command.

^^^^^^^^^^^^^
[-]-ADJACENCY
^^^^^^^^^^^^^

Specifies the logical adjacency of data blocks that MUPIP INTEG should assume while diagnosing the database. By default, MUPIP INTEG operates with ADJACENCY=10 and reports the logical adjacency in the "Adjacent" column of the MUPIP INTEG report.

* The complexity of contemporary disk controllers and the native file system may render this report superfluous. But when it is meaningful, this report measures the logical adjacency of data.

* A MUPIP REORG improves logical adjacency and a native file system defragmentation improves physical adjacency.

The format of the ADJACENCY qualifier is:

.. code-block:: none

   [-]-AD[JACENCY]=integer

^^^^^^^^^
[-]-BLOCK
^^^^^^^^^

Specifies the block for MUPIP INTEG command to start checking a sub-tree of the database. MUPIP INTEG BLOCK cannot detect "incorrectly marked busy errors".

The format of the BLOCK qualifier is:

.. code-block:: none

   [-]-BL[OCK]=block-number

* Block numbers are displayed in an INTEG error report or by using DSE.

* Incompatible with: SUBSCRIPT and TN_RESET

^^^^^^^^^
[-]-BRIEF
^^^^^^^^^

Displays a single summary report by database file of the total number of directory, index and data blocks. The format of the BRIEF qualifier is:

.. code-block:: none

   [-]-BR[IEF]

* By default, MUPIP INTEG uses the BRIEF qualifier.

* Incompatible with: FULL

.. _integ-dbg:

^^^^^^^
[-]-DBG
^^^^^^^

Produces verbose output to help with debugging. We recommend using :ref:`VERBOSE <integ-verbose>` instead.

^^^^^^^^^^^^^^^^
[-]DMAXBLOCKSIZE
^^^^^^^^^^^^^^^^

Specifies the maximum actual size of a data-level block in the database; any data block which contains data in excess of this size causes INTEG to report the block number, its path, and an error. The format of the DMAXBLOCKSIZE qualifier is:

.. code-block:: none

   [-]-DMAX[BLOCKSIZE]=integer

.. _integ-fast:

^^^^^^^^
[-]-FAST
^^^^^^^^

Checks only index blocks. FAST does not check data blocks.

The format of the FAST qualifier is:

.. code-block:: none

   [-]-FA[ST]


* FAST produces results significantly faster than a full INTEG because the majority of blocks in a typical database are data blocks.

* While INTEG FAST is not a replacement for a full INTEG, it very quickly detects those errors that must be repaired immediately to prevent accelerating database damage.

* By default, INTEG checks all active index and data blocks in the database.

* FAST reports include adjacency information.

* Incompatible with: TN_RESET.

^^^^^^^^
[-]-FILE
^^^^^^^^

Specifies the name of the database file for the MUPIP INTEG operation. FILE requires exclusive (stand-alone) access to a database file and does not require a Global Directory. The format of the FILE qualifier is:

.. code-block:: none

   [-]-FI[LE]

* With stand-alone access to the file, MUPIP INTEG FILE is able to check whether the reference count is zero. A non-zero reference count indicates prior abnormal termination of the database.

* The FILE qualifier is incompatible with the REGION qualifier.

* By default, INTEG operates on FILE.

.. _mupip-integ-full:

^^^^^^^^
[-]-FULL
^^^^^^^^

Displays an expanded report for a MUPIP INTEG operation. With FULL specified, MUPIP INTEG displays the number of index and data blocks in the directory tree and in each global variable tree as well as the total number of directory, index and data blocks. The format of the FULL qualifier is:

.. code-block:: none

   [-]-FU[LL]


* The FULL qualifier is incompatible with the BRIEF qualifier.

* By default, INTEG reports are BRIEF.

* Use FULL to have INTEG report all global names in a region or list of regions.

^^^^^^^^^^^^^^^^^
[-]-IMAXBLOCKSIZE
^^^^^^^^^^^^^^^^^

Specifies the maximum actual size of an index-level block in the database; any index block which contains data in excess of this size causes integ to report the block number, its path, and an error. The format of the IMAXBLOCKSIZE qualifier is:

.. code-block:: none

   [-]-IMAX[BLOCKSIZE]=integer

^^^^^^^^^^^^^
[-]-KEYRANGES
^^^^^^^^^^^^^

Specify whether the MUPIP INTEG report includes key ranges that it detects which identify the data suspected of problems. The format of the KEYRANGES qualifier is:

.. code-block:: none

   [-]-[NO]K[EYRANGES]

By default, INTEG displays KEYRANGES.

^^^^^^^
[-]-MAP
^^^^^^^

Specifies the maximum number of "incorrectly marked busy errors" that MUPIP INTEG reports. The format of the MAP qualifier is:

.. code-block:: none

   [-]-[NO]MAP[=max_imb_errors]


* <max_imb_errors> specifies the threshold limit for the number of incorrectly marked busy errors.

* NOMAP automatically sets a high threshold limit of 1000000 (1 million) incorrectly marked busy errors (MAP=1000000).

* By default, INTEG reports a maximum of 10 map errors (MAP=10).

.. note::
   MUPIP INTEG reports all "incorrectly marked free" errors as they require prompt action. MAP does not restrict their reports.

An error in an index block prevents INTEG from processing potentially large areas of the database. A single "primary" error may cause large numbers of "secondary" incorrectly marked busy errors, which are actually useful in identifying valid blocks that have no valid index pointer. Because "real" or primary incorrectly marked busy errors only make "empty" blocks unavailable to the system, they are low impact and do not require immediate repair.

.. note::
   After a database recovery with RECOVER (for example, using BEFORE_TIME) or ROLLBACK (for example, using FETCHRESYNC), the database may contain incorrectly marked busy errors. Although these errors are benign, they consume available space. Schedule repairs on the next opportunity.

^^^^^^^^^^^^^^
[-]-MAXKEYSIZE
^^^^^^^^^^^^^^

Specifies the maximum number of "key size too large" errors that a MUPIP INTEG operation reports. The format of the MAXKEYSIZE qualifier is:

.. code-block:: none

   [-]-[NO]MAX[KEYSIZE][=integer]

* By default, INTEG reports a maximum of 10 key size errors (MAXKEYSIZE=10).

* NOMAXKEYSIZE removes limits on key size reporting so that INTEG reports all "key size too large" errors.

* NOMAXKEYSIZE does not accept assignment of an argument.

* "Key size too large" errors normally only occur if a DSE CHANGE FILEHEADER KEY_MAX_SIZE command reduces the maximum key size.

.. _integ-online:

^^^^^^^^^^
[-]-ONLINE
^^^^^^^^^^

Specifies that while a MUPIP INTEG operation is active, other processes can update the database without affecting the result of the operation. Allows checking database structural integrity to run concurrently with database updates. The format of the ONLINE qualifier is:

.. code-block:: none

   [-]-[NO]O[NLINE]

* NOONLINE specifies that the database should be frozen during MUPIP INTEG.

* By default, MUPIP INTEG is online.

* Since MUPIP INTEG ONLINE does not freeze database updates, it cannot safely correct errors in the "blocks to upgrade" and "free blocks" fields in the file header, while MUPIP INTEG NOONLINE can correct these fields.

* As it checks each database file, MUPIP INTEG ONLINE creates a sparse file of the same size as the database. As each YottaDB process updates the database, it places a copy of the old block in the sparse file before updating the database. For any database blocks with a newer transaction number than the start of the INTEG, MUPIP uses the copy in the sparse file. Thus, analogous with MUPIP BACKUP ONLINE, INTEG reports on the state of the database as of when it starts, not when it completes. Note: a command such as ls -l shows sparse files at their full size, but does not show actual disk usage. Use a command such as :code:`du -sh` to see actual disk usage.

* The environment variable ydb_snaptmpdir can be used to indicate a directory where MUPIP should place the snapshot files (used by MUPIP INTEG ONLINE). If ydb_snaptmpdir does not exist, INTEG uses the location specified by ydb_baktmpdir and if neither of those environment variables is defined, INTEG places the snapshot files in the current directory at the time you issue the INTEG command. MUPIP and YottaDB processes automatically clean up these temporary snapshot files under a wide variety of conditions.

* Temporary directory security settings must allow write access by the MUPIP process and by all processes updating the database. MUPIP creates the temporary file with the same access as the database file so processes updating the database can write to the temporary file. If the database is encrypted, the updating processes write encrypted blocks to the snapshot file and the MUPIP INTEG process must start with access to appropriate key information as it does even NOONLINE.

*  MUPIP INTEG NOONLINE [FAST] {REGION|FILE} clears the KILLs in progress and the Abandoned Kills flags if the run includes the entire database and there are no incorrectly marked busy blocks.

* Only one online integ can be active per database region. If an online integ is already active, a subsequent one issues an error and immediately terminates. If an online integ does not successfully complete, YottaDB cleans it up in one of the following ways:

  * A subsequent online integ detects that an earlier one did not successfully complete and releases the resources held by the prior online integ before proceeding.
  * If a MUPIP STOP was issued to the online integ process, the process cleans up any resources it held. Note: since the process was stopped the results of the integ may not be valid.
  * subsequent MUPIP RUNDOWN ensures the release of resources held by prior unsuccessful online integs for the specified regions.
  * For every 64K transactions after the online integ initiation, online integ checks YottaDB's health for improperly abandoned online integs and releases resources held by any it finds.

* Incompatible with: FILE, TN_RESET (there should be no need to use TN_RESET on a YottaDB database).

.. _integ-region:

^^^^^^^^^^
[-]-REGION
^^^^^^^^^^

Specifies that the INTEG parameter identifies one or more regions rather than a database file. The format of the REGION qualifier is:

.. code-block:: none

   [-]-R[EGION]=region-list | -R[EGION] region-list

* The region-list identifies the target of INTEG. region-list may specify more than one region of the current global directory in a list. Region names in the list are case-insensitive, separated by commas, and wildcards can be used to specify them. Any region-name may include the wildcard characters * and ? (remember to escape them to protect them from inappropriate expansion by the shell). Any region name expansion occurs in M (ASCII) collation order. INTEG processes regions based on the order specified in the list after wild-card expansion.

* INTEG REGION requires the environment variable ydb_gbldir to specify a valid Global Directory. For more information on defining ydb_gbldir, refer to `Chapter 4: "Global Directory Editor" <./gde.html>`__.

* Because a KILL may briefly defer marking the blocks it releases "free" in the bit maps, INTEG REGION may report spurious "block incorrectly marked busy" errors. These errors are benign. If these errors occur in conjunction with a "Kill in progress" error, resolve the errors after the "Kill in progress" error is no longer present.

* By default, INTEG operates FILE.

* Incompatible with: FILE, TN_RESET

^^^^^^^^^^^^^
[-]-SUBSCRIPT
^^^^^^^^^^^^^

Specifies a key (an unsubscripted or subscripted global name) or a range of keys to INTEG.

An unsubscripted global name key may be enclosed in single or double quotes (for example, :code:`^a` or :code:`'^a'` or :code:`"^a"` are all valid). A subscripted global name with numeric subscripts must be enclosed in single or double quotes to avoid the shell from parsing the parentheses (for example, :code:`'^a(1)'` or :code:`"^a(1)"` are both valid). A subscripted global name with string subscripts must be enclosed in a mix of single and double quotes and use 2 double quotes for every double quote in the string subscript (for example, :code:`'"^a(1,""string"")"'` is valid). See example section below for more details.

Identify a range by separating the beginning key and ending key with a colon (:code:`:`).

SUBSCRIPT cannot detect incorrectly marked busy errors. The format of the SUBSCRIPT qualifier is:

.. code-block:: none

   [-]-SU[BSCRIPT]=subscript

Specify SUBSCRIPT only if the path to the keys in the subscript is not damaged. If the path is questionable or known to be damaged, use DSE to find the block(s) and INTEG BLOCK.

Incompatible with: BLOCK, TN_RESET

^^^^^^^^^
[-]-STATS
^^^^^^^^^

Specifies INTEG to check any active statistics database associated with the region(s) specified for the command. The format of the STATS qualifier is:

.. code-block:: none

   [-]-[NO]ST[ATS]

Specify STATS only if you have reason to understand that statistics reporting is failing with database errors or reporting incorrect results. Because FILE requires standalone access and statistic databases are automatically created and removed it is incompatible with STATS. The default is NOSTATS.

Incompatible with: BLOCK, FILE, TN_RESET

^^^^^^^^^^^^
[-]-TN_RESET
^^^^^^^^^^^^

Resets block transaction numbers and backup event recorded transaction numbers to one (1), and the current transaction number to two (2) which makes the backup event recorded transaction numbers more meaningful and useful. It also issues an advisory message to perform a backup.

The format of the TN_RESET qualifier is:

.. code-block:: none

   [-]-TN[_RESET]

* Transaction numbers can go up to 18,446,744,073,709,551,615. This means that a transaction processing application that runs flat out at a non-stop rate of 1,000,000 updates per second would need a TN reset approximately every 584,554 years.

* The TN_RESET qualifier rewrites all blocks holding data. If the transaction overflow resets to zero (0) database operation is disrupted.

* The TN_RESET qualifier is a protective mechanism that prevents the transaction overflow from resetting to 0.

* By default, INTEG does not modify the block transaction numbers.

.. note::
   There should never be a need for a TN_RESET on the database, even when cleaning up after a runaway process.

* The TN_RESET qualifier is incompatible with the FAST, BLOCK, REGION, and SUBSCRIPT qualifiers.

.. note::
   Any time a YottaDB update opens a database file that was not properly closed, YottaDB increments the transaction number by 1000. This automatic increment prevents problems induced by abnormal database closes, but users must always consider this factor in their operational procedures. The rate at which YottaDB "uses up" transaction numbers is a function of operational procedures and real database updates.

.. _integ-transaction:

^^^^^^^^^^^^^^^
[-]-TRANSACTION
^^^^^^^^^^^^^^^

Specifies the maximum number of block transaction-number-too-large errors that MUPIP INTEG reports. The format of the TRANSACTION qualifier is:

.. code-block:: none

   [-]-[NO]TR[ANSACTION][=integer]

* NOTRANSACTION removes limits on transaction reporting so MUPIP INTEG reports all transaction number errors.

* NOTRANSACTION does not accept assignment of an argument.

* A system crash may generate many "block transaction number too large" errors. These errors can cause problems for BACKUP INCREMENTAL and for transaction processing. Normally, the automatic increment of 1000 blocks that YottaDB adds when a database is reopened averts these errors. If a problem still exists after the database is reopened, users can use a value in the DSE CHANGE FILEHEADER CURRENT_TN= command to quickly fix "block transaction number too large" errors.

* By default, INTEG reports a maximum of 10 block transaction errors (TRANSACTION=10).

.. _integ-verbose:

~~~~~~~~~~~
[-]-VERBOSE
~~~~~~~~~~~

Produces verbose output to help with debugging.

~~~~~~~~~~~~~~~~~~~~~~~~~
Examples for MUPIP INTEG
~~~~~~~~~~~~~~~~~~~~~~~~~

Example:

.. code-block:: bash

   $ mupip integ -block=4 yottadb.dat

This command performs a MUPIP INTEG operation on BLOCK 4 of yottadb.dat.

Example:

.. code-block:: bash

   $ mupip integ -adjacency=20

A sample output from the above command follows:

.. code-block:: bash

   Type           Blocks         Records          % Used      Adjacent
   Directory           2             110          25.732            NA
   Index            1170          341639          88.298             6
   Data           340578          519489          99.268        337888
   Free             6809              NA              NA            NA
   Total          348559          861238              NA        337894
   [Spanning Nodes:3329 ; Blocks:341403]

This example performs a MUPIP INTEG operation assuming that logically related data occupies 20 data blocks in the current database. The sample output shows that out of 1137 data blocks, 1030 data blocks are adjacent to each other. One may be able to improve the performance of a database if all blocks are as adjacent as possible. "% Used" is the amount of space occupied across the in-use blocks divided by the space available in the in-use blocks, and thus represents the packing density for the in-use blocks (excluding local bit maps). Higher "% Used" may actually be undesirable from a performance perspective as they indicate a higher likelihood of block splits with upcoming updates.

Example:

.. code-block:: bash

   $ mupip integ -brief yottadb.dat

This command performs a MUPIP INTEG operation on the database yottadb.dat. A sample output from the above command follows:

.. code-block:: bash

   No errors detected by integ.
   Type           Blocks         Records          % Used      Adjacent
   Directory           2             110          25.732            NA
   Index            1170          341639          88.298             4
   Data           340578          519489          99.268        337617
   Free             6809              NA              NA            NA
   Total          348559          861238              NA        337621
   [Spanning Nodes:3329 ; Blocks:341403]

Example:

.. code-block:: bash

   $ mupip integ -fast yottadb.dat

This command performs a MUPIP INTEG operation only on the index block of the database file yottadb.dat. A sample output from the above command follows:

.. code-block:: bash

   No errors detected by fast integ.

  Type           Blocks         Records          % Used      Adjacent
  Directory           2             110          25.732            NA
  Index            1170          341639          88.298             4
  Data           340578              NA              NA        337617
  Free             6809              NA              NA            NA
  Total          348559              NA              NA        337621

Note the NA entries for Data type. It means that the MUPIP INTEG FAST operation checked only index blocks.

.. code-block:: bash

   $ mupip integ -full yottadb.dat

The sample output from the above command follows:

.. code-block:: bash


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

.. code-block:: bash

   $ mupip integ -map=20 -maxkeysize=20 -transaction=2 yottadb.dat

This command performs a MUPIP INTEG operation and restricts the maximum number of "key size too large" errors to 20.

Example:

.. code-block:: bash

   $ mupip integ -map=20 -transaction=2 yottadb.dat

This command performs a MUPIP INTEG operation and restricts the maximum number of "block transaction number too large" errors to 2.

.. code-block:: bash

   $ mupip integ -file yottadb.dat -tn_reset

This command resets the transaction number to one in every database block.

Example:

.. code-block:: bash

   $ mupip integ -subscript="^Parrots" yottadb.dat

This example performs a MUPIP INTEG operation on the global variable :code:`^Parrots` in the database file yottadb.dat.

Example:

.. code-block:: bash

   $ mupip integ -subscript='"^Amsterdam(100)"':'"^Bolivia(""Chimes"")"' -region DEFAULT

This example performs a MUPIP INTEG operation on all global variable nodes/keys greater than or equal to :code:`^Amsterdam(100)` and less than or equal to :code:`^Bolivia("Chimes")` in the DEFAULT region.

.. note::
   To specify a literal in the command string, use two double quotation marks for example, ^b(""c"").
