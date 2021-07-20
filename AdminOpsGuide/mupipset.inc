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

Use MUPIP SET for performance tuning and/or modifying certain database and journal file attributes.

The format of the SET command is:

.. code-block:: none

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
    -T[RIGGER_FLUSH]=integer
    -V[ERSION]={V4|V6}
    -W[AIT_DISK]=integer
    -WR[ITES_PER_FLUSH=integer


* Exclusive access to the database is required if the MUPIP SET command specifies ACCESS_METHOD, GLOBAL_BUFFERS, LOCK_SPACE or NOJOURNAL, or if any of the JOURNAL options ENABLE, DISABLE, or BUFFER_SIZE are specified.

* The file-name, journal_file_name, region-list or REPLICATION qualifier identify the target of the SET.

* The SET command must include one of the following target qualifiers which determine whether the argument to the SET is a file-name or a region-list.

~~~~~~~~~~~~~~~~
SET Qualifiers
~~~~~~~~~~~~~~~~

^^^^^^
-FILE
^^^^^^

Specifies that the argument is a file-name for a single database file. The format of the FILE qualifier is:

.. code-block:: none

   -F[ILE]

Incompatible with: JNLFILE, REGION and REPLICATION

^^^^^^^^^^
-JNLFILE
^^^^^^^^^^

Specifies that the argument is a journal-file-name. The format of the JNLFILE qualifier is:

.. code-block:: none

   -JNLF[ILE] journal-file-name

Incompatible with: FILE, REGION and REPLICATION

^^^^^^^^
-REGION
^^^^^^^^

Specifies that the argument is a region-list which identifies database file(s) mapped by the current Global Directory. The format of the REGION qualifier is:

.. code-block:: none

   -R[EGION] region-list

The region-list identifies the target of SET. region-list may specify more than one region of the current global directory in a list. Regions are case-insensitive, separated by a comma, and wild-cards can be used to specify them. Any region-name may include the wild-card characters * and % (remember to escape them to protect them from inappropriate expansion by the shell). Any region name expansion occurs in M (ASCII) collation order.

Incompatible with: FILE, JNLFILE and REPLICATION

^^^^^^^^^^^^^
-REPLICATION
^^^^^^^^^^^^^

Specifies whether replication is on or off. The format of the REPLICATION qualifier is:

.. code-block:: none

   -REP[LICATION]={ON|OFF}

Incompatible with: JNLFILE

The following sections describe the action qualifiers of the MUPIP SET command exclusive of the details related to journaling and replication, which are described in `Chapter 6: “YottaDB Journaling” <./ydbjournal.html>`_ and `Chapter 7: “Database Replication” <./dbrepl.html>`_. All of these qualifiers are incompatible with the JNLFILE and REPLICATION qualifiers.

.. _set-access-method:

^^^^^^^^^^^^^^^
-ACCESS_METHOD
^^^^^^^^^^^^^^^

Specifies the access method (YottaDB buffering strategy) for storing and retrieving data from the global database file. The format of the ACCESS_METHOD qualifier is:

.. code-block:: none

   -AC[CESS_METHOD]=code

For more information on specifying the ACCESS_METHOD,refer to :ref:`segment-qualifiers`.

^^^^^^^^^^^
-ASYNCIO
^^^^^^^^^^^

Specifies whether to use asynchronous I/O for an access method BG database, rather than using synchronous I/O through the file system cache. ASYNCIO is incompatible with the MM access method and an attempt to combine the two with MUPIP SET produces a ASYNCIONOMM error. The format of the ASYNCIO qualifier is:

.. code-block:: none

   -[NO]AS[YNCIO]

For more information on specifying ASYNCIO,refer to :ref:`segment-qualifiers`.

^^^^^^^^^^^^
-DEFER_TIME
^^^^^^^^^^^^

Specifies, in MM access mode, the multiplying factor applied to the flush time to produce a wait after an update before ensuring a journal buffer write to disk; the default is 1. A value of 2 produces a wait of double the flush time. NODEFER_TIME or a value of -1 turns off timed journal writing, leaving the journal, under light update conditions, to potentially get as stale as the epoch time. Note that, in MM mode without the sync_io option set, absent a VIEW("JNLFLUSH") from the application, YottaDB only fsyncs the journal at the epoch. The format of the DEFER_TIME qualifier is:

.. code-block:: none

   -[NO]D[efer_time][=seconds]

^^^^^^^^^^^^^^^^
-DEFER_ALLOCATE
^^^^^^^^^^^^^^^^

With DEFER_ALLOCATE, YottaDB instructs the file system to create the database file as a sparse file. Before using DEFER_ALLOCATE, ensure that your underlying file system supports sparse files. By default UNIX file systems, and YottaDB, use sparse (or lazy) allocation, which defers actual allocation until blocks are first written. The format of the DEFER_ALLOCATE qualifier is:

.. code-block:: none

   -[NO]DEFER_ALLOCATE

* Utilities such as du report typically show lower disk space usage for a database file with DEFER_ALLOCATE because YottaDB instructs the file system to defer disk space allocation to the time when there is an actual need. With NODEFER_ALLOCATE, such utilities report higher disk space usage count as YottaDB instructs the file system to preallocate disk space without waiting for a need to arise.

* DEFER_ALLOCATE makes database file extensions lighter weight. However, disk activity may tend towards causing fragmentation.

* To switch an existing database file so it immediately preallocates all blocks, first use MUPIP SET NODEFER_ALLOCATE to set the switch in the database file header, followed by MUPIP EXTEND BLOCKS=n, where n >= 0. Failures to preallocate space produce a PREALLOCATEFAIL error.

* The default is DEFER_ALLOCATE.

.. _mupip-set-epochtaper:

^^^^^^^^^^^^^
-EPOCHTAPER
^^^^^^^^^^^^^

Tries to minimize epoch duration by reducing the number of buffers to flush by YottaDB and the file system (via an fsync()) as the epoch (time-based or due to journal file auto-switch) approaches. Epoch tapering reduces the impact of I/O activity during an epoch event. Application that experience high load and/or need to reduce latency may benefit from epoch tapering. The format of the EPOCHTAPER qualifier is:

.. code-block:: none

   -[NO]EPOCHTAPER

^^^^^^^^^^^^^^^^^^
-EXTENSION_COUNT
^^^^^^^^^^^^^^^^^^

Specifies the number of GDS blocks by which an existing database file extends. A file or region name is required. This qualifier requires standalone access. The format of the EXTENSION_COUNT qualifier is:

.. code-block:: none

   -E[XTENSION_COUNT]=integer

For more information on specifying the EXTENSION_COUNT, refer to :ref:`segment-qualifiers`.

^^^^^^^^^^^^
-FLUSH_TIME
^^^^^^^^^^^^

Specifies the amount of time between deferred writes of stale cache buffers. The default value is 1 second and the maximum value is 1 hour. FLUSH_TIME requires standalone access. The format of the FLUSH_TIME qualifier is:

.. code-block:: none

   -F[LUSH_TIME]=[[[HOURS:]MINUTES:]SECONDS:]CENTISECONDS

^^^^^^^^^^^^^^^^
-GLOBAL_BUFFERS
^^^^^^^^^^^^^^^^

Specifies the number of cache buffers for a BG database. This qualifier requires standalone access.The format of the GLOBAL_BUFFERS qualifier is:

.. code-block:: none

   -G[LOBAL_BUFFERS]=integer

For more information on ways to determine good working sizes for GLOBAL_BUFFERS, refer to :ref:`segment-qualifiers`.

In general, increasing the number of global buffers improves performance by smoothing the peaks of I/O load on the system. However, increasing the number of global buffers also increases the memory requirements of the system, and a larger number of global buffers on memory constrained systems can increase the probability of the buffers getting swapped out. If global buffers are swapped out, any performance gain from increasing the number of global buffers will be more than offset by the performance impact of swapping global buffers. Most applications use from 1,000 to 4,000 global buffers for database regions that are heavily used. YottaDB does not recommend using fewer than 256 buffers except under special circumstances.

The minimum is 64 buffers and the maximum is 2,097,151 buffers. By default, MUPIP CREATE establishes GLOBAL_BUFFERS using information entered in the Global Directory.

On many UNIX systems, default kernel parameters may be inadequate for YottaDB global buffers, and may need to be adjusted by a system administrator.

^^^^^^^^^^^^^^^^^
-HARD_SPIN_COUNT
^^^^^^^^^^^^^^^^^

The mutex hard spin count specifies the number of attempts to grab the mutex lock before initiating a less CPU-intensive wait period. The format of HARD_SPIN_COUNT is:

.. code-block:: none

   -HARD_SPIN_COUNT=integer

The default value is 128. Except on the advice of your YottaDB support channel, YottaDB recommends leaving the default values unchanged in production environments, until and unless, you have data from testing and benchmarking that demonstrates a benefit from a change.

^^^^^^^^^^^^^^^^^^^^^^
-INST_FREEZE_ON_ERROR
^^^^^^^^^^^^^^^^^^^^^^

Enables or disables custom errors in a region to automatically cause an Instance Freeze. This flag modifies the "Inst Freeze on Error" file header flag. The format of the INST_FREEZE_ON_ERROR qualifier is:

.. code-block:: none

   -[NO]INST[_FREEZE_ON_ERROR]

For more information on creating a list of custom errors that automatically cause an Instance Freeze, refer to :ref:`instance-freeze`.

For more information on promptly setting or clearing an Instance Freeze on an instance irrespective of whether any region is enabled for Instance, refer to the :ref:`start-source-server` section of the Database Replication chapter.

^^^^^^^^^
-JOURNAL
^^^^^^^^^

Specifies whether the database allows journaling and, if it does, characteristics for the journal file.

.. note::
   In regions that have journaling enabled and on, users can switch journal files without either requiring standalone access or freezing updates.

The format of the JOURNAL qualifier is:

.. code-block:: none

   -[NO]J[OURNAL][=journal-option-list]

* NOJOURNAL specifies that the database does not allow journaling. And also it does not accept an argument assignment.

* JOURNAL specifies journaling is allowed. It takes one or more arguments in a journal-option-list.

For detailed description of the all JOURNAL qualifiers and its keywords, refer to :ref:`set-action-qualifiers`.

^^^^^^^^^^
-KEY_SIZE
^^^^^^^^^^

Specifies the maximum key size in bytes for storing and retrieving data from the global database file. The maximum supported size is 1019 bytes. The format of the KEY_SIZE qualifier is:

.. code-block:: none

   -K[EY_SIZE]=bytes

For more information on KEY_SIZE, refer to :ref:`region-qualifiers`.

^^^^^^^^^^^^
-LOCK_SPACE
^^^^^^^^^^^^

Specifies the number of pages allocated to the management of M locks associated with the database. The size of a page is always 512 bytes. The format of the LOCK_SPACE qualifier is:

.. code-block:: none

   -L[OCK]_SPACE=integer

* The maximum LOCK_SPACE is 262144 pages.

* The minimum LOCK_SPACE is 10 pages.

* The default LOCK_SPACE is 40 pages.

* For more information on LOCK_SPACE, refer to :ref:`segment-qualifiers`.

* This qualifier requires standalone access.

^^^^^^^^^^^^^
-MUTEX_SLOTS
^^^^^^^^^^^^^

Sets the size of a structure that YottaDB uses to manage contention for the principal critical section for a database. Performance issues may occur when there are many processes contending for database access and if this structure cannot accommodate all waiting processes. Therefore, YottaDB recommends setting this value to a minimum of slightly more than the maximum number of concurrent processes you expect to access the database.

The minimum value is 64 and the maximum value is 32768. The default value is 1024. The format of the MUTEX_SLOTS qualifier is:

.. code-block:: none

   -M[UTEX_SLOTS]=integer

^^^^^^^^^^^^^^^^^
-NULL_SUBSCRIPTS
^^^^^^^^^^^^^^^^^

Controls whether YottaDB accepts null subscripts in database keys.

Usage:

.. code-block:: none

   -N[ULL_SUBSCRIPTS]=value

* value can either be T[RUE], F[ALSE], ALWAYS, NEVER, or EXISTING. See GDE chapter for more information on these values of null_subscript.

* Prohibiting null subscripts can restrict access to existing data and cause YottaDB to report errors.

* The default value is never.

^^^^^^^^^^^^^^^^^^^^
-LCK_SHARES_DB_CRIT
^^^^^^^^^^^^^^^^^^^^

Specifies whether LOCK actions share the same resource and management as the database or use a separate resource and management. The format of the LCK_SHARES_DB_CRIT qualifier is:

.. code-block:: none

    -[NO]LC[K_SHARES_DB_CRIT]

The default is Sep(arate)/FALSE.

For more information, refer to :ref:`region-qualifiers`.

^^^^^^^^^^^^
-QDBRUNDOWN
^^^^^^^^^^^^

Shortens normal process shutdown when a large number of processes accessing a database file need to shutdown almost simultaneously, for example, in benchmarking scenarios or emergencies. The format of the QDBRUNDOWN qualifier is:

.. code-block:: none

   -[NO]Q[DBRUNDOWN]

When a terminating YottaDB process observes that a large number of processes are attached to a database file and QDBRUNDOWN is enabled, it bypasses checking whether it is the last process accessing the database. Such a check occurs in a critical section and bypassing it also bypasses the usual RUNDOWN actions which accelerates process shutdown removing a possible impediment to process startup. By default, QDBRUNDOWN is disabled.

Note that with QDBRUNDOWN there is a possibility that the last process to exit might leave the database shared memory and IPC resources in need of cleanup. Except after the number of concurrent processes exceeds 32Ki, QDBRUNDOWN minimizes the possibility of abandoned resources, but it cannot eliminate it. When using QDBRUNDOWN, use an explicit MUPIP command such as RUNDOWN or JOURNAL RECOVER or ROLLBACK of the database file after the last process exits, to ensure the cleanup of database shared memory and IPC resources; not doing so risks database damage.

When a database has QDBRUNDOWN enabled, if the number of attached processes ever exceeds 32Ki, YottaDB stops tracking the number of attached processes, which means that it cannot recognize when the number reaches zero (0) and the shared resources can be released. The process that detects this event issues a NOMORESEMCNT in the system log. This means that an orderly, safe shutdown requires a MUPIP JOURNAL -ROLLBACK -BACKWARD for replicated databases, a MUPIP JOURNAL RECOVER BACKWARD for unreplicated journaled databases and a MUPIP RUNDOWN for journal-free databases.

^^^^^^^^^^^^^^^^^^^^^^
-PARTIAL_RECOV_BYPASS
^^^^^^^^^^^^^^^^^^^^^^

Sets the CORRUPT_FILE flag in the database file header to FALSE. The CORRUPT_FILE flag indicates whether a region completed a successful recovery. The format of the PARTIAL_RECOV_BYPASS qualifier is:

.. code-block:: none

   -PA[RTIAL_RECOV_BYPASS]

For more information, refer to the :ref:`CORRUPT_FILE <dse-change-corrupt-file>` qualifier.

^^^^^^^^^^^^
-READ_ONLY
^^^^^^^^^^^^

Indicates whether YottaDB should treat an MM access method segment as read only for all users, including root. This designation augments UNIX authorizations and prevents any state updates that normally might require an operational action for a database with no current accessing (attached) processes. MUPIP emits an error on attempts to set READ_ONLY on databases with the BG access method, or to set the access method to BG on databases with READ_ONLY set. The YottaDB help databases have READ_ONLY set by default. The format of the READ_ONLY qualifier is:

.. code-block:: none

   -[NO]REA[D_ONLY]

.. note::
   When the first process connects to a database, it creates a access-control semaphore as part of the management of the shared resource. However, when processes connect to a READ_ONLY database , each creates a private copy of the in-memory structures for the database and thus a private semaphore.

^^^^^^^^^^^^^
-RECORD_SIZE
^^^^^^^^^^^^^

Specifies the maximum record size in bytes for storing and retrieving data from the global database file. The maximum supported size is 1MiB bytes. The format of the RECORD_SIZE qualifier is:

.. code-block:: none

   -REC[ORD_SIZE]=bytes

For more information on KEY_SIZE, refer to :ref:`region-qualifiers`.

^^^^^^^^^^^^^^^^^^
-REORG_SLEEP_NSEC
^^^^^^^^^^^^^^^^^^

Specifies the number of nanoseconds that a MUPIP REORG process operating between blocks takes to process, with default value of 0 and a maximum of 999999999 (i.e. 999,999,999, or 1 nanosecond less than 1 second). Using non-zero values reduces the IO impact of MUPIP REORG, at the cost of increasing the duration of the operation. Note that the existing environment variable ydb_poollimit is the appropriate technique to limit the impact of MUPIP REORG on global buffers; the -reorg_sleep_nsec can be used to limit the impact on the IO subsystem.

^^^^^^^^^^^^^^^^
-RESERVED_BYTES
^^^^^^^^^^^^^^^^

Specifies the size to be reserved in each database block. RESERVED_BYTES is generally used to reserve room for compatibility with other implementations of M or to observe communications protocol restrictions. The format of the RESERVED_BYTES qualifier is:

.. code-block:: none

   -RES[ERVED_BYTES]=size


* RESERVED_BYTES may also be used as a user-managed fill factor.

* The minimum RESERVED_BYTES is 0 bytes. The maximum RESERVED_BYTES is the block size minus the size of the block header which is 7 or 8 depending on your platform. Realistic determinations of this amount should leave room for at least one record of maximum size.

^^^^^^^^^^^^^^^^^^
-SLEEP_SPIN_COUNT
^^^^^^^^^^^^^^^^^^

Specifies the number of times a process suspends its activity while waiting to obtain critical sections for shared resources, principally those involving databases. The format of the SLEEP_SPIN_COUNT qualifier is:

.. code-block:: none

   -SLEEP_SPIN_COUNT=integer

* integer is the number of times the process yields to the OS scheduler or sleeps (depending in the SPIN_SLEEP_LIMIT) after exhausting its hard spin count and before enqueuing itself to be awakened by another process releasing the shared resource mutex.

* The default is 128.

* Except on the advice of your YottaDB support channel, YottaDB recommends leaving the default values unchanged in production environments, until and unless you have data from testing and benchmarking that demonstrates benefits from a change.

^^^^^^^^^^^^^^^^^^
-SPIN_SLEEP_MASK
^^^^^^^^^^^^^^^^^^

Specifies the maximum number of nanoseconds for processes to sleep while waiting to obtain critical sections for shared resources, principally those involving databases. The format of the SPIN_SLEEP_MASK qualifier is:

.. code-block:: none

   -SPIN_SLEEP_MASK=hex_mask

* hex_mask is a hexadecimal mask that controls the maximum time (in nanoseconds) the process sleeps on a sleep spin.

* The default is zero (0) which causes the process to return control to the UNIX kernel to be rescheduled with no explicit delay. When the value is non-zero, the process waits for a random value between zero (0) and the maximum value permitted by the mask.

* Except on the advice of your YottaDB support channel, YottaDB recommends leaving the default values unchanged in production environments, until and unless you have data from testing and benchmarking that demonstrates a benefit from a change.

^^^^^^^
-STATS
^^^^^^^

Specifies whether YottaDB should permit statistics sharing for this region. This characteristic permits operational exclusion of statistics sharing for a region. The format of the STATS qualifier is:

.. code-block:: none

   -[NO]STAT[S]

* At database creation, GDE controls this characteristic, which by default is specified as STATS (on). When on, this characteristic causes YottaDB to create a small MM database for the associated region to hold the shared statistics.
* A process disables itself from maintaining the shared statistics when it fails to open a statsDB. It does not, however, disable subsequently starting processes from maintaining the shared statistics.

^^^^^^^^^^^^^^
-STDNULLCOLL
^^^^^^^^^^^^^^

Specifies whether YottaDB uses standard or `historical null collation <../ProgrammersGuide/langfeat.html#null-subs-colltn>`_ for null-subscripted keys. YottaDB strongly recommends that you use STDNULLCOLL and not the historical null collation. The format of the STDNULLCOLL qualifier is:

.. code-block:: none

   -[NO]STD[NULLCOLL]

^^^^^^^^^^^^^^^^
-TRIGGER_FLUSH
^^^^^^^^^^^^^^^^

Specifies the decimal value, in buffers, for the threshold at which processes start flushing dirty buffers after each update. The format of the TRIGGER_FLUSH qualifier is:

.. code-block:: none

   -T[RIGGER_FLUSH]=integer

^^^^^^^^^^
-VERSION
^^^^^^^^^^

Sets the block format version (Desired DB Format field in the file header) for all subsequent new blocks. The format of the VERSION qualifier is:

.. code-block:: none

   -V[ERSION]={version}

* MUPIP UPGRADE and MUPIP REORG UPGRADE set the Desired DB Format field in the database file header to the latest version while MUPIP REORG DOWNGRADE sets it to the previous version.

For more information on the upgrading or downgrading your database, refer to the release notes document of your current YottaDB version(s).

^^^^^^^^^^^
-WAIT_DISK
^^^^^^^^^^^

Specifies the seconds to wait for disk space before giving up on a database block write, where zero (0) means to give an error immediately without waiting. The format of the WAIT_DISK qualifier is:

.. code-block:: none

   -W[AIT_DISK]=seconds

^^^^^^^^^^^^^^^^^^
-WRITES_PER_FLUSH
^^^^^^^^^^^^^^^^^^

Specifies the decimal number of blocks to write in each flush. The default value is 7. The format of the WRITES_PER_FLUSH qualifier is:

.. code-block:: none

   -WR[ITES_PER_FLUSH]=integer

~~~~~~~~~~~~~~~~~~~~~~
Examples for MUPIP SET
~~~~~~~~~~~~~~~~~~~~~~

Example:

.. code-block:: bash

   $ mupip set -journal=on,nobefore -region "*"

This example enables NOBEFORE image journaling and turns on journaling for all regions.

.. code-block:: bash

   $ mupip set -version=r120 -file yottadb.dat
   Database file yottadb.dat now has desired DB format r120

This example sets the block format to r1.20 for all subsequent new blocks in r1.10 database file yottadb.dat.

Example:

.. code-block:: bash

   $ mupip set -version=r110 -file yottadb.dat
   Database file yottadb.dat now has desired DB format r110

This example sets the block format to r1.10 for all subsequent new blocks in r1.00 database file yottadb.dat.

Example:

.. code-block:: bash

   mupip set -flush_time=01:00:00:00 -region DEFAULT

This example sets flush time to 1 hour. You can also specify flush time in any combination of [[[HOURS:]MINUTES:]SECONDS:]CENTISECONDS. MUPIP interprets FLUSH_TIME=360000 or FLUSH_TIME=00:60:00:00 as FLUSH_TIME=01:00:00:00.

Example:

.. code-block:: bash

   $ mupip set -region MAMMALS -inst_freeze_on_error

This example enables custom errors in region MAMMALS to cause an Instance Freeze.
