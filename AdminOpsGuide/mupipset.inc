.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2021-2025 YottaDB LLC and/or its subsidiaries.#
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

   SE[T] {[-]-FI[LE] file-name | [-]-JN[LFILE] journal-file-name | [-]-REG[ION] region-list}
    [-]-AC[CESS_METHOD]={BG|MM}
    [-]-[NO]AS[YNCIO]
    [-]-[NO]DE[FER_TIME][=seconds]
    [-]-[NO]DEFER_ALLOCATE
    [-]-EPOCHTAPER
    [-]-E[XTENSION_COUNT]=integer(no of blocks)
    [-]-F[LUSH_TIME]=integer
    [-]-FU[LLBLKWRT]={0|1|2}
    [-]-G[LOBAL_BUFFERS]=integer
    [-]-H[ARD_SPIN_COUNT]=integer
    [-]-[NO]INST[_FREEZE_ON_ERROR]
    [-]-K[EY_SIZE]=bytes
    [-]-L[OCK_SPACE]=integer
    [-]-MUTEX_S[LOTS]=integer
    [-]-MUTEX_T[YPE]={ADAPTIVE|YDB|PTHREAD}
    [-]-N[ULL_SUBSCRIPTS]=value
    [-]-[NO]LCK_SHARES_DB_CRIT
    [-]-PA[RTIAL_RECOV_BYPASS]
    [-]-PROBLKSPLIT=N
    [-]-[NO]Q[DBRUNDOWN]
    [-]-[NO]REA[D_ONLY]
    [-]-REC[ORD_SIZE]=bytes
    [-]-REORG_SLEEP_NSEC=integer
    [-]-REP[LICATION]={ON|OFF}
    [-]-RES[ERVED_BYTES]=integer]
    [-]-SL[EEP_SPIN_COUNT]=integer
    [-]-SPIN_SLEEP_M[ASK]=hex_mask
    [-]-STAN[DALONENOT]
    [-]-[NO]STAT[S]
    [-]-[NO]STD[NULLCOLL]
    [-]-T[RIGGER_FLUSH]=integer
    [-]-V[ERSION]={V4|V6}
    [-]-W[AIT_DISK]=integer
    [-]-WR[ITES_PER_FLUSH]=integer


* Exclusive access to the database is required if the MUPIP SET command specifies ACCESS_METHOD, GLOBAL_BUFFERS, LOCK_SPACE or NOJOURNAL, or if any of the JOURNAL options ENABLE, DISABLE, or BUFFER_SIZE are specified.

* The file-name, journal_file_name, region-list or REPLICATION qualifier identify the target of the SET.

* The SET command must include one of the following target qualifiers which determine whether the argument to the SET is a file-name or a region-list.

~~~~~~~~~~~~~~~~
SET Qualifiers
~~~~~~~~~~~~~~~~

.. _set-file:

^^^^^^^^
[-]-FILE
^^^^^^^^

Specifies that the argument is a file-name for a single database file. The format of the FILE qualifier is:

.. code-block:: none

   [-]-F[ILE]

Incompatible with: JNLFILE, REGION and REPLICATION

^^^^^^^^^^^
[-]-JNLFILE
^^^^^^^^^^^

Specifies that the argument is a journal-file-name. The format of the JNLFILE qualifier is:

.. code-block:: none

   [-]-JNLF[ILE] journal-file-name

Incompatible with: FILE, REGION and REPLICATION

.. _regiondesc:

^^^^^^^^^^
[-]-REGION
^^^^^^^^^^

Specifies that the argument is a region-list which identifies database file(s) mapped by the current Global Directory. The format of the REGION qualifier is:

.. code-block:: none

   [-]-R[EGION] region-list | [-]-R[EGION]=region-list

In other words, either a space or an equal sign can follow the qualifier. The region-list identifies the target of SET. region-list may specify more than one region of the current global directory in a list. Regions are case-insensitive, separated by a comma, and wild-cards can be used to specify them. Any region-name may include the wild-card characters * and % (remember to escape them to protect them from inappropriate expansion by the shell). Any region name expansion occurs in M (ASCII) collation order. Note that region-list must immediately follow the REGION qualifier. Older versions of YottaDB used only a space and permitted other qualifiers between the REGION qualified and region-list.

Incompatible with: FILE, JNLFILE and REPLICATION

.. _set-replication:

^^^^^^^^^^^^^^^
[-]-REPLICATION
^^^^^^^^^^^^^^^

Specifies whether replication is on or off. The format of the REPLICATION qualifier is:

.. code-block:: none

   [-]-REP[LICATION]={ON|OFF}

Incompatible with: JNLFILE

The following sections describe the action qualifiers of the MUPIP SET command exclusive of the details related to journaling and replication, which are described in `Chapter 6: "YottaDB Journaling" <./ydbjournal.html>`__ and `Chapter 7: "Database Replication" <./dbrepl.html>`__. All of these qualifiers are incompatible with the JNLFILE and REPLICATION qualifiers.

.. _set-access-method:

^^^^^^^^^^^^^^^^^
[-]-ACCESS_METHOD
^^^^^^^^^^^^^^^^^

Specifies the access method (YottaDB buffering strategy) for storing and retrieving data from the global database file. The format of the ACCESS_METHOD qualifier is:

.. code-block:: none

   [-]-AC[CESS_METHOD]=code

For more information on specifying the ACCESS_METHOD,refer to :ref:`segment-qualifiers`.

^^^^^^^^^^^
[-]-ASYNCIO
^^^^^^^^^^^

Specifies whether to use asynchronous I/O for an access method BG database, rather than using synchronous I/O through the file system cache. ASYNCIO is incompatible with the MM access method and an attempt to combine the two with MUPIP SET produces a ASYNCIONOMM error. The format of the ASYNCIO qualifier is:

.. code-block:: none

   [-]-[NO]AS[YNCIO]

For more information on specifying ASYNCIO,refer to :ref:`segment-qualifiers`.

^^^^^^^^^^^^^^^^^^^^^^^
[-]-DATA_RESERVED_BYTES
^^^^^^^^^^^^^^^^^^^^^^^

Specifies the size for YottaDB to reserve in data-level database blocks. DATA_RESERVED_BYTES can be used as a user-managed fill factor when it is appropriate to have a different level of sparseness for data and index blocks. The format of the DATA_RESERVED_BYTES qualifier is:

.. code-block:: none

   [-]-DATA[_RESERVED_BYTES]=size

The minimum DATA_RESERVED_BYTES is 0 bytes. The maximum DATA_RESERVED_BYTES = Block Size - Max Key Size - 40 (Block/Record header overheads). The default DATA_RESERVED_BYTES is 0 bytes.

^^^^^^^^^^^^^^
[-]-DEFER_TIME
^^^^^^^^^^^^^^

Specifies, in MM access mode, the multiplying factor applied to the flush time to produce a wait after an update before ensuring a journal buffer write to disk; the default is 1. A value of 2 produces a wait of double the flush time. NODEFER_TIME or a value of -1 turns off timed journal writing, leaving the journal, under light update conditions, to potentially get as stale as the epoch time. Note that, in MM mode without the sync_io option set, absent a VIEW("JNLFLUSH") from the application, YottaDB only fsyncs the journal at the epoch. The format of the DEFER_TIME qualifier is:

.. code-block:: none

   [-]-[NO]D[efer_time][=seconds]

^^^^^^^^^^^^^^^^^^
[-]-DEFER_ALLOCATE
^^^^^^^^^^^^^^^^^^

With DEFER_ALLOCATE, YottaDB instructs the file system to create the database file as a sparse file. Before using DEFER_ALLOCATE, ensure that your underlying file system supports sparse files. By default Linux file systems, and YottaDB, use sparse (or lazy) allocation, which defers actual allocation until blocks are first written. The format of the DEFER_ALLOCATE qualifier is:

.. code-block:: none

   [-]-[NO]DEFER_ALLOCATE

* Utilities such as du report typically show lower disk space usage for a database file with DEFER_ALLOCATE because YottaDB instructs the file system to defer disk space allocation to the time when there is an actual need. With NODEFER_ALLOCATE, such utilities report higher disk space usage count as YottaDB instructs the file system to preallocate disk space without waiting for a need to arise.

* DEFER_ALLOCATE makes database file extensions lighter weight. However, disk activity may tend towards causing fragmentation.

* To switch an existing database file so it immediately preallocates all blocks, first use MUPIP SET NODEFER_ALLOCATE to set the switch in the database file header, followed by MUPIP EXTEND BLOCKS=n, where n >= 0. Failures to preallocate space produce a PREALLOCATEFAIL error.

* The default is DEFER_ALLOCATE.

.. _mupip-set-epochtaper:

^^^^^^^^^^^^^^
[-]-EPOCHTAPER
^^^^^^^^^^^^^^

Tries to minimize epoch duration by reducing the number of buffers to flush by YottaDB and the file system (via an fsync()) as the epoch (time-based or due to journal file auto-switch) approaches. Epoch tapering reduces the impact of I/O activity during an epoch event. Application that experience high load and/or need to reduce latency may benefit from epoch tapering. The format of the EPOCHTAPER qualifier is:

.. code-block:: none

   [-]-[NO]EPOCHTAPER

^^^^^^^^^^^^^^^
[-]-ENCRYPTABLE
^^^^^^^^^^^^^^^

Performs some basic encryption checks and marks the database as encryptable. Note that marking a database as encryptable does not encrypt the database. For more information on encrypting a database, refer to :ref:`mupip-reorg-encrypt`. The format of the ENCRYPTABLE qualifier is:

.. code-block:: none

   [-]-[NO]ENCRYPTA[BLE]

^^^^^^^^^^^^^^^^^^^
[-]-EXTENSION_COUNT
^^^^^^^^^^^^^^^^^^^

Specifies the number of GDS blocks by which an existing database file extends. A file or region name is required. This qualifier requires standalone access. The format of the EXTENSION_COUNT qualifier is:

.. code-block:: none

   [-]-E[XTENSION_COUNT]=integer

For more information on specifying the EXTENSION_COUNT, refer to :ref:`segment-qualifiers`.

^^^^^^^^^^^^^^
[-]-FLUSH_TIME
^^^^^^^^^^^^^^

Specifies the amount of time between deferred writes of stale cache buffers. The default value is 1 second and the maximum value is 1 hour. The format of the FLUSH_TIME qualifier is:

.. code-block:: none

   [-]-F[LUSH_TIME]=[[[HOURS:]MINUTES:]SECONDS:]CENTISECONDS

^^^^^^^^^^^^^^
[-]-FULLBLKWRT
^^^^^^^^^^^^^^

Determines whether YottaDB writes bytes in the database or filesystem block past the last valid database record. Full block writes can be more efficient with some secondary storage because they avoid the potential read-before-write that can penalize partial block writes. The format of the FULLBLKWRT qualifier is:

.. code-block:: none

   [-]-FU[LLBLKWRT]={0|1|2}

* When FULLBLKWRT=2, when writing data to a newly allocated block, a process writes a full database block. For example, if the database block size is 8KiB, the filesystem block size is 4KiB, and there are 1587 bytes of database records, 8Ki bytes are written to the filesystem in an attempt to ensure that the database block occupies two contiguous blocks in the filesystem. If a subsequent write to that database block has 1259 bytes of database records, 1259 bytes are written to the filesystem.
* When FULLBLKWRT=1, a process writes entire filesystem blocks, even when the database records occupy less than a full block. This may improve filesystem load and improve response time on filesystems where writing less than a full block involves a read-modify-write sequence, and writing a full filesystem block avoids the read.
* When FULLBLKWRT=0, a process writes only the actual database records when writing to the filesystem. This is the default, and is appropriate for most situations. Evaluate all three values when you need to maximize throughput.

YottaDB ignores FULLBLKWRT for MM databases. Since MM databases are mapped to virtual memory, YottaDB has no control over actual database writes.

FULLBLKWRT was added to YottaDB effective release r1.36.

^^^^^^^^^^^^^^^^^^
[-]-GLOBAL_BUFFERS
^^^^^^^^^^^^^^^^^^

Specifies the number of cache buffers for a BG database. This qualifier requires standalone access.The format of the GLOBAL_BUFFERS qualifier is:

.. code-block:: none

   [-]-G[LOBAL_BUFFERS]=integer

For more information on ways to determine good working sizes for GLOBAL_BUFFERS, refer to :ref:`segment-qualifiers`.

In general, increasing the number of global buffers improves performance by smoothing the peaks of I/O load on the system. However, increasing the number of global buffers also increases the memory requirements of the system, and a larger number of global buffers on memory constrained systems can increase the probability of the buffers getting swapped out. If global buffers are swapped out, any performance gain from increasing the number of global buffers will be more than offset by the performance impact of swapping global buffers. Most applications use from 1,000 to 4,000 global buffers for database regions that are heavily used. YottaDB does not recommend using fewer than 256 buffers except under special circumstances.

The minimum is 64 buffers and the maximum is 2,097,151 buffers. By default, MUPIP CREATE establishes GLOBAL_BUFFERS using information entered in the Global Directory.

On many Linux systems, default kernel parameters may be inadequate for YottaDB global buffers, and may need to be adjusted by a system administrator.

^^^^^^^^^^^^^^^^^^^
[-]-HARD_SPIN_COUNT
^^^^^^^^^^^^^^^^^^^

The mutex hard spin count specifies the number of attempts to grab the mutex lock before initiating a less CPU-intensive wait period. The format of HARD_SPIN_COUNT is:

Note that this qualifier takes effect only if :ref:`mutex_type` is ADAPTIVE or YDB. It has no effect if set to PTHREAD.

.. code-block:: none

   [-]-HARD_SPIN_COUNT=integer

The default value is 128. Except on the advice of your YottaDB support channel, YottaDB recommends leaving the default values unchanged in production environments, until and unless, you have data from testing and benchmarking that demonstrates a benefit from a change.

^^^^^^^^^^^^^^^^^^^^^^^
[-]INDEX_RESERVED_BYTES
^^^^^^^^^^^^^^^^^^^^^^^

Specifies the size for YottaDB to reserve in index-level database blocks. INDEX_RESERVED_BYTES can be used as a user-managed fill factor when it is appropriate to have a different level of sparseness for data and index blocks. The format of the INDEX_RESERVED_BYTES qualifier is:

.. code-block:: none

   [-]-INDEX[_RESERVED_BYTES]=size

The minimum INDEX_RESERVED_BYTES is 0 bytes. The maximum INDEX_RESERVED_BYTES = Block Size - Max Key Size - 40 (Block/Record header overheads). The default INDEX_RESERVED_BYTES is 0 bytes.

^^^^^^^^^^^^^^^^^^^^^^^^
[-]-INST_FREEZE_ON_ERROR
^^^^^^^^^^^^^^^^^^^^^^^^

Enables or disables custom errors in a region to automatically cause an Instance Freeze. This flag modifies the "Inst Freeze on Error" file header flag. The format of the INST_FREEZE_ON_ERROR qualifier is:

.. code-block:: none

   [-]-[NO]INST[_FREEZE_ON_ERROR]

For more information on creating a list of custom errors that automatically cause an Instance Freeze, refer to :ref:`instance-freeze`.

For more information on promptly setting or clearing an Instance Freeze on an instance irrespective of whether any region is enabled for Instance, refer to the :ref:`start-source-server` section of the Database Replication chapter.

.. _set-journal:

^^^^^^^^^^^
[-]-JOURNAL
^^^^^^^^^^^

Specifies whether the database allows journaling and, if it does, characteristics for the journal file.

.. note::
   In regions that have journaling enabled and on, users can switch journal files without either requiring standalone access or freezing updates.

The format of the JOURNAL qualifier is:

.. code-block:: none

   [-]-[NO]J[OURNAL][=journal-option-list]

* NOJOURNAL specifies that the database does not allow journaling. And also it does not accept an argument assignment.

* JOURNAL specifies journaling is allowed. It takes one or more arguments in a journal-option-list.

For detailed description of the all JOURNAL qualifiers and its keywords, refer to :ref:`set-action-qualifiers`.

^^^^^^^^^^^^
[-]-KEY_SIZE
^^^^^^^^^^^^

Specifies the maximum key size in bytes for storing and retrieving data from the global database file. The maximum supported size is 1019 bytes. The format of the KEY_SIZE qualifier is:

.. code-block:: none

   [-]-K[EY_SIZE]=bytes

For more information on KEY_SIZE, refer to :ref:`region-qualifiers`.

^^^^^^^^^^^^^^
[-]-LOCK_SPACE
^^^^^^^^^^^^^^

Specifies the number of pages allocated to the management of M locks associated with the database. The size of a page is always 512 bytes. The format of the LOCK_SPACE qualifier is:

.. code-block:: none

   [-]-L[OCK]_SPACE=integer

* The maximum LOCK_SPACE is 262144 pages.

* The minimum LOCK_SPACE is 10 pages.

* The default LOCK_SPACE is 40 pages.

* For more information on LOCK_SPACE, refer to :ref:`segment-qualifiers`.

* This qualifier requires standalone access.

^^^^^^^^^^^^^^^
[-]-MUTEX_SLOTS
^^^^^^^^^^^^^^^

Sets the size of a structure that YottaDB uses to manage contention for the principal critical section for a database. Performance issues may occur when there are many processes contending for database access and if this structure cannot accommodate all waiting processes. Therefore, YottaDB recommends setting this value to a minimum of slightly more than the maximum number of concurrent processes you expect to access the database.

Note that this qualifier takes effect only if :ref:`mutex_type` is set to ADAPTIVE or YDB. It has no effect if set to PTHREAD.

The minimum value is 64 and the maximum value is 32768. The default value is 1024. The format of the MUTEX_SLOTS qualifier is:

.. code-block:: none

   [-]-MUTEX_S[LOTS]=integer

.. _mutex_type:

^^^^^^^^^^^^^^^
[-]-MUTEX_TYPE
^^^^^^^^^^^^^^^

Sets the mutex manager type for the database file/region. This controls how database critical section acquire and release is managed. There are 3 possible values. PTHREAD, YDB and ADAPTIVE. PTHREAD implies the use of `pthread mutex calls <https://pubs.opengroup.org/onlinepubs/9799919799/functions/pthread_mutex_lock.html>`_) for locking and unlocking (provided by glibc). YDB implies the use of YottaDB's internal mutex manager (which does a mix of hard spins, yields and sleeps while waiting for the database critical section lock). YDB has been seen to perform better than PTHREAD for a variety of workloads but there are some cases where PTHREAD performs better than YDB. ADAPTIVE tries to get the best of both worlds by switching to YDB or PTHREAD mutex type as appropriate based on the current workload. See `critical-section-performance blog post <https://yottadb.com/critical-section-performance-in-r2-04>`_) for more details.

Changing the mutex manager type does not require standalone access to the database. This means one can switch to different mutex types as appropriate for the current workload.

The database file mutex manager type setting is displayed by DSE DUMP FILEHEADER. In case of YDB or PTHREAD, those are displayed as is by DSE as the current mutex manager type can only be the database file mutex manager setting. But in case of ADAPTIVE, the database starts out with YDB mutex and later switches as needed between YDB and PTHREAD. And so, in the ADAPTIVE case, the user specified setting of ADAPTIVE along with the current mutex manager type in effect (YDB or PTHREAD) is displayed by DSE. So it would display one of :code:`ADAPTIVE (YDB)` or :code:`ADAPTIVE (PTHREAD)` depending on whether YDB or PTHREAD are currently in effect.

While we believe that ADAPTIVE is well suited to typical workloads, you should ascertain for yourself what is best for your workloads. Also note that ADAPTIVE relies on :ref:`set-stats` being turned on (which is the default) to help with deciding when to switch between YDB or PTHREAD mutex types.  If :ref:`set-stats` is turned off (using `-NOSTATS`) or if statistics sharing is turned off for all processes accessing the region (through `VIEW "[NO]STATSHARE" <../ProgrammersGuide/commands.html#view-nostatshare>`_) the setting will remain at its initial value when the database file is first opened (which is :code:`ADAPTIVE (YDB)`). Otherwise switching between :code:`ADAPTIVE (YDB)` and :code:`ADAPTIVE (PTHREAD)` will occur based on those processes which are sharing statistics.

One can also obtain the current mutex manager type for a database file using `PEEKBYNAME <../ProgrammersGuide/utility.html#peekbyname-util>`_). For example, :code:`$$^%PEEKBYNAME("mutex_struct.curr_mutex_type","DEFAULT")` would return the current mutex manager type in effect for the DEFAULT region. It would be one of the following values: 0 if ADAPTIVE(YDB), 1 if ADAPTIVE(PTHREAD), 2 if YDB and 3 if PTHREAD.

The format of the MUTEX_TYPE qualifier is:

.. code-block:: none

   [-]-MUTEX_T[YPE]={ADAPTIVE|YDB|PTHREAD}

^^^^^^^^^^^^^^^^^^^
[-]-NULL_SUBSCRIPTS
^^^^^^^^^^^^^^^^^^^

Controls whether YottaDB accepts null subscripts in database keys.

Usage:

.. code-block:: none

   [-]-N[ULL_SUBSCRIPTS]=value

* value can either be T[RUE], F[ALSE], ALWAYS, NEVER, or EXISTING. See GDE chapter for more information on these values of null_subscript.

* Prohibiting null subscripts can restrict access to existing data and cause YottaDB to report errors.

* The default value is never.

^^^^^^^^^^^^^^^^^^^^^^
[-]-LCK_SHARES_DB_CRIT
^^^^^^^^^^^^^^^^^^^^^^

Specifies whether LOCK actions share the same resource and management as the database or use a separate resource and management. The format of the LCK_SHARES_DB_CRIT qualifier is:

.. code-block:: none

    [-]-[NO]LC[K_SHARES_DB_CRIT]

The default is Sep(arate)/FALSE.

For more information, refer to :ref:`region-qualifiers`.

^^^^^^^^^^^^^^^
[-]-PROBLKSPLIT
^^^^^^^^^^^^^^^

Specifies the approximate number of records a block in the region must exceed before YottaDB considers a proactive block split. The format of the PROBLKSPLIT qualifier is:

.. code-block:: none

   [-]-PRO[BLKSPLIT]=integer

  * Proactive consideration occurs when a mini-transaction retries updating a data block more than once due to contention.
  * YottaDB never performs proactive block splitting inside a TP transaction.
  * Proactive block splits tend to make globals require more blocks and potentially deeper trees.
  * A value of one (1) instructs YottaDB to always consider proactive splits, which might be useful for regions used to hold globals with a first subscript of, for example, `$JOB <../ProgrammersGuide/isv.html#job>`_.
  * A value of five (5) indicates that YottaDB considers proactive block split only when the number of records in a block exceed 5.
  * The default is zero (0). It means that YottaDB does not consider proactive block splitting by default.

^^^^^^^^^^^^^^
[-]-QDBRUNDOWN
^^^^^^^^^^^^^^

Shortens normal process shutdown when a large number of processes accessing a database file need to shutdown almost simultaneously, for example, in benchmarking scenarios or emergencies. The format of the QDBRUNDOWN qualifier is:

.. code-block:: none

   [-]-[NO]Q[DBRUNDOWN]

When a terminating YottaDB process observes that a large number of processes are attached to a database file and QDBRUNDOWN is enabled, it bypasses checking whether it is the last process accessing the database. Such a check occurs in a critical section and bypassing it also bypasses the usual RUNDOWN actions which accelerates process shutdown removing a possible impediment to process startup. By default, QDBRUNDOWN is disabled.

Note that with QDBRUNDOWN there is a possibility that the last process to exit might leave the database shared memory and IPC resources in need of cleanup. Except after the number of concurrent processes exceeds 32Ki, QDBRUNDOWN minimizes the possibility of abandoned resources, but it cannot eliminate it. When using QDBRUNDOWN, use an explicit MUPIP command such as RUNDOWN or JOURNAL RECOVER or ROLLBACK of the database file after the last process exits, to ensure the cleanup of database shared memory and IPC resources; not doing so risks database damage.

When a database has QDBRUNDOWN enabled, if the number of attached processes ever exceeds 32Ki, YottaDB stops tracking the number of attached processes, which means that it cannot recognize when the number reaches zero (0) and the shared resources can be released. The process that detects this event issues a NOMORESEMCNT in the system log. This means that an orderly, safe shutdown requires a MUPIP JOURNAL -ROLLBACK -BACKWARD for replicated databases, a MUPIP JOURNAL RECOVER BACKWARD for unreplicated journaled databases and a MUPIP RUNDOWN for journal-free databases.

^^^^^^^^^^^^^^^^^^^^^^^^
[-]-PARTIAL_RECOV_BYPASS
^^^^^^^^^^^^^^^^^^^^^^^^

Sets the CORRUPT_FILE flag in the database file header to FALSE. The CORRUPT_FILE flag indicates whether a region completed a successful recovery. The format of the PARTIAL_RECOV_BYPASS qualifier is:

.. code-block:: none

   [-]-PA[RTIAL_RECOV_BYPASS]

For more information, refer to the :ref:`CORRUPT_FILE <dse-change-corrupt-file>` qualifier.

^^^^^^^^^^^^^
[-]-READ_ONLY
^^^^^^^^^^^^^

Indicates whether YottaDB should treat an MM access method segment as read only for all users, including root. This designation augments Linux authorizations and prevents any state updates that normally might require an operational action for a database with no current accessing (attached) processes. MUPIP emits an error on attempts to set READ_ONLY on databases with the BG access method, or to set the access method to BG on databases with READ_ONLY set. The YottaDB help databases have READ_ONLY set by default. The format of the READ_ONLY qualifier is:

.. code-block:: none

   [-]-[NO]REA[D_ONLY]

.. note::
   When the first process connects to a database, it creates a access-control semaphore as part of the management of the shared resource. However, when processes connect to a READ_ONLY database , each creates a private copy of the in-memory structures for the database and thus a private semaphore.

^^^^^^^^^^^^^^^
[-]-RECORD_SIZE
^^^^^^^^^^^^^^^

Specifies the maximum record size in bytes for storing and retrieving data from the global database file. The maximum supported size is 1MiB bytes. The format of the RECORD_SIZE qualifier is:

.. code-block:: none

   [-]-REC[ORD_SIZE]=bytes

For more information on KEY_SIZE, refer to :ref:`region-qualifiers`.

^^^^^^^^^^^^^^^^^^^^
[-]-REORG_SLEEP_NSEC
^^^^^^^^^^^^^^^^^^^^

Specifies the number of nanoseconds that a MUPIP REORG process operating between blocks takes to process, with default value of 0 and a maximum of 999999999 (i.e. 999,999,999, or 1 nanosecond less than 1 second). Using non-zero values reduces the IO impact of MUPIP REORG, at the cost of increasing the duration of the operation. Note that the existing environment variable ydb_poollimit is the appropriate technique to limit the impact of MUPIP REORG on global buffers; the -reorg_sleep_nsec can be used to limit the impact on the IO subsystem.

^^^^^^^^^^^^^^^^^^
[-]-RESERVED_BYTES
^^^^^^^^^^^^^^^^^^

Specifies the size to be reserved in each database block. RESERVED_BYTES is generally used as a user-managed fill factor. The format of the RESERVED_BYTES qualifier is:

.. code-block:: none

   [-]-RES[ERVED_BYTES]=size


* The minimum RESERVED_BYTES is 0 bytes. The maximum RESERVED_BYTES = Block Size - Max Key Size - 40 (Block/Record header overheads). The default RESERVED_BYTES is 0 bytes.

^^^^^^^^^^^^^^^^^^^^
[-]-SLEEP_SPIN_COUNT
^^^^^^^^^^^^^^^^^^^^

Specifies the number of times a process suspends its activity while waiting to obtain critical sections for shared resources, principally those involving databases. The format of the SLEEP_SPIN_COUNT qualifier is:

Note that this qualifier takes effect only if :ref:`mutex_type` is ADAPTIVE or YDB. It has no effect if set to PTHREAD.

.. code-block:: none

   [-]-SLEEP_SPIN_COUNT=integer

* integer is the number of times the process yields to the OS scheduler or sleeps (depending on the SPIN_SLEEP_LIMIT) after exhausting its hard spin count and before enqueuing itself to be awakened by another process releasing the shared resource mutex.

* The default is 128.

* Except on the advice of your YottaDB support channel, YottaDB recommends leaving the default values unchanged in production environments, until and unless you have data from testing and benchmarking that demonstrates benefits from a change.

^^^^^^^^^^^^^^^^^^^
[-]-SPIN_SLEEP_MASK
^^^^^^^^^^^^^^^^^^^

Specifies the maximum number of nanoseconds for processes to sleep while waiting to obtain critical sections for shared resources, principally those involving databases. The format of the SPIN_SLEEP_MASK qualifier is:

.. code-block:: none

   [-]-SPIN_SLEEP_MASK=hex_mask

* hex_mask is a hexadecimal mask that controls the maximum time (in nanoseconds) the process sleeps on a sleep spin.

* The default is zero (0) which causes the process to return control to the OS kernel to be rescheduled with no explicit delay. When the value is non-zero, the process waits for a random value between zero (0) and the maximum value permitted by the mask.

* Except on the advice of your YottaDB support channel, YottaDB recommends leaving the default values unchanged in production environments, until and unless you have data from testing and benchmarking that demonstrates a benefit from a change.

* This qualifier takes effect only if :ref:`mutex_type` is ADAPTIVE or YDB. It has no effect if set to PTHREAD.

.. _set-stats:

^^^^^^^^^
[-]-STATS
^^^^^^^^^

Specifies whether YottaDB should permit statistics sharing for this region. This characteristic permits operational exclusion of statistics sharing for a region. The format of the STATS qualifier is:

.. code-block:: none

   [-]-[NO]STAT[S]

* At database creation, GDE controls this characteristic, which by default is specified as STATS (on). When on, this characteristic causes YottaDB to create a small MM database for the associated region to hold the shared statistics.
* A process disables itself from maintaining the shared statistics when it fails to open a statsDB. It does not, however, disable subsequently starting processes from maintaining the shared statistics.

^^^^^^^^^^^^^^^^^^^^^^
[-]-STATSDB_ALLOCATION
^^^^^^^^^^^^^^^^^^^^^^

Specifies the initial number of GDS blocks for the shadow stats regions when the database region is first created. This qualifier requires standalone access and a file or region name. The format of the STATSDB_ALLOCATION qualifier is:

.. code-block:: none

   [-]-STATSD[B_ALLOCATION]=integer

  * YottaDB  implicitly doubles this block count each time it needs to extend the associated statsdb.
  * The minimum STATSDB_ALLOCATION is 128 blocks.
  * The maximum STATSDB_ALLOCATION is 8,388,607 blocks.
  * By default, GDE provides an initial STATSDB_ALLOCATION of 2050 blocks for use by MUPIP CREATE.

^^^^^^^^^^^^^^^
[-]-STDNULLCOLL
^^^^^^^^^^^^^^^

Specifies whether YottaDB uses standard or `historical null collation <../ProgrammersGuide/langfeat.html#null-subs-colltn>`_ for null-subscripted keys. YottaDB strongly recommends that you use STDNULLCOLL and not the historical null collation. The format of the STDNULLCOLL qualifier is:

.. code-block:: none

   [-]-[NO]STD[NULLCOLL]

^^^^^^^^^^^^^^^^^
[-]-TRIGGER_FLUSH
^^^^^^^^^^^^^^^^^

Specifies the decimal value, in buffers, for the threshold at which processes start flushing dirty buffers after each update. The format of the TRIGGER_FLUSH qualifier is:

.. code-block:: none

   [-]-T[RIGGER_FLUSH]=integer

^^^^^^^^^^^^^
[-]-WAIT_DISK
^^^^^^^^^^^^^

Specifies the seconds to wait for disk space before giving up on a database block write, where zero (0) means to give an error immediately without waiting. The format of the WAIT_DISK qualifier is:

.. code-block:: none

   [-]-W[AIT_DISK]=seconds

^^^^^^^^^^^^^^^^^^^^
[-]-WRITES_PER_FLUSH
^^^^^^^^^^^^^^^^^^^^

Specifies the decimal number of blocks to write in each flush. The default value is 7. The format of the WRITES_PER_FLUSH qualifier is:

.. code-block:: none

   [-]-WR[ITES_PER_FLUSH]=integer

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
