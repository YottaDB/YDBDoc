.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2017-2024 YottaDB LLC and/or its subsidiaries.#
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

.. index::
   Database Structure Editor

=================================
10. Database Structure Editor
=================================

.. contents::
   :depth: 5

---------------------------
Operating in DSE
---------------------------

The Database Structure Editor, DSE, is primarily a tool for authorized YottaDB consultants to examine and, under unusual circumstances, repair YottaDB Database Structure (GDS) databases. With DSE, it is possible to see and change most of the attributes of a YottaDB database.

DSE gives all possible control over a database and therefore, it may cause irreparable damage when used without knowing the consequences. Therefore, unless you have extensive experience, you should always get guidance from YottaDB or an equivalently knowledgeable support resource before running any DSE command that changes any attribute of any production database or other database you value. However, you can use those DSE commands that let you see the attributes of your database for collecting database metrics and monitoring status.

The YottaDB installation procedure places the DSE utility program in a directory specified by the environment variable ydb_dist.

Invoke DSE using the "dse" command at the shell prompt. If this does not work, consult your system manager to investigate setup and file access issues.

Example:

.. code-block:: bash

   $ydb_dist/dse
   File/usr/name/yottadb.dat
   Region  DEFAULT
   DSE>

DSE displays the DSE> prompt.

You may also specify a command when entering DSE.

By default, DSE starts with the region that stands first in the list of regions arranged in alphabetical order. In the above example, the first region is DEFAULT.

You may also specify a command when entering DSE.

Example:

.. code-block:: bash

   $ydb_dist/dse dump -fileheader

This command displays the fileheader of the region that stands first in the list of regions arranged in alphabetical order and then returns to the shell prompt. To look at other regions, at the DSE prompt you must first issue a FIND -REGION=<desired-region> command. The region name is case-insensitive.

As previously mentioned, DSE provides control over most of the attributes of your database. With DSE, it is possible to examine them and, with a few exceptions, change them.

All DSE commands are divided into two categories - Change commands and Inquiry commands. Change commands allow you to modify the attributes of your database, in most cases without any warning or error. As the low level tool of last resort, Change commands allow you to take certain actions that can cause extensive damage when undertaken without an extensive understanding of the underlying data structures on disk and in memory and with an imperfect understanding of the commands issued. Do not use Change commands unless you know exactly what you are doing and have taken steps to protect yourself against mistakes, both inadvertent and resulting from an incomplete understanding of the commands you issue. Change commands are not required for normal operation, and are usually only used under the direction of YottaDB support to recover from the unanticipated consequences of failures not adequately planned for (for example, you should configure YottaDB applications such that you never need a Change command to recover from a system crash).

Inquiry commands let you see the attributes of your database. You may frequently use the inquiry commands for collecting your database metrics and status reporting.

The list of Change commands is as follows:

.. code-block:: none

   AD[D]
   AL[L]
   B[UFFER_FLUSH]
   CH[ANGE]
   CR[ITICAL]
   REM[OVE]
   RES[TORE]
   SH[IFT]
   W[CINIT]
   OV[ERWRITE]
   M[APS] -BU[SY] -F[REE] -M[ASTER] -R[ESTORE_ALL]

The list of Inquiry commands is as follows:

.. code-block:: none

   CL[OSE]
   D[UMP]
   EV[ALUATE]
   EX[IT]
   F[IND]
   H[ELP]
   I[NTEGRIT]
   M[APS] -BL[OCK]
   OP[EN]
   P[AGE]
   RA[NGE]
   SA[VE]
   SP[AWN]

Although DSE can operate concurrently with other processes that access the same database file, YottaDB strongly recommends using DSE in standalone mode when using Change commands. Some DSE operations can adversely impact the database when they occur during active use of the database. Other DSE operations may be difficult to perform in a logically sound fashion because a DSE operator works on a block at a time, while normal database operations update all related blocks almost simultaneously.

.. note::
   When DSE attaches to a database with a version that does not match the DSE version, DSE issues an informational message and continues. At this point, you should exit DSE and find the version of DSE that matches the database. You should continue after this warning if and only if you are certain that the DSE is indeed from the YottaDB version that has the database open (and hence the error results from a damaged database file header or shared memory that you intend to repair, following instructions from YottaDB).

Use the DSE EXIT, or QUIT command to leave DSE.

.. note::
   DSE sends its output to stderr not stdout. On shells such as :code:`bash` stderr can be redirected to stdout by `specifying 2>&1 on the command line <https://www.gnu.org/software/bash/manual/bash.html#Redirecting-Standard-Output-and-Standard-Error>`_.

.. note::
   All command line parameters which accept decimal values as inputs also accept hexadecimal values. Hexadecimal values must be prefixed with ``0x`` or ``0X`` and digits greater than 9 are case insensitive. However, some command line parameters accept only hexadecimal values and prohibit decimal values.

----------------------------
DSE Commands and Qualifiers
----------------------------

The general format of DSE commands is:

.. code-block:: none

   command [-qualifier[...]] [object[,...]]

DSE interprets all numeric input as hexadecimal, except for time values, the values for the following qualifiers when used with CHANGE -FILEHEADER: -BLK_SIZE=, DECLOCATION=, -KEY_MAX_SIZE=, -RECORD_MAX_SIZE, -REFERENCE_COUNT=, -TIMERS_PENDING and -WRITES_PER_FLUSH, and the value for -VERSION= when used with the REMOVE and RESTORE commands. These conventions correspond to the displays provided by DSE and by MUPIP INTEG.

.. _dse-add:

+++++++++++
ADD
+++++++++++

Adds a record to a block. The format of the ADD command for blocks with a level greater than zero (0) is:

.. code-block:: none

   ADD [-B[LOCK]=[block] {-OFFSET=offset|-RECORD=record} -STAR -POINTER=block

or

.. code-block:: none

   ADD [-B[LOCK]=[block] {-OFFSET=offset|-RECORD=record} -KEY=key -POINTER=pointer

The format of the ADD command for level 0 blocks is:

.. code-block:: none

   ADD [-B[LOCK]=[block] {-OFFSET=offset|-RECORD=record} -KEY=key -DATA=string

The ADD command requires either the -OFFSET or -RECORD qualifier to position the record in the block, and either the -KEY or the -STAR qualifier to define the key for the block.

The -STAR qualifier is invalid at level 0 (a data block). The ADD command requires the -DATA qualifier at level 0 or the -POINTER qualifier at any other level to provide record content.

~~~~~~~~~~~~~~~~~
Qualifiers of ADD
~~~~~~~~~~~~~~~~~

^^^^^^^^^^^^^^^^^^^^^
-B[LOCK]=block-number
^^^^^^^^^^^^^^^^^^^^^

Specifies the block to receive the new record.

On commands with no -BLOCK= qualifier, DSE uses the last block handled by a DSE operation. When no block has been accessed, that is, on the first block-oriented command, DSE uses block one (1).

^^^^^^^^^^^^^^
-D[ATA]=string
^^^^^^^^^^^^^^

Specifies the data field for records added to a data block. Use quotation marks around the string and escape codes of the form \\ab, where "a" and "b" are hexadecimal digits representing non-printing characters. \\\\ translates to a single backslash. \\\"\" translates to a NULL value.

Incompatible with: -STAR,-POINTER

^^^^^^^^^^
-K[EY]=key
^^^^^^^^^^

Specifies the key of the new record. Enclose M-style global references, including the leading caret symbol (^), in quotation marks (" ").

Incompatible with: -STAR

^^^^^^^^^^^^^^^^
-O[FFSET]=offset
^^^^^^^^^^^^^^^^

Adds the new record at the next record boundary after the specified offset.

Incompatible with: -RECORD, -STAR

^^^^^^^^^^^^^^^^^^
-P[OINTER]=pointer
^^^^^^^^^^^^^^^^^^

Specifies the block pointer field for records added to an index block. The -POINTER qualifier cannot be used at level 0. Note that this means that to add pointers at level 0 of the Directory Tree, you must specify a string of bytes or temporarily change the block level.

Incompatible with: -DATA

^^^^^^^^^^^^^^^^^^^^^^^
-R[ECORD]=record-number
^^^^^^^^^^^^^^^^^^^^^^^

Specifies a record number of the new record.

Incompatible with: -OFFSET,-STAR

^^^^^^^
-S[TAR]
^^^^^^^

Adds a star record (that is, a record that identifies the last record in an indexed block) at the end of the specified block. The -STAR qualifier cannot be used at level 0.

Incompatible with: -DATA,-KEY,-OFFSET,-RECORD

~~~~~~~~~~~~~~~~
Examples for ADD
~~~~~~~~~~~~~~~~

.. code-block:: bash

   DSE>add -block=6F -record=57 -key="^Capital(""Mongolia"")" -data="Ulan Bator"

This command adds a new record with key ^Capital("Mongolia") at the specified location. Note that this command is applicable to level 0 blocks only.

Example:

.. code-block:: bash

   DSE>add -star -bl=59A3 -pointer=2

This command adds a star record in block 59A3. Note that this command is applicable to blocks > level 0.

Example:

.. code-block:: bash

   DSE>add -block=3 -record=4 -key="^Fruits(4)" -data="Grapes"

Suppose your database has 3 global nodes -- ^Fruits(1)="Apple", ^Fruits(2)="Banana", and ^Fruits(3)="Cherry". The above command adds a new node -- ^Fruits(4)="Grapes" at record 4. Note that this command is applicable to level 0 blocks only. The interpreted output as a result of the above command looks like the following:

.. code-block:: none

   Block 3   Size 4B   Level 0   TN 4 V6
   Rec:1  Blk 3  Off 10  Size 14  Cmpc 0  Key ^Fruits(1)
         10 : | 14  0  0  0 46 72 75 69 74 73  0 BF 11  0  0 41 70 70 6C 65|
              |  .  .  .  .  F  r  u  i  t  s  .  .  .  .  .  A  p  p  l  e|
   Rec:2  Blk 3  Off 24  Size D  Cmpc 8  Key ^Fruits(2)
         24 : |  D  0  8  0 21  0  0 42 61 6E 61 6E 61                     |
              |  .  .  .  .  !  .  .  B  a  n  a  n  a                     |
   Rec:3  Blk 3  Off 31  Size D  Cmpc 8  Key ^Fruits(3)
         31 : |  D  0  8  0 31  0  0 43 68 65 72 72 79                     |
              |  .  .  .  .  1  .  .  C  h  e  r  r  y                     |
   Rec:4  Blk 3  Off 3E  Size D  Cmpc 8  Key ^Fruits(4)
         3E : |  D  0  8  0 41  0  0 47 72 61 70 65 73                     |
              |  .  .  .  .  A  .  .  G  r  a  p  e  s                     |

Example:

.. code-block:: bash

   $dse add -star -bl=1 -pointer=2

This command adds a star record in block 1. Note that this command is applicable to blocks > Level 0.

Example:

.. code-block:: bash

   $ dse add -block=4 -key="^Vegetables" -pointer=7 -offset=10

This command creates a block with key ^Vegetables pointing to block 7.

Example:

.. code-block:: bash

   DSE> add -record=2 -key="^foo" -data=' '

This example adds a new node (set ^foo="") as the second record of the current database block.

.. _dse-all:

++++++++
ALL
++++++++

Applies action(s) specified by a qualifier to all GDS regions defined by the current global directory.

The format of the ALL command is:

.. code-block:: none

   AL[L]
   [
   -B[UFFER_FLUSH]
   -C[RITINIT]
   -D[UMP] -A[LL]
   -[NO]F[REEZE]
   -O[VERRIDE]]
   -REF[ERENCE]
   -REL[EASE]
   -REN[EW]
   -S[EIZE]
   -W[CINIT]
   ]


* This is a very powerful command; use it with caution.

* Be especially careful if you have an overlapping database structure (for example, overlapping regions accessed from separate application global directories).

* If you use this type of database structure, you may need to construct special Global Directories that exclude overlapped regions to use with DSE.

~~~~~~~~~~
Qualifiers
~~~~~~~~~~

.. _qual-all:

^^^^
-ALL
^^^^

Displays additional information on the database most of which is useful for YottaDB in diagnosing issues.

Meaningful only with: -D[UMP]

^^^^^^^^^^^^^
-BUFFER_FLUSH
^^^^^^^^^^^^^

Flushes to disk the file header and all pooled buffers for all regions of the current global directory.

Incompatible with: -RENEW

^^^^^^^^^^^
-C[RITINIT]
^^^^^^^^^^^

Initializes critical sections for all regions of the current directory.

Incompatible with: -RENEW, -RELEASE, -SIEZE

.. note::
   Never use CRITINIT while concurrent updates are in progress as doing so may damage the database.

^^^^^^^
-D[UMP]
^^^^^^^

Displays fileheader information.

Compatible with: -A[LL]

^^^^^^^^^^^^^
-[NO]F[REEZE]
^^^^^^^^^^^^^

Freezes or prevents updates on all regions of the current global directory.

* The FREEZE qualifier freezes all GDS regions except those previously frozen by another process. Regions frozen by a particular process are associated with that process.
* A frozen region may be unfrozen for updates in one of two ways: The process which froze the region may unfreeze it with the -NOFREEZE qualifier; or another process may override the freeze in conjunction with the -OVERRIDE qualifier. For more information on a preferred method of manipulating FREEZE, refer to “FREEZE ”.
* By default, the -NOFREEZE qualifier unfreezes only those GDS regions that were previously frozen by a process. Once a region is unfrozen, it may be updated by any process. To unfreeze all GDS regions of the Global Directory, use the -OVERRIDE qualifier.
* DSE releases any FREEZE it holds when it exits, therefore, use the same DSE invocation or SPAWN to perform operations after executing the ALL -FREEZE command.

Incompatible with: -RENEW

^^^^^^^^^^^
-O[VERRIDE]
^^^^^^^^^^^

Overrides the ALL -FREEZE or ALL -NOFREEZE operation.

When used with -NOFREEZE, -OVERRIDE unfreezes all GDS regions, including those frozen by other users.

When used with -FREEZE, -OVERRIDE freezes all GDS regions, including those frozen by other processes, associating all such freezes with the current process. The current process must then use -NOFREEZE to unfreeze the database; any other process attempting a -NOFREEZE should also have to include the -OVERRIDE qualifier.

Meaningful only with: [NO]FREEZE

^^^^^^^^^^^^
-REF[ERENCE]
^^^^^^^^^^^^

Resets the reference count field to 1 for all regions of the current global directory.

* A Reference count is a file header element field that tracks how many processes are accessing the database with read/write permissions.
* This qualifier is intended for use when DSE is the only process attached to the databases of the current global directory. Using it when there are other users attached produces an incorrect value.

Incompatible with: -RENEW

^^^^^^^^^^
-REL[EASE]
^^^^^^^^^^

Releases critical sections for all regions of the current global directory.

Incompatible with: -CRITINIT, -RENEW, -SEIZE

^^^^^^^^
-REN[EW]
^^^^^^^^

Reinitializes the critical sections (-CRITICAL) and buffers (-WCINIT), resets reference counts (-REFERENCE_COUNT) to 1, and clears freeze (-NOFREEZE) for all regions of the current global directory .

* -RENEW requires confirmation.
* The RENEW action will cause all current accessors of the affected database regions to receive a fatal error on their next access attempt.
* This operation is dangerous, drastic, and is a last resort if multiple databases have hangs that have not yielded to other resolution attempts; there is almost never a good reason to use this option.

^^^^^^^^
-S[EIZE]
^^^^^^^^

Seizes the critical section for all regions of the current global directory. The -SEIZE qualifier is useful when you encounter a DSEBLKRDFAIL error, generated when DSE is unable to read a block from the database.

Incompatible with: -RENEW, -RELEASE, -CRITINIT

^^^^^^^^^
-W[CINIT]
^^^^^^^^^

Reinitializes the buffers for all regions of the current global directory.

-WCINIT requires confirmation.

.. note::
   This operation is likely to cause database damage when used while concurrent updates are in progress.

Incompatible with: -RENEW

~~~~~~~~~~~~~~~
Examples of ALL
~~~~~~~~~~~~~~~

Example:

.. code-block:: bash

   DSE> all flush -buffer_flush

This command flushes the file header and cache buffers to disk for all regions.

Example:

.. code-block:: bash

   DSE> ALL -CRITINIT

This command initializes critical sections for all regions of the current directory.

Example:

.. code-block:: bash

   DSE> ALL -FREEZE
   DSE> SPAWN "yottadb -dir"

The first command freezes all regions of the current global directory. The second command creates a child (shell) process and executes the "yottadb -dir" command. Then type S ^A=1 at the prompt. Notice that the command hangs because of the DSE FREEZE in place.

Example:

.. code-block:: bash

   DSE> ALL -NOFREEZE -OVERRIDE

This command removes the FREEZE on all current region including the FREEZE placed by other users.

Example:

.. code-block:: bash

   DSE> ALL -REFERENCE

This command sets the reference count field in the file header(s) to 1.

Example:

.. code-block:: bash

   DSE> ALL -RELEASE

This command releases critical sections owned by the current process for all regions of the current global directory.

Example:

.. code-block:: bash

   DSE> ALL -RENEW

This command reinitializes critical sections, buffers, resets the reference count to 1, and clears freeze for all regions of the current global directory.

Example:

.. code-block:: bash

   DSE> ALL -SEIZE

This command seizes all critical sections for all regions of the current global directory.

Example:

.. code-block:: bash

   DSE> ALL -WCINIT

This command reinitializes the buffers for all regions of the current global directory.

.. _dse-buffer-flush:

+++++++++++++++
BUFFER_FLUSH
+++++++++++++++

Flushes the file header and the current region's buffers to disk.

The format of the BUFFER_FLUSH command is:

.. code-block:: none

   B[UFFER_FLUSH]

The BUFFER_FLUSH command has no qualifiers.

.. _dse-cache:

+++++++++++++
CACHE
+++++++++++++

Operates on the cache of a database having BG access method. The format of the CACHE command is:

.. code-block:: none

   CA[CHE]
   [
   -ALL
   -RE[COVER]
   -SH[OW]
   -VE[RIFY]
   ]

~~~~~~~~~~~~~~~~~~~
Qualifiers of CACHE
~~~~~~~~~~~~~~~~~~~

^^^^^^^^^^^^^^^^^
-RE[COVER] [-ALL]
^^^^^^^^^^^^^^^^^

Resets the cache of a database having BG access method to a "clean" state.

* With -ALL specified, DSE includes all region of the current global directory for cache recovery.
* Attempt DSE CACHE -RECOVER only if a DSE CACHE -VERIFY commands reports the cache is "NOT clean".

^^^^^^^
-SH[OW]
^^^^^^^

Displays the cache data structure information. All values are in 8-byte hexadecimal form. If the database has encryption turned on, SHOW additionally displays an element that gives information about the encrypted global buffer section in shared memory.

^^^^^^^^^^^^^^^^
-VE[RIFY] [-ALL]
^^^^^^^^^^^^^^^^

Verifies the integrity of the cache data structures as well as the internal consistency of any GDS blocks in the global buffers of the current region.

* With -ALL specified, DSE performs cache verification on all regions of the current global directory.
* It reports the time, the region and a boolean result indicating whether the cache is clean or NOT clean. If you see "NOT clean" in report, execute DSE CACHE -RECOVER as soon as possible to reset the cache in a clean state.

~~~~~~~~~~~~~~~~~~
Examples for CACHE
~~~~~~~~~~~~~~~~~~

Example:

.. code-block:: bash

   DSE> CACHE -VERIFY

This command checks the integrity of the cache data structures as well as the internal consistency of GDS blocks in the global buffers of the current region.

Example:

.. code-block:: bash

   DSE> CACHE -VERIFY -ALL
   Time 26-FEB-2011 14:31:30 : Region DEFAULT : Cache verification is clean
   Execute CACHE recover command if Cache verification is "NOT" clean.

This command reports the state of database cache for all regions.

Example:

.. code-block:: bash

   DSE> CACHE -RECOVER

This command reinitializes the cache data structures of the current region and reverts the cache of a database having BG access to "clean" state.

Example:

.. code-block:: bash

   DSE> CACHE -SHOW
   File    /home/jdoe/node1/areg.dat
   Region  AREG
   Region AREG : Shared_memory       = 0x00002B6845040000
   Region AREG :  node_local         = 0x0000000000000000
   Region AREG :  critical           = 0x0000000000010000
   Region AREG :  shmpool_buffer     = 0x0000000000023000
   Region AREG :  lock_space         = 0x0000000000125000
   Region AREG :  cache_queues_state = 0x000000000012A000
   Region AREG :  cache_que_header   = 0x000000000012A030 : Numelems = 0x00000407 : Elemsize = 0x00000098
   Region AREG :  cache_record       = 0x0000000000150458 : Numelems = 0x00000400 : Elemsize = 0x00000098
   Region AREG :  global_buffer      = 0x0000000000177000 : Numelems = 0x00000400 : Elemsize = 0x00000400
   Region AREG :  db_file_header     = 0x0000000000277000
   Region AREG :  bt_que_header      = 0x00000000002B7000 : Numelems = 0x00000407 : Elemsize = 0x00000040
   Region AREG :  th_base            = 0x00000000002C71D0
   Region AREG :  bt_record          = 0x00000000002C7200 : Numelems = 0x00000400 : Elemsize = 0x00000040
   Region AREG :  shared_memory_size = 0x00000000002D8000
   DSE>

.. _dse-change:

++++++++++++++
CHANGE
++++++++++++++

The CHANGE command changes fields of a block, file, or record header.

The format of the CHANGE command is:

.. code-block:: none

   CH[ANGE]

The CHANGE command either has a -FILEHEADER qualifier or an implicit or explicit -BLOCK qualifier, plus one or more of their associated qualifiers, to define the target of the change.

-BL[OCK]=block-number and one or more of the following qualifiers:

.. code-block:: none

   -BS[IZ]=block-size
   -L[EVEL]=level
   -TN[=transaction-number]
   -OF[FSET]=offset
   -RE[CORD]=record-number
   -CM[PC]=compression-count
   -RS[IZ]=record-size

or

-F[ILEHEADER] and one or more of the following qualifiers:

.. code-block:: none

   -AB[ANDONED_KILLS]=value
   -AVG_BLKS_READ=Average-blocks-read
   -B_B[YTESTREAM]=transaction-number
   -B_C[OMPREHENSIVE]=transaction-number
   -B_D[ATABASE]=transaction-number
   -B_I[NCREMENTAL]=transaction-number
   -B_R[ECORD]=transaction-number
   -BLK_SIZE=block-size
   -BLO[CKS_FREE]=free-blocks
   -CU[RRENT_TN]=transaction-number
   -COM[MITWAIT_SPIN_COUNT]=boolean
   -DEC[LOCATION]=value
   -DEF[_COLLATION]=value
   -ENCRYPTION_HASH
   -FL[USH_TIME][=delta-time]
   -FR[EEZE]=value
   -FU[LLY_UPGRADED]=boolean
   -GV[STATSRESET]
   -HARD_SPIN_COUNT=Mutex-hard-spin-count
   -[HEXLOCATION]=value
   -INT[ERRUPTED_RECOV]=boolean
   -JNL_YIELD_LIMIT=journal-yeild-limit
   -KE[Y_MAX_SIZE]=key-max-size
   -KI[LL_IN_PROG]=value
   -M[ACHINE_NAM]=value
   -N[ULL_SUBSCRIPTS]=value
   -NO[CRIT]
   -OV[ERRIDE]
   -Q[DBRUNDOWN]
   -RC_SRV_COUNT
   -RE_READ_TRIGGER=read-trigger
   -REC[ORD_MAX_SIZE]=record-max-size
   -REF[ERENCE_COUNT]=reference-count
   -REG[_SEQNO]=sequence-number
   -RESERVED_BYTES=reserved-bytes
   -SLEE[P_SPIN_COUNT]=mutex-sleep-spin-count
   -SPIN[_SLEEP_MASK]=mutex-spin-sleep-mask
   -STRM_NUM=stream-number STRM_REG_SEQNO=hexa
   -TIM[ERS_PENDING]=integer
   -TO[TAL_BLKS]=total-blocks
   -TR[IGGER_FLUSH]=trigger-flus
   -UPD_RESERVED_AREA=reserved-area
   -UPD_WRITER_TRIGGER_FACTOR=trigger-factor
   -W[RITES_PER_FLUSH]=writes-per-flush
   -WAIT_DISK=wait-disk
   -Zqgblmod_S[EQNO]=sequence-number
   -Zqgblmod_TN=database-transaction-number

~~~~~~~~~~~~~~~~~~~~~~~~
CHANGE -BLOCK Qualifiers
~~~~~~~~~~~~~~~~~~~~~~~~

This section describes -BLOCK and all of its qualifiers.

^^^^^^^^^^^^^^^^^^^^^
-BL[OCK]=block_number
^^^^^^^^^^^^^^^^^^^^^

Specifies the block to modify. The -BLOCK qualifier is incompatible with the -FILEHEADER qualifier and all qualifiers related to -FILEHEADER.

-BLOCK is the default qualifier. On commands with neither a -BLOCK nor a -FILEHEADER qualifier, DSE uses the last block handled by a DSE operation. When no block has been accessed, that is, on the first block-oriented command, DSE uses block one (1).

Incompatible with: -FILEHEADER and qualifiers used with -FILEHEADER

The following qualifiers operate on a block header.

^^^^^^^^^^^^^^^^^^
-BS[IZ]=block_size
^^^^^^^^^^^^^^^^^^

Changes the block size field of the specified block.

* block_size is in hexadecimal form.
* Decreasing the block size can result in the loss of existing data.

.. note::
   The block size must always be less than or equal to the block size in the file header.

Use only with: -BLOCK, -LEVEL, -TN

^^^^^^^^^^^^^^
-L[EVEL]=level
^^^^^^^^^^^^^^

Changes the level field for the specified block.

.. note::
   DSE lets you change the level of a bitmap block to -1 (the value of the level for a bitmap block) when the bitmap level gets corrupted and takes on an arbitrary value. Note that you should specify -1 in hexadecimal form, that is, FF.

Use only with: -BLOCK, -BSIZ, -TN

Example:

.. code-block:: bash

   DSE> change -level=FF

^^^^^^^^^^^^^^^^^^^^^^^^
-TN[=transaction_number]
^^^^^^^^^^^^^^^^^^^^^^^^

Changes the transaction number for the current block.

* When a CHANGE command does not include a -TN=, DSE sets the transaction number to the current transaction number.
* Manipulation of the block transaction number affects MUPIP BACKUP -BYTESTREAM, and -ONLINE.

Use only with: -BLOCK, -BSIZ, -LEVEL

^^^^^^^^^^^^^^^^
-OF[FSET]=offset
^^^^^^^^^^^^^^^^

Specifies the offset, in bytes, of the target record within the block. If the offset does not point to the beginning of a record, DSE rounds down to the last valid record start (for example, CHANGE -OFFSET=10 starts at -OFFSET=A, if that was the last record).

Use only with: -BLOCK, -CMPC, and -RSIZ.

^^^^^^^^^^^^^^^^^^^^^^^
-RE[CORD]=record_number
^^^^^^^^^^^^^^^^^^^^^^^

Specifies the record number of the target record.

Use only with: -BLOCK, -CMPC, and -RSIZ.

^^^^^^^^^^^^^^^^^^^^^^^^^
-CM[PC]=compression_count
^^^^^^^^^^^^^^^^^^^^^^^^^

Change the compression count field of the specified record.

* The compression count specifies the number of bytes at the beginning of a key that are common to the previous key in the same block.
* Because compression counts propagate from the "front" of the block, this can potentially change the keys of all records following it in the block. If the goal is to change only a single record, it may be preferable to add a new record and remove the old one.

Use only with: -BLOCK, -RECORD, -OFFSET, -RSIZE

^^^^^^^^^^^^^^^^^^^
-RS[IZ]=record_size
^^^^^^^^^^^^^^^^^^^

Changes the record size field of the specified record.

.. note::
   Changing -RSIZ impacts all records following it in the block.

Use only with: -BLOCK, -RECORD, -CMPC, -OFFSET

Example:

.. code-block:: bash

   DSE> change -record=3 -rsiz=3B -block=2

This command changes the record size of record 3 block 2 to 59 (Hex: 3B) bytes.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CHANGE -FILEHEADER Qualifiers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This section describes the -FILEHEADER qualifier and the other qualifiers that operate on a file header.

^^^^^^^^^^^^^
-FI[LEHEADER]
^^^^^^^^^^^^^

Modifies a file header element that you specify with an associated qualifier.

Incompatible with: -BSIZ, -CMPC, -TN, -LEVEL, -OFFSET, -RECORD, -RSIZ

^^^^^^^^^^^^^^^^^^^^^^^^
-AB[ANDONED_KILLS]=value
^^^^^^^^^^^^^^^^^^^^^^^^

Changes the value of the Abandoned Kills field. The value can be "NONE" or a decimal positive integer.

Use only with: -FILEHEADER

^^^^^^^^^^^^^^^^^^^^^^
-BLK[_SIZE]=block_size
^^^^^^^^^^^^^^^^^^^^^^

Changes the decimal block size field of the current file.

* DSE does not allow you to change the block size to any arbitrary value. It always rounds the block size to the next higher multiple of 512.
* Use the CHANGE -BLK_SIZE qualifier only upon receiving instructions from YottaDB and only in conjunction with the -FILEHEADER qualifier. This DSE command cannot change the working block size of a database and is useful only under very limited and extrordinary circumstances. If you need to change the block size on a database file, unload the data with MUPIP EXTRACT (or an appropriate alternative), change the global directory with GDE to specify the new block size, recreate the database with MUPIP CREATE and reload the data with MUPIP LOAD (or an appropriate alternative).

Use only with: -FILEHEADER

^^^^^^^^^^^^^^^^^^^^^^^^^^
-BLO[CKS_FREE]=free blocks
^^^^^^^^^^^^^^^^^^^^^^^^^^

Changes the hexadecimal free blocks field of the current file.

Use this to correct a value that MUPIP INTEG reports as needing a correction, but note that the "correct" value reported by INTEG may go out-of-date with the next update. It may be necessary to calculate a delta value from the INTEG report, FREEZE the region with DSE, DUMP the current -FILEHEADER value, then apply the delta and CHANGE the -BLOCKS_FREE, and finally turn -OFF the FREEZE.

Use only with: -FILEHEADER

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-B[YTESTREAM]=transaction_number
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Changes the transaction number in the file header of the last incremental backup to the value specified. Use this qualifier only in conjunction with the -FILEHEADER qualifier. For compatibility issues with prior versions, this can still be specified as -B_COMPREHENSIVE.

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-D[ATABASE]=transaction_number
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Changes the hexadecimal transaction number in the file header of the last comprehensive backup to the value specified. Use this qualifier only in conjunction with the -FILEHEADER qualifier. For compatibility issues with prior versions, this can still be specified as -B_COMPREHENSIVE.

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-B_R[ECORD]=transaction_number
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Changes the hexadecimal transaction number in the file header field that maintains this information about the last -RECORD backup.

.. _dse-change-corrupt-file:

^^^^^^^^^^^^^^^^^^^^^^^
-CO[RRUPT_FILE]=boolean
^^^^^^^^^^^^^^^^^^^^^^^

Indicates whether or not a region completed a successful recovery with the MUPIP JOURNAL -RECOVER command. Possible values are: T[RUE] or F[ALSE].

Changing this flag does not correct or cause database damage. When CORRUPT_FILE is set to TRUE, the DSE DUMP command displays a message like the following:

.. code-block:: bash

   %YDB-W-DBFLCORRP, /home/ydbnode1/yottadb.dat Header indicates database file is corrupt

.. note::
   After a CHANGE -FILEHEADER -CORRUPT=TRUE, the file is unavailable to future YottaDB access other than DSE. Under normal conditions, there should never be a need to change this flag manually. A MUPIP SET -PARTIAL_BYPASS_RECOV sets this flag to false.

Use only with: -FILEHEADER

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-COM[MITWAIT_SPIN_COUNT]=value
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Specifies the decimal number of times a YottaDB process waiting for control of a block to complete a block update should spin before yielding the CPU when YottaDB runs on SMP machines. When run on a uniprocessor system, YottaDB ignores this parameter. On SMP systems, when a process needs a critical section that another process has, and critical sections are short (as they are by design in YottaDB), spinning a little with the expectation that the process with the critical section will release it shortly provides a way to enhance performance at the cost of increased CPU usage. Eventually, a process awaiting a critical section yields the CPU if a little spinning does not get it the needed critical section. Note that on heavily loaded systems, increasing COMMITWAIT_SPIN_COUNT may not trade off CPU for throughput, but may instead degrade both. If you set the COMMITWAIT_SPIN_COUNT to 0, the waiting process performs a sequence of small sleeps instead of the spins or yields.

The default value is 16.

Use only with: -FILEHEADER

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-CU[RRENT_TN]=transaction_number
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Changes the hexadecimal current transaction number for the current region.

* Raising the -CURRENT_TN can correct "block transaction number too large" errors
* This qualifier has implications for MUPIP BACKUP -INCREMENTAL and -ONLINE.
* Used with the -BLOCK qualifier, CURRENT_TN places a transaction number in a block header.

Use only with: -FILEHEADER

^^^^^^^^^^^^
-DECLOCATION
^^^^^^^^^^^^

Specifies an offset with the file header. If -VALUE is specified (in decimal), YottaDB puts it at that location.

Use only with: -FILEHEADER

^^^^^^^^^^^^^^^^^^
-E[NCRYPTION_HASH]
^^^^^^^^^^^^^^^^^^

Changes the hash of the password stored in the database file header if and when you change the hash library. For more information on key management and reference implementation, refer to `Chapter 12: “Database Encryption” <./encryption.html>`_.

.. note::
   An incorrect hash renders the database useless.

Use only with: -FILEHEADER

^^^^^^^^^^^^^^^
-[NO]EPOCHTAPER
^^^^^^^^^^^^^^^

Sets a flag that indicates whether or not epoch tapering should be done. The default value is -EPOCHTAPER.

For more information, refer to “Region Qualifiers”.

^^^^^^^^^^^^^^^^^^^^^^^^^^
-FL[USH_TIME][=delta_time]
^^^^^^^^^^^^^^^^^^^^^^^^^^

Changes the flush_time default interval (in delta_time).

* The time entered must be between zero and one hour. Input is interpreted as decimal.
* A -FLUSH_TIME with no value resets the -FLUSH_TIME to the default value (one second for BG and 30 seconds for MM).
* The units of delta_time are hours:minutes:seconds:centi-seconds (hundredths of a second). For example, to change the flush time interval to a second, delta_time would be 00:00:01:00. To change it to 30 minutes, delta_time would be 00:30:00:00. Valid values for the qualifier are one centi-second to one hour.

Use only with: -FILEHEADER

^^^^^^^^^^^^^^^
-FR[EEZE]=value
^^^^^^^^^^^^^^^

Sets the availability of the region for updates. Possible values are: T[RUE] or F[ALSE]. Use to "freeze" (disable database writes) or "unfreeze" the database.

Use only with: -FILEHEADER

For information about a preferred method of manipulating FREEZE, refer to :ref:`mupip-extract-freeze`.

DSE releases -FREEZE when it EXITs. To hold the database(s), CHANGE -FILEHEADER -FREEZE=TRUE and then SPAWN to perform other operations.

^^^^^^^^^^^^^^^^^^^^^^^^^
-FU[LLY_UPGRADED]=boolean
^^^^^^^^^^^^^^^^^^^^^^^^^

Sets a flag that indicates whether or not the database was fully upgraded to the latest version. The value is either T[RUE] or F[ALSE].

Use only with: -FILEHEADER

^^^^^^^^^^^^^^^
-GV[STATSRESET]
^^^^^^^^^^^^^^^

Resets all the database file header global access statistics to 0. Note that this erases all statistics previously accumulated in the database file header.

Use only with: -FILEHEADER

^^^^^^^^^^^^
-HEXLOCATION
^^^^^^^^^^^^

Specifies a hexadecimal offset with the file header. If -VALUE is specified, YottaDB puts it at that location.

Use only with: -FILEHEADER

^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-INT[ERRUPTED_RECOV]=boolean
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Sets a flag that indicates whether or not a recovery with the MUPIP JOURNAL -RECOVER command was interrupted. The value is either T[RUE] or F[ALSE].

Use only with: -FILEHEADER

^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-K[EY_MAX_SIZE]=key_max_size
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Changes the decimal value for the maximum allowable key size. Reducing KEY_MAX_SIZE can restrict access to existing data and cause YottaDB to report errors. Do not create incompatible key and record sizes.

Before permanently changing the key size using DSE, use GDE to check that the appropriate Global Directory contains the same key size for the region. This prepares for future MUPIP CREATEs and performs a consistency check on the key and record size values. For more information on key and record sizes, refer to `Chapter 4: “Global Directory Editor” <./gde.html>`_.

Use only with: -FILEHEADER

^^^^^^^^^^^^^^^^^^^^^
-KI[LL_IN_PROG]=value
^^^^^^^^^^^^^^^^^^^^^

Changes the value of the KILLs in progress field. The value can be "NONE" or a positive decimal integer.

Use only with: -FILEHEADER

^^^^^^^^^^^^^^^^^^^^^^^^
-N[ULL_SUBSCRIPTS]=value
^^^^^^^^^^^^^^^^^^^^^^^^

Controls whether YottaDB accepts null subscripts in database keys.

* value can either be T[RUE], F[ALSE], ALWAYS, NEVER, or EXISTING. See the `GDE chapter <./gde.html>`_ for more information on these values of null_subscripts.
* Prohibiting null subscripts can restrict access to existing data and cause YottaDB to report errors.
* The default value is never.
* DSE cannot change the null subscript collation order. Instead, use GDE to change the null subscript collation order, MUPIP EXTRACT the current content, MUPIP CREATE the database file(s) with the updated collation and MUPIP LOAD the content.

Use only with: -FILEHEADER

^^^^^^^^^^^
-OV[ERRIDE]
^^^^^^^^^^^

Releases or "steals" a FREEZE owned by another process.

Use only with: -FREEZE

^^^^^^^^^^^^^^^^^
-[NO]Q[DBRUNDOWN]
^^^^^^^^^^^^^^^^^

Sets a flag that indicates whether or not the database is enabled for quick rundown. The default value is -NOQDBRUNDOWN.

For more information, refer to :ref:`region-qualifiers`.

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-REC[ORD_MAX_SIZE]=record_max_size
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Changes the decimal value for the maximum allowable record size. Use the -RECORD_MAX_SIZE qualifier only in conjunction with the -FILEHEADER qualifier. Reducing RECORD_MAX_SIZE can restrict access to existing data and cause YottaDB to report errors. Do not create incompatible key and record sizes.

Before making a permanent change to the records size using DSE, use GDE to check that the appropriate Global Directory contains the same record size for the region. This prepares for future MUPIP CREATEs and performs a consistency check on the key and record size values. For more information on key and record sizes, refer to `Chapter 4: “Global Directory Editor” <./gde.html>`_.

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-REF[ERENCE_COUNT]=reference_count
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Sets a field that tracks how many processes are accessing the database with read/write permissions. MUPIP INTEG and DSE use decimal numbers for -REFERENCE_COUNT. To accurately determine the proper reference count, restrict CHANGE -FILEHEADER -REFERENCE_COUNT to the case where the process running DSE has exclusive (standalone) access to the database file. When DSE has sole access to a database file the -REFERENCE_COUNT should be one (1). This is an informational field and does not have any effect on processing.

^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-REG[_SEQNO]=sequence-number
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In an LMS environment, this sets the "Region Seqno" field. For more information, refer to `Chapter 7: “Database Replication” <./dbrepl.html>`_.

^^^^^^^^^^^^^^^^^^^^
-RESET_MAX_PROCS
^^^^^^^^^^^^^^^^^^^^

Sets the maximum number of concurrent processes to zero, and the time to the time that the DSE CHANGE FILEHEADER command was run.

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-RESYNC_S[EQNO]=sequence-number
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In an LMS environment, this sets the hexadecimal value of the "Resync Seqno" field. For more information, refer to `Chapter 7: “Database Replication” <./dbrepl.html>`_.

^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-RESYNC_T[N]=sequence-number
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In an LMS environment, this sets the hexadecimal value ofthe "Resync transaction" field. For more information, refer to `Chapter 7: “Database Replication” <./dbrepl.html>`_.

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-S[PIN_SLEEP_MASK]=hexadecimal-mask
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Changes the hexadecimal Spin sleep time mask that controls the maximum time (in nanoseconds) the process sleeps on a sleep spin; zero (0), the default causes the process to just yield to the OS scheduler.

Use only with: -FILEHEADER

^^^^^^^^^^^^^^^^^^^^^^^^^^^
-SLEE[P_SPIN_COUNT]=integer
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Changes the hexadecimal Mutex Sleep Spin Count that controls the number of times a process waiting for a shared resource (usually a database) suspends its activity after exhausting its Mutex Hard Spin Count and before enqeueing itself to be awakened by a process releasing the resource.

Use only with: -FILEHEADER

^^^^^^^^^^^^^^^^^
-[NO]STD[NULLCOL]
^^^^^^^^^^^^^^^^^

Changes the collation of empty string ("NULL") subscripts for the database file. Although it is not the default, STDNULLCOLL is required with certain other characteristics, and highly recommended in any case. If you change this when there are existing "NULL" subscripts the results may be problematic. YottaDB recommends you establish this characteristic with GDE and load data with a consistent setting.

Use only with: -FILEHEADER

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-STRM_NUM=stream-number -STRM_R[EG_SEQNO]=str_num's_region_sequence_number
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Changes the hexadecimal values of Stream and its Reg Seqno. Use -STRM_NUM and -STRM_REG_SEQNO together as part of the same CHANGE -FILEHEADER command.

Use only with: -FILEHEADER

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-TI[MERS_PENDING]=timers_pending
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Sets a field that tracks the decimal number of processes considering a timed flush. Proper values are 0, 1, and 2.

Use the CHANGE -TIMERS_PENDING qualifier only upon receiving instructions from YottaDB.

Use only with: -FILEHEADER

^^^^^^^^^^^^^^^^^^^^^^^^^^
-TO[TAL_BLKS]=total_blocks
^^^^^^^^^^^^^^^^^^^^^^^^^^

Changes the hexadecimal total blocks field of the current file. Use only with: -FILEHEADER

.. note::
   The total blocks field should always reflect the actual size of the  database. Change this field only if it no longer reflects the database size.

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-TR[IGGER_FLUSH]=trigger_flush
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Sets the decimal value for the triggering threshold, in buffers, for flushing the cache-modified queue.

Use the CHANGE -TRIGGER_FLUSH qualifier only upon receiving instructions from YottaDB, and only in conjunction with the -FILEHEADER qualifier.

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-WR[ITES_PER_FLUSH]=writes_per_flush
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Sets the decimal number of block to write in each flush. The default value is 7.

Use only with -FILEHEADER

~~~~~~~~~~~~~~~~~~~
Examples for CHANGE
~~~~~~~~~~~~~~~~~~~

Example:

.. code-block:: bash

   DSE> change -block=3 -bsiz=400

This command changes the size of block 3 to 1024 bytes.

Example:

.. code-block:: bash

   DSE> change -block=4 -tn=10000

This command sets the transaction number to 65536 (Hex: 10000) for block 4.

Example:

.. code-block:: bash

   DSE> change -block=2 -record=4 -CMPC=10 -key="^CUS(""Jones,Vic"")"

This command changes the compression count of the key ^CUS(Jones,Vic) to 10. It is assumed that the key CUS(Jones,Tom) already exists. The following table illustrates how YottaDB calculates the value of CMPC in this case.

+---------------------------------------------------+------------------------------------------+---------------------------------------------------------+
| Record Key                                        | Compression Count                        | Resulting Key in Record                                 |
+===================================================+==========================================+=========================================================+
| CUS(Jones,Tom)                                    | 0                                        | CUS(Jones,Tom)                                          |
+---------------------------------------------------+------------------------------------------+---------------------------------------------------------+
| CUS(Jones,Vic)                                    | 10                                       | Vic)                                                    |
+---------------------------------------------------+------------------------------------------+---------------------------------------------------------+
| CUS(Jones,Sally)                                  | 10                                       | Sally)                                                  |
+---------------------------------------------------+------------------------------------------+---------------------------------------------------------+
| CUS(Smith,John)                                   | 4                                        | Smith,John)                                             |
+---------------------------------------------------+------------------------------------------+---------------------------------------------------------+

Example:

.. code-block:: bash

   DSE> dump -fileheader

This command displays fields of the file header.

Example:

.. code-block:: bash

   DSE> change -fileheader -blk_siz=2048

This command changes the block size field of the fileheader to 2048 bytes. The block field must always be a multiple of 512 bytes.

Example:

.. code-block:: bash

   DSE> change -fileheader -blocks_free=5B

This command changes the blocks-free fields of the file header to 91 (Hex: 5B). Example:

Example:

.. code-block:: bash

   DSE> change -fileheader -b_record=FF

This command sets the RECORD backup transaction to FF.

Example:

.. code-block:: bash

   DSE> change -fileheader corrupt_file=FALSE

This command sets the CORRUPT_FILE field to false.

Example:

.. code-block:: bash

   DSE> change -fileheader -current_tn=1001D1BF817

This command changes the current transaction number to 1100000000023 (Hex: 1001D1BF817). After you execute this command, subsequent transaction numbers will be greater than 1001D1BF817.

Example:

.. code-block:: bash

   DSE> change -fileheader -flush_time=00:00:02:00

This command changes the flush time field of the file header to 2 seconds.

Example:

.. code-block:: bash

   DSE> change -fileheader -freeze=true

This command makes the default region unavailable for updates.

Example:

.. code-block:: bash

   DSE> change -fileheader -key_max_size=20

This command changes the maximum key size to 20. Note that the default max key size is 64.

Example:

.. code-block:: bash

   DSE> CHANGE -FILEHEADER -NULL_SUBSCRIPTS="EXISTING"

This command changes the Null Subscripts field of the file header to EXISTING. Note that DSE cannot change the null subscript collation order. See the `GDE chapter <./gde.html>`_ for more information on changing the null subscript collation.

Example:

.. code-block:: bash

   DSE> change -fileheader -record_max_size=496

This command sets the maximum record size as 496 for the default region.

Example:

.. code-block:: bash

   DSE> change -fileheader -reference_count=5

This command sets the reference count field of the file header to 5.

Example:

.. code-block:: bash

   DSE> change -fileheader -timers_pending=2

This command sets the timers pending field of the file header to 2.

Example:

.. code-block:: bash

   DSE> change -fileheader -TOTAL_BLKS=64

This command sets the total size of the database to 100 (Hex: 64) blocks.

Example:

.. code-block:: bash

   DSE> change -fileheader -trigger_flush=1000

This command sets the Flush Trigger field of the file header to 1000. Note the default value of Flush Trigger is 960.

Example:

.. code-block:: bash

   DSE> change -fileheader -writes_per_flush=10

This command changes the number of writes/flush field of the file header to 10. Note that the default value for the number of writes/flush is 7.

Example:

.. code-block:: bash

   DSE> change -fileheader -zqgblmod_seqno=FF

This command changes the ZGBLMOD_SEQNO field to 255(Hex: FF).

.. _dse-close:

++++++++++
CLOSE
++++++++++

The CLOSE command closes the currently open output file.

The format of the CLOSE command is:

.. code-block:: none

   CL[OSE]

The CLOSE command has no qualifiers.

.. _dse-critical:

+++++++++
CRITICAL
+++++++++

Displays and/or modifies the status and contents of the critical section for the current region. The format of the CRITICAL command is:

.. code-block:: none

   CR[ITICAL]
   [
   -A[LL]
   -I[NIT]
   -O[WNER]
   -REL[EASE]
   -REM[OVE]
   -RES[ET]
   -S[EIZE]
   ]

* The critical section field identifies, by its process identification number (PID), the process presently managing updates to database.
* Think of a critical section as a common segment of a train track. Just as a train moves through the common segment as quickly as possible, the same way a process moves as quickly as possible through any critical section so that other processes can use it.
* By default, the CRITICAL command assumes the -OWNER qualifier, which displays the status of the critical section.

~~~~~~~~~~~~~~~~~~~~~~
Qualifiers of CRITICAL
~~~~~~~~~~~~~~~~~~~~~~

^^^^^^
-A[LL]
^^^^^^

Display all ids of processes owning critical section from all regions. If there are no processes owning critical section in a region, ALL displays "the CRIT is currently unowned" message for each region.

^^^^^^^
-I[NIT]
^^^^^^^

Reinitializes the critical section.

* The -INIT and -RESET qualifiers together cause all YottaDB processes actively accessing that database file to signal an error.
* YottaDB recommends against using -INIT without the -RESET parameter when other processes are actively accessing the region because it risks damaging the database.

Use only with: -RESET

^^^^^^^^
-O[WNER]
^^^^^^^^

Displays the ID of the process at the head of the critical section. DSE displays a warning message when the current process owns the critical section.

Use alone.

Example:

.. code-block:: bash

   DSE> critical -OWNER
   Write critical section is currently unowned

.. _crit-release:

^^^^^^^^^^
-REL[EASE]
^^^^^^^^^^

Releases the critical section if the process running DSE owns the section.

Use alone.

^^^^^^^^^
-REM[OVE]
^^^^^^^^^

Terminates any write ownership of the critical section. Use this when the critical section is owned by a process that is nonexistent or is known to no longer be running a YottaDB image.

Use alone.

.. note::
   Using CRITICAL -REMOVE when the write owner of a critical section is an active YottaDB process may cause structural database damage.

^^^^^^^^
-RES[ET]
^^^^^^^^

Displays the number of times the critical section has been through an online reinitialization.

Using -RESET with -INIT causes an error for processes that are attempting to get the critical section of the region. Under the guidance of YottaDB, use -RESET -INIT as a way to clear certain types of hangs.

Use only with: -INIT

.. _crit-seize:

^^^^^^^^
-S[EIZE]
^^^^^^^^

Seizes the critical section (if available).

* You can also use SEIZE to temporarily suspend database updates.
* Subsequently, execute CRITICAL -RELEASE command to restore normal operation.

~~~~~~~~~~~~~~~~~~~~~
Examples for CRITICAL
~~~~~~~~~~~~~~~~~~~~~

Example:

.. code-block:: bash

   DSE> critical -OWNER Write critical section owner is process id 4220

This command displays the ID of the process holding the critical section. Note that catching a process ID on a lightly loaded (or unloaded) system (for example, text environment) is like catching lightning in a bottle. Therefore, you can artificially hold a critical section using the DSE CRIT -SEIZE command in one session and view the owner using a different session.

.. _dse-dump:

+++++++++++
DUMP
+++++++++++

Displays blocks, records, or file headers. DUMP is one of the primary DSE examination commands.

The format of the DUMP command is:

.. code-block:: none

   D[UMP]
   [
   -A[LL]
   -B[LOCK]=block_number
   -C[OUNT]=count
   -F[ILEHEADER]
   -G[LO]
   -G[VSTATS]
   -[NO]C[RIT]
   -[NO]H[EADER]
   -O[FFSET]=offset
   -R[ECORD]=record-number
   -U[PDPROC]
   -Z[WR]
   ]

Use the error messages reported by MUPIP INTEG to determine what to DUMP and examine in the database. DUMP also can transfer records to a sequential file for future study and/or for input to MUPIP LOAD (see the section on OPEN). The DUMP command requires specification of an object using either -BLOCK, -HEADER, -RECORD, or -FILEHEADER.

~~~~~~~~~~~~~~~~~~
Qualifiers of DUMP
~~~~~~~~~~~~~~~~~~

.. _dump-all:

^^^^^^
-A[LL]
^^^^^^

When used with -FILEHEADER, the -A[LL] qualifier displays additional information on the database most of which is useful to YottaDB in diagnosing issues. A complete description of all the elements that show up with the DSE DUMP -FILEHEADER -ALL command are beyond the scope of this book.

Meaningful only with: -FILEHEADER

.. _dump-block:

^^^^^^^^^^^^^^^^^^^^^
-B[LOCK]=block-number
^^^^^^^^^^^^^^^^^^^^^

Specifies the starting block of the dump. For commands without an object qualifier, DSE uses the last block handled by a DSE operation. When no block has been accessed, (that is, on the first block-oriented command), DSE uses block one (1).

Incompatible with: -ALL, -FILEHEADER and -UPDPROC.

^^^^^^^^^^^^^^
-C[OUNT]=count
^^^^^^^^^^^^^^

Specifies the number of blocks, block headers, or records to DUMP.

Incompatible with: -ALL, -FILEHEADER and -UPDPROC.

.. _dse-dump-fileheader:

^^^^^^^^^^^^^
-F[ILEHEADER]
^^^^^^^^^^^^^

Dumps file header information. A DSE dump of a database file header prints a 0x prefix for all fields printed in hexadecimal format.

Use only with -ALL or -UPDPROC

^^^^^^
-G[LO]
^^^^^^

Dumps the specified record or blocks into the current output file in Global Output (GO) format. YottaDB strongly suggests using -ZWR rather than -GLO as the ZWR format handles all possible content values, including some that are problematic with -GLO (The GLO format is not supported for UTF-8 mode - use the ZWR format with UTF-8 mode.).

Incompatible with: -ALL, -FILEHEADER, -UPDPROC and -ZWR.

^^^^^^^^^^
-G[VSTATS]
^^^^^^^^^^

Displays the access statistics for global variables and database file(s).

^^^^^^^^^
-NO[CRIT]
^^^^^^^^^

Allows DSE DUMP to work even if another process is holding a critical section. Since results in this mode may be inconsistent, it should only be used if the critical section mechanism is not operating normally.

^^^^^^^^^^^^^
-[NO]H[EADER]
^^^^^^^^^^^^^

Specifies whether the dump of the specified blocks or records is restricted to, or excludes, headers. -HEADER displays only the header, -NOHEADER displays the block or record with the header suppressed. DUMP without the -[NO]HEADER qualifier dumps both the block/record and the header.

By default, DUMP displays all information in a block or record.

Incompatible with: -ALL, -FILEHEADER, -GLO, -UPDPROC and -ZWR.

.. _dump-offset:

^^^^^^^^^^^^^^^^
-O[FFSET]=offset
^^^^^^^^^^^^^^^^

Specifies the offset, in bytes, of the starting record for the dump. If the offset does not point to the beginning of a record, DSE rounds down to the last valid record start (e.g., DUMP -OFF=10 starts at -OFF=A if that was the beginning of the record containing offset 10).

Incompatible with: -ALL, -FILEHEADER, and -RECORD.

.. _dump-record:

^^^^^^^^^^^^^^^^^^^^^^^
-R[ECORD]=record_number
^^^^^^^^^^^^^^^^^^^^^^^

Specifies the record number of the starting record of the dump. If you try to dump a record number that is larger than the last actual record in the block, a DSE error message provides the number of the last record in the block.

Incompatible with: -ALL, -FILEHEADER, and -OFFSET.

^^^^^^^^^^
-U[PDPROC]
^^^^^^^^^^

Displays the helper process parameters with the fileheader elements.

Use only with -FILEHEADER.

^^^^^^
-Z[WR]
^^^^^^

Dumps the specified record or blocks into the current output file in ZWRITE (ZWR) format.

Incompatible with: -ALL, -GLO, -HEADER and -FILEHEADER.

~~~~~~~~~~~~~~~~~
Examples for DUMP
~~~~~~~~~~~~~~~~~

Example:

.. code-block:: bash

   DSE> DUMP -FILEHEADER

This command displays an output like the following:

.. code-block:: bash

   File    /tmp/test/r1.34_x86_64/g/yottadb.dat
   Region  DEFAULT
   File            /tmp/test/r1.34_x86_64/g/yottadb.dat
   Region          DEFAULT
   Date/Time       28-JAN-2022 10:08:27 [$H = 66137,36507]
     Access method                          MM  Global Buffers              100000
     Reserved Bytes                          0  Block size (in bytes)         4096
     Maximum record size                  4080  Starting VBN                   513
     Maximum key size                      255  Total blocks            0x003DEB89
     Null subscripts                     NEVER  Free blocks             0x000004DA
     Standard Null Collation              TRUE  Free space              0x00000000
     Last Record Backup     0x0000000000000001  Extension Count              10000
     Last Database Backup   0x0000000000000001  Number of local maps          7926
     Last Bytestream Backup 0x0000000000000001  Lock space              0x00000028
     In critical section            0x00000000  Timers pending                   0
     Cache freeze id                0x00000000  Flush timer            00:00:01:00
     Freeze match                   0x00000000  Flush trigger                93750
     Freeze online                       FALSE  Freeze online autorelease    FALSE
     Current transaction    0x000000001CC9E6C5  No. of writes/flush              7
     Maximum TN             0xFFFFFFFF83FFFFFF  Certified for Upgrade to        V6
     Maximum TN Warn        0xFFFFFFFD93FFFFFF  Desired DB Format               V6
     Master Bitmap Size                    496  Blocks to Upgrade       0x00000000
     Create in progress                  FALSE  Modified cache blocks            0
     Reference count                         1  Wait Disk                        0
     Journal State                    DISABLED
     Mutex Hard Spin Count                 128  Mutex Sleep Spin Count         128
     Mutex Queue Slots                    1024  KILLs in progress                0
     Replication State                     OFF  Region Seqno    0x0000000000000001
     Zqgblmod Seqno         0x0000000000000000  Zqgblmod Trans  0x0000000000000000
     Endian Format                      LITTLE  Commit Wait Spin Count          16
     Database file encrypted             FALSE  Inst Freeze on Error         FALSE
     Spanning Node Absent                 TRUE  Maximum Key Size Assured      TRUE
     Defer allocation                     TRUE  Spin sleep time mask    0x00000000
     Async IO                              OFF  WIP queue cache blocks           0
     DB is auto-created                  FALSE  DB shares gvstats             TRUE
     LOCK shares DB critical section     FALSE  Read Only                      OFF
     Recover interrupted                 FALSE
     Max conc proc time             1642870066  Max Concurrent processes       584
     Reorg Sleep Nanoseconds                 0


Note that certain fileheader elements appear depending on the current state of database. For example, if Journaling is not enabled in the database, DSE does not display Journal data element fields.

Example:

.. code-block:: bash

   $ dse dump -fileheader -updproc

This command displays the fileheader elements along with the following helper process parameters:

.. code-block:: bash

   Upd reserved area [% global buffers]   50  Avg blks read per 100 records                200
   Pre read trigger factor [% upd rsrvd]    50  Upd writer trigger [%flshTrgr]                 33

For more information, refer to the :ref:`file-header-data-elements` in “YottaDB Database Structure(GDS)”.

.. _dse-evaluate:

+++++++++++++
EVALUATE
+++++++++++++

Translates a hexadecimal number to decimal, and vice versa.

The format of the EVALUATE command is:

.. code-block:: none

   EV[ALUATE]
   [
   -D[ECIMAL]
   -H[EXADECIMAL]
   -N[UMBER]=number
   ]

The -DECIMAL and -HEXADECIMAL qualifiers specify the input base for the number. The -NUMBER qualifier is mandatory. By default, EVALUATE treats the number as having a hexadecimal base.

~~~~~~~~~~~~~~~~~~~~~~
Qualifiers of EVALUATE
~~~~~~~~~~~~~~~~~~~~~~

^^^^^^^^^^
-D[ECIMAL]
^^^^^^^^^^

Specifies that the input number has a decimal base.

Incompatible with: -HEXADECIMAL .

^^^^^^^^^^^^^^
-H[EXADECIMAL]
^^^^^^^^^^^^^^

Specifies that the input number has a hexadecimal base.

Incompatible with: -DECIMAL

^^^^^^^^^^^^^^^^
-N[UMBER]=number
^^^^^^^^^^^^^^^^

Specifies the number to evaluate. Required.

~~~~~~~~~~~~~~~~~~~~~
Examples for EVALUATE
~~~~~~~~~~~~~~~~~~~~~

Example:

.. code-block:: bash

   DSE> evaluate -number=10 -decimal
   Hex:  A   Dec:  10

This command displays the hexadecimal equivalent of decimal number 10.

Example:

.. code-block:: bash

   DSE> evaluate -number=10 -hexadecimal
   Hex:  10   Dec:  16

This command displays the decimal equivalent of hexadecimal 10.

Example:

.. code-block:: bash

   $ dse evaluate -number=10
   Hex:  10   Dec:  16

This command displays the decimal equivalent of Hexadecimal 10. Note that if you do not specify an qualifier with -NAME, then EVALUATE assumes Hexadecimal input.

.. _dse-exit:

++++++
EXIT
++++++

The EXIT command ends a DSE session.

The format of the EXIT command is:

.. code-block:: none

   EX[IT]

The EXIT command has no qualifiers.

.. _dse-find:

+++++
FIND
+++++

Locates a given block or region. The format of the FIND command is:

.. code-block:: none

   F[IND]
   [
   -B[LOCK]=block-number
   -E[XHAUSTIVE]
   -F[REEBLOCK] -H[INT]
   -K[EY]=key
   -[NO]C[RIT]
   -R[EGION][=region] | -R[EGION] region-name
   -SI[BLINGS]
   -ST[ATS]
   ]

* At the beginning of a DSE session, use the FIND -REGION command to select the target region.
* The FIND command, except when used with the -FREEBLOCK and -REGION qualifiers, uses the index tree to locate blocks. FIND can locate blocks only within the index tree structure. If you need to locate keys independent of their attachment to the tree, use the RANGE command.

~~~~~~~~~~~~~~~~~~
Qualifiers of FIND
~~~~~~~~~~~~~~~~~~

.. _find-block:

^^^^^^^^^^^^^^^^^^^^^
-B[LOCK]=block_number
^^^^^^^^^^^^^^^^^^^^^

Specifies the block to find.

On commands without the -BLOCK= qualifier, DSE uses the last block handled by a DSE operation. When no block has been accessed, that is, on the first block-oriented command, DSE uses block one (1).

Incompatible with: -KEY, -REGION

^^^^^^^^^^^^^
-E[XHAUSTIVE]
^^^^^^^^^^^^^

Searches the entire index structure for the desired path or siblings.

* FIND -EXHAUSTIVE locates blocks that are in the tree but not indexed correctly.
* FIND -EXHAUSTIVE locates all paths to a "doubly allocated" block.

.. note::
   A doubly allocated block may cause inappropriate mingling of data. As long as no KILLs occur, double allocation may not cause permanent loss of additional data. However, it may cause the application programs to generate errors and/or inappropriate results. When a block is doubly allocated, a KILL may remove data outside its proper scope. See `"Maintaining Database Integrity Chapter" <./integrity.html>`_ for more information on repairing doubly allocated blocks.

Incompatible with: -KEY, -REGION, -FREEBLOCK

^^^^^^^^^^^^
-F[REEBLOCK]
^^^^^^^^^^^^

Finds the nearest free block to the block specified by -HINT. FREEBLOCK accepts bit maps as starting or ending points.

* The -FREEBLOCK qualifier is incompatible with all other qualifiers except -BLOCK and -HINT.
* The -HINT qualifier is required with the -FREEBLOCK qualifier.
* FIND -FREEBLOCK relies on the bitmaps to locate its target, so be sure to fix any blocks incorrectly marked "FREE" before using this command. See MAP -BUSY for more information on fixing incorrectly marked free errors.

Required with -HINT; compatible with -BLOCK and [NO]CRIT.

^^^^^^^^^^^^^^^^^^^^
-H[INT]=block_number
^^^^^^^^^^^^^^^^^^^^

Designates the starting point of a -FREEBLOCK search.

FIND -FREE -HINT locates the "closest" free block to the hint. This provides a tool for locating blocks to add to the B-tree, or to hold block copies created with SAVE that would otherwise be lost when DSE exits. FIND -FREE relies on the bitmaps to locate its target, so be sure to fix any blocks incorrectly marked "FREE" before using this command.

Required with: -FREEBLOCK; compatible with -BLOCK and [NO]CRIT.

.. _find-key:

^^^^^^^^^^
-K[EY]=key
^^^^^^^^^^

Searches the database for the block containing the specified key or if the key does not exist, the block that would contain it, if it existed.

* Enclose an M-style key in quotation marks (" "). FIND -KEY is useful in locating properly indexed keys. The -KEY qualifier is incompatible with all other qualifiers.
* FIND -KEY= uses the index to locate the level zero (0) block , or data block, containing the key. If the key does not exist, it uses the index to locate the block in which it would reside. Note that FIND only works with the index as currently composed. In other words, it cannot FIND the "right" place, only the place pointed to by the index at the time the command is issued. These two locations should be, and may well be, the same; however, remind yourself to search for, understand and take into account all information describing any current database integrity issues.
* DSE accepts ^#t as a valid global name when specifying a key.

Compatible only with [NO]CRIT.

^^^^^^^^^^^
-[NO]C[RIT]
^^^^^^^^^^^

Allows FIND to work even if another process is holding a critical section.

As results in this mode may be inconsistent, it should only be used if the critical section mechanism is not operating normally

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-R[EGION][=region] | -R[EGION][ region]
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Switches to the named Global Directory region.

-REGION without a specified region, or "*", displays all existing regions in the database.

The region name is case-insensitive.

Use Alone.

^^^^^^^^^^^
-SI[BLINGS]
^^^^^^^^^^^

Displays the block number of the specified block and its logical siblings in hexadecimal format.

The logical siblings are the blocks, if any, that logically exist to the right and left of the given block in the database tree structure.

Incompatible with: -FREEBLOCK, -HINT, -KEY, -REGION

^^^^^^^^
ST[ATS]
^^^^^^^^

Switches to the name Global Directory shadow for the region's shared gvstats.

Compatible only with R[EGION].

~~~~~~~~~~~~~~~~~
Examples for FIND
~~~~~~~~~~~~~~~~~

Example:

.. code-block:: bash

   DSE> find -exhaustive -block=180
   Directory path
   Path--blk:off
   1:10 2:1E
   Global paths
   Path--blk:off
   6:51 1A4:249 180

This command locates block 180 by looking through the B-tree index for any pointer to the block. This command even finds blocks that are connected to the tree but the first key in the block does not match the index path.

Example:

.. code-block:: bash

   DSE> find -free -hint=180
   Next free block is D8F.

This command locates the "closest" free block to block 180.

You can use this command as a tool for locating blocks to add to the B-tree, or to hold block copies created with SAVE that would otherwise be lost when DSE exits.

Example:

.. code-block:: bash

   DSE>find -key="^biggbl(1)"

This command locates the key ^biggbl(1) in the database.

Example:

.. code-block:: bash

   DSE> find -freeblock -hint=232

This command starts to search for free block after block 232.

Example:

.. code-block:: bash

   DSE> FIND -FREEBLOCK -HINT=232 -NOCRIT

This command searches for freeblocks after block 232 even if another process is holding a critical section.

Example:

.. code-block:: bash

   DSE> find -sibling -block=10

This command operates like FIND -BLOCK; however, it reports the numbers of the blocks that logically fall before and after block 180 on the same level. This command produces an output like the following:

.. code-block:: bash

   Left sibling    Current block   Right sibling
    0x0000000F      0x00000010      0x00000011

.. _dse-help:

+++++++++
HELP
+++++++++

The HELP command explains DSE commands. The format of the HELP command is:

.. code-block:: none

   -H[ELP] [help topic]

.. _dse-integrit:

++++++++++
INTEGRIT
++++++++++

Checks the internal consistency of a single non-bitmap block. INTEGRIT reports errors in hexadecimal notation.

The format of the INTEGRIT command is:

.. code-block:: none

   I[NTEGRIT] -B[LOCK]=block-number

.. note::
   Unlike MUPIP INTEG, this command only detects errors internal to a block and cannot detect errors such as indices incorrectly pointing to another block. For information on the utility that checks multiple blocks, refer to the :ref:`mupip-integ` of the General Database Management chapter.

~~~~~~~~~~~~~~~~~~~~~~
Qualifiers of INTEGRIT
~~~~~~~~~~~~~~~~~~~~~~

.. _integ-block:

^^^^^^^^^^^^^^^^^^^^^
-B[LOCK]=block_number
^^^^^^^^^^^^^^^^^^^^^

Specifies the block for DSE to check. On commands with no -BLOCK qualifier, DSE uses the last block handled by a DSE operation. When no block has been accessed, that is, on the first block-oriented command, DSE uses block one (1).

.. _integ-nocrit:

^^^^^^^^^
-NO[CRIT]
^^^^^^^^^

Allows DSE INTEG to work even if another process is holding a critical section. Since results in this mode may be inconsistent, it should only be used if the critical section mechanism is not operating normally.

.. _dse-maps:

+++++++
MAPS
+++++++

Examines or updates bitmaps. The format of the MAPS command is:

.. code-block:: none

   M[APS]
   [
   -BL[OCK]=block-number
   -BU[SY]
   -F[REE]
   -M[ASTER]
   -R[ESTORE_ALL]
   ]

MAPS can flag blocks as being either -BUSY or -FREE. The -MASTER qualifier reflects the current status of a local bitmap back into the master map. The -RESTORE_ALL qualifier rebuilds all maps and should be used with caution since it can destroy important information.

By default, MAPS shows the status of the bitmap for the specified block.

~~~~~~~~~~~~~~~~~~
Qualifiers for MAP
~~~~~~~~~~~~~~~~~~

.. _map-block:

^^^^^^^^^^^^^^^^^^^^^
-BL[OCK]=block_number
^^^^^^^^^^^^^^^^^^^^^

Specifies the target block for MAPS. The -BLOCK qualifier is incompatible with the -RESTORE_ALL qualifier.

On commands with no -BLOCK= or -RESTORE_ALL qualifier, DSE uses the last block handled by a DSE operation. When no block has been accessed, that is, on the first block-oriented command, DSE uses block one (1).

Incompatible with: -RESTORE_ALL

^^^^^^^
-BU[SY]
^^^^^^^

Marks the current block as busy in the block's local map and appropriately updates the master bitmap. BUSY accepts bit map blocks.

Compatible only with: -BLOCK

^^^^^^^
-F[REE]
^^^^^^^

Marks the current block as free in the block's local map and appropriately updates the master bitmap.

Compatible only with: -BLOCK

^^^^^^^^^
-M[ASTER]
^^^^^^^^^

Sets the bit in the master bitmap associated with the current block's local map according to whether or not that local map is full. MASTER accepts bit map blocks.

Use only with: -BLOCK.

^^^^^^^^^^^^^^
-R[ESTORE_ALL]
^^^^^^^^^^^^^^

Sets all local bitmaps and the master bitmap to reflect the blocks used in the database file.

Use -RESTORE_ALL only if the database contents are known to be correct, but a large number of the bitmaps require correction.

.. note::
   The -RESTORE_ALL qualifier rebuilds all maps and should be used with a great deal of caution as it can destroy important information.

Use alone.

~~~~~~~~
Examples
~~~~~~~~

Example:

.. code-block:: bash

   DSE> MAPS -BLOCK=20 -FREE

This command flags block 20 as free. A sample DSE DUMP output block 0 is as follows:

.. code-block:: bash

   Block 0  Size 90  Level -1  TN 10B76A V5   Master Status: Free Space
                   Low order                         High order
   Block        0: |  XXXXXXXX  XXXXXXXX  XXXXXXXX  XXXXXXXX  |
   Block       20: |  :XXXXXXX  XXXXXXXX  XXXXXXXX  XXXXXXXX  |
   Block       40: |  XXXXXXXX  XXXXXXXX  XXXXXXXX  XXXXXXXX  |
   Block       60: |  XXXXXXXX  XXXXXXXX  XXXXXXXX  XXXXXXXX  |
   Block       80: |  XXXXXXXX  XXXXXXXX  XXXXXXXX  XXXXXXXX  |
   Block       A0: |  XXXXXXXX  XXXXXXXX  XXXXXXXX  XXXXXXXX  |
   Block       C0: |  XXXXXXXX  XXXXXXXX  XXXXXXXX  XXXXXXXX  |
   Block       E0: |  XXXXXXXX  XXXXXXXX  XXXXXXXX  XXXXXXXX  |
   Block      100: |  XXXXXXXX  XXXXXXXX  XXXXXXXX  XXXXXXXX  |
   Block      120: |  XXXXXXXX  XXXXXXXX  XXXXXXXX  XXXXXXXX  |
   Block      140: |  XXXXXXXX  XXXXXXXX  XXXXXXXX  XXXXXXXX  |
   Block      160: |  XXXXXXXX  XXXXXXXX  XXXXXXXX  XXXXXXXX  |
   Block      180: |  XXXXXXXX  XXXXXXXX  XXXXXXXX  XXXXXXXX  |
   Block      1A0: |  XXXXXXXX  XXXXXXXX  XXXXXXXX  XXXXXXXX  |
   Block      1C0: |  XXXXXXXX  XXXXXXXX  XXXXXXXX  XXXXXXXX  |
   Block      1E0: |  XXXXXXXX  XXXXXXXX  XXXXXXXX  XXXXXXXX  |
   'X' == BUSY  '.' == FREE  ':' == REUSABLE  '?' == CORRUPT

Note that BLOCK 20 is marked as REUSABLE, which means FREE but in need of a before-image journal record.

Example:

.. code-block:: bash

   DSE> maps -block=20 -busy

This command marks block 20 as busy. A sample DSE DUMP output of block 0 is as follows:

.. code-block:: bash

   Block 0  Size 90  Level -1  TN 1 V5   Master Status: Free Space
                   Low order                         High order
   Block        0: |  XXX.....  ........  ........  ........  |
   Block       20: |  X.......  ........  ........  ........  |
   Block       40: |  ........  ........  ........  ........  |
   Block       60: |  ........  ........  ........  ........  |
   Block       80: |  ........  ........  ........  ........  |
   Block       A0: |  ........  ........  ........  ........  |
   Block       C0: |  ........  ........  ........  ........  |
   Block       E0: |  ........  ........  ........  ........  |
   Block      100: |  ........  ........  ........  ........  |
   Block      120: |  ........  ........  ........  ........  |
   Block      140: |  ........  ........  ........  ........  |
   Block      160: |  ........  ........  ........  ........  |
   Block      180: |  ........  ........  ........  ........  |
   Block      1A0: |  ........  ........  ........  ........  |
   Block      1C0: |  ........  ........  ........  ........  |
   Block      1E0: |  ........  ........  ........  ........  |
   'X' == BUSY  '.' == FREE  ':' == REUSABLE  '?' == CORRUPT

Note that the BLOCK 20 is marked as BUSY.

.. _dse-open:

++++++
OPEN
++++++

Use the OPEN command to open a file for sequential output of global variable data. The format of the OPEN command is:

.. code-block:: none

   OP[EN] F[ILE]=file

* OPEN a file to which you want to "dump" information.
* If an OPEN command does not have a -FILE qualifier, DSE reports the name of the current output file.

~~~~~~~~~~~~~~~~~~~
Qualifiers for OPEN
~~~~~~~~~~~~~~~~~~~

^^^^^^^^^^^^^^^^^
-F[ILE]=file-name
^^^^^^^^^^^^^^^^^

Specifies the file to open.

~~~~~~~~~~~~~~~~~
Examples for OPEN
~~~~~~~~~~~~~~~~~

Example:

.. code-block:: bash

   DSE> OPEN
   Current output file:  var.out

This command displays the current output file. In this case, the output file is var.out.

Example:

.. code-block:: bash

   DSE> OPEN -FILE=var1.out

The command OPEN -FILE=var1.out sets the output file to var1.out.

.. _dse-overwrite:

++++++++++++
OVERWRITE
++++++++++++

Overwrites the specified string on the given offset in the current block. Use extreme caution when using this command.

The format of the OVERWRITE command is:

.. code-block:: none

   OV[ERWRITE]
   [
   -D[ATA]=string
   -O[FFSET]=offset
   ]

~~~~~~~~~~~~~~~~~~~~~~~~
Qualifiers for OVERWRITE
~~~~~~~~~~~~~~~~~~~~~~~~

.. _overwrite-block:

^^^^^^^^^^^^^^^^^^^^^
-B[LOCK]=block number
^^^^^^^^^^^^^^^^^^^^^

Directs DSE to OVERWRITE a specific block. If no block number is specified, the default is the current block.

.. _overwrite-data:

^^^^^^^^^^^^^^
-D[ATA]=string
^^^^^^^^^^^^^^

Specifies the data to be written. Use quotation marks around the string and escape codes of the form \\a or \\ab, where "a" and "b" are hexadecimal digits representing non-printing characters. \\\\ translates to a single backslash.

.. _overwrite-offset:

^^^^^^^^^^^^^^^^
-O[FFSET]=offset
^^^^^^^^^^^^^^^^

Specifies the offset in the current block where the overwrite should begin.

~~~~~~~~~~~~~~~~~~~~~~
Examples for OVERWRITE
~~~~~~~~~~~~~~~~~~~~~~

Example:

.. code-block:: bash

   DSE>overwrite -block=31 -data="Malvern" -offset=CA

This command overwrites the data at the specified location.

.. _dse-page:

++++++++++
PAGE
++++++++++

Sends one form feed to the output device. Use PAGE to add form feeds to a dump file, making the hard copy file easier to read. If you plan to use the dump file with MUPIP LOAD, do not use PAGE.

The format of the PAGE command is:

.. code-block:: none

   P[AGE]

The PAGE command has no qualifiers.

.. _dse-range:

++++++
RANGE
++++++

The RANGE command finds all blocks in the database whose first key falls in the specified range of keys. The RANGE command may take a very long time unless the range specified by -FROM and -TO is small. Use FIND -KEY and/or FIND -KEY -EXHAUSTIVE first to quickly determine whether the key appears in the index tree.

The format of the RANGE command is:

.. code-block:: none

   RA[NGE]
   [
   -F[ROM]=block-number
   -T[O]=block-number
   -I[NDEX]
   -LOS[T]
   -[NO]C[RIT]
   -[NO]BU[SY]
   -S[TAR]
   -LOW[ER]=key
   -U[PPER]=key
   ]

~~~~~~~~~~~~~~~~~~~
Qualifiers of RANGE
~~~~~~~~~~~~~~~~~~~

^^^^^^^^^^^^^^^^^^^^
-F[ROM]=block_number
^^^^^^^^^^^^^^^^^^^^

Specifies a starting block number for the range search. DSE RANGE accepts bit maps as starting or ending points.

By default, RANGE starts processing at the beginning of the file.

^^^^^^^^^^^^^^^^^^
-T[O]=block-number
^^^^^^^^^^^^^^^^^^

Specifies an ending block number for the range search. DSE RANGE accepts bit maps as starting or ending points. By default, RANGE stops processing at the end of the file.

^^^^^^^^
-I[NDEX]
^^^^^^^^

Restricts a search to index blocks.

^^^^^^^^^^^^^^^^^^^^
-LOS[T]=block_number
^^^^^^^^^^^^^^^^^^^^

Restricts a search to blocks not found by a FIND -BLOCK.

^^^^^^^^^^^^
-LOW[ER]=key
^^^^^^^^^^^^

Specifies the lower bound for the key range.

^^^^^^^^^^^^^^^^^^^^^
-[NO]BU[SY]=busy/free
^^^^^^^^^^^^^^^^^^^^^

Restricts a search to either BUSY or FREE blocks.

.. _range-nocrit:

^^^^^^^^^^^
-[NO]C[RIT]
^^^^^^^^^^^

Allows DSE RANGE to work even if another process is holding a critical section. Since results in this mode may be inconsistent, it should only be used if the critical section mechanism is not operating normally.

.. _range-star:

^^^^^^^
-S[TAR]
^^^^^^^

Includes index blocks that contain a single star key.

^^^^^^^^^^^^
-U[PPER]=key
^^^^^^^^^^^^

Specifies the upper bound for the key range.

~~~~~~~~~~~~~~~~~~
Examples for RANGE
~~~~~~~~~~~~~~~~~~

Example:

.. code-block:: bash

   DSE> range -lower="^abcdefgh" -upper="^abcdefghi" -from=A -to=CC

This command searches for a specified keys between block 10 and block 204. Note that the range (between FROM and TO) of blocks must be valid blocks specified in hexadecimal.

Example:

.. code-block:: bash

   DSE> range -lower="^abcdefgh" -upper="^abcdefghi" -from=A -to=CC -noindex

This command searches only data blocks for the specified keys between block 10 and block 204.

Example:

.. code-block:: bash

   DSE> range -lower="^abcdefgh" -upper="^abcdefghi" -from=A -to=CC -index

This command searches only index blocks for the specified keys between block 10 and block 204.

Example:

.. code-block:: bash

   DSE> range -lower="^abcdefgh" -upper="^abcdefghi" -lost

This command includes lost blocks while searching for the specified keys and reports only blocks which are not currently indexed.

Example:

.. code-block:: bash

   DSE> range -lower="^Fruits(15)" -upper="^Fruits(877)" -from=A -to=F
   Blocks in the specified key range:
   Block: 0000000A Level: 0
   Block: 0000000B Level: 0
   Block: 0000000C Level: 0
   Block: 0000000D Level: 0
   Block: 0000000E Level: 0
   Block: 0000000F Level: 0
   Found 6 blocks

This command searches for keys between ^Fruits(15) and ^Fruits(877).

.. _dse-remove:

++++++++++
REMOVE
++++++++++

Removes one or more records or a save buffer.

The format of the REMOVE command is:

.. code-block:: none

   REM[OVE]
   [
   -B[LOCK]=block-number
   -C[OUNT]=count
   -O[FFSET]=offset
   -R[ECORD]=record-number
   -V[ERSION]=version-number
   ]

The version number is specified in decimal.

~~~~~~~~~~~~~~~~~~~~
Qualifiers of REMOVE
~~~~~~~~~~~~~~~~~~~~

.. _remove-block:

^^^^^^^^^^^^^^^^^^^^^
-B[LOCK]=block_number
^^^^^^^^^^^^^^^^^^^^^

Specifies the block associated with the record or buffer being deleted.

On commands with no -BLOCK= qualifier, DSE uses the last block handled by a DSE operation. When no block has been accessed, that is, on the first block-oriented command, DSE uses block one (1).

BLOCK accepts blocks higher than the current database size because they deal with a set of saved block copies rather than the database and there are situations where a saved block may be outside the current database size (for example, due to a concurrent MUPIP REORG -TRUNCATE).

.. _remove-count:

^^^^^^^^^^^^^^
-C[OUNT]=count
^^^^^^^^^^^^^^

Specifies the number of records to remove.

By default, REMOVE deletes a single record.

Incompatible with: -VERSION

.. _remove-offset:

^^^^^^^^^^^^^^^^
-O[FFSET]=offset
^^^^^^^^^^^^^^^^

Specifies the offset (in bytes) of the record to be removed. If the offset does not point to the beginning of a record, DSE rounds down to the beginning of the record containing the offset (for example, REMOVE -OFF=10 starts at OFF=A if that was the last prior record boundary).

Incompatible with: -VERSION, -RECORD

.. _remove-record:

^^^^^^^^^^^^^^^^^^^^^^^
-R[ECORD]=record_number
^^^^^^^^^^^^^^^^^^^^^^^

Specifies the number that identifies the record to be removed. The -RECORD qualifier is incompatible with the -OFFSET and -VERSION qualifiers.

Incompatible with: -VERSION, -OFFSET

^^^^^^^^^^^^^^^^^^^^^^^^^
-V[ERSION]=version_number
^^^^^^^^^^^^^^^^^^^^^^^^^

Specifies the version number, in decimal, of the save buffer to be removed. If there is more than one version of a block, -VERSION is required; otherwise REMOVE works on that sole version. -VERSION is incompatible with all qualifiers except -BLOCK.

If there is only one version of the specified -BLOCK= block in the current region, DSE REMOVE defaults to that version.

Use only with: -BLOCK; decimal

.. _dse-restore:

+++++++++++
RESTORE
+++++++++++

The RESTORE command restores saved versions of blocks.

.. code-block:: none

   RES[TORE]
   [
   -B[LOCK]=block-number
   -F[ROM]=from
   -R[EGION]=region | -R[EGION] region
   -V[ERSION]=version-number
   ]

The version number is specified in decimal.

~~~~~~~~~~~~~~~~~~~~~
Qualifiers of RESTORE
~~~~~~~~~~~~~~~~~~~~~

.. _restore-block:

^^^^^^^^^^^^^^^^^^^^^
-B[LOCK]=block_number
^^^^^^^^^^^^^^^^^^^^^

Specifies the block to restore.

For commands with no -BLOCK= qualifier, DSE uses the last block handled by a DSE operation. When no block has been accessed, (i.e., on the first block-oriented command), DSE uses block one (1).

BLOCK accepts blocks higher than the current database size because it deals with a set of saved block copies rather than the database and there are situations where a saved block may be outside the current database size (for example, due to a concurrent MUPIP REORG -TRUNCATE).

.. _restore-from:

^^^^^^^^^^^^^^^^^^^^
-F[ROM]=block_number
^^^^^^^^^^^^^^^^^^^^

Specifies the block number of the SAVE buffer to restore.

DSE restores the block specified with -BLOCK qualifier with the block specified by the -FROM qualifier. If there is only one version of the specified -FROM= block, DSE RESTORE defaults to that version and it always restores the original block transaction number.

By default, RESTORE uses the target block number as the -FROM block number.

.. _restore-region:

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-R[EGION]=region | -R[EGION] region
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Specifies the region of the saved buffer to restore.

By default, RESTORE uses SAVE buffers from the current region.

.. _restore-version:

^^^^^^^^^^^^^^^^^^^^^^^^^
-V[ERSION]=version_number
^^^^^^^^^^^^^^^^^^^^^^^^^

Specifies the decimal version number of the block to restore. The version number is required.

.. _dse-save:

+++++++
SAVE
+++++++

The SAVE command preserves versions of blocks, or displays a listing of saved versions for the current DSE session. SAVE can preserve 128 versions. Saved information is lost when DSE EXITs.

Use with the RESTORE command to move SAVEd blocks to a permanent location, and as a safety feature use SAVE to retain copies of database blocks before changing them.

The format of the SAVE command is:

.. code-block:: none

   SA[VE]
   [
   -B[LOCK]=block-number
   -C[OMMENT]=string
   -L[IST]
   -[NO]C[RIT]
   ]

~~~~~~~~~~~~~~~~~~
Qualifiers of SAVE
~~~~~~~~~~~~~~~~~~

.. _save-block:

^^^^^^^^^^^^^^^^^^^^^
-B[LOCK]=block_number
^^^^^^^^^^^^^^^^^^^^^

Specifies the block to restore.

On commands with no -BLOCK= qualifier, DSE uses the last block handled by a DSE operation. When no block has been accessed, that is, on the first block-oriented command, DSE uses block one (1).

^^^^^^^^^^^^^^^^^
-C[OMMENT]=string
^^^^^^^^^^^^^^^^^

Specifies a comment to save with the block. Enclose the comment in quotation marks (" ").

Incompatible with: -LIST

^^^^^^^
-L[IST]
^^^^^^^

Lists saved versions of specified blocks. The -LIST qualifier is incompatible with the -COMMENT qualifier.

By default, SAVE -LIST provides a directory of all SAVEd blocks.

LIST may display blocks higher than the current database size because it deals with a set of saved block copies rather than the database and there are situations where a saved block may be outside the current database size (for example, due to a concurrent MUPIP REORG -TRUNCATE);

Incompatible with: -COMMENT

.. _save-nocrit:

^^^^^^^^^^^
-[NO]C[RIT]
^^^^^^^^^^^

Allows DSE SAVE to work even if another process is holding a critical section. Since results in this mode may be inconsistent, it should only be used if the critical section mechanism is not operating normally.

.. _dse-shift:

++++++++
SHIFT
++++++++

Use the SHIFT command to shift data in a block, filling the block with zeros, or shortening the block. The format of the SHIFT command is:

.. code-block:: none

   SH[IFT]
   [
   -B[ACKWARD]=b_shift
   -BL[OCK]=block_number
   -F[ORWARD]=f_shift
   -O[FFSET]=offset
   ]

b_shift must always be less than or equal to offset. This means that DSE SHIFT in the backward direction is restricted to a maximum of OFFSET number of bytes. This ensures that the shift does not cross block boundaries, either intentionally or unintentionally.

~~~~~~~~~~~~~~~~~~~
Qualifiers of SHIFT
~~~~~~~~~~~~~~~~~~~

^^^^^^^^^^^^^^^^^
-B[ACKWARD]=shift
^^^^^^^^^^^^^^^^^

Specifies the number of bytes to shift data in the direction of the block header.

Incompatible with: -FORWARD

.. _shift-block:

^^^^^^^^^^^^^^^^^^^^^
-BL[OCK]=block_number
^^^^^^^^^^^^^^^^^^^^^

Specifies the block number to perform the DSE SHIFT.

^^^^^^^^^^^^^^^^
-F[ORWARD]=shift
^^^^^^^^^^^^^^^^

Specifies the number of bytes to shift data toward the end of the block.

Incompatible with: -BACKWARD

.. _shift-offset:

^^^^^^^^^^^^^^^^
-O[FFSET]=offset
^^^^^^^^^^^^^^^^

Specifies the starting offset, in bytes, of the portion of the block to shift.

.. _dse-spawn:

+++++++++
SPAWN
+++++++++

Use the SPAWN command to fork a child process for access to the shell without terminating the current DSE environment.

The format of the SPAWN command is:

.. code-block:: none

   SP[AWN] [shell-command]

* The SPAWN command accepts an optional command string for execution by the spawned sub-process. If the SPAWN has no command string parameter, the created sub-process issues a shell prompt and accepts any legal shell command. To terminate the sub-process, use the shell logout command.

* The SPAWN command has no qualifiers.

* DSE SPAWN works with an argument. If the argument contains spaces, enclose it with quotes.

~~~~~~~~~~~~~~~~~
Examples of SPAWN
~~~~~~~~~~~~~~~~~

Example:

.. code-block:: bash

   DSE> SPAWN "yottadb -run ^GDE"

This command suspends a DSE session and executes the shell command yottadb -run ^GDE.

.. _dse-wcinit:

+++++++++++
WCINIT
+++++++++++

Use the WCINIT command to reinitialize the global buffers of the current region. Because it cleans out the cache, the WCINIT command should not be used except under the guidance of YottaDB.

.. note::
   A WCINIT command issued while normal database operations are in progress can cause catastrophic damage to the database.

The format of the WCINIT command is:

.. code-block:: none

   W[CINIT]

* The WCINIT command has no qualifiers.

* When you issue the WCINIT command, DSE issues the CONFIRMATION: prompt. You must verify the WCINIT command by responding with "YES."

If you do not confirm the WCINIT, DSE issues the message:

.. code-block:: bash

    No action taken, enter yes at the CONFIRMATION prompt to initialize global buffers.

* WCINIT operations are more safely performed by MUPIP RUNDOWN. Use this command only under instructions from YottaDB.

---------------------------
DSE Command Summary
---------------------------

+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| Commands                                 | Qualifiers                               | Comments                                                                   |
+==========================================+==========================================+============================================================================+
| :ref:`dse-add`                           | -B[LOCK]=block number                    | \-                                                                         |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -D[ATA]=string                           | Incompatible with -POINTER, -STAR                                          |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -K[EY]=key                               | Incompatible with -STAR                                                    |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -O[FFSET]=offset                         | Incompatible with -RECORD, -STAR                                           |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -P[OINTER]=pointer                       | Incompatible with -DATA                                                    |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -R[ECORD]=record-number                  | Incompatible with -OFFSET, -STAR                                           |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -S[TAR]                                  | Incompatible with -DATA,-KEY, -OFFSET, -RECORD                             |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| :ref:`dse-all`                           | -A[LL]                                   | Meaningful only with -DUMP                                                 |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -B[UFFER_FLUSH]                          | Incompatible with -RENEW                                                   |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -C[RITINIT]                              | Incompatible with -RENEW, -RELEASE, -SEIZE                                 |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -D[UMP]                                  | Use with: -ALL                                                             |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -[NO]F[REEZE]                            | Incompatible with -RENEW                                                   |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -O[VERRIDE]                              | Meaningful only with -[NO]FREEZE                                           |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -REF[ERENCE]                             | Incompatible with -RENEW                                                   |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -REL[EASE]                               | Incompatible with -CRITINIT, -RENEW,-SEIZE                                 |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -REN[EW]                                 | Use alone                                                                  |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -S[EIZE]                                 | Incompatible with -RENEW, -RELEASE, -CRITINIT                              |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -W[CINIT]                                | Incompatible with -RENEW                                                   |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| :ref:`dse-buffer-flush`                  | \-                                       | \-                                                                         |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| :ref:`dse-cache`                         | -ALL                                     | Used with -RECOVER, -SHOW, and -VERIFY                                     |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -RE[COVER]                               | Use only with -ALL.                                                        |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -SH[OW]                                  | Use only with -ALL.                                                        |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -VE[RIFY]                                | Use only with -ALL.                                                        |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| :ref:`dse-change`                        | -BL[OCK]=block number                    | Incompatible with -FILEHEADER and qualifiers used with -FILEHEADER         |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -BS[IZ]=block-size                       | Use only with -BLOCK, -LEVEL, -TN                                          |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -L[EVEL]=level                           | Use only with -BLOCK, -BSIZ, -TN                                           |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -TN [=transaction number]                | Use only with -BLOCK, -BSIZ, -LEVEL                                        |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -OF[FSET]=offset                         | Use only with -BLOCK, -CMPC, -RSIZ                                         |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -RE[CORD]=record number                  | Use only with -BLOCK, -CMPC, -RSIZ                                         |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -CM[PC]= compression count               | Use only with -BLOCK, -RECORD, -OFFSET, -RSIZ                              |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -RS[IZ]=record size                      | Use only with -CMPC -OFFSET, -RECORD, -BLOCK                               |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -F[ILEHEADER]                            | Incompatible with -BSIZ, -CMPC, -TN, -LEVEL, -OFFSET, -RECORD, -RSIZ       |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | AVG_BLKS_READ=Average blocks read        | \-                                                                         |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | B_B[YTESTREAM]=transaction number        | \-                                                                         |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -B_C[OMPREHENSIVE]=transaction number    | Use only with -FILEHEADER; decimal                                         |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | B_D[ATABASE] = transaction number        | Use only with -FILEHEADER; decimal                                         |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -B_I[NCREMENTAL] = transaction number    | Use only with -FILEHEADER; decimal                                         |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -BLK[_SIZE]=block size                   | Use only with -FILEHEADER; decimal                                         |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -BLO[CKS_FREE]=free blocks               | Use only with -FILEHEADER; decimal                                         |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -B_R[ECORD]=transaction number           | Use only with -FILEHEADER; decimal                                         |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -CO[RRUPT_FILE]=value                    | Use only with -FILEHEADER                                                  |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -CU[RRENT_TN]=transaction number         | Use only with -FILEHEADER                                                  |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | DECL[OCATION]=value                      | Use only with -FILEHEADER; decimal                                         |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | DEF[_COLLATION]=value                    | Use only with -FILEHEADER;                                                 |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -ENCRYPTION_HASH                         | Use only with -FILEHEADER                                                  |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -FL[USH_TIME][=delta time]               | Use only with -FILEHEADER                                                  |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -FR[EEZE]=value                          | Use only with -FILEHEADER                                                  |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -FU[LLY_UPGRADED]=boolean                | Use only with -FILEHEADER                                                  |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -GV[STATSRESET]                          | Use only with -FILEHEADER                                                  |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -HARD_SPIN_COUNT=Mutex hard spin count   | Use only with -FILEHEADER                                                  |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -HEXL[OCATION]=value                     | Use only with -FILEHEADER;hexa                                             |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -INT[ERRUPTED_RECOV]=boolean             | \-                                                                         |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -JNL_YIELD_LIMIT=journal yield limit     | \-                                                                         |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -K[EY_MAX_SIZE]=key_max_size             | Use only with -FILEHEADER; decimal                                         |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -M[ACHINE_NAM]=value                     | \-                                                                         |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -N[ULL_SUBSCRIPTS]=value                 | Use only with -FILEHEADER                                                  |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -NO[CRIT]                                | \-                                                                         |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -OV[ERRIDE]                              | \-                                                                         |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -RC_SRV_COUNT                            | \-                                                                         |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -RE_READ_TRIGGER=read trigger            | \-                                                                         |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -Q[UANTUM_INTERVAL] [=delta time]        | Use only with -FILEHEADER; decimal                                         |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -REC[ORD_MAX_SIZE]=maximum record size   | Use only with -FILEHEADER; decimal                                         |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -REF[ERENCE_COUNT]=reference count       | Use only with -FILEHEADER; decimal                                         |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -REG[_SEQNO]=sequence number             | Use only with -FILEHEADER; hexa                                            |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -RESERVED_BYTES=reserved bytes           | Use only with -FILEHEADER;decimal                                          |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -[NO] RES[PONSE_INTERVAL] [=delta time]  | Use only with -FILEHEADER; decimal                                         |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -SLEEP_SPIN_COUNT=mutex sleep spin count | Use only with -FILEHEADER;                                                 |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -SPIN_SLEEP_TIME=mutex sleep time        | \-                                                                         |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -[NO]S[TALENESS_TIMER] [=delta time]     | Use only with -FILEHEADER; decimal                                         |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -TIC[K_INTERVAL] [=delta time]           | Use only with -FILEHEADER; decimal                                         |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -TIM[ERS_PENDING]=timers pending         | Use only with -FILEHEADER; decimal                                         |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -TO[TAL_BLKS]=total_blocks               | Use only with -FILEHEADER                                                  |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -TR[IGGER_FLUSH]=trigger flush           | Use only with -FILEHEADER                                                  |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -W[RITES_PER_FLUSH]=writes per flush     | Use only with -FILEHEADER; decimal                                         |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -WAIT_DISK=wait disk                     | \-                                                                         |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -Zqgblmod_S[EQNO]=sequence number        | Use only with -FILEHEADER;hexa                                             |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -Zqgblmod_TN=database-transaction_number | Use only with -FILEHEADER;hexa                                             |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| :ref:`dse-close`                         | \-                                       | \-                                                                         |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| :ref:`dse-critical`                      | -I[NIT]                                  | Use only with -RESET                                                       |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -O[WNER]                                 | Use alone                                                                  |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -REL[EASE]                               | Use alone                                                                  |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -REM[OVE]                                | Use alone                                                                  |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -RES[ET]                                 | Use only with -INIT                                                        |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -S[EIZE]                                 | Use alone                                                                  |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| :ref:`dse-dump`                          | -B[LOCK]=block_number                    | Incompatible with -FILEHEADER                                              |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -C[OUNT]=count                           | Incompatible with -FILEHEADER                                              |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -F[ILEHEADER]                            | Use alone                                                                  |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -G[LO]                                   | Incompatible with -FILEHEADER, -HEADER                                     |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -G[VSTATS]                               | Use only with -FILEHEADER                                                  |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -[NO]H[EADER]                            | Incompatible with -FILEHEADER, -GLO                                        |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -O[FFSET]=offset                         | Incompatible with -FILEHEADER, -RECORD                                     |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -R[ECORD]=record_number                  | Incompatible with -FILEHEADER, -OFFSET                                     |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| :ref:`dse-evaluate`                      | -D[ECIMAL]                               | Incompatible with -HEXADECIMAL                                             |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -H[EXADECIMAL]                           | Incompatible with -DECIMAL                                                 |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -N[UMBER]=number                         | Required                                                                   |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| :ref:`dse-exit`                          | \-                                       | \-                                                                         |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| :ref:`dse-find`                          | -B[LOCK]=block_number                    | Incompatible with -KEY, -REGION                                            |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -E[XHAUSTIVE]                            | Incompatible with -KEY, -REGION, -FREEBLOCK                                |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -F[REEBLOCK]                             | Required with -HINT; compatible with -BLOCK                                |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -H[INT]=block_number                     | Required with -FREEBLOCK                                                   |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -K[EY]=key                               | Use alone                                                                  |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -R[EGION][=region] \| -R[EGION] region   | Use alone                                                                  |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -SI[BLINGS]                               | Incompatible with -FREEBLOCK, -HINT, -KEY, -REGION                        |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| :ref:`dse-help`                          | [help topic]                             | \-                                                                         |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| :ref:`dse-integrit`                      | -B[LOCK]=block_number                    | \-                                                                         |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| :ref:`dse-maps`                          | -BL[OCK]=block_number                    | Incompatible with -RESTORE_ALL                                             |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -BU[SY]                                  | Compatible only with -BLOCK                                                |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -F[REE]                                  | \-                                                                         |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -M[ASTER]                                | \-                                                                         |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -R[ESTORE_ALL]                           | Use alone                                                                  |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| :ref:`dse-open`                          | -F[ILE]=file                             | \-                                                                         |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| :ref:`dse-overwrite`                     | -B[LOCK]=block_number                    | \-                                                                         |
|                                          |                                          |                                                                            |
|                                          | -D[ATA]=string                           |                                                                            |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -O[FFSET]=offset                         | \-                                                                         |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| :ref:`dse-page`                          | \-                                       | \-                                                                         |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| :ref:`dse-range`                         | -F[ROM]=block_number                     | \-                                                                         |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -T[O]=block_number                       | \-                                                                         |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -I[NDEX]=block_number                    | \-                                                                         |
|                                          |                                          |                                                                            |
|                                          | -L[OST]=block_number                     |                                                                            |
|                                          |                                          |                                                                            |
|                                          | -[NOT]BUSY=busy/free                     |                                                                            |
|                                          |                                          |                                                                            |
|                                          | -S[TAR]=block_number                     |                                                                            |
|                                          |                                          |                                                                            |
|                                          | -L[OWER]=key                             |                                                                            |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -U[PPER]=key                             | \-                                                                         |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| :ref:`dse-remove`                        | -B[LOCK]=block-number                    | \-                                                                         |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -C[OUNT]=count                           | Incompatible with -VERSION                                                 |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -O[FFSET]=offset                         | Incompatible with -VERSION, -RECORD                                        |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -R[ECORD]=record-number                  | Incompatible with -VERSION, -OFFSET                                        |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -V[ERSION]=version-number                | Use only with -BLOCK; decimal                                              |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| :ref:`dse-restore`                       | -B[LOCK]=block-number                    | \-                                                                         |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -F[ROM]=block-number                     | \-                                                                         |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -R[EGION]=region \| -R[EGION] region     | \-                                                                         |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -V[ERSION]=version-number                | Required; decimal                                                          |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| :ref:`dse-save`                          | -B[LOCK]=block-number                    | \-                                                                         |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -C[OMMENT]=string                        | Incompatible with -LIST                                                    |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -L[IST]                                  | Incompatible with -COMMENT                                                 |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| :ref:`dse-shift`                         | -B[ACKWARD]=shift                        | Incompatible with -FORWARD                                                 |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -F[ORWARD]=shift                         | Incompatible with -BACKWARD                                                |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| \-                                       | -O[FFSET]=offset                         | \-                                                                         |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| :ref:`dse-spawn`                         | [CLI command]                            | \-                                                                         |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+
| :ref:`dse-wcinit`                        | \-                                       | \-                                                                         |
+------------------------------------------+------------------------------------------+----------------------------------------------------------------------------+

\* Use these qualifiers only with instructions from YottaDB.


