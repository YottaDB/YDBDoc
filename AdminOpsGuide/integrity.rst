.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2017-2025 YottaDB LLC and/or its subsidiaries.#
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
   Maintaining Database Integrity

==================================
11. Maintaining Database Integrity
==================================

.. contents::
   :depth: 2

This chapter discusses YottaDB methods for maintaining data availability and integrity.

A database with GDS integrity may not be consistent from an application data point of view. That is, certain types of failures that do not damage the GDS database structures may cause logical transactions (consisting of multiple database updates within an application) to stop in an "illogical" state with some, but not all, of the updates in place. Transaction processing and database journaling are good methods for maintaining application data consistency. For more information on transaction processing, refer to the `"General Language Features of M"  <../ProgrammersGuide/langfeat.html>`_ and `"Commands" <../ProgrammersGuide/commands.html>`_ chapters of the Programmer's Guide. For more information on journaling, refer to the `"YottaDB Journaling" <./ydbjournal.html>`_ chapter of this manual.

Maintaining database integrity is integral to YottaDB operation. You should seldom, if ever, need the material in this chapter, especially if you use journaling. However, databases can be corrupted by unusual events such as hardware failures, sudden loss of power, operating system failures, or improper operator actions. All such events should be followed with database integrity checks.

The chapter describes the following:

* Suggested times to use MUPIP INTEG for verifying database integrity
* Recommended methods for handling database problems
* General techniques and strategies for using DSE
* Instructions for identifying and repairing errors with DSE

--------------------------------
Verifying Database Integrity
--------------------------------

A consistent verification strategy expedites the process of rapid identification and correction of database damage, while minimizing the overhead of integrity checking. In YottaDB, this strategy is logically developed around MUPIP INTEG and its numerous options for verifying GDS integrity. For detailed information on MUPIP INTEG, refer to the `MUPIP chapter <dbmgmt.html#integ>`_. The following sections describe situations when executing MUPIP INTEG is the appropriate action.

GTMASSERT sends an operator log message in addition to the usual user message. Because these are potentially dangerous conditions, all GTMASSERTs should be immediately reported to YottaDB. Check database integrity with the -FAST qualifier, if appropriate, as soon as possible. GTMCHECK is similar to GTMASSERT but less sophisticated. It does not send an operation log message; however, it sends a message to the Principal Device.

+++++++++++++++++++++++++++++++++
Regularly Scheduled Verification
+++++++++++++++++++++++++++++++++

Schedule INTEGs at regular intervals to ensure that no unobserved or unreported events corrupt the database. These regular checks minimize the occurrence of damaged pointers, which may cause updates to incorrect places in the file, likely resulting in the escalation of damage.

++++++++++++++++++++++++++++++++
Before or After Major Transfers
++++++++++++++++++++++++++++++++

Because of the time they require, and their relative value to the total database organization, operations that move large amounts of information into or out of a database should be accompanied by an INTEG. INTEG should precede output operations such as MUPIP EXTRACT, and follow input operations such as MUPIP LOAD, RESTORE, and JOURNAL RECOVER.

One consistent occurrence of large-information-transfers occurs during database backups. In many cases, successful recovery from catastrophic events depends on having a reliable backup copy of the database. Therefore, backup procedures should be designed to complement database integrity verification. When the backup is to disk, the fastest method may be to INTEG the backup copy immediately after making it. If the backup is not in GDS format, the INTEG should precede the backup.

+++++++++++++++++++++++++++++++++++++++
Immediately after Catastrophic Events
+++++++++++++++++++++++++++++++++++++++

Any catastrophic event, such as hardware or operating system failure, should be immediately followed by an INTEG. To determine the cause of the failure, examine the system error messages, operator messages, and system log files, if available.

+++++++++++++++++++++++++++++++++++++++++++
Immediately after Runtime Database Errors
+++++++++++++++++++++++++++++++++++++++++++

Check database integrity when the YottaDB run-time system reports database access errors. The table in section R1 lists all run-time errors that indicate system problems. Most of these errors should be followed by an INTEG, or by one of the appropriate alternatives discussed in the section identified by the table.

++++++++++++++++++++++++++++++++++++
Immediately after Database Repairs
++++++++++++++++++++++++++++++++++++

Since the YottaDB run-time system normally performs GDS maintenance, based on a fairly complex set of rules, DSE depends on its operator to determine whatever subset of those rules apply to the repair. Even when you have skill and confidence, YottaDB recommends you verify the result of all database repairs with a database integrity check.

--------------------------------
Approaches to Database Recovery
--------------------------------

If you experience database integrity problems, there are three strategies to consider when approaching recovery:

* Recover with journaling
* Restore from backup and redo any lost work
* Repair the database

To achieve the intended result, correction of database errors requires careful planning. Each strategy differs from the others in the scope of damage it can handle, in skills needed, and in database availability.

+++++++++++++++++++++++++++++++
Recover from Journals
+++++++++++++++++++++++++++++++

Journaling is generally the most attractive approach to recovery from integrity problems. It allows management of recovery using logical rather than physical constructs, including suppression of updates based on time and/or source and preservation of application-level logical transactions. Backward journal recovery is generally the fastest means of repair. The cost of journaling is the added load it imposes on normal operation to make and store the journal files. For more information on journaling, refer to the `"YottaDB Journaling" chapter <./ydbjournal.html>`_.

+++++++++++++++++++++++++++
Restore from Backup
+++++++++++++++++++++++++++

Restoring the database from backup is the least technically sophisticated approach to handling integrity problems. This strategy is most beneficial when the data in the database is static or can be recomputed. In other cases, it requires operational controls to identify, and people to reenter, the work performed between the backup and the failure. For more information on MUPIP BACKUP, RESTORE, EXTRACT, and LOAD, refer to the :ref:`MUPIP chapter <mupip>`. You may also use UNIX utilities such as tar, dump, and restore.

Some database regions may be set up to hold only temporary data, typically only valid for the life of a process or even just during some operation performed by a process. Rather than restoring such a region, it is generally more appropriate to delete it and recreate it using MUPIP CREATE.

++++++++++++++++++++++++++
Repair with DSE
++++++++++++++++++++++++++

Database repair with DSE requires more skill, and potentially more time than the other approaches. Using DSE requires vigilant attention to, and a clear understanding of GDS. DSE can generally access and change almost any data in the database file. When using DSE, you assume the responsibility that YottaDB normally carries for ensuring the integrity of the database structure. Because DSE may be used concurrently with other processes, updates by concurrent processes may interfere with repair actions. When possible, prevent other users from accessing the region during repairs.

If you elect to repair the database, you may want to seek assistance from an available source of expertise such as your YottaDB support channel. If your organization plans to perform repairs beyond straightforward corrections to the file header, YottaDB strongly recommends that the responsible person(s) familiarize themselves with the material in the :ref:`INTEG section of the MUPIP chapter <mupip-integ>`, the `GDS <./gds.html>`_ and `DSE <./dse.html>`_ chapters, and this chapter. YottaDB recommends using DSE on test files, in advance of any work on production files.

+++++++++++++++++++++++++
Preventative Maintenance
+++++++++++++++++++++++++

Once you understand the cause of a database integrity problem, you can correct or improve the environment to prevent or minimize future damage. These changes may include hardware reconfiguration, such as improving the quality of power; changes to operational procedures, such as implementing journaling; and/or changes to the Global Directories, such as balancing data assignment into files of more manageable sizes.

Use the following tools to help determine the cause of a database integrity problem.

* Knowledge of the application and how it is used
* Context dumps produced by application programs
* Core dumps produced by application programs
* Core dumps produced by YottaDB
* Interviews with users to discover their actions
* Review of all recent changes to hardware, UNIX, YottaDB, the application, procedures, etc.
* Copies of damaged files
* The trail from DSE sessions in the form of notes, a script file recording the session, sequential files, and saved blocks.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Determining the cause of the Problem
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following questions may help you understand the type of information required to determine the nature of a database integrity problem.

* How seriously are operations affected?
* What level of urgency do you assign to getting the problem resolved?
* What were the circumstances under which the database became damaged or inaccessible?
* How was the problem first recognized?

Examine the accounting logs for information about recent process terminations. Capture information about what functions were in use. Look for any information which might be helpful in establishing patterns in case the problem is repetitive.

* Has the system crashed recently? If so, what caused the crash?
* Is there database damage?

  * What region(s) are affected? What globals?
  * What are the error messages?
  * What do you see when you examine the database?
  * Are you comfortable with fixing the problem?

* What version of YottaDB are you using? What version of UNIX and what UNIX platform are you running?

~~~~~~~~~~~~~~
MUPIP Recovery
~~~~~~~~~~~~~~

Bring down the damaged application using appropriate utilities, MUPIP RUNDOWN -REGION region or -FILE file-name, naming the problem database. Restart the application. Consider writing programs or procedures to partially automate shutting down one or all applications to reduce the chance of errors.

~~~~~~~~~
Follow-up
~~~~~~~~~

Make sure to transfer any relevant files or reports to YottaDB. Please also communicate any information regarding the circumstances surrounding the problem, including the answers to the questions above. Consider the following:

* Has any hardware or software component of your system recently changed?
* Was anyone doing anything new or unusual?
* Was the problem preceded or followed by any other notable events?
* Did you have any unusual problems during the analysis or recovery?
* Do you have any suggestions about this procedure?

--------------------------------
Repairing the Database with DSE
--------------------------------

Doing repairs with DSE should only be necessary if things have gone very wrong. MUPIP ROLLBACK and RECOVER are much better options in the vast majority of situations.

When using DSE:

* Always work in pairs to ensure appropriate planning and risk minimization.

* Restrict concurrent activity at least to the portion of the database on which you are working - for small changes, use CRIT -SEIZE to temporarily suspend database updates and CRIT -RELEASE to allow database updates.

* If at all possible, take a backup of the database before starting.

* Before changing a block, do a save and/or dump to a file - remember saves are in memory and lost on exit from DSE

* Address bit maps "incorrectly marked free" first - they can result in additional problems

* Address bit maps "incorrectly marked busy" last - they are protected from further problems, and can be addressed last

When doing repairs with DSE, understanding the nature of the information in the database provides a significant advantage in choosing an appropriate and efficient repair design.

For example, if you know that certain data is purged weekly, and you find damage in some of this type of data that is already five or six days old, you may be able to discard it rather than repair it. Similarly, you might find damage to a small cross-index global and have a program that can quickly rebuild it.

When you know what the data "looks" like, you are in a much better position to recognize anomalies and clues in both keys and data. For example, if you understand the format of a particular type of node, you might recognize a case where two pieces of data have been combined into a single GDS record.

+++++++++++++++++++++++++++++++
Using the proper database file
+++++++++++++++++++++++++++++++

Because DSE lets you perform arbitrary actions without imposing any logical constraints, you must ensure that they are applied to the proper file.

First, verify that ydb_gbldir names an appropriate Global Directory. Check the definition with the printenv command. You may create or use Global Directories that differ from the "normal" Global Directory. For instance, you might create a Global Directory that mapped all global names except a normally unused name to a file with integrity problems, and map that unused name to a new file. Then you could use MUPIP to CREATE the new file and use DSE to SAVE blocks from the damaged file and RESTORE them to the new file for later analysis.

When you initiate DSE, it operates on the default region specified by the Global Directory. Once DSE is invoked, use FIND -REGION to determine the available regions, and then to select the appropriate region. The technique of creating a temporary Global Directory, with the target region for the repair as the default region, prevents accidental changes to the wrong region.

++++++++++++++++++++++++++++++
Locating Structures with DSE
++++++++++++++++++++++++++++++

DSE provides the FIND command and the RANGE command for locating information.

**FIND -REGION=** redirects DSE actions to a specified region.

**FIND -BLOCK=** locates a block by using the key in the first record of the block to try to look up that block through the B-tree index. If the block is not part of the tree, or the indexing of the block is damaged, DSE reports that the search failed.

**FIND -SIBLING -BLOCK=** operates like FIND -BLOCK; however it reports the numbers of the blocks that logically fall before and after the specified block on the same level.

**FIND -EXHAUSTIVE -BLOCK=** locates a block by looking through the B-tree index for any pointer to the block. This should find the block in the case where the block is connected to the tree but the first key in the block does not match the index path. FIND -EXHAUSTIVE is useful in locating all paths to a "doubly allocated" block.

**FIND -KEY=** uses the index to locate the level zero (0) block , or data block, containing the key. If the key does not exist, it uses the index to locate the block in which it would reside. Note that FIND only works with the index as currently composed. In other words, it cannot FIND the "right" place, only the place pointed to by the index at the time the command is issued. These two locations should be, and may well be, the same; however, remind yourself to search for and take into account all information describing the failure.

**FIND -FREE -HINT** locates the "closest" free block to the hint. This provides a tool for locating blocks to add to the B-tree, or to hold block copies created with SAVE that would otherwise be lost when DSE exits. **FIND -FREE** relies on the bitmaps to locate its target, so be sure to fix any blocks incorrectly marked "FREE" before using this command.

The **RANGE** command sifts through blocks looking for keys. RANGE checks blocks without regard to whether they are in the B-tree, and without regard to whether they are marked free or busy in the bitmaps. RANGE provides a brute force way to find a key if it exists and can be very time consuming in a large database. Note that RANGE may report blocks that were previously used and were legitimately removed from the tree by an M KILL command.

++++++++++++++++++++++++++
Safety in Repairs
++++++++++++++++++++++++++

DSE is a powerful tool with few restrictions that places great responsibility on the user. Establishing the following habits can greatly increase the safety margin.

* Plan your fallback strategy before starting repairs with DSE.
* This will enable you to make the best choice between repair and restore and/or recovery strategies as your analysis proceeds. In addition, you will be able to reasonably assess the potential risks of your decision.
* Determine, at least approximately, the extent of the damage, and how much work has been done since the last backup.
* Check the existence, dates, and sizes of all files; do not assume that everything is as it "should" be.
* Estimate the time required to restore and redo the work. Determine if there are special circumstances, such as imminent deadlines.
* Consider whether you have the disk space to pursue two courses in parallel.
* Consider whether you should back up the damaged database for additional protection or for later analysis.
* Before changing any block in the database, always use the DSE SAVE command to make an in-memory copy of that block.

  If a modification fails to accomplish its intended goal, you can use the DSE RESTORE command to get the block back to its previous state. For instance, a CHANGE -BSIZ= that specifies a smaller block size causes DSE to discard all information falling beyond the new size.

  An important aspect of this strategy is recognizing that testing some modifications requires using other tools such as MUPIP INTEG, but once you leave DSE to invoke MUPIP you lose anything saved in memory. To avoid this problem, use SPAWN to access those tools.

  To save a copy of the block for further analysis, SAVE it, and then RESTORE it to an empty block. The best place to put such a copy, using RESTORE -REGION=, is in a special region created just to receive such blocks.

  Alternatively, you can RESTORE it temporarily in a free block within the region, preferably near the end of the file. If you RESTORE the block to the original database, it may be overlaid when normal operation requires more blocks. You may prevent this overlay by using MAP -BUSY on the target block of the RESTORE. However, this causes INTEG to report "incorrectly marked busy" errors.

* After changing a block, always check the quality of the result by using the DSE INTEG command.

  DSE INTEG does not check the placement of the block in the tree. It checks only the single block specified explicitly with the -BLOCK= qualifier or implicitly (the current block) when -BLOCK= is omitted. If you need to verify the index structure related to a block, SPAWN and use MUPIP INTEG -REGION -FAST, possibly with the -BLOCK or -SUBSCRIPT qualifiers.

  Specifying -BLOCK= tends to avoid incorrect assumptions about which block DSE last handled. Not specifying -BLOCK= tends to minimize typographical errors in identifying the block.

+++++++++++++++++++++++++++
Discarding Data
+++++++++++++++++++++++++++

When you must discard a block or a record, take steps to preserve or create structures that have integrity.

DSE has no single command that discards a block. You must locate the last block in its path with FIND [-BLOCK] or FIND -EXHAUSTIVE and REMOVE the record that points to the block being discarded. Then MAP the deleted block -FREE.

When you discard the only record in any block you must MAP that block -FREE and REMOVE the record (up one level) that points to the deleted block. The only exception is when it is the only block pointed to by the root block of the tree. Leaving empty blocks (except as the data level of empty or undefined globals) violates standard operating assumptions of GDS databases.

When you must discard the top block in a Global Variable Tree, you can alternatively use the method employed by YottaDB when it processes a KILL command. This method maintains a record of the global variable name. To use this method, use FIND -FREE to locate a free block, and MAP the new block -BUSY. Next, CHANGE the new block -BSIZ=header-size (7/8) -LEVEL=0. Finally, CHANGE the top level block -BSIZ=header-size (7/8) -LEVEL=1 and ADD -STAR -POINTER=the-new-block.

Never delete the only remaining record in block one (1). Block one (1) is the root block of the Directory Tree for the entire file.

++++++++++++++++++++++
Concurrent Repairs
++++++++++++++++++++++

DSE can operate concurrently with normal access by the YottaDB run-time system. This lets you perform an investigation and some types of repairs with minimal disruption.

Some repairs should only be undertaken by a process that has standalone access to the database, while other repairs present no danger when performed with other users accessing the file. However, there is still some risk with the latter type of repairs, depending on the "placement" of the error and the likelihood of concurrent access to that area of the database.

Unless availability is a critical problem, YottaDB recommends performing all repairs in standalone mode to ensure the safety of data. For environments where availability is an issue, your knowledge of the application and how it is used are the best guides in assessing the risk of performing concurrent repairs. To help you assess the amount of risk, the following sections identify repairs that should only be undertaken with standalone access.

If you attempt concurrent repairs, plan the order of your updates carefully. Always REMOVE the index record that points to a block before using MAP -FREE on that block. Always MAP a block -BUSY and assure that it meets GDS design criteria and accomplishes the repair goal before using ADD to create an index record that points to that block.

++++++++++++++++++++++
Terminating Processes
++++++++++++++++++++++

In performing some types of repairs, you may have to stop one or more processes. You can choose from several methods.

* If the process' principal device is not available, or the process does not respond to pressing <CTRL-C>, use MUPIP STOP. This allows YottaDB to disengage the process from all shared resources, such as I/O devices and open database files.
* The DSE command CRITICAL -INITIALIZE -RESET causes YottaDB to terminate all images that are actively accessing the target database. This DSE command has a similar effect on processes to that of MUPIP STOP , except that it simultaneously terminates all processes actively using a database.
* Finally, if the process does not respond to MUPIP STOP, use KILL -9. This terminates the process abruptly and may leave database files improperly closed and require a MUPIP RUNDOWN. Since KILL -9 may cause database damage, it should be followed by a MUPIP INTEG.

When processes have stopped or terminated abnormally, YottaDB recommends shutting down all YottaDB processes, checking the integrity of the database, then restarting the processes. First, use ps -af to determine the process IDs. Then use MUPIP STOP or KILL -15 to terminate all the YottaDB processes. Repeat the ps -af command to assure that all processes have terminated. If they have not, use KILL -9 instead of KILL -15.

When you have terminated all processes, do a MUPIP RUNDOWN on all database files:

.. code-block:: bash

   mupip rundown -file <name of database>

Use the UNIX ipcs utility to examine the states of message queues, shared memory, and semaphores. If any of these resources are left from the processes that have just been killed, use the UNIX ipcrm utility to remove them. Refer to `"Appendix A" <./ipcresource.html>`_ for more information.

.. note::
   Use ipcrm with extreme care, as removing the wrong resources can have disastrous results.

Example:

.. code-block:: bash

   ipcs
   IPC status from /dev/kmem as of Sat Feb 16 13:13:11 1999
   T     ID     KEY        MODE       OWNER    GROUP
   Shared Memory:
   m   1800 0x01021233 --rw-rw-rw-      uuu      dev
   m     91 0x01021232 --rw-rw-rw-      uuu      dev
   Semaphores:
   s   1360 0x01021233 --ra-ra-ra-      uuu      dev
   s     61 0x01021232 --ra-ra-ra-      uuu      dev

This shows the state of these resources with a user uuu working on two databases -m1800 -s1360 and -m91 -s61.

Check the integrity of the database:

.. code-block:: bash

   mupip integ -file <name of database>

To preserve database integrity, always verify that all YottaDB images have terminated and all GDS databases are RUNDOWN before shutting down your system.

Terminating YottaDB abnormally with KILL -9 can leave the terminal parameters improperly adjusted, making them unsuited for interactive use. If you terminate YottaDB with KILL -9 without terminating the job, logout to reset the terminal characteristics.

++++++++++++++++++++++++++++++++++++++++++++
Recovering Data from Damaged Binary Extracts
++++++++++++++++++++++++++++++++++++++++++++

~~~~~~~~~~~~~~
CORRUPT Errors
~~~~~~~~~~~~~~

You can recover the value of a corrupt global using the global variable name and the dump (in ZWRITE format) of the rest of the block from the point of corruption and then insert it into the database.

Because the ZWRITE format is used for reconstructing the value of the global, the part of the block after the point of corruption may contain internal structures, for example, a record header and other globals. Therefore, always take extra precautions while identifying the value portion of the global. In addition, ZWRITE format displays byte values as characters whenever it can. This may not reflect the actual usage of those bytes, for example, for internal structures. If the extract is damaged, you might need to do additional work to reconstruct the value.

After you reconstruct the value of a global, add it to the database using an M SET command. For very long values, build the value by using successive SETs with the concatenation operator or SET $EXTRACT().

~~~~~~~~~~~~~~~~~~~~~
LDSPANGLOINCMP Errors
~~~~~~~~~~~~~~~~~~~~~

To fix an LDSPANGLOINCMP error, use the following to reconstruct the value of the global and insert it into the database.

* The global variable name of the spanning node which has the LDSPANGLOINCMP error.
* The ZWRITE dump of the partial value corresponding to that global variable name, that is, whatever was accumulated.
* The global variable name found in the record.
* ZWRITE dump(s) of the errant chunk(s) from the point of corruption.

The conditions that lead to an LDSPANGLOINCMP error are as follows:

.. code-block:: none

   Case SN1 - While loading a spanning node the next record contained a non-spanning node:
   "Expected chunk number : ccccc but found a non-spanning node"

The partial value can be used as the basis for reconstructing the spanning node.

.. code-block:: none

   Case SN2 - While loading a spanning node the next record did contain the expected chunk:
   "Expected chunk number : ccccc but found chunk number : ddddd"

Use the partial value and the errant chunk as the basis for reconstructing the spanning node. After encountering this error, the binary load continues looking for the next global variable. If there are additional chunks from the damaged spanning node in the binary extract file, there is a case SN3 error for each of them. Use the errant chunk dumps from them as part of the reconstruction.

.. code-block:: none

   Case SN3 - Not loading a spanning node but found a record with a spanning node chunk:
   "Not expecting a spanning node chunk but found chunk : ccccc"

This can be the result of an immediately prior case SN2 error (as described in prior paragraphs) or an isolated errant chunk.

.. code-block:: none

   Case SN4 - While loading a spanning node adding the next chunk caused the value to go over expected size:
   "Global value too large: expected size : sssss actual size : tttttt chunk number : ccccc"

Adding the next chunk caused the value to go over the expected size. Examine the partial value and errant chunk dump.

.. code-block:: none

   Case SN5 - While loading a spanning node all of the chunks have been added but the value is not the expected size:
   "Expected size : sssss actual size : ttttt

All of the chunks were found but the size of the value is not what was expected.

**Example- Repairing an Error in a Binary Extract**

Here is an example for repairing an error in a binary extract.

Assume that during the load of a binary extract, you get the following error:

.. code-block:: bash

   %YDB-E-LDSPANGLOINCMP, Incomplete spanning node found during load
           at File offset : [0x0000027E]
           Expected Spanning Global variable : ^mypoem
           Global variable from record: ^mypoem(#SPAN32)
           Expected chunk number : 3 but found chunk number : 32
           Partial Value :
   "Half a league, half a league, Half a league onward, All in the valley of Death Rode the six hundred.
   Forward, the Light Brigade! Charge for the guns he said: Into the valley of Death Rode the six hundred.
   Forward, the Light Brigade! Was there a man dismayed? Not tho the soldiers knew Some one had blundered:
   Theirs not to make reply, Theirs not to reason why, Theirs but to do and die: Into the valley of Death Rode the six hundred.
   Cannon to the right of them, Cannon to the left of "
           Errant Chunk :
   "them, Cannon in front of them Volleyed and thundered;
   Stormed at with shot and shell, Boldly they rode and well, Into the jaws of Death, Into the mouth of Hell Rode the six hundred.
   Flashed all their sabres bare, Flashed as they turned in air Sabring the gunners there, Charging an army while All the world wondered:
   Plunged in the battery-smoke Right thro the line they broke; Cossack and Russian Reeled from the sabre-stroke Shattered and sundered.
   Then they rode back, but no"
   %YDB-E-LDSPANGLOINCMP, Incomplete spanning node found during load
           at File offset : [0x00000470]
           Global variable from record: ^mypoem(#SPAN4)
           Not expecting a spanning node chunk but found chunk : 4
           Errant Chunk :
   "t Not the six hundred.
   Cannon to the right of them, Cannon to the left of them, Cannon behind them Volleyed and thundered;
   Stormed at with shot and shell, While horse and hero fell, They that had fought so well Came thro the jaws of Death,
   Back from the mouth of Hell, All that was left of them, Left of six hundred.
   When can their glory fade? O the wild charge they made! All the world wondered.
   Honour the charge they made! Honour the Light Brigade, Noble six hundred!"

Because the only issue in this case is that one of the chunks' keys has been damaged, put the value back together from the partial value and the contents of the errant chunks.

Execute:

.. code-block:: bash

   $ $ydb_dist/yottadb -direct

From the first error message pick :

.. code-block:: bash

   Expected Spanning Global variable : ^mypoem

Use it together with the partial value:

.. code-block:: bash

   YDB>set ^mypoem="Half a league, half a league, Half a league onward, All in the valley of Death Rode the six hundred.
   Forward, the Light Brigade! Charge for the guns he said: Into the valley of Death Rode the six hundred.
   Forward, the Light Brigade! Was there a man dismayed? Not tho the soldiers knew Some one had blundered:
   Theirs not to make reply, Theirs not to reason why, Theirs but to do and die: Into the valley of Death Rode the six hundred.
   Cannon to the right of them, Cannon to the left of "

Add in the chunk that has the bad internal subscript:

.. code-block:: bash

   YDB>set ^mypoem=^mypoem_"them, Cannon in front of them Volleyed and thundered;
   Stormed at with shot and shell, Boldly they rode and well, Into the jaws of Death, Into the mouth of Hell Rode the six hundred.
   Flashed all their sabres bare, Flashed as they turned in air Sabring the gunners there, Charging an army while All the world wondered:
   Plunged in the battery-smoke Right thro the line they broke; Cossack and Russian Reeled from the sabre-stroke Shattered and sundered.
   Then they rode back, but no"

Finally, add the last chunk for that spanning node:

.. code-block:: bash

   YDB>set ^mypoem=^mypoem_"t Not the six hundred.
   Cannon to the right of them, Cannon to the left of them, Cannon behind them Volleyed and thundered;
   Stormed at with shot and shell, While horse and hero fell, They that had fought so well Came thro the jaws of Death,
   Back from the mouth of Hell, All that was left of them, Left of six hundred.
   When can their glory fade? O the wild charge they made!  All the world wondered.
   Honour the charge they made! Honour the Light Brigade, Noble six hundred!"

You have successfully reconstructed the global from the damaged binary load:

.. code-block:: bash

   YDB>w ^mypoem
   Half a league, half a league, Half a league onward, All in the valley of Death Rode the six hundred.
   Forward, the Light Brigade! Charge for the guns he said: Into the valley of Death Rode the six hundred.
   Forward, the Light Brigade! Was there a man dismayed? Not tho the soldiers knew Some one had blundered:
   Theirs not to make reply, Theirs not to reason why, Theirs but to do and die: Into the valley of Death Rode the six hundred.
   Cannon to the right of them, Cannon to the left of them, Cannon in front of them Volleyed and thundered;
   Stormed at with shot and shell, Boldly they rode and well, Into the jaws of Death, Into the mouth of Hell Rode the six hundred.
   Flashed all their sabres bare, Flashed as they turned in air Sabring the gunners there, Charging an army while All the world wondered:
   Plunged in the battery-smoke Right thro the line they broke; Cossack and Russian Reeled from the sabre-stroke Shattered and sundered.
   Then they rode back, but not Not the six hundred. Cannon to the right of them, Cannon to the left of them, Cannon behind them Volleyed and thundered;
   Stormed at with shot and shell, While horse and hero fell, They that had fought so well Came thro the jaws of Death,
   Back from the mouth of Hell, All that was left of them, Left of six hundred.
   When can their glory fade? O the wild charge they made! All the world wondered.
   Honour the charge they made! Honour the Light Brigade, Noble six hundred!

.. _find-fix-db-errs:

------------------------------------
Finding and Fixing Database Errors
------------------------------------

The rest of this chapter is arranged loosely in the form of a decision tree. The material covers a wide range of scenarios and possible actions.

As you begin the decision-making process, follow these general guidelines from this point:

*IF THE SYMPTOM IS A FAILURE TO PROCESS*, refer to :ref:`h1-process-hangs`.

*IF THE SYMPTOM IS A MUPIP INTEG ERROR REPORT*, refer to :ref:`i1-mupip-integ-errors`. If you are investigating a particular error message, refer to the MUPIP INTEG errors table.

*IF THE SYMPTOM IS A RUN-TIME ERROR REPORT*, refer to :ref:`r1-runtime-errors`. If you are investigating a particular error message, refer to the Runtime Error Messages table.

To facilitate use of the material as a troubleshooting guide, the text in these sections refers to other sections with alphanumeric designators. Each alphanumeric section describes suggested actions to employ in handling a particular situation.

+++++++++++++++++++++++++++++++++++++
C1 - Possible Cache Control Problems
+++++++++++++++++++++++++++++++++++++

When a process detects that a normal cache operating principal has been violated, or that a cache operation is taking an unexpectedly long time, that process triggers a cache verification and rebuild. Such events can be caused by abnormal process termination, or by inappropriately configured or managed database storage subsystems.

When such an event occurs, YottaDB sends a series of messages to the operator facility describing the results of the cache verification. If the cache rebuild is successful, no further immediate action is required. If the cache rebuild fails, the database administrator must close off access to the database and use DSE (CRIT and WCINIT) and MUPIP (INTEG) to reset the cache manually and verify that the database is not damaged.

If such events are delivered to the operator facility, you should investigate whether it is appropriate to modify your procedures to prevent abnormal termination, to reconfigure your disk subsystem, or to change the nature or schedule of disk activities so that database access is not disrupted during key periods of operation.

Also see section :ref:`r3-runtime-database-cache-problems`.

.. _h1-process-hangs:

+++++++++++++++++++++++++++
H1 - Process Hangs
+++++++++++++++++++++++++++

The term "hang" refers to a failure to process. Processes may hang for a variety of reasons that have nothing to do with YottaDB. However, hanging YottaDB processes may indicate that a database has become inaccessible. When you suspect a hang, first determine the extent of the problem.

Your tools include:

* Knowledge of the application and how it is used
* Communication with users
* The ps command and other UNIX system utilities

*WHEN MANY PROCESSES ON A SYSTEM ARE HANGING*, determine if the hangs are confined to a particular application. If all applications are affected or if processes not using YottaDB databases are affected, the problem is not a database-specific problem but something more general, such as a UNIX problem. Refer to :ref:`h6-unix-problems`.

*WHEN ONLY ONE PROCESS IS HANGING*, find out whether that process is the only one using a particular YottaDB application. If it is the only process, start some appropriate second process and determine whether the second process is also affected.

*IF A PROCESS HANGS WHILE OTHER PROCESSES ACCESSING THE SAME DATABASE CONTINUE TO PROCESS*, the problem is not a database problem. Refer to :ref:`h8-application-problems`.

*WHEN ONLY YottaDB PROCESSES RUNNING A PARTICULAR APPLICATION HANG*, the problem may be a database problem.

Is the system "hung?" If so, consider the following additional questions:

* Does LKE work? If not, then a database has problems (see below).

  * Are there locks owned by a nonexistent process? Can they be cleared? What were the circumstances of a process leaving locks?
  * Are there locks which are not changing? What is the state of the owning process(es)? If not all processes are hung, can the stalled process(es) be MUPIP STOPped?

* Does some region have a "persistent" owner of the critical section (crit)? Which one(s)?
* If there is a crit owner, what is its state? If it is a nonexistent process can it be -REMOVED?
* Does a CRIT -INIT -RESET free the section or just change who owns it?
* If CRIT -INIT -RESET doesn't free the problem, the cache is damaged.

The following is another way of testing the cache: If CRIT is cleared and DSE BUFFER hangs, the cache is not working. Use MUPIP STOP and/or CRIT -INIT -RESET to get everyone out of the segment, then use DSE WCINIT. After a WCINIT, make sure that you can successfully exit from DSE. Use MUPIP INTEG (-FAST) to check for damage which can be induced by WCINIT.

.. _h3-database-access-problems:

++++++++++++++++++++++++++++++
H3 - Database Access Problems
++++++++++++++++++++++++++++++

Use the following diagnostic steps and references to determine an appropriate course of action for database access problems.

* Determine if the disk volume is inaccessible.
* Use the UNIX ls utility to display information retrieved from the volume. If the volume is not accessible to UNIX, the problem is not a database problem. Refer to :ref:`h7-disk-hardware-problems`.
* Determine whether UNIX can write to the disk.
* Use a shell command such as mv or cp. If UNIX cannot write to the volume, the problem is not a database problem. Refer to :ref:`h7-disk-hardware-problems`.
* Determine whether any database file used by the application has "Cache Freeze" set.

  Use DSE FIND -REGION=region and DUMP -FILEHEADER to verify that CACHE FREEZE is zero (00000000) for any hung region(s).

  If CACHE FREEZE shows a PID, that process used MUPIP or DSE to FREEZE the database. In this case, investigate whether the process is currently producing the desired results. If the FREEZE is legitimate, do whatever is appropriate to speed up the process using FREEZE. For example, use the NICE command. If the process still exists, but should not be running at this time, stop it. If CACHE FREEZE is non-zero but not in use to protect the database, use DSE FIND -REGION=region and CHANGE -FILEHEAD -FREEZE=FALSE to clear the FREEZE state.

  Use the DSE commands FIND -REGION and DUMP -FILEHEADER. If any region is frozen, determine who initiated the freeze, and whether the process should be terminated or allowed to complete. The following actions freeze databases:

  * DSE CHANGE -FILEHEADER -FREEZE=TRUE
  * DSE ALL -FREEZE
  * MUPIP BACKUP -NOONLINE
  * MUPIP FREEZE
  * MUPIP INTEG -REGION
  * MUPIP EXTRACT -FREEZE

  DSE CHANGE -FILEHEADER -FREEZE=FALSE and MUPIP FREEZE -OFF clear a freeze. However, when used with -OVERRIDE, these commands may cause damage to the results of the process that initiated the freeze. After the freeze is cleared, re-examine the entire situation.

* Determine whether the database files used by the application are accessible for reading.

  Use an M function such as $DATA() or $ORDER().

* Determine whether the database files used by the application are accessible for writing.

  SET a safe dummy node in each database and then KILL it if appropriate. Alternatively make a harmless modification to an existing node, or modify a node and then change it back to its original value. YottaDB detects when a SET makes no actual change (the SET value is the same as the original value) and does not perform an actual update in that case.

*IF THE DATA CAN BE BOTH READ AND WRITTEN*, the problem is not a database problem. Refer to :ref:`h8-application-problems`.

*IF DATA CANNOT BE READ OR WRITTEN*, some process is unable to release full ownership of the database critical section. Determine the process identification number (PID) of the process using the DSE command CRITICAL. If the process exists, refer to :ref:`h4-database-cache-problems`. If the process is non-existent, use DSE CRITICAL -REMOVE to emulate a release and re-examine the entire situation.

Example:

.. code-block:: none

                Set pipe="pipe"
                Open pipe:(command="/bin/csh")::pipe
                Use pipe
                Set reg="",cmd=$ztrnlnm("gtm_dist")_"/mupip dumpfhead "
                For  Set reg=$View("GVNEXT",reg) Quit:""=reg  Do
                . Set reg(reg)="",file=$view("GVFILE",reg)
                . Write cmd,file,!
                . For i=1:1 read x(i):1 Quit:(x(i)["sgmnt_data.freeze")!$ZEOF!'$Test
                . Set pid=+$Piece(x(i),"=",2)
                . Set:pid frozen(reg)=pid
                Close pipe
                Set g="^%",$etrap="Write $ZStatus Set $ecode="""" Quit"
                Write !,"Attempting read access"
                If $Data(^%) Set reg=$View("REGION",g) Do read1
                For  Set g=$Order(@g) Quit:""=g  Set reg=$View("REGION",g)  Do:""=(reg(reg)) read1
                Set reg=""
                Write !!,"Attempting write access"
                For  Set reg=$Order(reg(reg)) Quit:""=reg  Do write1
                Write !
                Quit
        read1
                Write !,"Read in region: ",reg," of ",g," successful"
                If ($Data(@g)#2) Set reg(reg)=g
                Else  Set reg(reg)=$Query(@g)
                Quit
        write1
                If $Data(frozen(reg)) Write !,"Region ",reg," Frozen by PID ",frozen(reg) Quit
                If ""=reg(reg) Write !,"Region ",reg," has no data" Quit
                Write !,"Write to region: ",reg
                Set x=$Get(@reg(reg),"Yndef")
                Set @reg(reg)=1,@reg(reg)=x
                If "Yndef"=x ZKill @ref(ref); assumption that a value of Yndef is very unlikely
                Write " of ",reg(reg)," successful"
                Quit

This routine provides a generalized approach to automating some of the tasks described in this section. It contains argumentless DO commands primarily for typesetting reasons. The routine issues a report if any region is frozen, but does not report which regions are in that state. It may hang while reading or writing a database. However, unless the region(s) holding ^% and the next global after ^% has a problem, it displays the name of the region that it is about to try. If this routine runs to completion, the databases in the current Global Directory are completely accessible. The limitations of this routine can be overcome by writing custom shell scripts and/or M programs that include embedded information about one or more Global Directories.

.. note::
   If you have a Global Directory mapping globals to multiple files, you may create an alternative Global Directory using different mappings to those same files. Such a mapping prevents the test program(s) from touching the "real" data.

Example:

.. code-block:: none

   Mapping      Production region   Test region
   -----------------------------------------------
   A   to   M
   $DEFAULT            SCRATCH
   N   to   Z   SCRATCH
   $DEFAULT

.. _h4-database-cache-problems:

++++++++++++++++++++++++++++
H4 - Database Cache Problems
++++++++++++++++++++++++++++

To increase the access speed, YottaDB buffers data exchanged between processes and database files in the shared memory cache. If information in the memory cache is damaged, it can block the transfer of data to the disk.

*IF A PROCESS HAS BEEN DETERMINED (FROM SECTION H3) TO NEVER RELEASE FULL OWNERSHIP OF THE DATABASE CRITICAL SECTION*, there may be a problem with the database cache. To determine where the problem is occurring terminate the process. If this clears the hang, the problem was not in the database but in the process, which was somehow damaged. Refer to :ref:`p1-process-damage`. Otherwise, another process showing the same symptoms takes the place of the terminated process. In this case, the cache is damaged.

*IF THE CACHE IS DAMAGED*, it must be reinitialized. It is crucial to stop all other database activity during cache initialization. Refer to :ref:`q1-restricting-database-access` before continuing with this section.

To minimize database damage due to cache reinitialization, and to confirm that the problem is due to a damaged cache, use the DSE command CRITICAL SEIZE followed by BUFFER_FLUSH. The DSE command BUFFER_FLUSH attempts to flush the database cache which is a benign operation. Wait at least one minute for this operation to complete.

*IF THE BUFFER_FLUSH DOES NOT HANG*, the cache is not damaged, and you should review all previous steps starting with :ref:`h1-process-hangs`.

*IF THE BUFFER_FLUSH DOES HANG*, use the DSE command WCINIT to reinitialize the cache. This command requires confirmation. Never use WCINIT on a properly operating database. After a WCINIT always perform at least a MUPIP INTEG FAST to detect any induced damage that has a danger of spreading. If the WCINIT command hangs, clear the critical section as described in :ref:`h5-critical-section-problems` and reissue the WCINIT.

.. _h5-critical-section-problems:

++++++++++++++++++++++++++++++
H5 - Critical Section Problems
++++++++++++++++++++++++++++++

The concurrency control mechanism allows only one process at a time to execute code within a "critical section." To gain access to the database requires a process to first gain ownership of the critical section. The errors described in this section occur when a problem occurs in ownership control of the critical section.

*IF YOU HAVE DETERMINED WHICH PROCESS IS HOLDING THE CRITICAL SECTION* (from section H2 using system utilities), try terminating that process. If this corrects the problem, the damage was to the process, rather than the critical section. Refer to :ref:`p1-process-damage`.

*IF YOU CANNOT IDENTIFY THE PROCESS*, or if terminating such a process causes other processes to exhibit the same problem(s), the critical section is damaged and must be reinitialized. Restrict database activity during the reinitialization. Refer to :ref:`q1-restricting-database-access` before continuing with this section.

*TO REINITIALIZE THE DATABASE CRITICAL SECTION*: Reinitializing a critical section on an active database file carries some risk of causing database damage. You can minimize this risk by restricting database activity during the reinitialization. Refer to :ref:`q1-restricting-database-access` before continuing with this section.

The DSE command CRITICAL INITIALIZE RESET re-establishes the database-critical section and induces errors for all processes currently accessing the database in question. You can avoid the induced errors in other processes by dropping the RESET qualifier. However, this technique may result in other processes attempting to use partially created critical section structures, possibly corrupting them or the database contents.

After the CRITICAL INITIALIZE, use the DSE commands CRITICAL SEIZE and CRITICAL RELEASE to verify operation of the critical section. Actions such as those described in :ref:`h3-database-access-problems` test more thoroughly for proper operation.

.. _h6-unix-problems:

+++++++++++++++++++++++++
H6 - UNIX Problems
+++++++++++++++++++++++++

*IF YOU HAVE DETERMINED THAT MANY PROCESSES IN THE UNIX ENVIRONMENT ARE PERFORMING BADLY*, some processes may be using priorities to "hijack" the system. If this is the case, review why priorities are being adjusted and take appropriate action. Otherwise, you may have a UNIX-related problem.

.. _h7-disk-hardware-problems:

++++++++++++++++++++++++++++
H7 - Disk Hardware Problems
++++++++++++++++++++++++++++

* IF YOU HAVE DETERMINED THAT A DISK VOLUME IS INACCESSIBLE TO THE OS FOR READ AND/OR WRITE, use the df command to check that the correct volume is properly mounted. If the volume cannot be written, examine the physical device to see whether write lock switches or plugs have been disturbed.

* IF YOU CANNOT LOCATE THE PROBLEM, run disk diagnostics. Be aware that many disk diagnostics are destructive (that is, destroy your files). Avoid these diagnostics until you have exhausted all other avenues. If you have to run destructive disk diagnostics, or you determine that a disk spindle must be replaced, start planning for the recovery immediately.

.. _h8-application-problems:

++++++++++++++++++++++++++++++
H8 - Application Problems
++++++++++++++++++++++++++++++

Application problems may be caused by conflicting M LOCKs or OPEN commands in more than one process, or by a process waiting for completion of M READ or JOB command, which is dependent on an asynchronous event.

First, determine if processes are waiting, without relief, for M LOCKs using the LKE command SHOW ALL WAITING. M routines use LOCK commands to create mutual exclusion semaphores.

*IF THE SHOW COMMAND HANGS*, you have a cache or critical section problem. Restart your evaluation in :ref:`h5-critical-section-problems`.

*IF THE SHOW COMMAND DISPLAYS NO LOCKS WAITING*, the problem is not a LOCK problem. If repeated use of SHOW does not display the one or more LOCKs that persist every time, the problem is not a LOCK problem. However, even if the problem is not a lock problem, continue with this section because it discusses the M commands JOB, OPEN, and READ, which may also produce hangs.

A LOCK identified as belonging to a non-existent process results from an abnormal process termination. YottaDB automatically clears such LOCKs when some other process requests a conflicting LOCK.

~~~~~~~~~~~~~~~~
Persistent Locks
~~~~~~~~~~~~~~~~

Persistent LOCKs belonging to currently existing processes are best released by terminating those processes. Using the LKE command CLEAR with various qualifiers can clear LOCKs, but may cause the routines using the LOCKs to produce inappropriate results. For more information on LKE, refer to the `"M LOCK Utility" <./mlocks.html>`_ chapter.

The two most common reasons for persistent LOCKs are deadlocks and LOCKS held during operations that take indeterminate amounts of time.

~~~~~~~~~
Deadlocks
~~~~~~~~~

Deadlocks occur when two or more processes own resources and are trying to add ownership of an additional resource already owned by another of the deadlocked processes.

Example:

.. code-block:: none

   Process 1       Process 2
   ---------       ---------
   LOCK ^A         LOCK ^B
   LOCK +^B        LOCK +^A

This shows a sequence in which Process 1 owns ^A and Process 2 owns ^B. Each process is trying to get the resource owned by the other, while "refusing" to release the resource it owns.

Example:

.. code-block:: none

   Process 1       Process 2        Process 3
   ---------       ---------        ---------
   LOCK ^A         LOCK ^B          LOCK ^C
   LOCK +^B        LOCK +^C         LOCK +^A

This is similar to the previous example, except that it involves three processes. When an application uses LOCKs in a complex fashion, deadlocks may involve many processes.

~~~~~~~~~~~~~~~~~~~~
Preventing Deadlocks
~~~~~~~~~~~~~~~~~~~~

You can prevent deadlocks by using timeouts on the LOCK commands. Timeouts allow the program to recognize a deadlock. Once a routine detects a deadlock, it should release its LOCKs and restart execution from the beginning of the code that accumulates LOCKs. Without timeouts, there is no way in M to break a deadlock. You must use outside intervention to terminate at least one deadlocked process, or use LKE to strip a LOCK from such a process.

Example:

.. code-block:: none

   for  quit:$$NEW
   quit
  NEW()  lock ^X(0)
   set ^X(0)=^X(0)+1
   quit $$STORE(^X(0))
  STORE(x)
   lock +^X(x):10 if  set ^X(x)=name_"^"_bal
   lock
   quit $TEST

This uses a timeout on the LOCK of ^X(x) to cause a retry of NEW.

In addition to the LOCK command, the M JOB, OPEN, and READ commands can contribute to deadlocks.

Example:

.. code-block:: none

   Process 1         Process 2
   ---------         ---------
   LOCK ^A
                     OPEN "MSA0:"
                     OPEN "/dev/nrst0"
   OPEN "MSA0:"
   OPEN "/dev/nrst0"
                     LOCK +^A

This shows a sequence in which Process 1 owns ^A and Process 2 owns device /dev/nrst0. Again, each is trying to get the resource held by the other. Notice that the LOCK commands could be replaced by OPEN commands specifying some non-shared device other than /dev/nrst0.

An application may combine the technique of timeouts on "long" commands to protect the current process, with the technique of minimizing LOCK and OPEN durations, to minimize conflicts with other processes.

Another type of application hanging occurs when a process acquires ownership of a resource and then starts an operation that does not complete for a long period of time. Other processes that need the unavailable resource(s) then hang.

Example:

.. code-block:: none

   Process 1         Process 2
   ---------         ---------
   LOCK ^A
   READ x
                     LOCK ^A

If the READ by Process 1 is to an interactive terminal, and the operator has abandoned that device, the READ may take what seems, at least to Process 2, forever. The M commands OPEN and JOB, as well as READ, can produce this problem. When this situation arises, take action to get long-running commands completed or to terminate the process performing those commands.

There are two programming solutions that help avoid these situations. You can either limit the duration of those commands with timeouts, or defer resource ownership until any long operations are complete.

Example:

.. code-block:: none

   for  quit:$$UPD
   quit
  UPD()  set x=^ACCT(acct)
   do EDITACCT
   lock ^ACCT(acct)
   if x=^ACCT(acct) set ^ACCT(acct)=y
   else  write !,"Update conflict-Please Reenter"
   lock
   QUIT $TEST

This stores the contents of ^ACCT(acct) in local variable x, before the interactive editing performed by sub-routine EDITACCT (not shown). When the interaction is complete, it LOCKs the resource name and tests whether ^ACCT(acct) has been changed by some other process. If not, it updates the global variable. Otherwise, it informs the user and restarts UPD. This technique eliminates the "open update" problem, but it introduces the possibility the user may have to re-enter work. An application that needs to minimize the possibility of re-entry may extend this technique by testing individual fields (pieces) for conflicting changes.

.. _i1-mupip-integ-errors:

++++++++++++++++++++++++++++++++++++++
I1 - MUPIP INTEG Errors
++++++++++++++++++++++++++++++++++++++

Database errors reported by MUPIP INTEG differ in impact and severity. Some require an immediate action to prevent extending the damage. Action on other less severe errors may be delayed.

The next section provides general guidelines for determining your next course of action and a table with information related to the error messages you may encounter.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Evaluating the Danger Level of a Database Problem
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you encounter an anomaly in your database or its operations, the following list may offer some help in determining your next course of action. The heading of each section indicates the level of urgency YottaDB attributes to those items listed below it.

Requires Immediate Attention:

* Block incorrectly marked free errors are very serious and lead to accelerating damage. They degenerate into block doubly-allocated errors, which are also very dangerous. A database with these errors should be closed immediately for repairs.
* Any (structural) error in an index block is dangerous and should be repaired as soon as possible.

Repairs for such errors should also be performed on a database that has been closed to normal activity. The need for both of these actions occurring quickly arises from the likelihood of the bad index being used. Defer repairs only if your knowledge of the application allows you to predict that a damaged area is used exclusively by restricted functions which are not active (e.g., monthly processing or purges).

Can be Deferred:

* Any (structural) error in a data block (level 0) does not pose a threat of accelerating damage. However, level 0 errors may cause errors or unreliable behavior in the application.
* Block "incorrectly marked busy" errors only result in database space becoming unavailable until the errors are corrected. An index block error generates incorrectly marked busy errors, because INTEG cannot process the descendants of the damaged index. Therefore, incorrectly marked busy errors should be corrected only after all other errors, except for bitmap errors, are corrected.
* Any bitmap errors flag not only the incorrectly marked block, but also the associated bitmap, and sometimes the master map. Therefore, local and master map errors should be corrected only after all bitmap marked busy or free errors are corrected.
* Transaction number errors usually impact only incremental and online backups.
* File size errors can misdirect MUPIP but do not cause the YottaDB run-time system to generate further errors. An exception is auto-extend, which may not work properly if there are file size errors.
* Reference count errors, free block count errors, and block size exceeding user-specified limit errors are informational only.

~~~~~~~~~~~~~~~~~~~~~~~~~~
MUPIP INTEG Error Messages
~~~~~~~~~~~~~~~~~~~~~~~~~~

The following list of INTEG messages classifies error severity using the following codes, and refers you to a section identifying appropriate follow-up action.

* A Access: prevents database access - these indicate an operational error or a fundamental file damage that prevents INTEG from providing more information.
* B Benign: presents no risk of additional damage and has little or no effect on database performance; many bit map errors come under this nature code, but "incorrectly marked free errors" are dangerous.
* D Dangerous: presents a high risk that continuing updates may cause significant additional damage
* I Index: if the block is an index block, continuing updates will be very dangerous: INTEG reports such errors with a nature of DANGER; if the block is a data block, reported by INTEG with a nature of Data, continuing updates can only cause limited additional damage, but some data may be lost, become inaccessible, or cause processes to inappropriately loop
* S Spanning: prevents access to a block spanning node value; INTEG reports these with a nature of Data
* T Transient: usually cleared by an update to the database or possibly a straightforward operator action

INTEG reports these codes for many of the errors and in so doing transforms Index errors on index block to DANGER and otherwise to Data, meaning the issue is confined to a level 0 data block, and so very localized. However, when an index block has damage, YottaDB cannot correctly navigate the tree and if operations continue, subsequent updates can go where they do not belong, causing increasing damage. When a data block has damage, the worst thing that can happen from a YottaDB standpoint is that you get an indefinite loop. More commonly, some confined set of nodes becomes inaccessible, which may or may not be important from an application perspective, most commonly the application gets an error when it tries to use the data in question. It is possible for a single issue to cause multiple reports, and, in such a case, focus first on the most serious report.

Repair Dangerous and Access errors immediately. You may assess the benefits of deferring correction of less severe errors until normally scheduled down-time.

+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| Severity   | Mnemonic           | Error Message                                                                      | Section                                 |
+============+====================+====================================================================================+=========================================+
| B          | BSIZTOOLARGE       | ffff Block larger than specified maximum size.                                     | :ref:`o6-block-size-errors`             |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| T          | BUFFLUFAILED       | Error flushing buffers from rrrr for database file ffff.                           | :ref:`i7-database-rundown-problem`      |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| B          | DBBADFREEBLKCTR    | Free blocks counter in file header: nnnn appears incorrect, should be mmmm.        | :ref:`i3-file-header-errors`            |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| I          | DBBADKYNM          | Bad key name.                                                                      | :ref:`k1-bad-key`                       |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| I          | DBBADNSUB          | Bad numeric subscript.                                                             | :ref:`k1-bad-key`                       |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| D          | DBBADPNTR          | Bad pointer value in directory.                                                    | :ref:`k4-pointer-problems`              |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| I          | DBBDBALLOC         | Block doubly allocated.                                                            | :ref:`k3-blocks-doubly-allocated`       |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| D          | DBBFSTAT           | Block busy/free status unknown (local bitmap corrupted).                           | :ref:`m1-bitmap-errors`                 |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| D          | DBBNPNTR           | Bit map block number as pointer.                                                   | :ref:`k4-pointer-problems`              |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| D          | DBBPLMGT2K         | Blocks per local map is greater than 2K.                                           | :ref:`i3-file-header-errors`            |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| D          | DBBPLMLT512        | Blocks per local map is less than 512.                                             | :ref:`i3-file-header-errors`            |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| D          | DBBPLNOT512        | Blocks per local map is not a multiple of 512.                                     | :ref:`i3-file-header-errors`            |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| I          | DBBSIZMN           | Block too small.                                                                   | :ref:`o1-bad-block`                     |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| I          | DBBSIZMX           | Block larger than file block size.                                                 | :ref:`o1-bad-block`                     |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| A          | DBBSIZZRO          | Block size equals zero.                                                            | :ref:`i3-file-header-errors`            |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| T          | DBBTUWRNG          | Blocks-to-upgrade file-header field is incorrect. Expected nnnn, found mmmm.       | H2                                      |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| I          | DBCMPBAD           | Compression count not maximal.                                                     | :ref:`k6-compression-count-error`       |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| I          | DBCMPNZRO          | First record of block has nonzero compression count.                               | :ref:`o1-bad-block`                     |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| I          | DBCOMPTOOLRG       | Record has too large compression count.                                            | :ref:`o2-record-errors`                 |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| A          | DBCREINCOMP        | Header indicates file creation was interrupted before completion.                  | :ref:`i3-file-header-errors`            |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| I          | DBDATAMX           | Record too large.                                                                  | :ref:`o2-record-errors`                 |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| T          | DBFGTBC            | File size larger than block count would indicate.                                  | :ref:`i4-file-size-errors`              |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| A          | DBFLCORRP          | Header indicates file is corrupt.                                                  | :ref:`i8-repair-induced-problems`       |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| D          | DBFSTBC            | File size smaller than block count would indicate.                                 | :ref:`i4-file-size-errors`              |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| A          | DBFSTHEAD          | File smaller than database header.                                                 | :ref:`i3-file-header-errors`            |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| I          | DBGTDBMAX          | Key larger than database maximum.                                                  | :ref:`k7-key-warning`                   |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| A          | DBHEADINV          | Header size not valid for database.                                                | :ref:`i3-file-header-errors`            |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| D          | DBINCLVL           | Block at incorrect level.                                                          | :ref:`o1-bad-block`                     |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| A          | DBINCRVER          | Incorrect version of YottaDB database.                                             | :ref:`i2-yottadb-version-mismatch`      |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| I          | DBINVGBL           | Invalid mixing of global names.                                                    | :ref:`k3-blocks-doubly-allocated`       |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| I          | DBKEYGTIND         | Key greater than index key.                                                        | :ref:`k2-keys-misplaced`                |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| I          | DBKEYMN            | Key too short.                                                                     | :ref:`k1-bad-key`                       |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| I          | DBKEYMX            | Key too long.                                                                      | :ref:`k1-bad-key`                       |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| I          | DBKEYORD           | Keys out of order.                                                                 | :ref:`k2-keys-misplaced`                |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| I          | DBKGTALLW          | Key larger than maximum allowed length.                                            | :ref:`k1-bad-key`                       |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| B          | DBLOCMBINC         | Local bitmap incorrect.                                                            | :ref:`m2-bitmap-header-problems`        |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| D          | DBLRCINVSZ         | Last record of block has invalid size.                                             | :ref:`k5-star-key-problems`             |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| I          | DBLTSIBL           | Keys less than sibling's index key.                                                | :ref:`k2-keys-misplaced`                |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| B          | DBLVLINC           | Local bitmap block level incorrect.                                                | :ref:`m1-bitmap-errors`                 |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| I          | DBMAXNRSUBS        | Maximum number of subscripts exceeded.                                             | :ref:`k1-bad-key`                       |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| B          | DBMBMINCFRE        | Master bitmap incorrectly asserts this local map has free space.                   | :ref:`m1-bitmap-errors`                 |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| B          | DBMBPFLDIS         | Master bitmap shows this map full, in disagreement with both disk and INTEG result.| :ref:`m1-bitmap-errors`                 |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| B          | DBMBPFLDLBM        | Master bitmap shows this map full, agreeing with disk local map.                   | :ref:`m1-bitmap-errors`                 |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| B          | DBMBPFLINT         | Master bitmap shows this map full, agreeing with MUPIP INTEG.                      | :ref:`m1-bitmap-errors`                 |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| B          | DBMBPFRDLBM        | Master bitmap shows this map has space, agreeing with disk local map.              | :ref:`m1-bitmap-errors`                 |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| B          | DBMBPFRINT         | Master bitmap shows this map has space, agreeing with MUPIP INTEG.                 | :ref:`m1-bitmap-errors`                 |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| B          | DBMBPINCFL         | Master bitmap incorrectly marks this local map full.                               | :ref:`m1-bitmap-errors`                 |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| B          | DBMBSIXMN          | Map block too small.                                                               | :ref:`m2-bitmap-header-problems`        |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| B          | DBMBSIZMX          | Map block too large.                                                               | :ref:`m2-bitmap-header-problems`        |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| T          | DBMBTNSIZMX        | Map block transaction number too large.                                            | :ref:`i6-transient-errors`              |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| B          | DBMRKBUSY          | Block incorrectly marked busy.                                                     | :ref:`m1-bitmap-errors`                 |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| D          | DBMRKFREE          | Block incorrectly marked free.                                                     | :ref:`m1-bitmap-errors`                 |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| B          | DBNONUMSUBS        | Key contains a numeric form of subscript in a global defined to collate all        | :ref:`k1-bad-key`                       |
|            |                    | subscripts as strings.                                                             |                                         |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| A          | DBNOREGION         | None of the database regions accessible.                                           | :ref:`i6-transient-errors`              |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| A          | DBNOTGDS           | Unrecognized file format.                                                          | :ref:`i3-file-header-errors`            |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| A          | DBNOTMLTP          | Block size not a multiple of 512 bytes.                                            | :ref:`k1-bad-key`                       |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| I          | DBNULCOL           | NULL collation representation differs from the database file header setting.       | :ref:`k1-bad-key`                       |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| D          | DBPTRMX            | Block pointer larger than file maximum.                                            | :ref:`k4-pointer-problems`              |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| D          | DBPTRNOTPOS        | Block pointer negative.                                                            | :ref:`k4-pointer-problems`              |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| D          | DBRBNLBMN          | Root block number is a local bitmap number.                                        | :ref:`k4-pointer-problems`              |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| D          | DBRBNNEG           | Root block number negative.                                                        | :ref:`k4-pointer-problems`              |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| D          | DBRBNTOOLRG        | Root block number greater than last block number in file.                          | :ref:`k4-pointer-problems`              |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| T          | DBRDONLY           | Database file ffff read only.                                                      | :ref:`i6-transient-errors`              |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| D          | DBREADBM           | Read error on bitmap.                                                              | :ref:`h7-disk-hardware-problems`        |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| D          | DBRLEVLTONE        | Root level less than one.                                                          | :ref:`o1-bad-block`                     |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| D          | DBRLEVTOOHI        | Root level higher than maximum.                                                    | :ref:`o1-bad-block`                     |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| I          | DBRSIZMN           | Physical record too small.                                                         | :ref:`o2-record-errors`                 |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| I          | DBRSIZMX           | Physical record too large.                                                         | :ref:`o2-record-errors`                 |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| S          | DBSPANCHUNKORD     | Chunk of nnnn blocks is out of order.                                              | :ref:`o5-salvage-damaged-spanning-node` |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| S          | DBSPANGLOINCMP     | Spanning node is missing. Block no nnnn of of spanning node is missing.            | :ref:`o5-salvage-damaged-spanning-node` |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| D          | DBSTARCMP          | Last record of block has nonzero compression count.                                | :ref:`k5-star-key-problems`             |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| A          | DBSVBNMIN          | Start VBN smaller than possible.                                                   | :ref:`i3-file-header-errors`            |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| A          | DBSZGT64K          | Block size is greater than 64K.                                                    | :ref:`i4-file-size-errors`              |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| T          | DBTNLTCTN          | Current tn and early tn are not equal.                                             | :ref:`i6-transient-errors`              |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| T          | DBTNNEQ            | Cannot reset transaction number for this region.                                   | :ref:`i4-file-size-errors`              |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| T          | DBTNRESET          | Transaction numbers greater than the current transaction were found.               | :ref:`i6-transient-errors`              |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| A          | DBTTLBLK0          | Total blocks equals zero.                                                          | :ref:`i4-file-size-errors`              |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| T          | DBTNTOOLG          | Block transaction number too large.                                                | :ref:`i6-transient-errors`              |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| A          | DBUNDACCMT         | Cannot determine access method;trying with BG.                                     | :ref:`i6-transient-errors`              |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| A          | FREEZE             | Database for region rrr is already frozen, not INTEGing.                           | :ref:`i6-transient-errors`              |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| A          | MUSTANDALONE       | Could not get exclusive access to rrrr.                                            | :ref:`i6-transient-errors`              |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| B          | NOGTCMDB           | INTEG does not support operation on YottaDB database region.                       | :ref:`i5-more-database-access-problems` |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| I          | NULSUBSC           | Null subscripts are not allowed for file: rrrr.                                    | :ref:`k1-bad-key`                       |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| A          | REGFILENOTFOUND    | File ffff corresponding to region rrrr cannot be found.                            | :ref:`i6-transient-errors`              |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| B          | -                  | Cannot INTEG region across network.                                                | :ref:`i5-more-database-access-problems` |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+
| T          | -                  | Database requires flushing, which cant be performed wihtout write access.          | :ref:`i7-database-rundown-problem`      |
+------------+--------------------+------------------------------------------------------------------------------------+-----------------------------------------+

.. _i2-yottadb-version-mismatch:

+++++++++++++++++++++++++++++++++++
I2 - YottaDB Version Mismatch
+++++++++++++++++++++++++++++++++++

YottaDB databases and Global Directories may change with new releases of the product.

*IF YOU GET AN ERROR INDICATING A VERSION MISMATCH*, first identify the YottaDB version using the M command WRITE $ZVERSION from Direct Mode.

Then refer to the installation procedures for your new release. If you are running more than one release of YottaDB investigate the environment variables that define the environments, and take appropriate action.

.. _i3-file-header-errors:

++++++++++++++++++++++++++++++++++++++
I3 - File Header Errors
++++++++++++++++++++++++++++++++++++++

These errors indicate damage to the control or reference information in the file header.

"Start VBN smaller than possible" indicates that INTEG cannot locate the database structure. "Header indicates that file creation did not complete" indicates a MUPIP CREATE problem. In these cases, the database has effectively been lost. DSE cannot correct these problems. If you determine that the costs of recovering from a backup, hopefully with journal files, are prohibitive, consider consulting with YottaDB.

To correct the other errors of this type use the DSE CHANGE FILEHEADER command with the BLK_SIZE=, BLOCKS_FREE=, and TOTAL_BLKS qualifiers.

"Free blocks counter ..." indicates that the count of free blocks in the file header is not correct. This error only affects $VIEW("FREECNT",region), MUPIP DUMPFHEAD and DSE DUMP FILEHEADER which return the information.

.. _i4-file-size-errors:

+++++++++++++++++++++++++++++++++++++++
I4 - File Size Errors
+++++++++++++++++++++++++++++++++++++++

File size errors can misdirect MUPIP, but do not cause the YottaDB run-time system to generate further errors. Auto-extend is the exception and may not function properly if there are file size errors. One possible symptom of an auto-extend problem would be incorrectly marked busy errors from a partial bitmap at the "old" end of the database which had previously been incorrectly initialized.

These errors indicate that the total block count does not agree with the file size. Get the starting VBN and the block size for the file by using DSE DUMP FILEHEADER. Then calculate the correct value of the total blocks with the following formula:

.. code-block:: none

        (file_size_in_bytes-(starting_VBN-1)*512)/DB_block_size_in_bytes-1 ; where 512 is bytes/block

A decimal number results from this formula. Convert this decimal to a hexadecimal number, then change the total block count to this hexadecimal value using DSE CHANGE FILEHEADER TOTAL_BLKS= . You may also need to adjust the free blocks count with BLOCKS_FREE=. MUPIP INTEG informs you if this is necessary and gives the correct values.

.. _i5-more-database-access-problems:

+++++++++++++++++++++++++++++++++++++++
I5 - More Database Access Problems
+++++++++++++++++++++++++++++++++++++++

These error messages reflect failures to find, open, or access a database file. Examine any secondary error messages to obtain additional information about the problem.

Use printenv to check ydb_gbldir or use the M command WRITE $ZGBLDIR to verify that the "pointer" identifies the proper Global Directory. If the pointer is not appropriate, reset ydb_gbldir or use the M command SET $ZGBLDIR= to name the proper file.

Examine the Global Directory using GDE. If the Global Directory is not appropriate, correct or recreate it with GDE. For more information on the use of GDE, refer to the `"Global Directory Editor (GDE)" <gde.html>`_ chapter.

*IF THE GLOBAL DIRECTORY IS DAMAGED BUT ACCESSIBLE WITH GDE*, investigate who may have used GDE to perform the modifications. If the Global Directory is damaged and not accessible with GDE, investigate what program, other than YottaDB and its utilities, might have written to the file. Except for GDE, all YottaDB components treat the Global Directory as static and read-only.

*IF THE GLOBAL DIRECTORY APPEARS CORRECT*, use printenv to verify that any environment variables that it uses are properly defined for the process experiencing the problem. If the process has an environment to which you do not have access, you may have to carefully read the shell scripts used to establish that environment.

*IF THE ENVIRONMENT VARIABLES APPEAR CORRECT*, use the ls -l to examine the file protection. Remember to examine not only the file, but also all directories accessed in locating the file.

*IF THE FILES APPEAR TO BE PROPERLY MAPPED* by the Global Directory, correctly placed given all logical names, and correctly protected to permit appropriate access, use one of the DCL commands TYPE or DUMP to verify access to the files, independent of YottaDB.

*IF THE FILES APPEAR TO BE PROPERLY MAPPED* by the Global Directory, properly placed given all environment variables, and properly protected to permit appropriate access, use the od or cat utility to verify access to the files, independent of YottaDB.

*IF YOU SUSPECT A VERSION MISMATCH PROBLEM*, refer to :ref:`i2-yottadb-version-mismatch`.

*IF YOU SUSPECT A DISK HARDWARE PROBLEM*, refer to :ref:`h7-disk-hardware-problems`.

.. _i6-transient-errors:

++++++++++++++++++++++++++++++++++++++
I6 - Transient Errors
++++++++++++++++++++++++++++++++++++++

YottaDB corrects certain errors automatically. If you find that any of these errors persist, contact your YottaDB support channel.

"Block transaction number too large" indicates that the file header has a smaller transaction number than the database block.

If you are not running TP or incremental backup this is a benign error (from the database's point of view; application data consistency should be verified). YottaDB automatically self-corrects these errors as soon as it performs sufficient updates to get the current transaction number of the database higher than any block's transaction number. If this error persists, perform the following steps:

* Run the MUPIP INTEG command on your database and look for the following output:

  "Largest transaction number found in database was HHHHHHH"

* Run the following command:

  dse change -fileheader -current_tn=<HHHHHHH+1>

  Where <HHHHHHH+1> is the largest transaction number + 1. This command sets the current transaction number to one more than the largest transaction number found in the database. Note that HHHHHHH is in hexadecimal form.

"Current tn and early tn are not equal" indicates that the critical section has been damaged. "Reference count is not zero" indicates an improper file close. The first access that references a questionable database should correct these errors. Generally, these errors indicate that the file was not closed normally. This problem is typically caused by an unscheduled shutdown of the system. Review your institution's startup and shutdown procedures to ensure a controlled shutdown.

"Cannot determine access method..." indicates that the fileheader has been damaged. When INTEG detects this error, it forces the access method to BG and continues. If there is no other damage to the file header, no other action may be required.

However, if the access method should be MM, use MUPIP SET ACCESS_METHOD=MM to correct the database.

.. _i7-database-rundown-problem:

+++++++++++++++++++++++++++++++
I7 - Database Rundown Problem
+++++++++++++++++++++++++++++++

However, in the case where the database file was improperly closed, it must be put in an appropriate state with a MUPIP JOURNAL ROLLBACK/RECOVER or MUPIP RUNDOWN prior to a MUPIP INTEG.

.. _i8-repair-induced-problems:

++++++++++++++++++++++++++++++++
I8 - Repair Induced Problems
++++++++++++++++++++++++++++++++

These error messages are created by operator actions performed with DSE.

The DSE commands CRITICAL INITIALIZE RESET, ALL RESET, and ALL RENEW induce CRITRESET errors in all processes attempting to access the target database(s).

Any process attempting to access a database that has its "corrupt" flag set to TRUE receives a DBCRPT error.

.. note::
   Using the DSE command CHANGE FILEHEADER CORRUPT=TRUE is very dangerous. If the DSE session EXITs before issuing a CHANGE FILEHEADER CORRUPT=FALSE, the database becomes entirely useless.

.. _k1-bad-key:

++++++++++++++++++++++
K1 - Bad Key
++++++++++++++++++++++

This section describes appropriate actions when the error message indicates a damaged key. GDS transforms subscripted or unsubscripted global variable names into keys, which are part of the database record used to index the corresponding global variable data values. The keys are stored in a compressed form which omits that part of the prefix held in common with the previous key in the block. The compression count is the number of common characters. Except in the Directory Tree, all records after the first one have a non-zero count. The first record in a block always has a compression count of zero (0).

*IF THE BLOCK IS A DATA BLOCK*, that is, level zero (0), refer to :ref:`o3-data-block-errors`.

*IF THE BLOCK HAS A LEVEL GREATER THAN ZERO (0)*, examine the record with the DSE command DUMP BLOCK= OFFSET where the block and offset values are provided by the INTEG error report. If the record appears to have a valid block pointer, note the pointer. Otherwise, refer to :ref:`o2-record-errors`.

After noting the pointer, SPAWN and use MUPIP INTEG BLOCK=pointer (if you have time constraints, you may use the FAST qualifier) to check the structure.

*IF THE SUB-TREE IS INVALID*, according to the MUPIP INTEG, DSE REMOVE the record containing the reported bad key, INTEG, and refer to :ref:`o4-salvage-of-data-blocks-with-lost-indices`.

Otherwise use the DSE command DUMP BLOCK= RECORD=9999 to find the last record in the block and examine it using the DUMP RECORD= command. Continue using DSE to follow the pointer(s) down to level 0, always choosing the right-hand branch. Note the largest key at the data level. REMOVE the record containing the reported bad key. Determine the proper placement for the noted key using FIND KEY= and ADD KEY= POINTER where the key and the pointer are those noted in the preceding actions.

.. _k2-keys-misplaced:

++++++++++++++++++++
K2 - Keys Misplaced
++++++++++++++++++++

When the error is a misplaced key, the keys are not in proper collating sequence.

*IF THE BLOCK IS A DATA BLOCK*, that is, level zero (0), DUMP it GLO, REMOVE the records that point to it, MAP it FREE, and MUPIP LOAD the output of the DUMP GLO.

*IF THE BLOCK HAS A LEVEL GREATER THAN ZERO (0)*, you may choose to reposition the record in its proper place or use the salvage strategy discussed in :ref:`o4-salvage-of-data-blocks-with-lost-indices`. In general, the salvage strategy is less demanding and less dangerous. However, it may be time consuming if the index block holding the record has a level much greater than one (1). If you decide against the salvage strategy, note the contents of the damaged record. In either case, REMOVE the record. If using salvage, refer to :ref:`o4-salvage-of-data-blocks-with-lost-indices`. If not, determine the proper location for the record using FIND KEY= to display the closest existing path, then follow the procedure outlined in the last paragraph of :ref:`k1-bad-key`.

.. _k3-blocks-doubly-allocated:

+++++++++++++++++++++++++++++
K3 - Blocks Doubly Allocated
+++++++++++++++++++++++++++++

A doubly allocated block is dangerous because it causes data to be inappropriately mingled. As long as no KILLs occur, double allocation might not cause permanent loss of additional data. However, it may cause the application programs to generate errors and/or inappropriate results. When a block is doubly allocated, a KILL may remove data outside its proper scope.

A doubly allocated index block may also cause increasing numbers of blocks to become corrupted. Use the following process to correct the problem.

First, identify all pointers to the block, using FIND EXHAUSTIVE and/or information reported by MUPIP INTEG. If the error report identifies the block as containing inappropriate keys or a bad level, INTEG has identified all paths that include the block. In that case, INTEG reports all paths after the first with the doubly allocated error, and the first path with some other, for example, "Keys out of order" error.

*IF THE INTEG REPORT DOES NOT MENTION THE BLOCK PRIOR TO THE DOUBLY ALLOCATED ERROR*, use FIND EXHAUSTIVE to identify all pointers to that block.

*IF THE BLOCK IS A DATA BLOCK*, that is, level zero (0), DUMP it GLO, REMOVE the records that point to it, MAP it FREE, and MUPIP LOAD the output of the DUMP GLO.

*IF THE BLOCK HAS A LEVEL GREATER THAN ZERO (0)*, you may sort through the block and its descendants to disentangle intermixed data. If the block has a level of more than one (1), this may be worth a try. The salvage strategy (discussed in :ref:`o4-salvage-of-data-blocks-with-lost-indices`) may be time consuming and there may be only one misplaced node. However, in general, the salvage strategy is less demanding and less dangerous.

*IF YOU CHOOSE THE SALVAGE STRATEGY*, REMOVE the records that point to the block, MAP it FREE, and refer to :ref:`o4-salvage-of-data-blocks-with-lost-indices`.

*IF YOU DECIDE TO WORK WITH THE BLOCK*, choose the path to retain, REMOVE the other pointer record, and relocate any misplaced descendants with DSE ADD and REMOVE.

.. _k4-pointer-problems:

++++++++++++++++++++++++++++++++++
K4 - Pointer Problems
++++++++++++++++++++++++++++++++++

Each index block is made up of records that contain keys and corresponding pointers. In the case where database damage is a symptom of an incorrect key paired with a valid pointer, the repair strategy, which may be implemented with a number of tactics, is to use the pointer to locate the data and reconstruct the key.

While they occur very infrequently, invalid pointers do not permit the same strategy. If there is an invalid pointer, always eliminate the record containing the bad pointer using the DSE REMOVE command. Since no data can be stored under an invalid pointer, either the pointer error was discovered on the first attempt to use it and no data has been lost, or the pointer was damaged during use. If the pointer was damaged during use, the lost data should be located by examining "Block incorrectly marked busy" errors and generally be recovered as described in :ref:`o4-salvage-of-data-blocks-with-lost-indices`.

*IF MUCH DATA IS LOST*, it may be worthwhile attempting to reconstruct the bad record as follows. Before removing the record containing the bad pointer, use the DUMP command to note the key in the record. Using the error reports and/or the DSE RANGE command, locate the block to which the key should point. Then use DSE ADD to replace the previously deleted record with a new record that has the correct key and pointer in place.

.. _k5-star-key-problems:

++++++++++++++++++++++++++++++++
K5 - Star Key Problems
++++++++++++++++++++++++++++++++

The last record in every index block must be a star-key record that points to a block that continues the path to all data not covered by the preceding records in the block. Star-key records have a unique format with a size of twelve (12) with the V7 block format, and a compression count of zero (0). Star keys have a size of eight (8) with the V6 block format. The errors discussed in this section indicate a missing or damaged star-key and may be attacked with two strategies.

In general, you should turn the last existing record into a star-key. This works well as long as the block holds at least one valid record. If you choose this strategy, locate the last record using DUMP RECORD=9999. Then DUMP the last record and note its pointer. Next, REMOVE the last record. Finally, ADD STAR POINTER= to the key you noted.

If the star-key is the only record in a root block, you should add a new empty level 0 descendent. If you choose this strategy, add a new star-key using FIND FREEBLOCK HINT=this-block to locate a nearby block. Next, MAP the new block BUSY and CHANGE LEVEL= 0 and BSIZ=8 if V6 format and 12 if V7 format. If the new block has a level of zero (0), return to the damaged block and ADD STAR POINTER=the-first-new-block.

.. _k6-compression-count-error:

++++++++++++++++++++++++++++++++
K6 - Compression Count Error
++++++++++++++++++++++++++++++++

"Compression count not maximal" indicates that the compression count that is used to save space in key storage is not correct.

*IF THE BLOCK IS A DATA BLOCK*, that is, level zero (0), DUMP it GLO, REMOVE the records that point to it, MAP it FREE, and MUPIP LOAD the output of the DUMP GLO.

*IF THE BLOCK HAS A LEVEL GREATER THAN ZERO (0)*, REMOVE the record and ADD it back in the same location with the same KEY=, and POINTER= or STAR.

You may also adjust the compression count using CHANGE CMPC=. Because this changes the value of all subsequent keys in the block (except the star-key), you should try this alternative only if those keys also appear incorrect.

.. _k7-key-warning:

++++++++++++++++++++++++++
K7 - Key Warning
++++++++++++++++++++++++++

"Key too large for database maximum" indicates that the database holds a key that is legal to YottaDB but exceeds the KEY_MAX_SIZE for the database.

Use the DSE command CHANGE FILEHEADER KEY_MAX_SIZE= to adjust the file limitation. Alternatively, you may remove the record, using the M command KILL on an ancestor node. If any user attempts to modify or replace the record in the database while the key is over-length, YottaDB will reject the SET with an error.

.. _m1-bitmap-errors:

+++++++++++++++++++++++++++
M1 - Bitmap Errors
+++++++++++++++++++++++++++

Every block in the file has a corresponding bit in a bitmap. All blocks with valid data are marked busy in their maps; all blocks that are unused or no longer hold data are marked free. GDS uses bitmaps to locate free blocks efficiently. The errors discussed in this section indicate problems with bitmaps.

"Block incorrectly marked free" is the only potentially dangerous bitmap error. This error means that the block is within the B-tree structure, but that the bitmap shows it available for use (i.e., it is a "Block doubly allocated" waiting to happen). Immediately use DSE to MAP such blocks BUSY.

Bitmap information is redundant (i.e., bitmaps can be recreated by scanning the B-tree); however, the majority of bitmap errors reflect secondary errors emanating from flaws in the B-tree, which are often reported as key or data errors by MUPIP INTEG.

When INTEG encounters an error, it stops processing that leaf of the tree. When it subsequently compares its generated bitmaps to those in the database, it reports the blocks belonging in the tree that it could not find as "Block incorrectly marked busy." This error type can be viewed as a flag, marking the location of a block of lost data whose index is disrupted.

INTEG reports each block that it concludes is incorrectly marked, and also the local map that holds the "bad" bits. Furthermore, if the local map "errors" affect whether the local map should be marked full or not full in the master map, INTEG also reports the (potential) problem with the master map. Therefore, a single error in a level one (1) index block will generate, in addition to itself, one or more "Block incorrectly marked busy", one or more "Local bitmap incorrect", and possibly one or more "Master bitmap shows...". Errors in higher level index blocks can induce very large numbers of bitmap error reports.

Because bitmap errors are typically secondary to other errors, correcting the primary errors usually also cures the bitmap errors. For this reason and, more importantly, because bitmap errors tend to locate "lost" data, they should always be corrected at, or close to, the end of a repair session.

The DSE command MAP provides a way to switch bits in local maps with FREE and BUSY, propagate the status of a local map to the master map with MASTER, and completely rebuild all maps from the B-tree with RESTORE. Before beginning any MAP MASTER operation, first ensure that the database has no active updaters and that there are no non-bitmap errors to resolve.

.. _m2-bitmap-header-problems:

++++++++++++++++++++++++++++
M2 - Bitmap Header Problems
++++++++++++++++++++++++++++

Bitmaps are stored in blocks that have a unique header format with a level of minus one (-1) and a block size of 87 or 88 depending on the Euclidian ordering of the platform. The errors discussed in this section indicate a bitmap block header that violates that format.

Use the DSE command CHANGE with the BSIZ=87 or 88 (depending on platform) and LEVEL=-1FF qualifiers to correct the problem. If the block size is too small, the bitmap will have to be reconstructed using MAP RESTORE or manually from INTEG error reports using MAP FREE. If there are other errors, defer any MAP RESTORE until after they have been repaired.

.. _o1-bad-block:

+++++++++++++++++++++++++
O1 - Bad Block
+++++++++++++++++++++++++

GDS organizes the B-tree into logical blocks, each of which YottaDB handles discretely. A block consists of a block header and a lexically increasing sequence of records. Blocks starting with the root block up to the data blocks are index blocks. The last block in any complete path is a data block. The errors discussed in this section indicate a damaged block.

Determine if the block has other problems by using the DSE command INTEGRIT. Examine the contents of the block using the DSE command DUMP. You may also examine the block preceding this block in the path and/or blocks pointed to by records in this block. If you can determine an appropriate action, use CHANGE with the BSIZ= and/or LEVEL= qualifiers. If you cannot quickly repair the block, examine its level with DUMP HEADER. If the block is a data block, that is, level zero (0), refer to :ref:`o3-data-block-errors`. If the block has a level greater than zero (0), REMOVE the record that points to the block and refer to :ref:`o4-salvage-of-data-blocks-with-lost-indices`.

.. _o2-record-errors:

+++++++++++++++++++++++
O2 - Record Errors
+++++++++++++++++++++++

GDS organizes keys with pointers or data to form records. A record has a header, which holds the record size, and a compression count, which identifies how much of the preceding key is held in common by this record. Records in the block are ordered by the values of their keys. The errors discussed in this section indicate damage to a record. Record errors present an added challenge, in that they potentially prevent YottaDB from correctly interpreting subsequent records in the same block.

*IF THE BLOCK IS A DATA BLOCK*, that is, level zero (0), refer to :ref:`o3-data-block-errors`.

*IF THE BLOCK IS AN INDEX BLOCK*, that is, has a level greater than zero (0), the best option is generally to use the salvage strategy discussed in section O4. REMOVE the damaged record and INTEG the block. If the block is still corrupt, repeat the last step, REMOVE the pointer to it, and MAP it FREE. In any case, refer to :ref:`o4-salvage-of-data-blocks-with-lost-indices`.

.. _o3-data-block-errors:

+++++++++++++++++++++++
O3 - Data Block Errors
+++++++++++++++++++++++

The errors described in this section include damage to the header, the records, or the keys.

*IF THE BLOCK IS LEVEL ZERO (0)*, use DSE DUMP to examine the contents of the block. Note any information that might allow you to correct the problem or might help to identify and recreate the endangered data. If you are familiar with GDS and hexadecimal representations, you may be able to recognize data that DSE cannot recognize because of misalignment.

*IF THE BEGINNING OF THE BLOCK IS VALID*, DUMP GLO may be able to capture its contents up to the point where it is damaged. In the worst case, REMOVE the record that points to the block, MAP it FREE, and lose its entire contents. The extent and importance of the damage depends on the size of the block and what it should be holding. In a similar but not quite as drastic case, REMOVE the record with the problem and lose the contents of that record. Use application and business process knowledge to research and if appropriate reconstruct lost data; remember, if recently processed, it may exist in journal files.

.. _o4-salvage-of-data-blocks-with-lost-indices:

++++++++++++++++++++++++++++++++++++++++++++++
O4 - Salvage of Data Blocks with Lost Indices
++++++++++++++++++++++++++++++++++++++++++++++

This strategy uses bitmap errors to locate data blocks containing information that belongs in the B-tree, but are no longer indexed because of errors and/or repairs to defective indices.

The algorithm is based on the fact that most bitmap errors are secondary to index errors. Therefore, it is optimistic about bitmaps and pessimistic about indices, and tends to error on the side of restoring more rather than less data to the B-tree. After using this technique, you should always check to see if obsolete, deleted data was restored. If data was restored, and GDS integrity has been restored, you can safely KILL the "extra" data.

*IF THE INDICES HAVE BEEN DAMAGED FOR SOME TIME AND THE DAMAGE CAUSED DUPLICATE KEYS TO BE CREATED*, this strategy raises the issue of which value is the "correct" value. Because most applications either form new nodes or update existing nodes rather than simply overlaying them, this issue seldom arises. Usually the application will fail in an attempt to update any "misplaced" node. If the problem does arise, the issue may not be determining the "correct" value, but the best available value.

*IF YOU HAVE A DUPLICATE NODE PROBLEM*, you can load the sequential file produced in DSE with an M program that detects and reports duplicate nodes. You can also use the block transaction numbers as clues to the order in which blocks were updated. However, remember that you generally cannot know which record was modified on the last update, and that DSE repair actions modify the block transaction number.

If the duplicate node problem poses a significant problem, you should probably not use DSE to repair the database, but instead, use journals to recover or restore from backups.

This strategy works well when the missing indices are level one (1). However, the time required increases dramatically as the level of the missing index increases. If you have a problem with a level four (4) or level five (5) index, and you have developed skill with DSE, you may wish to try the more technically demanding approach of repairing the indices.

Once you have corrected all errors except bitmap errors, SPAWN and use MUPIP INTEG FAST REGION NOMAP to get a list of all remaining bitmap errors. If the report includes any "Blocks incorrectly marked free", MAP them BUSY. Then use DUMP HEADER BLOCK= to examine each "Block incorrectly marked busy." If the level is one (1), DUMP the block ZWR. In any case, MAP it FREE. Once all the blocks have been collected in a sequential file in this fashion, use MUPIP LOAD to reclaim the data from the sequential file.

~~~~~~~~~~~~~~~~~~
Download salvage.m
~~~~~~~~~~~~~~~~~~

salvage.m is a utility that removes all incorrectly-marked-busy blocks from the specified region. During execution it displays the DSE commands that it will execute and aborts execution when it encounters an error. It dumps the zwrite formatted content of blocks incorrectly-marked-busy to a file called <region>_db.zwr. Upon completion, it sets the abandoned_kills and kill_in_prog flags in the database fileheader to false.

You can download salvage.m from `GitLab <https://gitlab.com/YottaDB/DB/YDBDoc/blob/master/AdminOpsGuide/salvage.m>`_.

Steps to run the salvage utility are as follows:

* Perform an argumentless MUPIP RUNDOWN before running this utility.

* Ensure that there are no INTEG errors other than the incorrectly-marked-busy block errors.

* Run $ydb_dist/yottadb -r ^salvage.

* Specify the region name. If no region is specified, the utility assumes DEFAULT.

If the utility reports a DSE error, fix that error and run the salvage utility again.

After completing repairs with the salvage utility, open the <REGION>_db.zwr file and examine its contents. If there is a need to recover the data from the incorrectly marked busy blocks, perform a MUPIP LOAD <REGION>_db.zwr to load that data back to the database.

.. _o5-salvage-damaged-spanning-node:

+++++++++++++++++++++++++++++++++++++++++++++++++++++++
O5 - Salvage of a Damaged Spanning Node
+++++++++++++++++++++++++++++++++++++++++++++++++++++++

The following example shows how to salvage a damaged spanning node in ^mypoem.

Run MUPIP INTEG to find the location of the damaged spanning node. A MUPIP INTEG report of a region that has damaged spanning nodes might look something like the following:

.. code-block:: bash

   Integ of region DEFAULT
   Block:Offset Level
   %YDB-E-DBSPANGLOINCMP,
          7:10     0  Spanning node is missing. Block no 3 of spanning node is missing
                      Directory Path:  1:10, 2:10
                      Path:  4:31, 7:10
                      Spanning Node ^mypoem(#SPAN1) is suspect.
    %YDB-E-DBKEYGTIND,
          7:10     0  Key greater than index key
                      Directory Path:  1:10, 2:10
                      Path:  4:31, 7:10
                      Keys from ^mypoem(#SPAN48) to ^mypoem(#SPAN3*) are suspect.
    %YDB-E-DBSPANCHUNKORD,
          3:10     0  Chunk of 1 blocks is out of order
                      Directory Path:  1:10, 2:10
                      Path:  4:3D, 3:10
    Spanning Node Chunk ^mypoem(#SPAN4) is suspect.
    Total error count from integ:        3
    Type           Blocks         Records          % Used      Adjacent
    Directory           2               2           5.468            NA
    Index               1               4          13.476             1
    Data                4               5          76.562             4
    Free               93              NA              NA            NA
    Total             100              11              NA             5
    [Spanning Nodes:2 ; Blocks:3]
    %YDB-E-INTEGERRS, Database integrity errors

Notice the lines that contain: "Block no 3 of spanning node is missing", "Key greater than index key", and ^mypoem(#SPAN48) and there is an extra chunk that is not connected to ^mypoem(#SPAN4).

Confirm whether you have determined the spanning range of the node:

* Is ^mypoem(#SPAN48) the last node (block number 3)?
* Is ^mypoem(#SPAN4) the last node?

Clearly, YottaDB did not find block 3 and ^mypoem(#SPAN4) terminated the spanning node, so ^mypoem(#SPAN4) might be the last node. So, the parts of the spanning node that contain the value are ^mypoem(#SPAN2) through ^mypoem(#SPAN4).

Use DSE to find the spanned nodes:

.. code-block:: bash

   DSE> find -key=^mypoem(#SPAN2)
   Key found in block 6.
       Directory path
       Path--blk:off
       1:10,    2:10,
       Global tree path
       Path--blk:off
       4:25,    6:10,
   DSE> find -key=^mypoem(#SPAN3)
   Key not found, would be in block  7.
       Directory path
       Path--blk:off
       1:10,    2:10,
       Global tree path
       Path--blk:off
       4:31,    7:10,
   DSE> find -key=^mypoem(#SPAN4)
   Key found in block  3.
       Directory path
       Path--blk:off
       1:10,    2:10,
       Global tree path
       Path--blk:off
       4:3D,    3:10,
   DSE> f -k=^mypoem(#SPAN5)
   Key not found, would be in block  3.
       Directory path
       Path--blk:off
       1:10,    2:10,
       Global tree path
       Path--blk:off
       4:3D,    3:10,

Notice that there is #SPAN2 and #SPAN4 but no #SPAN5. Therefore, #SPAN4 is the last piece. #SPAN3 was not found and is most likely the damaged node.

Dump all the blocks in ZWRITE format to see what can be salvaged.

.. code-block:: bash

   DSE> open -file=mypoem.txt
   DSE> dump -block=6 -zwr
   1 ZWR records written.
   DSE> dump -block=7 -zwr
   1 ZWR records written.
   DSE> dump -block=3 -zwr
   1 ZWR records written.
   DSE> close
   Closing output file:  mypoem.txt
   $ cat mypoem.txt
   ; DSE EXTRACT
   ; ZWR
   $ze(^mypoem,0,480)="Half a league, half a league, Half a league onward, All in the valley of Death Rode the six hundred.
   Forward, the Light Brigade!  Charge for the guns he said: Into the valley of Death Rode the six hundred.
   Forward, the Light Brigade! Was there a man dismayed?  Not tho the soldiers knew Some one had blundered:
   Theirs not to make reply, Theirs not to reason why, Theirs but to do and die: Into the valley of Death Rode the six hundred.
   Cannon to the right of them, Cannon to the left of "
   $ze(^mypoem,22080,480)="them, Cannon in front of them Volleyed and thundered;
   Stormed at with shot and shell, Boldly they rode and well, Into the jaws of Death, Into the mouth of Hell Rode the six hundred.
   Flashed all their sabres bare, Flashed as they turned in air Sabring the gunners there, Charging an army while All the world wondered:
   Plunged in the battery-smoke Right thro the line they broke; Cossack and Russian Reeled from the sabre-stroke
   Shattered and sundered.  Then they rode back, but no"
   $ze(^mypoem,960,468)="t Not the six hundred.
   Cannon to the right of them, Cannon to the left of them, Cannon behind them Volleyed and thundered;
   Stormed at with shot and shell, While horse and hero fell, They that had fought so well Came thro the jaws of Death,
   Back from the mouth of Hell, All that was left of them, Left of six hundred.
   When can their glory fade?  O the wild charge they made!  All the world wondered.
   Honour the charge they made!  Honour the Light Brigade, Noble six hundred!"

Notice that block 3 (which is the second block above (because you started with block 2)) has the correct value but its internal subscript must have been damaged.

Fix the starting position in the $ZEXTRACT statement:

.. code-block:: bash

   $ze(^mypoem,480,480)="them, Cannon in front of them Volleyed and thundered;
   Stormed at with shot and shell, Boldly they rode and well, Into the jaws of Death, Into the mouth of Hell Rode the six hundred.
   Flashed all their sabres bare, Flashed as they turned in air Sabring the gunners there, Charging an army while All the world wondered:
   Plunged in the battery-smoke Right thro the line they broke; Cossack and Russian Reeled from the sabre-stroke
   Shattered and sundered.  Then they rode back, but no"

Verify the value for correctness if you have knowledge of the type of data in this global. This completes data recovery (whatever was possible).

Kill the existing global:

.. code-block:: bash

   YDB>kill ^mypoem
   YDB>write ^mypoem
   %YDB-E-GVUNDEF, Global variable undefined: ^mypoem

Load the salvaged global:

.. code-block:: bash

   $ mupip load -format=zwr mypoem.txt
   ; DSE EXTRACT
   ; ZWR
   Beginning LOAD at record number: 3
   LOAD TOTAL        Key Cnt: 3  Max Subsc Len: 8  Max Data Len: 480
   Last LOAD record number: 5
   $ ydb
   YDB>w ^mypoem
   Half a league, half a league, Half a league onward, All in the valley of Death Rode the six hundred.
   Forward, the Light Brigade!  Charge for the guns he said: Into the valley of Death Rode the six hundred.
   Forward, the Light Brigade! Was there a man dismayed?  Not tho the soldiers knew Some one had blundered:
   Theirs not to make reply, Theirs not to reason why, Theirs but to do and die: Into the valley of Death Rode the six hundred.
   Cannon to right of them, Cannon to left of them, Cannon in front of them Volleyed and thundered;
   Stormed at with shot and shell, Boldly they rode and well, Into the jaws of Death, Into the mouth of Hell Rode the six hundred.
   Flashed all their sabres bare, Flashed as they turned in air Sabring the gunners there, Charging an army while All the world wondered:
   Plunged in the battery-smoke Right thro the line they broke; Cossack and Russian Reeled from the sabre-stroke
   Shattered and sundered.  Then they rode back, but not Not the six hundred.
   Cannon to right of them, Cannon to left of them, Cannon behind them Volleyed and thundered;
   Stormed at with shot and shell, While horse and hero fell, They that had fought so well Came thro the jaws of Death,
   Back from the mouth of Hell, All that was left of them, Left of six hundred.
   When can their glory fade?  O the wild charge they made!  All the world wondered.
   Honour the charge they made!  Honour the Light Brigade, Noble six hundred!

.. _o6-block-size-errors:

++++++++++++++++++++++
O6 - Block Size Errors
++++++++++++++++++++++

If INTEG is passed one of -IMAXBLOCKSIZE=n and/or -DMAXBLOCKSIZE=m, it will check index and/or data blocks, respectively, to verify that the data they contain does not exceed the specified size. Errors of this kind do not themselves indicate any database damage, although they can indicate that a newly-imposed reserved bytes value has not propagated to blocks that have not since been affected by new updates, or that a fill_factor passed to reorg was imposed successfully on blocks that existed when the reorg took place but not on newly created blocks. Errors of this kind therefore do not call for any recovery efforts, but may indicate the need for an additional reorg if the intent is to ensure that all blocks in the database meet a certain level of sparseness.

.. _p1-process-damage:

+++++++++++++++++++++++++
P1 - Process Damage
+++++++++++++++++++++++++

A damaged process is one that has become internally "confused" and is executing in a pathological way not caused by circumstances in the external environment.

 If multiple processes exhibit the same symptoms, the problem is likely embedded in the database.

*IF YOU HAVE DISCOVERED THAT A PROCESS WAS DAMAGED*, carefully review all events related to that process leading to the discovery of the problem. It may be possible that the process had an elevated priority and was not hanging, but rather was "hogging" system resources. It is also possible that the problem is an application loop problem, missed by not performing the steps in :ref:`h3-database-access-problems` with enough rigor.

Check for evidence of any hardware problem that might damage a process.

.. _q1-restricting-database-access:

+++++++++++++++++++++++++++++++++
Q1 - Restricting Database Access
+++++++++++++++++++++++++++++++++

Prevent new users from attempting to access the database by taking steps such as bringing the system to the single-user state or removing execute access to YottaDB components for an appropriate class of users. Also, terminate or suspend all processes accessing the database in question, using the UNIX ps -af utility to find such processes. Because the DSE command CRITICAL -INITIALIZE -RESET generates errors for all processes accessing a database file, it provides a quick way to stop such processes.

.. _r1-runtime-errors:

+++++++++++++++++++++++++++++++++++
R1 - Runtime Errors
+++++++++++++++++++++++++++++++++++

YottaDB processes may detect errors at run-time. These errors trigger the YottaDB error handling mechanism, which generally places the process in direct mode, or triggers the application programs to transcribe an error context to a sequential file or to a global. For more information on error handling, refer to the `"Error Processing" chapter of the Programmer's Guide <../ProgrammersGuide/errproc.html>`_.

Most run-time errors are related to the application and its environment. However, some errors reflect the inability of a process to properly deal with a database. Some errors of this type are also, or only, generated by the YottaDB utility programs.

For descriptions of individual errors, refer to the `Messages and Recovery Procedures Reference Manual <../MessageRecovery/index.html>`_.

*IF YOU CANNOT REPRODUCE SUCH ERRORS WITH ANOTHER PROCESS PERFORMING THE SAME TASK*, or with an appropriately directed MUPIP INTEG, they were most likely reported by a damaged process. In this case, refer to :ref:`p1-process-damage`.

The following table lists run-time errors, alphabetically by mnemonic, each with a section reference for further information.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Run-Time Error Messages Identifying Potential System Problems
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+--------------------+---------------------------------------------------------------------------+----------------------------------------------------+
| Error Mnemonic     | Error Message Text                                                        | Section                                            |
+====================+===========================================================================+====================================================+
| BADDVER            | Incorrect database version vvv                                            | :ref:`i2-yottadb-version-mismatch`                 |
+--------------------+---------------------------------------------------------------------------+----------------------------------------------------+
| BITMAPSBAD         | Database bitmaps are incorrect                                            | :ref:`m1-bitmap-errors`                            |
+--------------------+---------------------------------------------------------------------------+----------------------------------------------------+
| BTFAIL             | The database block table is corrupt                                       | :ref:`r3-runtime-database-cache-problems`          |
+--------------------+---------------------------------------------------------------------------+----------------------------------------------------+
| CCPINTQUE          | Interlock failure accessing Cluster Control Program queue                 | :ref:`r7-interlocked-queue-hardware-problems`      |
+--------------------+---------------------------------------------------------------------------+----------------------------------------------------+
| CRITRESET          | The critical section crash count for rrr region has been incremented      | :ref:`i8-repair-induced-problems`                  |
+--------------------+---------------------------------------------------------------------------+----------------------------------------------------+
| DBCCERR            | Interlock instruction failure in critical mechanism for region rrr        | :ref:`r7-interlocked-queue-hardware-problems`      |
+--------------------+---------------------------------------------------------------------------+----------------------------------------------------+
| DBCRPT             | Database is flagged corrupt                                               | :ref:`i8-repair-induced-problems`                  |
+--------------------+---------------------------------------------------------------------------+----------------------------------------------------+
| DBFILERR           | Error with database file                                                  | :ref:`i5-more-database-access-problems`            |
+--------------------+---------------------------------------------------------------------------+----------------------------------------------------+
| DBNOFILEP          | No database file has been successfully opened                             | :ref:`i5-more-database-access-problems`            |
+--------------------+---------------------------------------------------------------------------+----------------------------------------------------+
| DBNOTGDS           | Unrecognized database file format                                         | :ref:`i5-more-database-access-problems`            |
+--------------------+---------------------------------------------------------------------------+----------------------------------------------------+
| DBOPNERR           | Error opening database file                                               | :ref:`i5-more-database-access-problems`            |
+--------------------+---------------------------------------------------------------------------+----------------------------------------------------+
| DBRDERR            | Cannot read database file after opening                                   | :ref:`i5-more-database-access-problems`            |
+--------------------+---------------------------------------------------------------------------+----------------------------------------------------+
| FORCEDHALT         | Image HALTed by MUPIP STOP                                                | :ref:`r4-stopped-processes`                        |
+--------------------+---------------------------------------------------------------------------+----------------------------------------------------+
| GBLDIRACC          | Global Directory access failed, cannot perform database functions         | :ref:`i5-more-database-access-problems`            |
+--------------------+---------------------------------------------------------------------------+----------------------------------------------------+
| GBLOFLOW           | Database segment is full                                                  | :ref:`r5-no-more-room-in-the-file`                 |
+--------------------+---------------------------------------------------------------------------+----------------------------------------------------+
| GVKILLFAIL         | Global variable KILL failed. Failure code: cccc                           | :ref:`r2-structural-database-integrity-errors`     |
+--------------------+---------------------------------------------------------------------------+----------------------------------------------------+
| GVORDERFAIL        | Global variable $ORDER or $NEXT function failed. Failure code: cccc       | :ref:`r2-structural-database-integrity-errors`     |
+--------------------+---------------------------------------------------------------------------+----------------------------------------------------+
| GVPUTFAIL          | Global variable put failed. Failure code: cccc                            | :ref:`r2-structural-database-integrity-errors`     |
+--------------------+---------------------------------------------------------------------------+----------------------------------------------------+
| GVQUERYFAIL        | Global variable $QUERY function failed. Failure code: cccc                | :ref:`r2-structural-database-integrity-errors`     |
+--------------------+---------------------------------------------------------------------------+----------------------------------------------------+
| GVRUNDOWN          | Error during global database rundown                                      | :ref:`i5-more-database-access-problems`            |
+--------------------+---------------------------------------------------------------------------+----------------------------------------------------+
| GDINVALID          | Unrecognized Global Directory format: fff                                 | :ref:`i5-more-database-access-problems`            |
+--------------------+---------------------------------------------------------------------------+----------------------------------------------------+
| GTMCHECK           | Internal error-report to YottaDB                                          | :ref:`r6-gtmassert-and-gtmcheck-errors`            |
+--------------------+---------------------------------------------------------------------------+----------------------------------------------------+
| GVDATAFAIL         | Global variable $DATA function failed. Failure code: cccc                 | :ref:`r2-structural-database-integrity-errors`     |
+--------------------+---------------------------------------------------------------------------+----------------------------------------------------+
| GVDIRECT           | Global variable name could not be found in global directory               | :ref:`i5-more-database-access-problems`            |
+--------------------+---------------------------------------------------------------------------+----------------------------------------------------+
| GVGETFAIL          | Global variable retrieval failed. Failure code: cccc                      | :ref:`r2-structural-database-integrity-errors`     |
+--------------------+---------------------------------------------------------------------------+----------------------------------------------------+
| GVZPREVFAIL        | Global variable $ZPREVIOUS function failed. Failure code: cccc            | :ref:`r2-structural-database-integrity-errors`     |
+--------------------+---------------------------------------------------------------------------+----------------------------------------------------+
| MUFILRNDWNFL       | File rundown failed                                                       | :ref:`i5-more-database-access-problems`            |
+--------------------+---------------------------------------------------------------------------+----------------------------------------------------+
| UNKNOWNFOREX       | Process halted by a forced exit from a source other than MUPIP            | :ref:`r4-stopped-processes`                        |
+--------------------+---------------------------------------------------------------------------+----------------------------------------------------+
| TOTALBLKMAX        | Extension exceeds maximum total blocks, not extending                     | :ref:`r5-no-more-room-in-the-file`                 |
+--------------------+---------------------------------------------------------------------------+----------------------------------------------------+
| WCFAIL             | The database cache is corrupt                                             | :ref:`r3-runtime-database-cache-problems`          |
+--------------------+---------------------------------------------------------------------------+----------------------------------------------------+

.. _r2-structural-database-integrity-errors:

++++++++++++++++++++++++++++++++++++++++++
R2 - Structural Database Integrity Errors
++++++++++++++++++++++++++++++++++++++++++

These run-time errors indicate that the process detected an integrity error within the body of the database.

Verify the error using the MUPIP command INTEG SUBSCRIPT=, specifying an immediate ancestor node of the global variable displayed in the error message. Alternatively, you may try the access by running the same routine in another process or by using Direct Mode to perform the same actions as those performed by the M line that triggered the error. If you cannot reproduce the error, refer to :ref:`p1-process-damage`.

Most of these errors terminate with a four-character failure code. Each character in the code represents the failure type for an attempt to perform the database access. In cases where the letters are not all the same, the last code is the most critical, because it reflects what happened when the process made its last retry. Earlier tries show error codes that are important to establishing the context of the last error.

.. _runtime-db-failure-codes:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Run-time Database Restart Codes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following table lists the failure codes, whether or not they require a MUPIP INTEG, a brief description of the code's meaning, and a section reference for locating more information. Except in the context of a final retry (four codes in a row), they can be ignored when they are reported in the syslog in conjunction with `NONTPRESTART <../MessageRecovery/errors.html#nontprestart>`_ or `TPRESTART <../MessageRecovery/errors.html#tprestart>`_ messages. They cannot be ignored in the context of :ref:`r1-runtime-errors` above, e.g., `GVGETFAIL <../MessageRecovery/errors.html#gvgetfail>`_.

+------------+------------+-------------------------------------------------------------------------------------------------------------------------+------------+
| Fail Code  | Run INTEG  | Description                                                                                                             | Section    |
+============+============+=========================================================================================================================+============+
| A          | x          | Special case of code C.                                                                                                 | O2         |
+------------+------------+-------------------------------------------------------------------------------------------------------------------------+------------+
| B          | x          | Key too large to be correct.                                                                                            | K1         |
+------------+------------+-------------------------------------------------------------------------------------------------------------------------+------------+
| C          | x          | Record unaligned: properly formatted record header did not appear where expected.                                       | O2         |
+------------+------------+-------------------------------------------------------------------------------------------------------------------------+------------+
| D          | x          | Record too small to be correct.                                                                                         | O2         |
+------------+------------+-------------------------------------------------------------------------------------------------------------------------+------------+
| E          |            | History overrun prevents validation of a block.                                                                         | R3         |
+------------+------------+-------------------------------------------------------------------------------------------------------------------------+------------+
| G          |            | Cache record modified while in use by the transaction.                                                                  | R3         |
+------------+------------+-------------------------------------------------------------------------------------------------------------------------+------------+
| H*         | x          | Development of a new version of a block encountered a likely concurrency conflict.                                      | P1         |
+------------+------------+-------------------------------------------------------------------------------------------------------------------------+------------+
| J          |            | Level on a child does not show it to be a direct descendent of its parent.                                              | O1         |
+------------+------------+-------------------------------------------------------------------------------------------------------------------------+------------+
| K          |            | Cache control problem encountered or suspected.                                                                         | C1         |
+------------+------------+-------------------------------------------------------------------------------------------------------------------------+------------+
| L          |            | Conflicting update of a block took priority.                                                                            | R3         |
+------------+------------+-------------------------------------------------------------------------------------------------------------------------+------------+
| M          | x          | Error during commit that the database logic does not handle.                                                            | P1         |
+------------+------------+-------------------------------------------------------------------------------------------------------------------------+------------+
| N          | x          | A primitive such as a file or queue operation failed.                                                                   | R7         |
+------------+------------+-------------------------------------------------------------------------------------------------------------------------+------------+
| O          |            | Before image was lost prior to its transfer to the journal buffer.                                                      | R3         |
+------------+------------+-------------------------------------------------------------------------------------------------------------------------+------------+
| P          |            | Multi-block update aborted - database damage likely.                                                                    | I5         |
+------------+------------+-------------------------------------------------------------------------------------------------------------------------+------------+
| Q          |            | Shared memory interlock failed.                                                                                         | R7         |
+------------+------------+-------------------------------------------------------------------------------------------------------------------------+------------+
| R          | x          | Critical section reset (probably by DSE).                                                                               | R5         |
+------------+------------+-------------------------------------------------------------------------------------------------------------------------+------------+
| S          |            | Attempt to increase the level beyond current maximum.                                                                   | R8         |
+------------+------------+-------------------------------------------------------------------------------------------------------------------------+------------+
| U          |            | Cache record unstable while in use by the transaction.                                                                  | R3         |
+------------+------------+-------------------------------------------------------------------------------------------------------------------------+------------+
| V          |            | Read-only process could not find room to work.                                                                          | R9         |
+------------+------------+-------------------------------------------------------------------------------------------------------------------------+------------+
| X          |            | Bitmap block header invalid.                                                                                            | M2         |
+------------+------------+-------------------------------------------------------------------------------------------------------------------------+------------+
| Y          | x          | Record offset outside of block bounds.                                                                                  | O2         |
+------------+------------+-------------------------------------------------------------------------------------------------------------------------+------------+
| Z          | x          | Block did not contain record predicted by the index.                                                                    | O2         |
+------------+------------+-------------------------------------------------------------------------------------------------------------------------+------------+
| a          |            | Predicted bitmap preempted by another update.                                                                           | R3         |
+------------+------------+-------------------------------------------------------------------------------------------------------------------------+------------+
| b          |            | History overrun prevents validation of a bitmap.                                                                        | R3         |
+------------+------------+-------------------------------------------------------------------------------------------------------------------------+------------+
| c          |            | Bitmap cache record modified while in use by the transaction.                                                           | R3         |
+------------+------------+-------------------------------------------------------------------------------------------------------------------------+------------+
| d          | x          | Not currently used.                                                                                                     | \-         |
+------------+------------+-------------------------------------------------------------------------------------------------------------------------+------------+
| e          |            | Attempt to read a block outside the bounds of the database.                                                             | O2         |
+------------+------------+-------------------------------------------------------------------------------------------------------------------------+------------+
| f          |            | Conflicting update took priority on a non-isolated global and a block split requires a TP_RESTART.                      | R3         |
+------------+------------+-------------------------------------------------------------------------------------------------------------------------+------------+
| g          |            | The number of conflicting updates on non-isolated global nodes exceed an acceptable level and requires a TP_RESTART.    | R3         |
+------------+------------+-------------------------------------------------------------------------------------------------------------------------+------------+

\* Indicates a process problem

.. _r3-runtime-database-cache-problems:

++++++++++++++++++++++++++++++++++++++++++
R3 - Runtime Database Cache Problems
++++++++++++++++++++++++++++++++++++++++++

These messages indicate probable process damage or database cache corruption. Retry the action with another process. If the second process also fails, refer to :ref:`h4-database-cache-problems`; otherwise, refer to :ref:`p1-process-damage`.

.. _r4-stopped-processes:

+++++++++++++++++++++++++++++++++++++++
R4 - Stopped Processes
+++++++++++++++++++++++++++++++++++++++

These errors indicate the process received a message from a kill system service requesting that the image terminate.

The MUPIP STOP command uses kill with a distinguished code. The code provided by MUPIP STOP allows the process to include the source of the stop directive in the error message.

.. _r5-no-more-room-in-the-file:

++++++++++++++++++++++++++++++++++++
R5 - No More Room in the File
++++++++++++++++++++++++++++++++++++

*IF THE DATABASE FILLS UP AND CANNOT EXPAND*, processes that try to add new information to the database experience run-time errors. The following conditions prevent automatic database expansion.

* Using the MM access method
* Using a file extension of zero (0)
* Inadequate free blocks available on the volume to handle the specified extension

You can handle the first two cases by using the MUPIP EXTEND command. MUPIP EXTEND may also help in dealing with the third case by permitting an extension smaller than that specified in the file header. Note that the extension size in the file header, or /BLOCKS= qualifier to MUPIP EXTEND, is in GDS blocks and does not include overhead for bitmaps.

*IF THERE IS NO MORE SPACE ON A VOLUME*, you may use the M command KILL to delete data from the database. To KILL an entire global, the database file must contain one free GDS block. You may acquire these by KILLing a series of subscripted nodes or by doing a small extension.

You may also use UNIX utilities such as tar, cp, and lprm to remove files from the volume and place them on another volume.

Finally, you may create or add to a bound volume set with the MOUNT utility invoked by the DCL command MOUNT. If you change the RMS placement of the files, be sure to adjust the Global Directory and/or the logical names to match the new environment.

You can also add a new disk. If you change the placement of the files, be sure to also adjust the Global Directory and/or the environment variables to match the new environment.

.. _r6-gtmassert-and-gtmcheck-errors:

+++++++++++++++++++++++++++++++++++
R6 - GTMASSERT and GTMCHECK Errors
+++++++++++++++++++++++++++++++++++

GTMASSERT and GTMCHECK errors indicate that a process has detected some sort of logical inconsistency. Consult with YottaDB after gathering all information about the circumstances surrounding the error.

.. _r7-interlocked-queue-hardware-problems:

+++++++++++++++++++++++++++++++++++++++++
R7 - Interlocked Queue Hardware Problems
+++++++++++++++++++++++++++++++++++++++++

These messages indicate possible problems with multiple processor synchronization. Initiate running of hardware diagnostics. If the diagnostics do not locate a problem, consider consulting with YottaDB after gathering all information about the circumstances of the error.

++++++++++++++++++++++++++++++++++++++++++
R8 - Database Tree Maximum Level Exceeded
++++++++++++++++++++++++++++++++++++++++++

An attempt has been made to create a tree in the database that contains seven or more levels. The legal levels for a tree are zero to seven. You can add new levels to the global either by killing some of the existing subscripts, or by extracting the global and reloading it into a database with a larger block size, so it does not require as large a tree.

+++++++++++++++++++++++++++++++++
R9 - Read-Only Process Blocked
+++++++++++++++++++++++++++++++++

While it is unlikely in normal operation, there is a possibility that a process that has read-only access to a database file may fail because it cannot acquire enough cache space to do its work. Because it does not have authority to write to the database, such a process cannot flush modified cache records to disk: it must rely on updating processes to keep the number of modified records down to a point that permits read-only access to the database to proceed successfully. However, if updating processes exit in a fashion that does not permit them to flush out modified records, the read-only process (particularly one doing a large transaction) may fail because the cache cannot supply enough blocks. This condition can be cleared by a DSE BUFFER command in the affected region(s).

.. raw:: html

    <img referrerpolicy="no-referrer-when-downgrade" src="https://download.yottadb.com/AdminOpsGuide.png" />





