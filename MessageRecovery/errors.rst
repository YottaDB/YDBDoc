.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2017-2022 YottaDB LLC and/or its subsidiaries.#
.. # All rights reserved.                                        #
.. #                                                             #
.. #     This document contains the intellectual property        #
.. #     of its copyright holder(s), and is made available       #
.. #     under a license.  If you do not know the terms of       #
.. #     the license, please stop and do not read further.       #
.. #                                                             #
.. ###############################################################

.. toctree::
    :glob:

.. index:: Error Messages

==============
Error Messages
==============

.. contents::

-----------
ABNCOMPTINC
-----------

ABNCOMPTINC, Deviceparameter xxxx and deviceparameter yyyy are not compatible in the zzzz command

Compile Time Error: The command specifies incompatible deviceparameters (e.g., specifying both FIXED and VARIABLE).

Action: Refer to `Chapter 9 Input Output Processing in the Programmer's Guide <../ProgrammersGuide/ioproc.html>`_ and modify the list.

----------
ACOMPTBINC
----------

ACOMPTBINC, Deviceparameter xxxx is compatible with only yyyy in the command zzzz

Run Time Error: An OPEN, USE, or CLOSE command specifies a deviceparameter that does not apply to the command.

Action: Look for deviceparameters that should be on other I/O commands. For example, the deviceparameter DELETE is valid on CLOSE but produces this error if it is applied to the USE command.

--------------
ACTCOLLMISMTCH
--------------

ACTCOLLMISMTCH, Global ^gggg inherits alternative collation sequence #nnnn from global directory but database file dddd contains different collation sequence #mmmm for this global

Run Time Error: This indicates that the global gggg inherits collation nnnn from the global directory (globals that span multiple regions inherit collation 0 by default) but the directory tree in database file dddd contains a different collation sequence mmmm for this global.

Action: If nnnn is the right collation sequence to use, fix the database file dddd by using a temporary global directory that maps all names to dddd, extract the global, KILL it, use $$set^%GBLDEF to fix the alternative collation for gggg from mmmm to nnnn, reload the global from the extract and switch back to the regular global directory. If mmmm is the right collation sequence to use, recreate the global directory and define GBLNAME gggg to have that collation instead.

------------
ACTIVATEFAIL
------------

ACTIVATEFAIL, Cannot activate passive source server on instance iiii while a receiver server and/or update process is running

MUPIP Error: MUPIP REPLIC -SOURCE -ACTIVATE -ROOTPRIMARY (or -UPDOK) issues this error when the command attempts to activate a passive source server (and switch the instance from being a replicating secondary instance to an originating primary) while a receiver server and/or update process is already running

Action: Shutdown the receiver serever and/or update process and reissue the MUPIP REPLIC -SOURCE -ACTIVATE -ROOTPRIMARY (or -UPDOK) command. Note that any other YottaDB or MUPIP process that was running before the activation does not need to be shut down for the activation to succeed.

--------------
ACTLSTTOOLONG
--------------

ACTLSTTOOLONG, More actual parameters than formal parameters: xxxx

Compile Time/Run Time Error: This indicates that the label xxxx with a formallist; is invoked from within a routine with a longer actuallist (during compile-time). At run-time, a similar error can occur when a longer actuallist is supplied by an invocation from another routine.

Action: Review the interface between the DO command and the subroutine. Modify the actuallist, formallist and/or label, as appropriate.

----------
ACTOFFSET
----------

ACTOFFSET, Actuallist not allowed with offset

Compile Time Error: This indicates that a DO command or an extrinsic specified an actuallist and an entryref that includes an offset.

Action: Look for an inappropriate offset.

---------
ACTRANGE
---------

ACTRANGE, Alternate Collating Type xxxx is out of range

Run Time Error: The alternate collation sequence type does not fall in the expected range of 0 to 255.

Action: Define a new collation sequence type that has a value between 0 and 255 inclusive. For more information, refer the `"Internationalization" chapter of the Programmer's Guide <../ProgrammersGuide/internatn.html>`_.

------------
ADDRTOOLONG
------------

ADDRTOOLONG, Socket address xxxx of length aaaa is longer than the maximum permissible length bbbb

Run Time Error: This indicates that the address value supplied with the CONNECT or LISTEN deviceparameters exceeds the maximum acceptable length.

Action: Examine the value and shorten the length of the address value.

-----------
AIMGBLKFAIL
-----------

AIMGBLKFAIL, After image build for block bbbb in region rrrr failed in DSE or MUPIP

MUPIP/DSE/GT.CM Error: DSE creates after-images of blocks as a result of its physical manipulation of blocks and processes them in the course of RECOVER or ROLLBACK with MUPIP. This error indicates that such a manipulation failed on the block and region indicated.

Action: If you get this error from DSE, you may be working with a block with a damaged state that your DSE action does not sufficiently address - analyze the situation and consider other approaches. If you get this error from MUPIP, it may mean your journal or replication is damaged, in which case you should investigate the state of block bbbb.

-----------
AIOBUFSTUCK
-----------

AIOBUFSTUCK, Waited mmmm minutes for PID: pppp to finish AIO disk write of block: bbbb

All YottaDB Components Warning: PID pppp did not receive a response from the I/O system after waiting for mmmm minutes. Block bbbb cannot accept further updates until the I/O completes.

Action: Examine the I/O subsystem characteristics for tuning or hardware problems, or competing activities.

----------------
AIOCANCELTIMEOUT
----------------

AIOCANCELTIMEOUT, Pid pppp timed out waiting for a pending asynchronous IO operation to complete/cancel in database file ffff

Run Time Error: Pid pppp timed out waiting for a pending asynchronous IO operation to complete/cancel in database file ffff.

Action: While terminating its connection with file ffff, process pppp gave up after waiting approximately one minute without receiving notification that an asynchronous IO it had initiated was successfully completed or canceled.

---------------
AIOQUEUESTUCK
---------------

AIOQUEUESTUCK, Waited mmmm minutes for AIO work queue to complete (cr = rrrr)

All Components Run Time Error: A process is taking over mmmm minutes for asynchronous IO (AIO) activity to complete on cache record rrrr. A zero value for rrrr indicates that the process is waiting for all AIO activity to complete.

Action: Check for trouble in the I/O subsystem. The process continues to wait for AIO activity to complete.

-------------
ALIASEXPECTED
-------------

ALIASEXPECTED, Alias or alias container variable expected in this context

Compile Time/Run Time Error: This indicates the argument for a SET * or KILL * command used a non-alias local variable where the syntax requires an alias or alias container variable.

Action: Correct the code or investigate the program logic to determine why the local variable in question is not in the expected state.

-------------
AMBISYIPARAM
-------------

AMBISYIPARAM, Parameter xxxx is ambiguous to $ZGETSYI()

Run Time Error: This indicates that the argument xxxx is ambiguous to $ZGETSYI() because it does not have enough characters.

Action: Add enough characters to make the argument unambiguous.

----------
ANCOMPTINC
----------

ANCOMPTINC, Deviceparameter xxxx is not compatible with any other deviceparameters in the yyyy command

Compile Time Error: This indicates that the specified deviceparameter can only be used by itself.

Action: Remove conflicting deviceparameters from the command.

-------------
APDCONNFAIL
-------------

APDCONNFAIL, Audit Principal Device failed to connect to audit logger

Run Time Error: The facility for logging activity on principal devices is enabled, but is unable to form a connection with its configured logging program. This prevents a process from taking actions configured for logging initiated on its principal device ($PRINCIPAL).

Action: Check to make sure the logger program is running and listening/accepting connections. If using a TCP or TLS-enabled logger, make sure the port number that the logger is listening/accepting on matches the port number provided in the restriction file. Ensure that the provided information (logger's connection info) in the restriction file is correct. Also make sure that the line in the restriction file is in the correct format. If you are running a TLS-enabled logger, make sure that the logger's TLS certificate is signed by a root CA that YottaDB is aware of through the TLS configuration file. Check the syslog for more information on the error. After addressing the identified issues, restart all processes subject to APD.

---------------
APDINITFAIL
---------------

APDINITFAIL, Audit Principal Device failed to initialize audit information

Run Time Error: YottaDB was unable to process or initialize the provided information (e.g. IP, hostname, port number, UNIX domain socket file path, or TLS ID) from the restriction file. This prevents a process from taking actions configured for logging initiated on its principal device ($PRINCIPAL).

Action: Check the restriction file to make sure information is in the proper format. After addressing identified issues, restart all processes subject to APD.

--------------
APDLOGFAIL
--------------

APDLOGFAIL, Audit Principal Device failed to log activity

Run Time Error: YottaDB was unable to send the to-be-logged activitiy to the logger. This prevents a process from taking the action initiated on its principal device ($PRINCIPAL).

Action: Check to make sure that YottaDB is able to successfully connect to the logger program. Check the syslog for more information on the error.

------------
ARCTLMAXHIGH
------------

ARCTLMAXHIGH, The environment variable XXXX = YYYY is too high. Assuming the maximum acceptable value of ZZZZ

Run Time Warning: The environment variable named XXXX that controls the maximum number of auto-relink routine entries is assigned a value (YYYY) that is too high. YottaDB will set the maximum routine count to ZZZZ and continue normal operation.

Action: Please set the environment variable XXXX to a value between 1,000 and 16,000,000.

------------
ARCTLMAXLOW
------------

ARCTLMAXLOW, The environment variable XXXX = YYYY is too low. Assuming the minimum acceptable value of ZZZZ

Run Time Warning: The environment variable named XXXX that controls the maximum number of auto-relink routine entries is assigned a value (YYYY) that is too low. YottaDB will set the maximum routine count to ZZZZ and continue normal operation.

Action: Please set the environment variable XXXX to a value between 1,000 and 16,000,000.

----------
ARROWNTDSP
----------

ARROWNTDSP, Unable to display ^----- due to length of source line

Compile Time Error: Displayed instead of the arrow indicating where the compilation error occurred due to a long source line.

Action: Refer to the source code. The line number and column number in the associated messages identify the position of the problem. Consider shortening the line, at least until the error is found and corrected.

------
ASSERT
------

ASSERT, Assert failed xxxx line yyyy

Run Time Error: An internal YottaDB consistency check failed. This error only occurs in some special versions of YottaDB software.

Action: Report the entire incident context to your YottaDB support channel.

-----------
ASYNCIONOMM
-----------

ASYNCIONOMM, Database file ffffssss cannot cccc

MUPIP Error: This indicates that the database file has current state ssss ("has ASYNCIO enabled;" or: "has MM access method;") and therefore cannot change to assume change cccc ("enable MM" or "enable ASYNCIO"). MUPIP SET can also issue the same message with the text: "; cannot enable MM and ASYNCIO at the same time".

Action: Address the blocking state: ASYNCIO by disabling it, or disabling the MM access method by changing it to BG before repeating the MUPIP command that produced this error.

-----------
ASYNCIONOV4
-----------

ASYNCIONOV4, rrrr database has ssss; cannot cccc

MUPIP Error: This indicates that region rrrr has current state ssss (ASYNCIO enabled) and therefore cannot change to assume change cccc (enable ASYNCIO).

Action: Address the blocking state: ASYNCIO by disabling it, or by completing an upgrade before repeating the MUPIP command that produced this error.

-------------
AUTODBCREFAIL
-------------

AUTODBCREFAIL, Automatic creation of database file DDDD associated with region RRRR failed; see associated messages for details

All YottaDB Components Error: Error occurs during the runtime creation of a database with the AUTODB flag set so it is automatically created on open. This includes automatically defined statistics databases. The message is followed with the reason for the failure.

Action: Fix the reason for the failure and retry.

----------
BACKUPCTRL
----------

BACKUPCTRL, Control Y or control C encountered during backup, aborting backup

MUPIP Warning: This indicates that BACKUP terminated because of an operator <CTRL>-C or <CTRL>-Y.

Action: Do not rely on the result of this BACKUP. If appropriate, investigate whether any of the BACKUP output files are complete and therefore potentially useable.

------------
BACKUPKILLIP
------------

BACKUPKILLIP, Kill in progress indicator is set for file ffff, backup database could have incorrectly marked busy integrity errors

MUPIP Warning: This indicates that one or more active process are performing a KILL cleanup in database file ffff. Generally, BACKUP can wait for this to finish in order to get a consistent copy of the database. However, this indicates that it waited several minutes and is now proceeding. The resulting backup will almost surely contain blocks incorrectly-marked-busy.

Action: Wait and perform the BACKUP when there are no large KILL operations triggering extensive cleanup. If this is not desirable, fix the errors in the backup copy (reported by an INTEG NOMAP) with DSE MAP FREE. If there are many such blocks, you can edit the INTEG output to create a script to drive the DSE operations. Alternatively, if you can get standalone access to the database, you may use DSE MAP RESTORE. Do not use MAP RESTORE on an active database.

----------
BADACCMTHD
----------

BADACCMTHD, Invalid access method was specified, file not created

MUPIP Warning: This indicates that CREATE encountered an invalid access method for the dynamic segment in the current Global Directory, which is defined by the logical name GTM$GBLDIR/environment variable ydb_gbldir.

Action: Use the Global Directory Editor (GDE) to verify the access method for the Global Directory. Look for the use of YottaDB components with different version numbers.

-----------
BADCASECODE
-----------

BADCASECODE, xxxx is not a valid case conversion code.

Run Time Error: The two-argument form of $ZCONVERT() reports this error if the case conversion specifier (second argument) is not one of the valid codes (U,u,L,l,T and t).

Action: Choose a valid case designation code.

-------
BADCHAR
-------

BADCHAR, XXX is not a valid character in the YYY encoding form.

Run Time Error: YottaDB triggers this error when it encounters a byte sequence that is not legal according to the given character set of the current device.

Action: Correct your application or its configuration to handle this situation. Depending on application requirements, the code may need to handle the illegal byte sequence or disable the triggering of the BADCHAR error using the VIEW "NOBADCHAR" command. If the error is from a READ or WRITE command to a device other than $PRINCIPAL, the application can perform I/O in M-mode and then handle any needed conversion, correction, or other manipulation. For more information, refer to the `Programmer's Guide <../ProgrammersGuide/index.html>`_.

--------
BADCHSET
--------

BADCHSET, xxxx is not a valid character mapping in this context.

Run Time Error: When YottaDB recognizes that the expr in ICHSET=expr or OCHSET=expr is not one of the supported character set names ("M", "UTF-8", "UTF-16", "UTF-16LE" or "UTF-16BE"), it reports this error. Note that not all modes are supported under all conditions.

Action: Choose the proper designation for a supported character set.

-----------------
BADCONNECTPARAM
-----------------

BADCONNECTPARAM, Error parsing or invalid parameter. [XXXX]

MUPIP Error: MUPIP produces this message when there is an error in any connection parameter specified with -CONNECTPARAMS. XXXX contains a brief description of the parameter and its valid value range.

Action: Specify valid values for the -CONNECTPARAM parameter. Refer to the `-CONNECTPARAM documentation <../AdminOpsGuide/dbrepl.html#connectparams>`_ in the Administration and Operations Guide for more information.

BADCONNECTPARAM was added to YottaDB effective release r1.36.

--------
BADDBVER
--------

BADDBVER, Incorrect database version: xxxx

Run Time Error: This indicates that the database version is not compatible with the current YottaDB version.

Action: Upgrade the database. For more information, refer to the release notes for the current YottaDB version and any intervening versions used until the prior version.

-------------
BADGBLSECVER
-------------

BADGBLSECVER, Global section xxxx does not match the current database version

Run Time Error: In attempting to start up a database file, YottaDB encountered a shared memory section containing a database version older than the current database version.

Action: Do not attempt to access the same database files simultaneously with different versions of YottaDB. Perform a MUPIP RUNDOWN with the prior version of the database existing in the shared memory section. If needed, contact your system administrator for help.

------------
BADGTMNETMSG
------------

BADGTMNETMSG, Invalid message sent to GT.CM server, type: xxxx

GT.CM Error: The GT.CM Server received an invalid message. Possible causes are an undetected network error, a message originating from a process that is inappropriately intruding on the GT.CM environment, or a protocol failure in a legitimate process.

Action: Retry the action that resulted in the error notification. If the problem persists, contact the group responsible for database operations on your network.

------------
BADJPIPARAM
------------

BADJPIPARAM, xxxx is not a legal parameter for $ZGETJPI()

Run Time Error: This indicates that the argument xxxx is not a valid keyword for $ZGETJPI().

Action: Refer to the `Programmer's Guide <../ProgrammersGuide/index.html>`_ for correct keyword usage.

-------------
BADLKIPARAM
-------------

BADLKIPARAM, xxxx is not a legal parameter for $ZGETLKI()

Run Time Error: This indicates that the argument xxxx is not a valid keyword for $ZGETLKI().

Action: Refer to the `Programmer's Guide <../ProgrammersGuide/index.html>`_ for correct keyword usage.

-----------
BADLOCKNEST
-----------

BADLOCKNEST, Unsupported nesting of LOCK commands

Run Time Error: YottaDB detected a LOCK (or ZALLOCATE) argument using an extrinsic function that performed other LOCK (or ZALLOCATE) operations, which it could not safely nest.

Action: Revise the code to avoid such a construct. Note that YottaDB recommends avoiding this code pattern as it can produce unintended results that YottaDB does not detect.

---------------
BADPARAMCOUNT
---------------

BADPARAMCOUNT, -CONNECTPARAMS accepts one to six parameter values

MUPIP Error: MUPIP produces this message when there are more than six parameters specified for -CONNECTPARAMS.

Action: Specify one to six parameters or omit -CONNECTPARAMS from the MUPIP REPLICATE -SOURCE -START command to use the default connection parameters.

BADPARAMCOUNT was added to YottaDB effective release r1.36.

-------
BADQUAL
-------

BADQUAL, Unrecognized qualifier: xxxx

Run Time Error: This indicates that a SET of $ZROUTINES specified xxxx, which is an unknown qualifier.

Action: Use an accepted qualifier: SRC= or NOSRC.

---------
BADREGION
---------

BADREGION, Region is not BG, MM, or CM

LKE Error: The current global directory attempted to map a region with an access method other than those listed.

Action: Use LKE only with database mapped to one of the listed database access methods.


-------------
BADSRVRNETMSG
-------------

BADSRVRNETMSG, Invalid message received from GT.CM server

Run Time Error: This indicates that a YottaDB process received an invalid message. Possible causes include an undetected network error, a message originating from a process that is inappropriately intruding on the GT.CM environment, or a protocol failure in a legitimate process.

Action: Retry the action that resulted in the notification error. If the problem persists, contact the group responsible for database operations on your network. Stop and restart the server to attempt to resolve the problem.

------------
BADSYIPARAM
------------

BADSYIPARAM, xxxx is not a legal parameter to $ZGETSYI()

Run Time Error: This indicates that the argument xxxx for $ZGETSYI() is not a valid keyword.

Action: Refer to the `Programmer's Guide <../ProgrammersGuide/index.html>`_ for correct keyword usage.

-----------
BADTRNPARAM
-----------

BADTRNPARAM, xxxx is not a legal parameter to $ZTRNLNM

Run Time Error: This indicates that the argument xxxx for $ZTRNLNM() is not a valid keyword.

Action: Refer to the `Programmer's Guide <../ProgrammersGuide/index.html>`_ for correct keyword usage.

-------------
BADZPEEKARG
-------------

BADZPEEKARG, Missing, invalid or surplus xxxx parameter for $ZPEEK()

Run Time Error: One of the parameters specified to $ZPEEK() is incorrect. Possible values of xxxx:

- mnemonic type - The mnemonic in the first argument is unknown.

- mnemonic argument - An argument for the given mnemonic (specified after a ":" character) is either expected and missing, present and unexpected, or not in its proper form.

- mnemonic argument (region name) - Expected a region name argument which is either missing or not available.

- mnemonic argument (array index) - Expected a numeric array index argument which is either missing or out of range.

- mnemonic argument (peek base address) - Expected an address in the form 0xHHHHHHHH.. which is either missing or invalid.

- offset - Expected a non-negative numeric value which is either missing or invalid.

- length - Expected a non-negative numeric value which is either missing or invalid.

- format - Expected a single character format code which is either missing or invalid.

Action: Review the invocation and correct the defective parameter.

------------
BADZPEEKFMT
------------

BADZPEEKFMT, $ZPEEK() value length inappropriate for selected format

Run Time Error: The format code specified is not valid for the length specified. For example, format code 'I' works with 1, 2, 4, and 8 byte fields. A field length of 3 would raise this error.

Action: Review the invocation and correct the defective length and/or format character.

-------------
BADZPEEKRANGE
-------------

BADZPEEKRANGE, Access exception raised in memory range given to $ZPEEK()

Run Time Error: Some combination of base address, offset, length and/or alignment caused YottaDB to raise a memory access exception when fetching the requested value.

Action: Review the invocation and correct the defective parameter.

------------
BCKUPBUFLUSH
------------

BCKUPBUFLUSH, Unable to flush buffer for online backup

MUPIP Error: This indicates that the online BACKUP was unable to flush the buffer data to disk. The most likely cause is that MUPIP does not have write access to the database file.

Action: Run the BACKUP from a process with write authorization, or wait until other processes have completed the buffer flush. MUPIP sets the repair flag if there is a serious problem. Run a MUPIP INTEG using the FA[ST] qualifier to reset this flag. Report to your System Administrator.

-------
BEGINST
-------

BEGINST, Beginning LOAD at record number: xxxx

MUPIP Information: This indicates that the LOAD command with the FORMAT=BINARY qualifier started with record number xxxx.

Action: -

--------------
BEGSEQGTENDSEQ
--------------

BEGSEQGTENDSEQ, Journal file xxxx has beginning sequence number aaaa greater than end sequence number bbbb

MUPIP Error: This indicates that the beginning sequence number aaaa of the journal file xxxx is greater than the end sequence number bbbb.

Action: Report the entire incident context with appropriate log messages to your YottaDB support channel.

----------
BFRQUALREQ
----------

BFRQUALREQ, The [NO]BEFORE qualifier is required for this command

MUPIP Error: Any MUPIP SET command with JOURNAL=ON must specify either BEFORE_IMAGE or NOBEFORE_IMAGE journaling.

Action: Add the argument and select either BEFORE_IMAGE or NOBEFORE_IMAGE journaling for the database in question.

------
BINHDR
------

BINHDR, gggg Date: dddd TIME: tttt Extract Region Characteristics rrrr Blk Size: xxxx Rec Size: yyyy Key Size: kkkk

MUPIP Information: This message displays header information for a binary format file. gggg is the loaded global. dddd is the date on which the region was extracted. tttt is the time when the region was extracted. rrrr is the region that contains the global. xxxx is the block size specified for the region. yyyy is the record size specified for the block. kkkk is the key size specified for the record.

Action: -

----------
BITMAPSBAD
----------

BITMAPSBAD, Database bit maps are incorrect

Run Time Error: This indicates that a database operation encountered a corrupt bit map.

Action: YottaDB uses bit maps in database files to determine whether a block is free or in use. Report this database structure error to the group responsible for database integrity at your operation.

-----------
BKUPRUNNING
-----------

BKUPRUNNING, Process dddd is currently backing up region xxxx. Cannot start another backup.

MUPIP Error: MUPIP BACKUP -ONLINE only supports one backup at a time and this error indicates an attempt to start one before a previously started backup finished.

Action: Cancel the running BACKUP or reschedule this BACKUP to a time after the running BACKUP completes.

--------------
BKUPTMPFILOPEN
--------------

BKUPTMPFILOPEN, Open of backup temporary file aaaa failed

Run Time Error: When an online backup is in progress, a YottaDB process doing updates to the database is saving away the pre-update images of the blocks it updates in a special backup area used to make sure the backups are consistent. Periodically, these blocks need to be flushed out to a temporary file and are flushed by the process needing the space to put its own changed blocks. This means every running process needs to have R/W access to the temporary file created by the backup. If the process cannot open the temporary file, this error is written to the operator log, the backup is flagged as having encountered an error and the process proceeds. Hence, this error is only backup related. It is NOT an error in the process itself, which proceeds as if backup were not running.

Action: Determine the cause of why the process could not open the temporary file, fix it, and restart backup.

---------------
BKUPTMPFILWRITE
---------------

BKUPTMPFILWRITE, Write to backup temporary file aaaa failed

Run Time Error: When an online backup is in progress, a YottaDB process updating the database is saving away the pre-update images of the blocks it updates in a special backup area used to make sure the backups are consistent. Periodically, these blocks need to be flushed out to a temporary file, and are flushed by the process needing the space to put its own changed blocks. This means that every running process needs to have R/W access to the temporary file created by the backup. If the database write generates an error, the BKUPTFWFAIL error is written to the operator log, the backup is flagged as having encountered an error and the process proceeds. So this error is only backup related. It is NOT an error in the process itself which proceeds as if backup were not running.

Action: Determine the cause of why the write failed, fix it, and restart backup.

------
BLKCNT
------

BLKCNT, Last LOAD Block/RMS Record number: xxxx

MUPIP Information: This indicates that a LOAD command with the FORMAT=BINARY qualifier completed at record number xxxx. It indicates the last block or record number successfully processed by MUPIP LOAD. This information may be useful for auditing the LOAD, or if a restart is required after a LOAD is stopped prematurely.

Action: -

--------------
BLKCNTEDITFAIL
--------------

BLKCNTEDITFAIL, MUPIP recover or rollback failed to correct the block count field in the file header for file xxxx

GDE/DSE Information: This indicates that rollback should correct the block count field in the file header. This message is an informational message issued by the Recovery/Rollback process and no action is required. This error is benign.

Action: -

----------
BLKINVALID
----------

BLKINVALID, bbbb is not a valid block as database file ffff has nnnn total blocks

DSE Error: The block (bbbb) you selected is not currently a valid block in the database file (ffff) for the region in which you are working.

Action: Select a block less than nnnn or move to a different region

---------
BLKSIZ512
---------

BLKSIZ512, Block size xxxx rounds to yyyy

GDE/DSE Information: This indicates that an ADD, CHANGE or TEMPLATE command defined the BLOCKSIZE qualifier to be equal to xxxx, which is not divisible by 512. GDE adjusted the block size to yyyy, which is the next largest multiple of 512.

Action: If yyyy is not acceptable, modify the BLOCKSIZE qualifier value so that it is divisible by 512.

-----------
BLKTOODEEP
-----------

BLKTOODEEP, Block level too deep

Compile Time Error: This indicates that the line starts with too many level-indicator delimiters (.) for the level of nesting associated with the argumentless DO on the previous line.

Action: Remove inappropriate level indicator(s).

------------
BLKWRITERR
------------

BLKWRITERR, Unable to queue disk write for block XXXX. Will keep trying.

Run Time Information: This indicates that the disk is offline or not working because of a hardware or software problem in the disk subsystem.

Action: Check the disk subsystem operation.

-------------
BOMMISMATCH
-------------

BOMMISMATCH, XXX Byte Order Marker found when YYY character set specified.

Run Time Error: A Byte Order Marker (BOM) for character set XXX was found at the beginning of a file specified as containing data in character set YYY.

Action: Specify the proper character set when opening the file. For UTF-16 data, specifying CHSET="UTF-16" will use the BOM to determine whether the data is Little Endian or Big Endian. If no BOM is found, YottaDB assumes Big Endian.

-----------------
BOOLEXPRTOODEEP
-----------------

BOOLEXPRTOODEEP,Boolean expression depth exceeds maximum supported limit of 2047

Compile / Run Time Error: The nesting depth of a Boolean expression exceeds 2047, the YottaDB limit.

Action: Fix the coding issue. Exceeding the nesting depth limit of 2047 is a pathological error that is most likely to occur in generated code.

--------------
BOOLSIDEFFECT
--------------

BOOLSIDEFFECT, Extrinsic ($$), External call ($&) or $INCREMENT() with potential side effects in Boolean expression

Compile Time Warning: This optional message, accompanied by a line and column pointing to the issue, indicates a Boolean expression that contains a side effect in a term other than its first. By default, YottaDB may skip evaluating such terms.

Action: Revise the code to your standards and use the VIEW (arguments [NO]FULL_BOOLEAN or FULLBOOL_WARN) command and/or the environment variable (ydb_boolean) to select the appropriate setting for YottaDB handling of this construct.

-------------
BOVTMGTEOVTM
-------------

BOVTMGTEOVTM, Journal file xxxx has beginning timestamp aaaa greater than end timestamp bbbb

MUPIP Error: This indicates that the beginning time stamp aaaa of the journal file xxxx is greater than the ending timestamp bbbb. This could be due to something that changed the system time while journaling was going on.

Action: Changing system time during YottaDB run-time is not allowed. Contact your YottaDB support channel for further assistance.

--------------
BOVTNGTEOVTN
--------------

BOVTNGTEOVTN, Journal file xxxx has beginning transaction yyyy which is greater than end transaction zzzz

MUPIP Error: This indicates that the MUPIP JOURNAL command has found that journal file xxxx has a beginning transaction yyyy which is greater than the end transaction zzzz.

Action: Report the error with appropriate log messages to your YottaDB support channel.

--------------
BREAK
--------------

BREAK, Break instruction encountered

Run Time Information: This indicates that YottaDB encountered a BREAK command within a routine and entered Direct Mode.

Action: All commands entered at the Direct Mode prompt are compiled and executed as they are entered. To continue program execution, enter the ZCONTINUE command. This message can be supressed using the VIEW BREAKMSG value.

--------------
BREAKDEA
--------------

BREAKDEA, Break instruction encountered during Device error action

Run Time Information: This indicates that YottaDB encountered a BREAK command within a device EXCEPTION string and entered Direct Mode.

Action: YottaDB activates EXCEPTION strings for deviceparameters when a device reports an exception condition. All commands entered at the Direct Mode prompt are compiled and executed as they are entered. To continue program execution, enter the ZCONTINUE command. It is important to ensure that the EXCEPTIONS in production code are thoroughly debugged.

----------
BREAKZBA
----------

BREAKZBA, Break instruction encountered during ZBREAK action

Run Time Information: This indicates that YottaDB encountered a BREAK command within a ZBREAK action string and entered Direct Mode. The ZBREAK command sets or clears breakpoints during debugging. All commands entered at the Direct Mode prompt are compiled and executed as they are entered.

Action: To continue program execution, enter the ZCONTINUE command.

-------------
BREAKZST
-------------

BREAKZST, Break instruction encountered during ZSTEP action

Run Time Information: This indicates that YottaDB encountered a BREAK command within a ZSTEP action string and entered Direct Mode.

Action: The ZSTEP command causes YottaDB to proceed to the beginning of the next line of M code that matches the characteristic specified by the ZSTEP argument. YottaDB compiles and executes all commands entered at the Direct Mode prompt as they are entered. To continue program execution, enter the ZCONTINUE command.

------------
BTFAIL
------------

BTFAIL, The database block table is corrupt; error type xxxx

Run Time Error: This indicates that a database operation failed because the tables of recently used blocks are damaged. YottaDB uses the block tables to control and optimize database traffic.

Action: Report this database cache error to the group responsible for database integrity at your operation.

-------------
BUFFLUFAILED
-------------

BUFFLUFAILED, Errors flushing buffers from uuuu for database file dddd

DSE/MUPIP Error: MUPIP or DSE (uuuu) could not flush the buffers for database file dddd completely. In the case of MUPIP, this typically means that some process is not releasing the critical section. In the case of DSE, this typically means there is some error in the global buffer cache which needs to be fixed.

Action: In the case of MUPIP, wait approximately 20 seconds and retry. In the case of DSE, try DSE CACHE RECOVER to fix the cache. If the error persists, report it to the group responsible for database integrity at your operation as soon as possible.

---------------
BUFOWNERSTUCK
---------------

BUFOWNERSTUCK, PID xxxx waiting for PID yyyy to finish disk read of block zzzz. Been waiting for aaaa minutes.

Run Time Warning: Poor response time from the I/O subsystem could cause this error, or if the process performing the disk-read was suspended.

Action: If the disk-reading process is found to be in a process-suspended state, un-suspend it. If it is not suspended, examine the I/O subsystem performance characteristics for behavior problems. Report the full error message, along with the operator log messages during the specific timeframe to your YottaDB support channel.

---------------
BUFRDTIMEOUT
---------------

BUFRDTIMEOUT, Pid xxxx timed out waiting for buffered read by process yyyy to complete in database file zzzz

Run Time Information: This indicates that a process requiring a buffer transfer began but did not complete. The system cancelled the process.

Action: The failed process must be run again. Other errors may appear in the operators log with this one to indicate why the process failed.

--------------
BUFSIZIS
--------------

BUFSIZIS, Journal Buffer size is xxxx

GDE Information: This message reports xxxx as the size of the journal buffer.

Action: Review the accompanying message(s) for additional information.

-------------
BUFSPCDELAY
-------------

BUFSPCDELAY, Request for bbbb blocks in region rrrr delayed

All Components Run Time Warning: A process is taking longer than expected to obtain bbbb free database buffers for region rrrr. If bbbb is zero, then the process was attempting to free one particular buffer.

Action: Check for trouble in the I/O subsystem. The process continues its attempt to obtain the free buffer(s).

---------------
BUFTOOSMALL
---------------

BUFTOOSMALL, But block size xxxx requires buffer size yyyy

GDE Information: This indicates that an ADD, CHANGE, or TEMPLATE command specified an xxxx argument definition for the BUFFER_SIZE qualifier, which is incompatible with the definition of the BLOCKSIZE. yyyy specifies the minimum buffer size that can support this block size.

Action: Modify the block size and/or buffer size so that they are compatible. Review the accompanying message(s) for the buffer size.


--------------
CALLERID
--------------

CALLERID, Routine xxxx called from yyyy

Run Time Error: This message provides the error location. Typically this error displays with other errors.

Action: Review the accompanying message(s) for additional information. Include this information if reporting the error to your YottaDB support channel.

------------------
CALLINAFTERXIT
------------------

CALLINAFTERXIT, After a ydb_exit, a process can never create a valid YottaDB context

Run Time Error: Once a call-in has done a call to ydb_exit(), a process can no longer do YottaDB call-ins

Action: Either move or remove the inappropriate call-ins or move the ydb_exit call to a later point.

----------------
CALLINTCOMMIT
----------------

CALLINTCOMMIT, TCOMMIT at call-in-level=xxxx not allowed as corresponding TSTART was done at lower call-in-level=yyyy.

Run Time Error: This indicates that at least one call-in invocation happened in between when the TP transaction started (either through a ydb_tp_s() call in C or a TSTART command in M), and when the corresponding transaction commit is attempted (through a TCOMMIT command in M).

Action: If a TP transaction is started using SimpleAPI, and the user function driven by ydb_tp_s() does a call-in invocation, care should be taken to ensure that the call-in code does not do a TCOMMIT.

----------------
CALLINTROLLBACK
----------------

CALLINTROLLBACK, TROLLBACK at call-in-level=xxxx not allowed as corresponding TSTART was done at lower call-in-level=yyyy

Run Time Error: This indicates that at least one call-in invocation happened in between when the TP transaction started (either through a ydb_tp_s() call in C or a TSTART command in M) and when the corresponding transaction rollback is attempted (through a TROLLBACK command in M).

Action: If a TP transaction is started using SimpleAPI, and the user function driven by ydb_tp_s() does a call-in invocation, care should be taken to ensure that the call-in code does not do a TROLLBACK.

-------------
CANTBITMAP
-------------

CANTBITMAP, Can't perform this operation on a bit map (block at a 200 hexadecimal boundary)

DSE Error: The selected DSE operation does not apply to bit maps (blocks divisible by 0x200).

Action: Select an appropriate block for the operation or an appropriate operation for a bit map.

--------------
CEBIGSKIP
--------------

CEBIGSKIP, Compiler escape user routine skip count is too large

Compile Time/Run Time Error: This indicates that the skip count exceeded the maximum line length.

Action: Verify the user-supplied escape-handling routine(s).


-----------
CENOINDIR
-----------

CENOINDIR, Indirection type information not available for compiler escape feature

Compile Time/Run Time Error: This indicates that YottaDB does not currently support this feature.

Action: -

-------------
CETOOLONG
-------------

CETOOLONG, Compiler escape substitution exceeds maximum line size

Compile Time/Run Time Error: This indicates that the length of the substitution string exceeds the maximum line length.

Action: Determine whether the input source file can be modified to have less source on the line in question. If this is not possible, modify the substitution mechanism design or implementation.

--------------
CETOOMANY
--------------

CETOOMANY, Too many compiler escape substitutions in a single statement

Compile Time/Run Time Error: This indicates that the program being compiled contained more nested substitutions than allowed; that is, 1024.

Action: Reduce the number of substitutions to less than 1024.


------------
CEUSRERROR
------------

CEUSRERROR, Compiler escape user routine returned error code xxxx

Compile Time/Run Time Error: This indicates that the compiler encountered the error specified by error code xxxx.

Action: Use the error code as the function argument of $ZMESSAGE() to determine the text associated with the error and the appropriate corrective action.


-----------------
CHANGELOGINTERVAL
-----------------

CHANGELOGINTERVAL, ssss Server now logging to ffff with a IIII second interval

MUPIP Information: This message confirms a change to a replication server (ssss) by showing the current log file (ffff) and log interval (IIII)

Action: None Required

------------------
CHNGTPRSLVTM
------------------

CHNGTPRSLVTM, MUPIP will change tp_resolve_time from xxxx to yyyy because expected EPOCH or EOF record was not found in Journal File zzzz.

MUPIP Information: At startup, backward recovery/rollback internally computes a time called the tp_resolve_time, which is until when backward processing will be performed across journal files of all regions. During backward processing, it is possible in very rare cases that recovery does not see an EPOCH record or an EOF record as the last record in the journal file of the regions that had not been updated for quite a long time. In such cases, recovery reduces the tp_resolve_time further by taking into account the timestamp of the last journal record. This effectively causes further backward processing but is necessary for a clean recovery. A CHNGTPRSLVTM message is printed whenever such journal files are encountered by backward recovery.

Action: None necessary

------------------
CHSETALREADY
------------------

CHSETALREADY, Socket device already contains sockets with iCHSET=xxxx, oCHSET=xxxx

Run Time Error: The code tried to create a new socket, specifying a CHSET different than the CHSET previously assigned to the SOCKET device.

Action: If different CHSETs are needed for different sockets, place them in different SOCKET devices. Also, if the CHSET of the SOCKET device needs to be changed, it can be done with the OPEN/USE command which doesn't create a new socket.

----------------
CIDIRECTIVE
----------------

CIDIRECTIVE, Invalid directive parameter passing. Expected I, O or IO.

Syntax Error: This indicates that a missing directive or syntactically invalid directive was found for the parameter, pointed to by the previous messages EXTSRCLIN and EXTSRCLOC.

Action: One of the directives I, O or IO should be specified for the parameter in the entry displayed.


------------------
CIENTNAME
------------------

CIENTNAME, No label reference found for this entry in call-in table

Syntax/Call in Error: This indicates that a label reference to the M routine is missing or syntactically invalid for an entry in the call-in table (specified by ydb_ci environment variable)

Action: Correct the syntax errors in the call-in table entry, at the location pointed to by the two previous messages (EXTSRCLIN and EXTSRCLOC), displaying the line and the column number respectively. Make sure a valid M label reference is bound to the C call-name specified for this entry.

--------------------
CIMAXLEVELS
--------------------

CIMAXLEVELS, Too many nested Call-ins. Nested resources exhaused at level xxxx.

Call in/Run Time Error: This indicates that YottaDB runs out of its internal condition handlers stack due to too many levels of nested call-ins.

Action: Ensure that the call-in application is not nested more than the limit (xxxx) that YottaDB supports. The number of nested call-ins can be reduced by not using call-ins, wherever possible, from external call functions.

-----------------------
CIMAXPARAM
-----------------------

CIMAXPARAM, Exceeded maximum number of parameters in the call-in table entry. An M routine cannot accept more than 32 parameters.

Call in/Run Time Error: This indicates that the call-in table specified by $ydb_ci contains more than 32 parameters. Since an M formallist can only accept up to 32 parameters, the user cannot pass more than 32 arguments to ydb_ci(), excluding <c-call-name> and <ret-type>.

Action: Reduce the number of parameters to be less than 32 in the call-in table as well as in the M routine.

----------------------
CINOENTRY
----------------------

CINOENTRY, No entry specified for xxxx in the call-in table

Run Time Error: This indicates that the call-name invoked by the C program does not have a corresponding entry in the call-in table specified by ydb_ci environment variable.

Action: Add an entry to the call-in table for the call-name. Refer to the `External Calls section in the Programmer's Guide <../ProgrammersGuide/langfeat.html#ext-calls>`_.

-----------------------
CIPARTYPE
-----------------------

CIPARTYPE, Invalid type specification for O/IO directive - expected pointer type

Syntax/Call in Error: This indicates that non-pointer types were specified for the parameters to be passed by output-only (O) and input-output (IO) conventions.

Action: Make sure one of the valid pointer types is specified for O and IO parameters. Refer to the `External Calls section in the Programmer's Guide <../ProgrammersGuide/langfeat.html#ext-calls>`_.


------------------
CIRCALLNAME
------------------

CIRCALLNAME, Call-in routine name expected but not found

Syntax/Call in Error: This indicates that a call-name, which is to be bound to an M routine, is either missing or syntactically invalid for an entry in the call-in table file.

Action: Make sure a valid call-name is specified in the call-in table entry, at the location pointed to by the two previous messages: EXTSRCLIN and EXTSRCLOC, displaying the line and the column number respectively.

-------------------
CIRPARMNAME
-------------------

CIRPARMNAME, Invalid parameter specification for call-in table

Syntax/Call in Error: This indicates that a syntax error was found in parameter specification in the call-in table.

Action: Correct the syntax errors and make sure the parameters are correctly specified. Refer to the `External Calls section in the Programmer's Guide <../ProgrammersGuide/langfeat.html#ext-calls>`_.


------------------
CIRTNTYP
------------------

CIRTNTYP, Invalid return type

Syntax/Call in Error: This indicates that the return type specified in the call-in entry is either missing or found invalid.

Action: Correct the return type syntax errors in the call-in table entry, at the location pointed to by the two previous messages: EXTSRCLIN and EXTSRCLOC, displaying the line and the column number respectively. Make sure a valid return type is specified for this entry.


------------------
CITABENV
------------------

CITABENV, Environment variable for call-in table xxxx not set

Call in/Run Time Error: This indicates that the environment variable ydb_ci is not defined when an external C routine is about to call an M routine through the YottaDB call-in mechanism.

Action: Check if ydb_ci is defined to be a valid file path to a call-in table. The call-in table file should contain a list of entries, each entry describing the parameter types, their passing convention of each M routine and its binding to a C routine.

------------
CITABOPN
------------

CITABOPN, Unable to open call-in table: xxxx

Call in/Run Time Error: This indicates that the call-in table defined by the environment variable ydb_ci could not be opened.

Action: Check if the file path specified by ydb_ci is correct and has at least read permissions for the user running YottaDB. Check for secondary message(s) accompanying this error.


-------------
CIUNTYPE
-------------

CIUNTYPE, Unknown parameter type encountered

Syntax/Call in Error: This indicates that missing or invalid parameter type specified for the entry displayed by the previous messages EXTSRCLIN and EXTSRCLOC.

Action: Make sure one of the valid parameter types is specified for the parameter in the entry displayed. Refer to the `External Calls section in Programmer's Guide <../ProgrammersGuide/langfeat.html#ext-calls>`_.


-----------------
CLIERR
-----------------

CLIERR, xxxx

Run Time Error: This indicates that an invalid command has been entered. xxxx provides further detail to the invalid command entered.

Action: Review the error and enter valid command.

--------------
CLISTRTOOLONG
--------------

CLISTRTOOLONG, SSSS specified is BBBB bytes long which is greater than the allowed maximum of MMMM bytes

All YottaDB Components Error: A command string SSSS of BBBB bytes exceeds the maximum supported command line length of MMMM

Action: Reduce the line length, by using unambiguous abbreviations or shortening names.

-----------------
CLOSEFAIL
-----------------

CLOSEFAIL, Error while closing file descriptor dddd

Run Time Error: YottaDB records this error in the syslog whenever it attempts to close an open file descriptor dddd and the close returns with an error. After recording this error, the YottaDB process resumes normal operation.

Action: Report the above error message along with the accompanying YDB-I-CALLERID message to your YottaDB support channel, as it may be a symptom of out-of-design operation.


---------------
CLSTCONFLICT
---------------

CLSTCONFLICT, Cluster conflict opening database file xxxx; could not secure access. Already open on node yyyy.

Run Time Error: This indicates that the process attempted to access non-clustered database xxxx, which was opened by node yyyy.

Action: Review the accompanying message(s) for additional information. Move the process to the appropriate node or use GDE to change the mapping in the Global Directory.

This error message can also occur after a database is improperly shut down. Perform a MUPIP RUNDOWN with a FILE or REGION qualifier.


----------------
CMD
----------------

CMD, Command expected but not found

Compile Time Error: This indicates that YottaDB encountered something other than a command- the next valid syntax element. This error can occur when there is an invalid character in the middle of a variable name or keyword, such as in the M line S X=Y_$B or in the command W "this is a tab <TAB>".

Action: Verify the line syntax. Replace the line if it contains invisible (non-graphic) characters because diagnosing the line syntax may prove time consuming.

------------
CMEXCDASTLM
------------

CMEXCDASTLM, Exceeded AST limit. Cannot open database.

GT.CM Error: This indicates that the GT.CM server exceeded its quota of asynchronous system traps (ASTs).

Action: Increase the ASTLIM for the GT.CM server by modifying GTCMSTART.COM, shut down and restart the GT.CM server. Review SYSGEN factors and user authorizations for AST limits.


--------------
CMICHECK
--------------

CMICHECK, Internal CMI error. Report to YottaDB Support.

GT.CM Error: This indicates that GT.CM and DECNET cannot communicate properly. See GTMCHECK.

Action: Report the entire incident context to your YottaDB support channel.


--------------
CMINTQUE
--------------

CMINTQUE, Interlock failure accessing GT.CM server queue

GT.CM Error: This indicates that interlock cannot move data onto the queue interlock bit.

Action: Accompanying messages should indicate the hardware or software problem that caused the interlock failure.


----------------
CMSYSSRV
----------------

CMSYSSRV, Error doing system service, status:

GT.CM Error: This indicates that the GT.CM server could not successfully perform some system service. This message is followed by a secondary error message that describes the nature of the failure.

Action: Retry the action that resulted in the error. If the problem persists, contact the group responsible for database operations on your network.


---------------------
CNOTONSYS
---------------------

CNOTONSYS, command is not supported by this operating system

Compile Time Error: This indicates that the operating system does not support the command.

Action: Check the operating system documentation for a supported command.

-----------------
COLLARGLONG
-----------------

COLLARGLONG, Collation sequence nnn does not contain routines for long strings

Run Time Error: Strings longer than 32,767 bytes have been used with alternative collation and only gtm_ac_xform and gtm_ac_xback are defined in the collation library.

Action: Define gtm_ac_xfrom_1 and gtm_ac_xback_1 routines in the collation library.

----------------
COLLATIONUNDEF
----------------

COLLATIONUNDEF, Collation type xxxx is not defined

Run Time Error: This indicates that an attempt was made to reference a collation sequence that is not available.

Action: Ensure that the environment variable GTM_COLLATE_n is properly defined, where n is the identification number of the failing collation type. Ensure that the file referencing GTM_COLLATE_n is available to the process. Use host shell commands to verify its location and protections. Examine the executable (and/or the objects that comprise it) to determine whether it has the proper entry points. For more information, refer to the `"Internationalization" chapter of the Programmer's Guide <../ProgrammersGuide/internatn.html>`_.

----------------
COLLDATAEXISTS
----------------

COLLDATAEXISTS, Collation type cannot be changed while xxxx data exists

Run Time Error: This indicates that an attempt was made to change the local collation type while xxxx was either a subscripted local, for a process collation change, or a gvn name, for global variable collation.

Action: KILL or NEW the local variables before you change the local collation type, or KILL a gvn before changing its collation.


----------------
COLLFNMISSING
----------------

COLLFNMISSING, Routine xxx is not found for collation sequence nnn

Run Time Error: Required transformation back routine is missing in collation library.

Action: If gtm_ac_xfrom_1 is defined, also define gtm_ac_xback_1. Or if gtm_ac_xform is defined, also define gtm_ac_xback in the collation library.

-----------------
COLLTYPVERSION
-----------------

COLLTYPVERSION, Collation type xxxx, version yyyy mismatch

Run Time Error: This indicates that the user image with collation type xxxx does not accept the version of collation for an existing global. yyyy is the version associated with the global.

Action: Review the implementation history of the current collation algorithm. Modify or replace the image, if appropriate. If the version should change, temporarily RESTORE the older matching module, unLOAD and KILL any globals using the older algorithms, RESTORE the new algorithms, and reLOAD the global.

--------------
COLON
--------------

COLON, Colon (:) expected in this context

Compile Time Error: This indicates that YottaDB did not encounter a colon where expected.

Action: Look for a $SELECT() function that does not have a colon separating the conditional expression from its corresponding value expression. Also, look for a ZGOTO that is missing a colon between the level and an entry reference.


----------------
COLTRANSSTR2LONG
----------------

COLTRANSSTR2LONG, Output string after collation transformation is too long

Run Time Error: an alternative collation transform or reverse transform attempted to use more bytes than the configuration permits.

Action: Adjust the implementation of the collation transform to minimize key expansion; increase the maximum permitted key size if appropriate. Note that the current supported maximum is 1019 bytes. If the key size is already maxed out and the transformation algorithm is optimal, you must modify the application to reduce the key size.


----------------
COMMA
----------------

COMMA, comma expected in this context

Compile Time Error: This indicates that YottaDB did not encounter a comma where expected.

Action: Look for a missing argument or comma in a function that requires multiple arguments.


-----------------
COMMAORRPAREXP
-----------------

COMMAORRPAREXP, Comma or right parenthesis expected but not found

Compile Time Error: This indicates that YottaDB did not encounter a comma or right parenthesis where expected.

Action: Look for a list of improperly formatted subscripts, arguments or parameters.


----------------
COMMENT
----------------

COMMENT, Comment line. Placed zbreak at next executable line.

Run Time Information: This indicates that a ZBREAK specified a line that had no active code. Therefore, YottaDB set the ZBREAK at the next line containing source code.

Action: -

-------------------
COMMFILTERERR
-------------------

COMMFILTERERR, Error executing the command filter for FFFF DDDD

Run Time Error: Reports a problem in filter code where FFFF describes the nature of the filter and DDDD some thing about the nature of the issue. There may be associated/related messages. Because filters are a potential security tool, these errors tend are generally reported to the operator log.

Action: Analyze the filter code in light of the messages and revise accordingly.

----------------
COMMITWAITPID
----------------

COMMITWAITPID, Pid wwww waited tttt minute(s) for pid pppp to finish commits in database file dddd

Run Time Warning: This warning message in the operator log indicates the total amount of time that the process wwww waited for another process pppp to finish a database transaction commit. If the $ydb_procstuckexec mechanism is enabled, this message invokes it. If a process waits for more than one process to finish database transaction commits, it issues this message for each one it encounters.

Action: If the process pppp is still running, get a C-stack trace of the process (using a debugger) and report to your YottaDB support channel with system log and operator log information.


--------------------
COMMITWAITSTUCK
--------------------

COMMITWAITSTUCK, Pid wwww timed out after waiting tttt minute(s) for nnnn concurrent YottaDB process(es) to finish commits in database file dddd

Run Time Error: This error message indicates that a process could not finish a database transaction commit and timed out waiting for other concurrent processes to finish. The process will continue to wait.

Action: Check the operator log for accompanying COMMITWAITPID messages. Every concurrent YottaDB process reporting COMMITWAITSTUCK messages would have accompanying COMMITWAITPID message(s). If so, review those messages. If not, report to your YottaDB support channel with system log and operator log information.

----------------
COMPILEQUALS
----------------

COMPILEQUALS, Error in compiler qualifiers: xxxx

Compile Time/Run Time Error: This indicates that a run-time compilation specified an invalid qualifier (xxxx). Qualifiers for run-time compilation can be specified with ZLINK or by setting $ZCOMPILE to a qualifier string that YottaDB uses for auto-ZLINKs with no qualifiers.

Action: Review the qualifiers in the ZLINK sub-argument or those being SET into $ZCOMPILE.

---------------
CONNSOCKREQ
---------------

CONNSOCKREQ, Socket not connected

Run Time Error: The operation attempted requires a socket in the CONNECTED state, and the provided socket was not connected.

Action: Make sure the correct socket is being used and that the socket is connected. ZSHOW "D" may provide useful details on the current socket state.

--------------------
COREINPROGRESS
--------------------

COREINPROGRESS, Previous core attempt failed; core generation bypassed.

Run Time Error: This indicates that the process, which failed, was unable to create a memory dump file and tried to create another one.

Action: Report the entire incident context to your YottaDB support channel for further analysis.

----------------------
CORRUPT
----------------------

CORRUPT, Corrupt input in Blk #xxxx, Key #yyyy; resuming with next global block

MUPIP Warning: This indicates that LOAD encountered bad data in the input file.

Action: Refer to the topic MUPIP LOAD Errors in `"About This Manual" section <./about.html#mupip-load-errors>`_.

--------------------------
CORRUPTNODE
--------------------------

CORRUPTNODE, Corrupt input in Record # rrrr, Key #yyyy; resuming with next global node

MUPIP Error: This message reports that record rrrr with apparent key kkkk does not have a valid format for MUPIP LOAD.

Action: USE %GO or MUPIP EXTRACT to recapture the problematic node(s) or use an editor to create valid copies of the nodes in an LOAD file.

-----------------------------
CPBEYALLOC
-----------------------------

CPBEYALLOC, Attempt to copy beyond the allocated buffer

DSE Error: This indicates that DSE tried to add a record that did not fit into the block size and/or the balanced tree structure.

Action: Check the block size. Move or create the record in a different location.

---------------------------
CREDNOTPASSED
---------------------------

CREDNOTPASSED, Socket message contained no passed credentials

Run Time Error: WRITE /PASS or WRITE /ACCEPT was given a process id to verify, but YottaDB was unable to obtain the peerprocess id.

Action: See the accompanying ENO error for details.

------------------------
CRITRESET
------------------------

CRITRESET, The critical section crash count for region xxxx has been incremented

Run Time Fatal: This indicates that the critical section for region xxxx was reset by the DSE command CRITICAL with the qualifiers INIT and RESET, while this process was accessing that database.

Action: Wait until the DSE repair operations are complete before retrying or restarting the process.

-------------------------
CRITSEMFAIL
-------------------------

CRITSEMFAIL, Error with semaphores for region xxxx

Run Time Error: This indicates that YottaDB encountered a missing or damaged semaphore. This typically indicates that an agent external to YottaDB has deleted or modified the semaphores YottaDB uses to manage database and LOCK interactions.

Action: Investigate the state of the YottaDB semaphores and all previous actions that might have damaged them.

-------------------
CRYPTBADCONFIG
-------------------

CRYPTBADCONFIG, Could not retrieve data from encrypted file ffff due to bad encryption configuration. eeee

Run Time Error: The error occurs when a YottaDB utility program starts with a bad encryption configuration (like a bad password) and attempts to read a block corresponding to file ffff (either from memory or disk).

Action: Look at the accompanying messages (or prior messages related to encryption) for more details on what encryption configuration parameter is incorrect.

-------------------
CRYPTBADWRTPOS
-------------------

CRYPTBADWRTPOS, Encrypted WRITE disallowed from a position different than where the last WRITE completed

Run Time Error: A WRITE attempt to an encrypted device violates the integrity of the produced ciphertext. This is the case, for example, when trying to WRITE to a previously encrypted and CLOSEd file. Because encryption ciphers rely on state machine algorithms, YottaDB prohibits WRITEs performed in non-sequential fashion or when they threaten to overlay already encrypted data.

Action: Revise your M code to avoid illegal I/O operations with encryption. Note in particular, that when using encryption, non-empty files cannot be opened in APPEND mode; the SEEK deviceparameter is prohibited; and the TRUNCATE is only permitted at the beginning or end of a file.

-----------------------
CRYPTDLNOOPEN
-----------------------

CRYPTDLNOOPEN, Failed to load encryption library while opening encrypted file ffff. eeee

Run Time Error: YottaDB failed to load the gtmcrypt plug-in or one of its related libraries.

Action: Refer to the accompanying details (eeee) and verify that the gtmcrypt plug-in and related libraries are properly installed and that $LD_LIBRARY_PATH and $LIBPATH are properly set.

---------------------
CRYPTDLNOOPEN2
---------------------

CRYPTDLNOOPEN2, Failed to load encryption library dddd. Eeee

Run Time Error: YottaDB failed to load the gtmcrypt plug-in or one of its related libraries during startup.

Action: Refer to the accompanying details (eeee) and verify that the gtmcrypt plug-in and related libraries are properly installed and that $LD_LIBRARY_PATH and $LIBPATH are properly set.

------------------
CRYPTHASHGENFAILED
------------------

CRYPTHASHGENFAILED, Failed to generate cryptographic hash for symmetric key corresponding to file ffff. eeee

Run Time Error: gtmcrypt plug-in reports that there is a problem with the hash function.

Action: Examine the message (eeee) from the plug-in and take the appropriate action.

------------------
CRYPTINIT
------------------

CRYPTINIT, Failed to initialize encryption library while opening encrypted file ffff. eeee

Run Time Error: The gtmcrypt plug-in reports that it is unable to initialize one or more of its related libraries.

Action: Examine the detailed message (eeee) from the plug-in and take appropriate action.

------------------
CRYPTINIT2
------------------

CRYPTINIT2, Failed to initialize encryption library during YottaDB startup. eeee

Run Time Error: The gtmcrypt plug-in reports that it is unable to initialize one or more of its related libraries during YottaDB startup.

Action: Examine the detailed message (eeee) from the plug-in and take appropriate action.

------------------
CRYPTJNLMISMATCH
------------------

CRYPTJNLMISMATCH, Encryption settings mismatch between journal file jjjj and corresponding database file dddd

All Components Error: Encryption settings in the header of database file dddd do not match those stored in the header of journal file jjjj. The most likely cause is inappropriate operator action such as replacing the current journal file with an older journal file.

Action: Correct the error that caused the incorrect journal file to be pointed to by the database file. If the correct journal file has been inadvertently deleted, create new journal files with the -noprevjnl switch. Take a backup as soon as possible thereafter. Depending on your situation, you may need to refresh secondary instances.

--------------------
CRYPTKEYFETCHFAILED
--------------------

CRYPTKEYFETCHFAILED, Failed to retrieve encryption key corresponding to file ffff. eeee

Run Time Error: gtmcrypt plug-in reports it was unable to obtain an encryption key for file ffff.

Action: Examine the message (eeee) from the plug-in and take the needed action: for example, verify that the encryption key for this file is pointed to by the database key file, verify proper permissions on the directory path and file and so on. Also, make sure that there is an appropriate maximum process limit because obtaining an encryption key may fork other processes.

---------------------
CRYPTKEYFETCHFAILEDNF
---------------------

CRYPTKEYFETCHFAILEDNF, Cannot obtain encryption key. xxxx

Run Time Error: gtmcrypt plug-in reports it was unable to obtain an encryption key based upon matching the hash of an encryption key.

Action: Examine the message (xxxx) from the plug-in and take the needed action: for example, verify that the encryption keys for all database files are pointed to by the database key file. For extracts and backups, verify that all the keys from the databases that provided records are in the database key file.

---------------------
CRYPTKEYRELEASEFAILED
---------------------

CRYPTKEYRELEASEFAILED, Could not safely release encryption key corresponding to file ffff. eeee

All YottaDB Components Error: gtmcrypt plug-in reports that it is unable to release the memory pertaining to the encryption key associated with file ffff due to error eeee

Action: Examine message eeee from the plug-in and take the needed action: for example, ensure that the memory is accessible, the process has correct permissions, and so on.

-----------------------
CRYPTKEYTOOBIG
-----------------------

CRYPTKEYTOOBIG, Specified key has length xxxx, which is greater than the maximum allowed key length yyyy

Run Time Error: A key name value specified with the [I|O]KEY deviceparameter on an OPEN or USE command is too long.

Action: Verify that the key name portion of the [I|O]KEY deviceparameter's value (substring before the first space, if any) corresponds to an existing field name in the 'database.keys' or 'files' section of the configuration file and does not exceed yyyy characters in length.

-------------------
CRYPTNOAPPEND
-------------------

CRYPTNOAPPEND, APPEND disallowed on the encrypted file xxxx

Run Time Error: An OPEN command specifies both an APPEND deviceparameter and a non-empty value for the [I|O]KEY deviceparameter.

Action: Because encryption algorithms maintain state as they process text, APPENDing encrypted data to a non-empty file is prohibited; revise your application code accordingly.

----------------------
CRYPTNOKEY
----------------------

CRYPTNOKEY, No encryption key specified

MUPIP Error: MUPIP REORG -ENCRYPT prints this message if no encryption key is specified.

Action: Provide the requisite encryption key to the command as instructed in YottaDB documentation.

-------------------
CRYPTNOKEYSPEC
-------------------

CRYPTNOKEYSPEC, Key name needs to be specified with KEY, IKEY, or OKEY device parameter for encrypted I/O

Run Time Error: A key name value specified with the [I|O]KEY deviceparameter on an OPEN or USE command is empty while the initialization vector (IV) is not.

Action: If enabling encryption or modifying encryption attributes, be sure to include an appropriate key name; if disabling encryption, leave the IV portion of the [I|O]KEY deviceparameter's value (substring after the first space) empty.

----------------
CRYPTNOMM
----------------

CRYPTNOMM, ffff is an encrypted database. Cannot support MM access method.

MUPIP Error: This error is triggered by an attempt to mark an MM database as encrypted with GDE or to switch an encrypted database from BG to MM with MUPIP SET. The MM access method is not supported for encrypted databases.

Action: Use the BG access method for encrypted files.

---------------
CRYPTNOOVERRIDE
---------------

CRYPTNOOVERRIDE, Cannot override IVEC and/or key without compromising integrity

Run Time Error: An OPEN or USE command attempted to change the encryption attributes i.e. attempted to enable or disable encryption or change the key name, initialization vector (IV), or both, after a prior encrypted READ or WRITE.

Action: Because encryption algorithms maintain state as they process text, changing the encryption attributes of a device is prohibited if an encrypted READ or WRITE has already occurred, so revise your application code accordingly.

---------------------
CRYPTNOSEEK
---------------------

CRYPTNOSEEK, SEEK disallowed on the encrypted file ffff

Run Time Error: An OPEN or USE command specifies a SEEK deviceparameter on an encryption-enabled device.

Action: Because encryption algorithms maintain state as they process text, SEEKs are prohibited with encrypted devices, so revise your application code accordingly.

-----------------
CRYPTNOTRUNC
-----------------

CRYPTNOTRUNC, Not positioned at file start or EOF. TRUNCATE disallowed on the encrypted file ffff

Run Time Error: An OPEN or USE command specifies a TRUNCATE deviceparameter on a encryption-enabled device which is not positioned at the end of a file.

Action: When using encryption, because encryption algorithms maintain state as they process text, a TRUNCATE is only permitted at the beginning or end of a file, the former deleting the entire contents, and the latter effectively a no-op.

-------------------
CRYPTNOV4
-------------------

CRYPTNOV4, ffff is an encrypted database. Cannot downgrade with Encryption option enabled

MUPIP Error: An attempt to downgrade ffff which is an encrypted database to the previous format failed because it does not support encrypted database files.

Action: Use the database in the current format. If an older format is required, extract the data in unencrypted ZWRite format with MUPIP EXTRACT and load it into a newly created database.

--------------------
CRYPTOPFAILED
--------------------

CRYPTOPFAILED, Encrypt/Decrypt operation failed for file ffff. eeee

Run Time Error: gtmcrypt plug-in reports that there is a problem with encryption or decryption.

Action: Examine the message (eeee) from the plug-in and take appropriate action.

------------------
CTLMNEMAXLEN
------------------

CTLMNEMAXLEN, The maximum length of a control mnemonic has been exceeded

Run Time Error: This indicates that YottaDB encountered a control mnemonic that exceeds the supported maximum length.

Action: Modify the control mnemonic so that it does not exceed the permitted length.

-----------------
CTLMNEXPECTED
-----------------

CTLMNEXPECTED, Control mnemonic is expected in this context

Run Time Error: This indicates that YottaDB requires a control mnemonic in this context.

Action: Modify the spelling of the control mnemonic. Refer to the `Input Output Processing chapter in the Programmer's Guide <../ProgrammersGuide/ioproc.html>`_.

-----------------
CTRAP
-----------------

CTRAP, Character trap $C(xxxx) encountered

Run Time Error: This indicates that the current device encountered character xxxx in its input stream while xxxx was defined as an exception by a CTRAP deviceparameter.

Action: Determine why this character was defined as an error or why an EXCEPTION string was not defined to address it.

-------------------
CTRLC
-------------------

CTRLC, CTRL_C encountered

Run Time Information: This indicates that the principal device encountered a <CTRL>-C in its input stream and YottaDB put the process into Direct Mode.

Action: YottaDB compiles and executes all commands entered at the Direct Mode prompt as they are entered. To continue program execution, enter the ZCONTINUE command. Response to <CTRL>-C is controlled with the [NO]CENABLE and CTRAP = deviceparameters.

-----------------
CTRLY
-----------------

CTRLY, User interrupt encountered

Run Time Information: This indicates that the principal device encountered a <CTRL>-Y in its input stream. The normal CLI is DCL.

Action: You can resume operation with a CONTINUE command if your actions do not invoke other images.

---------------
CURRSOCKOFR
---------------

CURRSOCKOFR, Current socket of index xxxx is out of range. There are only yyyy sockets.

Run Time Error: This indicates that an OPEN, USE, READ or WRITE attempted to select a socket outside the range of available sockets.

Action: Review the socket management logic and revise to only use available sockets.

--------------
CUSTERRNOTFND
--------------

CUSTERRNOTFND, Error mnemonic eeee specified in custom errors file is not valid for this version of YottaDB

Run Time Error: This error indicates that the YottaDB runtime did not recognize the error mnemonic eeee in the file referenced by $ydb_custom_errors.

Action: Modify the file so that it no longer contains the invalid mnemonic or set the ydb_custom_errors environment variable to point to an appropriate file.

---------------
CUSTERRSYNTAX
---------------

CUSTERRSYNTAX, Syntax error in file ffff at line number nnnn

Run Time Error: This error indicates that the custom errors file ffff contains an inappropriate syntax on line nnnn.

Action: Modify the file ffff so that it contains a single valid error mnemonic on line nnnn or set the ydb_custom_errors environment variable to point to an appropriate file.

---------------
CUSTOMFILOPERR
---------------

CUSTOMFILOPERR, Error while doing oooo operation on file ffff

Run Time Error: This indicates that the operating system reported an error while performing operation oooo on custom errors file ffff.

Action: Check that ffff is a proper path to the custom errors file. If it is incorrect, set the ydb_custom_errors environment variable to point to the correct file. If the file path is correct, verify that the user has access to the file and correct any permission issues.

-----------------
DBADDRALIGN
-----------------

DBADDRALIGN, Database file xxxx, element location aaaa: blk = bbbb: [yyyy] control cccc was unaligned relative to base dddd and element size eeee

Run Time Information: This indicates that a control structure in the database cache is damaged.

Action: None needed. YottaDB fixes this error as part of cache recovery, which follows cache verification. If this message shows up frequently or is reproducible, contact your YottaDB support channel.

------------------
DBADDRANGE
------------------

DBADDRANGE, Database file xxxx, element location yyyy: control zzzz was outside aaaa range bbbb to cccc

Run Time Information: This indicates that a process was abnormally terminated. Database control structures may be damaged.

Action: YottaDB often fixes this error unless there is a serious problem causing this error. If there is a serious problem, the accompanying messages identify the cause.

--------------------
DBADDRANGE8
--------------------

DBADDRANGE8, Database file xxxx, element location yyyy: control zzzz was outside aaaa range bbbb to cccc.

Run Time Error: This message is the same as a DBADDRANGE message except that bbb8 and ccc8 are 8-byte quantities (as opposed to 4-byte quantitites in DBADDRANGE).

Action: YottaDB often fixes this error unless there is a serious problem causing this error. If there is a serious problem, the accompanying messages identify the cause.

----------------------
DBBADFREEBLKCTR
----------------------

DBBADFREEBLKCTR, Database xxxx free blocks counter in file header: oooo appears incorrect; should be nnnn. Auto-corrected.

Run Time Warning: This indicates that during a file extension, because they differed, YottaDB adjusted the free blocks counter (oooo) in the file header to agree with free blocks indicated by the master map (nnnn). Because this may indicate a master bitmap integrity error (DBMBPINCFL), check the next MUPIP INTEG carefully.

Action: Run MUPIP INTEG; if it reports a DBMBPINCFL integrity error, use DSE to correct it, and to increase the file header free blocks counter by the amount YottaDB reduced it in the DBBADFREEBLKCTR message. Run an additional INTEG to confirm the corrections.

----------------------
DBBADKYNM
----------------------

DBBADKYNM, xxxx is an invalid key name

MUPIP Error: This is a MUPIP INTEG error. Refer to the topic `MUPIP INTEG Errors in the About This Manual section <./about.html#mupip-integ-errors>`_.

Action: -

-------------------------
DBBADNSUB
-------------------------

DBBADNSUB, xxxx Bad numeric subscript

MUPIP Error: This is a MUPIP INTEG error. Refer to the topic `MUPIP INTEG Errors in the About This Manual section <./about.html#mupip-integ-errors>`_.

Action: -

-------------------------
DBBADPNTR
-------------------------

DBBADPNTR, xxxx Bad pointer value in directory

MUPIP Error: This is a MUPIP INTEG error. Refer to the topic `MUPIP INTEG Errors in the About This Manual section <./about.html#mupip-integ-errors>`_.

Action: -

-----------------------
DBBADUPGRDSTATE
-----------------------

DBBADUPGRDSTATE, Correcting conflicting values for fields describing database version upgrade state in the file header for region rrrr (ffff) - make fresh backups with new journal files immediately.

Run Time Warning: This warning message in the operator log indicates region rrrr (file ffff) had an out-of-design combination of database upgrade conditions, which may have caused defective journal files and -online BACKUPs. YottaDB automatically corrects this condition, but you should investigate the possible causes for such file header damage.

Action: Make fresh backups with new journal files immediately.

-----------------------
DBBDBALLOC
-----------------------

DBBDBALLOC, xxxx Block doubly allocated

MUPIP Error: This is a MUPIP INTEG error. Refer to the topic `MUPIP INTEG Errors in the About This Manual section <./about.html#mupip-integ-errors>`_.

Action: -

------------------------
DBBFSTAT
------------------------

DBBFSTAT, xxxx Block busy/free status unknown (local bitmap corrupted)

MUPIP Error: This is a MUPIP INTEG error. Refer to the topic `MUPIP INTEG Errors in the About This Manual section <./about.html#mupip-integ-errors>`_.

Action: -

-------------------------
DBBLEVMN
-------------------------

DBBLEVMN, xxxx Block level less than zero

Run Time Information: This indicates that a database operation failed. The level specified for block xxxx is less than zero.

Action: Report this database structure error to the group responsible for database integrity at your operation.

-----------------
DBBLEVMX
-----------------

DBBLEVMX, xxxx Block level higher than maximum

Run Time Information: This indicates that a database operation failed. The level specified for block xxxx exceeds the maximum allowed during a block certification or a DSE integrity check.

Action: Report this database structure error to the group responsible for database integrity at your operation.

---------------
DBBLKSIZEALIGN
---------------

DBBLKSIZEALIGN, Database file ffff has AIO=ON and block_size=bbbb which is not a multiple of filesystem block size ssss

Run Time Error: This indicates that database file ffff has ASYNCIO enabled but has a block size bbbb that does not align with a multiple of the block size ssss supported by the current file system.

Action: Use a file system with an appropriate block size for the database file. Alternatively, find a compatible file system (perhaps the file system that previously held the database) for the database file and move the data using replication, MERGE, or MUPIP EXTRACT and LOAD to a database with appropriate block size for the target file system.

----------------
DBBMBARE
----------------

DBBMBARE, xxxx Bit map does not protect itself

DSE/Run Time Information: This indicates that a bitmap error was encountered during a block certification or a DSE integrity check.

Action: Report this database structure error to the group responsible for database integrity at your operation.

-----------------
DBBMINV
-----------------

DBBMINV, xxxx Bit map contains an invalid pattern

DSE/Run Time Information: This indicates that a bitmap error was encountered during a block certification or a DSE integrity check.

Action: Report this database structure error to the group responsible for database integrity at your operation.

------------------
DBBMLCORRUPT
------------------

DBBMLCORRUPT, Database xxxx: Bitmap blk yyyy is corrupt (Size = aaaa, levl = bbbb, tn = cccc: Dbtn = dddd): Database integrity errors likely

Run Time Error: This indicates that a local bitmap block was found corrupted in the global buffers. In the event that the message is followed by a GTMASSERT in the operator log, a dump-file/core may also be produced.

Action: MUPIP RUNDOWN the indicated database and check for integrity errors and bitmap-related errors and fix them before resuming operations on the database. Report to your YottaDB support channel with the operator log information and dump- cores/files, if any.

--------------------
DBBMMSTR
--------------------

DBBMMSTR, xxxx Bit map does not match master map

Run Time Information: This indicates that a bitmap error was encountered during a block certification or a DSE integrity check.

Action: Report this database structure error to the group responsible for database integrity at your operation.

--------------------
DBBMSIZE
--------------------

DBBMSIZE, xxxx Bit map has incorrect size

DSE/Run Time Information: This indicates that a bitmap error was encountered during a block certification or a DSE integrity check.

Action: Report this database structure error to the group responsible for database integrity at your operation.

--------------------
DBBNPNTR
--------------------

DBBNPNTR, Bit map block number as pointer

MUPIP Error: This is a MUPIP INTEG error. Refer to the topic `MUPIP INTEG Errors in the About This Manual section <./about.html#mupip-integ-errors>`_.

Action: -

----------------------
DBBPLMGT2K
----------------------

DBBPLMGT2K, Blocks per local map is greater than 2k

MUPIP Error: This is a MUPIP INTEG error. Refer to the topic `MUPIP INTEG Errors in the About This Manual section <./about.html#mupip-integ-errors>`_.

Action: -

------------------------
DBBPLMLT512
------------------------

DBBPLMLT512, Blocks per local map is less than 512

MUPIP Error: This is a MUPIP INTEG error. Refer to the topic `MUPIP INTEG Errors in the About This Manual section <./about.html#mupip-integ-errors>`_.

Action: -

-----------------------
DBBPLNOT512
-----------------------

DBBPLNOT512, Blocks per local map is not 512

MUPIP Error: This is a MUPIP INTEG error. Refer to the topic `MUPIP INTEG Errors in the About This Manual section <./about.html#mupip-integ-errors>`_.

Action: -


----------------------
DBBSIZMN
----------------------

DBBSIZMN, xxxx Block too small

Run Time Warning: This indicates that during block certification or DSE/MUPIP Integrity Check, the size of block xxxx was found to be less than the minimum allowed.

Action: Report this database structure error to the group responsible for database integrity at your operation.

--------------------
DBBSIZMX
--------------------

DBBSIZMX, xxxx Block larger than file block size

Run Time Warning: This indicates that during block certification or DSE/MUPIP Integrity Check, the size of block xxxx was found to exceed the block size for the database region.

Action: Report this database structure error to the group responsible for database integrity at your operation.

------------------
DBBSIZZRO
------------------

DBBSIZZRO, Block size equals zero

MUPIP Error: This is a MUPIP INTEG error. Refer to the topic `MUPIP INTEG Errors in the About This Manual section <./about.html#mupip-integ-errors>`_.

Action: -

-------------------
DBBTUFIXED
-------------------

DBBTUFIXED, The blocks-to-upgrade file-header field has been changed to the correct value

MUPIP Information: MUPIP INTEG has corrected the blocks-to-upgrade field.

Action: Report this to the group responsible for database integrity at your operation.

-------------------
DBBTUWRNG
-------------------

DBBTUWRNG, The blocks-to-upgrade file-header field is incorrect. Expected xxxx, found yyyy

MUPIP Error: The "Blocks to Upgrade" counter was found to be incorrect by MUPIP INTEG (this is only checked for non-FAST integs).

Action: If there are no other integrity errors, MUPIP INTEG will repair the counter. If there are other integrity errors, fix those errors first, then rerun MUPIP INTEG which will repair the counter if it is still found to be in error. Although this error is not indicative of any specific kind of database damage it does represent an out-of-design condition (except following a system crash in which before-image journaling was not in use) that your YottaDB support channel would like to know about.

----------------------
DBCBADFILE
----------------------

DBCBADFILE, Source file xxx does not appear to have been generated by DBCERTIFY SCAN - rerun SCAN or specify correct file

DBCERTIFY Error: V5CBSU and DBCERTIFY CERTIFY require the output file from DBCERTIFY SCAN. The file which was specified is not in the correct format.

Action: Specify the file created by DBCERTIFY SCAN. Rerun DBCERTIFY SCAN if needed.

-------------------
DBCCERR
-------------------

DBCCERR, Interlock instruction failure in critical mechanism for region xxxx

Run Time Error: This indicates that an interlocked operation for the specified region failed.

Action: Report this database concurrency error to the group responsible for database integrity at your operation.


---------------------
DBCCMDFAIL
---------------------

DBCCMDFAIL, Executed command failed with return code xxxx yyyy which executed yyyy yyyy

DBCERTIFY Error: During processing, the DBCERTIFY attempts to execute certain DSE and/or MUPIP commands in temporary command scripts that DBCERTIFY creates. The specified command failed to execute.

Action: The action to take depends on the code returned by the attempt and if any associated messages were created on either the console or the operator log. Some common causes of problems could be that $ydb_dist is not properly pointing to the current YottaDB version or that DBCERTIFY has no access or access to the wrong global directory for which it is executing commands.

---------------------------
DBCDBCERTIFIED
---------------------------

DBCDBCERTIFIED, Database xxx has been certified for use with xxxx

DBCERTIFY Information: DBCERTIFY CERTIFY has successfully completed and marked the database as certified for use by the specified YottaDB version.

Action: Either keep running the current YottaDB version or proceed immediately to YottaDB MUPIP UPGRADE at the user's discretion.

---------------------
DBCDBNOCERTIFY
---------------------

DBCDBNOCERTIFY, Database xxxx HAS NOT been certified due to the preceding errors - rerun DBCERTIFY SCAN

MUPIP Error: MUPIP UPGRADE triggers this error if it finds the DBCERTIFY CERTIFY command has not run to completion on database xxx.

Action: Complete the scan phase of DBCERTIFY by executing the DBCERTIFY SCAN command.

------------------
DBCINTEGERR
------------------

DBCINTEGERR, Encountered integrity error in database xxxx

DBCERTIFY Error: DBCERTIFY discovered what appears to be an integrity error while processing the specified database. This error is accompanied by a secondary message giving an explanation of what the error is.

Action: Run a MUPIP INTEG (not FAST integ) on the database in question; fix damage, then re-run the phase reporting the error. If the integrity error persists, contact your YottaDB support channel.

---------------
DBCKILLIP
---------------

DBCKILLIP, Cannot proceed with kill-in-progress indicator set for database xxx

DBCERTIFY Error: DBCERTIFY discovered that the kill in progress indicator was on for the specified database. DBCERTIFY will not process a database with this indicator on.

Action: Run a MUPIP INTEG (FAST integ is OK) on the database in question; correct errors, then re-run the phase reporting the error. If the error persists, contact your YottaDB support channel.

----------------
DBCLNUPINFO
----------------

DBCLNUPINFO, Database file xxxx / yyyy

Run Time Information: When a process that holds the critical section lock on one or more databases gets abnormally terminated, it dumps information pertaining to its current state into the global sections for each of the concerned databases. The next process that references the concerned database notices the previous abnormal termination and uses the dumped information to update the global buffers and takes the database to a safe and consistent state. During this transition, the process displays a subset of the dumped information in the operator log to be used for debugging purposes by your YottaDB support channel, in case database integrity errors are experienced later.

Action: The message text describes the cause of this error. Report any database structure errors to the group responsible for database integrity at your operation.

------------------
DBCMODBLK2BIG
------------------

DBCMODBLK2BIG, Block 0xaaa has been modified since DCERTIFY SCAN but is still too large or has an earlier TN than in DCERTIFY SCAN - Rerun scan

DBCERTIFY Error: DBCERTIFY reports this error when the block it is processing has a different TN than it did in the scan phase, yet the block is still too large.

Action: This condition indicates that something has been done to the database since the scan phase was run - either it was restored from an earlier backup or the reserved bytes value was (even temporarily) reduced. DBCERTIFY SCAN must be rerun.

------------------
DBCMPBAD
------------------

DBCMPBAD, xxxx yyyy Compression count not maximal

Run Time Warning: This indicates that a database operation failed because block xxxx contains a record at offset yyyy with a compression count that is too low.

Action: Report this database structure error to the group responsible for database integrity at your operation.

------------------
DBCMPMX
------------------

DBCMPMX, xxxx yyyy Record compression count is too large

Run Time Warning: This indicates that during block certification or DSE/MUPIP integrity check, the block xxxx was found to contain a record at offset yyyy that exceeds the compression count.

Action: Report this database structure error to the group responsible for database integrity at your operation.

-----------------
DBCMPNZRO
-----------------

DBCMPNZRO, xxxx yyyy First record of block has nonzero compression count

Run Time Warning: This indicates that during block certification or DSE/MUPIP integrity check, the first record at offset yyyy of block xxxx was found to have a nonzero compression count.

Action: Report this database structure error to the group responsible for database integrity at your operation.

-------------------
DBCNOEXTND
-------------------

DBCNOEXTND, Unable to extend database xxx

DBCERTIFY Error: DBCERTIFY attempted to use MUPIP EXTEND to extend the database but the attempt failed.

Action: Examine the accompanying messages from the MUPIP EXTEND attempt to see why the extend failed. A common cause for this is that $ydb_dist did not properly point to the currently installed distribution, or there was insufficient disk space to perform the expansion.

------------------
DBCNOFINISH
------------------

DBCNOFINISH, DBCERTIFY unable to finish all requested actions

DBCERTIFY Error: This indicates DBCERTIFY encountered an error, which prevented the requested action from completing. The action has partially completed.

Action: Review the accompanying message(s) for additional information to identify the cause.

------------------
DBCNOTSAMEDB
------------------

DBCNOTSAMEDB, Database has been moved or restored since DBCERTIFY SCAN - Rerun scan

DBCERTIFY Error: DBCERTIFY has noted that the unique database identifiers for the database have changed since DBCERTIFY SCAN was run.

Action: The database is required to have not been moved around or restored or recovered since DBCERTIFY SCAN was run. DBCERTIFY SCAN must be rerun.

-------------
DBCNTRLERR
-------------

DBCNTRLERR, Database file xxxx: control error suspected but not found

Run Time Error: This indicates that YottaDB detected the possibility of damage to database cache structures and performed a cache verification and rebuild, but found no evidence of damage.

Action: Verify that there are no locked or runaway processes. Check disk loads for evidence of resource constraints.

------------------
DBCOLLREQ
------------------

DBCOLLREQ, JOURNAL EXTRACT proceeding without collation information for globals in database. eeee ffff .

MUPIP Warning: This is MUPIP JOURNAL EXTRACT Warning. This indicates that the MUPIP process uses the default collation, as it is not able to read the database file ffff because of error eeee

Action: Be aware that if the EXTRACT contains variables with alternative collation, that this extract represents them as YottaDB stores them, rather than as they are used by the application. Attempting to LOAD such an EXTRACT will produce incorrect results.

----------------
DBCOMMITCLNUP
----------------

DBCOMMITCLNUP, Pid dddd [hhhh] handled error (code = eeee) during commit of xxxx transaction in database file yyyy

Success Information: This message is output to the operator log and indicates that there was an error in the midst of committing an xxxx (TP or non-TP) transaction that involved the database file yyyy, but the process (pid = dddd in decimal and hhhh in hexadecimal) handled the error and completed the commit. If non-zero, the error code eeee is what triggered the error in the first place. If zero, accompanying syslog messages will contain information on the cause.

Action: In most cases the commit will be successfully completed. But in very rare cases, there might be errors that prevent the transaction from completing successfully. To determine if there was an error, examine the following operator log messages. If there are more then 3 (three) DBCLNUPINFO messages for the same database file from the same process-id, then that particular database is suspect and an integrity check of that database needs to be done at the earliest. In addition, contact your YottaDB support channel with the operator log messages.

-----------------
DBCOMPTOOLRG
-----------------

DBCOMPTOOLRG, xxxx Record has too large compression count

MUPIP Error: This is a MUPIP INTEG error. Refer to the topic `MUPIP INTEG Errors in the About This Manual section <./about.html#mupip-integ-errors>`_.

Action: -

--------------
DBCREC2BIG
--------------

DBCREC2BIG, Record with key xxx is length yyy in block 0xaaa is greater than the maximum length yyy in database xxx

DBCERTIFY Error: DBCERTIFY has identified a record with the given key in the given block with a length that exceeds the maximum length allowed in the given database.

Action: This is typically due to the user reducing the maximum record length to meet the DBCERTIFY requirements but not verifying that no records exist that exceed that length. The solution is to either delete or otherwise restructure the record or to MUPIP extract/load into a database with a larger blocksize.

----------------
DBCREC2BIGINBLK
----------------

DBCREC2BIGINBLK, A Record in block bbbb has a length greater than the maximum uuuuu in database dddd.

MUPIP Error: MUPIP UPGRADE triggers this error when the size of a record in block bbbb exceeds the maximum record size of uuuu in database region dddd.

Action: -


----------------
DBCREINCOMP
----------------

DBCREINCOMP, xxxx Header indicates database file creation was interrupted before completion

Run Time/MUPIP Error: This is either a MUPIP Integ error, or this indicates that a database operation tried to activate a database file that was improperly initialized.

Action: Delete the damaged file and use MUPIP CREATE to recreate the database. Refer to `'MUPIP INTEG Error Messages' table in Chapter 11 - Maintaining Database Integrity of the Administration and Operations Guide <../AdminOpsGuide/integrity.html>`_.


----------------
DBCRERR
----------------

DBCRERR, Database file xxxx, cr location yyyy blk = zzzz error: aaaa was bbbb, expecting cccc -- called from module xxx at line yyy

Run Time Error: This usually indicates that a process was abnormally terminated and left database control structures in an inconsistent state in shared memory.

Action: YottaDB often fixes this error unless there is a more serious problem causing this error. If there is a more serious problem, accompanying messages identify the cause.

------------------
DBCRERR8
------------------

DBCRERR8, Database file xxxx, or location yyyy blk = zzzz error: aaaa was bbbb, expecting cccc -- called from module yyy at line xxx

Run Time Error: This message is the same as a DBCRERR message except that bbbb and cccc are 8-byte quantities (as opposed to 4-byte quantitites in DBCRERR). See Error description for message DBCRERR above.

Action: YottaDB often fixes this error unless there is a more serious problem causing this error. If there is a more serious problem, accompanying messages identify the cause.

-------------------
DBCSCNNOTCMPLT
-------------------

DBCSCNNOTCMPLT, Specified DBCERTIFY SCAN output file is not complete - Rerun scan

DBCERTIFY Error: DBCERTIFY CERTIFY has noted that the header of the scan phase output is not filled in, indicating that the scan phase did not complete normally.

Action: Rerun DBCERTIFY SCAN to produce a complete output file for the certify phase to process.

-------------------
DBDANGER
-------------------

DBDANGER, Process pppp killed while committing update for database file xxxx. Possibility of damage to block yyyy.

Run Time Warning: This message is issued when a recovery of the database global buffer cache structures needs to be performed. It might discover that the cache recovery was necessary because of a YottaDB process being killed (kill-9) while in the process of committing a change to the database. The cache recovery routine issues this message while proceeding with the recovery.

Action: This is a warning type message indicating possible database corruption due to process kills (kill-9). A database integrity check is recommended. Make sure that kill-9 or STOP/ID is not used to stop any YottaDB processes.

-------------------
DBDATAMX
-------------------

DBDATAMX, xxxx Record too large

MUPIP Error: This is a MUPIP INTEG error. Refer to the `MUPIP INTEG Errors section <./about.html#mupip-integ-errors>`_.

Action: N/A

-------------------
DBDIRTSUBSC
-------------------

DBDIRTSUBSC, xxxx Directory tree block contains non name-level entries

DSE/Run Time Information: This indicates that the specified database block has internal structural damage, since it contains subscripts and global variable names even though this block is part of the directory tree.

Action: Report this database structure error to the group responsible for database integrity at your operation.

------------------
DBDSRDFMTCHNG
------------------

DBDSRDFMTCHNG, Database file xxx, Desired DB Format set to yyy by zzz with pid ppp [0xppp] at transaction number [0xttt]

MUPIP Information: The desired database block format has been changed to version yyy for database file xxx by the zzz command with process number ppp at transaction number ttt.

Action: N/A

--------------------
DBDUPNULCOL
--------------------

DBDUPNULCOL, Discarding kkkk=vvvv key due to duplicate null collation record

MUPIP Error: This indicates that MUPIP LOAD discarded a key-value pair from a binary EXTRACT because it contained conflicting empty string subscripts. This can only happen if someone changes the "Null" subscript representation used by a database while it contains such subscripts. YottaDB recommends against such a change.

Action: Determine whether the described data has value and restore it, typically with a SET command, appropriately.

--------------------
DBENDIAN
--------------------

DBENDIAN, Database file xxxx is aaaa endian on a gggg endian system

Run Time/MUPIP Error: This indicates that the database file being opened is in the wrong endian format for the current system. This usually means that the file was copied from another system with the opposite endian format.

Action: To use the database file on the current system, change the endian format using the MUPIP ENDIANCVT command.


--------------------
DBFGTBC
--------------------

DBFGTBC, xxxx File size larger than block count would indicate

MUPIP Error: This is a MUPIP INTEG error. Refer to the topic `MUPIP INTEG Errors in the About This Manual section <./about.html#mupip-integ-errors>`_.

Action: -

-------------------------
DBFHEADERR4
-------------------------

DBFHEADERR4, Database file ffff: control problem: aaaa was xxxx expecting yyyy

Run Time Error: This indicates that database cache recovery was triggered due to some abnormal event, and the recovery routine detected damage to the control structures in the database global buffer cache.

Action: The system automatically attempts to correct the problem. If this error continues to occur, attempt MUPIP RUNDOWN and if that fails too, restore database from backup and replay journal files. Report the entire incident context to your YottaDB support channel if necessary.

----------------------
DBFHEADERR8
----------------------

DBFHEADERR8, Database file ffff: control problem: aaaa was xxxx expecting yyyy

Run Time Error: This indicates that database cache recovery was triggered due to some abnormal event, and the recovery routine detected damage to the control structures in the database global buffer cache.

Action: The system automatically attempts to correct the problem. If this error continues to occur, attempt MUPIP RUNDOWN and if that fails too, restore database from backup and replay journal files. Report the entire incident context to your YottaDB support channel if necessary.


---------------------
DBFHEADERRANY
---------------------

DBFHEADERRANY, Database file ffff: control problem: aaaa was xxxx expecting yyyy

Run Time Information: This indicates that database cache recovery was triggered due to an abnormal event and the recovery routine detected damage to the control structures in the database global buffer cache or the file header.

Action: The system automatically attempts to correct the problem. If this error continues to occur, attempt MUPIP RUNDOWN and if that fails too, restore database from backup and replay journal files. If necessary, report the entire incident context to your YottaDB support channel.


------------------
DBFHEADLRU
------------------

DBFHEADLRU, Database file ffff LRU pointer: pppp is outside of range: bbbb to tttt or misaligned

All YottaDB Components Error: An element used to manage global buffers is invalid, so YottaDB has reset it; this message should appear only if there is a hardware issue or an abnormal termination.

Action: Not required, but YottaDB recommends an investigation to identify a possible cause.


-------------------
DBFILECREATED
-------------------

DBFILECREATED, Database file DDDD created

Run Time/MUPIP Error: Indicates YottaDB successfully created the database file DDDD.

Action: None required.

--------------
DBFILERDONLY
--------------

DBFILERDONLY, The database file ffff was opened as read-only (perms pppp)

All YottaDB Components Error: Database file ffff was opened read-only with permissions pppp, but the read-only status is inconsistent with application expectations.

Action: Use the error and any follow-on messages to assess whether or not the read-only status is correct or the rejection is appropriate.

DBFILERDONLY was added to YottaDB effective release r1.36.

---------------------
DBFILERR
---------------------

DBFILERR, Error with database file. xxxx.

Run Time Error: This indicates that an I/O operation on the database file encountered an error.

Action: Review the accompanying message(s) for a detailed error status.

---------------
DBFILEXT
---------------

DBFILEXT, Database file xxxx extended from yyyy blocks to zzzz blocks at transaction aaaa

Run Time/MUPIP Information: This operator log message indicates that the specified database file was extended as described by the message.

Action: -

----------------
DBFILNOFULLWRT
----------------

DBFILNOFULLWRT, Disabling fullblock writes. iiii tttt: bbbb

MUPIP Warning: Indicates full block writes were not successfully enabled. iiii describes the issue, tttt describes the type and bbbb is a block size.

Action: Consider planning to choose a blocksize better aligned with the file system blocksize at the next opportunity.

DBFILNOFULLWRT was added to YottaDB effective release r1.36.

----------------------
DBFILOPERR
----------------------

DBFILOPERR, Error doing database I/O to database file xxxx

Run Time Error: This indicates that the database manager portion of the run-time system encountered an error when it attempted to open, read, write, or close a database file.

Action: Report this error to the group responsible for database integrity at your operation. Review the accompanying message(s) for additional information and analyze the system error log.

------------------
DBFLCORRP
------------------

DBFLCORRP, xxxx Header indicates database file is corrupt

Run Time/MUPIP Error: This indicates that a database operation tried to activate database file xxxx, which was previously marked as damaged.

Action: If ROLLBACK (either -NOONLINE or -ONLINE) terminates abnormally (say because of a kill -9), it leaves the database in a potentially inconsistent state indicated by the FILE corrupt field in the database file header. When an ROLLBACK terminates leaving this field set, all other processes receive DBFLCORRP errors any time they attempt to interact with the database. The best way to clear DBFLCORRP is by running another ROLLBACK. MUPIP SET -FILE -PARTIAL_RECOV_BYPASS and DSE CHANGE -FILE -CORRUPT=FALSE -NOCRIT can also clear this condition, but these commands do not ensure that the database has a consistent state, so you should always run MUPIP INTEG after executing these commands.

------------------
DBFREEZEOFF
------------------

DBFREEZEOFF, Database file ffff is UNFROZEN ([NO]OVERRIDE [NO]AUTOREL)

Operator log/MUPIP Information: The database file ffff is no longer frozen, most likely due to a MUPIP FREEZE -OFF, with the selected options. [NO]AUTOREL indicates whether an autorelease of the region occurred prior to the MUPIP FREEZE -OFF command.

Action: Confirm that this was the desired action.

------------------
DBFREEZEON
------------------

DBFREEZEON, Database file ffff is FROZEN ([NO]OVERRIDE [NO]ONLINE [NO]AUTOREL)

Operator log/MUPIP Information: The database file ffff is frozen, most likely due to a MUPIP FREEZE -ON, with the reported options.

Action: Confirm that this was the desired action.

-------------
DBFRZRESETFL
-------------

DBFRZRESETFL, Freeze release failed on database file xxxx

MUPIP Error: This indicates that MUPIP failed to release the freeze on database file xxxx.

Action: Review the accompanying message(s) for additional information. Analyze DSE DUMP /FILE /ALL output.

-------------
DBFRZRESETSUC
-------------

DBFRZRESETSUC, Unfreeze successfully done on database file xxxx.

MUPIP Information: This indicates that RECOVER encountered a database file that had a state of FREEZE, and released the FREEZE.

Action: -

-------------
DBFSTBC
-------------

DBFSTBC, xxxx File size smaller than block count would indicate

MUPIP Error: This is a MUPIP INTEG error. Refer to the topic `MUPIP INTEG Errors in the About This Manual section <./about.html#mupip-integ-errors>`_.

Action: -

------------
DBFSTHEAD
------------

DBFSTHEAD, xxxx File smaller than database header

MUPIP Error: This is a MUPIP INTEG error. Refer to the topic `MUPIP INTEG Errors in the About This Manual section <./about.html#mupip-integ-errors>`_.

Action: -


-----------
DBFSYNCERR
-----------

DBFSYNCERR, Error synchronizing database file xxxx to disk

Run Time Error: While using before-image journaling, the database is hardened to disk every time YottaDB writes an epoch-record. If this operation returns an error, then the DBFSYNCERR error is issued to the user accompanied by system information about the cause of the system service error.

Action: Trouble shoot the file system, on which the database file resides, for issues related to FSYNC(). Report the entire incident context to your YottaDB support channel along with any operator log messages within the same time frame.


------------
DBGTDBMAX
------------

DBGTDBMAX, xxxx Key larger than database maximum

MUPIP Error: This is a MUPIP INTEG error. Refer to the topic `MUPIP INTEG Errors in the About This Manual section <./about.html#mupip-integ-errors>`_.

Action: -


---------------
DBHEADINV
---------------

DBHEADINV, xxxx Header size not valid for database

MUPIP Error: This is a MUPIP INTEG error. Refer to the topic `MUPIP INTEG Errors in the About This Manual section <./about.html#mupip-integ-errors>`_.

Action: -


-------------
DBIDMISMATCH
-------------

DBIDMISMATCH, Database file xxxx ID (region yyyy) does not match file ID in shared memory (ID=zzzz). Ensure region is properly rundown.

Run Time Error: When a YottaDB process attaches to a database and finds the corresponding shared memory structures initialized already, it performs integrity checks on the shared memory contents to ensure that they correspond to the database file. When the shared memory copy of the database file ID does not match with the actual file ID of the database, the above error is issued.

Action: Perform a MUPIP RUNDOWN on that region. If it fails with the same DBIDMISMATCH error, then the shared memory contents are corrupt. Consult your YottaDB support channel before proceeding further.

------------
DBINCLVL
------------

DBINCLVL, xxxx Block at incorrect level

MUPIP Error: This is a MUPIP INTEG error. Refer to the topic `MUPIP INTEG Errors in the About This Manual section <./about.html#mupip-integ-errors>`_.

Action: -


---------------
DBINCRVER
---------------

DBINCRVER, xxxx Incorrect version of YottaDB database

MUPIP Error: This is a MUPIP INTEG error. Refer to the topic `MUPIP INTEG Errors in the About This Manual section <./about.html#mupip-integ-errors>`_.

Action: -


---------------
DBINVGBL
---------------

DBINVGBL, xxxx Invalid mixing of global names

MUPIP Error: This is a MUPIP INTEG error. Refer to the topic `MUPIP INTEG Errors in the About This Manual section <./about.html#mupip-integ-errors>`_.

Action: -

----------------
DBIOERR
----------------

DBIOERR, Error while doing write operation on region rrrr (ffff)

Run Time Error: This error indicates that the process encountered an I/O error (other than ENOSPC) while trying to write to database file ffff (corresponding to region rrrr).

Action: Examine accompanying messages to identify the cause of the I/O error and take actions to rectify it.

--------------
DBJNLNOTMATCH
--------------

DBJNLNOTMATCH, Database xxxx points to journal file name yyyy but the journal file points to database file zzzz

MUPIP Error: This indicates that there is a mismatch in the name of the database file xxxx and the name zzzz, saved in the journal file header yyyy.

Action: Contact your YottaDB support channel if the cause of the error cannot be diagnosed. If appropriate, change the database file name in the journal file using the command MUPIP SET /jnlfile /dbfilename=xxxx yyyy.

---------------
DBKEYGTIND
---------------

DBKEYGTIND, xxxx Key greater than index key

MUPIP Error: This is a MUPIP INTEG error. Refer to the topic `MUPIP INTEG Errors in the About This Manual section <./about.html#mupip-integ-errors>`_.

Action: -

-------------
DBKEYMN
-------------

DBKEYMN, xxxx Key too short

Run Time Warning: This indicates that a block certification or DSE/MUPIP integrity check failed on block xxxx since it contains a record at offset yyyy with a key that does not meet the minimum size requirement.

Action: Report this database structure error to the group responsible for database integrity at your operation.

----------------
DBKEYMX
----------------

DBKEYMX, xxxx Key too long

Run Time Warning: This indicates that a block certification or DSE/MUPIP integrity check failed on block xxxx since it contains a record at offset yyyy whose key exceeds the allowable size.

Action: Report this database structure error to the group responsible for database integrity at your operation.

-----------------
DBKEYORD
-----------------

DBKEYORD, xxxx Keys out of order

DSE/Run Time/MUPIP Warning: This indicates that a DSE/MUPIP INTEG command determined that the block contains a record at offset yyyy whose key is not in proper M collating sequence. This error is also reported at run-time if block certification fails on a particular block. Block certification is active if GDSCERT is enabled by a VIEW command.

Action: Report this database structure error to the group responsible for database integrity at your operation.

-----------------
DBKGTALLW
-----------------

DBKGTALLW, xxxx Key larger than maximum allowed length

MUPIP Error: This is a MUPIP INTEG error. Refer to the topic `MUPIP INTEG Errors in the About This Manual section <./about.html#mupip-integ-errors>`_.

Action: -

---------------
DBLOCMBINC
---------------

DBLOCMBINC, xxxx Local bit map incorrect

MUPIP Error: This is a MUPIP INTEG error. Refer to the topic `MUPIP INTEG Errors in the About This Manual section <./about.html#mupip-integ-errors>`_.

Action: -

----------------
DBLRCINVSZ
----------------

DBLRCINVSZ, xxxx Last record of block has invalid size

MUPIP Error: This is a MUPIP INTEG error. Refer to the topic `MUPIP INTEG Errors in the About This Manual section <./about.html#mupip-integ-errors>`_.

Action: -

-----------------
DBLTSIBL
-----------------

DBLTSIBL, xxxx Keys less than sibling's index key

MUPIP Error: This is a MUPIP INTEG error. Refer to the topic `MUPIP INTEG Errors in the About This Manual section <./about.html#mupip-integ-errors>`_.

Action: -


------------------
DBLVLINC
------------------

DBLVLINC, xxxx Local bitmap block level incorrect

MUPIP Error: This is a MUPIP INTEG error. Refer to the topic `MUPIP INTEG Errors in the About This Manual section <./about.html#mupip-integ-errors>`_.

Action: -


----------------
DBMAXKEYEXC
----------------

DBMAXKEYEXC, xxxx Maximum key size for database exceeds design maximum

MUPIP Error: This is a MUPIP INTEG error. Refer to the topic `MUPIP INTEG Errors in the About This Manual section <./about.html#mupip-integ-errors>`_.

Action: -

------------
DBMAXNRSUBS
------------

DBMAXNRSUBS, XXXX Maximum number of subscripts exceeded.

Compile Time Error: The subscripted variable required more than 31 subscripts.

Action: Modify the routine to observe this limit on subscripts in a single variable.

------------
DBMAXREC2BIG
------------

DBMAXREC2BIG, Maximum record size (xxx) is too large for this block size (yyy) - Maximum is zzz

DBCERTIFY/MUPIP Error: DBCERTIFY and MUPIP UPGRADE report this error when the maximum record size is too close to the database blocksize and does not allow room for an expanded block header.

Action: Reduce the maximum record size or mupip extract/load into a database with a larger blocksize. Note that if the maximum record size is reduced with DSE, it is possible that records that exceed the reduced size still exist in the database which is now an integrity error. DBCERTIFY SCAN will find these blocks and report on them if they exist.

--------------
DBMBMINCFRE
--------------

DBMBMINCFRE, xxxx Master bit map incorrectly asserts this local map has free space

MUPIP Error: This is a MUPIP INTEG error. Refer to the topic `MUPIP INTEG Errors in the About This Manual section <./about.html#mupip-integ-errors>`_.

Action: -

----------------
DBMBMINCFREFIXED
----------------

DBMBMINCFREFIXED, Master bitmap incorrectly marks local bitmap 0xAAAA as free. Auto-corrected.

Run Time Warning: The above error is issued when the runtime engine detects an integrity error with the master map that indicates that the local bitmap 0xAAAA is free when it is actually not. The error is also auto-corrected by the runtime engine by marking the local bitmap as full in the master bitmap.

Action: This error is entirely benign, but because it should not occur, be sure to check your next MUPIP INTEG output thoroughly and also check your operator logs prior to this warning for other unusual events.

------------------
DBMBPFLDIS
------------------

DBMBPFLDIS, xxxx Master bit map shows this map full, in disagreement with both disk and INTEG results

MUPIP Error: This is a MUPIP INTEG error. Refer to the topic `MUPIP INTEG Errors in the About This Manual section <./about.html#mupip-integ-errors>`_.

Action: -


------------------
DBMBPFLDLBM
------------------

DBMBPFLDLBM, xxxx Master bit map shows this map full, agreeing with disk local map

MUPIP Error: This is a MUPIP INTEG error. Refer to the topic `MUPIP INTEG Errors in the About This Manual section <./about.html#mupip-integ-errors>`_.

Action: -

------------------
DBMBPFLINT
------------------

DBMBPFLINT, xxxx Master bit map shows this map full, agreeing with MUPIP INTEG

MUPIP Error: This is a MUPIP INTEG error. Refer to the topic `MUPIP INTEG Errors in the About This Manual section <./about.html#mupip-integ-errors>`_.

Action: -

-----------------
DBMBPFRDLBM
-----------------

DBMBPFRDLBM, xxxx Master bit map shows this map has space, agreeing with disk local map

MUPIP Error: This is a MUPIP INTEG error. Refer to the topic `MUPIP INTEG Errors in the About This Manual section <./about.html#mupip-integ-errors>`_.

Action: -


----------------
DBMBPFRINT
----------------

DBMBPFRINT, xxxx Master bit map shows this map has space, agreeing with MUPIP INTEG

MUPIP Error: This is a MUPIP INTEG error. Refer to the topic `MUPIP INTEG Errors in the About This Manual section <./about.html#mupip-integ-errors>`_.

Action: -

-----------------
DBMBPINCFL
-----------------

DBMBPINCFL, xxxx Master bit map incorrectly marks this local map full

MUPIP Error: This is a MUPIP INTEG error. Refer to the topic `MUPIP INTEG Errors in the About This Manual section <./about.html#mupip-integ-errors>`_.

Action: -

------------------
DBMBSIZMN
------------------

DBMBSIZMN, xxxx Map block too small

MUPIP Error: This is a MUPIP INTEG error. Refer to the topic `MUPIP INTEG Errors in the About This Manual section <./about.html#mupip-integ-errors>`_.

Action: -

-----------------
DBMBSIZMX
-----------------

DBMBSIZMX, xxxx Map block too large

MUPIP Error: This is a MUPIP INTEG error. Refer to the topic `MUPIP INTEG Errors in the About This Manual section <./about.html#mupip-integ-errors>`_.

Action: -

--------------------
DBMBTNSIZMX
--------------------

DBMBTNSIZMX, xxxx Map block transaction number too large

MUPIP Error: This is a MUPIP INTEG error. Refer to the topic `MUPIP INTEG Errors in the About This Manual section <./about.html#mupip-integ-errors>`_.

Action: -

-------------------
DBMINRESBYTES
-------------------

DBMINRESBYTES, Minimum RESERVED BYTES value required for certification/upgrade is xxx - Currently is yyy

DBCERTIFY/MUPIP Error: DBCERTIFY and MUPIP UPGRADE report this error when the reserved bytes field of the database file header (as shown by DSE DUMP -FILEHEADER) is not at a sufficient value for the YottaDB upgrade.

Action: Increase the reserved bytes value with either MUPIP or DSE so that the value is at least 8 bytes. Note that the reserved bytes value is reduced by the above amounts by MUPIP UPGRADE.

------------------
DBMISALIGN
------------------

DBMISALIGN, Database file xxxx has yyyy blocks which does not match alignment rules. Reconstruct the database from a backup or extend it by at least zzzz blocks.

MUPIP Error: This is an auxiliary message, and is preceded by a primary message.

Action: Follow the primary message description and action as specified in this manual.


------------------
DBMRKBUSY
------------------

DBMRKBUSY, xxxx Block incorrectly marked busy

MUPIP Error: This is a MUPIP INTEG error. Refer to the topic `MUPIP INTEG Errors in the About This Manual section <./about.html#mupip-integ-errors>`_.

Action: -


------------------
DBMRKFREE
------------------

DBMRKFREE, xxxx Block incorrectly marked free

MUPIP Error: This is a MUPIP INTEG error. Refer to the topic `MUPIP INTEG Errors in the About This Manual section <./about.html#mupip-integ-errors>`_.

Action: -

-----------------
DBMXRSEXCMIN
-----------------

DBMXRSEXCMIN, xxxx Maximum record size for database exceeds what the block size can support

MUPIP Error: This is a MUPIP INTEG error. Refer to the topic `MUPIP INTEG Errors in the About This Manual section <./about.html#mupip-integ-errors>`_.

Action: -

------------------
DBNAMEMISMATCH
------------------

DBNAMEMISMATCH, Database file xxxx (region (yyyy) referenced by shared memory (ID=zzzz) is not accessible. Ensure region is properly rundown.

Run Time Warning: When a YottaDB process attaches to a database and finds the corresponding shared memory structures already initialized, it performs integrity checks on the shared memory contents to ensure that they correspond back to the database file. When the shared memory points to a database file name that is not valid, this error is issued.

Action: This error means that the shared memory contents are corrupt; consult your YottaDB support channel before proceeding further.

---------------
DBNOCRE
---------------

DBNOCRE, Not all specified databases, or their associated journal files were created

MUPIP Warning: This indicates that MUPIP CREATE failed a task in creating the new database files.

Action: See accompanying messages for more detailed information on the failure.

---------------
DBNONUMSUBS
---------------

DBNONUMSUBS, kkkk key contains a numeric form of subscript in a global defined to collate all subscripts as strings

Run Time/MUPIP Error: The record has a numeric subscript but the collation setting for the global or region indicates that all subscripts are filed as strings. The leading context (XXXX) identifies the block and offset of the problematic record. This can arise if an operator uses DSE to force a change to a collation setting or to modify a key when the global already has content.

Action: If you can determine the cause of, and reason for, the change, you may choose to reverse it. If you need to change the collation, the appropriate procedure is to EXTRACT the data, KILL the global, or remove and recreate the database file, and then LOAD the extracted data.

-----------------
DBNOREGION
-----------------

DBNOREGION, None of the database regions accessible

DSE/MUPIP Error: MUPIP INTEG or DSE can report this error. This indicates that none of the database files specified in the Global Directory could be opened (or they do not exist).

Action: Ensure the proper assignment for the environment variable, ydb_gbldir/logical name GTM$GBLDIR. Verify that the database files specified in the Global Directory exist and that their protection allows access. Also, refer to the 'MUPIP INTEG Error Messages' table in the `Chapter 11 - Maintaining Database Integrity of the Administration and Operations Guide <../AdminOpsGuide/integrity.html>`_.

-------------------
DBNOTDB
-------------------

DBNOTDB, xxxx File does not have a valid GDS file header

MUPIP Error: This is a MUPIP INTEG error. Refer to the topic `MUPIP INTEG Errors in the About This Manual section <./about.html#mupip-integ-errors>`_.

Action: -

-----------------
DBNOTGDS
-----------------

DBNOTGDS, xxxx - Unrecognized database file format

Run Time Error: This indicates that a database operation attempted to activate file xxxx, which is not a GDS file.

Action: Use GDE to ensure that the files in the Global Directory are properly named. It is likely that something other than YottaDB or its utilities wrote to a database file or created a file with a name that coincides with one specified in the current Global Directory.

--------------
DBNOTMLTP
--------------

DBNOTMLTP, xxxx Block size not a multiple of 512 bytes

MUPIP Error: This is a MUPIP INTEG error. Refer to the topic `MUPIP INTEG Errors in the About This Manual section <./about.html#mupip-integ-errors>`_.

Action: -

-----------------
DBNULCOL
-----------------

DBNULCOL, NULL collation representation differs from the database file header settings

DSE/MUPIP/Run Time Error: This indicates that the database contains a record with an empty subscript ("Null" subscript) representation that is incompatible with the current database file header setting for such a representation. The leading context (XXXX) specifies the block number and the offset of the problematic record. This can only arise if someone changes the setting for the database while it contains one or more such subscripts. YottaDB recommends against making such a change. This message can originate from MUPIP INTEG, DSE INTEG or from running with VIEW "GDSCERT".

Action: Use the record and block information to remove the problematic record with DSE and restore the data appropriately, typically with a SET command. Note that the record and block of the record may change due to ongoing updates, so this operation requires great care and familiarity with DSE.

---------------
DBOPNERR
---------------

DBOPNERR, Error opening database file xxxx

Run Time Error: This indicates that a database operation tried to open the database file xxxx, which was inaccessible.

Action: Use GDE to ensure that the files in the Global Directory are properly named. Use the host shell command to ensure that the files exist and have proper security. Review the accompanying message(s) that indicate the reason for file-open failure.

-------------
DBPREMATEOF
-------------

DBPREMATEOF, Premature end of file with database file xxxx

Run Time Error: This indicates that the size of the database file is less than the size of the minimum required database file header. The file may not be a valid YottaDB file.

Action: Investigate whether the file was properly created (with MUPIP) or inappropriately truncated. Also check whether the global directory points to a valid database.

--------------
DBPRIVERR
--------------

DBPRIVERR, No privilege for attempted update operation for file: xxxx

Run Time Error: This indicates that the process did not have write access to database file xxxx.

Action: Disable application access to the function that resulted in the error or have the security manager grant write access to the appropriate user. Under some circumstances, security considerations may require moving some globals to other regions.

---------------
DBPTRMAP
---------------

DBPTRMAP, xxxx Block pointer is a bit map block number

Run Time Error: This indicates that the block certification facility encountered a block pointer to a bitmap location (in the index block).

Action: Report this error to the group responsible for database integrity at your operation.

---------------
DBPTRMX
---------------

DBPTRMX, xxxx Block pointer larger than file maximum

Run Time Warning: This indicates that a database operation failed because the block contains a record at offset yyyy whose block pointer points beyond the end of the file.

Action: Report this database structure error to the group responsible for database integrity at your operation.

----------------
DBPTRNOTPOS
----------------

DBPTRNOTPOS, xxxx Block pointer negative

Run Time Warning: This indicates that xxxx block contains a record yyyy, which nests an invalid index pointer, and so failed block certification.

Action: Report this database structure error to the group responsible for database integrity at your operation.

-----------------
DBQUELINK
-----------------

DBQUELINK, Database file xxxx, element location yyyy: blk = zzzz: control aaaa queue problem: was bbbb, expecting cccc

Run Time Error: This indicates that database cache recovery was triggered due to some abnormal event and that the recovery routine detected damage to an internal YottaDB queue control structure in the database global buffer cache.

Action: The system automatically attempts to correct the problem. If this error continues to occur, attempt MUPIP RUNDOWN and if that fails too, restore database from backup and replay the journal files. Report the error to your YottaDB support channel if necessary.

--------------
DBRBNLBMN
--------------

DBRBNLBMN, xxxx Root block number is a local bit map number

MUPIP Error: This is a MUPIP INTEG error. Refer to the topic `MUPIP INTEG Errors in the About This Manual section <./about.html#mupip-integ-errors>`_.

Action: -

---------------
DBRBNNEG
---------------

DBRBNNEG, xxxx Root block number negative

MUPIP Error: This is a MUPIP INTEG error. Refer to the topic `MUPIP INTEG Errors in the About This Manual section <./about.html#mupip-integ-errors>`_.

Action: -

------------------
DBRBNTOOLRG
------------------

DBRBNTOOLRG, xxxx Root block number greater than the last block number in file

MUPIP Error: This is a MUPIP INTEG error. Refer to the topic `MUPIP INTEG Errors in the About This Manual section <./about.html#mupip-integ-errors>`_.

Action: -

---------------
DBRDERR
---------------

DBRDERR, Cannot read database file xxxx after opening

Run Time Error: This indicates that a database operation attempted reading the file xxxx without having the read access.

Action: Use the host shell commands to verify the file access and adjust it, if appropriate.

----------------
DBRDONLY
----------------

DBRDONLY, Database file xxxx read only

Run Time Error: This indicates that a database operation tried to write to a read-only file or database.

Action: Verify the read and write privileges for the database and adjust it, if appropriate

------------------
DBREADBM
------------------

DBREADBM, xxxx Read error on bitmap

MUPIP Error: This is a MUPIP INTEG error. Refer to the topic `MUPIP INTEG Errors in the About This Manual section <./about.html#mupip-integ-errors>`_.

Action: -


----------------
DBREMOTE
----------------

DBREMOTE, Database region xxxx is remote; perform maintenance on the server node

DSE/MUPIP/LKE Error: This indicates that a database maintenance operation was attempted on region xxxx. This node does not maintain the region directly; instead, it uses GT.CM to access the node as a client. This error is also reported by ^%GBLDEF if the target global is mapped to another node and served by GT.CM.

Action: Perform database maintenance on the server node.

---------------
DBRLEVLTONE
---------------

DBRLEVLTONE, xxxx Root level less than one

MUPIP Error: This is a MUPIP INTEG error. Refer to the topic `MUPIP INTEG Errors in the About This Manual section <./about.html#mupip-integ-errors>`_.

Action: -

--------------
DBRLEVTOOHI
--------------

DBRLEVTOOHI, xxxx Root level higher than maximum

MUPIP Error: This is a MUPIP INTEG error. Refer to the topic `MUPIP INTEG Errors in the About This Manual section <./about.html#mupip-integ-errors>`_.

Action: -

--------------
DBRNDWN
--------------

DBRNDWN, Error during global database rundown for region xxxx. Please notify those responsible for proper database operation.

Run Time Error: This indicates that a process encountered a problem attempting to rundown the database file xxxx.

Action: Refer to the associated messages for more information.

.. _dbrndwnbypass:

--------------
DBRNDWNBYPASS
--------------

DBRNDWNBYPASS, YottaDB database rundown may have been bypassed due to timeout - run MUPIP JOURNAL ROLLBACK BACKWARD / MUPIP JOURNAL RECOVER BACKWARD / MUPIP RUNDOWN

Run Time Warning (Go wrapper specific): When the database rundown invoked by :code:`yottadb.Exit()` takes longer than :code:`MaximumNormalExitWait` (for normal exits) / :code:`MaximumPanicExitWait` (for fatal signal exits) seconds, the process terminates without waiting for the rundown to complete. See `Go Using Signals <../MultiLangProgGuide/goprogram.html#go-using-signals>`_ for more information.

Action: If the exiting process was not the last process accessing any file in the database, no action is needed. If the exiting process was the last, verify and restore database structural integrity. In either case, investigate why the database rundown took a long time. It could be caused by system load, or IO issues.

--------------
DBRNDWNWRN
--------------

DBRNDWNWRN, Global database xxxx not rundown successfully by PID yyyy [zzzz]. Global section was not removed.

Run Time Error: When the last process attached to a YottaDB database shared memory segment or global section detaches from the same, it normally removes the segment/section from the system. In case of an error while flushing the contents from the segment/section to the database file on disk, this removal is not done and this error is issued.

Action: Attempt a MUPIP RUNDOWN on that region. In case of an error, attempt corrective action corresponding to the displayed error.

---------------
DBROLLEDBACK
---------------

DBROLLEDBACK, Concurrent ONLINE ROLLBACK detected on one or more regions. The current operation is no longer valid

Run Time Error: This indicates that a non-TP mini-transaction attempted to interact with the database and found that a concurrent online rollback had taken the database to a state earlier than the one at the end of the process' last mini-transaction, unless there has been an intervening TP transaction.

Action: Application dependent - this error indicates a discontinuity in the database state that may cause inconsistent application data.

------------------
DBROOTBURN
------------------

DBROOTBURN, xxxx Root block has data level

DSE/Run Time Information: This indicates that the specified block has a block certification error or a DSE integrity error.

Action: Report this database structure error to the group responsible for database integrity at your operation.

--------------------
DBRSIZMN
--------------------

DBRSIZMN, xxxx Physical record too small

Run Time Warning: This indicates that a DSE or MUPIP INTEG command failed because block xxxx contains a record that does not meet the minimum size requirement.

Action: Report this database structure error to the group responsible for database integrity at your operation.

-------------------
DBRSIZMX
-------------------

DBRSIZMX, xxxx Physical record too large

Run Time Warning: This indicates that a DSE or MUPIP INTEG command failed because block xxxx contains a record that exceeds the maximum record size (1MB) for a GDS database.

Action: Report this database structure error to the group responsible for database integrity at your operation.

-----------------
DBSHMNAMEDIFF
-----------------

DBSHMNAMEDIFF, Database file ffff points to shared memory mmmm which points to a different database file

Run Time Error: Database access gives this error, if the database is copied or moved without properly closing it. This error indicates that database ffff and shared memory mmmmm do not correspond to each other.

Action: Perform MUPIP RUNDOWN on the database.

------------------
DBSPANCHUNKORD
------------------

DBSPANCHUNKORD, xxxx Chunk of yyyy blocks is out of order

MUPIP Error: This is a MUPIP INTEG error. Refer to the `MUPIP INTEG Errors section <./about.html#mupip-integ-errors>`_.

Action: N/A

-----------------
DBSPANGLOINCMP
-----------------

DBSPANGLOINCMP, xxxx Spanning node is missing. Block no yyyy of spanning node is missing

MUPIP Error: This is a MUPIP INTEG error. Refer to the `MUPIP INTEG Errors section <./about.html#mupip-integ-errors>`_.

Action: N/A

--------------------
DBSTARCMP
--------------------

DBSTARCMP, xxxx Star record has nonzero compression count

Run Time Warning: This indicates that a block certification or DSE integrity check failed on xxxx. Block xxxx contains a record at offset yyyy that should be a star key; however, it has a non-zero compression count.

Action: Report this database structure error to the group responsible for database integrity at your operation.

----------------
DBSTARSIZ
----------------

DBSTARSIZ, xxxx Star record has wrong size

Run Time Warning: This indicates that a block certification or DSE integrity check failed on xxxx. Block xxxx contains a record at offset yyyy whose star key does not have the proper size.

Action: Report this database structure error to the group responsible for database integrity at your operation.

-------------------
DBSVBNMIN
-------------------

DBSVBNMIN, xxxx Start VBN smaller than possible

MUPIP Error: This is a MUPIP INTEG error. Refer to the topic `MUPIP INTEG Errors in the About This Manual section <./about.html#mupip-integ-errors>`_.

Action: -

----------------
DBSZGT64K
----------------

DBSZGT64K, xxxx Block size is greater than 64k

MUPIP Error: This is a MUPIP INTEG error. Refer to the topic `MUPIP INTEG Errors in the About This Manual section <./about.html#mupip-integ-errors>`_.

Action: -

--------------
DBTN
--------------

DBTN, Block TN is xxxx

MUPIP Information: This is an auxiliary message and is preceded by a primary message.

Action: Follow the primary message description and action as specified in this manual.

--------------------
DBTNLTCTN
--------------------

DBTNLTCTN, Transaction numbers greater than or equal to the current transaction were found

MUPIP Information: This is an auxiliary message and is preceded by a primary message. It accompanies DBTNTOOLG.

Action: Follow the primary message description and action as specified in this manual.

------------------
DBTNNEQ
------------------

DBTNNEQ, xxxx Current tn and early tn are not equal

MUPIP Error: This is a MUPIP INTEG error. Refer to the topic `MUPIP INTEG Errors in the About This Manual section <./about.html#mupip-integ-errors>`_.

Action: -

-------------------
DBTNRESET
-------------------

DBTNRESET, Cannot reset transaction number for this region

Run Time Error: This message is an auxiliary message to the DBRDONLY message.

Action: Follow the primary message description and action as specified in this manual.

---------------------
DBTNRESETINC
---------------------

DBTNRESETINC, WARNING: tn_reset for database is incomplete due to integrity errors

Run Time Warning: Automatic resetting of transaction number has not been done due to other errors.

Action: Clean up the other errors and then run integ again.

----------------------
DBTNTOOLG
----------------------

DBTNTOOLG, xxxx Block transaction number too large

MUPIP Error: This is a MUPIP INTEG error. Refer to the topic `MUPIP INTEG Errors in the About This Manual section <./about.html#mupip-integ-errors>`_.

Action: -

---------------------
DBTOTBLK
---------------------

DBTOTBLK, File header indicates total blocks is tttt but file size indicates total blocks would be eeee

MUPIP Information: This is an auxiliary message, and is preceded by a primary message.

Action: Follow the primary message description and action as specified in this manual.

------------------
DBTTLBLK0
------------------

DBTTLBLK0, xxxx Total blocks equal zero

MUPIP Error: This is a MUPIP INTEG error. Refer to the topic `MUPIP INTEG Errors in the About This Manual section <./about.html#mupip-integ-errors>`_.

Action: -

-----------------
DBUNDACCMT
-----------------

DBUNDACCMT, xxxx Cannot determine access method; trying with BG

MUPIP Error: This is a MUPIP INTEG error. Refer to the topic `MUPIP INTEG Errors in the About This Manual section <./about.html#mupip-integ-errors>`_.

Action: N/A

---------------
DBVERPERFWARN1
---------------

DBVERPERFWARN1, Performance warning: Database aaaa is running in compatibility mode which degrades performance. Run MUPIP REORG UPGRADE for best overall performance.

Run Time Warning: This is a warning that the database is currently in compatibility (downgrade) mode. This mode causes all modified GDS blocks to be reformatted (to the downgraded database format) before they are flushed to the database file on disk. This is a very large performance hit.

Action: As the message indicates, run MUPIP REORG UPGRADE as soon as possible to move away from compatibility mode. This command can be run without taking the database offline. Once that completes successfully, the database is fully upgraded and there is no reformatting overhead anymore while flushing modified blocks to disk.

---------------
DBVERPERFWARN2
---------------

DBVERPERFWARN2, Peformance warning: Database aaaa is not fully upgraded. Run MUPIP REORG UPGRADE for best overall performance.

Run Time Warning: This is a performance warning message that indicates that the database is not yet fully upgraded i.e. there are still blocks in the database file that need to be upgraded. Staying in this mode causes some inefficiencies which include (but are not limited to) reading blocks from disk.

Action: As the message indicates, run MUPIP REORG UPGRADE at the earliest. This command can be run without taking the database offline. Once that completes successfully, the database file is fully upgraded.

------------------
DBWCVERIFYEND
------------------

DBWCVERIFYEND, Database file xxxx, write cache verification finished by pid pppp [aaaa] at transaction number yyyy

Run Time Information: This indicates that process-id pppp (aaaa in hexadecimal) has completed verification of the database cache for the database file xxxx.

Action: -

-----------------
DBWCVERIFYSTART
-----------------

DBWCVERIFYSTART, Database file xxxx, write cache verification started by pid pppp [aaaa] at transaction number bbbb

Run Time Information: This indicates that process-id pppp (aaaa in hexadecimal) has started a verification of the database cache for the database file xxxx.

Action: -

-------------------
DCNINPROG
-------------------

DCNINPROG, Attempt to initiate operation while disconnect was in progress

GT.CM Error: This indicates that the GT.CM tried to link while disconnecting or experiencing network problems.

Action: Review network error logs.

------------------
DELIMSIZNA
------------------

DELIMSIZNA, Delimiter size is not appropriate

Compile Time/Run Time Error: A socket related IO command (OPEN or USE) triggers this error if the delimiter string exceeds its maximum length.

Action: Use a delimiter string with an appropriate length.

---------------------
DELIMWIDTH
---------------------

DELIMWIDTH, Delimiter length xxxx exceeds device width yyyy

Run Time Information: This indicates that the length of the first delimiter string specified in the DELIMITER deviceparameter exceeds the WIDTH of the socket device being OPENed (or USEd).

Action: Modify the first delimiter string to have a length of, at most, the WIDTH of the socket device.

----------------------
DEVICEREADONLY
----------------------

DEVICEREADONLY, Cannot write to a read-only device

Run Time Error: The application made an attempt to WRITE to a read-only device.

Action: Review code and context to see if the WRITE was intended for another device and if so, add the appropriate USE. If the WRITE is intended for this device, change the device OPEN to permit the WRITE.

--------------------
DEVICEWRITEONLY
--------------------

DEVICEWRITEONLY, Cannot read from a write-only device

Run Time Error: The application made an attempt to READ from a device in a WRITEONLY state, typically due to the OPEN command specifications.

Action: Check for logic errors and revise the code.

--------------------
DEVNOTIMP
--------------------

DEVNOTIMP, XXXX device not implemented on in this environment

Run Time Error: This indicates that the device support is not available in the currently running version of YottaDB.

Action: Refer to the YottaDB documentation. Contact your YottaDB support channel for information about the support available for this type of device on your platform.

----------------------
DEVOPENFAIL
----------------------

DEVOPENFAIL, Error opening xxxx

Run Time Error: This indicates that a YottaDB process encountered an error while opening the device xxxx. A supplementary TEXT message and a system message provide more details about the cause of the error.

Action: Verify that the device exists on the system where the OPEN is being attempted.

-------------------
DEVPARINAP
-------------------

DEVPARINAP, Device parameter inappropriate to this command

Compile Time Error: This indicates that an OPEN, USE, or CLOSE command specifies a deviceparameter that does not apply to the command.

Action: Look for deviceparameters that should be on other I/O commands. For example, the deviceparameter "DELETE" is valid on CLOSE but produces this error if it is applied to the USE command.

------------------
DEVPARMNEG
------------------

DEVPARMNEG, Deviceparameter must be a positive value

Run Time Error: This indicates that the argument to the deviceparameter had a negative value where only positive values are appropriate.

Action: Modify the argument to provide a positive value.

------------------
DEVPARMTOOSMALL
------------------

DEVPARMTOOSMALL, Deviceparameter must be greater than zero (0)

Compile Time/Run Time Error: This error occurs when the TIMEOUT=<seconds> deviceparameter of a CLOSE command specifies a value less than one second. For PIPE devices that are not OPEN'd with the INDEPENDENT deviceparameter, the CLOSE command waits for a maximum of TIMEOUT=<seconds> before checking the termination status of the PIPE co-process.

Action: Specify an integer value greater than 0 as the TIMEOUT.

----------------
DEVPARPROT
----------------

DEVPARPROT, The protection specification is invalid

Compile Time Error: This indicates that an OPEN, USE, or CLOSE command specified a protection deviceparameter with an improperly formatted argument.

Action: Modify the protection mask.

-----------------
DEVPARTOOBIG
-----------------

DEVPARTOOBIG, String deviceparameter exceeds 255 character limit

Compile Time/Run Time Error: This indicates that an OPEN, USE, or CLOSE command specified a deviceparameter that equated to a string expression whose evaluated length exceeds 255 characters.

Action: Verify the program logic and modify it to use shorter deviceparameter strings.

------------------
DEVPARUNK
------------------

DEVPARUNK, Deviceparameter unknown

Compile Time Error: This indicates that an OPEN, USE, or CLOSE command specified an unrecognized keyword instead of an expected deviceparameter.

Action: Modify the deviceparameter in question.

---------------------
DEVPARVALREQ
---------------------

DEVPARVALREQ, A value is required for this device parameter

Compile Time Error: This indicates that an OPEN, USE, or CLOSE command specified a valid deviceparameter that requires a value; however, one was not provided.

Action: Ensure that deviceparameters have values where required. For example, the deviceparameter WRAP is valid but must include a value for the wrap length.

--------------------
DIRONLY
--------------------

DIRONLY, Directories only are allowed in file specs: xxxx

Run Time Error: This indicates that a SET of $ZROUTINES specified a SRC qualifier with an argument element xxxx that was not a valid directory specification.

Action: Look for missing parenthesis or brackets.

------------------
DISTPATHMAX
------------------

DISTPATHMAX, $ydb_dist path is greater than maximum (xxxx)

Run Time Error: This indicates that the path specified by the ydb_dist environment variable has exceeded the indicated maximum limit of 1024 bytes.

Action: Move the directory or use a link to shorten the path.

-----------------
DIVZERO
-----------------

DIVZERO, Attempt to divide by zero

Run Time Error: This indicates that a divide or modulo operator had a zero for its divisor operand.

Action: Modify the routine to protect against zero division.

-----------------
DLCKAVOIDANCE
-----------------

DLCKAVOIDANCE, Possible deadlock detected: Database pppp: Dbtn qqqq: t_tries rrrr: dollar_trestart ssss: now_crit tttt: TP transaction restarted

Run Time Error: This indicates that YottaDB's deadlock avoidance algorithm got triggered and aborted a possible deadlock.

Action: Report the error to your YottaDB support channel with complete operator log information.

-----------------
DLLCHSETM
-----------------

DLLCHSETM, Routine XXX in library YYY was compiled with CHSET=M which is different from $ZCHSET. Recompile with CHSET=UTF-8 and re-link.

Run Time Error: This error is triggered when a UTF-8 mode process attempts to execute a shared library's routine that was compiled in M-mode.

Action: Recompile and relink the routine using UTF-8-mode settings or switch to M mode.

--------------
DLLCHSETUTF8
--------------

DLLCHSETUTF8, Routine XXX in library YYY was compiled with CHSET=UTF-8 which is different from $ZCHSET. Recompile with CHSET=M and re-link.

Run Time Error: This error is triggered when an M mode process attempts to execute a shared library's routine that was compiled in UTF-8 mode.

Action: Recompile and relink the routine using M-mode settings or switch to UTF-8 mode.

------------------
DLLNOCLOSE
------------------

DLLNOCLOSE, Failed to unload external dynamic library

Run Time Error: This indicates that the process encountered a problem attempting to unload a dynamically linked library.

Action: Refer to the associated messages for more information.

--------------------
DLLNOOPEN
--------------------

DLLNOOPEN, Failed to load external dynamic library xxxx

Run Time Error: This indicates that the process encountered a problem attempting to load a dynamically linked library.

Action: Refer to the associated messages for more information.

---------------------
DLLNORTN
---------------------

DLLNORTN, Failed to look up the location of the symbol xxxx

Run Time Error: This indicates that the process was unable to find the routine it needed in the dynamically linked library.

Action: Ensure that the environment variable for dynamic library path is defined and correctly locates the shared library file, as well as any other dependent shared libraries. Also ensure that the symbol xxxx is defined in one of the libraries.

------------------------
DLLVERSION
------------------------

DLLVERSION, Routine aaaa in library bbbb was compiled with an incompatible version of YottaDB. Recompile with the current version and re-link.

Run Time Error: This indicates that the routine aaaa that was loaded out of the shared library bbbb was compiled with a version of YottaDB that is not compatible with the current version of YottaDB.

Action: Recompile the M routine aaaa and re-link (recreate) the shared library. The linker options for creating a shared library are platform dependant. Refer to the `Programmer's Guide <../ProgrammersGuide/index.html>`_ for details.

--------------------------
DLRCILLEGAL
--------------------------

DLRCILLEGAL, Illegal $CHAR() value xxxx

MUPIP Error: This indicates that MUPIP LOAD with the qualifier FORMAT=GO or ZWR encountered an invalid Unicode code point xxxx for $CHAR() in its input stream.

Action: Edit or recreate the input file so the value falls within the valid range of Unicode code points.

---------------------------
DLRCTOOBIG
---------------------------

DLRCTOOBIG, xxxx value cannot be greater than 255

MUPIP Error: This indicates that MUPIP LOAD with the qualifier FORMAT=GO encountered xxxx in its input stream. xxxx was in the $CHAR() format used for non-graphic characters but it exceeded the maximum acceptable value of 255.

Action: Refer to the topic `MUPIP LOAD Errors in the About This Manual section <./about.html#mupip-load-errors>`_. Edit the input file so the value falls within the range of 0-255.

------------------------------
DLRCUNXEOR
------------------------------

DLRCUNXEOR, xxxx unexpected end of record in $CHAR()/$ZCHAR() subscript

MUPIP Error: This indicates that MUPIP LOAD with the qualifier FORMAT=GO encountered xxxx in its input stream. xxxx was in the $CHAR() format used for non-graphic characters but the $C() format did not complete properly.

Action: Refer to the topic `MUPIP LOAD Errors in the About This Manual section <./about.html#mupip-load-errors>`_. Edit the input file to remove a spurious "$" or fix a $CHAR() representation.

-----------
DONOBLOCK
-----------

DONOBLOCK, Argumentless DO not followed by a block

Compile Time Warning: This indicates the compiler detected an argumentless DO with no subsequent block with an appropriate level, and optimized it away.

Action: This indicates a coding issue where the block is missing or has the wrong level indication. This may occur in code under development where the block is yet to be coded, or code being debugged where the block has been commented out. Otherwise, it likely indicates a logic bug where a programmer intended to provide a block of code but did not provide one. Correct as appropriate.

--------------------
DSEBLKRDFAIL
--------------------

DSEBLKRDFAIL, Failed attempt to read block

DSE Error: This indicates that DSE could not read the block from the database file. This error may also be caused by attempts to reference blocks outside of the database. It is very likely that the database may have integrity errors or that the process-private data structures are corrupted.

Action: Make sure that the referenced block is less than the total blocks. If not, report this database cache error to the group responsible for database integrity at your operation.

------------------------
DSEFAIL
------------------------

DSEFAIL, DSE failed. Failure code: xxxx.

DSE Error: This indicates that DSE could not complete a database operation. xxxx contains failure codes for the four attempts. It is very likely that the database may have integrity errors or that the process-private data structures are corrupted.

Action: Report this database structure error to the group responsible for database integrity at your operation.

---------------------------
DSEINVLCLUSFN
---------------------------

DSEINVLCLUSFN, Specified function is invalid for clustered databases

DSE Error: This indicates that the DSE command (WCINIT or ALL used with the qualifiers RENEW or WCINIT) requested a cache reinitialization on a clustered database.

Action: This operation is managed automatically by the Cluster Control Program (CCP) on the first node to open the database file. Once the database is in use, all nodes must drop access in order to reinitialize the database cache.

-------------------------
DSEMAXBLKSAV
-------------------------

DSEMAXBLKSAV, DSE cannot SAVE another block as it already has the maximum of mmmm

DSE Error: The current SAVE -BLOCK operation exceeds DSE's capacity to hold more than mmmm saved blocks

Action: Delete some saved blocks, possibly after RESTOREing them to free blocks, or restart DSE if none of the currently saved blocks have value.

------------------------
DSENOFINISH
------------------------

DSENOFINISH, DSE unable to finish all requested actions

DSE Error: This indicates that DSE was not able to complete the actions it was directed to perform.

Action: Refer to the associated message(s) for more information.

-------------------------
DSENOTOPEN
-------------------------

DSENOTOPEN, DSE could not open region rrrr - see DSE startup error message for cause

DSE Error: DSE could not operate on region rrrr because it was not able to open it when DSE started.

Action: Review the error messages issued when DSE started and address the issue(s) they describe.

------------------------
DSEONLYBGMM
------------------------

DSEONLYBGMM, xxxx is supported only for BG/MM access methods

DSE Warning: This indicates that the current region has an access method that is neither Buffered Globals nor Memory Mapped. DSE can not flush such a region.

Action: Before starting DSE, make sure that the Global Directory contains regions that have either Buffered Globals or Memory Mapped access methods.

-------------------------
DSEWCINITCON
-------------------------

DSEWCINITCON, No action taken, enter YES at CONFIRMATION prompt to initialize global buffers

DSE Warning: This indicates that DSE did not perform the operation that was initiated by the WCINIT command or the ALL command with either the WCINIT or RENEW qualifier yet, because the operator did not confirm it.

Action: To perform these operations, enter YES at the CONFIRMATION prompt to verify your intention to perform this potentially disruptive operation. The DSE WCINIT command reinitializes shared memory structures for the current region.

-------------------------
DSEWCREINIT
-------------------------

DSEWCREINIT, Database cache reinitialized by DSE for region rrrr

DSE Information: This indicates a DSE operator action to rebuild the database cache for region rrrr.

Action: None required.

---------------------
DSKNOSPCAVAIL
---------------------

DSKNOSPCAVAIL, Attempted write to file FFFF failed due to lack of disk space. Retrying indefinitely.

Run Time Error: This error indicates that YottaDB could not update file FFFF due to a lack of disk space in the file system. If -INST_FREEZE_ON_ERROR is enabled, YottaDB automatically disables it (sending a DSKSPCAVAILABLE message to the operator log) when adequate disk space becomes available again.

Action: Make disk space available in the file system to allow updates to file FFFF.

--------------------
DSKNOSPCBLOCKED
--------------------

DSKNOSPCBLOCKED, Retry of write to file FFFF suspended due to new instance freeze. Waiting for instance to be unfrozen.

Run Time Error: This error indicates that a process waiting for space to write to file FFFF determined that another process froze the replication instance. The process will not make any more attempts to write to the file until the replication instance is unfrozen.

Action: Check the system log for the most recent REPLINSTFROZEN message to determine the cause of the current freeze and resolve it.

---------------------
DSKSPACEFLOW
---------------------

DSKSPACEFLOW, Disk space for file xxxx nearing maximum size. YYYY blocks available.

Run Time Warning: This indicates that the disk space for the specified file system on which the database/journal files are located is almost full.

Action: Review and make disk space.

----------------------
DSKSPCAVAILABLE
----------------------

DSKSPCAVAILABLE, Write to file FFFF succeeded after out-of-space condition cleared.

Success Information: This indicates that the file system of file FFFF has enough space to allow further updates (was previously not possible, as indicated by a DSKNOSPCAVAIL error message).

Action: None Required.

-----------------------
DSKSPCCHK
-----------------------

DSKSPCCHK, Error while checking for available disk space to create file DDDD

All YottaDB Components Error: While checking if there was available space to create the database file, some service failed. This error is followed by a description of what caused the failure.

Action: Address the reason for the failure and retry.

----------------------
DUPTN
----------------------

DUPTN, Duplicate transaction found [TN = xxxx] at offset aaaa in journal file yyyy

MUPIP Warning: This indicates that two different transactions have the same transaction number.

Action: Report the entire incident context to your YottaDB support channel.

------------------
DUPTOKEN
------------------

DUPTOKEN, Token xxxx is duplicate in the journal file yyyy for database zzzz

MUPIP Error: This indicates that two transactions (TP or ZTP) have the same token (xxxx) in the specified journal file yyyy, violating the uniqueness of the ID that distinguishes transactions from one another. The result is that both transactions are considered broken and reported in the broken transactions extract file.

Action: Report the entire incident context to your YottaDB support channel.

-------------------
DVIKEYBAD
-------------------

DVIKEYBAD, $ZGETDVI("xxxx","yyyy") contains an illegal keyword

Run Time Error: This indicates that a $ZGETDVI function encountered an invalid keyword. xxxx is the device. yyyy is the keyword.

Action: Verify the spelling of the keyword.

-----------------
DYNUPGRDFAIL
-----------------

DYNUPGRDFAIL, Unable to dynamically upgrade block 0xaaa in database yyy due to lack of free space in block

DBCERTIFY/Run Time Error: There was not enough free space in the block to convert it (in place) to the current format during normal database access. This indicates that the DBCERTIFY database certification procedure was not properly carried out.

Action: Either mark the block free (making appropriate index changes) or downgrade the database and re-run DBCERTIFY (both phases).

-------------------
DZTRIGINTRIG
-------------------

DZTRIGINTRIG, $ZTRIGGER() is not allowed inside trigger context. Trigger name: nnnn

Run Time Error: This message indicates an attempt to use the $ZTRIGGER() function, which potentially modifies triggers, while executing code within the context of some trigger.

Action: Rework the code to modify or examine triggers so that it falls outside of trigger execution.

--------------------
DZWRNOALIAS
--------------------

DZWRNOALIAS, $ZWRTAC cannot be aliased.

Compile Time Error: This indicates the argument for a SET * command attempted to assign a $ZWRTAC* pseudo-variable as an alias.

Action: Correct the code in question - the $ZWRTAC* is only useful in restoring context from ZSHOW or ZWRITE output and has very narrow capabilities.

---------------------
DZWRNOPAREN
---------------------

DZWRNOPAREN, $ZWRTACxxx is not allowed inside a parenthesized SET target

Compile Time Error: This indicates the argument for a SET command attempted to assign a $ZWRTAC* pseudo-variable within a parenthesized list of left-hand arguments.

Action: Correct the code in question - the $ZWRTAC* is only useful in restoring context from ZSHOW or ZWRITE output and has very narrow capabilities.

----------------------
ECLOSTMID
----------------------

ECLOSTMID, $ECODE overflow, the first and last ecodes are retained, but some intervening ecodes have been lost

Run Time Warning: If the $ECODE exceeds the maximum string length, references to it return only the codes for the earliest and latest errors separated by the code for ECLOSTMID, which indicates that a suppression of intervening error codes has occurred to accommodate string length restrictions.

Action: Consider whether it would be appropriate to introduce code to SET $ECODE= . This error is encountered either when the $ETRAP error handling is recursing (and probably defective), or while using $ZTRAP error handling that was coded prior to the introduction of $ECODE.

-------------------
ENCRYPTCONFLT
-------------------

ENCRYPTCONFLT, MUPIP REORG -ENCRYPT and MUPIP EXTRACT -FORMAT=BIN cannot run concurrently - skipping oooo on region: rrrr, file: ffff

MUPIP Error: MUPIP cannot perform REORG -ENCRYPT and EXTRACT -FORMAT=BIN on file ffff at the same time; rrrr is the region that mapped the file; oooo is the operation that was just started.

Action: Reschedule the just-started operation or terminate the conflicting operation to allow the just-started operation to run immediately.

-------------------
ENCRYPTCONFLT2
-------------------

ENCRYPTCONFLT2, A concurrent MUPIP REORG -ENCRYPT changed the encryption key for RRRR before the process could initialize it

Run Time Warning: Due to a concurrent MUPIP REORG -ENCRYPT, a process was forced to defer encryption key initialization for region RRRR.

Action: None. This information message is only important when followed by other encryption errors.

-------------------
ENDIANCVT
-------------------

ENDIANCVT, Converted database file xxxx from yyyy endian to zzzz endian on a wwww endian system

MUPIP Information: When MUPIP ENDIANCVT has successfully completed, it displays conversion information. This information includes the database file, its previous endian format, the new endian format, and the endian format that is native to the current system.

Action: N/A

------------------
ENOSPCQIODEFER
------------------

ENOSPCQIODEFER, Write to file FFFF deferred due to lack of disk space

Information: This indicates YotttaDB chose to defer updating the file FFFF to avoid a possible deadlock. YottaDB uses this message only if the environment is configured for Instance Freeze.

Action: None.

-------------------
EORNOTFND
-------------------

EORNOTFND, xxxx End of record not found

MUPIP Error: This indicates that LOAD encountered a database reference record containing an open parenthesis without a closiing parenthesis. xxxx is the record.

Action: Refer to the topic `MUPIP LOAD Errors in the About This Manual section <./about.html#mupip-load-errors>`_.

--------------------
EPOCHTNHI
--------------------

EPOCHTNHI, At the EPOCH record at offset xxxx of yyyy transaction number [0xaaaa] is higher than database transaction number [0xbbbb]

MUPIP Error: This indicates that at the turnaround point from where MUPIP applies logical records, Backward Recover found that the epoch's transaction number is greater than the current database transaction number.

Action: Contact your system administrator and if necessary, report the entire incident context to your YottaDB support channel.

-------------------------
EQUAL
-------------------------

EQUAL, Equal sign expected but not found

Compile Time Error: This indicates that a SET or FOR specified the left side of an assignment statement but not the expected equal (=) sign.

Action: Look for a missing equal sign.

---------------------
ERRCALL
---------------------

ERRCALL, Error called from xxxx line yyyy

Run Time Error: This message provides additional diagnostic information related to accompanying messages.

Action: Review accompanying messages for additional information about the cause of this error. If necessary, report the entire incident context to your YottaDB support channel.

-------------------
ERRORSUMMARY
-------------------

ERRORSUMMARY, Errors occurred during compilation

Compile Time Error: This indicates that YottaDB encountered one or more errors during compilation.

Action: Review the individual error messages for further information. The compilation may have produced code that is usable as long as the execution path does not encounter the error(s).

-----------------------
ERRWETRAP
-----------------------

ERRWETRAP, Error while processing $ETRAP

Run Time Error: This indicates that $ETRAP contained invalid M code or caused a run time error.

Action: Check the $ETRAP variable. To get more information about the errors- SET $ZTRAP to empty string, and a temporary variable to the contents of $ETRAP. Then, SET $ETRAP to "BREAK" and XECUTE the temporary variable. It is best to keep the source code in $ETRAP simple if $ZTRAP is an empty string, since ETRAP specifies a string that YottaDB invokes upon encountering an exception condition.

-------------------
ERRWEXC
-------------------

ERRWEXC, Error while processing exception string

Run Time Error: This indicates that an exception string contained invalid M code or caused a run-time error.

Action: Review the exception string and $ZTRAP. To get more information about the errors, SET a temporary variable to the contents of the exception string. Then, SET the exception string to "BREAK" and XECUTE the temporary variable.

------------------
ERRWIOEXC
------------------

ERRWIOEXC, Error while processing I/O exception string

Run Time Error: This indicates that a device EXCEPTION string contained invalid M code or caused a run-time error.

Action: Review the exception string. To get more information about the errors, SET a temporary variable to the contents of the exception string and XECUTE the temporary variable. The EXCEPTION deviceparameter on an OPEN, USE, or CLOSE command defines an error handler for an I/O device.

------------------
ERRWZBRK
------------------

ERRWZBRK, Error while processing ZBREAK action string

Run Time Error: This indicates that a ZBREAK action contained invalid M code or caused a run-time error.

Action: Review the ZBREAK action string. To get more information about the errors, SET a temporary variable to the contents of the ZBREAK action string and XECUTE the variable. ZBREAK sets temporary break and trace points.

------------------
ERRWZINTR
------------------

ERRWZINTR, Error while processing $ZINTERRUPT

Run Time Error: This indicates that a job interrupt was signaled but there was an error compiling the $ZINTERRUPT string. This message is sent to the operator log facility at the direct mode prompt or when executing a direct mode command to the user console.

Action: Correct the $ZINTERRUPT to contain valid YottaDB commands.

--------------------
ERRWZTIMEOUT
--------------------

ERRWZTIMEOUT, Error while processing $ZTIMEOUT

Run Time Error: This indicates a problem invoking the current $ZTIMEOUT vector and usually accompanies other error messages

Action: Examine and correct the code vector specified by $ZTIMEOUT, or if there is none, examine the current value for $ETRAP or $ZTRAP. Unlike $ETRAP and code values for $ZTRAP, which are evaluated when they are assigned, compilation of $ZTIMEOUT vectors occurs when the vector is invoked by the expiration of the specified time.

-------------------
ERRWZTRAP
-------------------

ERRWZTRAP, Error while processing $ZTRAP

Run Time Error: This indicates that $ZTRAP contained invalid M code or caused a run-time error.

Action: Verify the $ZTRAP variable. To get more information about the errors, SET a temporary variable to the contents of $ZTRAP. Then, SET $ZTRAP to "BREAK" and XECUTE the variable. Make sure the source code in $ZTRAP is simple because ZTRAP specifies a string that YottaDB invokes when it encounters an exception condition.

---------------------
EVENTLOGERR
---------------------

EVENTLOGERR, Error in event logging subsystem

Run Time Error: This indicates that the user is unable to access the event logging shared library or an event logging routine within the shared library.

Action: Review accompanying messages for additional information.

------------------
EXCEEDSPREALLOC
------------------

EXCEEDSPREALLOC, Preallocated size ssss for M external call label LLLL exceeded by string of length SSSS

Call out Error: The code invoked as externroutinename LLLL returned a string of length SSSS, but the call table specified a maximum length of ssss for the return.

Action: Revise the external routine to abide by the call table size or change the call table to preallocate a suitably larger size.

------------------
EXCLUDEREORG
------------------

EXCLUDEREORG, Global: xxxx is present in the EXCLUDE option. REORG will skip the global.

MUPIP Warning: This indicates that MUPIP did not reorg the specified global because it was also mentioned in the EXCLUDE qualifier.

Action: Take out the global name from the EXCLUDE option and do not specify a global name in both the SELECT and EXCLUDE options.

----------------------
EXECOM
----------------------

EXECOM, Executing command file xxxx

GDE/DSE Information: This indicates that an @ command activated command file xxxx.

Action: -

-------------
EXITSTATUS
-------------

EXITSTATUS, Unexpected process exit (xxxx), exit status aaaa -- called from module yyyy at line zzzz

Run Time Error: Indicates a non-zero exit status aaaa returned from a process started in the context of xxxx. The following are common values (other values are possible depending on the script called) and descriptions for the exit status: 1-"Catchall for general errors", 2-"Misuse of shell builtins", 126-"Command invoked cannot execute", 127-"Command not found", 128-"Invalid argument to exit" and 130-"Script terminated by Control-C".

Action: Use the exit status aaaa to adjust the script causing the unexpected exit.

EXITSTATUS was added to YottaDB effective release r1.36.

-----------------------
EXPR
-----------------------

EXPR, Expression expected but not found

Compile Time Error: This indicates that YottaDB did not encounter a valid expression when it expected one.

Action: Look for missing expressions or extra delimiters, such as a space, comma, or colon.

--------------------------
EXTCALLBOUNDS
--------------------------

EXTCALLBOUNDS, Wrote outside bounds of external call buffer. M label: LLLL

Call out Fatal: The code invoked as externroutinename LLLL violated the bounds of its allocated buffers.

Action: Ensure the non-YottaDB code uses appropriate allocations, pointer management logic and bounds checking.

--------------------------
EXTGBLDEL
--------------------------

EXTGBLDEL, Invalid delimiter for extended global syntax

Run Time Error: This indicates that the global reference started with a vertical bar (|) or left-bracket ([), indicating that it includes an environment specification (Global Directory). However, the environment specification did not terminate with a vertical bar (|) or right-bracket (]) respectively.

Action: Insert the appropriate trailing delimiter for the environment specification or remove the environment specification.

---------------------
EXTRACTCTRLY
---------------------

EXTRACTCTRLY, User interrupt encountered during extract, halting

MUPIP Warning: This indicates that EXTRACT encountered either a <CTRL>-Y or two <CTRL>-C in quick succession during the course of its operation and aborted prior to normal completion.

Action: If the results of the EXTRACT are needed, reactivate it.

--------------------
EXTRACTFILERR
--------------------

EXTRACTFILERR, Error with extract file xxxx

MUPIP Error: This indicates that EXTRACT encountered an error when opening its output file: xxxx.

Action: Review the accompanying message(s) for additional information.

---------------------
EXTRFAIL
---------------------

EXTRFAIL, Extract failed for the global gggg. MUPIP INTEG should be run.

MUPIP Error: A MUPIP EXTRACT operation on the global gggg failed because of database consistency issues.

Action: Run the MUPIP INTEG command to identify the database consistency issues.

-------------------------
EXTRFILEXISTS
-------------------------

EXTRFILEXISTS, Error opening output file: ffff -- File exists

Run Time Error: This message indicates that the specified output file already exists.

Action: Specify another file name and/or directory.

----------------------
EXTRFMT
----------------------

EXTRFMT, Extract error: invalid record format - no records found

MUPIP Error: This indicates that LOAD could not process the sequential output file because the record after the header is invalid.

Action: Verify the file has a valid format and actually contains records.

-----------------------
EXTRINTEGRITY
-----------------------

EXTRINTEGRITY, Database ffff potentially contains spanning nodes or data encrypted with two different keys

MUPIP Error: MUPIP EXTRACT cannot run because the database file ffff might contain spanning nodes or be partially encrypted with a particular key. Proceeding on a live database in such a situation could result in data corruption.

Action: If you encounter this error while running MUPIP EXTRACT with -FORMAT="BINARY", re-run the command with the -FREEZE qualifier. MUPIP EXTRACT requires -FREEZE to acquire stand-alone access to produce a consistent copy of the data. However, not using -FREEZE when you request a MUPIP EXTRACT may produce a loadable, if inconsistent output. If you encountered this error while running MUPIP EXTRACT with ZWR or GO format, it is likely that your database is encrypted with more than one key; with BINARY output it may be multiple keys or spanning node data. If the issue is a key change, run MUPIP REORG -ENCRYPT to complete the encryption of the database. As a final resort, you may use an -OVERRIDE qualifier to proceed on a live database that either contains spanning nodes or is undergoing (re)encryption. Although EXTRACT -OVERRIDE may produce text for analysis, the result is not suitable as input for MUPIP LOAD and YottaDB highly discourages using -OVERRIDE.

-------------------------
EXTSRCLIN
-------------------------

EXTSRCLIN, xxxx yyyy

Run Time Error: This indicates that there is an error in the external call table. The message indicates the line where YottaDB found the error.

Action: Review the line listed in the message and correct the error.

------------------------
EXTSRCLOC
------------------------

EXTSRCLOC, At column xxxx, line yyyy, source module zzzz

Run Time Error: This indicates that there is an error in the external call table. The message indicates the line and the location within that line where the error is located.

Action: Review the listed line and location and correct the error.

-----------------------
FAILEDRECCOUNT
-----------------------

FAILEDRECCOUNT, LOAD unable to process MMMM records

MUPIP Error: MUPIP LOAD was unable to load MMMM records from the specified input extract.

Action: Examine prior RECLOAD error messages for causes for the failed records and address them.

-----------------------
FALLINTOFLST
-----------------------

FALLINTOFLST, Fall-through to a label with formallist is not allowed

Run Time Error: This error indicates that M code reached a label with a formallist by falling through from the previous label.

Action: Revisit your code to ensure that all invocations of labels with a formallist occur using a DO command or extrinsic function ($$).

------------
FATALERROR1
------------

FATALERROR1, Fatal error raised. Generating core and terminating process. Error: <error>.

Run Time Error: This indicates that there was a fatal error in a SimpleAPI call that resulted in the termination of the running process and the generation of a core file. Appears in the system log.

Action: Look up the error indicated in the secondary message text in the documentation to correct the cause of the fatal error.

------------
FATALERROR2
------------

FATALERROR2, Fatal error raised. Bypassing core generation and terminating process. Error: <error>

Run Time Error: This indicates that there was a fatal error in a SimpleAPI call that resulted in the termination of the running process, and no core file was generated as a result of this. Appears in the system log.

Action: Look up the error indicated in the secondary message text in the documentation to correct the cause of the fatal error.

--------------------
FCHARMAXARGS
--------------------

FCHARMAXARGS, Argument count of $CHAR() function exceeded the maximum of 255

Compile Time Error: This indicates that a $CHAR() function specified an argument that was not in the range of 0 to 255. This error is also reported by services that attempt to format data using $CHAR() format.

Action: Look for large or negative $CHAR() arguments and ensure that all the arguments contain valid ASCII codes.

------------------
FCNSVNEXPECTED
------------------

FCNSVNEXPECTED, Function or special variable expected in this context

Compile Time Error: This indicates that YottaDB encountered a dollar sign in an expression that was not followed by a valid function or special variable name.

Action: Look for misspelled functions and special variable names or a missing $ in an extrinsic.

--------------------
FILECREATE
--------------------

FILECREATE, AAAA file xxxx created

MUPIP Information: This indicates that a file xxxx was created due to AAAA, where AAAA is a lost transaction, broken transaction, or a Journal Extract file.

Action: Look for the xxxx file for further relevant data.

---------------------
FILECREERR
---------------------

FILECREERR, Error OOOO for file DDDD during DB creation

Run Time/MUPIP Error: While creating a database file, some IO operation OOOO failed; this error is followed by the error that occurred.

Action: Address the error and retry.

----------------
FILEDEL
----------------

FILEDEL, File xxxx successfully deleted

Run Time/MUPIP Information: This indicates that YottaDB or MUPIP has successfully deleted a file. This message is issued when a journal file in an inconsistent state is found and deleted by the run-time system or MUPIP SET. This message is also issued by MUPIP JOURNAL RECOVER/ROLLBACK command when it deletes journal files that were created by a previously interrupted RECOVER/ROLLBACK command and are no longer necessary.

Action: -

---------------------
FILEDELFAIL
---------------------

FILEDELFAIL, Deletion of file xxxx failed

Run Time/MUPIP Warning: This indicates that YottaDB or MUPIP failed to remove the specified journal file xxxx.

Action: Review the accompanying message(s) for additional information.

-------------------
FILEEXISTS
-------------------

FILEEXISTS, File xxxx already exists

MUPIP Error: This indicates that MUPIP discovered a file with the filename xxxx already existing, and did not overwrite it while executing the specified command(s). In many cases, this is an expected outcome when the action has an explicit or implicit target of multiple database files which may be in differing states.

Action: Rename the already existing file xxxx and reissue the MUPIP command(s), or modify the MUPIP command to name (explicitly/implicitly) a file different from xxxx. If you encountered this error with MUPIP BACKUP, use the -REPLACE qualifier if you want to replace the existing backup files.

---------------------
FILEIDGBLSEC
---------------------

FILEIDGBLSEC, File ID in global section does not match with the database file

Run Time Error: When a YottaDB process attaches to a database and finds the corresponding shared memory structures initialized already, it performs integrity checks on the shared memory contents to ensure that they correspond back to the database file. When the shared memory copy of the database file ID does not match with the actual file ID of the database, this error is issued.

Action: Perform a MUPIP RUNDOWN on that region. If it fails with the same FILEIDGBLSEC error, then the shared memory contents are corrupt. Consult your YottaDB support channel before proceeding further.

-----------------------
FILEIDMATCH
-----------------------

FILEIDMATCH, Saved File ID does not match the current ID - the file appears to have been moved

Run Time Error: This indicates that the journal file identified by a database in turn identifies itself as belonging to another database. Since a journal file must have a one-to-one relationship with a database, the process cannot do updates on this region until the problem is resolved.

Action: Create a new journal file, make a backup if appropriate and resume work.

-------------------------
FILENAMETOOLONG
-------------------------

FILENAMETOOLONG, File name too long

Run Time/MUPIP Information: This indicates that YottaDB or MUPIP has encountered a file name exceeding the maximum permissible length.

Action: -

---------------------
FILENOTCREATE
---------------------

FILENOTCREATE, AAAA file xxxx not created

MUPIP Information: This indicates that the file xxxx was not created due to AAAA; where AAAA is a lost transaction, or broken transaction or Journal Extract file.

Action: Review accompanying messages for any further information. If there are no accompanying messages, it indicates that MUPIP did not find any lost or broken transactions for the corresponding extract file to be created.

------------------
FILENOTFND
------------------

FILENOTFND, File xxxx not found

Compile Time/Run Time Error: This indicates that YottaDB could not locate the specified source file xxxx.

Action: Look for a misspelling of the file-specification or improper preparation of the environment. If xxxx is a source file, it could have been moved or modified since the object in the image was compiled. Use ZLINK to make the object match the source.

--------------------
FILEOPENFAIL
--------------------

FILEOPENFAIL, Failed to open file ffff.

MUPIP Error: This message indicates that the MUPIP LOAD failed to open input file ffff.

Action: Please verify the path and permissions of input file ffff.

---------------------
FILEPARSE
---------------------

FILEPARSE, Error parsing file specification: xxxx

Run Time Error: This indicates a problem with the specification of file xxxx, or the path to it. If the file is a source or object file, a ZLINK command or $ZROUTINES-related action encountered the error.

Action: Look for and correct any typographical errors in the file-specification.

---------------------
FILERENAME
---------------------

FILERENAME, File xxxx is renamed to yyyy

Run Time Information: This indicates that an existing file xxxx has been renamed to yyyy so that a new file created with the original name does not overwrite the existing one. YottaDB renames files during an automatic journal switch in case no explicit journal file name is specified, in which case the message is sent to the operator log. The utilities (MUPIP, GT.CM) rename files while opening log files or journal extract files and they send the message to the terminal. YottaDB or utilities rename files only if the new file name specified already exists.

Action: This information messages confirms the success of the file rename operation. No futher action is necessary unless there are other WARNING, FATAL, and/or ERROR category messages.

-----------------------
FILTERBADCONV
-----------------------

FILTERBADCONV, Bad conversion of transaction xxxx by filter

Run Time Error: This error is logged to the replication server log file. This indicates that the output of the user-supplied external replication filter for the transaction with journal sequence number xxxx is incorrect.

Action: Fix the filter. Restart the replication server with the fixed filter.

----------------------
FILTERCOMM
----------------------

FILTERCOMM, Error communicating transaction xxxx with the filter

Run Time Error: This error is logged to the replication server log file. This indicates that the replication server encountered an error writing a transaction with a journal sequence number xxxx to the user supplied external filter's input. The accompanying system error message gives more details.

Action: Stop the filter and restart the replication server with the filter. Report the entire incident context to your YottaDB support channel.

--------------------
FILTERNOTALIVE
--------------------

FILTERNOTALIVE, Replication server detected that the filter is not alive while attempting to send transaction xxxx

Run Time Error: This error is logged to the replication server log file. This indicates that the replication server detected an abnormal termination of the user-supplied external filter while attempting to write a transaction with journal sequence number xxxx to the filter's input.

Action: Determine the cause of the filter's abnormal termination. Fix the filter and restart the replication server with the fixed filter.

------------------
FILTERTIMEDOUT
------------------

FILTERTIMEDOUT, Replication server timed out attempting to read seqno ssss from external filter

MUPIP Error: This indicates that either a Source or Receiver Replication Server using an external filter took more than 30 secs to read a transaction with a journal sequence number ssss from the user supplied external filter's output. The replication server reports this error in its log, stops the filter and terminates.

Action: Determine the cause for the filter's write delay. Fix the filter and restart the replication server with the fixed filter. If you cannot determine the reason for delay, report the entire incident context to your YottaDB support channel.

------------------
FMLLSTMISSING
------------------

FMLLSTMISSING, The formal list is absent from a label called with an actual list: xxxx

Compile Time/Run Time Error: This indicates that a DO attempted to transfer control with an actuallist to a label xxxx that has no formallist or that an extrinsic function was called with an actuallist (even if the list is empty) that has no formallist.

Action: Look at the interface between the DO and the subroutine or look at the extrinsic function. Modify the actuallist, formallist, and/or label as appropriate.

------------------
FNARGINC
------------------

FNARGINC, Format specifiers to $FNUMBER are incompatible: "xxxx"

Run Time Error: This indicates that a $FNUMBER function specified a format containing codes xxxx, which is incompatible code.

Action: Look for the character code "P" or "p" with any character other than the code ",".

----------------
FNNAMENEG
----------------

FNNAMENEG, Depth argument to $NAME cannot be negative

Run Time Error: This indicates that YottaDB encountered a $NAME() reference with the optional integer expression that is set to a negative number.

Action: Modify the routine to ensure that $NAME() arguments are never negative.

-----------------
FNOTONSYS
-----------------

FNOTONSYS, Function or special variable is not supported by this operating system

Compile Time Error: This indicates that YottaDB encountered a function or special variable that it could not process on the current operating system.

Action: Some functions are not appropriate to all operating environments. Contact your YottaDB support channel if you have questions about how to accomplish a particular task.

------------------
FNTRANSERROR
------------------

FNTRANSERROR, Buffer too small error occurred trying to translate filename FFFF

All YottaDB Components Error: While creating a database, resolving environment variables in a database path exceeded the maximum supported file name size.

Action: Reduce the path size by altering base components of the path or database name and/or the values of the environment variables to create a shorter overall filename and retry.

---------------------
FNUMARG
---------------------

FNUMARG, $FNUMBER format specifier xxxx contains an illegal character: yyyy

Run Time Error: This indicates that an $FNUMBER function specified a format xxxx that contains invalid codes.

Action: Ensure that the format specifier in a $FNUMBER function is a sequence of the code characters "P", "p", "T", "t", "+", "-", and ",".

-------------------
FORCEDHALT
-------------------

FORCEDHALT, Image HALTed by MUPIP STOP

Run Time Warning: This indicates that a YottaDB process recognized the receipt of a MUPIP STOP command and is terminating. This command stops YottaDB processes in an orderly fashion.

Action: Determine who initiated the MUPIP STOP and why they did so. Restart the process, if appropriate.

-------------------------
FOROFLOW
-------------------------

FOROFLOW, FOR commands nested too deeply

Compile Time Error: This indicates that a single line contained more than 32 FOR statements.

Action: Rework the routine so FORs are not so deeply nested in a single line.

--------------------------
FREEBLKSLOW
--------------------------

FREEBLKSLOW, Only bbbb free blocks left out of tttt total blocks for ffff

Run Time Warning: This message warns that database file ffff with automatic file extensions disabled has only bbbb blocks left out of a total allocation of tttt.

Action: Use MUPIP EXTEND to extend the file, or MUPIP SET to enable automatic extensions, or reduce the amount of data in the file.

-----------------------
FREEMEMORY
-----------------------

FREEMEMORY, Error occurred freeing memory

Run Time Error: Indicates an internal problem with storage management. The error is usually accompanied by a secondary message, which lists the reason towards the request failure.

Action: Report the entire incident context to your YottaDB support channel.

-------------------------
FREEZE
-------------------------

FREEZE, Region: xxxx is already frozen

DSE/MUPIP Information: MUPIP FREEZE or DSE CHANGE commands generate this error if the region is already frozen.

Action: If the existing FREEZE is appropriate, no further action is necessary. If you decide to remove the prior FREEZE, issue a FREEZE command with the OFF qualifier.

--------------------
FREEZECTRL
--------------------

FREEZECTRL, Control Y or control C encountered during attempt to freeze the database. Aborting freeze.

MUPIP Information: This indicates that the user aborted the MUPIP FREEZE or MUPIP BACKUP/NOONLINE command; while the command was attempting to freeze the already frozen database.

Action: -

-----------------------
FREEZEERR
-----------------------

FREEZEERR, Error while trying to ffff region rrrr

MUPIP Error: This indicates an unsuccessful database freeze or unfreeze (ffff) operation on region rrrr.

Action: Look for accompanying text that explains the cause of the error and take appropriate action.

-----------------------
FSEXP
-----------------------

FSEXP, File specification expected but not found

Run Time Error: This indicates that a $ZROUTINES-related action did not specify a valid file-specification.

Action: Look for missing brackets.

--------------------
FSYNCTIMOUT
--------------------

FSYNCTIMOUT, Timed out on fsync for journal file xxxx

Run Time Error: This indicates that the process has been unable to get the journal flush lock on the journal file for nearly two minutes and has timed out. It is very likely that another process is holding the journal flush lock, wanting to do an fsync() on the journal file, and has not yet released the lock, which would suggest an issue with disk subsystem response times.

Action: Check the disk subsystem for a software or hardware problem.

--------------------
FTOKERR
--------------------

FTOKERR, Error getting ftok of the file xxxx

Run Time Error: This indicates that YottaDB failed to take ftok of the database.

Action: Review the accompanying message(s) to identify the cause of the failure.

-------------------
FTOKKEY
-------------------

FTOKKEY, FTOK key 0xnnnn

MUPIP Information: This message reports additional information for an associated error which had trouble with the FTOK key 0xnnnn.

Action: Check "ipcs -s" for the given key, and see the associated error.

-------------------
GBLEXPECTED
-------------------

GBLEXPECTED, Global variable reference expected in this context

Run Time/MUPIP Error: This message indicates an attempt to apply a trigger definition to something other than a global variable.

Action: Review and correct the trigger definition.

--------------------
GBLMODFAIL
--------------------

GBLMODFAIL, Global variable Conflict Test failed. Failure code: xxxx.

Run Time Error: This indicates that a $ZQGBLMOD function encountered an integrity error while restoring from a failover. It is very likely that the database may have integrity errors or that the process-private data structures are corrupted.

Action: Report this error to the group responsible for database integrity at your operation.

--------------------
GBLNAMCOLLRANGE
--------------------

GBLNAMCOLLRANGE, Collation sequence #nnnn is out of range (0 thru 255)

GDE Error: This indicates that the collation sequence nnnn is out of the supported range of 0 thru 255.

Action: Specify a collation sequence number inside the supported range.

-----------------
GBLNAMCOLLUNDEF
-----------------

GBLNAMCOLLUNDEF, Error opening shared library of collation sequence #nnnn for GBLNAME gggg

GDE Error: This indicates that there was an error opening the shared library for collation sequence nnnn.

Action: Define the environment variable ydb_collate_<nnnn> to point to the shared library for collation sequence nnnn. Also ensure that the path to the library is readable and the library is usable on that platform.

---------------
GBLNAMCOLLVER
---------------

GBLNAMCOLLVER, Global directory indicates GBLNAME gggg has collation sequence #nnnn with a version #vvvv but shared library reports different version #llll

GDE Error: This indicates that the shared library for collation sequence nnnn reported the version as vvvv when the collation properties for global name gggg were first added by GDE into the global directory and that this invocation of GDE noticed the shared library reporting an incompatible version llll.

Action: See Action section for `COLLTYPVERSION error in the Message and Recovery Procedures Manual <./errors.html#colltypversion>`_.

-----------------
GBLNAME
-----------------

GBLNAME, Either an identifier or a left parenthesis is expected after a ^ in this context

Compile Time Error: This indicates that YottaDB encountered a circumflex in a valid location for a global variable name; however, the circumflex was not followed by a variable name or a left parenthesis.

Action: Look for unwanted circumflexes in expressions. Ensure that global variable names are valid.

-------------------
GBLNAMEIS
-------------------

GBLNAMEIS, in gblname gggg

GDE Information: This indicates the gblname where an out-of-range value was specified. This is usually a secondary message and is preceded by a VALTOOSMALL or VALTOOBIG error.

Action: Fixing the preceding error would automatically address this accompanying informational message.

-------------------
GBLNOEXIST
-------------------

GBLNOEXIST, Global xxxx no longer exists

MUPIP Information: The specified global variable does not exist in the database. This indicates that the global variable xxxx was present when MUPIP reorg started, but was killed when reorg was working on it.

Action: -

-----------------
GBLNOMAPTOREG
-----------------

GBLNOMAPTOREG, Global gggg does not map to region rrrr in current global directory

Run Time Error: This indicates that a VIEW "YDIRTREE" or $VIEW("YDIRTREE") was done with global gggg and region rrrr as parameters but the global does not map to that region in the current global directory.

Action: VIEW "YDIRTREE" or $VIEW("YDIRTREE") is an undocumented feature and so should NOT be used directly. $$get^%GBLDEF is the only tool that uses this but internally catches the GBLNOMAPTOREG error. This means the GBLNOMAPTOREG error message will never be visible to the end-user.

------------------
GBLOFLOW
------------------

GBLOFLOW, Database file FFFF is full

Run Time/MUPIP Error: This indicates that an error was encountered while extending the database file FFFF.

Action: Examine the accompanying message(s) for the cause of the error. If the error is due to insufficient authorization, address that. If the error is due to TOTALBLKMAX (refer to the explanation of that message) or a lack of enough free space on the disk to fit the size of a database file, try performing a KILL on some nodes in the database to get free blocks in the existing allocated space (you may need to KILL several subscripted nodes before you can KILL a name node).

---------------
GBLSECNOTGDS
---------------

GBLSECNOTGDS, Global section xxxx is not a YottaDB global section

Run Time Error: This indicates that when attempting to startup a database file, YottaDB encountered an existing global section whose contents it did not recognize.

Action: Investigate whether you have a global section name conflict between YottaDB and some other application. YottaDB uses GT$ as a prefix for all global section names that it creates. Make sure no other application in the system is using the same naming convention; it is very likely the global section contents are damaged. If necessary, report the entire incident context to your YottaDB support channel.

--------------
GDCREATE
--------------

GDCREATE, Creating global directory File xxxx

GDE Information: This indicates that an EXIT command caused GDE to write a new Global Directory into file xxxx.

Action: N/A

---------------
GDEASYNCIONOMM
---------------

GDEASYNCIONOMM, ssss segment has ASYNCIO turned on. Cannot support MM access method

GDE Error: This indicates that segment ssss has enabled ASYNCIO, which is not compatible with the MM access method.

Action: Do not use ASYNCIO for the MM access method; in order to choose ASYNCIO, use the BG access method.

-------------
GDECHECK
-------------

GDECHECK, Internal GDE consistency check

GDE Fatal: This indicates that an internal consistency check failed.

Action: Look in the user's current working directory for a GDEDUMP.DMP context file that your YottaDB support channel can examine to help determine the cause of the error. If necessary, report the entire incident context to your YottaDB support channel.

--------------
GDECRYPTNOMM
--------------

GDECRYPTNOMM, ssss segment has encryption turned on. Cannot support MM access method

GDE Error: This error is triggered by an attempt to mark an MM database segment ssss as encrypted with GDE. The MM access method is not supported for encrypted databases.

Action: Use the BG access method for encrypted files.

---------------
GDINVALID
---------------

GDINVALID, Unrecognized Global Directory file format: ffff, expected label: eeee, found: bbbb

Run Time Error: This indicates that a version of the global directory file ffff does not match with the version expected by YottaDB. The file might have been created by an incompatible YottaDB version. If the text of eeee or bbbb contain non-graphic characters, YottaDB replaces each of them with a period (.).

Action: Compare the labels eeee and bbbb. If the global directory was created by an earlier YottaDB version, upgrade the file by loading and then saving the file using the GDE of the new YottaDB version.

------------------
GDNOTSET
------------------

GDNOTSET, Global Directory not changed because the current GD cannot be written

GDE Information: This indicates that GDE could not complete a SETGD command because it could not verify the current Global Directory. This prevented a write of the current information.

Action: Either modify the current Global Directory or abandon it by adding the QUIT qualifier to the SETGD command.

------------------
GDREADERR
------------------

GDREADERR, Error reading Global Directory: xxxx

GDE Information: This indicates that GDE encountered an error when it attempted to read the existing Global Directory in file xxxx.

Action: Review the accompanying message(s) for additional information.


---------------
GDUNKNFMT
---------------

GDUNKNFMT, xxxx is not formatted as a global directory

GDE Information: This indicates that GDE could not load the specified file xxxx because it is not a valid Global Directory file. GDE aborts the load after it issues this message.

Action: Verify that the file is valid and look for typographical errors. Something other than YottaDB or its utilities may have written to the Global Directory file or created a file with a name that coincides with the one specified by GTM$GBLDIR/ydb_gbldir.

--------------
GDUPDATE
--------------

GDUPDATE, Updating Global Directory File xxxx

GDE Information: This indicates that an EXIT or SETGD command caused GDE to write a new version of the existing Global Directory in file xxxx.

Action: -


--------------
GDUSEDEFS
--------------

GDUSEDEFS, Using defaults for Global Directory xxxx

GDE Information: This indicates that GDE did not find an existing Global Directory using the logical name GTM$GBLDIR/ydb_gbldir. As a result, it is starting the session with default values.

Action: -

----------------
GETADDRINFO
----------------

GETADDRINFO, Error in getting address info

Run Time Error: This message indicates a problem converting a host name to an IP address.

Action: See associated TEXT message for more details. Check host names used for replication, backup, SOCKET devices, or GT.CM.

-----------------
GETNAMEINFO
-----------------

GETNAMEINFO, Error in getting name info

Run Time Error: This message indicates a problem converting an IP address to a readable format.

Action: See associated TEXT message for more details. Report the error to your YottaDB support channel.

------------------
GETSOCKNAMERR
------------------

GETSOCKNAMERR, Getting the socket name failed from getsockname(): (errno == aaaa) xxxx

Run Time Error: This indicates that the getsockname() system call, which retrieves the locally bound address of the specified socket, failed.

Action: Review the accompanying messages and error code.

-------------------
GETSOCKOPTERR
-------------------

GETSOCKOPTERR, Getting the socket attribute xxxx failed: (errno == yyyy) zzzz

Run Time Error: This indicates that an attempt to determine a socket's attributes failed.

Action: Consider the OPEN or USE deviceparameters and the error code.

-----------------
GOQPREC
-----------------

GOQPREC, Numeric precision in key error: Blk #xxxx, Key #yyyy. Record not loaded.

MUPIP Information: This indicates that YottaDB was unable to precisely represent a key in the GOQ input file to a MUPIP LOAD.

Action: Examine the key on the source system, modify it and repeat the process, or manually enter the modified record into YottaDB.


-------------------
GTMASSERT
-------------------

GTMASSERT, xxxx - assert failed yyyy line zzzz

Compile Time/Run Time Fatal: This indicates that a design assumption failed at the location specified.

Action: Preserve the core (dump) files and report the entire incident context to your YottaDB support channel for further analysis. If appropriate, verify database integrity by using the -FAST qualifier.

----------------
GTMASSERT2
----------------

GTMASSERT2, YottaDB eeee - Assert failed LLLL for expression (eeee)

Compile Time/Run Time Fatal: This indicates that a design assumption failed at the location LLLL because the expression eeee was FALSE.

Action: Preserve the core (dump) files and report the entire incident context to your YottaDB support channel for further analysis. If appropriate, verify database integrity by using the -FAST qualifier.

-----------------
GTMCHECK
-----------------

GTMCHECK, Internal YottaDB error. Report to YottaDB Support.

Compile Time/Run Time Fatal: This indicates that a design assumption failed at the location specified.

Action: Preserve the core (dump) files and report the entire incident context to your YottaDB support channel for further analysis. If appropriate, verify database integrity by using the -FAST qualifier.

-----------------
GTMDISTUNDEF
-----------------

GTMDISTUNDEF, Environmental variable $ydb_dist is not defined

DSE/Run Time/MUPIP/LKE Error: This indicates that the environment variable ydb_dist, is not defined for all processes attempting to use (a particular version of) YottaDB.

Action: Define the environment variable.

--------------------
GTMDISTUNVERIF
--------------------

GTMDISTUNVERIF, Environment variable $ydb_dist (dddd) could not be verified against the executables path (pppp)

MUPIP/LKE/GT.CM/DSE/Run Time Error: This indicates that the executable pppp does not reside in the path pointed to by the environment variable ydb_dist, dddd.

Action: Ensure that the setting for $ydb_dist matches that of the executable.

------------------
GTMEISDIR
------------------

GTMEISDIR, dddd : Is a directory

Run Time Error: The file dddd opened for reading is a directory. Directories cannot be opened for reading.

Action: Check the argument to the OPEN for the appropriate file and its path.

------------------
GTMSECSHR
------------------

GTMSECSHR, xxxx Error during GTMSECSHR operation

Run Time Error: This indicates that user privileges do not allow access to GTMSECSHR; or GTMSECSHR was not properly installed.

Action: Verify that SECSHR is properly installed and review user privileges in the SECSHR log file.

----------------
GTMSECSHRBADDIR
----------------

GTMSECSHRBADDIR, gtmsecshr is not running from $ydb_dist/gtmsecshrdir or $ydb_dist cannot be determined

Run Time Error: This message indicates an inappropriate gtmsecshr invocation. Either gtmsecshr is improperly installed or an inappropriate access attempt is underway.

Action: Verify that YottaDB (and gtmsecshr) are correctly installed following documented procedures and that filesystem mount points have not changed. If YottaDB is correctly installed and filesystem mount points have not changed, investigate this as an attempt to break system security.

--------------------
GTMSECSHRCHDIRF
--------------------

GTMSECSHRCHDIRF, GTMSECSHR process error: GTMSECSHR is not able to change directory to its temporary directory, dddd.

GTMSECSHR Error: The UNIX gtmsecshr process, which assists other process with cross-user signaling and similar things, uses a temporary directory determined by the operating-system-defined temporary directory (typically /tmp or /var/tmp) when it needs to save a core file, but it was unable to find that directory.

Action: Verify that the environment provides the desired dddd, that dddd exists and that it is a directory.

.. note::
   The permissions on dddd should not matter (as long as it is a directory) since GTMSECSHR runs as root.

------------------------
GTMSECSHRDMNSTARTED
------------------------

GTMSECSHRDMNSTARTED, [client pid pppp] File (ffff) removed

GTMSECSHR Information: This message indicates that GTMSECSHR removed file ffff on behalf of process pppp.

Action: N/A

------------------------
GTMSECSHRFORKF
------------------------

GTMSECSHRFORKF, GTMSECSHR server unable to fork off a child process

Run Time Error: This indicates that a GTMSECSHR was unsuccessful in starting because it was unable to create an independent process to run as a daemon.

Action: Refer to the associated message(s) for more information.

---------------------
GTMSECSHRGETSEMFAIL
---------------------

GTMSECSHRGETSEMFAIL, error getting semaphore errno = xxxx

GTMSECSHR Error: This error indicates that GTMSECSHR failed to obtain a semaphore set identifier for a specific IPC key during process termination, and that the error code returned by semget() is xxxx.

Action: The IPC resources that GTMSECSHR uses should be unique to YottaDB, and this message indicates an unexpected condition. Investigate whether other software is using IPC resources in a way that conflicts with YottaDB. If you can't find an explanation, report the entire incident context to your YottaDB support channel.

---------------------
GTMSECSHRISNOT
---------------------

GTMSECSHRISNOT, gtmsecshr is not running as gtmsecshr but xxxxx - must be gtmsecshr

Run Time Error: gtmsecshr is running with a name other than the one it is allowed to run by design.

Action: Verify that YottaDB (and gtmsecshr) are correctly installed following documented procedures and that filesystem mount points have not changed. If YottaDB is correctly installed and filesystem mount points have not changed, investigate this as an attempt to break system security.

------------------
GTMSECSHRNOARG0
------------------

GTMSECSHRNOARG0, gtmsecshr cannot identify its origin - argv[0] is null

Run Time Error: This message occurs when gtmsecshr is called in an inappropriate manner by facilities other those allowed by design (like the gtmsecshr wrapper).

Action: Investigate this as an attempt to break system security.

--------------------
GTMSECSHROPCMP
--------------------

GTMSECSHROPCMP, GTMSECSHR operation may be compromised

Run Time Error: This indicates that GTMSECSHR could not acquire the privileges required to assist more than a restricted set of processes.

Action: If this is the proper mode of operation, ignore the warning. Normally GTMSECSHR should be a setuid executable owned by root.

-------------------
GTMSECSHRPERM
-------------------

GTMSECSHRPERM, The GTMSECSHR module in $ydb_dist (DDDD) does not have the correct permission and UID (permission: PPPP, and UID: UUUU)

Run Time Warning: This indicates that a client did not start a GTMSECSHR, installed to DDDD, because the executable was not owned by root (UUUU is the actual owner) and/or did not have setuid and/or execute permissions (actual permissions are PPPP).

Action: Arrange to provide the GTMSECSHR executable with the proper characteristics. The executable must be SETUID root with execute permissions for the current user.

-------------------
GTMSECSHRRECVF
-------------------

GTMSECSHRRECVF, GTMSECSHR receive on server socket failed

Run Time Error: This indicates that a receive operation failed in GTMSECSHR.

Action: Refer to the associated message(s) for more information.

--------------------
GTMSECSHRREMFILE
--------------------

GTMSECSHRREMFILE, [client pid pppp] File (ffff) removed

GTMSECSHR/Operator log Information: This message indicates that GTMSECSHR removed file ffff on behalf of process pppp.

Action: -

-------------------
GTMSECSHRREMSEM
-------------------

GTMSECSHRREMSEM, [client pid pppp] Semaphore (ssss) removed

GTMSECSHR Error: This message indicates that GTMSECSHR removed a semaphore identified by the key ssss on behalf of process pppp.

Action: This is benign. No action necessary.

-------------------
GTMSECSHRREMSEMFAIL
-------------------

GTMSECSHRREMSEMFAIL, error removing semaphore errno = xxxx

GTMSECSHR Error: This error indicates that GTMSECSHR failed to remove a semaphore set identified by a specific IPC key during process termination, and that the error code returned by semctl() is xxxx.

Action: The IPC resources that GTMSECSHR uses should be unique to YottaDB, and this message indicates an unexpected condition. Investigate whether other software is using IPC resources in a way that conflicts with YottaDB. If you can't find an explanation, report the entire incident context to your YottaDB support channel.

------------------
GTMSECSHRREMSHM
------------------

GTMSECSHRREMSHM, [client pid pppp] Shared memory segment (ssss) removed, nattch = nnnn

GTMSECSHR Information: This message indicates that GTMSECSHR removed a shared memory segment identified by the key ssss on behalf of process pppp, and that there were nnnn processes attached to that segment.

Action: N/A

-----------------
GTMSECSHRSCKSEL
-----------------

GTMSECSHRSCKSEL, GTMSECSHR select on socket failed

Run Time Error: This indicates that a select operation failed in GTMSECSHR.

Action: Refer to the associated message(s) for more information.

-----------------
GTMSECSHRSEMGET
-----------------

GTMSECSHRSEMGET, semget error errno = xxxx

GTMSECSHR Error: This error indicates that the GTMSECSHR process failed to obtain a semaphore set identifier for a specific IPC key, and that the error code returned by semget() is xxxx.

Action: Consult your system administrator to ensure semaphores are appropriately configured.

-----------------
GTMSECSHRSENDF
-----------------

GTMSECSHRSENDF, GTMSECSHR send on server socket failed

Run Time Error: This indicates that a socket operation failed in a GTMSECSHR.

Action: Refer to the associated message(s) for more information.

-----------------
GTMSECSHRSGIDF
-----------------

GTMSECSHRSGIDF, GTMSECSHR server setGID to root failed

Run Time Error: This indicates that the setgid operation failed during GTMSECSHR startup.

Action: Refer to the associated message(s) for more information.

--------------------
GTMSECSHRSHMCONCPROC
--------------------

GTMSECSHRSHMCONCPROC, More than one process attached to Shared memory segment (ssss) not removed (nnnn)

GTMSECSHR Error: This error indicates that the shared memory segment identified by the key ssss has not been removed because nnnn processes are currently attached to it.

Action: The IPC resources that GTMSECSHR uses should be unique to YottaDB, and this message indicates an unexpected condition. Investigate whether other software is using IPC resources in a way that conflicts with YottaDB. If you can't find an explanation, report the entire incident context to your YottaDB support channel.

--------------------
GTMSECSHRSHUTDN
--------------------

GTMSECSHRSHUTDN, GTMSECSHR process has received a shutdown request. Shutting down.

Run Time Information: This indicates that the GTMSECSHR daemon has shutdown.

Action: -

-----------------
GTMSECSHRSOCKET
-----------------

GTMSECSHRSOCKET, xxxx - yyyy; Error initializing GTMSECSHR socket

Run Time Error: This indicates that a YottaDB process or GTMSECSHR with PID yyyy was unable to open a socket for communication with either the server or client.

Action: Refer to the associated message(s) for more information.

------------------
GTMSECSHRSRVF
------------------

GTMSECSHRSRVF, Client - yyyy; Attempt to service request failed (retry = zzzz)

Run Time Error: This indicates that a YottaDB process with PID yyyy was unable to communicate with gtmsecshr after zzzz attempts (a maximum of four retries).

Action: This message is displayed when a process that needs service from gtmsecshr cannot communicate with gtmsechsr, or cannot start one. While the most likely cause is a mismatch in the value of the ydb_tmp environment variable between the YottaDB process and the gtmsecshr process, examples of other causes include the removal of socket files used for communication between YottaDB and gtmsecshr processes. Check for a following associated message in syslog or in the stderr of the YottaDB process.

-----------------
GTMSECSHRSRVFID
-----------------

GTMSECSHRSRVFID, xxxx: yyyy - Attempt to service request failed. Client ID: zzzz, mesg ID: aaaa, mesg code: bbbb

Run Time Warning: This indicates that the GTMSECSHR was unable to complete the request of the YottaDB client.

Action: Examine the information in the message to see whether the message is appropriate to the environment; examine the environment and correct any inappropriate set up (such as the privileges available for GTMSECSHR).

-----------------
GTMSECSHRSRVFIL
-----------------

GTMSECSHRSRVFIL, xxxx: yyyy; Attempt to service request failed. Client ID: zzzz, mesg type: aaaa, file: bbbb

GTMSECSHR Warning: This indicates that the GTMSECSHR was unable to complete the request of the YottaDB client.

Action: Review accompanying message(s) for information on why GTMSECSHR could not delete the file.


-----------------
GTMSECSHRSSIDF
-----------------

GTMSECSHRSSIDF, GTMSECSHR server setSID failed

Run Time Error: This indicates that the setsid operation failed during GTMSECSHR startup.

Action: Refer to the associated message(s) for more information.

------------------
GTMSECSHRSTART
------------------

GTMSECSHRSTART, xxxx - yyyy; GTMSECSHR failed to startup

Run Time Warning: This indicates that GTMSECSHR startup failed.

Action: Refer to the associated message(s) for more information.

------------------
GTMSECSHRSUIDF
------------------

GTMSECSHRSUIDF, GTMSECSHR server setUID to root failed

Run Time Error: This indicates that the setuid operation failed during GTMSECSHR startup.

Action: Refer to the associated message(s) for more information.

-----------------
GTMSECSHRTMOUT
-----------------

GTMSECSHRTMOUT, GTMSECSHR exiting due to idle timeout

Run Time Information: This indicates that the GTMSECSHR had been idle long enough to time out and terminate.

Action: No action is required, another GTMSECSHR is started when it is needed.

-----------------
GTMSECSHRTMPPATH
-----------------

GTMSECSHRTMPPATH, gtmsecshr path is pppp

Information: YottaDB displays this message when different users of an instance of YottaDB connect using a socket or a semaphore and when gtmsecshr is started and it detects an existing gtmsecshr. pppp indicates the ydb_tmp path set in the clients. Gtmsecshr inherits the path from the first YottaDB process that uses its services.

Action: If different clients of the same instance of YottaDB are using different gtmsecshr paths, then set a common value for the environment variable ydb_tmp for all users of an instance of YottaDB, then stop and restart the processes that were using incorrect paths. If gtmsecshr itself has the incorrect path, all processes that are using that incorrect path must be stopped first - then stop gtmsecshr with a kill command.

--------------------
GTMSECSHRUPDDBHDR
--------------------

GTMSECSHRUPDDBHDR, [client pid pppp] database fileheader (dddd) updated iiii

GTMSECSHR Information: This message indicates that GTMSECSHR updated database fileheader dddd on behalf of process pppp for the purpose of iiii.

Action: N/A

--------------------
GVDATAFAIL
--------------------

GVDATAFAIL, Global variable $DATA function failed. Failure code: xxxx

Run Time Error: This indicates that a $DATA function encountered a database problem. xxxx contains the failure codes for the four attempts. It is very likely that the database may have integrity errors or that the process-private data structures are corrupted.

Action: Report this database error to the group responsible for database integrity at your operation.

-----------------
GVDATAGETFAIL
-----------------

GVDATAGETFAIL, Global variable DATAGET sub-operation (in KILL function) failed. Failure code: cccc.

Trigger/Run Time Error: The target node for a KILL operation could not present its state to the trigger logic due to a database problem. cccc contains the failure codes for the failed attempts. The database may have integrity errors or the process-private data structures may be corrupted.

Action: Report this database error to the group responsible for database integrity at your operation.


----------------------
GVFAILCORE
----------------------

GVFAILCORE, A core file is being generated for later analysis if necessary

Run Time Error: This is an operator log-only message, which indicates that a core (dump) is being generated for the immediately preceding xxxxFAIL error message.

Action: Report this database error to the group responsible for database integrity at your operation. If the cause of the xxxxFAIL message is not otherwise known (for example, database damage due to recent system crash), the produced core will contain information that your YottaDB support channel can use to determine the source of failure.

------------------
GVGETFAIL
------------------

GVGETFAIL, Global variable retrieval failed. Failure code: xxxx.

Run Time Error: This indicates that a database lookup of a global variable encountered an error. xxxx contains the failure codes for the four attempts. It is very likely that the database may have integrity errors or that the process-private data structures are corrupted.

Action: Report this database error to the group responsible for database integrity at your operation.

-------------------
GVINCRFAIL
-------------------

GVINCRFAIL, Global variable $INCR failed. Failure code: xxxx

Run Time Error: This indicates that a $INCREMENT command encountered a database problem when it attempted to update a global variable. xxxx contains the failure codes for the four attempts. It is very likely that the database may have structural damage or that the process-private data structures are corrupted.

Action: Report this database error to the group responsible for database integrity at your operation.

--------------------
GVINVALID
--------------------

GVINVALID, xxxx Invalid global name

MUPIP Error: This indicates that LOAD encountered invalid global name xxxx in the input stream.

Action: Refer to the topic `MUPIP LOAD Errors in the About This Manual section <./about.html#mupip-load-errors>`_.

------------------
GVIS
------------------

GVIS, Global variable: xxxx

Run Time Information: This message identifies a global variable.

Action: Refer to the accompanying message(s) for more information.

------------------
GVKILLFAIL
------------------

GVKILLFAIL, Global variable kill failed. Failure code: xxxx.

Run Time Error: This indicates that a KILL of a global variable encountered a database problem. xxxx contains the failure codes for the four attempts. It is very likely that the database may have integrity errors or that the process-private data structures are corrupted.

Action: Report this database error to the group responsible for database integrity at your operation.

------------------
GVNAKED
------------------

GVNAKED, Illegal naked global reference

Run Time Error: This indicates that the naked indicator was referenced before any named global reference or after an event that left it undefined.

Action: Review naked indicator references and correct them, if necessary. For example, the naked indicator cannot be the first global symbol referenced.

------------------
GVNAKEDEXTNM
------------------

GVNAKEDEXTNM, Cannot reference different Global Directory in a naked reference

Compile Time Error: This indicates that a global variable reference used the environment syntax but did not specify a name.

Action: Verify that the environment specifies a full global name.

-------------------
GVNEXTARG
-------------------

GVNEXTARG, Argument to global variable $NEXT must be subscripted

Compile Time Error: This indicates that an attempt was made to use an un-subscripted global or local variable as the argument for a $NEXT() function. In contrast to $ORDER(), which can operate on un-subscripted names, $NEXT() requires subscripted names.

Action: Use $ORDER() or revise the code.

---------------
GVORDERFAIL
---------------

GVORDERFAIL, Global variable $ORDER or $NEXT function failed. Failure code: xxxx.

Run Time Error: This indicates that a $ORDER or $NEXT function encountered a database problem. xxxx contains the failure codes for the four attempts. It is very likely that the database may have integrity errors or that the process-private data structures are corrupted.

Action: Report this database error to the group responsible for database integrity at your operation.

-----------------
GVPUTFAIL
-----------------

GVPUTFAIL, Global variable put failed. Failure code: xxxx.

Run Time Error: This indicates that a SET command encountered a database problem when it attempted to update a global variable. xxxx contains the failure codes for the four attempts. It is very likely that the database may have integrity errors or that the process-private data structures are corrupted.

Action: Report this database error to the group responsible for database integrity at your operation.

-------------------
GVQUERYFAIL
-------------------

GVQUERYFAIL, Global variable $QUERY function failed. Failure code: xxxx.

Run Time Error: This indicates that a $QUERY function failed. xxxx contains the failure codes for the four attempts. It is very likely that the database may have integrity errors or that the process-private data structures are corrupted.

Action: Report this database error to the group responsible for database integrity at your operation.

-------------------
GVQUERYGETFAIL
-------------------

GVQUERYGETFAIL, Global variable QUERY and GET failed. Failure code: xxxx.

Run Time Error: This indicates that database query and Lookup in the same atomic transaction encountered a problem. xxxx contains the failure codes for the four attempts.

Action: Contact the system administrator and if needed, report to your YottaDB support channel.

-------------------
GVREPLERR
-------------------

GVREPLERR, Error replicating global in region xxxx

Run Time Error: This indicates that the database system successfully updated a global node on the primary copy of the database but it encountered an error making the same update on a replicated copy.

Action: Examine any secondary error. Investigate whether the problem is with the disk where the copy is located or with the communications system to the secondary copy, if it is remote. Correct the problem and resynchronize the copies.

--------------------
GVRUNDOWN
--------------------

GVRUNDOWN, Error during global database rundown

Run Time Error: This indicates that the process encountered an error when it attempted to RUNDOWN all database files as part of image termination.

Action: Report this database error to the group responsible for database integrity at your operation.

--------------------
GVSUBOFLOW
--------------------

GVSUBOFLOW, Maximum combined length of global subscripts exceeded

Run Time Error: This indicates that a subscripted global variable reference required a key size (which includes the length of the specified global variable name and subscripts) that exceeds the maximum key size specified in the database file header for the region mapping this subscripted global reference. This message is accompanied by a GVIS message which indicates the subscripted global reference. If the subscripted global reference in the GVIS message has a * at the end, it indicates a truncated part of the specified global reference. If it does not have a * at the end, it accurately identifies the complete specified global reference. Note that if the unsubscripted global variable name exceeds the maximum key size, a KEY2BIG error is issued instead of a GVSUBOFLOW error.

Action: Use $VIEW("REGION") to identify the region corresponding to the specified subscripted global reference and DSE DUMP -FILE to identify the Maximum key size of the corresponding region. Specify smaller subscripted global references OR use MUPIP SET -REGION command with the -KEY_SIZE qualifier to modify the maximum key size as required by the application.

------------------
GVUNDEF
------------------

GVUNDEF, Global variable undefined: xxxx

Run Time Error: This indicates that the program attempted to evaluate an undefined global variable.

Action: Review the program flow and the preparation of the environment.

-------------------
GVZPREVFAIL
-------------------

GVZPREVFAIL, Global variable $ZPREVIOUS function failed. Failure code: xxxx

Run Time Error: This indicates that a $ZPREVIOUS function encountered a database problem. xxxx contains the failure codes for the four attempts. It is very likely that the database may have integrity errors or that the process-private data structures are corrupted.

Action: Report this database error to the group responsible for database integrity at your operation.

--------------------
GVZTRIGFAIL
--------------------

GVZTRIGFAIL, ZTRIGGER of a global variable failed. Failure code: cccc.

Run Time Error: A ZTRIGGER command failed because of problems in the database. cccc is a list of four codes indicating the reason for the failure on each of the attempts to commit the ZTRIGGER action.

Action: Report this database error to the group responsible for database integrity at your operation.

-------------------
HLPPROC
-------------------

HLPPROC, Helper Process error

MUPIP Error: YottaDB replication was not able to start a helper process.

Action: Ensure that the ydb_dist environment variable points to a valid YottaDB distribution that is executable by the user.

-------------------
HOSTCONFLICT
-------------------

HOSTCONFLICT, Host hhhh could not open database file dddd because it is marked as already open on node nnnn

Run Time Error: The database file (dddd) has already been opened by a host (nnnn) other than the local host (hhhh).

Action: Ensure that host nnnn has closed dddd. Make sure both host names are correct. Changing host names in the middle of a database access can cause this error.

-------------------
HTOFLOW
-------------------

HTOFLOW, Hash table overflow, local or region name space exceeded

Run Time/MUPIP Error: This indicates that the hash table contains too many local names or region names.

Action: Reduce the number of local and region name entries in the table.

-------------
HTSHRINKFAIL
-------------

HTSHRINKFAIL, Hash table compaction failed to allocate new smaller table due to lack of memory

Run Time Error: YottaDB found an internal hash table over-allocated but was unable to reduce its size because the process memory was too large to allocate a new smaller table; YottaDB must allocate the new table before it can release the old table because it must copy the contents out of the "too-large" table into the smaller one. After this warning, the process continues running with the larger table.

Action: Investigate whether the process size can be reduced, or the available memory increased.

------------------
ICUERROR
------------------

ICUERROR, ICU returned status ssss which is either unrecognized or inconsistent with the operating context

Run Time Error: The open-source ICU module which YottaDB uses for some Unicode processing returned an error code ssss that YottaDB did not recognize as valid for the current context.

Action: Consult the ICU documentation and/or refresh the ICU library with a known correct version.

--------------------
ICUNOTENABLED
--------------------

ICUNOTENABLED, ICU libraries not loaded

Run Time Warning: The operation required the library containing support for International Components for Unicode (ICU) but YottaDB could not find libicu. There may be other messages.

Action: If you require UTF-8 support, install an appropriate ICU library - see the Administration and Operations Guide for information on ICU setup.

--------------------
ICUSYMNOTFOUND
--------------------

ICUSYMNOTFOUND, Symbol xxxxx not found in ICU libraries. ICU needs to be built with symbol-renaming disabled or xxxxx environment variable needs to be specified

Run Time Error: The ICU version installed on the machine is built with symbol renaming and neither ydb_icu_version nor gtm_icu_version has been defined.

Action: Build ICU without symbol renaming or set ydb_icu_version environment variable to point to an appropriate ICU version.

-------------------
ICUVERLT36
-------------------

ICUVERLT36, Type 1 - $ydb_icu_version/$gtm_icu_version is aaa.bbb. ICU version greater than or equal to 3.6 should be used. Type 2 - libicuio has version aaa.bbb. ICU version greater than or equal to 3.6 should be used.

Run Time Error: This message indicates an attempt to use an ICU version that is less than 3.6 with YottaDB or utilities like MUPIP or DSE.

Action: Upgrade ICU version to at least 3.6.

--------------------
IFBADPARM
--------------------

IFBADPARM, External Interface Bad Parameter

Run Time Error: This indicates that an external routine could not access a YottaDB database library routine because it had an invalid parameter in its call argument list. The YottaDB database library routines allow an external routine to access a YottaDB database.

Action: Look for and correct any typographical errors in the call format for the YottaDB library routine.

-------------------
IFNOTINIT
-------------------

IFNOTINIT, External Interface must first call GTM$INIT or M routine

Run Time Error: This indicates that an external routine could not access a YottaDB database library routine because it did not call the GTM$INIT library routine first. The GTM$INIT library routine initializes the YottaDB run-time environment.

Action: Call GTM$INIT or a YottaDB M routine before calling any other database access library routine.

-------------------
IGNBMPMRKFREE
-------------------

IGNBMPMRKFREE, Ignoring bitmap free-up operation for region rrrr (dddd) due to concurrent ONLINE ROLLBACK

Run Time Information: A multi-node KILL bit map cleanup operation detected a concurrent online rollback in region rrrr mapped to database file dddd and abandoned the cleanup, possibly leaving incorrectly marked busy errors.

Action: If there are incorrectly marked busy errors, match them with this cause and clean them up using DSE.

---------------------
ILLCHAR
---------------------

ILLCHAR, xxxx is not a legal character in this context

GDE Information: This indicates that GDE encountered the invalid character xxxx in its command input stream. This character should never appear in the context in which it was found.

Action: Review and re-enter a valid command sequence.

---------------------
ILLEGALUSE
---------------------

ILLEGALUSE, Illegal use of the special character "?" in %GSEL

Utility Error: This is an illegal use of the special character "?" in %GSEL. The special character "?" is not valid as the first character of a global name search pattern. "?" only valid as the first character of a search pattern when invoking the commands "?D" or "?d".

Action: Review and re-enter a valid search pattern.

------------------
ILLESOCKBFSIZE
------------------

ILLESOCKBFSIZE, The specified socket buffer size is xxxx, which is either 0 or too big

Run Time Error: This indicates that the OPEN command specified an inappropriate buffer size.

Action: Revise the command.

-----------------
IMAGENAME
-----------------

IMAGENAME, The executing module name should be xxxx instead of yyyy

Run Time Error: This indicates that the executable invoked should have been named xxxx instead of its current name yyyy.

Action: Revisit the YottaDB installation.

------------------
INDEXTRACHARS
------------------

INDEXTRACHARS, Indirection string contains extra trailing characters

Compile Time Error: This indicates that an indirection string ends with a syntactically incorrect sequence.

Action: Look for extra trailing characters in the indirection string.

-------------------
INDRCOMPFAIL
-------------------

INDRCOMPFAIL, Compilation of indirection failed

Run Time Error: This indicates that an indirection or XECUTE command failed due to syntax errors.

Action: Review the code and make sure the indirection or XECUTE string has valid syntax and contains no non-graphic characters. Consider using $ZWRITE to identify any such characters.

--------------------
INDRMAXLEN
--------------------

INDRMAXLEN, Maximum length xxxxx of an indirection argument was exceeded

Run Time Error: This indicates that an indirection or XECUTE used a value that exceeded the maximum length for a source code element.

Action: Review the code to shorten the length of the XECUTE or indirection string.

--------------------
INITORRESUME
--------------------

INITORRESUME, UPDATERESYNC on a Supplementary Instance must additionally specify INITIALIZE or RESUME

Receiver Server Log/MUPIP Error: Issued by a Receiver Server when started with -UPDATERESYNC on a Supplementary Instance which allows local updates, but started without specifying either -INITIALIZE or -RESUME.

Action: Additionally specify -INITIALIZE if this is the first time this supplementary instance is connecting to the source side OR if the receiver side databases have been refreshed from a backup of the source side. If on the other hand, the receiving instance had already been replicating from the source before and only had its instance file recreated in between, -RESUME might be appropriate with the -UPDATERESYNC. Check the documentation on -RESUME for more details.


-------------------
INPINTEG
-------------------

INPINTEG, Input integrity error -- aborting load

GDE Fatal: This indicates that GDE is aborting the session because integrity errors prevented it from loading the specified Global Directory. GDE usually displays this message with other error messages. GDE aborts the load after issuing this message.

Action: Review the accompanying message(s) for additional information. Verify whether the command specified the intended file. Something other than YottaDB and its utilities probably wrote to a Global Directory file or created a file with a name identical to the one specified by GTM$GBLDIR/ydb_gbldir.

-------------------
INSNOTJOINED
-------------------

INSNOTJOINED, Replicating Instance RRRR is not a member of the same Group as Instance IIII

Receiver Server log/MUPIP Error: A Receiver Server or a MUPIP JOURNAL -ROLLBACK -FETCHRESYNC on instance RRRR produces this error when it attempts to establish a replication connection with an instance that belongs to a different replication configuration or Group. MUPIP performs this safety check at the time it establishes a replication connection between two instances.

Action: Use the Remote IP Address in the Receiver/Source Server log files or the primary instance name field from MUPIP REPLICATE -JNLPOOL -SHOW command to identify the Source Server that may have inadvertently attempted to establish a replication connection with your Source Server. Shut down the Source Server if the Source Server does not belong to your replication configuration. If you are attempting to move a Source Server from a different Group, reinitialize the Source Server. Note that only supplementary instances started with -UPDOK can accept updates from a different Group.

--------------------
INSROLECHANGE
--------------------

INSROLECHANGE, Supplementary Instance SSSS and non-Supplementary Instance IIII belong to the same Group

Receiver Server log/MUPIP Error: Issued by a Receiver Server or a MUPIP JOURNAL -ROLLBACK -FETCHRESYNC on Supplementary Instance SSSS attempted to connect to non-Supplementary Instance IIII, but found they have the same Group identification. Because supplementary and non-Supplementary Instances cannot belong to the same Group, one of these instances must have changed roles without appropriate re-initialization.

Action: Either reinitialize the instance that is changing roles or revert the inappropriate role change.

-------------------
INSTFRZDEFER
-------------------

INSTFRZDEFER, Instance Freeze initiated by eeee error on region rrrrr deferred due to critical resource conflict.

Run Time Information: eeee error encountered on region rrrrr triggered the Instance Freeze mechanism in an attempt to set the freeze, but couldn't complete the freeze due to a critical resource conflict. Any process subsequently attempting an update will reattempt the freeze later until one succeeds or the error subsides.

Action: None necessary.

---------------------
INSUFFSUBS
---------------------

INSUFFSUBS, Return subscript array for an API call too small.

Run Time Error: This indicates that the return subscript array needs more entries for the ydb_node_next_s() or ydb_node_previous_s() SimpleAPI call than is currently allocated (specified by the input/output parameter \*ret_subs_used). In this case \*ret_subs_used is set to the needed entries.

Action: Ensure the return subscript array ("ret_subsarray" parameter of ydb_node_next_s() or ydb_node_previous_s()) is allocated with at least \*ret_subs_used entries and retry the ydb_node_next_s() or ydb_node_previous_s() call.

-----------------------
INSUNKNOWN
-----------------------

INSUNKNOWN, Supplementary Instance SSSS has no instance definition for non-Supplementary Instance IIII

Receiver Server log/MUPIP Error: Issued by a Receiver Server or a MUPIP JOURNAL -ROLLBACK -FETCHRESYNC on Supplementary Instance SSSS, that started with -UPDOK and attempted to connect to non-Supplementary Instance IIII, but found it has no matching instance information.

Action: Take a backup of the database and replication instance file from a current instance on the non-Supplementary Group, load the backup data on the Supplementary Instance and start the Receiver Server on the supplementary instance using -UPDATERESYNC=<instbak.repl> where instbak.repl is the backup of the replication instance file taken along with the database backup.

------------------------
INTEGERRS
------------------------

INTEGERRS, Database integrity errors

MUPIP Error: This indicates that INTEG encountered one or more errors in the database file.

Action: Review the accompanying errors for more information, and report this database error to the group responsible for database integrity at your operation.

-------------------
INVACCMETHOD
-------------------

INVACCMETHOD, Invalid access method

MUPIP Error: This indicates that the user specified an invalid access method in a MUPIP SET command.

Action: This command can only use Memory Map (MM) or Buffered Globals (BG) access methods.

------------------
INVADDRSPEC
------------------

INVADDRSPEC, Invalid IP address specification

Run Time Error: This indicates the IP address and/or port specified is not in a valid format.

Action: Verify and correct the IP address and port.

-----------------
INVALIDGBL
-----------------

INVALIDGBL, Search pattern is invalid in %GSEL

Utility Error: The search pattern used is invalid due to either using invalid characters or improper formatting.

Action: Review and re-enter a valid search pattern

-----------------
INVALIDRIP
-----------------

INVALIDRIP, Invalid read-in-progress field in Cache Record. Resetting and continuing. Region: xxxx.

Run Time Error: This indicates that the read-in-progress field corresponding to a particular global buffer had an invalid value. The read-in-progress field usually indicates whether this global buffer is currently being read into from disk or not, and hence takes on two values only. Whenever the field takes on any value outside of these two, YottaDB detects the situation and corrects it in addition to logging this incident in the operator log.

Action: Report the operator log message with any other relevant information to your YottaDB support channel.

--------------------
INVBITLEN
--------------------

INVBITLEN, Invalid size of the bit string

Run Time Error: This indicates that an attempt was made to create a bit string of size less than 1 or more than 253,952 bits.

Action: Modify the code so it adheres to the permitted range.

---------------------
INVBITPOS
---------------------

INVBITPOS, Invalid position in the bit string

Compile Time/Run Time Error: This indicates that YottaDB encountered a bit position argument to a $ZBITGET or $ZBITSET function that exceeded the length of the bit string, or was less than one (1).

Action: Use $ZBITLEN() to modify the code so the bit reference falls within the allocated length of the bit string.

-------------------
INVBITSTR
-------------------

INVBITSTR, Invalid bit string

Run Time Error: This indicates that an attempt was made to use a bit string with a leading character that was not within the permitted range of values.

Action: Determine the source for the first character of the bit string and modify the method of its creation to limit possible values to the valid range of 0 to 7.

-------------------
INVCMD
-------------------

INVCMD, Invalid command keyword encountered

Compile Time Warning: This indicates that the program attempted to use an invalid keyword where a command was expected.

Action: Look for typographical errors or improper command abbreviations.

-----------------
INVCTLMNE
-----------------

INVCTLMNE, Invalid control mnemonics

Run Time Error: The current device does not support the specified control mnemonic.

Action: Check the spelling of the control mnemonic, and be sure the mnemonicspace (if any) for the current device supports the requested usage of the control mnemonic.

------------------
INVDLRCVAL
------------------

INVDLRCVAL, Invalid $CHAR() value.

Run Time Error: The $CHAR() function triggers this error if its arguments contains an invalid code-point. According to the Unicode Standard version 5.0, invalid code-points include the following sets:

- The "too big" code-points (those greater than the maximum U+10FFFF).

- The "surrogate" code-points (in the range [U+D800, U+DFFF]) which are reserved for UTF-16 encoding.

- The "non-character" code-points that are always guaranteed to be not assigned to any valid characters. This set consists of [U+FDD0, U+FDEF] and all U+nFFFE and U+nFFFF (for each n from 0x0 to 0x10).

Action: Specify the argument in the range of valid Unicode code-points.


----------------------
INVECODEVAL
----------------------

INVECODEVAL, Invalid value for $ECODE (xxxx).

Run Time Error: This indicates that an attempt was made to assign $ECODE an invalid value. Such an action modifies $ECODE to have a valid value indicating this error, which triggers an error trap.

Action: Revise the SET $ECODE value complies with the required specification of the error codes of the format ,Mnnn,Zxxx,Uxxx,. The error codes in the form of ,Mnnn, ,Zxxx, and ,Uxxx, represent ANSI standard codes, implementation-specific codes and end-user defined codes respectively.

-------------------------
INVERRORLIM
-------------------------

INVERRORLIM, Invalid ERROR_LIMIT qualifier value. Must be at least zero

MUPIP Error: This indicates that the value assigned to the ERROR_LIMIT qualifier is negative (less than zero).

Action: Assign a value greater than zero (0) for ERROR_LIMIT qualifier.

------------------------
INVFCN
------------------------

INVFCN, Invalid function name

Compile Time Error: This indicates that an expression contained a string of the form "$name(...)", but "name" was not a valid function name.

Action: Look for typographical errors, improper function name abbreviation, or a missing $ in an extrinsic.

-------------------
INVGBLDIR
-------------------

INVGBLDIR, Invalid Global Directory spec: xxxx. Continuing with yyyy.

GDE Information: This indicates that the Global Directory xxxx specified by GTM$GBLDIR/ydb_gbldir or by SETGD and the qualifier FILE= is not a valid file-specification.

Action: When this error occurs, GDE uses the default specification of the current process default directory. Continue with the default and rename the result after leaving GDE, or change the specification with a SETGD command and the FILE= qualifier.

-------------------
INVGLOBALQUAL
-------------------

INVGLOBALQUAL, Error in GLOBAL qualifier : Parse error at offset xxxx in yyyy

MUPIP Error: This indicates a syntax error in GLOBAL qualifier value yyyy at offset xxxx.

Action: Specify correct value for GLOBAL.

----------------
INVGVPATQUAL
----------------

INVGVPATQUAL, Invalid Global Value Pattern file qualifier value

MUPIP Error: This indicates that -GVPATFILE did not specify a valid file name. The maximum file name length is 256.

Action: Specify a valid file name with the appropriate path.

--------------------
INVIDQUAL
--------------------

INVIDQUAL, Invalid ID qualifier value xxxx

MUPIP Error: This indicates that an invalid value xxxx was assigned to the ID qualifier.

Action: Assign a valid value for ID qualifier.

-------------------
INVINTMSG
-------------------

INVINTMSG, Invalid interrupt message received

GT.CM Error: An invalid interrupt request was received. This may indicate a network problem.

Action: Check the DECnet error logs and other network components.


-------------------
INVLINKTMPDIR
-------------------

INVLINKTMPDIR, Value for $ydb_linktmpdir is either not found or not a directory: dddd

Run Time Error: Indicates the process cannot access directory dddd, which it needs in order to auto-relink as specified by its $ZROUTINES; the directory may not exist as a directory or the process lacks authorization to the directory.

Action: The directory specification comes from $ydb_linktmpdir if it is defined, otherwise from $ydb_tmp if that is defined; otherwise it defaults to the system temporary directory, typically /tmp. Either correct the environment variable definition or ensure directory dddd is appropriately set up. Note that all users of auto-relink for a directory normally need to use the same temporary directory for their relink control files.

-----------------------
INVLNPAIRLIST
-----------------------

INVLNPAIRLIST, Invalid lockname/subscript pair list (uneven number of lockname/subscript parameters)

Runtime Error: This message comes only from the Golang wrapper. When specifying the locks to be acquired in the parameter list of LockE(), each lock is represented by a pair of arguments - a lock name and a list of subscripts. If the arguments aren't paired properly such that the last lock name read has no subscripts specified, this error is given.

Action: Determine why the parameter list is not paired up correctly and fix it.

------------------------
INVLOCALE
------------------------

INVLOCALE, Attempt to reset locale to supplied value of $ydb_locale xxxx failed

All YottaDB Components Error: YottaDB found the value of $ydb_locale xxxx did not specify a valid currently supported local

Action: Correct the locale setup and restart the process.

-------------
INVMAINLANG
-------------

INVMAINLANG, Invalid main routine language id specified: xxxx

Run Time Error: This indicates that an internal feature of YottaDB, an alternative signal handling mechanism, is used by an unsupported language.

Action: If this error occurs for a language wrapper provided by YottaDB, contact your YottaDB support channel. Otherwise, contact the developer of the language wrapper the application is using.

---------------------
INVMEMRESRV
---------------------

INVMEMRESRV, Could not allocate YottaDB memory reserve (xxxx)

Images Warning: YottaDB could not allocate xxxx KiB of reserve memory for handling and reporting out-of-memory conditions. Examine the subsequent messages for more information on why the memory reserve allocation failed.

Action: If $ydb_memory_reserve is too high, specify a lower value and retry. If the value is reasonable, determine what else is preventing the allocation (process or system limits or usage by other system components). Note that YottaDB uses this reserve only when a process runs out of memory so it mostly requires address space and almost never requires actual memory.

-------------------
INVMNEMCSPC
-------------------

INVMNEMCSPC, Unsupported mnemonicspace xxxx

Run Time Error: The mnemonicspace xxxx specified in an OPEN command is not supported by YottaDB.

Action: Replace the mnemonicspace with a supported one.

--------------------
INVMVXSZ
--------------------

INVMVXSZ, Invalid block size for GOQ load format

MUPIP Error: This indicates that the LOAD command with the qualifier FORMAT=GOQ determined that the input file did not have the proper block size for that format.

Action: Determine how the file was created and use the proper specification for the FORMAT= qualifier.

-------------------
INVNAMECOUNT
-------------------

INVNAMECOUNT, Number of varnames cannot be less than zero.

Runtime Error: This indicates that the number of variable names specified in a SimpleAPI call (identified in the message text) is less than zero.

Action: Retry the SimpleAPI call with a number of variable names that is greater than or equal to zero.

-----------------------
INVNETFILNM
-----------------------

INVNETFILNM, Invalid file name following node designation in global directory

GT.CM Error: This indicates that the GT.CM Server received a node name that does not exist on the network.

Action: Use GDE to check the Global Directory on the originating node for typographical errors in a remote node file-specification.

-----------------------
INVOBJ
-----------------------

INVOBJ, Cannot ZLINK object file due to unexpected format

Run Time Error: This indicates that ZLINK encountered invalid records in the object file it was trying to integrate into the image.

Action: Determine whether ZLINK has the intended argument. If the object file has been damaged, recreate it with a ZLINK that specifies the source file using a .M extension.

-------------------
INVOBJFILE
-------------------

INVOBJFILE, Cannot ZLINK object file ffff due to unexpected format

Run Time Error: This indicates that ZLINK encountered invalid records in the object file ffff it was trying to integrate into the image.

Action: Determine whether ZLINK has the intended argument. If the object file has been damaged, recreate it with a ZLINK that specifies the source file using a .m extension, a ZCOMPILE or a yottadb command at the shell.


----------------------
INVPORTSPEC
----------------------

INVPORTSPEC, Invalid port specification

Run Time Error: This indicates that the OPEN command socket parameter contained an invalid port number.

Action: Redefine the socket parameter to a value between 0 and 65535.

-----------------------
INVPROT
-----------------------

INVPROT, Invalid protocol specified by remote partner

GT.CM Error: This indicates that the remote networked system used a protocol incompatible with the locally installed version of GT.CM.

Action: Verify that both systems have compatible versions of GT.CM installed.

------------------
INVQUALTIME
------------------

INVQUALTIME, Invalid time qualifier value. Specify as xxxx=delta_or_absolute_time.

MUPIP Error: This indicates that the time qualifier value specified for xxxx is invalid.

Action: Reissue the command with correct syntax.

---------------------
INVREDIRQUAL
---------------------

INVREDIRQUAL, Invalid REDIRECT qualifier value. xxxx

MUPIP Error: This indicates a syntax error in REDIRECT qualifier value.

Action: Reissue the command with correct syntax for REDIRECT.

-----------------
INVROLLBKLVL
-----------------

INVROLLBKLVL, Rollback level (xxxx) not less than the current $tlevel (yyyy). Cannot rollback.

Run Time Error: This indicates that the application is attempting to ROLLBACK to a transaction level that is zero (0) or negative. The minimum transaction level that an application can be rolled back to is one (1).

Action: Review the logic and code path that led to the error and modify the code appropriately.

----------------
INVSEQNOQUAL
----------------

INVSEQNOQUAL, Invalid SEQNO qualifier value xxxx

MUPIP Error: This error indicates that MUPIP JOURNAL -EXTRACT -SEQNO command could not extract a journal file because an invalid SEQNO format was specified.

Action: Enter a comma-separated list of valid sequence numbers (‘0’ or positive integers) as values for the SEQNO qualifier. The format of the -SEQNO qualifier is -SEQNO=seqno1[,seqno2,seqno3…..] where seqno is the region sequence number in decimal format.

---------------------
INVSPECREC
---------------------

INVSPECREC, pppp Invalid global modifier record

MUPIP Error: This indicates that MUPIP INTEG found a corrupt 4-byte collation record was found for a global variable (that is the 1st of the 4 bytes is not 1). pppp identifies the path in the directory tree (each element of the path consisting of a block#/offset) leading to the error.

Action: Use DSE to examine the corrupt record and fix it. Report the entire incident context to your YottaDB support channel.

--------------------
INVSTACODE
--------------------

INVSTACODE, Invalid value for second parameter of $STACK (xxxx).

Run Time Error: This indicates that the intrinsic function $STACK received an unrecognized string xxxx for the info (second) parameter.

Action: Make sure the second argument is "MCODE", "ECODE" or "PLACE".

---------------------
INVSTATSDB
---------------------

INVSTATSDB, Database file SSSS associated with statistics database region RRRR is not a valid statistics database

Run Time Error: Indicates that the file SSSS designated as a statistics database is either not a valid database at all, or is not a statistics database.

Action: Rename (if it might be a valid database) or delete (if its origin is unknown) the file in question as appropriate and retry.

--------------------
INVSTRLEN
--------------------

INVSTRLEN, Invalid string length xxxx: max yyyy

Run Time Error: This indicates that YottaDB encountered a string with a length of xxxx that exceeds the maximum acceptable length yyyy in this context. If the caller is a SimpleAPI function, this indicates that a buffer provided by the caller is not long enough for a string to be returned, or the length of a string passed as a parameter exceeds YDB_MAX_STR. In the event the return code is YDB_ERR_INVSTRLEN and if \*xyz is a ydb_buffer_t structure whose xyz->len_alloc indicates insufficient space, then xyz->len_used is set to the size required of a sufficiently large buffer. In this case, the len_used field of a ydb_buffer_t structure is greater than the len_alloc field, and the caller is responsible for correcting the xyz->len_used field.

Action: Modify the string to an acceptable length.

-------------------
INVSVN
-------------------

INVSVN, Invalid special variable name

Compile Time Error: This indicates that a variable of the form "$name" did not match any valid special variable name.

Action: Look for typographical errors, an improper special variable name abbreviation, or a missing $ in an extrinsic.

-------------------
INVTMPDIR
-------------------

INVTMPDIR, Value for $ydb_tmp is either not found or not a directory: dddd - Reverting to default value

Error: Indicates the process cannot access directory dddd, which it may need for a number of actions; the directory may not exist as a directory or the process lacks authorization to locate the directory.

Action: The directory specification comes from $ydb_tmp if it is defined, otherwise it defaults to the system temporary directory, typically /tmp in most environments. Either correct the environment variable definition or ensure directory dddd is appropriately set up. Note that all users of a particular YottaDB instance normally need to use the same temporary directory to ensure proper interprocess communication.

---------------------
INVTPTRANS
---------------------

INVTPTRANS, Invalid TP transaction - either invalid TP token or transaction not in progress

Runtime Error: This message comes from the threaded Simple API engine (both C and Golang). Each TP callback routine is given a 'tptoken' when it is driven. This token (unmodified!) must be used in all calls to the runtime from THIS TP callback routine at this level or this error message is returned.

Action: Determine why the tptoken value is incorrect and correct it.

------------------
INVTRCGRP
------------------

INVTRCGRP, Invalid trace group specified in $ydb_trace_groups: gggg

Run Time Error: The process startup environment attempted to activate a diagnostic tracing facility but specified a group name of gggg and there is currently no such group.

Action: Check with your YottaDB support channel for the currently available group names.

-------------------
INVTRNSQUAL
-------------------

INVTRNSQUAL, Invalid TRANSACTION qualifier. Specify only one of TRANSACTION=[NO]SET or TRANSACTION=[NO]KILL.

MUPIP Error: This indicates that an invalid value was assigned to the -TRANSACTION qualifier.

Action: Specify appropriate value to the -TRANSACTION qualifier.

----------
INVVALUE
----------

INVVALUE, VVVV is invalid DEC value for $ZCONVERT(). Range is -9223372036854775808 to 18446744073709551615
INVVALUE, VVVV is invalid HEX value for $ZCONVERT(). Range is 1 to 16 unsigned hexadecimal digits

Run Time Error: This message has two forms both of which indicate that the value VVVV is not valid input for $ZCONVERT() in the specified base.

Action: If the input value is expected to be within the range supported by $ZCONVERT(), the actual results from a coding issue or from input that is not validated, correct the issue. If input values are legitimately expected outside the range supported by $ZCONVERT() use the `%DH <../ProgrammersGuide/utility.html#dh-util>`_ and `%HD <../ProgrammersGuide/utility.html#hd-util>`_ utility programs.

-----------
INVVARNAME
-----------
INVVARNAME, Invalid local/global/ISV variable name supplied to API call.

Run Time Error: This indicates that a SimpleAPI call received an invalid variable name. The invalidity can be one of the following types:

a) The ydb_buffer_t structure corresponding to the variable name has a "len_used" field greater than "alloc_len" OR
b) The ydb_buffer_t structure corresponding to the variable name has a zero value of "len_used" OR
c) The ydb_buffer_t structure corresponding to the variable name has a non-zero value of "len_used" but a NULL value of "buf_addr" OR
d) The variable name starts with a ^ (i.e. is a global variable name), but the second character is not a % or an alpha character (lower or upper case) or at least one of the following characters is not an alphanumeric character (lower or upper case alphabet or a decimal digit) OR
e) The variable name starts with a $ (i.e. is an intrinsic special variable name), but is not followed by any other character (i.e. "len_used" has a value of 1) OR
f) The variable name starts with a character other than a % or an alpha character (lower or upper case) OR
g) The variable name starts with a % or alpha character (lower or upper case) but at least one of the following characters is not an alphanumeric character (lower or upper case alphabet or a decimal digit)

Action: Determine which of the described failures scenarios is the issue and accordingly fix the variable name passed in to the SimpleAPI call.

.. _invydbexit-error:

--------------------
INVYDBEXIT
--------------------

INVYDBEXIT, Inappropriate invocation of ydb_exit. ydb_exit cannot be invoked from external calls.

Call in/Run Time Error: This indicates that the call-in shut-down function ydb_exit() has been called from an external call C function. Since the YottaDB run-time system must be operational even after the external call function returns, ydb_exit() is meant to be called only once during a process lifetime, and only from the base C/C++ program when YottaDB functions are no longer required by the program.

Action: Remove all invocations of ydb_exit() from external call functions.

--------------------
INVZBREAK
--------------------

INVZBREAK, Cannot set ZBREAK in direct mode routine (GTM$DMOD)

Run Time Error: GTM$DMOD is an embedded routine that provides direct mode and it does not permit insertion of a ZBREAK.

Action: Issue ZBREAK only for application code

-------------
INVZCONVERT
-------------

INVZCONVERT, Translation supported only between DEC/HEX OR between UTF-8/UTF-16/UTF-16LE/UTF-16BE

Run Time Error: This indicates that the base from which a number is to be converted is the same as that to which it is to be coverted, or for a string that conversion between the requested character sets is not supported. Numeric conversion is not meaningful when the bases are the same.

Action: Review the code, and modify as necessary to ensure that $ZCONVERT() is being used correctly.

--------------------
INVZDIRFORM
--------------------

INVZDIRFORM, Illegal value (xxxx) specified for ZDIR_FORM

Run Time Error: This indicates that the value specified for ZDIR_FORM in the VIEW command is not recognized by YottaDB.

Action: Specify a valid value for ZDIR_FORM.


-------------------
INVZROENT
-------------------

INVZROENT, xxxx is neither a directory nor an object library(DLL)

Run Time Error: This indicates that an invalid entry (xxxx), which is neither an object directory nor a shared library, has been specified in $ZROUTINES.

Action: Remove xxxx or replace it with a valid directory or a shared library.

--------------------
INVZSTEP
--------------------

INVZSTEP, Invalid ZSTEP qualifier

Run Time Error: This indicates that ZSTEP had an argument other than OVER, INTO, or OUTOF.

Action: ZSTEP only accepts these three keyword arguments. It does not accept variables or indirection. Use one of the valid arguments. If you need additional control, consider using ZBREAK.

--------------------
IOEOF
--------------------

IOEOF, Attempt to read past an end-of-file

Run Time/MUPIP Error: This indicates that a READ command for a run-time system or a MUPIP command attempted to move past an end-of-file.

Action: Verify that the $ZEOF special variable is tested by the function between READs or that an EXCEPTION code string is assigned to handle EOFs. Alternatively, have your $ETRAP (or $ZTRAP) error handling deal with this error. The USE command has a REWIND deviceparameter that allows you to read from the beginning of the file without having to CLOSE and OPEN again, which may facilitate recovery from this error. Attempting to READ from a non-existent file that is not opened as READONLY also causes this error. In the event of a MUPIP error, make sure the file being read is not corrupted.

----------------
IOERROR
----------------

IOERROR, Error occurred while doing aaaa in oooo operation -- called from module mmmm at line LLLL

Run Time Error: On UNIX this indicates that a system call used to manage the underlying O/S device for a FIFO, PIPE or Sequential Disk failed in a way that YottaDB did not anticipate.

Action: Use the OS documentation to investigate the failure.

-----------------
IONOTOPEN
-----------------

IONOTOPEN, Attempt to USE an I/O device which has not been opened

Run Time Error: This indicates that a USE command attempted to make the current device one that had not been OPENed. The current device remains unchanged when this error occurs.

Action: Look for a missing OPEN or an extra CLOSE command.


-------------------
IORUNDOWN
-------------------

IORUNDOWN, Error during image rundown

Run Time Error: This indicates that as part of image termination, the process attempted to deallocate all devices and files allocated in YottaDB but encountered an error.

Action: Use the appropriate host shell commands to display the statuses of the devices being used by the process, and deallocate any device that is still allocated.

----------------------
IOWRITERR
----------------------

IOWRITERR, IO write by PID xxxx to block yyyy of database zzzz failed. PID aaaa retrying the IO.

Run Time Error: This error message is sent to operator log when a queued write fails and is about to be retried. If an error status is available, it follows this message.

Action: Appearance of this message usually indicates disk subsystem error condition. Check disk error logs, in addition to operator logs for accompanying messages.

-----------------------
IPCNOTDEL
-----------------------

IPCNOTDEL, xxxx : yyyy did not delete IPC resources for region zzzz

MUPIP Information: This indicates that MUPIP did not delete the shared system resources of the region. The shared system resources may still be being in use by some other processes.

Action: Find out if some other process was attached to the shared system resource. If appropriate, issue MUPIP RUNDOWN REG * to remove the shared resource.

-----------------------
ISOLATIONSTSCHN
-----------------------

ISOLATIONSTSCHN, Error changing NOISOLATION status for global xxxx within a TP transaction from aaaa to bbbb

Run Time Error: In YottaDB, the VIEW "NOISOLATION" command changes the isolation-status of the global variable(s) specified. If a process attempts to change the global variable's isolation-status within a TP transaction after it has referenced the global variable in the same TP transaction, the ISOLATIONSTSCHN error gets triggered.

Action: Change the application to issue the VIEW "NOISOLATION" command in conformance with the allowed usage.

-------------------
ISSPANGBL
-------------------

ISSPANGBL, Operation cannot be performed on global ^gggg as it spans multiple regions in current global directory

Run Time Error: This indicates that a $ $ set^%GBLDEF or $ $ kill^%GBLDEF was attempted on a global that spans multiple regions.

Action: Only $$get^%GBLDEF is supported for spanning globals. Specify a non-spanning global or change the set/kill to a get. For spanning globals, use the GDE ADD -GBLNAME command to set collation characteristics.


---------------------
JIUNHNDINT
---------------------

JIUNHNDINT, An error during $ZINTERRUPT processing was not handled: eeee;

Run Time Error: When returning from code invoked by MUPIP INTRPT (clearing $ZININTERRPT), YottaDB implicitly clears any error(s) detected while 1=$ZININTERRUPT, sends this error notification to the operator log and continues processing; eeee is the mnemonic for the unhandled error.

Action: Fix $ZINTERRUPT handler to either not generate the error or to correctly handle it before returning to interrupted code

-----------------
JNI
-----------------

JNI, xxxx

Run Time Error: YottaDB uses this message with appropriate accompanying text xxxx to indicate an error condition with a Java call-out invocation.

Action: Examine the text and address the described error condition.

------------------
JNLACCESS
------------------

JNLACCESS, Error accessing journal file jjjj

Run Time Error: YottaDB sends this message to the system log followed by other messages detailing the failure. jjjj is the file-specification for the inaccessible journal. In most situations, this error occurs when the journal file storage runs out of disk space or there are permission issues.

Action: Review the accompanying message(s) for additional information. This means an error while trying to write to the journal file.

------------------
JNLACTINCMPLT
------------------

JNLACTINCMPLT, Mupip journal action might be incomplete

MUPIP Warning: This indicates that MUPIP did not finish to completion successfully.

Action: Review and analyze the accompanying message(s).

---------------------
JNLALIGNSZCHG
---------------------

JNLALIGNSZCHG, Journal ALIGNSIZE is rounded up to xxxx blocks (closest next higher power of two)

Run Time Information: This indicates that the ALIGNSIZE specified in the MUPIP SET JOURNAL command was not a perfect power of two. It has been rounded up to the closest next higher power of two and the new journal file created (if any) will use this value for ALIGNSIZE.

Action: -

--------------------
JNLALIGNTOOSM
--------------------

JNLALIGNTOOSM, Alignsize xxxx (bytes) is smaller than block size yyyy (bytes) for aaaa bbbb. Using alignsize of cccc (bytes) instead.

MUPIP Warning: This indicates that the specified alignsize xxxx is smaller than the specified block size yyyy for the mentioned region/database (aaaa) file bbbb. MUPIP will use the default cccc bytes instead of the specified xxxx.

Action: If the alignsize cccc used is not acceptable, choose some other legal value for alignsize and reissue the command.

---------------------
JNLALLOCGROW
---------------------

JNLALLOCGROW, Increased Journal ALLOCATION from [ssss blocks] to [aaaa blocks] to match AUTOSWITCHLIMIT for ffff nnnn

GDE/MUPIP Information: The utility increased the journal allocation value from ssss to aaaa for the journal files associated with ffff nnnn, which is either "database file" followed by a database file name or "region" followed by a region name. This indicates that the specified journal allocation and journal extension values combined exceed the specified journal autoswitchlimit and the utility has adjusted the journal allocation value accordingly.

Action: None required.

-------------------------
JNLBADLABEL
-------------------------

JNLBADLABEL, Specified File xxxx fdoes not have a YottaDB Journal File Label

MUPIP Error: This indicates that the journal file indicated in the accompanying previous message does not match the expected format.

Action: If a command specification caused this error, determine whether the command has the proper file-specification. Make sure the journal file was created by the current YottaDB version. Also ensure that a process is not using a journal file-specification for some other purpose.

---------------------
JNLBADRECFMT
---------------------

JNLBADRECFMT, Journal Record Format Error encountered for file jjjj at disk address yyyy

MUPIP/Run Time Error: This indicates that an attempt to open a journal file encountered an invalid record.

Action: Report the entire incident context to your YottaDB support channel.

--------------------
JNLBUFFDBUPD
--------------------

JNLBUFFDBUPD, Journal file buffer size for database file dddd has been adjusted from xxxx to yyyy

MUPIP Warning: The journal buffer size specified by the user for the database file dddd or previously stored in the database file header fell outside the permissible range, and was automatically adjusted up or down from xxxx to a legitimate value of yyyy.

Action: None Required.

--------------------
JNLBUFFPHS2SALVAGE
--------------------

JNLBUFFPHS2SALVAGE, Salvaged journal records from process PPPP for database file DDDD at transaction number NNNN and journal-sequence-number/unique-token JJJJ with journal file starting offset OOOO and length LLLL

Run Time Information: Operator log message indicating clean up of journaling information abandoned by an abnormally terminated process.

Action: Investigate the cause of the process termination; report to your YottaDB support channel when coincident with other issues.

----------------------
JNLBUFFREGUPD
----------------------

JNLBUFFREGUPD, Journal file buffer size for region rrrr has been adjusted from xxxx to yyyy

MUPIP Warning: The journal buffer size specified by the user for the region rrrr or previously stored in the corresponding database file header fell outside the permissible range, and was automatically adjusted, up or down, from xxxx to a legitimate value of yyyy.

Action: None Required.

-------------------
JNLBUFINFO
-------------------

JNLBUFINFO, Pid aaaa dsk bbbb free cccc bytcnt dddd io_in_prog eeee fsync_in_prog ffff dskaddr gggg freeaddr hhhh qiocnt iiii now_writer xxxx fsync_pid yyyy filesize zzzz cycle oooo errcnt pppp wrtsize qqqq fsync_dskaddr rrrr

Run Time Information: This message always accompanies some other YottaDB journaling error message. This gives detailed information on the state of the journal buffers at the time of the accompanying error.

Action: For information purposes only. Review the accompanying message(s) for additional information.


---------------------
JNLCLOSE
---------------------

JNLCLOSE, Error closing journal file: xxxx

Run Time Error: This indicates that YottaDB could not properly close journal file xxxx.

Action: Review the accompanying message(s) for additional information.

--------------
JNLCLOSED
--------------

JNLCLOSED, Journaling closed for database file dddd at transaction number xxx

Run Time Warning: This message indicates YottaDB had to turn journaling OFF on the specified database. Other preceding messages identify the cause (e.g. lack of disk space while writing to journal file, permissions issue while auto-switching to new journal files etc.). The message also displays the database transaction number.

Action: Fix the issue that caused journaling to get turned OFF in the first place (disk space, permissions etc.).

Turn journaling back ON by issuing a MUPIP SET JOURNAL=ON or MUPIP BACKUP NEWJNL command. This command can work while processes are concurrently updating the database and causes YottaDB to journal subsequent updates in the journal file.

---------------------
JNLCNTRL
---------------------

JNLCNTRL, Journal control unsynchronized for ffff.

Run Time Error: This indicates that there is a discrepancy between the journal file updates and the database updates. The system is not updating journal files.

Action: Review the accompanying message(s) and take appropriate action. After the cause is resolved, to reestablish durability, perform a MUPIP BACKUP that turns journaling back on. Once the system is up and running, contact your YottaDB support channel with operator log information and any additional information that you feel is relevant for further diagnosis.

----------------------
JNLCREATE
----------------------

JNLCREATE, Journal file xxxx created for <database/region> yyyy with aaaa

MUPIP Information: This indicates that a journal file xxxx is created for database/region yyyy with the NOBEFORE_IMAGES or BEFORE_IMAGES journaling options (aaaa).

Action: This informational message confirms the success of the new journal file creation operation for a region. No futher action is necessary unless there are other WARNING, FATAL, and/or ERROR category messages.

--------------------
JNLCRESTATUS
--------------------

JNLCRESTATUS, xxxx at line aaaa for journal file yyyy, database file zzzz encountered error

Run Time/MUPIP Warning: This indicates that the creation of journal file yyyy for database file zzzz failed.

Action: Review the accompanying messages and take appropriate action.

--------------------
JNLCYCLE
--------------------

JNLCYCLE, Journal file jjjj causes cycle in the journal file generations of database file dddd

MUPIP Error: This indicates that MUPIP encountered journal file jjjj causing a cycle in the journal file generations of database file dddd; i.e. following the back-pointers in the journal files can wind up repeatedly finding the same journal file.

Action: Contact your YottaDB support channel with appropriate log messages.

------------------
JNLDBERR
------------------

JNLDBERR, Journal file jjjj does not correspond to database dddd

Run Time Error: This indicates that YottaDB could not open journal file jjjj for database file dddd because the journal file header identifies itself as belonging to a different database file that does not exist in the system.

Action: Use a MUPIP SET command with the qualifier JOURNAL to create a journal file that matches the database.

-----------------------
JNLDBSEQNOMATCH
-----------------------

JNLDBSEQNOMATCH, Journal file ffff has beginning region sequence number jjjj but database dddd has region sequence number ssss

MUPIP Error: MUPIP JOURNAL ROLLBACK FORWARD has found that journal file ffff has a beginning region sequence number jjjj, but the corresponding database file dddd has a region sequence number ssss. This condition may arise due to missing or incorrect journal files, for example, due to a -NOCHAIN specification.

Action: Use "*" and/or do not use -NOCHAIN to specify the list of journal files. If you are specifying an explicit list of journal file names, verify that you are specifying the exact set of needed journal file names.

---------------------
JNLDBTNNOMATCH
---------------------

JNLDBTNNOMATCH, Journal file xxxx has beginning transaction number aaaa but database yyyy has current transaction number bbbb

MUPIP Error: MUPIP JOURNAL FORWARD has found that journal file xxxx has beginning transaction number aaaa, but the corresponding database file yyyy has current transaction number bbbb. This condition may arise due to missing or duplicate transactions.

Action: Verify that the correct journal file names were specified. If appropriate, force forward recovery using the NOCHECKTN qualifier.

--------------------
JNLDISABLE
--------------------

JNLDISABLE, Specified journal option(s) cannot take effect as journaling is DISABLED on database file dddd

MUPIP Warning: This indicates that none of the specified journal option(s) in MUPIP SET -JOURNAL or MUPIP BACKUP command took effect, because journaling was found DISABLED on database file dddd.

Action: Revise the selection qualification to exclude the DISABLED region(s) or, if appropriate, enable journaling on those regions.

--------------------
JNLENDIANBIG
--------------------

JNLENDIANBIG, Journal file jjjj is BIG endian on a LITTLE endian system

MUPIP Error: The MUPIP command on a little endian system specified journal file jjjj which was created on a big endian system. YottaDB does not convert journal files with incompatible byte ordering.

Action: Set up operational procedures that ensure journal files are used on systems with the same byte ordering as where they are created. If necessary, extract journal file data on the source system and use an M program on the opposite endian system to restore it.

---------------------
JNLENDIANLITTLE
---------------------

JNLENDIANLITTLE, Journal file jjjj is LITTLE endian on a BIG endian system

MUPIP Error: The MUPIP command on a big endian system specified journal file jjjj which was created on a little endian system. YottaDB does not convert journal files with incompatible byte ordering.

Action: Set up operational procedures that ensure journal files are used on systems with the same byte ordering as where they are created. If necessary, extract journal file data on the source system and use an M program on the opposite endian system to restore it.

----------------------------
JNLEXTEND
----------------------------

JNLEXTEND, Journal file extension error for file jjjj.

Run Time Error: Journal file jjjj failed to extend. If the environment is not configured for instance freeze, this causes journaling to be turned off for the region.

Action: Review the accompanying message(s) and take appropriate action. If the environment is not configured for instance freeze, perform a MUPIP BACKUP that turns journaling on again to reestablish durability.

------------------------
JNLEXTR
------------------------

JNLEXTR, Error writing journal extract file: xxxx

MUPIP Error: This indicates that an error was encountered while trying to write to either the JNL EXTRACT file or lost-transaction file or broken-transaction file as part of a MUPIP JOURNAL command.

Action: Review the accompanying message(s) for additional information.

--------------------------
JNLEXTRCTSEQNO
--------------------------

JNLEXTRCTSEQNO, Journal Extracts based on sequence numbers are restricted to a single region when replication is OFF

MUPIP Error: When replication is enabled YottaDB applies a uniform set of sequence numbers across regions, but when it is not in use each region has its own set of sequence numbers, and in that case, MUPIP only works on one region at a time.

Action: If you need cross-region sequence numbers, start replication with at least a passive Source Server; otherwise use one MUPIP JOURNAL -EXTRACT command for each region when using the -SEQNO qualifier.

----------------------
JNLFILECLOSERR
----------------------

JNLFILECLOSERR, Error closing journal file xxxx

MUPIP Error: This indicates that the MUPIP JOURNAL command failed to close the specified journal file xxxx.

Action: Review the accompanying message(s) for additional information.


-----------------------
JNLFILEDUP
-----------------------

JNLFILEDUP, Journal files xxxx and yyyy are the same

MUPIP Information: MUPIP JOURNAL -RECOVER -FORWARD does not allow duplicated journal files in forward recovery.

Action: Remove any duplicated journal file(s) and re-issue the forward recovery command.

---------------------
JNLFILEOPNERR
---------------------

JNLFILEOPNERR, Error opening journal file xxxx

MUPIP Error: This indicates that MUPIP JOURNAL command failed to open the specified journal file xxxx.

Action: Ensure the journal file name specified is correct. Review the accompanying message(s) for additional information.


------------------------
JNLFILEXTERR
------------------------

JNLFILEXTERR, Error during extension of journal file xxxx

Run Time Error: This indicates that an error was encountered during the course of journal file extension, while trying to determine the available space on the file system housing the journal file xxxx. This causes the journaling to be turned off.

Action: Locate appropriate disk space and adjust the journal file path. To reestablish durability, perform a MUPIP BACKUP that turns journaling back on again.

----------------------------
JNLFILNOTCHG
----------------------------

JNLFILNOTCHG, Journal file not changed

MUPIP Error: This indicates that the MUPIP SET-JNLFILE command was unable to change the journal file as specified.

Action: Review accompanying message(s) for additional information.

----------------------------
JNLFILOPN
----------------------------

JNLFILOPN, Error opening journal file jjjj for database file dddd

Run Time Error: This indicates that YottaDB was unable to open journal file jjjj for the specified database file dddd. The Source Server exits with a JNLFILOPN message after six failed attempts to open journal files.

Action: Review the accompanying message(s) for additional information.

---------------------------
JNLFILRDOPN
---------------------------

JNLFILRDOPN, Error opening journal file xxxx for read for database file yyyy

Source Server log/MUPIP Error: This indicates that YottaDB was unable to open journal file xxxx in read-only mode for the specified database file. The Source Server exits with a JNLFILRDOPN message after six failed attempts to open journal files.

Action: Review the accompanying message(s) for additional information.

-------------------------
JNLFLUSH
-------------------------

JNLFLUSH, Error flushing journal buffers to journal file xxxx

Run Time Error: This indicates that an attempt to write existing journal records to the journal file failed.

Action: Review the accompanying message(s) for additional information.

--------------------------
JNLFLUSHNOPROG
--------------------------

JNLFLUSHNOPROG, No progress while attempting to flush journal file jjjj

Run Time Warning: Indicates that processes needing space in the journal buffers were unable to write to journal jjjj, because even though multiple processes have controlled the resource, this process has not been able to flush records. JNLPROCSTUCK means one process is hogging the reseource, while this message means that more than one process has tried but none have succeeded. Might indicate a clogged disk subsystem on which journal file jjjj resides.

Action: Check the log file for other journaling related messages. Consider balancing disk subsystem load.

---------------------------
JNLFNF
---------------------------

JNLFNF, Journal file xxxx not found

MUPIP Information: This indicates that MUPIP did not find the specified journal file xxxx while executing the command.

Action: -

----------------------
JNLFSYNCERR
----------------------

JNLFSYNCERR, Error synchronizing journal file xxxx to disk

Run Time Error: This indicates that the fsync() function on the journal file xxxx failed. This is likely a disk subsystem related problem.

Action: Review the accompanying messages for the cause of the failure.

--------------------
JNLFSYNCLSTCK
--------------------

JNLFSYNCLSTCK, Journaling fsync lock is stuck in journal file jjjj

Run Time Error: A resource controlling journal file actions has remained unavailable for a long period.

Action: Check on the condition of the process identified in the associated messages.

---------------------
JNLINVALID
---------------------

JNLINVALID, jjjj is not a valid journal file Region: rrrr

Run Time Error: This indicates that YottaDB could not open journal file jjjj, due to an error that is detailed in the accompanying previous message(s). While trying to create a new journal file for the same region, it encountered errors. rrrr is the region name associated with the journal.

Action: Review the accompanying error message(s) to determine the cause of the failure of the new journal file creation. After the cause is resolved, to re-establish durability, perform a MUPIP BACKUP that turns journaling back on.

-----------------
JNLINVALLOC
-----------------

JNLINVALLOC, Journal file allocation xxxx is not within the valid range of yyyy to zzzz. Journal file not created.

MUPIP Warning: This indicates that a SET command modified with the JOURNAL qualifier failed because ALLOCATION=xxxx was less than the minimum or greater than the maximum number of blocks. yyyy is the minimum allocation permitted. zzzz is the maximum allocation permitted.

Action: Adjust the ALLOCATION= to adhere to the valid range.

------------------
JNLINVEXT
------------------

JNLINVEXT, Journal file extension xxxx is greater than the maximum allowed size of yyyy. Journal file not created.

MUPIP Warning: This indicates that a SET command modified with the JOURNAL qualifier failed because the EXTENSION=xxxx argument exceeded the maximum number of pages. yyyy is the maximum extension permitted.

Action: Reduce the EXTENSION= qualifier definition.

------------------
JNLINVSWITCHLMT
------------------

JNLINVSWITCHLMT, Specified AUTOSWITCHLIMIT xxxx falls outside of allowed limits aaaa and bbbb

MUPIP Error: This indicates that the specified autoswitchlimit for the journal file is outside of the allowed range [that is indicated in the error message].

Action: Specify an autoswitchlimit within the specified allowed range.

-------------------
JNLMINALIGN
-------------------

JNLMINALIGN, Journal Record Alignment xxxx is less than the minimum value of yyyy

MUPIP Warning: This indicates that a MUPIP SET JOURNAL command specified an alignsize for the new journal file, which is less than the minimum allowed yyyy.

Action: Specify an alignsize that is greater than the allowed minimum.


-------------------
JNLMOVED
-------------------

JNLMOVED, Journal file appears to have been moved. Journaling activity will not be done.

Run Time Error: This indicates that while opening a journal file the system encountered the journal file name in the database header file, pointing to a different location than the journal file ID.

Action: Use MUPIP SET to specify the correct journal file location.

----------------
JNLNEWREC
----------------

JNLNEWREC, Target system cannot recognize journal record of type xxxx. Last recognized type is yyyy.

MUPIP Error: This error is logged to the replication server log file. This indicates that the YottaDB application on the replication primary generated a newly introduced journal record (of type xxxx) that is not recognized by the secondary system. The highest numbered journal record type on the secondary is yyyy. A new type that can be transformed to an older type internally by the source server on the primary side will be automatically done. This error occurs when the primary source server cannot transform the new type to an older type due to impact on the application logic.

Action: The application either should not generate a journal record that is not recognized by the secondary, or, write a filter that transforms the unknown type to a known type on the target system. The replication source server on the primary should be restarted with the filter.

------------------
JNLNMBKNOTPRCD
------------------

JNLNMBKNOTPRCD, Journal file xxxx does not match the current journal file yyyy of database file zzzz

MUPIP Error: This indicates that MUPIP JOURNAL BACKWARD cannot proceed because the journal file name xxxx is not same as the journal file name yyyy in the database file header of zzzz.

Action: Specify the correct journal file name for the database zzzz. If the database file header is not pointing to the correct journal file, fix it using MUPIP SET JOURNAL.

--------------------
JNLNOBIJBACK
--------------------

JNLNOBIJBACK, MUPIP JOURNAL BACKWARD cannot continue as journal file xxxx does not have before image journaling

MUPIP Error: This indicates that an attempt to use the BACKWARD qualifier on xxxx journal file was made without enabling before-image journaling on the file.

Action: Ensure before-image journaling is enabled prior to the usage of BACKWARD qualifier, alternatively use FORWARD qualifier for nobefore image journaling enabled files.

-------------------
JNLNOCREATE
-------------------

JNLNOCREATE, Journal file jjjj not created

MUPIP/Run Time Error: This indicates that MUPIP could not create journal file jjjj.

Action: Review the accompanying message(s) for additional information.

-----------------
JNLNOREPL
-----------------

JNLNOREPL, Replication not enabled for journal file jjjj (database file dddd)

Source Server log/MUPIP Error: Replication Source Server startup encountered a database dddd with journal file jjjj for which replication was turned off because of a journaling issue and has not since been re-enabled.

Action: Use MUPIP SET to re-enable replication. Take steps to ensure that there is sufficient management of journal file space to prevent a reoccurrence of this issue.

-----------------
JNLOPNERR
-----------------

JNLOPNERR, Error opening journal file xxxx for region yyyy

Run Time/MUPIP Error: This indicates that YottaDB could not open the journal file xxxx.

Action: Review and troubleshoot accompanying messages.

---------------
JNLORDBFLU
---------------

JNLORDBFLU, Error flushing database blocks to dddd. See related messages in the operator log

Run Time Error: This message indicates that hardening journal or database records could not be completed due to an error. The operator log should contain one or more accompanying messages indicating the cause of the error.

Action: Verify the normal state of the file system and appropriate permissions of the database and journal files. Report the entire incident context to your YottaDB support channel along with any operator log messages within the same time frame.

----------------
JNLPOOLBADSLOT
----------------

JNLPOOLBADSLOT, Source server slot for secondary instance xxxx is in an inconsistent state. Pid = pppp, State = ssss, SlotIndex = iiii

Source Server log/MUPIP Warning: This is a debugging message sent to the syslog (operator log) whenever a source server startup or showbacklog command finds a structure in the journal pool holding inconsistent information.

Action: Forward the information to your YottaDB support channel. No action otherwise necessary. The source server command will automatically fix the inconsistency of that structure

------------------
JNLPOOLPHS2SALVAGE
------------------

JNLPOOLPHS2SALVAGE, Salvaged journal records from process PPPP for replication instance file iiii at journal sequence number JJJJ with journal pool starting offset OOOO and length LLLL

Run Time Information: Operator log message indicating that the clean up of replication information was abandoned by an abnormally terminated process.

Action: Investigate the cause of the process termination; report to your YottaDB support channel when coincident with other issues.

-----------------
JNLPOOLRECOVERY
-----------------

JNLPOOLRECOVERY, The size of the data written to the journal pool (xxxx) does not match the size of the data in the journal record (yyyy) for the replication instance file zzzz. The journal pool has been recovered.

Run Time Error: An internal error was detected while writing to the journal pool associated with the instance file zzzz, and the journal file has been recovered. Subsequent transactions will be written to the journal pool, but the source server will switch to reading from files until it reaches them. A core file may have been produced.

Action: Report the entire incident context to your YottaDB support channel.

-------------------
JNLPOOLSETUP
-------------------

JNLPOOLSETUP, Journal Pool setup error

Run Time/MUPIP Error: This indicates that an error occurred in the replication subsystem while opening the journal pool.

Action: Verify that the source server has been configured correctly. Review accompanying messages for more information about the cause of this error.

------------
JNLPREVRECOV
------------

JNLPREVRECOV, Journal file has nonzero value in prev_recov_end_of_data field

Run Time Error: This indicates that YottaDB encountered a non-zero value for the journal file header prev_recov_end_of_data field. MUPIP JOURNAL RECOVER/ROLLBACK can cause the field to be non-zero but it cannot become a current generation journal file for YottaDB run-time. Run-time considers the journal file as bad and switches to a new journal file cutting the back-link.

Action: Report the complete error to YottaDB support along with appropriate log messages within the same time frame.

----------------
JNLPROCSTUCK
----------------

JNLPROCSTUCK, Journal file writes blocked by process xxxx

Run Time Warning: This indicates that a YottaDB process waited for nearly one minute and is not able to flush the journal as the journal write mechanism seems to be blocked by process xxxx.

Action: If the situation does not improve, kill the offending process xxxx. This may indicate an overloaded disk subsystem on which journal file xxxx resides. Consider balancing disk subsystem load. If necessary, report the entire incident context with operator log information to your YottaDB support channel.

-------------------
JNLPVTINFO
-------------------

JNLPVTINFO, Pid aaaa cycle mmmm fd_mismatch nnnn channel rrrr sync_io ssss pini_addr xxxx qio_active yyyy old_channel zzzz

Run Time Information: This message always accompanies some other YottaDB journaling error message. This gives detailed information on the state of the journal buffers at the time of the accompanying error.

Action: For information purposes only. Review the accompanying message(s) for additional information.

----------------------
JNLQIOSALVAGE
----------------------

JNLQIOSALVAGE, Journal IO lock salvaged

Run Time Information: An active process salvaged a critical resource marked as belonging to a no longer active (terminated) process during a journal flush.

Action: The system automatically returns the critical resource to normal operation and continues execution. If this message continues to occur, please investigate why the process holding the crit abnormally exited.

--------------------
JNLRDERR
--------------------

JNLRDERR, Error reading journal file xxxx: unable to initialize.

Run Time Error: This indicates that YottaDB encountered an error while trying to read from the journal file xxxx. This can happen if the journal file size is less than the minimum size of the journal file header, or if the journal-file-specification does not match the intended file, or if the intended file is not accessible by the processes that update the database. YottaDB automatically attempts to create a new journal file. If the attempt to create a new journal file fails, YottaDB issues another error and the intended update is lost and does not get registered in the database and journal.

Action: To reestablish durability, perform a MUPIP BACKUP that switches to a new set of journal files.


-----------------
JNLRDONLY
-----------------

JNLRDONLY, Journal file xxxx read only

MUPIP Error: This indicates that the process in use does not have journal file write privileges or the journal file has been set to read-only.

Action: You may be performing an invalid operation. Contact your Systems Administrator.

------------------
JNLREAD
------------------

JNLREAD, Error reading from journal file xxxx at offset yyyy

MUPIP Error: This indicates that MUPIP failed to read from journal file xxxx at offset yyyy.

Action: Review the accompanying message(s) for additional information.

-----------------
JNLREADBOF
-----------------

JNLREADBOF, Beginning of journal file encountered for xxxx

MUPIP Error: This indicates that the MUPIP JOURNAL command reached the beginning of journal file xxxx, while processing backward, and is not able to process backward anymore. This may be due to the time qualifiers used to control the length of the backward processing.

Action: Verify that the time qualifiers specified are as intended.

----------------
JNLREADEOF
----------------

JNLREADEOF, End of journal file encountered for jjjj

MUPIP/Run Time Error: This indicates that MUPIP JOURNAL or a run-time journal operation encountered the end-of-file for the journal file jjjj, before it completed processing.

Action: This error indicates an improperly closed journal file. Restart journaling with a MUPIP BACKUP -NEWJNLFILES or a MUPIP SET -JOURNAL and report all available circumstances to those responsible for supporting your database operations.

-----------------
JNLRECFMT
-----------------

JNLRECFMT, Journal file record format error encountered

MUPIP Error: This indicates that MUPIP JOURNAL encountered an invalid record in the journal file.

Action: In the event of YottaDB issuing this error message, use MUPIP BACKUP to ensure durability by creating a fresh set of journals consistent with the database. Else, to resume operation, restore the database from the last backup and play forward the updates using the appropriate MUPIP JOURNAL command. As soon as possible, report the entire incident context with information from the operator log and any other relevant information to your YottaDB support channel.

-----------------
JNLRECINCMPL
-----------------

JNLRECINCMPL, Incomplete journal record at disk address aaaa for file jjjj while attempting to read seqno ssss

Source Server log/MUPIP Error: The replication Source Server had a problem with journal file jjjj at disk offset aaaa attempting to read the record with sequence number ssss.

Action: Report the entire incident context to your YottaDB support channel for further analysis. Use MUPIP SET JOURNAL -EXTRACT to investigate the issue.

-------------------
JNLRECTYPE
-------------------

JNLRECTYPE, Journal record type does not match expected type

Run Time Error: This indicates that when YottaDB tried to open the journal file as part of an M update, the end of the journal file as indicated by the journal-file-header did not contain an EOF journal record implying that the journal file is either damaged or corrupted. This message follows a JNLOPNERR message, which indicates the journal file name and the corresponding region. YottaDB automatically attempts to create a new journal file and errors out if the attempt does not succeed, in which case the intended update is not reflected in the database and journal.

Action: To reestablish durability, perform a MUPIP BACKUP that switches to a new set of journal files consistent with the database.

-------------------
JNLSENDOPER
-------------------

JNLSENDOPER, pid = aaaa : status = bbbb : jpc_status = cccc : jpc_status2 = dddd : iosb.cond = eeee

Run Time Information: This message gives information on the process that encountered an error in YottaDB journaling and the error code encountered. This message is always followed by a YottaDB journaling error message that gives the details of the error.

Action: Review the accompanying message(s) for additional information.

-------------------
JNLSETDATA2LONG
-------------------

JNLSETDATA2LONG, SET journal record has data of length xxxx. Target system cannot handle more than yyyy bytes.

MUPIP Error: This error message is logged to the replication server log file. The version of YottaDB running on the replication primary system supports longer data lengths for globals than the YottaDB version running on the secondary system.

Action: Until the secondary is upgraded to the newer version of YottaDB, the application should not use the new feature of longer data lengths.

---------------
JNLSPACELOW
---------------

JNLSPACELOW, Journal file jjjj nearing maximum size, nnnn blocks to go

Run Time Information: Depending on your settings for ALLOCATION, AUTOSWITCHLIMIT, and EXTENSION journaling options, you may see one to three JNLSPACELOW messages for each generation of a journal file. When the difference between AUTOSWITCHLIMIT and ALLOCATION is an exact multiple of EXTENSION, YottaDB attempts to write the JNLSPACELOW message to the operator log three times as a journal file reaches its maximum size. The first JNLSPACELOW message appears in the operator log when the available free space (blocks) in a journal file is equal to twice the EXTENSION, the second appears when the available free space is equal to EXTENSION, and the third appears when the journal file reaches the maximum size (AUTOSWITCHLIMIT). With EXTENSION=0 or EXTENSION=AUTOSWITCHLIMIT, YottaDB logs the JNLSPACELOW message only once per journal file to the operator log.

Action:  The JNLSPACELOW message is an information message and requires no action. However, you can use the JNLSPACELOW messages as part of monitoring journaling space requirements or as an operational practice to a trigger to intervene in journal file management. Use the frequency of JNLSPACELOW messages to proactively monitor how fast a journal file grows and as part of a monitoring alorithm that helps predict how soon the disk is likely to hit a quota limit.

-----------------
JNLSTATE
-----------------

JNLSTATE, Journaling state for <database/region> xxxx is now yyyy

MUPIP Information: This indicates that journal state for the database/region xxxx is now yyyy.

Action: This information message confirms the success of the journal state change operation. No further action is necessary unless there are other WARNING, FATAL, and/or ERROR category messages.

-----------------
JNLSTATEOFF
-----------------

JNLSTATEOFF, ROLLBACK or RECOVER BACKWARD cannot proceed as database file xxxx does not have journaling ENABLED and ON

MUPIP Error: This indicates that ROLLBACK or RECOVER cannot proceed because MUPIP encountered a database file xxxx, which does not have journaling ENABLED and ON.

Action: Verify that the file(s) specified is correct. Ensure that Journaling is ENABLED and ON for RECOVER BACKWARD to work.

-------------------
JNLSUCCESS
-------------------

JNLSUCCESS, xxxx successful

MUPIP Success: This indicates that xxxx command has finished successfully.

Action: -

-------------------
JNLSWITCHFAIL
-------------------

JNLSWITCHFAIL, Failed to switch journal file xxxx for database file yyyy

All YttaDB Components Error: This indicates that YottaDB could not create a new generation of journal file xxxx, due to an error that is detailed in the accompanying previous message(s). yyyy is the database file associated with the journal.

Action: Review the accompanying error message(s) to determine the cause of the failure of the new journal file creation. After the cause is resolved, to re-establish durability, perform a MUPIP BACKUP that turns journaling back on.


--------------------
JNLSWITCHRETRY
--------------------

JNLSWITCHRETRY, Retrying previously abandoned switch of journal file jjjj for database dddd

All YottaDB Components Information: Internal message, not delivered to the user.

Action: n/a

--------------------
JNLSWITCHSZCHG
--------------------

JNLSWITCHSZCHG, Journal AUTOSWITCHLIMIT [aaaa blocks] is rounded down to [bbbb blocks] to equal the sum of journal ALLOCATION [cccc blocks] and a multiple of journal EXTENSION [dddd blocks]

MUPIP Information: This indicates that the specified AUTOSWITCHLIMIT value was rounded down as little as possible to make it aligned to the ALLOCATION + a multiple of EXTENSION. Any subsequently created journal file will use this value for AUTOSWITCHLIMIT.

Action:  If the automatically rounded value for AUTOSWITCHLIMIT is inappropriate, specify an appropriate value for ALIGNSIZE, ALLOCATION, and/or EXTENSION.


--------------------
JNLSWITCHTOOSM
--------------------

JNLSWITCHTOOSM, Journal AUTOSWITCHLIMIT [aaaa blocks] is less than journal ALLOCATION [bbbb blocks] for database file dddd

Run Time Error: This indicates that the specified value or the automatically calculated value for AUTOSWITCHLIMIT specified in a MUPIP SET JOURNAL command is less than the default or specified value of ALLOCATION. This error also indicates that the AUTOSWITCHLIMIT value specified was greater than or equal to the ALLOCATION but in turn got rounded down, and this rounded down value is less than the ALLOCATION.

Action: Specify a higher value of AUTOSWITCHLIMIT or specify an ALLOCATION value that is less than the AUTOSWITCHLIMIT.

------------------
JNLTMQUAL1
------------------

JNLTMQUAL1, Time qualifier BEFORE_TIME=xxxx is less than SINCE_TIME=yyyy

MUPIP Error: This indicates that the specified before time xxxx is earlier than the since time yyyy.

Action: Specify correct values for the time qualifiers and make sure that BEFORE_TIME is specified to be later than the SINCE_TIME.

------------------
JNLTMQUAL2
------------------

JNLTMQUAL2, Time qualifier LOOKBACK_TIME=xxxx is later than SINCE_TIME=yyyy

MUPIP Error: This indicates that the specified lookback time xxxx is later than the since time yyyy.

Action: Specify correct values for time qualifiers and make sure that the LOOKBACK_TIME qualifier is set to an earlier time value than the SINCE_TIME qualifier.


-------------------
JNLTMQUAL3
-------------------

JNLTMQUAL3, Time qualifier BEFORE_TIME=xxxx is less than the journal file(s) minimum timestamp=yyyy

MUPIP Error: This error indicates that the -BEFORE_TIME xxxx is earlier than the earliest timestamp yyyy found in the journal file(s).

Action: Issue the MUPIP command again with an appropriate value for -BEFORE_TIME. Note that journal files record time based on a UTC clock, which is time zone independent, while MUPIP interprets time-based input based on your local clock and adjusts its actions accordingly. If you use a local clock (in UNIX, set by the TZ environment variable) that is subject to significant shifts, such as between standard time and daylight savings, when the time shifts back (for example, from daylight to standard), the time change may cause this error. One way to address this is to, possibly temporarily, switch your local time setting to UTC.

--------------------
JNLTMQUAL4
--------------------

JNLTMQUAL4, Time qualifier BEFORE_TIME="xxxx is less than AFTER_TIME="yyyy"

MUPIP Error: This indicates that the specified BEFORE_TIME xxxx is earlier than the AFTER_TIME yyyy specified

Action: Specify correct values for the time qualifiers and make sure that AFTER_TIME is earlier than BEFORE_TIME qualifier.

----------------------
JNLTNOUTOFSEQ
----------------------

JNLTNOUTOFSEQ, End transaction aaaa of journal xxxx different from Begin transaction bbbb of next generation journal yyyy

MUPIP Error: MUPIP JOURNAL FORWARD command has found that the transaction numbers (aaaa and bbbb) of two consecutive generation journal files (xxxx and yyyy) are not in sequence. It is expected that the end transaction of a journal file is the same as the begin transaction of the immediately succeeding generation.

Action: Ensure that the specification of journal file names is as intended. Verify if the journal file xxxx and yyyy are really in sequence. Find out if any of the transactions are missing or duplicate using MUPIP JOURNAL SHOW=HEAD FORWARD NOVERIFY. If appropriate, force forward recovery using the NOTNCHECK qualifier.

------------------------
JNLTPNEST
------------------------

JNLTPNEST, Mupip journal command found nested TP transactions for journal file jjjj at offset oooo at transaction number nnnn

MUPIP Warning: MUPIP JOURNAL -RECOVER or ROLLBACK encountered a TSTART record for transaction nnnn at offset oooo in journal file jjjj while already processing an uncommitted transaction. Since the run-time system should never produce this situation, the journal file is suspect. MUPIP discards the in-progress transaction and proceeds.

Action: Extract the journal file(s) and use the context from the message to find the transactions in question and adjust for any lost or tangled transaction(s).

------------------------
JNLTRANS2BIG
------------------------

JNLTRANS2BIG, Transaction needs an estimated [aaaa blocks] in journal file xxxx which exceeds the AUTOSWITCHLIMIT of bbbb

Run Time Error: This indicates that a database update transaction needs aaaa blocks of space in the journal file for its corresponding journal records and this exceeds the AUTOSWITCHLIMIT value of the current journal file.

Action: Increase the AUTOSWITCHLIMIT, or if the transaction is a TP transaction decrease the number of updates done within one transaction thereby decreasing its journal file space requirement.

-------------------------
JNLTRANSGTR
-------------------------

JNLTRANSGTR, Transaction number in journal is greater than in database

Run Time Warning: This indicates that YottaDB was unable to open the journal file because its transaction number does not match the database files transaction number.

Action: YottaDB automatically closes the current journal file and creates a new one. To reestablish durability, perform MUPIP BACKUP to create a fresh set of journals consistent with the database. Review the accompanying message(s) for information on the journal file name.

---------------------
JNLTRANSLSS
---------------------

JNLTRANSLSS, Transaction number in journal is less than in database

Run Time Warning: This indicates that YottaDB was unable to open the journal file because its transaction number does not match the database files transaction number.

Action: YottaDB automatically closes the current journal file and creates a new one. To reestablish durability, perform MUPIP BACKUP to create a fresh set of journals consistent with the database. Review the accompanying message(s) for information on the journal file name.

--------------------
JNLUNXPCTERR
--------------------

JNLUNXPCTERR, Unexpected error encountered for Journal aaaa at disk address 0xbbbb

MUPIP Error: This indicates that MUPIP JOURNAL has detected an unexpected error in the journal file that prevents the command from proceeding. A recovery or rollback that uses this journal file cannot successfully complete.

Action: Report the entire incident context to your YottaDB support channel.

---------------------
JNLVSIZE
---------------------

JNLVSIZE, Journal File xxxx has incorrect virtual_filesize aaaa Allocation is bbbb extension is cccc filesize is dddd file_system_block_size is eeee

Run Time Error: This indicates that journal file xxxx has incorrect value in the Virtual filesize file header field. Either it is less than the actual filesize or it is not the same as allocation + n * extension.

Action: Run time system creates and switches to a new journal file and continues to run. Report to your YottaDB support channel with any accompanying message(s).

----------------------
JNLWRERR
----------------------

JNLWRERR, Error writing journal file jjjj. Unable to update header Region: yyyy

Run Time/MUPIP Error: This indicates that YottaDB encountered an error while updating the journal file header as part of trying to open the journal file.

Action: Review the accompanying message(s) for detail on the cause of the error. YottaDB automatically closes the current journal file and creates a new one. To reestablish durability, perform MUPIP BACKUP to create a fresh set of journals consistent with the database.

----------------------
JNLWRTDEFER
----------------------

JNLWRTDEFER, Journal write start deferred

Run Time Information: This message always accompanies some other YottaDB journaling message. This indicates that a flush of the journal buffer to the disk system was deferred since some other process is currently busy flushing the journal buffer.

Action: Review the accompanying message(s)for additional information.


---------------------
JOBACTREF
---------------------

JOBACTREF, Actual parameter in job command passed by reference

Compile Time Error: This indicates that arguments to JOB cannot be passed by reference.

Action: Arguments to JOB must be passed by value.

---------------------
JOBEXAMDONE
---------------------

JOBEXAMDONE, YottaDB process aaaa completed job examine to xxxx

Run Time/Operator log Information: This informational message reports that a $ZJOBEXAM was performed and gives a complete file specification. The message is sent to the operator log.

Action: -

----------------------
JOBEXAMFAIL
----------------------

JOBEXAMFAIL, YottaDB process aaaa executing $ZJOBEXAM function failed with the preceding error message

Run Time/Operator log Error: This is a secondary message that accompanies a $ZJOBEXAM function error. This error message is sent to the operator log.

Action: Review the accompanying message(s) and take appropriate action.

-----------------------
JOBFAIL
-----------------------

JOBFAIL, JOB command failure

Run Time Error: This indicates that a JOB command did not complete successfully.

Action: Review the accompanying message(s) for additional information. If a STARTUP jobparameter is specified, make sure that the file is accessible and has the desired content.


-------------------
JOBLABOFF
-------------------

JOBLABOFF, Label and offset not found in created process

Run Time Error: This indicates that a JOB command specified an entry reference that could not be located in the image used by the new job.

Action: Verify that the image being JOBbed is properly linked. If the image has been changed since it was last LINKed, determine whether the new job has access to the files necessary to ZLINK the changes. You can also LINK the image to include the changes.

---------------------
JOBLVN2LONG
---------------------

JOBLVN2LONG, The zwrite representation of a local variable transferred to a JOB'd process is too long. The zwrite representation cannot exceed MMMM. Encountered size: LLLL

Run Time Error: This error indicates that the total length LLLL (in bytes) of the ZWRITE representation of the variable name, subscripts, and value exceeds the maximum MMMM supported by the PASSCURLVN facility. Note that the ZWRITE representation contains the appropriate punctuation for any subscripts, the equal-sign and replaces any non-graphic characters with their $[Z]CHAR() representations.

Action: Consider whether the JOB'd process needs the variable(s) that exceed the maximum for PASSCURLVN - if not, they can be taken out of scope before the JOB command. Alternatively, pass them using global variables or a local SOCKET device.

----------------------
JOBPARNOVAL
----------------------

JOBPARNOVAL, This job parameter cannot take a value

Compile Time Error: This indicates that a JOB command specified a value for a job parameter that does not accept a value.

Action: Modify the job parameter or remove its argument.

-----------------------
JOBPARNUM
-----------------------

JOBPARNUM, The value of this job parameter must be an integer

Compile Time Error: This indicates that a JOB command specified a valid job parameter but it did not assign an integer value, which is required.

Action: Verify that the job parameter has an integer literal argument and not a string or variable argument.

------------------------
JOBPARSTR
------------------------

JOBPARSTR, The value of this job parameter must be a string

Compile Time Error: This indicates that a JOB command specified a valid job parameter but did not assign the job parameter a string value as expected.

Action: Ensure that the job parameter has a string literal argument and not a variable or keyword argument.

--------------------
JOBPARTOOLONG
--------------------

JOBPARTOOLONG, Total parameter length is too long for job command

Run Time Error: This indicates that the total length of job parameters that must be passed to the created job exceeded the size of the buffer that was available to handle them.

Action: Use fewer and/or shorter items in parameter passing; consider passing information in a global.

---------------------
JOBPARUNK
---------------------

JOBPARUNK, Job parameter unknown

Compile Time Error: This indicates that a JOB command specified an invalid jobparameter keyword.

Action: Specify a valid jobparameter keyword. Refer to the `Programmer's Guide <../ProgrammersGuide/index.html>`_ for a valid keyword.

-----------------------
JOBPARVALREQ
-----------------------

JOBPARVALREQ, A value is required for this job parameter

Compile Time Error: This indicates that a JOB command specified a valid job parameter but did not assign the jobparameter a value, which is required.

Action: Review the job parameters for proper assignments.

--------------------
JOBSETUP
--------------------

JOBSETUP, Error receiving aaaa from parent process

Run Time Error: This message indicates that a process created by the JOB command was unable to receive setup information aaaa from the process which issued the JOB command.

Action: Report this and the associated SYSTEM-E-ENO## message to your YottaDB support channel.

------------------
JOBSTARTCMDFAIL
------------------

JOBSTARTCMDFAIL, JOB command STARTUP script invocation failed.

Run Time Error: This message indicates STARTUP script specified as JOB command process parameter failed.

Action: Verify the STARTUP script is present, and check it has appropriate permissions to execute.

-----------------
JRTNULLFAIL
-----------------

JRTNULLFAIL, Applying NULL journal record failed. Failure code: xxxx.

Update Process log/MUPIP Error: Issued by an Update Process, MUPIP JOURNAL -ROLLBACK or MUPIP JOURNAL -RECOVER indicating it encountered a database problem when it attempted to play a NULL journal record into the database. xxxx contains the failure codes for the four attempts. It is very likely that the database may have integrity errors or that the process-private data structures are corrupted.

Action: Report the entire incident context to your YottaDB support channel for further analysis.


---------------------
JUSTFRACT
---------------------

JUSTFRACT, Fraction specifier to $JUSTIFY cannot be negative

Run Time Error: This indicates that a $JUSTIFY or $FNUMBER function specified a negative value as its third argument.

Action: Modify the third argument of the function so that it specifies the positive number of fractional digits to which the function rounds its result.

-----------------
KEY2BIG
-----------------

KEY2BIG, Key size (xxxx) is greater than maximum (yyyy) for region zzzz

Run Time Error: This indicates that the key size of xxxx bytes for the specified global variable name (which includes an overhead of 2 bytes in addition to the length of the global name) exceeds the maximum key size yyyy specified in the database file header for the current region zzzz.

Action: Use global variable names mapping to region zzzz that are smaller in length OR use MUPIP SET -REGION zzzz command with the -KEY_SIZE qualifier to modify the maximum key size as required by the application.

--------------------
KEYFORBLK
--------------------

KEYFORBLK, But block size bbbb and reserved bytes rrrr limit key size to kkkk.

GDE Error: The maximum key for a region must fit in the block size less record overhead and any reserved bytes for that region; kkkk is the maximum key size for block size bbbb.

Action: Reduce the key size or reserved bytes or increase the block size.

----------------
KEYSIZIS
----------------

KEYSIZIS, Key size is xxxx

GDE/DSE Information: This message displays the maximum key size xxxx of the REGION with which you are working.

Action: -


-----------------
KEYTOOBIG
-----------------

KEYTOOBIG, But record size xxxx can only support key size yyyy

GDE Warning: This indicates that an ADD, CHANGE, or TEMPLATE command specified a value for the KEYSIZE qualifier that is incompatible with the value xxxx assigned to RECORDSIZE. yyyy is the maximum value of KEYSIZE that this RECORDSIZE value can support.

Action: Review the accompanying message for the key size. Modify the key size and/or record size so that they are compatible.

-----------------
KEYWRDAMB
-----------------

KEYWRDAMB, xxxx is ambiguous for yyyy

GDE Error: This indicates that the keyword xxxx is ambiguous for the command or local qualifier yyyy.

Action: Ensure that the command or qualifier has enough characters to differentiate it from similar command elements.

-------------------
KEYWRDBAD
-------------------

KEYWRDBAD, xxxx is not a valid yyyy in this context

GDE Error: This indicates that GDE did not encounter a valid syntax element. xxxx is the invalid element. yyyy designates whether the element in context is a verb (command), object, or qualifier.

Action: Look for and correct typographical errors.

-----------------
KILLABANDONED
-----------------

KILLABANDONED, Abandoned kills counter is greater than zero for file ffff, tttt

Run Time Error: This indicates a process terminated during KILL cleanup in database file ffff; tttt is text warning of the implications. Generally, this leaves a database with "block incorrectly marked busy" errors. Such errors are benign in that they only cause blocks to be inappropriately unavailable. Nonetheless they should be addressed promptly to avoid operators becoming desensitized to errors in INTEGs.

Action: Use DSE MAP to carefully FREE individual incorrectly marked busy blocks. If there are many blocks, you can edit the output of the integ (run with NOMAP) to create a script for driving repeated DSE MAP FREE. Alternatively, if you can get standalone access to the database you may use DSE MAP RESTORE - never use MAP RESTORE on an active database.

-------------------
KILLBYSIG
-------------------

KILLBYSIG, Process xxxx has been killed by a signal yyyy

Run Time Error: This indicates that the xxxx process failed due to signal yyyy.

Action: Preserve the core (dump) files and report the entire incident context to your YottaDB support channel for further analysis.

-------------------
KILLBYSIGSINFO1
-------------------

KILLBYSIGSINFO1, iiii process xxxx has been killed by a signal yyyy at address aaaa (vaddr bbbb)

Run Time Error: This indicates that the process failed due to the yyyy signal, which occurred at the code address aaaa. bbbb is the virtual address attempting to be accessed from code address aaaa.

Action: Preserve the core (dump) files and report the entire incident context to your YottaDB support channel for further analysis.

------------------
KILLBYSIGSINFO2
------------------

KILLBYSIGSINFO2, iiii process xxxx has been killed by a signal yyyy at address aaaa

Run Time Error: This indicates that the process iiii failed due to a signal, which occurred while attempting a memory access with an instruction at location aaaa.

Action: Preserve the core (dump) files and report the entire incident context to your YottaDB support channel for further analysis.

-------------------
KILLBYSIGSINFO3
-------------------

KILLBYSIGSINFO3, iiii process xxxx has been killed by a signal yyyy accessing vaddress aaaa

Run Time Error: This indicates that the iiii (YottaDB, MUPIP, DSE, and so on) process failed due to the yyyy signal, which occurred while attempting to access virtual address aaaa.

Action: Preserve the core (dump) files and report the entire incident context to your YottaDB support channel for further analysis.

--------------------
KILLBYSIGUINFO
--------------------

KILLBYSIGUINFO, Process xxxx has been killed by a signal yyyy from process zzzz with userid number aaaa

Run Time Error: This indicates that the process failed due to a signal, sent by another process zzzz, owned by user ID aaaa.

Action: Preserve the core (dump) files and report the entire incident context to your YottaDB support channel for further analysis.

-----------------
LABELEXPECTED
-----------------

LABELEXPECTED, Label expected in this context

Compile Time Error: This indicates that YottaDB did not find a valid line reference where expected.

Action: Look for a missing label in an extrinsic or in a command such as DO, GOTO, or JOB.

-------------------
LABELMISSING
-------------------

LABELMISSING, Label referenced but not defined : xxxx

Compile Time Error: This indicates that a transfer of control command specified a label xxxx that does not exist in the routine.

Action: Look for a missing or misspelled label.

-------------------
LABELNOTFND
-------------------

LABELNOTFND, GOTO referenced a label that does not exist

Run Time Error: A GOTO referenced a label with neither a routine nor an offset but that label does not currently exist in the current routine. The location that accompanies this message is the last line in the routine.

Action: Check the errors from the compilation, as they provide the name of the missing label. As appropriate, add the label or a routine, or better yet - refactor to remove the GOTO.

.. _labelonly-error:

---------------------
LABELONLY
---------------------

LABELONLY, Routine xxxx was compiled for label-only entry.

Run Time Error: This indicates that a transfer of control command specified an offset in routine xxxx that was compiled with the NOLINE_ENTRY qualifier; therefore, it can be invoked only at a label.

Action: Modify the invocation or recompile the routine without the NOLINE_ENTRY qualifier.

--------------------
LABELUNKNOWN
--------------------

LABELUNKNOWN, Label referenced but not defined

Compile Time Error: This indicates that a transfer of control command specified a label that is not defined in the image.

Action: Look for an unresolved reference in the last LINK caused by a missing or misspelled label.

------------------
LASTFILCMPLD
------------------

LASTFILCMPLD, The file currently being compiled is xxxx

Compile Time Information: This indicates that the YottaDB compiler encountered a <CTRL>-C in the input stream and issued this status. xxxx is the name of the routine the compiler is currently processing.

Action: Use <CTRL>-Y to abort the process.

------------------
LASTWRITERBYPAS
------------------

LASTWRITERBYPAS, The last writer for database file xxxx bypassed the rundown

All YottaDB Components Warning: This indicates that the last process which had the xxxx database file open for writing bypassed the rundown while disconnecting.

Action: This may occur due to an instance freeze. If so, first ensure that the Instance Freeze is resolved, manually clearing the Instance Freeze if necessary. If there is a source server still running, a normal shutdown of the source server will perform the rundown. Otherwise, the DSE ALL -B[UFFER_FLUSH] command may be used to ensure that any changes remaining in shared memory are written to disk.

--------------------
LCKGONE
--------------------

LCKGONE, Lock removed: xxxx

LKE Success: This indicates that CLEAR removed an M LOCK. xxxx is the resource name.

Action: -

------------------
LCKSCANCELLED
------------------

LCKSCANCELLED, Error on remote node holding locks or zallocates. All locks and zallocates cancelled.

Run Time Error: This indicates that when a YottaDB process encounters a network error that involves a node holding LOCKs and/or ZALLOCATEs, the process attempts to cancel all LOCKs and ZALLOCATEs regardless of their node.

Action: If YottaDB determines that communication with any part of its lock database is suspect, it releases all locks to establish a known state and minimize the impact of the failure on remaining network processes. After this error occurs, ensure that any restart reinstates ALL locks.

--------------------
LCKSGONE
--------------------

LCKSGONE, Locks selected for deletion removed

LKE Success: This indicates that CLEAR removed an M LOCK on a remote database.

Action: -

------------------
LCKSTIMOUT
------------------

LCKSTIMOUT, DAL timed lock request expired

Run Time Warning: This indicates that a call to seize a named M resource specified a timeout, and the resource was not available within the timeout.

Action: This is a normal signal to the calling process. If it occurs at an inappropriate time, use LKE to examine the lock environment.

------------------
LDBINFMT
------------------

LDBINFMT, Unrecognized header for load file

MUPIP Error: This message identifies a MUPIP load file that is not having the correct header format in either BINARY, ZWR or GO format.

Action: Examine the file with a text editor for possible correction to format header. If fixing the header does not resolve the error, attempt MUPIP EXTRACT with a different file format.

------------------
LDGOQFMT
------------------

LDGOQFMT, Corrupt GOQ format header information

MUPIP Error: This indicates that MUPIP terminated the loading of a GOQ format file because of a corrupt file header.

Action: Ensure that the proper tape is mounted and review how it was created. Use the host shell DUMP command to examine the first few blocks of the tape.

------------------
LDSPANGLOINCMP
------------------

LDSPANGLOINCMP, Incomplete spanning node found during load at File offset : oooo

MUPIP Error: This error indicates that MUPIP LOAD encountered an issue with a spanning node in the input file at offset oooo. MUPIP LOAD produces the following LDSPANGLOINCMP errors:

- Expected chunk number : ccccc but found a non-spanning node
- Expected chunk number : ccccc but found chunk number : ddddd
- Not expecting a spanning node chunk but found chunk : ccccc
- Global value too large: expected size : sssss actual size : tttttt chunk number : ccccc
- Expected size : sssss actual size : ttttt

Action: Refer to the LDSPANGLOINCMP Errors section in the `Maintaining Database Integrity chapter of the Administration and Operations Guide <../AdminOpsGuide/integrity.html>`_.

----------------------
LIBYOTTAMISMTCH
----------------------

LIBYOTTAMISMTCH, $ydb_dist/libyottadb.so does not match the shared library path.

Runtime Error: This indicates that the full path of the currently running libyottadb.so shared library does not match the path described by $ydb_dist. This is possible for example if a C program tries to directly invoke a base image function (e.g. ydb_main, dse_main, mupip_main etc.) for more than one build/release of YottaDB in the same process.

Action:  Make sure a C program invokes a base image function of only one libyottadb.so executable.

-----------------------
LINKVERSION
-----------------------

LINKVERSION, This image must be relinked with the current version of YottaDB

Run Time Fatal: This indicates that YottaDB attempted to access an image that was created with a previous version of YottaDB.

Action: Relink the image using the current version of YottaDB. If the previous version of YottaDB is still available, adjust the logical names to activate the appropriate old version of YottaDB.

---------------------
LISTENPASSBND
---------------------

LISTENPASSBND, Control mnemonic LISTEN can be applied to PASSIVE socket which is in the state BOUND ONLY

Run Time Error: This indicates that the LISTEN control mnemonic can only be applied to passive sockets in a bound state.

Action: Use ZSHOW to verify that the command syntax is correct. Use the USE command to bind the socket.

----------------------
LITNONGRAPH
----------------------

LITNONGRAPH, standard requires graphics in string literals; found non-printable: $ZCHAR(cccc)

Compile Time Warning: Flags a standard violation. The generated code will accept the string, even though it contains cccc, which is not a visible character.

Action: Consider revising the literal to use $[Z]CHAR() and possibly concatenation to make the code more maintainable.

-----------------------
LKENOFINISH
-----------------------

LKENOFINISH, LKE unable to finish all requested actions

LKE Error: This indicates that the previously reported error(s) prevented LKE from completing the requested action.

Action: Review the accompanying error message(s).

---------------------
LKNAMEXPECTED
---------------------

LKNAMEXPECTED, An identifier is expected after a ^ in this context

Compile Time Error: This indicates that the LOCK command specified an argument that started with ^, but does not contain a valid global name.

Action: Look for and correct any typographical errors or attempted naked references in LOCK names.

-------------------------
LKRUNDOWN
-------------------------

LKRUNDOWN, Error during lock database rundown

Run Time Error: This indicates that the process encountered an error when it attempted to release its LOCKs as part of image termination.

Action: Report this error to the group responsible for database integrity within your organization. Although this is not strictly a database error, other processes can be affected if LOCKs were left behind.

-------------------------
LKSECINIT
-------------------------

LKSECINIT, Error creating lock section for database xxxx

Run Time Error: This indicates that YottaDB encountered a problem initializing the lock database associated with the database file (xxxx) it was trying to open.

Action: Review the accompanying message(s) for additional information.

----------------------
LNKNOTIDLE
----------------------

LNKNOTIDLE, Attempt to initiate operation before previous operation completed

GT.CM Error: This indicates that the networking protocol failed by trying to open an already accessed connection.

Action: Review DECnet error logs to determine the cause and location of the failure. Report the entire incident context to your YottaDB support channel.

-----------------
LOADABORT
-----------------

LOADABORT, Aborting load at record xxxx

MUPIP Error: This indicates that LOAD encountered an error while processing input record number xxxx.

Action: Refer to the topic `MUPIP LOAD Errors in the About This Manual section <./about.html#mupip-load-errors>`_.

------------------
LOADBGSZ
------------------

LOADBGSZ, Load error: BEGIN too small. No records loaded.

MUPIP Error: This indicates that an operation initiated by MUPIP LOAD with the qualifier FORMAT=GO did not take place because the record specified for the qualifier BEGIN= is negative or zero.

Action: Specify a record that is within the actual file.

--------------------
LOADCTRLY
--------------------

LOADCTRLY, Control Y encountered during load. Load halting.

MUPIP Warning: This indicates that LOAD encountered a <CTRL>-Y in its input stream and terminated.

Action: The result of the LOAD is incomplete. If the LOAD was with FORMAT=GO, the database is usable. If the LOAD was with FORMAT=BIN, the database may be corrupt.

--------------------
LOADEDBG
--------------------

LOADEDBG, Load error: END smaller than BEGIN. No records loaded.

MUPIP Error: This indicates that a MUPIP LOAD operation did not occur because the record specified for the qualifier END= is smaller than the record specified for the qualifier BEGIN=.

Action: Specify a record for the qualifier END= that is greater than or equal to the record for the qualifier BEGIN=.

-------------------
LOADEDSZ
-------------------

LOADEDSZ, Load error: END too small. No records loaded.

MUPIP Error: This indicates that a MUPIP LOAD operation did not occur because the record specified for the qualifier END= is smaller than 2.

Action: Modify the qualifier END= value.

-----------------
LOADEOF
-----------------

LOADEOF, Load error: EOF reached prior to BEGIN record xxxx. No records loaded.

MUPIP Error: This indicates that LOAD did not transfer any records to the database because its input steam reached the end-of-file before the record specified by the qualifier BEGIN=xxxx.

Action: Specify a record for the qualifier BEGIN= that does not exceed the number of records in the file.

-----------------
LOADFILERR
-----------------

LOADFILERR, Error with load file xxxx

MUPIP Error: This indicates that LOAD encountered an error when opening its input file xxxx.

Action: Make sure correct load file has been specified. Review the accompanying message(s) for additional information.

-------------------
LOADFMT
-------------------

LOADFMT, Load error: bad format type. Must be GO, BINARY, or GOQ.

MUPIP Error: This indicates that a MUPIP LOAD operation did not take place because the qualifier FORMAT= specified an unsupported format.

Action: Look for and correct any typographical errors in the qualifier FORMAT= value.

------------------
LOADGD
------------------

LOADGD, Loading Global Directory xxxx

GDE Information: GDE displays this message at the beginning of a GDE session when Global Directory xxxx already exists.

Action: -

---------------------
LOADINVCHSET
---------------------

LOADINVCHSET, Extract file CHSET xxx is incompatible with ydb_chset/gtm_chset.

MUPIP Information: This indicates that a MUPIP LOAD operation did not take place because the value of the environment variable ydb_chset or gtm_chset at the time of creating the extract file was not the same as the current value of ydb_chset or gtm_chset.

Action: Determine whether to change the current character set or retry the EXTRACT with a different character set. Alternatively, you can edit the extract file so the EXTRACT file header matches the ydb_chset environment variable. This enables an M mode MUPIP LOAD to treat the input as a byte stream or a UTF-8 mode MUPIP LOAD, which either detects BADCHAR errors or not, depending on the setting of the ydb_badchar environment variable.

-------------------
LOADRECCNT
-------------------

LOADRECCNT, Last EXTRACT record processed by LOAD: RRRR

MUPIP Information: This message indicates number of records (RRRR) MUPIP LOAD processed. The number of records represents the sum of header records, successfully loaded data records, and failed records. Note LOAD may have stopped processing due to a record limit in the command or a <CTRL-C>.

Action: Ensure the identified stopping point corresponds with your intentions.

-------------------
LOADRUNNING
-------------------

LOADRUNNING, Cannot ZLINK an active routine xxxx

Run Time Error: This indicates that a ZLINK specified a routine xxxx, that is currently on the M invocation stack. A routine cannot be altered in the image if its current form may be required for continued processing.

Action: ZLINK the routine prior to or after running it. Use QUIT or ZGOTO to remove the routine from the M stack.

-----------------------
LOCALSOCKREQ
-----------------------

LOCALSOCKREQ, LOCAL socket required

Run Time Error: The operation attempted requires a LOCAL socket, and a non-LOCAL (TCP) socket was specified.

Action: Make sure the correct socket is being used and that the socket is OPENed with the ":LOCAL" suffix. ZSHOW "D" may provide useful details on the current socket state.

----------------------
LOCKCRITOWNER
----------------------

LOCKCRITOWNER, LOCK crit is held by: PPPP

Run Time/LKE Information: This shows any current owner of the resource managing M LOCKs.

Action: If a process persists in this state investigate what it's doing and, if appropriate, consider terminating it.

------------------------
LOCKINCR2HIGH
------------------------

LOCKINCR2HIGH, Attempt to increment a LOCK more than LLLL times

Run Time Error: This message indicates that a LOCK + command attempted to increase a LOCK increment higher than LLLL (the maximum level). The following associated message gives the resource name for the LOCK that failed.

Action: Examine the application for pathological use of incremental LOCKs (LOCK +) and ensure that no process LOCKs a single resource more than 511 increments with no intervening decrements (LOCK -) for that resource or lock releases (LOCK with no + or - in its argument).

-------------------
LOCKIS
-------------------

LOCKIS, Resource name: RRRR

Run Time Information: This message identifies a lock resource.

Action: Refer to the accompanying message(s) for more information.

------------------
LOCKSPACEFULL
------------------

LOCKSPACEFULL, No more room for LOCK slots on database file ffff

Run Time Error: This indicates that the environment attempted more concurrent M LOCKs than the configured LOCK_SPACE for file ffff can support.

Action: Analyze the LOCK protocol for efficiency. Use mupip set -file -lock_space=size ffff to increase the lock space for region xxx. To avoid the same problem the next time you recreate the database, use GDE to make the analogous change to lock_space for the segment mapped to the ffff file in the global directory used to MUPIP CREATE this region.

-------------------
LOCKSPACEINFO
-------------------

LOCKSPACEINFO, Region: rrrr: processes on queue: pppp/qqqq; LOCK slots in use: llll/kkkk; SUBSCRIPT slot bytes in use: ssss/tttt.

Run Time Error: This indicates that the environment attempted more concurrent M LOCKs than the configured LOCK_SPACE for region rrrr can support. pppp processes are waiting for a lock. llll locks are in use. qqqq and kkkk indicate maximum number of process queue entries, and maximum number of locks respectively.

Action: Analyze the LOCK protocol for efficiency. Use mupip set -region -lock_space=size "rrrr" to increase the lock space for region rrrr. To avoid the same problem the next time you recreate the database, use GDE to make the analogous change to lock_space for the segment mapped to the ffff file in the global directory used to MUPIP CREATE this region.

-------------------
LOCKSPACEUSE
-------------------

LOCKSPACEUSE, Estimated free lock space: xxx% of pppp pages.

LKE Information: SHOW command displays the amount of free space along with the number of pages configured for lock space.

Action: If the free lock space report does not show a comfortable amount of free space, use MUPIP SET -LOCK_SPACE to increase the space; remember to also use GDE to revise the LOCK_SPACE in the global directory used to create the region in question so the change remains when the database is recreated.

--------------------
LOCKSUB2LONG
--------------------

LOCKSUB2LONG, Following subscript is xxxx bytes long which exceeds 255 byte limit.

Run Time Error: This indicates that one of the substrings of a lock is taking more than 255 bytes. Check the following message to see which substring caused this error.

Action: Make sure none of the substrings are larger than 255 bytes. If UTF-8 is enabled, use the encoded byte length rather than the character length for the key size.

---------------------
LOCKTIMINGINTP
---------------------

LOCKTIMINGINTP, A LOCK at pppp within a TP transaction is waiting in a final TP retry, which may lead to a general response gap

Run Time Warning: This message indicates that a LOCK command at location pppp with a non-zero (0) or no timeout and within a critical resource holding retry of a TP transaction is waiting to acquire a resource currently owned by another process. This condition may cause other processes to pause for perceptible periods. The associated LOCKIS message identifies the LOCK resource name.

Action: Examine the application, especially at pppp, for pathological use of LOCKs within TP. A zero (0) timeout prevents this warning. Note that YottaDB recommends avoiding the use of LOCK commands within TP transactions.

------------------
LOGOFF
------------------

LOGOFF, No longer logging to file xxxx

GDE Information: This indicates that a LOG command with the qualifier OFF terminated logging of GDE commands to log file xxxx.

Action: When appropriate, resume logging with LOG and the qualifier ON[=]. GDE closes the log file(s) at the end of the GDE session.

--------------------
LOGON
--------------------

LOGON, Logging to file xxxx

GDE Information: This indicates that a LOG command with the qualifier ON[=] initiated the logging of GDE commands to log file xxxx.

Action: You can suspend logging with LOG and the qualifier OFF.

-------------------
LOGTOOLONG
-------------------

LOGTOOLONG, Environment variable eeee is too long. Maximum length allowed is llll bytes.

Information: This error is triggered whenever the length of an environment variable that YottaDB cares about exceeds the maximum allowed limit.

Action: The maximum allowed limit is indicated in the message. Specify a value for the environment variable within this length.

-------------------
LOWSPACECRE
-------------------

LOWSPACECRE, Disk space for database file xxxx is not enough for yyyy future extension. aaaa blocks are needed, only bbbb available.

MUPIP Warning: This indicates that the database file xxxx was created but it was found that the file system/volume does not have enough space for even yyyy future extensions.

Action: Check the allocations and extension sizes specified in the Global Directory. If no extensions are anticipated, no action is required. Otherwise, consider moving some files to another file system/volume, or reconfiguring the file system/volume housing the database file.

-----------------
LOWSPC
-----------------

LOWSPC, WARNING: Database DDDD has less than PPPP% of the total block space remaining. Blocks Used: UUUU Total Blocks Available: AAAA

Operator log Information: The database has UUUU blocks in use and is appoaching its current limit of AAAA blocks. When the database reaches the 88% size threshold, and for every 1% increase in size and beyond, YottaDB reports the blocks used in the LOWSPC warning as the sum of the data blocks and the local bit map blocks.

Action: Purge data if possible. Consider a MUPIP REORG to compact the remaining data. Investigate whether migrating to a database created by a current version has a higher limit. Move some data to another, possibly new, region and delete it from this one.

-------------------
LPARENMISSING
-------------------

LPARENMISSING, Left parenthesis expected

Compile Time Error: This indicates that YottaDB did not find a left parenthesis in the next source position.

Action: Look for invalid subscripts in indirection operations and errors in SET $PIECE commands.

---------------
LPARENREQD
---------------

LPARENREQD, xxxx Left parenthesis expected

MUPIP Error: This indicates that LOAD failed because it found xxxx in the input stream where it expected to find a left parenthesis.

Action: Refer to the topic `MUPIP LOAD Errors in the About This Manual section <./about.html#mupip-load-errors>`_.

-----------------
LQLENGTHNA
-----------------

LQLENGTHNA, Listening queue length xxxx not appropriate, it should be between 1 and 5

Run Time Error: This indicates that the YottaDB listening queue restricts the number of pending connections between one (1) and five (5).

Action: Specify the number of pending connections in the queue as a number between one and five.

-----------------
LSEXPECTED
-----------------

LSEXPECTED, A line separator is expected here

Compile Time Error: This indicates that a source line did not specify a space or tab before the first command.

Action: Look for and correct typographical errors. If missing, put a tab or space at the beginning of the line.

-----------------
LSINSERTED
-----------------

LSINSERTED, Line YYYY, source module XXXX exceeds maximum source line length; line seperator inserted, terminating scope of any prior IF, ELSE, or FOR

Compile Time Warning: Indicates that source XXXX line YYYY exceeded the maximum line length and YottaDB separated it into multiple lines to allow continued parsing. Internally, YottaDB represents the generated code as N lines for this source line, where N is the number of segments extracted from this source line. Be aware that as a result of this, source lines containing a command whose scope is the rest of the line (IF, ELSE, FOR), are now split into multiple lines, each with a separate scope.

Action: Consider refactoring code to avoid source line lengths in excess of 8192 characters.

---------------
LVNULLSUBS
---------------

LVNULLSUBS, LVNULLSUBS Null subscripts not allowed in local variables

Run Time Error: This indicates that an attempt was made to set a local variable with a null subscript.

Action: Modify the generation of subscripts to avoid the null subscript or change the LVNULLSUBS parameter for this job or process. For information on changing LVNULLSUBS, refer to documentation on GTM$DEFAULTS and the VIEW command in the `Programmer's Guide <../ProgrammersGuide/index.html>`_.

-----------------
LVORDERARG
-----------------

LVORDERARG, Argument to local variable $NEXT must be subscripted

Compile Time Error: This indicates that a $NEXT function specified an unsubscripted local variable as an argument.

Action: Use the $ORDER function or ZWRITE command to display local variables.

----------------
LVSTARALON
----------------

LVSTARALON, The * name cannot be deleted or renamed

GDE Error: This indicates that a DELETE or RENAME command attempted to delete or rename the * namespace. The * namespace is protected because it is associated with namespaces that are not explicitly mapped.

Action: None.

-----------------
LVUNDEF
-----------------

LVUNDEF, Undefined local variable: xxxx

Run Time Error: This indicates that an expression referenced a local variable xxxx, that was not defined.

Action: Ensure that all variables are assigned values before they are referenced; use $GET(), or change the image or process to NOUNDEF mode.

----------------
MALLOCMAXUNIX
----------------

MALLOCMAXUNIX, Exceeded maximum allocation defined by $ydb_max_storalloc.

Run Time Error: This error accompanies a MEMORY error as a secondary error to indicate that the limit the process hit was not an OS limit but one artificially defined by the $ydb_max_storalloc environment variable.

Action: Increase the value of, or unset, $ydb_max_storalloc, or identify the source of the memory consumption (for example, creating and keeping lots of local variables) and reduce it.

-------------------
MAPBAD
-------------------

MAPBAD, xxxx for yyyy does not exist

GDE Information: This indicates that a NAME points to a REGION or a REGION points to a SEGMENT that does not exist. xxxx is the missing object. yyyy describes the type of the object. When you enter the VERIFY or EXIT command, GDE displays this message after it verifies the global directory.

Action: Use the ADD command to add the REGION or SEGMENT.

------------------
MAPDUP
------------------

MAPDUP, xxxx and yyyy both map to zzzz

GDE Information: This indicates that GDE encountered two REGIONs mapped to the same SEGMENT or two SEGMENTS mapped to the same FILE. xxxx and yyyy are the REGIONS or SEGMENTS with the same mapping. zzzz is the SEGMENT or FILE with more than one mapping. When you enter the VERIFY or EXIT command, GDE displays this message after it verifies the global directory.

Action: Delete mappings to eliminate duplication.

----------------
MAXACTARG
----------------

MAXACTARG, Maximum number of actual arguments exceeded

Compile Time Error: This indicates that a DO or extrinsic function supplied an actual list with more than 32 elements.

Action: Modify the routine so that it passes fewer parameters explicitly.

.. _maxargcnt-error:

--------------
MAXARGCNT
--------------

MAXARGCNT, Maximum number of arguments xxxx exceeded

Compile Time/Run Time Error: If this error occurs during compilation, it indicates that a command or function specified more than xxxx arguments. If this error occurs during run-time execution, it indicates that a SET of $ZROUTINES has more than the allowed number of elements in an array. The maximum number of arguments is xxxx. This error can also occur if more than 253 arguments are concatenated. Concatenation in M is described in the `String Operators <../ProgrammersGuide/langfeat.html#m-string-operators>`_ section.

Action: If this error occurs during compilation, it indicates that a command or function specified more than xxxx arguments. If this error occurs during run-time execution, it indicates that a SET of $ZROUTINES has more than the allowed number of elements in an array. The maximum number of arguments is xxxx.

---------------
MAXBTLEVEL
---------------

MAXBTLEVEL, Global ^gggg in region rrrr reached maximum level

Run Time/MUPIP Error: This indicates that the global-variable-tree for global xxxx reached the maximum level permissible. Very likely, MUPIP REORG was specified with a fill-factor much less than 100. Small fill-factors can cause REORG to revise existing GDS-blocks (in order to accommodate the fill-factor requirement), in turn causing block-splits, which might lead to an increase of the tree height. Alternatively, a SET or MERGE has made the global too large for the current block size, which is most likely to happen with large (spanning) database nodes. Note that if this message does not specify the global name, it means that the directory tree for the region hit the limit - YottaDB believes that the "directory tree full" condition is almost impossible to create in practice.

Action: If MUPIP reorg was specified with a small fill-factor, try higher number (close to 100) to reduce tree-height. Other techniques include increasing GDS-block-size, reducing reserved bytes, killing unwanted portions of the tree or moving some nodes in the global to a different database region.

--------------------
MAXFORARGS
--------------------

MAXFORARGS, Maximum number of arguments to a single FOR command exceeded

Compile Time Error: This indicates that a FOR statement specified more than 127 arguments.

Action: Modify the routine so that it uses fewer arguments in one FOR command.

-------------------
MAXGTMPATH
-------------------

MAXGTMPATH, The executing module path is greater than the maximum xxxx

Run Time Error: This indicates that the path specified for the yottadb executable environment variable has a length limitation of xxxx.

Action: Move the directory or use a link to shorten the path.

------------------
MAXNRSUBSCRIPTS
------------------

MAXNRSUBSCRIPTS, Maximum number of subscripts exceeded

Compile Time Error: This indicates that a subscripted variable exceeded the maximum limit of 31 subscripts.

Action: Modify the routine to observe this limit on subscripts in a single variable.

------------------
MAXSEMGETRETRY
------------------

MAXSEMGETRETRY, Failed to get ftok semaphore after tttt tries because it is being continually deleted

Run Time Error: A process was unable to open a database file because on every one of tttt tries, it found that something kept deleting the IPC semaphore that gates access to the file.

Action: Check for one or more rogue processes disrupting IPC semaphore, or for damage to the Operating System semaphore services.

--------------------
MAXSSREACHED
--------------------

MAXSSREACHED, Maximum snapshots - mmmm - for region rrrr reached. Please wait for the existing snapshots to complete before starting a new one.

MUPIP Error: Starting this snapshot would exceed the maximum number of snapshots.

Action: Wait for a currently active process using snapshots to complete or terminate an existing snapshot activity.

.. _maxstrlen-error:

---------------------
MAXSTRLEN
---------------------

MAXSTRLEN, Maximum string length exceeded

Run Time Error: This indicates that a string exceeded the maximum limit of 1,048,576 bytes. In M mode, each byte holds a character, but in UTF-8 mode, a character may take between one and four bytes.

Action: Modify the routine so that it uses shorter strings.

--------------------
MAXTRIGNEST
--------------------

MAXTRIGNEST, Maximum trigger nesting level LLLL exceeded

Trigger/Run Time Error: YottaDB limits trigger invocation depth to LLLL.

Action: If you are sure that you do not have an application code bug or misfeature, reduce the depth of trigger invocation, possibly by consolidating triggers.

---------------------
MBXRDONLY
---------------------

MBXRDONLY, Mailbox is read only, cannot write to it

Run Time Error: This indicates that a WRITE command attempted to access a mailbox that was opened read-only.

Action: Verify that the routine is using the right mailbox and that the mailbox was opened with the appropriate device parameter.

--------------------
MBXWRTONLY
--------------------

MBXWRTONLY, Mailbox is write only, cannot read from it

Run Time Error: This indicates that a READ command attempted to access a mailbox that was opened write-only.

Action: Verify that the routine is using the correct mailbox and that the mailbox was opened with the appropriate device parameter.

--------------------
MEMORY
--------------------

MEMORY, Central memory exhausted during request for xxxx bytes

Compile Time/Run Time Error: This indicates that the compiler or the run-time system could not allocate sufficient storage.

Action: Look for very large variables. This error can also be caused by problems in the YottaDB environment, such as using components of different versions or different platforms. Verify that there is no such problem in the environment.

----------------------
MEMORYRECURSIVE
----------------------

MEMORYRECURSIVE, Memory Subsystem called recursively

Run Time Error: This indicates that YottaDB made an error calling the memory subsystem.

Action: Report the entire incident context to your YottaDB support channel.

----------------------
MERGEDESC
----------------------

MERGEDESC, Merge operation not possible. xxxx is descendent of yyyy.

Run Time Error: This indicates that YottaDB was not able to MERGE xxxx into yyyy or vice versa, because xxxx is a descendent of yyyy. When merging global variables, specifications included extended references - the MERGE command issues a MERGDESC error if any part of the source or target tree, as mapped, is a descendant of the other. In MERGE ^\|"x.gld"\|a(1)=^\|"yottadb.gld"\|a there is no error if yottadb.gld maps ^a to different database files than those to which x.gld maps ^a(1). A MERGDESC error occurs if any part of ^a as mapped by yottadb.gld overlaps any part of ^a(1) as mapped by x.gld.

Action: Modify the routine to avoid MERGE operation between two variables where one is the descendant of the other.

----------------------
MERGEINCOMPL
----------------------

MERGEINCOMPL, Error encountered during MERGE; operation may be incomplete

Run Time Error: This indicates that YottaDB was not able to complete the MERGE operation.

Action: Review the accompanying message(s) for additional information.

--------------------
MINNRSUBSCRIPTS
--------------------

MINNRSUBSCRIPTS, Number of subscripts cannot be a negative number.

Run Time Error: This indicates that the number of subscripts in an input array (usually the "subs_used" parameter in various SimpleAPI calls) is a negative number.

Action: Retry the SimpleAPI call with a subscript count that is greater than or equal to zero.

----------------------
MISSINGDELIM
----------------------

MISSINGDELIM, Delimiter dddd expected before qqqq vvvv

GDE Error: This indicates that the delimiter dddd (usually dash character) is expected just before vvvv is specified. vvvv is a GDE object or qualifier indicated by qqqq.

Action: Specify the delimiter as indicated.

-------------------
MIXIMAGE
-------------------

MIXIMAGE, Cannot load more than one base image function on a process.

Run Time Error: This indicates that a C function tries to invoke more than one base image function included in libyottadb.so (e.g. ydb_main, dse_main, mupip_main etc.). Only one base image function can be invoked and only once for the lifetime of the process.

Action: Make sure only one base image function is invoked for the lifetime of one process.

--------------------
MLKCLEANED
--------------------

MLKCLEANED, LOCK garbage collection freed aaaa lock slots for region rrrr

LKE Information: LKE CLNUP was able to free lock slots when requested.

Action: No action required.

--------------------
MLKHASHRESIZE
--------------------

MLKHASHRESIZE, LOCK hash table increased in size from aaaa to bbbb and placed in shared memory (id = mmmm)

Operator log Information: YottaDB needed to expand a hash table used for managing LOCK information.

Action: No user action is required, but shared memory monitoring will show an additional shared memory segment with id mmmm.

-------------------
MLKHASHRESIZEFAIL
-------------------

MLKHASHRESIZEFAIL, Failed to increase LOCK hash table size from aaaa to bbbb. Will retry with larger size.

Operator log Warning: YottaDB needed to expand a hash table used for managing LOCK information needed to be expanded, but the initial attempt failed, necessitating a retry.

Action: A subsequent MLKHASHRESIZE indicates that the retry succeeded and no user action is required.

-------------------
MLKHASHTABERR
-------------------

MLKHASHTABERR, A LOCK control structure is damaged and could not be corrected. Lock entry for LLLL is invalid.

LKE Error: LKE CLNUP -INTEG encountered an out-of-design situation for LOCK LLLL and was unable to repair it automatically.

Action: Immediately report the entire incident context with information from the operator log and any other relevant information to your YottaDB support channel.

--------------------
MLKHASHWRONG
--------------------

MLKHASHWRONG, A LOCK control structure has an invalid state; LOCK table failed integrity check. TTTT

LKE Error: MLK CLNUP -INTEG encountered damage to the data structures related to LOCK management. The text in TTTT describes whether LKE was able to correct the error or not.

Action: If LKE was not able to correct the error, immediately report the entire incident context with information from the operator log and any other relevant information to your YottaDB support channel as soon as possible.

----------------------
MLKREHASH
----------------------

MLKREHASH, LOCK hash table rebuilt for region rrrr (seed = ssss)

Run Time Information: YottaDB has detected an issue with the LOCK hash table for region rrrr and regenerated it using a new seed value ssss.

Action: This information message confirms the success of the rehash operation. No further action is necessary unless it is issued repeatedly or with a large seed value.

----------------------
MMBEFOREJNL
----------------------

MMBEFOREJNL, BEFORE image journaling cannot be set with MM access method in database file ffff

MUPIP Error: MM access method is incompatible with BEFORE_IMAGE journaling.

Action: If you require BEFORE_IMAGE journaling, use the BG access method. If you wish to use MM, turn off BEFORE_IMAGE journaling before selection MM as the access method.

--------------------
MMFILETOOLARGE
--------------------

MMFILETOOLARGE, Size of rrrr region (ffff) is larger than maximum size supported for memory mapped I/O on this platform.

Run Time Error: YottaDB and its Utility programs issue this to indicate an attempt to open the database ffff corresponding to region rrrr when the size of the database file is greater than the maximum size supported for memory mapped I/O.

Action: Consider as appropriate: migrating to a platform not having this limitation, using more, but smaller, regions, or using the BG access method.

-------------------
MMNOBEFORIMG
-------------------

MMNOBEFORIMG, MM segments do not support before image journaling

GDE Information: This indicates that a JOURNAL=BEFORE_IMAGE region qualifier appeared on a segment that has segment qualifier ACCESS_METHOD=MM.

Action: Change the segment qualifier to ACCESS_METHOD=BG or use NOBEFORE_IMAGE for the region.

----------------------
MMNOBFORRPL
----------------------

MMNOBFORRPL, Replication cannot be used in database file ffff which uses MM access method and NOBEFORE image journaling

MUPIP Error: You can't turn on replication for MM access method database file ffff.

Action: Forgo replication for the file or change the access method to BG.

-----------------------
MMNODYNDWNGRD
-----------------------

MMNODYNDWNGRD, Unable to use dynamic downgrade with MM access method for region xxx. Use BG access method for downgrade.

Run Time/MUPIP Error: An attempt was made to use the MM mode on a database that has not completed being downgraded. MM mode is only supported on fully downgraded or fully upgraded databases.

Action: Use MUPIP SET FILE or MUPIP SET REGION with the ACCESS_METHOD parameter to set the access mode to BG. Then complete the file downgrade using MUPIP REORG DOWNGRADE or file upgrade using MUPIP REORG UPGRADE. And finally set the access mode back to MM using the MUPIP SET FILE or MUPIP SET REGION command again.

---------------------
MMNODYNUPGRD
---------------------

MMNODYNUPGRD, Unable to use MM access method for region yyy until all database blocks are upgraded

Run Time/MUPIP Error: An attempt was made to use MM mode on a database that has not completed being upgraded. MM mode is only supported on fully upgraded or fully downgraded databases.

Action: Use MUPIP SET FILE or MUPIP SET REGION with the ACCESS_METHOD parameter to set the access mode to BG. Then complete the file upgrade using MUPIP REORG UPGRADE. And finally set the access mode back to MM using the MUPIP SET FILE or MUPIP SET REGION command again.

-----------------------
MMREGNOACCESS
-----------------------

MMREGNOACCESS, Region rrrr (ffff) is no longer accessible. See prior error messages in the operator and application error logs

Run Time Error: Issued when a process attempts to access region rrrr corresponding to database file ffff (opened with the MM access method) which previously became inaccessible to this process due to failure during file extension.

Action: Review the operator log for DBFILERR messages and application error logs for GBLOFLOW status to diagnose the circumstances for the earlier failure of memory mapped I/O operations and take corrective action.

------------------------
MPROFRUNDOWN
------------------------

MPROFRUNDOWN, Error during M-profiling rundown

Run Time Error: During process exit, YottaDB attempted to store the results of an M-profiling trace but encountered an error.

Action: Report this database error to the group responsible for database integrity at your operation.

---------------------
MRTMAXEXCEEDED
---------------------

MRTMAXEXCEEDED, Maximum value of xxxx for SOCKET deviceparameter MOREREADTIME exceeded.

Compile Time/Run Time Error: YottaDB triggers this error when MOREREADTIME exceeds its maximum value of 999ms.

Action: Specify a value between 1 and 999. Never set MOREREADTIME to 0 as it may cause a CPU to "spin". See `"Input/Output Processing" Chapter of the Programmer's Guide <../ProgrammersGuide/ioproc.html>`_ for more information.

--------------------
MSTACKCRIT
--------------------

MSTACKCRIT, User-specified M stack size critical threshold of xxxx not appropriate; must be between mmmm and nnnn; reverting to kkkk

Run Time Error: The environment variable ydb_mstack_crit_threshold was set to an invalid value - either too large, in which case YottaDB uses the largest acceptable value or too low, in which case YottaDB uses the smallest acceptable value.

Action: If the adjusted value is unacceptable, revise or unset the environment variable.

---------------------
MSTACKSZNA
---------------------

MSTACKSZNA, User-specified M stack size of SSSS KiB not appropriate; must be between LLLL KiB and MMMM KiB; reverting to VVVV KiB

Run Time Information: The ydb_mstack environment variable species an M stack size outside the range YottaDB supports, where LLLL and MMMM are the lower and upper bounds respectively; VVVV is the value actually used.

Action: None required immediately as the process operates with the reported size M stack, however, it would be preferable to eliminate such messages by setting ydb_mstack to a value in the supported range.

----------------------
MTNOSKIP
----------------------

MTNOSKIP, SKIP operation not supported on this device

Run Time Error: This indicates that the program attempted to use the SKIP deviceparameter for a type of tape that is not able to SKIP.

Action: Remove the SKIP deviceparameter or select a type of tape that supports SKIP.

---------------------
MUBCKNODIR
---------------------

MUBCKNODIR, MUPIP backup aborted due to error in output directory

MUPIP Error: This indicates that the output directory specified in a BACKUP command could not receive the output file.

Action: Use the host shell commands to verify that the output directory exists, that it is properly protected, and has enough space.

-------------------------
MUCREFILERR
-------------------------

MUCREFILERR, Error in/at EEEE creating database DDDD (region RRRR)

Run Time Error: Message accompanying another message indicating the failure to create an autodb database file. EEEE indicates the $ZPOSITION of the application which made the global reference that attempted to bring the database file into existence.

Action: Use the preceding message to diagnose and correct the problem, which may include missing environment variables and/or insufficient space or user privileges. Consider whether autodb creation is appropriate for this database file.

--------------------
MUDWNGRDNOTPOS
--------------------

MUDWNGRDNOTPOS, Start VBN value is [xxx] while downgraded YottaDB version can support only [yyy]. Downgrade not possible

MUPIP Error: Older versions of YottaDB require the first GDS block be at Virtual Block Number yyy but it is at VBN xxx. This is likely due to the file initially being created using a newer version of YottaDB and thus cannot be downgraded.

Action: To use the database with an older version of YottaDB, it must be extracted with the current version and loaded into the older version both in ZWR format.

----------------------
MUDWNGRDNRDY
----------------------

MUDWNGRDNRDY, Database xxx is not ready to downgrade - still yyy database blocks to downgrade

MUPIP Error: A MUPIP DOWNGRADE was attempted when the file-header blks_to_upgrd counter was not equal to the database used block count. This means that not all database blocks have been converted to the previous format.

Action: Before the database file-header can be downgraded, all of the blocks in the database must be downgraded to the previous format. This is normally accomplished with MUPIP REORG DOWNGRADE. If this fails to set the counter correctly, run MUPIP INTEG (not FAST) on the region which will compute and set the correct counter.

------------------------
MUDWNGRDTN
------------------------

MUDWNGRDTN, Transaction number 0xaaa in database xxx is too big for MUPIP [REORG] DOWNGRADE. Renew database with MUPIP INTEG TN_RESET

MUPIP Error: A MUPIP DOWNGRADE or MUPIP REORG DOWNGRADE was attempted when the database transaction number was greater than 4,026,531,839 (the TN_RESET warning limit for previous versions of databases).

Action: Before the database can be downgraded, the transaction number must be reset with the MUPIP INTEG TN_RESET command. This requires standalone access to the database and may take a significant amount of time.

------------------------
MUFILRNDWNFL
------------------------

MUFILRNDWNFL, File: xxxx rundown failed

MUPIP Error: This indicates that the RUNDOWN command could not close a database.

Action: This message indicates that information in memory may need to be transferred to disk. Review the accompanying message(s) for additional information.

-----------------------
MUFILRNDWNFL2
-----------------------

MUFILRNDWNFL2, Database section (id = dddd) belonging to database file ffff rundown failed

MUPIP Error: This error indicates that an argumentless MUPIP RUNDOWN failed for database ffff and could not safely remove the shared memory with ID dddd from the system.

Action: Refer to accompanying messages for more detail on why the argumentless MUPIP RUNDOWN failed.

------------------------
MUFILRNDWNSUC
------------------------

MUFILRNDWNSUC, File successfully rundown

MUPIP Success: This indicates that RUNDOWN ensured that the disk file is current.

Action: -

--------------------
MUINFOSTR
--------------------

MUINFOSTR, xxxx : aaaa

MUPIP Information: MUINFOSTR message is issued by a variety of MUPIP commands to inform the user of the command's progress. This indicates that the string xxxx has the value aaaa.

Action: None necessary.

--------------------
MUINFOUINT4
--------------------

MUINFOUINT4, xxxx : aaaa [0xbbbb]

MUPIP Information: MUINFOUINT4 message is issued by a variety of MUPIP commands to inform the user of the command's progress. This indicates that the string xxxx has the decimal value aaaa and hexadecimal value bbbb.

Action: None necessary.

------------------
MUINFOUINT6
------------------

MUINFOUINT6, tttt : vvvv [0x!hhhh] ; $H=dddddd,tttttt

MUPIP Information: This is a secondary information message that provides additional context for some other MUPIP message; tttt is explanatory text, vvvv is a numeric value, hhhh is the hexadecimal equivalent of vvvv, dddddd and tttttt are date and time in $HOROLOG format.

Action: Refer to the preceding message.

---------------------
MUINFOUINT8
---------------------

MUINFOUINT8, xxxx : aaaa [0xbbbb]

MUPIP Information: MUINFOUINT4 message is issued by a variety of MUPIP commands to inform the user of the command's progress. This indicates that the string xxxx has the decimal 8-byte value aaaa and hexadecimal 8-byte value bbbb.

Action: None necessary.

---------------------
MUINSTFROZEN
---------------------

MUINSTFROZEN, tttt : Instance iiii is frozen. Waiting for instance to be unfrozen before proceeding with writes to database file ffff

Run Time/MUPIP Information: This indicates that the process attempting a write to database file ffff finds the instance iiii frozen (due to either a manual or an anticipatory freeze action). All writes are suspended until the instance is unfrozen.

Action: Examine the cause of the Instance Freeze and take necessary actions to unfreeze the instance.

---------------------
MUINSTUNFROZEN
---------------------

MUINSTUNFROZEN, tttt : Instance iiii is now unfrozen. Continuing with writes to database file ffff

Run Time/MUPIP Information: This indicates that the instance iiii (that was previously frozen) is now unfrozen and the MUPIP operation can continue with writes to database file ffff that were previously suspended. Additionally, tttt provides the time stamp at which the MUPIP operation noticed the unfrozen instance.

Action: None needed.

------------------------
MUJNLPREVGEN
------------------------

MUJNLPREVGEN, Previous generation journal file xxxx included for database file yyyy

MUPIP Information: This indicates that MUPIP included the journal file xxxx for database file yyyy for recovery.

Action: -

------------------------
MUJNLSTAT
------------------------

MUJNLSTAT, xxxx at yyyy

MUPIP Information: This displays the system time yyyy, when the step xxxx was executed.

Action: -

----------------------
MUJPOOLRNDWNFL
----------------------
MUJPOOLRNDWNFL, Jnlpool section (id = xxxx) belonging to the replication instance yyyy rundown failed

MUPIP Error: This indicates that an attempt to run-down the shared memory for a journal pool failed; xxxx is the resource ID of the memory and yyyy is the instance designation.

Action: Analyze the preceding messages for additional information on the failure before attempting the run-down again.

-------------------
MUJPOOLRNDWNSUC
-------------------

MUJPOOLRNDWNSUC, Jnlpool section (id = xxxx) belonging to the replication instance yyyy successfully rundown

MUPIP Information: This indicates that the journal pool for instance yyyy was successfully closed and removed; xxxx is the shared memory resource id for the pool and yyyy is the instance designation.

Action: -

---------------------
MUKEEPNODEC
---------------------

MUKEEPNODEC, Expected decimal integer input for keep

MUPIP Error: The value for the MUPIP REORG -keep qualifier does not have the appropriate syntax.

Action: Revise the argument for -keep to be a decimal integer number of blocks, or a 0-99 percentage followed by a percent sign (%).

---------------------
MUKEEPNOTRUNC
---------------------

MUKEEPNOTRUNC, Keep issued without -truncate

MUPIP Error: The -keep qualifier for MUPIP REORG only applies when used with -truncate.

Action: Adjust the MUPIP REORG command qualifiers to provide a valid combination.

---------------------
MUKEEPPERCENT
---------------------

MUKEEPPERCENT, Keep threshold percentage should be from 0 to 99

MUPIP Error: The MUPIP REORG -KEEP qualifier can accept either a number of blocks or a percentage from 0% to 99%.

Action: If you wish to specify a number of blocks, remove the trailing %; if you wish to use a percentage, ensure it is within range.

---------------------
MUKILLIP
---------------------

MUKILLIP, Kill in progress indicator is set for file xxxx, incorrectly marked busy errors should follow

MUPIP Warning: This indicates that the kill-in-progress flag (shows up as KILLs in progress in DSE DUMP file) is set to a non zero value for database file xxxx.

Action: If there are no accompanying integrity errors, no action is required. Else fix those integrity errors and then perform a MUPIP INTEG -F[AST] -FILE on the database which will then reset the kill-in-progress flag to zero.

--------------------
MULOGNAMEDEF
--------------------

MULOGNAMEDEF, logical name xxxx, needed to start replication server is already defined for this job. Check for an existing or improperly terminated server.

MUPIP Error: This indicates that the logical name xxxx is already defined, which prevents MUPIP from starting the replication server. Either there is an already running server, or a previous server was not properly terminated.

Action: Check for an existing or improperly terminated server, use MUPIP RUNDOWN to clean up.

---------------------
MULTFORMPARM
---------------------

MULTFORMPARM, This formal parameter is multiply defined

Compile Time Error: This indicates that an element appears more than once in a formallist.

Action: Modify the formallist.

------------------
MULTIPROCLATCH
------------------

MULTIPROCLATCH, Failed to get multi-process latch at xxxx

MUPIP Error: A process was unable to acquire a multi-process latch (the resource that ensures correctness of execution amongst multiple processes) in a timely manner; xxxx is the address of the failing request.

Action: Report the entire incident context to your YottaDB support channel.

--------------------
MULTLAB
--------------------

MULTLAB, This label has been previously defined

Compile Time Error: This indicates that a label is defined more than once in the routine.

Action: Rework the labels so that each one is unique. If labels contain more than eight characters, they are truncated to eight characters, which can cause conflicts.

----------------------
MUNOACTION
----------------------

MUNOACTION, MUPIP unable to perform requested action

MUPIP Error: This indicates that MUPIP encountered an error, which prevented the requested action.

Action: Review the accompanying message(s) to identify the cause that prevented MUPIP from performing the requested operation.

--------------------
MUNODBNAME
--------------------

MUNODBNAME, A database name or the region qualifier must be specified

MUPIP Error: This indicates that a MUPIP command did not have a FILE or REGION or JNLFILE qualifier.

Action: Add one of the required qualifiers to the command.

---------------------
MUNODWNGRD
---------------------

MUNODWNGRD, MUPIP downgrade did not occur because of preceding errors

MUPIP Error: This indicates that MUPIP failed to downgrade a database.

Action: Review the accompanying message(s) for additional information.

--------------
MUNOFINISH
--------------

MUNOFINISH, MUPIP unable to finish all requested actions

MUPIP Error: This indicates that MUPIP encountered an error, which prevented the requested action from completing. The action has partially completed.

Action: Review the accompanying message(s) for additional information to identify the cause.

----------------
MUNOSTRMBKUP
----------------

MUNOSTRMBKUP, Database xxxx has a block size larger than yyyy and thus cannot use stream (incremental) backup

MUPIP Warning: YottaDB does not support bytestream (a.k.a incremental) backup of a database file that is created with a GDS block size larger than xxxx. MUPIP CREATE issues MUNOSTRMBKUP warning when creating a database file with a block size that exceeds the limit. MUPIP BACKUP -BYTESTREAM issues MUNOSTRMBKUP error and skips backing up a file that has block size that exceeds the limit. NOTE: Comprehensive BACKUP does not impose any limit on the GDS block size of the database file being backed up.

Action: Create the database file with a block size that does not exceed the limit.

----------------
MUNOTALLINTEG
----------------

MUNOTALLINTEG, At least one region skipped. See the earlier messages

MUPIP Warning: The INTEG report is incomplete because MUPIP could not access all of the selected regions.

Action: If appropriate, correct the issue(s) that caused INTEG to skip one or more regions

-----------------
MUNOTALLSEC
-----------------

MUNOTALLSEC, WARNING: not all global sections accessed were successfully rundown

MUPIP Warning: This indicates that RUNDOWN encountered at least one database that appeared to be in use and therefore could not be processed.

Action: If appropriate, initiate actions to cause all YottaDB users to exit from YottaDB and repeat the MUPIP RUNDOWN.

-----------
MUNOUPGRD
-----------

MUNOUPGRD, MUPIP upgrade did not occur because of preceding errors

MUPIP Error: This indicates that MUPIP could not upgrade a database from one version to another.

Action: Review the preceding error messages. Review all the release notes for the new version and for all versions between it and the existing version.

--------------------
MUPCLIERR
--------------------

MUPCLIERR, Action not taken due to CLI errors

MUPIP Error: This indicates that a MUPIP command did not process because of invalid syntax.

Action: Review the command documentation for correct syntax. Review the accompanying message(s), if any, for more information about the cause of this error.

--------------------
MUPGRDSUCC
--------------------

MUPGRDSUCC, Database file xxx successfully yyy to zzz

MUPIP Information: The database file header for xxx has been upgraded or downgraded to the version zzz format.

Action: -

-----------------
MUPIPINFO
-----------------

MUPIPINFO, xxxx

MUPIP Information: The LOAD command with the FORMAT qualifier defined to GO or GOQ uses this message to display the input file label xxxx.

Action: -

------------------
MUPIPSET2BIG
------------------

MUPIPSET2BIG, vvvv too large, maximum tttt allowed is mmmm

MUPIP Error: The value vvvv for tttt specified in a MUPIP SET command is above the maximum mmmm for tttt

Action: Decrease the specified value to not exceed the maximum.

----------------
MUPIPSET2SML
----------------

MUPIPSET2SML, vvvv too small, minimum tttt allowed is mmmm

MUPIP Error: The value vvvv for tttt specified in a MUPIP SET command is below the minimum mmmm for tttt

Action: Increase the specified value to meet or exceed the minimum.

-----------------
MUPIPSIG
-----------------

MUPIPSIG, STOP (signal xxxx) issued from process yyyy to process zzzz

MUPIP Information: This message indicates that process yyyy issued a MUPIP STOP to process zzzz, and xxxx signal is sent to process zzzz. Note that the message is logged at the time the signal is sent, regardless of when or if it is processed by process zzzz.

Action: -

-------------------
MUPJNLINTERRUPT
-------------------

MUPJNLINTERRUPT, Database file xxxx indicates interrupted MUPIP JOURNAL command. Restore from backup for forward recover/rollback.

MUPIP Error: This indicates that a MUPIP JOURNAL -ROLLBACK -FORWARD or a MUPIP JOURNAL -RECOVER -FORWARD did not proceed because a previous MUPIP JOURNAL command attempted on the database was terminated abnormally.

Action: Restore the database and journal files from a backup to proceed with the MUPIP JOURNAL -ROLLBACK -FORWARD or MUPIP JOURNAL -RECOVER -FORWARD.

------------------
MUPRECFLLCK
------------------

MUPRECFLLCK, Database file xxxx is locked by MUPIP RECOVER. Could not secure access.

Run Time Error: This indicates that YottaDB could not open a database file xxxx because MUPIP JOURNAL with the RECOVER qualifier was applying a journal to the file.

Action: Wait for the RECOVER command to complete.

-----------------
MUPRESTERR
-----------------

MUPRESTERR, MUPIP RESTORE aborted due to preceding errors

MUPIP Error: This indicates that a RESTORE operation failed, which left the database in an indeterminate state.

Action: Review the preceding errors for additional information.

------------------
MUQUALINCOMP
------------------

MUQUALINCOMP, Incompatible qualifiers - FILE and REGION

MUPIP Error: This indicates that the FILE and REGION qualifiers cannot be used in the same command.

Action: Choose one or the other.

-----------------
MURAIMGFAIL
-----------------

MURAIMGFAIL, MUPIP RECOVER failed while processing after-image journal record. Failure code: xxxx.

MUPIP Error: This indicates that MUPIP RECOVER/ROLLBACK encountered an error when processing an after-image journal record written for each DSE database update. xxxx contains the failure codes for the four attempts. It is very likely that the database may have integrity errors or that the process-private data structures are corrupted.

Action: Attempt the MUPIP RECOVER/ROLLBACK again. If the error persists, report the entire incident context with as much information about the system as possible to your YottaDB support channel.

------------------
MUREENCRYPTEND
------------------

MUREENCRYPTEND, Database ffff : MUPIP REORG ENCRYPT finished by pid pppp at transaction number 0xtttt

MUPIP Information: The MUPIP REORG -ENCRYPT initiated by process pppp completed an encyption change for database file ffff at transaction number 0xtttt

Action: None required.

------------------
MUREENCRYPTSTART
------------------

MUREENCRYPTSTART, Database ffff : MUPIP REORG ENCRYPT started by pid pppp at transaction number 0xtttt

MUPIP Information: Process pppp used MUPIP REORG -ENCRYPT to start or restart an encyption change at a transaction number 0xtttt for database file ffff

Action: None required.

--------------------
MUREENCRYPTV4NOALLOW
--------------------

MUREENCRYPTV4NOALLOW, Database (re)encryption supported only on fully upgraded V5 databases. ffff has V4 format blocks

MUPIP Error: MUPIP cannot enable or perform encryption on database file ffff while it contains GDS V4 format blocks.

Action: Upgrade the database to V5 and re-run the action.

--------------------
MUREORGFAIL
--------------------

MUREORGFAIL, MUPIP REORG failed. Failure code: xxxx.

MUPIP Error: This indicates that a REORG encountered a database error with failure code xxxx.

Action: Report this error to the group responsible for database integrity within your organization.

-------------------
MUREPLPOOL
-------------------

MUREPLPOOL, Error with replpool section xxxx

MUPIP Error: This indicates that the MUPIP RUNDOWN command found the specified replication pool shared memory section that had a problem.

Action: Refer to the subsequent message text for details.

---------------------
MUREPLSECDEL
---------------------

MUREPLSECDEL, Replication section xxxx deleted

MUPIP Information: This indicates that a replication pool was successfully closed and removed; xxxx is the shared memory resource ID for the pool.

Action: -

-----------------------
MUREPLSECNOTDEL
-----------------------

MUREPLSECNOTDEL, Replication section xxxx not deleted

MUPIP Error: This indicates that an attempt to rundown the shared memory for a replication pool failed; xxxx is the resource ID of the memory.

Action: Review the preceding messages for additional information on the failure before attempting a rundown again.

-------------------
MUREUPDWNGRDEND
-------------------

MUREUPDWNGRDEND, Region xxxx : MUPIP REORG UPGRADE/DOWNGRADE finished by pid aaaa [0xbbbb] at transaction number [0xcccc]

MUPIP Information: This is an informational message printed by MUPIP REORG UPGRADE or DOWNGRADE when the reorg has successfully completed its upgrade or downgrade respectively.

Action: None necessary.

------------------
MURNDWNARGLESS
------------------

MURNDWNARGLESS, Argumentless MUPIP RUNDOWN started with process id PPPP by userid UUUU from directory DDDD

MUPIP Information: Operator log message indicating an argumentless MUPIP RUNDOWN, which uses IPC resources on the node to clean up inactive YottaDB shared resources (memory and semaphores).

Action: None typically required; may be useful in diagnosing operational issues.

------------------
MURNDWNOVRD
------------------

MURNDWNOVRD, OVERRIDE qualifier used with MUPIP RUNDOWN on database file dddd

MUPIP Information: This message records use of the OVERRIDE qualifier with a MUPIP RUNDOWN command to bypass an error, which would normally suggest a more appropriate action.

Action: No action required. This message serves primarily to facilitate analysis of database crashes and recovery procedures.

-----------------
MURPOOLRNDWNFL
-----------------

MURPOOLRNDWNFL, Recvpool section (id = xxxx) belonging to the replication instance yyyy rundown failed

MUPIP Error: This indicates that an attempt to rundown the shared memory for a receive pool failed; xxxx is the resource ID of the memory and yyyy is the instance designation.

Action: Review the preceding messages for additional information on the failure before attempting a rundown again.

-----------------
MURPOOLRNDWNSUC
-----------------

MURPOOLRNDWNSUC, Recvpool section (id = xxxx) belonging to the replication instance yyyy successfully rundown

MUPIP Information: This indicates that the receive pool for the specified instance was successfully closed and removed; xxxx is the shared memory resource ID for the pool and yyyy is the instance designation.

Action: -

-------------------
MUSECDEL
-------------------

MUSECDEL, Section xxxx deleted

MUPIP Information: This indicates that RUNDOWN removed the global memory section xxxx that is associated with an inactive database.

Action: -

---------------------
MUSECNOTDEL
---------------------

MUSECNOTDEL, Section xxxx not deleted

MUPIP Information: This indicates that RUNDOWN could not eliminate the global memory section xxxx that is associated with an apparently inactive database.

Action: -

---------------------
MUSELFBKUP
---------------------

MUSELFBKUP, Database file xxxx can not be backed upon itself

MUPIP Error: This indicates that YottaDB attempted to perform a backup that would have overlaid the database being backed up.

Action: Modify the name of the output file and reissue the command.

--------------------
MUSIZEFAIL
--------------------

MUSIZEFAIL, MUPIP SIZE : failed. Failure code: xxxx.

MUPIP Error: This error indicates that MUPIP SIZE command encountered a database error with failure code xxxx.

Action: Report this error to the group responsible for database integrity within your organization.

--------------------
MUSIZEINVARG
--------------------

MUSIZEINVARG, MUPIP SIZE : Invalid parameter value for: xxxx

MUPIP Error: This indicates that MUPIP SIZE encountered a qualifier or parameter xxxx with an invalid value

Action: Review the proper syntax for MUPIP SIZE. Refer to the `Administration and Operations Guide <../AdminOpsGuide/index.html>`_ or the online help for the MUPIP SIZE command.

---------------------
MUSTANDALONE
---------------------

MUSTANDALONE, Could not get exclusive access to xxxx

MUPIP Information: This indicates that the process required but could not get exclusive access to the listed resource.

Action: Retry the process at a time when there are no other users accessing the resource or log the users off the resource.

---------------------
MUTEXERR
---------------------

MUTEXERR, Mutual Exclusion subsystem failure

Run Time Error: This indicates that YottaDB encountered a system error while intializing mutual exclusion resource(s).

Action: Review the accompanying message(s) for more information about the cause of the error.

--------------------
MUTEXFRCDTERM
--------------------

MUTEXFRCDTERM, Mutual Exclusion subsystem detected forced termination of process xxxx. Crit salvaged from database file dddd.

Run Time Warning: This indicates that YottaDB confirmed an inappropriate termination of the process xxxx, while holding crit on database file dddd.

Action: Determine the cause of the termination and take appropriate action.

---------------------
MUTEXLCKALERT
---------------------

MUTEXLCKALERT, Mutual Exclusion subsystem ALERT - Lock attempt threshold crossed for region rrrr. Process pppp is in crit cycle cccc.

Run Time Error: This warning indicates that a process could not obtain a critical section lock for region rrrr even after waiting longer than the YottaDB determined threshold (approximately 45 seconds), because the critical section lock was held by another process pppp that entire time. cccc is the crit cycle count which YottaDB increases by one every time it successfully grants the mutual exclusion (mutex) lock to a process. cccc provides a measure of the frequency of mutex lock usage. MUTEXLCKALERT messages indicate that process pppp is blocking access to region rrrr for inappropriately long periods of time and thereby impacting performance for other processes that need access to that region.

YottaDB produces this warning when:

- A process owning a critical section dies (most likely because of a kill -9) and the OS gives its PID to another process. To reclaim the inappropriately held critical section, YottaDB first checks whether the process is alive and whether it holds the critical section. On finding that the process is alive but does not hold the critical section, YottaDB concludes that it is not safe to free the critical section and alerts the operator with this message.

- The process holding the critical section is using a non-Isolated command such as ZSYSTEM, BREAK or a timed command in a way that creates a deadlock or a live-lock. YottaDB attempts to limit this by limiting the time a process using one of these commands can hold a critical section, but the use of non-Isolated commands and the settings for $ZMAXTPTIM and/or the environment variable $ydb_tpnotacidtime may be such that MUTEXLCKALERT messages are generated. Revise your settings for $ydb_tpnotacidtime and $ZMAXTPTIM appropriately.

- There is an IO bottleneck that caused YottaDB to slow down: YottaDB detects that process pppp is currently using the critical section lock.

Action: Monitor the system to determine whether there is a process with process id pppp and whether that process is a YottaDB process.

Implement a script to get a stack trace for process pppp or take other appropriate actions, and use the $ydb_procstuckexec environment variable to activate it before the block process sends the MUTEXLCKALERT message.

Identify and terminate process pppp to release control of that resource. If the process is a YottaDB process, use a MUPIP STOP to terminate it. If the process is for another application, use an appropriate mechanism to stop it.

If this message is due to an IO bottleneck, adopt a strategy that reduces IO. Some of the IO reducing strategies are:

- Revisit your database configuration parameters (especially block size, number of global buffers, journal buffers, and so on) to see if you can make improvements.

- Create separate regions (database) for temporary globals and do not replicate them.

- Consider whether a different database access method and journaling strategy could improve throughput while satisfying your operational needs.

- For application configurations with a large number of concurrent processes and/or large process memory footprints, consider placing object code in shared libraries on YottaDB editions that support it. This may free system memory which the OS can use for its file system cache, or which can be used to increase the number of global buffers.

.. note::
    Do not apply IO reduction strategies all at once. Try them one at a time and always verify/measure the results of each strategy.

----------------------------
MUTEXRELEASED
----------------------------

MUTEXRELEASED, Process xxxx [aaaa] has released the critical section for database yyyy to avoid deadlock. $TLEVEL: pppp t_tries: qqqq

Run Time Information: This indicates an out-of-design state within YottaDB that was recoverable.

Action: If this message is frequent, report the entire incident context to your YottaDB support channel.

----------------------
MUTEXRSRCCLNUP
----------------------

MUTEXRSRCCLNUP, Mutex subsystem leftover resource xxxx removed.

Run Time Information: This indicates that YottaDB removed the leftover system resource xxxx, used by the mutual exclusion subsystem. The resource was leftover due to abnormal termination of a YottaDB component.

Action: -

----------------------
MUTNWARN
----------------------

MUTNWARN, Database file xxxx has 0xaaa more transactions to go before reaching the transaction number limit (0xbbbb). Renew database with MUPIP INTEG TN_RESET.

MUPIP Warning: This indicates that MUPIP INTEG detected that the transaction numbers in the named database are approaching the maximum number as specified by the Maximum TN Warn field in the database file header. The actual maximum TN is less than this theoretical limit. DSE DUMP FILEHEADER shows what the limit is. The actual limit reflects some overhead used, for example, during a TN_RESET operation.

Action: Use MUPIP INTEG with the qualifier TN_RESET to reset the transaction numbers in the database. If the database is in an older format, consider converting it to the most recent format.

---------------------
MUTRUNC1ATIME
---------------------

MUTRUNC1ATIME, Process with PID iiii already performing truncate in region rrrr

MUPIP Information: Issued when a REORG -TRUNCATE on a region rrrr detects some other active REORG process concurrently processing a truncation.

Action: No action required. The other process will complete the truncate.

--------------------
MUTRUNCBACKINPROG
--------------------

MUTRUNCBACKINPROG, Truncate detected concurrent backup in progress for region rrrr

MUPIP Information: REORG truncate process detected concurrent backup. Database file not truncated.

Action: Ensure that the backup has completed and rerun MUPIP REORG -TRUNCATE command.

-------------------
MUTRUNCERROR
-------------------

MUTRUNCERROR, Truncate of region rrrr encountered service error eeee

MUPIP Error: This indicates that a system call failed during REORG truncate.

Action: Use the OS documentation to investigate the failure.

------------------
MUTRUNCFAIL
------------------

MUTRUNCFAIL, Truncate failed after reorg

MUPIP Error: This indicates that REORG encountered an unexpected error. Truncate may be partially complete.

Action: Review accompanying message(s) for more information.

--------------------
MUTRUNCNOSPACE
--------------------

MUTRUNCNOSPACE, Region rrrr has insufficient space to meet truncate target percentage of yyyy

MUPIP Information: Issued when REORG truncate determines that there is not enough free space at the end of the file; database file not truncated.

Action: If appropriate, specify a larger threshold.

-------------------
MUTRUNCNOSPKEEP
-------------------

MUTRUNCNOSPKEEP, Region rrrr has insufficient space to meet truncate target percentage of pppp with keep at bbbb blocks

MUPIP Information: MUPIP REORG -KEEP for region rrrr could not meet the specified percentage pppp so it left all the available blocks bbbb.

Action: None required, other than evaluating the space situation for the region and file system to ensure that it is wholesome and does not require additional intervention.

-------------------
MUTRUNCNOTBG
-------------------

MUTRUNCNOTBG, Region rrrr does not have access method BG

MUPIP Error: The truncate feature is only supported with the BG access method.

Action: Use the BG access method for files you wish to truncate.

-------------------
MUTRUNCNOV4
-------------------

MUTRUNCNOV4, Region rrrr is not fully upgraded from V4 format.

MUPIP Error: The truncate feature is only available for fully upgraded database files.

Action: In order to use truncate, first upgrade the database file to the current major version.

--------------------
MUTRUNCPERCENT
--------------------

MUTRUNCPERCENT, Truncate threshold percentage should be from 0 to 99

MUPIP Error: This indicates that the value entered for MUPIP REORG -TRUNCATE is invalid.

Action: Specify a valid threshold percentage.

----------------------
MUTRUNCSSINPROG
----------------------

MUTRUNCSSINPROG, Truncate detected concurrent snapshot in progress for region rrrr

MUPIP Information: REORG truncate process detected concurrent snapshot; database file not truncated.

Action: Ensure that the snapshot, for example, a MUPIP INTEG, has completed and rerun the MUPIP REORG -TRUNCATE command.

--------------------
MUTRUNCSUCCESS
--------------------

MUTRUNCSUCCESS, Database file dddd truncated from oooo blocks to nnnn at transaction tttt

MUPIP Information: This operator log message indicates that the specified database file was truncated by MUPIP REORG as described by the message.

Action: -

--------------------
MUUPGRDNRDY
--------------------

MUUPGRDNRDY, Database xxx has not been certified as being ready to upgrade to yyy format

MUPIP Error: The named database file is in an older format than is in use by this YottaDB version and has not been certified as ready for use by this YottaDB version.

Action: Run DBCERTIFY to certify the database as being ready for upgrade.

-----------------------
MUUSERECOV
-----------------------

MUUSERECOV, Abnormal shutdown of journaled database dddd detected

Run Time Error: This error is issued when attempting a MUPIP RUNDOWN on a previously crashed journaling-enabled database dddd.

Action: Use MUPIP RECOVER to restore the normal state of the database.

----------------------
MUUSERLBK
----------------------

MUUSERLBK, Abnormal shutdown of replication-enabled database dddd detected

Run Time Error: This error is issued when attempting a MUPIP RUNDOWN on a previously crashed replication-enabled (with BEFORE IMAGE journaling) database dddd.

Action: Use MUPIP ROLLBACK to restore the normal state of the database.

---------------------
NAMECOUNT2HI
---------------------

NAMECOUNT2HI, Number of varnames specified as the namecount parameter in xxxx (vvvv) exceeds the maximum (mmmm).

Runtime Error: This indicates that the number of variable names vvvv, specified in the SimpleAPI call identified xxxx, exceeds the maximum number of allowed variable names mmmm.

Action: Retry the SimpleAPI call with fewer variable names specified.

---------------------
NAMEEXPECTED
---------------------

NAMEEXPECTED, A local variable name is expected in this context

Compile Time Error: This indicates that an actualname or a formallist item did not specify a local variable name.

Action: Look for and correct typographical errors. Verify that actualnames and formallist items are local variable names.

--------------------
NAMENDBAD
--------------------

NAMENDBAD, Subscripted name ssss must end with right parenthesis

GDE Error: This indicates that a subscripted name ssss (global name immediately followed by a left parenthesis) was specified without a balancing right parenthesis at the end of the subscripts.

Action: Specify the subscripted name with the appropriate right parenthesis.

--------------------
NAMGVSUBOFLOW
--------------------

NAMGVSUBOFLOW, Subscripted name hhhh...tttt is too long to be represented in the database using collation value #nnnn

GDE Error: This indicates that the subscripted name is too big to be represented in the database (exceeds the maximum limits of YottaDB for the key size). The message also reports the alternative collation nnnn which was used to arrive at the subscript/key representation inside the database. The head (hhhh) and tail (tttt) of the long subscript is displayed with a ... in the middle.

Action: Specify a shorter subscripted name.

---------------------
NAMGVSUBSMAX
---------------------

NAMGVSUBSMAX, Subscripted Name specification nnnn has more than the maximum # of subscripts (mmmm)

GDE Error: This indicates that a name nnnn was specified with more than the maximum allowed number of subscripts mmmm.

Action: Specify a name within the maximum allowed number of subscripts.

---------------------
NAMLPARENNOTBEG
---------------------

NAMLPARENNOTBEG, Subscripted Name specification nnnn needs to have a left parenthesis at the beginning of subscripts

GDE Error: This indicates that a name was specified using ":" or "," or ")", which indicates a subscripted name, but the left parenthesis was missing.

Action: Specify a name with the appropriate left parenthesis.

----------------------
NAMNOTSTRSUBS
----------------------

NAMNOTSTRSUBS, Subscript #nnnn with value vvvv in name specification is not a properly formatted string subscript

GDE Error: This indicates that the nnnn'th subscript in a name did not have a valid string subscript. For example, usages like GBL("AB"_) - where the "_" syntax is used to concatenate string subscripts but the right side of the "_" operator is missing the string specification.

Action: Specify the name with a properly formatted string subscript.

---------------------
NAMNUMSUBSOFLOW
---------------------

NAMNUMSUBSOFLOW, Subscript #nnnn with value vvvv in name specification has a numeric overflow

GDE Error: This indicates that the nnnn'th subscript in a name specification includes a number that is too big to be represented in YottaDB.

Action: Specify a subscript with a number that is inside the numeric range supported by YottaDB.

---------------------
NAMONECOLON
---------------------

NAMONECOLON, Subscripted Name specification nnnn must have at most one colon (range) specification

GDE Error: This indicates that a subscripted name was specified with a range specification using ":" in the last subscript, but more than one colon character was used.

Action: Specify a range with only one colon.

--------------------
NAMRANGELASTSUB
--------------------

NAMRANGELASTSUB, Ranges in name specification nnnn are allowed only in the last subscript

GDE Error: This indicates that one or more ranges (using ":" syntax) were specified in a name somewhere other than in the last subscript.

Action: Specify ranges only in the last subscript of a name.

-------------------
NAMRANGEORDER
-------------------

NAMRANGEORDER, Range in name specification nnnn specifies out-of-order subscripts using collation sequence #cccc

GDE Error: This indicates that the range in the name specification is out-of-order. For example yy(10:1) specifies numeric subscripts that are not in order (10 is greater than 1 and so it should have been 1:10 instead). With string subscripts, the collation sequence cccc is used to arrive at the subscript representation in the database and this is what gets compared to the left and right ends of the range to determine if they are in order or not. For example yy("a":"g") is in order for the collation sequence 0 (ascii ordering), but might not be in order if the name yy has a non-zero collation defined and if that collation sorts strings in reverse ascii order.

Action: Specify ranges in order i.e. lower end of the range on the left hand side and the higher end of the range on the right hand side.

-----------------
NAMRANGEOVERLAP
-----------------

NAMRANGEOVERLAP, Range in name specifications mmmm and nnnn overlap using collation sequence #cccc

GDE Error: This indicates that the subscripted name specifications mmmm and nnnn belong to the same unsubscripted global name and map to different regions, but define ranges that overlap.

Action: Ranges that overlap cannot map to different regions. The only exception is sub-ranges where one range lies completely inside of another. Fix the range specifications to either map to the same region or split the ranges further to avoid overlap.

-------------------
NAMRPARENNOTEND
-------------------

NAMRPARENNOTEND, Subscripted Name specification nnnn cannot have anything following the right parenthesis at the end of subscripts

GDE Error: This indicates that a subscripted name was specified where the right parenthesis denoting the end of the subscripts was followed by more characters.

Action: Specify all subscripts of a name inside the left and right parenthesis that immediately follow the unsubscripted name.

--------------------
NAMSTARSUBSMIX
--------------------

NAMSTARSUBSMIX, Name specification nnnn cannot contain * and subscripts at the same time

GDE Error: This indicates that the name nnnn contains both * and subscripts which is not allowed.

Action: Specify either a wildcard (*) or subscripts, but not both.

----------------------
NAMSTRSUBSCHARG
----------------------

NAMSTRSUBSCHARG, Subscript #nnnn with value vvvv in name specification specifies a $C/$ZCH with number cccc that is invalid in the current $zchset

GDE Error: This indicates that the nnnn'th subscript in a name specifies a string subscript using $CHAR/$ZCHAR, but one of the arguments to this function (potentially in a comma-separated list) is invalid.

Action: An invalid integer argument returns a null string when passed to $CHAR/$ZCHAR with the current $zchset setting. Specify the string subscript with a valid argument to $CHAR/$ZCHAR.

--------------------
NAMSTRSUBSCHINT
--------------------

NAMSTRSUBSCHINT, Subscript #nnnn with value vvvv in name specification does not have a positive integer inside $C/$CHAR/$ZCH/$ZCHAR

GDE Error: This indicates that the nnnn'th subscript in a name specifies a string subscript using $CHAR/$ZCHAR, but one of the arguments to this function (potentially in a comma-separated list) is not a positive integer.

Action: Specify the string subscript with a positive integer argument to $CHAR/$ZCHAR.

---------------------
NAMSTRSUBSFUN
---------------------

NAMSTRSUBSFUN, Subscript #nnnn with value vvvv in name specification uses function other than $C/$CHAR/$ZCH/$ZCHAR

GDE Error: This indicates that the nnnn'th subscript in a name specifies a string subscript using an unsupported function. The only two supported functions are $CHAR or $ZCHAR (long form and short forms).

Action: Specify the string subscript using only the supported functions.

--------------------
NAMSUBSBAD
--------------------

NAMSUBSBAD, Subscript #nnnn with value vvvv in name specification is an invalid number or string

GDE Error: This indicates that the nnnn'th subscript in a name specification is neither a valid number or a string.

Action: Specify a valid subscript.

--------------------
NAMSUBSEMPTY
--------------------

NAMSUBSEMPTY, Subscript #nnnn is empty in name specification

GDE Error: This indicates that the nnnn'th subscript in a name specification is empty. For example the 2nd subscript in a(1,,3) is empty.

Action: Specify the subscripted name with a non-empty subscript.

-----------------
NCTCOLLDIFF
-----------------

NCTCOLLDIFF, Source and destination for MERGE cannot have different numerical collation type

Run Time Error: This indicates that two arguments of the MERGE command have different numerical collation types.

Action: Use the %GBLDEF utility to set the same numerical collation type for both arguments or use another method, such as a $ORDER() loop or MUPIP EXTRACT and LOAD to move the data.

--------------------
NCTCOLLSPGBL
--------------------

NCTCOLLSPGBL, Database region rrrr contains portion of spanning global ^gggg and so cannot support non-zero numeric collation type

Run Time Error: This indicates that region rrrr contains parts of a global gggg that spans multiple regions according to the current global directory, but the directory tree in rrrr indicates that gggg has a non-zero numeric collation type.

Action: Spanning globals only support a value of zero for numeric collation (i.e. numbers collate as numbers, not as strings). Access region rrrr using a temporary global directory that maps all names (including gggg) to rrrr, extract the gggg global, KILL it, use $$set^%^GBLDEF to fix the numeric collation to 0, reload the global from the extract, and switch back to the regular global directory. An alternative recovery action that does not require extract/load is to change the global directory, so that gggg is no longer a spanning global or has no mappings into any region that collates numbers using their string value.

----------------------
NEEDTRIGUPGRD
----------------------

NEEDTRIGUPGRD, Cannot do trigger operation on database file ffff until it is upgraded; Run MUPIP TRIGGER -UPGRADE first

Run Time Error: Upgrades from some versions to some more recent versions change aspects of trigger definitions. This message indicates that YottaDB encountered a trigger definition in an old format.

Action: Run MUPIP TRIGGER -UPGRADE. Alternatively, revert to the older version, use MUPIP TRIGGER or $ZTRIGGER() to select all triggers and save the result, delete all triggers, then return to the newer version and reinstall the triggers.

----------------------
NEGFRACPWR
----------------------

NEGFRACPWR, Invalid operation: fractional power of negative number

Run Time Error: This indicates that the power of an exponentiation operation is negative and contains a fractional portion. This type of operation produces an imaginary component in its result, and M does not specify such operations.

Action: Modify the code to prevent negative powers with fractional parts in exponentiation operations, or trap the resulting errors.

---------------------
NESTFORMP
---------------------

NESTFORMP, Formal parameter list cannot be combined with nested line

Compile Time Error: This indicates that a line included both a formal list and a nesting level-indicator (.).

Action: Parameter passing is incompatible with argumentless DO commands. Remove the formal list or the level-indicator and reorganize the routine, if appropriate.

-----------------------
NETDBOPNERR
-----------------------

NETDBOPNERR, Error while attempting to open database across net

Run Time Error: This indicates that YottaDB encountered an error when it attempted to open a database that is on a remote node served by a GT.CM server.

Action: Review subsequent message(s) to determine the nature of the problem.

----------------------
NETFAIL
----------------------

NETFAIL, Failure of Net operation

Run Time Error: This indicates that a network failure occurred and that it could not be traced to any current activity.

Action: If the problem persists, contact the group responsible for database operations on your network.

-----------------
NETLCKFAIL
-----------------

NETLCKFAIL, Lock operation across Net failed

Run Time Error: This indicates that a LOCK, ZALLOCATE, or ZDEALLOCATE that involved a remote database failed.

Action: This network failure involves M LOCKs. Retry the operation from a point that establishes all necessary LOCKs. If the problem persists, contact the group responsible for database operations on your network.

--------------------
NEWJNLFILECREAT
--------------------

NEWJNLFILECREAT, Journal file xxxx nearing maximum size. New journal file created.

Run Time/MUPIP Information: This indicates that YottaDB created a new journal file as it reached the maximum allowed journal size.

Action: Refer to the documentation on the maximum allowed journal filesize.

------------------
NLMISMATCHCALC
------------------

NLMISMATCHCALC, Location of xxxx expected at yyyy, but found at zzzz

Run Time Error: This indicates that the shared memory location of xxxx shows a layout problem. Typically, this is caused by attempting to use databases opened by a YottaDB version, different from the currently running version.

Action: Rundown the database and ensure a stable and consistent database configuration before attempting to use it again.

-------------------
NLRESTORE
-------------------

NLRESTORE, DB file header field FFFF: VVVV does not match the value used in original mapping - restoring to: OOOO

DSE Warning: When DSE encounters a internal header field named FFFF whose value VVVV conflicts with the original value OOOO, DSE issues a warning message and uses the original value in order to successfully access shared memory.

Action: Please restore the header fields to their correct values. As a low level database repair tool of last resort, DSE assumes a knowledgeable user and does no edit checking of input values. Do not use DSE to make routine changes and do not use DSE to change a parameter if you can accomplish the same goal with MUPIP. As the normal system administration and operations tool, MUPIP has the ability to change parameters you might normally need to change, and it does check that input values are reasonable. Changing fileheader parameters with DSE should normally be performed with stand-alone access. Change fileheader parameters on an open database only under the guidance of an expert YottaDB support channel.

-------------------
NOACTION
-------------------

NOACTION, Not updating Global Directory xxxx

GDE Information: This indicates that GDE did not write a new version of the existing Global Directory xxxx due to a QUIT or an EXIT when no changes had been made.

Action: GDE displays this message when you EXIT GDE without making any changes to the Global Directory. It also displays this message when you terminate a GDE session with the QUIT command. If you made changes you want to save - restart GDE, perform your work and save the changes before exiting GDE.

---------------------
NOALIASLIST
---------------------

NOALIASLIST, Parenthetical lists of multiple arguments cannot have a preceding alias introducer or include alias (*) forms

Run Time Error: This indicates that the argument for a SET command attempted to assign an alias using a parenthesized list as a left-hand argument, which is unsupported syntax.

Action: Correct the code in question to avoid the parenthesized list.

-----------------------
NOCANONICNAME
-----------------------

NOCANONICNAME, Value is not a canonic name (xxxx).

Run Time Error: This indicates that the argument supplied to $QLENGTH or the first argument to $QSUBSCRIPT is not a valid glvn.

Action: Pass valid argument to $QLENGTH/$QSUBSCRIPT

--------------
NOCCPPID
--------------

NOCCPPID, Cannot find CCP process ID

CCE Error: This indicates that a CCE DUMP did not complete because it could not find a process with the name for the CCP.

Action: The CCP is not running properly on your node. Report this error to the group responsible for clustered databases at your site.

-----------------
NOCHLEFT
-----------------

NOCHLEFT, Unhandled condition exception (all handlers exhausted) process terminating

Run Time Fatal: This indicates an internal error in the handling of error conditions in YottaDB.

Action: Report the entire incident context with the complete operator log generated to your YottaDB support channel.

--------------------
NOCREMMBIJ
--------------------

NOCREMMBIJ, MM access method not compatible with BEFORE image journaling; Database file DDDD not created

MUPIP/Run Time Error: While creating a database file with access mode MM, YottaDB found the database file DDDD to be configured for BEFORE-image journaling, which is not supported with that access method.

Action: Either change the access mode of the file or do not enable BEFORE image journaling and retry.

-----------------------
NOCRENETFILE
-----------------------

NOCRENETFILE, Database file DDDD not created; cannot create across network

All YottaDB Components Error: While creating a database with the AUTODB flag on, YottaDB discovered that the database is configured to be a remote GT.CM database. This configuration is not supported.

Action: Either change the file to be local or remove the AUTODB flag from the file description in the global directory and retry.

--------------------
NODEEND
--------------------

NODEEND, End of list of nodes/subscripts

Runtime Error: This 'error' is an indicator (like EOF) that the list of nodes or subscripts being fetched via ydb_node_next/ydb_node_previous or ydb_subscript_next/ydb_subscript_previous or their equivalent Golang wrappers, is at an end.

Action: Not an error strictly speaking - Terminate the loop the list is being read in as it is complete.

------------------------
NODFRALLOCSUPP
------------------------

NODFRALLOCSUPP, The NODEFER_ALLOCATE qualifier is not allowed on this operating system. Not changing the defer allocation flag

MUPIP Error: Indicates that disk space preallocation is not supported on the current operating system.

Action: Consider using an external utility, such as FALLOCATE, to fulfill the need.

---------------------
NOEDITOR
---------------------

NOEDITOR, Can't find an executable editor: eeee

Run Time Error: The ZEDIT command cannot find an executable editor.

Action: Ensure that the EDITOR environment variable points to an editor that is executable by the user.

--------------------
NOENDIANCVT
--------------------

NOENDIANCVT, Unable to convert the endian format of file dddd due to eeee

MUPIP Error: One of the requirements for the MUPIP ENDIANCVT command was not met. The problems include: "database format is not the current version", "minor database format is not the current version", "some blocks are not upgraded to the current version", "kills in progress", "the database is frozen", "a GT.CM server accessing the database", "recovery was interrupted", "database creation in progress", "wc_blocked is set- rundown needed", "the database is corrupted".

Action: Resolve the reported conditions and repeat the command or use the -OVERRIDE qualifier if it is appropriate to bypass the error condition.

----------------------
NOEXCLUDE
----------------------

NOEXCLUDE, None of the excluded variables exist

MUPIP Information: This indicates that MUPIP REORG did not find any of the variables specified in the EXCLUDE qualifier to be present in the database.

Action: Verify the names specified in the EXCLUDE qualifier, in case you expected them to be present in the database file and not be reorged.

--------------------
NOEXCNOZTRAP
--------------------

NOEXCNOZTRAP, Neither an exception nor a Ztrap is specified

Run Time Warning: This indicates that a $CTRAP character arrived, but no EXCEPTION or $ZTRAP existed to handle it.

Action: Determine why these circumstances coincide. This error never appears on a device; in this particular case, it is assigned to the image termination status as a warning.

-------------------
NOEXIT
-------------------

NOEXIT, Cannot exit because of verification failure

GDE Information: This indicates that GDE encountered errors in the REGION-SEGMENT or SEGMENT-FILE mappings and cannot exit.

Action: Review the accompanying message(s) for additional information. Verify the mappings and modify them as appropriate.

-------------------
NOFILTERNEST
-------------------

NOFILTERNEST, Filter nesting not allowed

Run Time Error: Filter code must not invoke other code that requires a filter.

Action: Revise the filter code to adhere to the requirement.

---------------------
NOFORKCORE
---------------------

NOFORKCORE, Unable to fork off process to create core. Core creation postponed.

Run Time Warning: This indicates that the process that failed was unable to create a memory dump file, possibly due to a lack of system resources.

Action: Reduce the number of users and stop any unnecessary processes.

----------------------
NOGTCMDB
----------------------

NOGTCMDB, ffff does not support operation on GT.CM database region: rrrr

Utility Error: Facility ffff cannot perform the requested operation on a GT.CM database such as rrrr.

Action: Use the utility on the remote GT.CM server system or move the database so that it is local rather than remote.

--------------------
NOJNL
--------------------

NOJNL, ssss segments do not support journaling.

MUPIP Error: This error indicates that the segment type ssss does not support journaling.

Action: For more information on Journaling, refer to the `Journaling chapter in the Administration and Operations Guide <../AdminOpsGuide/ydbjournal.html>`_.

--------------------
NOJNLPOOL
--------------------

NOJNLPOOL, No journal pool info found in the replication instance of xxxx

Run Time/MUPIP Error: This indicates that YottaDB/MUPIP did not get replication information from the instance file specified. Replication instance file was not initialized because replication did not start, or some other process reset the replication instance file.

Action: Start the source server if it was not started. Note that the first Source Server process creates the Journal Pool. Subsequent Source Server processes use the Journal Pool that the first Source Server process creates. If the source server was running, stop the server and perform an optimum recovery using MUPIP JOURNAL -ROLLBACK -BACKWARD "*" and restart the Source Server. If optimum recovery command fails, perform a MUPIP RUNDOWN (or a MUPIP RUNDOWN -REGION "*" ), and then restart the Source Server.

--------------------------
NOLBRSRC
--------------------------

NOLBRSRC, Object libraries cannot have SRC paths associated

Run Time Error: This indicates that GTM$ROUTINES/ydb_routines or a SET $ZROUTINES attempted to place a source specification (SRC qualifier/source directory path) on an object library.

Action: Remove the source specification. YottaDB does not use the qualifier SRC= or source directories on object libraries. If you must provide access to sources corresponding to objects in the shared library, attach the source directory to an existing object directory entry. Since YottaDB does not support automatic recompilations into libraries, care must be taken when providing access to sources of library routines.

-----------------------
NOLOCKMATCH
-----------------------

NOLOCKMATCH, No matching locks were found in rrrr

LKE Information: SHOW or CLEAR, found that no LOCKs match the specified criteria in region rrrr; note that specifying no search criteria acts like a wildcard, checking all LOCKs in the region.

Action: If this is not the expected result, check the search criteria and/or research the LOCK protocol to validate its correct operation.

----------------------
NOLOG
----------------------

NOLOG, Logging is currently disabled. Log file is xxxx.

GDE Error: This indicates that GDE is not logging user activities.

Action: Use the LOG -ON command to turn on GDE logging.

-------------------
NOMORESEMCNT
-------------------

NOMORESEMCNT, SSSS counter semaphore has reached its maximum and stopped counting for database DDDD. Run MUPIP JOURNAL -ROLLBACK -BACKWARD, MUPIP JOURNAL -RECOVER -BACKWARD or MUPIP RUNDOWN to restore the database files and shared resources to a clean state

All YottaDB Components Information: The counter semaphore reached its system-imposed limit, so YottaDB no longer maintains the count. SSSS is either "access" or "ftok" signifying the particular counter type that stopped. DDDD is the database of the corresponding counter.

Action: YottaDB will not automatically shutdown the database. To clean the database file header and shared resources after the last process has exited the database file - do an explicit MUPIP -ROLLBACK -BACKWARD (for replicated database files), MUPIP JOURNAL -RECOVER -BACKWARD (for database files that are journaled but not replicated), or MUPIP RUNDOWN (for database files that are neither replicated nor journaled) on the database file DDDD.

-----------------
NONASCII
-----------------

NONASCII, ssss is illegal for a oooo as it contains non-ASCII characters

GDE Error: The specification ssss contains non-ASCII characters which are required for an object of type oooo.

Action: Choose an object name or value containing only ASCII characters.

-------------------
NONEGATE
-------------------

NONEGATE, Qualifier xxxx cannot be negated

GDE Error: This indicates that the qualifier does not support this usage.

Action: Review the `Administration and Operations Guide <../AdminOpsGuide/index.html>`_ or the `Programmer's Guide <../ProgrammersGuide/index.html>`_ for the correct usage.

-------------------
NONTPRESTART
-------------------

NONTPRESTART, Database dddd; code: cccc; blk: bbbb in glbl: ^gggg; blklvl: llll, type: tttt, zpos: pppp

Run Time Information: This is an informational message for non-TP transaction messages. The frequency of this message can be set by $ydb_nontprestart_log_delta and $ydb_nontprestart_log_first environment variables. dddd is the database where the restart occurred; cccc is the code described in the `Maintaining Database Integrity chapter of the Administration and Operations Guide <../AdminOpsGuide/integrity.html>`_; bbbb is the block where YottaDB detected a concurrency conflict that caused the restart; gggg shows the global reference within that block; llll is the level of that block; tttt indicates the type of activity that detected the conflict; pppp is the source line where restart ocurred on.

Action: None required in most cases. If the messages are too frequent, either investigate the processes that reference to that particular global and its block, or reduce the number of messages by tweaking the $ydb_nontprestart_log_delta and $ydb_nontprestart_log_first environment variables.

--------------------
NONUTF8LOCALE
--------------------

NONUTF8LOCALE, Locale has character encoding (cccc) which is not compatible with UTF-8 character set

Run Time Error: This error is reported by YottaDB when it recognizes that the LC_CTYPE locale category cccc (as shown by the UNIX locale command) does not use UTF-8 character encoding when ydb_chset is "UTF-8".

Action: Set the environment variable LC_CTYPE to a Unicode locale name with UTF-8 character encoding. Note that LC_ALL, if defined, overrides LC_CTYPE. The name of the locale varies between different UNIX platforms, but mostly in the form of <lang>_<country>.<charset>, where each element (without the angular brackets) has the form shown below:

- <lang> is the language code in lower case (such as en, or de).
- <country> is the country name in upper case (such as US, GB)
- <charset> is the character set encoding (such as UTF-8, ISO8859-1)

Refer to the operating system manuals for the specific details of available locale names on the system.

-----------------
NOPERCENTY
-----------------

NOPERCENTY, ^%Y* is a reserved global name in YottaDB

GDE Error: This indicates an attempt to map user values to YottaDB reserved space ^%Y*.

Action: Map application names to something other than ^%Y*.

---------------------
NOPINI
---------------------

NOPINI, PINI journal record expected but not found in journal file xxxx at offset yyyy

MUPIP Error: This indicates that MUPIP did not encounter a valid and expected Process Initialization Record (PINI), at offset yyyy of the journal file xxxx.

Action: Run MUPIP JOURNAL EXTRACT -FULL DETAIL FORWARD FENCE=NONE NOERROR_LIMIT xxxx. Report the entire incident context with the appropriate extract file and error message(s) to your YottaDB support channel.

-------------------
NOPLACE
-------------------

NOPLACE, Line specified in a ZBREAK cannot be found

Run Time Warning: This indicates that the ZBREAK was ignored because it specified a line that could not be found in the image.

Action: Modify the ZBREAK or ZLINK the routine that has the missing label.

--------------------
NOPREVLINK
--------------------

NOPREVLINK, Journal file xxxx has a null previous link

MUPIP Error: This indicates that MUPIP found the previous link of journal file xxxx to be null, when it needed to process backward beyond the journal files first record.

Action: Verify that the specified resync or time qualifiers are as intended. If correct values were specified, and the null link was due to operator action, restore the journal generation link and reissue the command.

--------------------
NOPRINCIO
--------------------

NOPRINCIO, Unable to dddd principal device: DDDD at LLLL due to: SSSS

Run Time Fatal: This indicates that YottaDB attempted to but could not READ from or WRITE to (direction indicated by dddd), the PRINCIPAL device and therefore attempted to issue an appropriate error, for example, an IOEOF, TERMHANGUP, or TERMWRITE at location LLLL, with a status of SSSS. However, if the error handling does not prevent any and all subsequent READs and WRITEs to the no longer available PRINCIPAL device, the next I/O error shuts down the process immediately with a NOPRINCIO to prevent mysteriously lost output, or worse, an indefinite loop.

Action: The NOPRINCIO error message is FATAL which does not drive device or trap handlers and terminates the process. This termination does not allow any application level orderly shutdown and, depending on the application, may lead to out-of-design application state. Therefore we recommend appropriate application level error handling that recognizes the error and performs an orderly shutdown without issuing any additional READ or WRITE to the principal device. The most common causes for the principal device to cease to exist involve terminal sessions or socket connections (including those from processes started by inetd/xinetd). When the remote client terminates the connection, the underlying PRINCIPAL device becomes inaccessible making any subsequent attempt to READ from, or WRITE to, it hopeless. In the case of terminals, a user closing the window of a session without cleanly exiting from the YottaDB process sets up the case that can drive this error. YottaDB does not issue NOPRINCIO errors from Direct Mode, because it is a developer tool, or at the completion a HEREDOC in a shell script. However, this means a HEREDOC must use ZHALT to return a specific status to the shell, and that a process in direct mode as the result of a $ETRAP terminates without evidence.

--------------------
NORECVPOOL
--------------------

NORECVPOOL, No receiver pool info found in the replication instance of xxxx

Run Time/MUPIP Error: This indicates that YottaDB/MUPIP did not get replication information from the instance file specified. The replication instance file was not initialized because the receiver server did not start, or some other process reset the replication instance file.

Action: Start the receiver server if the server was not started. If the receiver server was running, stop the server and perform a MUPIP RUNDOWN (if MUPIP RUNDOWN fails, try MUPIP RUNDOWN region * ).

---------------------
NOREGION
---------------------

NOREGION, REGION not found: xxxx

Utility Error: This error can be issued by various MUPIP, DSE, LKE commands that specify a REGION=xxxx qualifier value but the region name could not be located in the current Global Directory. This error can also be issued in case the special region name of :code:`*` is specified and all regions have the AUTODB flag turned on and their corresponding database file does not exist.

Action: Look for and correct any typographical errors in the region name. Use GDE to look in the Global Directory for names of defined regions.

-------------------------
NOREPLCTDREG
-------------------------

NOREPLCTDREG, Replication subsystem found no region replicated for dddd ffff

MUPIP Warning: This indicates that the replication system is present, but no globals are configured for replication in ffff, where dddd is "instance file" for UNIX.

Action: Use MUPIP SET to specify which database regions to replicate.

---------------------
NORESYNCSUPPLONLY
---------------------

NORESYNCSUPPLONLY, NORESYNC only supported for Supplementary Instances

Receiver Server log/MUPIP Error: Issued by a Receiver Server on a non-Supplementary Instance when it is started with a -NORESYNC; -NORESYNC only applies to Supplementary Instances started with -UPDOK, not to non-Supplementary Instances.

Action: Use this qualifier only in a valid context.

----------------------
NORESYNCUPDATERONLY
----------------------

NORESYNCUPDATERONLY, NORESYNC qualifier only allowed on a Supplementary Instance which allows local updates

Receiver Server log/MUPIP Error: Issued by a Receiver Server when started with -NORESYNC on a Supplementary Instance but started without -UPDOK; -NORESYNC applies only to Supplementary Instances started with -UPDOK.

Action: Use this qualifier only in a valid context.

-------------------
NOSELECT
-------------------

NOSELECT, None of the selected variables exist, halting

MUPIP Information: This indicates that a MUPIP EXTRACT or REORG operation did not occur because the global variables specified by the SELECT= qualifier do not exist.

Action: Look for an inappropriate definition for GTM$GBLDIR/ydb_gbldir or typographical errors in the specified variables.

-------------------
NOSOCKETINDEV
-------------------

NOSOCKETINDEV, There is no socket in the current socket device

Run Time Error: This indicates that either no sockets have been established for the device or that all the sockets attached to the device have been closed prior to the current command.

Action: Review the logic managing the sockets and correct it.

---------------------
NOSOCKHANDLE
---------------------

NOSOCKHANDLE, No socket handle specified in WRITE /PASS

Run Time Error: WRITE /PASS was called without specifying at least one socket handle to pass.

Action: Make sure the code specifies at least one socket handle.

----------------
NOSPACECRE
----------------

NOSPACECRE, Not enough space to create database file xxxx. aaaa blocks are needed, only bbbb available.

MUPIP Error: This indicates that the requested file was not created because the file system did not have sufficient space.

Action: Check the allocation size specified in the global directory. Choose another location or reconfigure the file system or volume.

----------------
NOSPACEEXT
----------------

NOSPACEEXT, Not enough disk space for file xxxx to extend. aaaa blocks needed. bbbb blocks available.

Run Time Error: This indicates that there is not adequate space to do a needed journal file extension of the currently specified extension size of aaaa. If the Instance Freeze mechanism is active, YottaDB modifies the NOSPACEEXT message from error (-E-) to warning (-W-), to indicate it is performing the extension even though the available space is less than the specified extension amount.

Action: Locate appropriate disk space and adjust the journal file path. To reestablish durability, perform a MUPIP BACKUP that turns journaling on again.

--------------
NOSTARFILE
--------------

NOSTARFILE, Only star(*) argument can be specified with xxxx

MUPIP Error: This indicates that the qualifier xxxx, specified with the MUPIP JOURNAL command allows only star (*) as an argument.

Action: Specify star (*) as an argument instead of explicit journal file names.

------------------
NOSUBSCRIPT
------------------

NOSUBSCRIPT, No such subscript found (xxxx)

Run Time Error: This indicates that the second argument to $QSUBSCRIPT is less than -1.

Action: Pass a value greater than -1 as the second argument to $QS.

-----------------
NOSUCHPROC
-----------------

NOSUCHPROC, Process xxxx does not exist no need to yyyy it

Run Time Information: This indicates the specified process xxxx does not exist, to which an attempt to signal yyyy was made. This may occur in normal operation, but is reported to the operator logging facility in case an abnormal situation needs to be studied.

Action: -

-------------------
NOSUPPLSUPPL
-------------------

NOSUPPLSUPPL, Instance ssss is configured to perform local updates, so it cannot receive from Supplementary Instance iiii

Receiver Server log/MUPIP Error: A Receiver Server, or a MUPIP JOURNAL -ROLLBACK -FETCHRESYNC on a Supplementary Instance ssss started with -UPDOK, attempted to connect to instance iiii, but found that IIII is also a Supplementary Instance. A Supplementary Instance that permits local updates can only replicate updates that originate on a non-Supplementary Instance.

Action: Reconfigure the instances to a supported configuration.

-------------------
NOTALLDBOPN
-------------------

NOTALLDBOPN, Not all required database files were opened.

MUPIP Fatal: This indicates that all the databases needed for replication could not be opened; the server will not start.

Action: Refer to the accompanying message(s) to determine why all required files would not open. Fix the problem and retry.

------------------
NOTALLDBRNDWN
------------------

NOTALLDBRNDWN, Not all regions were successfully rundown.

Run Time Error: This message indicates an error while running down the database. It could be caused by various conditions such as running out of disk space or IO error.

Action: Look at the previous error messages to identify the cause of this error.

--------------------
NOTALLJNLEN
--------------------

NOTALLJNLEN, Journaling disabled/off for dddd regions

MUPIP Warning: This indicates that some or all regions do not have journal state ON.

Action: Ensure you have journaling enabled for all regions that require it; use MUPIP SET to enable journaling.

----------------------
NOTALLREPLON
----------------------

NOTALLREPLON, Replication off for dddd regions

MUPIP Warning: This indicates that some or all regions have the replication state OFF.

Action: Ensure you have replication on for all regions that require it; use MUPIP SET to enable replication.

---------------------
NOTERMENTRY
---------------------

NOTERMENTRY, TERM = "xxxx" has no "terminfo" entry. Possible terminal handling problems.

Run Time Information: This indicates that while opening a terminal device, the value of the environment variable TERM was xxxx, for which no matching entry was found in the terminfo database. YottaDB uses this information in the terminfo entry to perform terminal specific functions such as cursor movement on screen clearing. With an incorrect entry, such functions are not performed properly.

Action: Exit YottaDB and set the TERM environment variable to a value which exists in the terminfo database, and matches the terminal or terminal emulator being used. See user documentation for more information about terminfo.

--------------------
NOTERMENV
--------------------

NOTERMENV, Environment variable TERM not set. Assuming "unknown."

Run Time Information: This indicates that the TERM environment variable indicating the terminal type in use does not have a value specified.

Action: Find the correct value for the TERM environment variable for the terminal in use and specify that terminal type.

-------------------
NOTERMINFODB
-------------------

NOTERMINFODB, No "terminfo" database. Possible terminal handling problems.

Run Time Information: This indicates that the operating system could not find the terminfo database. The database may be deleted or moved to different location. YottaDB needs this database to display information and accept user input correctly.

Action: This message reflects an operating system problem.

----------------------
NOTEXTRINSIC
----------------------

NOTEXTRINSIC, Quit does not return to an extrinsic function, argument not allowed

Run Time Error: This indicates that a QUIT command specified an argument but did not match an extrinsic function or special variable.

Action: Look for a missing double space after a QUIT, a faulty logic path or a routine that should be invoked as an extrinsic but was invoked with a DO.

--------------------
NOTGBL
--------------------

NOTGBL, Expected a global variable name starting with an up-arrow (^): xxxx

Run Time/MUPIP Error: This indicates that the VIEW argument expression for tracing specifies xxxx, which is not a valid global name. In case of MUPIP error, it indicates that LOAD aborted because it encountered xxxx in its input stream, which is not a valid global name.

Action: Correct the argument of the VIEW command to point to a valid global name. For MUPIP error, refer to the topic MUPIP LOAD Errors in the `About This Manual section of this manual <./about.html>`_.

------------------
NOTMNAME
------------------

NOTMNAME, XXXX is not a valid M name

Compile Time Error: M names must be ASCII, start with a "%" or an alpha and thereafter contain only alphanumeric characters. In YottaDB M, names are currently functionally limited to 31 characters, in most cases, by truncation.

Action: Correct the (typically) routine name to comply with the supported format. Names are also used for labels and both global and local variables. Note that YottaDB usually truncates names longer than its supported maximum - which YottaDB recommends against, because while it can provide embedded information, it can lead to ambiguity or other unintended behavior.

------------------
NOTPOSITIVE
------------------

NOTPOSITIVE, xxxx qualifier must be given a value greater than zero

MUPIP Error: This indicates that the value assigned to the xxxx qualifier value is negative (less than zero).

Action: Assign a value greater than zero (0) for qualifier xxxx.

---------------
NOTPRINCIO
---------------

NOTPRINCIO, Output currently directed to device xxxx

Run Time Warning: This message displays the current device xxxx when it is not the principal device and the process enters Direct Mode.

Action: To redirect all I/O to the terminal, note the current device or save it in a temporary variable and USE $P. If you decide to resume program execution, remember to restore the current device with a USE command.

------------------
NOTREPLICATED
------------------

NOTREPLICATED, Transaction number xxxx generated by the yyyy process (PID = zzzz) is not replicated to the secondary

DSE Information: This indicates that a transaction generated by DSE Update was not replicated to the secondary side. This is expected behaviour. Inappropriate DSE updates while running replication can cause the primary and secondary to be out of sync.

Action: -

------------------
NOTRNDMACC
------------------

NOTRNDMACC, Only random access files are supported as backup files for non-incremental backup

MUPIP Error: This indicates that for comprehensive BACKUP, only random access files are supported, other types of devices, for example, TCP devices and pipes, are not supported.

Action: Perform the BACKUP to a random access file.

-----------------
NOTTOEOFONPUT
-----------------

NOTTOEOFONPUT, Not positioned to EOF on write (sequential organization only)

Run Time Error: This indicates that a WRITE command attempted to update a sequential disk file that was not positioned to end-of-file (EOF).

Action: Read to end-of-file or OPEN the file with the APPEND deviceparameter, if you want to add to the file. If you need a fresh copy of the file, OPEN it with the NEWVERSION deviceparameter.

-------------------
NOUSERDB
-------------------

NOUSERDB, ffff does not support operation on non-GDS format region: rrrr

Utility Error: Facility ffff cannot perform the requested operation on a user-defined database such as rrrr.

Action: Convert the database to GDS format or use the utilities appropriate to the user-defined format.

---------------------
NOVALUE
---------------------

NOVALUE, Qualifier xxxx does not take a value

GDE Error: This indicates that GDE encountered a value for a qualifier that does not accept a value. xxxx is the name of the qualifier.

Action: Specify the qualifier without a value.

-------------------
NOZBRK
-------------------

NOZBRK, No zbreak at that location

Run Time Information: This indicates that a ZBREAK command attempted to remove a ZBREAK from a line that did not specify one. Therefore, no action occurred.

Action: Review the current ZBREAKs using ZSHOW "B". All breaks can be removed using ZBREAK -\*

-----------------------
NOZTRAPINTRIG
-----------------------

NOZTRAPINTRIG, Use of $ZTRAP in a database trigger environment ($ZTLEVEL greater than 0) is not supported.

Trigger/Run Time Error: YottaDB requires the use of $ETRAP for error handling within trigger logic.

Action: Modify the application code to use $ETRAP to handle errors in trigger logic.

-------------------
NULLCOLLDIFF
-------------------

NULLCOLLDIFF, Null collation order cannot be different for all regions

Run Time Error: The standard null collation setting is not the same for all regions.

Action: Using GDE show or DSE dump fileheader, check the standard null collation field for all regions and make sure they are the same.

--------------------
NULLENTRYREF
--------------------

NULLENTRYREF, JOB command did not specify entryref

Run Time Error: This error is issued when the mandatory entryref is not specified with the JOB command.

Action: Specify the entryref for JOB command.

--------------------
NULLPATTERN
--------------------

NULLPATTERN, Empty line found in the Pattern file

MUPIP Warning: MUPIP JOURNAL -EXTRACT pattern file contained an empty line, which generates this message.

Action: Remove the empty line

--------------------
NULSUBSC
--------------------

NULSUBSC, XXXX Null subscripts are not allowed for current region

Run Time/MUPIP Error: This indicates that a global variable specified a null subscript in a database file which does not accept null subscripts. The leading context (XXXX) identifies more about the event or the location of the issue.

Action: Look for the source of the null subscript(s) and consider whether they are appropriate or due to a coding error. If they are appropriate, use MUPIP SET -NULL_SUBSCRIPTS, and remember to make the same adjustment with GDE CHANGE REGION -NULL_SUBSCRIPTS to ensure the next time you recreate a database that the characteristic persists.

------------------
NUMOFLOW
------------------

NUMOFLOW, Numeric overflow

Compile Time/Run Time Error: This indicates that a numeric literal or a string, evaluated to a numeric that exceeds the numeric range of YottaDB.

Action: Look for the source of the large number.

------------------
NUMPROCESSORS
------------------

NUMPROCESSORS, Could not determine number of processors

Run Time Warning: This indicates that the process was unable to determine the number of CPUs in the machine. (The subsequent message(s) give more detailed information.) This causes the number to default to one (1), which if incorrect, may cause sub-optimal tuning.

Action: Analyze the accompanying message(s). If you require assistance, report the entire incident context to your YottaDB support channel.

-------------------
NUMUNXEOR
-------------------

NUMUNXEOR, xxxx unexpected end of record in numeric subscript

MUPIP Error: This indicates that LOAD aborted because it encountered an improperly formatted numeric subscript xxxx in its input stream.

Action: Refer to the topic MUPIP LOAD Errors in the `About This Manual section <./about.html>`_.


---------------------
OBJDUP
---------------------

OBJDUP, xxxx yyyy already exists

GDE Error: This indicates that an ADD command attempted to add a NAME, REGION, or SEGMENT xxxx, that already exists. yyyy is the NAME, REGION, or SEGMENT.

Action: Use the CHANGE command or specify a different object name.

----------------------
OBJFILERR
----------------------

OBJFILERR, Error with object file I/O on file xxxx

Run Time Error: This indicates that [auto]ZLINK processing encountered an error when it attempted to access object file-specification xxxx.

Action: Use host shell commands to examine the file and its protection.

------------------
OBJNOTADD
------------------

OBJNOTADD, Not adding xxxx

GDE Error: This indicates that GDE did not add the specified NAME, REGION, or SEGMENT. xxxx is the NAME, REGION, or SEGMENT specified with the ADD command.

Action: Review the accompanying message(s) for additional information.

--------------------
OBJNOTCHG
--------------------

OBJNOTCHG, Not changing xxxx

GDE Error: This indicates that GDE has not changed the specified NAME, REGION, or SEGMENT. xxxx is the NAME, REGION, or SEGMENT specified with the CHANGE command.

Action: Review the accompanying message(s) for additional information.

---------------------
OBJNOTFND
---------------------

OBJNOTFND, xxxx does not exist

GDE Error: This indicates that a CHANGE or DELETE command specified a NAME, REGION, or SEGMENT that does not exist. xxxx is the NAME, REGION, or SEGMENT.

Action: Use the ADD command, or look for and correct any typographical errors.

-----------------
OBJREQD
-----------------

OBJREQD, xxxx required

GDE Error: This indicates that an ADD, CHANGE, DELETE, RENAME, or TEMPLATE command does not specify a NAME, REGION, or SEGMENT. xxxx is the required object-type.

Action: Look for a missing space or supply the NAME, REGION or SEGMENT.

-------------------
OFFSETINV
-------------------

OFFSETINV, Entry point xxxx+yyyy not valid

Compile Time/Run Time Error: This indicates that YottaDB encountered a label xxxx and an offset yyyy that did not fall within the actual lines of the routine.

Action: Modify the routine so the entry point is a valid entryref.

--------------------
OFRZACTIVE
--------------------

OFRZACTIVE, Region aaaa has an Online Freeze

MUPIP Warning: A MUPIP operation has been requested while an Online Freeze is in place, but the operation cannot be performed with an Online Freeze.

Action: The operation was not performed. Remove the freeze with MUPIP FREEZE -OFF and retry the operation.

------------------
OFRZAUTOREL
------------------

OFRZAUTOREL, Online Freeze automatically released for region aaaa

Operator log Warning: A process needed to modify the database file for region aaaa, which had an Online Freeze, but with AutoRelease selected. The process continued normally, modifying the file.

Action: Discard any database copy or snapshot made after the Online Freeze, as its contents are suspect. Perform a MUPIP FREEZE -OFF to clean up the prior Online Freeze. If the AutoRelease behavior is not desired, try again with MUPIP FREEZE -ON -ONLINE -NOAUTORELEASE. If the cause of the AutoRelease is unclear, report this and the accompanying ERRCALL message to your YottaDB support channel.

--------------------
OFRZCRITREL
--------------------

OFRZCRITREL, Proceeding with a write to region aaaa after Online Freeze while holding crit

Operator log Warning: A process previously encountered a OFRZCRITSTUCK condition, which has since been resolved.

Action: None.

-------------------
OFRZCRITSTUCK
-------------------

OFRZCRITSTUCK, Unable to proceed with a write to region rrrr with Online Freeze while holding crit. Region stuck until freeze is removed.

Operator log Warning: A process needed to do a database write while holding a critical resource, but an Online Freeze was in place without AutoRelease enabled. No other process will be able to acquire the critical resource until the Online Freeze is removed.

Action: MUPIP FREEZE -OFF will remove the freeze and allow the process to continue, at which time it will send a OFRZCRITREL message to the operator log. This situation can be avoided by specifying MUPIP FREEZE -ON -ONLINE without the -NOAUTORELEASE option, or by including the -AUTORELEASE option.

------------------
OFRZNOTHELD
------------------

OFRZNOTHELD, Online Freeze had been automatically released for at least one region

MUPIP Warning: A MUPIP FREEZE -OFF command encountered at least one region which previously had an Online Freeze, but a process had AutoReleased it.

Action: The command cleaned up the region with the AutoReleased Online Freeze, and database operations are back to normal. However, any database file snapshots or copies made after the Online Freeze should be discarded, as processes would likely have written to the file since the AutoRelease. An OFRZAUTOREL message in the operator log will report which process performed the AutoRelease.

------------------
OLDBINEXTRACT
------------------

OLDBINEXTRACT, Loading an older version (xxxx) of binary extract

Run Time Error: This indicates that a MUPIP LOAD input file is of an older type that may not properly deal with collations other than the default (standard M) collation.

Action: No action is required if collation is not an issue. If collation is an issue, the source of the EXTRACT should be upgraded and the MUPIP EXTRACT re-run before the LOAD.

------------------
OMISERVHANG
------------------

OMISERVHANG, GTCM OMI server is hung

GT.CM Error: The GT.CM OMI server has gone a long time exceeding the design expectation without acknowledged activity. At the point when this error occurs, YottaDB creates a core image of the GT.CM OMI server.

Action: Investigate the state of the server and its clients. Restart processes including the server as appropriate; refer diagnostic information to the group responsible for database integrity at your operation.

-----------------
OPCOMMISSED
-----------------

OPCOMMISSED, n errors and m MBFULLs sending prior operator messages

Information: YottaDB issues this message to the operator log if any operator messages prior to the immediately preceding one had not been sent due to errors from $SNDOPR. m is the number of times a persistent MBFULL error prevented a message from being sent and n is the number of other errors whose reports were bypassed.

Action: None.

------------------
OPENCONN
------------------

OPENCONN, Error opening socket connection

Run Time Error: This indicates that the process of opening a socket resulted in a device error.

Action: Review the accompanying message(s) for additional information.

------------------
OPRCCPSTOP
------------------

OPRCCPSTOP, The Cluster Control Program has been halted by an operator stop request

CCE Error: This indicates that a CCE STOP command halted access to clustered databases from this node.

Action: Contact the group responsible for databases at your site for information about when clustered operation will resume.

------------------
ORDER2
------------------

ORDER2, Invalid second argument to $ORDER. Must be -1 or 1

Run Time Error: This indicates that the second argument to a $ORDER function was not a 1 or -1, which are the values the standard permits.

Action: Modify the argument.

---------------
ORLBKCMPLT
---------------

ORLBKCMPLT, ONLINE ROLLBACK completed successfully on instance iiii corresponding to dddd

MUPIP Information: Issued by MUPIP ROLLBACK -ONLINE when it successfully completes work on database file dddd on instance iiii.

Action: None required.

------------------
ORLBKFRZOVER
------------------

ORLBKFRZOVER, tttt : FREEZE on region rrrr (ddd) cleared

MUPIP Information: Issued by MUPIP ROLLBACK -ONLINE when it clears a FREEZE on region rrrr mapped to database file dddd; tttt is the time it cleared the FREEZE.

Action: None required.

------------------
ORLBKFRZPROG
------------------

ORLBKFRZPROG, tttt : waiting for FREEZE on region rrrr (dddd) to clear

MUPIP Information: Issued by MUPIP ROLLBACK -ONLINE when it encounters a region rrrr mapped to database file dddd which is frozen; tttt is the time it encountered the condition.

Action: ROLLBACK waits for a period determined the ydb_db_startup_max_wait environment variable, after which it clears the FREEZE and proceeds. If the ROLLBACK is inappropriate due to the conditions that led to the FREEZE - Cancel the ROLLBACK, otherwise cancel the FREEZE or wait for ROLLBACK to clear it automatically.

--------------------
ORLBKINPROG
--------------------

ORLBKINPROG, Online ROLLBACK in progress by PID pppp in region rrrr

Run Time Information: This message in the operator log indicates that an online rollback has been in progress by process pppp on region rrrr for more than 30 seconds.

Action: None Required.

--------------------
ORLBKNOSTP
--------------------

ORLBKNOSTP, ONLINE ROLLBACK proceeding with database updates. MUPIP STOP will no longer be allowed

MUPIP Information: Issued by MUPIP ROLLBACK -ONLINE when it starts processing that cannot be interrupted without jeopardizing database integrity.

Action: Wait for the ROLLBACK to complete.

--------------------
ORLBKNOV4BLK
--------------------

ORLBKNOV4BLK, Region rrrr (dddd) has V4 format blocks. Database upgrade required. ONLINE ROLLBACK cannot continue

MUPIP Error: Issued by MUPIP ROLLBACK -ONLINE when it finds that the region rrrr mapped to the database file dddd contains V4 format blocks - online rollback does not support old format blocks.

Action: Upgrade the database to the current major version before attempting to use online rollback.

-------------------
ORLBKREL
-------------------

ORLBKREL, ONLINE ROLLBACK releasing all locking resources to allow a freeze OFF to proceed

MUPIP Information: MUPIP ROLLBACK -ONLINE encountered an Instance Freeze and must release its resources and restart to prevent a possible deadlock.

Action: None required as this is an informational message.

-------------------
ORLBKRESTART
-------------------

ORLBKRESTART, ONLINE ROLLBACK restarted on instance iiii corresponding to rrrr

MUPIP Information: MUPIP ROLLBACK -ONLINE is restarting on the instance iiii with replication journal pool rrrr

Action: None required for this informational message

--------------------
ORLBKSTART
--------------------

ORLBKSTART, ONLINE ROLLBACK started on instance iiii corresponding to dddd

MUPIP Information: Issued by MUPIP ROLLBACK -ONLINE when it starts work on database file dddd on instance iiii.

Action: None required.

-------------------
ORLBKTERMNTD
-------------------

ORLBKTERMNTD, ONLINE ROLLBACK terminated on instance iiii corresponding to dddd with the above errors

MUPIP Error: Issued by MUPIP ROLLBACK -ONLINE when it encounters issues that prevent it from operating on database file dddd on instance iiii.

Action: Analyze and address the errors in the output preceding this message.

-------------------
OUTOFSPACE
-------------------

OUTOFSPACE, Database file xxxx ran out of disk space. Detected by process aaaa. Exit without clearing shared memory due to the disk space constraints. Make space and then perform mupip rundown to ensure database integrity.

Run Time Fatal: This indicates that the specified database is full and cannot extend due to lack of disk space. The database could not properly run down.

Action: Examine the space management procedures and take actions to prevent any reoccurrence of this error. The database might get damaged if you do not make enough space for MUPIP RUNDOWN to succeed.

--------------------
PADCHARINVALID
--------------------

PADCHARINVALID, PAD deviceparameter cannot be greater than 127.

Run Time Error: The PAD deviceparameter (valid only for Sequential Disk files) specified in the open command can be between 0 and 127 (both inclusive).

Action: Specify a value within the allowed range.

------------------
PARAMINVALID
------------------

PARAMINVALID, Invalid parameter specified in an API call.

Run Time Error: This indicates that a parameter in a SimpleAPI call was not properly specified. The function name (e.g. ydb_set_s()) and the name of the invalid parameter (e.g. subsarray) along with the type of the invalidity is identified in the error message text. If the parameter is an array, the index of the element where the invalidity is detected is also identified. If the parameter is an input parameter of type ydb_buffer_t, it is invalid if "len_used" is greater than "alloc_len" OR if it has NULL value for "buf_addr" but a non-zero value for "len_used". If the parameter is an output parameter, it is invalid if the ydb_buffer_t pointer is NULL or if the "buf_addr" field in the ydb_buffer_t structure is NULL. Note that no error checks are done if an input ydb_buffer_t typed pointer parameter is NULL (the process would get a SIG-11 and dump core in that case).

Action: Fix the cause of the invalidity and pass in a valid parameter to the SimpleAPI call.

------------------
PARBUFSM
------------------

PARBUFSM, Parse buffer too small

Run Time Error: This indicates that an attempt to parse a file-specification exceeded the maximum length for file-specifications.

Action: Review the file-specification for valid syntax; if it is a logical name/environment variable, confirm its definition.

--------------------
PARFILSPC
--------------------

PARFILSPC, Parameter: xxxx file specification: yyyy

Run Time Error: This indicates that a JOB command jobparameter xxxx specified an invalid file-specification yyyy. For file specifications of the form "SOCKET:<handle>", this message indicates that "<handle>" is not a valid socket handle in the socket pool.

Action: Review the file-specification for valid syntax based on the operating system. For sockets, verify that the socket handle is in the socket pool.

-------------------
PARNORMAL
-------------------

PARNORMAL, Parse successful

Run Time Information: This indicates that the parse was completed successfully.

Action: -

----------------------
PATALTER2LARGE
----------------------

PATALTER2LARGE, Pattern match alternation exceeded the LLLL repetition limit on prospective matches

Run Time Error: An alternation pattern applied to a long occurrence of that pattern reached a YottaDB limit (LLLL) on tracking the match.

Action: Revise the logic to reduce the size of the string being matched or to otherwise break up the match into smaller parts.

----------------------
PATCLASS
----------------------

PATCLASS, Illegal character class for pattern code

Compile Time Error: This indicates that a pattern match specified an invalid pattern class code.

Action: Look for a code that is not supported by the current code definitions or for a literal match that is not enclosed in quotes.

--------------------
PATCODE
--------------------

PATCODE, Illegal syntax for pattern

Compile Time Error: This indicates that a pattern match specified an invalid syntax.

Action: Look for a missing repeat count.

---------------------
PATLIT
---------------------

PATLIT, Illegal character or unbalanced quotes for pattern literal

Compile Time Error: This indicates that a pattern match included the start of a string literal that did not finish properly.

Action: Look for unbalanced quotes in the string literal.

-------------------
PATLOAD
-------------------

PATLOAD, Error loading pattern file xxxx

Compile Time Error: This indicates that YottaDB failed to load the pattern file.

Action: Review accompanying messages for additional information about the cause of this error.

------------------
PATMAXLEN
------------------

PATMAXLEN, Pattern code exceeds maximum length

Compile Time Error: This indicates that a pattern match specification required more temporary storage than is available.

Action: Modify the routine so it uses shorter pattern specifications.

------------------
PATNOTFOUND
------------------

PATNOTFOUND, Current pattern table has no characters with pattern code xxxx

Run Time Error: This indicates that the specified pattern code does not exist in the pattern table.

Action: Update the pattern table with the code, or change the program to make sure the specified pattern code is not referenced in the table.

-------------------
PATTABNOTFND
-------------------

PATTABNOTFND, Pattern table xxxx not found

Run Time Error: This indicates that an attempt to load a pattern table failed because it was not found in the file described by the logical name/environment variable ydb_pattern_file or loaded by the VIEW "PATLOAD" command.

Action: Use host shell commands to examine the file and modify either the file or the VIEW command that performs the load.

-----------------------
PATTABSYNTAX
-----------------------

PATTABSYNTAX, Error in xxxx at line yyyy

Compile Time/Run Time Error: This indicates that YottaDB found an error on line yyyy of the file xxxx that defines the patterns to be used by the pattern match operator.

Action: Modify the pattern match file and reload it. For more information, refer to the `"Internationalization" chapter in the Programmer's Guide <../ProgrammersGuide/internatn.html>`_.

---------------------
PATUPPERLIM
---------------------

PATUPPERLIM, Pattern code upper limit is less than lower limit

Compile Time Error: This indicates that a pattern match specified a repeat count range with an upper limit that is below the lower limit.

Action: Look for improperly ordered repeat count ranges.

-------------------
PBNINVALID
-------------------

PBNINVALID, ssss does not have a field named ffff

Utility Error: This message comes from %PEEKBYNAME() when a valid struct but an invalid field name is given as the first argument. A struct, ssss, does not have a field named ffff.

Action: Check the field name. Verify that the field exists and its specification has no typographical error.

------------------
PBNNOFIELD
------------------

PBNNOFIELD, %ZPEEKBYNAME() requires a field. item as its first parameter.

Utility Error: The first argument of %ZPEEKBYNAME() may be missing, empty, contain an unsupported field or be missing an item.

Action: Verify that the first parameter to %ZPEEKBYNAME() is not NULL

------------------
PBNNOPARM
------------------

PBNNOPARM, First parameter pppp does not support a second parameter

Utility Error: pppp does not take a region name or index number as the second parameter to %PEEKBYNAME().

Action: Omit the second parameter of %PEEKBYNAME() or make it NULL.

--------------------
PBNPARMREQ
--------------------

PBNPARMREQ, A first parameter value pppp requires a second parameter specified containing rrrr

Utility Error: pppp requires a second parameter but the second parameter of %PEEKBYNAME() is NULL or undefined. rrrr indicates whether the required parameter is an index number or region name.

Action: Depending on rrrr, choose a valid index number or region name and make sure the second parameter is not NULL.

----------------------
PBNUNSUPSTRUCT
----------------------

PBNUNSUPSTRUCT, $ZPEEK() does not support structure ssss

Utility Error: The first argument of %PEEKBYNAME() is a value that is not known to $ZPEEK().

Action: Make sure the first argument of %PEEKBYNAME() is a valid struct name that is accessible to $ZPEEK.

-----------------------
PBNUNSUPTYPE
-----------------------

PBNUNSUPTYPE, $ZPEEK() does not support type tttt

Run Time Error: The $ZPEEK() function has encountered an invalid type argument: tttt.

Action: Refer to the $ZPEEK() documentation for information on valid types. Examine the $ZPEEK invocation to determine and correct the source of the invalid type.

-----------------------
PCONDEXPECTED
-----------------------

PCONDEXPECTED, Post-conditional expression expected but not found

Compile Time Error: This indicates that a colon (:) appeared to start a postconditional, but it was not followed by a valid postconditional expression.

Action: Look for unwanted colons or missing post-conditional expressions.

-------------------
PCTYRESERVED
-------------------

PCTYRESERVED, Attempted operation not supported on ^%Y* namespace

All YottaDB Components Error: The ^%Y* global namespace is reserved for YottaDB and is not available to application code except as otherwise documented.

Action: Map all application globals somewhere other than ^%Y* and make sure that application code references are correct.

------------------
PEERPIDMISMATCH
------------------

PEERPIDMISMATCH, Local socket peer with PID=pppp does not match specified PID=qqqq

Run Time Error: WRITE /PASS or WRITE /ACCEPT was given a process id qqqq to verify, but the connection peer process id is pppp.

Action: Make sure that only the specified process has opened the socket connection.

------------------
PERMGENDIAG
------------------

PERMGENDIAG, Permissions: Proc(uid:uuuu,gid:gggg), DB File(uid:vvvv,gid:hhhh,perm:pppp), Lib File(gid:iiii,perm:qqqq), Group Mem(opener:jjjj,owner:kkkk)

Run Time Information: This shows the permissions involved in resource creation for the process, the associated database file, the libyottadb and the process group membership.

Action: Typically none, but if you have a permission issue, use this key information for diagnosis.

------------------
PERMGENFAIL
------------------

PERMGENFAIL, Failed to determine access permissions to use for creation of xxxx for file yyyy

Run Time/MUPIP Error: This message indicates that YottaDB was unable to determine the permissions to use when creating a file or resource associated with database file yyyy. xxxx may be "ipc resources", "journal file", "backup file", or "snapshot file".

Action: Note the user and group ownership of the database file and $ydb_dist/libyottadb.*, and the user and group permissions of the YottaDB process, and report them to your YottaDB support channel.

------------------
PINENTRYERR
------------------

PINENTRYERR, Custom pinentry program failure

Run Time Error: The encryption reference plugin's custom pinentry program failed.

Action: Please refer to the documentation for the supplemental error messages and correct the errors.

------------------
PRCNAMLEN
------------------

PRCNAMLEN, Process name xxxx length is greater than yyyy

Run Time Error: This indicates that a JOB command PROCESS_NAME=xxxx jobparameter specified a value that exceeds the maximum acceptable length yyyy.

Action: Modify the process name so that it does not exceed yyyy characters.

-----------------
PREALLOCATEFAIL
-----------------

PREALLOCATEFAIL, Disk space reservation for SSSS segment has failed

MUPIP/Run Time Error: Indicates that disk space preallocation has failed due to a system call error.

Action: Please read the accompanying system message to find out why the system call error occurred and resolve that problem.

----------------
PREFIXBAD
----------------

PREFIXBAD, xxxx must start with an alphabetic character to be a yyyy

GDE Error: This indicates that an ADD, CHANGE, DELETE, RENAME, or TEMPLATE command specified a REGION or SEGMENT name that does not begin with an alphabetical character. xxxx is the REGION or SEGMENT name. yyyy is the object-type.

Action: Look for and correct typographical errors.

---------------
PREMATEOF
---------------

PREMATEOF, Premature end of file detected

MUPIP/Run Time Error: A file read or write detected an end-of-file when it was expecting additional records.

Action: Analyze accompanying messages for the type of file on which the operation failed. If the operation was a MUPIP LOAD, refer to the `About this Manual section on MUPIP LOAD errors <./about.html#mupip-load-errors>`_. If the circumstances warrant, contact the group responsible for database integrity at your operation with all the diagnostic context you can gather.

------------------
PREVJNLLINKCUT
------------------

PREVJNLLINKCUT, Previous journal file name link set to NULL in new journal file xxxx created for database file yyyy

Run Time/MUPIP Error: This indicates that YottaDB or MUPIP has removed the link of the previous journal file name and set it to NULL in the new xxxx journal file's header. This could possibly be because the journal state was ON for the database file yyyy and its corresponding journal file was inaccessible, which triggered MUPIP or YottaDB to create new journal file xxxx clearing the previous generation journal file name(s).

Action: If the error is issued by YottaDB, review the accompanying message(s) in the operator log.

If a MUPIP SET -JOURNAL=ON command produces this message for the region in the operator log, it may indicate that one or more of the current generation journal files are damaged/missing and new journal files were created with no back pointers to the previous journal files. YottaDB recommends taking a database backup at the earliest convenience because a MUPIP RECOVER/ROLLBACK will not be able to go back past xxxx. If this message is for a specified region(s), consider switching the journal files for all regions (with REGION "*") that the process has opened (all journaled/replicated regions in the instance, if replication is in use) to ensure that the RECOVER/ROLLBACK for other regions remains unaffected.

No action is required if the MUPIP BACKUP -NEWJNLFILES=NOPREVLINK issues the error.

------------------
PREVJNLLINKSET
------------------

PREVJNLLINKSET, Previous generation journal file name is changed from xxxx to yyyy

MUPIP Information: This indicates that MUPIP SET -JNLFILE command has changed the previous generation journal file name from xxxx to yyyy.

Action: -

------------------
PREVJNLNOEOF
------------------

PREVJNLNOEOF, A previous generation journal file xxxx does not have valid EOF

MUPIP Error: This indicates that while opening the previous generation journal file xxxx, MUPIP encountered the journal file in an inconsistent state; it had not been terminated properly.

Action: Report the entire incident context to your YottaDB support channel.

-----------------
PRIMARYISROOT
-----------------

PRIMARYISROOT, Attempted operation not valid on root primary instance xxxx

MUPIP Error: If a replication instance has local updates enabled i.e. the Source Server that created the journal pool was started with -UPDOK, issuing any Source Server command with the start, activate or deactivate qualifiers where the command explicitly specifies the propagateprimary qualifier or that qualifier is implicitly assumed by default, or, if this is a not a supplementary instance, attempting to start a Receiver Server causes MUPIP to issue this error.

Action: Do not start a Receiver Server on a root primary non-supplementary instance. Use rootprimary qualifier instead of propagateprimary in the source server command.

------------------
PRIMARYNOTROOT
------------------

PRIMARYNOTROOT, Attempted operation not valid on non-root primary instance xxxx

MUPIP Error: If a replication instance is not a root primary (the journal pool already exists and was created by a source server command that specified propagateprimary), issuing a source server command with the start or deactivate qualifiers that has the rootprimary qualifier explicitly specified (or implicitly assumed) on this instance will cause this error to be issued. This error can also be issued by the receiver server or mupip rollback if the instance that the source server is running on is not a root primary and it connects to a receiver server or a mupip journal -rollback -fetchresync running on an instance that was formerly a root primary and has not yet had a mupip replic -source -losttncomplete command run either explicitly or implicitly on it.

Action: Use the propagateprimary qualifier instead of rootprimary in the source server command. If this error is issued by the receiver server or fetchresync rollback, the secondary instance has to be brought up as the secondary of a root primary since it was a root primary immediately before this. The rule is that any instance that was previously a root primary should be brought up as a secondary of the new root primary. This will create a lost transaction file that needs to be applied on the new root primary. Once that is done, a mupip replic -source -losttncomplete command should be run either explicitly or implicitly on this instance before trying to bring this up as a secondary of a propagating primary.

--------------------
PROCTERM
--------------------

PROCTERM, uuuu process termination due to cccc from eeee

Utility Warning: A utility uuuu, typically MUPIP, executing application code, possibly from a trigger, encountered a command cccc to terminate at $zposition location pppp.

Action: It is not typically wholesome for MUPIP to terminate this way - review your error handling and trigger definitions for a possible bug or misfeature.

---------------------
PROTNOTSUP
---------------------

PROTNOTSUP, Protocol xxxx not supported

Run Time Error: This indicates that the protocol specified on the CONNECT or LISTEN deviceparameters is not currently supported.

Action: Use TCP/IP domain sockets by specifying TCP for the protocol string or LOCAL (aka UNIX) domain sockets by specifying LOCAL.

-------------------
QUALBAD
-------------------

QUALBAD, xxxx is not a valid qualifier

GDE Error: This indicates that GDE encountered a command with an invalid qualifier xxxx.

Action: Look for and correct typographical errors in the qualifier.

---------------
QUALDUP
---------------

QUALDUP, xxxx qualifier appears more than once in the list

GDE Error: This indicates that GDE encountered the qualifier xxxx more than once in the command.

Action: Specify the qualifier only once in the list.

-----------------
QUALEXP
-----------------

QUALEXP, Qualifier expected but not found

Run Time Error: This indicates that a $ZROUTINES function did not encounter a qualifier, which is the next valid syntax element.

Action: Look for a missing right parenthesis or extra left parenthesis in a source directory specification.

---------------
QUALREQD
---------------

QUALREQD, xxxx required

GDE Error: This indicates that a command was missing the required xxxx qualifier.

Action: Enter the missing qualifier.

--------------
QUALVAL
--------------

QUALVAL, Qualifier value required but not found

Run Time Error: This indicates that a $ZROUTINES function did not specify a value for the SRC qualifier.

Action: The SRC qualifier requires a value.

---------------
QUERY2
---------------

QUERY2, Invalid second argument to $QUERY. Must be -1 or 1.

Run Time Error: This indicates that there is an invalid second argument passed to the function $QUERY. It must be either -1 or 1.

Action: Refer to `$QUERY in the Programmer's Guide <../ProgrammersGuide/functions.html#query-function>`_ for correct usage.

------------------
QUITALSINV
------------------

QUITALSINV, QUIT * return when the extrinsic was not invoked with SET *

Run Time Error: A [sub-]routine tried to pass an alias back to the caller, but the routine was not invoked to accept an alias return.

Action: Rework either the invocation or the return, or troubleshoot why the inappropriate invocation occurred. If the routine should conditionally return an alias, use $QUIT to select the proper type of return.

-------------------
QUITARGLST
-------------------

QUITARGLST, Quit cannot take a list of arguments

Compile Time Error: This indicates that a QUIT specified multiple arguments; M accepts only one argument.

Action: Look for a missing space after the QUIT or a typographical error. Modify the QUIT argument so that it consists of a single expression.

-------------------
QUITARGREQD
-------------------

QUITARGREQD, Quit from an extrinsic must have an argument

Run Time Error: This indicates that a QUIT did not specify an argument but it corresponded to an invocation by an extrinsic function or special variable.

Action: Review the interface between the extrinsic and the invoked routine. Modify the QUIT or the invocation.

--------------------
QUITARGUSE
--------------------

QUITARGUSE, Quit cannot take an argument in this context

Compile Time Error: This indicates that a QUIT in the scope of a FOR command specified an argument.

Action: Look for a missing space after the QUIT.

---------------------
RANDARGNEG
---------------------

RANDARGNEG, Random number generator argument must be greater than or equal to one

Run Time Error: This indicates that a $RANDOM function specified a zero or a negative argument.

Action: Look for the source of the argument. If you want to generate a 0 or 1 result, the argument should be 2 because a seed of 1 always produces the less-than-random result of 0.

--------------------
RAWDEVUNSUP
--------------------

RAWDEVUNSUP, RAW device for region RRRR is not supported

Run Time/MUPIP Error: This indicates an attempt to configure a raw device database. This is no longer supported.

Action: Create the database in a regular filesystem.

-------------------
RCVRMANYSTRMS
-------------------

RCVRMANYSTRMS, Receiver server now connecting to source stream NNNN but had previously connected to a different stream nnnn

Receiver Server log/MUPIP Error: Issued by a Receiver Server on a Supplementary Instance (started with -UPDOK) which had formerly connected to a source server corresponding to a non-Supplementary stream nnnn, later disconnected and on reconnection found that the Source Server corresponds to a different non-Supplementary stream NNNN.

Action: Mixing of non-Supplementary streams are not allowed in the same Receiver Server process. Restart the Receiver Server.

---------------------
RDFLTOOLONG
---------------------

RDFLTOOLONG, Length specified for fixed length read exceeds the maximum string size

Run Time Error: The size specified in the fixed length READ is too large. It should not exceed 1048576, which is the maximum string size supported by YottaDB.

Action: Modify the READ to specify a length less than or equal to 1048576.

-----------------------
RDFLTOOSHORT
-----------------------

RDFLTOOSHORT, Length specified for fixed length read less than or equal to zero

Run Time Error: This indicates that a READ fixed length (#) specified a value of less than one.

Action: Change the length (i.e., the portion of the READ argument that appears after the delimiter (#)) to a valid value, or add a postconditional to the READ command to suppress the length when it is less than or equal to zero.

-------------------------
READONLYLKFAIL
-------------------------

READONLYLKFAIL, Failed to get a lock on READ_ONLY database file.

Run Time Error: This error is issued by a MUPIP command that requires standalone access (e.g. MUPIP SET -NOREAD_ONLY) to a database file (which has Read-only mode turned on) if other processes are still accessing the database OR by any process that tries to open a database file (which again has Read-only mode turned on) while a MUPIP command that has standalone access on the same database file is concurrently running.

Action: If the error is from the MUPIP command which requires standalone access, ensure all processes which have the database file open are shut down and reattempt the command. If the error is from a process trying to open the database file, wait for the concurrent MUPIP command requiring standalone access to finish and reattempt to open the database.

-------------------------
READONLYNOBG
-------------------------

READONLYNOBG, Read-only cannot be enabled on non-MM databases

MUPIP Error: This indicates an attempt to change a BG database to -READ_ONLY or to change a -READ_ONLY to MM access method; -READ_ONLY only compatible with the MM access mode.

Action: Verify whether the database should not be read-only and adjust if appropriate. Alternatively, set the database to MM access mode and then mark it as read-only.

-------------------------
READONLYNOSTATS
-------------------------

READONLYNOSTATS, Read-only and Statistics sharing cannot both be enabled on database.

Run Time Error: This error is issued if one tries to enable the Read-only mode on a database that has Statistics sharing turned on OR if one tries to enable Statistics sharing on a database that has Read-only mode turned on OR if one tries to enable both at the same time.

Action: Make sure that at most one of Read-only or Statistics sharing is turned on in the database at any point in time.

-------------------------
REC2BIG
-------------------------

REC2BIG, Record size (xxxx) is greater than maximum (yyyy) for region: zzzz

Run Time Error: This indicates that a SET attempted to create a database node with a combined length of keys and data (xxxx) that exceeds the maximum length yyyy permitted for region zzzz.

Action: Use smaller data records or keys in the program. If you want to enlarge the record size for the region, use GDE to change the Global Directory and recreate the database with MUPIP CREATE. If it is necessary to permit the data without allowing time to rebuild the database, use MUPIP CHANGE -RECORD_SIZE. Be careful when you increase the size for existing databases; use GDE to ensure that they have proper characteristics the next time they are CREATEd.

----------------------
RECCNT
----------------------

RECCNT, Last LOAD record number: xxxx

MUPIP Information: EXTRACT and LOAD use this message to display xxxx, the total number of records processed.

Action: -

---------------------
RECLOAD
---------------------

RECLOAD, Error loading record number: nnnn

MUPIP Error: This message identifies nnnn, a record or a range of records, that MUPIP could not LOAD and follows a message about the cause. If this message is Fatal, which it can be for BIN format, it produces a core file for diagnostic analysis.

Action: Address the cause or, for GO and ZWR format input files, examine the record or range of records with a text editor for possible correction or alternate action and for BIN format if fixing the cause does not resolve the error switch to ZWR format EXTRACT.

---------------------
RECORDSTAT
---------------------

RECORDSTAT, gggg: Key cnt: kkkk max subsc len: ssss max rec len: dddd max node len: rrrr

MUPIP Information: LOAD and EXTRACT use this to report on some characteristics of the global variables they processed, where gggg is an unsubscripted global name (region name appears in parentheses if gggg spans multiple regions), kkkk is the number of unique data cells in the array, ssss is the maximum subscripted key length, dddd is the maximum data length and rrrr is the maximum combined length of keys and subscripts.

Action: Use the information as appropriate.

---------------------
RECSIZENOTEVEN
---------------------

RECSIZENOTEVEN, RECORDSIZE [xxxx] needs to be a multiple of 2 if ICHSET or OCHSET is UTF-16, UTF-16LE or UTF-16BE

Run Time Error: This error is issued when the OPEN command specifies an ICHSET or OCHSET or CHSET of UTF-16 or UTF-16LE or UTF-16BE and the RECORDSIZE specified (xxxx) is not a multiple of 2.

Action: Specify a RECORDSIZE that is a multiple of 2.

-------------------
RECSIZIS
-------------------

RECSIZIS, Record size is xxxx

GDE Information: This message displays the record size of the REGION with which you are working.

Action: Review the accompanying message(s) for additional information.

-----------------
RECTOOBIG
-----------------

RECTOOBIG, Block size xxxx and yyyy reserved bytes limit record size to zzzz

GDE Warning: This indicates that an ADD, CHANGE, or TEMPLATE command specified a value for the qualifier RECORDSIZE that is incompatible with the value of xxxx specified for BLOCKSIZE. zzzz is the maximum RECORDSIZE supported by this BLOCKSIZE, and yyyy RESERVED_BYTES for the block. GDE displays this message with other error messages, including one that reports the specified record size.

Action: Modify the RECORDSIZE, BLOCKSIZE and/or RESERVED_BYTES, so they are compatible.

-----------------
RECVPOOLSETUP
-----------------

RECVPOOLSETUP, Receive Pool setup error

Run Time Error: This indicates that an error occurred in the replication subsystem while initializing the receive pool.

Action: Verify that the receiver server has been configured correctly. See accompanying messages for more information about the cause of this error.

---------------------
REGFILENOTFOUND
---------------------

REGFILENOTFOUND, Database file DDDD corresponding to region RRRR cannot be found

MUPIP Error: This indicates that MUPIP cannot locate the database file DDDD mapped to region RRRR.

Action: Ensure that the current global directory is the one intended to map the file intended. If the path is relative or includes environment variables, ensure that the current working directory and any environment variables are appropriate. Also ensure the file exists and has authorizations, including its path, that make it available to the user attempting to access it. If the MUPIP command involves a statsDB (for example MUPIP INTEG -STATS), ensure that the appropriate regions have STATS enabled, that the $ydb_statsdir environment variable has been properly defined, and that other processes are using shared statistics, as MUPIP by itself does not create new statsDB databases. Note that MUPIP INTEG does not create statsDB and reports any that it skips with an informational message, but exits with a normal status after such skips.

--------------------
REGIS
--------------------

REGIS, in region xxxx

GDE Information: This message displays the name of the REGION with which you are working.

Action: Review the accompanying message(s) for additional information.

---------------------
REGNTFND
---------------------

REGNTFND, Region referenced not initialized

GT.CM Error: This indicates that there has been a region management error. The region may not be present, or there may have been an error during initialization.

Action: Record any accompanying messages and if necessary report the entire incident context to your YottaDB support channel.

-------------------
REGOPENFAIL
-------------------

REGOPENFAIL, Failed to open region rrrr (dddd) due to conflicting database shutdown activity

DSE/LKE Error: Another process or processes repeatedly removed shared memory right after each one of the interlocking bypass. As a result of this, LKE/DSE failed to initialize region rrrr (database for dddd)

Action: Identify the process or processes that causes LKE/DSE to bypass interlocking mechanism by holding semaphore(s). To do that, follow RESRCINTRLCKBYPAS messages. They should tell you the PID of the process currently holding the semaphore. Check the process to see if it is stuck. If it is not stuck or has already terminated, it is likely that the database is being closed and opened abnormally fast. You may try running LKE/DSE again to achieve successful initialization.

---------------------
REGSSFAIL
---------------------

REGSSFAIL, Process pppp encountered error eeee contributing to the snapshot for region rrrr - the snapshot is no longer valid.

MUPIP Error: A YottaDB process encountered failure while opening snapshot file or attaching to shared memory or writing a block to the snapshot file, any of which invalidate the snapshot file. The original error eeee that process pppp encountered follows the REGSSFAIL error message and can also be found in the syslog (search for messages from process pppp).

Action: Examine the syslog for messages issued by process pppp to obtain details of the failure and take action, possibly by modifying file access characteristics or user roles, to address the problem.

----------------------
RELINKCTLERR
----------------------

RELINKCTLERR, Error with relink control structure for $ZROUTINES directory dddd

Run Time Error: Indicates a problem accessing a relink control file in the temporary directory typically specified by the ydb_linktmpdir environment variable.

Action: Use the accompanying message(s) for a detailed error status to diagnose and address the access issue.

---------------------
RELINKCTLFULL
---------------------

RELINKCTLFULL, Relinkctl file for directory dddd is full (maximum entries mmmm)

Run Time Error: A process using directory dddd initiated an auto-relink action with ZLINK or ZRUPDATE, or an autolink check with DO, GOTO, ZBREAK, ZGOTO, ZPRINT or $TEXT() which required adding information for the routine in question to the relinkctl file for directory dddd, but the relinkctl file was full with mmmm entries.

Action: Shut down all processes accessing the relinkctl file and issue MUPIP RUNDOWN -RELINKCTL dddd to cleanup the relinkctl file of potentially unused entries. Additionally, reduce the number of objects in the directory, typically by adding an additional object directory.

----------------------
REMOTEDBNOSPGBL
----------------------

REMOTEDBNOSPGBL, Database region rrrr contains portion of a spanning global and so cannot point to a remote file

Run Time Error: This indicates that region rrrr of the current global directory contains parts of a spanning global and therefore cannot point to a remote database file.

Action: Fix the global directory file so region rrrr points to a local file or remove the global nodes that span into this region.

---------------------
REMOTEDBNOTRIG
---------------------

REMOTEDBNOTRIG, Trigger operations on global gggg not supported as it maps to database region rrrr that points to a remote file

MUPIP/Run Time Error: $ZTRIGGER() or MUPIP TRIGGER attempted to add, delete or modify a trigger in global gggg mapped to database region rrrr, which is defined as a GT.CM remote region, but GT.CM does not support trigger update actions.

Action: If you have a need for triggers on globals in that region, log on to the remote instance in order to perform trigger maintenance.

--------------------
RENAMEFAIL
--------------------

RENAMEFAIL, Rename of file xxxx to yyyy failed

MUPIP Warning: This indicates that MUPIP failed in its attempt to rename the existing file xxxx to yyyy, before creating the new xxxx file.

Action: Check the accompanying message(s) for additional information.

------------------
REORGCTRLY
------------------

REORGCTRLY, User interrupt encountered during database reorg -- halting

MUPIP Information: This indicates that a REORG was interrupted. The reorganization is incomplete but the database is intact with no loss of data.

Action: Initiate REORG with RESUME qualifier, if appropriate.

-------------------
REORGINC
-------------------

REORGINC, Reorg was incomplete. Not all globals were reorged.

MUPIP Warning: This indicates that MUPIP did not reorg all the globals because of some resource constraint errors.

Action: Review the accompanying message(s) for more information.

------------------
REPL2OLD
------------------

REPL2OLD, Instance IIII uses a YottaDB version that does not support connection with the current version on iiii.

MUPIP Error: Issued by a Source Server, Receiver Server or MUPIP JOURNAL -ROLLBACK -FETCHRESYNC on Instance iiii attempted to connect to instance IIII, but found IIII is running an earlier version that does not support the current replication protocol. This can indicate either that the older version is just too old for any connection with the newer version, or that the older version doesn't have the logic required to support a Supplementary Instance.

Action: Upgrade the YottaDB version on IIII to a version that can support communication with the current version or, if this is a Supplementary Instance, that can deal with a Supplementary Instance or choose another appropriate instance for the connection.

---------------------
REPLACCSEM
---------------------

REPLACCSEM, Error with replication access semaphore (id = xxxx) for instance file aaaa

MUPIP Error: This indicates a problem with the semaphore xxxx associated with the instance designated by aaaa.

Action: Review the accompanying message(s) for details.

-----------
REPLALERT
-----------

REPLALERT, Source Server could not connect to replicating instance [XXXX] for [NNNN] seconds

MUPIP Warning: The Source Server records this warning message when the Source Server fails to establish a replication connection with the secondary instance [XXXX] for [NNNN] seconds. The frequency of recording this warning message can be adjusted with the soft connection attempt period (the third -CONNECTPARAM).

Action: Use the REPLALERT message as an mechanism to alert operations about replication network issues. Specify 0 as the REPLALERT period parameter (the third -CONNECTPARAM) to disable logging this message. The REPLALERT messages are disabled by default (that is, without specifying -CONNECTPARAM).

REPLALERT was added to YottaDB effective release r1.36.

---------------------
REPLBRKNTRANS
---------------------

REPLBRKNTRANS, Replication subsystem found transaction xxxx broken or missing in the journal files

MUPIP Error: This indicates that while attempting to read the transaction with journal sequence number xxxx from journal files, the source server could not find all (or any) journal records belonging to that transaction.

Action: Restore the journal generation links, and/or the journal files. Deactivate and activate the source server (or shutdown and restart the source server). If the journal files that are needed are no longer available, follow the procedure for restoring the secondary from the backup of the primary detailed in the `Replication chapter of the Administration and Operations Guide <../AdminOpsGuide/dbrepl.html>`_.

-----------------------
REPLCOMM
-----------------------

REPLCOMM, Replication subsystem communication failure

MUPIP Error: This is a generic error indicating that there has been a communication error between the two systems performing replication.

Action: Review the accompanying message(s) for more information about the cause of this error.

----------------------
REPLERR
----------------------

REPLERR, XXXX

MUPIP Warning: This indicates that YottaDB is performing tasks that may result in an error.

Action: Review accompanying messages for more information about why YottaDB generated this message.

--------------------
REPLEXITERR
--------------------

REPLEXITERR, Replication process encountered an error while exiting

Run Time Error: This indicates that the source, receiver or update process encountered an error during exit processing.

Action: Review accompanying message(s) for more information.

--------------------
REPLFILIOERR
--------------------

REPLFILIOERR, Replication subsystem file I/O error xxxx

MUPIP Error: This indicates that the system was unable to perform an I/O operation on a file on the replication primary server. The accompanying message also tells whether there was a read error or a write error and names the file on which the error occurred.

Action: Review the accompanying message(s) for more information about the cause of this error.

---------------------
REPLFILTER
---------------------

REPLFILTER, Replication filter subsystem failure

MUPIP Error: This indicates that the replication filter subsystem failed to start the application filter.

Action: In order to restart the filter subsystem, it is necessary to shut down the replication server and restart. If this error continues to appear, report the entire incident context to your YottaDB support channel.

------------------------
REPLFTOKSEM
------------------------

REPLFTOKSEM, Error with replication semaphores for instance file xxxx

MUPIP Error: This indicates that MUPIP could not create semaphore for replication instance file.

Action: Review the accompanying message(s) for more information.

--------------------------
REPLINFO
--------------------------

REPLINFO, xxxx

Run Time Information: The information is contained in the message.

Action: -

--------------------
REPLINSTACC
--------------------

REPLINSTACC, Error accessing replication instance file xxxx

Run Time/MUPIP Error: This indicates that some errors were encountered while accessing the specified replication instance file defined by $ydb_repl_instance or the relevant global directory.

Action: Refer to the accompanying message(s) for additional information.

--------------
REPLINSTCLOSE
--------------

REPLINSTCLOSE, Error closing replication instance file xxxx

Run Time Error: There was an error when YottaDB or MUPIP tried to close the replication instance file. The error detail accompanies this message.

Action: Look at the accompanying error detail. Possible causes are file permissions, system quotas, etc. Fix the cause if possible. If not, report the entire incident context to your YottaDB support channel for further analysis.

--------------------
REPLINSTCREATE
--------------------

REPLINSTCREATE, Error creating replication instance file xxxx

Run Time Error: There was an error when YottaDB or MUPIP tried to create the replication instance file. The error detail accompanies this message.

Action: Look at the accompanying message that gives error details. Possible causes are file permissions, system quotas, and so on.  If possible, correct the cause and retry creating the replication instance file. If the error persists, report to your YottaDB support channel along with the error details.

---------------------
REPLINSTDBMATCH
---------------------

REPLINSTDBMATCH, Replication instance file xxxx has seqno xxxx while database has a different seqno yyyy

MUPIP Error: This error is issued by the first source server that is started on a replication instance or a mupip journal -rollback command if the journal sequence numbers stored in the instance file do not match those stored in the database file header. This is possible if the database was recreated or refreshed from a backup on another instance without correspondingly recreating the instance file.

Action: If this instance is not the root primary, this error can be handled by restoring both the database and the instance file from a previous backup (consistent backup of the instance file AND database files taken together at the same time) and restarting the instance. Subsequent to such a restore, all transactions since the last backup will be sent across from this instance's primary. Alternatively, this can be handled by shipping a copy of the database from any other instance (either the primary or any other secondary/tertiary), recreating the instance file and starting this instance as a secondary with the -updateresync qualifier. In either case, this procedure has to be repeated on all tertiary instances etc. that descend from this instance, ensuring that for every primary-secondary instance pair, the secondary is not ahead of the primary in terms of journal sequence number. If this instance is the root primary, restoring from a prior backup may not be viable as it may mean loss of transactions that occurred after the backup. The alternative way to handle this error is to recreate the instance file on the root primary, ship a copy of the database from the primary and recreate instance files on ALL secondaries (tertiaries etc.) and restart the secondaries with the -updateresync qualifier. In addition, report the entire incident context to your YottaDB support channel.

-------------------------
REPLINSTDBSTRM
-------------------------

REPLINSTDBSTRM, Replication instance file rrrr has seqno xxxx for Stream nnnn while database has a different seqno XXXX

MUPIP Error: Issued by the first source server started on a supplementary instance if the journal stream sequence numbers (for any non-supplementary stream from 0 through 15) stored in the instance file do not match those stored in the database file headers. This is possible if a database was recreated or refreshed from a backup on another instance without correspondingly recreating the instance file.

Action: If the database file is known to be accurate, recreate the instance file. If not, reinitialize this instance from a backup of some other instance in the same LMS Group (see Action section of REPLINSTDBMATCH error above for more details on this).

------------------------
REPLINSTFMT
------------------------

REPLINSTFMT, Format error encountered while reading replication instance file xxxx. Expected yyyy. Found zzzz.

Run Time/MUPIP Error: This error is issued by YottaDB or MUPIP whenever it tries to open the replication instance file and finds that it was created with a format that the current version of YottaDB cannot interpret. YottaDB also produces this error when it encounters:

- an instance file created on a different endian system or
- an instance file created by a 32-bit (or 64-bit) version of YottaDB that is different from the current 64-bit (or 32-bit) version of YottaDB.

Action: Recreate the instance file using the mupip replic -instance_create command with the current version of YottaDB.

Action: If the error is issued by YottaDB, review the accompanying message(s) in the operator log.

If a MUPIP SET -JOURNAL=ON command produces this message for the region in the operator log, it may indicate that one or more of the current generation journal files are damaged/missing and new journal files were created with no back pointers to the previous journal files. YottaDB recommends taking a database backup at the earliest convenience because a MUPIP RECOVER/ROLLBACK will not be able to go back past xxxx. If this message is for a specified region(s), consider switching the journal files for all regions (with REGION "*") that the process has opened (all journaled/replicated regions in the instance if replication is in use) to ensure that the RECOVER/ROLLBACK for other regions remains unaffected.

No action is required if the MUPIP BACKUP -NEWJNLFILES=NOPREVLINK issues the error.

---------------------
REPLINSTFREEZECOMMENT
---------------------

REPLINSTFREEZECOMMENT, Freeze Comment: xxxx

Run Time Information: This message contains details about a freeze on a replication instance. The instance information is included in an associated REPLINSTFROZEN message. In the case of an automatic freeze, xxxx identifies an error that triggered the freeze. In the case of an administrative freeze, xxxx contains the text provided by MUPIP REPLICATE -SOURCE -FREEZE=ON -COMMENT="xxxx".

Action: Refer to REPLINSTFROZEN below.

---------------------
REPLINSTFROZEN
---------------------

REPLINSTFROZEN, Instance xxxx is now Frozen

Run Time Error: This indicates that the replication instance xxxx is frozen due to a custom error or an out-of-space condition on a region with INST_FREEZE_ON_ERROR set, or by an administrator with the MUPIP REPLICATE -SOURCE -FREEZE=ON command. Updates to database files or shared memory for regions in the instance are blocked.

Action: Check the associated REPLINSTFREEZECOMMENT message for details on the cause of the freeze. For out-of-space conditions, make sufficient disk space available to remove the freeze. For custom errors or for administrative freezes, MUPIP REPLICATE -SOURCE -FREEZE=OFF or a system restart removes the freeze.

--------------------
REPLINSTMISMTCH
--------------------

REPLINSTMISMTCH, Process has replication instance file ffff (jnlpool shmid = ssss) open but database dddd is bound to instance file gggg (jnlpool shmid =tttt)

Run Time Error: The process attempted an update on the replicated database dddd associated with the replication instance file ffff and journal pool shared memory id ssss; however, the process has already associated the database with a different replication instance file gggg or journal pool shmid tttt.

Action: A replicated database can only accept updates by processes that have the same replication instance file (defined by the environment variable ydb_repl_instance or in the global directory) open for that database. Ensure the same replication instance file is used for all processes that update the same replicated database file. This error can also occur if the replication instance file was recreated (while processes were still accessing the replication instance). In this case, the name ffff and gggg would be the same but the corresponding journal pool shared memory ids would be different. To recover from this situation, shut down all processes accessing the instance from before and after the instance file recreate. Run an argumentless MUPIP RUNDOWN to clean up the older journal pool tttt and restart the instance. The Source Server (which is the first process to start on a replicated instance) only binds replicated databases from its global directory to the journal pool that it creates. No other replicated database file can be bound with this journal pool.

--------------------
REPLINSTNMLEN
--------------------

REPLINSTNMLEN, Replication instance name xxxx should be 1 to 15 characters long

MUPIP Error: This error is issued by the mupip replic instance_create command if the instance name was specified either through the name qualifier or through the environment variable ydb_repl_instname and if name was longer than 15 characters or was the empty string.

Action: Specify a valid instance name that is 1 to 15 characters long.

-------------------
REPLINSTNMSAME
-------------------

REPLINSTNMSAME, Primary and Secondary instances have the same replication instance name xxxx

MUPIP Error: This error is issued by any source server command where the -instsecondary qualifier specifies a secondary instance name that matches the name of the primary instance the command is started from.

Action: Two instances should never have the same name. Recreate the instance file on the secondary with a different name and restart the receiver server with the updateresync qualifier.

--------------------
REPLINSTNMUNDEF
--------------------

REPLINSTNMUNDEF, Replication instance name not defined

MUPIP Error: This error is issued by the mupip replic -instance_create command if the -name qualifier was not specified and if the environment variable ydb_repl_instname is not defined either.

Action: Specify the instance name using the -name qualifier.

------------------
REPLINSTNOHIST
------------------

REPLINSTNOHIST, History record for xxxx not found in replication instance file yyyy

MUPIP Error: The source server or receiver server issues this message as an error while mupip rollback issues this message as a warning when they scan the replication instance file looking for a history record corresponding to a journal sequence number that is lower than the earliest sequence number or greater than the latest sequence number stored in the instance file. This means that the replication instance files on the primary and secondary have differing levels of history detail (possible if the instance file was later recreated in one instance) and that it is no longer possible to determine the sync point (resync seqno) between the two instances.

Action: If mupip rollback issues this error, it truncates the replication instance file history. This means that if this instance is a secondary, it should be brought up with the -updateresync qualifier. If the source or receiver server issues this error, this error needs to be handled by ensuring the primary and secondary databases are in sync (by shipping a copy of the database from the primary to the secondary if not already done), recreating the instance file on the secondary (if not already done) and starting the receiver server on the secondary with the -updateresync qualifier.

-------------------
REPLINSTNOSHM
-------------------

REPLINSTNOSHM, Database dddd has no active connection to a replication journal pool

Run Time Error: The Source server was started with a replication instance that had this database file listed but later the source server and this particular database file were shut down while other database files in this instance file were still active.

Action: Restart the source server.

----------------------
REPLINSTOPEN
----------------------

REPLINSTOPEN, Error opening replication instance file xxxx

Run Time Error: There was an error when YottaDB or MUPIP tried to open the replication instance file. The error detail accompanies this message.

Action: Look at the accompanying error detail. Possible causes are file permissions, system quotas, and so on. Fix the cause if possible. If not, report the entire incident context to your YottaDB support channel for further analysis.

--------------------
REPLINSTREAD
--------------------

REPLINSTREAD, Error reading xxxx bytes at offset yyyy from replication instance file ffff

Run Time Error: There was an error when YottaDB or MUPIP tried to read from the replication instance file. The error detail accompanies this message.

Action: Look at the accompanying error detail. Possible causes are file permissions, system quotas, etc. Fix the cause if possible. If not, report the entire incident context to your YottaDB support channel for further analysis.

--------------------
REPLINSTSECLEN
--------------------

REPLINSTSECLEN, Secondary replication instance name xxxx should be 1 to 15 characters long

MUPIP Error: This error is issued by any mupip replic -source command that specifies a secondary instance name. This error is issued if the secondary instance name was specified either through the -instsecondary qualifier or through the environment variable ydb_repl_instsecondary and if the name was longer than 15 characters or was the empty string.

Action: Specify a valid secondary instance name that is 1 to 15 characters long.

----------------------
REPLINSTSECMTCH
----------------------

REPLINSTSECMTCH, Secondary replication instance name xxxx sent by receiver does not match yyyy specified at source server startup

Source Server log/MUPIP Error: This error is issued by a source server that connects to a receiver server on the secondary and finds that the secondary instance name sent by the receiver does not match the secondary instance name specified (INSTSECONDARY qualifier) when the source server was started. The source server terminates after issuing this error.

Action: Restart the source server with the correct -instsecondary qualifier value. Also make sure the instance name in the -instsecondary qualifier and the host/port information in the secondary qualifier of the source server startup command correspond to each other.

--------------------
REPLINSTSECNONE
--------------------

REPLINSTSECNONE, No information found for secondary instance xxxx in instance file yyyy

MUPIP Error: This error is issued by any mupip replic source command that specifies a replicating (secondary) instance name (except for the one which specifies -start) if no information on this name can be found in the instance file. This is possible if no Source Server was ever started since the initialization of this instance file for such a replicating instance.

Action: Make sure the replicating instance name is correct. If it is, make sure a Source Server for that replicating instance has been started at least once in the life of the instance file even if it is currently not up and running.

------------------
REPLINSTSECUNDF
------------------

REPLINSTSECUNDF, Secondary replication instance name not defined

MUPIP Error: This error is issued by any mupip replic -source command that requires a secondary instance name to be specified. The source server commands that require this qualifier are those that have any of -activate, changelog, deactivate, needrestart, start, statslog or stopsourcefilter specified. The secondary name can be specified either through the INSTSECONDARY qualifier or through the environment variable ydb_repl_instsecondary. If neither of them is specified, this error is issued.

Action: Specify the secondary instance name using the INSTSECONDARY qualifier.

------------------
REPLINSTSEQORD
------------------

REPLINSTSEQORD, ssss has seqno xxxx which is less than last record seqno yyyy in replication instance file zzzz

MUPIP Error: This error is issued in one of two scenarios. The instance file consists of a sequence of history records that should correspond to an increasing range of sequence numbers. They need to hence have their starting sequence number in increasing order. If an attempt is made to append a history record with a starting sequence number that is lower than the last history record currently existing in the instance file, the source or receiver server issues this error. In this case, ssss would be the string new history record. This error is also issued if at journal pool creation time, the source server notices that the instance file header has a value of the current seqno that is lower than the starting seqno of the last history record in the instance file. In this case ssss would be the string instance file header.

Action: If this instance is not the root primary, this error can be handled by restoring both the database and the instance file from a previous backup (consistent backup of the instance file AND database files taken together at the same time) and restarting the instance. Subsequent to such a restore, all transactions since the last backup will be sent across from this instance's primary. Alternatively, this can be handled by shipping a copy of the database from any other instance (either the primary or any other secondary/tertiary), recreating the instance file and starting this instance as a secondary with the UPDATERESYNC qualifier. In either case, this procedure has to be repeated on all tertiary instances etc. that descend from this instance ensuring that for every primary-secondary instance pair, the secondary is not ahead of the primary in terms of journal seqno. If this instance is the root primary, restoring from a prior backup may not be viable as it may mean loss of transactions that occurred after the backup. The alternative way to handle this error is to recreate the instance file on the root primary, ship a copy of the database from the primary and recreate instance files on ALL secondaries (tertiaries etc.) and restart the secondaries with the UPDATERESYNC qualifier. In addition, report the entire incident to your YottaDB support channel.

--------------------
REPLINSTSTNDALN
--------------------

REPLINSTSTNDALN, Could not get exclusive access to replication instance file xxxx

MUPIP Error: This error is issued by MUPIP REPLIC INSTANCE_CREATE if it finds that the replication instance file it is attempting to create already exists and is being used (the journal pool for that instance exists) by YottaDB and/or MUPIP process(es).

Action: Shutdown all YottaDB and/or MUPIP processes that are using the replication instance file and reissue the command. If it fails even though you know for sure that there is no other YottaDB or MUPIP process accessing the replication instance file, delete the instance file and reissue the command.

-------------------
REPLINSTUNDEF
-------------------

REPLINSTUNDEF, Replication instance environment variable $ydb_repl_instance is undefined

Run Time/MUPIP Error: This indicates that the replication instance environment variable $ydb_repl_instance is undefined.

Action: Define the environment variable to the appropriate instance file.

--------------------
REPLINSTUNFROZEN
--------------------

REPLINSTUNFROZEN, Instance xxxx is now Unfrozen

Run Time Information: This indicates that a replication instance which had previously been frozen is no longer frozen and updates to regions in the instance will resume.

Action: None required.

-------------------
REPLINSTWRITE
-------------------

REPLINSTWRITE, Error writing xxxx bytes at offset yyyy from replication instance file ffff

Run Time Error: There was an error when YottaDB or MUPIP tried to write to the replication instance file. The error detail accompanies this message.

Action: Look at the accompanying error detail. Possible causes are file permissions, system quotas, etc. Fix the cause if possible. If not, report the entire incident context to your YottaDB support channel.

---------------------
REPLJNLCLOSED
---------------------

REPLJNLCLOSED, Replication in jeopardy as journaling for database file ddd. Current region seqno is xxx[XXX] and system seqno is yyy[YYY]

Run Time Warning: This message indicates that YottaDB turned OFF journaling and switched replication from ON to WAS_ON in the specified database. Other preceding messages identify the cause (for example, a lack of disk space while writing to a journal file, a permissions issue while auto-switching to new journal files,and so on). The message also displays the region and journal sequence numbers. From this point, replicating updates on the primary to the secondary might or might not work depending on the backlog on the primary until replication/journaling gets turned back ON.

Action: First, correct the cause (lack of disk space, permission issues, and so on) that turned journaling OFF.

Execute the MUPIP SET REPLICATION=ON or MUPIP BACKUP REPLICATION=ON command to turn replication (and journaling) ON and switch to a new set of journal files. This command can work while processes are concurrently updating the database and causes YottaDB to journal subsequent updates in both the journal file and journal pool (rather than only in the journal pool as it does when replication is in the WAS_ON state).

Execute the MUPIP REPLIC -SOURCE -SHOWBACKLOG command. Note down the value of "sequence number of last transaction written to journal pool".

Execute the above command at regular intervals and note down the value of "sequence number of last transaction sent by source server."

If the "sequence number of last transaction sent by source server" is greater than "sequence number of last transaction written to journal pool", it means that the source server successfully sent all journal records during the time interval when journaling was turned OFF. In this case, no further action is required.

On the other hand, if the "sequence number of last transaction sent by source server" is less than "sequence number of last transaction written to journal pool" and reports the same value across repeated SHOWBACKLOG commands, then check the source server log file for any error messages - most likely a NOPREVLINK error from the source server. This means the source server could not locate the corresponding journal records required from the journal files to replicate a particular sequence number and therefore, it failed to synchronize the primary and secondary. In this case, take an online backup of the primary, restore it on the secondary and start the secondary with the UPDATERESYNC qualifier to synchronize the secondary with the primary.

---------------------
REPLJNLCNFLCT
---------------------

REPLJNLCNFLCT, Journaling cannot be turned nnnn on database file ffff as the replication state is rrrr and must also be turned nnnn in the same command

MUPIP Warning: This message indicates that the requested journaling state (nnnn) and current replication state (rrrr) do not match and the command must explicitly specify an outcome such that they do match.

Action: Using DSE, dump the file header for each affected region. If the replication state is "WAS_ON" please consult the section on "Recovering from the replication WAS_ON state" from the `Administration and Operations Guide <../AdminOpsGuide/index.html>`_. For all other cases, issue one or more commands that leave journaling and replication either both ON or both OFF.

-----------------------
REPLLOGOPN
-----------------------

REPLLOGOPN, Replication subsystem could not open log file LLLL : eeee. Logging done to OOOO

MUPIP Error: This indicates that MUPIP could not find or did not have the access permissions to open the log file LLLL, because of the error eeee. If there is another log file available (a previously opened file), MUPIP writes to the other log file OOOO. If there is no other log file available, MUPIP sends any remaining messages to /dev/null and terminates the replication server process.

Action: Check the log file permissions, and if the permissions are correct, move the log file and specify that MUPIP should log to a log file which has appropriate access permissions.

----------------------
REPLMULTINSTUPDATE
----------------------

REPLMULTINSTUPDATE, Previous updates in the current transaction are to xxxx so updates to yyyy (in rrrr) not allowed

Run Time Error: Previous updates in the current TP transaction mapped to database files associated with replication instance file xxxx, so it cannot make updates to database file yyyy which is associated with replication instance file rrrr.

Action: Modify the application so that all updates in a TP transaction to replicated regions are associated with a single replication instance.

------------------
REPLNOBEFORE
------------------

REPLNOBEFORE, NOBEFORE option cannot be used when the current replication state is ON for a database file xxxx

MUPIP Warning: This indicates that YottaDB could not use the NOBEFORE journal option because replication is already turned ON for the database file xxxx.

Action: Use the BEFORE option for the database file xxxx. NOBEFORE image journaling is not currently supported for replication. If NOBEFORE is necessary, use the -replication=OFF option.

-----------------
REPLNOHASHTREC
-----------------

REPLNOHASHTREC, Sequence number NNNN contains trigger definition updates. IIII side must be at least V6.2-000 for replication to continue

Receiver Server log/Source Server log Error: $ZTRIGGER() or MUPIP TRIGGER updated a trigger definition in a replicated region, but the replicating instance is running a version that cannot process trigger definitions from this version. Previously, trigger maintenance actions replicated as data, and now, trigger maintenance actions replicate as logical actions.

Action: Use V6.2-000 as a step on an upgrade path from earlier to later versions. Alternatively, if you need to perform trigger maintenance on replicating instances that are on either side of V6.2-000, you must perform them independently on the two systems when replication is off, recording and manipulating the sequence numbers such that you can resume replication.

----------------------
REPLNOTLS
----------------------

REPLNOTLS, xxxx requested TLS/SSL communication but the yyyy was either not started with TLSID qualifier or does not support TLS/SSL protocol

MUPIP Error: This indicates that xxxx (Source Side or Receiver Side) requested TLS/SSL communication but the other side was not started with a TLSID qualifier or was a previous version that does not support TLS/SSL protocol.

Action: If both sides are running with the latest version, then make sure the TLSID qualifier is specified for both the Source and Receiver Server startup commands. If one of the instances involved in the replication is a previous version, upgrade it to the latest version to support TLS.

-------------------
REPLNOTON
-------------------

REPLNOTON, Replication is not on for journal file xxxx, rollback will not continue

MUPIP Error: This indicates that ROLLBACK cannot proceed because MUPIP encountered xxxx, a journal file for which replication is not turned ON.

Action: ROLLBACK cannot be used for a journal file if it does not have replication state ON, use MUPIP JOURNAL RECOVER instead.

--------------------
REPLOFFJNLON
--------------------

REPLOFFJNLON, Replication state for database file <xxx> is OFF but journaling state is enabled.

MUPIP Error: In a replicated environment, this indicates that the database file <xxx> cannot have journaling ENABLED or ON when the replication state is OFF. This is an out-of-design situation due to implications on recovery as journal files can't be a mix of SET/KILL records that were created when replication was ON and those created when replication was OFF.

Action: In order to prevent this situation, enable replication state for the database file <xxx> (using MUPIP SET -REPLICATION=ON or MUPIP BACKUP -REPLICATION=ON) or disable journaling using MUPIP SET -JOURNAL=DISABLE whichever is desirable.

-------------------
REPLPOOLINST
-------------------

REPLPOOLINST, Error with replication pool xxxx for instance file yyyy

MUPIP Error: This indicates that MUPIP encountered an error for the replication shared memory of shared memory xxxx.

Action: Refer to the accompanied message(s) for detailed information.

----------------------
REPLRECFMT
----------------------

REPLRECFMT, Replication journal record format error encountered

Source Server log/MUPIP Fatal: This indicates that a formatting error has been encountered by the replication source server for a journal record.

Action: Report the entire incident context along with any YottaDB logs, dump, and/or core files created within the same timeframe to your YottaDB support channel.

----------------------
REPLREQROLLBACK
----------------------

REPLREQROLLBACK, Replication instance file xxxx indicates abnormal shutdown. Run MUPIP JOURNAL ROLLBACK first.

MUPIP Error: This error is issued by MUPIP REPLIC SOURCE START if it is about to create the journal pool and finds that the replication instance file header indicates that the journal pool was not previously shutdown cleanly. This may cause the instance file to not correspond to the database and/or journals.

Action: Run MUPIP JOURNAL ROLLBACK to clean up the instance file, database and journal files before starting a source server on this instance.

----------------------
REPLREQRUNDOWN
----------------------

REPLREQRUNDOWN, Error accessing replication instance xxxx. Must be rundown on cluster node yyyy.

Run Time/MUPIP Error: This indicates that YottaDB could not open the specified replication instance file because it was not properly closed on the cluster node yyyy.

Action: Issue the MUPIP RUNDOWN command on the cluster node. A YottaDB process or replication server on that node may have been terminated by a method other than MUPIP STOP. If MUPIP RUNDOWN with no parameters fails, try MUPIP RUNDOWN region * with an appropriate global directory.

------------------------
REPLSRCEXITERR
------------------------

REPLSRCEXITERR, Source server for secondary instance xxxx exited abnormally. See log file yyyy for details.

Operator log Warning: This indicates that the source server for instance xxxx exited abnormally.

Action: Check the end of the log file at yyyy for additional message(s).

--------------------------
REPLSTATE
--------------------------

REPLSTATE, Replication state for region/database file xxxx is now yyyy

MUPIP Information: This indicates that the replication state for region/database file xxxx is now yyyy.

Action: -

-------------------------
REPLSTATEERR
-------------------------

REPLSTATEERR, Replication state cannot be changed to the specified value for database file <xxx>.

MUPIP Error: This MUPIP BACKUP error indicates that the specified change in the replication state cannot be done due to the reason described in a following YDB-E-TEXT message.

Action: If the message indicates "Standalone access required", try to enable the replication in the standalone mode using MUPIP SET REPLICATION. If the message suggests switching journal files, specify the backup qualifier NEWJNL in the command line.

-----------------
REPLSTATEOFF
-----------------

REPLSTATEOFF, MUPIP JOURNAL -ROLLBACK -BACKWARD cannot proceed as database xxxx does not have replication ON

MUPIP Error: This indicates that a MUPIP JOURNAL -ROLLBACK -BACKWARD command cannot proceed because the specified database xxxx does not have replication state ON. In most situations, this error occurs when the journal file storage runs out of disk space.

Action: Ensure replication is turned ON for a database, before executing the MUPIP JOURNAL -ROLLBACK -BACKWARD command. If the database is in the WAS_ON state, refer to the "Recovering from the WAS_ON state" section in the `Database Replication chapter of the Administration and Operations Guide <../AdminOpsGuide/dbrepl.html>`_. Alternatively, if replication was not in use on the database, use MUPIP JOURNAL -RECOVER.

---------------------
REPLTRANS2BIG
---------------------

REPLTRANS2BIG, Transaction xxxx of size yyyy too large to be accommodated in the zzzz pool

MUPIP Error: This indicates that the size of the incoming transaction is larger than the specified receive pool.

Action: The receiver server must be shut down and restarted with a larger receive pool size or, if possible, break the file transmission into smaller files.

----------------------------
REPLWARN
----------------------------

REPLWARN, xxxx

MUPIP Warning: This indicates that YottaDB is performing tasks that may result in an error.

Action: Review accompanying messages for more information about why YottaDB generated this message. The Source Server exits with a REPLWARN message for the first five failed attempts to open journal files.

--------------------------
REPLXENDIANFAIL
--------------------------

REPLXENDIANFAIL, SSSS side encountered error while doing endian conversion at journal sequence number JJJJ

MUPIP Error: The originating or the replicating instance in a cross-endian replication environment report this error whenever they detect that the endian conversion failed.

Action: Restart replication - if the transmission caused the problem, it's probably intermittent. Perform a MUPIP JOURNAL -EXTRACT -DETAIL on the journal files and search for the sequence number JJJJ to look for anything different about that journal record. If the report is on the secondary, take a fresh backup on the originating instance requesting new journal files, refresh the replicating instance and restart replication.

----------------------
REQ2RESUME
----------------------

REQ2RESUME, Request to resume suspended processing received from process xxxx owned by userid yyyy

Run Time Information: The information is logged to the operator facility. This indicates that a suspended process received signal SIGCONT to resume processing. This happens when a process is suspended while holding a scarce resource in order to permit other processes to access the resource. On systems that do not support advanced signal information, xxxx and yyyy both are 0 (zero).

Action: -

---------------------------
REQDVIEWPARM
---------------------------

REQDVIEWPARM, Required View parameter is missing

Run Time Error: This indicates that the failed program attempted to use the VIEW function without specifying a keyword argument or with an invalid keyword argument.

Action: Programmers should consult the `Programmer's Guide <../ProgrammersGuide/index.html>`_ for the correct syntax.

---------------------------
REQRECOV
---------------------------

REQRECOV, Error accessing database dddd. Must be recovered on cluster node ccccc.

Run Time Error: This indicates that YottaDB could not open a previously journaled database file dddd due to a prior improper shutdown on cluster node ccccc. A YottaDB process on cluster node ccccc may have failed to attach a database memory segment or it was terminated by a method other than MUPIP STOP.

Action: Perform a MUPIP JOURNAL RECOVER operation to address this issue.

-------------------------
REQRLNKCTLRNDWN
-------------------------

REQRLNKCTLRNDWN, Error accessing relinkctl file rrrr for $ZROUTINES directory dddd. Must be rundown

Run Time Error: A process initiated an auto-relink action with ZLINK or ZRUPDATE, or an auto-relink check with DO, GOTO, ZBREAK, ZGOTO, ZPRINT or $TEXT() which required adding information for the routine in question to the Relinkctl file rrrr for directory dddd, but the shared memory associated with that Relinkctl file had been removed, presumably by an operator using a ipcrm.

Action: Run MUPIP RUNDOWN -RELINKCTL dddd to clear the state of the Relinkctl file. Determine the cause for the improper close and take action to prevent additional occurrences.

----------------------------
REQROLLBACK
----------------------------

REQROLLBACK, Error accessing database dddd. Run MUPIP JOURNAL -ROLLBACK -NOONLINE on cluster node cccc.

Run Time Error: This indicates that YottaDB could not open a previously replicated database file dddd due to a prior improper shutdown on cluster node cccc. A YottaDB process on cluster node ccccc may have failed to attach a database memory segment or it was terminated by a method other than MUPIP STOP.

Action: Perform MUPIP JOURNAL -ROLLBACK -NOONLINE to cleanup the instance file, database, and journal files before starting a source server on this instance.

--------------------
REQRUNDOWN
--------------------

REQRUNDOWN, Error accessing database dddd. Must be rundown on cluster node ccccc.

Run Time Error: This indicates that YottaDB could not open database file dddd due to a prior improper shutdown on cluster node ccccc. A YottaDB process on cluster node ccccc may have failed to attach a database memory segment or it was terminated by a method other than MUPIP STOP.

Action: Perform MUPIP RUNDOWN from cluster node ccccc. If MUPIP RUNDOWN with no parameters does not work, specify the region name or file-specification with -REGION or -FILE, respectively.

----------------------
RESOLVESEQNO
----------------------

RESOLVESEQNO, Resolving until sequence number dddd [0xxxxx]

MUPIP Information: This indicates that MUPIP JOURNAL ROLLBACK expects to do backward processing until it reaches sequence number with hexadecimal value xxxx (decimal value dddd). This is usually the common sequence number agreed upon between the primary and secondary by a -FETCHRESYNC rollback or the sequence number specified in a -RESYNC rollback.

Action: No action required.

-----------------------
RESOLVESEQSTRM
-----------------------

RESOLVESEQSTRM, Resolving until stream sequence number Stream nnnn : Seqno dddd xxxx

MUPIP Information: This indicates that MUPIP JOURNAL ROLLBACK expects to do backward processing until it reaches the stream sequence number whose hexadecimal value is xxxx (decimal value dddd). This is usually the common stream sequence number agreed upon between the primary and secondary by a -FETCHRESYNC rollback or the stream sequence number specified in a -RESYNC rollback where -RSYNC_STRM is also specified.

Action: No action required.

---------------------------
RESRCINTRLCKBYPAS
---------------------------

RESRCINTRLCKBYPAS, tttt with PID qqqq bypassing the ssss semaphore for region rrrr (ffff) currently held by PID pppp.

All YottaDB Components Information: YottaDB issues the RESRCINTRLCKBYPAS message to the system log as an indication it may not detect when the last process detaches from the shared resource and therefore may not rundown the database shared resources as it normally would. YottaDB protects the actions of setting up and tearing down the shared resources associated with a database with a pair of semaphores. Because DSE and LKE are tools for diagnosing issues, when they start and find they cannot acquire the semaphores after a reasonable number of tries, they proceed to open the database anyway because it is highly probable the database is already set up. When DSE and LKE bypass the semaphore acquisition, they leave the count of attached processes incorrect. When many processes terminate at the same time, typically because of a system shutdown, there can be significant contention for the semaphores that can cause their terminations to take an unusually long time. When this happens, and the count of remaining attached processes is significant, a process may skip the semaphore acquisition, again leaving the count of attached process incorrect. If either of these events occurs, YottaDB issues the RESRCINTRLCKBYPAS message where tttt identifies the process type: "LKE", "DSE" or "YottaDB"; qqqq is the bypassing process's PID; ssss identifies the semaphore type: "FTOK" or "access control"; rrrr is the region bypassed; ffff is the file corresponding to region rrrr; pppp is the PID of the process holding the semaphore.

Action: These messages when shutting down YottaDB activity may indicate a need to complete the process by invoking a MUPIP JOURNAL -ROLLBACK -BACKWARD for replicated databases, a MUPIP JOURNAL -RECOVER -BACKWARD for unreplicated journaled databases and a MUPIP RUNDOWN for journal-free databases to get the database to a safe state; doing so as part of every shutdown is good practice.

---------------------
RESRCWAIT
---------------------

RESRCWAIT, Waiting briefly for the tttt semaphore for region rrrr (ffff) was held by PID pppp (Sem. ID: ssss)

Run Time Information: A process started a three (3) second wait for an FTOK or access control semaphore. If the process with PID pppp does not release the semaphore before the timeout expires, the waiting process bypasses acquiring the semaphore. tttt identifies the semaphore type: "FTOK" or "access control"; rrrr is the region; ffff is the database file corresponding to region rrrr; ssss is the semaphore ID.

Action: None required.

---------------------
RESTRICTEDOP
---------------------

RESTRICTEDOP, Attempt to perform a restricted operation: xxxx

All YottaDB Components Error: The attempted operation, xxxx, was prevented based on the policy specified by the $ydb_dist/restrict.txt file.

Action: Check the permissions and contents of the restrict.txt file against the permissions of the user performing the operation.

-------------------
RESTRICTSYNTAX
-------------------

RESTRICTSYNTAX, Syntax error in file ffff at line number nnnn. All facilities restricted for process.

All YottaDB Components Error: The file ffff, or $ydb_dist/restrict.txt, contains a syntax error at line nnnn. All facilities which may be specified in a restrict.txt file will be considered restricted, and restricted operations will result in RESTRICTEDOP errors or operations being ignored.

Action: Edit the restrict.txt file to remove the syntax error, and verify that the permissions of the file reflect the desired access.

------------------
RESUMESTRMNUM
------------------

RESUMESTRMNUM, Error with stream number specified in RESUME qualifier

MUPIP Error: Issued by a Receiver Server as an accompanying message to a UPDSYNCINSTFILE error. The stream number nnnn can be any integer value from -1 through 15.

Action: No action required for this message. Action is required for the preceding UPDSYNCINSTFILE error.

--------------------------
RESYNCSEQLOW
--------------------------

RESYNCSEQLOW, MUPIP JOURNAL -ROLLBACK -FORWARD -RESYNC=NNNN [0xXXXX] requested is lower than LLLL which is the starting sequence number for the instance

MUPIP Error: The MUPIP JOURNAL -ROLLBACK -FORWARD command has a -RESYNC qualifier value (NNNN in decimal or XXXX in hexidecimal) lower than that (LLLL) of the instance to the target instance.

Action: Reissue the command after adjusting the resync value and/or adopting a different target instance.

-----------------------
REUSEINSTNAME
-----------------------

REUSEINSTNAME, Error with instance name specified in REUSE qualifier

MUPIP Error: Issued by a Receiver Server when started with the -REUSE qualifier in case of either inappropriate use of this qualifier or an inappropriate instance name specified as a value to this qualifier.

Action: An accompanying YDB-I-TEXT message describes the particular error situation. Take appropriate corrective action based on that.

----------------------
RHMISSING
----------------------

RHMISSING, Right-hand side of expression expected

Compile Time Error: This indicates that a binary operator did not specify a corresponding right-hand value.

Action: Look for missing or invalid expressions.

--------------------
RLBKJNLNOBIMG
--------------------

RLBKJNLNOBIMG, Journal file jjjj has NOBEFORE_IMAGE journaling.

MUPIP Information: MUPIP JOURNAL ROLLBACK displays this informational message whenever it finds journal file jjjj with NOBEFORE_IMAGE journaling (DSE DUMP -FILE for the corresponding database reports "Journal Before-imaging" as FALSE). As there are no before-image records in this journal file, MUPIP JOURNAL ROLLBACK does not roll back the database. Instead, it only generates a lost-transaction file.

Action: No user action required except to confirm that this type of rollback is appropriate and expected.

------------------
RLBKJNSEQ
------------------

RLBKJNSEQ, Journal seqno of the instance after rollback is xxxx[yyyy]

MUPIP Information: This indicates that the journal sequence number of the instance after MUPIP JOURNAL ROLLBACK command is xxxx.

Action: -

----------------
RLBKLOSTTNONLY
----------------

RLBKLOSTTNONLY, ROLLBACK will only create a lost transaction file (database and journal files will not be modified)

MUPIP Information: MUPIP JOURNAL -ROLLBACK displays this informational message at startup if it finds at least one database region with NOBEFORE_IMAGE journaling. In such a case, MUPIP JOURNAL -ROLLBACK can only create broken and lost transaction files (if appropriate) but otherwise not modify the database, journal files, or replication instance files.

Action: No user action required except to confirm that this type of rollback is appropriate and expected.

-----------------
RLBKNOBIMG
-----------------

RLBKNOBIMG, ROLLBACK cannot proceed as database dddd has NOBEFORE_IMAGE journaling

MUPIP Error: Rollback relies on BEFORE_IMAGE journaling and dddd did not have it turned on, so no rollback is currently possible.

Action: Restore the database from a backup and use forward recovery or a replication resynchronization to recover the database state. Use BEFORE_IMAGE journaling for databases you wish to be able to rollback.

-------------------
RLBKSTRMSEQ
-------------------

RLBKSTRMSEQ, Stream journal seqno of the instance after rollback is Stream nnnn : Seqno dddd xxxx

MUPIP Information: On a Supplementary Instance, MUPIP JOURNAL -ROLLBACK issues this message for each stream (from 0 through 15) that has at least one update. This message indicates how many updates in each stream this Supplementary Instance has processed.

Action: No action required.

-------------------
RLNKCTLRNDWNFL
-------------------

RLNKCTLRNDWNFL, Relinkctl file for $ZROUTINES directory dddd failed to rundown as it is open by nnnn process(es)

MUPIP Error: MUPIP RUNDOWN -RELINKCTL attempted to rundown the Relinkctl file for directory dddd, but did not because there were nnnn processes still using it.

Action: This may be expected, particularly if the command has no directory argument specified. Target MUPIP RUNDOWN -RELINKCTL to directories that are not currently in use, which under some circumstances can require stopping processes.

--------------------
RLNKCTLRNDWNSUC
--------------------

RLNKCTLRNDWNSUC, Relinkctl file for $ZROUTINES directory dddd successfully rundown

MUPIP Information: MUPIP RUNDOWN -RELINKCTL successfully ensured the auto-relink Relinkctl file for directory dddd had a quiescent state.

Action: None required

-------------------
RLNKRECLATCH
-------------------

RLNKRECLATCH, Failed to get latch on relinkctl record for routine name rrrr in $ZROUTINES directory dddd

Error: The process attempted to auto-relink or ZLINK routine rrrr from directory dddd but was unable to acquire the resource that ensures auto-relink state information for the routine is consistent.

Action: Report the entire incident context to your YottaDB support channel.

----------------------
RLNKSHMLATCH
----------------------

RLNKSHMLATCH, Failed to get latch on relinkctl shared memory for $ZROUTINES directory dddd

Run Time Error: The process attempted to access auto-relink information for directory dddd but an interlocked operation failed in the shared memory segment corresponding to the relinkctl file for directory dddd.

Action: Report the entire incident context to your YottaDB support channel.

----------------------
RMBIGSHARE
----------------------

RMBIGSHARE, File with BIGRECORD specified may only be SHARED if READONLY

Run Time Error: An OPEN command specified BIGRECORD and SHARED without also specifying READONLY. BIGRECORD files may only be shared if all uses are READONLY.

Action: If the file will only be read, add READONLY to the OPEN. If the file is to be written, remove the SHARED.

-------------------------
RMNOBIGRECORD
-------------------------

RMNOBIGRECORD, File record size requires BIGRECORD parameter

Run Time Error: The RECORDSIZE specified is larger than 32767. The BIGRECORD parameter must be specified before a RECORDSIZE larger than 32767.

Action: Modify the OPEN to specify BIGRECORD before RECORDSIZE.

------------------------
RMWIDTHPOS
------------------------

RMWIDTHPOS, File record size or width must be greater than zero

Run Time Error: This indicates that the WIDTH deviceparameter specified a negative argument.

Action: Modify the routine to ensure a positive WIDTH.

-------------------
RMWIDTHTOOBIG
-------------------

RMWIDTHTOOBIG, File record size too big

Run Time Error: The RECORDSIZE specified is too large. For disk files, the maximum is one megabyte.

Action: Modify the OPEN to use a smaller RECORDSIZE.

---------------------
RNDWNSEMFAIL
---------------------

RNDWNSEMFAIL, Attempting to acquire gds_rundown semaphore when it is already owned

Run Time Error: This indicates a logic error in YottaDB.

Action: Report the entire incident context to your YottaDB support channel.

----------------------
RNDWNSTATSDBFAIL
----------------------

RNDWNSTATSDBFAIL, Rundown of statistics database region RRRR (DB DDDD) failed at/in LLLL with following error: EEEE

All YottaDB Components Error: This indicates that YottaDB was unable to close out the shared resources associated with region RRRR (database file DDDD) becaus of the error EEEE, which it received at location LLLL

Action: Address the reason for the failure and retry.

----------------------
ROLLBKINTERRUPT
----------------------

ROLLBKINTERRUPT, Database file xxxx indicates interrupted ROLLBACK. Reissue the MUPIP JOURNAL ROLLBACK command.

MUPIP Error: This indicates that ROLLBACK has been interrupted on xxxx database file. Even if a MUPIP JOURNAL RECOVER command is issued instead of MUPIP JOURNAL ROLLBACK, a previous MUPIP JOURNAL ROLLBACK command was terminated abnormally. Note that, when ROLLBACK is interrupted, only ROLLBACK can be used to fix the interrupted operation.

Action: Reissue the MUPIP JOURNAL ROLLBACK command.

--------------------
ROUTINEUNKNOWN
--------------------

ROUTINEUNKNOWN, Routine could not be found

Run Time Error: This indicates that a command (such as DO or JOB) or a $TEXT function referred to a routine that is not in the running image.

Action: Look for unresolved reference warnings on the LINK that created the image. This error occurs on a JOB command when the routine is not in the image or available for ZLINKing, or when an auto-ZLINK finds that the routine reference has been damaged by an incomplete LINK.

------------------
RPAREN
------------------

RPAREN, List must end with right parenthesis or continue with comma

YottaDB/GDE Error: This indicates that a qualifier that accepts a list of arguments had an improper list format.

Action: Modify the command so that the list is enclosed in parentheses () and separated by commas (,).

-------------------
RPARENMISSING
-------------------

RPARENMISSING, Right parenthesis expected

Compile Time Error: This indicates that an expression, function, or subscripted variable contained a left parenthesis and no matching right parenthesis.

Action: Look for and correct any typographical errors.

------------------
RPARENREQD
------------------

RPARENREQD, xxxx Right parenthesis expected

MUPIP Error: This indicates that LOAD failed because it encountered xxxx in its input stream when it expected to find a right parenthesis.

Action: Refer to the topic MUPIP LOAD Errors in the `About This Manual section in this manual <./about.html>`_.

----------------
RSVDBYTE2HIGH
----------------

RSVDBYTE2HIGH, Record size ssss is greater than the maximum allowed for region rrrr with Block size bbbb and Reserved bytes cccc

Run Time Error: The attempted database update would result in a record size that is greater than what is allowed by the current database block size and reserved byte setting.

Action: If the Reserved Bytes setting for the database region identified is non-zero, try reducing it to allow this update. Otherwise, modify the update to reduce the resulting record size.

------------------
RSYNCSTRMSUPPLONLY
------------------

RSYNCSTRMSUPPLONLY, RSYNC_STRM qualifier only supported for Supplementary Instances

MUPIP Error: Issued by MUPIP JOURNAL -ROLLBACK indicating the -RSYNC_STRM qualifier only applies to Supplementary Instances - Business Continuity instances require comprehensive synchronization.

Action: Reissue the command without the -RSYNC_STRM qualifier.

-------------------
RSYNCSTRMVAL
-------------------

RSYNCSTRMVAL, RSYNC_STRM qualifier can only take on a value from 0 to 15

MUPIP Error: Issued by a MUPIP JOURNAL -ROLLBACK -RESYNC command which also specifies -RSYNC_STRM with a stream number outside of the range of 0 through 15.

Action: Specify a stream number within the allowed range.

--------------------
RTNNAME
--------------------

RTNNAME, Routine name expected here

Compile Time Error: This indicates that an entry reference specified a circumflex without a valid routine name.

Action: Look for a missing routine name in commands such as DO, GOTO, and JOB.

-------------------
RTSLOC
-------------------

RTSLOC, At M source location xxxx

Run Time Information: YottaDB uses this message to display the line, offset, and routine where it encountered a run-time error.

Action: Review the accompanying message(s) for additional information.

----------------
RUNPARAMERR
----------------

RUNPARAMERR, Error accessing parameter for run command

Run Time Error: This indicates that a yottadb -RUN command had a missing or invalid argument.

Action: Ensure that a yottadb -RUN has an argument that specifies a valid entryref ([label]^routinename).

-----------------
RWARG
-----------------

RWARG, This is not a legal argument for a READ command

Compile Time Error: This indicates that a READ command specified an invalid argument.

Action: Look for a non-alphanumeric character in the READ argument.

----------------
RWFORMAT
----------------

RWFORMAT, A valid format expression (!!, #, or ?expr) expected here

Compile Time Error: This indicates that a READ or WRITE command specified a format with invalid trailing characters.

Action: Look for and correct any typographical errors.

---------------
SCNDDBNOUPD
---------------

SCNDDBNOUPD, Database updates not allowed on the secondary

Run Time Error: This indicates that updates on secondary are currently not allowed as they may lead to inconsistency between primary and secondary.

Action: If you need to do an implicit database update on the secondary, contact the group responsible for maintaining database integrity at your operation.

-------------------
SDSEEKERR
-------------------

SDSEEKERR, Sequential device seek error

Run Time Error: This indicates that a YottaDB process encountered an error using the SEEK deviceparameter for an OPEN or USE on a sequential disk device. A supplementary TEXT message provides more details about the cause of the error.

Action: Analyze the accompanying message and appropriately adjust the SEEK deviceparameter or its value.

---------------------
SECNOTSUPPLEMENTARY
---------------------

SECNOTSUPPLEMENTARY, ssss is a Supplementary Instance and so cannot act as a source to non-Supplementary Instance iiii

Source Server log/MUPIP Error: Issued by a Source Server on a Supplementary Instance. ssss attempted to connect to a Replicating Instance iiii, but found that iiii is not configured as a Supplementary Instance.

Action: Reconfigure the instances to a supported configuration.

---------------------
SECONDAHEAD
---------------------

SECONDAHEAD, Secondary ahead of Primary: Secondary db possibly updated by process other than Update process. Do rollback first.

Run Time Error: The update process issues this error on finding that the Secondary database contains more updates than the Primary.

Action: If you allow database updates on Secondary, no action is needed. If not, investigate the cause. Make sure the database on secondary and primary are consistent.

---------------------
SEFCTNEEDSFULLB
---------------------

SEFCTNEEDSFULLB, Current side effect setting does not permit full Boolean to be turned off

Run Time Error: A VIEW "NOFULL_BOOLEAN" cannot enable YottaDB short-circuit Boolean compilation for a process running with a ydb_side_effects setting of 1 or 2.

Action: Keeping in mind that ydb_boolean and ydb_side_effects affect compilation behavior, and that ydb_boolean must be 1 or 2 (Standard Boolean mode) for ydb_side_effects setting 1 or 2 (Standard side effects), choose appropriate compilation modes. Note that once you choose the modes for your application you would typically not change them except to get warnings and modify the application to be somewhat more efficient.

---------------------
SEGIS
---------------------

SEGIS, in xxxx segment yyyy

GDE/DSE Information: This message displays the name xxxx of the SEGMENT with which you are working.

Action: Review the accompanying message(s) for additional information.

---------------------
SELECTFALSE
---------------------

SELECTFALSE, No argument to $SELECT was true

Run Time Error: This indicates that a $SELECT function did not specify any truth value expressions that evaluated to true.

Action: Modify the $SELECT(). The common technique is to end the selection list with-1:expr)-where expr is some default value and the integer constant 1 is always true.

----------------------
SELECTSYNTAX
----------------------

SELECTSYNTAX, Argument to xxxx clause is not valid

MUPIP Error: This indicates that EXTRACT encountered a qualifier with an invalid value and aborted.

Action: Review the proper syntax for EXTRACT with the qualifier SELECT. Refer to the `Administration and Operations Guide <../AdminOpsGuide/index.html>`_ or the online help for the MUPIP EXTRACT command.

--------------------
SEMID
--------------------

SEMID, Semaphore id nnnn

MUPIP Information: This message reports additional information for an associated error which had trouble with the semaphore with id nnnn.

Action: Check "ipcs -s" for the given id, and see the associated error.

---------------------
SEMKEYINUSE
---------------------

SEMKEYINUSE, Semaphore key xxxx is already in use (possibly by an older version)

Run Time/MUPIP Error: This indicates that YottaDB failed to create a semaphore. One possible cause is that a semaphore of ID xxxx already exists in the system and the number of semaphores in that semaphore set is different than the segment has attempted to create. If a new YottaDB version increases the number of semaphores in the semaphore set, collision with an older YottaDB version can cause this error.

Action: Check accompanying message(s) for additional information. Use the semstat2 tool to find detailed information about the existing semaphores in the system.

-------------------
SEMREMOVED
-------------------

SEMREMOVED, Semaphore id xxxx removed from the system

MUPIP Information: This indicates that the orphaned semaphore with ID xxxx is removed; because it contained the signature of a YottaDB semaphore, and no process was currently using it.

Action: -

-----------------
SEMWT2LONG
-----------------

SEMWT2LONG, Process wwww waited ssss second(s) for the llll lock for region rrrr, lock held by pid pppp

Run Time Error: This indicates that the process pppp appears to be holding the llll control semaphore for region rrrr for longer than YottaDB expects.

Action: Analyze the behavior of process pppp, and terminate it if appropriate. This error may indicate that the system is under-configured for the workload.

-------------------
SERVERERR
-------------------

SERVERERR, Severe error on server: xxxx

GT.CM Error: This indicates that the GT.CM Server encountered a fatal error and terminated.

Action: Review subsequent message(s) for more information. If necessary, report the entire incident context to your YottaDB support channel.

--------------------
SETECODE
--------------------

SETECODE, Non-empty value assigned to $ECODE (user-defined error trap)

Run Time Error: This indicates that an error trap occured because $ECODE got altered to a non-null string in an M routine.

Action: Make sure that either $ETRAP or $ZTRAP is set to the valid value, if the error needs to be handled by YottaDB.

---------------------
SETENVFAIL
---------------------

SETENVFAIL, VIEW "SETENV":"eeee" failed in setenv() system call

Run Time Error: This indicates that a setenv() system call failed for the environment variable named eeee.

Action: Examine the accompanying SYSCALL error message which has more detail on the error returned by the setenv() call.

-------------------
SETEXTRENV
-------------------

SETEXTRENV, Database files are missing or Instance is frozen; supply the database files, wait for the freeze to lift or define ydb_extract_nocol to extract possibly incorrect collation

MUPIP Error: It indicates that the ydb_extract_nocol environment variable needs to be defined to run MUPIP JOURNAL EXTRACT to completion if the Instance is Frozen or if Database files are missing.

Action: If you know that there are no variables with alternate collation or if the EXTRACT is for analysis rather than a LOAD, define ydb_extract_nocol to be a positive value and reissue the command. Otherwise correct the condition before reissuing the EXTRACT.

----------------------
SETINSETTRIGONLY
----------------------

SETINSETTRIGONLY, ISV iiii can only be modified in a 'SET' type trigger

Run Time Error: Code that was invoked for a trigger other than SET (such as KILL or ZTRIGGER) attempted to modify Intrinsic Special Variable iiii, which applies only to a SET trigger context.

Action: Review the trigger definition and correct the types or the code to avoid the issue.

------------------
SETINTRIGONLY
------------------

SETINTRIGONLY, ISV iiii cannot be modified outside of the trigger environment

Trigger/Run Time Error: The Intrinsic Special variable iiii can only be SET within the context of trigger logic ($ZTLEVEL > 0)

Action: Examine the application logic to determine whether code intended for use in a trigger context falls in an execution path outside of trigger logic. For code intended to execute both inside and outside triggers, use a postcondition that limits the SET to be within a trigger.

-----------------
SETITIMERFAILED
-----------------

SETITIMERFAILED, A setitimer() call returned an error status of ssss

Run Time Fatal: The above error is issued when YottaDB fails to schedule or stop a system timer using the setitimer() system call.

Action: Verify the normal state of the OS kernel. Report the entire incident context to your YottaDB support channel along with any YottaDB operator log messages within the same time frame.

-----------------
SETQUALPROB
-----------------

SETQUALPROB, Error getting qqqq qualifier value

MUPIP Error: The utility was unable to parse the command input to successfully determine the value supplied for the qqqq qualifier

Action: Examine the command and correct the value

-----------------
SETREG2RESYNC
-----------------

SETREG2RESYNC, Setting resync sequence number xxxx to region sequence number yyyy for database zzzz

MUPIP Information: This displays that the resync sequence number xxxx is being set to a region sequence number yyyy for database zzzz, because the journal file had crash field set and update was disabled.

Action: -

-----------------
SETSOCKOPTERR
-----------------

SETSOCKOPTERR, Setting the socket attribute xxxx failed: (errno == aaaa) yyyy

Run Time Error: This indicates that an attempt to modify the xxxx socket attribute failed for the reason described by yyyy.

Action: Review the message(s) and take appropriate action.

--------------------
SETZDIR
--------------------

SETZDIR, Cannot change working directory to xxxx.

Run Time Error: This indicates that there is an invalid directory specified in the SET $ZDIR=<xxxx> command. The accompanying message indicates the exact cause of the failure.

Action: Make sure the specified argument conforms to the syntax of a directory specification on the host operating system. Check for the existence of the directory and the access control permissions associated with the directory.

------------------------
SHMPLRECOV
------------------------

SHMPLRECOV, Shared memory pool block recovery invoked for region xxxx

Run Time Information: YottaDB carves out a portion of shared memory/global section allocated for each database region to use for ONLINE BACKUP - this portion is called the "shared memory pool". In the unlikely event of corruption of the shared memory pool, or if the blocks are "lost" due to stopped/killed or failed processes, YottaDB detects the corruption or lost blocks and runs a recovery procedure to fix these errors. Such an occurrence is logged in the operator log (syslog on UNIX) with SHMLRECOV message.

Action: Report the occurrence to your YottaDB support channel. No user action required. YottaDB will continue to operate normally.

-------------------
SHMREMOVED
-------------------

SHMREMOVED, Removed Shared Memory id mmmm corresponding to file ffff

MUPIP Information: MUPIP RUNDOWN removed shared memory segment mmmm, corresponding to the file ffff, which could be a database file or a replication instance file with a YottaDB signature because the resource was not actively in use.

Action: No action required.

--------------------
SHRMEMEXHAUSTED
--------------------

SHRMEMEXHAUSTED, Attempt by process to use more shared memory than currently permitted

Run Time Error: An out-of-memory error was encountered while trying to open a shared global section for a database file.

Action: Reduce the shared global section usage by reducing the number of global buffers, or database block size, or the number of database files that the process tries to open.

-------------------
SIDEEFFECTEVAL
-------------------

SIDEEFFECTEVAL, Extrinsic ($$), External call ($&) or $INCREMENT() with potential side effects in actuallist, function arguments, non-Boolean binary operands or subscripts

Compile Time Warning: A side effect expression appeared to the right of a global or local variable (glvn) in an order within an outer expression where the side effect might modify the glvn. Setting the ydb_side_effects environment variable to 2 (two) activates this check.

Action: Analyze the effect(s) of the side effect expressions, which are $INCREMENT(), extrinsics ($$),or external calls ($& or $ZCALL()) as to whether they modify a glvn earlier in the expression. If they do, the setting of ydb_side_effects modifies the behavior and you either need to modify the code to eliminate the side effect interaction or be sure to select the behavior you desire.

----------------------
SIGACCERR
----------------------

SIGACCERR, Signal was caused by invalid permissions for mapped object

Run Time Error: This message is an auxiliary message and is preceded by a primary KILLBYSIGxxx message.

Action: Refer to the accompanying message(s) and take appropriate action. Refer to the user documentation. If necessary, report the entire incident context to your YottaDB support channel for further analysis.

.. _sigacktimeout:

----------------------
SIGACKTIMEOUT
----------------------

SIGACKTIMEOUT, Signal completion acknowledgement timeout: xxxx

Run Time Error (Go wrapper specific): When YottaDB waits more than :code:`MaximumSigAckWait` seconds for an application signal handler to notify the YottaDB Go wrapper on :code:`ackChan` that it has completed its work, the process exits without waiting for the handler to complete. See `Go Using Signals <../MultiLangProgGuide/goprogram.html#go-using-signals>`_ for more information.

Action: If the exiting process was not the last process accessing any file in the database, no action is needed. If the exiting process was the last, verify and restore database structural integrity. In either case, investigate why the signal handler took a long time.

----------------------
SIGADRALN
----------------------

SIGADRALN, Signal was caused by invalid address alignment

Run Time Error: This message is an auxiliary message and is preceded by a primary KILLBYSIGxxx message.

Action: Refer to the accompanying message(s) and take appropriate action. Refer to the user documentation. If necessary, report the entire incident context to your YottaDB support channel for further analysis.

-----------------------
SIGADRERR
-----------------------

SIGADRERR, Signal was caused by non-existent physical address

Run Time Error: This message is an auxiliary message and is preceded by a primary KILLBYSIGxxx message.

Action: Refer to the accompanying message(s) and take appropriate action. Refer to the user documentation. If necessary, report the entire incident context to your YottaDB support channel for further analysis.

--------------------
SIGBADSTK
--------------------

SIGBADSTK, Signal was caused by an internal stack error

Run Time Error: This message is an auxiliary message and is preceded by a primary KILLBYSIGxxx message.

Action: Refer to the accompanying message(s) and take appropriate action. Refer to the user documentation. If necessary, report the entire incident context to your YottaDB support channel for further analysis.

----------------------
SIGCOPROC
----------------------

SIGCOPROC, Signal was caused by a coprocessor error

Run Time Error: This message is an auxiliary message and is preceded by a primary KILLBYSIGxxx message.

Action: Refer to the accompanying message(s) and take appropriate action. Refer to the user documentation. If necessary, report the entire incident context to your YottaDB support channel for further analysis.

---------------------
SIGFLTDIV
---------------------

SIGFLTDIV, Signal was caused by a floating point divided by zero

Run Time Error: This message is an auxiliary message and is preceded by a primary KILLBYSIGxxx message.

Action: Refer to the accompanying message(s) and take appropriate action. Refer to the user documentation. If necessary, report the entire incident context to your YottaDB support channel for further analysis.

-------------------------
SIGFLTINV
-------------------------

SIGFLTINV, Signal was caused by an invalid floating point operation

Run Time Error: This message is an auxiliary message and is preceded by a primary KILLBYSIGxxx message.

Action: Refer to the accompanying message(s) and take appropriate action. Refer to the user documentation. If necessary, report the entire incident context to your YottaDB support channel for further analysis.

------------------------
SIGFLTOVF
------------------------

SIGFLTOVF, Signal was caused by a floating point overflow

Run Time Error: This message is an auxiliary message and is preceded by a primary KILLBYSIGxxx message.

Action: Refer to the accompanying message(s) and take appropriate action. Refer to the user documentation. If necessary, report the entire incident context to your YottaDB support channel for further analysis.

------------------
SIGFLTRES
------------------

SIGFLTRES, Signal was caused by a floating point inexact result

Run Time Error: This message is an auxiliary message and is preceded by a primary KILLBYSIGxxx message.

Action: Refer to the accompanying message(s) and take appropriate action. Refer to the user documentation. If necessary, report the entire incident context to your YottaDB support channel for further analysis.

--------------
SIGFLTUND
--------------

SIGFLTUND, Signal was caused by a floating point underflow

Run Time Error: This message is an auxiliary message and is preceded by a primary KILLBYSIGxxx message.

Action: Refer to the accompanying message(s) and take appropriate action. Refer to the user documentation. If necessary, report the entire incident context to your YottaDB support channel for further analysis.

.. _siggortntimeout:

------------------
SIGGORTNTIMEOUT
------------------

SIGGORTNTIMEOUT, Shutdown of signal goroutines timed out

Run Time Warning (Go wrapper specific): When YottaDB waits more than :code:`MaximumSigShutDownWait` seconds for goroutines to terminate, the process exits without waiting for the goroutines to complete. See `Go Using Signals <../MultiLangProgGuide/goprogram.html#go-using-signals>`_ for more information.

Action: If the exiting process was not the last process accessing any file in the database, no action is needed. If the exiting process was the last, verify and restore database structural integrity. In either case, investigate why the goroutines took a long time to shut terminate.

------------------
SIGILLADR
------------------

SIGILLADR, Signal was caused by illegal addressing mode

Run Time Error: This message is an auxiliary message and is preceded by a primary KILLBYSIGxxx message.

Action: Refer to the accompanying message(s) and take appropriate action. Refer to the user documentation. If necessary, report the entire incident context to your YottaDB support channel for further analysis.

-----------------
SIGILLOPC
-----------------

SIGILLOPC, Signal was caused by an illegal opcode

Run Time Error: This message is an auxiliary message and is preceded by a primary KILLBYSIGxxx message.

Action: Refer to the accompanying message(s) and take appropriate action. Refer to the user documentation. If necessary, report the entire incident context to your YottaDB support channel for further analysis.

-----------------
SIGILLOPN
-----------------

SIGILLOPN, Signal was caused by an illegal operand

Run Time Error: This message is an auxiliary message and is preceded by a primary KILLBYSIGxxx message.

Action: Refer to the accompanying message(s) and take appropriate action. Refer to the user documentation. If necessary, report the entire incident context to your YottaDB support channel for further analysis.

------------------
SIGILLTRP
------------------

SIGILLTRP, Signal was caused by an illegal trap

Run Time Error: This message is an auxiliary message and is preceded by a primary KILLBYSIGxxx message.

Action: Refer to the accompanying message(s) and take appropriate action. Refer to the user documentation. If necessary, report the entire incident context to your YottaDB support channel for further analysis.

-----------------
SIGINTDIV
-----------------

SIGINTDIV, Signal was caused by an integer divided by zero

Run Time Error: This message is an auxiliary message and is preceded by a primary KILLBYSIGxxx message.

Action: Refer to the accompanying message(s) and take appropriate action. Refer to the user documentation. If necessary, report the entire incident context to your YottaDB support channel for further analysis.

--------------------
SIGINTOVF
--------------------

SIGINTOVF, Signal was caused by an integer overflow

Run Time Error: This message is an auxiliary message and is preceded by a primary KILLBYSIGxxx message.

Action: Refer to the accompanying message(s) and take appropriate action. Refer to the user documentation. If necessary, report the entire incident context to your YottaDB support channel for further analysis.

--------------------
SIGMAPERR
--------------------

SIGMAPERR, Signal was caused by an address not mapped to an object

Run Time Error: This message is an auxiliary message and is preceded by a primary KILLBYSIGxxx message.

Action: Refer to the accompanying message(s) and take appropriate action. Refer to the user documentation. If necessary, report the entire incident context to your YottaDB support channel for further analysis.

--------------------
SIGOBJERR
--------------------

SIGOBJERR, Signal was caused by an object specific hardware error

Run Time Error: This message is an auxiliary message and is preceded by a primary KILLBYSIGxxx message.

Action: Refer to the accompanying message(s) and take appropriate action. Refer to the user documentation. If necessary, report the entire incident context to your YottaDB support channel for further analysis.

--------------------
SIGPRVOPC
--------------------

SIGPRVOPC, Signal was caused by a privileged opcode

Run Time Error: This message is an auxiliary message and is preceded by a primary KILLBYSIGxxx message.

Action: Refer to the accompanying message(s) and take appropriate action. Refer to the user documentation. If necessary, report the entire incident context to your YottaDB support channel for further analysis.

------------------
SIGPRVREG
------------------

SIGPRVREG, Signal was caused by a privileged register

Run Time Error: This message is an auxiliary message and is preceded by a primary KILLBYSIGxxx message.

Action: Refer to the accompanying message(s) and take appropriate action. Refer to the user documentation. If necessary, report the entire incident context to your YottaDB support channel for further analysis.

.. _simpleapinest-error:

-----------------
SIMPLEAPINEST
-----------------

SIMPLEAPINEST, Attempt to nest a SimpleAPI call with another SimpleAPI call.

Run Time Error: This indicates that a SimpleAPI call (function name identified in the message text) was attempted while another SimpleAPI call (whose function name is also identified in the message text) is still running (possible for example, through a call-in or trigger invocation). Nesting of such SimpleAPI calls is not currently permitted.

Action: Avoid nesting SimpleAPI calls. Finish one SimpleAPI call before attempting another.

--------------------
SIMPLEAPINOTALLOWED
--------------------

SIMPLEAPINOTALLOWED, Process cannot switch to using Simple API while already using threaded Simple API

Run Time Error: This indicates a process has started using the threaded Simple API functions (e.g. ydb_set_st()) and is now trying to use the Simple API functions (e.g. ydb_set_s()).

Action: A process can only use either Simple API functions or threaded Simple API functions, not both.

------------------
SIZENOTVALID4
------------------

SIZENOTVALID4, Size (in bytes) must be either 1, 2, or 4

Run Time Error: The DSE CACHE command triggers this error when the size operand is not 1, 2, or 4.

Action: Specify 1, 2, or 4 as size (in bytes).

-------------------
SIZENOTVALID8
-------------------

SIZENOTVALID8, Size (in bytes) must be either 1, 2, 4, or 8

Run Time Error: Both the DSE CHANGE -FILEHEADER command and MUPIP REPLICATE -SOURCE -JNLPOOL -CHANGE command triggers this error when the SIZE qualifier is not set to 1, 2, 4, or 8.

Action: Specify 1, 2, 4, or 8 as size (in bytes).

-------------------
SNAPSHOTNOV4
-------------------

SNAPSHOTNOV4, Cannot downgrade (to V4) while snapshots are in progress. Currently ssss snapshots are in progress for region rrrr.

MUPIP Error: A request to downgrade a region to V4 occurred while a snapshot is in progress.

Action: Wait for a currently active process using snapshots to complete before running the downgrade. Since a downgrade to V4 would not normally be expected, check to verify that the downgrade invocation is appropriate.

-----------------
SOCKACCEPT
-----------------

SOCKACCEPT, Socket accept failed

Run Time Error: WRITE /ACCEPT encountered an I/O error while attempting to accept the sockets. No sockets were added to the socket pool.

Action: See the accompanying ENO or TEXT message for details.

------------------
SOCKACPT
------------------

SOCKACPT, Error accepting socket connection

Run Time Error: This indicates that the process of opening a socket resulted in a device error.

Action: Review the accompanying message(s) for additional information.

----------------
SOCKBFNOTEMPTY
----------------

SOCKBFNOTEMPTY, Socket buffer size cannot be set to xxxx due to aaaa bytes of buffered data. Read first.

Run Time Error: This indicates that a USE command attempted to adjust the size of the socket buffer while it contained data.

Action: Make sure the buffer is empty when adjusting the size.

----------------
SOCKBIND
----------------

SOCKBIND, Error in binding socket

Run Time Error: This message indicates a problem binding a socket to a port or file.

Action: Check the associated ENO message for more details.

-----------------
SOCKETEXIST
-----------------

SOCKETEXIST, Socket xxxx already exists

Run Time Error: This error is issued:

- On OPEN: ATTACH=xxxx is used to name a newly created socket but the name already exists in the current Socket device's collection.

- On USE: DETACH=xxxx is used to move a socket to the socketpool but the socketpool already has a socket with the specified name. ATTACH=xxxx is used to move a socket from the socketpool but the current Socket device already has a socket with the specified name.

Action: Review the names of the sockets already present on that device and specify a unique name.

------------------
SOCKINIT
------------------

SOCKINIT, Error initializing socket: (errno == aaaa) xxxx

Run Time Error: This indicates that the process of opening a socket resulted in a device error. xxxx is the text description of the failure for the OS service.

Action: Review the accompanying message(s) for additional information.

-----------------
SOCKLISTEN
-----------------

SOCKLISTEN, Error listening on a socket

Run Time Error: This indicates that YottaDB was unable to listen on the specified socket.

Action: Review accompanying messages for more information on the cause of the failure.

-----------------
SOCKMAX
-----------------

SOCKMAX, Attempt to exceed maximum sockets xxx for the SOCKET device

Run Time Error: Attempting to connect more than the maximum number of sockets defined for the process triggers this error. xxx is the maximum for the current process.

Action: Reduce the number of connections or use a process that has a higher maximum number of sockets defined by the ydb_max_sockets environment variable.

------------------
SOCKNOTFND
------------------

SOCKNOTFND, Socket xxxx not found

Run Time Error: This error is issued:

- On CLOSE when SOCKET=xxxx is used to specify which socket to close.

- On USE:

  * DETACH=xxxx when the specified socket in not in the current Socket device.
  * ATTACH=xxxx when the specified socket is not in the socketpool.
  * SOCKET=xxxx when the specified socket is not in the current Socket device.

Action: Make sure the socket is created before an I/O operation attempts using it.

-----------------------
SOCKNOTPASSED
-----------------------

SOCKNOTPASSED, Socket message contained no passed socket descriptors

Run Time Error: WRITE /ACCEPT received no sockets over the LOCAL connection.

Action: Verify the connection and make sure the WRITE /PASS on the sender is correct.

------------------
SOCKPASS
------------------

SOCKPASS, Socket pass failed

Run Time Error: WRITE /PASS encountered an I/O error while attempting to pass the sockets. No sockets were closed.

Action: See the accompanying ENO or TEXT message for details.

------------------
SOCKPASSDATAMIX
------------------

SOCKPASSDATAMIX, Attempt to use a LOCAL socket for both READ/WRITE and PASS/ACCEPT

Run Time Error: The code attempted to use a LOCAL socket for both data communication, using READ and/or WRITE, and socket passing, using WRITE /PASS or WRITE /ACCEPT. Using both forms of communication on the same socket is not supported.

Action: Use separate sockets for data and socket passing.

---------------------
SOCKWAIT
---------------------

SOCKWAIT, Error waiting for socket connection

Run Time Error: This indicates that the process of waiting for an event on a socket resulted in a device error.

Action: Review the accompanying message(s) for additional information.

---------------------
SOCKWRITE
---------------------

SOCKWRITE, Write to a socket failed

Run Time Error: This indicates that YottaDB was unable to write to a socket.

Action: Review the accompanying messages for more information on the cause of the failure.

-------------------
SPCLZMSG
-------------------

SPCLZMSG, The following error message cannot be driven through ZMESSAGE

Run Time Error: The specified error code is not allowed to be driven through ZMESSAGE

Action: Make sure ZMESSAGE is not driving any prohibited error message.

--------------------
SPOREOL
--------------------

SPOREOL, Either a space or an end-of-line was expected but not found

Compile Time Error: This indicates that a command that required an argument did not specify one or that an ELSE attempted to specify an argument.

Action: Look for and correct any typographical errors.

--------------------
SRCFILERR
--------------------

SRCFILERR, Error with source file I/O on file xxxx

Compile Time Error: This indicates that a ZCOMPILE, ZLINK, or auto-ZLINK encountered an error when it attempted to access source file xxxx. An M command may also report this error at compile-time.

Action: Use host operating system commands to list the file and review accompanying messages for additional information.

---------------------
SRCLIN
---------------------

SRCLIN, xxxx

Compile Time Information: This message displays the source code line where an error occurred.

Action: -

---------------------
SRCLNNTDSP
---------------------

SRCLNNTDSP, Source lines exceeding wwww character width are not displayed

Compile Time Error: Displayed instead of the source line when source line exceeds 1023 characters.

Action: Refer to the source code. The line number and column number in the associated messages identify the position of the problem. Consider shortening the line, at least until the error is found and corrected.

--------------------
SRCLOC
--------------------

SRCLOC, At column xxxx, line yyyy, source module zzzz

Compile Time Error: YottaDB uses this message to display the line, offset, and routine where it encountered a compile-time error. xxxx is the column. yyyy is the line number. zzzz is the routine name.

Action: Review the accompanying message(s) for additional information.

------------------
SRCLOCUNKNOWN
------------------

SRCLOCUNKNOWN, M source location unknown

Run Time Warning: This indicates that YottaDB could not locate the source line associated with the error.

Action: Find out if source code is available to you - if not, this message is expected. Otherwise, check ZROUTINES and file permissions.

-------------------
SRCNAM
-------------------

SRCNAM, in source module xxxx

Compile Time Information: This message identifies the module xxxx, which contains some other error.

Action: Review the accompanying message(s) for additional information.

------------------
SRCSRVEXISTS
------------------

SRCSRVEXISTS, Source server for secondary instance xxxx is already running with pid yyyy

Source Server log/MUPIP Error: This error is issued by a Source Server startup command if there is already a Source Server up and running for the secondary instance name specified in the command.

Action: Do not start multiple source servers for the same secondary instance.

--------------------
SRCSRVNOTEXIST
--------------------

SRCSRVNOTEXIST, Source server for secondary instance xxxx is not alive

Source Server log/MUPIP Error: This error is issued by a mupip replic -source command that specifies any one of activate, changelog, checkhealth, deactivate, shutdown, showbacklog, statslog, stopsourcefilter if it finds no Source Server up and running for the replicating (secondary) instance name specified in the command.

Action: Make sure the Source Server for the specified replicating instance name is up and running to provide working replication.

---------------------
SRCSRVTOOMANY
---------------------

SRCSRVTOOMANY, Cannot start more than xxxx source servers in primary instance file yyyy

MUPIP Error: A maximum of 16 active and/or passive source servers are allowed at any point in time per instance. If 16 source servers are already running and another source server startup is attempted, it will issue this error.

Action: Shutdown any active or passive source server to allow the new source server to start up.

-------------------
SRVLCKWT2LNG
-------------------

SRVLCKWT2LNG, PID pppp is holding the source server lock. Waited for mmmm minute(s). Now exiting

MUPIP Error: Issued by MUPIP ROLLBACK -ONLINE when it finds process pppp has not released the journal pool resource for mmmm minutes.

Action: Investigate the state of process pppp and whether it should be stopped by operator action.

-------------------
SSATTACHSHM
-------------------

SSATTACHSHM, Error while attaching to shared memory identifier iiii

Run Time Error: A YottaDB process encountered an error while trying to attach to shared memory that was created to manage a snapshot and reports the above error in the operator log.

Action: Examine the accompanying system error message and take appropriate action.

-----------------
SSFILCLNUPFAIL
-----------------

SSFILCLNUPFAIL, Error while unlinking snapshot file -- xxxx

MUPIP Error: An attempt to terminate snapshot file maintenance by YottaDB updater processes encountered a problem.

Action: Try a MUPIP RUNDOWN. If that has a similar problem, it may be prudent to shut down all access to the database in question in order to stop the burden of maintaining the snapshot file and to ensure it doesn't unnecessarily consume more space.


-----------------
SSFILOPERR
-----------------

SSFILOPERR, Error while doing oooo operation on file ffff

MUPIP Error: This operator log message indicates that operation oooo on snapshot file ffff failed. Note that in certain timing situations, this action might be reported to the operator log after the snapshot consuming process (say MUPIP INTEG) has finished with the snapshot file, in which case it's harmless. If the consuming process issues a REGSSFAIL error, then it was definitely prevented from completing its tack because of the SSFILOPERR.

Action: Analyze the operation and the file characteristics and take appropriate action to clear the problem.

----------------
SSPREMATEOF
----------------

SSPREMATEOF, Premature end of file while reading block nnnn of size: bbbb bytes at offset: oooo from zzzz

MUPIP Error: The action attempted access to a block beyond the end of the snapshot file. This means that either the process was confused, or the file is damaged.

Action: Retry the action. If the problem persists, contact YottaDB with information on how to recreate the problem.

------------------
SSSHMCLNUPFAIL
------------------

SSSHMCLNUPFAIL, Error while doing snapshot shared memory cleanup. Operation -- ssss. Identifier -- dddd

MUPIP Error: There was an error while doing a snapshot cleanup. The operation ssss indicates what system call failed. The identifier dddd indicates the shared memory identifier that is being cleaned up.

Action: Analyze the failure details and take corrective measures. If appropriate carefully clear abandoned resources using the system ipcrm utility.

------------------
SSTMPCREATE
------------------

SSTMPCREATE, Cannot create the temporary file in directory dddd for the requested snapshot

MUPIP Error: An action requiring a snapshot file was unable to create it.

Action: Verify that the directory has appropriate access permissions for the user performing the action.

-----------------
SSTMPDIRSTAT
-----------------

SSTMPDIRSTAT, Cannot access temporary directory dddd

MUPIP Error: An action requiring a snapshot file was unable to access the temporary directory.

Action: Verify that the directory exists and has appropriate access permissions for the user performing the action.

-------------------
SSV4NOALLOW
-------------------

SSV4NOALLOW, Database snapshots are supported only on fully upgraded V5 databases. nnnn has V4 format blocks.

MUPIP Error: An action requiring a snapshot was attempted on a database that contains V4 format blocks.

Action: Upgrade the database to V5 and re-run the action.

------------------
STACKCRIT
------------------

STACKCRIT, Stack space critical

Run Time Error: This indicates that the process has consumed almost all of the available stack space.

Action: If you do not take immediate action to reduce stack usage, YottaDB is likely to produce a STACKOFLOW error, which terminates the process. There are two common causes:

 * Infinite recursion. The most common cause of infinite recursion is a buggy error trap. Whatever the cause, you can examine the stack with ZSHOW, and use ZGOTO, QUIT to try to regain control of the process, or HALT or ZHALT to terminate the process.

 * Application level memory leak caused by NEW of local variables in a loop. Correct the application code to NEW variables outside any loop, and replace the NEW inside loops with KILL.

-----------------
STACKOFLOW
-----------------

STACKOFLOW, Stack overflow

Run Time Fatal: This indicates that the process required more stack space than was available in memory.

Action: Reduce the stack when you get a STACKCRIT error. This error terminates the process.

-------------------
STACKUNDERFLO
-------------------

STACKUNDERFLO, Stack underflow

Run Time Error: This indicates that the process stack was corrupt.

Action: Review the accompanying messages for additional information. If necessary, report the entire incident context to your YottaDB support channel for further analysis.

-------------------
STAPIFORKEXEC
-------------------

STAPIFORKEXEC, Calls to YottaDB are not supported after a fork() if threaded Simple API functions were in use in parent. Call exec() first

Run Time Error: After calling at least one threaded Simple API function, a process performed a fork(), and the child called another Simple API function (threaded or non-threaded) without calling exec() first.

Action: Once a process that has used threaded Simple API functions or threaded Simple API functions does a fork(), the child process must call exec() before it can call again into YottaDB (using Simple API functions or threaded Simple API functions).

-----------------
STARFILE
-----------------

STARFILE, Star(*) argument cannot be specified with xxxx

MUPIP Error: This indicates that the qualifier xxxx, specified with the MUPIP JOURNAL command does not allow star (*) as an argument.

Action: Specify the journal file names explicitly.

-------------------
STATCNT
-------------------

STATCNT, xxxx: Key cnt: yyyy max subsc len: zzzz max data len: wwww

MUPIP Information: LOAD uses this message to display status. xxxx is the name of the global being updated. yyyy is the number of nodes handled. zzzz is the largest subscript encountered. wwww is the size of the largest node.

Action: -

------------------
STATSDBERR
------------------

STATSDBERR, Error in/at LLLL attempting to use a statistics database: SSSS

Run Time Error: Indicates that an error occurred while attempting to open a statistics database. This error is followed by one or more additional error messages describing the actual condition that occured. The LLLL is an entryref or module name where the error occurred.

Action: Address the condition(s) causing the error and retry.

-----------------
STATSDBFNERR
-----------------

STATSDBFNERR, This database has no accessible statistics database due to the following error: EEEE

Run Time Error: This indicates that there was an error EEEE opening the statistics database. This error usually shows up as a subordinate message to STATSDBERR.

Action: Address the condition causing the error and retry.

------------------
STATSDBINUSE
------------------

STATSDBINUSE, Statistics database SSSS is in use with database DDDD so cannot also be used with database OOOO

Run Time Error: The statistics database SSSS is currently associated with database DDDD and therefore cannot be associated with database OOOO.

Action: Determine why two separate base databases would both be trying to use the same statistics database (often this is because statistics database names are soft linked to each other), fix the condition, and retry.

------------------
STATSDBMEMERR
------------------

STATSDBMEMERR, Process attempted to create stats block in statistics database SSSS and received SIGBUS--invalid physical address. Check file system space.

Run Time Error: A process attempted to enable shared statistics collection for the region associated with SSSS, but was unable to find room to add its records, so it cannot contribute to sharing. This message goes to the operator log facility rather than the process as an error, but the process continues without shared statistics.

Action: Adjust the environment so that SSSS can expand and then, if possible, have the process again attempt to enable sharing.

-----------------
STATSDBNOTSUPP
-----------------

STATSDBNOTSUPP, Attempted operation is not supported on statistics database file SSSS

All YottaDB Components Error: An attempt was made to use a statistics database in a manner that is not supported. Statistics databases have very restricted capabilities, for example: they only accept updates from YottaDB itself.

Action: Identify and remove the inappropriate action(s).

------------------
STDERRALREADYOPEN
------------------

STDERRALREADYOPEN, STDERR deviceparameter specifies an already open device xxxx

Run Time Error: This indicates that the STDERR deviceparameter in the OPEN command of a PIPE device specifies a device name xxxx that is already open in the process.

Action: Specify a device name that is not already an open device in the process.

-----------------
STDNULLCOLLREQ
-----------------

STDNULLCOLLREQ, Region rrrr needs Standard Null Collation enabled because global gggg spans through it

GDE Information: This indicates that the global gggg spans through region rrrr but the region has historical null collation enabled.

Action: All regions containing parts of spanning globals need to have Standard Null Collation enabled. Fix region rrrr to have Standard Null Collation enabled.

----------------
STPCRIT
----------------

STPCRIT, String pool space critical

Run Time Error: This indicates that the process has exceeded the heap (string pool) limit specified in the $ZSTRPLLIM ISV. If you do not take prompt action to reduce the process memory requirements, at the next heap expansion, YottaDB produces an STPOFLOW error, which terminates the process.

Action: Investigate whether the process memory usage is appropriate, and if so, increase or remove the limit. Otherwise correct the cause(s) of the excessive memory consumption. Please see the documentation for `$ZSTRPLLIM <../ProgrammersGuide/isv.html#zstrpllim-isv>`_ for additional information.

----------------
STPEXPFAIL
----------------

STPEXPFAIL, Stringpool expansion failed. It could not expand to xxxx bytes

Run Time Error: The stringpool, an internally expanding data structure maintained by YottaDB to store primarily M-local variable content, needs more memory than is available in the process virtual memory.

Action: Increase process memory quotas to increase available process virtual memory. Change application to reduce memory requirements of the stringpool by using lesser M-local variables.

----------------
STPOFLOW
----------------

STPOFLOW, String pool space overflow

Run Time Fatal: This indicates that the process has previously exceeded the heap (string pool) limit specified in the $ZSTRPLLIM ISV and still needs more memory, so YottaDB terminates the process.

Action: Investigate whether the process memory usage is appropriate, and if so, increase or remove the limit. Otherwise correct the cause(s) of the excessive memory consumption. Please see the documentation for $ZSTRPLLIM for additional information.

------------------
STRINGOFLOW
------------------

STRINGOFLOW, String pool overflow

Compile Time Error: This indicates that some action attempted to use the string pool when it was full.

Action: Look for very large string constants in a very large program; reduce one or both.

-----------------
STRMISSQUOTE
-----------------

STRMISSQUOTE, Missing double-quote at end of string specification ssss

GDE Error: This indicates that a subscripted name was specified with string subscripts - parts of which were enclosed inside double-quotes, but the closing double-quote is missing.

Action: Specify the subscripted name with the appropriate double quote(s).

------------------
STRMNUMIS
------------------

STRMNUMIS, Stream # is ssss

MUPIP Information: Issued by a Receiver Server to designate a stream associated with the the immediately preceding message.

Action: Refer to the associated prior message.

----------------
STRMNUMMISMTCH1
----------------

STRMNUMMISMTCH1, Stream nnnn exists on the receiver instance file but is unknown on the source instance

MUPIP Error: Issued by a Source Server on a Supplementary Instance when it detects that a non-Supplementary stream number nnnn (which can be any value from 1 through 15) exists on the receiving instance but not on the source instance. This indicates the two instances are not in sync, at least with respect to stream nnnn, so replication cannot proceed.

Action: Reinitialize the receiving instance from a backup of the source instance and restart replication between the two instances.

---------------------
STRMNUMMISMTCH2
---------------------

STRMNUMMISMTCH2, Stream nnnn exists on the source instance file but is unknown on the receiver instance

MUPIP Error: Issued by a Source Server on a supplementary instance when it detects that a non-supplementary stream number nnnn (which can be any value from 1 through 15) exists on the source instance but not on the receiving instance. This indicates the two instances are not in sync, at least with respect to stream nnnn, and so replication cannot proceed.

Action: Reinitialize the receiving instance from a backup of the source instance and restart replication between the two instances.

----------------------
STRMSEQMISMTCH
----------------------

STRMSEQMISMTCH, Unable to play update on Stream nnnn with seqno xxxx as receiving instance has a different stream seqno XXXX

Update Process log/MUPIP Error: Issued by the Update Process on a supplementary instance started with -UPDNOTOK (that is, local updates are disabled) when it finds the source and receiving instances have different values for the stream sequence number of the non-Supplementary stream nnnn (can be any value from 1 through 15). This indicates that the two instances are not in sync, at least with respect to stream nnnn, and so replication cannot proceed.

Action: Reinitialize the receiving instance from a backup of the source instance and restart replication between the two instances.

-------------------
STRNOTVALID
-------------------

STRNOTVALID, Error: cannot convert xxxx value to valid yyyy value.

DSE Error: This error shows in DSE when there is a string input that cannot be converted to a number. For example, attempting "change -fileheader -location=0x123rt456" would apply to this error.

Action: Review and correct typographical errors.

-------------------
STRUCTNOTALLOCD
-------------------

STRUCTNOTALLOCD, Structure not previously called with Alloc() method

Runtime Error: This message comes only from the Golang wrapper. If a BufferT or BufferTArray structure is used without having been allocated by the Alloc() message, the wrapper can return this error.

Action: Be sure the structure has been allocated before attempting to use it.

----------------
STRUNXEOR
----------------

STRUNXEOR, xxxx unexpected end of record in string subscript

MUPIP Error: This indicates that LOAD aborted because it encountered an end of file while processing string subscript xxxx.

Action: Refer to the topic `MUPIP LOAD Errors in the About This Manual section <./about.html#mupip-load-errors>`_.

-----------------
STUCKACT
-----------------

STUCKACT, Process stuck script invoked: rrrr : pppp

Run Time Information: This message shows the success or failure status (return rrrr) of invoking the script pppp pointed to by the environment variable $ydb_procstuckexec

Action: If the result is a success, analyze the output of the script. If the result is a failure, check the script and any output it produced up to the point of failure and rework the script or adjust the environment appropriately.

------------------
SUB2LONG
------------------

SUB2LONG, Subscript invalid, too long

MUPIP Error: This indicates that INTEG encountered a subscript that is too long for its display mechanism.

Action: Examine the subscript with DSE DUMP, and take action to eliminate the subscript if it is invalid.

------------------
SUBSARRAYNULL
------------------

SUBSARRAYNULL, Non-zero number of subscripts xxxx specified but subscript array parameter is NULL in API call.

Run Time Error: This indicates that the value of the subscript array parameter is NULL, meaning there are no subscripts specified, but the parameter specifying the number of subscripts (usually the "subs_used" parameter) has a non-zero value.

Action: Retry the SimpleAPI call with a non-NULL subscript array parameter or with a zero value for the parameter specifying the number of subscripts.

-------------------
SUPRCVRNEEDSSUPSRC
-------------------

SUPRCVRNEEDSSUPSRC, Instance iiii is not configured to perform local updates, so it cannot act as a receiver for non-Supplementary Instance ssss

MUPIP Error: Issued by a Receiver Server or a MUPIP JOURNAL -ROLLBACK -FETCHRESYNC on a Supplementary Instance ssss started with -UPDNOTOK attempted to connect to non-Supplementary Instance iiii. A Supplementary Instance that does not permit local updates can only replicate from another Supplementary Instance.

Action: Reconfigure the instances to a supported configuration.

------------------
SUSPENDING
------------------

SUSPENDING, Suspending processing on user request or attempt to do terminal I/O while running in the background

Run Time Information: The message signifies that a YottaDB process is suspended on user initiated ^Z (or key stroke that is set to shell "susp"). It is also displayed if the process attempts to do terminal I/O while running in the background. Before suspending itself, the process logs the SUSPENDING message to the operator facility. Suspended processes may be automatically released from that state if they hold a shared resource that blocks other processes.

Action: Because YottaDB uses shared resources, suspending a YottaDB process can lock those resources and prevent other processes from working. Disable process suspension from <CTRL-Z> by appropriately configuring the shell. If you find a YDB-I-SUSPENDING in the syslog, match it with a YDB-I-REQ2RESUME to ensure that the process went back to work and released any resources it had. Identify the cause of the suspension and take action to prevent a recurrence. If there is no resume, check the process listing to confirm that there is a shell associated with the process and request the user to unsuspend the process. If there is no shell associated with process, it is likely that the process was backgrounded before the shell was terminated. Kill this process with MUPIP STOP.

-------------------
SVNEXPECTED
-------------------

SVNEXPECTED, Special variable expected in this context

Compile Time/Run Time Error: This indicates that YottaDB encountered a dollar sign in a NEW command that was not followed by a valid special variable name.

Action: Look for misspelled special variable names or a missing $ in an extrinsic.

--------------------
SVNONEW
--------------------

SVNONEW, Cannot NEW this special variable

Compile Time/Run Time Error: This indicates that a NEW command tried to new an intrinsic special variable that is not a valid argument for a NEW.

Action: Look for inappropriate $ prefixes. $ZTRAP, $ETRAP, $ESTACK, $ZYERROR, $ZGBLDIR are the only intrinsic special variables that can be NEWed.

------------------
SVNOSET
------------------

SVNOSET, Cannot SET this special variable

Compile Time Error: This indicates that a SET command specified an assignment that attempted to modify a read-only special variable.

Action: Look for inappropriate $ prefixes and attempts to modify read-only special variables. $DEVICE, $ECODE, $ETRAP, $DEVICE, $KEY, $X, and $Y are the only ANSI standard variables that can be SET.

------------------
SYSCALL
------------------

SYSCALL, Error received from system call xxxx -- called from module yyyy at line zzzz

Run Time Error: This indicates that a system call failed due to some unusual error condition.

Action: Report to the system administrator and if necessary report the entire incident context to your YottaDB support channel for further analysis.

--------------------
SYSUTILCONF
--------------------

SYSUTILCONF, Error determining the path for system utility. tttt

Run Time Error: tttt represents text describing details of an issue finding a POSIX function that it needed.

Action: Check for aliases or environment variables related to paths that might be interfering with YottaDB’s ability to invoke functions.

--------------
TCGETATTR
--------------

TCGETATTR, Error while getting terminal attributes on file descriptor xxxx

Run Time Error: This indicates that the terminal attributes are inaccessible where xxxx is the file descriptor for the device.

Action: Review and correct the OS configuration of the device.

-------------------
TCOMMITDISALLOW
-------------------

TCOMMITDISALLOW, TROLLBACK required after an unhandled error in trigger context

Run Time Error: This transaction did an update that invoked a trigger which in turn encountered an error that was not handled by the application error trap inside the trigger context. Because of this, the exit from the trigger context was abnormal. YottaDB does not commit such transactions since they would not preserve the atomicity of trigger updates (triggering update + triggered updates).

Action: Such transactions can only be rolled back. If this is a nested TSTART (subtransaction), it can optionally be rolled back incrementally, that is, only the nested TSTART needs to be rolled back while the parent TSTART can still be committed.

----------------------
TCPCONNTIMEOUT
----------------------

TCPCONNTIMEOUT, Connection wait timeout (ssss seconds) has expired

Runtime Error: When MUPIP RESTORE or MUPIP BACKUP are pushing/pulling data over a socket, the attempt to attach to "the other side" can timeout. If it does timeout, this is the error that is returned.

Action: Determine the source of the connection delay, fix and retry.

-------------------
TCSETATTR
-------------------

TCSETATTR, Error while setting terminal attributes on file descriptor xxxx

Run Time Error: This indicates that the terminal attributes are inaccessible where xxxx is the file descriptor for the device.

Action: Review and correct the OS configuration of the device.

------------------
TERMASTQUOTA
------------------

TERMASTQUOTA, Process AST quota exceeded, cannot open terminal

Run Time Error: This indicates that an OPEN command failed because it required an AST that would violate the process quota.

Action: Reduce the number of terminals in use by a single process or ask your system administrator about changing your AST quota.

-------------------
TERMHANGUP
-------------------

TERMHANGUP, Terminal has disconnected

Run Time Error: This indicates that the terminal serving as the PRINCIPAL device has disconnected. By default, YottaDB ignores terminal "hang-ups", which can allow the terminal to reconnect at a later time to a process that does not need the terminal to continue work. You can enable recognition of principal device disconnects with USE $PRINCIPAL:HUPENABLE or by starting the process with the ydb_hupenable set to 1, TRUE or YES, or disable them with USE $PRINCIPAL:NOHUPENABLE.

Action: When a process receives this error it must avoid any further READs from, or WRITEs to $PRINCIPAL, typically shutting down in a wholesome fashion. Failure to do so causes YottaDB to terminate the process with a NOPRINCIO message to the operator log.

TERMHANGUP was added to YottaDB effective release `r1.34 <https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.34>`_.

-------------------
TERMWRITE
-------------------

TERMWRITE, Error writing to terminal, status:

Run Time Error: This indicates that a WRITE to a terminal failed. Such failures may be detected and reported asynchronously to the actual WRITE command.

Action: Review the accompanying message(s) for additional information.

-------------------
TEXT
-------------------

TEXT, xxxx

Run Time Information: YottaDB uses this message with various accompanying text, xxxx, to expand on other errors.

Action: Examine the text and review any accompanying message(s).

------------------
TEXTARG
------------------

TEXTARG, Invalid argument to $TEXT function

Compile Time Error: This indicates that a $TEXT function specified an invalid argument.

Action: Modify the $TEXT() argument so it is in the format of an entryref.

----------------------
THREADEDAPINOTALLOWED
----------------------

THREADEDAPINOTALLOWED, Process cannot switch to using threaded Simple API while already using Simple API

Run Time Error: This indicates a process has started using the Simple API functions (e.g. ydb_set_s()) and is now trying to use the threaded Simple API functions (e.g. ydb_set_st()).

Action: A process can only use either Simple API functions or threaded Simple API functions, not both. If the base program corresponding to this process is an M program, it can only use Simple API functions.

------------------
TIME2LONG
------------------

TIME2LONG, Specified time value exceeds supported maximum limit xxxx allowed.

Run Time Error: This indicates that a timer value specified in a SimpleAPI call (e.g. ydb_lock_s(), ydb_lock_incr_s() etc.) exceeded the maximum allowed limit. Both the specified time value and the maximum allowed limit are indicated in the message text.

Action: Specify a time value below the maximum limit and retry the SimpleAPI call.

---------------------
TIMERHANDLER
---------------------

TIMERHANDLER, Incorrect SIGALRM handler xxxx found by yyyy

Run Time Information: This indicates that an external user-supplied routine (C or other language) called from YottaDB, incorrectly manipulated the system timer handler. The xxxx is the hexadecimal address of the handler installed by the external routine and yyyy is the routine within YottaDB, which discovered the problem.

Action: Use the YottaDB-provided timer facility described in the `Programmer's Guide <../ProgrammersGuide/index.html>`_ .

-----------------
TIMEROVFL
-----------------

TIMEROVFL, Timer overflow; interval probably too large

Run Time Error: This indicates that a timeout or timer calculation exceeded the maximum allowable interval.

Action: Check the system maximum interval and set the interval accordingly.

------------------
TIMRBADVAL
------------------

TIMRBADVAL, Bad value specified. Timer not changed.

DSE Error: This indicates that a CHANGE command with the FILEHEADER qualifier specified a time value that was improperly formatted or inappropriate.

Action: Modify the time value.

------------------
TLSCONNINFO
------------------

TLSCONNINFO, Failed to obtain information on the TLS/SSL connection

MUPIP Warning: This indicates that an attempt to establish TLS/SSL connection failed.

Action: Review the following TEXT message from the plug-in for additional diagnostic information and adjust the environment accordingly.

-------------------
TLSCONVSOCK
-------------------

TLSCONVSOCK, Failed to convert UNIX TCP/IP socket to TLS/SSL aware socket

Run Time/MUPIP Error: This indicates that an attempt to establish TLS/SSL connection failed.

Action: Review the following TEXT message from the plug-in for additional diagnostic information, review your keys and related certificates, and adjust the environment accordingly.

--------------------
TLSDLLNOOPEN
--------------------

TLSDLLNOOPEN, Failed to load YottaDB TLS/SSL library for secure communication

MUPIP Error: This indicates that the attempt to load the YottaDB dynamically linked library for TLS/SSL operation failed.

Action: Review the following TEXT message from the plug-in for additional diagnostic information, check the path and authorizations for the library, and adjust the environment accordingly.

-----------------
TLSHANDSHAKE
-----------------

TLSHANDSHAKE, Connection to remote side using TLS/SSL protocol failed

Run Time/MUPIP Error: This indicates that an attempt to establish SSL/TLS connection failed.

Action: Review the following TEXT message from the plug-in for additional diagnostic information, and adjust the environment accordingly.

------------------
TLSINIT
------------------

TLSINIT, Failed to initialize YottaDB TLS/SSL library for secure communication

Run Time/MUPIP Error: This indicates that the attempt to initialize a secure context for TLS/SSL operation failed.

Action: Review the following TEXT message from the plug-in for additional diagnostic information, check your certificates, and adjust the environment accordingly.

------------------
TLSIOERROR
------------------

TLSIOERROR, Error during TLS/SSL oooo operation

Run Time/MUPIP Error: This indicates that while attempting oooo operation (receive or send), the TLS/SSL library encountered an error.

Action: Review the following TEXT message from the plug-in for additional diagnostic information, and adjust the environment accordingly.

--------------------
TLSPARAM
--------------------

TLSPARAM, TLS parameter pppp eeee

Run Time Error: This indicates a problem with a parameter on a WRITE /TLS command. pppp identifies the parameter and eeee describes the problem.

Action: Address the reported problem described by eeee.

---------------------
TLSRENEGOTIATE
---------------------

TLSRENEGOTIATE, Failed to renegotiate TLS/SSL connection

Run Time/MUPIP Error: This indicates that an attempt to renegotiate the SSL/TLS connection failed.

Action: Review the following TEXT message from the plug-in for additional diagnostic information, and adjust the environment accordingly.

---------------------
TLVLZERO
---------------------

TLVLZERO, Transaction is not in progress

Compile Time/Run Time Error: This indicates that a TCOMMIT, TROLLBACK, or TRESTART command attempted to change the state of the current transaction; however, a transaction was not in progress.

Action: Look for missing TSTARTs, extra TCOMMITs or TROLLBACKs, or an unanticipated flow of control; conditionalize the command on $TLEVEL, if appropriate.

----------------
TMPFILENOCRE
----------------

TMPFILENOCRE, Error in MUPIP BACKUP while trying to create temporary file xxxx

MUPIP Error: This indicates that MUPIP BACKUP was not able to create the temporary file xxxx needed in the course of its processing, possibly due to earlier incomplete backups.

Action: Make sure the temporary files are removed from the backup directory, and re-issue the BACKUP.

-------------------
TMPSTOREMAX
-------------------

TMPSTOREMAX, Maximum space for temporary values exceeded

Compile Time Error: YottaDB uses 1024 temporary registers to hold intermediate results of expression evaluation. Because of the line-oriented nature of the M language, these temporary registers get reused for each line. This error indicates that there is a single calculation on a line that requires more than 1024 temporary registers.

Action: Look for excessive function nesting on a very long line of code. Reduce the amount of nesting by assigning intermediate results to a variable. Alternatively, if compiling the routine with the -DYNAMIC_LITERALS qualifier, try compiling with -NODYNAMIC_LITERALS.

-----------------
TNTOOLARGE
-----------------

TNTOOLARGE, Database file xxx has reached the transaction number limit (0xaaa). Renew database with MUPIP INTEG TN_RESET

Run Time Information: This indicates that YottaDB detected that the transaction numbers in the named database have reached the maximum number. Note that the actual maximum TN is less than this theoretical limit. DSE DUMP FILEHEADER shows what the limit is. The actual limit reflects some overhead used, for example, during a TN_RESET operation.

Action: Use MUPIP INTEG with the qualifier TN_RESET to reset the transaction numbers in the database. If the database is in a previous format, consider converting it to the most recent format. The database cannot otherwise be used until the condition is removed by either a TN_RESET or, if a previous version of the database, changing the output mode to the latest version with MUPIP SET VERSION.

----------------------
TNWARN
----------------------

TNWARN, Database file xxx has 0xaaa more transactions to go before reaching the transaction number limit (0xaaa). Renew database with MUPIP INTEG TN_RESET.

Run Time Information: This indicates that YottaDB detected that the transaction numbers in the named database are approaching the maximum number. This message is sent to the operator log periodically at decreasing intervals as the transaction number approaches the maximum. Note that the actual maximum TN is less than this theoretical limit. DSE DUMP FILEHEADER shows what the limit is. The actual limit reflects some overhead used, for example, during a TN_RESET operation.

Action: Use MUPIP INTEG with the qualifier TN_RESET to reset the transaction numbers in the database. If the database is in a previous format, consider converting it to the most recent format.

------------------
TOOMANYCLIENTS
------------------

TOOMANYCLIENTS, YottaDB is serving the maximum number of clients. Try again later.

Run Time Error: This indicates that the process failed in accessing a region served via GT.CM server, which currently cannot accept another connection. This is unlikely to happen unless many of the servers clients have abruptly disconnected and the server has been running for a long time.

Action: Try again later. Stop and restart the server to resolve the problem. If the problem persists, contact the group responsible for database operations on your network.

--------------------
TOTALBLKMAX
--------------------

TOTALBLKMAX, Extension exceeds maximum total blocks, not extending

Run Time Error: This indicates that the database file extension specified implicitly or explicitly (using MUPIP EXTEND) would cause the GDS file to exceed its maximum size. The maximum database size is:

+----------------------------------------------------------------+---------------------------------------------------------------+
| Max blocks in a DB file (MiB)                                  | Max DB Size* (GiB)                                            |
+================================================================+===============================================================+
| 992                                                            | 4096                                                          |
+----------------------------------------------------------------+---------------------------------------------------------------+

(for a database file with block size of 8192 bytes).

Action: Modify the extension to use a smaller size. This may indicate that you should move some contents of the database file to another file.

---------------------
TPCALLBACKINVRETVAL
---------------------

TPCALLBACKINVRETVAL, Invalid return type for TP callback function

Run Time Error: This is not an error that YottaDB generates, but one generated by a language wrapper for a dynamically typed language such as `Python <https://www.python.org/>`_. When an application function that implements transaction logic returns an inappropriately typed value to the wrapper, it raises the TPCALLBACKINVRETVAL error.

Action: Examine application logic and ensure that transaction logic returns an appropriately typed return value to the wrapper.

--------------------
TPFAIL
--------------------

TPFAIL, Transaction COMMIT failed. failure code: xxxx.

Run Time Error: This indicates that YottaDB attempted to process this transaction four times, but encountered an error every time. xxxx contains the failure codes for the four attempts. It is very likely that the database may have integrity errors or that the process-private data structures are corrupted.

Action: Report this database error to the group responsible for database integrity at your operation.

-----------------
TPLOCK
-----------------

TPLOCK, Cannot release lock(s) held prior to current TSTART

Run Time Error: This indicates that a LOCK or ZDEALLOCATE command attempted to release named resources that were LOCKed or ZALLOCATEd prior to the initial TSTART of the current transaction.

Action: Do not release the named resource until after the TCOMMIT, or LOCK/ZALLOCATE has reserved the resource after the TSTART has begun.

-------------------
TPMIXUP
-------------------

TPMIXUP, xxxx transaction cannot be started within yyyy transaction

Run Time Error: This indicates that the software, function, or routine may use incompatible transaction fencing.

Action: Transaction fences and ZT transaction fences cannot be used in combination. Review the code for incompatible use of T and ZT transaction fences.

----------------
TPNOSTATSHARE
----------------

TPNOSTATSHARE, VIEW "[NO]STATSHARE" is not allowed inside a TP transaction

Run Time Error: Because statistics sharing is a non-ACID (due to not being Isolated) action, YottaDB does not permit changing it within a TP transaction.

Action: Move any switching of statistics outside of any TP transaction.

----------------
TPNOSUPPORT
----------------

TPNOSUPPORT, Operation cannot be performed while inside of a TP transaction

Run Time Error: This indicates that a $$set^%GBLDEF or $$kill^%GBLDEF was attempted while inside of a TP transaction (TSTART/TCOMMIT fence).

Action: ^%GBLDEF only supports TP from the $$get entry point; it does not support inclusion of $$set or $$kill entry points within a TP transaction.

---------------
TPNOTACID
---------------

TPNOTACID, tttt at xxxx in a final TP retry violates ACID properties of a TRANSACTION; indefinite RESTARTs may occur

Run Time Information: YottaDB issues this message if it is executing a TP TRANSACTION in the final retry and control gets transferred out of YottaDB due to any one of the following three conditions:

1. ZSYSTEM command
2. Entering direct mode (e.g. due to a BREAK command) or
3. a long running command (those which accept timeout specifications) encountered potentially indefinite restarts. The xxxx indicates the $ZPOSITION where the transfer of control occurred and the condition that caused this is identified in tttt.

Action: Review your code to determine whether the non-Isolated commands can be moved outside of transaction encapsulation. Alternatively, ensure that they are minimally disruptive by using $ZMAXTPTIME to prevent transactions from running unreasonably long times and setting ydb_tpnotacidtime to specify the wait period for long running commands before YottaDB logs a TPNOTACID message.

.. note::
   Because a process that gives a TPNOTACID message is in an indefinite restart, it might fail to produce an error indicating database damage should it encounter such an unlikely eventuality. In this case, indefinite restarts do not cause any additional damage.

---------------
TPQUIT
---------------

TPQUIT, Cannot QUIT out of a routine with an active transaction

Run Time Error: This indicates that an implicit or explicit QUIT attempted to leave an invocation level where a transaction or subtransaction was TSTARTed but not matched by either a TCOMMIT or TROLLBACK.

Action: Modify the routine to TCOMMIT or TROLLBACK the transaction before QUITting, or move the TSTART to a prior invocation level.

--------------
TPRESTART
--------------

TPRESTART, Database mmmm; code: xxxx; blk: yyyy in glbl: zzzz; pvtmods: aaaa, blkmods: bbbb, blklvl: cccc, type: dddd, readset: eeee, writeset: ffff, local_tn: gggg, zpos: hhhh

Run Time Information: The UNIX environment variables GTM_TPRESTART_LOG_FIRST and GTM_TPRESTART_LOG_DELTA control the logging of TPRESTART messages. GTM_TPRESTART_LOG_FIRST indicates the number of TP restarts to log from a YottaDB invocation. Once that many have been logged, every GTM_TPRESTART_LOG_DELTA TP restarts, YottaDB logs a restart message. If GTM_TPRESTART_LOG_DELTA is undefined, YottaDB performs no operator logging. The default value for GTM_TPRESTART_LOG_FIRST is 0 (zero), which leaves the control completely with GTM_TPRESTART_LOG_DELTA. The facility that produces this message can serve as a diagnostic tool in developmental environments for investigating contention due to global updates. A zzzz of "\*BITMAP" indicates contention in block allocation which might involve multiple globals. hhhh is the $ZPOSITION of the line of M code that caused the restart of the transaction; utilities leave this field blank. Non-graphic codes are reported as :code:`0xnn`.

Action: Disable, or adjust the frequency of, these messages with the mechanism described above. To reduce the number of restarts, consider changes to the global structure, or varying the time when work is scheduled. Consider whether the business and program logic permits the use of NOISOLATION.

---------------------
TPRESTNESTERR
---------------------

TPRESTNESTERR, TP restart signaled while handing error - treated as nested error - Use TROLLBACK in error handler to avoid this

Run Time Error: YottaDB does not allow a TP restart to occur either explicitly or implicitly while doing error processing ($ECODE is not NULL). Note that if a $ZINTERRUPT interrupts an error handler, this restriction then extends to the $ZInterrupt code even though $ECODE is temporarily nullified for the duration of the $ZInterrupt.

Action: Doing a TROLLBACK in the error handler before attempting to set any globals avoids this error.

------------------
TPTIMEOUT
------------------

TPTIMEOUT, Transaction timeout

Run Time Error: This indicates that the transaction took too long to process successfully. Timeouts prevent runaway processes, damage from software bugs, and transactions so large as to be hostile to other users.

Action: Review accompanying message(s) for more information about what caused the transaction timeout. If the transaction uses custom programs, routines, or functions, they may need to be debugged.

--------------------
TPTOODEEP
--------------------

TPTOODEEP, $TLEVEL cannot exceed 126

Run Time Error: This indicates that a TSTART attempted to initiate more concurrent subtransactions than YottaDB permits.

Action: Determine whether the transaction is implemented as designed. Modify the routine to reduce the levels of subtransaction nesting.

---------------------
TRACEON
---------------------

TRACEON, Missing global name (with optional subscripts) to dump M-tracing information into

Compile Time Error: This indicates that no global variable was supplied on a VIEW "TRACE":1:"^gvn".

Action: Supply the global variable.

---------------------
TRACINGON
---------------------

TRACINGON, Tracing already turned on

Run Time Information: This indicates that M profiling is already turned on when a command was issued to turn tracing on.

Action: -

------------------
TRANS2BIG
------------------

TRANS2BIG, Transaction exceeded available buffer space for region rrrr

Run Time Error: This indicates that a transaction updated more blocks than the global buffer could hold (half-2) for a particular region rrrr or accessed more than the single transaction limit of 64K blocks.

Action: Look for missing TCOMMIT commands; modify the code to reduce the total content or change content of the transaction. If the transaction is as intended and the issue is the number of updates, increase the GLOBAL_BUFFERS for the region using MUPIP SET, or modify the Global Directory to redistribute the relevant globals to more regions.  If this occurs on a replicating instance it may indicate either a difference in configuration between the originating and replicating instances, which probably should be addressed, or a transaction that was borderline on the originating instance, but failed on the replicating instance because of difference in the database layout. In the later case, consider examining the application code to see if it's possible to reduce the size of the transaction, or alternatively increase the global buffers on both the instances.

----------------
TRANSMINUS
----------------

TRANSMINUS, Negative numbers not allowed with ZTCOMMIT

Run Time Error: This indicates that a ZTCOMMIT must have a zero (0) or positive integer argument.

Action: Modify the ZTCOMMIT argument.

-----------------
TRANSNEST
-----------------

TRANSNEST, Maximum transaction nesting levels exceeded

Run Time Error: This indicates that a ZTSTART failed an attempt to establish another level of nested subtransactions.

Action: Rework the application so that it requires no more than 255 levels of journal transaction nesting.

------------
TRANSNOSTART
------------

TRANSNOSTART, ZTCOMMIT(s) issued without corresponding ZTSTART(s)

Run Time Error: This indicates that a ZTCOMMIT specified an explicit number of ZTSTARTs to close that exceeded the number of ZSTARTs that were currently open.

Action: Use ZTCOMMIT with an argument of 0 to close all open transactions, or modify the ZTCOMMIT to use a positive integer argument that does not exceed the number of open ZTSTARTs.

------------------------
TRANSREPLJNL1GB
------------------------

TRANSREPLJNL1GB, Transaction can use at most 1GiB of replicated journal records across all journaled regions

Run Time Error: This indicates that a database update transaction needs more than 1GiB of space to store the logical journal records corresponding to all updates in this transaction that target journaled database regions. Logical records are exactly the records that are also replicated in case the update happens on a database region that has not just journaling on but also replication turned on. Examples for logical journal records are `SET`, `KILL` etc. That is, all records corresponding to updates initiated by the user. Examples of journal records that are not logical are `PINI`, `PFIN`, `PBLK`, `ALIGN` etc. That is, all records internally generated by YottaDB to help with journaling the logical records.

Action: Decrease the number of updates done within one transaction thereby decreasing its total logical journal record requirement.

----------------
TRESTLOC
----------------

TRESTLOC, Transaction start: xxxx, Transaction failure: yyyy

Run Time Error: This indicates that YottaDB detected a resource conflict that attempted to RESTART a transaction that did not enable RESTART. This message follows TRESTNOT error message, xxxx is the starting location for the transaction and yyyy is the point of failure.

Action: Enable RESTART with the initial TSTART command argument, or add external LOCKs to serialize the transaction.


------------------
TRESTMAX
------------------

TRESTMAX, TRESTART not allowed in a final TP retry more than once

Run Time Error: The code contained one or more explicit TRESTART command(s) which called for a RESTART after more than once after $TRESTART reached 3. Once $TRESTART reaches 3, YottaDB switches from an optimistic to a conventional locking strategy so that such restarts are expensive and potentially disruptive. Therefore YottaDB limits the ability of the application code to repeatedly demand such RESTARTs.

Action: Investigate the cause and reason(s) for the explicit restart. When combined with a subsequent TROLLBACK, explicit TRESTARTs may provide a way to manipulate the variable context and flow of control in useful ways, but other uses are unusual. If explicit TRESTARTs are appropriate, consider whether they should be conditional on the value of $TRESTART.

----------------
TRESTNOT
----------------

TRESTNOT, Cannot TRESTART, transaction is not restartable

Run Time Error: This indicates that a TRESTART command attempted to RESTART a transaction that did not enable RESTART; the error occurs at the point of the initial TSTART, so all global updates within the transaction are rolled back and any local variables specified have their values as of the TSTART.

Action: Enable RESTART with the initial TSTART command argument, add external LOCKs to serialize the transaction, or eliminate the TRESTART command.

------------------
TRIG2NOTRIG
------------------

TRIG2NOTRIG, Sending transaction sequence number xxxx which used triggers to a replicator that does not support triggers.

MUPIP Warning: The source server encountered a transaction that includes triggers, but its replicating node does not support triggers. Unless you are using application level filters to handle this case, your originating instance and replicating instance are no longer consistent.

Action: If this case it not handled by your application level filters, you should either enhance your filters or upgrade the replicating instance to a version of YottaDB that supports triggers and load the the appropriate trigger definitions with MUPIP TRIGGER (or $ZTRIGGER()), and then take appropriate action (such as recreating the replicating instance from a backup of the originating instance) to restore consistency.

------------------
TRIGCOMPFAIL
------------------

TRIGCOMPFAIL, Compilation of database trigger named tttt failed

Trigger/Run Time Error: The -Xecute code of a trigger specification has syntax errors. Because triggers are precompiled when you define them, this error may indicate that either:

- A database upgrade was performed but the trigger code was not updated to eliminate obsolete syntax

- The portion of the database holding the trigger definitions may be corrupted

Action: Validate the definitions by a SELECT option with MUPIP TRIGGER or $ZTRIGGER(), correct the trigger code syntax and apply a trigger update.

-------------------
TRIGDATAIGNORE
-------------------

TRIGDATAIGNORE, Ignoring trigger data tttt. Use MUPIP TRIGGER to load trigger definitions

MUPIP Information: MUPIP LOAD displays this warning when it encounters trigger metadata during extract file processing (GO/ZWR extracts).

Action: Identify and remove trigger metadata information from GO/ZWR extract files and, if appropriate, process it with MUPIP TRIGGER or $ZTRIGGER().

----------------
TRIGDEFBAD
----------------

TRIGDEFBAD, Trigger initialization failed for global ^gggg. Error while processing ^#t("xxxx",yyyy[,zzzz])

Trigger/MUPIP Error: Missing or corrupted trigger metadata causes this error.

Action: Delete and replace defective triggers. If possible, analyze the cause of the trigger damage and report the incident to your YottaDB support channel.

-----------------
TRIGDEFNOSYNC
-----------------

TRIGDEFNOSYNC, Global ^gggg has triggers defined on the [originating/replicating] instance but none on the [replicating/originating] instance. Current journal sequence number is 0xjjjj

Update Process log/Trigger Warning: The Update Process detected that there is a mismatch in trigger definitions for global gggg between originating and replicating instances and sent this warning to the operator log.

Action: Differences in triggers between originating and replicating instances typically mean the replicating instance is not in a position to stand in as a system of record by becoming an originating instance. Unless the difference is intended because of a special use of the replicating instance, shutdown and resynchronize the replicating instance.

----------------
TRIGINVCHSET
----------------

TRIGINVCHSET, Trigger tttt for global gggg was created with CHSET=cccc which is different from the current $ZCHSET of this process

Trigger/Run Time Error: TRIGINVCHSET occurs when a process invokes a trigger on a global using a $ZCHSET that is different from the $ZCHSET used at the time of loading the first trigger on that global. YottaDB implicitly uses the $ZCHSET of the first trigger on a global to invoke all triggers on that global. Note that tttt is the name of the first trigger on the global gggg-not necessarily the name of the trigger being invoked. cccc is the $ZCHSET of the process at the time of loading tttt on global gggg.

Action: Ensure that the process invoking a trigger on a global uses the same $ZCHSET that was used to load the first trigger on that global. If your application requires triggers in both M and UTF-8 modes, use different globals to load M mode and UTF-8 mode triggers.

-----------------
TRIGIS
-----------------

TRIGIS, Trigger name: tttt

Trigger/Run Time Information: This message identifies a trigger name.

Action: Refer to the accompanying message(s) for more information.

--------------
TRIGLOADFAIL
--------------

TRIGLOADFAIL, MUPIP TRIGGER or $ZTRIGGER operation failed. Failure code: xxxx

Trigger/Run Time Error: This indicates that a trigger install (using $ZTRIGGER() or MUPIP TRIGGER) encountered a database problem when it attempted to update a global variable. xxxx contains the failure codes for the four attempts as documented in section R2 of the `Maintaining Database Integrity chapter in the Administration and Operations Guide <../AdminOpsGuide/integrity.html>`_. It is very likely that the database may have integrity errors or that the process-private data structures are corrupted.

Action: Report this database error to the group responsible for database integrity at your operation

------------------
TRIGMODREGNOTRW
------------------

TRIGMODREGNOTRW, Trigger(s) cannot be added/changed/deleted because region rrrr is read-only

Run Time Error: This error occurs when $ZTRIGGER() or MUPIP TRIGGER attempts to write to read-only region rrrr.

Action: Check for appropriate global directory mapping and appropriate permissions on the database file mapped to region rrrr.

-----------------
TRIGNAMBAD
-----------------

TRIGNAMBAD, Trigger initialization failed. Error while processing ^#t(tttt,cccc)

Run Time/MUPIP Error: A trigger operation encountered a trigger definition for a trigger with an apparent internal inconsistency while looking for characteristic cccc of type tttt.

Action: Delete and redefine the trigger in question. Consult with the group responsible for database integrity at your operation to discuss what actions might have led to this error.

----------------
TRIGNAMENF
----------------

TRIGNAMENF, Trigger name nnnn not found with the current default global directory

Run Time/MUPIP Error: This message indicates that a trigger lookup by name failed. YottaDB has a name cross reference in the default region of the current global directory. If you use multiple global directories with different default regions, trigger lookups by name, such as $ZTRIGGER(), ZBREAK and ZPRINT, only work when they use the same default region as the one in use at the time of the trigger definition.

Action: Consider using a SET $ZGBLDIR to change to an appropriate global directory when using name lookup. Also consider restructuring your global directories so they share a common default region.

-------------------
TRIGNAMEUNIQ
-------------------

TRIGNAMEUNIQ, Unable to make trigger name tttt unique beyond vvvv versions already loaded

Trigger/Run Time Error: YottaDB encountered more than vvvv different instances of the same trigger name across database regions used by the same process.

Action: Revise trigger names to prevent such a high degree of overlap.

----------------
TRIGREPLSTATE
----------------

TRIGREPLSTATE, Trigger cannot update replicated database file dddd since triggering update was not replicated

Run Time Error: A process performed an update on a global in a database region which is not currently replicated, and that update invoked a trigger that in turn attempted an update on a global in a database region that is replicated. This would produce a journal state with insufficient information to properly recover the replicated region.

Action: Investigate whether the global directories, journaling characteristics or trigger logic need revision.

-------------------
TRIGSUBSCRANGE
-------------------

TRIGSUBSCRANGE, Trigger definition for global ^gggg has one or more invalid subscript range(s) : ssss

YottaDB/MUPIP error: This error indicates one or more subscript range(s) of of order given the current collation subscript ordering - for global gggg in tthe trigger definition files.

Action: Verify the validity of subscript ranges in trigger definition file for the particular global, taking its collation into account, and redefine the trigger with correct subscript ranges for the collation of the global in question.

-------------------
TRIGTCOMMIT
-------------------

TRIGTCOMMIT, TCOMMIT at $ZTLEVEL=LLLL not allowed as corresponding TSTART was done at lower $ZTLEVEL=BBBB

Trigger/Run Time Error: A TCOMMIT in trigger logic attempted to complete the active transaction that was started outside of the current trigger. Because trigger actions are atomic with the update initiating them, committing a transaction started prior to or by the triggering update cannot be committed inside the trigger.

Action: Within the trigger context, review the TCOMMIT logic to ensure that it commits only those transactions that are started within the trigger. Ensure that TCOMMIT does not attempt to commit any transaction started prior to or by the triggering update.

------------------
TRIGTLVLCHNG
------------------

TRIGTLVLCHNG, Detected a net transaction level ($TLEVEL) change during trigger tttt. Transaction level must be the same at exit as when the trigger started

Trigger/Run Time Error: While the trigger logic can use balanced sub-transactions, it cannot cause a net change in $TLEVEL.

Action: Review the transaction management (TSTART, TCOMMIT and TROLLBACK) within trigger logic to ensure that it commits or rolls back any transactions it starts and does not attempt to commit any transaction started prior to, or by, the trigger update. You can use TROLLBACK within trigger logic to block the current transaction, possibly to write error context information. Nonetheless if you use such a TROLLBACK, YottaDB subsequently signals this error when you leave the trigger context in order to notify the process that the original triggering update has been discarded.

------------------
TRIGUPBADLABEL
------------------

TRIGUPBADLABEL, Trigger upgrade cannot upgrade label NNNN to CCCC on ^GGGG in region RRRR

MUPIP Error: MUPIP TRIGGER UPGRADE cannot upgrade trigger version number NNNN to the current version number CCCC for the global GGGG in region RRRR.

Action: Reload the triggers in region RRRR.

---------------
TRIGZBREAKREM
---------------

TRIGZBREAKREM, ZBREAK in trigger tttt removed due to trigger being reloaded

Run Time Warning: This indicates that your process had a ZBREAK defined within the XECUTE code for trigger tttt, but some action replaced the definition for trigger tttt so YottaDB removed the ZBREAK.

Action: If appropriate, examine the trigger with ZPRINT and reestablish the ZBREAK. The message is tied to BREAKMSG mask 16 (See VIEW BREAKMSG). The default message mask is 31, which includes masks 1, 2, 4, 8, and 16. Using the VIEW command to set the BREAKMSG mask to 7 or any other pattern that excludes 16 disables this message.

-------------------
TRNLOGFAIL
-------------------

TRNLOGFAIL, Translation of environmental variable xxxx failed

Compile Time Error: This indicates that the translation of the indicated environment variable failed. The message is accompanied with another message describing the failure type and the reason behind it. Most probable cause of the failure could be resource limitation.

Action: Report the error to your system administrator.

----------------
TROLLBK2DEEP
----------------

TROLLBK2DEEP, Intended rollback (xxxx) deeper than the current $tlevel (yyyy)

Run Time Error: This indicates an attempt to TROLLBACK more levels (as indicated by $TLEVEL) of transaction nesting than are currently active.

Action: Review the logic and code path that led to the error and modify the code appropriately.

-----------------
TRUNCATE
-----------------

TRUNCATE, Error while truncating jnl-file xxxx to length aaaa

Run Time Error: This message is issued by the Recovery/Rollback process when it is unable to truncate the journal file at the end of recovery. The journal file remains in the same state as it was before the processing and may contain incomplete data, in case of a YottaDB crash.

Action: Look into the secondary error message and if necessary report the entire incident context to your YottaDB support channel for further analysis.


-----------------
TSTRTPARM
-----------------

TSTRTPARM, Error parsing TSTART qualifier

Compile Time Error: This indicates that the TSTART specified an improperly formatted argument.

Action: Modify the TSTART argument.

-----------------
TTINVFILTER
-----------------

TTINVFILTER, Invalid FILTER argument

Run Time Information: This indicates that the FILTER= deviceparameter appeared in a device command for a terminal with an argument that evaluated to something other than "[NO]CHARACTERS" or "ESCAPE", which are the only valid arguments.

Action: Remove or rework the FILTER=.

---------------
TTLENGTHTOOBIG
---------------

TTLENGTHTOOBIG, Terminal LENGTH exceeds the maximum allowed limit

Run Time Error: The LENGTH specified is too large for a terminal device. The maximum page length is 255.

Action: Modify the USE to specify a smaller LENGTH.

---------------
TTWIDTHTOOBIG
---------------

TTWIDTHTOOBIG, Terminal WIDTH exceeds the maximum allowed limit

Run Time Error: The WIDTH specified is too large for a terminal device. The maximum page width is 511.

Action: Modify the USE to specify a smaller WIDTH.

-----------------
TXTSRCFMT
-----------------

TXTSRCFMT, $TEXT encountered an invalid source program file format

Run Time Error: This indicates that a $TEXT function encountered an improperly formatted source file. This error only occurs when the source file has been corrupted.

Action: Use a host system command to examine the source file. Determine whether the source file was properly maintained.

----------------
TXTSRCMAT
----------------

TXTSRCMAT, M object module and source file do not match

Run Time Error: This indicates that a $TEXT function referenced a routine whose source file does not correspond to the current object file. ZPRINT may deliver this message as a warning.

Action: ZLINK the source file or use host shell commands to rearrange the current source file to match the object file.

-----------------
UIDMSG
-----------------

UIDMSG, Unidentified message received

Run Time Error: This indicates that the process was performing a JOB command and received an unanticipated message while attempting to communicate with the JOBbed process.

Action: Look for indiscriminate mailbox use by other processes.

-----------------
UIDSND
-----------------

UIDSND, Unidentified sender PID

Run Time Error: This indicates that the process was performing a JOB command and received a message from an unidentified source while attempting to communicate with the JOBbed process.

Action: Look for indiscriminate mailbox use by other processes.

---------------
UNIMPLOP
---------------

UNIMPLOP, Unimplemented construct encountered

Run Time Error: This indicates that YottaDB encountered an unsupported data type while passing arguments between typed C and type-less M.

Action: Review the call-in table and ensure that the parameter types match the following table:

+-------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
| Directions                    |  Allowed Parameter Types                                                                                                               |
+===============================+========================================================================================================================================+
| I                             | ydb_long_t, ydb_ulong_t, ydb_float_t, ydb_double_t,_ydb_long_t*, ydb_ulong_t*, ydb_float_t*, ydb_double_t*,_ydb_char_t*, ydb_string_t* |
+-------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
| O/IO                          | ydb_long_t*, ydb_ulong_t*, ydb_float_t*, ydb_double_t*,_ydb_char_t*, ydb_string_t*                                                     |
+-------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+

-----------------
UNIQNAME
-----------------

UNIQNAME, Cannot provide same file name (nnnn) for ffff and FFFF

MUPIP Error: The command species the same name, nnnn for both output ffff and output FFFF.

Action: Revise the command to use unique names for different outputs.

---------------
UNKNOWNFOREX
---------------

UNKNOWNFOREX, Process halted by a forced exit from a source other than MUPIP

Run Time Warning: This indicates that a process has been terminated by a source (usually a user program) other than MUPIP.

Action: Investigate why an operator or program is stopping YottaDB processes without using MUPIP STOP.

-----------------
UNKNOWNSYSERROR
-----------------

UNKNOWNSYSERR, ssss does not correspond to a known YottaDB error code: <errorstring>

Runtime Error:  An unrecognized error code has been passed to ydb_message_t() (C) or MessageT() (Golang)

Action: Determine the source and meaning of the unknown message code such that a proper error message code can be passed in.

-----------------
UNSDCLASS
-----------------

UNSDCLASS, Unsupported descriptor class

Run Time Error: This indicates that an external call or $ZCALL function encountered an unsupported argument-passing mechanism in the external call table.

Action: Make sure the argument-passing mechanism is spelled correctly in the external call table.

-----------------
UNSDDTYPE
-----------------

UNSDDTYPE, Unsupported descriptor data type

Run Time Error: This indicates that an external call or $ZCALL function encountered an unsupported data type in the external call table.

Action: Review the external call table for invalid data types.

-------------------
UNSETENVFAIL
-------------------

UNSETENVFAIL, VIEW "UNSETENV":"eeee" failed in unsetenv() system call

Run Time Error: This indicates that a unsetenv() system call failed for the environment variable named eeee.

Action: Examine the accompanying SYSCALL error message which has more detail on the error returned by the unsetenv() call.

-------------------
UNSOLCNTERR
-------------------

UNSOLCNTERR, An unsolicited error message has been received from the network

Run Time Error: This indicates that a GT.CM component received a network message from an unknown source.

Action: Ensure that no agents outside the GT.CM environment are improperly sending messages to GT.CM.


------------------
UPDATEFILEOPEN
------------------

UPDATEFILEOPEN, Update file open error

Run Time Error: This indicates that file permissions were either set incorrectly or replication was turned off.

Action: Review accompanying message(s) for additional information. Correcting accompanying messages should correct this message.

----------------
UPDPROC
----------------

UPDPROC, Update Process error

MUPIP Error: YottaDB replication was not able to start the update process.

Action: Ensure that the ydb_dist environment variable points to a valid YottaDB distribution that is executable by the user.

------------------
UPDREPLSTATEOFF
------------------

UPDREPLSTATEOFF, Error replicating global gggg as it maps to database xxxx which has replication turned OFF.

MUPIP Error: This indicates that the update process encountered an update record in the replication pipe that is destined for a non-replicated database file. YottaDB does not allow such updates because they make the journal sequence numbers go out of sync between the replication primary and secondary. After issuing the message, the update process shuts down immediately. No more updates are processed on the secondary until replication is turned ON in the mentioned database file.

Action: Shut down the secondary instance including the receiver server, passive source server and any other helper processes accessing this instance. Turn replication ON in the mentioned database. Restart the secondary instance. The update process should now be able to process the update successfully.

-------------------
UPDSYNC2MTINS
-------------------

UPDSYNC2MTINS, Can only UPDATERESYNC with an empty instance file

MUPIP Error: Issued by a Receiver Server started with the -UPDATERESYNC qualifier on a non-supplementary instance or on a Supplementary Instance started specifying the -UPDNOTOK qualifier, when the replication instance file on the Receiver side contains at least one history record. The purpose of -UPDATERESYNC is to unconditionally declare the instance state as a valid state in the set of current and prior states of the originating instance disregarding any history. Note that the receiver server on a Supplementary Instance started with -UPDOK does not issue this error.

Action: Verify that -UPDATERESYNC is appropriate and if so, recreate the instance file to discard the history then reissue the command. If the replication state is not a valid match to some available current or prior state of the originating instance, either do a normal resync or refresh the replicating instance to an appropriate state.

--------------------
UPDSYNCINSTFILE
--------------------

UPDSYNCINSTFILE, Error with instance file name specified in UPDATERESYNC qualifier

Receiver Server log/MUPIP Error: Issued by a Receiver Server when started with the -UPDATERESYNC qualifier in case of any error while processing the instance file specified as a value to this qualifier.

Action: An accompanying message, usually a YDB-I-TEXT message, describes the particular error situation. Take appropriate corrective action based on that.

-------------------
USRIOINIT
-------------------

USRIOINIT, User-defined device driver not successfully initialized

Run Time Error: This indicates that a user-implemented mnemonicspace driver image did not call the initialization entry in YottaDB.

Action: Modify the user-supplied image to adhere to the mnemonicspace image-calling convention.

-------------------
UTF16ENDIAN
-------------------

UTF16ENDIAN, The device previously set UTF-16 endianness to cccc and cannot change to eeee

Run Time Error: YottaDB does not permit changing endian ordering on an OPEN device.

Action: Check for a logic error and for appropriate device set up on the OPEN command. If the input has changed its characteristic, CLOSE and re-OPEN the device.

-------------------
VALTOOBIG
-------------------

VALTOOBIG, xxxx is larger than the maximum of yyyy for a zzzz

GDE Error: This indicates that GDE has encountered a qualifier value that exceeds the maximum allowed. xxxx is the value that is too big. yyyy is the maximum value accepted. zzzz is the qualifier.

Action: Specify a qualifier value that is less than the maximum allowed.

-----------------
VALTOOLONG
-----------------

VALTOOLONG, xxxx exceeds the maximum length of yyyy for a zzzz

GDE Error: This indicates that GDE encountered a value that exceeds the maximum length allowed for an object name or qualifier. xxxx is the value that is too long. yyyy is the maximum length. zzzz is the object-name or qualifier.

Action: Specify an object-name or qualifier that is shorter than the maximum length.

---------------
VALTOOSMALL
---------------

VALTOOSMALL, xxxx is less than the minimum of yyyy for a zzzz

GDE Error: This indicates that GDE encountered a value that is less than the minimum allowed for a qualifier. xxxx is the value that is too small. yyyy is the minimum limit. zzzz is the object-name or qualifier.

Action: Specify a value that is larger than the minimum requirement for the object-name or qualifier.

------------------
VALUEBAD
------------------

VALUEBAD, xxxx is not a valid yyyy

GDE Error: This indicates that GDE encountered something other than the valid syntax element it was expecting. xxxx is the invalid element. yyyy is the valid element type.

Action: Specify a valid element. This error occurs if GDE is expecting an element (such as a file-specification, qualifier, or number) but receives a value that does not evaluate to the expected element type.

------------------
VALUEREQD
------------------

VALUEREQD, Qualifier xxxx requires a value

GDE Error: This indicates that GDE encountered a qualifier with a missing value. xxxx is the qualifier with a missing value.

Action: Supply a value for the qualifier.

------------------
VAREXPECTED
------------------

VAREXPECTED, Variable expected in this context

Compile Time Error: This indicates that YottaDB expected a variable but encountered an invalid one.

Action: Look for proper variable names. This error is reported by commands and functions that require a variable argument such as SET and KILL and $DATA() and $QUERY().

-------------------
VARNAME2LONG
-------------------

VARNAME2LONG, Variable name length exceeds maximum allowed length xxxx.

Run Time Error: This indicates that the length of a variable name specified in a SimpleAPI call exceeded the maximum limit, even though the same name could work in M due to silent truncation. The maximum value is identified in the message text.

Action: Specify the variable name within the maximum length limit and retry the SimpleAPI call.

------------------
VARRECBLKSZ
------------------

VARRECBLKSZ, Blocksize must be at least record size + 4 bytes

Run Time Error: This indicates that an OPEN command attempted to initialize a variable-length magnetic tape or sequential disk file with a RECORDSIZE of less than 5 bytes.

Action: Modify the routine to use a larger record size or use FIXED length records. The minimum variable RECORDSIZE reflects a single byte of data and 4 bytes of overhead in every variable-length record.

--------------------
VERIFY
--------------------

VERIFY, Verification xxxx

GDE Information: This indicates that an EXIT or VERIFY command caused GDE to verify the GDE mappings. xxxx is "OK" if the verification was successful or "BAD" if the verification failed.

Action: If the mappings are valid, GDE displays a confirmation message. GDE terminates the GDE session if the verification is OK on an EXIT command. If the mappings are not valid, review the accompanying message(s) for additional information.

------------------
VERMISMATCH
------------------

VERMISMATCH, Attempt to access xxxx with version yyyy, while already using zzzz

Run Time Error: This indicates that two different versions of YottaDB attempted to access the same database at the same time.

Action: Update YottaDB so that both systems use the same version of YottaDB.

-------------------
VERSION
-------------------

VERSION, Version mismatch - This program must be recompiled

Run Time Error: This indicates that the process attempted to activate a routine that was compiled with an incompatible version of YottaDB.

Action: Recompile the routine or use a compatible version of YottaDB.

---------------------
VIEWAMBIG
---------------------

VIEWAMBIG, View parameter xxxx is ambiguous

Run Time Error: This indicates that the argument xxxx for a VIEW command or $VIEW function is ambiguous due to insufficient characters.

Action: Add enough characters to make the argument unambiguous.


--------------------
VIEWARGCNT
--------------------

VIEWARGCNT, View parameter xxxx has inappropriate number of subparameters

Run Time Error: The argument xxxx for a VIEW command or a $VIEW function has too many or too few sub-arguments.

Action: Modify the argument so it has the proper number of sub-arguments.


--------------------
VIEWCMD
--------------------

VIEWCMD, View parameter pppp is not valid with the VIEW command

Run Time Error: This indicates that the VIEW command has an argument that is only valid with the $VIEW() function.

Action: Modify the argument.

-------------------
VIEWFN
-------------------

VIEWFN, View parameter is not valid with the VIEW command

Run Time Error: This indicates that the $VIEW() function has an argument pppp that is only valid with the VIEW command.

Action: Modify the argument.

--------------
VIEWGVN
--------------

VIEWGVN, Invalid global key name used with VIEW/$VIEW(): xxxx

Run Time Error: This indicates that $VIEW("REGION":gvn) failed because of an invalid global variable name xxxx in a sub-argument.

Action: Modify the sub-argument to be a valid M global variable name.

------------------
VIEWLVN
------------------

VIEWLVN, Invalid local variable name used with VIEW or $VIEW(): vvvv

Run Time Error: This indicates the argument for a VIEW command or $VIEW() function required a local variable name, but it (vvvv) was either missing or invalid.

Action: Correct the code or investigate the logic to determine why the local variable in question is not in the expected state.

-----------------
VIEWNOTFOUND
-----------------

VIEWNOTFOUND, View parameter xxxx not valid

Run Time Error: This indicates that the VIEW command or $VIEW() function has an invalid argument xxxx.

Action: Modify the argument.

------------------
WAITDSKSPACE
------------------

WAITDSKSPACE, Process xxxx will wait aaaa seconds for necessary disk space to become available for yyyy

Run Time Error: This indicates that the database yyyy is full and cannot extend due to lack of space. All updates are suspended. Failure to address this message in the specified seconds will result in an OUTOFSPACE error.

Action: Immediately make enough space for the database to extend, and if the space is still not sufficient, stop all processes until more space is available. Examine your space management procedures and take actions to prevent any reoccurrence of this error.

--------------------
WCBLOCKED
--------------------

WCBLOCKED, Field xxxx is set by process yyyy at transaction number aaaa for database file zzzz

MUPIP Warning: This indicates that the current state of the global buffer cache necessitates the need for cache recovery. The next process to attempt to obtain the critical lock on the database will perform the recovery.

Action: Refer to the accompanying message(s).

-----------------
WCERRNOTCHG
-----------------

WCERRNOTCHG, Not all specified databases were changed

MUPIP Error: This indicates that SET could not modify database characteristics for at least one database.

Action: Review the accompanying message(s) for additional information.

--------------
WCSFLUFAILED
--------------

WCSFLUFAILED, EEEE error while flushing buffers at transaction number TTTT for database file DDDD

Run Time Error: For a BG database file, this means that a process attempting to flush modified global buffers to disk encountered an error. EEEE is the error it encountered, for database file DDDD when attempting to flush the blocks for database transaction number TTTT. This is usually accompanied by other messages that can together help provide more information and context. If you need further assistance and have purchased support, contact your YottaDB support channel.

Action: Refer to the description of error EEEE and take appropriate action.

------------------
WCWRNNOTCHG
------------------

WCWRNNOTCHG, Not all specified databases were changed

MUPIP Warning: This indicates that a SET command could not modify database characteristics for a particular database.

Action: Review the accompanying message(s) for additional information.

----------------
WEIRDSYSTIME
----------------

WEIRDSYSTIME, Time reported by the system clock is outside the acceptable range. Please check and correct the system clock.

Run Time Error: Time reported by the system clock is outside the acceptable range. Please check and correct the system clock.

Action: YottaDB requires that the system time be set to be between January 1, 1970 00:00:00 UTC (the UNIX epoch) and September 27, 33658 01:46:40 UTC

------------------
WIDTHTOOSMALL
------------------

WIDTHTOOSMALL, WIDTH should be at least 2 when device ICHSET or OCHSET is UTF-8 or UTF-16.

Run Time Error: This error is issued whenever the ICHSET or OCHSET of the current terminal or file device is UTF-8 or UTF-16 and the WIDTH specified is 1. The minimum width allowed is 2 for such devices.

Action: Specify a width that is 2 or greater.

-----------------
WILDCARD
-----------------

WILDCARD, Wild cards are prohibited: xxxx

Run Time Error: This indicates that a $ZROUTINES function that does not allow wild cards encountered one in xxxx.

Action: Replace the wild card with a specific directory name.

---------------------
WORDEXPFAILED
---------------------

WORDEXPFAILED, wordexp() call for string ssss returned tttt error. See wordexp() man pages for details

Run Time Error: This indicates that the command $VIEW("WORDEXP",tttt), received the tttt error code from the OS wordexp() function which it called.

Action: Refer to the OS wordexp() documentation to identify the error and correct the application accordingly.

WORDEXPFAILED was added to YottaDB effective release `r1.32 <https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.32>`_.

------------------
WRITEERROR
------------------

WRITEERROR, Cannot exit because of write failure. Reason for failure: xxxx.

GDE Error: This indicates that GDE was unable to write a new or revised global directory. xxxx text describes the failure.

Action: Review the text for additional information. If the failure is due to file permissions, it may be possible to correct it from the shell, accessed by means of the SPAWN command or another session. If the reason is not tractable, QUIT from GDE and re-invoke it after correcting the problem.

------------------
WRITERSTUCK
------------------

WRITERSTUCK, Buffer flush stuck waiting for [xxxx] concurrent writers to finish writing to database file aaaa

Run Time Error: This indicates that YottaDB timed out after waiting nearly a minute for concurrent processes to complete flushing modified global buffers to the disk.

Action: This is usually symptomatic of a stressed I/O subsystem, where disk writes take a long time. System Administration might be warranted to improve the performance.

------------------
WRITEWAITPID
------------------

WRITEWAITPID, PID wwww waited mmmm minute(s) for PID hhhh to finish writing block bbbb in database file ffff

Run Time Warning: This operator log message indicates process wwww needed access to block bbbb in database file ffff, but had waited mmmm minutes for process hhhh to finish with that block. mmmm exceeds the expected design criteria for the processing by hhhh.

Action: Investigate the state and activities of process hhhh (possibly using the ydb_procstuckexec facility); try to identify any coincident operating system, file system or storage sub-system issues that might contribute to this unexpected behavior.

------------------
XCRETNULLREF
------------------

XCRETNULLREF, Returned null reference from external call LLLL

Call out Error: The code invoked as externroutinename LLLL returned a NULL pointer. While YottaDB accepts returns of a zero (0) value or an empty string, it does not support the return of a NULL pointer.

Action: Revise the external call code to return a pointer to an appropriate value.

------------------
XCVOIDRET
------------------

XCVOIDRET, Attempt to return a value from function xxxx, which is declared void in external call table yyyy

Run Time Error: This indicates that the specified function was typed as void in call table yyyy, but when the function was executed it attempted to return a value.

Action: Modify either the function to make it void, or the table to declare the proper type for the function.

-----------------
XTRNRETSTR
-----------------

XTRNRETSTR, Return string from extended reference translation algorithm is NULL.

Run Time Error: This indicates that NULL was returned instead of a string from the user specified global variable name environment translation routine.

Action: Correct the user environment translation algorithm to return a string (which can be empty). Restart YottaDB for the changes to become effective.

----------------
XTRNRETVAL
----------------

XTRNRETVAL, Length of return value from extended reference translation algorithm is out of bound

Run Time Error: This indicates that the return string from the user specified global variable name environment translation routine is of an invalid length (i.e. greater than 32767).

Action: Correct the user environment translation algorithm to return a string of valid length (in the 0-32767 range). Restart YottaDB for the changes to become effective.

----------------
XTRNTRANSDLL
----------------

XTRNTRANSDLL, Error during extended reference environment translation. Please check the above message.

Run Time Error: This indicates that the external object (dynamically linked library in UNIX), which holds the global variable name environment translation routine, or the entry point ydb_env_translate in this object, is not accessible.

Action: Check if the value of the ydb_env_translate points to a valid dll object, which has the entry point ydb_env_translate.

-------------------
XTRNTRANSERR
-------------------

XTRNTRANSERR, Error attempting to generate an environment using an external algorithm.

Run Time Error: This indicates that the external environment translation routine returned an error.

Action: Check the external routine and the conditions it errors on. A supplementary TEXT message is printed if more information is provided by the external environment translation routine.

------------------
ZATRANSERR
------------------

ZATRANSERR, The input string is too long to convert

Run Time Error: The first (expression) argument to a $ZATRANSFORM() produces a result that exceeds the maximum key length.

Action: Analyze the logic to determine if the argument is correct. If you need to produce translations that exceed the maximum key length, you must use $ZCOLLATE() or break them into chunks to avoid this error. Note that some transforms may use context such that selecting the chunks requires an understanding of the transform.

-------------------
ZATTACHERR
-------------------

ZATTACHERR, Error attaching to xxxx

Run Time Error: This indicates that a ZATTACH command failed because the argument specified a process xxxx that did not exist or was otherwise ineligible for attachment.

Action: Verify that the process exists within the job. Verify the spelling of the process name.

-------------------
ZBREAKFAIL
-------------------

ZBREAKFAIL, Could not set breakpoint at xxxx due to insufficient memory

Run Time Information: This indicates that a ZBREAK command failed to set a breakpoint at the entry reference xxxx due to lack of memory. No breakpoint will be effective when control reaches the given entry reference.

Action: Check for very large local variables. The memory requirement for ZBREAK is proportional to the size of the routine containing the entry reference. If the routine is large, divide the routine into smaller routines thereby reducing the memory requirement for ZBREAK. If appropriate, increase the memory quota for the user.

-------------------
ZCALLTABLE
-------------------

ZCALLTABLE, External call: Table format error

Run Time Error: This indicates that an external call or $ZCALL() function encountered an error in the external call table.

Action: Modify the external call table. This message is usually displayed with another message.

-------------------
ZCARGMSMTCH
-------------------

ZCARGMSMTCH, External call: Actual argument count, xxxx is greater than formal argument count, yyyy

Run Time Error: This indicates that an external call or $ZCALL() function specified xxxx arguments, which is more than the number yyyy, which is defined by the corresponding external call table entry.

Action: Ensure that the number of arguments in the external call matches the number of arguments in the external call table.

------------------
ZCCLNUPRTNMISNG
------------------

ZCCLNUPRTNMISNG, External call: Cleanup routine name missing. Cannot continue

Run Time Error: This error occurs when there is no value assigned to the GTMSLIBEXIT (shared library exit handler keyword) in the external call table.

Action: Either remove the line that contains GTMSLIBEXIT= or provide a suitable function name in the external call table.

------------------
ZCCOLON
------------------

ZCCOLON, Colon expected but not found

Run Time Error: This indicates that there is a syntax error in the table designated by the accompanying message.

Action: Refer to the accompanying message(s) and correct the syntax error.

------------------
ZCCONMSMTCH
------------------

ZCCONMSMTCH, External call: Too many input arguments

Run Time Error: This indicates that an external call or $ZCALL() function specifies more input arguments than the matching entry in the external call table.

Action: Ensure that the number of input arguments in the external call matches the number of input arguments in the external call table.

-------------------
ZCCONVERT
-------------------

ZCCONVERT, External call: error converting output argument from external call

Run Time Error: This indicates that an external call failed because an output argument supplied by the external routine did not match the corresponding output description in the external call table.

Action: Change the external call table or the called routine so that they correspond.

------------------
ZCCSQRBR
------------------

ZCCSQRBR, Closing Square bracket expected

Compile Time Error: Brackets specify the beginnings and ends of pre-allocations. This error indicates that a pre-allocation was begun with a bracket, but was not closed.

Action: Correct the syntax by placing a closing square bracket at the end of the pre-allocation.


------------------
ZCCTENV
------------------

ZCCTENV, Environmental variable for external package xxxx not set

Compile Time Error: This indicates that the program made an external call, but could not find the external call table as specified in the UNIX environmental variable ydb_xc.

Action: If this calls the default external call table, locate the external call table and specify the correct path in the UNIX environmental variable ydb_xc. Otherwise, you can point to the package table in the environmental variable or specify the package name in the program ydb_xc_[PACKAGE_NAME].

------------------
ZCCTNULLF
------------------

ZCCTNULLF, External call table contains no records: xxxx

Compile Time Error: This indicates that the program found the requested call table, but could not find external call data.

Action: Verify that the program calls the correct call table. If the table name is correct, add external call data.

-------------------
ZCCTOPN
-------------------

ZCCTOPN, Unable to open external call table: xxxx

Compile Time Error: This indicates that the program found the external call table but did not have permission to open it.

Action: Verify that the external call table and the user have appropriate permissions.

------------------
ZCENTNAME
------------------

ZCENTNAME, No entry found in external call table

Run Time Error: This indicates that the entry point the program is trying to access cannot be found.

Action: Create a corresponding entry point and match that link with the interface found in the C library.

-----------------
ZCINPUTREQ
-----------------

ZCINPUTREQ, External call: Required input argument missing

Run Time Error: This indicates that an external call or $ZCALL function did not specify an input argument that is defined in the external call table as required.

Action: Change the external call table or the called routine so that they correspond.

-----------------
ZCINVALIDKEYWORD
-----------------

ZCINVALIDKEYWORD, External call: Invalid keyword found. Cannot continue. Invalid keyword encountered in the ext call config file.

Run Time Error: This error occurs when the keyword for the shared library exit handler configuration is wrongly spelled. The correct keyword is "GTMSHLIBEXIT".

Action: Make sure that GTMSHLIBEXIT is correctly spelled in the external call table.

----------------
ZCMAXPARAM
----------------

ZCMAXPARAM, Exceeded maximum number of external call parameters

Run Time Error: YottaDB allows a maximum of 31 parameters for a single external call. This error message indicates that the external call has exceeded this number.

Action: Break up the external call into two or more external calls or rewrite the program to pass a valid number of parameters.

-----------------
ZCMLTSTATUS
-----------------

ZCMLTSTATUS, Multiple entries of xc_status in a single entry in external call table

Run Time Error: This indicates that a call definition contains more than one XC_STATUS.

Action: Check parameters for multiple occurences of TYPE XC_STATUS.

------------------
ZCNOPREALLOUTPAR
------------------

ZCNOPREALLOUTPAR, Parameter xxxx in external call yyyy.zzzz is an output only parameter requiring pre-allocation.

Run Time Error: This indicates that a pre-allocation value was not specified for the output only parameter xxxx in package yyyy, external call zzzz.

Action: Specify a pre-allocation value for the output only parameter xxxx. A package designation of "<DEFAULT>"indicates the default package rather than an actual package name.

-----------------
ZCOPT0
-----------------

ZCOPT0, External call: Qualifier OPTIONAL_0 can be used only with mechanisms REFERENCE or DESCRIPTOR

Run Time Error: This indicates that an external call or $ZCALL function encountered an external call table input line that contained an OPTIONAL_0 QUALIFIER with a MECHANISM other than REFERENCE or DESCRIPTOR.

Action: Verify the type and spelling of the specified argument-passing mechanism in the external call table entry.

------------------
ZCPOSOVR
------------------

ZCPOSOVR, External call: Invalid overlapping of arguments in table position xxxx

Compile Time/Run Time Error: This indicates that an external call or $ZCALL function encountered input and output descriptions for the same position, but one or both descriptions had invalid mechanisms or types.

Action: Modify the mechanism type and/or position it to remove the conflict.

-----------------
ZCPREALLNUMEX
-----------------

ZCPREALLNUMEX, Pre-allocation value should be a decimal number

Compile Time Error: This indicates that YottaDB can only accept pre-allocation values between zero (0) to nine (9).

Action: Specify a pre-allocation value between zero and nine.

------------------
ZCPREALLVALINV
------------------

ZCPREALLVALINV, The pre-allocation value exceeded the maximum string length

Run Time Error: Pre-allocation value of an output parameter in the external call table is greater than the maximum allowed limit (i.e. maximum string size plus one byte for terminating NULL).

Action: In the external call table, modify the pre-allocation value to less than or equal to 1,048,577.

------------------
ZCPREALLVALPAR
------------------

ZCPREALLVALPAR, Pre-allocation allowed only for variables passed by reference

Compile Time Error: This indicates that the program specified a pre-allocation for a scalar variable passed by value.

Action: Determine if the program should use a pre-allocation or should be passed by value. If it uses a pre-allocation, the variable must be passed by reference. If the variable must be passed by value, the program cannot use a pre-allocation for that variable.

-------------------
ZCRCALLNAME
-------------------

ZCRCALLNAME, Routine name expected but not found

Run Time Error: This indicates that the compiler encountered an open parenthesis (indicating that parameters are listed, but found no function, which the parameters could modify).

Action: Check that the function name has been specified correctly. The function may not have been created, or the program uses an incorrect function name, or the function name may be missing.

-------------------
ZCRPARMNAME
-------------------

ZCRPARMNAME, Parameter name expected but not found

Run Time Error: This indicates that the program specified a direction and included a colon indicating that parameters would follow, but no parameters were found.

Action: The compiler indicates where the error occurred. Review the code to determine if parameters are needed or the colon should be removed.

-----------------
ZCRTENOTF
-----------------

ZCRTENOTF, External call routine xxxx not found

Compile Time/Run Time Error: This indicates that YottaDB could not locate routine xxxx.

Action: Relink your external call descriptor image to include the missing routine.

----------------
ZCRTNTYP
----------------

ZCRTNTYP, Unknown return type

Compile Time Error: This indicates that the program specified an unrecognized return type. The compiler indicates where the invalid return type was found.

Action: Review the external calls table documentation for valid return types.

---------------
ZCSTATUSRET
---------------

ZCSTATUSRET, External call returned error status

Run Time Error: This indicates that the called program may contain a logic error.

Action: Review accompanying messages for more information about the cause of this error.

-----------------
ZCUNAVAIL
-----------------

ZCUNAVAIL, Package, xxxx unavailable

Run Time Error: This indicates that the shared library may not be specified in the external call table or the path may be specified incorrectly.

Action: Verify that the external call table and the program point to the correct shared library and path.

--------------------
ZCUNKMECH
--------------------

ZCUNKMECH, External call: Unknown parameter-passing mechanism

Run Time Error: This indicates that an external call or $ZCALL() function encountered an unsupported argument-passing MECHANISM in the external call table.

Action: Verify the spelling of the argument-passing MECHANISM values in the external call table.

------------------
ZCUNKQUAL
------------------

ZCUNKQUAL, External call: Unknown input qualifier

Run Time Error: This indicates that an external call or $ZCALL() function encountered an unsupported input QUALIFIER in the external call table.

Action: Verify the spelling of the input QUALIFIER value in the external call table.

---------------
ZCUNKTYPE
---------------

ZCUNKTYPE, External call: Unknown argument type

Run Time Error: This indicates that an external call or $ZCALL() function encountered an unsupported argument TYPE in the external call table.

Action: Verify the spelling of the argument TYPE in the external call table.

------------------
ZCUNTYPE
------------------

ZCUNTYPE, Unknown type entered

Run Time Error: This indicates that the program used an invalid external call parameter.

Action: Refer to the `Programmer's Guide <../ProgrammersGuide/index.html>`_ for valid external call parameters.

--------------------
ZCVECTORINDX
--------------------

ZCVECTORINDX, Invalid Vector Index xxxx

Run Time Error: This indicates that YottaDB can only accept positive values for vector index.

Action: Specify a valid vector index.

------------------
ZCWRONGDESC
------------------

ZCWRONGDESC, A string longer than 65535 is passed via 32-bit descriptor

Run Time Error: A string size greater than 65,535 byte was passed while the input mechanism is descriptor.

Action: Modify the external call table’s input mechanism to be descriptor64 instead of descriptor and modify the called routine to handle the new descriptor type.

------------------
ZDATEBADDATE
------------------

ZDATEBADDATE, $ZDATE() date argument dddd is less than -365 (the $HOROLOG value for 01-JAN-1840) or greater than 364570088 (the $HOROLOG value for 31-DEC-999999)

Run Time Error: The value of the date portion of the argument dddd to $ZDATE() is outside the range of values the function handles.

Action: Examine the code that created the value of dddd for errors or revise the design to create dates within the range supported by $ZDATE().

-------------------
ZDATEBADTIME
-------------------

ZDATEBADTIME, $ZDATE() time argument tttt is less than 0 or greater than 86399 (the $HOROLOG value for a second before midnight)

Run Time Error: The value of the time portion of the argument tttt to $ZDATE() is outside the range of values the function handles.

Action: Examine the code that created the value of tttt for errors or revise the design to create times within the range supported by $ZDATE().

--------------------
ZDATEFMT
--------------------

ZDATEFMT, $ZDATE format string contains invalid character

Run Time Error: This indicates that a $ZDATE() function specified a second argument that contains one or more invalid format characters.

Action: Verify the $ZDATE() format.

-------------------
ZDIROUTOFSYNC
-------------------

ZDIROUTOFSYNC, $ZDIRECTORY xxxx is not the same as its cached value yyyy

Run Time Warning: For performance purposes, YottaDB caches the value of $ZDIRECTORY when it is modified using the SET command. This cached value is passed to an external environment translation routine. YottaDB issues ZDIROUTOFSYNC error when $ZDIRECTORY is referenced and its cached value differs from the current working directory. This might happen if an external routine called from YottaDB modifies the current working directory and the application does not modify $ZDIRECTORY to the modified directory.

.. note::
   ZSHOW of intrinsic special variables appends ->%YDB-W-ZDIROUTOFSYNC error to the text corresponding to the $ZDIRECTORY if the out of sync condition is detected.

Action: Avoid changing the current working directory except with SET $ZDIRECTORY, or ensure that SET $ZDIRECTORY is used along with the other mechanisms.

------------------
ZEDFILSPEC
------------------

ZEDFILSPEC, Illegal ZEDIT file specification: xxxx

Run Time Error: This indicates that a ZEDIT command argument contains a file-specification that could not be parsed or is not a valid type for ZEDIT.

Action: Look for an attempt to edit a .o/.OBJ file.

--------------
ZFF2MANY
--------------

ZFF2MANY, Number of characters specified for ZFF deviceparameter (xxxx) is more than the allowed (yyyy)

Run Time Error: This indicates that the number of characters specified for ZFF deviceparameter for the socked device being OPENed (or USEd) exceeds the maximum allowed.

Action: Modify the string specified for ZFF to have a length of at most the maximum allowed.

---------------
ZFILENMTOOLONG
---------------

ZFILENMTOOLONG, xxxx is longer than 255 characters

Run Time Error: This indicates that a $ZFILE() function argument exceeded 255 characters. xxxx is the length of the file-specification supplied.

Action: Modify the file-specification argument.

--------------
ZFILKEYBAD
--------------

ZFILKEYBAD, xxxx is not a legal keyword for $ZFILE()

Run Time Error: This indicates that a $ZFILE() function specified the invalid keyword xxxx.

Action: Review the routine for correct spelling and the validity of keywords.

-----------------
ZFILNMBAD
-----------------

ZFILNMBAD, xxxx is not a legal file name

Run Time Error: This indicates that a $ZFILE() function argument supplied an invalid file-specification xxxx.

Action: Look for invalid characters or improper punctuation within the file-specification.

------------------
ZGBLDIRACC
------------------

ZGBLDIRACC, Cannot access global directory xxxx. Continuing with yyyy.

Run Time Error: This indicates that a SET of a $ZGBLDIR or external global reference specified a Global Directory (xxxx) that does not exist or cannot be accessed due to permissions. YottaDB retains the previous Global Directory (yyyy).

Action: Ensure that you specified the intended Global Directory. Use a host shell command to verify that the specified directory exists and has the protections required for the desired access.

----------------
ZGOCALLOUTIN
----------------

ZGOCALLOUTIN, ZGOTO level 0 with entry ref not valid when using call-ins

Call in/Run Time Error: Code invoked by a call-in contained a ZGOTO 0:entryref. The purpose of a ZGOTO 0:entryref is to "refresh" the YottaDB routine context, but that action would invalidate the interface with the calling (in) code.

Action: Refactor the code invoked by the call-in to avoid or appropriately conditionalize the ZGOTO 0:entryref.

-------------------
ZGOTOINVLVL
-------------------

ZGOTOINVLVL, ZGOTO in a trigger running in mmmm cannot ZGOTO level LLLL

MUPIP Error: A ZGOTO command in the trigger logic attempted to specify an inappropriate destination. Currently, that is a ZGOTO in a trigger context with a target level of one (1) and an entryref. YottaDB does not support such ZGOTO arguments in MUPIP because there is no context outside that of the trigger.

Action: Revise the trigger logic to only use ZGOTO with an entryref within the trigger context of trigger logic. Note that you can ZGOTO out of a trigger, but doing so in MUPIP terminates the MUPIP process. YottaDB recommends limiting the use of ZGOTO to debugging, error handling and testing. Use of ZGOTO in production code, even for error processing, should always be thoroughly tested.

-----------------
ZGOTOLTZERO
-----------------

ZGOTOLTZERO, Cannot ZGOTO a level less than zero

Run Time Error: This indicates that a ZGOTO specified a negative level in its argument.

Action: Ensure that the ZGOTO level is between zero (0) and the current $ZLEVEL.

---------------
ZGOTOTOOBIG
---------------

ZGOTOTOOBIG, Cannot ZGOTO a level greater than present level

Run Time Error: This indicates that ZGOTO command specified a level greater than the current stack depth.

Action: Check the source of the level. The level argument of a ZGOTO indicates the depth the stack is to be after the ZGOTO, which cannot be greater than the current stack depth. The only way to increase stack depth is by performing DOs or XECUTEs.


------------------
ZINTDIRECT
------------------

ZINTDIRECT, Attempt to enter direct mode from $ZINTERRUPT

Run Time Error: A $ZINTERRUPT routine cannot break to direct mode if the current IO device is the same as $PRINCIPAL.

Action: Modify the $ZINTERRUPT routine to not break to direct mode.

----------------
ZINTRECURSEIO
----------------

ZINTRECURSEIO, Attempt to do IO to the active device in $ZINTERRUPT

Run Time Error: A $ZINTERRUPT routine cannot perform I/O to the current IO device if it was active when the interrupt was recognized and the device is a terminal, socket device, FIFO, or PIPE.

Action: Modify the $ZINTERRUPT routine to not perform I/O to the active device.

----------------
ZLINKFILE
----------------

ZLINKFILE, Error while ZLINKing "xxxx"

Run Time Error: This indicates that ZLINK command failed while trying to include routine xxxx in the image. Note that a jobbed off process could encounter a ZLINKFILE error if the $ZROUTINES ISV and the $ydb_routines environment variable differ, and the routine xxxx can only be found through $ZROUTINES, but not through $ydb_routines (since the jobbed off process only inherits $ydb_routines).

Action: Use host shell commands to ensure that the file to be ZLINKed is in the proper directory and has the appropriate protection. Review the accompanying message(s) for additional information.

---------------
ZLMODULE
---------------

ZLMODULE, Object file name does not match module name: xxxx

Run Time Error: This indicates that YottaDB did not perform a ZLINK or an auto-ZLINK because the name of the object file specified with the ZLINK xxxx is not the same as the name of the source file.

Action: Look for and correct any typographical errors in the object file specified for the ZLINK, or assign the same name to the source and object.

-----------------
ZLNOOBJECT
-----------------

ZLNOOBJECT, No object module was produced

Run Time Error: This indicates that a run-time compile specified a NOOBJECT qualifier. This can be accomplished with a ZLINK qualifier or by setting $ZCOMPILE to a qualifier string that YottaDB uses for auto-ZLINKs or ZLINKs with no qualifiers.

Action: Remove the NOOBJECT qualifier.

---------------------
ZPARSETYPE
---------------------

ZPARSETYPE, Illegal TYPE argument to $ZPARSE(): xxxx

Run Time Error: This indicates that a $ZPARSE() function specified a type argument xxxx that was not a null string, SYNTAX_ONLY, or NO_CONCEAL.

Action: Verify the spelling of the keyword.

----------------------
ZPARSFLDBAD
----------------------

ZPARSFLDBAD, Illegal $ZPARSE() field parameter: xxxx

Run Time Error: This indicates that a $ZPARSE() function specified a field argument xxxx that was not NODE, DEVICE, DIRECTORY, NAME, TYPE, or VERSION.

Action: Verify that the keyword is spelled correctly.

------------------
ZPEEKNOJNLINFO
------------------

ZPEEKNOJNLINFO, $ZPEEK() unable to access requested journal structure - region rrrr is not currently journaled

Run Time Error: A $ZPEEK() invocation requested journal related information on region rrrr, but that region does not currently have journaling enabled and on. Note that if the process has not updated the database region, or used a VIEW or $VIEW() referencing the region, some fields may have a value of zero (0).

Action: Use $VIEW("JNLACTIVE",region) to determine that journaling is enabled and on for a region before attempting to access its journal-related information.

-----------------
ZPEEKNORPLINFO
-----------------

ZPEEKNORPLINFO, $ZPEEK() unable to access requested replication structure

Run Time Error: The replication structure specified in the mnemonic is not available to this process because either replication is not running or, in the case of receive pool type requests, is not running on a non-primary where such control blocks are available.

Action: Only fetch replication values when replication is active and if accessing gtmrecv.* fields, do not run on a primary.

-------------------
ZPIDBADARG
-------------------

ZPIDBADARG, The tvexpr must be FALSE if last $ZPID() not found

Run Time Error: This indicates that a $ZPID() function specified an argument that was TRUE when no active PID scan was in progress.

Action: Review the routine to ensure that $ZPID() is first initialized with a FALSE truth-valued expression. $ZPID() cannot be invoked with a TRUE truth-valued expression unless the last $ZPID() returned a PID. If it has not been previously invoked, it must be first invoked with a FALSE truth-valued expression.

------------------
ZPRTLABNOTFND
------------------

ZPRTLABNOTFND, Label not found in routine

Run Time Error: This indicates that a ZPRINT command specified a label that could not be found in the routine.

Action: Verify the spelling of the label. Ensure that the current version of the program has the label by ZPRINTing the entire routine.

-------------------
ZROSYNTAX
-------------------

ZROSYNTAX, $ZROUTINES syntax error: xxxx

Syntax/Run Time Error: This indicates that a $ZROUTINES related action encountered syntax error xxxx.

Action: Modify the UNIX environment variable ydb_routines or the expression being SET into $ZROUTINES.

----------------------
ZSHOWBADFUNC
----------------------

ZSHOWBADFUNC, An illegal function was specified for ZSHOW

Run Time Error: This indicates that ZSHOW argument code specified an invalid action.

Action: Modify the argument to use a valid code.

---------------------
ZSOCKETATTR
---------------------

ZSOCKETATTR, Attribute "xxxx" invalid for $ZSOCKET function msg name

Run Time Error: This indicates that the named attribute is not recognized for the $ZSOCKET function.

Action: Check the spelling, review the documented list of available attributes and adjust the attribute argument.

-------------------
ZSOCKETNOTSOCK
-------------------

ZSOCKETNOTSOCK, $ZSOCKET function called but device is not a socket

Run Time Error: The code invoked the $ZSOCKET() function with a device which is not a Socket Device.

Action: Review device usage and revise as appropriate.

--------------------
ZSRCHSTRMCT
--------------------

ZSRCHSTRMCT, Search stream identifier out of range

Run Time Error: This indicates that $ZSEARCH() function specified a stream less than 1 or greater than 255.

Action: Change the stream value to be within the range of 1-255.

-------------------
ZSTEPARG
-------------------

ZSTEPARG, ZSTEP argument expected

Compile Time Error: This indicates that ZSTEP command did not specify an argument or a <SP> to hold the place of the missing argument.

Action: Modify the ZSTEP command so it has an argument or a trailing double space.

------------------
ZTIMEOUT
------------------

ZTIMEOUT, ZTIMEOUT Time expired

Run Time Warning: This warning message appears when $ZTIMEOUT expires and there were no vectors defined. If no error handlers are defined, YottaDB invokes the default trap which puts the control to Direct Mode.

Action: Check the message(s) for more information on where the timer expired in the current process. If needed, set an appropriate error handler to specify an action associated with $ZTIMEOUT expiry or define a $ZTIMEOUT with a vector.

-----------------
ZTRIGINVACT
-----------------

ZTRIGINVACT, Missing or invalid subcode (first) parameter given to $ZTRIGGER()

Trigger/Run Time Error: The first argument to $ZTRIGGER() is required to specify its mode of action.

Action: for the first argument of $ZTRIGGER() use an expression that evaluates to "FILE", "ITEM" or "SELECT".

--------------------
ZTRIGNOTRW
--------------------

ZTRIGNOTRW, ZTRIGGER cannot operate on read-only region rrrr

Run Time Error: This error occurs when ZTRIGGER attempts to write to read-only region rrrr.

Action: Check for appropriate global directory mapping and appropriate permissions on the database file mapped to region rrrr.

-------------------
ZTWORMHOLE2BIG
-------------------

ZTWORMHOLE2BIG, String length of LLLL bytes exceeds maximum length of mmmm bytes for $ZTWORMHOLE

Trigger/Run Time Error: YottaDB limits $ZTWORMHOLE length to mmmm bytes and the application attempted to use LLLL bytes.

Action: Restrict the size of the string stored in $ZTWORMHOLE to mmmm bytes. Ensure that $ZTWORMHOLE only holds the information that the application needs during trigger execution. If necessary, reorganize the logic to reduce the amount of local context needed during trigger execution, possibly by using global variables.

--------------------
ZWRSPONE
--------------------

ZWRSPONE, Subscript patterns in ZWRITE are atomic; Invalid delimiter

Compile Time Error: This indicates that ZWRITE specification contained a pattern match that held or terminated with a punctuation character that was not within a string literal.

Action: Look for missing quotes or typographical errors and make any corrections that are necessary.

-------------------
ZYSQLNULLNOTVALID
-------------------

ZYSQLNULLNOTVALID, $ZYSQLNULL cannot be used as an integer, numeric, gvn subscript/value or lock subscript

Compile / Run Time Error: $ZYSQLNULL can only be used in expressions, as a local variable subscript, or the value of a local variable node.

Action: Correct the coding issue resulting in the incorrect $ZYSQLNULL usage.
