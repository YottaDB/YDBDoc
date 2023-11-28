.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2017-2024 YottaDB LLC and/or its subsidiaries.#
.. # All rights reserved.                                        #
.. #                                                             #
.. #     This document contains the intellectual property        #
.. #     of its copyright holder(s), and is made available       #
.. #     under a license.  If you do not know the terms of       #
.. #     the license, please stop and do not read further.       #
.. #                                                             #
.. ###############################################################

.. index::
   Language Extensions

================================
2. YottaDB Language Extensions
================================

.. contents::
   :depth: 2

In addition to providing all of the ANSI standard M features, YottaDB offers a number of language extensions. In this chapter, the language extensions are grouped by intended function to demonstrate their relationships to each other and to the programming process. A summary table is provided in each section. For a full description of a particular extension, refer to its complete entry in the `“Commands” <./commands.html>`_, `“Functions” <./functions.html>`_, or `“Intrinsic Special Variables” <./isv.html>`_ chapters.

The following sections describe the YottaDB language extensions listed below:

* UNIX interface facilities
* Debugging tools
* Exception-handling extensions
* Interrupting Execution Flow
* Journaling extensions
* Extensions providing additional capability
* Device Handling Extensions
* Alias Variables Extensions
* Extensions for Unicode Support

--------------------------------------
Operating System Interface Facilities
--------------------------------------

To improve efficiency and reduce duplication and inconsistency, YottaDB is closely integrated with the host operating system environment. With YottaDB you can gain access to the operating system facilities to examine:

* System information, such as quotas and SIDs
* Jobs and processes
* Directories and files
* Devices
* Messages
* Privileges

The following table summarizes the YottaDB operating system interface facilities.

**Operating System Interface Facilities**

+------------------------------------------+---------------------------------------------------------------------------------------------------------------+
| Extension                                | Explanation                                                                                                   |
+==========================================+===============================================================================================================+
| `ZSYstem <commands.html#zsystem>`_       | Provides access to the shell.                                                                                 |
+---------------------------------+------------------------------------------------------------------------------------------------------------------------+
| `$ZMessage() <functions.html#zmessage>`_ | Translates an error condition code into text form.                                                            |
+---------------------------------+------------------------------------------------------------------------------------------------------------------------+
| `$ZCMdline <isv.html#zcmdline>`_         | Contains a string value specifying the "excess" portion of the command line that invoked the YottaDB process. |
+---------------------------------+------------------------------------------------------------------------------------------------------------------------+
| `$ZJob <isv.html#zjob>`_                 | Holds the pid of the process created by the last JOB command performed by the current process.                |
+---------------------------------+------------------------------------------------------------------------------------------------------------------------+
| `$ZPARSE() <functions.html#zparse>`_     | Parses a UNIX filename.                                                                                       |
+---------------------------------+------------------------------------------------------------------------------------------------------------------------+
| `$ZSEARCH() <functions.html#zsearch>`_   | Searches for one or more UNIX files.                                                                          |
+---------------------------------+------------------------------------------------------------------------------------------------------------------------+
| `$ZSYstem <isv.html#zsystem>`_           | Contains the status code of the last ZSYSTEM.                                                                 |
+---------------------------------+------------------------------------------------------------------------------------------------------------------------+
| `$ZTRNLNM() <functions.html#ztrnlnm>`_   | Returns the value of an environment variable.                                                                 |
+---------------------------------+------------------------------------------------------------------------------------------------------------------------+
| `$ZDIRectory <isv.html#zdirectory>`_     | Contains current working directory.                                                                           |
+---------------------------------+------------------------------------------------------------------------------------------------------------------------+

.. note::
   Operating system services accessed by YottaDB commonly treat a NULL ($CHAR(0)) character as a terminator. Therefore embedded NULLs in strings that are passed to the OS as arguments may cause non-obvious behavior.

-------------------------------------------
Debugging Facilities
-------------------------------------------

YottaDB provides a number of debugging features. These features include the ability to:

* Interactively execute routines using M commands.
* Display lines that may contain errors using the ZPRINT command and the $ZPOSITION special variable.
* Redisplay error messages using the $ZSTATUS special variable and the ZMESSAGE command.
* Set breakpoints and actions to bypass an error using the ZBREAK command.
* Execute a line at a time using the ZSTEP command.
* Display information about the M environment using the ZSHOW command.
* Modify the invocation stack with QUIT and ZGOTO.
* Incrementally add or modify code using the ZLINK and ZEDIT commands.
* Continue execution using the ZCONTINUE command.
* Establish "watch points" with triggers to trap incorrect accesses on global variable updates.

The following table summarizes the YottaDB language extensions that facilitate debugging.

**YottaDB Debugging Tools**

+-------------------------------+----------------------------------------------------------------------------------------------------------------------------+
| Extension                     | Explanation                                                                                                                |
+===============================+============================================================================================================================+
| ZBreak                        | Establishes a temporary breakpoint, with optional M action and/or activation count.                                        |
+-------------------------------+----------------------------------------------------------------------------------------------------------------------------+
| ZContinue                     | Continues routine execution from a break.                                                                                  |
+-------------------------------+----------------------------------------------------------------------------------------------------------------------------+
| ZEDit                         | Invokes the UNIX text editor specified by the EDITOR environment variable.                                                 |
+-------------------------------+----------------------------------------------------------------------------------------------------------------------------+
| ZGoto                         | Removes multiple levels from the M invocation stack and transfers control.                                                 |
+-------------------------------+----------------------------------------------------------------------------------------------------------------------------+
| ZLink                         | Includes a new or modified M routine in the current M image; automatically recompiles if necessary.                        |
+-------------------------------+----------------------------------------------------------------------------------------------------------------------------+
| ZMessage                      | Signals the specified condition.                                                                                           |
+-------------------------------+----------------------------------------------------------------------------------------------------------------------------+
| ZPrint                        | Displays lines of source code.                                                                                             |
+-------------------------------+----------------------------------------------------------------------------------------------------------------------------+
| ZSHow                         | Displays information about the M environment.                                                                              |
+-------------------------------+----------------------------------------------------------------------------------------------------------------------------+
| ZSTep                         | Incrementally executes a routine to the beginning of the next line of the same type.                                       |
+-------------------------------+----------------------------------------------------------------------------------------------------------------------------+
| ZWRite                        | Displays all or some local or global variables.                                                                            |
+-------------------------------+----------------------------------------------------------------------------------------------------------------------------+
| $ZCSTATUS                     | Holds the value of the status code for the last compile performed by a ZCOMPILE command.                                   |
+-------------------------------+----------------------------------------------------------------------------------------------------------------------------+
| $ZEDit                        | Contains the status code for the last ZEDit.                                                                               |
+-------------------------------+----------------------------------------------------------------------------------------------------------------------------+
| $ZJOBEXAM()                   | Performs a ZSHOW "*" to a default file location and name, or the one optionally specified by the argument.                 |
+-------------------------------+----------------------------------------------------------------------------------------------------------------------------+
| $ZLEVel                       | Contains the current level of DO/XECUTE nesting.                                                                           |
+-------------------------------+----------------------------------------------------------------------------------------------------------------------------+
| $ZMessage()                   | Translates an error condition code into text form.                                                                         |
+-------------------------------+----------------------------------------------------------------------------------------------------------------------------+
| $ZPOSition                    | Contains a string indicating the current execution location.                                                               |
+-------------------------------+----------------------------------------------------------------------------------------------------------------------------+
| $ZPROmpt                      | Controls the symbol displayed as the direct mode prompt.                                                                   |
+-------------------------------+----------------------------------------------------------------------------------------------------------------------------+
| $ZROutines                    | Contains a string specifying a directory list containing the object, and optionally the source, files.                     |
+-------------------------------+----------------------------------------------------------------------------------------------------------------------------+
| $ZSOurce                      | Contains name of the M source program most recently ZLINKed or ZEDITed; default name for next ZEDIT or ZLINK.              |
+-------------------------------+----------------------------------------------------------------------------------------------------------------------------+
| $ZStatus                      | Contains error condition code and location of the last exception condition occurring during routine execution.             |
+-------------------------------+----------------------------------------------------------------------------------------------------------------------------+
| $ZSTep                        | Controls the default ZSTep action.                                                                                         |
+-------------------------------+----------------------------------------------------------------------------------------------------------------------------+

--------------------------------
Exception Handling Facilities
--------------------------------

The YottaDB exception trapping allows you to do the following:

* DO a recovery routine and resume the original command stream.
* GOTO any special handling; an extended ZGOTO provides for context management.
* Report an error and enter Direct Mode for debugging.
* OPEN Input/Output devices with specific traps in addition to the main trap.
* Trap and process an exception based on a device error.
* Trap and process an exception based on terminal input.

The following table summarizes the YottaDB language extensions that facilitate exception handling.

**Exception Handling Extensions**

+--------------------------------+----------------------------------------------------------------------------------------------------------------------------+
| Extension                      | Explanation                                                                                                                |
+================================+============================================================================================================================+
| ZGoto                          | Removes zero or more levels from the M Invocation stack and, optionally, transfers control.                                |
+--------------------------------+----------------------------------------------------------------------------------------------------------------------------+
| ZMessage                       | Signals the specified condition.                                                                                           |
+--------------------------------+----------------------------------------------------------------------------------------------------------------------------+
| $ZCSTATUS                      | Holds the value of the status code for the last compile performed by a ZCOMPILE command.                                   |
+--------------------------------+----------------------------------------------------------------------------------------------------------------------------+
| $ZEOF                          | Contains indication of whether the last READ reached end-of-file.                                                          |
+--------------------------------+----------------------------------------------------------------------------------------------------------------------------+
| $ZMessage()                    | Translates an error condition code into text form.                                                                         |
+--------------------------------+----------------------------------------------------------------------------------------------------------------------------+
| $ZLevel                        | Contains current level of DO/XECUTE nesting.                                                                               |
+--------------------------------+----------------------------------------------------------------------------------------------------------------------------+
| $ZStatus                       | Contains error condition code and location of last exception condition occurring during routine execution.                 |
+--------------------------------+----------------------------------------------------------------------------------------------------------------------------+
| $ZSYstem                       | Contains the status code of the last ZSYSTEM.                                                                              |
+--------------------------------+----------------------------------------------------------------------------------------------------------------------------+
| $ZTrap                         | Contains an XECUTE string or entryref that YottaDB invokes upon encountering an exception condition.                       |
+--------------------------------+----------------------------------------------------------------------------------------------------------------------------+
| EXCEPTION                      | Provides a deviceparameter specifying an XECUTE string or entryref that YottaDB invokes upon encountering a device-        |
|                                | related exception condition.                                                                                               |
+--------------------------------+----------------------------------------------------------------------------------------------------------------------------+

-----------------------------------
Interrupting Execution Flow
-----------------------------------

YottaDB process execution is interruptible by the following events:

* Typing CTRL+C or getting SIGINT (if `CENABLE <ioproc.html#cenable>`_ is on for a terminal `$PRINCIPAL <ioproc.html#principal>`_). YottaDB ignores SIGINT (CTRL+C) if $PRINCIPAL is not a terminal.
* Typing one of the `CTRAP <ioproc.html#ctrap>`_ characters on a terminal $PRINCIPAL.
* Exceeding `$ZMAXTPTIME <isv.html#zmaxtptime>`_ in a transaction.
* Getting a `MUPIP INTRPT <../AdminOpsGuide/dbmgmt.html#intrpt>`_ (SIGUSR1).
* `+$ZTEXIT <isv.html#ztexit>`_ evaluates to a truth value at the outermost `TCOMMIT <commands.html#tcommit>`_ or `TROLLBACK <commands.html#trollback>`_.

When YottaDB detects any of these events, it transfers control to a vector that depends on the event. For CTRAP characters and $ZMAXTPTIME, YottaDB uses the `$ETRAP <isv.html#etrap>`_ or `$ZTRAP <isv.html#ztrap>`_ vectors described in more detail in `Error Processing <errproc.html>`_. For MUPIP INTRPT and $ZTEXIT, it `XECUTEs <commands.html#xecute>`_ the interrupt handler code placed in `$ZINTERRUPT <isv.html#zinterrupt>`_. If $ZINTERRUPT is an empty string, a the response to a MUPIP INTRPT ia a no-op.

YottaDB recognizes most of these events when they occur but transfers control to the interrupt vector at the start of each M line, at each iteration of a `FOR <commands.html#for>`_ loop, at certain points during the execution of commands which may take a "long" time. For example, `ZWRITE <commands.html#zwrite>`_, `HANG <commands.html#hang>`_, `LOCK <commands.html#lock>`_, `MERGE <commands.html#merge>`_, `ZSHOW "V" <commands.html#zshow-information-codes>`_, `OPENs <commands.html#open>`_ of disk files and `FIFOs <ioproc.html#fifo-characteristics>`_, OPENs of `SOCKETs <ioproc.html#using-socket-devices>`_ with the `CONNECT <ioproc.html#connect>`_ deviceparameter (unless the timeout is zero), `WRITE /WAIT <ioproc.html#write-command>`_ for SOCKETs, and `READ <commands.html#read>`_ for terminals, SOCKETs, FIFOs, and `PIPEs <https://docs.yottadb.com/ProgrammersGuide/ioproc.html#using-pipe-devices>`_. If +$ZTEXIT evaluates to a truth value at the outermost TCOMMIT or TROLLBACK, YottaDB XECUTEs $ZINTERRUPT after completing the commit or rollback. CTRAP characters are recognized when they are read by the operating system.

If an interrupt event occurs in a long running external call (for example, waiting in a message queue), YottaDB recognizes the event but makes the vector transfer after the external call returns when it reaches the next appropriate execution boundary.

When an interrupt handler is invoked, YottaDB saves and restores the current values of `$REFERENCE <isv.html#reference>`_. However, the current device (`$IO <isv.html#io>`_) is neither saved nor restored. If an interrupt handler changes $IO (via `USE <commands.html#use>`_), ensure that the interrupt handler restores the current device before returning. To restore the device which was current when the interrupt handler began, specify USE without any deviceparameters. Any attempt to do IO on a device which was actively doing IO when the interrupt was recognized may result in a `ZINTRECURSEIO <../MessageRecovery/errors.html#zintrecurseio>`_ error.

-----------------------------------
Journaling Extensions
-----------------------------------

Journaling records redundant copies of database update information to increase protection against loss of information due to hardware and software failure. In YottaDB, TSTART and TCOMMIT mark the beginning and end of an application (logical) transaction, which may consist of multiple global variable updates. When a TCOMMIT takes $TLEVEL from one (1) to zero (0), it transfer all of the transaction updates to the journal file, and, except if TRANSACTIONID="BATCH", returns control to the application only after the associated records reach the secondary storage holding the journal file.

The following table summarizes the YottaDB language extensions for journaling.

**Journaling Extensions**

+-------------------------------+---------------------------------------------------------------------------------------------------------------------+
| Extensions                    | Explanation                                                                                                         |
+===============================+=====================================================================================================================+
| View                          | Extended to ensure that YottaDB has transferred all updates to the journal file.                                    |
+-------------------------------+---------------------------------------------------------------------------------------------------------------------+
| $View()                       | Extended for examining journaling status.                                                                           |
+-------------------------------+---------------------------------------------------------------------------------------------------------------------+

---------------------------------------
Extensions for Additional Capability
---------------------------------------

For ways to adjust some process operating characteristics, see the command description :ref:`view-command`. For ways to get information about certain process operating characteristics, see the function description :ref:`view-function`.

In YottaDB, support of environment specification for global names and resource names is possible. It is possible to excercise user code to customize interpretation of the environment specification. See `Chapter 5: “General Language Features of M” <./langfeat.html>`_ for details.

The following table summarizes YottaDB extensions that increase general capability.

**Extensions for Additional Capability**

+------------------------------+-----------------------------------------------------------------------------------------------------------------------+
| Extension                    | Explanation                                                                                                           |
+==============================+=======================================================================================================================+
| View                         | Modifies the environment.                                                                                             |
+------------------------------+-----------------------------------------------------------------------------------------------------------------------+
| ZAllocate*                   | Facilitates incremental locking by locking a name without unlocking previously locked names.                          |
+------------------------------+-----------------------------------------------------------------------------------------------------------------------+
| ZDeallocate*                 | Unlocks one or more names without necessarily unlocking other names.                                                  |
+------------------------------+-----------------------------------------------------------------------------------------------------------------------+
| ZHelp                        | Provides access to on-line help.                                                                                      |
+------------------------------+-----------------------------------------------------------------------------------------------------------------------+
| ZWIthdraw                    | "Kills" data in a node without affecting the node's descendants.                                                      |
+------------------------------+-----------------------------------------------------------------------------------------------------------------------+
| $Order()                     | Enhanced to return the next unsubscripted variable in collating sequence from the current environment. Name-level     |
|                              | $ORDER() always returns an empty string when used with extended references.                                           |
+------------------------------+-----------------------------------------------------------------------------------------------------------------------+
| $View()                      | Examines the YottaDB environment.                                                                                     |
+------------------------------+-----------------------------------------------------------------------------------------------------------------------+
| $ZCStatus                    | Returns the status from the last compile.                                                                             |
+------------------------------+-----------------------------------------------------------------------------------------------------------------------+
| $ZDate()                     | Converts a date and/or time in $HOROLOG format into formatted text, using a user-specified format string.             |
+------------------------------+-----------------------------------------------------------------------------------------------------------------------+
| $ZPrevious()**               | Returns the previous element in a collating sequence, at the current level of a local or global array.                |
+------------------------------+-----------------------------------------------------------------------------------------------------------------------+
| $ZA,$ZB, $ZEOF               | Return device dependent I/O status information.                                                                       |
+------------------------------+-----------------------------------------------------------------------------------------------------------------------+
| $ZCOmpile                    | Maintains the compiler qualifiers to be used on automatic compilation.                                                |
+------------------------------+-----------------------------------------------------------------------------------------------------------------------+
| $ZBIT functions              | A series of functions beginning with the characters $ZBIT that allow manipulation of bits.                            |
+------------------------------+-----------------------------------------------------------------------------------------------------------------------+
| $ZGBLdir                     | Maintains the name of the current global directory; may be set to switch this process to a new database.              |
+------------------------------+-----------------------------------------------------------------------------------------------------------------------+
| $ZIO                         | Contains translated name of current I/O device.                                                                       |
+------------------------------+-----------------------------------------------------------------------------------------------------------------------+
| $ZINTerrupt                  | Specifies the code to be XECUTE'd when an interrupt is processed.                                                     |
+------------------------------+-----------------------------------------------------------------------------------------------------------------------+
| $ZKEY                        | SD: Returns current position in the sequential file based on last read.                                               |
+------------------------------+-----------------------------------------------------------------------------------------------------------------------+
| $ZMAXTPTIme                  | Contains an integer value indicating the time duration YottaDB should wait for the completion of all activities       |
|                              | fenced by the current transaction's outermost TSTART/TCOMMIT pair.                                                    |
+------------------------------+-----------------------------------------------------------------------------------------------------------------------+
| $ZROutines                   | Maintains the list of directories to search during look-ups of object and source files.                               |
+------------------------------+-----------------------------------------------------------------------------------------------------------------------+
| $ZSYstem                     | Returns the status code for the last subprocess invoked with the ZSYSTEM command.                                     |
+------------------------------+-----------------------------------------------------------------------------------------------------------------------+
| $ZVERsion                    | Contains a designation of the current version name, level, and operating system.                                      |
+------------------------------+-----------------------------------------------------------------------------------------------------------------------+

\* The ZALLOCATE and ZDEALLOCATE commands are provided for compatibility with other M systems. However, YottaDB recommends use of the standard LOCK command, which provides an incremental locking facility. The incremental lock provides both flexibility and greater compatibility with the M language standard.

\*\* The $ZPREVIOUS function is provided for compatibility with previous versions of YottaDB and other M systems. However, YottaDB recommends use of the standard two-argument form for the $ORDER function.

-----------------------------
Device Handling Extensions
-----------------------------

In the earlier versions of the M standard, device behavior was defined as a framework, with the details left to the implementations. YottaDB supports Terminals, Sequential Disks, FIFOs, PIPEs and a Null device under this model. Subsequently device mnemonicspaces were added to the standard and some of them defined. YottaDB supports the SOCKET device under this model with some extensions identified with controlmnemonics starting with the letter "Z."

For details of YottaDB device handling see `Chapter 9: “Input/Output Processing” <./ioproc.html>`_.

.. _alias-var-ext:

---------------------------
Alias Variable Extensions
---------------------------

Alias variables provide a layer of abstraction between the name of a local variable and an array analogous to that provided by M pass by reference in routines and function calls. Multiple local variables can be aliased to the same array, and a SET or KILL to one acts as a SET or KILL to all. Alias container variables provide a way to store a reference to an entire local variable array, using a subscrpted local, which protects the associated array even when it's not accessible through any current local variable name.

YottaDB aliases provide low level facilities on which an application can implement object-oriented techniques. An object can be mapped onto, and stored and manipulated in an array, then saved in an alias container variable from where it can be retrieved for processing. The use of appropriate subscripts in the array used for a container, provides a way to organize the stored objects and retrieve them by using the $ORDER() function to traverse the container array. The use of alias variables to implement objects provides significant efficiencies over traditional local variables because alias variables and alias container variables eliminate the need to execute MERGE commands to move objects.

Example:

.. code-block:: bash

   YDB>kill A,B
   YDB>set A=1,*B=A ; B & A are aliases
   YDB>write B
   1
   YDB>

Within the context of Alias Variables extensions:

* an array is very similar to its definition in the M standard, and means an entire tree of nodes, including the root and all descendants, except that it only applies to local variables and not to global variables.
* "Associated alias variables" means all alias variables and all alias container variables associated with an array.
* lvn is very similar to its definition in the M standard except that in the context of alias variables lvn is used to refer to a local variable name with a subscript.
* lname is very similar to its definition in the M standard, except that in the context of alias variables, lname is just the name of an unsubscripted local variable (root of an array).
* "Data cell" and "node" are synonyms.

The following table summarizes Alias Variables extensions.

**Alias Variables Extensions**

+----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Extension            | Explanation                                                                                                                                                              |
+======================+==========================================================================================================================================================================+
| Set *                | Explicitly creates an alias. For more information, refer to the description of SET * in :ref:`set-command`                                                               |
+----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Kill *               | Removes the association between its arguments, and any associated data cells. For more information, refer to the description of KILL * in                                |
|                      | :ref:`kill-command`                                                                                                                                                      |
+----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Quit *               | When QUIT * terminates an extrinsic function or an extrinsic special variable, it always returns an alias container. For more information, refer to the description of   |
|                      | QUIT * in :ref:`quit-command`.                                                                                                                                           |
+----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| ZWrite / ZSHow "V"   | Produces Alias Variables format output. For more information, refer to                                                                                                   |
|                      | :ref:`zwrite-format-alias-vars`                                                                                                                                          |
+----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| New                  | For the scope of the NEW, a NEW of a name suspends its alias association. For more information, refer to                                                                 |
|                      | :ref:`new-command`.                                                                                                                                                      |
+----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Exclusive New        | Create a scope in which some associations between an lname or an lvn and an array may be invisible. For more information, refer to                                       |
|                      | :ref:`new-command`.                                                                                                                                                      |
+----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZAHandle()          | returns a unique identifier (handle) for the array associated with an lname or an alias container; for an subscripted lvn that is not an alias container, it returns an  |
|                      | empty string. For more information, refer to :ref:`zahandle-function`.                                                                                                   |
+----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZDATA()             | Extends $DATA() to reflect the current alias state of the lvn or lname argument in order to identify alias and alias container variables. For more information, refer to |
|                      | :ref:`zdata-function`.                                                                                                                                                   |
+----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| View and $View()     | VIEW provides LV_GCOL, LV_REHASH, and STP_GCOL to perform garbage collection and local variable lookup table reorganization operations which normally happen             |
|                      | automatically at appropriate times. For more information on the keywords of the VIEW command, refer to                                                                   |
|                      | :ref:`keywords-view-command`.                                                                                                                                            |
|                      |                                                                                                                                                                          |
|                      | $VIEW() provides LV_CREF, LV_GCOL, and LV_REF. YottaDB uses the LC_CREF, LV_GCOL, LV_REF keywords in testing and is documenting them to ensure completeness in           |
|                      | product documentation. They may (or may not) be useful during application development for debugging or performance testing implementation alternatives. For more         |
|                      | information the keywords of $VIEW(), refer to :ref:`arg-kwrds-view`.                                                                                                     |
+----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| TSTART, RESTART, and | TSTART command can optionally list names whose arrays are restored on a transaction RESTART. If any of these are alias variables or have nodes which are alias container |
| ROLLBACK             | variables, their associations are also restored on transaction RESTART. For more information, refer to                                                                   |
|                      | Chapter 6: `Commands <./commands.html>`_.                                                                                                                                |
+----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------+

+++++++++++++++++++++
Definitions
+++++++++++++++++++++

**Alias Variables**

Alias Variables provide access to an array through multiple names. Conceptually an alias variable is the same as a pass-by-reference joining of multiple variable names, except that the joining of alias variables is explicit, whereas that of variables passed by reference is implicit. Indeed, the underlying implementation of alias variables and pass-by-reference within YottaDB is the same.

* All alias variables associated with the same array are equivalent in their access to its nodes - for example, a SET of a node in an array via one name is no different than a SET to that node using any other name of which it is an alias. Nothing about the order of their creation or association has any significance.
* Once an array becomes accessible via only a single unsubscripted name, YottaDB treats that name as a traditional local variable.
* YottaDB treats variables joined through pass-by-reference as a special variant of an alias variable. Pass-by-reference relates to the M stack model with implicit aliasing as a side effect of invocation with DO or $$ and implicit unaliasing as a side effect of QUIT. In the broader alias case, the program directly commands aliased and unaliased names without any binding to the M stack.
* YottaDB treats the state of a TP (Transaction Processing) RESTART variable as an internal alias, which it only exposes if the transaction creating it RESTARTs.
* YottaDB treats variables hidden by exclusive NEW as a type of alias.
* Owing to their implicit behavior, under certain circumstances, pass-by-reference aliases, RESTART variable and exclusive NEW aliases are not entirely symmetrical with respect to explicitly created alias variables (that is, they may come and go at different times, whereas alias variables come and go under application program control).

**Alias Container Variables**

Alias container variables are subscripted lvns that protect arrays for subsequent access by an alias variable. Since accessing an array requires a name, aliasing a name with the alias container regains access to an array stored in a container. For example:

.. code-block:: bash

   YDB>kill A,B,C
   YDB>set A=1,*C(2)=A ; C(2) is a container
   YDB>zwrite
   A=1 ;*
   *C(2)=A
   YDB>set *B=C(2) ; B is now an alias
   YDB>write B,":",$length(C(2)),":" ; An alias variable provides access but a container does not
   1:0:
   YDB>

* The value of an alias container is the empty string.
* Use the SET * command to associate an lname with the container to obtain an alias that provides access to the array in a container.
* SET with an alias container as left-hand side target replaces the value at that node of the container variable and destroys any prior alias association with an array.
* References to descendants of an alias container variable refer to nodes of the named parent array and have no relationship to any alias container held by a parent node.
* An alias container variable serves as a way to organize and manage entire arrays.
* While it takes two alias variables for an array to be considered aliased, it only takes one alias container variable to do so.

++++++++++++++++++
Performance
++++++++++++++++++

With two exceptions, alias and alias container variables add no overhead to normal local variable performance:

1. Complex patterns of aliases layered onto TSTART RESTART variables.
2. Complex patterns of aliases intermixed with NEW scope management, particularly when using exclusive NEW.

There is no reason to avoid aliases in any situation, but in those two contexts, YottaDB rewards attention to tidy design. YottaDB uses garbage collection to manage the storage used for local variables. Increasing the use of local variables, for example, to implement objects, will increase the need for garbage collection, even though the garbage collector and storage management are designed to be light weight and self-tuning. The use of alias variables to implement objects, however, is as efficient as any other method is likely to be, and except for the normal admonition to not keep arrays and local variables around when they are not needed, and to not create levels of contexts over and above those actually needed by application logic, use alias variables as liberally as your application needs dictate.

+++++++++++++++++++++++
ZWRITE/ZSHOW "V" Format
+++++++++++++++++++++++

ZWRITE as applied to local variables and ZSHOW "V" are conceptually similar, with two differences:

* ZWRITE allows the use of patterns to specify the variables and subscripts to display, whereas ZSHOW "V" applies to all local variables.
* ZSHOW "V" optionally allows the output to be directed to a global or local variable, whereas ZWRITE always directs its output to the current output device.

For more information on the ZWRITE/ZSHOW "V" format for alias variables, refer to :ref:`zwrite-format-alias-vars`.

++++++++++++++++++++++++
Pass By Reference
++++++++++++++++++++++++

YottaDB's underlying implementation of pass-by-reference and alias variables is the same. As illustrated by the program "killalias" previously, ZWRITE displays variables joined though pass-by-reference using alias conventions. Pass-by-reference is distinguished from alias variables by its implicit creation and elimination. Note the interaction between pass-by-reference and alias variables when the association of a formallist parameter in a subprogram is changed:

.. code-block:: bash

   $ /usr/local/lib/yottadb/r120/ydb -run ^switchalias
   switchalias ; Demonstrate Set * on formalist parameter
     zprint ; Print this program
     set A=1,B=2
     write "------------",!
     write "Initial Values:",!
     zwrite
     do S1(.A)
     write "------------",!
     write "On return:",!
     zwrite
     quit
     ;
   S1(X) ;
      set X=3
      write "------------",!
      write "Inside call - note alias association for formallist parameter:",!
      zwrite
      set *X=B,X=4 ; Change association of formallist parameter
      write "------------",!
      write "Note changed association",!
      zwrite
      quit
    ------------
    Initial Values:
    A=1
    B=2
    ------------
    Inside call - note alias association for formallist parameter:
    A=3 ;*
    B=2
    *X=A
    ------------
    Note changed association
    A=3
    B=4 ;*
    *X=B
    ------------
    On return:
    A=3
    B=4
    $


++++++++++++++++++++++++++++
SET* and QUIT* Examples
++++++++++++++++++++++++++++

The following table show the type of data movement of alias and alias container variables from QUIT * in a function to a SET * target:

+-----------------------------------------------+-------------------------------+--------------------------------------+----------------------------------------------------+----------------------------------------------+
|                                               | QUIT*                         | SET*                                 | Result                                             | ZWRITE                                       |
+===============================================+===============================+======================================+====================================================+==============================================+
| set \*a=$$makealias(.c)                       | Creates an alias container    | Dereferences the alias container     | Same as set \*a=c                                  | \*c=a                                        |
+-----------------------------------------------+-------------------------------+--------------------------------------+----------------------------------------------------+----------------------------------------------+
| set \*a(1)=$$makealias(.c)                    | Creates an alias container    | Dereferences the alias container     | Same as set \*a(1)=c                               | \*a(1)=c                                     |
+-----------------------------------------------+-------------------------------+--------------------------------------+----------------------------------------------------+----------------------------------------------+
| set \*a=$$makecntnr(.c)                       | Returns an alias container    | Copies the alias container           | Same as set \*a=c(1)                               | \*c=a                                        |
+-----------------------------------------------+-------------------------------+--------------------------------------+----------------------------------------------------+----------------------------------------------+
| set \*a(1)=$$makecntnr(.c)                    | Returns an alias container    | Copies the alias container           | Same as set \*a(1)=c(1)                            | \*a(1)=c                                     |
+-----------------------------------------------+-------------------------------+--------------------------------------+----------------------------------------------------+----------------------------------------------+

The makealias function returns an alias of the argument:

.. code-block:: none

   makealias(var)
   quit *var

The makecntr function returns an alias container of the argument:

.. code-block:: none

   makecntnr(var)
   new cont
   set *cont(1)=var
   quit *cont(1)

+++++++++++++++++++
KILL* Examples
+++++++++++++++++++

Example:

.. code-block:: bash

   YDB>Set A=1,*B=A ; Create an array and an association
   YDB>ZWRite ; Show that the array and association exist
   A=1 ;*
   *B=A
   YDB>Kill *A ; Remove the association for A - it now has no association and no array
   YDB>ZWRite ; B is a traditional local variable
   B=1

Example:

.. code-block:: bash

   YDB>Set A=2 ; add a value for A
   YDB>ZWRite ; A and B have different values and both are traditional local variables
   A=2
   B=1
   YDB>

KILL on the other hand, removes data in the array (and possibly the array itself) without affecting any alias association.

.. code-block:: bash

   YDB>Set A=2,*B=A ; Create an array and an association
   YDB>ZWRite ; Both array and association exist
   A=2 ;*
   *B=A
   YDB>Kill A ; Kill the array
   YDB>ZWRite ; There is no data to show - only the association
   *B=A
   YDB>Set B=3 ; Create a new value
   YDB>ZWRite ; The association was unaffected by the Kill
   A=3 ;*
   *B=A
   YDB>

Example:

.. code-block:: bash

   $ /usr/local/lib/yottadb/r120/ydb -run ^killalias
   killalias ; Demonstrate Kill * of pass-by-reference
          ZPrint ; Print this program
          Set A=1,C=3
          Write "------------",!
          Write "Initial Values:",!
          ZWRite
          Do K1(.A,.C) ; Pass A & C by reference
          Write "------------",!
          Write "Value of A is unchanged because of Kill *B, but C has changed: ",!
          ZWRite
          Quit
    ;
    K1(B,D) ; A & C are bound to B & D respectively
          Write "------------",!
          Write "A & B are aliases, as are C & D:",!
          ZWRite
          Kill *B
          Set B=2,D=4
          Write "------------",!
          Write "After Kill *B, A & B are different but C & D remain associated:",!
          ZWrite
          Quit
   ------------
   Initial Values:
   A=1
   C=3
   ------------
   A & B are aliases, as are C & D:
   A=1 ;*
   *B=A
   C=3 ;*
   *D=C
   ------------
   After Kill *B, A & B are different but C & D remain associated:
   A=1
   B=2
   C=4 ;*
   *D=C
   ------------
   Value of A is unchanged because of Kill *B, but C has changed:
   A=1
   C=4
   Example:
   YDB>Set A=1,*B=A ; Create an array and association
   YDB>ZWRite ; Verify that it is there
   A=1 ;*
   *B=A
   YDB>Kill (A) ; Kill everything except A
   YDB>ZWRite ; Demonstrate that A also has no array
   YDB>Set A=2 ; Create an array
   YDB>ZWRite ; The association survived the Kill
   A=2 ;*
   *B=A
   YDB>

+++++++++++++++++++++++++++++
Annotated Alias Examples
+++++++++++++++++++++++++++++

Example:

.. code-block:: none

   $ /usr/local/lib/yottadb/r120/ydb -run ^tprestart
   tprestart ; Transaction restart variable association also restored on restart
     zprint ; Print this program
     set A="Malvern",C="Pennsylvania",E="USA"
     set *B=C,*D(19355)=E
     write "------------",!
     write "Initial values & association",!
     zwrite
     tstart (B,D) ; On restart: A not restored, B,D restored, C,E restored by association
     if '$TRestart Do  ; Change C,E if first time through
     .set C="Wales",E="UK"
     .kill *D(19355)
     .write "------------",!
     .write "First time through transaction; B,C,D,E changed",!
     .zwrite
     .set A="Brynmawr"
     .kill *B
     .write "------------",!
     .write "A changed; association between B & C and D & E killed; B,D have no value",!
     .zwrite
     .trestart
     else  Do  ; Show restored values on restart
     write "------------",!
     write "Second time through transaction; B,C,D,E & association restored",!
     zwrite
     tcommit ; No global updates in this transaction!
     quit
   ------------
   Initial values & association
   A="Malvern"
   B="Pennsylvania" ;*
   *C=B
   *D(19355)=E
   E="USA" ;*
   ------------
   First time through transaction; B,C,D,E changed
   A="Malvern"
   B="Wales" ;*
   *C=B
   E="UK" ;*
   ------------
   A changed; association between B & C and D & E killed; B,D have no value
   A="Brynmawr"
   C="Wales" ;*
   E="UK" ;*
   ------------
   Second time through transaction; B,C,D,E & association restored
   A="Brynmawr"
   B="Pennsylvania" ;*
   *C=B
   *D(19355)=E
   E="USA" ;*

Note that TROLLBACK does not restore alias variables:

.. code-block:: bash

   /usr/local/lib/yottadb/r120/ydb -run ^tprollback
   tprollback ;
     zprint ; Print this program
     set A(1)=1,A(2)=2,A(3)=3
     set B(1)="1b",*B(2)=A,B(3)=3 ; B includes a container for A
     set *C(1)=B   ; C includes a container for B
     kill *A,*B   ; C is the only way to the data
     write "------------",!
     write "Only containers before transaction:",!
     zwrite
     tstart (C)
     if '$trestart
     .set *D=C(1) ; D is now an alias for what used to be B
     .set D(3)=-D(3)
     .set *D=D(2) ; D is now an alias for what used to be A
     .set D(1)=-D(1)
     .kill *D  ; Kill D after is used to manipulate the arrays
     .write "------------",!
     .write "Changed values before restart:",!
     .zwrite
     .trestart
     write "------------",!
     write "Restored values restart:",!
     zwrite
     kill C ; Kill only handle to arrays
     write "------------",!
     write "No local arrays left:",!
     zwrite
     trollback  ; Rollback transaction, don't commit it
     write "------------",!
     write "Rollback doesnt restore names and local arrays",!
     zwrite
     quit
   ------------
   Only containers before transaction:
   $ZWRTAC=""
   *C(1)=$ZWRTAC1
   $ZWRTAC1(1)="1b"
   *$ZWRTAC1(2)=$ZWRTAC2
   $ZWRTAC2(1)=1
   $ZWRTAC2(2)=2
   $ZWRTAC2(3)=3
   $ZWRTAC1(3)=3
   $ZWRTAC=""
   ------------
   Restored values restart:
   $ZWRTAC=""
   *C(1)=$ZWRTAC1
   $ZWRTAC1(1)="1b"
   *$ZWRTAC1(2)=$ZWRTAC2
   $ZWRTAC2(1)=1
   $ZWRTAC2(2)=2
   $ZWRTAC2(3)=3
   $ZWRTAC1(3)=3
   $ZWRTAC=""
   ------------
   No local arrays left:
   ------------
   Rollback doesnt restore names and local arrays

Example:

.. code-block:: bash

   $ /usr/local/lib/yottadb/r120/ydb -run ^aliasexample; Extended annotated alias example
       zprint
       write "------------",!
       set x="name level",x(1)=1,x(1,2)="1,2",x("foo")="bar"
       write $ZDATA(x),! ; x is a conventional lvn - output 11
       set *y=x ; x an y are now alias variables
       write $ZDATA(x),! ; output appears as 111
       set *a(1)=y ; a(1) is now an alias container variable
       set b="bness",b("b")="bbness" ; b is a conventional lvn
       set *b=a(1) ; b joins x and y as alias variables for the same data
       ; prior b values are lost
       ; set *<name> is equivalent to Kill *<name> Set *<name>
       set y("hi")="sailor" ; Assignment applies to all of {b,x,y}
       kill b("foo") ; Kill applies to all of {b,x,y}
       kill *x ; x is undefined and no longer an alias variable
       ; b and y still provide access to the data
       write a(1),"<",! ; output appears as <
       write a(1)*3,! ; output appears as 0
       write $length(a(1)),! ; output appears as 0
       set c=y,c("legs")="tars" ; c is conventional lvn with value "name level"
       do sub1
       write $Data(c),! ; output is 1
       do sub2(.c)
       set a(1)="" ; a(1) ceases to be an alias container variable
       ; has the value ""
       write $D(i),! ; output is 0
       kill *c,*y ; c and y become undefined lvns
       zwrite b ; output is b("got")="a match"
       ; it is no longer an alias variable
       ; as everything else has gone
       quit
  sub1
      new y ; in this scope y is no longer an alias for b
      set *y=c ; in this scope c and y are alias variables
      kill y("legs") ; Kill apples to all of {c,y}
      kill *y ; in this scope y is no longer an alias for c
      ; this is really redundant as
      ; the Quit implicitly does the same thing
      quit
  sub2(i) ; i and c are joined due to pass-by-reference
      write $ZAHandle(c)=$ZAHandle(i),! ; output appears as 1
      kill b ; data for {b,y} is gone
      ; both are undefined, but remain alias variables
      set *c=a(1) ; c joins {b,y} as alias variable; prior value of c lost
      ; c is no longer alias of i
      write $ZAHandle(c)=$ZAHandle(i),! ; output appears as 0
      set i=a(1) ; Assignment applies to i - value is ""
      wet c("got")="a match" ; Assignment applies to all of {b,c,y)
      quit

  ------------
  11
  111
  <
  0
  0
  1
  1
  0
  0
  b("got")="a match"


-----------------------------------------------------
Extensions for Unicode® standard support
-----------------------------------------------------

To represent and process strings that use international characters, YottaDB processes can use the UTF-8 encoding defined by the Unicode standard. YottaDB Unicode support is optional, and the :code:`--utf8` option of the `ydbinstall / ydbinstall.sh script <../AdminOpsGuide/installydb.html#ydbinstall-script>`_ installs YottaDB with Unicode support.

If the environment variable `ydb_chset <../AdminOpsGuide/basicops.html#ydb-chset>`_ has a value of UTF-8 and the locale is one with UTF-8 support (for example, zh_CN.utf8), a YottaDB process interprets strings as containing characters encoded in the UTF-8 encoding. The locale is determined by the locale setting LC_CTYPE as reported by the :code:`locale` command. In UTF-8 mode, YottaDB no longer assumes that one character is one byte, or that the glyph display width of a character is one. Depending on how ICU is built on a computer system, in order to operate in UTF-8 mode, a YottaDB process may well also need a third environment variable, `ydb_icu_version <../AdminOpsGuide/basicops.html#ydb-icu-version>`_ set appropriately.

If the environment variable ydb_chset has no value, the string "M", or any value other than "UTF-8", YottaDB treats each 8-bit byte as a character, which suffices for English, and many single-language applications.

Object code for routines written in M and which are part of YottaDB reside in a :code:`libyottadbutil.so` shared library. As object code for routines compiled in M mode differs from object code compiled in UTF-8 mode, when UTF-8 support is installed. The former reside in :code:`$ydb_dist/libyottadbutil.so` and the latter in :code:`$ydb_dist/utf8/libyottadbutil.so`. The environment variable `ydb_routines <../AdminOpsGuide/basicops.html#ydb-routines>`_ should therefore include the shared library that matches ydb_chset. Attempting to execute a routine compiled in a mode that does not match ydb_chset results in a `DLLCHSETM <../MessageRecovery/errors.html#dllchsetm>`_ or `DLLCHSETUTF8 <../MessageRecovery/errors.html#dllchsetutf8>`_ error.

M mode and UTF-8 mode are set for the process, not for the database. As a subset of Unicode, ASCII characters ($CHAR() values 0 through 127) are interpreted identically by processes in M and UTF-8 modes. The indices and values in the database are simply sequences of bytes and therefore it is possible for one process to interpret a global node as encoded in UTF-8 and for another to interpret the same node as bytecodes. Note that such an application configuration would be extremely unusual, except perhaps during a transition phase or in connection with data import/export.

In UTF-8 mode, string processing functions (such as $EXTRACT()) operate on strings of multi-byte characters, and can therefore produce different results in M and UTF-8 modes, depending on the actual data processed. Each function has a "Z" alter ego (for example, $ZEXTRACT()) that can be used to operate on sequences of bytes identically in M and UTF-8 modes (that is, in M mode, $EXTRACT() and $ZEXTRACT() behave identically).

In M mode, the concept of an illegal character does not exist. In UTF-8 mode, a sequence of bytes may not represent a valid character, and generates an error when encountered by functions that expect and process UTF-8 strings. During a migration of an application to add support for Unicode, illegal character errors may be frequent and indicative of application code that is yet to be modified. VIEW "NOBADCHAR" suppresses these errors at times when their presence impedes development.

In UTF-8 mode, YottaDB also supports IO encoded in UTF-16 variants as well as in the traditional one byte per character encoding from devices other than $PRINCIPAL.

The following table summarizes YottaDB Unicode support.

+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Extension                   | Explanation                                                                                                                                                                                |
+=============================+============================================================================================================================================================================================+
| $ASCII()                    | IN UTF-8 mode, the $ASCII() function returns the integer Unicode code-point value of a character in the given string. Note that the name $ASCII() is somewhat anomalous for Unicode data   |
|                             | but that name is the logical extension of the function from M mode to UTF-8 mode. For more information and usage examples, refer to                                                        |
|                             | :ref:`ascii-function`.                                                                                                                                                                     |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $Char()                     | In UTF-8 mode, $CHAR() returns a string composed of characters represented by the integer equivalents of the Unicode code-points specified in its argument(s). For more information and    |
|                             | usage examples, refer to :ref:`char-function`.                                                                                                                                             |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $Extract()                  | The $EXTRACT() function returns a substring of a given string. For more information and usage examples, refer to                                                                           |
|                             | :ref:`extract-function`.                                                                                                                                                                   |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $Find()                     | The $FIND() function returns an integer character position that locates the occurrence of a substring within a string. For more information and usage examples, refer to                   |
|                             | :ref:`find-function`.                                                                                                                                                                      |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $Justify()                  | The $JUSTIFY() function returns a formatted string. For more information and usage examples, refer to :ref:`justify-function`.                                                             |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $Length()                   | The $LENGTH() function returns the length of a string measured in characters, or in "pieces" separated by a delimiter specified by its optional second argument. For more information and  |
|                             | usage examples, refer to :ref:`length-function`.                                                                                                                                           |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $Piece()                    | The $PIECE() function returns a substring delimited by a specified string delimiter made up of one or more characters. For more information and usage examples, refer to                   |
|                             | :ref:`piece-function`.                                                                                                                                                                     |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $TRanslate()                | The $TRANSLATE() function returns a string that results from replacing or dropping characters in the first of its arguments as specified by the patterns of its other arguments. For more  |
|                             | information and usage examples, refer to :ref:`translate-function`.                                                                                                                        |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $X                          | For UTF-8 mode and TRM and SD output, $X increases by the display-columns (width in glyphs) of a given string that is written to the current device. For more information and usage        |
|                             | examples, refer to :ref:`x-isv`.                                                                                                                                                           |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZASCII()                   | The $ZASCII() function returns the numeric byte value (0 through 255) of a given sequence of octets (8-bit bytes). For more information and usage examples, refer to                       |
|                             | :ref:`zascii-function`.                                                                                                                                                                    |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZCHset                     | The read-only intrinsic special variable $ZCHSET takes its value from the environment variable ydb_chset. An application can obtain the character set used by a YottaDB process by the     |
|                             | value of $ZCHSET. $ZCHSET can have only two values –"M", or "UTF-8" and it cannot appear on the left of an equal sign in the SET command. For more information and usage examples, refer to|
|                             | :ref:`zchset-isv`.                                                                                                                                                                         |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZCHar()                    | The $ZCHAR() function returns a byte sequence of one or more bytes corresponding to numeric byte value (0 through 255) specified in its argument(s). For more information and usage        |
|                             | examples, refer to :ref:`zchar-function`.                                                                                                                                                  |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZCOnvert()                 | The $ZCONVERT() function returns its first argument as a string converted to a different encoding. The two argument form changes the encoding for case within a character set. The three   |
|                             | argument form changes the encoding scheme. For more information and usage examples, refer to :ref:`zconvert-function`.                                                                     |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZExtract()                 | The $ZEXTRACT() function returns a byte sequence of a given sequence of octets (8-bit bytes). For more information and usage examples, refer to                                            |
|                             | :ref:`zextract-function`.                                                                                                                                                                  |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZFind()                    | The $ZFIND() function returns an integer byte position that locates the occurrence of a byte sequence within a sequence of octets(8-bit bytes). For more information and usage examples,   |
|                             | refer to :ref:`zfind-function`.                                                                                                                                                            |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZJustify()                 | The $JUSTIFY() function returns a formatted and fixed length byte sequence. For more information and usage examples, refer to                                                              |
|                             | :ref:`zjustify-function`.                                                                                                                                                                  |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZLength()                  | The $ZLENGTH() function returns the length of a sequence of octets measured in bytes, or in "pieces" separated by a delimiter specified by its optional second argument. For more          |
|                             | information and usage examples, refer to :ref:`zlength-function`.                                                                                                                          |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZPATNumeric                | ZPATN[UMERIC] is a read-only intrinsic special variable that determines how YottaDB interprets the patcode N used in the pattern match operator. With $ZPATNUMERIC="UTF-8", the            |
|                             | patcode N matches any numeric character as defined by Unicode. By default patcode N only matches the ASCII digits, which are the only digits which M actually treats as numerics. For more |
|                             | information and usage examples, refer to :ref:`zpatnumeric-isv`.                                                                                                                           |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZPIece()                   | The $ZPIECE() function returns a sequence of bytes delimited by a specified byte sequence made up of one or more bytes. In M, $ZPIECE() typically returns a logical field from a logical   |
|                             | record. For more information and usage examples, refer to :ref:`zpiece-function`.                                                                                                          |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZPROMpt                    | $ZPROM[PT] contains a string value specifying the current Direct Mode prompt. By default, YDB> is the Direct Mode prompt. M routines can modify $ZPROMPT by means of a SET command.        |
|                             | $ZPROMPT cannot exceed 31 bytes. If an attempt is made to assign $ZPROMPT to a longer string, YottaDB takes only the first 31 bytes and truncates the rest. With character set UTF-8       |
|                             | specified, if the 31st byte is not the end of a valid UTF-8 character, YottaDB truncates the $ZPROMPT value at the end of last character that completely fits within the 31 byte           |
|                             | limit. For more information and usage examples, refer to :ref:`zprompt-isv`.                                                                                                               |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZSUBstr()                  | The $ZSUBSTR() function returns a properly encoded string from a sequence of bytes. For more information and usage examples, refer to                                                      |
|                             | :ref:`zsubstr-function`.                                                                                                                                                                   |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZTRanslate()               | The $ZTRANSLATE() function returns a byte sequence that results from replacing or dropping bytes in the first of its arguments as specified by the patterns of its other arguments.        |
|                             | $ZTRANSLATE() provides a tool for tasks such as encryption. For more information and usage examples, refer to                                                                              |
|                             | :ref:`ztranslate-function`.                                                                                                                                                                |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZWidth()                   | The $ZWIDTH() function returns the numbers of columns required to display a given string on the screen or printer. For more information and usage examples, refer to                       |
|                             | :ref:`zwidth-function`.                                                                                                                                                                    |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| %HEX2UTF                    | The %HEX2UTF utility returns the encoded character string from the given bytestream in hexadecimal notation. This routine has entry points for both interactive and non-interactive use.   |
|                             | For more information and usage examples, refer to :ref:`hex2utf-util`.                                                                                                                     |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| %UTF2HEX                    | The %UTF2HEX utility returns the hexadecimal notation of the internal byte encoding of a UTF-8 encoded character string. This routine has entry points for both interactive and            |
|                             | non-interactive use. For more information and usage examples, refer to :ref:`utf2hex-util`.                                                                                                |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| [NO]WRAP (USE)              | Enables or disables automatic record termination. When the current record size ($X) reaches the maximum WIDTH and the device has WRAP enabled, YottaDB starts a new record, as if the      |
|                             | routine had issued a WRITE ! command. For more information and usage examples, refer to :ref:`wrap-ioproc`.                                                                                |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| DSE and LKE                 | In UTF-8 mode, DSE and LKE accept characters in Unicode in all their command qualifiers that require file names, keys, or data (such as DSE -KEY, DSE -DATA and LKE -LOCK qualifiers).     |
|                             | For more information, refer to the `LKE <../AdminOpsGuide/mlocks.html>`_ and `DSE <../AdminOpsGuide/dse.html>`_ chapter. For more information                                              |
|                             | and usage examples, refer to the `Administration and Operations Guide <../AdminOpsGuide/index.html>`_.                                                                                     |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| GDE Objects                 | GDE allows the name of a file to include characters in Unicode                                                                                                                             |
|                             |                                                                                                                                                                                            |
|                             | In UTF-8 mode, GDE considers a text file to be encoded in UTF-8 when it is executed via the "@" command. For more information, refer to the                                                |
|                             | `GDE <../AdminOpsGuide/gde.html>`_ chapter in the Administration and Operations Guide.                                                                                                     |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| FILTER[=expr]               | Specifies character filtering for specified cursor movement sequences on devices where FILTER applies.                                                                                     |
|                             |                                                                                                                                                                                            |
|                             | In UTF-8 mode, the usual Unicode line terminators (U+000A (LF), U+0000D (CR), U+000D followed by U+000A (CRLF), U+0085 (NEL), U+000C (FF), U+2028 (LS) and U+2029 (PS)) are recognized. If |
|                             | FILTER=CHARACTER is enabled, all of the terminators are recognized to maintain the values of $X and $Y. For more information, refer to                                                     |
|                             | :ref:`no-filter`.                                                                                                                                                                          |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Job                         | The Job command spawns a background process with the same environment as the M process doing the spawning. Therefore, if the parent process is operating in UTF-8 mode, the Job'd process  |
|                             | also operates in UTF-8 mode. In the event that a background process must have a different mode from the parent, create a shell script to alter the environment as needed, and spawn it with|
|                             | a ZSYstem command, for example, ZSYstem "/path/to/shell/script &", or start it as a PIPE device. For more information and UTF-8 mode examples, refer                                       |
|                             | :ref:`job-command`.                                                                                                                                                                        |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| MUPIP                       | MUPIP EXTRACT                                                                                                                                                                              |
|                             |                                                                                                                                                                                            |
|                             | In UTF-8 mode, MUPIP EXTRACT, MUPIP JOURNAL -EXTRACT and MUPIP JOURNAL -LOSTTRANS write sequential output files in the UTF-8 character encoding form. For example, in UTF-8 mode if ^A has |
|                             | the value of 主要雨在西班牙停留在平原, the sequential output file of the MUPIP EXTRACT command is:                                                                                         |
|                             |                                                                                                                                                                                            |
|                             | 09-OCT-2006 04:27:53 ZWR                                                                                                                                                                   |
|                             |                                                                                                                                                                                            |
|                             | YottaDB MUPIP EXTRACT UTF-8                                                                                                                                                                |
|                             |                                                                                                                                                                                            |
|                             | ^A="主要雨在西班牙停留在平原"                                                                                                                                                              |
|                             |                                                                                                                                                                                            |
|                             | MUPIP LOAD                                                                                                                                                                                 |
|                             |                                                                                                                                                                                            |
|                             | MUPIP LOAD command considers a sequential file as encoded in UTF-8 if the environment variable ydb_chset is set to UTF-8. Ensure that MUPIP EXTRACT commands and corresponding MUPIP LOAD  |
|                             | commands execute with the same setting for the environment variable ydb_chset. The M utility programs %GO and %GI have the same requirement for mode matching. For more information on     |
|                             | MUPIP EXTRACT and MUPIP LOAD, refer to the `General Database Management <../AdminOpsGuide/dbmgmt.html>`_ chapter in the Administration and Operations Guide.                               |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Open                        | In UTF-8 mode, the OPEN command recognizes ICHSET, OCHSET, and CHSET as three additional deviceparameters to determine the encoding of the input/output devices. For more information and  |
|                             | usage examples, refer to :ref:`open-command`.                                                                                                                                              |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Pattern Match Operator (?)  | YottaDB allows the pattern string literals to contain the characters in Unicode. Additionally, YottaDB extends the M standard pattern codes (patcodes) A, C, N, U, L, P and E to           |
|                             | the Unicode character set. For more information, refer to :ref:`pattern-match-op` and                                                                                                      |
|                             | :ref:`zpatnumeric-isv`.                                                                                                                                                                    |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Read                        | In UTF-8 mode, the READ command uses the character set value specified on the device OPEN as the character encoding of the input device. If character set "M" or "UTF-8" is specified, the |
|                             | data is read with no transformation. If character set is "UTF-16", "UTF-16LE", or "UTF-16BE", the data is read with the specified encoding and transformed to UTF-8. If the READ command   |
|                             | encounters an illegal character or a character outside the selected representation, it triggers a run-time error. The READ command recognizes all Unicode line terminators for non-FIXED   |
|                             | devices. For more information and usage examples, refer to :ref:`read-ioproc`.                                                                                                             |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Read #                      | When a number sign (#) and a non-zero integer expression immediately follow the variable name, the integer expression determines the maximum number of characters accepted as the input to |
|                             | the READ command. In UTF-8 or UTF-16 modes, this can occur in the middle of a sequence of combining code-points (some of which are typically non-spacing). When this happens, any display  |
|                             | on the input device, may not represent the characters returned by the fixed-length READ (READ #). For more information and usage examples, refer to                                        |
|                             | :ref:`read-ioproc`.                                                                                                                                                                        |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Read *                      | In UTF-8 or UTF-16 modes, the READ * command accepts one character in Unicode of input and puts the numeric code-point value for that character into the variable. For more information and|
|                             | usage examples, refer to :ref:`read-ioproc`.                                                                                                                                               |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| View "[NO]BADCHAR"          | As an aid to migrating applications to Unicode, this UTF-8 mode VIEW command determines whether Unicode enabled functions trigger errors when they encounter illegal strings. For more     |
|                             | information and usage examples, refer to :ref:`view-command`.                                                                                                                              |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| User-defined Collation      | For some languages (such as Chinese), the ordering of strings according to Unicode code-points (character values) may not be the linguistically or culturally correct ordering. Supporting |
|                             | applications in such languages requires development of collation modules - YottaDB natively supports M collation, but does not include pre-built collation modules for any specific        |
|                             | natural language. Therefore, applications that use characters in Unicode may need to implement their own collation functions. For more information on developing a collation module for    |
|                             | Unicode, refer to :ref:`implement-alt-colltn-seq-unicode-chars`.                                                                                                                           |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Unicode Byte Order Marker   | When ICHSET is UTF-16, YottaDB uses BOM (U+FEFF) to automatically determine the endianess. For this to happen, the BOM must appear at the beginning of the file or data stream. If BOM     |
| (BOM)                       | is not present, YottaDB assumes big endianess. SEEK or APPEND operations require specifying the endianess (UTF-16LE or UTF-16BE) because they do not go to the beginning of the file       |
|                             | or data stream to automatically determine the endianess. When endianess is not specified, SEEK or APPEND assume big endianess.                                                             |
|                             |                                                                                                                                                                                            |
|                             | If the character set of a device is UTF-8, YottaDB checks for and ignores a BOM on input.                                                                                                  |
|                             |                                                                                                                                                                                            |
|                             | If the BOM does not match the character set specified at device OPEN, YottaDB produces an error. READ does not return BOM to the application and the BOM is not counted as part of the     |
|                             | first record.                                                                                                                                                                              |
|                             |                                                                                                                                                                                            |
|                             | If the output character set for a device is UTF-16 (but not UTF-16BE or UTF-16LE,) YottaDB writes a BOM before the initial output. The application code does not need to explicitly        |
|                             | write the BOM.                                                                                                                                                                             |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| WIDTH=intexpr (USE)         | In UTF-8 mode and TRM and SD output, the WIDTH deviceparameter specifies the display-columns and is used with $X to control the truncation and WRAPping of the visual representation of the|
|                             | stream. For more information and usage examples, refer to :ref:`width-ioproc`.                                                                                                             |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Write                       | In UTF-8 mode, the WRITE command uses the character set specified on the device OPEN as the character encoding of the output device. If character set specifies "M" or "UTF-8",            |
|                             | YottaDB WRITEs the data with no transformation. If character set specifies "UTF-16", "UTF-16LE" or "UTF-16BE", the data is assumed to be encoded in UTF-8 and WRITE transforms it to       |
|                             | the character encoding specified by the character set device parameter. For more information and usage examples, refer to                                                                  |
|                             | :ref:`write-ioproc`.                                                                                                                                                                       |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Write *                     | When the argument of a WRITE command consists of a leading asterisk (*) followed by an integer expression, the WRITE command outputs the character represented by the code-point value of  |
|                             | that integer expression. For more information and usage examples, refer to :ref:`write-ioproc`.                                                                                            |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| ZSHow                       | In UTF-8 mode, the ZSHOW command exhibits byte-oriented and display-oriented behavior as follows:                                                                                          |
|                             |                                                                                                                                                                                            |
|                             | - ZSHOW targeted to a device (ZSHOW "*") aligns the output according to the numbers of display columns specified by the WIDTH deviceparameter.                                             |
|                             |                                                                                                                                                                                            |
|                             | For more information and usage examples, refer to :ref:`zshow-dest-vars`.                                                                                                                  |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Philosophy of YottaDB's support for the Unicode® standard
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

With the support for the Unicode® standard, there is no change to the YottaDB database engine or to the way that data is stored and manipulated. YottaDB has always allowed indices and values of M global and local variables to be either canonical numbers or any arbitrary sequence of bytes. There is also no change to the character set used for M source programs. M source programs have always been in ASCII (standard ASCII - $C(0) through $C(127) - is a proper subset of the UTF-8 encoding specified by the Unicode standard). YottaDB accepts some non-ASCII characters in comments and string literals.

The changes in YottaDB to support the Unicode® standard are principally enhancements to M language features. Although conceptually simple, these changes fundamentally alter certain previously ingrained assumptions. For example:

1. The length of a string in characters is not the same as the length of a string in bytes. The length of a Unicode string in characters is always less than or equal to its length in bytes.
2. The display width of a string on a terminal is different from the length of a string in characters - for example, with Unicode, a complex glyph may actually be composed of a series of glyphs or component symbols, each in turn a UTF-8 encoded character in a Unicode string.
3. As a glyph may be composed of multiple characters, a string in Unicode can have canonical and non-canonical forms. The forms may be conceptually equivalent, but they are different strings of characters in Unicode.

.. note::
   YottaDB treats canonical and non-canonical versions of the same string as different and unequal. YottaDB recommends that applications be written to use canonical forms. Where conformance to a canonical representation of input strings cannot be assured, application logic linguistically and culturally correct for each language should convert non-canonical strings to canonical strings.

Applications may operate on a combination of character and binary data - for example, some strings in the database may be digitized images of signatures and others may include escape sequences for laboratory instruments. Furthermore, since M applications have traditionally overloaded strings by storing different data items as pieces of the same string, the same string may contain both Unicode and binary data. YottaDB has functionality to allow a process to manipulate Unicode strings as well as binary data including strings containing both Unicode and binary data.

The YottaDB design philosophy is to keep things simple, but no simpler than they need to be. There are areas of processing where the use of Unicode adds complexity. These typically arise where interpretations of lengths and interpretations of characters interact. For example:

1. A sequence of bytes is never illegal when considered as binary data, but can be illegal when treated as a UTF-8 string. The detection and handling of illegal UTF-8 strings adds complexity, especially when binary and UTF-8 data reside in different pieces of the same string.

2. Since binary data may not map to graphic UTF-8 characters, the ZWRite format must represent such characters differently. A sequence of bytes that is output by a process interpreting it as UTF-8 data may require processing to form correctly input to a process that is interpreting that sequence as binary, and vice versa. Therefore, when performing IO operations, including MUPIP EXTRACT and MUPIP LOAD operations in ZWR format, ensure that processes have the compatible environment variables and/or logic to generate the desired output and correctly read and process the input.

3. Application logic managing input/output that interacts with human beings or non-YottaDB applications requires even closer scrutiny. For example, fixed length records in files are always defined in terms of bytes. In Unicode-related operations, an application may output data such that a character would cross a record boundary (for example, a record may have two bytes of space left, and the next UTF-8 character may be three bytes long), in which case YottaDB fills the record with one or more pad bytes. When a padded record is read as UTF-8, trailing pad bytes are stripped by YottaDB and not provided to the application code.

For some languages (such as Chinese), the ordering of strings according to UTF-8 code-points (character values) may not be the linguistically or culturally correct ordering. Supporting applications in such languages requires development of collation modules - YottaDB natively supports M collation, but does not include pre-built collation modules for any specific natural language.

**Glyphs and Unicode Characters**

Glyphs are the visual representation of text elements in writing systems and Unicode code-points are the underlying data. Internally, YottaDB stores UTF-8 encoded strings as sequences of Unicode code-points. A Unicode compatible output device - terminal, printer or application - renders the characters as sequences of glyphs that depict the sequence of code-points, but there may not be a one-to-one correspondence between characters and glyphs.

For example, consider the following word from the Devanagari writing system.

अच्छी

On a screen or a printer, it is displayed in 4 columns. Internally, YottaDB stores it as a sequence of 5 Unicode code-points:

+-----------+---------------------+--------------------------------+----------------------------------------------+
| Number    | Character           | Unicode code-point             | Name                                         |
+===========+=====================+================================+==============================================+
| 1         | |dev-a|             | U+0905                         | DEVANAGARI LETTER A                          |
+-----------+---------------------+--------------------------------+----------------------------------------------+
| 2         | |dev-ca|            | U+091A                         | DEVANAGARI LETTER CA                         |
+-----------+---------------------+--------------------------------+----------------------------------------------+
| 3         | |dev-sign-virama|   | U+094D                         | DEVANAGARI SIGN VIRAMA                       |
+-----------+---------------------+--------------------------------+----------------------------------------------+
| 4         | |dev-cha|           | U+091B                         | DEVANAGARI LETTER CHA                        |
+-----------+---------------------+--------------------------------+----------------------------------------------+
| 5         | |dev-sign-ii|       | U+0940                         | DEVANAGARI VOWEL SIGN II                     |
+-----------+---------------------+--------------------------------+----------------------------------------------+

The Devanagari writing system (U+0900 to U+097F) is based on the representation of syllables as contrasted with the use of an alphabet in English. Therefore, it uses the half-form of a consonant to represent certain syllables. The above example uses the half-form of the consonant (U+091A).

Although the half-form form consonant is a valid text element in the context of the Devanagari writing system, it does not map directly to a character in the Unicode Standard. It is obtained by combining the DEVANAGARI LETTER CA, with DEVANAGARI SIGN VIRAMA, and DEVANAGARI LETTER CHA.

.. code-block:: none

   च + ्  +  छ  =  च्छ

On a screen or a printer, the terminal font detects the glyph image of the half-consonant and displays it at the next display position. Internally, YottaDB uses ICU's glyph-related conventions for the Devanagari writing system to calculate the number of columns needed to display it. As a result, YottaDB advances $X by 1 when it encounters the combination of the 3 Unicode code-points that represent the half-form consonant.

To view this example at the YottaDB prompt, type in the following command sequence:

.. code-block:: bash

   YDB>write $ZCHSET
   UTF-8
   YDB>set DS=$char($$FUNC^%HD("0905"))_$char($$FUNC^%HD("091A"))_$char($$FUNC^%HD("094D"))
   YDB>set DS=DS_$char($$FUNC^%HD("091B"))_$char($$FUNC^%HD("0940"))
   YDB>write $zwidth(DS); 4 columns are required to display local variable DS on the screen.
   4
   YDB>write $length(DS); DS contains 5 characters or Unicode code-points.
   5
   YDB>

For all writing systems supported by Unicode, a character is a code-point for string processing, network transmission, storage, and retrieval of Unicode data whereas a character is a glyph for displaying on the screen or printer. This holds true for many other popular programming languages. Keep this distinction in mind throughout the application development life-cycle.

+++
ICU
+++

ICU is a widely used, defacto standard package (see http://icu-project.org for more information) that YottaDB relies on for most operations that require knowledge of the Unicode® character sets, such as text boundary detection, character string conversion between UTF-8 and UTF-16, and calculating glyph display widths.

.. note::
   Unless Unicode support is sought for a process (that is, unless the environment variable ydb_chset is UTF8), YottaDB processes do not need ICU. In other words, existing, non-Unicode, applications continue to work on supported platforms without ICU.

An ICU version number is of the form major.minor.milli.micro where major, minor, milli and micro are integers. Two versions that have different major and/or minor version numbers can differ in functionality, and API compatibility is not guaranteed. The differences in milli or micro versions are maintenance releases that preserve functionality and API compatibility. ICU reference releases are defined by major and minor version numbers. Note that display widths for some characters changed in ICU 4.0 and may change again in the future, as both languages and ICU evolve.

An operating system's distribution generally includes an ICU library tailored to the OS and hardware, therefore, YottaDB does not provide any ICU libraries. In order to support Unicode functionality, YottaDB requires an appropriate version of ICU to be installed on the system - check the release notes for your YottaDB release for supported ICU versions.

YottaDB expects ICU to be compiled with symbol renaming disabled and will issue an error at startup if the available version of ICU is built with symbol renaming enabled. To use a version of ICU built with symbol renaming enabled, the $ydb_icu_version environment variable indicates the MAJOR VERSION and MINOR VERSION numbers of the desired ICU formatted as MajorVersion.MinorVersion (for example "3.6" to denote ICU-3.6). When $ydb_icu_version is so defined, YottaDB attempts to open the specific version of ICU. In this case, YottaDB works regardless of whether or not symbols in this ICU have been renamed. A missing or ill-formed value for this environment variable causes YottaDB to only look for non-renamed ICU symbols. The release notes for each YottaDB release identify the required reference release version number as well as the milli and micro version numbers that were used to test YottaDB prior to release. In general, it should be safe to use any version of ICU with the specific ICU reference version number required and milli and micro version numbers greater than those identified in the release notes for that YottaDB version.

ICU supports multiple threads within a process, and an ICU binary library can be compiled from source code to either support or not support multiple threads. In contrast, YottaDB does not support multiple threads within a YottaDB process. On some platforms, the stock ICU library, which is usually compiled to support multiple threads, may work unaltered with YottaDB. On other platforms, it may be required to rebuild ICU from its source files with support for multiple threads turned off. Refer to the release notes for each YottaDB release for details about the specific configuration tested and supported. In general, the YottaDB team's preference for ICU binaries used for each YottaDB version are, in decreasing order of preference:

1. The stock ICU binary provided with the operating system distribution.
2. A binary distribution of ICU from the download section of the ICU project page.
3. A version of ICU locally compiled from source code provided by the operating system distribution with a configuration disabling multi-threading.
4. A version of ICU locally compiled from the source code from the ICU project page with a configuration disabling multi-threading.

YottaDB uses the POSIX function dlopen() to dynamically link to ICU. In the event you have other applications that require ICU compiled with threads, place the different builds of ICU in different locations, and use the dlopen() search path feature (for example, the LD_LIBRARY_PATH environment variable on Linux) to enable each application to link with its appropriate ICU.

+++++++++++++++++++++++++++++++++++++++
Discussion and Best Practices
+++++++++++++++++++++++++++++++++++++++

~~~~~~~~~~~~~~~~~~~
Data Interchange
~~~~~~~~~~~~~~~~~~~

The support for Unicode® in YottaDB only affects the interpretation of data in databases, and not databases themselves, a simple way to convert from a ZWR format extract in one mode to an extract in the other is to load it in the database using a process in the mode in which it was generated, and to once more extract it from the database using a process in the other mode.

If a sequence of 8-bit octets contains bytes other than those in the ASCII range (0 through 127), an extract in ZWR format for the same sequence of bytes is different in "M" and "UTF-8" modes. In "M" mode, the $C() values in a ZWR format extract are always equal to or less than 255. In "UTF-8" mode, they can have larger values - the code-points of legal characters in Unicode can be far greater than 255.

Note that the characters written to the output device are subject to the OCHSET transformation of the controlling output device. If OCHSET is "M", the multi-byte characters are written in raw bytes without any transformation.

1. Each multi-byte graphic character (as classified by $ZCHSET) is written directly to the device converted to the encoding form specified by the OCHSET of the output device.
2. Each multi-byte non-graphic character (as classified by $ZCHSET) is written in $CHAR(nnnn) notation, where nnnn is the decimal character code (that is, code-point up to 1114111 if $ZCHSET="UTF-8" or up to 255 if $ZCHSET="M").
3. If $ZCHSET="UTF-8" and a subscript or data contains a malformed UTF-8 byte sequence, ZWRITE treats each byte in the sequence as a separate malformed character. Each such byte is written in $ZCHAR(nn[,...]) notation, where each nn is the corresponding byte in the illegal UTF-8 byte sequence.

Note that attempts to use ZWRITE output from a system as input to another system using a different character set may result in errors or not yield the same state as existed on the source system. Application developers can deal with this by defining and using one or more pattern tables that declare all non-ASCII characters (or any useful subset thereof) to be non-graphic. For more details on defining pattern tables, please refer to :ref:`pattern-code-defn`.

~~~~~~~~~~~~
Limitations
~~~~~~~~~~~~

**User-defined pattern codes are not supported**

Although the M standard patcodes (A,C,L,U,N,P,E) are extended to work with Unicode, application developers can neither change their default classification nor define the non-standard patcodes ((B,D,F-K,M,O,Q-T,V-X) beyond the ASCII subset. This means that the pattern tables cannot contain characters with codes greater than the maximum ASCII code 127.

**String Normalization**

In YottaDB, strings are not implicitly normalized. Unicode normalization is a method of computing canonical representation of the character strings. Normalization is required if the strings contain combination characters (such as accented characters consisting of a base character followed by an accent character) as well as precomposed characters. The Unicode™ standard has assigned code-points to such precomposed characters for backward compatibility with legacy code sets. For the applications containing both versions of the same character (or combining characters), Unicode recommends one of the normal forms. Because YottaDB does not normalize strings, the application developers must develop the functionality of normalizing the strings, as needed, in order for string matching and string collation to behave in a conventional and wholesome fashion. In such a case, edit checks can be used that only accept a single representation when multiple representations are possible.

**UTF-16 is not supported for $PRINCIPAL Device**

YottaDB does not support UTF-16, UTF-16LE and UTF-16BE encodings for $PRINCIPAL I/O devices (including Terminal, Sequential and Socket devices). In order to perform Unicode™-related I/O with the $PRINCIPAL device, application developers must use "UTF-8" for the ICHSET or OCHSET deviceparameters.

**UTF-16 is not supported for Terminal Devices**

Due to the uncommon usage and lack of support for UTF-16 by UNIX terminals and terminal emulators, YottaDB does not support UTF-16, UTF-16LE and UTF-16BE encodings for Terminal I/O devices. Note that UNIX platforms use UTF-8 as the defacto character encoding for Unicode. The terminal connections from remote hosts (such as Windows) must communicate with YottaDB in UTF-8 encoding.

**Error Messages are in [American] English**

YottaDB has no facility for a translation of product error messages or on-line help into languages other than [American] English. All error message text (except the messages arguments that could include Unicode™ data) is in the [American] English language.

~~~~~~~~~~~~~~~~~~~~~~~~~
Performance and Capacity
~~~~~~~~~~~~~~~~~~~~~~~~~

With the use of "UTF-8" as YottaDB's internal character encoding, the additional requirements for CPU cycles, excluding collation algorithms, should not increase significantly compared with the identical application using the "M" character set. Additional memory requirements for "UTF-8" vary depending on the application as well as the actual character set used. For example, applications based on Latin-1 (2-byte encoded) characters may require up to twice the memory and those based on Chinese/Japanese (3-byte encoded) characters may require up to three times the memory compared to an identical application using "M" characters. The additional disk-space and I/O performance trade-offs for "UTF-8" also vary based on the application and the characters used.

**Characters in arguments exchanged with external routines must be validated by the external routines**

YottaDB does not check for illegal characters in a string before passing it to an external routine or in a returned value before assigning it to a YottaDB variable. This is because such checks add parameter-processing overhead. The application must ensure that the strings are in the encoding form expected by the respective routines. More robustly, external routines must interpret passed strings based on the value of the intrinsic variable $ZCHSET or the environment variable ydb_chset. The external routines can perform validation if needed.

~~~~~~~~~~~~~~~~~~
Maximums
~~~~~~~~~~~~~~~~~~

In prior versions of YottaDB, the restrictions on certain objects were put in place with the assumption that a character is represented by a single byte. With support for Unicode enabled in YottaDB, the following restrictions are in terms of bytes- not characters.

**M Name Length**

The maximum length of an M identifier is restricted to 31 bytes. Since identifier names are restricted to be in ASCII, programmers can define M names up to 31 characters long.

**M String Length**

The maximum length of an M string is restricted to 1,048,576 bytes (1Mib). Therefore, depending on the characters used, the maximum number of characters could be reduced from 1,048,576 characters to as few as 262,144 (256K) characters.

**M Source Line Length**

The maximum length of a program or indirect source line is restricted to 2,048 bytes. Application developers must be aware of this byte limit if they consider using multi-byte source comments or string literals in a source line.

**Database Key and Record Sizes**

The maximum allowed size for database keys (both global and nref keys) is 255 bytes, and for database records is 32K bytes. Application developers must be aware that keys or data containing multi-byte characters in Unicode are limited at a smaller number of characters than the number of available bytes.

~~~~~~~~~~~~~~~~~~~~~~~~~~
Golden Rules
~~~~~~~~~~~~~~~~~~~~~~~~~~

Adhere to the following rules of thumb to design and develop Unicode-based applications for deployment on YottaDB.

* YottaDB functionality related to Unicode becomes available only in UTF-8 mode.
* [At least] in UTF-8 mode, byte manipulation must use Z* equivalent functions.
* In M mode, standard functions are always identical to their Z equivalents.
* Use the same character set for all globals names and subscripts in an instance.
* Define a collation system according to the linguistic and cultural tenets of the language used.
* Create the application logic to ensure strings used as keys are canonical.
* Specify CHSET="M" or otherwise handle illegal characters during the I/O operations.
* Communicate with any external routines using a compatible character encoding form.
* Compile and run programs in the same setting of $ZCHSET and "BADCHAR".

.. |dev-a|    unicode:: U+0905 .. DEVANAGARI LETTER A
.. |dev-ca|    unicode:: U+091A .. DEVANAGARI LETTER CA
.. |dev-sign-virama|    unicode:: U+094D .. DEVANAGARI SIGN VIRAMA
.. |dev-cha|    unicode:: U+091B .. DEVANAGARI LETTER CHA
.. |dev-sign-ii|    unicode:: U+0940 .. DEVANAGARI VOWEL SIGN II




