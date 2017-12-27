
.. index::
   Language Extensions

================================
YottaDB/GT.M Language Extensions
================================

.. contents::
   :depth: 2

In addition to providing all of the ANSI standard M features, YottaDB/GT.M offers a number of language extensions. In this chapter, the language extensions are grouped by intended function to demonstrate their relationships to each other and to the programming process. A summary table is provided in each section. For a full description of a particular extension, refer to its complete entry in the “Commands”, “Functions”, or “Intrinsic Special Variables” chapter.

The following sections describe the YottaDB/GT.M language extensions listed below:

* UNIX interface facilities
* Debugging tools
* Exception-handling extensions
* Journaling extensions
* Extensions providing additional capability
* Device Handling Extensions
* Alias Variables Extensions
* Extensions for Unicode Support

--------------------------------------
Operating System Interface Facilities
--------------------------------------

To improve efficiency and reduce duplication and inconsistency, YottaDB/GT.M is closely integrated with the host operating system environment. With YottaDB/GT.M you can gain access to the operating system facilities to examine:

* System information, such as quotas and SIDs
* Jobs and processes
* Directories and files
* Devices
* Messages
* Privileges

The following table summarizes the YottaDB/GT.M operating system interface facilities.

**Operating System Interface Facilities**

+---------------------------------+---------------------------------------------------------------------------------------------------------------------+
| Extension                       | Explanation                                                                                                         |
+=================================+=====================================================================================================================+
| ZSYstem                         | Provides access to the shell.                                                                                       |
+---------------------------------+---------------------------------------------------------------------------------------------------------------------+
| $ZMessage()                     | Translates an error condition code into text form.                                                                  |
+---------------------------------+---------------------------------------------------------------------------------------------------------------------+
| $ZCMdline                       | Contains a string value specifying the "excess" portion of the command line that invoked the YottaDB/GT.M process.  |
+---------------------------------+---------------------------------------------------------------------------------------------------------------------+
| $ZJob                           | Holds the pid of the process created by the last JOB command performed by the current process.                      |
+---------------------------------+---------------------------------------------------------------------------------------------------------------------+
| $ZPARSE()                       | Parses a UNIX filename.                                                                                             |
+---------------------------------+---------------------------------------------------------------------------------------------------------------------+
| $ZSEARCH()                      | Searches for one or more UNIX files.                                                                                |
+---------------------------------+---------------------------------------------------------------------------------------------------------------------+
| $ZSYstem                        | Contains the status code of the last ZSYSTEM.                                                                       |
+---------------------------------+---------------------------------------------------------------------------------------------------------------------+
| $ZTRNLNM()                      | Translates an environment variable.                                                                                 |
+---------------------------------+---------------------------------------------------------------------------------------------------------------------+
| $ZDIRectory                     | Contains current working directory.                                                                                 |
+---------------------------------+---------------------------------------------------------------------------------------------------------------------+

-------------------------------------------
Debugging Facilities
-------------------------------------------

YottaDB/GT.M provides a number of debugging features. These features include the ability to:

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

The following table summarizes the YottaDB/GT.M language extensions that facilitate debugging.

**YottaDB/GT.M Debugging Tools**

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

The YottaDB/GT.M exception trapping allows you to do the following:

* DO a recovery routine and resume the original command stream.
* GOTO any special handling; an extended ZGOTO provides for context management.
* Report an error and enter Direct Mode for debugging.
* OPEN Input/Output devices with specific traps in addition to the main trap.
* Trap and process an exception based on a device error.
* Trap and process an exception based on terminal input.

The following table summarizes the YottaDB/GT.M language extensions that facilitate exception handling.

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
| $ZTrap                         | Contains an XECUTE string or entryref that YottaDB/GT.M invokes upon encountering an exception condition.                  |
+--------------------------------+----------------------------------------------------------------------------------------------------------------------------+
| EXCEPTION                      | Provides a deviceparameter specifying an XECUTE string or entryref that YottaDB/GT.M invokes upon encountering a device-   |
|                                | related exception condition.                                                                                               |
+--------------------------------+----------------------------------------------------------------------------------------------------------------------------+


-----------------------------------
Journaling Extensions
-----------------------------------

Journaling records redundant copies of database update information to increase protection against loss of information due to hardware and software failure. In YottaDB/GT.M, TSTART and TCOMMIT mark the beginning and end of an application (logical) transaction, which may consist of multiple global variable updates. When a TCOMMIT takes $TLEVEL from one (1) to zero (0), it transfer all of the transaction updates to the journal file, and, except if TRANSACTIONID="BATCH", returns control to the application only after the associated records reach the secondary storage holding the journal file.

The following table summarizes the YottaDB/GT.M language extensions for journaling.

**Journaling Extensions**

+-------------------------------+---------------------------------------------------------------------------------------------------------------------+
| Extensions                    | Explanation                                                                                                         |
+===============================+=====================================================================================================================+
| View                          | Extended to ensure that YottaDB/GT.M has transferred all updates to the journal file.                               |
+-------------------------------+---------------------------------------------------------------------------------------------------------------------+
| $View()                       | Extended for examining journaling status.                                                                           |
+-------------------------------+---------------------------------------------------------------------------------------------------------------------+

---------------------------------------
Extensions for Additional Capability
---------------------------------------

For ways to adjust some process operating characteristics, see the command description “View”. For ways to get information about certain process operating characteristics, see the function description “$View()”.

In YottaDB/GT.M, support of environment specification for global names and resource names is possible. It is possible to excercise user code to customize interpretation of the environment specification. See Chapter 5: “General Language Features of M” for details.

The following table summarizes YottaDB/GT.M extensions that increase general capability.

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
| $View()                      | Examines the YottaDB/GT.M environment.                                                                                |
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
| $ZMAXTPTIme                  | Contains an integer value indicating the time duration YottaDB/GT.M should wait for the completion of all activities  |
|                              | fenced by the current transaction's outermost TSTART/TCOMMIT pair.                                                    |
+------------------------------+-----------------------------------------------------------------------------------------------------------------------+
| $ZROutines                   | Maintains the list of directories to search during look-ups of object and source files.                               |
+------------------------------+-----------------------------------------------------------------------------------------------------------------------+
| $ZSYstem                     | Returns the status code for the last subprocess invoked with the ZSYSTEM command.                                     |
+------------------------------+-----------------------------------------------------------------------------------------------------------------------+
| $ZVERsion                    | Contains a designation of the current version name, level, and operating system.                                      |
+------------------------------+-----------------------------------------------------------------------------------------------------------------------+

\* The ZALLOCATE and ZDEALLOCATE commands are provided for compatibility with other M systems. However, YottaDB/FIS recommends use of the standard LOCK command, which provides an incremental locking facility. The incremental lock provides both flexibility and greater compatibility with the M language standard.

\*\* The $ZPREVIOUS function is provided for compatibility with previous versions of YottaDB/GT.M and other M systems. However, YottaDB/FIS recommends use of the standard two-argument form for the $ORDER function.

-----------------------------
Device Handling Extensions
-----------------------------

In the earlier versions of the M standard, device behavior was defined as a framework, with the details left to the implementations. YottaDB/GT.M supports Terminals, Sequential Disks, FIFOs, PIPEs and a Null device under this model. Subsequently device mnemonicspaces were added to the standard and some of them defined. YottaDB/GT.M supports the SOCKET device under this model with some extensions identified with controlmnemonics starting with the letter "Z."

For details of YottaDB/GT.M device handling see Chapter 9: “Input/Output Processing”.

---------------------------
Alias Variable Extensions
---------------------------

Alias variables provide a layer of abstraction between the name of a local variable and an array analogous to that provided by M pass by reference in routines and function calls. Multiple local variables can be aliased to the same array, and a SET or KILL to one acts as a SET or KILL to all. Alias container variables provide a way using a subscripted local to store a reference to an entire local variable array, which protects the associated array even when it's not accessible through any current local variable name.

YottaDB/GT.M aliases provide low level facilities on which an application can implement object-oriented techniques. An object can be mapped onto, and stored and manipulated in an array, then saved in an alias container variable whence it can be retrieved for processing. The use of appropriate subscripts in the array used for a container, provides a way to organize the stored objects and retrieve them by using the $ORDER() function to traverse the container array. The use of alias variables to implement objects provides significant efficiencies over traditional local variables because alias variables and alias container variables eliminate the need to execute MERGE commands to move objects.

Example:

.. parsed-literal::
   GTM>kill A,B
   GTM>set A=1,*B=A ; B & A are aliases
   GTM>write B
   1
   GTM> 

Within the context of Alias Variables extensions:

* array is very similar to its definition in the M standard, and means an entire tree of nodes, including the root and all descendants, except that it only applies to local variables and not to global variables.
* "Associated alias variables" means all alias variables and all alias container variables associated with an array.
* lvn is very similar to its definition in the M standard except that in the context of alias variables lvn is used to refer to a local variable name with a subscript.
* lname is very similar to its definition in the M standard, except that in the context of alias variables, lname is just the name of an unsubscripted local variable (root of an array).
* "Data cell" and "node" are synonyms.

The following table summarizes Alias Variables extensions. 

**Alias Variables Extensions**

+----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Extension            | Explanation                                                                                                                                                              |
+======================+==========================================================================================================================================================================+
| Set *                | Explicitly creates an alias. For more information, refer to the description of SET * in “Set”                                                                            |
+----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Kill *               | Removes the association between its arguments, and any associated data cells. For more information, refer to the description of KILL * in “Kill”                         |
+----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Quit *               | When QUIT * terminates an extrinsic function or an extrinsic special variable, it always returns an alias container. For more information, refer to the description of   |
|                      | QUIT * in “Quit”.                                                                                                                                                        |
+----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| ZWrite / ZSHow "V"   | Produces Alias Variables format output. For more information, refer to “ZWRITE Format for Alias Variables”                                                               |
+----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| New                  | For the scope of the NEW, a NEW of a name suspends its alias association. For more information, refer to “New”.                                                          |
+----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Exclusive New        | Create a scope in which some associations between an lname or an lvn and an array may be invisible. For more information, refer to “New”.                                |
+----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZAHandle()          | returns a unique identifier (handle) for the array associated with an lname or an alias container; for an subscripted lvn that is not an alias container, it returns an  |
|                      | empty string. For more information, refer to “$ZAHandle()”                                                                                                               |
+----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZDATA()             | Extends $DATA() to reflect the current alias state of the lvn or lname argument in order to identify alias and alias container variables. For more information, refer to |
|                      | “$ZDATA()”.                                                                                                                                                              |
+----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| View and $View()     | VIEW provides LV_GCOL, LV_REHASH, and STP_GCOL to perform garbage collection and local variable lookup table reorganization operations which normally happen             |
|                      | automatically at appropriate times. For more information on the keywords of the VIEW command, refer to “Key Words in VIEW Command”.                                      |
|                      |                                                                                                                                                                          |
|                      | $VIEW() provides LV_CREF, LV_GCOL, and LV_REF. YottaDB/FIS uses the LC_CREF, LV_GCOL, LV_REF keywords in testing and is documenting them to ensure completeness in       |
|                      | product documentation. They may (or may not) be useful during application development for debugging or performance testing implementation alternatives. For more         |
|                      | information the keywords of $VIEW(), refer to “Argument Keywords of $VIEW()”.                                                                                            |
+----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| TSTART, RESTART, and | TSTART command can optionally list names whose arrays are restored on a transaction RESTART. If any of these are alias variables or have nodes which are alias container |
| ROLLBACK             | variables, their associations are also restored on transaction RESTART. For more information, refer to Chapter 6: “Commands”.                                            |
+----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------+

+++++++++++++++++++++
Definitions
+++++++++++++++++++++

**Alias Variables**

Alias Variables provide access to an array through multiple names. Conceptually an alias variable is the same as a pass-by-reference joining of multiple variable names, except that the joining of alias variables is explicit, whereas that of variables passed by reference is implicit. Indeed, the underlying implementation of alias variables and pass-by-reference within YottaDB/GT.M is the same.

* All alias variables associated with the same array are equivalent in their access to its nodes - for example, a SET of a node in an array via one name is no different than a SET to that node using any other name of which it is an alias. Nothing about the order of their creation or association has any significance.
* Once an array becomes accessible via only a single unsubscripted name, YottaDB/GT.M treats that name as a traditional local variable.
* YottaDB/GT.M treats variables joined through pass-by-reference as a special variant of an alias variable. Pass-by-reference relates to the M stack model with aliasing implicit as a side effect of invocation with DO or $$ and unaliasing implicit as a side effect of QUIT. In the broader alias case, program commands directly alias and unalias names without any binding to the M stack.
* YottaDB/GT.M treats the state of a TP (Transaction Processing) RESTART variable as an internal alias, which it only exposes if the transaction creating it RESTARTs.
* YottaDB/GT.M treats variables hidden by exclusive NEW as a type of alias.
* Owing to their implicit behavior, under certain circumstances, pass-by-reference aliases, RESTART variable and exclusive NEW aliases are not entirely symmetrical with respect to explicitly created alias variables (that is, they may come and go at different times, whereas alias variables come and go under application program control).

**Alias Container Variables**

Alias container variables are subscripted lvns that protect arrays for subsequent access by an alias variable. Since accessing an array requires a name, aliasing a name with the alias container regains access to an array stored in a container. For example:

.. parsed-literal::
   GTM>kill A,B,C
   GTM>set A=1,*C(2)=A ; C(2) is a container
   GTM>zwrite
   A=1 ;*
   \*C(2)=A
   GTM>set \*B=C(2) ; B is now an alias
   GTM>write B,":",$length(C(2)),":" ; An alias variable provides access but a container doesn't
   1:0:
   GTM>



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

There is no reason to avoid aliases in any situation, but in those two contexts, YottaDB/GT.M rewards attention to tidy design. YottaDB/GT.M uses garbage collection to manage the storage used for local variables. Increasing the use of local variables, for example, to implement objects, will increase the need for garbage collection, even though the garbage collector and storage management are designed to be light weight and self-tuning. The use of alias variables to implement objects, however, is as efficient as any other method is likely to be, and except for the normal admonition to not keep arrays and local variables around when they are not needed, and to not create levels of contexts over and above those actually needed by application logic, use alias variables as liberally as your application needs dictate.

+++++++++++++++++++++++
ZWRITE/ZSHOW "V" Format
+++++++++++++++++++++++

ZWRITE as applied to local variables and ZSHOW "V" are conceptually similar, with two differences:

* ZWRITE allows the use of patterns to specify variables and subscripts to display whereas ZSHOW "V" applies to all local variables. 
* ZSHOW "V" optionally allows the output to be directed to a global or local variable, whereas ZWRITE always directs its output to the current output device.

For more information on the ZWRITE / ZSHOW "V" format for alias variables, refer to “ZWRITE Format for Alias Variables”.

++++++++++++++++++++++++
Pass By Reference
++++++++++++++++++++++++

YottaDB/GT.M's underlying implementation of pass-by-reference and alias variables is the same. As illustrated by the program "killalias" above, ZWRITE displays variables joined though pass-by-reference using alias conventions. Pass-by-reference is distinguished from alias variables by its implicit creation and elimination. Note the interaction between pass by reference and alias variables when the association of a formallist parameter in a subprogram is changed:

.. parsed-literal::
   $ /usr/lib/fis-gtm/V5.4-002B/gtm -run ^switchalias
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
      set \*X=B,X=4 ; Change association of formallist parameter
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
    \*X=A
    ------------
    Note changed association
    A=3
    B=4 ;*
    \*X=B
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

.. parsed-literal::
   makealias(var)
   quit \*var

The makecntr function returns an alias container of the argument:

.. parsed-literal::
   makecntnr(var)
   new cont
   set \*cont(1)=var
   quit \*cont(1)

+++++++++++++++++++
KILL* Examples
+++++++++++++++++++

Example:

.. parsed-literal::
   GTM>Set A=1,*B=A ; Create an array and an association
   GTM>ZWRite ; Show that the array and association exist
   A=1 ;*
   \*B=A
   GTM>Kill \*A ; Remove the association for A - it now has no association and no array
   GTM>ZWRite ; B is a traditional local variable
   B=1
  
Example:

.. parsed-literal::
   GTM>Set A=2 ; add a value for A
   GTM>ZWRite ; A and B have different values and both are traditional local variables
   A=2
   B=1
   GTM>

KILL on the other hand, removes data in the array (and possibly the array itself) without affecting any alias association.

.. parsed-literal::
   GTM>Set A=2,*B=A ; Create an array and an association
   GTM>ZWRite ; Both array and association exist
   A=2 ;*
   \*B=A
   GTM>Kill A ; Kill the array
   GTM>ZWRite ; There's no data to show - only the association
   \*B=A
   GTM>Set B=3 ; Create a new value
   GTM>ZWRite ; The association was unaffected by the Kill
   A=3 ;*
   \*B=A
   GTM>

Example:

.. parsed-literal::
   $ /usr/lib/fis-gtm/V5.4-002B_x86/gtm -run ^killalias
   killalias ; Demonstrate Kill * of pass-by-reference
          ZPrint ; Print this program
          Set A=1,C=3
          Write "------------",!
          Write "Initial Values:",!
          ZWRite
          Do K1(.A,.C) ; Pass A & C by reference
          Write "------------",!
          Write "Value of A is unchanged because of Kill \*B, but C has changed: ",!
          ZWRite
          Quit
    ;
    K1(B,D) ; A & C are bound to B & D respectively
          Write "------------",!
          Write "A & B are aliases, as are C & D:",!
          ZWRite
          Kill \*B
          Set B=2,D=4
          Write "------------",!
          Write "After Kill \*B, A & B are different but C & D remain associated:",!
          ZWrite
          Quit
   ------------
   Initial Values:
   A=1
   C=3
   ------------
   A & B are aliases, as are C & D:
   A=1 ;*
   \*B=A
   C=3 ;*
   \*D=C
   ------------
   After Kill *B, A & B are different but C & D remain associated:
   A=1
   B=2
   C=4 ;*
   \*D=C
   ------------
   Value of A is unchanged because of Kill \*B, but C has changed: 
   A=1
   C=4
   Example:
   GTM>Set A=1,*B=A ; Create an array and association
   GTM>ZWRite ; Verify that it's there
   A=1 ;*
   \*B=A
   GTM>Kill (A) ; Kill everything except A
   GTM>ZWRite ; Demonstrate that A also has no array
   GTM>Set A=2 ; Create an array
   GTM>ZWRite ; The association survived the Kill
   A=2 ;*
   \*B=A
   GTM>

+++++++++++++++++++++++++++++
Annotated Alias Examples
+++++++++++++++++++++++++++++

Example:

.. parsed-literal::
   $ /usr/lib/fis-gtm/V5.4-002B/gtm -run ^tprestart
   tprestart ; Transaction restart variable association also restored on restart
     zprint ; Print this program
     set A="Malvern",C="Pennsylvania",E="USA"
     set \*B=C,*D(19355)=E
     write "------------",!
     write "Initial values & association",!
     zwrite
     tstart (B,D) ; On restart: A not restored, B,D restored, C,E restored by association
     if '$TRestart Do  ; Change C,E if first time through
     .set C="Wales",E="UK"
     .kill \*D(19355)
     .write "------------",!
     .write "First time through transaction; B,C,D,E changed",!
     .zwrite
     .set A="Brynmawr"
     .kill \*B
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
   \*C=B
   \*D(19355)=E
   E="USA" ;*
   ------------
   First time through transaction; B,C,D,E changed
   A="Malvern"
   B="Wales" ;*
   \*C=B
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
   \*C=B
   \*D(19355)=E
   E="USA" ;*

Note that TROLLBACK does not restore alias variables:

.. parsed-literal::
   /usr/lib/fis-gtm/V5.4-002B_x86/gtm -run ^tprollback
   tprollback ;
     zprint ; Print this program
     set A(1)=1,A(2)=2,A(3)=3
     set B(1)="1b",*B(2)=A,B(3)=3 ; B includes a container for A
     set \*C(1)=B   ; C includes a container for B
     kill \*A,*B   ; C is the only way to the data
     write "------------",!
     write "Only containers before transaction:",!
     zwrite
     tstart (C)
     if '$trestart
     .set \*D=C(1) ; D is now an alias for what used to be B
     .set D(3)=-D(3)
     .set \*D=D(2) ; D is now an alias for what used to be A
     .set D(1)=-D(1)
     .kill \*D  ; Kill D after is used to manipulate the arrays
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
   \*C(1)=$ZWRTAC1
   $ZWRTAC1(1)="1b"
   \*$ZWRTAC1(2)=$ZWRTAC2
   $ZWRTAC2(1)=1
   $ZWRTAC2(2)=2
   $ZWRTAC2(3)=3
   $ZWRTAC1(3)=3
   $ZWRTAC=""
   ------------
   Restored values restart:
   $ZWRTAC=""
   \*C(1)=$ZWRTAC1
   $ZWRTAC1(1)="1b"
   \*$ZWRTAC1(2)=$ZWRTAC2
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

.. parsed-literal::
   $ /usr/lib/fis-gtm/V5.4-002B_x86/gtm -run ^aliasexample; Extended annotated alias example
       zprint
       write "------------",!
       set x="name level",x(1)=1,x(1,2)="1,2",x("foo")="bar"
       write $ZDATA(x),! ; x is a conventional lvn - output 11
       set \*y=x ; x an y are now alias variables
       write $ZDATA(x),! ; output appears as 111
       set \*a(1)=y ; a(1) is now an alias container variable
       set b="bness",b("b")="bbness" ; b is a conventional lvn
       set \*b=a(1) ; b joins x and y as alias variables for the same data
       ; prior b values are lost
       ; set \*<name> is equivalent to Kill \*<name> Set \*<name>
       set y("hi")="sailor" ; Assignment applies to all of {b,x,y}
       kill b("foo") ; Kill applies to all of {b,x,y}
       kill \*x ; x is undefined and no longer an alias variable
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
       kill \*c,\*y ; c and y become undefined lvns
       zwrite b ; output is b("got")="a match"
       ; it's no longer an alias variable
       ; as everything else has gone
       quit
  sub1
      new y ; in this scope y is no longer an alias for b
      set \*y=c ; in this scope c and y are alias variables
      kill y("legs") ; Kill apples to all of {c,y}
      kill \*y ; in this scope y is no longer an alias for c
      ; this is really redundant as
      ; the Quit implicitly does the same thing
      quit
  sub2(i) ; i and c are joined due to pass-by-reference
      write $ZAHandle(c)=$ZAHandle(i),! ; output appears as 1
      kill b ; data for {b,y} is gone
      ; both are undefined, but remain alias variables
      set \*c=a(1) ; c joins {b,y} as alias variable; prior value of c lost
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


-----------------------------------------
Extensions for Unicode™ Support
-----------------------------------------

To represent and process strings that use international characters, YottaDB/GT.M processes can use Unicode.

If the environment variable gtm_chset has a value of UTF-8 and either LC_ALL or LC_CTYPE is set to a locale with UTF-8 support (for example, zh_CN.utf8), a YottaDB/GT.M process interprets strings as containing characters encoded in the UTF-8 representation. In the UTF-8 mode, YottaDB/GT.M no longer assumes that one character is one byte, or that the glyph display width of a character is one. Depending on how ICU is built on a computer system, in order to operate in UTF-8 mode, a YottaDB/GT.M process may well also need a third environment variable, gtm_icu_version set appropriately.

If the environment variable gtm_chset has no value, the string "M", or any value other than "UTF-8", YottaDB/GT.M treats each 8-bit byte as a character, which suffices for English, and many single-language applications.

All YottaDB/GT.M components related to M mode reside in the top level directory in which a YottaDB/GT.M release is installed and the environment variable gtm_dist should point to that directory for M mode processes. All Unicode-related components reside in the utf8 subdirectory and the environment variable gtm_dist should point to that subdirectory for UTF-8 mode processes. So, in addition to the values of the environment variables gtm_chset and LC_ALL/LC_CTYPE, gtm_dist for a UTF-8 process should also point to the utf8 subdirectory.

M mode and UTF-8 mode are set for the process, not for the database. As a subset of Unicode, ASCII characters ($CHAR() values 0 through 127) are interpreted identically by processes in M and UTF-8 modes. The indexes and values in the database are simply sequences of bytes and therefore it is possible for one process to interpret a global node as encoded in UTF-8 and for another to interpret the same node as bytecodes. Note that such an application configuration would be extremely unusual, except perhaps during a transition phase or in connection with data import/export.

In UTF-8 mode, string processing functions (such as $EXTRACT()) operate on strings of multi-byte characters, and can therefore produce different results in M and UTF-8 modes, depending on the actual data processed. Each function has a "Z" alter ego (for example, $ZEXTRACT()) that can be used to operate on sequences of bytes identically in M and UTF-8 modes (that is, in M mode, $EXTRACT() and $ZEXTRACT() behave identically).

In M mode, the concept of an illegal character does not exist. In UTF-8 mode, a sequence of bytes may not represent a valid character, and generates an error when encountered by functions that expect and process UTF-8 strings. During a migration of an application to add support for Unicode, illegal character errors may be frequent and indicative of application code that is yet to be modified. VIEW "NOBADCHAR" suppresses these errors at times when their presence impedes development.

In UTF-8 mode, YottaDB/GT.M also supports IO encoded in UTF-16 variants as well as in the traditional one byte per character encoding from devices other than $PRINCIPAL.

The following table summarizes YottaDB/GT.M Unicode support.

+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Extension                   | Explanation                                                                                                                                                                                |
+=============================+============================================================================================================================================================================================+
| $ASCII()                    | IN UTF-8 mode, the $ASCII() function returns the integer Unicode code-point value of a character in the given string. Note that the name $ASCII() is somewhat anomalous for Unicode data   |
|                             | but that name is the logical extension of the function from M mode to UTF-8 mode. For more information and usage examples, refer to “$ASCII()”.                                            |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $Char()                     | In UTF-8 mode, $CHAR() returns a string composed of characters represented by the integer equivalents of the Unicode code-points specified in its argument(s). For more information and    |
|                             | usage examples, refer to “$Char()”.                                                                                                                                                        |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $Extract()                  | The $EXTRACT() function returns a substring of a given string. For more information and usage examples, refer to “$Extract()”.                                                             |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $Find()                     | The $FIND() function returns an integer character position that locates the occurrence of a substring within a string. For more information and usage examples, refer to “$Find()”.        |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $Justify()                  | The $JUSTIFY function returns a formatted string. For more information and usage examples, refer to “$Justify()”.                                                                          |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $Length()                   | The $LENGTH() function returns the length of a string measured in characters, or in "pieces" separated by a delimiter specified by its optional second argument. For more information and  |
|                             | usage examples, refer to “$Length()”.                                                                                                                                                      |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $Piece()                    | The $PIECE() function returns a substring delimited by a specified string delimiter made up of one or more characters. For more information and usage examples, refer to “$Piece()”.       |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $TRanslate()                | The $TRANSLATE() function returns a string that results from replacing or dropping characters in the first of its arguments as specified by the patterns of its other arguments. For more  |
|                             | information and usage examples, refer to “$TRanslate()”.                                                                                                                                   |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $X                          | For UTF-8 mode and TRM and SD output, $X increases by the display-columns (width in glyphs) of a given string that is written to the current device. For more information and usage        |
|                             | examples, refer to “$X”.                                                                                                                                                                   |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZASCII()                   | The $ZASCII() function returns the numeric byte value (0 through 255) of a given sequence of octets (8-bit bytes). For more information and usage examples, refer to “$ZAscii()”.          |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZCHset                     | The read-only intrinsic special variable $ZCHSET takes its value from the environment variable gtm_chset. An application can obtain the character set used by a YottaDB/GT.M process by the|
|                             | value of $ZCHSET. $ZCHSET can have only two values –"M", or "UTF-8" and it cannot appear on the left of an equal sign in the SET command. For more information and usage examples, refer to|
|                             | “$ZCHset”.                                                                                                                                                                                 |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZCHar()                    | The $ZCHAR() function returns a byte sequence of one or more bytes corresponding to numeric byte value (0 through 255) specified in its argument(s). For more information and usage        |
|                             | examples, refer to “$ZCHar()”.                                                                                                                                                             |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZCOnvert()                 | The $ZCONVERT() function returns its first argument as a string converted to a different encoding. The two argument form changes the encoding for case within a character set. The three   |
|                             | argument form changes the encoding scheme. For more information and usage examples, refer to “$ZCOnvert()”.                                                                                |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZExtract()                 | The $ZEXTRACT() function returns a byte sequence of a given sequence of octets (8-bit bytes). For more information and usage examples, refer to “$ZExtract()”.                             |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZFind()                    | The $ZFIND() function returns an integer byte position that locates the occurrence of a byte sequence within a sequence of octets(8-bit bytes). For more information and usage examples,   |
|                             | refer to “$ZFind()”.                                                                                                                                                                       |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZJustify()                 | The $JUSTIFY() function returns a formatted and fixed length byte sequence. For more information and usage examples, refer to “$ZJustify()”.                                               |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZLength()                  | The $ZLENGTH() function returns the length of a sequence of octets measured in bytes, or in "pieces" separated by a delimiter specified by its optional second argument. For more          |
|                             | information and usage examples, refer to “$ZLength()”.                                                                                                                                     |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZPATNumeric                | ZPATN[UMERIC] is a read-only intrinsic special variable that determines how YottaDB/GT.M interprets the patcode N used in the pattern match operator. With $ZPATNUMERIC="UTF-8", the       |
|                             | patcode N matches any numeric character as defined by Unicode. By default patcode N only matches the ASCII digits, which are the only digits which M actually treats as numerics. For more |
|                             | information and usage examples, refer to “$ZPATNumeric”.                                                                                                                                   |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZPIece()                   | The $ZPIECE() function returns a sequence of bytes delimited by a specified byte sequence made up of one or more bytes. In M, $ZPIECE() typically returns a logical field from a logical   |
|                             | record. For more information and usage examples, refer to “$ZPIece()”.                                                                                                                     |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZPROMpt                    | $ZPROM[PT] contains a string value specifying the current Direct Mode prompt. By default, GTM> is the Direct Mode prompt. M routines can modify $ZPROMPT by means of a SET command.        |
|                             | $ZPROMPT cannot exceed 31 bytes. If an attempt is made to assign $ZPROMPT to a longer string, YottaDB/GT.M takes only the first 31 bytes and truncates the rest. With character set UTF-8  |
|                             | specified, if the 31st byte is not the end of a valid UTF-8 character, YottaDB/GT.M truncates the $ZPROMPT value at the end of last character that completely fits within the 31 byte      |
|                             | limit. For more information and usage examples, refer to “$ZPROMpt”.                                                                                                                       |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZSUBstr()                  | The $ZSUBSTR() function returns a properly encoded string from a sequence of bytes. For more information and usage examples, refer to “$ZSUBstr()”.                                        |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZTRanslate()               | The $ZTRANSLATE() function returns a byte sequence that results from replacing or dropping bytes in the first of its arguments as specified by the patterns of its other arguments.        |
|                             | $ZTRANSLATE() provides a tool for tasks such as encryption. For more information and usage examples, refer to “$ZTRanslate()”.                                                             |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZWidth()                   | The $ZWIDTH() function returns the numbers of columns required to display a given string on the screen or printer. For more information and usage examples, refer to “$ZWidth()”.          |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| %HEX2UTF                    | The %HEX2UTF utility returns the encoded character string from the given bytestream in hexadecimal notation. This routine has entry points for both interactive and non-interactive use.   |
|                             | For more information and usage examples, refer to “%HEX2UTF”.                                                                                                                              |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| %UTF2HEX                    | The %UTF2HEX utility returns the hexadecimal notation of the internal byte encoding of a UTF-8 encoded character string. This routine has entry points for both interactive and            |
|                             | non-interactive use. For more information and usage examples, refer to “%UTF2HEX”.                                                                                                         |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| [NO]WRAP (USE)              | Enables or disables automatic record termination. When the current record size ($X) reaches the maximum WIDTH and the device has WRAP enabled, YottaDB/GT.M starts a new record, as if the |
|                             | routine had issued a WRITE ! command. For more information and usage examples, refer to “WRAP”.                                                                                            |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| DSE and LKE                 | In UTF-8 mode, DSE and LKE accept characters in Unicode in all their command qualifiers that require file names, keys, or data (such as DSE -KEY, DSE -DATA and LKE -LOCK qualifiers).     |
|                             | For more information, refer to the LKE and DSE chapter For more information and usage examples, refer to the Administration and Operations Guide.                                          |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| GDE Objects                 | GDE allows the name of a file to include characters in Unicode                                                                                                                             |
|                             |                                                                                                                                                                                            |
|                             | In UTF-8 mode, GDE considers a text file to be encoded in UTF-8 when it is executed via the "@" command. For more information, refer to the GDE chapter in the Administration and          |
|                             | Operations Guide.                                                                                                                                                                          |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| FILTER[=expr]               | Specifies character filtering for specified cursor movement sequences on devices where FILTER applies.                                                                                     |
|                             |                                                                                                                                                                                            | 
|                             | In UTF-8 mode, the usual Unicode line terminators (U+000A (LF), U+0000D (CR), U+000D followed by U+000A (CRLF), U+0085 (NEL), U+000C (FF), U+2028 (LS) and U+2029 (PS)) are recognized. If |
|                             | FILTER=CHARACTER is enabled, all of the terminators are recognized to maintain the values of $X and $Y. For more information, refer to “FILTER”.                                           |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Job                         | The Job command spawns a background process with the same environment as the M process doing the spawning. Therefore, if the parent process is operating in UTF-8 mode, the Job'd process  |
|                             | also operates in UTF-8 mode. In the event that a background process must have a different mode from the parent, create a shell script to alter the environment as needed, and spawn it with|
|                             | a ZSYstem command, for example, ZSYstem "/path/to/shell/script &", or start it as a PIPE device. For more information and UTF-8 mode examples, refer “Job”.                                |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| MUPIP                       | MUPIP EXTRACT                                                                                                                                                                              |
|                             |                                                                                                                                                                                            |
|                             | In UTF-8 mode, MUPIP EXTRACT, MUPIP JOURNAL -EXTRACT and MUPIP JOURNAL -LOSTTRANS write sequential output files in the UTF-8 character encoding form. For example, in UTF-8 mode if ^A has |
|                             | the value of 主要雨在西班牙停留在平原, the sequential output file of the MUPIP EXTRACT command is:                                                                                         |
|                             |                                                                                                                                                                                            |
|                             | 09-OCT-2006 04:27:53 ZWR                                                                                                                                                                   |
|                             |                                                                                                                                                                                            |
|                             | YottaDB/GT.M MUPIP EXTRACT UTF-8                                                                                                                                                           |
|                             |                                                                                                                                                                                            |
|                             | ^A="主要雨在西班牙停留在平原"                                                                                                                                                              |
|                             |                                                                                                                                                                                            |
|                             | MUPIP LOAD                                                                                                                                                                                 |
|                             |                                                                                                                                                                                            |
|                             | MUPIP LOAD command considers a sequential file as encoded in UTF-8 if the environment variable gtm_chset is set to UTF-8. Ensure that MUPIP EXTRACT commands and corresponding MUPIP LOAD  |
|                             | commands execute with the same setting for the environment variable gtm_chset. The M utility programs %GO and %GI have the same requirement for mode matching. For more information on     |
|                             | MUPIP EXTRACT and MUPIP LOAD, refer to the General Database Management chapter in the Administration and Operations Guide.                                                                 |
+-----------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+


