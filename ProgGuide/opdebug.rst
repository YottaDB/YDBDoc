.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.     #
.. # All rights reserved.                                        #
.. #                                                             #
.. #     This source code contains the intellectual property     #
.. #     of its copyright holder(s), and is made available       #
.. #     under a license.  If you do not know the terms of       #
.. #     the license, please stop and do not read further.       #
.. #                                                             #
.. ###############################################################

.. index::
   Operating and Debugging in Direct Mode

=========================================
4. Operating and Debugging in Direct Mode
=========================================

.. contents::
   :depth: 5

Direct Mode is an important tool in YottaDB because it allows you to interactively debug, modify, and execute M routines. Direct Mode is a shell that immediately compiles and executes YottaDB commands providing an interpretive-like interface. M simplifies debugging by using the same commands for debugging that are used for programming.

The focus of this chapter is to describe the debugging process in Direct Mode, and to illustrate the YottaDB language extensions that enhance the process. Command functionality is described only in enough detail to illustrate why a particular command is useful for a debugging activity being described. If you have specific functionality questions about a command or variable, see the `“Commands” <./commands.html>`_, `“Functions” <./functions.html>`_, or `“Intrinsic Special Variables” <./isv.html>`_ chapter.

It is also from Direct Mode that you activate YottaDB tools used to create M source code. The interaction of M commands used for editing and compiling is described in greater detail within `Chapter 3: “Development Cycle” <./devcycle.html>`_.

-------------------------------------
Operating in Direct Mode
-------------------------------------

This section provides an overview of the following basic operational issues in Direct Mode:

* Entering Direct Mode
* Available functionality
* Exiting Direct Mode

+++++++++++++++++++++
Entering Direct Mode
+++++++++++++++++++++

To enter Direct Mode, type $ydb_dist/yottadb -direct at the shell prompt.

.. code-block:: bash

   $ $ydb_dist/yottadb -direct
   YDB>

This shows using $ydb_dist/yottadb -direct at the prompt to enter Direct Mode.

Another way to enter Direct Mode for an editing or debugging session is by simply typing ydb at the shell prompt.

Example:

.. code-block:: bash

   $ ydb
   YDB>


+++++++++++++++++++++++++++++++++++++++++++
Functionality Available in Direct Mode
+++++++++++++++++++++++++++++++++++++++++++

This section provides an overview of basic functionality and concepts that enhance your use of Direct Mode.

~~~~~~~~~~~~~~~
Command Recall
~~~~~~~~~~~~~~~

Direct Mode includes a line command recall function to display previously entered command lines. Use <CTRL-B> or the Up Arrow key at the YDB> prompt to scroll back through command lines. Use the Down Arrow key to scroll forward through the command lines. YottaDB displays one command line at a time. You may delete and reenter characters starting at the end of a recalled line.

The RECALL command is another way to access previously entered Direct Mode command lines. RECALL is only valid in Direct Mode and causes an error if it appears in other M code.

The format of the RECALL command is:

.. code-block:: none

   REC[ALL] [intlit|strlit]

* The optional integer literal specifies a previously entered command by counting back from the present.
* The optional string literal specifies the most recently entered command line that starts with characters matching the (case-sensitive) literal.
* When the RECALL command has no argument, it displays up to a maximum of 99 available previous Direct Mode entries.

If the session in Direct Mode has just started, you may not have entered 99 lines for YottaDB to save and therefore you will not have 99 lines to look at. The most recently entered YottaDB command line has the number one (1), older lines have higher numbers. YottaDB does not include the RECALL command in the listing. If the RECALL command is issued from a location other than the Direct Mode prompt, YottaDB issues a run-time error.

Example:

.. code-block:: bash

   YDB>write $zgbldir
   /usr/lib/yottadb/r120/yottadb.gld
   YDB>set $zgbldir="test.gld"
   YDB>set a=10
   YDB>set b=a
   YDB>recall
   1 set b=a
   2 set a=10
   3 set $zgbldir="test.gld"
   4 write $zgbldir
   YDB>

This REC[ALL] command displays the previously entered commands.

You can also display a selected command by entering RECALL and the line number of the command you want to retrieve.

Example:

.. code-block:: bash

   YDB>recall 2
   YDB>set a=10

This RECALLs the line number two (2).

If the RE[CALL] command includes a text parameter, YottaDB displays the most recent command matching the text after the RE[CALL] command.

Example:

.. code-block:: bash

   YDB>recall write
   YDB>write $zgbldir

This RECALLs "WRITE", the command most recently beginning with this text. Note that the RECALL command text is case sensitive. The RECALL command with a text argument treats WRITE and write differently, that is, it treats them case sensitively. If you first type the WRITE command in lower-case and then type WRITE in upper-case to recall it, the RECALL command does not find a match.

~~~~~~~~~~~~~
Line Editing
~~~~~~~~~~~~~

YottaDB permits the use of the YottaDB command line editor at the Direct Mode prompt and during M READs from a terminal. The YottaDB line editor allows cursor positioning using the <CTRL> key, edit keypad and function keys.

The Direct Mode line editing keys are as follows:

* **Backspace**: Deletes the character to the left of the cursor

* **Delete**: Deletes the character under the cursor

* **Up-arrow**: Moves to a less recent item in the RECALL list

* **Down-arrow**: Moves to a more recent item in the RECALL list

* **Left-arrow**: Moves the cursor one character to the left

* **Right-arrow**: Moves the cursor one character to the right

* **<CTRL-A>**: Moves the cursor to the beginning of the line

* **<CTRL-B>**: Moves the cursor one character towards the beginning of the line

* **<CTRL-D>**: On an empty line, terminates YottaDB and returns control to the shell.

* **<CTRL-E>**: Moves the cursor to the end of the line

* **<CTRL-F>**: Moves the cursor one character towards the end of the line

* **<CTRL-K>**: Deletes all characters from the cursor to the end of the line

* **<CTRL-U>**: Deletes the entire line

.. note::
   When entering commands at the direct mode prompt, the insert mode can be toggled for that line by using the insert key. When YottaDB starts, insert mode is enabled unless the value of the ydb_principal_editing environment variable includes the string NOINSERT. If insert mode is disabled or enabled for the $PRINCIPAL device by a USE statement before returning to Direct Mode, it will remain disabled or enabled in Direct Mode. The insert mode can be toggled within a Direct Mode line using the terminal's INSERT key.

YottaDB deletes the character under the cursor when you press the key on the keyboard that sends the escape sequence which maps to the kdch1 capability in your current terminfo entry (by convention, the Delete key). If the current terminfo entry is missing the kdch1 capability, YottaDB uses a default value derived from members of the DEC VT terminal family, as it does for selected other missing terminfo capabilities. If you wish the Backspace and Delete keys to have the same behavior, the simplest way is to configure your terminal emulator to send the same character sequences for the Delete key that it does for the Backspace key. You can alternatively modify your terminfo setting: for example, create an editable version of your terminfo entry in a temporary file with a command such as: infocmp > /tmp/$$_$TERM and edit the temporary file to replace the entry for the kbs capability with the one in the kdch1 capability. Save your changes, and compile the edited file into a usable terminfo entry, for example:

.. code-block:: bash

   export TERMINFO=$HOME/.terminfo # You may need to add this to your login profile
   profilemkdir -p $TERMINFO
   tic /tmp/$$_$TERM # or whatever your temporary file name was

When modifying terminfo capabilities, always look for unintended changes in the behavior of other applications, for example, text editors, that also rely on those capabilities. In the worst case, you may need to toggle between alternate terminfo entries for YottaDB and other applications while you evaluate different options. Also, for terminfo entries without the cud1 capability, YottaDB uses a linefeed when moving to the next line in direct mode.

~~~~~~~~~~~~~~~~~~~~~~~~
The M Invocation Stack
~~~~~~~~~~~~~~~~~~~~~~~~

The ANSI M Standard describes certain M operations in terms of how a stack-based virtual machine would operate. A stack is a repository for tracking temporary information on a "last-in/first-out" (LIFO) basis. M program behavior can be understood using a stack-based model. However, the standard is not explicit in defining how an implementation must maintain a stack or even whether it must use one at all.

The stack model provides a trail of routines currently in progress that shows the location of all the M operations that performed the invocations leading to the current point.

The ZSHOW command makes this stack information available within YottaDB. For more information, see `“Using the Invocation Stack in Debugging” <./opdebug.html#id1>`_ in this chapter, and the command description at `“ZSHow” <./commands.html#zshow>`_.

+++++++++++++++++++++++++++++++
Exiting Direct Mode
+++++++++++++++++++++++++++++++

Five M commands can terminate a Direct Mode session:

* HALT
* ZHALT
* ZCONTINUE
* GOTO
* ZGOTO

The HALT command exits Direct Mode and terminates the M process.

The ZHALT command exits Direct Mode and returns the exit status to the calling environment.

The ZCONTINUE command instructs YottaDB to exit Direct Mode and resume routine execution at the current point in the M invocation stack. This may be the point where YottaDB interrupted execution and entered Direct Mode. However, when the Direct Mode interaction includes a QUIT command, it modifies the invocation stack and causes ZCONTINUE to resume execution at another point.

The GOTO and ZGOTO commands instruct YottaDB to leave Direct Mode, and transfer control to a specified entry reference.

----------------------------------------
Debugging a Routine in Direct Mode
----------------------------------------

To begin a debugging session on a specific routine, type the following command at the YottaDB prompt:

.. code-block:: bash

   YDB>DO ^routinename

You can also begin a debugging session by pressing <CTRL-C> after running an M application at the shell. To invoke Direct Mode by pressing <CTRL-C>, process must have the Principal Device in the CENABLE state and not have the device set to CTRAP=$C(3).

When YottaDB receives a <CTRL-C> command from the principal device, it invokes Direct Mode at the next opportunity, (usually at a point corresponding to the beginning of the next source line). YottaDB can also interrupt at a FOR loop iteration or during a command of indeterminate duration such as LOCK, OPEN or READ. The YottaDB USE command enables/disables the <CTRL-C> interrupt with the [NO]CENABLE deviceparameter. By default, YottaDB starts <CTRL-C> enabled. The default setting for <CTRL-C> is controlled by $ydb_nocenable which controls whether <CTRL-C> is enabled at process startup. If $ydb_nocenable has a value of 1, "TRUE" or "YES" (case-insensitive), and the process principal device is a terminal, $PRINCIPAL is initialized to a NOCENABLE state where the process does not recognize <CTRL-C> as a signal to enter direct mode. No value, or other values of $ydb_nocenable initialize $PRINCIPAL with the CENABLE state. The [NO]CENABLE deviceparameter on a USE command can still control this characteristic from within the process.

YottaDB displays the YDB> prompt on the principal device. Direct Mode accepts commands from, and reports errors to, the principal device. YottaDB uses the current device for all other I/O. If the current device does not match the principal device when YottaDB enters Direct Mode, YottaDB issues a warning message on the principal device. A USE command changes the current device. For more information on the USE command, see `Chapter 9: “Input/Output Processing” <./ioproc.html>`_.

The default "compile-as-written" mode of the YottaDB compiler lets you run a program with errors as part of the debugging cycle. The object code produced includes all lines that are correct and all commands on a line with an error, up to the error. When YottaDB encounters an error, it XECUTEs non empty values of $ETRAP or $ZTRAP. By default $ZTRAP contains a BREAK command, so YottaDB enters Direct Mode.

The rest of the chapter illustrates the debugging capabilities of YottaDB by taking a sample routine, dmex, through the debugging process. dmex is intended to read and edit a name, print the last and first name, and terminate if the name is an upper-case or lower-case "Q".

Each of the remaining sections of the chapter uses dmex to illustrate an aspect of the debugging process in YottaDB.

+++++++++++++++++++++++++++++++++++
Creating and Displaying M Routines
+++++++++++++++++++++++++++++++++++

To create or edit a routine, use the ZEDIT command. ZEDIT invokes the editor specified by the EDITOR environment variable, and opens the specified file. dmex.m, for editing.

Example:

.. code-block:: bash

   YDB>ZEDIT "dmex"

Once in the editor, use the standard editing commands to enter and edit text. When you finish editing, save the changes, which returns you to Direct Mode.

To display M source code for dmex, use the ZPRINT command.

Example:

.. code-block:: bash

   YDB>ZPRINT ^dmex
   dmex;dmex - Direct Mode example
   ;
   beg  for read !,"Name: ",name do name
      quit
   name
   set ln=$l(name)
     if ln,$extract("QUIT",1,ln)=$tr(name,"quit","QUIT") do
     . s name="Q"
     . quit
     if ln<30,bame?1.a.1"-".a1","1" "1a.ap do print quit
     write !,"Please use last-name, "
     write "first-name middle-initial or 'Q' to Quit."
     quit
   print
     write !,$piece(name,", ",2)," ",$piece(name,", ")
     quit
   YDB>

This uses the ZPRINT command to display the routine dmex.

.. note::
   The example misspells the variable name as bame.

+++++++++++++++++++++++++++++++++++
Executing M Routines Interactively
+++++++++++++++++++++++++++++++++++

To execute an M routine interactively, it is not necessary to explicitly compile and link your program. When you refer to an M routine that is not part of the current image, YottaDB automatically attempts to compile and ZLINK the program.

Example:

.. code-block:: bash

   YDB>DO ^dmex
   Name: Revere, Paul
   %YDB-E-UNDEF, Undefined local variable: bame
   At M source location name+3^dmex
   YDB>

In this example YottaDB places you in Direct Mode, but also cites an error found in the program with a run-time error message. In this example, it was a reference to "bame", which is undefined.

To see additional information about the error message, examine the $ECODE or $ZSTATUS special variables.

$ECODE is read-write intrinsic special variable that maintains a list of comma delimited codes that describe a history of past errors - the most recent ones appear at the end of the list. In $ECODE, standard errors are prefixed with an "M", user defined errors with a "U", and YottaDB errors with a "Z". A YottaDB code always follows a standard code.

$ZSTATUS is a read-write intrinsic special variable that maintains a string containing the error condition code and location of the last exception condition occurring during routine execution. YottaDB updates $ZSTATUS only for errors found in routines and not for errors entered at the Direct Mode prompt.

.. note::
   For more information on $ECODE and $STATUS see `Chapter 8: “Intrinsic Special Variables” <./isv.html>`_.

Example:

.. code-block:: bash

   YDB>WRITE $ECODE
   ,M6,Z150373850

This example uses a WRITE command to display $ECODE.

Example:

.. code-block:: bash

   YDB>WRITE $ZS
   150373850,name+3^dmex,%YDB-E-UNDEF, Undefined
   local variable: bame

This example uses a WRITE command to display $ZSTATUS. Note that the $ZSTATUS code is the same as the "Z" code in $ECODE.

You can record the error message number, and use the $ZMESSAGE function later to re-display the error message text.

Example:

.. code-block:: bash

   YDB>WRITE $ZM(150373850)
   %YDB-E-UNDEF, Undefined local variable: !AD

This example uses a WRITE command and the $ZMESSAGE function to display the error message generated in the previous example. $ZMESSAGE() is useful when you have a routine that produces several error messages that you may want to examine later. The error message reprinted using $ZMESSAGE() is generic; therefore, the code !AD appears instead of the specific undefined local variable displayed with the original message.

++++++++++++++++++++++++++++++++++++++++++
Processing with Run Time and Syntax Errors
++++++++++++++++++++++++++++++++++++++++++

When YottaDB encounters a run-time or syntax error, it stops executing and displays an error message. YottaDB reports the error in the message. In this case, YottaDB reports an undefined local variable and the line in error, name+3^dmex. Note that YottaDB re-displays the YDB> prompt so that debugging may continue.

To re-display the line and identify the error, use the ZPRINT command.

Example:

.. code-block:: bash

   YDB>ZPRINT, name+3
   %YDB-E-SPOREOL, Either a space or an end-of-line was expected but not found
   ZP, name+3
   ^_____
   YDB>

This example shows the result of incorrectly entering a ZPRINT command in Direct Mode. YottaDB reports the location of the syntax error in the command line with an arrow. $ECODE and $ZSTATUS do not maintain this error message because YottaDB did not produce the message during routine execution. Enter the correct syntax, (i.e., remove the comma) to re-display the routine line in error.

Example:

.. code-block:: bash

   YDB>WRITE $ZPOS
   name+3^dmex

This example writes the current line position.

$ZPOSITION is a read-only YottaDB special variable that provides another tool for locating and displaying the current line. It contains the current entry reference as a character string in the format label+offset^routine, where the label is the closest preceding label. The current entry reference appears at the top of the M invocation stack, which can also be displayed with a ZSHOW "S" command.

To display the current value of every local variable defined, use the ZWRITE command with no arguments.

Example:

.. code-block:: bash

   YDB>ZWRITE
   ln=12
   name="Revere, Paul"

This ZWRITE displays a listing of all the local variables currently defined.

.. note::
   ZWRITE displays the variable name. ZWRITE does not display a value for bame, confirming that it is not defined.

++++++++++++++++++++++++++
Correcting Errors
++++++++++++++++++++++++++

Use the ZBREAK command to establish a temporary breakpoint and specify an action. ZBREAK sets or clears routine-transparent breakpoints during debugging. This command simplifies debugging by interrupting execution at a specific point to examine variables, execute commands, or to start using ZSTEP to execute the routine line by line.

YottaDB suspends execution during execution when the entry reference specified by ZBREAK is encountered. If the ZBREAK does not specify an expression "action", the process uses the default (BREAK) and puts YottaDB into Direct Mode. If the ZBREAK does specify an expression "action", the process XECUTEs the value of "action", and does not enter Direct Mode unless the action includes a BREAK. The action serves as a "trace-point". The trace-point is silent unless the action specifies terminal output.

Example:

.. code-block:: bash

   YDB>ZBREAK name+3^dmex:"set bame=name"

This uses a ZBREAK with an action that SETs the variable bame equal to name.

++++++++++++++++++++++++++++
Stepping through a Routine
++++++++++++++++++++++++++++

The ZSTEP command provides a powerful tool to direct YottaDB execution. When you issue a ZSTEP from Direct Mode, YottaDB executes the program to the beginning of the next target line and performs the ZSTEP action.

The optional keyword portion of the argument specifies the class of lines where ZSTEP pauses its execution, and XECUTEs the ZSTEP action specified by the optional action portion of the ZSTEP argument. If the action is specified, it must be an expression that evaluates to valid YottaDB code. If no action is specified, ZSTEP XECUTEs the code specified by the $ZSTEP intrinsic special variable; by default $ZSTEP has the value "B", which causes YottaDB to enter Direct Mode.

ZSTEP actions, that include commands followed by a BREAK, perform the specified action, then enter Direct Mode. ZSTEP actions that do not include a BREAK perform the command action and continue execution. Use ZSTEP actions that issue conditional BREAKs and subsequent ZSTEPs to perform tasks such as testing for changes in the value of a variable.

Use ZSTEP to incrementally execute a routine or a series of routines. Execute any YottaDB command from Direct Mode at any ZSTEP pause. To resume normal execution, use ZCONTINUE. Note that ZSTEP arguments are keywords rather than expressions, and they do not allow indirection.

Example:

.. code-block:: bash

   YDB>ZSTEP INTO
   Break instruction encountered during ZSTEP action
   At M source location print^dmex
   YDB>ZSTEP OUTOF
   Paul Revere
   Name: Q
   %YDB-I-BREAKZST, Break instruction encountered during ZSTEP action
   At M source location name^dmex
   YDB>ZSTEP OVER
   Break instruction encountered during ZSTEP action
   At M source location name+1^dmex

This example shows using the ZSTEP command to step through the routine dmex, starting where the execution was interrupted by the undefined variable error. The ZSTEP INTO command executes line name+3 and interrupts execution at the beginning of line print.

The ZSTEP OUTOF continues execution until line name. The ZSTEP OVER, which is the default, executes until it encounters the next line at this level on the M invocation stack. In this case, the next line is name+1. The ZSTEP OVER could be replaced with a ZSTEP with no argument because they do the same thing.

++++++++++++++++++++++++++++++++++++++
Continuing Execution from a Breakpoint
++++++++++++++++++++++++++++++++++++++

Use the ZCONTINUE command to continue execution from the breakpoint.

Example:

.. code-block:: bash

   YDB>ZCONTINUE
   Paul Revere
   Name: q
   Name: QUIT
   Name: ?
   Please use last-name, first name middle-initial
   or 'Q' to Quit.
   Name:

This uses a ZCONTINUE command to resume execution from the point where it was interrupted. As a result of the ZBREAK action, bame is defined and the error does not occur again. Because the process does not terminate as intended when the name read has q as a value, we need to continue debugging.

+++++++++++++++++++++++
Interrupting Execution
+++++++++++++++++++++++

Press <CTRL-C> to interrupt execution, and return to the YottaDB prompt to continue debugging the program.

Example:

.. code-block:: bash

   %YDB-I-CTRLC, CTRLC_C encountered.
   YDB>

This invokes direct mode with a <CTRL-C>.

++++++++++++++++++++++++++++++++++++++++
Using the Invocation Stack in Debugging
++++++++++++++++++++++++++++++++++++++++

M DOs, XECUTEs, and extrinsics add a level to the invocation stack. Matching QUITs take a level off the stack. When YottaDB executes either of these commands, an extrinsic function or an extrinsic special variable, it "pushes" information about the new environment on the stack. When YottaDB executes the QUIT, it "pops" the information about the discarded environment off the stack. It then reinstates the invoking routine information using the entries that have now arrived at the active end of the stack.

.. note::
   In the M stack model, a FOR command does not add a stack frame, and a QUIT that terminates a FOR loop does not remove a stack frame.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Determining Levels of Nesting
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

$STACK contains an integer value indicating the "level of nesting" caused by DO commands, XECUTE commands, and extrinsic functions in the M virtual stack.

$STACK has an initial value of zero (0), and increments by one with each DO, XECUTE, or extrinsic function. Any QUIT that does not terminate a FOR loop or any ZGOTO command decrements $STACK. In accordance with the M standard, a FOR command does not increase $STACK. M routines cannot modify $STACK with the SET or KILL commands.

Example:

.. code-block:: bash

   YDB>WRITE $STACK
   2
   YDB>WRITE $ZLEVEL
   3
   YDB>

This example shows the current values for $STACK and $ZLEVEL. $ZLEVEL is like $STACK except that uses one (1) as the starting level for the M stack, which $STACK uses zero (0), which means that $ZLEVEL is always one more than $STACK. Using $ZLEVEL with "Z" commands and functions, and $STACK with standard functions avoids the need to calculate the adjustment.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Looking at the Invocation Stack
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The $STACK intrinsic special variable and the $STACK() function provide a mechanism to access M stack context information.

Example:

.. code-block:: bash

   YDB>WRITE $STACK
   2
   YDB>WRITE $STACK(2,"ecode")
   ,M6,Z150373850,
   YDB>WRITE $STACK(2,"place")
   name+3^dmex
   YDB>WRITE $STACK(2,"mcode")
   if ln<30,bame?1.a.1"-".a1","1" "1a.ap do print q
   YDB>

This example gets the value of $STACK and then uses that value to get various types of information about that stack level using the $STACK() function. The "ecode" value of the error information for level two, "place" is similar to $ZPOSITION, "mcode" is the code for the level.

In addition to the $STACK intrinsic special variable, which provides the current stack level, $STACK(-1) gives the highest level for which $STACK() can return valid information. Until there is an error, $STACK and $STACK(-1) are the same, but once $ECODE shows that there is an "current" error, the information returned by $STACK() is frozen to capture the state at the time of the error; it unfreezes after a SET $ECODE="".

Example:

.. code-block:: bash

   YDB>WRITE $STACK
   2
   YDB>WRITE $STACK(-1)
   2
   YDB>

This example shows that under the conditions created (in the above example), $STACK and $STACK(-1) have the same value.

The $STACK() can return information about lower levels.

Example:

.. code-block:: bash

   +1^GTM$DMOD
   YDB>WRITE $STACK(1,"ecode")
   YDB>WRITE $STACK(1,"place")
   beg^dmex
   YDB>WRITE $STACK(1,"mcode")
   beg for read !,"Name:",namde do name
   YDB>

This example shows that there was no error at $STACK level one, as well as the "place" and "mcode" information for that level.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Using ZSHOW to examine Context Information
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ZSHOW command displays information about the M environment.

Example:

.. code-block:: bash

   YDB>zshow "*"
   $DEVICE=""
   $ECODE=",M6,Z150373850,"
   $ESTACK=2
   $ETRAP=""
   $HOROLOG="64813,21971"
   $IO="/dev/pts/0"
   $JOB=14550
   $KEY=$C(13)
   $PRINCIPAL="/dev/pts/0"
   $QUIT=0
   $REFERENCE=""
   $STACK=2
   $STORAGE=2147483647
   $SYSTEM="47,ydb_sysid"
   $TEST=1
   $TLEVEL=0
   $TRESTART=0
   $X=0
   $Y=26
   $ZA=0
   $ZALLOCSTOR=680360
   $ZAUDIT=0
   $ZB=$C(13)
   $ZCHSET="M"
   $ZCLOSE=0
   $ZCMDLINE=""
   $ZCOMPILE=""
   $ZCSTATUS=0
   $ZDATEFORM=0
   $ZDIRECTORY="/path/to/the/current/directory"
   $ZEDITOR=0
   $ZEOF=0
   $ZERROR="Unprocessed $ZERROR, see $ZSTATUS"
   $ZGBLDIR="/path/to/the/global/directory"
   $ZHOROLOG="64813,21971,720675,14400"
   $ZININTERRUPT=0
   $ZINTERRUPT="IF $ZJOBEXAM()"
   $ZIO="/dev/pts/0"
   $ZJOB=0
   $ZKEY=""
   $ZLEVEL=3
   $ZMAXTPTIME=0
   $ZMODE="INTERACTIVE"
   $ZONLNRLBK=0
   $ZPATNUMERIC="M"
   $ZPIN="/dev/pts/0"
   $ZPOSITION="name+5^dmex"
   $ZPOUT="/dev/pts/0"
   $ZPROMPT="YDB>"
   $ZQUIT=0
   $ZREALSTOR=697936
   $ZRELDATE="20180614 00:33"
   $ZROUTINES=". /usr/local/lib/yottadb/r128 /usr/local/lib/yottadb/r128/plugin/o(/usr/local/lib/yottadb/r128/plugin/r)"
   $ZSOURCE=""
   $ZSTATUS="150373850,name+5^dmex,%YDB-E-UNDEF, Undefined local variable: bame"
   $ZSTEP="B"
   $ZSTRPLLIM=0
   $ZSYSTEM=0
   $ZTIMEOUT=-1
   $ZTDATA=0
   $ZTDELIM=""
   $ZTEXIT=""
   $ZTLEVEL=0
   $ZTNAME=""
   $ZTOLDVAL=""
   $ZTRAP="B"
   $ZTRIGGEROP=""
   $ZTSLATE=""
   $ZTUPDATE=""
   $ZTVALUE=""
   $ZTWORMHOLE=""
   $ZUSEDSTOR=671689
   $ZUT=1528970771720738
   $ZVERSION="YottaDB r1.28 Linux x86_64"
   $ZYERROR=""
   ln=8
   name="John Doe"
   /dev/pts/0 OPEN TERMINAL NOPAST NOESCA NOREADS TYPE WIDTH=165 LENG=48
   MLG:0,MLT:0
   GLD:*,REG:*,SET:0,KIL:0,GET:0,DTA:0,ORD:0,ZPR:0,QRY:0,LKS:0,LKF:0,CTN:0,DRD:0
   DWT:0,NTW:0,NTR:0,NBW:0,NBR:0,NR0:0,NR1:0,NR2:0,NR3:0,TTW:0,TTR:0,TRB:0,TBW:0,
   TBR:0,TR0:0,TR1:0,TR2:0,TR3:0,TR4:0,TC0:0,TC1:0,TC2:0,TC3:0,TC4:0,ZTR:0,DFL:0,
   DFS:0,JFL:0,JFS:0,JBB:0,JFB:0,JFW:0,JRL:0,JRP:0,JRE:0,JRI:0,JRO:0,JEX:0,DEX:0,
   CAT:0,CFE:0,CFS:0,CFT:0,CQS:0,CQT:0,CYS:0,CYT:0,BTD:0
   name+5^dmex    ($ZTRAP)
       (Direct mode)
   beg+1^dmex:51a6a6c4739b004094c4545246ce4d68
   +1^GTM$DMOD    (Direct mode)
   YDB>

This example uses the asterisk (*) argument to show all information that ZSHOW offers in this context. First are the Intrinsic Special Variables ($DEVICE-$ZYERROR, also available with ZSHOW "I"), then the local variables (bame, ln and name, also available with ZSHOW "V"), then the ZBREAK locations (name+3^dmex, also available with ZSHOW "B"), then the device information (also available with ZSHOW "D"), then the M stack (also available with ZSHOW "S"). ZSHOW "S" is the default for ZSHOW with no arguments.

Context information that does not exist in this example includes M LOCKs of this process (ZSHOW "L").

In addition to directing its output to the current device, ZSHOW can place its output in a local or global variable array. For more information, see the command description `“ZSHow” <./commands.html#zshow>`_.

.. note::
   ZSHOW "V" produces the same output as ZWRITE with no arguments, but ZSHOW "V" can be directed to a variable as well as a device.

++++++++++++++++++++++++++++++++
Transferring Routine Control
++++++++++++++++++++++++++++++++

The ZGOTO command transfers control from one part of the routine to another, or from one routine to another, using the specified entry reference. The ZGOTO command takes an optional integer expression that indicates the M stack level reached by performing the ZGOTO, and an optional entry reference specifying the location to where ZGOTO transfers control. A ZGOTO command, with an entry reference, performs a function similar to the GOTO command with the additional capability of reducing the M stack level. In a single operation, the process executes $ZLEVEL-intexpr, implicit QUITs from DO or extrinsic operations, and a GOTO operation transferring control to the named entry reference.

The ZGOTO command leaves the invocation stack at the level of the value of the integer expression. YottaDB implicitly terminates any intervening FOR loops and unstacks variables stacked with NEW commands, as appropriate.

ZGOTO $ZLEVEL:LABEL^ROUTINE takes the same action as GO LABEL^ROUTINE.

ZGOTO $ZLEVEL-1 produces the same result as QUIT (followed by ZCONTINUE, if in Direct Mode).

If the integer expression evaluates to a value greater than the current value of $ZLEVEL, or less than zero (0), YottaDB issues a run-time error.

If ZGOTO has no entry reference, it performs some number of implicit QUITs and transfers control to the next command at the specified level. When no argument is specified, ZGOTO 1 is the result, and operation resumes at the lowest level M routine as displayed by ZSHOW "S". In the image invoked by yottadb -direct, or a similar image, a ZGOTO without arguments returns the process to Direct Mode.

+++++++++++++++++++++++++++++
Displaying Source Code
+++++++++++++++++++++++++++++

Use the ZPRINT command to display source code lines selected by its argument. ZPRINT allows you to display the source for the current routine and any other related routines. Use the ZPRINT command to display the last call level.

Example:

.. code-block:: bash

   YDB>ZPRINT beg
   beg for read !,"Name: ",name do name

This example uses a ZPRINT command to print the line indicated as the call at the top of the stack. Notice that the routine has an error in logic. The line starting with the label beg has a FOR loop with no control variable, no QUIT, and no GOTO. There is no way out of the FOR loop.

++++++++++++++++++++++++++++++++++
Correcting Errors in an M Routine
++++++++++++++++++++++++++++++++++

Now that the routine errors have been identified, correct them in the M source file. Use ZEDIT to invoke the editor and open the file for editing. Correct the errors previously identified and enter to exit the editor.

Example:

.. code-block:: bash

   YDB>ZEDIT "dmex.m"
   dmex;dmex - Direct Mode example
   ;
   beg
     for read !,"Name: ",name do name
     quit
   name
     set ln=$l(name)
     if ln,$extract("QUIT",1,ln)=$tr(name,"quit","QUIT") do
     . set name="Q"
     if ln<30,name?1.a.1"-".a1","1" "1a.ap do print q
     write !,"Please use last-name, "
     write "first-name middle-initial or 'Q' to Quit."
     quit
   print
     write !,$piece(name,", ",2)," ",$piece(name,", ")
     quit
   YDB>

This example shows the final state of a ZEDIT session of dmex.m. Note that the infinite FOR loop at line beg is corrected.

++++++++++++++++++++++++++++++++
Relinking the Edited Routine
++++++++++++++++++++++++++++++++

Use the ZLINK command to add the edited routine to the current image. ZLINK automatically recompiles and relinks the routine. If the routine was the most recent one ZEDITed or ZLINKed, you do not have to specify the routine name with the ZLINK command.

.. note::
   When you issue a DO command, YottaDB determines whether the routine is part of the current image, and whether compiling or linking is necessary. Because this routine is already part of the current image, YottaDB does not recompile or relink the edited version of the routine if you run the routine again without ZLINKing it first. Therefore, YottaDB executes the previous routine image and not the edited routine.

.. note::
   You may have to issue a ZGOTO or a QUIT command to remove the unedited version of the routine from the M invocation stack before ZLINKing the edited version.

Example:

.. code-block:: bash

   YDB>ZLINK
   Cannot ZLINK an active routine

This illustrates a YottaDB error report caused by an attempt to ZLINK a routine that is part of the current invocation stack.

To ZLINK the routine, remove any invocation levels for the routine off of the call stack. You may use the ZSHOW "S" command to display the current state of the call stack. Use the QUIT command to remove one level at a time from the call stack. Use the ZGOTO command to remove multiple levels off of the call stack.

Example:

.. code-block:: bash

   YDB>ZSHOW "S"
   name+3^dmex ($ZTRAP) (Direct mode)
   beg^dmex (Direct mode)
   ^GTM$DMOD (Direct mode)
   YDB>ZGOTO
   YDB>ZSHOW "S"
   ^GTM$DMOD (Direct mode)
   YDB>ZLINK

This example uses a ZSHOW "S" command to display the current state of the call stack. A ZGOTO command without an argument removes all the calling levels above the first from the stack. The ZLINK automatically recompiles and relinks the routine, thereby adding the edited routine to the current image.

++++++++++++++++++++++++++++++++
Reexecuting the Routine
++++++++++++++++++++++++++++++++

Re-display the DO command using the RECALL command.

Execute the routine using the DO command.

Example:

.. code-block:: bash

   YDB>D ^dmex
   Name: Revere, Paul
   Paul Revere
   Name: q

This example illustrates a successful execution of dmex.

++++++++++++++++++++++++
Using Forked Processes
++++++++++++++++++++++++

The ZSYSTEM command creates a new process called the child process, and passes its argument to the shell for execution. The new process executes in the same directory as the initiating process. The new process has the same operating system environment, such as environment variables and input/output devices, as the initiating process. The initiating process pauses until the new process completes before continuing execution.

Example:

.. code-block:: bash

   YDB>ZSYSTEM
   $ ls dmex.*
   dmex.m dmex.o
   $ ps
   PID TTY TIME COMMAND
   7946 ttyp0 0:01 sh
   7953 ttyp0 0:00 ydb
   7955 ttyp0 0:00 ps
   $ exit
   YDB>

This example uses ZSYSTEM to create a child process, perform some shell actions, and return to YottaDB.

----------------------------------
Summary of Debugging Tools
----------------------------------

The following table summarizes YottaDB commands, functions, and intrinsic special variables available for debugging. For more information on these commands, functions, and special variables, see the `“Commands” <./commands.html>`_, `“Functions” <./functions.html>`_, and `“Intrinsic Special Variables” <./isv.html>`_ chapters.

For more information on syntax and run-time errors during Direct Mode, see `Chapter 13: “Error Processing” <./errproc.html>`_.

**Debugging Tools**

+-------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| Extension                                       | Explanation                                                                                                                 |
+=================================================+=============================================================================================================================+
| `$ECode <./isv.html#ecode>`_                    | Contains a list of errors since it was last cleared                                                                         |
+-------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| `$STack <./isv.html#id1>`_                      | Contains the current level of DO/XECUTE nesting from a base of zero (0).                                                    |
+-------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| `$STack() <./functions.html#stack>`_            | Returns information about the M virtual stack context, most of which freezes when an error changes $ECODE from the empty    |
|                                                 | string to a list value.                                                                                                     |
+-------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| `ZBreak <./commands.html#zbreak>`_              | Establishes a temporary breakpoint, with optional count and M action.                                                       |
+-------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| `ZCOMpile <./commands.html#zcompile>`_          | Invokes the YottaDB compiler without a corresponding ZLINK.                                                                 |
+-------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| `ZContinue <./commands.html#zcontinue>`_        | Continues routine execution from a break.                                                                                   |
+-------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| `ZEDit <./commands.html#zedit>`_                | Invokes the UNIX text editor specified by the EDITOR environment variable.                                                  |
+-------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| `ZGoto <./commands.html#zgoto>`_                | Removes zero or more levels from the M invocation stack and transfers control.                                              |
+-------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| `ZLink <./commands.html#zlink>`_                | Includes a new or modified M routine in the current M image; automatically recompiles if necessary.                         |
+-------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| `ZMessage <./commands.html#zmessage>`_          | Signals a specified condition.                                                                                              |
+-------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| `ZPrint <./commands.html#zprint>`_              | Displays lines of source code.                                                                                              |
+-------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| `ZSHow <./commands.html#zshow>`_                | Displays information about the M environment.                                                                               |
+-------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| `ZSTep <./commands.html#zstep>`_                | Incrementally executes a routine to the beginning of the next line of the specified type.                                   |
+-------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| `ZSYstem <./commands.html#zsystem>`_            | Invokes the shell, creating a forked process.                                                                               |
+-------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| `ZWRite <./commands.html#id19>`_                | Displays all or some local or global variables.                                                                             |
+-------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| `$ZCSTATUS <./isv.html#zcstatus>`_              | Contains the value of the status code for the last compile performed by a ZCOMPILE command.                                 |
+-------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| `$ZEDit <./commands.html#zedit>`_               | Contains the status code for the last ZEDit.                                                                                |
+-------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| `$ZLEVel <./isv.html#zlevel>`_                  | Contains the current level of DO/EXECUTE nesting.                                                                           |
+-------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| `$ZMessage() <./functions.html#zmessage>`_      | Returns the text associated with an error condition code.                                                                   |
+-------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| `$ZPOSition <./isv.html#zposition>`_            | Contains a string indicating the current execution location.                                                                |
+-------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| `$ZPROmpt <./isv.html#zprompt>`_                | Controls the symbol displayed as the direct mode prompt.                                                                    |
+-------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| `$ZROutines <./isv.html#zroutines>`_            | Contains a string specifying a directory list containing the object, and optionally, the source files.                      |
+-------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| `$ZSOurce <./isv.html#zsource>`_                | Contains the name of the M source program most recently ZLINKed or ZEDITed; default name for next ZEDIT or ZLINK.           |
+-------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| `$ZStatus <./isv.html#id13>`_                   | Contains error condition code and location of the last exception condition occurring during routine execution.              |
+-------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| `$ZSTep <./isv.html#zstep>`_                    | Controls the default ZSTep action.                                                                                          |
+-------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| `$ZSYstem <./isv.html#zsystem>`_                | Contains the status code of the last ZSYSTEM.                                                                               |
+-------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+


