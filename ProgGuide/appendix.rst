.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2018-2022 YottaDB LLC and/or its subsidiaries.#
.. # All rights reserved.                                        #
.. #                                                             #
.. #     This document contains the intellectual property        #
.. #     of its copyright holder(s), and is made available       #
.. #     under a license.  If you do not know the terms of       #
.. #     the license, please stop and do not read further.       #
.. #                                                             #
.. ###############################################################

.. index::
   M Coding Standards

=================================================
Appendix A: M Coding Standards - Do's and Don'ts
=================================================

.. contents::
   :depth: 3

--------------------------------------
M Coding Standards - Do's and Don'ts
--------------------------------------

M coding standards:

* Improve quality and maintainability by producing uniform standardized code.
* Aid in readability and comprehension of developed source code by providing an unambiguous, easy to read standard to follow.
* Help avoid common errors that would not be picked up by a compiler by using these as a framework for good coding practices.
* Provide an objective reference point for the code authors, maintainers, and reviewers.

+++++
Do's
+++++

~~~~~~~~~~~~~~~
Character Set
~~~~~~~~~~~~~~~

Construct literal strings using only graphic ASCII characters. For non-graphic characters, use the :code:`$CHAR()` or :code:`$ZCHAR()` functions to build strings, use a resource file, or build them from comments in the source code using :code:`$TEXT()`.

Use VIEW "BADCHAR" in production environments.

~~~~~~~~~~~~~~~~~~~~~~
Internationalization
~~~~~~~~~~~~~~~~~~~~~~

Use standard M functions - :code:`$ASCII()`, :code:`$CHAR()`, :code:`$EXTRACT()`, :code:`$FIND()`, :code:`$JUSTIFY()`, :code:`$LENGTH()`, :code:`$PIECE()`, and :code:`$TRANSLATE()` - for character-oriented operations. Use analogous byte-oriented functions - :code:`$ZASCII()`, :code:`$ZCHAR()`, :code:`$ZEXTRACT()/$ZSUBSTR()`, :code:`$ZFIND()`, :code:`$ZJUSTIFY()`, :code:`$ZLENGTH()`, :code:`$ZPIECE()`, and :code:`$ZTRANSLATE()` when the logic calls for byte, rather than character operations. Use :code:`$ZSUBSTR()` to ensure the byte-oriented operation produces a valid character result and :code:`$ZEXTRACT()` when the operation is strictly byte-oriented, with no character implications.

Use :code:`$ZCONVERT()` rather than :code:`$TRANSLATE()/$ZTRANSLATE()` for case conversion.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Alias Variables and Containers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Use Alias Variables and Containers where they promote modularity and an appropriate object-like approach.

~~~~~~~~~~~~~~~~~~~
Parameter Passing
~~~~~~~~~~~~~~~~~~~

Use parameter passing to minimize scoping risk and to implicitly document [sub]routine interfaces. Place variables that are optionally passed at the end of the parameter list. Choose pass-by-reference variable names for clarity and to avoid side effects. Note that using the same name in the actuallist and the formallist may have implications.

~~~~~~~~~~~~~~~~~~
Naked References
~~~~~~~~~~~~~~~~~~

Avoid naked references except when they reduce the width of the line and materially improve readability. In such cases, ensure any naked references follows the full reference, on the same line with no intervening invocation of other code.

~~~~~~~~~~~
Entryrefs
~~~~~~~~~~~

Use the top entryref for invocation from the shell rather than from another routine. Where there is a risk of inadvertent execution from the shell of a routine intended only to be called from M code, protect the top entryref with code that uses :code:`$STACK` to validate whether or not it is invoked from the shell. Existing utility routines with interfaces documented as not requiring a label are exempt from this requirement.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Lines with Multiple Commands
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Use lines with multiple, related commands to improve readability as long as they are not too long. Avoid lines with multiple, unrelated commands.

~~~~~~~~~~~~~
Lines Width
~~~~~~~~~~~~~

Limit lines to no wider than 132 columns.

.. note::
   The line oriented nature of M means that sometimes a single long line can be more readable than a block of code. Such occasional long lines are permissible where justified.

~~~~~~~~~~~~~~~~
Error Handling
~~~~~~~~~~~~~~~~

All product code must have an error trap. Unless the intent of the code is to invoke Direct Mode, the error handler must never use a BREAK explicitly or implicitly to do so. The base error handler in a program suite, and possibly some other error handlers, must provide a way to appropriately preserve the context of unpredictable errors.

New code should generally use :code:`$ETRAP` error handling rather than :code:`$ZTRAP` error handling. As changing error handling can be risky (for example, indirect references in databases), retain error handling in existing code unless you have tested and verified the safety of the change.

~~~~~~~~~~~~~~~~~~~~~~~~
Transaction Processing
~~~~~~~~~~~~~~~~~~~~~~~~

Except for tests, code transactions as restartable, avoid non-Isolated actions (BREAK, JOB, LOCK ZSYSTEM or I/O) within transactions; minimize transaction size, use TRANSACTION="BATCH" for better performance where the solution doesn't require strong Durability or provides Durability with application logic. When LOCKs are appropriate, place them outside the transaction.

~~~~~~~
BREAK
~~~~~~~

To prevent applications from inadvertently falling into direct mode, only use the BREAK command when there is a specific requirement for its use. When circumstances require BREAK commands more persistent than those placed with ZBREAK, or in places within lines, conditionalize them on a debug setting, for example :code:`BREAK:($get(debug)&(<condition>))`.

~~~~~~~~~~~~~~~~~
Argumentless DO
~~~~~~~~~~~~~~~~~

This language construct provides a way to code an embedded subroutine, which stacks $TEST - something extrinsic functions ($$) also do, but DO with an entryref argument does not. It also provides some relief from the line-oriented structure of the language, albeit at some cost.

When the logic calls for multiple invocations of a subroutine avoid using multiple copies of the same argumentless DO body.

Leave a space between the last level indicator (li) and the first command or a comment delimiter.

Be careful with level indicators, as any reduction in number, even for a comment, terminates one or more levels.

~~~~~~~~~~~~~~~~
GOTO and ZGOTO
~~~~~~~~~~~~~~~~

Except for handling logic to effect an exit from nested logic and nested function calls or where appropriate in test code, avoid the use of GOTO and ZGOTO commands. When using these commands outside of test code, you must include a comment explaining why such use works better than any refactoring that would eliminate the [Z]GOTO.

~~~~~~~~~~~~~~~~~~~~
HALT,QUIT and HALT
~~~~~~~~~~~~~~~~~~~~

Choose QUIT, rather than HALT, to terminate a routine, unless there is a clear requirement to the contrary.

Account for the difference in QUIT from a FOR, which does not change the stack level of the M virtual machine and QUIT from a [sub]routine which does change the stack level. This may require the use of a state flag to terminate a FOR that invokes a subroutine.

Choose the argumentless (which requires a following double space) or value form of QUIT as appropriate. When a subroutine can be invoked with either a DO or extrinsic ($$), explicitly code the alternative exits, rather than relying on a setting of $ZQUIT_ANYWAY.

QUIT at the end of a [sub]routine is required unless the [sub]routine ends in a HALT. QUIT at the end of an argumentless DO level is optional.

Use ZHALT to return an invocation to the shell that the termination is abnormal.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
LOCK, ZALLOCATE, and ZDEALLOCATE
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Always specify a timeout on LOCK or LOCK+ commands.

Use a protocol for resource name order to minimize deadlocks. Use the standard incremental LOCK (+/-), rather than ZALLOCATE and ZDEALLOCATE.

~~~~~
NEW
~~~~~

Minimize use of argumentless and exclusive NEW except to satisfy requirements. These variants may be appropriate at the beginning of the base routine of an application.

~~~~~~
READ
~~~~~~

Always use a timeout, except when READing from a file in NOFOLLOW mode. Except when collecting raw or externally validated data, READ into a local variable and validate that the value is appropriate - check for length, range, delimiters and any value restrictions. Always validate input before using it in ways that assume it meets expectations; this is critical when using it in indirection or XECUTE, or storing it durably in a global variable.

~~~~~
SET
~~~~~

When setting several nodes to the same value, specify a list of names within parentheses, rather than separately, e.g., SET (A,B,C)=0 vs. SET A=0,B=0,C=0.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
XECUTE, Indirection, $ZSYSTEM and PIPE device commands
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To minimize run-time errors and prevent out-of-design user induced outcomes, ensure strings for use by the indirection operator, the XECUTE command, the ZSYSTEM command and PIPE device commands are valid, either by program design and implementation, or by validating the value prior to use.

~~~~~~
ELSE
~~~~~~

Use ELSE with care. Because YottaDB stacks $TEST only at the execution of an extrinsic or an argumentless DO command, any XECUTE or DO with an argument has the potential side effect of altering $TEST.

~~~~~~~~~~~~~~~~~~~
Post Conditionals
~~~~~~~~~~~~~~~~~~~

When conditionalizing a single command or transfer of control argument, and there is no need to set $TEST, use postconditionals, as they provide a slight performance advantage and tend to improve readability by tying the condition closely to the action. For example, choose SET:<condition> over IF <condition> SET.

~~~~~~~~~~~~~~~~~~~~~~
$ZDATA() and $DATA()
~~~~~~~~~~~~~~~~~~~~~~

Use $ZDATA() rather than $DATA() unless the logic needs to ignore alias implications.

~~~~~~~~~~~~
$INCREMENT()
~~~~~~~~~~~~

You may use an IF to discard the result of INCREMENT() in order to take advantage of the INCREMENT() side-effect.

~~~~~~~
$NEXT()
~~~~~~~

Use $ORDER() rather than $NEXT(), which is deprecated.

~~~~~~~~~~
$PIECE()
~~~~~~~~~~

If using a piece of data more than once, extract the data to a local variable for reuse, rather than using repeated invocations of $PIECE() to extract the same piece of data.

~~~~~~~~~~~
$RANDOM()
~~~~~~~~~~~

$RANDOM(1) always returns 0 and so is never appropriate. Adjust $RANDOM() results with appropriate arithmetic to achieve the desired range.

~~~~~~~~~~~~~~
$ZPREVIOUS()
~~~~~~~~~~~~~~

Use the standard $ORDER(x,-1), rather than $ZPREVIOUS(x).

~~~~~~~~~~~~~~~~~~~
$ETRAP vs. $ZTRAP
~~~~~~~~~~~~~~~~~~~

Use $ETRAP rather than $ZTRAP unless there are good reasons not to.

~~~~~~~~~~~~~~
$KEY vs. $ZB
~~~~~~~~~~~~~~

Use $KEY rather than $ZB, unless code must run on old versions of YottaDB which do not maintain $KEY for the target device.

~~~~~~~~~~~~~~~~~~~~
$STACK vs. $ZLEVEL
~~~~~~~~~~~~~~~~~~~~

Use $STACK rather than $ZLEVEL, unless it eliminated arithmetic or existing uses of $ZLEVEL are so numerous as to make a change high risk.

~~~~~~~~~~~
$ZCMDLINE
~~~~~~~~~~~

Validate all input from $ZCMDLINE as if it were from a READ for user input.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Extrinsic Functions and Special Variables ($$)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When a subroutine needs to return a single value or an array of values, choose an Extrinsic invocation over a DO in order to minimize scoping risks.

~~~~~~~~~~
Triggers
~~~~~~~~~~

While YottaDB does not restrict trigger code from performing I/O operations, avoid using OPEN, USE, READ, WRITE and CLOSE within trigger application code. Such operations may be useful for development and diagnostic purposes. Triggers implicitly run as TP transactions and I/O violates the ACID property of Isolation , as do JOB, LOCK, ZSYSTEM and external calls.

Use comprehensive and strong coding conventions for trigger code or rely on user-specified names in managing the deletion and replacement of triggers.

Except when using triggers for debugging, use journaling on any region that uses triggers.

~~~~~~~~~~~~~~~~~~~
Call-in/Call-outs
~~~~~~~~~~~~~~~~~~~

Use ydb_malloc()/ydb_free() in the external functions for enhanced performance and better debugging capability in case memory management problems occur with external calls.

Use ydb \*t types defined in libyottadb.h instead of the native types (int, float, char, etc) to avoid potential size mismatches with the parameter types.

~~~~~~~~~~~~
Autorelink
~~~~~~~~~~~~

Either auto-relink-enable or auto-relink-disable the directory in $zroutines for the life of the process.

Use the same value of $ydb_linktmpdir for all processes. All processes that share a directory whose contents are subject to ZRUPDATE use the same value for $ydb_linktmpdir so that all processes see update notifications - with different values of $ydb_linktmpdir, a ZRUPDATE by a process with one value of $ydb_linktmpdir would not be observed by a process with a different value of that environment variable.

+++++++++++++++++
Don'ts
+++++++++++++++++

~~~~~~~~~~~~~~~~~~~~~~~~~
Source and Object Files
~~~~~~~~~~~~~~~~~~~~~~~~~

Never change the name of an object file.

When forming routine names, the compiler truncates object filenames to a maximum length of 31 characters. For example, for a source file called Adatabaseenginewithscalabilityproven.m the compiler generates an object file called Adatabaseenginewithscalabilityp.o. Never let YottaDB routines file names exceed 31 characters.

~~~~~~~~~
kill -9
~~~~~~~~~

Killing a process with kill -9 may cause database damage. Use MUPIP STOP or MUPIP INTRPT instead. Use kill -9 as the last resort if the process does not respond to MUPIP STOP. kill -9 terminates the process abruptly and may leave database files improperly closed and require a MUPIP RUNDOWN. Because kill -9 may cause database damage, perform a MUPIP INTEG immediately after a kill -9.

~~~~~~~~~~~~~~~~~
Operate as Root
~~~~~~~~~~~~~~~~~

Never run a routine as root.

Other than YottaDB installation, never perform any YottaDB operation as root.

~~~~~~~~~~
Triggers
~~~~~~~~~~
Never use chained and nested triggers that potentially update the same piece of a global variable. You should always assess the significance of having chained triggers for a database update especially because of the arbitrary trigger execution order.

Never access ^#t with DSE, except with guidance from your YottaDB support channel. Manage trigger definitions with MUPIP TRIGGER and $ZTRIGGER().

~~~~~~~~~~~~~~~~~
Local Variables
~~~~~~~~~~~~~~~~~

Never use exponential numeric form in the subscripts. It may lead to ambiguities. Because numeric subscripts collate ahead of string subscripts, the string subscript "01E5" is not the same as the numeric subscript 01E5.

Never SET $ZWRTACn "variables". They are used by YottaDB to make ZWRITE output more useful but are not supported for any other purpose. They are only mentioned here because you may see them in the output of ZWRITE and ZSHOW "V".

You can use SET @ to process ZWRITE or ZSHOW "V" output containing $ZWRTACn variables for restoring an alias container variable to a prior state. While processing the output, never attempt to inject or manipulate $ZWRTACn lines as it may lead to unintended consequences or undermine the benefit you might achieve from using alias containers. Lines containing SET $ZWRTACn=<value> are no-ops unless they have a preceding SET $ZWRTAC="" and an alias container variable association. In the ZWRITE or ZSHOW "V" output of an alias container, SET $ZWRTAC lines appear in the order that YottaDB expects for restoration. YottaDB can change the use of $ZWRTAC in YottaDB at any time.


