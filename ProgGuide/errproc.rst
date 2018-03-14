
.. index::
   Error Processing

============================
13. Error Processing
============================

.. contents::
   :depth: 2

This chapter describes YottaDB features and techniques for handling errors. Errors in programs may be classified as "predictable" meaning foreseen, or "unpredictable" meaning unforeseen.

M programs may attempt to recover from predictable errors. Device errors that can be remedied by an operator are the most common class of errors for which recovery provides a large benefit. Predictable errors from which the program does not attempt to recover are generally treated the same as unpredictable errors.

A typical application handles unpredictable errors by logging as much information about the error as the designer considers useful, then terminating or restarting the application from a known point.

Because YottaDB invokes error handling when things are not normal, careful design and implementation of error handling are required to minimize the impact of errors and the cost of subsequent prevention.

The YottaDB compiler detects and reports syntax errors at:

* Compile time while producing the object module from a source file.
* Run time while compiling code for M indirection and XECUTEs.
* Run time when the user is working in Direct Mode.

The YottaDB run-time system:

* Recognizes and reports execution errors when they occur.
* Reports errors flagged by the compiler when they fall in the execution path.

--------------------------------------------
Compile Time Error Message Format
--------------------------------------------

To understand the compile-time error message format, consider this incorrect source line:

.. parsed-literal::
   S=B+C        

If this were line 7 of a source file ADD2.m, the compiler reports the compile-time error with the message:

.. parsed-literal::
   S=B+C 
       ^-----
   At column 4, line 7, source module ADD2
   Variable expected in this context

The compile-time error message format consists of three lines. The first two lines tell you the line and location where the error occurred. The last line describes the M syntax error. The positioning accuracy of the carat pointing to the location of an issue in a source line depends on your terminal settings, particularly in UTF-8 mode where character widths are not uniform. If the line exceeds the terminal width as understood by YottaDB, it replaces the carat line with an ARROWNTDSP error.

If you requested a listing file, it contains the same information and looks as follows: 

.. parsed-literal::
   .
   .
   6 .  .  .
   7 S=B+C
       ^-----
   Variable expected in this context
   8 . . .
   .
   .

---------------------------------------
Processing Compile Time Errors
---------------------------------------

At compile-time, the compiler stops processing a routine line as soon as it detects the first error on that line. By default, the compiler displays the line in error on stderr, and also in a listing file when the compiler options include -list. By default, the compiler processes the remaining source lines until it exceeds the maximum error count of 127.

The compile-time error message format displays the line containing the error and the location of the error on the line. The error message also indicates what was incorrect about the M statement. For more information on the error message format, refer to the `Messages and Recovery Procedures Reference Manual <https://docs.yottadb.com/MessageRecovery/index.html>`_.

You may correct compile-time errors immediately by activating an editor and entering the correct syntax in the source program. Because several errors may occur on a line, examine the line carefully to avoid compiling the routine several times.

The MUMPS command qualifier -ignore, which is the default, instructs YottaDB to produce an object file even if the compiler detects errors in the source code. As long as the execution path does not encounter the compile-time errors, the YottaDB run-time system executes the compiled-as-written routine. You may take advantage of this feature to exercise some parts of your program before correcting errors detected by the compiler.

-----------------------------
Run Time Error Message Format
-----------------------------

To understand the run-time error message format, consider this short program printsum.m:

.. parsed-literal::
       SET A=17
  GO   SET B=21
       WRITE A+C

When you try to execute this program, the last statement causes an error since the variable C is undefined. If $ETRAP="B", YottaDB displays the run-time error message:

.. parsed-literal::
   $ mumps -run printsum
   %GTM-E-UNDEF, Undefined local variable: C
   At MUMPS source location GO+1^printsum
   YDB>

YottaDB informs you of the error (Undefined local variable) and where in the routine the error occurred (GO+1). Note that the run-time system displays the YDB> prompt, indicating that the process has entered Direct Mode. YottaDB places run time error information in the intrinsic special variables $ECODE and $ZSTATUS.

Compile-time error messages may appear at run time. This is because errors in indirection and the compile-as-written feature leave errors that are subsequently reported at run time.

The YottaDB utilities use portions of the run-time system and therefore may issue run-time errors as well as their own unique errors.

-------------------------------------
Processing Run-Time Errors
-------------------------------------

YottaDB does not detect certain types of errors associated with indirection, the functioning of I/O devices, and program logic until run-time. Also, the compile-as-written feature may leave errors which YottaDB reports at run-time when it encounters them in the execution path. At run-time, YottaDB reports any error encountered to stderr. The run-time system suspends normal execution of the routine as soon as it detects an error.

YottaDB responds to errors differently depending on whether it encounters them in Direct Mode (at the command line) or during normal program execution.

When an executing YottaDB image encounters an error:

* if Direct Mode is active at the top of the invocation stack, YottaDB stays in Direct Mode.
* otherwise, if the error comes from a device that has an EXCEPTION, YottaDB executes the EXCEPTION string.
* otherwise, if $ETRAP'="" YottaDB transfers control to the code defined by $ETRAP as if it had been inserted at the point of the error, unless $ECODE'="", in which case it executes a TROLLBACK:$TLEVEL followed by a QUIT:$QUIT "" QUIT.
* otherwise, if $ZTRAP'="" YottaDB executes $ZTRAP.
* otherwise, YottaDB performs a QUIT:$QUIT "" QUIT and reissues the error at the new stack level, if no other error traps ($ETRAP or $ZTRAP) are uncovered by descending the stack, YottaDB reports the error on the principal device and terminates the image.

After the action, if any, invoked by $ETRAP, $ZTRAP or EXCEPTION:

* if the process ends in Direct Mode – as a result either of performing a BREAK in the executed string or of starting in Direct Mode – YottaDB reports the error on the principal device.
* otherwise, if the executed string contains an unstacked transfer of control, the only implicit behavior is that as long as $ECODE'="" and $ZTRAP'="" an attempt to QUIT from the level of the current error causes that error to be reissued at the new stack level.
* otherwise, if $ETRAP'="" YottaDB performs a QUIT$QUIT "" QUIT and reissues the error at the new stack level.
* otherwise, $ZTRAP must contain code and YottaDB retries the line of M on which the error occurred.

+++++++++++++++++++++++++++++++
Run-Time Errors in Direct Mode
+++++++++++++++++++++++++++++++

When YottaDB detects an error in Direct Mode, it reports the error with a message and leaves the process at the YDB> prompt.

Example:

.. parsed-literal::
   YDB>ZW
   ZW
   ^_____
   %GTM-E-INVCMD, Invalid command keyword encountered
   YDB>   

In Direct Mode, YottaDB provides access to the RECALL command. RECALL allows you to retrieve a Direct Mode command line with a minimum of typing. The YottaDB line editor allows you to make quick changes or corrections to the command line. For more information on RECALL and the line editor, see `Chapter 4: “Operating and Debugging in Direct Mode” <https://docs.yottadb.com/ProgrammersGuide/opdebug.html>`_.

+++++++++++++++++++++++++++++++++++++++++++++
Run-Time Errors Outside of Direct Mode
+++++++++++++++++++++++++++++++++++++++++++++

If YottaDB encounters an error outside of code entered in Direct Mode, YottaDB executes the $ETRAP or $ZTRAP special variable, if either of them have a length greater than zero, which only one can have at a given point in time.

The $ETRAP and $ZTRAP special variables specify an action that YottaDB should perform when an error occurs during routine execution. $ETRAP and $ZTRAP can establish one or more error handling "actions". 

.. note::
   The environment variable gtm_etrap specifies an initial value of $ETRAP to override the default value of "B" for $ZTRAP as the base level error handler. The gtmprofile script sets gtm_etrap to "Write:(0=$STACK) ""Error occurred: "",$ZStatus,!" which you can customize to suit your needs. For more information, refer to “Processing Errors from Direct Mode and Shell”.

------------------------------
Program Handling of Errors
------------------------------

YottaDB provides the error handling facilities described in the M standard. In addition, YottaDB provides a number of extensions for error handling. Both are discussed in the following sections. The following table summarizes some of the tools, which are then described in more detail within the context of various techniques and examples. 

**Summary of YottaDB Error-Handling Facilities**

+-------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Extension                     | Explanation                                                                                                                                                                 |
+===============================+=============================================================================================================================================================================+
| OPEN/USE/CLOSE EXCEPTION      | Provides a deviceparameter specifying an XECUTE string or entryref that YottaDB invokes upon encountering a device-related exception condition.                             |
+-------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| MUMPS -list ZLINK :"-list"    | Creates a listing file of all the errors detected by the compiler and detects syntax errors. Useful in the process of re-editing program to correct errors.                 |
+-------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Zgoto                         | Provides for removing multiple levels from the M invocation stack.                                                                                                          |
+-------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| ZMESSAGE                      | Creates or emulates arbitrary errors.                                                                                                                                       |
+-------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $STACK                        | Contains the current level of M execution stack depth.                                                                                                                      |
+-------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $STACK()                      | Returns values describing aspects of the execution environment.                                                                                                             |
+-------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ECODE                        | Contains a list of error codes for "active" errors; these are the errors that have occurred, but have not yet been cleared.                                                 |
+-------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ESTACK                       | Contains an integer count of M virtual machine stack levels that have been activated and not removed, since the last time $ESTACK was NEW'd.                                |
+-------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ETRAP                        | Contains a string value that YottaDB invokes when an error occurs during routine execution.                                                                                 |
+-------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $QUIT                         | Indicates whether the current block of code was called as an extrinsic function or a subroutine.                                                                            |
+-------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZCSTATUS                     | Holds the value of the status code for the last compilation performed by a ZCOMPILE command.                                                                                |
+-------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZEDIT                        | Holds the value of the status code for the last edit session invoked by a ZEDIT command.                                                                                    |
+-------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZEOF                         | Holds the value '1' (TRUE) if the last READ on the current device reached end-of-file, otherwise holds a '0' (FALSE).                                                       |
+-------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZERROR                       | Contains a string supplied by the application, typically one generated by the code specified in $ZYERROR.                                                                   |
+-------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZLEVEL                       | Contains current level of DO/EXECUTE nesting ($STACK+1).                                                                                                                    |
+-------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZMESSAGE()                   | Translates a UNIX/YottaDB condition code into text form.                                                                                                                    |
+-------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZSTATUS                      | Contains the error condition code and location of last exception condition occurring during routine execution.                                                              |
+-------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZTRAP                        | Contains an XECUTE string or entryref that YottaDB invokes upon encountering an exception condition.                                                                        |
+-------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZYERROR                      | Contains an entryref to invoke when an error occurs; typically used to maintain $ZERROR.                                                                                    |
+-------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------+

+++++++++++++++++++
$ECODE
+++++++++++++++++++

The value of $ECODE is a string that may reflect multiple error conditions. As long as no error has occured, the value of $ECODE is equal to the empty string.

$ECODE contains a list of errors codes for "active" errors - the error conditions which are not yet resolved. If there are no active errors, $ECODE contains the empty string. The value of $ECODE can be SET.

The most recent error in $ECODE appears first, the oldest last. If the error is defined by the M standard, the code starts with an "M", YottaDB error codes including those provided by OS services start with "Z", and application defined codes must start with "U". Every code is separated by a comma (,) and there is always a comma at the beginning and at the end of a list. YottaDB provided codes are those reported in $ZSTATUS, interpreted by $ZMESSAGE() and recognized as arguments to ZMESSAGE command. When YottaDB supplies a standard error code in $ECODE, it also supplies a corresponding 'Z' code.

.. note::
   See “$ECode” for a detailed description of $ECODE.

Example (setting $ECODE):

.. parsed-literal::
   SET $ECODE="" ;sets $ECODE to the empty string
   SET $ECODE=",M20," ;an ANSI M standardized error code
   SET $ECODE=",U14," ;user defined error code
   SET $PIECE($ECODE,",",2)="Z3," ;insert a non-ANSI error code
   SET $PIECE($ECODE,",",$LENGTH($ECODE,",")+1)="An..," ;append

Standard Error processing affects the flow of control in the following manner. Detection of an error causes GOTO implicit sub-routine. When $ECODE="", the implicit subroutine is $ETRAP and QUIT:$QUIT "" QUIT. Otherwise the implicit subroutine is $ETRAP followed by TROLLBACK:$TLEVEL and then QUIT:$QUIT "" QUIT.

The QUIT command behaves in a special fashion while the value of $ECODE is non-empty. If a QUIT command is executed that returns control to a less nested level than the one where the error occurred, and the value of $ECODE is still non-empty, first all normal activity related to the QUIT command occurs (especially the unstacking of NEWed variables) and then the current value of $ETRAP is executed. Note that, if $ETRAP had been NEWed at the current or intervening level, the unstacked value of $ETRAP is executed.

SETting $ECODE to an invalid value is an error. SETting $ECODE to a valid error behaves like detection of error. SETting $ECODE="" does not cause a change in the flow, but effects $STACK(), subsequent $QUITs and errors.

**Note:**

To force execution of an error trap or to flag a user-defined error ("U" errors), make the value of $ECODE non-empty:

.. parsed-literal::
   SET $ECODE=",U13-User defined error trap,"

.. note::
   The value of $ECODE provides information about errors that have occurred since the last time it was reset to an empty string. In addition to the information in this variable, more detailed information can be obtained from the intrinsic function $STACK. For more information, see the section on “$STack()”.

++++++++++++++++++
$ZSTATUS Content
++++++++++++++++++

$ZSTATUS contains a string value specifying the error condition code and location of the last exception condition that occurred during routine execution. For further details, see “$ZStatus”.

+++++++++++++++++++++++
$ZERROR and $ZYERROR
+++++++++++++++++++++++

After an error occurs, if $ZYERROR is set to a valid entryref that exists in the current environment, YottaDB invokes the routine at that entryref with an implicit DO before returning control to M code specified by a device EXCEPTION, $ETRAP or $ZTRAP. It is intended that the code invoked by $ZYERROR use the value of $ZSTATUS to select or construct a value to which it SETs $ZERROR.

If $ZYERROR is empty, $ZYERROR="unprocessed $ZERROR, see $ZSTATUS".

If there is a problem with the content of $ZYERROR or if the execution of the code it invokes, YottaDB sets $ZERROR=$ZSTATUS for the secondary error and terminates the attempt to use $ZYERROR. During code evoked by $ZYERROR, the value of $ZERROR is the empty string.

+++++++++++++++++++++
$ETRAP Behavior
+++++++++++++++++++++

If, at the time of any error, the value of $ETRAP is non-empty, YottaDB proceeds as if the next instruction to be excuted were the first one on "the next line" and the code on that next line would be the same as the text in the value of $ETRAP. Furthermore, YottaDB behaves as if the line following "the next line" looks like:

.. parsed-literal::
   QUIT:$QUIT "" QUIT

When SET assigns a value to $ETRAP, the new value replaces the previous value, and if $ZTRAP was not empty (in control), the value of $ZTRAP becomes equal to the empty string without being stacked.

+++++++++++++++++++++++++++++++++
Nesting $ETRAP and using $ESTACK
+++++++++++++++++++++++++++++++++

When you need to set up a stratified scheme where one level of subroutines use one error trap setting and another more nested subroutine uses a different one; the more nested subroutine must NEW $ETRAP. When $ETRAP is NEWed, its old value is saved and copied to the current value. A subsequent SET $ETRAP=<new-value> then establishes the error trapping code for the current execution level.

The QUIT command that reverts to the calling routine causes the NEWed values to be unstacked, including the one for $ETRAP.

If an error occurs while executing at the current execution level (or at an execution level farther from the initial base stack frame), YottaDB executes the code from the current $ETRAP. Unless a GOTO or ZGOTO in $ETRAP or any code it invokes redirects the flow of execution, when the execution of the $ETRAP code completes, control reverts to the implicit QUIT command, which returns to the routine that invoked the code that encountered the error. At this time, the QUIT reinstates any prior value of $ETRAP.

While at the more nested execution level(s), if an error occurs, YottaDB executes the code from the current $ETRAP. After the QUIT to a less nested level, YottaDB invokes the code from the now current $ETRAP. The current $ETRAP may be different from the $ETRAP at the time of the error due to unstacking. This behavior continues until one of the following possible situations occur:

* $ECODE is empty. When the value of $ECODE is equal to the empty string, error processing is no longer active, and normal processing resumes.
* A QUIT reaches an execution level where the value of $ETRAP is empty ($ZTRAP might be non-empty at that level). When the values of both $ZTRAP and $ETRAP are equal to the empty string, no error trapping is active and the QUIT repeats until it unstacks a $ETRAP or $ZTRAP.
* The stack is reduced to an empty state. When there is no previous level left to QUIT into, YottaDB returns to the operating system level shell. A frame that is in direct mode stops the process by putting the user back into the Direct Mode shell.

When dealing with stratified error trapping, it is important to be aware of two additional intrinsic variables: $STACK and $ESTACK. The values of both of these variables indicate the current execution level. The value of $STACK is an "absolute" value that counts from the start of the YottaDB process, whereas the value of $ESTACK restarts at zero (0) each time $ESTACK is NEWed.

It is often beneficial to NEW both $ETRAP and $ESTACK a the same time.

++++++++++++++++++++++++++
$ZTRAP Behavior
++++++++++++++++++++++++++

If, at the time of any error, the value of $ZTRAP is non-empty, YottaDB uses the $ZTRAP contents to direct execution of the next action. Refer to the $ZTRAP section in `Chapter 8: “Intrinsic Special Variables” <https://docs.yottadb.com/ProgrammersGuide/isv.html>`_.

By default, execution proceeds as if the next instruction to be executed were the first one on "the next line", and the code on that next line would be the same as the text in the value of $ZTRAP. Unless $ZTRAP or any code it invokes issues a GOTO or ZGOTO, after YottaDB has executed the code in $ZTRAP, YottaDB attempts to execute the line with the error again. When a value is assigned to $ZTRAP, the new value replaces the previous value. If the value of $ETRAP is a non-empty one, $ETRAP is implicitly NEWed, and the value of $ETRAP becomes equal to the empty string; this ensures that at most one of $ETRAP and $ZTRAP is not the empty string. If the environment variable gtm_ztrap_new evaluates to Boolean TRUE (case insensitive string "TRUE", or case insensitive string "YES", or a non-zero number), $ZTRAP is NEWed when $ZTRAP is SET; otherwise $ZTRAP is not stacked when it is SET.

Other than the default behavior, $ZTRAP settings are controlled by the environment variable gtm_ztrap_form as described in the following table.

+------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| gtm_ztrap_form         | $ZTRAP and EXCEPTION Behavior                                                                                                                                                     |
+========================+===================================================================================================================================================================================+
| code                   | Content is code executed after the error; in the absence of GOTO, ZGOTO, or QUIT, execution resumes at the beginning of the line containing the error - note that the default     |
|                        | behavior tends to create an indefinite loop.                                                                                                                                      |
+------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| entryref               | Content is an entryref to which control is transferred by an implicit GOTO                                                                                                        |
+------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| adaptive               | If content is valid code treat it as described for "code", otherwise attempt to treat it as an entryref                                                                           |
+------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| popentryref            | Content is entryref - remove M virtual stack levels until the level at which $ZTRAP was SET, then GOTO the entryref; the stack manipulation occurs only for $ZTRAP and not for    |
|                        | EXCEPTION                                                                                                                                                                         |
+------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| popadaptive            | If content is valid code treat it as described for code, otherwise attempt to treat it as an entryref used as described for popentryref                                           |
+------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+

Although the "adaptive" and "popadaptive" behaviors permit mixing of two behaviors based on the current value of $ZTRAP, the $ZTRAP behavior type is selected at process startup from gtm_ztrap_form and cannot be modified during the life of the process.

.. note::
   Like $ZTRAP values, invocation of device EXCEPTION values, with the exception noted, follow the pattern specified by the current gtm_ztrap_form setting.

++++++++++++++++++++++++++++++++++++++++++++++
Differences between $ETRAP and $ZTRAP
++++++++++++++++++++++++++++++++++++++++++++++

The activation of $ETRAP and $ZTRAP are the same, however there are a number of differences in their subsequent behavior.

For subsequent errors the then current $ZTRAP is invoked, while with $ETRAP, behavior is controlled by the state of $ECODE. This means that when using $ZTRAP, it is important to change $ZTRAP, possibly to the empty string, at the beginning of the action in order to protect against recursion caused by any errors in $ZTRAP itself or in the code it invokes.

If there is no explicit or implicit GOTO or ZGOTO in the action, once a $ZTRAP action completes, execution resumes at the beginning of the line where the error occurred, while once a $ETRAP action completes, there is an implicit QUIT. This means that $ZTRAP actions that are not intended to permit a successful retry of the failing code should contain a GOTO, or more typically a ZGOTO. In contrast, $ETRAP actions that are intended to cause a retry must explicitly reinvoke the code where the error occurred.

For QUITs from the level at which an error occurred, $ZTRAP has no effect, where $ETRAP behavior is controlled by the state of $ECODE. This means that to invoke an error handler nested at the lower level, $ZTRAP actions need to use an explicit ZMESSAGE command, while $ETRAP does such invocations implicitly unless $ECODE is SET to the empty string.

+++++++++++++++++++++++++++++++++
$ZTRAP Interaction With $ETRAP
+++++++++++++++++++++++++++++++++

It is important to be aware of which of the trap mechanisms is in place to avoid unintended interactions, and aware of which conditions may cause a switch-over from one mode of error handling to the other.

When a SET command assigns a value to either $ZTRAP or $ETRAP, YottaDB examines the value of the other error handling variable. If the other value is non-empty, YottaDB executes an implicit NEW command that saves the current value of that variable, and then assigns that variable to the empty string, then makes the requested assignment effective.

For example, re-setting $ETRAP is internally processed as:

.. parsed-literal::
   NEW:$LENGTH($ZTRAP) $ZTRAP $ETRAP SET $ETRAP=code        

Whereas, SET $ZTRAP=value is internally processed as:

.. parsed-literal::
   NEW:$LENGTH($ETRAP) $ETRAP SET:$LENGTH($ETRAP)="" SET $ZTRAP=value

Note that, after saving the prior value, YottaDB ensures the superseded $ETRAP or $ZTRAP implicitly gets the value of the empty string. As a result, at most one of the two error handling mechanisms can be effective at any given point in time.

If an error handling procedure was invoked through the $ETRAP method, and the value of $ECODE is non-empty when QUITing from the level of which the error occurred, the behavior is to transfer control to the error handler associated with the newly unstacked level. However, if the QUIT command at the end of error level happens to unstack a saved value of $ZTRAP (and thus cause the value of $ETRAP to become empty), the error handling mechanism switches from $ETRAP-based to $ZTRAP-based.

.. note::
   At the end of an error handling procedure invoked through $ZTRAP, the value of $ECODE is not examined, and this value (if any) does not cause any transfer to another error handling procedure. However, if not cleared it may later trigger a $ETRAP unstacked by a QUIT.

++++++++++++++++++++++++++++++++++++
Choosing $ETRAP or $ZTRAP
++++++++++++++++++++++++++++++++++++

Making a choice between the two mechanisms for error handling is mostly a matter of compatibility. If compatibility with existing YottaDB code is important, and that code happens to use $ZTRAP, then $ZTRAP is the best effort choice. If compatibility with code written in MUMPS dialects from other vendors is important, then $ETRAP or a non-default form of $ZTRAP probably is the better choice.

When no pre-existing code exists that favors one mechanism, the features of the mechanisms themselves should be examined.

Almost any effect that can be achieved using one mechanism can also be achieved using the other. However, some effects are easier to achieve using one method, and some are easier using with the other.

If the mechanisms are mixed, or there is a desire to refer to $ECODE in an environment using $ZTRAP, it is recommended to have $ZTRAP error code SET $ECODE="" at some appropriate time, so that $ECODE does not become cluttered with errors that have been successfully handled.

.. note::
   A device EXCEPTION gets control after a non-fatal device error and $ETRAP/$ZTRAP get control after other non-fatal errors.

**Example 1: Returning control to a specific execution level**

The following example returns control to the execution level "level" and then to an error processing routine "proc^prog".

With $ZTRAP: Set $ZTRAP="ZGOTO "_level_":proc^prog"

With $ETRAP: Set $ETRAP="Quit:$STACK>"_level_" Do proc^prog"

Note that, ZGOTO can be used with $ETRAP and $STACK with $ZTRAP. Alternatively if $ESTACK were NEWed at LEVEL:

.. parsed-literal::
   Set $ETRAP="Quit:$ESTACK>0 Do proc^prog"

**Example 2: Ignoring an Error**

With $ZTRAP: Set $ZTRAP="Quit"

With $ETRAP: Set $ETRAP="Set $ECODE="""" Quit"

Note that, while it is not necessary to SET $ECODE="" when using $ZTRAP it is advisable to do it in order to permit mixing of the two mechanisms.

**Example 3: Nested Error Handlers**

With $ZTRAP: New $ZTRAP Set $ZTRAP=...

With $ETRAP: New $ETRAP Set $ETRAP=...

.. note::
   In both cases, QUITting to a lower level may effectively make the other mechanism active.

**Example 4: Access to "cause of error"**

With $ZTRAP: If $ZSTATUS[...

With $ETRAP: If $ECODE[...

.. note::
   The value of $ZSTATUS reflects only the most recent error, while the value of $ECODE is the cumulative list of all errors since its value was explicitly set to empty. Both values are always maintained and can be used with either mechanism.

++++++++++++++++++++++++++++++
Error Processing Cautions
++++++++++++++++++++++++++++++

$ETRAP and $ZTRAP offer many features for catching, recognizing, and recovering from errors. Any code within an error processing subroutine may cause its own errors and these need to be processed without causing an infinite loop (where an error is caught, which, while being processed causes another error, which is caught, and so on).

During the debugging phase, such loops are typically the result of typographical errors in code. Once these typographical errors are corrected, the risk remains that an error trapping subroutine was designed specifically to deal with an expected condition; such as the loss of a network connection. This then creates an unexpected error of its own, such as:

* a device that had not yet been opened because the loss of network connectivity occured sooner than expected
* an unexpected data configuration caused by the fact that an earlier instance of the same program did not complete its task for the same reason

.. note::
   It is important to remain aware of any issues that may arise within an error trapping procedure, and also of the conditions that might cause the code in question to be invoked.

$ETRAP is recursively invoked if it invokes a GOTO or a ZGOTO and the error condition persists in the code path and the code SETs $ECODE="". $ZTRAP is recursively invoked if the error condition persists in the code path.

+++++++++++++++++++++++++++++++
Input/Output Errors
+++++++++++++++++++++++++++++++

When YottaDB encounters an error in the operation of an I/O device, YottaDB executes the EXCEPTION deviceparameter for the OPEN/USE/CLOSE commands. An EXCEPTION deviceparameter specifies an action to take when an error occurs in the operation of an I/O device. The form of the EXCEPTION action is subject to the gtm_ztrap_form setting described for $ZTRAP, except that there is never any implicit popping with EXCEPTION actions. If a device has no current EXCEPTION, YottaDB uses $ETRAP or $ZTRAP to handle an error from that device.

YottaDB provides the option to:

* Trap or process an exception based on device error.
* Trap or process an exception based on terminal input.

An EXCEPTION based on an error for the device applies only to that device, and provides a specific error handler for a specific I/O device.

The CTRAP deviceparameter for USE establishes a set of trap characters for terminal input. When YottaDB encounters an input character in that set, YottaDB executes the EXCEPTION deviceparamenter, or, $ETRAP or $ZTRAP if the device has no current EXCEPTION.

Example:

.. parsed-literal::
   YDB>ZPRINT ^EP12
   EP12    WRITE !,"THIS IS ",$TEXT(+0)
           SET $ECODE="";this only affects $ETRAP
           SET $ETRAP="GOTO ET"
           ;N $ZT S $ZT="W !,"CAN'T TAKE RECIPROCAL OF 0"",*7"
           USE $P:(EXCEPTION="D BYE":CTRAP=$C(3))
           WRITE !,"TYPE <CTRL-C> TO STOP"
   LOOP    FOR  DO
           . READ !,"TYPE A NUMBER: ",X
           . WRITE ?20,"HAS RECIPROCAL OF: ",1/X
           . QUIT
   ET      . WRITE !,"CAN'T TAKE RECIRPOCAL OF 0",*7
           . SET $ECODE=""
           QUIT
   BYE     WRITE !,"YOU TYPED <CTRL-C> YOU MUST BE DONE!"
           USE $P:(EXCEPTION="":CTRAP="")
           WRITE !,"$ZSTATUS=",$ZSTATUS
           ZGOTO 1
   YDB>DO ^EP12
   THIS IS EP12
   TYPE <CTRL-C> TO STOP
   TYPE A NUMBER: 1 HAS RECIPROCAL OF: 1
   TYPE A NUMBER: 2 HAS RECIRPOCAL OF: .5
   TYPE A NUMBER: 3 HAS RECIPROCAL OF: .33333333333333
   TYPE A NUMBER: 4 HAS RECIPROCAL OF: .25
   TYPE A NUMBER: HAS RECIPROCAL OF:
   CAN'T TAKE RECIPROCAL OF 0
   TYPE A NUMBER:
   YOU TYPED <CTRL-C> YOU MUST BE DONE!
   $ZSTATUS=150372498,LOOP+1^EP12,%GTM-E-CTRAP,Character trap $C(3) encountered
   YDB>


This routine prompts the user to enter a number at the terminal. If the user enters a zero, YottaDB encounters an error and executes $ETRAP (or $ZTRAP). The action specified reports the error and returns to prompt the user to enter a number. With $ZTRAP, this is very straightforward. With $ETRAP, some care is required to get the code to resume at the proper place. The CTRAP deviceparameter establishes <CTRL-C> as a trap character. When YottaDB encounters a <CTRL-C>, YottaDB executes the EXCEPTION string whcih transfers control to the label BYE. At the label BYE, the routine terminates execution with an error message. Using the EXCEPTION deviceparameter with CTRAP generally simplifies $ETRAP or $ZTRAP handling.

$ZSTATUS allows the routine to find out which trap character YottaDB encountered. When a routine has several character traps set, $ZSTATUS provides useful information for identifying which character triggered the trap, and thereby allows a custom response to a specific input.

---------------------------------
Error Actions
---------------------------------

In the following examples (and the previous one as well), $ETRAP and $ZTRAP in most cases have similar behavior. The most prominent difference is that, when $ETRAP is active, $ECODE determines whether or not a second error in an M stack level triggers an immediate implicit QUIT from that level. For additional information, see the sections on $ECODE and $ETRAP in `Chapter 8: “Intrinsic Special Variables” <https://docs.yottadb.com/ProgrammersGuide/isv.html>`_. Because of the effect of $ECODE on the processing flow when $ETRAP is active, there is a benefit to including appropriate $ECODE maintenance in $ZTRAP related code, so that things stay well behaved when the two mechanisms are intermixed. Other differences are discussed in some of the examples.

++++++++++++++++++++
Break on an Error
++++++++++++++++++++

When $ZTRAP is set to a BREAK command and an error occurs, YottaDB puts the process into Direct Mode. The default for $ZTRAP is a BREAK command. When developing a program, $ZTRAP="BREAK" allows you to investigate the cause of the error from Direct Mode. For information on YottaDB debugging tools, see `Chapter 4: “Operating and Debugging in Direct Mode” <https://docs.yottadb.com/ProgrammersGuide/opdebug.html>`_.

Example:

.. parsed-literal::
   YDB>zprint ^EP1
   EP1    WRITE !,"THIS IS "_$TEXT(+0) 
          KILL A 
   BAD    WRITE A 
          WRITE !,"THIS IS NOT DISPLAYED" 
          QUIT 
                         
   YDB>do ^EP1
   THIS IS EP1%GTM-E-UNDEF, Undefined local variable: A
   At M source location BAD^EP1
   YDB>ZSHOW
   BAD^EP1    ($ZTRAP)
      (Direct mode) 
   +1^GTM$DMOD    (Direct mode) 
   YDB>QUIT
   YDB>ZSHOW
   EP1+1^EP1    (Direct mode) 
   +1^GTM$DMOD    (Direct mode) 
   YDB>

Because by default $ETRAP="" and $ZTRAP="B", this example does not explicitly set either $ETRAP or $ZTRAP. When the routine encounters an error at BAD^EP1, YottaDB initiates Direct Mode. The ZSHOW displays the M stack that has, at the bottom, the base Direct Mode frame and, at the top, EP1 with a notation that $ZTRAP has been invoked. The QUIT command at the prompt removes EP1 from the stack.

To prevent a program such as a production image from accessing Direct Mode, assign an action other than "BREAK" to $ETRAP or $ZTRAP. The following sections discuss various alternative values for $ETRAP or $ZTRAP.

In order to prevent inappropriate access to Direct Mode, eliminate all BREAKs from the production code. If the code contains BREAK commands, the commands should be subject to a postconditional flag that is only turned on for debugging. ZBREAK serves as an alternative debugging tool that effects only the current process and lasts only for the duration of an image activation.

++++++++++++++++++++++++++++++++++++++
Unconditional Transfer on an Error
++++++++++++++++++++++++++++++++++++++

The GOTO command instructs YottaDB to transfer execution permanently to another line within the routine or to another routine. When stopping to investigate an error is undesirable, use the GOTO command in $ETRAP or $ZTRAP to continue execution at some other point.

Example:

.. parsed-literal::
   YDB>ZPRINT ^EP2
   EP2     WRITE !,"THIS IS "_$TEXT(+0) 
           SET $ECODE=""        ;this affects only $ETRAP 
           SET $ETRAP="GOTO ET"        ;this implicitly stacks $ZTRAP 
           ;N $ZT S $ZT="GOTO ET"  ;would give a similar result 
           DO SUB1 
           WRITE !,"THIS IS THE END" 
           QUIT 
   SUB1    WRITE !,"THIS IS SUB1" 
           DO SUB2 
           QUIT 
   SUB2    WRITE !,"THIS IS SUB2" 
           KILL A 
   BAD     WRITE A 
           WRITE !,"THIS IS NOT DISPLAYED" 
           QUIT 
   ET      ;SET $ZTRAP=""         ;if using $ZTRAP to prevent recursion 
           WRITE !,"CONTINUING WITH ERROR TRAP AFTER AN ERROR" 
           WRITE !,"$STACK: ",$STACK 
           WRITE !,"$STACK(-1): ",$STACK(-1) 
           WRITE !,"$ZLEVEL: ",$ZLEVEL 
           FOR I=$STACK(-1):-1:1 DO 
           . WRITE !,"LEVEL: ",I 
           . SET K=10 
           . FOR J="PLACE","MCODE","ECODE" DO 
           . . WRITE ?K," ",J,": ",$STACK(I,J) 
           . . SET K=K+20 
           WRITE !,$ZSTATUS,!  
           ZSHOW "S" 
           SET $ECODE=""        ;this affects only $ETRAP 
           QUIT 
                                                                                                                                                                                                            
   YDB>do ^EP2
   THIS IS EP2
   THIS IS SUB1
   THIS IS SUB2
   CONTINUING WITH ERROR TRAP AFTER AN ERROR
   $STACK: 3
   $STACK(-1): 3
   $ZLEVEL: 4
   LEVEL: 3   PLACE: BAD^EP2      MCODE: BAD     WRITE A  ECODE: ,M6,Z150373850,
   LEVEL: 2   PLACE: SUB1+1^EP2   MCODE:         DO SUB2  ECODE: 
   LEVEL: 1   PLACE: EP2+4^EP2    MCODE:         DO SUB1  ECODE: 
   150373850,BAD^EP2,%GTM-E-UNDEF, Undefined local variable: A
   ET+12^EP2
   SUB1+1^EP2
   EP2+4^EP2
   +1^GTM$DMOD    (Direct mode) 
   THIS IS THE END
   YDB>

This routine specifies a GOTO command transferring execution to the ET label when an error occurs. The $ZLEVEL special variable contains an integer indicating the M stack level.

The ZGOTO command is similar to the GOTO command, however, the ZGOTO allows the removal of multiple levels from the program stack. ZGOTO can ensure that execution returns to a specific point, such as a menu.

Example:

.. parsed-literal::
   YDB>ZPRINT ^EP3
   EP3     ;
   MENU    WRITE !,"THIS IS MENU IN ",$TEXT(0) 
           SET $ECODE=""        ;this affects only $ETRAP 
           SET $ETRAP="SET $ECODE="""" ZGOTO 2" 
           ;N $ZT S $ZT="ZGOTO 2" ;would give a similar result 
           DO SUB1 
           WRITE !,"'MENU' AFTER $ETRAP" 
           WRITE !,"$STACK: ",$STACK 
           WRITE !,"$ZLEVEL: ",$ZLEVEL 
           QUIT 
   SUB1    WRITE !,"THIS IS SUB1" 
           DO SUB2 
           WRITE !,"THIS IS SKIPPED BY ZGOTO" 
           QUIT 
   SUB2    WRITE !,"THIS IS SUB2" 
           KILL A 
           BAD     WRITE A 
           WRITE !,"THIS IS NOT DISPLAYED" 
           QUIT 
                                                                                                                    
   YDB>do ^EP3
   THIS IS MENU IN 
   THIS IS SUB1
   THIS IS SUB2
   'MENU' AFTER $ETRAP
   $STACK: 1
   $ZLEVEL: 2

This routine instructs YottaDB to reset the execution to level 2 if it encounters an error. YottaDB removes all intermediate levels.

In general, coding ZGOTO level information based on $ZLEVEL provides a more robust technique than the "hard-coding" shown in the previous example.

Example:

.. parsed-literal::
   YDB>ZPRINT ^EP4
   EP4     WRITE !,"THIS IS "_$TEXT(+0) 
           SET $ECODE=""        ;this affects only $ETRAP 
           DO MAIN 
           WRITE !,"THIS IS ",$TEXT(+0)," AFTER THE ERROR" 
           WRITE !,"$ZLEVEL: ",$ZLEVEL 
           QUIT 
   MAIN    WRITE !,"THIS IS MAIN" 
           WRITE !,"$ZLEVEL: ",$ZLEVEL 
           SET $ETRAP="ZGOTO "_$ZLEVEL\_":ET" 
           ;N $ZT S $ZT="ZGOTO "_$ZLEVEL\_":ET ;alternative 
           DO SUB1 
           QUIT 
   SUB1    WRITE !,"THIS IS SUB1" 
           WRITE !,"$ZLEVEL: ",$ZLEVEL 
           DO SUB2 
           QUIT 
   SUB2    WRITE !,"THIS IS SUB2" 
           WRITE !,"$ZLEVEL :",$ZLEVEL 
           KILL A 
   BAD     WRITE A 
           WRITE !,"THIS IS NOT DISPLAYED" 
           QUIT 
   ET     ;SET $ZTRAP="" ;if using $ZTRAP to prevent recursion 
           WRITE !,"CONTINUING WITH ERROR TRAP AFTER AN ERROR" 
           WRITE !,"$STACK: ",$STACK 
           WRITE !,"$STACK(-1): ",$STACK(-1) 
           WRITE !,"$ZLEVEL: ",$ZLEVEL 
           FOR I=$STACK(-1):-1:1 DO 
           . WRITE !,"LEVEL: ",I 
           . SET K=10 
           . FOR J="PLACE","MCODE","ECODE" DO 
           . . WRITE ?K," ",J,": ",$STACK(I,J) 
           . . SET K=K+20 
           WRITE !,$ZSTATUS,!
           ZSHOW "S" 
           SET ECODE=""        ;this affects only $ETRAP 
           QUIT 
                                                                                                                                                                                                                                                            
   YDB>do ^EP4
   THIS IS EP4
   THIS IS MAIN
   $ZLEVEL: 3
   THIS IS SUB1
   $ZLEVEL: 4
   THIS IS SUB2
   $ZLEVEL :5
   CONTINUING WITH ERROR TRAP AFTER AN ERROR
   $STACK: 2
   $STACK(-1): 4
   $ZLEVEL: 3
   LEVEL: 4   PLACE: BAD^EP4      MCODE: BAD     WRITE A  ECODE: ,M6,Z150373850,
   LEVEL: 3   PLACE: SUB1+2^EP4   MCODE:         DO SUB2  ECODE: 
   LEVEL: 2   PLACE: MAIN+4^EP4   MCODE:         DO SUB1  ECODE: 
   LEVEL: 1   PLACE: EP4+2^EP4    MCODE:         DO MAIN  ECODE: 
   150373850,BAD^EP4,%GTM-E-UNDEF, Undefined local variable: A
   ET+12^EP4
   EP4+2^EP4
   +1^GTM$DMOD    (Direct mode) 
   THIS IS EP4 AFTER THE ERROR
   $ZLEVEL: 2
   YDB>

This routine sets $ETRAP or $ZTRAP to a ZGOTO specifying the current level. When the routine encounters an error at label BAD, YottaDB switches control to label ET at the level where $ETRAP (or $ZTRAP) was established. At this point in the execution, ET replaces SUB1+2^EP4 as the program stack entry for the level specified, that is, $ZLEVEL=3. The QUIT command then returns control to the level where $ZLEVEL=2.

++++++++++++++++++++++++++++++
Setting $ZTRAP for each level
++++++++++++++++++++++++++++++

The command NEW $ETRAP or NEW $ZTRAP stacks the current value of $ETRAP or $ZTRAP respectively and, in the case of $ZTRAP, sets the value equal to the empty string. Normally, a SET $ETRAP or $ZTRAP immediately follows a NEW $ETRAP or $ZTRAP. When YottaDB encounters a QUIT command that leaves a level where $ETRAP or $ZTRAP had been NEWed, YottaDB deletes any value set to the ISV after the NEW command and restores the value that the ISV held previous to the NEW. NEW $ETRAP or $ZTRAP enables the construction of error handlers corresponding to the nesting of routines. A SET $ETRAP or $ZTRAP implicitly NEWs the other variable if it does not already have the value of the empty string. This enables the interleaving of $ETRAP and $ZTRAP at different levels, although (as mentioned above) such interleaving requires that $ZTRAP handlers deal appropriately with $ECODE.

Example:

.. parsed-literal::
   YDB>ZPRINT ^EP5
   EP5     WRITE !,"THIS IS "_$TEXT(+0)
           SET $ECODE="";this affects only $ETRAP
           WRITE !,"STARTING $ETRAP: ",$ETRAP
           WRITE !,"STARTING $ZTRAP: ",$ZTRAP
           DO SUB1
           WRITE !,"ENDING $ETRAP: ",$ETRAP
           WRITE !,"ENDING $ZTRAP: ",$ZTRAP
           QUIT
   MAIN    WRITE !,"THIS IS MAIN"
           WRITE !,"$ZLEVEL: ",$ZLEVEL
           DO SUB1
           QUIT
   SUB1    WRITE !,"THIS IS SUB1"
           NEW $ETRAP SET $ETRAP="GOTO ET1"
           ;NEW $ZTRAP SET $ZTRAP="GOTO ET1" ;alternative
           WRITE !,"$ETRAP FOR SUB1: ",$ETRAP
           KILL A
   BAD     WRITE A
           WRITE !,"THIS IS NOT DISPLAYED"
           QUIT
   ET1     WRITE !,"ERROR TRAP 1"
           WRITE !,"$ETRAP AFTER THE TRAP: ",$ETRAP
           WRITE !,"$ZTRAP AFTER THE TRAP: ",$ZTRAP
           SET $ECODE="";this affects only $ETRAP
           QUIT
                                                                                                                                                                    
   YDB>do ^EP5
   THIS IS EP5
   STARTING $ETRAP: 
   STARTING $ZTRAP: B
   THIS IS SUB1
   $ETRAP FOR SUB1: GOTO ET1
   ERROR TRAP 1
   $ETRAP AFTER THE TRAP: GOTO ET1
   $ZTRAP AFTER THE TRAP: 
   ENDING $ETRAP: 
   ENDING $ZTRAP: B
   YDB>

At SUB1, this routine NEWs $ETRAP and assigns it a value, which implicitly NEWs $ZTRAP. When the routine encounters an error at the SUB1 level, YottaDB transfers control to label ET1 without modifying the value of $ETRAP or $ZTRAP. When the routine encounters a QUIT command in routine ET1, YottaDB transfers control to the command after the DO that invoked ET1 and restores $ETRAP or $ZTRAP to the values they held before the NEW and the SET. 

.. note::
   If the transfer to ET1 was accomplished with a ZGOTO that reduced the stack level, after the trap, $ETRAP would have the value of the empty string and $ZTRAP would be "B".

++++++++++++++++++++++++++++
Nested Error Handling
++++++++++++++++++++++++++++

$ETRAP or $ZTRAP set to a DO command instructs YottaDB to transfer execution temporarily to another line within this or another routine when it encounters an error. A QUIT command within the scope of the DO transfers control back to the code specified by the $ETRAP or $ZTRAP. When the code in the ISV terminates due to an explicit or implicit QUIT, the behavior of $ETRAP and $ZTRAP is different. When $ETRAP is in control, the level at which the error occurred is removed, and control returns to the invoking level. When $ZTRAP contains code, execution picks up at the beginning of the line with the error. A DO command within $ZTRAP is normally used for I/O errors that an operator may resolve, because a DO command permits re-execution of the line containing the error.

Example:

.. parsed-literal::
   YDB>ZPRINT ^EP6
   EP6     WRITE !,"THIS IS "_$TEXT(+0)
           NEW
           NEW $ZTRAP SET $ZTRAP="DO ET"
           SET (CB,CE)=0
   BAD     SET CB=CB+1 WRITE A SET CE=CE+1
           WRITE !,"AFTER SUCCESSFUL EXECUTION OF BAD:",!
           SET A="A IS NOT DEFINED"
           ZWRITE
           QUIT
   ET      W !,"CONTINUING WITH ERROR TRAP AFTER AN ERROR",!
           ZWRITE
           SET A="A IS NOW DEFINED"
           YDB>do ^EP6
           THIS IS EP6
           CONTINUING WITH ERROR TRAP AFTER AN ERROR
           CB=1
           CE=0
           A IS NOW DEFINED
           AFTER SUCCESSFUL EXECUTION OF BAD:
           A="A IS NOT DEFINED"
           CB=2
           CE=1
           YDB>

This example sets $ZTRAP to a DO command. When the routine encounters an error in the middle of the line at label BAD, YottaDB transfers control to label ET. After QUITting from routine ET, YottaDB returns control to the beginning of the line at label BAD.

Example:

.. parsed-literal::
   YDB>ZPRINT ^EP6A
   EP6A    WRITE !,"THIS IS "_$TEXT(+0) 
           NEW 
           NEW $ETRAP SET $ETRAP="GOTO ET" 
           SET (CB,CE)=0 
   BAD     SET CB=CB+1 WRITE A SET CE=CE+1 
           WRITE !,"AFTER SUCCESSFUL EXECUTION OF BAD:",! 
           ZWRITE 
           QUIT 
   ET      W !,"CONTINUING WITH ERROR TRAP AFTER AN ERROR",! 
           ZWRITE 
           SET A="A IS NOW DEFINED" 
           SET RETRY=$STACK($STACK,"PLACE") 
           SET $ECODE="" 
           GOTO @RETRY 
                                                                           
   YDB>DO ^EP6A
   THIS IS EP6A
   CONTINUING WITH ERROR TRAP AFTER AN ERROR
   CB=1
   CE=0
   A IS NOW DEFINED
   AFTER SUCCESSFUL EXECUTION OF BAD:
   A="A IS NOW DEFINED"
   CB=2
   CE=1
   RETRY="BAD^EP6A"
   YDB> 

This routine is an example of how $ETRAP handling can be coded to perform the same kind of resumtion of the original execution stream that occurs by default with $ZTRAP when there is no unconditional transfer of control.

+++++++++++++++++++++++++++++++++
Terminating Execution on an Error
+++++++++++++++++++++++++++++++++

If both $ETRAP and $ZTRAP are set to the empty string upon encountering an error, the current level is discarded and the error is reissued at the invoking level. When already at the lowest M stack level, YottaDB terminates routine execution and returns control to the shell level. If $ZTRAP is used exclusively, $ZTRAP="" suppresses the unstacking of NEWed values of $ZTRAP associated with lower levels. $ETRAP values are always unstacked, however if the lowest level $ETRAP is the empty string (which it is by default when YottaDB starts), YottaDB performs the same termination as it does with $ZTRAP. These terminations with both ISVs empty provides a mechanism for returning to the shell with a status message when YottaDB encounters an error.

Example:

.. parsed-literal::
   YDB>ZPRINT ^EP7
   EP7     WRITE !,"THIS IS ",$TEXT(+0)
           SET $ECODE="";this only affects $ETRAP
           SET $ETRAP="",$ZTRAP=""
           KILL A
   BAD     WRITE A
           WRITE !,"THIS IS NOT DISPLAYED"
           QUIT
                                            
   YDB>do ^EP7
   THIS IS EP7
   %GTM-E-UNDEF, Undefined local variable: A
   %GTM-I-RTSLOC, At M source location BAD^EP7
   $

YottaDB issues a message describing the M error and releases control to the shell.

When the action specified by $ZTRAP results in another run-time error before changing the value of $ZTRAP, the routine may iteratively invoke $ZTRAP until a stack overflow terminates the YottaDB image. SETting $ZTRAP="" at the beginning of error processing ensures that this type of infinite loop does not occur. Because $ETRAP is implicitly followed by a QUIT it does not have the tendency to recurse. While $ETRAP is resistant to recursion, it is not completely immune, because a GOTO or a ZGOTO within the same level can evade the implicit QUIT. $ETRAP error handling involving errors on more than one stack level can also be induced to recurse if $ECODE is inappropriately cleared before the errors at all levels have been properly dealt with.

Example:

.. parsed-literal::
   YDB>ZPRINT ^EP8
   EP8     WRITE !,"THIS IS ",$TEXT(+0)
           NEW $ZTRAP SET $ZTRAP="DO ET"
           KILL A
   BAD     WRITE A
           WRITE !,"THIS IS NOT DISPLAYED"
           QUIT
   ET      WRITE 2/0
           QUIT
                                           
   YDB>DO ^EP8
   THIS IS EP8
   %GTM-E-STACKCRIT, Stack space critical
   %GTM-E-ERRWZTRAP, Error while processing $ZTRAP
   YDB>

When the routine encounters an error at label BAD, YottaDB transfers control to label ET. When the routine encounters an error at label ET, it recursively does ET until a stack overflow condition terminates the YottaDB image.

A set $ZTRAP="" command as soon as the program enters an error-handling routine prevents this type of "infinite" recursion.

.. parsed-literal::
   YDB>zprint ^EP8A
   EP8A    WRITE !,"THIS IS ",$TEXT(+0)
           SET $ECODE=""
           SET $ZTRAP="",$ETRAP="DO ET"
           KILL A
   BAD     WRITE A
           WRITE !,"THIS IS NOT DISPLAYED"
           QUIT
   ET      WRITE !,"CONTINUING WITH ERROR TRAP AFTER AN ERROR"
           ZSHOW "S"
           WRITE !,"HERE COMES AN ERROR IN THE TRAP CODE"
           WRITE 2/0
           QUIT
                                                                            
   YDB>DO ^EP8A
   THIS IS EP8A
   CONTINUING WITH ERROR TRAP AFTER AN ERRORET+1^EP8A
   BAD^EP8A    ($ZTRAP)
   +1^GTM$DMOD    (Direct mode) 
   HERE COMES AN ERROR IN THE TRAP CODE
   %GTM-E-DIVZERO, Attempt to divide by zero
   YDB>

This demonstrates how $ETRAP behavior in this circumstance is more appropriate. Note that the $ZTRAP="" at the lowest level, prevents exection from returning to Direct Mode when the initial value of $ZTRAP ("B") is unstacked; this step takes $ZTRAP out of the equation and should be part of initialization when the intention is to use $ETRAP exclusively.

Example:

.. parsed-literal::
   YDB>ZPRINT ^EP9
   EP9     WRITE !,"THIS IS ",$TEXT(+0)
           SET $ZTRAP="DO ET"
           KILL A
   BAD     WRITE A
           WRITE !,"THIS IS NOT DISPLAYED"
           QUIT
   ET      SET $ZT=""
           WRITE !,"THIS IS THE ERROR TRAP"
           ERROR   WRITE !,"HERE COMES AN ERROR IN THE ERROR TRAP"
           WRITE 2/0
           QUIT
                                                            
   YDB>DO ^EP9
   THIS IS EP9
   THIS IS THE ERROR TRAP
   HERE COMES AN ERROR IN THE ERROR TRAP
   %GTM-E-DIVZERO, Attempt to divide by zero
   %GTM-I-RTSLOC,                 At M source location ERROR+1^EP9
   $

This routine sets the value of $ZTRAP to null as soon as the program enters the error handler. This insures program termination when an error occurs in the error handler. 

++++++++++++++++++++++++++++++++
Setting $ZTRAP to Other Actions
++++++++++++++++++++++++++++++++

The QUIT, HALT and ZHALT commands also serve as useful $ETRAP or $ZTRAP actions.

The QUIT command terminates execution at that invocation level.

Example:

.. parsed-literal::
   YDB>zprint ^EP10
   EP10    WRITE !,"THIS IS ",$TEXT(+0)
           SET $ECODE="";this affects only $ETRAP
           S $ET="S $EC="""" Q" ;this implicitly stacks $ZTRAP
           ;N $ZT S $ZT="QUIT" ;would give a similar result
           DO SUB1
           QUIT
   SUB1    WRITE !,"THIS IS SUB1"
           DO SUB2
           WRITE !,"THIS IS SUB1 AFTER THE ERROR WAS 'IGNORED'"
           QUIT
   SUB2    WRITE !,"THIS IS SUB2"
           KILL A
           BAD     WRITE A
           WRITE !,"THIS IS NOT DISPLAYED"
           QUIT
                                                                                            
   YDB>do ^EP10
   THIS IS EP10
   THIS IS SUB1
   THIS IS SUB2
   THIS IS SUB1 AFTER THE ERROR WAS 'IGNORED'
   YDB>

This routine sets $ETRAP or $ZTRAP to the QUIT command. When the routine encounters an error at label BAD, YottaDB executes the active error handling ISV. The QUIT command terminates execution of SUB2 and transfers execution back to SUB1. The WRITE displays the error message using the $ZSTATUS special variable. Because the default behavior is to QUIT after $ETRAP code completes, this technique is mostly useful with $ETRAP as a place holder to avoid the $ETRAP="" semantics when there is no action to take at the current level. With $ZTRAP, where the default behavior is to resume execution at the beginning the line that triggered the error, the QUIT is more than a placeholder.

The HALT command terminates routine execution and returns control to the shell level. Setting $ETRAP="HALT" or $ZTRAP="HALT" is similar to setting the ISV to the empty string except that the "HALT" code does not pass the error condition code back to the shell. After a HALT, $? contains zero (0).

ZHALT acts like HALT but takes and argument, which YottaDB passes back to the OS shell. Note that UNIX shells typically limit return codes to a byte, so they may truncate the value of the ZHALT argument.

Example:

.. parsed-literal::
   YDB>ZPRINT ^EP11
   EP11    WRITE !,"THIS IS ",$TEXT(+0)
           SET $ECODE="";this affects only $ETRAP
           SET $ETRAP="HALT";this implicitly stacks $ZTRAP
           ;SET $ZTRAP="HALT";would give a similar result
           KILL A
   BAD     WRITE !,A
           WRITE !,"THIS IS NOT DISPLAYED"
           QUIT
                                                           
   YDB>DO ^EP11
   THIS IS EP11
   $ 


++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Summary of $ETRAP & $ZTRAP Error-Handling Options
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

+----------------------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Error Handling Feature                 | Description and Possible Uses                                                                                                                                                  |
+========================================+================================================================================================================================================================================+
| $ETRAP="BREAK" or $ZTRAP="BREAK"       | Returns to Direct Mode upon encountering an error that enables interactive debugging to determine the nature of the error.                                                     |
+----------------------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ETRAP="GOTO" or $ZTRAP="GOTO"         | Transfers control upon encountering an error and allows for continuation of execution after the error. Use with an error handling routine that may record or report an error.  |
+----------------------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ETRAP="ZGOTO" or $ZTRAP="ZGOTO"       | Similar to GOTO, but additionally allows for removal of levels from the stack. Use to allow recovery to specific point, such as a menu.                                        |
+----------------------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| NEW $ETRAP or NEW %ZTRAP               | NEW $ETRAP stacks the old value but does not change the current value, while NEW $ZTRAP stacks the old value and sets the current value to the empty string. Usually followed  |
|                                        | by a SET $ETRAP or SET $ZTRAP. After a QUIT from a given level, YottaDB restores the value held prior to the NEW. Use to enable different methods of error handling at         |
|                                        | different levels within an application.                                                                                                                                        |
+----------------------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ETRAP="DO..."                         | Transfers execution temporarily to another label upon encountering an error. After return from a DO, YottaDB QUITs from the stack level at which the error occured.            |
|                                        | Whether control returns to the invoking code or to the trap handler at the less nested level, depends on the value of $ECODE.                                                  |
+----------------------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZTRAP="DO..."                         | Transfers execution temporarily to another label upon encountering an error. When YottaDB returns from a DO and completes the $ZTRAP action, execution continues at the        |
|                                        | beginning of the line containing the error and re-executes the entire line containing the error. Use with I/O device errors where operator may intervene to correct the error  |
|                                        | condition.                                                                                                                                                                     |
+----------------------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZTRAP=""                              | Returns to shell with the Status Code and terminates execution. If SET in error handling routines, prevents infinite loops. Prevents access to Direct Mode. Use in production  |
|                                        | code when the invoking shell needs to test $?.                                                                                                                                 |
+----------------------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ETRAP="SET $ECODE=""""" or $ZTRAP="QUI| Terminates execution at that level upon encountering an error, and returns to the invocation level at the point immediately following the invocation. Use to ignore errors on a|
| T""                                    | particular level and continue executing.                                                                                                                                       |
+----------------------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZTRAP="HALT"                          | Returns to the shell as if normal termination occurred. Avoids access to Direct Mode. Use in production code when the invoking shell does not need to examine the exit status  |
|                                        | of the YottaDB process.                                                                                                                                                        |
+----------------------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+

++++++++++++++++++++++++++++
Errors in $ZTRAP
++++++++++++++++++++++++++++

If $ZTRAP contains invalid source code, YottaDB displays an error message and puts the process into Direct Mode.

If the action specified by $ZTRAP results in another run-time error before changing the value of $ZTRAP, it may result in a loop that iteratively invokes $ZTRAP until a stack overflow terminates the YottaDB image. Keep $ZTRAP simple and take special care to debug exception handling. 

.. note::
   An error in $ETRAP code triggers an implicit TROLLBACK:$TLEVEL QUIT:$QUIT "" QUIT.

+++++++++++++++++++++++++++++++++++
Recording Information about Errors
+++++++++++++++++++++++++++++++++++

YottaDB provides a number of standard features and extensions to examine and record information about an error condition.

The extensions are:

* ZSHOW
* ZWRITE
* $ECODE
* $STACK
* $STACK()
* $ZSTATUS
* $ZJOBEXAM()
* $ZLEVEL

The ZSHOW command displays information about the current M environment. A ZSHOW argument may contain an expression that contains codes selecting one or more types of information for output.

A: selects auto-relink information

B: selects ZBREAK information

C: provides the list of loaded external call packages and their routines. ZSHOW "C" does not report packages that are accessible but have not been accessed by the process.

D: selects open device information

G: selects global statistic information

I: selects intrinsic special variables

L: selects locks held by the process

R: selects the M stack but with routine hashes

S: selects the M stack

V: selects local variables

\*: selects all possible ZSHOW information except A.

A ZSHOW with no argument displays the M stack on the current device. It lists the program stack from initiation to the current execution level.

The ZWRITE command prints the current value of defined variables. ZWRITE provides a tool for examining or saving variable context. ZWRITE and ZSHOW can only display the current local variables, not any local variable states that have been protected by NEW commands, or appearance in an invoked formallist. A WRITE may also display current global variables.

The $ECODE special variable contains a M standardized/user defined/YottaDB specific error code.

The $STACK special variable contains the current level of M execution stack depth.

The $STACK() function returns strings describing aspects of the execution environment.

The $ZLEVEL special variable maintains an integer that indicates the level of nesting of DO and XECUTE commands. $ZLEVEL always contains an integer count of the number of levels displayed by issuing a ZSHOW "S" in that context.

The $ZJOBEXAM() function returns a string indicating the full path to the file where it stored a process context dump.

The $ZSTATUS special variable records the error condition code and location of the last error condition during execution.

For I/O operations, YottaDB uses the $ZA, $ZB and $ZEOF special variables. $ZA contains a status determined by the last read on the current device.

To simplify record keeping, an application may set $ZTRAP to an error-handling routine that records information about an error. The next section provides an example of a routine ERR.m that does this. 

**Program to Record Information on an Error using $ZTRAP**

.. parsed-literal::
   YDB>ZPRINT ^ERR
   ERR0;;RECORD CONTECT OF AN ERROR
   ;
   RECORD  SET $ZTRAP="GOTO OPEN"
           ZSHOW "*":^ERR($J,$H)
           GOTO LOOPT;$H might change
   LOOPV   ZSHOW "V":^ERR($J,$H,"VL",$ZLEVEL)
   LOOPT   IF $ZLEVEL>1 ZGOTO $ZLEVEL-1:LOOPV
   STACK   SET $ZTRAP="GOTO WARN"
           SET %ERRVH=$H;can cause error if memory low
           SET ^ERR($J,%ERRVH,"$STACK")=$STACK
           SET ^ERR($J,%ERRVH,"$STACK",-1)=$STACK(-1)
           FOR %ERRVI=$STACK(-1):-1:1 DO
           . SET %ERRVK=""
           . FOR %ERRVJ="PLACE","MCODE","ECODE" DO
           . . SET %ERRVK=%ERRVK_$STACK(%ERRVI,%ERRVJ)_"\|~\|"
           . SET ^ERR($J,%ERRVH,"$STACK",%ERRVI)=%ERRVK
           GOTO WARN
   OPEN    SET $ZTRAP="GOTO OPEN1"
           SET %ERRIO=$IO,%ERRZA=$ZA,%ERRZB=$ZB,%ERRZE=$ZEOF
           SET %ERRVF="REC.ERR"
           SET %ERRVF=$ZDATE($H,"YEARMMDD2460SS")_"_"_$J_".ERR"
           OPEN %ERRVF:NEWVERSION
           USE %ERRVF
           S $ZT="S $ZT="" G WARN"" U $P:(NOCENA:CTRAP="""") G STAC"
           ZSHOW "*"
           KILL %ERRVF,%ERRIO,%ERRZA,%ERRZB,%ERRZE
           GOTO LOOPU
   LOOPF   WRITE !,"LOCAL VARIABLES FOR ZLEVEL: ",$ZLEVEL,!
           ZWRITE
   LOOPU   IF $ZLEVEL>1 ZGOTO $ZLEVEL-1:LOOPF
           WRITE !
   STAC    SET $ZTRAP="GOTO WARN"
           WRITE !,"PROGRAM STACK: ",!
           WRITE !,"$STACK: ",$STACK,!
           WRITE !,"$STACK(-1): ",$STACK(-1),!
           FOR %ERRVI=$STACK(-1):-1:1 DO
           . WRITE !,"LEVEL: ",%ERRVI
           . SET %ERRVK=10
           . FOR %ERRVJ="PLACE","MCODE","ECODE" DO
           .. W ?%ERRVK,"",%ERRVJ,":",$STACK(%ERRVI,%ERRVJ)
           .. SET %ERRVK=%ERRVK+20
           CLOSE $IO
   WARN    SET $ZTRAP="GOTO FATAL"
           IF $P=$I SET %ERRIO=$IO,%ERRZA,%ERRZB=$ZB,%ERRZE=$ZEOF
           USE $P:(NOCENABLE:CTRAP="":EXCEPTION="")
           WRITE !,"YOU HAVE ENCOUNTERED AN ERROR"
           WRITE !,"PLEASE NOTIFY JOAN Q SUPPORT PERSON",!
   FATAL   SET $ZTRAP=""
           ZM +$P($ST($ST(-1),"ECODE"),"Z",2)

The routine sets $ZTRAP to a sequence of values so that, in the event of an error various fallback actions are taken. If a STACKCRIT error occurs, YottaDB makes a small amount of space for error handling. However, if the error handler uses up significant amounts of space by nesting routines or manupulating local variables, the error handler may cause another STACKCRIT error. In this case, it is possible for the error handling to loop endlessly, therefore this routine changes $ZTRAP so that each error moves the routine closer to completion.

First it attempts to store the context information in the global ^ERR. The LOOPV-LOOPT code records the invocation levels using the ZSHOW command. This technique addresses the situation where the application program defines or NEWs local variables for each level. The code executes a pass through the loop for each instance where the value of $ZLEVEL is greater than one (1). For each pass, ERR.M decrements the value of $ZLEVEL with the ZGOTO. When the value of $ZLEVEL reaches one (1), the code at, and following, the STACK label stores the error context available in the $STACK() function.

If there is a problem with storing any of this information, ^ERR attempts to store the context information in a file in the current default working directory. If it uses a file, in order to (at the label OPEN), record information about I/O operations, on the current device at the time of the error, the error handler SETs local variables to the values of the device specific I/O special variables $IO, $ZA, $ZB and $ZEOF before opening the log file.

The routine OPENs the log file with a name made up of the date and $JOB of the process. The NEWVERSION deviceparameter instructs YottaDB to create a new version of the file. The LOOPF-LOOPU code records the invocation levels using the ZWRITE command in a manner analogous to that described above. If an error occurs trying to write to the file, $ZTRAP USEs the principal device and transfers control to the STAC label in an attempt to provide a minimal error context on the user terminal. The code at and following the STAC label records the error context available in the $STACK() function.

At the label WARN, the routine attempts to notify the user that an error has occurred and who to notify.

At the label FATAL, the ZMESSAGE command resignals the error. Because (with proper setup) $ETRAP and $ZTRAP are now null, YottaDB releases control of the process to the host shell. In this example, the user never has access to Direct Mode.

Example:

.. parsed-literal::
   YDB>zprint ^EP13
   EP13    WRITE !,"THIS IS ",$TEXT(+0)
           SET $ZTRAP="GOTO NODB"
           KILL ^ERR
   NODB    SET $ECODE="";this affects only $ETRAP
           ;S $ET="GOTO ^ERR";this implicitly stacks $ZTRAP
           N $ZT S $ZT="GOTO ^ERR" ;gives similar result
           DO SUB1
           WRITE !,"THIS IS THE END"
           QUIT
   SUB1    WRITE !,"THIS IS SUB1"
           NEW
           SET (A,B,C)=$ZLEVEL
           DO SUB2
           QUIT
   SUB2    WRITE !,"THIS IS SUB2"
           NEW
           SET (B,C,D)=$ZLEVEL
           DO SUB3
           QUIT
           SUB3    WRITE !,"THIS IS SUB3"
           NEW
           SET (A,C,D)=$ZLEVEL
           DO BAD
           BAD     NEW (A)
           SET B="BAD"
           WRITE 1/0
           WRITE !,"THIS IS NOT DISPLAYED"
           QUIT

   YDB>do ^EP13
   THIS IS EP13
   THIS IS SUB1
   THIS IS SUB2
   THIS IS SUB3
   PROGRAM STACK: 
   $STACK: 5
   $STACK(-1): 5
   LEVEL: 5  PLACE:BAD+2^EP13    MCODE: WRITE 1/0    ECODE:,M9,Z150373210,
   LEVEL: 4  PLACE:SUB3+3^EP13   MCODE: DO BAD       ECODE:
   LEVEL: 3  PLACE:SUB2+3^EP13   MCODE: DO SUB3      ECODE:
   LEVEL: 2  PLACE:SUB1+3^EP13   MCODE: DO SUB2      ECODE:
   LEVEL: 1  PLACE:NODB+3^EP13   MCODE: DO SUB1      ECODE:
   YOU HAVE ENCOUNTERED AN ERROR
   PLEASE NOTIFY JOAN Q SUPPORT PERSON
   %GTM-E-DIVZERO, Attempt to divide by zero
   %GTM-I-RTSLOC,                 At M source location FATAL+1^ERR


Example EP13 uses the error recording routine by setting $ZTRAP="GOTO ^ERR". When the routine encounters an error at label BAD, YottaDB transfers control to routine ERR. Afterwards the .ERR file would have contents like:

.. parsed-literal::
   YDB>zwrite ^ERR
   ^ERR(4806,"62364,27842","D",1)="/dev/pts/8 OPEN TERMINAL NOPAST NOESCA NOREADS T
             YPE WIDTH=80 LENG=22 EDIT "
   ^ERR(4806,"62364,27842","G",0)="GLD:*,REG:*,SET:68,KIL:3,GET:0,DTA:0,ORD:0,ZPR:0
             ,QRY:0,LKS:0,LKF:0,CTN:0,DRD:3,DWT:0,NTW:68,NTR:6,NBW:71,NBR:154,NR0:0
             ,NR1:0,NR2:0,NR3:0,TTW:0,TTR:0,TRB:0,TBW:0,TBR:0,TR0:0,TR1:0,TR2:0,TR3
             :0,TR4:0,TC0:0,TC1:0,TC2:0,TC3:0,TC4:0,ZTR:0"
   ^ERR(4806,"62364,27842","G",1)="GLD:/home/jdoe/.fis-gtm/V5.4-002B_x86/g/gtm.gld
             ,REG:DEFAULT,SET:69,KIL:4,GET:0,DTA:0,ORD:0,ZPR:0,QRY:0,LKS:0,LKF:0,CT
              N:69,DRD:3,DWT:0,NTW:69,NTR:7,NBW:72,NBR:160,NR0:0,NR1:0,NR2:0,NR3:0,T
              TW:0,TTR:0,TRB:0,TBW:0,TBR:0,TR0:0,TR1:0,TR2:0,TR3:0,TR4:0,TC0:0,TC1:0
             ,TC2:0,TC3:0,TC4:0,ZTR:0"
   ^ERR(4806,"62364,27842","I",1)="$DEVICE="""""
   ^ERR(4806,"62364,27842","I",2)="$ECODE="",M9,Z150373210,"""
   ^ERR(4806,"62364,27842","I",3)="$ESTACK=5"
   ^ERR(4806,"62364,27842","I",4)="$ETRAP="""""
   ^ERR(4806,"62364,27842","I",5)="$HOROLOG=""62364,27842"""
   ^ERR(4806,"62364,27842","I",6)="$IO=""/dev/pts/8"""
   ^ERR(4806,"62364,27842","I",7)="$JOB=4806"
   ^ERR(4806,"62364,27842","I",8)="$KEY="""""
   ^ERR(4806,"62364,27842","I",9)="$PRINCIPAL=""/dev/pts/8"""
   ^ERR(4806,"62364,27842","I",10)="$QUIT=0"
   ^ERR(4806,"62364,27842","I",11)="$REFERENCE=""^ERR(4806,""""62364,27842"""",""""
             I"""",10)"""
   ^ERR(4806,"62364,27842","I",12)="$STACK=5"
   ^ERR(4806,"62364,27842","I",13)="$STORAGE=2147483647"
   ^ERR(4806,"62364,27842","I",14)="$SYSTEM=""47,gtm_sysid"""
   ^ERR(4806,"62364,27842","I",15)="$TEST=1"
   ^ERR(4806,"62364,27842","I",16)="$TLEVEL=0"
   ^ERR(4806,"62364,27842","I",17)="$TRESTART=0"
   ^ERR(4806,"62364,27842","I",18)="$X=12"
   ^ERR(4806,"62364,27842","I",19)="$Y=21"
   ^ERR(4806,"62364,27842","I",20)="$ZA=0"
   ^ERR(4806,"62364,27842","I",21)="$ZALLOCSTOR=893732"
   ^ERR(4806,"62364,27842","I",22)="$ZB="""""
   ^ERR(4806,"62364,27842","I",23)="$ZCHSET=""M"""
   ^ERR(4806,"62364,27842","I",24)="$ZCMDLINE="""""
   ^ERR(4806,"62364,27842","I",25)="$ZCOMPILE="""""
   ^ERR(4806,"62364,27842","I",26)="$ZCSTATUS=0"
   ^ERR(4806,"62364,27842","I",27)="$ZDATEFORM=0"
   ^ERR(4806,"62364,27842","I",28)="$ZDIRECTORY=""/home/jdoe/"""
   ^ERR(4806,"62364,27842","I",29)="$ZEDITOR=0"
   ^ERR(4806,"62364,27842","I",30)="$ZEOF=0"
   ^ERR(4806,"62364,27842","I",31)="$ZERROR=""Unprocessed $ZERROR, see $ZSTATUS"""
   ^ERR(4806,"62364,27842","I",32)="$ZGBLDIR=""/home/jdoe/.fis-gtm/V5.4-002B_x86/g
             /gtm.gld"""
   ^ERR(4806,"62364,27842","I",33)="$ZININTERRUPT=0"
   ^ERR(4806,"62364,27842","I",34)="$ZINTERRUPT=""IF $ZJOBEXAM()"""
   ^ERR(4806,"62364,27842","I",35)="$ZIO=""/dev/pts/8"""
   ^ERR(4806,"62364,27842","I",36)="$ZJOB=0"
   ^ERR(4806,"62364,27842","I",37)="$ZLEVEL=6"
   ^ERR(4806,"62364,27842","I",38)="$ZMAXTPTIME=0"
   ^ERR(4806,"62364,27842","I",39)="$ZMODE=""INTERACTIVE"""
   ^ERR(4806,"62364,27842","I",40)="$ZPATNUMERIC=""M"""
   ^ERR(4806,"62364,27842","I",41)="$ZPOSITION=""RECORD+1^ERR"""
   ^ERR(4806,"62364,27842","I",42)="$ZPROCESS="""""
   ^ERR(4806,"62364,27842","I",43)="$ZPROMPT=""YDB>"""
   ^ERR(4806,"62364,27842","I",44)="$ZQUIT=0"
   ^ERR(4806,"62364,27842","I",45)="$ZREALSTOR=898568"
   ^ERR(4806,"62364,27842","I",46)="$ZROUTINES=""/home/jdoe/.fis-gtm/V5.4-002B_x86
             /o(/home/jdoe/.fis-gtm/V5.4-002B_x86/r /home/jdoe/.fis-gtm/r) /usr/l
             ib/fis-gtm/V5.4-002B_x86"""
   ^ERR(4806,"62364,27842","I",47)="$ZSOURCE="""""
   ^ERR(4806,"62364,27842","I",48)="$ZSTATUS=""150373210,BAD+2^EP13,%GTM-E-DIVZERO,
              Attempt to divide by zero"""
   ^ERR(4806,"62364,27842","I",49)="$ZSTEP=""B"""
   ^ERR(4806,"62364,27842","I",50)="$ZSYSTEM=0"
   ^ERR(4806,"62364,27842","I",51)="$ZTNAME="""""
   ^ERR(4806,"62364,27842","I",52)="$ZTDATA=0"
   ^ERR(4806,"62364,27842","I",53)="$ZTEXIT="""""
   ^ERR(4806,"62364,27842","I",54)="$ZTLEVEL=0"
   ^ERR(4806,"62364,27842","I",55)="$ZTOLDVAL="""""
   ^ERR(4806,"62364,27842","I",56)="$ZTRAP=""GOTO OPEN"""
   ^ERR(4806,"62364,27842","I",57)="$ZTRIGGEROP="""""
   ^ERR(4806,"62364,27842","I",58)="$ZTSLATE="""""
   ^ERR(4806,"62364,27842","I",59)="$ZTUPDATE="""""
   ^ERR(4806,"62364,27842","I",60)="$ZTVALUE="""""
   ^ERR(4806,"62364,27842","I",61)="$ZTWORMHOLE="""""
   ^ERR(4806,"62364,27842","I",62)="$ZUSEDSTOR=893732"
   ^ERR(4806,"62364,27842","I",63)="$ZVERSION=""GT.M V5.4-002B Linux x86"""
   ^ERR(4806,"62364,27842","I",64)="$ZYERROR="""""
   ^ERR(4806,"62364,27842","L",0)="MLG:0,MLT:0"
   ^ERR(4806,"62364,27842","S",1)="RECORD+1^ERR"
   ^ERR(4806,"62364,27842","S",2)="SUB3+3^EP13"
   ^ERR(4806,"62364,27842","S",3)="SUB2+3^EP13"
   ^ERR(4806,"62364,27842","S",4)="SUB1+3^EP13"
   ^ERR(4806,"62364,27842","S",5)="NODB+3^EP13"
   ^ERR(4806,"62364,27842","S",6)="+1^GTM$DMOD    (Direct mode) "
   ^ERR(4806,"62364,27842","V",1)="A=5 ;*"
   ^ERR(4806,"62364,27842","V",2)="B=""BAD"""
   File contents:
   $DEVICE=""
   $ECODE=",M9,Z150373210,M6,Z150373850,"
   $ESTACK=5
   $ETRAP=""
   $HOROLOG="62364,27842"
   $IO="20110930074402_4806.ERR"
   $JOB=4806
   $KEY=""
   $PRINCIPAL="/dev/pts/8"
   $QUIT=0
   $REFERENCE="^ERR(4806,""62364,27842"",""S"",6)"
   $STACK=5
   $STORAGE=2147483647
   $SYSTEM="47,gtm_sysid"
   $TEST=1
   $TLEVEL=0
   $TRESTART=0
   $X=0
   $Y=18
   $ZA=0
   $ZALLOCSTOR=895460
   $ZB=""
   $ZCHSET="M"
   $ZCMDLINE=""
   $ZCOMPILE=""
   $ZCSTATUS=0
   $ZDATEFORM=0
   $ZDIRECTORY="/home/jdoe/"
   $ZEDITOR=0
   $ZEOF=1
   $ZERROR="Unprocessed $ZERROR, see $ZSTATUS"
   $ZGBLDIR="/home/jdoe/.fis-gtm/V5.4-002B_x86/g/gtm.gld"
   $ZININTERRUPT=0
   $ZINTERRUPT="IF $ZJOBEXAM()"
   $ZIO="20110930074402_4806.ERR"
   $ZJOB=0
   $ZLEVEL=6
   $ZMAXTPTIME=0
   $ZMODE="INTERACTIVE"
   $ZPATNUMERIC="M"
   $ZPOSITION="OPEN+7^ERR"
   $ZPROCESS=""
   $ZPROMPT="YDB>"
   $ZQUIT=0
   $ZREALSTOR=898568
   --------------------
   Extra lines removed
   --------------------


