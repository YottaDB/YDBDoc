
.. index::
   Instrinsic Special Variables

===========================
Intrinsic Special Variables
===========================

.. contents::
   :depth: 2

This chapter describes the M Intrinsic Special Variables implemented in YottaDB/GT.M. All entries starting with the letter Z are YottaDB/GT.M additions to the ANSI standard Intrinsic Special Variables. None of the Intrinsic Special Variables are case sensitive.

M Intrinsic Special Variables start with a single dollar sign ($). YottaDB/GT.M provides such variables for program examination. In some cases, the Intrinsic Special Variables may be set to modify the corresponding part of the environment.

.. note::
   None of the Intrinsic Special Variables can be KILLed. SETting or NEWing is generally not allowed, but is specifically noted in the descriptions of those that do.

++++++++++++++
$ZTrap
++++++++++++++

$ZT[RAP] contains a string value that YottaDB/GT.M XECUTEs when an error occurs during routine execution. 

.. note::
   The following discussion assumes that $ETRAP error handling is simultaneously not in effect (that is, $ETRAP="").  See Chapter 13: “Error Processing” for more information on the interaction between $ETRAP and $ZTRAP.

When the $ZTRAP variable is not null, YottaDB/GT.M executes $ZTRAP at the current level. The $ZTRAP variable has the initial value of "B," and puts the process in Direct Mode when an error condition occurs. If the value of $ZTRAP is null (""), an exception causes the image to run-down with the condition code associated with the exception. If $ZTRAP contains invalid source code, YottaDB/GT.M displays an error message and puts the process into Direct Mode.

$ZTRAP is a read-write Intrinsic Special Variable, (that is, it can appear on the left side of the equal sign (=) in the argument to the SET command).

$ZTRAP may also appear as an argument to an inclusive NEW command. NEW $ZTRAP causes YottaDB/GT.M to stack the current $ZTRAP value, and set its value to the empty string ($ZTRAP=""). The NEW command puts the $ZTRAP in control for error handling. When the program QUITs from the invocation level where the NEW occurred, YottaDB/GT.M restores the value previously stacked by the NEW. NEW $ZTRAP provides nesting of $ZTRAP. Because $ZTRAP="" terminates the image when an error occurs, SET $ZTRAP= generally follows immediately after NEW $ZTRAP. You may use this technique to construct error handling strategies corresponding to the nesting of your programs. If the environment variable gtm_ztrap_new evaluates to boolean TRUE (case insensitive string "TRUE", or case insensitive string "YES", or a non-zero number), $ZTRAP is NEWed when $ZTRAP is SET; otherwise $ZTRAP is not stacked when it is SET.

.. note::
   QUIT from a $ZTRAP terminates the level at which the $ZTRAP was activated.

Keep $ZTRAP simple and put complicated logic in another routine. If the action specified by $ZTRAP results in another run-time error before changing the value of $ZTRAP, YottaDB/GT.M invokes $ZTRAP until it exhausts the process stack space, terminating the image. Carefully debug exception handling.

Example:

.. parsed-literal::
   GTM>S $ZTRAP="ZP @$ZPOS B"

This example modifies $ZTRAP to display source code for the line where GT.M encounters an error before entering Direct Mode.

There are four settings of $ZTRAP controlled by the UNIX environment variable gtm_ztrap_form.

The four settings of gtm_ztrap_form are:

*  code - If gtm_ztrap_form evaluates to "code" (or a value that is not one of the subsequently described values), then YottaDB/GT.M treats $ZTRAP as code and handles it as previously described in the documentation.
* entryref - If gtm_ztrap_form evaluates to "entryref" then YottaDB/GT.M treats it as an entryref argument to an implicit GOTO command.
* adaptive - If gtm_ztrap_form evaluates to "adaptive" then if $ZTRAP does not compile to valid M code, then $ZTRAP is treated as just described for "entryref." Since there is little ambiguity, code and entryref forms of $ZTRAP can be intermixed in the same application.
* pope[ntryref] / popa[daptive] - If gtm_ztrap_form evaluates to "POPE[NTRYREF]" or "POPA[DAPTIVE]" (case insensitive) and $ZTRAP value is in the form of entryref, YottaDB/GT.M unwinds the M stack from the level at which an error occurred to (but not including) the level at which $ZTRAP was last SET. Then, YottaDB/GT.M transfers control to the entryref in $ZTRAP at the level where the $ZTRAP value was SET. If the UNIX environment variable gtm_zyerror is defined to a valid entryref, YottaDB/GT.M transfers control to the entryref specified by GTM_ZYERROR (with an implicit DO) after unwinding the stack and before transferring control to the entryref specified in $ZTRAP.

.. note::
   YottaDB/GT.M attempts to compile $ZTRAP before evaluating $ZTRAP as an entryref. Because YottaDB/GT.M allows commands without arguments such as QUIT, ZGOTO, or HANG as valid labels, be careful not to use such keywords as labels for error handling code in "adaptive" mode.

.. note::
   Like $ZTRAP values, invocation of device EXCEPTION values follow the pattern specified by the current gtm_ztrap_form setting except that there is never any implicit popping with EXCEPTION action.

---------------
$DEVICE
---------------

$D[EVICE] reflects the status of the current device. If the status of the device does not reflect any error-condition, the value of $DEVICE, when interpreted as a truth-value is 0 (FALSE). If the status of the device reflect any error-condition, the value of $DEVICE, when interpreted as a truth-value is 1 (TRUE).

.. parsed-literal::
   The initial value of $DEVICE is implementation dependant. However, if the initial value of $IO is the empty string, then the initial value of $DEVICE is also empty string.

$DEVICE gives status code and meaning, in one access:

Example:

.. parsed-literal::
   1,Connection reset by peer

The above message is displayed on the server side when the socket device is closed on the client side.

------------------
$ECODE
------------------

$EC[ODE] contains a list of error codes for "active" errors -the error conditions which are not yet resolved. If there are no active errors, $ECODE contains the empty string. Whenever an error occurs, a code for that error is appended to the value of $ECODE in such a way that the value of $ECODE always starts and ends with a comma.

The value of $ECODE can be SET, and when it is set to a non-NULL value, error processing starts.

.. note::
   See Chapter 13: “Error Processing” to learn about $ECODE's role in error processing.

List of codes for $ECODE start with comma seperated by commas. A code starts with "M", "U", or "Z", with rest numeric. "M" codes are assigned by MDC (MUMPS Development Committee), "U" by application (programmers), and "Z" codes by MUMPS implementors (in this case YottaDB/GT.M).

An error always has a YottaDB/GT.M specified code and many errors also have an ANSI Standard code. The complete list of standardized error codes can be referenced from the Message and Recovery Procedures Reference Manual version 4.3 and onwards.

.. parsed-literal::
   IF $ECODE[",M61," WRITE "Undefined local variable"

.. note::
   The leftmost character of the value of $ECODE is always a comma. This means that every error code that is stored in $ECODE is surrounded by commas. If $ECODE was to contains the error code without the commas (that is, "M61"), the variable would check for subset "M6" as well. Thus, it is recommended that you include the commas in the value to check. For example; check whether $ECODE contains ",M61,".

$ECODE can be SET but not NEW'd. When $ECODE is set to the empty string (" "), error handling becomes "inactive" and therefore QUIT does not trigger additional error handling.

When $ECODE is not set to the empty string, M error handling is active, which also affects behavior in some aspects of $STACK.

--------------
$ESTACK
--------------

$ES[TACK] contains an integer count of the number of M virtual machine stack levels that have been activated and not removed since the last time $ESTACK was NEW'd.

A NEW $ESTACK saves the value of current $ESTACK and then sets its value to zero (0). If $ESTACK has not been NEW'd in the current execution path, $ESTACK=$STACK.

.. parsed-literal::
   SET $ETRAP="QUIT:$ESTACK GOTO LABEL^ROUTINE"

$ESTACK maybe used as a flag to indicate error traps invoked in particular stack levels needed to perform some different action(s). $ESTACK can be most useful in setting up a layered error trapping mechanism.

.. note::
   YottaDB/GT.M does not permit $ESTACK to be SET, however $ESTACK can be NEWed.

--------------
$ETRAP
--------------

$ET[RAP] contains a string value that YottaDB/GT.M invokes when an error occurs during routine execution. When a process is initiated, YottaDB/GT.M assigns $ETRAP the value of the gtm_etrap environment variable, if gtm_etrap is defined, and otherwise the empty string, in which case $ZTRAP="B" controls initial error handling.

The value of this variable is the M[UMPS] code that YottaDB/GT.M executes when it encounters an error.

.. parsed-literal::
   SET $ETRAP="QUIT:$ESTACK GOTO LABEL^ROUTINE"

The value of $ETRAP is changed with the SET command. Changing the value of $ETRAP with the SET command initiates a new trap; it does not save the old trap.

$ETRAP may also appear as an argument to an inclusive NEW command. NEW $ETRAP causes YottaDB/GT.M to stack the active condition handler's ($ETRAP) old value. NEW leaves the $ETRAP unchanged regardless of the previously active condition handler. NEW $ETRAP command puts $ETRAP in control for error handling.

For more examples of the use of special variable $ETRAP, see the function $STACK().

----------------
$HOROLOG
----------------

$H[OROLOG] contains a string value specifying the number of days since "31 December, 1840," and the number of seconds since midnight of date in the time zone of the process, separated by a comma (,). At midnight, the piece of the string following the comma resets to zero (0), and the piece preceding the comma increments by one (1). YottaDB/GT.M does not permit the SET command to modify $HOROLOG. A process takes the system time from the system clock, but can adjust the time zone by appropriately setting the TZ environment variable before invoking YottaDB/GT.M.

Example:

.. parsed-literal::
   GTM>Write $HOROLOG

Produces the result 58883,55555 at 3:25:55 pm on 20 March, 2002.

For further information on formatting $HOROLOG for external use, refer to “$ZDate()”.

---------
$IO
---------

$I[O] contains the name of the current device specified by the last USE command. The M standard does not permit the SET command to modify $IO. USE 0 produces the same $IO as USE $P[RINCIPAL], but $P is the preferred construct.

-----------
$JOB
-----------

$J[OB] the current process identifier.

YottaDB/GT.M uses the decimal representation of the current process identifier (PID) for the value of $JOB. $JOB is guaranteed to be unique for every concurrently operating process on a system. However, operating systems reuse PIDs over time. YottaDB/GT.M does not permit the SET command to modify $JOB.

Example:

.. parsed-literal::
   LOOP0 for  set itm=$order(^tmp($J,itm)) quit:itm=""  do LOOP1

This uses $J as the first subscript in a temporary global to insure that every process uses separate data space in the global ^tmp.

------------
$KEY
------------

$K[EY] contains the string that terminated the most recent READ command from the current device (including any introducing and terminating characters). If no READ command was issued to the current device or if no terminator is used, the value of $KEY is an empty string. However, when input is terminated by typing a function key, the value of $KEY is equal to the string of characters that is transmitted by that function key.

The effect of a READ \*glvn on $KEY is unspecified.

For terminals, $KEY and $ZB both have the terminator.

.. note::
   See the READ and WRITE commands in Chapter 6: “Commands”.

For SOCKET:

$KEY contains the socket handle and the state information of the current SOCKET device after certain I/O commands.

After a successful OPEN or USE with the LISTEN deviceparameter, $KEY contains for TCP sockets:

.. parsed-literal::
   "LISTENING|<socket_handle>|<portnumber>"

and for LOCAL sockets:

.. parsed-literal::
   "LISTENING|<socket_handle>|<address>"

After a successful OPEN or USE with the CONNECT device parameter or when GT.M was started with a socket as the $PRINCIPAL device, $KEY contains:

.. parsed-literal::
   "ESTABLISHED|<socket handle>|<address>"

When WRITE /WAIT selects an incoming connection, $KEY contains:

.. parsed-literal::
   "CONNECT|<socket_handle>|<address>"

When WRITE /WAIT selects a socket with data available for reading, $KEY contains:

.. parsed-literal::
   "READ|<socket_handle>|<address>"

For TCP sockets, <address> is the numeric IP address for the remote end of the connection. For LOCAL sockets it is the path to the socket.

For TCP LISTENING sockets, <portnumber> is the local port on which socket_handle is listening for incoming connections. For LOCAL LISTENING sockets, it is the path of the socket.

If the WRITE /WAIT was timed, $KEY returns an empty value if the wait timed out or there was no established connection. $KEY only has the selected handle, if any, immediately after a WRITE /WAIT. $KEY is also used by other socket I/O commands such as READ which sets it to the delimiter or bad Unicode character, if any, which terminated the read.

---------------
$QUIT
---------------

$Q[UIT] indicates whether the current block of code was called as an extrinsic function or as a subroutine.

If $Q[UIT] contains 1 (when the current process-stack frame is invoked by an extrinsic function), the QUIT would therefore require an argument.

.. note::
   When a process is initiated, but before any commands are processed, the value of $Q[UIT] is zero (0). 

This special variable is mainly used in error-trapping conditions. Its value tells whether the current DO level was reached by means of a subroutine call (DO xxx) or by a function call (SET variable=$$xxx).

A typical way of exiting from an error trap is:

.. parsed-literal::
   QUIT:$QUIT "" QUIT

.. note::
   YottaDB/GT.M does not permit $QUIT to be SET or NEWed.

--------------------
$REFERENCE
--------------------

$R[EFERENCE] contains the last global reference. Until the first global reference is made by an M program or after a global reference with an invalid key, $REFERENCE contains the empty string (""). This way it is useful in determining if the usage of a naked reference is valid.

A typical way of using this is:

.. parsed-literal::
   IF $REFERENCE="" QUIT "<undefined>"

.. note::
   $R[EFERENCE] being a read-only variable cannot be SET or NEW'd.

----------------
$STACK
----------------

$ST[ACK] contains an integer value of zero (0) or greater indicating the current level of M execution stack depth.

When a process is initiated but before any command is executed, the value of $STACK is zero (0).

.. note::
   The difference between $STACK and $ESTACK is that $ESTACK may appear as an argument of the NEW command. NEWing $ESTACK resets its value to zero (0), and can be useful to set up a layered error trapping mechanism.

The value of $STACK is "absolute" since the start of a YottaDB/GT.M. process, whereas the value of $ESTACK is "relative" to the most recent "anchoring point".

For examples on the use of special variable $STACK, see “$STack()”.

--------------
$STORAGE
--------------

$S[TORAGE] contains an integer value specifying the number of free bytes of address space remaining between the memory currently under management by the process and the theoretical maximum available to the process.

YottaDB/GT.M uses memory for code (instructions) and data. If the amount of virtual memory available to the process exceeds 2,147,483,647 bytes, it is reported as 2,147,483,647 bytes.

Instruction space starts out with the original executable image. However, YottaDB/GT.M may expand instruction space by ZLINKing additional routines.

Data space starts out with stack space that never expands, and pool space which may expand. Operations such as opening a database or creating a local variable may cause an expansion in pool space. YottaDB/GT.M expands pool space in fairly large increments. Therefore, SETs of local variables may not affect $STORAGE at all or may cause an apparently disproportionate drop in its value.

Once a YottaDB/GT.M process adds either instruction or data space, it never releases that space. However, YottaDB/GT.M does reuse process space made available by actions such as KILLs of local variables. $STORAGE can neither be SET or NEWed.

----------------
$SYSTEM
----------------

$SY[STEM] contains a string that identifies the executing M instance. The value of $SYSTEM is a string that starts with a unique numeric code that identifies the manufacturer. Codes are assigned by the MDC (MUMPS Development Committee).

$SYSTEM in YottaDB/GT.M starts with "47" followed by a comma and the evaluation of the environment variable gtm_sysid. If the name has no evaluation, the value after the comma is gtm_sysid.

---------------
$TEST
---------------

$T[EST] contains a truth value specifying the evaluation of the last IF argument or the result of the last operation with timeout. If the last timed operation timed out, $TEST contains FALSE (0); otherwise, it contains TRUE (1).

$TEST serves as the implicit argument for ELSE commands and argumentless IF commands.

M stacks $TEST when invoking an extrinsic and performing an argumentless DO. After these operations complete with an implicit or explicit QUIT, M restores the corresponding stacked value. Because, with these two exceptions, $TEST reflects the last IF argument or timeout result on a process wide basis. Use $TEST only in immediate proximity to the operation that last updated it.

Neither $SELECT() nor post-conditional expressions modify $TEST.

M routines cannot modify $TEST with the SET command.

Example:

.. parsed-literal::
   IF x=+x DO ^WORK
   ELSE SET x=0

The ELSE statement causes M to use the value of $TEST to determine whether to execute the rest of the line. Because the code in routine WORK may use IFs and timeouts, this use of $TEST is not recommended.

Example:

.. parsed-literal::
   SET MYFLG=x=+x
   IF MYFLG DO ^WORK
   IF 'MYFLG SET x=0

This example introduces a local variable flag to address the problems of the prior example. Note that its behavior results in the opposite $TEST value from the prior example.

Example:

.. parsed-literal::
   IF x=+x DO ^WORK IF 1
   ELSE SET x=0

This example uses the IF 1 to ensure that the ELSE works counter to the IF.

----------------
$TLEVEL
---------------

$TL[EVEL] contains a count of executed TSTARTs that are currently unmatched by TCOMMITs. $TLEVEL is zero (0) when there is no TRANSACTION in progress. When $TLEVEL is greater than one (>1), it indicates that there are nested sub-transactions in progress. Sub-transactions are always subject to the completion of the main TRANSACTION and cannot be independently acted upon by COMMIT, ROLLBACK, or RESTART.

$TLEVEL can be used to determine whether there is a TRANSACTION in progress and to determine the level of nesting of sub-transactions.

M routines cannot modify $TLEVEL with SET.

Example:

.. parsed-literal::
   IF $TLEVEL TROLLBACK

This example performs a TROLLBACK if a transaction is in progress. A statement like this should appear in any error handler used with transaction processing. For more information on transaction processing, see Chapter 5: “General Language Features of M”.

---------------
$TRESTART
---------------

$TR[ESTART] contains a count of the number of times the current TRANSACTION has been RESTARTed. A RESTART can be explicit (specified in M as a TRESTART) or implicit (initiated by YottaDB/GT.M as part of its internal concurrency control mechanism). $TRESTART can have values of 0 through 4. When there is no TRANSACTION in progress, $TRESTART is zero (0).

$TRESTART can be used by the application to limit the number of RESTARTs, or to cause a routine to perform different actions during a RESTART than during the initial execution.

.. note::
   YottaDB/GT.M does not permit the SET command to modify $TRESTART.

Example:

.. parsed-literal::
   TRANS TSTART ():SERIAL
   IF $TRESTART>2 WRITE !;"Access Conflict" QUIT

This example terminates the sub-routine with a message if the number of RESTARTs exceeds 2.

----------
$X
----------

$X contains an integer value ranging from 0 to 65,535, specifying the horizontal position of a virtual cursor in the current output record. $X=0 represents the left-most position of a record or row.

Every OPEN device has a $X. However, M only accesses $X of the current device. Therefore, exercise care in sequencing USE commands and references to $X.

Generally, YottaDB/GT.M increments $X for every character written to and read from the current device. Usually, the increment is 1, but for a process in UTF-8 mode, the increment is the number of glyphs or codepoints (depends on the type of device). M format control characters, write filtering, and the device WIDTH also have an effect on $X.

$X never equals or exceeds the value of the device WIDTH. Whenever it reaches the value equal to the device WIDTH, it gets reset to zero (0).

YottaDB/GT.M follows the MDC Type A recommendation and permits an M routine to SET $X. However, SET $X does not automatically issue device commands or escape sequences to reposition the physical cursor.

For more information, refer to “$X”.

-----
$Y
-----

$Y contains an integer value ranging from 0 to 65,535 specifying the vertical position of a virtual cursor in the current output page. $Y=0 represents the top row or line.

Every OPEN device has a $Y. However, M only accesses $Y of the current device. Therefore, exercise care in sequencing USE commands and references to $Y.

When YottaDB/GT.M finishes the logical record in progress, it generally increments $Y. YottaDB/GT.M recognizes the end of a logical record when it processes certain M format control characters, or when the record reaches its maximum size, as determined by the device WIDTH, and the device is set to WRAP. The definition of "logical record" varies from device to device. For an exact definition, see the sections on each device type. Write filtering and the device LENGTH also have an effect on $Y.

$Y never equals or exceeds the value of the device LENGTH. Whenever it reaches the value equal to the device LENGTH, it gets reset to zero (0)

YottaDB/GT.M permits an M routine to SET $Y. However, SET $Y does not automatically issue device commands or escape sequences to reposition the physical cursor.

For more information, refer to “$Y”.

-------
$ZA
-------

$ZA contains a status determined by the last read on the device. The value is a decimal integer with a meaning determined by the device as follows:

For Terminal I/O:

0Indicating normal termination of a read operation

1: Indicating a parity error

2: Indicating that the terminator sequence was too long

9: Indicating a default for all other errors

For Sequential Disk and Tape Files I/O:

0: Indicating normal termination of a read operation

9: Indicating a failure of a read operation

For Fifos I/O:

Decimal representing $JOB (identifier) of the process that wrote the last message the current process read

$ZA refers to the status of the current device. Therefore, exercise care in sequencing USE commands and references to $ZA.

YottaDB/GT.M does not permit the SET command to modify $ZA.

For more information on $ZA, refer "Input/Output Processing".

-------------
$ZALLOCSTOR
-------------

$ZALLOCSTOR contains the number of bytes that are (sub) allocated (including overhead) by YottaDB/GT.M for various activities. It provides one view (see also “$ZREalstor” and “$ZUSedstor”) of the process memory utilization and can help identify storage related problems. YottaDB/GT.M does not permit $ZALLOCSTOR to be SET or NEWed.

----------
$ZB
----------

$ZB contains a string specifying the input terminator for the last terminal READ. $ZB contains null and is not maintained for devices other than terminals. $ZB may contain any legal input terminator, such as <CR> (ASCII 13) or an escape sequence starting with <ESC> (ASCII 27), from zero (0) to 15 bytes in length. $ZB contains null for any READ terminated by a timeout or any fixed-length READ terminated by input reaching the maximum length.

$ZB contains the actual character string, not a sequence of numeric ASCII codes.

Example:

.. parsed-literal::
   SET zb=$ZB FOR i=1:1:$L(zb) WRITE !,i,?5,$A(zb,i)

This displays the series of ASCII codes for the characters in $ZB.

$ZB refers to the last READ terminator of the current device. Therefore, exercise care in sequencing USE commands and references to $ZB.

YottaDB/GT.M does not permit the SET command to modify $ZB.

For more information on $ZB, refer to the "Input/Output Processing" chapter.

--------------
$ZCHSET
--------------

$ZCHSET is a read-only intrinsic special variable that takes its value from the environment variable gtm_chset. An application can obtain the character set used by a YottaDB/GT.M process by the value of $ZCHSET. $ZCHSET can have only two values --"M", or "UTF-8".

.. note::
   YottaDB/GT.M performs operations on literals at compile time and the character set may have an impact on such operations. Therefore, always compile with the same character set as that used at runtime.

Example:

.. parsed-literal::
  $ export gtm_chset=UTF-8
  $ /usr/lib/fis-gtm/V6.0-001_x86/gtm
  GTM>write $zchset
  UTF-8
  GTM>

------------
$ZCMDLINE
------------

$ZCM[DLINE] contains a string value specifying the "excess" portion of the command line that invoked the YottaDB/GT.M process. By "excess" is meant the portion of the command line that is left after YottaDB/GT.M has done all of its command line processing. For example, a command line mumps -direct extra1 extra2 causes YottaDB/GT.M to process the command line upto mumps -direct and place the "excess" of the command line, that is "extra1 extra2" in $ZCMDLINE. $ZCMDLINE gives the M routine access to the shell command line input.

Note that the actual user input command line might have been transformed by the shell (for example, removing one level of quotes, filename, and wildcard substituion, and so on.), and it is this transformed command line that YottaDB/GT.M processes.

Example:

.. parsed-literal::
   $ cat > test.m
   write " $ZCMDLINE=",$ZCMDLINE,!
   quit
   $ mumps -run test OTHER  information
   $ZCMDLINE=OTHER information
   $

This creates the program test.m, which writes the value of $ZCMDLINE. Note how the two spaces specified in OTHER information in the command line gets transformed to just one space in OTHER information in $ZCMDLINE due to the shell's pre-processing.

Example:

.. parsed-literal::
   $ cat foo.m
   foo     ; a routine to invoke an arbitrary entry with or without 
    parameters
    ;
    set $etrap="" ; exit if the input isn't valid
   if $length($zcmdline) do @$zcmdline quit
   quit
   $ mumps -run foo 'BAR^FOOBAR("hello")'

In this example, YottaDB/GT.M processes the shell command line up to foo and puts the rest in $ZCMDLINE. This mechanism allows mumps -run to invoke an arbitrary entryref with or without parameters. Note that this example encloses the command line argument with single quotes to prevent inappropriate expansion in Bourne-type shells. Always remember to use the escaping and quoting conventions of the shell and YottaDB/GT.M to prevent inappropriate expansion. 

.. note::
   Use the ^%XCMD utility to XECUTEs code from the shell command line and return any error status (truncated to a single byte on UNIX) that the code generates. For more information, refer to “%XCMD”. 

-------------
$ZCOMPILE
-------------

$ZCO[MPILE] contains a string value composed of one or more qualifiers that control the YottaDB/GT.M compiler. Explicit ZLINKs and auto-ZLINKs use these qualifiers as defaults for any compilations that they perform.

$ZCOMPILE is a read-write ISV, that is, it can appear on the left side of the equal sign (=) in the argument to the SET command. A $ZCOMPILE value has the form of a list of M command qualifiers each separated by a space ( ).

When the environment variable gtmcompile is defined, YottaDB/GT.M initializes $ZCOMPILE to the translation of gtmcompile. Otherwise YottaDB/GT.M initializes $ZCOMPILE to null. Changes to the value of $ZCOMPILE during a YottaDB/GT.M invocation only last for the current invocation and do not change the value of the environment variable gtmcompile.

ZCOMPILE returns a status of 1 after any error in compilation.

When $ZCOMPILE is null, YottaDB/GT.M uses the default M command qualifiers -IGNORE, -LABEL=LOWER, -NOLIST, and -OBJECT. See Chapter 3: “Development Cycle” for detailed descriptions of the M command qualifiers.

Example:

.. parsed-literal::
   $ export gtmcompile="-LIST -LENGTH=56 -SPACE=2"
   $ gtm
   GTM>WRITE $ZCOMPILE
   -LIST -LENGTH=56 -SPACE=2
   GTM>SET $ZCOMPILE="-LIST -NOIGNORE"
   GTM>WRITE $ZCOMPILE
   -LIST -NOIGNORE
   GTM>ZLINK "A.m"
   GTM>HALT
   $ echo $gtmcompile
   -LIST -LENGTH=56 -SPACE=2

This example uses the environment variable gtmcompile to set up $ZCOMPILE. Then it modifies $ZCOMPILE with the SET command. The ZLINK argument specifies a file with a .m extension (type), which forces a compile. The compile produces a listing for routine A.m and does not produce an object module if A.m contains compilation errors. After YottaDB/GT.M terminates, the shell command echo $gtmcompile demonstrates that the SET command did not change the environment variable.

-----------------
$ZCSTATUS
-----------------

$ZC[STATUS] holds the value of the status code for the last compilation performed by a ZCOMPILE command.

YottaDB/GT.M does not permit the SET command to modify $ZSTATUS.

--------------------
$ZCLOSE
--------------------

Provides termination status of the last PIPE CLOSE as follows:

* -99 when the check times out
* -98 for unanticipated problems with the check
* the negative of the signal value if a signal terminated the co-process.

If positive, $ZCLOSE contains the exit status returned by the last co-process.



