.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2018-2025 YottaDB LLC and/or its subsidiaries.#
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
   Instrinsic Special Variables

==============================
8. Intrinsic Special Variables
==============================

.. contents::
   :depth: 3

This chapter describes the M Intrinsic Special Variables implemented in YottaDB. All entries starting with the letter Z are YottaDB additions to the ANSI standard Intrinsic Special Variables. None of the Intrinsic Special Variables are case sensitive.

M Intrinsic Special Variables start with a single dollar sign ($). YottaDB provides such variables for program examination. In some cases, the Intrinsic Special Variables may be set to modify the corresponding part of the environment.

.. note::
   None of the Intrinsic Special Variables can be KILLed. Setting or Newing is generally not allowed, but is specifically noted in the descriptions of those that do.

---------------
$DEVICE
---------------

$D[EVICE] reflects the status of the current device. If the status of the device does not reflect an error-condition, the value of $DEVICE, when interpreted as a truth-value is 0 (FALSE). If the status of the device reflects an error condition, the value of $DEVICE, when interpreted as a truth-value is 1 (TRUE).

.. code-block:: none

   The initial value of $DEVICE is implementation dependent. However, if the initial value of $IO is an empty string, then the initial value of $DEVICE is also an empty string.

$DEVICE gives status code and meaning, in one access:

Example:

.. code-block:: none

   1,Connection reset by peer

The above message is displayed on the server side when the socket device is closed on the client side.

.. _ecode-isv:

------------------
$ECODE
------------------

$EC[ODE] contains a list of error codes for "active" errors - error conditions which are not yet resolved. If there are no active errors, $ECODE contains the empty string. Whenever an error occurs, a code for that error is appended to the value of $ECODE in such a way that the value of $ECODE always starts and ends with a comma.

The value of $ECODE can be SET, and when it is set to a non-NULL value, error processing starts.

.. note::
   See `Chapter 13: "Error Processing" <./errproc.html>`_ to learn about $ECODE's role in error processing.

The list of codes in $ECODE start with a comma, and are seperated by commas. A code starts with "M", "U", or "Z", with the rest of it being numeric. "M" codes are assigned by MDC (M Development Committee), "U" by application (programmers), and "Z" codes by M implementors (in this case YottaDB).

An error always has a YottaDB specified code and many errors also have an ANSI Standard code. The complete list of standardized error codes can be referenced from the `Message and Recovery Procedures Reference Manual <../MessageRecovery/index.html>`_ and onwards.

.. code-block:: none

   IF $ECODE[",M61," WRITE "Undefined local variable"

.. note::
   The leftmost character of the value of $ECODE is always a comma. This means that every error code that is stored in $ECODE is surrounded by commas. If $ECODE was to contain an error code without commas around it(that is, "M61"), the variable would check for the subset "M6" as well. Thus, it is recommended that you include the commas in the value to check. For example; check whether $ECODE contains ",M61,".

$ECODE can be SET but not NEW'd. When $ECODE is set to the empty string (" "), error handling becomes "inactive" and therefore QUIT does not trigger additional error handling.

When $ECODE is not set to the empty string, M error handling is active, which also affects behavior in some aspects of $STACK.

--------------
$ESTACK
--------------

$ES[TACK] contains an integer count of the number of M virtual machine stack levels that have been activated and not removed since the last time $ESTACK was NEW'd.

A NEW $ESTACK saves the value of current $ESTACK and then sets its value to zero (0). If $ESTACK has not been NEW'd in the current execution path, $ESTACK=$STACK.

.. code-block:: none

   SET $ETRAP="QUIT:$ESTACK GOTO LABEL^ROUTINE"

$ESTACK maybe used as a flag to indicate error traps invoked in particular stack levels needed to perform some different action(s). $ESTACK can be most useful in setting up a layered error trapping mechanism.

.. note::
   YottaDB does not permit $ESTACK to be SET, however $ESTACK can be NEWed.

.. _etrap-isv:

--------------
$ETRAP
--------------

$ET[RAP] contains a string value that YottaDB invokes when an error occurs during routine execution. When a process is initiated, YottaDB assigns $ETRAP the value of the ydb_etrap environment variable, if ydb_etrap is defined, and otherwise the empty string, in which case $ZTRAP="B" controls initial error handling.

The value of this variable is the M[UMPS] code that YottaDB executes when it encounters an error.

.. code-block:: none

   SET $ETRAP="QUIT:$ESTACK GOTO LABEL^ROUTINE"

The value of $ETRAP is changed with the SET command. Changing the value of $ETRAP with the SET command initiates a new trap; it does not save the old trap.

$ETRAP may also appear as an argument to an inclusive NEW command. NEW $ETRAP causes YottaDB to stack the active condition handler's ($ETRAP) old value. NEW leaves the $ETRAP unchanged regardless of the previously active condition handler. NEW $ETRAP command puts $ETRAP in control for error handling.

For more examples of the use of special variable $ETRAP, see the function :ref:`stack-function`.

----------------
$HOROLOG
----------------

$H[OROLOG] contains a string value specifying the number of days since "31 December, 1840," and the number of seconds since the midnight of that date in the time zone of the process, separated by a comma (,). At midnight, the piece of the string following the comma resets to zero (0), and the piece preceding the comma increments by one (1). YottaDB does not permit the SET command to modify $HOROLOG. A process takes the system time from the system clock, but can adjust the time zone by appropriately setting the TZ environment variable before invoking YottaDB.

Example:

.. code-block:: bash

   YDB>Write $HOROLOG

Produces the result 58883,55555 at 3:25:55 pm on 20 March, 2002.

For further information on formatting $HOROLOG for external use, refer to :ref:`zdate-function`.

.. _io-isv:

---------
$IO
---------

$I[O] contains the name of the current device specified by the last USE command. The M standard does not permit the SET command to modify $IO. USE 0 produces the same $IO as USE $P[RINCIPAL], but $P is the preferred construct.

-----------
$JOB
-----------

$J[OB] is the current process identifier.

YottaDB uses the decimal representation of the current process identifier (PID) for the value of $JOB. $JOB is guaranteed to be unique for every concurrently operating process on a system. However, operating systems reuse PIDs over time. YottaDB does not permit the SET command to modify $JOB.

Example:

.. code-block:: none

   LOOP0 for  set itm=$order(^tmp($J,itm)) quit:itm=""  do LOOP1

This uses $J as the first subscript in a temporary global to insure that every process uses separate data space in the global ^tmp.

.. note::

   $KEY contains the UNIX process id of the created process shell which executes the command connected to the PIPE.

------------
$KEY
------------

$K[EY] contains the string that terminated the most recent READ command from the current device (including any introducing and terminating characters). If no READ command was issued to the current device or if no terminator is used, the value of $KEY is an empty string. However, when input is terminated by typing a function key, the value of $KEY is equal to the string of characters that is transmitted by that function key.

The effect of a READ \*glvn on $KEY is unspecified.

For terminals, $KEY and $ZB both have the terminator.

.. note::
   See the READ and WRITE commands in `Chapter 6: "Commands" <commands.html>`_.

For SOCKET:

$KEY contains the socket handle and the state information of the current SOCKET device after certain I/O commands.

After a successful OPEN or USE with the LISTEN deviceparameter, $KEY contains for TCP sockets:

.. code-block:: none

   "LISTENING|<socket_handle>|<portnumber>"

and for LOCAL sockets:

.. code-block:: none

   "LISTENING|<socket_handle>|<address>"

After a successful OPEN or USE with the CONNECT device parameter or when YottaDB was started with a socket as the $PRINCIPAL device, $KEY contains:

.. code-block:: none

   "ESTABLISHED|<socket handle>|<address>"

When WRITE/WAIT selects an incoming connection, $KEY contains:

.. code-block:: none

   "CONNECT|<socket_handle>|<address>"

When WRITE/WAIT selects a socket with data available for reading, $KEY contains:

.. code-block:: none

   "READ|<socket_handle>|<address>"

For TCP sockets, <address> is the numeric IP address for the remote end of the connection. For LOCAL sockets it is the path to the socket.

For TCP LISTENING sockets, <portnumber> is the local port on which socket_handle is listening for incoming connections. For LOCAL LISTENING sockets, it is the path of the socket.

If the WRITE /WAIT was timed, $KEY returns an empty value if the wait timed out or there was no established connection. $KEY only has the selected handle, if any, immediately after a WRITE /WAIT. $KEY is also used by other socket I/O commands such as READ which sets it to the delimiter or malformed Unicode® character, if any, which terminated the read.

When a socket has enabled non-blocking WRITEs, additional values will show if it is possible to write at least one byte to the socket. If the second argument of WRITE /WAIT is omitted or specifies both "READ" and "WRITE" and the socket selected by WRITE /WAIT is ready for both READ and WRITE, $KEY contains:

.. code-block:: none

   "READWRITE|<socket handle>|<address>"

If the socket selected by a WRITE /WAIT which implicitly or explicitly requests the write state would block on a READ but not block on WRITE, $KEY contains:

.. code-block:: none

   "WRITE|<socket handle>|<address>"

If the socket selected by WRITE /WAIT which implicitly or explicitly requests the read state would not block on a READ but would block on a WRITE, $KEY contains:

.. code-block:: none

   "READ|<socket handle>|<address>"

.. _principal-isv:

---------------
$PRINCIPAL
---------------

$P[RINCIPAL] contains the absolute pathname of the principal (initial $IO) device. $PRINCIPAL is an MDC Type A enhancement to standard M.

Input and output for a process may come from separate devices, namely, the standard input and output. However, the M I/O model allows only one device to be USEd (or active) at a time. When an image starts, YottaDB implicitly OPENs the standard input and standard output device(s) and assigns the device(s) to $PRINCIPAL. For USE deviceparameters, it is the standard input that determines the device type.

For an image invoked interactively, $PRINCIPAL is the user's terminal. For an image invoked from a terminal by means of a shell script, $PRINCIPAL is the shell script's standard input (usually the terminal) and standard output (also usually the terminal) for output, unless the shell redirects the input or output.

YottaDB provides a mechanism for the user to create a name for $PRINCIPAL in the shell before invoking YottaDB. The environment variable ydb_principal, if defined, becomes a synonym for the actual device and the value for $PRINCIPAL. $IO holds the same value as $PRINCIPAL. $ZIO in this case, holds the fully expanded name of the actual device. See "$ZIO" for an example of its usage.

YottaDB ignores a CLOSE specifying the principal device. YottaDB does not permit the SET command to modify $PRINCIPAL.

YottaDB fulfills READ to $PRINCIPAL when it is an empty socket device (that is, one with all sockets detached) with an empty string, and discards WRITE output to such a device.

YottaDB opens /dev/null as a placeholder for a socket which used to be associated with $PRINCIPAL via stdin when it is closed.

YottaDB creates a SOCKET device for $PRINCIPAL when standard input is a LOCAL domain socket and sets the default DELIMITER to "$C(10)" for sockets in the device.

When $PRINCIPAL identifies a device that supports REWIND, the REWIND or INREWIND device parameters perform a REWIND of the input and OUTREWIND performs a REWIND of the output.

When $PRINCIPAL has different input/output devices, the USE command recognizes intrinsic special variables $ZPIN or $ZPOUT and applies appropriate deviceparameters to the input or output side of $PRINCIPAL, respectively. A USE with $ZPIN or $ZPOUT sets $IO to $PRINCIPAL for READs and WRITEs from the input and output side of $PRINCIPAL. For more information refer to "$ZPIN" or "$ZPOUT" .

---------------
$QUIT
---------------

$Q[UIT] indicates whether the current block of code was called as an extrinsic function or as a subroutine.

If $Q[UIT] contains 1 (when the current process-stack frame is invoked by an extrinsic function), the QUIT would therefore require an argument.

.. note::
   When a process is initiated, but before any commands are processed, the value of $Q[UIT] is zero (0).

This special variable is mainly used in error-trapping conditions. Its value tells whether the current DO level was reached by means of a subroutine call (DO xxx) or by a function call (SET variable=$$xxx).

A typical way of exiting from an error trap is:

.. code-block:: none

   QUIT:$QUIT "" QUIT

.. note::
   YottaDB does not permit $QUIT to be SET or NEWed.

.. _reference-isv:

--------------------
$REFERENCE
--------------------

$R[EFERENCE] contains the last global reference. Until the first global reference is made by an M program or after a global reference with an invalid key, $REFERENCE contains the empty string (""). This way it is useful in determining if the usage of a naked reference is valid.

A typical way of using this is:

.. code-block:: none

   IF $REFERENCE="" QUIT "<undefined>"

.. note::
   $R[EFERENCE] being a read-only variable cannot be SET or NEW'd.

.. _stack-isv:

----------------
$STACK
----------------

$ST[ACK] contains an integer value of zero (0) or greater indicating the current level of M execution stack depth.

When a process is initiated but before any command is executed, the value of $STACK is zero (0).

.. note::
   The difference between $STACK and $ESTACK is that $ESTACK may appear as an argument of the NEW command. Newing $ESTACK resets its value to zero (0), and can be useful to set up a layered error trapping mechanism.

The value of $STACK is "absolute" since the start of a YottaDB process, whereas the value of $ESTACK is "relative" to the most recent "anchoring point".

For examples on the use of special variable $STACK, see :ref:`stack-function`.

--------------
$STORAGE
--------------

$S[TORAGE] contains an integer value specifying the number of free bytes of address space remaining between the memory currently under management by the process and the theoretical maximum available to the process.

YottaDB uses memory for code (instructions) and data. If the amount of virtual memory available to the process exceeds 2,147,483,647 bytes, it is reported as 2,147,483,647 bytes.

Instruction space starts out with the original executable image. However, YottaDB may expand instruction space by ZLINKing additional routines.

Data space starts out with stack space that never expands, and pool space which may expand. Operations such as opening a database or creating a local variable may cause an expansion in pool space. YottaDB expands pool space in fairly large increments. Therefore, SETs of local variables may not affect $STORAGE at all or may cause an apparently disproportionate drop in its value.

Once a YottaDB process adds either instruction or data space, it never releases that space. However, YottaDB does reuse process space made available by actions such as KILLs of local variables. $STORAGE can neither be SET or NEWed.

----------------
$SYSTEM
----------------

$SY[STEM] contains a string that identifies the executing M instance. The value of $SYSTEM is a string that starts with a unique numeric code that identifies the manufacturer. Codes are assigned by the MDC (M Development Committee).

$SYSTEM in YottaDB starts with "47" followed by a comma and the evaluation of the environment variable ydb_sysid or gtm_sysid. If neither of the names have any evaluation (i.e. both are undefined), the value after the comma is gtm_sysid.

YottaDB accepts SET $SYSTEM=expr, where expr appends to the initial value; an empty string removes any current added value.

.. _test-isv:

---------------
$TEST
---------------

$T[EST] contains a truth value specifying the evaluation of the last IF argument or the result of the last operation with timeout. If the last timed operation timed out, $TEST contains FALSE (0); otherwise, it contains TRUE (1).

$TEST serves as the implicit argument for ELSE commands and argumentless IF commands.

M stacks $TEST when invoking an extrinsic and performing an argumentless DO. After these operations complete with an implicit or explicit QUIT, M restores the corresponding stacked value. Because, with these two exceptions, $TEST reflects the last IF argument or timeout result on a process wide basis. Use $TEST only in immediate proximity to the operation that last updated it.

Neither $SELECT() nor post-conditional expressions modify $TEST.

M routines cannot modify $TEST with the SET command.

Example:

.. code-block:: none

   IF x=+x DO ^WORK
   ELSE SET x=0

The ELSE statement causes M to use the value of $TEST to determine whether to execute the rest of the line. Because the code in routine WORK may use IFs and timeouts, this use of $TEST is not recommended.

Example:

.. code-block:: none

   SET MYFLG=x=+x
   IF MYFLG DO ^WORK
   IF 'MYFLG SET x=0

This example introduces a local variable flag to address the problems of the prior example. Note that its behavior results in the opposite $TEST value from the prior example.

Example:

.. code-block:: none

   IF x=+x DO ^WORK IF 1
   ELSE SET x=0

This example uses the IF 1 to ensure that the ELSE works counter to the IF.

.. _tlevel-isv:

---------------
$TLEVEL
---------------

$TL[EVEL] contains a count of executed TSTARTs that are currently unmatched by TCOMMITs. $TLEVEL is zero (0) when there is no TRANSACTION in progress. When $TLEVEL is greater than one (>1), it indicates that there are nested sub-transactions in progress. Sub-transactions are always subject to the completion of the main TRANSACTION and cannot be independently acted upon by COMMIT, ROLLBACK, or RESTART.

$TLEVEL can be used to determine whether there is a TRANSACTION in progress and to determine the level of nesting of sub-transactions.

M routines cannot modify $TLEVEL with SET.

Example:

.. code-block:: none

   IF $TLEVEL TROLLBACK

This example performs a TROLLBACK if a transaction is in progress. A statement like this should appear in any error handler used with transaction processing. For more information on transaction processing, see `Chapter 5: "General Language Features of M" <./langfeat.html>`_.

---------------
$TRESTART
---------------

$TR[ESTART] contains a count of the number of times the current TRANSACTION has been RESTARTed. A RESTART can be explicit (specified in M as a TRESTART) or implicit (initiated by YottaDB as part of its internal concurrency control mechanism). $TRESTART can have values of 0 through 4. When there is no TRANSACTION in progress or during the first iteration of a transaction, $TRESTART is zero (0).

$TRESTART can be used by the application to limit the number of RESTARTs, or to cause a routine to perform different actions during a RESTART than during the initial execution.

.. note::
   YottaDB does not permit the SET command to modify $TRESTART.

Example:

.. code-block:: none

   TRANS TSTART ():SERIAL
   IF $TRESTART>2 WRITE !;"Access Conflict" QUIT

This example terminates the sub-routine with a message if the number of RESTARTs exceeds 2.

.. _x-isv:

----------
$X
----------

$X contains an integer value ranging from 0 to 65,535, specifying the horizontal position of a virtual cursor in the current output record. $X=0 represents the left-most position of a record or row.

Every OPEN device has a $X. However, M only accesses $X of the current device. Therefore, exercise care in sequencing USE commands and references to $X.

Generally, YottaDB increments $X for every character written to and read from the current device. Usually, the increment is 1, but for a process in UTF-8 mode, the increment is the number of glyphs or codepoints (depends on the type of device). M format control characters, write filtering, and the device WIDTH also have an effect on $X.

$X never equals or exceeds the value of the device WIDTH. Whenever it reaches the value equal to the device WIDTH, it gets reset to zero (0).

YottaDB follows the MDC Type A recommendation and permits an M routine to SET $X. However, SET $X does not automatically issue device commands or escape sequences to reposition the physical cursor.

-----
$Y
-----

$Y contains an integer value ranging from 0 to 65,535 specifying the vertical position of a virtual cursor in the current output page. $Y=0 represents the top row or line.

Every OPEN device has a $Y. However, M only accesses $Y of the current device. Therefore, exercise care in sequencing USE commands and references to $Y.

When YottaDB finishes the logical record in progress, it generally increments $Y. YottaDB recognizes the end of a logical record when it processes certain M format control characters, or when the record reaches its maximum size, as determined by the device WIDTH, and the device is set to WRAP. The definition of "logical record" varies from device to device. For an exact definition, see the sections on each device type. Write filtering and the device LENGTH also have an effect on $Y.

$Y never equals or exceeds the value of the device LENGTH. Whenever it reaches the value equal to the device LENGTH, it gets reset to zero (0).

YottaDB permits an M routine to SET $Y. However, SET $Y does not automatically issue device commands or escape sequences to reposition the physical cursor.

-------
$ZA
-------

$ZA contains a status determined by the last read on the device. The value is a decimal integer with a meaning determined by the device as follows:

For Terminal I/O:

0: Indicating normal termination of a read operation

1: Indicating a parity error

2: Indicating that the terminator sequence was too long

9: Indicating a default for all other errors

For Sequential Disk and Tape Files I/O:

0: Indicating normal termination of a read operation

9: Indicating a failure of a read operation

For Fifos I/O:

Decimal representing $JOB (identifier) of the process that wrote the last message the current process read

$ZA refers to the status of the current device. Therefore, exercise care in sequencing USE commands and references to $ZA.

YottaDB does not permit the SET command to modify $ZA.

For more information on $ZA, refer `"Input/Output Processing" <./ioproc.html>`_.

.. _zallocstor-isv:

-------------
$ZALLOCSTOR
-------------

$ZALLOCSTOR contains the number of bytes that are (sub) allocated (including overhead) by YottaDB for various activities. It provides one view (see also "$ZREalstor" and "$ZUSedstor") of the process memory utilization and can help identify storage related problems. YottaDB does not permit $ZALLOCSTOR to be SET or NEWed.

.. _zaudit-isv:

---------
$ZAUDIT
---------

$ZAUDIT contains a boolean value that indicates whether Audit Principal Device in the :ref:`audit-logging-facility` is enabled. YottaDB does not permit $ZAUDIT to be SET or NEWed.

----------
$ZB
----------

$ZB contains a string specifying the input terminator for the last terminal READ. $ZB contains null and is not maintained for devices other than terminals. $ZB may contain any legal input terminator, such as <CR> (ASCII 13) or an escape sequence starting with <ESC> (ASCII 27), from zero (0) to 15 bytes in length. $ZB contains null for any READ terminated by a timeout or any fixed-length READ terminated by input reaching the maximum length.

$ZB contains the actual character string, not a sequence of numeric ASCII codes.

Example:

.. code-block:: none

   SET zb=$ZB FOR i=1:1:$L(zb) WRITE !,i,?5,$A(zb,i)

This displays the series of ASCII codes for the characters in $ZB.

$ZB refers to the last READ terminator of the current device. Therefore, exercise care in sequencing USE commands and references to $ZB.

YottaDB does not permit the SET command to modify $ZB.

For more information on $ZB, refer to the `"Input/Output Processing" chapter <./ioproc.html>`_.

.. _zchset-isv:

--------------
$ZCHSET
--------------

$ZCHSET is a read-only intrinsic special variable that takes its value from the environment variable ydb_chset. An application can obtain the character set used by a YottaDB process by the value of $ZCHSET. $ZCHSET can have only two values --"M", or "UTF-8".

.. note::
   YottaDB performs operations on literals at compile time and the character set may have an impact on such operations. Therefore, always compile with the same character set as that used at runtime.

Example:

.. code-block:: bash

  $ export ydb_chset=UTF-8
  $ ydb
  YDB>write $zchset
  UTF-8
  YDB>

--------------------
$ZCLOSE
--------------------

Provides termination status of the last PIPE CLOSE as follows:

* -99 when the timed (see :ref:`ioproc-timeout`) check on the termination status of the PIPE co-process (of a PIPE device that is not OPEN'd with the INDEPENDENT deviceparameter) times out
* -98 for unanticipated problems with the timed check
* the negative of the signal value if a signal terminated the PIPE co-process.

If positive, $ZCLOSE contains the exit status returned by the last PIPE co-process.

------------
$ZCMDLINE
------------

$ZCM[DLINE] is a read-write ISV that initially contains a string value specifying the "excess" portion of the command line that invoked the YottaDB process. By "excess" is meant the portion of the command line that is left after YottaDB has done all of its command line processing. For example, a command line yottadb -direct extra1 extra2 causes YottaDB to process the command line upto yottadb -direct and place the "excess" of the command line, that is "extra1 extra2" in $ZCMDLINE. $ZCMDLINE gives the M routine access to the shell command line input.

Note that the actual user input command line might have been transformed by the shell (for example, removing one level of quotes, filename, and wildcard substituion, and so on.), and it is this transformed command line that YottaDB processes.

Example:

.. code-block:: bash

   $ cat test.m
   test	write " $ZCMDLINE=",$ZCMDLINE,!
   	quit
   $ yottadb -run test OTHER  information
   $ZCMDLINE=OTHER information
   $ yottadb -run test 'OTHER  information'
   $ZCMDLINE=OTHER  information
   $

Note how the two spaces specified in OTHER information in the unquoted command line gets transformed to just one space in OTHER information in $ZCMDLINE due to the shell's pre-processing. However, the spaces are preserved when quoted.

Example:

.. code-block:: none

   $ yottadb -run %XCMD 'zprint %XCMD^%XCMD:CLIERR+-2'
   %XCMD   ; Usage: mumps -run %XCMD '<string>'
           ; If no $ETRAP defined, use CLIERR^%XCMD overriding a potential $ZTRAP error handler
           if ""=$ETRAP new $ETRAP set $ETRAP="goto CLIERR^%XCMD"
           new etrap,%cli set etrap=$ETRAP,%cli=$zcmdline
           ; Protect %XCMD's error handler by NEWing and SETing $ETRAP at the beginning of the XECUTEd command
           xecute "new $ETRAP,$zcmdline set $ETRAP=etrap "_%cli
           quit
   $

This example shows how the utility program `%XCMD <./utility.html#xcmd>`_ takes a line of M code from a shell command and executes it. This mechanism allows shell scripts to run M commands. It is good practice to enclose the command line argument with single quotes to prevent inappropriate shell expansion. Note also how $zcmdline is NEW'd to protect the value from any changes by XECUTE'd M code.

-------------
$ZCOMPILE
-------------

$ZCO[MPILE] contains a string value composed of one or more qualifiers that control the YottaDB compiler. Explicit ZLINKs and auto-ZLINKs use these qualifiers as defaults for any compilations that they perform.

$ZCOMPILE is a read-write ISV, that is, it can appear on the left side of the equal sign (=) in the argument to the SET command. A $ZCOMPILE value has the form of a list of M command qualifiers each separated by a space ( ).

When the environment variable ydb_compile is defined, YottaDB initializes $ZCOMPILE to the translation of ydb_compile. Otherwise YottaDB initializes $ZCOMPILE to null. Changes to the value of $ZCOMPILE during a YottaDB invocation only last for the current invocation and do not change the value of the environment variable ydb_compile.

ZCOMPILE returns a status of 1 after any error in compilation.

When $ZCOMPILE is null, YottaDB uses the default M command qualifiers -IGNORE, -LABEL=LOWER, -NOLIST, and -OBJECT. See `Chapter 3: "Development Cycle" <./devcycle.html>`_ for detailed descriptions of the M command qualifiers.

Example:

.. code-block:: bash

   $ export ydb_compile="-LIST -LENGTH=56 -SPACE=2"
   $ ydb
   YDB>WRITE $ZCOMPILE
   -LIST -LENGTH=56 -SPACE=2
   YDB>SET $ZCOMPILE="-LIST -NOIGNORE"
   YDB>WRITE $ZCOMPILE
   -LIST -NOIGNORE
   YDB>ZLINK "A.m"
   YDB>HALT
   $ echo $ydb_compile
   -LIST -LENGTH=56 -SPACE=2

This example uses the environment variable ydb_compile to set up $ZCOMPILE. Then it modifies $ZCOMPILE with the SET command. The ZLINK argument specifies a file with a .m extension (type), which forces a compile. The compile produces a listing for routine A.m and does not produce an object module if A.m contains compilation errors. After YottaDB terminates, the shell command echo $ydb_compile demonstrates that the SET command did not change the environment variable.

.. _zcstatus-isv:

-----------------
$ZCSTATUS
-----------------

$ZC[STATUS] holds the value of the status code for the last compilation performed by a ZCOMPILE, ZLINK or auto-ZLINK. One (1) indicates a clean compilation, a positive number greater than one is an error code whose text you can ascertain using $ZMESSAGE(), and a negative number is a negated error code that indicates YottaDB was not able to produce an object file. The error details appear in the compilation output, so $ZCSTATUS typically contains the code for `ERRORSUMMARY <../MessageRecovery/errors.html#errorsummary>`_.

YottaDB does not permit the SET command to modify $ZSTATUS.

-----------------
$ZDATEFORM
-----------------

$ZDA[TEFORM] contains an integer value, specifying the output year format of $ZDATE(). $ZDATEFORM can be modified using the SET command. YottaDB initializes $ZDATEFORM to the translation of the environment variable ydb_zdate_form. If ydb_zdate_form is not defined, YottaDB initializes $ZDATEFORM to zero (0).

See :ref:`zdate-function` for the usage of $ZDATEFORM. $ZDATEFORM also defines the behavior of some date and time utility routines; refer `"Utility Routines" <./utility.html>`_.

Example:

.. code-block:: bash

   YDB>WRITE $ZDATEFROM
   0
   YDB>WRITE $ZDATE($H)
   11/15/18
   YDB>SET $ZDATEFORM=1
   YDB>WRITE $ZDATE($H)
   11/15/2018

----------------
$ZDIRECTORY
----------------

$ZD[IRECTORY] contains the string value of the full path of the current directory. Initially $ZDIRECTORY contains the default/current directory from which the YottaDB image/process was activated.

If the current directory does not exist at the time of YottaDB process activation, YottaDB errors out.

Example:

.. code-block:: bash

   YDB>WRITE $ZDIR
   /usr/tmp
   YDB>SET $ZDIR=".."
   YDB>WRITE $ZDIR
   /usr

This example displays the current working directory and changes $ZDIR to the parent directory.

$ZDIRECTORY is a read-write Intrinsic Special Variable, that is, it can appear on the left side of the equal sign (=) in the argument to a SET command. If an attempt is made to set $ZDIRECTORY to a non-existent directory specification, YottaDB issues an error and keeps the value of $ZDIRECTORY unchanged.

At image exit, YottaDB restores the current directory to the directory that was the current directory when YottaDB was invoked even if that directory does not exist.

-----------------
$ZEDITOR
-----------------

(Last updated: `r1.24 <https://gitlab.com/YottaDB/DB/YDB/tags/r1.24>`_)

$ZED[ITOR] holds the value of the status code for the last edit session invoked by a ZEDIT command.

YottaDB does not permit the SET or NEW command to modify $ZEDITOR.

-----------------------
$ZEOF
-----------------------

$ZEO[F] contains a truth-valued expression indicating whether the last READ operation reached the end-of-file. $ZEOF equals TRUE (1) at EOF and FALSE (0) at other positions.

YottaDB does not maintain $ZEOF for terminal devices.

$ZEOF refers to the end-of-file status of the current device. Therefore, exercise care in sequencing USE commands and references to $ZEOF.

YottaDB does not permit the SET or NEW command to modify $ZEOF.

For more information on $ZEOF, refer to the `"Input/Output Processing" chapter <./ioproc.html>`_.

--------------------
$ZERROR
--------------------

$ZE[RROR] is supposed to hold the application-specific error-code corresponding to the YottaDB error-code stored in $ECODE/$ZSTATUS (see :ref:`ecode-isv` and :ref:`zstatus-isv`).

$ZERROR contains a default value of "Unprocessed $ZERROR, see $ZSTATUS" at process startup.

$ZERROR can be SET but not NEWed.

The mapping of a YottaDB error-code to the application-specific error-code is achieved as follows. Whenever YottaDB encounters an error, $ECODE/$ZSTATUS gets set first. It then invokes the code that $ZYERROR points to if it is not null. It is intended that the code invoked by $ZYERROR use the value of $ZSTATUS to select or construct a value to which it SETs $ZERROR. If an error is encountered by the attempt to execute the code specified in $ZYERROR, YottaDB sets $ZERROR to the error status encountered. If $ZYERROR is null, YottaDB does not change the value of $ZERROR. In all cases, YottaDB proceeds to return control to the code specified by $ETRAP/$ZTRAP or device EXCEPTION whichever is applicable. For details, see :ref:`zyerror-isv`.

-------------------
$ZGBLDIR
-------------------

$ZG[BLDIR] contains the value of the current Global Directory filename. When $ZGBLDIR specifies an invalid or inaccessible file, YottaDB cannot successfully perform database operations.

YottaDB initializes $ZGBLDIR to the translation of the environment variable ydb_gbldir. The value of the ydb_gbldir environment variable may include a reference to another environment variable. If ydb_gbldir is not defined, YottaDB initializes $ZGBLDIR to the value of the environment variable gtmgbldir, and if that is not defined, then to the empty string (:code:`""`). When $ZGBLDIR is the empty string, any attempt to access the global directory results in a :code:`ZGBLDIRUNDEF` error.

$ZGBLDIR is a read-write Intrinsic Special Variable, (i.e., it can appear on the left side of the equal sign (=) in the argument to the `SET <commands.html#set>`_ command). SET $ZGBLDIR="" causes YottaDB to assign $ZGBLDIR using the same logic as at process startup. Newing $ZGBLDIR is the same as SET $ZGBLDIR="", which as just noted may change its value. As with all `NEWed <commands.html:new>`_ local variables, `QUIT <commands.html#quit>`_ restores the prior value in effect at the time of call. A $ZGBLDIR value may include an environment variable.

Setting $ZGBLDIR sets the environment variable `ydb_cur_gbldir <../AdminOpsGuide/basicops.html#>`_ to the new value of $ZGBLDIR. This allows a child process to decide whether to use the current global directory of the parent or the :code:`$ydb_gbldir`/ :code:`$gtmgbldir` at parent process startup.

Setting $ZGBLDIR also causes YottaDB to attempt to open the specified file. If the file name is invalid or the file is inaccessible, YottaDB triggers an error without changing the value of $ZGBLDIR.

To establish a value for $ZGBLDIR outside of M, use the appropriate shell command to assign a translation to ydb_gbldir. Defining ydb_gbldir provides a convenient way to use the same Global Directory during a session where you repeatedly invoke and leave YottaDB.

Example:

.. code-block:: bash

   $ ydb_gbldir=test.gld
   $ export ydb_gbldir
   $ ydb
   YDB>WRITE $zgbldir
   /usr/dev/test.gld
   YDB>SET $zgbldir="yottadb.gld"
   YDB>WRITE $zgbldir
   yottadb.gld
   YDB>HALT
   $ echo $ydb_gbldir
   test.gld

This example defines the environment variable ydb_gbldir. Upon entering YottaDB Direct Mode, $ZGBLDIR has the value supplied by ydb_gbldir. The SET command changes the value. After the YottaDB image terminates, the echo command demonstrates that ydb_gbldir was not modified by the M SET command.

.. code-block:: bash

   $ ls test.gld
   test.gld not found
   $ ydb
   YDB>WRITE $zgbldir
   /usr/dev/yottadb.gld
   YDB>set $zgbldir="test.gld"
   %YDB-E-ZGBLDIRACC, Cannot access global directory
   "/usr/dev/test.gld". Retaining /usr/dev/yottadb.gld"
   %SYSTEM-E-ENO2, No such file or directory
   YDB>WRITE $zgbldir
   /usr/dev/yottadb.gld
   YDB>halt
   $

The SET command attempts to change the value of $ZGBLDIR to :code:`test.gld`. Because the file does not exist, YottaDB reports an error and does not change the value of $ZGBLDIR.

To facilitate application migration to YottaDB from other M implementations (for example to convert UCI and VOL specifications to global directories) in the environment specification, YottaDB provides an :ref:`interface to translate strings to global directory filenames <opt-ydb-gbldir-xltn-fac>`. With the exception of the function name, this facility is identical to that :ref:`used for extended references <opt-ydb-env-xltn-fac>`.

.. note::
   Using this facility impacts the performance of *every* global access. Make sure you use it only when static determination of the global directory is not feasible. When used, maximize the efficiency of the translation routines.

-----------------
$ZHOROLOG
-----------------

$ZH[OROLOG] returns 4 comma-separated pieces (for example, "63638,39194,258602,14400"). The first two pieces are identical to the two pieces of $HOROLOG. $ZHOROLOG is a drop-in replacement for $HOROLOG in all application code of the form $PIECE($HOROLOG,",",...). For example, $ZHOROLOG can be used as the first argument of $ZDATE(). The third piece is the number of microseconds in the current second. The accuracy of the third piece is subject to the precision of the system clock. The fourth piece is an offset in seconds to UTC. For any valid UTC time offset, the fourth piece is a number between -43200 (for UTC-12:00) and +50400 (for UTC+14:00). The value of the fourth piece remains constant all through the year except for those places that observe daylight saving time. To obtain the $HOROLOG representation of UTC, add the fourth piece to the second piece of $ZHOROLOG and proceed as follows:

* If the result is a negative number, subtract one from the first piece and add 86400 (number of seconds in a day) to the second piece.
* If the result is a positive number greater than 86400, add one to the first piece and subtract 86400 from the second piece.

Example:

.. code-block:: none

   YDB>zprint ^zhoro
   zhoro(zone)
    set:'$data(zone) zone="Europe/London"
    new zutzh
    set zutzh=$$getzutzh(zone)
    do displaytzdetails(zutzh,zone)
    quit
   getzutzh(zone)
    set shcommand="TZ="_zone_" $ydb_dist/yottadb -run %XCMD 'write $zut,"" "",$zhorolog,"" "",$zdate($horolog,""MON DD,YYYY 12:60:SS AM""),!'"
    set descname="tzpipe"
    open descname:(shell="/bin/sh":command=shcommand:readonly)::"pipe"
    use descname read dateline use $principal close descname
    quit dateline
   displaytzdetails(zutzh,zone)
    set zut=$piece(zutzh," ",1)   ; $ZUT
    set zh=$piece(zutzh," ",2)    ; $ZHOROLOG
    set zhfp=$piece(zh,",",1)     ; first piece of $ZH of zone
    set zhsp=$piece(zh,",",2)
    set zhtp=$piece(zh,",",3)
    set zhfop=$piece(zh,",",4)
    set tz=zhfop/3600,hours=$select(tz*tz=1:" Hour ",1:" Hours ")
    write "Time in ",zone," ",$piece(zutzh," ",3,6)," $ZUT=",zut,!,$select(tz<0:-tz_hours_"Ahead of",1:tz_hours_"Behind")," UTC",!
    set zhsp=zhsp+zhfop
    if zhsp>86400 set zhfp=zhfp+1,zhsp=zhsp-86400     ; 86400 seconds in a day
    else  if zhsp<1 set zhfp=zhfp-1,zhsp=zhsp+86400
    write "Time in UTC ",$zdate(zhfp_","_zhsp,"MON DD,YYYY 12:60:SS AM")
    quit
   YDB>do ^zhoro
   Time in Europe/London APR 10,2018 05:20:29 PM $ZUT=1428682829213711
   1 Hour Ahead of UTC
   Time in UTC APR 10,2018 04:20:29 PM
   YDB>


--------------------
$ZININTERRUPT
--------------------

$ZINI[NTERRUPT] evaluates to 1 (TRUE) when a process is executing code initiated by the interrupt mechanism, and otherwise 0 (FALSE).

YottaDB does not permit the SET or NEW commands to modify $ZININTERRUPT.

.. _zinterrupt-isv:

---------------------
$ZINTERRUPT
---------------------

$ZINT[ERRUPT] specifies the code to be `XECUTE'd <commands.html#xecute>`_ when an interrupt (for example, through a `MUPIP INTRPT <../AdminOpsGuide/dbmgmt.html#intrpt>`_) is processed. While a $ZINTERRUPT action is in process, any additional interrupt signals are discarded. When an interrupt handler is invoked, the current values of `$ECODE <#ecode>`_ and `$REFERENCE <#reference>`_ are saved and restored when the interrupt handler returns. The current device (`$IO <isv.html#io>`_) is neither saved nor restored.

$ZINTERRUPT can be modified by the `SET <commands.html#set>`_ command.

When YottaDB uses an interrupt handler, it saves and restores the current value of $ECODE and $REFERENCE.

If an interrupt handler changes the current IO device (via `USE <commands.html#use>`_), it is the responsibility of the interrupt handler to restore the current IO device before returning. There are sufficient legitimate possibilities for why an interrupt routine would want to change the current IO device (for example; log switching), that this part of the process context is not saved and restored automatically. To restore the device which was current when the interrupt handler began, specify USE without any deviceparameters. Any attempt to do IO on a device which was actively doing IO when the interrupt was recognized may cause a `ZINTRECURSEIO <../MessageRecovery/errors.html#zintrecurseio>`_ error.

The initial value for $ZINTERRUPT is taken from the environment variable `ydb_zinterrupt <../AdminOpsGuide/basicops.html#ydb-zinterrupt>`_ if it is specified, otherwise from the environment variable gtm_zinterrupt, otherwise it defaults to the following string:

.. code-block:: none

   IF $ZJOBEXAM()

The `IF <commands.html#if>`_ statement executes the `$ZJOBEXAM() <functions.html#zjobexam>`_ function but effectively discards the return value.

.. note::
   If the default value for $ZINTERRUPT is modified, no $ZJOBEXAM() will occur unless the replacement value directly or indirectly invokes that function. In other words, while $ZJOBEXAM() is part of the interrupt handling by default, it is not an implicit part of the interrupt handling.

The interrupt handler is executed by YottaDB at a statement boundary or at an appropriate boundary in a potentially long running COMMAND. If a process is in a long running external call (for example; waiting in a message queue) YottaDB does not have sufficient control of the process state to immediately drive the interrupt handler. It recognizes the interrupt request and drives the handler after the external call returns to YottaDB and the process reaches an appropriate execution boundary. See `Interrupting Execution Flow <langext.html#interrupting-execution-flow>`_.

..  note::
    The interrupt handler does not operate "outside" the current M environment but rather within the environment of the process.

It is possible for the interrupt handler to be executed while the process executing a TP transaction holds the critical section for one or more regions. Use of this feature may create temporary hangs or pauses while the interrupt handler executes. For the default case where the interrupt handler uses $ZJOBEXAM() to create a dump, the pause duration depends on the number of local variables in the process at the time of the dump and on the speed of the secondary storage device. Such a dump is slower on a network-mounted secondary storage device than on a storage device directly connected to the local system. Design interrupt driven code to account for this issue.

Code in $ZINTERRUPT must use routine names in any entryref argument to a `DO <commands.html#do>`_, `GOTO <commands.html#goto>`_, `JOB <commands.html#job>`_, `ZGOTO <commands.html#zgoto>`_ or any extrinsic as the arrival of the interrupt can activate the code while executing an arbitrary routine in the application.

During the execution of the interrupt handling code, `$ZININTERRUPT <#zininterrupt>`_ evaluates to 1 (TRUE).

If an error occurs while compiling the $ZINTERRUPT code, the error handler is not invoked. YottaDB sends the `ERRWZINTR <../MessageRecovery/errors.html#errwzintr>`_ message and the compiler error message to syslog. If the YottaDB process is at a direct mode prompt or is executing a direct mode command (for example, a FOR loop), YottaDB also sends the ERRWZINTR error message to the user console along with the compilation error. In both cases, the interrupted process resumes execution without performing any action specified by the defective $ZINTERRUPT vector.

The error handler is invoked if an error occurs while executing the $ZINTERRUPT code. If an error occurs during execution of the interrupt handler's stack frame (before it calls anything), that error is prefixed with the ERRWZINTR error. The error handler then executes normal error processing associated with the module that was interrupted. Any other errors that occur in code called by the interrupt handler are handled by normal error handling. See `Error Processing <errproc.html>`_.

If a TP transaction is in progress (0<`$TLEVEL <#tlevel>`_), updates to globals are not safe since a TP restart can be signaled at any time prior to the transaction being committed - even after the interrupt handler returns. A TP restart reverses all global updates and unwinds the M stack so it is as if the interrupt never occurred. The interrupt handler is not redriven as part of a transaction restart. Referencing (reading) globals inside an interrupt handler can trigger a TP restart if a transaction is active. When programming interrupt handling, either discard interrupts when 0<$TLEVEL (forcing the interrupting party to try again), or use local variables that are not restored by a TRESTART to defer the interrupt action until after the final `TCOMMIT <commands.html#tcommit>`_.

.. note::
   Because sending an interrupt signal requires the sender to have appropriate permissions, the use of the job interrupt facility itself does not present any inherent security exposures. Nonetheless, because dump files created by the default action contain the values of every local variable in the context at the time they are made, inappropriate access to the dump files would constitute a security exposure. Make sure the design and implementation of any interrupt logic includes careful consideration to security issues.

Example:

.. code-block:: none

   set $zinterrupt="do ^interrupthandler($io)"
   interrupthandler(currentdev)
	  do ^handleinterrupt ; handle the interrupt
	  use currentdev      ; restore the device which was current when the interrupt was recognized
	  quit

For more information on interrupt handling, refer to `Interrrupting Execution Flow <langext.html#interrupting-execution-flow>`_.

---------------
$ZIO
---------------

$ZIO contains the translated name of the current device, in contrast to $IO, which contains the name as specified by the USE command.

YottaDB does not permit the SET or NEW command to modify $ZIO.

An example where $ZIO contains a value different from $IO is if the environment variable ydb_principal is defined.

Example:

.. code-block:: bash

   $ ydb_principal="foo"
   $ export ydb_principal
   YDB>WRITE $IO
   foo
   YDB>WRITE $ZIO
   /dev/pts/8

Notice that $ZIO contains the actual terminal device name while $IO contains the string pointed to by the environment variable ydb_principal.

.. _zjob-isv:

----------------
$ZJOB
----------------

$ZJ[OB] holds the pid of the process created by the last JOB command performed by the current process.

YottaDB initializes $ZJOB to zero (0) at process startup. If the JOB command fails to spawn a new job, YottaDB sets $ZJOB to zero (0). Note that because of the left to right evaluation order of M, using $ZJOB in the jobparameter string results in using the value created by the last, rather than the current JOB command, which is not likely to match common coding practice.

YottaDB does not permit the SET or NEW command to modify $ZJOB.

.. note::

   $KEY contains the UNIX process id of the created process shell which executes the command connected to the PIPE.

-----------------
$ZKEY
-----------------

For Socket devices:

$ZKEY contains a list of sockets in the current SOCKET device which are ready for use. Its contents include both non selected but ready sockets from the prior WRITE/WAITs and any sockets with unread data in their YottaDB buffer. $ZKEY can be used any time a SOCKET device is current. Once an incoming socket (that is, "LISTENING") has been accepted either by being selected by WRITE/WAIT or by USE socdev:socket="listeningsocket", it is removed from $ZKEY.

$ZKEY contains any one of the following values:

.. code-block:: none

   "LISTENING|<listening_socket_handle>|{<portnumber>|</path/to/LOCAL_socket>}"

.. code-block:: none

   "READ|<socket_handle>|<address>"

.. code-block:: none

   "WRITE|<socket_handle>|<address>"

If $ZKEY contains one or more "READ|<socket_handle>|<address>" entries, it means there are ready to READ sockets that were selected by WRITE/WAIT or were partially read and there is data left in their buffer. Each entry is delimited by a ";".

If $ZKEY contains one or more "LISTENING|<listening_socket_handle>|{<portnumber|/path/to/LOCAL_socket>}" entries, it means that there are pending connections and a USE s:socket=listening_socket_handle will accept a pending connection and remove the LISTENING|<listening_socket_handle> entry from $ZKEY.

If $ZKEY contains one or more "WRITE|<socket_handle>|<address>" entries, it means that the prior WRITE /WAIT considered the non-blocking sockets writeable. This is likely to be the case most of the time. If $ZKEY contains one WRITE and one READ entry, it means that the non-blocking socket is both readable and writeable.

$ZKEY is empty if no sockets have data in the buffer and there are no unaccepted incoming sockets from previous WRITE/WAITs.

For a Sequential File Device:

$ZKEY contains the current position in the file based on the last READ. This is in bytes for STREAM and VARIABLE formats, and in a record,byte pair for FIXED format. For FIXED format, SEEKs and normal READs always produce a zero byte position; a non-zero byte position in $ZKEY for FIXED format operation indicates a partially read record, caused by a READ # or READ \*. In FIXED mode, the information returned for $ZKEY is a function of record size, and, if a USE command changes record size by specifying the WIDTH deviceparameter while the file is open, $ZKEY offsets change accordingly; if record size changes, previously saved values of $ZKEY are likely inappropriate for use with SEEK.

.. _zlevel-isv:

-----------------
$ZLEVEL
-----------------

$ZL[EVEL] contains an integer value indicating the "level of nesting" caused by DO commands, XECUTE commands, and extrinsic functions in the M invocation stack.

$ZLEVEL has an initial value of one (1) and increments by one with each DO, XECUTE or extrinsic function. Any QUIT that does not terminate a FOR loop decrements $ZLEVEL. ZGOTO may also reduce $ZLEVEL. In accordance with the M standard, a FOR command does not increase $ZLEVEL. M routines cannot modify $ZLEVEL with the SET or NEW commands.

Use $ZLEVEL in debugging or in an error-handling mechanism to capture a level for later use in a ZGOTO argument.

Example:

.. code-block:: bash

   YDB>zprint ^zleve
   zleve;
    do B
    write X,!
    quit
   B
    goto C
    quit
   C
    do D
    quit
   D
    set X=$ZLEVEL
    quit
   YDB>do ^zleve
    4
   YDB>

This program, executed from Direct Mode, produces a value of 4 for $ZLEVEL. If you run this program from the shell, the value of $ZLEVEL is three (3).

------------------
$ZMALLOCLIM
------------------

$ZMALL[OCLIM] contains an integer value specifying a number of bytes of process memory, which, if exceeded, causes YottaDB to issue a `MALLOCCRIT <../MessageRecovery/errors.html#malloccrit>`_ error. When the value is set, or defaults, to zero (0), YottaDB imposes no limit, although the OS still does. A positive value specifies a byte limit with a minimum of 2.5MB. A value of minus one (-1) provides a value of half the system imposed limit if any.

When a request for additional memory exceeds the limit, YottaDB does the expansion and then produces trappable MALLOCCRIT warning. By default, some later request for memory is likely to produce a fatal `MEMORY <../MessageRecovery/errors.html#memory-err>`_ error, unless subsequent to MALLOCCRIT, a limit has been reestablished by SET $ZMALLOCLIM to the same or higher limit, but one not exceeding any system limit.

Note that YottaDB allocates memory from the OS in large blocks so the interaction of $ZMALLOCLIM with memory growth is not exact. MEMORY errors are fatal and terminate the process. In the case of a MEMORY error, YottaDB makes an attempt to marshal available memory to enable as graceful a termination as possible. Note that independent of this mechanism, the OS may kill the process without recourse if it determines the greed of the process for memory jeopardizes the viability of the system.

When the integer byte value specified in a SET $ZMALLOCLIM=intexpr or by the environment variable `ydb_malloc_limit  <../AdminOpsGuide/basicops.html#ydb-malloc-limit>`_  ($gtm_malloc_limit if $ydb_malloc_limit is not defined) at process startup, specifies a positive value, YottaDB uses the smaller of that value and any OS defined amount for the value of $ZMALLOCLIM. YottaDB does not give errors or messages about its choice for $ZMALLOCLIM between a specified value and some other more appropriate value, so if the application needs to verify the result, it should examine the resulting ISV value.

------------------
$ZMAXTPTIME
------------------

$ZMAXTPTI[ME] contains an integer value indicating the time duration, in seconds, YottaDB should wait for the completion of all activities fenced by the current transaction's outermost TSTART/TCOMMIT pair.

$ZMAXTPTIME can be SET but cannot be NEWed.

$ZMAXTPTIME takes its value from the environment variable ydb_maxtptime. If ydb_maxtptime is not defined, the initial value of $ZMAXTPTIME is zero (0) seconds which indicates "no timeout" (unlimited time). The value of $ZMAXTPTIME when a transaction's outermost TSTART operation executes determines the timeout setting for that transaction.

When a $ZMAXTPTIME expires, YottaDB executes the $ETRAP/$ZTRAP exception handler currently in effect.

.. note::
   Negative values of $ZMAXTPTIME are also treated as "no timeout". Timeouts apply only to the outermost transaction, that is, $ZMAXTPTIME has no effect when TSTART is nested within another transaction.

Example:

.. code-block:: none

   Test;testing TP timeouts
     set $ZMAXTPTIME=6,^X=0,^Y=0,^Z=0
     write "Start with $ZMAXTPTIME=",$ZMAXTPTIME,":",!
     for sleep=3:2:9 do
     . set retlvl=$zlevel
     . do longtran;ztrap on longtran
     ;continues execution
     ;on next line
     . write "(^X,^Y)=(",^X,",",^Y,")",!
     write !,"Done TP Timeout test.",!
     quit
   longtran ;I/O in TP doesn't get rolled back
     set $etrap=" goto err"
     tstart ():serial
     set ^X=1+^X
     write !,"^X=",^X,",will set ^Y to ",sleep
     write " in ",sleep," seconds..."
     hang sleep
     set ^Y=sleep
     write "^Y=",^Y
     tcommit
     write "...committed.",!
     quit
   err;
     write !,"In $ETRAP handler. Error was: "
     write !," ",$zstatus
     if $TLEVEL do ;test allows handler use outside of TP
     . trollback
     . write "Rolled back transaction."
     write !
     set $ecode=""
     zgoto retlvl

Results:

.. code-block:: none

   Start with $ZMAXTPTIME=6:
   ^X=1,will set ^Y to 3 in 3 seconds...^Y=3...committed.
   ^X=2,will set ^Y to 5 in 5 seconds...^Y=5...committed.
   ^X=3,will set ^Y to 7 in 7 seconds...
   In $ETRAP handler. Error was:
   150377322,longtran+7^tptime,%YDB-E-TPTIMEOUT, Transaction timeoutRolled back transaction.
   ^X=3,will set ^Y to 9 in 9 seconds...
   In $ETRAP handler. Error was:
   150377322,longtran+7^tptime,%YDB-E-TPTIMEOUT, Transaction timeoutRolled back transaction.
   Done TP Timeout test.

--------------
$ZMODE
--------------

$ZMO[DE] contains a string value indicating the process execution mode.

The mode can be:

* INTERACTIVE
* OTHER

M routines cannot modify $ZMODE.

Example:

.. code-block:: bash

   YDB>WRITE $ZMODE
   INTERACTIVE

This displays the process mode.

--------------------
$ZONLNRLBK
--------------------

$ZONLNRLBK increments every time a process detects a concurrent MUPIP JOURNAL -ONLINE -ROLLBACK.

YottaDB initializes $ZONLNRLBK to zero (0) at process startup. YottaDB does not permit the SET or NEW commands to modify $ZONLNRLBK.

For more information on online rollback, refer to the -ONLINE qualifier of -ROLLBACK in the `Administration and Operations Guide <../AdminOpsGuide/index.html>`_.

.. _zpatnumeric-isv:

-------------------
$ZPATNUMERIC
-------------------

$ZPATN[UMERIC] is a read-only intrinsic special variable that determines how YottaDB interprets the patcode "N" used in the pattern match operator.

With $ZPATNUMERIC="UTF-8", the patcode "N" matches any numeric character as defined by UTF-8 encoding. With $ZPATNUMERIC="M", YottaDB restricts the patcode "N" to match only ASCII digits 0-9 (that is, ASCII 48-57). When a process starts in UTF-8 mode, intrinsic special variable $ZPATNUMERIC takes its value from the environment variable ydb_patnumeric. YottaDB initializes the intrinsic special variable $ZPATNUMERIC to "UTF-8" if the environment variable ydb_patnumeric is defined to "UTF-8". If the environment variable ydb_patnumeric is not defined or set to a value other than "UTF-8", YottaDB initializes $ZPATNUMERIC to "M".

YottaDB populates $ZPATNUMERIC at process initialization from the environment variable ydb_patnumeric and does not allow the process to change the value.

.. note::
   YottaDB performs operations on literals at compile time and the pattern codes settings may have an impact on such operations. Therefore, always compile with the same pattern code settings as those used at runtime.

For UTF-8 characters, YottaDB assigns patcodes based on the default classification of the Unicode® character set by the ICU library with three adjustments:

* If $ZPATNUMERIC is not "UTF-8", non-ASCII decimal digits are classified as A.
* Non-decimal numerics (Nl and No) are classified as A.
* The remaining characters (those not classified by ICU functions: u_isalpha, u_isdigit, u_ispunct, u_iscntrl, the above options) are classified into either patcode P or C. The ICU function u_isprint is used since it returns "TRUE" for non-control characters.

The following table contains the resulting Unicode general category to M patcode mapping:

+-------------------------------------------+---------------------------------------------------+
| Unicode General Category                  | Patcode Class                                     |
+===========================================+===================================================+
| L* (all letters)                          | A                                                 |
+-------------------------------------------+---------------------------------------------------+
| M* (all marks)                            | P                                                 |
+-------------------------------------------+---------------------------------------------------+
| Nd (decimal numbers)                      | N (if decimal digit is ASCII or $ZPATNUMERIC is   |
|                                           | "UTF-8", otherwise A                              |
+-------------------------------------------+---------------------------------------------------+
| Nl (letter numbers)                       | A (examples of Nl are Roman numerals)             |
+-------------------------------------------+---------------------------------------------------+
| No (other numbers)                        | A (examples of No are fractions)                  |
+-------------------------------------------+---------------------------------------------------+
| P* (all punctuation)                      | P                                                 |
+-------------------------------------------+---------------------------------------------------+
| S* (all symbols)                          | P                                                 |
+-------------------------------------------+---------------------------------------------------+
| Zs (spaces)                               | P                                                 |
+-------------------------------------------+---------------------------------------------------+
| Zl (line separators)                      | C                                                 |
+-------------------------------------------+---------------------------------------------------+
| Zp (paragraph separators)                 | C                                                 |
+-------------------------------------------+---------------------------------------------------+
| C* (all control code points)              | C                                                 |
+-------------------------------------------+---------------------------------------------------+

For a description of the Unicode general categories, refer to http://unicode.org/charts/.

Example:

.. code-block:: bash

   YDB>write $zpatnumeric
   UTF-8
   YDB>Write $Char($$FUNC^%HD("D67"))?.N ; This is the Malayalam decimal digit 1
   1
   YDB>Write 1+$Char($$FUNC^%HD("D67"))
   1
   YDB>Write 1+$Char($$FUNC^%HD("31")) ; This is the ASCII digit 1
   2

.. _zpin-isv:

----------------
$ZPIN
----------------

When $PRINCIPAL has different input/output devices, the USE command recognizes intrinsic special variable $ZPIN to apply appropriate deviceparameters to the input side of $PRINCIPAL. A USE with $ZPIN sets $IO to $PRINCIPAL for READs and WRITEs from the input and output side of $PRINCIPAL. $ZSOCKET() also accepts $ZPIN as its first argument and, if the device is a split SOCKET device, supplies information on the input SOCKET device. In any context other than USE or $ZSOCKET(), or if $PRINCIPAL is not a split device, $PRINCIPAL, $ZPIN and $ZPOUT are synonyms. In the case of a split $PRINCIPAL, $ZPIN returns the value of $PRINCIPAL followed by the string "< /" Any attempt to OPEN $ZPIN results in a DEVOPENFAIL error.

For more information refer to :ref:`principal-io-isv`, :ref:`zpout-isv`, and :ref:`zsocket-function`.

.. _zposition-isv:

-----------------
$ZPOSITION
-----------------

$ZPOS[ITION] contains a string value specifying the current entryref, where entryref is [label][+offset]^routine, and the offset is evaluated from the closest preceding label.

YottaDB does not permit the SET or NEW commands to modify $ZPOSITION.

Example:

.. code-block:: bash

   YDB>WRITE !,$ZPOS,! ZPRINT @$ZPOS

This example displays the current location followed by the source code for that line.

.. _zpout-isv:

-----------------
$ZPOUT
-----------------

When $PRINCIPAL has different input/output devices, the USE command recognizes intrinsic special variables $ZPOUT to apply appropriate deviceparameters to the output side of $PRINCIPAL. A USE with $ZPOUT sets $IO to $PRINCIPAL for READs and WRITEs from the input and output side of $PRINCIPAL. $ZSOCKET() also accepts $ZPOUT as its first argument and, if the device is a split SOCKET device, supplies information on the output SOCKET device. In any context other than USE or $ZSOCKET(), or if $PRINCIPAL is not a split device, $PRINCIPAL, $ZPIN and $ZPOUT are synonyms. In the case of a split $PRINCIPAL, $ZPOUT returns the value of $PRINCIPAL followed by the string "> /" Any attempt to OPEN $ZPOUT results in a DEVOPENFAIL error.

For more information refer to :ref:`principal-io-isv`, :ref:`zpin-isv`, and :ref:`zsocket-function`.

Example:

.. code-block:: none

   ;zpioin
   ;123456789012345678901234567890123456789012345678901234567890
   ;A12345678901234567890123456789012345678901234567890123456789
   zpio
     ; yottadb -r zpio < zpioin
     write "$PRINCIPAL = ",$P,!
     write "$ZPIN = ",$ZPIN,!
     write "$ZPOUT = ",$ZPOUT,!
     write "Read first line from zpioin with default settings",!
     read x
     write x,!
     zshow "d"
     use $ZPIN:(wrap:width=50)
     write "After $ZPIN set to wrap and width set to 50",!
     zshow "d"
     write "Read next 50 characters from zpioin",!
     read y
     write y,!
     use $ZPOUT:wrap
     use $ZPIN:nowrap
     write "After $ZPOUT set to wrap and $ZPIN set to nowrap",!
     zshow "d"
     use $ZPOUT:nowrap
     write "After $ZPOUT set to nowrap",!
     zshow "d"
     use $P:wrap
     write "After $P set to wrap",!
     zshow "d"
     use $ZPOUT:width=40
     write "After $ZPOUT width set to 40",!
     zshow "d"
     use $ZPOUT:nowrap
     write "After $ZPOUT set to nowrap",!
     zshow "d"
     write x,!
     quit


.. _zprompt-isv:

--------------
$ZPROMPT
--------------

$ZPROM[PT] contains a string value specifying the current Direct Mode prompt. By default, YDB> is the Direct Mode prompt. M routines can modify $ZPROMPT by means of a SET command. $ZPROMPT cannot exceed 16 characters. If an attempt is made to assign $ZPROMPT to a longer string, only the first 16 characters will be taken.

In UTF-8 mode, if the 31st byte is not the end of a valid UTF-8 character, YottaDB truncates the $ZPROMPT value at the end of the last character that completely fits within the 31 byte limit.

The environment variable ydb_prompt initializes $ZPROMPT at process startup.

Example:

.. code-block:: bash

   YDB>set $zprompt="Test01">"
   Test01>set $zprompt="YDB>"

This example changes the YottaDB prompt to Test01> and then back to YDB>.

------------------
$ZQUIT
------------------

$ZQUIT indicates whether the functionality associated with the ydb_zquit_anyway environment variable is enabled. If the environment variable ydb_zquit_anyway is defined and evaluates to 1 or any case-independent string or leading substrings of "TRUE" or "YES", YottaDB sets $ZQUIT to 1 and executes code of the form QUIT <expr< as if it were SET <tmp>=<expr> QUIT:$QUIT tmp QUIT, where <tmp> is a temporary local variable in the YottaDB runtime system that is not visible to the application code. The value of $ZQUIT is a compile time setting, i.e., it can be set during run-time, but it has no effect on the behavior existing object code - it only effects code generated by auto-ZLINK or the ZCOMPILE command. This setting has no effect on late bound code such as that in XECUTE arguments, $ETRAP and $ZTRAP device EXCEPTIONS.

If ydb_zquit_anyway is not defined or evaluates to 0 or any case-independent string or leading substrings of "FALSE" or "NO", YottaDB sets $ZQUIT to 0 and executes QUIT <expr> as specified by the standard. This feature is helpful when you want to run a block of code both as a routine and as an extrinsic function. Consider the following example:

.. code-block:: bash

   YDB>zprint ^rtnasfunc
   rtnasfunc(expr1)
   write "Hello ",expr1,!
   quit

When the ydb_zquit_anyway functionality is diabled, extrinsic function invocations return an error as per the standard.

.. code-block:: bash

   YDB>write $zquit
   0
   YDB>write $$^rtnasfunc("jdoe")
   Hello jdoe
   %YDB-E-QUITARGREQD, Quit from an extrinsic must have an argument

However, if you enable the ydb_zquit_anyway feature, extrinsic function invocations do not return an error.

.. code-block:: bash

   YDB>write $zquit
   1
   YDB>write $$^rtnasfunc("jdoe")
   Hello jdoe
   YDB>

.. _zrealstor-isv:

---------------
$ZREALSTOR
---------------

$ZREALSTOR contains the total memory (in bytes) allocated by the YottaDB process, which may or may not actually be in use. It provides one view (see also :ref:`zallocstor-isv` and :ref:`zusedstor-isv`) of the process memory utilization and can help identify storage related problems. YottaDB does not permit $ZREALSTOR to be SET or NEWed.

---------------
$ZRELDATE
---------------

$ZRELDATE provides the UTC date/time of the YottaDB build in the form YYYYMMDD 24:60 ffffffffffffffffffffffffffffffffffffffff (where date/time stamp is in $ZDATE() notation). The third part of the timestamp is the hexadecimal hash assigned by the version control system to identify the last source code commit. While $ZYRELEASE is probably a better identifier for most uses, $ZRELDATE may be a helpful alternative for those testing pre-release builds.

For example:

.. code-block:: bash

   YDB>write $zreldate
   20200813 12:10 177eb8e48098204dafe564cac2bcb84312b2853a

.. _zroutines-isv:

-----------------
$ZROUTINES
-----------------

$ZRO[UTINES] contains a string value specifying a directory or list of directories containing object files. Each object directory may also have an associated directory, or list of directories, containing the corresponding source files. These directory lists are used by certain YottaDB functions, primarily auto-ZLINK, to locate object and source files. The order in which directories appear in a given list determines the order in which they are searched for the appropriate item.

Any environment variables that are part of the string used to set $ZROUTINES, either at process startup or explicitly during execution, are expanded to absolute path names. This eliminates the need for application code using $ZROUTINES to do the expansion.

Searches that use $ZROUTINES treat files as either object or source files. YottaDB treats files with an extension of .o as object files and files with an extension of .m as source files.

.. note::
   Paths used in $ZROUTINES to locate routines must not include embedded spaces, as $ZROUTINES uses spaces as delimiters.

++++++++++++++++++++++++++++++++++++++++++
Establishing the value from $ydb_routines
++++++++++++++++++++++++++++++++++++++++++

If the environment variable :code:`ydb_routines` is not set when the :code:`yottadb` process starts, or if it is set to the empty string (:code:`""`), YottaDB sets it in the environment to :code:`$ydb_dist/libyottadbutil.so` in M mode, if it exists, or to :code:`$ydb_dist/utf8/libyottadbutil.so` in UTF-8 mode, if it exists, and to :code:`$ydb_dist` if it does not, and then uses that value.

Commands or functions such as DO, GOTO, ZGOTO, ZBREAK, ZPRINT, and $TEXT may auto-ZLINK and thereby indirectly use $ZROUTINES. If their argument does not specify a directory, ZEDIT and explicit ZLINK use $ZROUTINES. ZPRINT and $TEXT use $ZROUTINES to locate a source file if YottaDB cannot find the source file pointed to by the object file. For more information on ZLINK and auto-ZLINK, see the `"Development Cycle" <devcycle.html>`_ and `"Commands" <commands.html>`_ chapters.

+++++++++++++++++++++++++++++++
Setting a Value for $ZROUTINES
+++++++++++++++++++++++++++++++

$ZRO[UTINES] is a read-write Intrinsic Special Variable, so M can also SET the value.

By default, each directory entry in $ZROUTINES is assumed to contain both object and source files. However, each object directory may have an associated directory or list of directories to search for the corresponding source files. This is done by specifying the source directory list, in parentheses, following the object directory specification.

If the command specifies more than one source directory for an object directory, the source directories must be separated by spaces, and the entire list must be enclosed in parentheses ( ) following the object directory specification. If the object directory should also be searched for source, the name of that directory must be included in the parentheses (usually as the first element in the list). Directory-specifications may also include empty parentheses, directing YottaDB to proceed as if no source files exist for objects located in the qualified directory.

To set $ZROUTINES outside of M, use the appropriate shell command to set ydb_routines. Because ydb_routines is a list, enclose the value in quotation marks (" ").

Changes to the value of $ZROUTINES during a YottaDB invocation only last for the current invocation, and do not change the value of ydb_routines.

Directory specifications may include an environment variable. When YottaDB SETs $ZROUTINES, it translates all environment variables and verifies the syntax and the existence of all specified directories. If $ZROUTINES is set to an invalid value, YottaDB generates a run-time error and does not change the value of $ZROUTINES. Because the environment variables are translated when $ZROUTINES is set, any changes to their definition have no effect until $ZROUTINES is set again.

++++++++++++++++++++++++++++
$ZROUTINES Examples
++++++++++++++++++++++++++++

Example:

.. code-block:: bash

   YDB>s $zroutines=".(../src) $ydb_dist"

This example directs YottaDB to look for object modules first in your current directory, then in the distribution directory that contains the percent routines. YottaDB locates sources for objects in your current directory in the sibling /src directory.

Example:

.. code-block:: bash

   $ ydb_routines="/usr/jones /usr/smith"
   $ export ydb_routines
   $ ydb
   YDB>write $zroutines
   "/usr/jones /usr/smith"
   YDB>set $zro="/usr/jones/utl /usr/smith/utl"
   YDB>write $zroutines
   "/usr/jones/utl /usr/smith/utl"
   YDB>halt
   $ echo $ydb_routines
   /usr/jones /usr/smith

This example defines the environment variable ydb_routines. Upon entering YottaDB Direct Mode $zroutines has the value supplied by ydb_routines. The SET command changes the value. When the YottaDB image terminates, the shell echo command demonstrates that ydb_routines has not been modified by the M SET command.

Example:

.. code-block:: bash

   YDB>SET $ZRO=". /usr/smith"

This example sets $zroutines to a list containing two directories.

Example:

.. code-block:: bash

   YDB>SET $ZRO=". /usr/smith"

This example sets $zroutines to a list containing two directories.

Example:

.. code-block:: bash

   YDB>set $zro="/usr/smith(/usr/smith/tax /usr/smith/fica)"

This example specifies that YottaDB should search the directory /usr/smith for object files, and the directories /usr/smith/tax and /usr/smith/fica for source files. Note that in this example. YottaDB does not search /usr/smith for source files.

Example:

.. code-block:: bash

   YDB>set $zro="/usr/smith(/usr/smith /usr/smith/tax /usr/smith/fica)"

This example specifies that YottaDB should search the directory /usr/smith for object files and the directories /usr/smith/tax and /usr/smith/fica for source files. Note that the difference between this example and the previous one is that YottaDB searches /usr/smith for both object and source files.

Example:

.. code-block:: bash

   YDB>set $zro="/usr/smith /usr/smith/tax() /usr/smith/fica"

This specifies that YottaDB should search /usr/smith and /usr/smith/fica for object and source files. However, because the empty parentheses indicate directories searched only for object files, YottaDB does not search /usr/smith/tax for source files.

Omitting the parentheses indicates that YottaDB can search the directory for both source and object files. $ZROUTINES=/usr/smith is equivalent to $ZROUTINES=/usr/smith(/usr/smith).

++++++++++++++++++++++++++++
$ZROUTINES Search Types
++++++++++++++++++++++++++++

YottaDB uses $ZRO[UTINES] to perform three types of searches:

* Object-only when the command or function using $ZROUTINES requires a .o file extension.
* Source-only when the command or function using $ZROUTINES requires a file extension other than .o.
* Object-source match when the command or function using $ZROUTINES does not specify a file extension.

An explicit ZLINK that specifies a non .OBJ .o extension is considered to be a function that has not specified a file extension for the above searching purposes.

All searches proceed from left to right through $ZROUTINES. By default, YottaDB searches directories for both source and object files. YottaDB searches directories followed by empty parentheses ( ) for object files only. YottaDB searches directories in parentheses only for source files.

Once an object-matching search locates an object file, the source search becomes limited. If the directory containing the object file has an attached parenthetical list of directories, YottaDB only searches the directories in the attached list for matching source files. If the directory containing the object files does not have following parentheses, YottaDB restricts the search for matching source files to the same directory. If the object module is in a directory qualified by empty parentheses, YottaDB cannot perform any operation that refers to the source file.

The following table shows YottaDB commands and functions using $ZROUTINES and the search types they support.

**YottaDB Commands and $ZROUTINES Search Types**

+---------------------------------------+-----------------------------------------+-------------------------------+--------------------------------+-------------------------------+
| Search / Function                     | File Extension Specified                | Search Type                                                                                    |
+=======================================+=========================================+===============================+================================+===============================+
|                                       |                                         | **Obj Only**                  | **Source Only**                | **Match**                     |
+---------------------------------------+-----------------------------------------+-------------------------------+--------------------------------+-------------------------------+
| EXPLICIT ZLINK                        | .o                                      | X                             |                                |                               |
+---------------------------------------+-----------------------------------------+-------------------------------+--------------------------------+-------------------------------+
|                                       | Not .o                                  |                               |                                | X                             |
+---------------------------------------+-----------------------------------------+-------------------------------+--------------------------------+-------------------------------+
|                                       | None                                    |                               |                                | X                             |
+---------------------------------------+-----------------------------------------+-------------------------------+--------------------------------+-------------------------------+
| AUTO-ZLINK                            | None                                    |                               |                                | X                             |
+---------------------------------------+-----------------------------------------+-------------------------------+--------------------------------+-------------------------------+
| ZEDIT                                 | Not .o                                  |                               | X                              |                               |
+---------------------------------------+-----------------------------------------+-------------------------------+--------------------------------+-------------------------------+
| ZPRINT                                | None                                    |                               | X                              |                               |
+---------------------------------------+-----------------------------------------+-------------------------------+--------------------------------+-------------------------------+
| $TEXT                                 | None                                    |                               | X                              |                               |
+---------------------------------------+-----------------------------------------+-------------------------------+--------------------------------+-------------------------------+

If ZPRINT or $TEXT() require a source module for a routine that is not in the current image, YottaDB first performs an auto-ZLINK with a matching search.

ZPRINT or $TEXT locate the source module using a file specification for the source file located in the object module. If YottaDB finds the source module in the directory where it was when it was compiled, the run-time system does not use $ZROUTINES. If YottaDB cannot find the source file in the indicated location, the run-time system uses $ZROUTINES.

++++++++++++++++++++++++++++++++
$ZROUTINES Search Examples
++++++++++++++++++++++++++++++++

This section describes a model for understanding $ZROUTINES operations and the illustrating examples, which may assist you if you wish to examine the topic closely.

You may think of $ZROUTINES as supplying a two dimensional matrix of places to look for files. The matrix has one or more rows. The first row in the matrix contains places to look for object files and the second and following rows contain places to look for source files. Each column represents the set of places that contain information related to the object modules in the first row of the column.

Example:

.. code-block:: bash

   YDB>s $zro=". /usr/smi/utl() /usr/jon/utl
   (/usr/jon/utl/so /usr/smi/utl)"

The following table illustrates the matrix view of this $ZROUTINES.

**$ZROUTINES Search Matrix**

+---------------------+----------------------------+------------------------------+--------------------------+
| Search For          | Column 1                   | Column 2                     | Column 3                 |
+=====================+============================+==============================+==========================+
| OBJECTS             | \.                         | /usr/smi/utl                 | /usr/jon/utl             |
+---------------------+----------------------------+------------------------------+--------------------------+
| SOURCE              | \.                         |                              | /usr/jon/utl/so          |
+---------------------+----------------------------+------------------------------+--------------------------+
|                     |                            |                              | /usr/smi/utl             |
+---------------------+----------------------------+------------------------------+--------------------------+

To perform object-only searches, YottaDB searches only the directories or object libraries in the top 'objects' row for each column starting at column one. If YottaDB does not locate the object file in a directory or object library in the 'objects' row of a column, YottaDB begins searching again in the next column. If YottaDB cannot locate the file in any of the columns, it issues a run-time error.

As illustrated in the preceding table, YottaDB searches for object files in the directories . ,/usr/smi/utl and /usr/jon/utl.

To perform source-only searches, YottaDB looks down to the 'source' row at the bottom of each column, excluding columns headed by object-only directories (that is, those object directories, which consist of an empty list of source directories) or object libraries. If YottaDB cannot locate the source file in the 'source' row of a column, it searches the next eligible column.

To perform object-source match searches, YottaDB looks at each column starting at column one. YottaDB does an object-only search in the 'objects' row of a column and a source-only search in the 'source' row(s) of a column. If YottaDB locates either the object-file or the source-file, the search is completed. Else, YottaDB starts searching the next column. If YottaDB cannot locate either the object file or the source file in any of the columns, it issues a run-time error.

As illustrated in the preceding table, YottaDB searches for source-files in the directory "." in column one. If YottaDB cannot locate the source file in ".", it omits column two because it is an object-only directory and instead searches column three. Since column three specifies /usr/jon/utl/so and /usr/smi/utl, YottaDB searches for the source-file in these directories in column three and not in /usr/jon/utl. If YottaDB cannot locate the source-file in column three, it terminates the search and issues a run-time error.

Once the object-source match search is done, YottaDB now has either the object-file or source-file or both available. YottaDB then recompiles the source-file based on certain conditions, before linking the object-file into the current image. See "ZLink" for more information on those conditions.

If auto-ZLINK or ZLINK determines that the source file requires [re]compilation, YottaDB places the object file in the above object directory in the same column as the source file. For example, if YottaDB locates the source file in /usr/smi/utl in column three, YottaDB places the resultant object file in /usr/jon/utl.

++++++++++++++++++++++++++++++++++++++++++++++++
Shared Library File Specification in $ZROUTINES
++++++++++++++++++++++++++++++++++++++++++++++++

The $ZROUTINES ISV allows individual UNIX shared library file names to be specified in the search path. During a search for auto-ZLINK, when a shared library is encountered, it is probed for a given routine and, if found, that routine is linked/loaded into the image. During an explicit ZLINK, all shared libraries in $ZROUTINES are ignored and are not searched for a given routine.

$ZROUTINES syntax contains a file-specification indicating shared library file path. YottaDB does not require any designated extension for the shared library component of $ZROUTINES. Any file specification that does not name a directory is treated as a shared library. However, it is recommended that the extension commonly used on a given platform for shared library files be given to any YottaDB shared libraries. See "Linking YottaDB Shared Images". A shared library component cannot specify source directories. YottaDB reports an error at an attempt to associate any source directory with a shared library in $ZROUTINES.

The following traits of $ZROUTINES help support shared libraries:

* The $ZROUTINES search continues to find objects in the first place, processing from left to right, that holds a copy; it ignores any copies in subsequent locations. However, now for auto-ZLINK, shared libraries are accepted as object repositories with the same ability to supply objects as directories.
* Explicit ZLINK, never searches Shared Libraries. This is because explicit ZLINK is used to link a newly created routine or to re-link a modified routine and there is no mechanism to load new objects into an active shared library.
* ZPRINT and $TEXT() of the routines in a shared library, read source file path from the header of the loaded routine. If the image does not contain the routine, an auto-ZLINK is initiated. If the source file path recorded in the routine header when the module was compiled cannot be located, ZPRINT and $TEXT() initiate a search from the beginning of $ZROUTINES, skipping over the shared library file specifications. If the located source does not match the code in image (checked via checksum), ZPRINT generates an object-source mismatch status and $TEXT() returns a null string.
* ZEDIT, when searching $ZROUTINES, skips shared libraries like explicit ZLINK for the same reasons. $ZSOURCE ISV is implicitly set to the appropriate source file.

For example, if libshare.so is built with foo.o compiled from ./shrsrc/foo.m, the following commands specify that YottaDB should search the library ./libshare.so for symbol foo when do ^foo is encountered.

.. code-block:: bash

   YDB>SET $ZROUTINES="./libshare.so ./obj(./shrsrc)"
   YDB>DO ^foo;auto-ZLINK foo - shared
   YDB>ZEDIT "foo";edit ./shrsrc/foo.m
   YDB>W $ZSOURCE,!;prints foo
   YDB>ZPRINT +0^foo;issues a source-object mismatch status TXTSRCMAT error message
   YDB>ZLINK "foo";re-compile ./shrsrc/foo.m to generate ./obj/foo.o.
   YDB>W $TEXT(+0^foo);prints foo

Note that ZPRINT reports an error, as foo.m does not match the routine already linked into image. Also note that, to recompile and re-link the ZEDITed foo.m, its source directory needs to be attached to the object directory [./obj] in $ZROUTINES. The example assumes the shared library (libshare.so) has been built using shell commands. For the procedure to build a shared library from a list of YottaDB generated object (.o) files, see "Linking YottaDB Shared Images" below.


**Linking YottaDB Shared Images**

Following are the steps (UNIX system commands, and YottaDB commands) that need to be taken to use YottaDB shared image linking with $ZROUTINES.

* *Compile source (.m) files to object (.o) files*

In order to share M routines, YottaDB generates objects (.o) with position independent code, a primary requirement for shared libraries. No change to the compiling procedures is needed. However, any objects generated by a previous release must be recompiled.

* *Create a shared library from object (.o) files*

To create a shared library, use the following syntax:

.. code-block:: none

   ld -shared -o libshr.so file1.o file2.o

Where libshr.so is replaced with name of the shared library one wishes to create. The file1.o and file2.o are replaced with one or more object files created by the YottaDB compiler that the user wishes to put into the shared library. Refer to the ld man page on specific platform for details.

.. note::
   Source directories cannot be specified with a shared library in $ZROUTINES, as YottaDB does not support additions or modifications to active shared libraries. Searching for a routine in a shared library is a two step process: (1) Load the library and (2) Lookup the symbol corresponding to the M entryref. Since YottaDB always performs the first step (even on platforms with no shared binary support), use shared libraries in $ZROUTINES with care to keep the process footprint minimal. On all platforms, it is strongly recommended not to include unused shared libraries in $ZROUTINES.

* *Establish $ZROUTINES from ydb_routines*

When the environment variable ydb_routines is defined, YottaDB initializes $ZROUTINES to the value of ydb_routines. The $ZROUTINES ISV can also be modified using SET command.

Example:

.. code-block:: bash

   $ ydb_routines="./libabc.so ./obj(./src)"
   $ export ydb_routines
   $ yottadb -direct
   YDB>w $ZROUTINES,!;writes "./libabc.so ./obj(./src)"
   YDB>do ^a;runs ^a from libabc.so
   YDB>do ^b;runs ^b from libabc.so
   YDB>do ^c;runs ^c from libabc.so
   YDB>h
   $

* *$ZROUTINES settings for auto-relink*

By suffixing one or more directory names in $ZROUTINES with a single asterisk (*), processes can subscribe to updates of object files published in those directories. At the invocation of DO, GOTO, or ZGOTO, extrinsic functions, $TEXT(), or ZPRINT that specify an entryref which includes a routine name (vs. a label without a routine name), yottadb processes (and mupip processes executing trigger logic) automatically relink ("auto-relink") and execute published new versions of routines.

* Label references (that is, without a routine name), whether direct or through indirection, always refer to the current routine, and do not invoke auto-relink logic.
* Use shell quoting rules when appending asterisks to directory names in the ydb_routines environment variable - asterisks must be passed in to YottaDB, and not expanded by the shell.
* YottaDB accepts but ignores asterisk suffixes to directory names on 32-bit Linux on x86 platforms, where it does not provide auto-relinking.
* Changing $ZROUTINES causes all routines linked from auto-relink-enabled directories in the process to be re-linked.
* Note that a relink does not automatically reload a routine every time. When YottaDB initiates a relink and the object file (object hash) is the same as the existing one, YottaDB bypasses the relink and uses the existing object file.

The ZRUPDATE command publishes new versions of routines to subscribers.

.. _zsource-isv:

--------------------
$ZSOURCE
--------------------

$ZSO[URCE] contains a string value specifying the default pathname for the ZEDIT and ZLINK commands. ZEDIT or ZLINK without an argument is equivalent to ZEDIT/ZLINK $ZSOURCE.

$ZSOURCE initially contains the null string. When ZEDIT and ZLINK commands have a pathname for an argument, they implicitly set $ZSOURCE to that argument. This ZEDIT/ZLINK argument can include a full pathname or a relative one. A relative path could include a file in the current directory, or the path to the file from the current working directory. In the latter instance, do not include the slash before the first directory name. $ZSOURCE will prefix the path to the current working directory including that slash.

The file name may contain a file extension. If the extension is .m or .o, $ZSOURCE drops it. The ZEDIT command accepts arguments with extensions other than .m or .o. $ZSOURCE retains the extension when such arguments are passed.

If $ZSOURCE contains a file with an extension other than .m or .o, ZEDIT processes it but ZLINK returns an error message

$ZSOURCE is a read-write Intrinsic Special Variable, (i.e., it can appear on the left side of the equal sign (=) in the argument to the SET command). A $ZSOURCE value may include an environment variable. YottaDB handles logical names that translate to other logical names by performing iterative translations.

Example:

.. code-block:: bash

   YDB>ZEDIT "subr.m"
   .
   .
   YDB>WRITE $ZSOURCE
   subr

Example:

.. code-block:: bash

   YDB>ZEDIT "test"
   .
   .
   .
   YDB>WRITE $ZSOURCE
   "test"

Example:

.. code-block:: bash

   YDB>ZEDIT "/usr/smith/report.txt"
   .
   .
   .
   YDB>WRITE $ZSOURCE
   /usr/smith/report.txt

Example:

.. code-block:: bash

   YDB>ZLINK "BASE.O"
   .
   .
   .
   YDB>WRITE $ZSOURCE
   BASE

.. _zstatus-isv:

-------------------
$ZSTATUS
-------------------

$ZS[TATUS] contains a string value specifying the error condition code and location of the last exception condition that occurred during routine execution.

YottaDB maintains $ZSTATUS as a string consisting of three or more substrings. The string consists of the following:

* An error message number as the first substring.
* The entryref of the line in error as the second substring; a comma (,) separates the first and second substrings.
* The message detail as the third substring. The format of this is a percent sign (%) identifying the message facility, a hyphen (-) identifying the error severity, another hyphen identifying the message identification followed by a comma (,), which is followed by the message text if any:

.. code-block:: none

   Format: %<FAC>-<SEV>-<ID>, <TEXT>
   Example: %YDB-E-DIVZERO, Attempt to divide by zero

YottaDB sets $ZSTATUS when it encounters errors during program execution, but not when it encounters errors in a Direct Mode command.

$ZSTATUS is a read-write Intrinsic Special Variable, (i.e., it can occur on the left side of the equal sign (=) in the argument to the SET command). While it will accept any string, YottaDB recommends setting it to null. M routines cannot modify $ZSTATUS with the NEW command.

Example:

.. code-block:: bash

   YDB>WRITE $ZSTATUS
   150373110,+1^MYFILE,%YDB-E-DIVZERO,
   Attempt to divide by zero

This example displays the status generated by a divide by zero (0).

.. _zstep-isv:

---------------
$ZSTEP
---------------

$ZST[EP] contains a string value specifying the default action for the ZSTEP command. $ZSTEP provides the ZSTEP action only when the ZSTEP command does not specify an action.

$ZSTEP initially contains the the value of the $ydb_zstep environment variable or string "B" if $ydb_zstep is not defined; note that the default "B" causes the process to enter direct mode. $ZSTEP is a read-write Intrinsic Special Variable, (that is, it can appear on the left side of the equal sign (=) in the argument to the SET command).

Example:

.. code-block:: bash

   YDB>WRITE $ZSTEP
   B
   YDB>

This example displays the current value of $ZSTEP, which is the default.

Example:

.. code-block:: bash

   YDB>SET $ZSTEP="ZP @$ZPOS B"

This example sets $ZSTEP to code that displays the contents of the next line to execute, and then enters Direct Mode.

.. _zstrpllim-isv:

-----------------
$ZSTRPLLIM
-----------------

$ZSTRP[LLIM] provides a way for a process to limit its process private memory used for local variable and scratch storage. When the value is 0 or negative, the default, there is no limit. A positive value specifies a byte limit. An attempt to assign a value smaller than the minimum value (150,000 bytes) will assign the minimum value to $ZSTRPLLIM. When a request for additional memory exceeds the limit, YottaDB does the expansion and then produces an STPCRIT error. By default, a later request for memory produces an STPOFLOW, unless subsequent to STPCRIT , $ZSTRPLLIM has been set to the same or higher limit. Note that YottaDB allocates memory in large blocks so the interaction of $ZSTRPLLIM with memory growth is not exact. When the ydb_string_pool_limit environment variable specifies a positive value, YottaDB uses it for the initial value of $ZSTRPLLIM.

.. _zsystem-isv:

-----------------
$ZSYSTEM
-----------------

$ZSY[STEM] holds the value of the status code for the last subprocess invoked with the ZSYSTEM command.

---------------
$ZTEXIT
---------------

$ZTE[XIT] contains an expression that controls the YottaDB interrupt facility at the transaction commit or rollback. At each outermost TCOMMIT or TROLLBACK, If +$ZTEXIT evaluates to non-zero (TRUE), then $ZINTERRUPT is XECUTEd after completing the commit or rollback.

$ZTEXIT is a read-write ISV, that is, it can appear on the left side of the equal sign (=) in the argument to the SET command. M routines cannot NEW $ZTEXIT. YottaDB initializes $ZTEXIT to null at the process startup. Note that the changes to the value of $ZTEXIT during a YottaDB invocation last for the entire duration of the process, so it is the application's responsibility to reset $ZTEXIT after $ZINTERRUPT is delivered in order to turn off redelivering the interrupt every subsequent transaction commit or rollback.

Example:

.. code-block:: bash

   $ export sigusrval=10
   $ ydb
   YDB>zprint ^ztran
   foo;
     set $ztexit=1
     set $zinterrupt="d ^throwint"
     tstart ()
     for i=1:1:10 do
     . set ^ACN(i,"bal")=i*100
     tstart ()
     do ^throwint
     ;do ^proc
     tcommit:$tlevel=2
     for i=1:1:10 do
     . set ^ACN(i,"int")=i*0.05
     ;do ^srv
     if $tlevel trollback
     ;do ^exc
     set $ztexit="",$zinterrupt=""
     quit
   bar;
     write "Begin Transaction",!
     set $ztexit=1
     tstart ()
     i '$zsigproc($j,$ztrnlnm("sigusrval")) write "interrupt sent...",!!
     for i=1:1:4 set ^B(i)=i*i
     tcommit
     write "End Transaction",!
     ;do ^srv
     quit
   YDB>zprint ^throwint
   throwint
     set $zinterrupt="write !,""interrupt occurred at : "",$stack($stack-1,""PLACE""),! set $ztexit=1"
     if '$zsigproc($job,$ztrnlnm("sigusrval")) write "interrupt sent to process"
     write "***************************************",!!
     quit
   YDB>do foo^ztran
   interrupt sent to process
   interrupt occurred at : throwint+3^throwint
   ***************************************
   interrupt occurred at : foo+13^ztran
   YDB>

In the above call to foo^ztran, the interrupt handler is a user-defined routine, throwint. The process is sent a signal (SIGUSR1), and $ZINTERRUPT is executed. At the outermost trollback, the interrupt is rethrown, causing $ZINTERRUPT to be executed again.

Example:

.. code-block:: bash

   YDB>w $zinterrupt
   "IF $ZJOBEXAM()"
   YDB>zsystem "ls YDB_JOBEXAM*"
   ls: No match.
   YDB>do bar^ztran
   Begin Transaction
   interrupt sent...
   End Transaction
   YDB>zsystem "ls YDB_JOBEXAM*"
   YDB_JOBEXAM.ZSHOW_DMP_3951_1  YDB_JOBEXAM.ZSHOW_DMP_3951_2
   YDB>

This uses the default value of $ZINTERRUPT to service interrupts issued to the process. The $ZJOBEXAM function executes a ZSHOW "*", and stores the output in each YDB_ZJOBEXAM_ZSHOW_DMP for the initial interrupt, and at tcommit when the interrupt is rethrown.

----------------
$ZTIMEOUT
----------------

$ZTIMEOUT controls a single process wide timer. The format of the $ZTIMEOUT ISV is:

.. code-block:: none

   $ZTIMeout=([timeout][:expr])

For SET of $ZTIMEOUT:

* If the optional timeout is specified:

  * A positive value specifies a time in seconds, with millisecond resolution, how long from the current time, the timer interrupts the process. When the timer interrupts the process, it XECUTEs the vector.
  * A negative value cancels the timer.
  * A zero value causes the vector to be XECUTEd immediately.

* If the timeout is not specified, a colon (:) must precede an expr. If the optional expr is specified, it is a vector to be XECUTEd when a pending or subsequently set timer interrupts the process.

* If neither the timeout nor the expr are specified, the SET of $ZTIMEOUT to an empty string is a no-op.

Interrupts are only XECUTEd at safe points, such as the beginning of a line, when waiting for a command with a timeout, or when starting a FOR iteration. When an interrupt occurs, if the vector is not valid M code (an empty string vector is not valid M code), YottaDB invokes the current $ETRAP or $ZTRAP. YottaDB rejects an attempted KILL of $ZTIMeout with the `VAREXPECTED <../MessageRecovery/errors.html#varexpected>`_ error and an attempted NEW of $ZTIMeout with the `SVNONEW <../MessageRecovery/errors.html#svnonew>`_ error.

When accessed, the timeout in $ZTIMEOUT is the remaining time of any pending interrupt, and zero otherwise.

Example:

.. code-block:: bash

   YDB>write $ztimeout ; Initial value is -1 and there is no vector
   -1
   YDB>set $ztimeout="2:write $zpiece($horolog,"","",2)" write $zpiece($horolog,",",2),!,$ztimeout ; Set the timeout and wait for it
   61243
   1.999:write $zpiece($horolog,",",2)
   YDB>61245
   YDB>; Note the timer interrupted after two seconds

   YDB>write $zpiece($horolog,",",2) set $ztimeout=1 ; Omitting expr uses the previously set vector
   61321
   YDB>61322
   YDB>write $zpiece($horolog,",",2) set $ztimeout=0 ; Setting to 0 executes the vector immediately
   61365
   61365
   YDB>write $zpiece($horolog,",",2) set $ztimeout=-1 ; Setting to a negative number cancels the timer
   61392
   YDB>

.. _ztrap-isv:

--------------
$ZTRAP
--------------

$ZT[RAP] contains a string value that YottaDB XECUTEs when an error occurs during routine execution.

.. note::
   The following discussion assumes that $ETRAP error handling is simultaneously not in effect (that is, $ETRAP="").  See `Chapter 13: "Error Processing" <./errproc.html>`_ for more information on the interaction between $ETRAP and $ZTRAP.

When the $ZTRAP variable is not null, YottaDB executes $ZTRAP at the current level. The $ZTRAP variable has the initial value of "B," and puts the process in Direct Mode when an error condition occurs. If the value of $ZTRAP is null (""), an exception causes the image to run-down with the condition code associated with the exception. If $ZTRAP contains invalid source code, YottaDB displays an error message and puts the process into Direct Mode.

$ZTRAP is a read-write Intrinsic Special Variable, (that is, it can appear on the left side of the equal sign (=) in the argument to the SET command).

$ZTRAP may also appear as an argument to an inclusive NEW command. NEW $ZTRAP causes YottaDB to stack the current $ZTRAP value, and set its value to the empty string ($ZTRAP=""). The NEW command puts the $ZTRAP in control for error handling. When the program QUITs from the invocation level where the NEW occurred, YottaDB restores the value previously stacked by the NEW. NEW $ZTRAP provides nesting of $ZTRAP. Because $ZTRAP="" terminates the image when an error occurs, SET $ZTRAP= generally follows immediately after NEW $ZTRAP. You may use this technique to construct error handling strategies corresponding to the nesting of your programs. If the environment variable ydb_ztrap_new evaluates to boolean TRUE (case insensitive string "TRUE", or case insensitive string "YES", or a non-zero number), $ZTRAP is NEWed when $ZTRAP is SET; otherwise $ZTRAP is not stacked when it is SET.

.. note::
   QUIT from a $ZTRAP terminates the level at which the $ZTRAP was activated.

Keep $ZTRAP simple and put complicated logic in another routine. If the action specified by $ZTRAP results in another run-time error before changing the value of $ZTRAP, YottaDB invokes $ZTRAP until it exhausts the process stack space, terminating the image. Carefully debug exception handling.

Example:

.. code-block:: bash

   YDB>S $ZTRAP="ZP @$ZPOS B"

This example modifies $ZTRAP to display source code for the line where YottaDB encounters an error before entering Direct Mode.

There are four settings of $ZTRAP controlled by the UNIX environment variable ydb_ztrap_form.

The four settings of ydb_ztrap_form are:

*  code - If ydb_ztrap_form evaluates to "code" (or a value that is not one of the subsequently described values), then YottaDB treats $ZTRAP as code and handles it as previously described in the documentation.
* entryref - If ydb_ztrap_form evaluates to "entryref" then YottaDB treats it as an entryref argument to an implicit GOTO command.
* adaptive - If ydb_ztrap_form evaluates to "adaptive" then if $ZTRAP does not compile to valid M code, then $ZTRAP is treated as just described for "entryref." Since there is little ambiguity, code and entryref forms of $ZTRAP can be intermixed in the same application.
* pope[ntryref] / popa[daptive] - If ydb_ztrap_form evaluates to "POPE[NTRYREF]" or "POPA[DAPTIVE]" (case insensitive) and $ZTRAP value is in the form of entryref, YottaDB unwinds the M stack from the level at which an error occurred to (but not including) the level at which $ZTRAP was last SET. Then, YottaDB transfers control to the entryref in $ZTRAP at the level where the $ZTRAP value was SET. If the UNIX environment variable ydb_zyerror is defined to a valid entryref, YottaDB transfers control to the specified entryref (with an implicit DO) after unwinding the stack and before transferring control to the entryref specified in $ZTRAP.

.. note::
   YottaDB attempts to compile $ZTRAP before evaluating $ZTRAP as an entryref. Because YottaDB allows commands without arguments such as QUIT, ZGOTO, or HANG as valid labels, be careful not to use such keywords as labels for error handling code in "adaptive" mode.

.. note::
   Like $ZTRAP values, invocation of device EXCEPTION values follow the pattern specified by the current ydb_ztrap_form setting except that there is never any implicit popping with EXCEPTION action.

.. _zusedstor-isv:

-----------------
$ZUSEDSTOR
-----------------

$ZUSEDSTOR is the value in $ZALLOCSTOR minus storage management overhead and represents the actual memory, in bytes, requested by current activities. It provides one view (see also :ref:`zallocstor-isv` and :ref:`zrealstor-isv`) of the process memory utilization and can help identify storage related problems. YottaDB does not permit $ZUSEDSTOR to be SET or NEWed.

-----------------
$ZUT
-----------------

$ZUT (UNIX time or universal time) returns the number of microseconds since January 1, 1970 00:00:00 UTC, which provides a time stamp for directly comparing different timezones. $ZUT accuracy is subject to the precision of the system clock (use man gettimeofday from the UNIX shell for more information).

---------------
$ZVERSION
---------------

$ZV[ERSION] contains a string value identifying the GT.M version on which the YottaDB release is built. $ZV[ERSION] is a space-delimited string with four pieces as follows:

.. code-block:: none

   <product> <release> <OS> <architecture>

<product> is always "GT.M".

<release> always begins with "V", and has the structure V<DB_Format>.<major_release>-<minor_release>[<bug_fix_level>] where:

* <DB_Format> identifies the block format of YottaDB database files compatible with the release. For example, V4, V5, and V6. The <DB_Format> piece in $ZVERSION does not change even when a MUPIP UPRGRADE or MUPIP DOWNGRADE changes the DB Format element in the database fileheader.
* <major_release> identifies a release with major enhancements.
* <minor_release> identifies minor enhancements to a major release. The classification of major and minor enhancements is at the discretion of GT.M.
* An optional <bug_fix_level> is an upper-case letter indicating bug fixes but no new enhancements. Note that GT.M is built monolithically and never patched. Even though a bug fix release has only bug fixes, it should be treated as a new GT.M release and installed in a separate directory.

<OS> is the host operating system name.

<architecture> is the hardware architecture for which the release of GT.M is compiled. Note that GT.M retains its original names for continuity even if vendor branding changes, for example, "RS6000".

M routines cannot modify $ZVERSION.

.. note::
   YottaDB treats $ZVERSION as a literal at compile time. Therefore, always compile with the same version as that used at runtime.

Example:

.. code-block:: bash

   YDB>write $zversion
   GT.M V6.3-008 Linux x86_64

This example displays the current version identifier for GT.M.

In order to ensure drop-in compatibility with GT.M for applications, YottaDB reports a $ZVERSION identical with GT.M. See the `$ZYRELEASE <#zyrelease-isv>`_ intrinsic special variable to identify the YottaDB release.

.. _zyerror-isv:

-----------------
$ZYERROR
-----------------

$ZYER[ROR] is a read/write ISV that contains a string value pointing to an entryref. After YottaDB encounters an error, if $ZYERROR is set to a non-null value, YottaDB invokes the routine at the entryref specified by $ZYERROR with an implicit DO. It is intended that the code invoked by $ZYERROR use the value of $ZSTATUS to select or construct a value to which it SETs $ZERROR. If $ZYERROR is not a valid entryref or if an error occurs while executing the entryref specified by $ZYERROR, YottaDB SETs $ZERROR to the error status encountered. YottaDB then returns control to the M code specified by $ETRAP/$ZTRAP or device EXCEPTION.

$ZYERROR is implicitly NEWed on entry to the routine specified by $ZYERROR. However, if YottaDB fails to compile, YottaDB does not transfer control to the entryref specified by $ZYERROR.

YottaDB permits $ZYERROR to be modified by the SET and NEW commands.

---------------
$ZYINTRSIG
---------------

$ZYINTRSIG is a read only ISV that holds the value :code:`"SIGUSR1"` or :code:`"SIGUSR2"` depending on whether a :code:`SIGUSR1` or :code:`SIGUSR2` signal triggers the current :code:`$ZINTERRUPT` invocation (i.e. if :code:`$ZININTERRUPT` is 1). If :code:`$ZININTERRUPT` is 0, :code:`$ZYINTRSIG` holds the value :code:`" "`. Note that the value can be :code:`"SIGUSR2"` only if the environment variable :code:`ydb_treat_sigusr2_like_sigusr1` was set to a non-zero value.

:code:`$ZYINTRSIG` allows M code inside :code:`$ZINTERRUPT` differentiate whether it was a :code:`SIGUSR1` or :code:`SIGUSR2` that invoked the :code:`$ZINTERRUPT` mechanism and take different actions if necessary.

$ZYINTRSIG was added to YottaDB effective release `r1.32 <https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.32>`_.

.. _zyrelease-isv:

------------------
$ZYRELEASE
------------------

The $ZYRE[LEASE] intrinsic special variable contains a string value that application code can use to determine the YottaDB release it is running on. $ZYRELEASE is a space delimited string with four pieces as follows:

.. code-block:: none

   <product><release><os><architecture>

<product> is always "YottaDB".

<release> always begins with an "r" and is a number structured as follows:

* The number before the decimal point (".") is the major release number.
* A decimal point.
* The first digit after the decimal point is a minor release number.
* The second (and last) digit after the decimal point is a bug fix level (so "r3.21" would the first bug fix level of the "r3.20" release).

<os> is the operating system, e.g., "Linux"

<architecture> is the underlying CPU architecture, e.g., "x86_64"

.. note::
   $zyrelease is a read-only intrinsic special variable. As the code generator treats $zyrelease as a string constant known at compile time, and optimizes accordingly, ensure that you run object code only on the same YottaDB release on which you compiled it.

.. _zysqlnull-isv:

------------------
$ZYSQLNULL
------------------

The read-only $ZYSQLNULL intrinsic special variable is conceptually equivalent to a logical value of unknown, and can be assigned as the value or used as a subscript of a local variable.

When $ZYSQLNULL is an operand, the results are as follows:

+-----------------------------------------------------+-----------------------------------------------------------------------------------------------------+
| Operator(s)                                         | Result                                                                                              |
+-----------------------------------------------------+-----------------------------------------------------------------------------------------------------+
| Binary :code:`!`                                    | If the other operand evaluates to true (1), the result is true, otherwise the result is $ZYSQLNULL  |
+-----------------------------------------------------+-----------------------------------------------------------------------------------------------------+
| Binary :code:`&`                                    | If the other operand evalutes to false (0), the result is false, otherwise the result is $ZYSQLNULL |
+-----------------------------------------------------+-----------------------------------------------------------------------------------------------------+
| All binary operators except :code:`!` and :code:`&` | Regardless of the value of the other operand, the result is $ZYSQLNULL                              |
+-----------------------------------------------------+-----------------------------------------------------------------------------------------------------+
| Unary :code:`+`                                     | $ZYSQLNULL                                                                                          |
+-----------------------------------------------------+-----------------------------------------------------------------------------------------------------+
| Unary :code:`-`                                     | $ZYSQLNULL                                                                                          |
+-----------------------------------------------------+-----------------------------------------------------------------------------------------------------+
| Unary :code:`'`                                     | $ZYSQLNULL                                                                                          |
+-----------------------------------------------------+-----------------------------------------------------------------------------------------------------+

$TEST has only 2 values: false (0) and true (1). An IF statement whose condition evaluates to $ZYSQLNULL sets $TEST to 0 and does not execute the rest the line. Commands with postconditionals that evaluate to $ZYSQLNULL do not execute the command.

ZSHOW and ZWRITE of $ZYSQLNULL values show a value of $ZYSQLNULL. WRITE does not show any value for $ZYSQLNULL just as it does with :code:`""`.

$ZYSQLNULL can be a subscript of a local variable. In that case, it collates after all other subscripts, i.e., $ORDER() and $QUERY() return that subscript at the very end.

The function $ZYISSQLNULL() returns 1 if its sole argument has a value of $ZYSQLNULL, and 0 otherwise.

Using $ZYSQLNULL as a subscript or assigning it as the value of a global variable (including implicitly with a MERGE), using it as a subscript in a LOCK/ZALLOCATE/ZDEALLOCATE command, or in a context that expects an integer or a numeric value raises the ZYSQLNULLNOTVALID error. Other than usage as an operand as discussed above, $ZYSQLNULL in a context that expects a string, e.g. :code:`$ASCII($ZYSQLNULL,1)`, is treated like the empty string  :code:`""`.

$ZYSQLNULL was added to YottaDB effective release `r1.30 <https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.30>`_.

.. _trigger-isvs:

------------------
Trigger ISVs
------------------

YottaDB provides nine ISVs (Intrinsic Special Variables) to facilitate trigger operations. With the exception of $ZTWORMHOLE, all numeric trigger-related ISVs return zero (0) outside of a trigger context; non-numeric ISVs return the empty string.

.. _ztdata-isv:

+++++++++++
$ZTDATA
+++++++++++

Within trigger context, $ZTDATA returns $DATA(@$REFERENCE)#2 for a SET or $DATA(@$REFERENCE) for a KILL, ZKILL or ZWITHDRAW prior to the explicit update. This provides a fast path alternative, avoiding the need for indirection in trigger code, to help trigger code determine the characteristics of the triggering node prior to the triggering update. For a SET, it shows whether the node did or did not hold data - whether a SET is modifying the contents of an existing node or creating data at a new node. For a KILL it shows whether the node had descendants and whether it had data.

.. _ztdelim-isv:

++++++++++++++
$ZTDELIM
++++++++++++++

$ZTDE[LIM] returns the piece delimiter/separator, as specified by -delim or -zdelim in the trigger definition. This allows SET type triggers to extract updated pieces defined in $ZTUPDATE and KILL/ZKILL type triggers to extract the relevant pieces in the node value (of the node being killed) without having the piece separator hard coded into the trigger routine. $ZTDELIM is the empty string outside of a trigger context. It is also the empty string inside a trigger context if -delim or -zdelim was not specified in the trigger definition.

.. _ztlevel-isv:

+++++++++++++
$ZTLEVEL
+++++++++++++

Within trigger context, $ZTLEVEL returns the current level of trigger nesting (invocation by a trigger of an additional trigger by an update in trigger context).

$ZTLEVEL greater than one (>1) indicates that there are nested triggers in progress. When a single update invokes multiple triggers solely because of multiple trigger matches of that initial (non-trigger) update, they are not nested (they are chained) and thus all have same $ZTLEVEL.

Example:

.. code-block:: none

   +^Cycle(1) -commands=Set -xecute="Write ""$ZTLevel for ^Cycle(1) is: "",$ZTLevel Set ^Cycle(2)=1"
   +^Cycle(2) -commands=Set -xecute="Write ""$ZTLevel for ^Cycle(2) is: "",$ZTLevel Set ^Cycle(1)=1"

These trigger definitions show different values of $ZTLEVEL when two triggers are called recursively (and pathologically).

.. code-block:: none

   +^Acct("ID") -commands=set -xecute="set ^Acct(1)=$ztvalue+1"
   +^Acct(sub=:) -command=set -xecute="set ^X($ztvalue)=sub"

SET ^Acct("ID")=10 invokes both the above triggers in some order and $ZTLEVEL will have the same value in both because these triggers are chained rather than nested.

.. _ztname-isv:

+++++++++++++++
$ZTNAME
+++++++++++++++

Within a trigger context, $ZTNAME returns the trigger name. Outside a trigger context, $ZTNAME returns an empty string.

.. _ztoldval-isv:

++++++++++++
$ZTOLDVAL
++++++++++++

Within trigger context, $ZTOLDVAL returns the prior (old) value of the global node whose update caused the trigger invocation. This provides a fast path alternative to $GET(@$REFERENCE) at trigger entry (which avoids the heavyweight indirection). If there are multiple triggers matching the same node (chained), $ZTOLDVAL returns the same result for each of them.

Example:

.. code-block:: none

   +^Acct(1,"ID") -commands=Set -xecute="Write:$ZTOLdval ""The prior value of ^Acct(1,ID) was: "",$ZTOLdval"

This trigger gets invoked with a SET and displays the prior value (if it exists) of ^Acct(1,"ID").

.. code-block:: bash

   YDB>w ^Acct(1,"ID")
   1975
   YDB>s ^Acct(1,"ID")=2011
   The prior value of ^Acct(1,ID) was: 1975

.. _ztriggerop-isv:

+++++++++++++++
$ZTRIGGEROP
+++++++++++++++

Within trigger context, for SET (including MERGE and $INCREMENT() operations), $ZTRIGGEROP has the value "S". For KILL, $ZTRIGGEROP has the value "K" For ZKILL or ZWITHDRAW, $ZTRIGGEROP has the value "ZK".

.. _ztslate-isv:

+++++++++++++++
$ZTSLATE
+++++++++++++++

$ZTSLATE allows you to specify a string that you want to make available in chained or nested triggers invoked for an outermost transaction (when a TSTART takes $TLEVEL from 0 to 1). You might use $ZTSLATE to accumulate transaction-related information, for example $ZTOLDVAL and $ZTVALUE, available within trigger context for use in a subsequent trigger later in the same transaction. For example, you can use $ZTSLATE to build up an application history or journal record to be written when a transaction is about to commit.

You can SET $ZTSLATE only while a database trigger is active. YottaDB clears $ZTSLATE for the outermost transaction or on a TRESTART. However, YottaDB retains $ZTSLATE for all sub-transactions (where $TLEVEL>1).

Example:

.. code-block:: none

   TSTART ()       ; Implicitly clears $ZTSLAT
   SET ^ACC(ACN1,BAL)=AMT          ; Trigger sets $ZTSLATE=ACN_"|"
   SET ^ACC(ACN2,BAL)=-AMT         ; Trigger sets $ZTSLATE=$ZTSLATE_ACN_"|"
   ZTRIGGER ^ACT("TRANS")          ; Trigger uses $ZTSLATE to update transaction log
   TCOMMIT

.. _ztupdate-isv:

++++++++++++
$ZTUPDATE
++++++++++++

Within trigger context, for SET commands where the YottaDB trigger specifies a piece separator, $ZTUPDATE provides a comma separated list of piece numbers that differ between the current values of $ZTOLDVAL and $ZTVALUE. If the trigger specifies a piece separator, but does not specify any pieces of interest, $ZTUPDATE identifies all changed pieces. $ZTUPDATE is 0 in all other cases (that is: for SET commands where the YottaDB trigger does not specify a piece separator or for KILLs). Note that if an update matches more than one trigger, all matching triggers see the same $ZTOLDVAL at trigger entry but potentially different values of $ZTVALUE so $ZTUPDATE could change due to the actions of each matching trigger even though all matching triggers have identical -[z]delim and -piece specifications.

Example:

.. code-block:: none

   +^trigvn -commands=Set -pieces=1;3:6 -delim="|" -xecute="Write !,$ZTUPDATE"

In the above trigger definition entry, $ZTUPDATE displays a comma separated list of the changed piece numbers if one of the pieces of interest: 1,3,4,5,or 6 are modified by the update.

.. code-block:: bash

   YDB>write ^trigvn
   Window|Table|Chair|Curtain|Cushion|Air Conditioner
   YDB>set ^trigvn="Window|Dining Table|Chair|Vignette|Pillow|Air Conditioner"
   4,5

Note that even though piece numbers 2,4 and 5 are changed, $ZTUPDATE displays only 4,5 because the trigger is not defined for updates for the second piece.

.. _ztvalue-isv:

+++++++++++++
$ZTVALUE
+++++++++++++

For SET, $ZTVALUE has the value assigned to the node by the explicit SET operation. Modifying $ZTVALUE within a trigger modifies the eventual value YottaDB assigns to the node. Note that changing $ZTVALUE has a small performance impact because it causes an additional update operation on the node once all trigger code completes. If a node has multiple associated triggers, each trigger receives the current value of $ZTVALUE, however, because the triggers run in arbitrary order, YottaDB strongly recommends no more than one trigger change any given element of application data, for example: a particular piece. For KILL and its variants, $ZTVALUE returns the empty string. While YottaDB accepts updates to $ZTVALUE within the trigger code invoked for a KILL or any of its variants, it ultimately discards any such value. Outside trigger context, attempting to SET $ZTVALUE produces a SETINTRIGONLY error.

.. _ztwormhole-isv:

+++++++++++++
$ZTWORMHOLE
+++++++++++++

$ZTWORMHOLE allows you to specify a string up to 128KB of information that you want to make available during trigger execution. You can use $ZTWORMHOLE to supply an application context or process context to your trigger logic. Because YottaDB makes $ZTWORMHOLE available throughout the duration of the process, you can access or update $ZTWORMHOLE both from inside and outside a trigger.

$ZTWORMHOLE provides a mechanism to access information from a process/application context that is otherwise unavailable in a trigger context. As part of any update that invokes at least one trigger which references/sets $ZTWORMHOLE, YottaDB records any non-empty string value of $ZTWORMHOLE in the YottaDB database journal file as part of the :ref:`transaction that includes the original and trigger updates <about-triggers>`. The value stored in the journal file is the value of $ZTWORMHOLE at the time the trigger code returns to application code. YottaDB also transmits this $ZTWORMHOLE value from the journal file in the replication stream, thus providing the same context to triggers invoked by the replicating instance Update Process. Therefore, whenever you use $ZTWORMHOLE in a trigger, you create a software `wormhole <https://en.wikipedia.org/wiki/Wormhole>`_ for process context that is otherwise unavailable to a replicating instance.

While a `MUPIP JOURNAL EXTRACT <https://docs.yottadb.com/AdminOpsGuide/ydbjournal.html#extract-file-name-stdout>`_ displays the $ZTWORMHOLE journal record before the journal records corresponding to the database update that initiated the trigger as well as journal records corresponding to database updates that happen inside the trigger, the value displayed in the $ZTWORMHOLE journal record is the value when trigger code returns back to application code.

Note that if a trigger sets values of $ZTWORMHOLE or NEW's it within the trigger, it is always the last value that is propagated through the wormhole. If you are not careful, you can write "clever" code that is unmaintainable by later programmers; consider yourself warned.

Note that if trigger code does not reference/set $ZTWORMHOLE, YottaDB does not make it available in the journal files or replication stream. Therefore, if a replicating secondary has different trigger code than the initiating primary and the triggers on the replicating node require information from $ZTWORMHOLE, the triggers on the initiating node must reference/set $ZTWORMHOLE to ensure YottaDB maintains the data it contains for use by the update process on the replicating node. While you can change $ZTWORMHOLE within trigger code, because of the arbitrary ordering of trigger invocation on the same node, if multiple triggers for the same update modify $ZTWORMHOLE to different values, the final value of $ZTWORMHOLE after all triggers have been invoked (which is what gets stored in the journal file and gets replicated across to the replicating instance update process) becomes unpredictable. Therefore, such an approach requires careful design and implementation.

YottaDB allows $ZTWORMHOLE to be NEW'd. Newing $ZTWORMHOLE is like Newing `$ZGBLDIR <#zgbldir>`_ in that the NEW'd value is initialized with the value pushed on the stack (i.e. $ZTWORMHOLE retains its original value after the NEW). However, like other NEWs, YottaDB restores $ZTWORMHOLE's value when the stack level pops.

The ability to propagate a value of $ZTWORMHOLE set in a trigger was added in `r1.32 <https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.32>`_.

The following table summarizes the read/write permissions assigned to all trigger-related ISVs within trigger context and outside trigger context.

+------------------------------+----------------------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------+
| Intrinsic Special Variable   | Within Trigger Context           | Notes                                                                                                                                                 |
+==============================+==================================+=======================================================================================================================================================+
| $ETRAP                       | Read/Write                       | Set to ydb_trigger_etrap or the empty string when entering trigger context. For more information on using the $ETRAP mechanism for handling errors    |
|                              |                                  | during trigger execution, refer to                                                                                                                    |
|                              |                                  | :ref:`err-handling-trigger-exec`.                                                                                                                     |
+------------------------------+----------------------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------+
| $REFERENCE                   | Read Only                        | Restored at the completion of a trigger.                                                                                                              |
+------------------------------+----------------------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------+
| $TEST                        | Read Only                        | Restored at the completion of a trigger.                                                                                                              |
+------------------------------+----------------------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------+
| $TLEVEL                      | Read Only                        | Always >=1 in trigger code; must be the same as the completion of processing a trigger as it was at the start.                                        |
+------------------------------+----------------------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZTNAME                      | Read Only                        | Returns the trigger name.                                                                                                                             |
+------------------------------+----------------------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZTDATA                      | Read Only                        | Shows prior state.                                                                                                                                    |
+------------------------------+----------------------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZTLEVEL                     | Read Only                        | Shows trigger nesting.                                                                                                                                |
+------------------------------+----------------------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZTOLDVAL                    | Read Only                        | Shows the pre-update value.                                                                                                                           |
+------------------------------+----------------------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZTRAP                       |  Read only - ""                  | Must use $ETRAP in trigger code.                                                                                                                      |
+------------------------------+----------------------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZTRIGGEROP                  | Read Only                        | Shows the triggering command.                                                                                                                         |
+------------------------------+----------------------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZTUPDATE                    | Read Only                        | Lists modified pieces (if requested) for SET.                                                                                                         |
+------------------------------+----------------------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZTVALUE                     | Read/Write                       | Can change the eventual applied value for SET.                                                                                                        |
+------------------------------+----------------------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZTWORMHOLE                  | Read/Write                       | Holds application context because trigger code has no access to the local variable context.                                                           |
+------------------------------+----------------------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZTSLATE                     | Read/Write                       | Holds outermost transaction context for chained or nested triggers.                                                                                   |
+------------------------------+----------------------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------+

**Examples of Trigger ISVs**

The following examples are derived from the FIS Profile application.

Nodes in ^ACN(CID,50) have TYPE in piece 1, CLS in piece 2, FEEPLN in piece 15 and EMPLNO in piece 31. Indexes are ^XACN(CLS,ACN,CID), ^XREF("EMPLCTA",EMPLNO,ACN,TYPE,CID) and ^XREF("FEEPLN",FEEPLN,CID) and use ACN from the first piece of ^ACN(CLS,99). These indices are maintained with four triggers: one invoked by a KILL or ZKill of an ^ACN(:,50) node and three invoked by SETs to different pieces of ^ACN(:,50) nodes. Note that ACN, CID, CLS and TYPE are required, whereas EMPLNO and FEEPLN can be null, which requires (in our convention) the use of $ZC(254) in indices. The triggerfile definitions are:

.. code-block:: none

   +^ACN(cid=:,50) -zdelim="|" -pieces=2 -commands=SET -xecute="Do ^SclsACN50"
   +^ACN(cid=:,50) -zdelim="|" -pieces=1,31 -commands=SET -xecute="Do ^SemplnoTypeACN50" +^ACN(cid=:,50) -zdelim="|" -pieces=15 -commands=SET -xecute="Do ^SfeeplnACN50"
   +^ACN(cid=:,50) -commands=KILL,ZKill -xecute="Do ^KACN50"

The code in KACN50.m KILLs cross reference indices when the application deletes any ^ACN(:,50).

.. code-block:: none

   KACN50 ; KILL of entire ^ACN(:,50) node, e.g., from account deletion
     ; Capture information
     Set cls=$Piece($ZTOLD,"|",2)                   ; CLS
     Set emplno=$Piece($ZTOLD,"|",31)
     Set:'$Length(emplno) emplno=$ZC(254)                ; EMPLNO
     Set feepln=$Piece($ZTOLD,"|",15)
     Set:'$L(feepln) feepln=$ZC(254)                     ; FEEPLN
     Set type=$Piece($ZTOLD,"|",1)                  ; TYPE
     Set acn=$Piece(^ACN(cid,99),"|",1)             ; ACN
     Kill ^XACN(cls,acn,cid)
     Kill ^XREF("EMPLCTA",emplno,acn,type,cid)
     Kill ^XREF("FEEPLN",feepln,cid)
     Quit

The routine in SclsACN50.m creates cross references for a SET or a SET $PIECE() that modifies the second piece of ^ACN(:,50).

.. code-block:: none

   SClsACN50 ; Update to CLS in ^ACN(,50)
    ; Capture information
    Set oldcls=$Piece($ZTOLD,"|",2)                ; Old CLS
    Set cls=$Piece($ZTVAL,"|",2)                   ; New CLS
    Set acn=$Piece(^ACN(cid,99),"|",1)             ; ACN
    Set processMode=$Piece($ZTWORM,"|",1)          ; Process
    If processMode<2 Kill ^XACN(oldcls,acn,cid)
    Set ^XACN(cls,acn,cid)=""
    Quit

Note that the example is written for clarity. Eliminating values that need not be assigned to temporary local variables produces:

.. code-block:: none

   SclsACN50
     S acn=$P(^ACN(cid,99),"|",1)
     I $P($ZTWORM,"|",1)<2 K ^XACN($P($ZTOLD,"|",2),acn,cid)
     S ^XACN($P($ZTVAL,"|",2),acn,cid)=""
     Q

Indeed, this index can simply be included in the (one line) triggerfile specification itself:

.. code-block:: none

   +^ACN(cid=:,50) -zdelim="|" -pieces=2 -commands=SET -xecute="S oldcls=$P($ZTOLD,""|"",2),acn=$P(^ACN(cid,99),""|"",1) K:$P($ZTWO,""|"",1)<2 ^XACN(oldcls,acn,cid) S ^XACN($P($ZTVAL,""|"",2),acn,cid)="""""

In the interest of readability most triggerfile definitions in this chapter are written as complete routines. The code in SemplnoTypeACN50.m handles changes to pieces 1 and 31 of ^ACN(:,50). Note that a SET to ^ACN(:,50) that modifies either or both pieces causes this trigger to execute just once, whereas two sequential SET $Piece() commands, to first modify one piece and then the other cause it to execute twice, at different times, once for each piece.

.. code-block:: none

   EmplnoTypeACN50 ; Update to EMPLNO and/or TYPE in ^ACN(,50)
    ; Capture information
    Set oldemplno=$Piece($ZTOLD,"|",31)
    Set:'$Length(oldemplno) oldemplno=$ZC(254)
    Set emplno=$Piece($ZTVAL,"|",31)
    Set:'$L(emplno) emplno=$ZC(254)
    Set oldtype=$Piece($ZTOLD,"|",1)
    Set type=$Piece($ZTVAL,"|",1)
    Set acn=$Piece(^ACN(cid,99),"|",1)
    Set processMode=$Piece($ZTWORM,"|",1)
    If processMode<2 Do
    . Kill ^XREF("EMPLNO",oldemplno,acn,oldtype,cid)
    . Set ^XREF("EMPLNO",emplno,acn,type,cid)=""
    Quit

The code in SFeeplnACN50.m handles changes to piece 15.

.. code-block:: none

   SFeeplnACN50 ; Update to FEEPLN in ^ACN(,50)
     ; Capture information
     Set oldfeepln=$Piece($ZTOLD,"|",15)
     Set:'$Length(oldfeepln) oldfeepln=$ZC(254)
     Set feepln=$Piece($ZTVAL,"|",15)
     Set:'$Length(feepln) feepln=$ZC(254)
     Set processMode=$Piece($ZTWORM,"|",1)
     If processMode<2 Do
     . Kill ^XREF("FEEPLN",oldfeepln,cid) Set ^XREF("FEEPLN",feepln,cid)=""
     Quit

.. raw:: html

    <img referrerpolicy="no-referrer-when-downgrade" src="https://download.yottadb.com/MProgGuide.png" />
