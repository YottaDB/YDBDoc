
.. index::
   Commands

=====================
Commands
=====================

.. contents::
   :depth: 2

This chapter describes M language commands implemented in YottaDB/GT.M. All commands starting with the letter Z are YottaDB/GT.M additions to the ANSI standard command set. The M standard specifies standard abbreviations for commands and rejects any non-standard abbreviation. Behavior of I/O commands including OPEN, USE, READ, WRITE, and CLOSE is described in Chapter 9: “Input/Output Processing”.

------------
Break
------------

The BREAK command pauses execution of the code and initiates Direct Mode.

The format of the BREAK command is:

.. parsed-literal::
   B[REAK][:tvexpr] [expr[:tvexpr][,...]]

* The optional truth-valued expression immediately following the command is a command postconditional that controls whether or not YottaDB/GT.M executes the command.
* The optional expression contains a fragment of YottaDB/GT.M code to XECUTE before the process enters Direct Mode.
* The BREAK command without an argument causes a pause in execution of the routine code and immediately initiates Direct Mode. In this case, at least two (2) spaces must follow the BREAK to separate it from the next command on the line.
* The optional truth-valued expression immediately following the expression is the argument postconditional that controls whether GT.M XECUTEs the argument. If present and true, the process executes the code before entering Direct Mode. If present and false, the process does not execute the code before entering Direct Mode.
* If an argument postconditional is present and true, the process pauses code execution and initiates Direct Mode before and after XECUTing the argument.
* An indirection operator and an expression atom evaluating to a list of one or more BREAK arguments form a legal argument for a BREAK.

Issuing a BREAK command inside an M transaction destroys the Isolation of that transaction. Because of the way that YottaDB/GT.M implements transaction processing, a BREAK within a transaction may cause the transaction to suffer an indefinite number of restarts ("live lock").

Generally, programs in production must not include BREAK commands. Therefore, YottaDB/GT.M provides the ZBREAK and ZSTEP commands, which insert temporary breakpoints into the process rather than the source code. BREAKs inserted with ZBREAK only exist until the image terminates or until explicitly removed by another ZBREAK command. ZSTEP also inserts temporary BREAKs in the image that only exist for the execution of the ZSTEP command. In the YottaDB/GT.M debugging environment, ZBREAKs and ZSTEPs that insert BREAKs provide a more flexible and less error-prone means of setting breakpoints than coding BREAKs directly into a routine. For more information on ZBREAK and ZSTEP, refer to the sections that describe those commands. Any BREAK commands in code intended for production should be conditionalized on something that is FALSE in production, as, unlike ZBREAK commands, YottaDB/GT.M currently has no means to "turn off" BREAK commands.

ZCONTINUE resumes execution of the interrupted program.

YottaDB/GT.M displays messages identifying the source of a BREAK as:

* The body of a program
* A ZBREAK action
* A device EXCEPTION
* A ZSTEP action

The VIEW "BREAKMSG" mask selectively enables or disables these messages. For an explanation of the mask, refer to “View”. By default, a process executing a YottaDB/GT.M image displays all BREAK messages.

When a process encounters a BREAK, it displays a prompt indicating readiness to process commands in Direct Mode. By default, Direct Mode displays the GTM> or YDB> prompt. SETting the $ZPROMPT intrinsic special variable alters the prompt.

+++++++++++++++++++++++
Examples of BREAK
+++++++++++++++++++++++

Example:

.. parsed-literal::
   LOOP0     F  S act=$O(^act(act)) Q:act=""  B:debug  D LOOP1

This FOR loop contains a BREAK with a command postconditional.

Example:

.. parsed-literal::
   GTM>ZPRINT ^br
   br;
        kill
        for i=1:1:3 do break;
        quit
   break;
        write "Iteration ",i,?15,"x=",$get(x,"<UNDEF>"),!
        break:$data(x) "write ""OK"",!":x,"write ""Wrong again"",!":'x
        set x=$increment(x,$data(x))
        quit
   GTM>DO ^br
   Iteration 1    x=<UNDEF>
   Iteration 2    x=0
   %GTM-I-BREAK, Break instruction encountered
        At M source location break+2^br
   GTM>ZCONTINUE
        Wrong again
   %GTM-I-BREAK, Break instruction encountered
        At M source location break+2^br
   GTM>ZCONTINUE
   Iteration 3    x=1
        OK
   %GTM-I-BREAK, Break instruction encountered
        At M source location break+2^br
   GTM>ZCONTINUE
   %GTM-I-BREAK, Break instruction encountered
        At M source location break+2^br
   GTM>ZCONTINUE
   GTM>

This uses a BREAK with both command and argument postconditionals. The actions display debugging messages.

------------------
Close
------------------

The CLOSE command breaks the connection between a process and a device.

The format of the CLOSE command is:

.. parsed-literal::
   C[LOSE][:tvexpr] expr[:(keyword[=expr][:...])][,...]

* The optional truth-valued expression immediately following the command is a command postconditional that controls whether or not YottaDB/GT.M executes the command.
* The required expression specifies the device to CLOSE.
* The optional keywords specify device parameters that control device behavior; some device parameters take arguments delimited by an equal sign (=). If there is only one keyword, the surrounding parentheses are optional.
* An indirection operator and an expression atom evaluating to a list of one or more CLOSE arguments form a legal argument for a CLOSE.

----------------
Do
----------------

The DO command makes an entry in the YottaDB/GT.M invocation stack and transfers execution to the location specified by the entryref.

The format of the DO command is:

.. parsed-literal::
   D[O][:tvexpr] [entryref[(expr|.lvn[,...])][:tvexpr][,...]]

* The optional truth-valued expression immediately following the command is a command postconditional that controls whether or not YottaDB/GT.M executes the command.
* The optional entryref specifies a location (with some combination of label, offset, and routinename) at which execution continues immediately following the DO.
* A DO command without an argument (that is, a DO followed by two (2) spaces) transfers execution to the next line in the routine if that line contains an appropriate number of periods (.) after the optional label and before the required linestart. These periods indicate the current level of "immediate" nesting caused by argumentless DOs. If the line following the DO contains too many periods, YottaDB/GT.M reports an error; if the line following the DO contains too few periods, YottaDB/GT.M ignores the DO command.
* A DO command without an argument stacks the current value of $TEST, in contrast to a DO with an argument, which does not protect the current value of $TEST.
* The optional parameter list enclosed in parentheses ( ) contains parameters to pass to the routine entry point.
* Label invocations using DO do not require parentheses for calls with no actuallist. If DO or a $$ that does not specify an actuallist invokes a label with a formallist, the missing parameters are undefined in the called routine.

 .. note::
    If DO or $$ specifies a routine but no label using an actuallist, then whether that routine's top label has a formallist or not, the actuallist applies to it directly, whereas before the actuallist would "fall through" to the first label with executable code.

* If the DO specifies a parameter list, the entryref location must start with a label and an argument list (M prohibits entryrefs with offsets during parameter passing).
* If an element in the parameter list starts with a period, it specifies an unsubscripted local variable name and the DO passes that variable by reference. Otherwise, the element specifies an expression that the DO evaluates and passes as a value.
* The optional truth-valued expression following the parameter list, or the entryref if the argument contains no parameter list, specifies the argument postconditional and controls whether YottaDB/GT.M performs a DO using that argument.
* An indirection operator and an expression atom evaluating to a list of one or more DO arguments form a legal argument for a DO.

An explicit or implicit QUIT within the scope of the DO, but not within the scope of any other DO, FOR, XECUTE, or extrinsic, returns execution to the instruction following the calling point. This point may be the next DO argument or another command. At the end of a routine, or the end of a nesting level created by an argumentless DO, YottaDB/GT.M performs an implicit QUIT. Any line that reduces the current level of nesting by changing the number of leading periods (.) causes an implicit QUIT, even if that line only contains a comment. Terminating the image and execution of ZGOTO commands are the only ways to avoid eventually returning execution to the calling point.

A DO command may optionally pass parameters to the invoked subroutine. For more information about entryrefs and parameter passing, refer to Chapter 5: “General Language Features of M”.

+++++++++++++++++++++
Examples of DO
+++++++++++++++++++++

Example:

.. parsed-literal::
   GTM>DO ^%RD

This example invokes the routine directory utility program (%RD) from Direct Mode. The caret symbol (^) specifies that the DO command invokes %RD as an external routine.

Example:

.. parsed-literal::
   GTM>DO A(3)

This example invokes the subroutine at label A and passes the value 3 as a parameter. The DO argument does not have a caret symbol (^), therefore, it identifies A as a label in the current routine.

Example:

.. parsed-literal::
   ReportA ; Label for ReportA
           SET di="" OPEN outfile USE outfile 
           FOR  SET di=$ORDER(^div(di)) QUIT:di=""  DO PREP DO  DO POST
           .SET de="",(nr,gr)=0
           .WRITE "Division ",di,! F   S de=$ORDER(^de(di,de)) QUIT:de=""   DO
           ..WRITE "Department ",de," Gross Rev: ",^grev(di,de),!
           ..WRITE "Department ",de," Net Rev: ",^nrev(di,de),!
           ..SET gr=gr+^grev(di,de),nr=nr+^nrev(di,de)
           .W "Division Gross Rev: ",gr,!,"Division Net Rev: ",nr,!
           DO PRINT^OUTPUT(outfile)
           QUIT

This routine first uses a DO with a label argument (PREP) to do some pre-processing. Then, it uses an argumentless DO to loop through each division of a company to format a report. Within the first argumentless DO, a second argumentless DO (line 4) loops through and formats each department within a division. After the processing of all departments, control returns to the first argumentless DO, which prints a summary of the division. Following processing of all divisions, a DO with a label argument (POST) does some post-processing. Finally, at the next-to-last line, the routine uses a DO that invokes a subroutine at a label (PRINT) in an external routine (^OUTPUT), passing the name of the output file (outfile) as a parameter.

Example:

.. parsed-literal::
   GTM>zprint ^SQR
   SQR(z);
     set revert=0
     if $view("undef") set revert=1 view "noundef"
     if z="" write "Missing parameter.",!     view:revert "undef" quit
     else  write z*z,! view:revert "undef" quit  
   GTM>do ^SQR(10)
   100
   GTM>do ^SQR
   Missing parameter.

This examples demonstrates label invocations using DO with and without parentheses.

-------------
Else
-------------

ELSE executes the remainder of the line after the ELSE if $TEST is FALSE (0). YottaDB/GT.M does not execute the rest of the line if $TEST is TRUE (1).

The format of the ELSE command is:

.. parsed-literal::
   E[LSE]

* Because ELSE is a conditional command, it does not support a command postconditional.
* The scope of the ELSE is the remainder of the line. The scope of an ELSE can be extended with DO (or XECUTE) commands.
* Because the ELSE has no argument, at least two (2) spaces must follow the command to separate it from the next command on the line.

Because the scopes of both the IF and the ELSE commands extend to the rest of the YottaDB/GT.M line, placing an ELSE on the same line as the corresponding IF cannot achieve the desired result (unless the intent of the ELSE is to test the result of a command using a timeout). If an ELSE were placed on the same line as its corresponding IF, then the expression tested by the IF would be either TRUE or FALSE. If that condition is TRUE, the code following the ELSE would not execute; if that condition is FALSE, the ELSE would not be in the execution path.

ELSE is analogous to IF '$TEST, except the latter statement switches $TEST to its complement and ELSE never alters $TEST.

.. note::
   Use ELSE with care. Because YottaDB/GT.M stacks $TEST only at the execution of an extrinsic or an argumentless DO command, any XECUTE or DO with an argument has the potential side effect of altering $TEST. For information about $TEST, refer to “$Test”.

+++++++++++++++++
Examples of ELSE
+++++++++++++++++

Example:

.. parsed-literal::
   If x=+x Set x=x+y
   Else  Write !,x

The IF command evaluates the conditional expression x=+x and sets $TEST. If $TEST=1 (TRUE), YottaDB/GT.M executes the commands following the IF. The ELSE on the following line specifies an alternative action to take if the expression is false.

Example:

.. parsed-literal::
   If x=+x Do ^GOFISH
   Else  Set x=x\_"^"_y

The DO with an argument after the IF raises the possibility that the routine ^GOFISH changes the value of $TEST, thus making it possible to execute both the commands following the IF and the commands following the ELSE.

Example:

.. parsed-literal::
   Open dev::0 Else  Write !,"Device unavailable" QUIT

This ELSE depends on the result of the timeout on the OPEN command. If the OPEN succeeds, it sets $TEST to one (1) and YottaDB/GT.M skips the rest of the line after the ELSE. If the OPEN fails, it sets $TEST to zero (0), and YottaDB/GT.M executes the remainder of the line after the ELSE.


----------
For
----------

The FOR command provides a looping mechanism in GT.M. FOR does not generate an additional level in the M standard stack model.

The format of the FOR command is:

.. parsed-literal::
   F[OR][lvn=expr[:numexpr1[:numexpr2]][,...]]]

* Because FOR is a conditional command, it does not support a command postconditional.
* The scope of the FOR is the remainder of the line. The scope of a FOR can be extended with DO (or XECUTE) commands.
* When the FOR has no argument, at least two (2) spaces must follow the command to separate it from the next command on the line. This specifies a loop that must be terminated by a QUIT, HALT, GOTO, or ZGOTO.
* The optional local variable name specifies a loop control variable delimited by an equal sign (=). A FOR command has only one control variable, even when it has multiple arguments.
* When initiating the FOR, YottaDB/GT.M assigns the loop control variable the value of the expression. When only an initial value appears, YottaDB/GT.M executes the remainder of the line once for that argument without forcing the control variable to be numeric.
* If the argument includes an increment and, optionally, a terminator, YottaDB/GT.M treats the initial expression as a number.
* The optional numeric expression after the first colon (:) delimiter specifies the increment for each iteration. The FOR command does not increment the control variable on the first iteration.
* The optional numeric expression after the second colon (:) delimiter specifies the limiting value for the control variable. This terminating expression is evaluated only when the control variable is initialized to the corresponding initial value, then used for all subsequent iterations.
* YottaDB/GT.M does not execute the commands on the same line following the FOR if:
   * The increment is non-negative and the initial value of the control variable is greater than the limiting value.
   * The increment is negative and the initial value of the control variable is less than the limiting value.

* After the first iteration, GT.M does not alter the control variable and ceases execution under the control of the FOR if:
   * The increment is non-negative, and altering the control variable by the increment would cause the control variable to be greater than the limiting value.
   * The increment is negative, and altering the control variable by the increment would cause the control variable to be less than the limiting value.

* When the FOR has multiple arguments, each one affects the loop control variable in sequence. For an argument to gain control, no prior argument to the FOR can have an increment without a limit.

Increments and limits may be positive, negative, an integer, or a fraction. YottaDB/GT.M never increments a FOR control variable "beyond" a limit. Other commands may alter a control variable within the extended scope of a FOR that it controls. When the argument includes a limit, such modification can cause the FOR argument to yield control at the start of the next iteration, or, less desirably loop indefinitely.

NOUNDEF does not apply to an undefined FOR control variable. This prevents an increment of an undefined FOR control variable from getting into an unintended infinite loop. For example, FOR A=1:1:10 KILL A gives an UNDEF error on the increment from 1 to 2 even with VIEW "NOUNDEF".

YottaDB/GT.M terminates the execution of a FOR when it executes an explicit QUIT or a GOTO (or ZGOTO in YottaDB/GT.M) that appears on the line after the FOR. FOR commands with arguments that have increments without limits and argumentless FORs can be indefinite loops. Such FORs must terminate with a (possibly postconditional) QUIT or a GOTO within the immediate scope of the FOR. FORs terminated by such commands act as "while" or "until" control mechanisms. Also, such FORs can, but seldom, terminate by a HALT within the scope of the FOR as extended by DOs, XECUTEs, and extrinsics. 

++++++++++++++++++
Examples of FOR
++++++++++++++++++

Example:

.. parsed-literal::
   GTM>Kill i For i=1:1:5 Write !,i
   1
   2
   3
   4
   5
   GTM>Write i
   5
   GTM>

This FOR loop has a control variable, i, which has the value one (1) on the first iteration, then the value two (2), and so on, until in the last iteration i has the value five (5). The FOR terminates because incrementing i would cause it to exceed the limit. Notice that i is not incremented beyond the limit.

Example:

.. parsed-literal::
   GTM>FOR x="hello",2,"goodbye" WRITE !,x
   hello
   2
   goodbye
   GTM>

This FOR loop uses the control variable x and a series of arguments that have no increments or limits. Notice that the control variable may have a string value.

Example:

.. parsed-literal::
   GTM>For x="hello":1:-1 Write !,x
   GTM>ZWRite x
   x=0
   GTM>

Because the argument has an increment, the FOR initializes the control variable x to the numeric evaluation of "hello" (0). Then, YottaDB/GT.M never executes the remainder of the line because the increment is positive, and the value of the control variable (0) initializes to greater than the limiting value (-1).

Example:

.. parsed-literal::
   GTM>For y=-1:-3:-6,y:4:y+10,"end" Write !,y
   -1
   -4
   -4
   0
   4
   end
   GTM>

This FOR uses two limited loop arguments and one value argument. The first argument initializes y to negative one (-1), then increments y to negative four (-4). Because another increment would cause y to be less than the limit (-6), the first argument terminates with y equal to negative four (-4). The second argument initializes the loop control variable to its current value and establishes a limit of six (6=-4+10). After two iterations, incrementing y again would cause it to be greater than the limit (6), so the second argument terminates with y equal to four (4). Because the final argument has no increment, the FOR sets y to the value of the third argument, and YottaDB/GT.M executes the commands following the FOR one more time.

Example:

.. parsed-literal::
   GTM>Set x="" For  Set x=$Order(ar(x)) Quit:x=""  Write !,x

This example shows an argumentless FOR used to examine all first level subscripts of the local array ar. When $ORDER() indicates that this level contains no more subscripts, the QUIT with the postconditional terminates the loop.


-----------------
Goto
-----------------

The GOTO command transfers execution to a location specified by its argument.

The format of the GOTO command is:

.. parsed-literal::
   G[OTO][:tvexpr] entryref[:tvexpr][,...]

* The optional truth-valued expression immediately following the command is a command postconditional that controls whether or not YottaDB/GT.M executes the command.
* The required entryref specifies the target location for the control transfer.
* The optional truth-valued expression immediately following the entryref specifies the argument postconditional, and controls whether YottaDB/GT.M performs a GOTO with that argument.
* Additional commands on a line following a GOTO do not serve any purpose unless the GOTO has a postconditional.
* An indirection operator and an expression atom evaluating to a list of one or more GOTO arguments form a legal argument to a GOTO.

A GOTO command within a line following a FOR command terminates that FOR command.

For more information on entryrefs, refer to Chapter 5: “General Language Features of M”.

++++++++++++++++++++++++
Examples of GOTO
++++++++++++++++++++++++

Example:

.. parsed-literal::
   GTM>GOTO TIME+4

This GOTO command transfers control from Direct Mode to the line that is four (4) lines after the line labeled TIME (in the currently active routine). Using an offset is typically a debugging technique and rarely used in production code.

Example:

.. parsed-literal::
   GOTO A:x<0,^A:x=0,A^B

This GOTO command transfers control to label A in the current routine, if x is less than zero (0), to routine ^A if x is equal to zero (0), and otherwise to label A in routine ^B. Once any of the transfers occurs, the rest of the arguments have no effect.

--------------------------
Halt
--------------------------

The HALT command stops the program execution and causes YottaDB/GT.M to return control to the operating system environment that invoked the YottaDB/GT.M image.

The format of the HALT command is:

.. parsed-literal::
   H[ALT][:tvexpr]

* The optional truth-valued expression immediately following the command is a command postconditional that controls whether YottaDB/GT.M executes the command.
* Because the HALT command has no argument, at least two (2) spaces must follow the command to separate it from the next command on the line. Note that additional commands do not serve any purpose unless the HALT has a postconditional.

A HALT releases all shared resources held by the process, such as devices OPENed in YottaDB/GT.M, databases, and YottaDB/GT.M LOCKs. If the the process has an active M transaction (the value of $TLEVEL is greater than zero (0)), YottaDB/GT.M performs a ROLLBACK prior to terminating.

Because HALT and HANG share the same abbreviation (H), YottaDB/GT.M differentiates them based on whether an argument follows the command.

Example:

.. parsed-literal::
   $ gtm
   GTM>HALT
   $

Because we invoke this YottaDB/GT.M image interactively, the HALT in Direct Mode leaves the process at the shell prompt.

-------------------
Hang
-------------------

The HANG command suspends YottaDB/GT.M program execution for a period of time specified by the command argument.

The format of the HANG command is:

.. parsed-literal::
   H[ANG][:tvexpr] numexpr[,...]

* The optional truth-valued expression immediately following the command is a command postconditional that controls whether or not YottaDB/GT.M executes the command.
* The numeric expression specifies the time in seconds to elapse before resuming execution; actual elapsed time may vary slightly from the specified time. If the numeric expression is negative, HANG has no effect. Portability requirements for YottaDB/GT.M only guarantee accuracy to the nearest second. However, more accuracy can be found on different UNIX systems.
* An indirection operator and an expression atom evaluating to a list of one or more HANG arguments form a legal argument to a HANG.

A process that repeatedly tests for some event, such as a device becoming available or another process modifying a global variable, may use a HANG to limit its consumption of computing resources.

Because HALT and HANG share the same abbreviation (H), YottaDB/GT.M differentiates them based on whether an argument follows the command.

++++++++++++++++++
Examples of HANG
++++++++++++++++++

Example:

.. parsed-literal::
   For  Quit:$Data(^CTRL(1))  Hang 30

This FOR loop repeatedly tests for the existence of ^CTRL(1), and terminates when that global variable exists. Otherwise the routine HANGs for 30 seconds and tests again.

Example:

.. parsed-literal::
   SET t=1 For  Quit:$Data(^CTRL(1))  Hang t If t<30 Set t=t+1

This is similar to the previous example, except that it uses an adaptive time that lengthens from 1 second to a limit of 30 seconds if the routine stays in the loop.

-------------
If
-------------

The IF command provides conditional execution of the remaining commands on the line. When IF has an argument, it updates $TEST with the truth value of its evaluated argument. YottaDB/GT.M executes the remainder of a line after an IF statement when $TEST is 1 (TRUE). When $TEST is 0 (FALSE), YottaDB/GT.M does not execute the rest of the line. When the IF argument evaluates to a literal FALSE (0), YottaDB/GT.M discards the command and its arguments at compile time, which means it does not perform any validity checking on the remainder of the line.

The format of the IF command is:

.. parsed-literal::
   I[F] [tvexpr[,...]]


* Because IF is a conditional command, it does not support a command postconditional.
* The scope of the IF is the remainder of the line. The scope of an IF can be extended with DO (or XECUTE) commands.
* The action of IF is controlled by the value of the expression and by $TEST, if there is no expression.
* IF with no argument acts on the existing value of $TEST (which it does not change); in this case, at least two (2) spaces must follow the IF to separate it from the next command on the line.
* An indirection operator, and an expression atom evaluating to a list of one or more IF arguments form a legal argument to IF.

.. note::
   Commands with timeouts also maintain $TEST. For information about $TEST, refer to Chapter 8: “Intrinsic Special Variables”. Because YottaDB/GT.M stacks $TEST only at the execution of an extrinsic or an argumentless DO command, any XECUTE or DO with an argument has the potential side effect of altering $TEST. Use the argumentless IF with caution.

Example:

.. parsed-literal::
   IF A,B ...
   is equivalent to
   IF A IF B

An IF with more than one argument behaves as if those arguments were logically "ANDed." However, execution of the line ceases with the evaluation of the first false argument. For IF argument expressions containing the "AND" operator (&), by default, execution still ceases with the evaluation of the first false argument, however any global references within the expression act in sequence to maintain the naked reference. The "FULL_BOOLEAN" and "SIDE_EFFECTS" compiler settings modify this behavior if you desire YottaDB/GT.M to provide side effects it would otherwise bypass due to short-circiuting of Boolean expressions.

Postconditionals perform a function similar to IF; however, their scope is limited to a single command or argument, and they do not modify $TEST. For more information on postconditionals, see Chapter 5: “General Language Features of M”.

++++++++++++++++
Examples of IF
++++++++++++++++

Example:

.. parsed-literal::
   IF x=+x!(x="") Do BAL

In this example, the DO executes if x contains a number or a null string.

Example:

.. parsed-literal::
   Write !,?50,BAL If 'BAL Write "\*\*\*\*"
   IF  Set EMPTY(acct)=""

The IF in the first line changes the value of $TEST, determining the execution of the code following the argumentless IF in the second line. Such argumentless IFs may serve as a form of line continuation.

Example:

.. parsed-literal::
   GTM>Set X=1,Y=1,Z=2 Kill UNDEF
   GTM>If X=1,Y=1,Z=3,UNDEF=0 Write "HI"
   GTM>

The IF command causes YottaDB/GT.M to cease executing the line after it determines Z is not equal to three (3). Therefore, GT.M never evaluates the reference to the undefined variable and never generates an error.

Example:

.. parsed-literal::
   GTM>Set X=1 Kill UNDEF
   GTM>If X=1!(UNDEF=3) Write "HI"
   HI
   GTM>

Because YottaDB/GT.M recognizes that the X=1 fulfills the IF, it skips evaluation of the UNDEF variable and executes this IF command without generating an error. Because YottaDB/GT.M does not require such optimizations and in fact discourages them by requiring that all global references maintain the naked indicator, other implementations may generate an error.

-----------------
Job
-----------------

The JOB command initiates another YottaDB/GT.M process that executes the named routine.

$ZJOB is set to the pid of the process created by the JOB command. For more details, refer to “$ZJob”.

The format of the JOB command is:

.. parsed-literal::
   J[OB][:tvexpr] entryref[(expr[,...])]
   [:[(keyword[=value][:...])][:numexpr]][,...]

* The optional truth-valued expression immediately following the command is a command postconditional that controls whether or not YottaDB/GT.M executes the command.
* The required entryref specifies a location at which the new process starts.
* The optional parameter list enclosed in parentheses () contains parameters to pass to the routine entry point.
* If the JOB specifies a parameter list, the entryref location must start with a label and a formallist. M prohibits entryrefs with offsets during parameter passing.
* The optional elements in the parameter list specify expressions that the JOB evaluates and passes as values; because the JOB command creates a new process, its arguments cannot specify pass-by-reference.
* The keywords specify optional processparameters that control aspects of the environment for the new process.
* If the JOB command has only one processparameter, the surrounding parentheses are optional.
* Some keywords take numeric or string literals delimited by an equal sign (=) as arguments. Because the values are constants, strings must be enclosed in quotation marks (" "), and variable arguments require that the entire argument be constructed and referenced using indirection.
* The optional numeric expression specifies a time in seconds after which the command should timeout if unsuccessful; 0 results in a single attempt.
* When a JOB command contains no processparameters, double colons (::) separate the time-out numeric expression from the entryref.
* An indirection operator and an expression atom, evaluating to a list of one or more JOB command arguments, form a legal argument for a JOB command.
* The maximum command-line length for a JOB command is 8192 bytes.
* If the parent process is operating in UTF-8 mode, the JOB'd process also operates in UTF-8 mode.
* If your background process must have a different mode from its parent, then create a shell script to alter the environment as needed, and spawn it with a ZSYstem command using ZSYstem "/path/to/shell/script &".

The operating system deletes the resultant process when execution of its YottaDB/GT.M process is complete. The resultant process executes asynchronously with the current process. Once YottaDB/GT.M starts the resultant process, the current process continues.

If a JOB command specifies a timeout, and YottaDB/GT.M creates the resultant process before the timeout elapses, JOB sets $TEST to true (1). If YottaDB/GT.M cannot create the process within the specified timeout, JOB sets $TEST to false (0). If a JOB command does not specify a timeout, the execution of the command does not affect $TEST.

If YottaDB/GT.M cannot create the process because of something that is unlikely to change during the timeout interval, such as invalid DEFAULT directory specification, or the parameter list is too long, the JOB command generates a run-time error. If the command does not specify a timeout and the environment does not provide adequate resources, the process waits until resources become available to create the resultant process.

+++++++++++++++++++++++++++++
The JOB Environment
+++++++++++++++++++++++++++++

When the JOB is forked, UNIX creates the environment for the new process by copying the environment of the process issuing the JOB command and making a few minor modifications. By default, the standard input is assigned to the null device, the standard output is assigned to routinename.mjo, and the standard error is assigned to routinename.mje.

**JOB Implications for Directories**

By default, YottaDB/GT.M uses the current working directory of the parent process for the working directory of the initiated process.

If the files specified by processparameters, do not exist, and YottaDB/GT.M does not have permission to create them, the JOBed process terminates. When the corresponding files are in the current working directory, the OUTPUT, INPUT, and ERROR processparameters do not require a full pathname.

+++++++++++++++++++++++++
JOB processparameters
+++++++++++++++++++++++++

The following sections describe the processparameters available for the JOB command in YottaDB/GT.M.

**CMD[LINE]="strlit"**

The string literal specifies the $ZCMDLINE of the JOB'd process.

**DEF[AULT]=strlit**

The string literal specifies the default directory.

The maximum directory length is 255 characters.

If the JOB command does not specify a DEFAULT directory, YottaDB/GT.M uses the current default directory of the parent process.

**ERR[OR]=strlit**

strlit specifies the stderr of the JOBbed process. strlit can either be a file or a DETACHed socket (that is, a socket from the socket pool). To pass a DETACHed socket as the stderr of the JOBbed process, specify strlit in the form of "SOCKET:<handle>" where <handle> is the socket handle. On successful completion of the JOBbed process, the passed socket is closed and is no longer available to the parent process.

The maximum string length is 255 characters.

By default, JOB constructs the error file from the routinename using a file extension of .mje: the default directory of the process created by the JOB command.

**GBL[DIR]=strlit**

The string literal specifies a value for the environment variable gtmgbldir.

The maximum string length is 255 characters.

By default, the job uses the same specification for gtmgbldir as that defined in $ZGBLDIR for the process using the JOB command.

**IN[PUT]=strlit**

strlit specifies the stdin of the JOBbed process. strlit can either be a file or a DETACHed socket (that is, a socket from the socket pool). To pass a DETACHed socket as the stdin of the JOBbed process, specify strlit in the form of "SOCKET:<handle>" where <handle> is the socket handle. On successful completion of the JOB command, the passed socket is closed and is no longer available to the parent process.

.. note::
   Specify a DETACHed socket in both INPUT and OUTPUT parameters to pass it as the $PRINCIPAL of the JOBbed process. 

The maximum string length is 255 characters.

YottaDB/GT.M does not supply a default file extension.

By default, the job takes its input from the null device.

**OUT[PUT]=strlit**

strlit specifies the stdout of the JOBbed process. strlit can either be a file or a DETACHed socket (that is, a socket from the socket pool). To pass a DETACHed socket as the stdout of the job, specify strlit in the form of "SOCKET:<handle>" where <handle> is the socket handle. On successful completion of the JOB command, the passed socket is closed and is no longer available to the parent process.

.. note::
   Specify a DETACHed socket in both INPUT and OUTPUT parameters to pass it as the $PRINCIPAL of the JOBbed process.

The maximum string length is 255 characters.

By default, JOB constructs the output file pathname from the routinename using a file extension of .mjo and the current default directory of the process created by the JOB command.

**PASS[CURLVN]**

With the PASSCURLVN jobparameter, the JOB'd process inherits the current collation, local variables, aliases, and alias containers from the current stack level of the parent process. Therefore, a ZWRITE in the JOB'd process has the same output, except for any out of scope aliases, as a ZWRITE in the context of the JOB command. If the JOB command finds a ZWRITE representation of any lvn, consisting of its full name, its subscripts, corresponding value, quotes and the equal-sign (=), exceeding 1MiB, it produces a JOBLVN2LONG error in the parent process, and a JOBLVNDETAIL error in the error output stream of the JOB'd process. If a JOB command does not specify PASSCURLVN, the JOB'd process(es) inherits no local variables from the parent, although it can receive values passed as parameters to an actuallist entryref. While not an inexpensive command, you can use the "exclusive" NEW command to control the context passed to the JOB'd process; for example, adding "NEW (LOCALA,LOCALB)" before the JOB command would pass only LOCALA and LOCALB.

If a parameter in the formal list of JOB'ed entryref shares the same name with a local in the parent process, the parameter passing facility applies the actuallist in the JOB command argument to the formallist at the invoked label superseding any local variable passed from the parent process by the PASSCURLVN option.

**STA[RTUP]="/path/to/shell/script"**

Specifies the location of the shell script that executes before running the named routine.

The JOBbed process spawns a shell session to execute the shell script. If the shell script fails, the JOB'd process terminates without running the named routine. Because STARTUP executes in a separate shell, it has no impact on the environment of the JOB'd process, which is inherited from the parent. STARTUP is useful for actions such as creating directories. Use PIPE devices instead of the JOB command to control the environment of a spawned process.

**JOB Processparameter Summary Table**

The processparameters are summarized in the following table.

+---------------------------+--------------------------+---------------------------------+------------------------------------+
| Parameter                 | Default                  | Minimum                         | Maximum                            |
+===========================+==========================+=================================+====================================+
| DEF[AULT]=strlit          | Same directory as the    | none                            | 255 characters                     |
|                           | process issuing the JOB  |                                 |                                    |
|                           | command                  |                                 |                                    |
+---------------------------+--------------------------+---------------------------------+------------------------------------+
| ERR[OR]=strlit            | ./routinename.mje        | none                            | 255 characters                     |
+---------------------------+--------------------------+---------------------------------+------------------------------------+
| GBL[DIR]                  | Same as gtmgbldir for the| none                            | 255 characters                     |
|                           | process issuing the JOB  |                                 |                                    |
|                           | command                  |                                 |                                    |
+---------------------------+--------------------------+---------------------------------+------------------------------------+
| IN[PUT]=strlit            | Null device              | none                            | 255 characters                     |
+---------------------------+--------------------------+---------------------------------+------------------------------------+
| OUT[PUT]=strlit           | ./routinename.mjo        | none                            | 255 characters                     |
+---------------------------+--------------------------+---------------------------------+------------------------------------+
| PASS[CURLVN]              | Only pass any formallist | N/A                             | ZWRITE key/value representations of|
|                           | values                   |                                 | any lvn must not exceed 1MiB       |
+---------------------------+--------------------------+---------------------------------+------------------------------------+
| STA[RTUP]=strlit          | none                     | none                            | Determined by the maximum length a |
|                           |                          |                                 | file pathname can have on the      |
|                           |                          |                                 | operating system, which is at least|
|                           |                          |                                 | 255 bytes on all systems which     |
|                           |                          |                                 | YottaDB/GT.M currently supports.   |
+---------------------------+--------------------------+---------------------------------+------------------------------------+

+++++++++++++++++++++++++
Examples of JOB
+++++++++++++++++++++++++

Example:

.. parsed-literal::
   GTM>JOB ^TEST("V54001","")

This creates a job that starts doing the routine ^TEST (with 2 parameters) in the current working directory.

Example:

.. parsed-literal::
   JOB PRINTLABELS(TYPE,PRNTR,WAITIM)

This passes three values (TYPE, PRNTR, and WAITIM) to the new job, which starts at the label PRINTLABELS of the current routine.

Example:

Refer to the sockexamplemulti31.m program in Using Socket Devices section for more examples on the JOB command.

----------------
Kill
----------------

The KILL command deletes local or global variables and their descendant nodes.

The format of the KILL command is:

.. parsed-literal::
   K[ILL][:tvexpr] [glvn | (glvn[,...]) | \*lname | \*lvn ]

* The optional truth-valued expression immediately following the command is a command postconditional that controls whether or not YottaDB/GT.M executes the command.
* The optional global or local variable name specifies the variable to delete; KILL deletes not only the variable specified in the argument, but also all variables descended from that variable, that is, those starting with the identical key-prefix.
* KILLing a variable that does not currently exist has no effect.
* The KILL command without an argument deletes all currently existing local variables; in this case, at least two (2) spaces must follow the KILL to separate it from the next command on the line.
* When a KILL argument consists of local variable names enclosed in parentheses, that "exclusive" KILL deletes all local variables except those listed in the argument.
* KILL does not affect copies of local variables that have been "stacked" by NEW or parameter passing with the possible exception of the following: For KILL arguments enclosed in parentheses, the environment variable gtm_stdxkill enables the standard-compliant behavior to kill local variables in the exclusion list if they had an explicit or implicit (pass-by-reference) alias not in the exclusion list. By default, this behavior is disabled. If gtm_stdxkill is set to 1,"TRUE", or "YES", KILL deletes a local variable unless all its names are in the parenthesized list. If gtm_stdxkill is not defined or set to 0 KILL operations exclude the data associated with an item if any one of its names appears in the parenthesized list. While non-standard, the default behavior decouples call-by-reference functions or functions using aliases from needing knowledge of the caller's parameters.
* In conformance with the M standard, KILL of a variable joined by pass-by-reference to a formallist variable always KILLs the formalist variable when the actuallist variable is KILL'd even if the formallist variable is specified as protected by an exclusive KILL.
* KILL * removes the association between its argument and any associated arrays. The arguments are left undefined, just as with a standard KILL. If the array has no remaining associations after the KILL \*, YottaDB/GT.M can reuse the memory it occupied. If there are no array(s) or association(s) the KILL * happily and silently does nothing.
* KILL * of an alias container variable is just like a KILL of an alias variable, and deletes the association between the lvn and the array.
* KILL * treats an alias formed though pass-by-reference the same as any alias variable by removing the alias association.
* KILL * with no arguments removes all aliases and alias containers connections.
* You can intermix KILL and KILL * in an argument list. For example, KILL \*A,B
* Kill * is not permitted inside a parenthesized list of exclusions, e.g.: KILL (\*A) is an error.
* An exclusive KILL where one associated name is inside the parenthetic list of exclusions and another associated name is not with that list kills the array through the name that is not inside the list. The association, however, is preserved.
* For more information and KILL * examples, refer to “Alias Variables Extensions”.
* An indirection operator and an expression atom evaluating to a list of one or more KILL arguments form a legal argument for a KILL.

.. note::
   Use KILL with caution because it can have a major impact on the process environment (local variables) or shared data (global variables). 

+++++++++++++++++++++
Examples of KILL
+++++++++++++++++++++

Example:

.. parsed-literal::
   GTM>Kill  Set a=0,a(1)=1,a(1,1)="under" KILL a(1) ZWR
   a=0
   GTM>

This uses an argumentless KILL to get a "fresh start" by deleting all existing local variables. After SETting a, a(1), and a(1,1), the KILL deletes a(1) and its descendants. The ZWRITE shows only a remaining.

Example:

.. parsed-literal::
   GTM>Kill (a,b),^AB(a,b)

The first argument (an exclusive KILL) specifies to KILL all local variables except a and b. The second argument deletes ^AB(a,b) and any descendants of that global variable node.

Example:

.. parsed-literal::
   kill *
   write !,"gtm_stdxkill=",+$ztrnlnm("gtm_stdxkill"),!
   set (A,B,C,E)="input"
   do X(.A,.B)
   zwrite
   write !,"____________",!
   set (A,B,C,E)="input"
   do Y(.A,.B)
   zwrite
   write !,"____________",!
   set (A,B,C,E)="base"
   set \*C=A,\*D=B
   kill (C,D)
   zwrite
   quit
   X(C,D)    set (C,D)="output"
   kill (C,D)
   quit
   Y(C,D)    set (C,D)="output"
   kill (A,C,D)
   quit

Produces the following output:

.. parsed-literal::
   gtm_stdxkill=0
   A="output"
   B="output"
   C="input"
   ____________
   A="output"
   B="output"
   C="input"
   ____________
   A="base" ;*
   B="base" ;*
   \*C=A
   \*D=B

----------------------
Lock
----------------------

The LOCK command reserves and releases resource names, and provides a semaphore capability for YottaDB/GT.M processes. This capability can be used for interprocess synchronization and signaling.

Assigning a LOCK does not specify any explicit control over variables and does not directly effect either read or write access to global (or local) data. However, an application that adheres to clearly defined conventions of LOCKing before any access can indirectly achieve such an effect.

YottaDB/FIS recommends implementing database Consistency using transaction processing rather than LOCKs. If you wish to avoid YottaDB/GT.M's use of optimistic concurrency for TP, place the LOCK just before the original TSTART and release it after the final TCOMMIT.

The format of the LOCK command is:

.. parsed-literal::
   L[OCK][:tvexpr] [[-\|+]nref|(nref[,...])[:numexpr] [,...]]

* The optional truth-valued expression immediately following the command is a command postconditional that controls whether or not YottaDB/GT.M executes the command.
* The nref argument specifies a resource name in the format of the YottaDB/GT.M name, with or without subscripts and with or without a preceding caret (^). An nref can optionally have an environment specification, including one without a preceding caret (^).
* Outside of transactions, only one process in an environment can own a particular LOCK at any given time.
* Because the data storage in YottaDB/GT.M uses hierarchical sparse arrays, and LOCK frequently serves to protect that data from inappropriate "simultaneous" access by multiple processes, LOCK treats resource names in a hierarchical fashion; a LOCK protects not only the named resource, but also its ancestors and descendants.
* When one or more nrefs are enclosed in parentheses (), LOCK reserves all the enclosed names "simultaneously," that is, it reserves none of them until all become available.
* A LOCK with no argument or an argument with no leading sign releases all names currently reserved with previous LOCK commands by the process; when a LOCK has no argument, at least two (2) spaces must follow the LOCK to separate it from the next command on the line.
* A LOCK argument with a leading plus sign (+) acquires the named resources without releasing currently held resources; if the named resource is already LOCKed, such a LOCK "counts up" the process interest in the resource.
* A LOCK argument with a leading minus sign (-) "counts down" the process interest in a named resource; if the count on a particular lock reaches zero (0), YottaDB/GT.M releases the lock without releasing any other currently held locks; a LOCK that releases a named resource not currently owned by the process has no effect.
* YottaDB/GT.M allows the "process interest" lock counter on a named resource to increment up to 511.
* The optional numeric expression specifies a time in seconds after which the command should timeout if unsuccessful; 0 provides a single attempt; timed LOCK commands maintain $TEST: 1 for a successful LOCK action, 0 for an unsuccessful (within the specified time) LOCK action. Note that untimed LOCK commands do not change $TEST.
* A LOCK operation that finds no room in LOCK_SPACE to queue a waiting LOCK so another process releasing a blocking LOCK can wake it, does a slow poll waiting for LOCK_SPACE to become available. If LOCK does not acquire the ownership of the named resource with the specified timeout, it returns control to the application with $TEST=0. If timeout is not specified, LOCK continues slow poll till space becomes available.
* If a LOCK command in a TP transaction specifies no timeout or a timeout that exceeds the limit specified by $gtm_tpnotacidtime when 2 is less than $TRESTART, the process releases the database critical sections and generates TPNOACID messages, which may live-lock the process, possibly until the transaction terminates because it reaches $ZMAXTPTIME. While such a process may have an impact on system performance this behavior moderates the impact of potential deadlocks on other database operations.
* An indirection operator and an expression atom evaluating to a list of one or more LOCK arguments form a legal argument for a LOCK.

YottaDB/GT.M records LOCK and ZALLOCATE information in the "lock database." YottaDB/GT.M distributes the lock database in space associated with the database identified by the current Global Directory. However, the lock database does not overlap or coincide with the body of the database files holding the global data. Only the LOCK, ZALLOCATE and ZDEALLOCATE commands, and the LKE utility program access the lock database.

YottaDB/GT.M maps reservations of names starting with ^ to the database file used to map global variables of the same name. If the Global Directory maps the name A to file A.DAT, YottaDB/GT.M maps all reservations on ^A to file space associated with A.DAT.

YottaDB/GT.M maps reservations on names not starting with ^ to the region of the database specified with the GDE command LOCK -REGION=. By default, when GDE creates a Global Directory any reservations of local names are mapped to the region DEFAULT.

These two factors effect the following result in the programming environment:

* ^ reservations automatically intersect for all users of the same data in any database file independent of the Global Directory mapping that file.
* reservations without a leading ^ intersect in an arbitrary pattern dependent on the Global Directory and therefore controlled by a design decision potentially made independently of application code design.

Since YottaDB/GT.M uses resource names as semaphores for signaling among multiple processes in a database environment, they interlock in a tree structured fashion. When LOCK or ZALLOCATE reserves a subscripted resource name such as ^D(1), other users of the database mapped by the LOCKing (or ZALLOCATEing) process cannot reserve ancestors of that name, such as ^D, or descendants, such as ^D(1,2), until LOCK or ZDEALLOCATE releases that name.

Execution of the LOCK command does not affect the value or the state of a variable. LOCK tests each argument to determine whether the process can claim the name space. If another YottaDB/GT.M process has a LOCK on that name space, YottaDB/GT.M suspends the current process until the other process releases the name space. To prevent the potential "indefinite" suspension of a routine execution, specify a timeout for the LOCK command.

LOCK with a leading plus (+) or minus (-) sign (incremental LOCKing) allows the acquisition and release of locks without releasing all currently held locks. This can lead to deadlocks. For example, a deadlock occurs if two users LOCK resources named A and B in the following sequence.

**Deadlock Situation**

+----------------------------------+------------------------------------+
| User X                           | User Y                             |
+==================================+====================================+
| L +A                             | L +B                               |
+----------------------------------+------------------------------------+
| L +B                             | L +A                               |
+----------------------------------+------------------------------------+

To avoid deadlocks, use LOCK without a leading + or - sign on its arguments because such a command releases all previously LOCKed resources, or uniformly implement well designed LOCK accumulation orders and/or use a timeout with the LOCK command.

If a LOCK command specifies a timeout, and YottaDB/GT.M acquires ownership of the named resource before the timeout elapses, LOCK sets $TEST to TRUE (1). If YottaDB/GT.M cannot acquire ownership of the named resource within the specified timeout, LOCK sets $TEST to FALSE (0). If a LOCK command does not specify a timeout, the execution of the command does not affect $TEST. If a LOCK with an argument having a leading minus sign (-) specifies a timeout, the command always sets $TEST to TRUE (1).

If a process issues a LOCK command for a named resource already ZALLOCATEd by that process, the resource is both ZALLOCATEd and LOCKed. LOCK does not release ZALLOCATEd resources. To release such a named resource, the process must both ZDEALLOCATE and unLOCK the resource. For more information, refer to “ZAllocate”.

Currently, LOCK of an argument within a parenthetical list where the argument includes an extrinsic function that performs LOCK, ZALLOCATE or ZDEALLOCATE actions produces a BADLOCKNEST error except where there is only one such argument, it is the first argument in the list and the LOCK'ng as a consequence of the extrinsic function(s) is simple. Note that this pattern may still produce some unintended outcomes, so FIS recommends against its use.

For more information on troubleshooting locks with the Lock Utility (LKE), refer to the chapter on that utility in the Administration and Operations Guide.

---------------------------------
Using Locks within Transactions
---------------------------------

Within transactions, LOCKs are used by YottaDB/GT.M to ensure the ability to serialize. There is no guarantee, however, that attempts by other processes to examine LOCKs held with a transaction will produce the same results as when LOCKs are outside of a transaction. In other words, LOCKs within transactions should never be used as simple semaphores.

The LOCK command locks a specified resource name that controls a tree structured name space. Outside of transactions when one process in an environment acquires a LOCK or a ZALLOCATE on a named resource, no other YottaDB/GT.M process in that environment can LOCK a resource with an "overlapping" name until the first process releases the LOCK that it holds.

For information on the use of LOCKs within transactions, refer to Chapter 5: “General Language Features of M”.

**Lock Command Operation Summary**

+-----------------+----------------------+------------------------------------------------------------------------------------------------------------+
| Commands Issued | Resulting Locks      | Comments                                                                                                   |
+=================+======================+============================================================================================================+
| L               | none                 | Remove all prior locks.                                                                                    |
+-----------------+----------------------+------------------------------------------------------------------------------------------------------------+
| L A             | A                    | Remove prior locks then lock A.                                                                            |
+-----------------+----------------------+------------------------------------------------------------------------------------------------------------+
| L               |                      | This sequence is equivalent to L A                                                                         |
| L +A            | A                    |                                                                                                            |
+-----------------+----------------------+------------------------------------------------------------------------------------------------------------+
| L A             |                      | Remove prior locks before locking A, then remove lock on A. This is equivalent to L A L                    |
| L -A            | none                 |                                                                                                            |
+-----------------+----------------------+------------------------------------------------------------------------------------------------------------+
| L A             |                      | Remove prior locks before locking A, increment lock on A without releasing prior lock on A, decrement lock |
| L +A            |                      | on A without releasing prior lock on A.                                                                    |
| L -A            | A                    |                                                                                                            |
+-----------------+----------------------+------------------------------------------------------------------------------------------------------------+
| L A             |                      | Remove prior locks before locking A, then lock B without releasing A.                                      |
| L +B            | A, B                 |                                                                                                            |
+-----------------+----------------------+------------------------------------------------------------------------------------------------------------+
| L A,B           | B                    | Remove prior locks before locking A, unlock A, then lock B.                                                |
+-----------------+----------------------+------------------------------------------------------------------------------------------------------------+
| L (A,B)         | A, B                 | Remove prior locks before locking A and B simultaneously.                                                  |
+-----------------+----------------------+------------------------------------------------------------------------------------------------------------+
| L A             |                      | Remove prior locks before locking A, lock B without releasing A, lock C without releasing A and B          |
| L +B            |                      |                                                                                                            |
| L +C            | A, B, C              |                                                                                                            |
+-----------------+----------------------+------------------------------------------------------------------------------------------------------------+
| L A             |                      | Remove prior locks before locking A, lock B and C simultaneously without releasing A.                      |
| L +(B,C)        | A, B, C              |                                                                                                            |
+-----------------+----------------------+------------------------------------------------------------------------------------------------------------+
| L (A,B,C)       |                      | Remove prior locks before locking A, B, and C simultaneously, remove lock on B without releasing A and C,  |
| L -B            |                      | remove lock on C without releasing A.                                                                      |
| L -C            | A                    |                                                                                                            |
+-----------------+----------------------+------------------------------------------------------------------------------------------------------------+
| L (A,B,C)       |                      | Remove prior locks before locking A, B, and C simultaneously, remove lock on B and C without releasing A.  |
| L -(B,C)        | A                    |                                                                                                            |
+-----------------+----------------------+------------------------------------------------------------------------------------------------------------+
| L (A,B)         |                      | Remove prior locks before locking A and B simultaneously, remove lock on B without releasing A.            |
| L -B            |                      |                                                                                                            |
+-----------------+----------------------+------------------------------------------------------------------------------------------------------------+

+++++++++++++++++++++
Examples of LOCK
+++++++++++++++++++++

Example:

.. parsed-literal::
   Lock A,^B,@C
   Lock (A,B,@C)

The first LOCK command LOCKs A and unLOCKs A before LOCKing ^B, then unLOCKs ^B before locking the name specified by the variable C. The second LOCK command acquires all three resources at once. YottaDB/GT.M waits until all the named resources in the argument list become available before LOCKing all the resources. For example, if the resource specified by the variable C is not available for LOCKing, YottaDB/GT.M waits until that resource becomes available before LOCKing A and ^B.

Example:

.. parsed-literal::
   LOCK (A,B)
   LOCK +C
   LOCK -B

This LOCKs A and B, then incrementally LOCKs C. Finally it releases the LOCK on B, while retaining the LOCKs on A and C.

Example:

.. parsed-literal::
   LOCK (A,B,C) 
   LOCK +(B,C)
   LOCK -(B)

This LOCKs A, B and C together. It then increments the lock "counts" of B and C. The last LOCK command removes one "count" of B, leaving one count of A and B and two counts of C.

Example:

.. parsed-literal::
   LOCK ^D:5

This command attempts to LOCK ^D with a timeout of five seconds. If LOCK acquires the named resource before the timeout elapses, YottaDB/GT.M sets $TEST to 1 (TRUE). If LOCK fails to acquire the named resource before the timeout elapses, YottaDB/GT.M sets $TEST to 0 (FALSE).

------------------
Merge
------------------

The MERGE command copies a variable and all its descendants into another variable. MERGE does not delete the destination variable, nor any of its descendants.

The format of MERGE command is:

.. parsed-literal::
   M[ERGE][:tvexpr] glvn1=glvn2[,...]

* The optional truth-valued expression immediately following the command is a command post conditional that controls whether or not YottaDB/GT.M executes the command.
* When both glvn1 and glvn2 are local variables, the naked indicator does not change.
* If glvn2 is a global variable and glvn1 is a local variable, the naked indicator references glvn2.
* When both are global variables, the state of the naked indicator is unchanged if glvn2 is undefined ($DATA(glvn2)=0).
* In all other cases including $DATA(glvn2)=10, the naked indicator takes the same value that it would have if the SET command replaced the MERGE command and glvn2 had a value.
* If glvn1 is a descendant of glvn2, or if glvn2 is a descendant of glvn1; YottaDB/GT.M generates an error.
* If $data(glvn2) is 0 then the command is a NOOP and YottaDB/GT.M issues no errors.
* An indirection operator and an expression atom evaluating to a list of one or more MERGE arguments form a legal argument for a MERGE.

.. note::
   YottaDB/GT.M may permit certain syntax or actions that are described by the standard as in error. For example, a MERGE command that specifies an operation where the source and destination overlap but $DATA(source)=0 does not produce an error (which is equivalent to a no-operation).

MERGE simplifies the copying of a sub-tree of a local or global variable to another local or global variable. A sub-tree is all global or local variables that are descendants of a specified variable. MERGE offers a one-command alternative to the technique of using a series of SET commands with $ORDER() or $QUERY() references for doing sub-tree copy.

+++++++++++++++++++++++
Examples of MERGE
+++++++++++++++++++++++

Example:

.. parsed-literal::
   GTM>Set ^gbl1="one"
                   
   GTM>Set ^gbl1(1,1)="oneone"
   GTM>Set ^gbl1(1,1,3)="oneonethree"
   GTM>Set ^gbl1(1,2,4)="onetwofour"
   GTM>Set ^gbl2(2)="gbl2_2"
   GTM>Set ^gbl2(2,1,3)="gbl2_2_1_3"
   GTM>Set ^gbl2(2,1,4,5)="gbl2_2_1_4_5"
   GTM>Merge ^gbl1(1)=^gbl2(2)
   GTM>WRITE $Reference
   ^gbl1(1)
   GTM>ZWRite ^gbl1
   ^gbl1="one"
   ^gbl1(1)="gbl2_2"
   ^gbl1(1,1)="oneone"
   ^gbl1(1,1,3)="gbl2_2_1_3"
   ^gbl1(1,1,4,5)="gbl2_2_1_4_5"
   ^gbl1(1,2,4)="onetwofour"
   GTM>ZWRITE ^gbl2
   ^gbl2(2)="gbl2_2"
   ^gbl2(2,1,3)="gbl2_2_1_3"
   ^gbl2(2,1,4,5)="gbl2_2_1_4_5"
   GTM>

This example illustrates how MERGE copies a sub-tree of one global into another. The nodes in the sub-tree of ^gbl(2), for which $DATA() value is 1 or 11, are copied to sub-tree of ^gbl1(1) as follows:

.. parsed-literal::
   ^gbl1(1) is updated from the value of ^gbl2(2)
   ^gbl1(1,1,3) is updated from the value of ^gbl2(2,1,3)
   ^gbl1(1,1,4,5) is updated from the value of ^gbl2(2,1,4,5)

Since ^gbl1(2,1) and ^gbl2(2,2,4) do not have values ($DATA()=0), the corresponding nodes ^gbl1(1,1) and ^gbl(1,2,4) respectively are left unchanged. The naked indicator takes the value ^gbl(1) as if SET replaced MERGE. Notice that the MERGE command does not change ^gbl2(2) or its descendants. Ancestor nodes of ^gbl(1) are also left unchanged.

Example:

.. parsed-literal::
   GTM>Kill
                   
   GTM>Set ^gbl(1,2)="1,2"
   GTM>Merge lcl(3,4)=^gbl(1)
   GTM>Set ^("naked")=2
   GTM>ZWRite ^gbl
   ^gbl(1,2)="1,2"
   ^gbl("naked")=2
   GTM>ZWRite lcl
   lcl(3,4,2)="1,2"
   GTM>

This example illustrates how MERGE creates a sub-tree of a variable when the variable does not exist. Also, notice how the naked indicator is set when the source of the MERGE is a global and the destination a local.

-------------------
New
-------------------

The NEW command "stacks" copies of local variables and reinitializes those variables. An explicit or implicit QUIT from a DO, XECUTE or extrinsic function "unstacks" the NEWed variables, that is, restores the variable to the stacked value. A NEW lasts only while the current scope of execution is active.

The format of the NEW command is:

.. parsed-literal::
   N[EW][:tvexpr] [[(]lvn[,...][)][,...]]

* The optional truth-valued expression immediately following the command is a command postconditional that controls whether or not YottaDB/GT.M executes the command.
* NEW arguments are unsubscripted local variable names; NEW affects not only the variable specified in the argument, but also all variables descended from that variable.
* When an undefined variable is NEWed, the fact that it is undefined is "stacked", and when leaving the current scope, it returns to being undefined, that is, the variable is implicitly KILLed during transfer of control.
* Without an argument YottaDB/GT.M NEWs all currently existing local variables; in this case, at least two (2) spaces must follow the NEW to separate it from the next command on the line.
* For the scope of the NEW, a NEW of a name suspends its alias association. The association is restored when the scope of the New ends. The array remains in existence - it can be modified through other alias variables with which it is associated and which remain in scope. If none of its alias variables is in scope, the array remains intact and again becomes visible when the scope is restored.
* When a NEW argument is enclosed in parentheses, that NEW is considered "exclusive". An exclusive NEW creates a fresh data environment and effectively aliases the excluded variables with their original copies. This technique tends to improve performance and meets the M standard. However, it has two implications: The alias operation KILL \*, with no arguments, or naming an exclusively NEW'd variable, acts as a KILL in the current scope (has the same effect as a non-alias KILL), and ZWRITE, ZSHOW "V", $ZDATA() report any exclusively NEW'd variable as an alias. Refer to the section on the KILL command for a description of alternative behaviors for the interaction of KILL and exclusive NEW. For a comprehensive discussion on alias variables, refer to “Alias Variables Extensions”.
* When the flow of execution terminates the scope of an argumentless or an exclusive NEW, YottaDB/GT.M restores all stacked variables to their previous values, and deletes all other local variables.
* The intrinsic special variables $ESTACK, $ETRAP, $ZGBLDIR, and $ZYERROR can be an explicit argument of a NEW.For more information, refer to Chapter 8: “Intrinsic Special Variables”.
* The intrinsic special variable $ZTRAP can also be an explicit argument of a NEW; this stacks the current value of $ZTRAP and assigns $ZTRAP a null value ($ZTRAP="").
* An indirection operator and an expression atom evaluating to a list of one or more NEW arguments form a legal argument for a NEW.

The NEW command provides a means of confining the scope of local variables. NEW operates only on unsubscripted local names and acts on the entire named array.

+++++++++++++++++++
Examples of NEW
+++++++++++++++++++

Example:

.. parsed-literal::
   NEW1;
     Set A(1)=1,B=4,C=5
     Write !,"VARIABLES BEFORE NEW:",!
     ZWRite
     Do LABEL
     Write !,"VARIABLES AFTER RETURN:",!
     ZWRite
     Quit
  LABEL    
     New A Set C=7
     Write !,"VARIABLES AFTER NEW:",!
     ZWRite
     Quit

Produces the results:

.. parsed-literal::
   VARIABLES BEFORE NEW:
   A(1)=1
   B=4
   C=5
   VARIABLES AFTER NEW:
   B=4
   C=7
   VARIABLES AFTER RETURN:
   A(1)=1
   B=4
   C=7

Example:

.. parsed-literal::
   NEW2;
     Set (A,B,C,D)="TEST"
     Do LABEL
     Write !,"VARIABLES AFTER RETURN:",!
     ZWRite
     Quit
   LABEL
     New (B,C) SET (A,B,Z)="NEW"
     Write !,"VARIABLES AFTER EXCLUSIVE NEW:",!
     ZWRite
     Quit

Produces the results:

.. parsed-literal::
   VARIABLES AFTER EXCLUSIVE NEW:
   A="NEW"
   B="NEW"
   C="TEST"
   Z="NEW"
   VARIABLES AFTER RETURN:
   A="TEST"
   B="NEW"
   C="TEST"
   D="TEST"

Example:

.. parsed-literal::
   /usr/lib/fis-gtm/V5.4-002B_x86/gtm -run ^stackalias   
   stackalias ; Demonstrate New with alias
     ZPrint ; Print this program
     Set A=1,*B=A,*C(2)=A ; Create some aliases
     Write "------------",!
     Write "ZWRite in the caller before subprogram",!
     ZWRite
     Do S1 ; Call a subprogram
     Write "------------",!
     Write "ZWRite in the caller after subprogram - A association is restored",!
     ZWRite
     Quit
     ;
   S1  ; Subprogram
     New A
     Set A="I am not an alias",B="I am an alias"
     Write "------------",!
     Write "ZWRite in the subprogram with new A and modified B",!
     ZWRite
     Quit
   ------------
   ZWRite in the caller before subprogram
   A=1 ;*
   \*B=A
   C=3
   \*C(2)=A
   D=4
   ------------
   ZWRite in the subprogram with new A and modified B
   A="I am not an alias"
   B="I am an alias" ;*
   C=3
   \*C(2)=B
   D=4
   ------------
   ZWRite in the caller after subprogram - A association is restored
   A="I am an alias" ;*
   \*B=A
   C=3
   \*C(2)=A
   D=4

The following is essentially the same as the prior example but using an exclusive NEW:

.. parsed-literal::
   $ /usr/lib/fis-gtm/V5.4-002B_x86/gtm -run ^stackalias1
   stackalias1 ; Demonstrate New with alias
     ZPrint ; Print this program
     Set A=1,*B=A,*C(2)=A ; Create some aliases
     Write "------------",!
     Write "ZWRite in the caller before subprogram",!
     ZWRite
     Do S1 ; Call a subprogram
     Write "------------",!
     Write "ZWRite in the caller after subprogram - A association is restored",!
     ZWRite
     Quit
     ;
   S1  ; Subprogram
     New (B)
     Set A="I am not an alias",B="I am an alias"
     Write "------------",!
     Write "ZWRite in the subprogram - Notice B is flagged as an alias",!
     ZWRite
     Quit
   ------------
   ZWRite in the caller before subprogram
   A=1 ;*
   \*B=A
   C=3
   \*C(2)=A
   D=4
   ------------
   ZWRite in the subprogram - Notice B is flagged as an alias
   A="I am not an alias"
   B="I am an alias" ;*
   -----------
   ZWRite in the caller after subprogram - A association is restored
   A="I am an alias" ;*
   \*B=A
   C=3
   \*C(2)=A
   D=4

An exclusive New can create a scope in which only one association between a name or an lvn and an array may be visible. In this case, ZWRITE nevertheless shows the existence of an alias, even when that array is accessible from only one name or lvn.

--------------------
Open
--------------------

The OPEN command creates a connection between a YottaDB/GT.M process and a device.

The format of the OPEN command is:

.. parsed-literal::
   O[PEN][:tvexpr] expr[:[(keyword[=expr][:...])] [:numexpr]][,...]

* The optional truth-valued expression immediately following the command is a command postconditional that controls whether or not YottaDB/GT.M executes the command.
* The required expression specifies the device to OPEN.
* The optional keywords specify deviceparameters that control device behavior; some deviceparameters take arguments delimited by an equal sign (=); if the argument only contains one deviceparameter, the surrounding parentheses are optional.
* The optional numeric expression specifies a time in seconds after which the command should timeout if unsuccessful; choosing 0 results in a single attempt to open the device.
* When an OPEN command specifying a timeout contains no deviceparameters, double colons (::) separate the timeout numeric expression from the device expression.
* An indirection operator and an expression atom evaluating to a list of one or more OPEN arguments form a legal argument for an OPEN.
* In UTF-8 mode, the OPEN command recognizes the ICHSET, OCHSET, and CHSET deviceparameters to determine the encoding of the the input / output devices.
* OPEN on a directory produces a GTMEISDIR error in both READONLY and NOREADONLY modes along with the directory name which failed to open. UNIX directories contain metadata that is only available to the file system. Note that you can use the ZSEARCH() function to identify files in a directory, and you can call the POSIX stat() function to access metadata. The optional YottaDB/GT.M POSIX plug-in packages the stat() function for easy access from M application code.

---------------------
Quit
---------------------

Except when a QUIT appears on a line after a FOR, the QUIT command terminates execution of the current YottaDB/GT.M invocation stack level initiated by a DO, XECUTE, extrinsic function or special variable, and return control to the next "lower" level. In this case, QUIT restores any values stacked at the current level by NEWs or by parameter passing. A QUIT command terminates any closest FOR command on the same line. Note that M overloads the QUIT command to terminate DO, FOR, XECUTE and extrinsics ($$) of which FOR is the most different.

The format of the QUIT command is:

.. parsed-literal::
   Q[UIT][:tvexpr] [expr | \*lname | \*lvn]


* The optional truth-valued expression immediately following the command is a command postconditional that controls whether or not YottaDB/GT.M executes the command.
* When a QUIT terminates an extrinsic function, it must have an argument that supplies the value returned by the function; in all other cases, QUIT must not have an argument and must be followed by at least two (2) spaces to separate it from the next command on the line.
* An indirection operator and an expression atom evaluating to a QUIT argument form a legal argument for a QUIT.
* An unsubscripted lvn (lname) specifies the root of an array, while a subscripted lvn must specify an alias container.
* When QUIT * terminates an extrinsic function or an extrinsic special variable, it always returns an alias container. If lvn is an lname that is not an alias, QUIT * creates an alias container. For more information and examples of alias variables, refer to “Alias Variables Extensions”.
* The QUIT performs two similar, but different, functions depending on its context. Because FORs do not add levels to the YottaDB/GT.M invocation stack, QUITs inside FOR loops simply terminate the loop. QUITs that terminate DOs, XECUTEs and extrinsics remove a YottaDB/GT.M invocation stack level and therefore may adjust the local variable environment resulting from previous NEWs or parameter passing. A QUIT from an extrinsic or a frame created by an argumentless DO restores $TEST to its stacked value.
* An indirection operator and an expression atom evaluating QUIT arguments forms a legal argument for a QUIT other than from a FOR.
* Attempting to QUIT (implicitly or explicitly) from code invoked by a DO, XECUTE or extrinsic after that code issued a TSTART not yet matched by a TCOMMIT, produces an error.

++++++++++++++++++++++
Examples of Quit
++++++++++++++++++++++

Example:

.. parsed-literal::
        Do A
        Quit
   A    Write !,"This is label A"

The explicit QUIT at the line preceding the label A prevents line A from executing twice. The sub-routine at line A terminates with the implicit QUIT at the end of the routine.

Example:

.. parsed-literal::
   Write $$ESV
    Quit
 ESV()
    QUIT "value of this Extrinsic Special Variable"

Because the label ESV has an argument list (which is empty), YottaDB/GT.M can only legally reach that label with an extrinsic invocation. The QUIT on the second line prevents execution from erroneously "falling through" to the line labeled ESV. Because ESV identifies a subroutine that implements an extrinsic special variable, the QUIT on the line after ESV has an argument to provide the value of the extrinsic.

Example:

.. parsed-literal::
   Set x="" For  Set x=$Order(^BAL(x)) Quit:x]]"AR5999"!'$Length(x)  DO STF

The postconditional QUIT terminates the FOR loop. Note the two spaces after the QUIT because it has no argument.

----------------------
Read
----------------------

The READ command transfers the input from the current device to a global or local variable specified as a READ argument. For convenience, READ also accepts arguments that perform limited output to the current device.

The format of the READ command is:

.. parsed-literal::
   R[EAD][:tvexpr] (glvn|*glvn|glvn\#intexpr)[:numexpr]|strlit|fcc[,...]

* The optional truth-valued expression immediately following the command is a command postconditional that controls whether or not YottaDB/GT.M executes the command.
* A subscripted or unsubscripted global or local variable name specifies a variable into which to store the input; the variable does not have to exist prior to the READ; if the variable does exist prior to the READ, the READ replaces its old value.
* When an asterisk (*) immediately precedes the variable name, READ accepts one character of input and places the ASCII code for that character into the variable.
* When a number-sign (#) and a positive non-zero integer expression immediately follow the variable name, the integer expression determines the maximum number of characters accepted as input to the read; such reads terminate when YottaDB/GT.M reads the number of characters specified by the integer expression or a terminator character in the input stream or the optional timeout expires, whichever occurs first.
* The optional numeric expression specifies a time in seconds at most, for which the command waits for input to be terminated. When a timeout is specified, if the input has been terminated before the timeout expires, $TEST is set to 1 (true), otherwise, $TEST is set to 0 (false). When a READ times out, the target variable takes the value of the string received before the timeout.
* To provide a concise means of issuing prompts, YottaDB/GT.M sends string literal and format control character (!,?intexpr,#) arguments of a READ to the current device as if they were arguments of a WRITE.
* An indirection operator and an expression atom evaluating to a list of one or more READ arguments form a legal argument for a READ.
* In UTF-8 mode, the READ command uses the character set value specified on the device OPEN as the character encoding of the input device. If character set "M" or "UTF-8" is specified, the data is read with no transformation. If character set is "UTF-16", "UTF-16LE", or "UTF-16BE", the data is read with the specified encoding and transformed to UTF-8. If the READ command encounters an illegal character or a character outside the selected representation, it generates a run-time error. The READ command recognizes all Unicode line terminators for non-FIXED devices.

For more information on READ, devices, input, output and format control characters, refer to Chapter 9: “Input/Output Processing”.

--------------------
Set
--------------------

SET assigns values to variables or to a selected portion of a variable.

The format of the SET command is:

.. parsed-literal::
   S[ET][:tvexpr] setleft=expr | (setleft[,...])=expr | \*lvn=lname | aliascontainer[,...]

where

.. parsed-literal::
   setleft == glvn | $EXTRACT(glvn,[,intexpr1[,intexpr2]]) | $PIECE(glvn,expr1[,intexpr1[,intexpr2]]) | isv

and

.. parsed-literal::
   aliascontainer == lvn | exfunc | exvar

* The optional truth-valued expression immediately following the command is a command postconditional that controls whether or not YottaDB/GT.M executes the command.
* A subscripted or unsubscripted local or global variable name on the left of the equal-sign (=) specifies a variable in which to store the expression found on the right side of the equal-sign; the variable need not exist prior to the SET; if the variable exists prior to the SET, the SET replaces its old value.
* During a SET, YottaDB/GT.M evaluates the right side of the equal sign before the left; this is an exception to the left-to-right order of evaluation in YottaDB/GT.M and means that YottaDB/GT.M maintains the naked indicator using the expression on the right-hand side of the equal sign (=) before setting the variable.
* When the portion of the argument to the left of the equal sign is in the form of a $PIECE function, SET replaces the specified piece or pieces of the variable (specified as the first argument to the $PIECE() form) with the value of the expression on the right side of the equal-sign; if the variable did not exist prior to the SET or does not currently contain the pieces identified by the optional third and fourth arguments to the $PIECE() form, SET adds sufficient leading delimiters, as specified by the second argument to the $PIECE form, to make the assignment fit the $PIECE() form. Note that if the fourth argument exceeds the third argument, SET does not modify the target glvn or change the naked indicator.
* When the portion of the argument to the left of the equal sign is in the form of a $EXTRACT function, SET replaces the specified character or characters of the variable (specified as the first argument to the $EXTRACT() form) with the value of the expression on the right side of the equal-sign; if the variable did not exist prior to the SET or does not contain the characters identified by the optional second and third arguments to the $EXTRACT() form, SET adds sufficient leading spaces to make the assignment fit the $EXTRACT() form. Note that if the third argument exceeds the second argument, SET does not modify the target glvn or change the naked indicator .
* isv on the left-hand side of the equal-sign specifies an Intrinsic Special Variable. Not all ISVs permit SET updates by the application - see the description of the individual ISV.
* When the portion of the argument to the left of the equal-sign is in the form of a list of setlefts enclosed in parentheses, SET assigns the value of the expression on the right of the equal sign to all the destinations.
* If a SET updates a global node matching a trigger definition, YottaDB/GT.M executes the trigger code after the node has been updated in the process address space, but before it is applied to the database. When the trigger execution completes, the trigger logic commits the value of a node from the process address space only if $ZTVALUE is not set. if $ZTVALUE is set during trigger execution, the trigger logic commits the value of a node from the value of $ZTVALUE. For more information on using SET in Triggers, refer to “Set” section in the Triggers chapter.
* A SET * command explicitly makes the lvn on the left-hand side of the equal-sign an alias if it is an unsubscripted lvn (the root of an array) or an alias container if it is a subscripted lvn. If the portion of the argument on the right-hand side of the equal-sign is other than an lname (the root of an array), it must evaluate to an alias or alias container. Extrinsic functions and extrinsic special variables return an alias container if they terminate with a QUIT \*. For more information on Alias Variables, refer to “Alias Variables Extensions”.
* In a SET * command, any previous array associated with the lvn on the left-hand side of the equal-sign ceases to be associated with it, and if lvn was the only lvn associated with that (old) array in any scope, YottaDB/GT.M may reclaim the space it occupied. Alias assignment does not require that any data set exist for a name on the right-hand side of the equal-sign - the assignment simply creates an association.
* SET * left-hand side arguments cannot be parenthetically enclosed lists such as SET (a,*b)=c or SET (\*a,\*b)=c.
* SET and SET * assignments can be combined into one command in a comma separated list, for example, SET \*a=b,^c(3)=d(4).
* SET * only accepts argument indirection, that is, while SET accepts x="\*a=b",@x, SET does not permit x="\*a",@x=b or SET x="b",*a=@x.
* An indirection operator and an expression atom evaluating to a list of one or more SET arguments form a legal argument for a SET.
* A SET with proper syntax always succeeds regardless of the prior state or value of the variable, as long as YottaDB/GT.M can evaluate the expression to the right of the equal sign (=).

For the syntax of $PIECE() or $EXTRACT(), refer to Chapter 7: “Functions”.

+++++++++++++++++++++++++
Examples of SET
+++++++++++++++++++++++++

Example:

.. parsed-literal::
   GTM>Kill  Set a="x",(b,c)=1,@a="hello" ZWRite
   a=x
   b=1
   c=1
   x="hello"
   GTM>

The KILL command deletes any previously defined local variables. The SET command has three arguments. The first shows a simple direct assignment. The second shows the form that assigns the same value to multiple variables. The third shows atomic indirection on the left of the equal sign. The ZWRITE command displays the results of the assignments.

Example:

.. parsed-literal::
   GTM>Set ^(3,4)=^X(1,2)

As YottaDB/GT.M evaluates the right-hand side of the equal sign before the left-hand side within a SET argument, the right-hand expression determines the naked reference indicator prior to evaluation of the left-hand side. Therefore, this example assigns ^X(1,3,4) the value of ^X(1,2).

Example:

.. parsed-literal::
   GTM>Kill x Set $Piece(x,"^",2)="piece 3" ZWRite x
   x="^^piece 3"
   GTM>

This SET demonstrates a "set piece" and shows how SET generates missing delimiters when required. For more information on $PIECE(), refer to Chapter 7: “Functions”.

Example:

.. parsed-literal::
   GTM>Set x="I love hotdogs"
                   
   GTM>Set $Extract(x,3,6)="want"
   GTM>Write x
   I want hotdogs
   GTM>Set $Extract(x,7)=" many "
   GTM>Write x
   I want many hotdogs
   GTM>

The SET $EXTRACT command replaces and extracts the specified characters with the value of the expression on the right hand side of the equal-sign (=). For more information on $EXTRACT(), refer to Chapter 7: “Functions”.

Example:

.. parsed-literal::
   GTM>kill A,B
           
   GTM>set A=1,A(1)=1,A(2)=2
   GTM>set \*B=A ; A & B are aliases. 
   GTM>zwrite B
   B=1 ;*
   B(1)=1
   B(2)=2
   GTM>

This SET * command creates an alias associated between A and B. It associates the entire tree of nodes of A including its root and all descendants with B.

Example:

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

This SET * command creates an alias by dereferencing an alias container. 

----------------------
TCommit
----------------------

The TCOMMIT command marks the end of a transaction or sub-transaction and decrements $TLEVEL. If TCOMMIT marks the end of a transaction (decrements $TLEVEL to zero), it invokes a COMMIT, which makes the database updates performed by the transaction generally available. A TCOMMIT issued when no transaction is in progress ($TLEVEL=0) produces an error.

The format of the TCOMMIT command is:

.. parsed-literal::
   TC[OMMIT][:tvexpr]

* The optional truth-valued expression immediately following the command is a command postconditional that controls whether or not YottaDB/GT.M executes the command.
* Because TCOMMIT has no argument, at least two (2) spaces must follow the command to separate it from the next command on the line.

For an example of the use of the TCOMMIT command, see Chapter 5: “General Language Features of M”.

-------------------
TREstart
-------------------

The TRESTART command attempts to RESTART the current transaction. A RESTART transfers control back to the initial TSTART and restores much of the process state to what it was when that TSTART was originally executed. A TRESTART issued when no transaction is in progress ($TLEVEL=0) or when the transaction does not have RESTART enabled produces an error.

A TRESTART command causes the TP transaction to RESTART in the same way that YottaDB/GT.M uses to implicitly restart the transaction in case of resource conflicts. All restarts increment the internal transaction retry count to a maximum of three (3), at which point, YottaDB/GT.M performs the entire TP transaction within a critical section on all databases referenced in the transaction.

YottaDB/GT.M issues a TRESTMAX runtime error when application code attempts a TRESTART more than once during a transaction while $TRESTART=4 (note: in order to be wholesome, TRESTART usage in application code should always be conditional). In the final retry, YottaDB/GT.M holds the critical section lock on all databases involved in the transaction. Since a TRESTART cancels all the work done in the current transaction and transfers control back to the TSTART, limiting the number of times this can be done in the final retry limits the time a process can (by virtue of holding a critical section lock on the databases) prevent other processes from updating the database.

YottaDB/GT.M limits TP restarts in the final retry due to non-availability of M-locks in a similar fashion. YottaDB/GT.M allows a maximum of 16 such restarts after which it issues a TPLOCKRESTMAX runtime error.

The format for the TRESTART command is:

.. parsed-literal::
   TRE[START][:tvexpr]

The optional truth-valued expression immediately following the command is a command postconditional that controls whether or not YottaDB/GT.M executes the command.

Because TRESTART has no argument, at least two (2) spaces must follow the command to separate it from the next command on the line.

TRESTARTs (and implicit RESTARTs) do not restore any device states; they do restore the following to the state they had when YottaDB/GT.M executed the initial TSTART:

* $TEST
* All global variables modified by the current base transaction and any of its sub-transactions
* The naked indicator
* LOCKs held by the process

A TP RESTART, either implicit or explicit, while executing $ZINTERRUPT in response to an interrupt (that is, $ZININTERRUPT is 1), and while error processing is in effect (that is, $ECODE'=""), raises a TPRESTNESTERR error and engages nested error handling, which unstacks M virtual machine frames back to where the incompletely handled error occurred, unstacks that frame and rethrows the error.

They also restore any local variables named by one or more active TSTARTs to the values they had when they were first named.

For an example of the use of the TRESTART command, see Chapter 5: “General Language Features of M”.

----------------------
TROllback
----------------------

The TROLLBACK command terminates a transaction by causing a ROLLBACK, which removes all database updates performed within a transaction. A TROLLBACK without an argument also sets $TLEVEL and $TRESTART to zero (0). Issuing a TROLLBACK when no transaction is in progress ($TLEVEL=0) produces an error.

The format of the TROLLBACK command is:

.. parsed-literal::
   TRO[LLBACK][:tvexpr] [intexpr]

* The optional truth-valued expression immediately following the command is a command postconditional that controls whether or not YottaDB/GT.M executes the command.
* The optional integer expression indicates an argument specifying incremental rollback. If the value of the argument expression is greater than zero, it specifies the value of $TLEVEL to be achieved by the rollback. If the value of the expression is less than zero, the result is the number of levels to rollback. For example; -1 means rollback one level. If the argument expression is zero, the effect is same as not specifying the argument, that is, the entire YottaDB/GT.M transaction is rolled back.
* Attempting to rollback more than $TLEVEL levels (the outermost transaction) generates an error.
* When the TROLLBACK has no argument, at least two (2) spaces must follow the command to separate it from the next command on the line.
* In order to allow for error recovery and/or access to the global context of the error, errors do not initiate implicit ROLLBACKs. Therefore, the code for handling errors during transactions should generally include a TROLLBACK. Because the TROLLBACK releases resources held by the transaction, it should appear as early as possible in the error handling code.
* A TROLLBACK does not cause a transfer of control but is typically associated with one such as a QUIT (or GOTO).
* TROLLBACK to a $TLEVEL other than zero (0) leaves $REFERENCE empty. This behavior is same as a full TROLLBACK to $TEVEL=0.

For an example of the use of the TROLLBACK command, see Chapter 5: “General Language Features of M”.

------------------
TStart
------------------

The TSTART command marks the beginning of a transaction or sub-transaction and increments $TLEVEL. When TSTART marks the beginning of a transaction ($TLEVEL=1), its arguments determine whether the transaction may RESTART and whether serializability is enforced. If a transaction may RESTART, the TSTART arguments determine which local variables are restored during a RESTART. Serializability is enforced by LOCK commands or, if the SERIAL keyword is specified, by YottaDB/GT.M.

The format of the TSTART command is:

.. parsed-literal::
   TS[TART][:tvexpr] [([lvn...])\|lvn|*\|][:keyword|(keyword...)]

* The optional truth-valued expression immediately following the command is a command postconditional that controls whether or not YottaDB/GT.M executes the command.
* If $TLEVEL is 0 before the TSTART, the TSTART starts a transaction; otherwise it starts a sub-transaction.
* If the TSTART initiates a transaction and the portion of the argument before the colon (:) delimiter is empty, the transaction is not eligible for RESTART. If the TSTART starts a transaction ($TLEVEL=0) and the portion of the argument before the colon is not empty, the transaction is eligible for RESTART. If the TSTART is nested (starts a sub-transaction), its arguments have no effect on whether the transaction is eligible for RESTART.
* If the portion of the argument before the colon is an asterisk (*), any subsequent RESTART restores all local variables to the value they had when the TSTART was executed.
* If the portion of the argument before the colon is an unsubscripted local variable name or a list of such names enclosed in parentheses, a RESTART restores the named variables to the value they had when the TSTART was executed.
* If the portion of the argument before the colon is a set of empty parentheses (), a RESTART does not restore any local variables.
* The optional portion of the argument after the colon is a keyword or a colon-separated list of keywords enclosed in parentheses, where the keywords specify transaction characteristics.
* An indirection operator and an expression atom evaluating to a TSTART argument form a legal argument for a TSTART.
* Using TSTART in direct mode may not behave as expected because there is no code repository to support an appropriate transaction restart.

A TSTART within a transaction starts a sub-transaction. The argument to such a TSTART has no effect on whether the existing transaction may RESTART or whether serializability of the transaction is enforced. This type of TSTART may add local variables to be restored in a transaction that has RESTART enabled.

It is good coding practice to synchronize enabling of RESTART on TSTARTs at all levels of a transaction. A nested TSTART that does not permit RESTART where the transaction does, may indicate that the sub-transaction has not been coded to properly handle RESTART.

Sub-transactions cannot COMMIT independently from the transaction, nor can they RESTART independently. Sub-transactions exist largely as a programming convenience to allow flexibility in organizing code in a modular fashion, and in addition to allow incremental ROLLBACKs.

When journaling, a transaction with an initial TSTART that has an argument specifying TRANSACTIONID=expr, where expr is an expression that evaluates to the keyword (case insensitive) BA[TCH], does not wait for the journal update to be written before returning control to the application after a successful TCOMMIT. The goal of this feature is to permit application control over any performance impact of journaling on any subset of transactions that can be recreated or recovered by means other than journaling.

For an example of the TSTART command, refer to Chapter 5: “General Language Features of M”.

The following keywords may appear in a TSTART argument:

+++++++++++++++++++++
S[ERIAL]
+++++++++++++++++++++

The SERIAL keyword indicates that YottaDB/GT.M must ensure the serializability of the transaction. Note that YottaDB/GT.M always serializes transactions regardless of the SERIAL keyword. On a nested TSTART, this portion of the argument is irrelevant.

+++++++++++++++++++++
T[RANSACTIONID]=expr
+++++++++++++++++++++

The TRANSACTIONID keyword declares an arbitrary transaction identification.

If TRANSACTIONID="BATCH" or "BA" at transaction completion, the process immediately continues execution. When a process issues a [final] TCOMMIT for a transaction and journaling is active, by default the process waits until the entire transaction is written to the journal file(s) before executing the next command. This ensures that every transaction is durable before the process moves on to the next step. Transactions flagged as "BATCH" have lower latency and higher throughput, but a lower guarantee of durability. Normally this flag is used when operational procedures (such as a backup) or application code (such as a checkpoint algorithm) provides an acceptable alternative means of ensuring durability.

--------------------
Use
--------------------

The USE command selects the current device for READs (input) and WRITEs (output).

The format of the USE command is:

.. parsed-literal::
   U[SE][:tvexpr] expr[:(keyword[=expr][:...])][,...]

* The optional truth-valued expression immediately following the command is a command postconditional that controls whether or not YottaDB/GT.M executes the command.
* The required expression specifies the device to make the current device.
* A USE that selects a device not currently OPENed by the process causes a run-time error.
* The optional keywords specify deviceparameters that control device behavior; some deviceparameters take arguments delimited by an equal sign (=); if the argument only contains one deviceparameter, the surrounding parentheses are optional.
* An indirection operator and an expression atom evaluating to a list of one or more USE arguments form a legal argument for a USE.

---------------------
View
---------------------

The VIEW command adjusts an environmental factor selected by a keyword argument. For example, VIEW controls journal buffer flushing, determines whether YottaDB/GT.M reports undefined variables as errors or treats them as null, and determines which BREAK commands should display messages.

The format of the VIEW command is:

.. parsed-literal::
   V[IEW][:tvexpr] keyword[:expr2[:...]][,...]

* The optional truth-valued expression immediately following the command is a command postconditional that controls whether or not YottaDB/GT.M executes the command.
* The keyword specifies the environmental factor to change.
* The optional expression following the keyword specifies the nature of the change to the environmental factor.
* An indirection operator and an expression atom evaluating to a list of one or more VIEW arguments form a legal argument for a VIEW

+++++++++++++++++++++++++
Key Words in VIEW Command
+++++++++++++++++++++++++

The following sections describe the keywords available for the VIEW command in YottaDB/GT.M.

**"BREAKMSG":value**

Sets the value of the BREAK message mask. When YottaDB/GT.M processes a BREAK command, the BREAK message mask controls whether to display a message describing the source of the BREAK.

The mask uses the following four values that are added together to provide the BREAKMSG value.

1 - BREAKs within the body of a program

2 - BREAKs within a ZBREAK action

4 - BREAKs within a device EXCEPTION

8 - BREAKs within a ZSTEP action

16 - ZBREAKs within a trigger removed due to updated trigger (TRIGZBREAKREM)

The default BREAKMSG mask is 31 (1+2+4+8+16) which means that YottaDB/GT.M displays all BREAK messages.

Example:

.. parsed-literal::
   GTM>VIEW "BREAKMSG":5

In this example the BREAKMSG value is 5, representing the sum of 1 and 4. This enables BREAKS within the body of a program (value 1) and for a device EXCEPTION (value 4).

**[NO]BADCHAR**

Enables or disable the gneration of an error when character-oriented functions encounter malformed byte sequences (illegal characters).

At process startup, YottaDB/GT.M initializes BADCHAR from the environment variable gtm_badchar. Set the environment variable $gtm_badchar to a non-zero number or "YES" (or "Y") to enable VIEW "BADCHAR". Set the environment variable $gtm_badchar to 0 or "NO" or "FALSE" (or "N" or "F") to enable VIEW "NOBADCHAR". By default, YottaDB/GT.M enables VIEW "BADCHAR".

With VIEW "BADCHAR", YottaDB/GT.M functions generate the BADCHAR error when they encounter malformed byte sequences. With this setting, YottaDB/GT.M detects and clearly reports potential application program logic errors as soon as they appear. As an illegal UTF-8 character in the argument of a character-oriented function likely indicates a logic issue, YottaDB/FIS recommends using VIEW "BADCHAR" in production environments.

.. parsed-literal::
   When all strings consist of well-formed characters, the value of VIEW [NO]BADCHAR has no effect whatsoever. With VIEW "NOBADCHAR", the same functions treat malformed byte sequences as valid characters. During the migration of an application to add support for Unicode, illegal character errors are likely to be frequent and indicative of application code that is yet to be modified. VIEW "NOBADCHAR" suppresses these errors at times when their presence impedes development.

**"DBFLUSH"[:REGION[:N]]**

When using the BG access method, writes modified blocks in the global buffers to the database file. By default, this command option operates on all regions under the current global directory. N specifies the number of blocks to write; by default, DBFLUSH writes all modified blocks. Normally YottaDB/GT.M schedules block flushing at appropriate times, but this option exists for an application to explore the impact of flushing on their work load. See also the DBSYNC and EPOCH VIEW Options.

**"DBSYNC"[:REGION]**

Performs a file system hardening sync - fsync() - operation on the database file. By default, this command option operates on all regions under the current global directory. Normally YottaDB/GT.M schedules block flushing at appropriate times, but this option exists for an application to explore the impact of file hardening on their work load. See also the DBFLUSH and EPOCH VIEW Options.

**[NO]DMTERM**

Provides a mechanism to retain default line terminators for direct mode user interaction (including the BREAK command) independent of any TERMINATOR deviceparameter changes for $PRINCIPAL. With VIEW "NODMTERM", TERMINATOR deviceparameter apply to both READs from $PRINCIPAL and direct mode interactions. A case-insensitive value of the environment variable gtm_dmterm is "1", "yes", or "true" establishes a DMTERM state at process initiation; all other values, including no value, result in the default VIEW "NODMTERM" behavior. $VIEW("DMTERM") returns 1 for DMTERM mode or 0 for NODMTERM mode. 

**"EPOCH"[:REGION]**

Flushes the database buffers and, if journaling is enabled, writes an EPOCH record. By default, this command option operates on all regions under the current global directory. Normally YottaDB/GT.M schedules epochs as a user controlled journaling characteristic, but this option exists for an application to explore the impact of epochs on their work load. See also the DBFLUSH and DBSYNC VIEW Options. Epochs include DBFLUSH and DBSYNC actions, but performing them before the epoch may reduce the duration of these actions within the epoch.

**"FLUSH"[:REGION]**

Flushes dirty global buffers from the global buffer pool. If journaling is turned on, "FLUSH" writes an EPOCH record and flushes dirty journal buffers prior to flushing dirty global buffers. If no region is specified, VIEW "FLUSH" flushes all regions in the current global directory that the YottaDB/GT.M process has opened.

**[NO]FULL_BOOL[EAN][WARN]**

Controls the evaluation of Boolean expressions (expressions evaluated as a logical TRUE or FALSE).

By default, YottaDB/GT.M enables VIEW "NOFULL_BOOLEAN" which means that YottaDB/GT.M stops evaluating a Boolean expression as soon as it establishes a definitive result. For example, neither 0& $ $ abc^def() nor 1! $ $ abc^def() executes $$abc^def(). However, in the case of global references, such as 0&^a or 1!^a, YottaDB/GT.M sets $reference and the naked indicator without actually accessing the global variable.

With VIEW "FULL_BOOLEAN", YottaDB/GT.M ensures that all side effect expression atoms, extrinsic functions ($$), external functions ($&), and $INCREMENT() execute in left-to-right order.

With VIEW "FULL_BOOLWARN", YottaDB/GT.M not only evaluates Boolean expressions like "FULL_BOOLEAN" but produces a BOOLSIDEFFECT warning when it encounters Boolean expressions that may induce side-effects; that is: expressions with side effects after the first Boolean operator - extrinsic functions, external calls and $INCREMENT().

YottaDB/GT.M picks up the value of [NO]FULL_BOOL[EAN][WARN] from the environment variable gtm_boolean. If gtm_boolean is undefined or evaluates to an integer zero (0), the initial setting the default "NOFULL_BOOLEAN", if it evaluates to an integer one (1), the initial setting is "FULL_BOOLEAN" and if it evaluates to integer two (2) the initial setting is "FULL_BOOLWARN".

VIEW "[NO]FULL_BOOL[EAN][WARN]" takes effect immediately for indirection and XECUTE.

VIEW "NOFULLBOOLEAN" produces an error when gtm_side_effects is on. For more information on the gtm_side_effects environment variable, refer to the Environment Variables section in the Basic Operations chapter of the Administration and Operations Guide.

**"GDSCERT":value**

Enables (value=1) or disables (value=0) database block certification.

Database block certification causes YottaDB/GT.M to check the internal integrity of every block as it writes the block. Block certification degrades performance and exists primarily as a tool for use by YottaDB/FIS. The default is GDSCERT:0.

**"GVSRESET":"<region>"**

Resets the process-specific fields that are part of the ZSHOW "G" result and database file header fields holding records reported by: GVSTAT, BG trace, buffer pool accounting and the TP block modification details. Note a VIEW "GVSRESET" performed by a process with read-only database access changes only the process-specific information and has no effect on the database file header. DSE CHANGE -FILEHEADER -GVSTATSRESET clears the same database file header fields as VIEW "GVRESET"; 

**"GVDUPSETNOOP":value**

Enables (VIEW "GVDUPSETNOOP":1) or disables (VIEW "GVDUPSETNOOP":0) duplication set optimization.

Duplicate set optimization prevents a SET that does not change the value of an existing node from performing the update or executing any trigger code specified for the node. By default, duplicate set optimization is enabled.

**"JNLFLUSH"[:region]**

Writes or flushes journaling buffers associated with the given region to permanent storage, for example, to disk. If the VIEW "JNLFLUSH" does not specify the optional region, YottaDB/GT.M flushes all journaled regions of the current Global Directory.

Normally YottaDB/GT.M writes journal buffers when it completes a transaction (unless TRANSACTIONID="BATCH"), fills the journal buffer or when some period of time passes with no journal activity.

**JNLWAIT**

Causes a process to pause until its journaling buffers have been written. JNLWAIT ensures that YottaDB/GT.M successfully transfers all database updates issued by the process to the journal file before the process continues. Normally, YottaDB/GT.M performs journal buffer writes synchronously for TP updates, and asynchronously, while the process continues execution, for non-TP updates or TP updates with TRANSACTIONID=BATCH.

JNLWAIT operates only on those regions for which the current process has opened journal files. As all the journal activity for a TP transaction occurs at commit time, YottaDB/GT.M ignores JNLWAIT when inside a TP TRANSACTION ($TLEVEL > 0). For more information on journaling, refer to the "YottaDB/GT.M Journaling" chapter in the Administration and Operations Guide.

**"JOBPID":"value"**

Enables (value=1) or disables (value=0) the addition of the child process ID to the output and error file names used (either implicitly generated or explicitly defined) by the JOB command. The default is 0.

Using the value=1 option prevents the JOB command from overwriting output files each time the same JOB command executes.

**"LABELS":"value"**

Enables (value="LOWER") or disables (value="UPPER") case sensitivity for labels within routines.

It is important to have the same case handling at compile-time and run-time.

Because YottaDB/GT.M stores routines as regular files and file names are case sensitive on UNIX, YottaDB/GT.M always treates routine names as case sensitive.

**"LINK":"[NO]RECURSIVE"**

Enables ("LINK":"RECURSIVE") or disables ("LINK":"RECURSIVE") the ZLINK command to accept and relink routines on the YottaDB/GT.M invocation stack. With VIEW "LINK":"RECURSIVE" specified, the ZLINK command adds an executable routine even when a routine with the same name is active and available in the current stack. When a process links a routine with the same name as an existing routine, future calls use the new routine. Prior versions of that routine referenced by the stack remain tied to the stack until they QUIT, at which point they become inaccessible. This provides a mechanism to patch long-running processes.

The default is VIEW "LINK":"NORECURSIVE".

**[NO]LOGN[ONTP][=intexpr]**

Allows a process to dynamically change the logging of NONTPRESTART messages to the operator log established at process startup by the environment variables gtm_nontprestart_log_delta and gtm_nontprestart_log_first.

VIEW "NOLOGNONTP" turns off the logging of NONTPRESTART messages to the operator log.

VIEW "LOGNONTP"[=intexpr] turns on logging of NONTPRESTART messages to the operator log. If no intexpr is specified, YottaDB/GT.M uses the value of environment variable gtm_nontprestart_log_delta, if it is defined, and one otherwise (that is, every transaction restart will be logged). A negative value of intexpr turns off the logging of NONTPRESTART messages.

Note that it is not possible to perform the operations of gtm_nontprestart_log_first with VIEW "LOGNONTP"[=intexpr].

**[NO]LOGT[PRESTART][=intexpr]**

Allows a process to dynamically change the logging of TPRESTART messages to the operator log established at process startup by the environment variables gtm_tprestart_log_delta and gtm_tprestart_log_first.

VIEW "NOLOGTPRESTART" turns off the logging of TPRESTART messages to the operator log.

VIEW "LOGTPRESTART"[=intexpr] turns on logging of TPRESTART messages to the operator log. If no intexpr is specified, YottaDB/GT.M uses the value of environment variable gtm_tprestart_log_delta, if it is defined, and one otherwise (that is, every transaction restart will be logged). A negative value of intexpr turns off the logging of TPRESTART messages.

Note that it is not possible to perform the operations of gtm_tprestart_log_first with VIEW "LOGTPRESTART"[=intexpr].

**LV_GCCOL**

Starts a data-space garbage collection, which normally happens automatically at appropriate times.

.. note::
   There are no visible effects from LV_GCOL, LV_REHASH, and STP_GCOL except for the passage of time depending on the state of your process. YottaDB/FIS uses these VIEW "LV_GCOL","LV_REHASH","STP_GCOL" facilities in testing. They are documented to ensure completeness in product documentation. You may (or may not) find them useful during application development for debugging or performance testing implementation alternatives.

**LV_REHASH**

Starts a reorganization of the local variable look-up table, which normally happens automatically at appropriate times.

.. note::
   There are no visible effects from LV_REHASH, LV_GCOL, and STP_GCOL except for the passage of time depending on the state of your process. YottaDB/FIS uses these VIEW "LV_GCOL","LV_REHASH","STP_GCOL" facilities in testing. They are documented to ensure completeness in product documentation. You may (or may not) find them useful during application development for debugging or performance testing implementation alternatives.

**[NEVER]|[NO]LVNULLSUBS**

Disallows, partially disallows, or allows local arrays to have empty string subscripts. The default is LVNULLSUBS.

NOLVNULLSUBS disallows any variant of SET to operate on a local array having an empty string subscript.

NEVERLVNULLSUBS disallows any variant of SET or KILL ($DATA(),$GET(),$ORDER(), and $QUERY()) to operate on a local array having an empty string subscript. An empty string as the last subscript in $ORDER() and $QUERY() has the semantic significance of requesting the next lexical item and is not subject to NULLSUBS errors.

LVNULLSUBS allows local arrays to have empty string subscripts.

At process startup, YottaDB/GT.M initializes [NEVER][NO]LVNULLSUBS from $gtm_lvnullsubs. Set the environment variable $gtm_lvnullsubsv to:

* 0 - equivalent to VIEW "NOLVNULLSUBS"
* 1 (the default) - equivalent to VIEW "LVNULLSUBS" or
* 2 - equivalent to VIEW "NEVERLVNULLSUBS".

.. note::
   Remember that for global variables, empty string subscript checking is controlled by a database region characteristic. YottaDB/FIS recommends using LVNULLSUBS, NOLVNULLSUBS, or NEVERLVNULLSUBS for local variables and NULLSUBS options ALWAYS or NEVER for global variables.

**"NOISOLATION":<expr>**

where expr must evaluate to one of the following forms:

* "", that is, the empty string : turn off the feature for all globals for which it has previously been turned on
* "^gvn1,^gvn2,..." : turn on the feature for the globals in the list, turning it off for globals for which it has previously been turned on
* "+^gvn1,^gvn2,..." : add these globals to the list of globals that have this feature turned on
* "-^gvn1,^gvn2,..." : turn off the feature for these globals leaving the status for other globals unchanged

YottaDB/GT.M transaction processing permits the application to specify a set of globals that do not require YottaDB/GT.M to preserve Isolation, one of the "ACID" properties of TP. This shifts the responsibility for Isolation from YottaDB/GT.M to the application logic, and permits YottaDB/GT.M to relax its TP Isolation rules. This avoids TP restarts in certain cases thus improving the performance of the application. For example, if a global variable includes $JOB as a subscript, the application may be written and scheduled in such a way that no more than one process uses a node of that global at any given time. Specifying such a global as "NOISOLATED" avoids transaction restarts that occur when different processes concurrently update and access nodes that share the same GDS block.

The rules for enforcement by YottaDB/GT.M of Isolation, and therefore potentially Consistency, are relaxed for application-specified global variables in order to allow the application to manage these properties. YottaDB/GT.M is responsible for Atomicity and Durability, as well as for database integrity for all variables, and for Isolation and Consistency for any global variables for which the application does not accept responsibility.

Note that if an application incorrectly specifies a global to be NOISOLATED, severe, and possibly intermittent and difficult to diagnose damage to application-level integrity is likely to result. A thorough understanding of the application is necessary before declaring a global to be noisolated. YottaDB/GT.M preserves database integrity (accessibility) for NOISOLATED, as well as ISOLATED global variables.

YottaDB/GT.M ignores attempts to turn on (or off) the feature for globals that already have the feature turned on (or off). It is an error to modify the isolation-status of a global variable within a transaction across different references (either reads or writes) of that global variable. The VIEW command by itself is not considered to be a reference of the global variable. While not recommended programming practice, this means that a process can change a global's isolation-status within a transaction as long as it hasn't referenced it yet.

Any reads on a NOISOLATION global are validated at the time of the read and not re-validated at TCOMMIT time. This means that if the value that was read changed after the read but before the TCOMMIT, the transaction would still be committed. Therefore it is important that any reads on a NOISOLATED global (if any) should be of data insensitive to change with time (unchanging or where consistency with other data accessed by the transaction doesn't matter). 

**"PATCODE":"tablename"**

Identifies the alternative table of unique patterns for use with the "?" operator to be loaded from the pattern definition file. For additional information, refer to Chapter 12: “Internationalization”.

**"PATLOAD":"file-specification"**

Identifies the file containing definitions of unique patterns for use with the "?" operator. These pattern definitions can be used in place of, or in addition to, the standard C, N, U, L, and P. For more information on creating the file-specification, refer to Chapter 12: “Internationalization”.

**"POOLLIMIT":<region>:expr**

VIEW "POOLLIMIT":<region>:expr, where expr is of the form n[%] provides a mechanism for a process that has the potential to "churn" global buffers to limit the potential impact on other processes by restricting the number of global buffers it uses. If the expression ends with a per-cent sign (%), the number is taken as an as a percentage of the configured global buffers and otherwise as an ordinal number of preferred buffers; standard M parsing and integer conversions apply. Preferred buffer values are limited to between 32 and one less than half the buffer pool inclusive; with the exception of zero (0) or 100 per cent, which turn off the limitation; specifications exceeding those limits provide the value of the nearer limit. If the argument specifies "*" for the region, the command applies to all regions. $VIEW("POOLLIMIT",<region>) returns the current value for the region as an ordinal number - zero (0) when there is no limit in place. Note that this facility is designed for use by a relatively small subset of processes. In addition, MUPIP REORG uses this facility to limit its buffers to a value established by the UNIX environment variable (or OpenVMS logical name) gtm_poollimit using the syntax described for VIEW "POOLLIMIT" with a default of 64 if gtm_poollimit is not specified. Note that this may slightly slow a standalone REORG but can be overridden by defining gtm_poollimit as 0 or "100%". 

**RCTLDUMP**

Displays the created relinkctl files and the routines looked for in their related directories. An entry in these files does not mean that a given routine was found there. It merely means it was looked for there and shows a cycle number (which ZRUPDATE bumps) whose change indicates a new published version of the given object file. As it is a diagnostic tool for the new feature, YottaDB/FIS may remove or modify this VIEW option in subsequent releases.

.. note::
   YottaDB/GT.M no longer supports VIEW "RCTLDUMP" as it has been supplanted by ZSHOW "A" and MUPIP RCTLDUMP.

**RESETGVSTATS**

Resets all the process-private global access statistics to 0. This is particularly useful for long running processes which would periodically like to restart the counting without requiring a shut down and restart.

**[NO]STATSHARE**

Opt in or out of sharing process statistics for monitoring by other processes.

YottaDB/GT.M provides a fast and efficient mechanism for processes to share their database access statistics for other processes to monitor. Processes opt in or out with the VIEW "[NO]STATSHARE" command, defaulting to VIEW "NOSTATSHARE". At process startup, a value of 1, or any case-independent string or leading substrings of "TRUE" or "YES" in the environment variable gtm_statshare provides an initial setting of VIEW "STATSHARE". When a process changes whether it is opting in or out, there is no change to the output of a ZSHOW "G" within that process. YottaDB/GT.M does not permit this form of the VIEW command within a TP transaction. Monitoring the statistics of other processes does not require opting-in.

Processes opted-in place their statistics as binary data in database files located in the directory specified by the gtm_statsdir environment variable. All processes that share statistics MUST use the same value for $gtm_statsdir. The ^%YGBLSTAT utility program gathers and reports statistics. 

**STP_GCOL**

Starts a string-pool garbage collection, which normally happens automatically at appropriate times. 

.. note::
   There are no visible effects from STP_GCOL, LV_GCOL and LV_REHASH except for the passage of time depending on the state of your process. YottaDB/FIS uses these VIEW "LV_GCOL","LV_REHASH","STP_GCOL" facilities in testing. They are documented to ensure completeness in product documentation. You may (or may not) find them useful during application development for debugging or performance testing implementation alternatives.

**[NO]UNDEF**

Enables or disables handling of undefined variables as errors. With UNDEF, YottaDB/GT.M handles all references to undefined local or global variables as errors. With NOUNDEF, YottaDB/GT.M handles all references to undefined local or global variables as if the variable had a value of the empty string. In other words, YottaDB/GT.M treats all variables appearing in expressions as if they were the argument of an implicit $GET(). UNDEF is the default.

The environment variable $gtm_noundef specifies the initial value value of [NO]UNDEF at process startup. If it is defined, and evaluates to a non-zero integer or any case-independent string or leading substring of "TRUE" or "YES", then YottaDB/GT.M treats undefined variables as having an implicit value of an empty string. 

.. note::
   NOUNDEF does not apply to an undefined FOR control variable. This prevents an increment (or decrement) of an undefined FOR control variable from getting into an unintended infinite loop. For example, FOR A=1:1:10 KILL A gives an UNDEF error on the increment from 1 to 2 even with VIEW "NOUNDEF". 

**"TRACE":value:<expr>**

Traces YottaDB/GT.M program execution and generates profiling information about the lines and functions executed; with low impact on the run-time performance.

The feature turns on (value=1) or turns off (value=0) M-profiling. This expression must evaluate to a string containing the name of a YottaDB/GT.M global variable. The global may also have subscripts; however the subscripts must be literals or the special variable $JOB. For the $JOB process identifier description, refer to Chapter 8: “Intrinsic Special Variables”.

The expression is optional when turning M-profiling off, if it exists, it overrides the global variable set when M-profiling was turned on.

gtm_trace_gbl_name enables YottaDB/GT.M tracing at process startup. Setting gtm_trace_gbl_name to a valid global variable name instructs YottaDB/GT.M to report the data in the specified global when a VIEW command disables the tracing, or implicitly at process termination. This setting behaves as if the process issued a VIEW "TRACE" command at process startup. However, gtm_trace_gbl_name has a capability not available with the VIEW command, such that if the environment variable is defined but evaluates to zero (0) or, only on UNIX, to the empty string, YottaDB/GT.M collects the M-profiling data in memory and discards it when the process terminates (this feature is mainly used for in-house testing). Note that having this feature activated for process that otherwise do not open a database file (such as GDE) can cause them to encounter an error.

In addition, if a process issues a malformed VIEW command that attempts to turn tracing off, YottaDB/GT.M issues an error but retains all accumulated profiling data and continues tracing. If the tracing is still enabled at the process shutdown and the trace start specified a reporting location, YottaDB/GT.M attempts to place the trace data there. Note that if there is a problem updating the specified trace-reporting global variable, YottaDB/GT.M issues an error at process termination.

M-profiling uses a technique called Basic Block Counting where calls are made to special profiling functions at key points in a YottaDB/GT.M program. A trace consists of the following run-time data as output for each YottaDB/GT.M function, as well as for each YottaDB/GT.M statement:

* The number of times it is executed.
* The total CPU time, subject to the granularity of the operating system provided time functions, spent across all invocations for each function and each YottaDB/GT.M statement as five values: count, user time, system time, total time, and elapsed time.

VIEW "TRACE" also reports details of child processes using two aggregate entries -- "\*RUN" for the current process and "\*CHILDREN" for all of child processes spawned by the current process, each containing user, system, and combined CPU times. The "CHILD" category data excludes processes that result from the JOB command, PIPE devices OPENed with the INDEPENDENT device parameter and processes from PIPE devices that are still active.

Instead of modifying the generated code as done by common profiling tools, such as gprof, M-profiling operates entirely within the YottaDB/GT.M run-time system; therefore, this feature does not require a special compilation, has no effect on code size and minimizes run-time overhead.

When M-profiling is activated, it gathers profiling information for each line and YottaDB/GT.M function invocation. The reported time for a YottaDB/GT.M line is the time spent in generated code for that line, and does not include time spent in entreyrefs called from that line. When M-profiling is deactivated, the accumulated statistics are loaded into a YottaDB/GT.M global. YottaDB/GT.M profiling accumulates and provides the data; the user chooses tools and techniques to analyze the data.

The M-profiling information is stored in the variable in the following format:

* If the expression is a global variable without subscripts such as "^foo", the M-profiling information is stored in the nodes ^foo(<routine>,<label>) and ^foo(<routine>,<label>,<offset>), each holding a value in the form "<count>:<usertime>,:<systemtime>,:<total_time>".
* If the expression has a value such as "^foo("MYTRACE",$J)", the trace information is stored in the nodes ^foo("MYTRACE",<pid>,<routine>,<label>) and ^foo("MYTRACE",<pid>,<routine>,<label>,<offset>), each of which has a value in the form "<count>,<usertime>,<systemtime>,<total_time>" as described above.
* For FOR loops, information for each level of the loop is stored in the nodes as described above, with the extra subscipts "FOR LOOP". <for_level> is the value of the number of iterations at that level of the FOR loop.

Example:

.. parsed-literal::
   GTM>zprint ^profiling
   ; In this example, query^profiling, order^profiling, and merge^profling perform the same operation -- store even-numbered subscripts of a global to a subscripted loc
   al variable. M-profiling results show which yields the fastest execution between the three.
   profiling
     kill ^TMP,^trc
     view "trace":1:"^trc"
     set ulimit=1500
     for i=1:1:ulimit set ^TMP(i)=i
     do qom("^TMP")
     view "trace":0:"^trc"
     zwrite ^trc
     quit
   qom(y)
     do query(y)
     do order(y)
     do merge(y)
     quit
   query(y)
     new i,qryval
     set i=0,y=$query(@y)
     for  quit:y=""   do
     .      set:i#2 qryval(i)=@y
     .      set y=$query(@y)
     .      set i=i+1
     quit
   order(y)
     new i,ordval
     set x="",i=0,y=y_"(x)",x=$order(@y)
     for  quit:x=""  do
     .      set:i#2 ordval(i)=x
     .      set x=$order(@y)
     .      set i=i+1
     quit
   merge(y)
     new i,merval
     set i=0,merval=0
     merge merval=@y
     for i=1:1:$order(merval(""),-1)  do
     .      kill:i#2 merval(i)
     quit

On a Ubuntu system running GTM V6.1-000_x86_64, this example produces an output like the following:

.. parsed-literal::
   GTM>do ^profiling
   ^trc("\*CHILDREN")="0:0:0"
   ^trc("\*RUN")="144009:76004:220013"
   ^trc("profiling","merge")="1:8001:12000:20001:16231"
   ^trc("profiling","merge",0)="1:0:0:0:5"
   ^trc("profiling","merge",1)="1:0:0:0:4"
   ^trc("profiling","merge",2)="1:0:0:0:4"
   ^trc("profiling","merge",3)="1:8001:0:8001:8044"
   ^trc("profiling","merge",4)="1:0:12000:12000:7992"
   ^trc("profiling","merge",4,"FOR_LOOP",1)=1500
   ^trc("profiling","merge",5)="1500:0:0:0:4"
   ^trc("profiling","merge",6)="1:0:0:0:174"
   ^trc("profiling","order")="1:12001:8001:20002:25720"
   ^trc("profiling","order",0)="1:0:0:0:8"
   ^trc("profiling","order",1)="1:0:0:0:6"
   ^trc("profiling","order",2)="1:0:0:0:90"
   ^trc("profiling","order",3)="1:0:8001:8001:7160"
   ^trc("profiling","order",3,"FOR_LOOP",1)=1501
   ^trc("profiling","order",4)="1500:0:0:0:6319"
   ^trc("profiling","order",5)="1500:12001:0:12001:12069"
   ^trc("profiling","order",6)="1500:0:0:0:0"
   ^trc("profiling","order",7)="1:0:0:0:63"
   ^trc("profiling","profiling",3)="1:0:0:0:9"
   ^trc("profiling","profiling",4)="1:52003:20001:72004:74499"
   ^trc("profiling","profiling",4,"FOR_LOOP",1)=1500
   ^trc("profiling","profiling",5)="1:0:0:0:14"
   ^trc("profiling","profiling",6)="1:0:0:0:10"
   ^trc("profiling","qom")="1:0:0:0:78"
   ^trc("profiling","qom",0)="1:0:0:0:18"
   ^trc("profiling","qom",1)="1:0:0:0:11"
   ^trc("profiling","qom",2)="1:0:0:0:9"
   ^trc("profiling","qom",3)="1:0:0:0:11"
   ^trc("profiling","qom",4)="1:0:0:0:5"
   ^trc("profiling","query")="1:72004:20001:92005:88031"
   ^trc("profiling","query",0)="1:0:0:0:5"
   ^trc("profiling","query",1)="1:0:0:0:14"
   ^trc("profiling","query",2)="1:0:0:0:108"
   ^trc("profiling","query",3)="1:12000:0:12000:7625"
   ^trc("profiling","query",3,"FOR_LOOP",1)=1501
   ^trc("profiling","query",4)="1500:8000:0:8000:28256"
   ^trc("profiling","query",5)="1500:52004:20001:72005:51919"
   ^trc("profiling","query",6)="1500:0:0:0:0"
   ^trc("profiling","query",7)="1:0:0:0:85"

* CPU times are reported in microseconds. 1 second = 1,000,000 microseconds.
* ^trc("\*CHILDREN")="0:0:0" indicates that the main process did not spawn any child process.
* ^trc("\*RUN")="144009:76004:220013" : the three pieces specify the aggregate User Time, System Time and Total Time values for the main process.
* ^trc("profiling","query",3,"FOR_LOOP",1)=1501 specifies the number of times the FOR loop was executed on line #3 of query^profiling.
* ^trc("profiling","merge")="1:8001:12000:20001:16231", ^trc("profiling","order")="1:12001:8001:20002:25720", ^trc("profiling","query")="1:72004:20001:92005:88031": the five pieces specify the aggregate Execution Count, User Time, System,Time, Total Time and the Elapsed Time of the code execution for merge^profiling, order^profling, and query^profiling. merge^profiling has the fastest execution time followed by order^profiling. query^profiling is the slowest amongst the three.
* ^trc("profiling","merge",3)="1:8001:0:8001:8044" and others like it specifies the cumulative Execution Count, User Time, System Time, Total Time and the Elapsed Time of the code execution of line 3 of merge^profiling.
* The M-profiling results are subject to the granularity of the operating system provided time functions. CPU time entries having 0:0:0 values indicate lightweight M mode having 0 to less than 1 microsecond.

Consider the following program that presents the output of this M-profiling result in a tabular report. 

.. parsed-literal::
   GTM>zprint ^tracereport
   tracereport(gbl,label,rtn)
     set gap=15
     set $piece(x,".",gap*6)="" write x,!
     write "Line #",?gap,"Count",?gap*2,"User Time",?gap*3,"System Time",?gap*4,"Total Time",?gap*5,"Elapsed Time",!
     set $piece(x,".",gap*6)="" write x,!
     for  set gbl=$query(@gbl) quit:gbl=""  do
     .      if ($length(@gbl,":")=5)&($qsubscript(gbl,1)=rtn)&($qsubscript(gbl,2)=label) do
            ..      set gap=15 set lineno=$qsubscript(gbl,3)
            ..      if lineno="" write label," total",?gap set zp=""
            ..      else  write lineno,?gap set zp=label\_"+"_lineno_"^"_rtn
            ..      for i=1:1:5 set gap=gap+15 write $piece(@gbl,":",i),?gap
            ..      write !
            ..      set maxlines=$qsubscript(gbl,3)
      for i=0:1:maxlines do
      .      set zp=label\_"+"_i_"^"_rtn
      .      write "Line #",i,": ",?9
      .      zprint @zp
   
   GTM>do ^tracereport("^trc","order","profiling")
   .........................................................................................
   Line #         Count          User Time      System Time    Total Time     Elapsed Time
   .........................................................................................
   order total    1              12001          8001           20002          25720
   0              1              0              0              0              8
   1              1              0              0              0              6
   2              1              0              0              0              90
   3              1              0              8001           8001           7160
   4              1500           0              0              0              6319
   5              1500           12001          0              12001          12069
   6              1500           0              0              0              0
   7              1              0              0              0              63
   Line #0: order(y)
   Line #1:   new i,ordval
   Line #2:   set x="",i=0,y=y\_"(x)",x=$order(@y)
   Line #3:   for  quit:x=""  do
   Line #4:   .      set:i#2 ordval(i)=x
   Line #5:   .      set x=$order(@y)
   Line #6:   .      set i=i+1
   Line #7:   quit

This shows that order^profiling has an elapsed time of 25720 and the maximum elapsed time was on line #5, which was executed 1500 times.

.. parsed-literal::
   GTM>do ^tracereport("^trc","merge","profiling")
   .........................................................................................
   Line #         Count          User Time      System Time    Total Time     Elapsed Time
   .........................................................................................
   merge total    1              8001           12000          20001          16231
   0              1              0              0              0              5
   1              1              0              0              0              4
   2              1              0              0              0              4
   3              1              8001           0              8001           8044
   4              1              0              12000          12000          7992
   5              1500           0              0              0              4
   6              1              0              0              0              174
   Line #0: merge(y)
   Line #1:   new i,merval
   Line #2:   set i=0,merval=0
   Line #3:   merge merval=@y
   Line #4:   for i=1:1:$order(merval(""),-1)  do
   Line #5:   . kill:i#2 merval(i)
   Line #6:   quit
   GTM>

This shows that merge^profiling has an elapsed time of 16231 and the maximum elapsed time was on line #3, which was executed once.

Note that M-profiling results are reported for each line. While reporting time for a line containing an invocation of a label, M-profiling excludes the execution time of that label.

Here is an example:

.. parsed-literal::
   GTM>do ^tracereport("^trc","qom","profiling")
   .........................................................................................
   Line #         Count          User Time      System Time    Total Time     Elapsed Time
   .........................................................................................
   qom total      1              0              0              0              78
   0              1              0              0              0              18
   1              1              0              0              0              11
   2              1              0              0              0              9
   3              1              0              0              0              11
   4              1              0              0              0              5
   Line #0: qom(y)
   Line #1:   do query(y)
   Line #2:   do order(y)
   Line #3:   do merge(y)
   Line #4:   quit

Notice that the execution of do merge(y) reports an Elapsed Time of 9 whereas merge^profiling reported an Elapsed Time of 1149.

You can write programs like tracereport.m to interpret the results of the M-profiling data and also use them to analyze your code execution path based on your unique requirements.

view "trace":1: "<gbl>" and view "trace":0: "<gbl>" commands enable and disable M-profiling.

To perform entryref-specific M-profiling without modifying the source program, use ZBREAK. For example, to perform M-profiling of the entryref merge^profiling, remove VIEW "TRACE" commands from profiling.m and then execute the following commands:

.. parsed-literal::
   GTM>ZBREAK merge^profiling:"view ""TRACE"":1:""^mtrc"" write ""Trace"""
   GTM>do ^profiling
   Trace
   GTM>view "TRACE":0:"^mtrc"
    
   GTM>zwrite ^mtrc
   ^mtrc("\*CHILDREN")="0:0:0"
   ^mtrc("\*RUN")="132008:52003:184011"
   ^mtrc("GTM$DMOD","^")="1:0:0:0:4"
   ^mtrc("profiling","merge")="1:8001:0:8001:13450"
   ^mtrc("profiling","merge",1)="1:0:0:0:6"
   ^mtrc("profiling","merge",2)="1:0:0:0:5"
   ^mtrc("profiling","merge",3)="1:8001:0:8001:6188"
   ^mtrc("profiling","merge",4)="1:0:0:0:7149"
   ^mtrc("profiling","merge",4,"FOR_LOOP",1)=1500
   ^mtrc("profiling","merge",5)="1500:0:0:0:4"
   ^mtrc("profiling","merge",6)="1:0:0:0:63"
   ^mtrc("profiling","profiling")="1:0:0:0:9"
   ^mtrc("profiling","profiling",8)="1:0:0:0:4"
   ^mtrc("profiling","qom")="1:0:0:0:9"
   ^mtrc("profiling","qom",4)="1:0:0:0:4" 


Example:

If prof.m is:

.. parsed-literal::
   prof;
       set start=1
       set finish=1000
       view "TRACE":1:"^trc"
       kill cycle S max=$$docycle(start,finish,"cycle")
       view "TRACE":0:"^trc"
       zwrite ^trc
       quit
       ;
   docycle(first,last,var)
       new i,currpath,current,maxcycle,n
       set maxcycle=1
       for current=first:1:last do cyclehelper
       quit maxcycle
       ;
   cyclehelper
       set n=current
       kill currpath
       for i=0:1 quit:$data(@var@(n))!(1=n)  D
       .    set currpath(i)=n
       .    do iterate
       if 0<i do
       .    if 1=n set i=i+1
       .    else  set i=i+@var@(n)
       .    do updatemax
       .    set n="" for  set n=$O(currpath(n)) Q:""=n  S @var@(currpath(n))=i-n
       Q
       ;
    iterate
       if 0=(n#2) set n=n/2
       else  set n=3*n+1
       quit
       ;
    updatemax
       set:i>maxcycle maxcycle=i
       quit
       ;

On executing prof, the output looks like the following (times in the example were chosen for clarity of illustration and are not typical).

.. parsed-literal::
   ^trc("\*CHILDREN")="0:0:0"
   ^trc("\*RUN")="224014:12000:236014"
   ^trc("prof","cyclehelper")="1000:200013:0:200013:206318"
   ^trc("prof","cyclehelper",1)="1000:12001:0:12001:3202"
   ^trc("prof","cyclehelper",2)="1000:0:0:0:3766"
   ^trc("prof","cyclehelper",3)="1000:64004:0:64004:94215"
   ^trc("prof","cyclehelper",3,"FOR_LOOP",1)=3227
   ^trc("prof","cyclehelper",4)="2227:0:0:0:9864"
   ^trc("prof","cyclehelper",5)="2227:0:0:0:7672"
   ^trc("prof","cyclehelper",6)="1000:12000:0:12000:3758"
   ^trc("prof","cyclehelper",7)="432:0:0:0:1520"
   ^trc("prof","cyclehelper",8)="432:8000:0:8000:11003"
   ^trc("prof","cyclehelper",9)="432:0:0:0:3298"
   ^trc("prof","cyclehelper",10)="432:104008:0:104008:61564"
   ^trc("prof","cyclehelper",10,"FOR_LOOP",1)=2659
   ^trc("prof","cyclehelper",11)="1000:0:0:0:3424"
   ^trc("prof","docycle")="1:12001:0:12001:4886"
   ^trc("prof","docycle",0)="1:0:0:0:83"
   ^trc("prof","docycle",1)="1:0:0:0:36"
   ^trc("prof","docycle",2)="1:0:0:0:4"
   ^trc("prof","docycle",3)="1:12001:0:12001:4706"
   ^trc("prof","docycle",3,"FOR_LOOP",1)=1000
   ^trc("prof","docycle",4)="1:0:0:0:1718579845"
   ^trc("prof","iterate")="2227:12000:12000:24000:30240"
   ^trc("prof","iterate",1)="2227:0:0:0:8271"
   ^trc("prof","iterate",2)="2227:12000:0:12000:7727"
   ^trc("prof","iterate",3)="2227:0:0:0:7658"
   ^trc("prof","prof",4)="1:0:0:0:22"
   ^trc("prof","prof",5)="1:0:0:0:8"
   ^trc("prof","updatemax")="432:0:0:0:4276"
   ^trc("prof","updatemax",1)="432:0:0:0:1465"
   ^trc("prof","updatemax",2)="432:0:0:0:1496"

Example:

If fortypes.m is:

.. parsed-literal::
   fortypes;
       new i,j,k,v
       set k=1
       view "TRACE":1:"^trc"
       for i=1:1:3  set v=i
       for i=1:1  set v=0  quit:i=3
       for i=1,2:1:4,6  set v=0
       for i=1:1,2  set v=0  quit:i=3
       for i=1:1:2  for j=1:1:3  set v=0
       for i=1:1:2  
       .    for j=1:1:1  do
       ..        set v=0
       set j=5  for i=1:1:j  do
       .    set j=(j-1)
       for i=1:1:2  for j=1:1:3  do
       .    set v=0
       for i=1:1:2  do
       .    for j=1:1:3  set v=0
       for i=1:1:2  do
       .    for j=1:1:3  do
       ..        set v=0
       for i="foo","bar",1:1  set v=0  quit:i=3
       for  set k=k+1  quit:k=3
       for i=1:1:3  for j=1:1:(3-i)  set v=0
       for i=1:1:3  for j=1:1:(3-i)  for k=1:1:(j+1)  set v=0
       set k=3  view "TRACE":0:"^trc"
       zwrite ^trc
       quit

On executing fortypes, the output looks something like the following:

.. parsed-literal::
   ^trc("\*CHILDREN")="4000:0:4000"
   ^trc("\*RUN")="468029:48003:516032"
   ^trc("fortypes","fortypes",5)="1:0:0:0:9"
   ^trc("fortypes","fortypes",5,"FOR_LOOP",1)=3
   ^trc("fortypes","fortypes",7)="1:0:0:0:6"
   ^trc("fortypes","fortypes",7,"FOR_LOOP",1)=3
   ^trc("fortypes","fortypes",9)="1:0:0:0:6"
   ^trc("fortypes","fortypes",9,"FOR_LOOP",1)=5
   ^trc("fortypes","fortypes",11)="1:0:0:0:6"
   ^trc("fortypes","fortypes",11,"FOR_LOOP",1)=3
   ^trc("fortypes","fortypes",13)="1:0:0:0:8"
   ^trc("fortypes","fortypes",13,"FOR_LOOP",1)=2
   ^trc("fortypes","fortypes",13,"FOR_LOOP",2)=6
   ^trc("fortypes","fortypes",15)="1:0:0:0:4"
   ^trc("fortypes","fortypes",15,"FOR_LOOP",1)=2
   ^trc("fortypes","fortypes",19)="1:0:0:0:26"
   ^trc("fortypes","fortypes",19,"FOR_LOOP",1)=5
   ^trc("fortypes","fortypes",20)="5:0:0:0:4"
   ^trc("fortypes","fortypes",22)="1:0:0:0:27"
   ^trc("fortypes","fortypes",22,"FOR_LOOP",1)=2
   ^trc("fortypes","fortypes",22,"FOR_LOOP",2)=6
   ^trc("fortypes","fortypes",23)="6:0:0:0:3"
   ^trc("fortypes","fortypes",25)="1:0:0:0:11"
   ^trc("fortypes","fortypes",25,"FOR_LOOP",1)=2
   ^trc("fortypes","fortypes",26)="2:0:0:0:6"
   ^trc("fortypes","fortypes",26,"FOR_LOOP",1)=6
   ^trc("fortypes","fortypes",28)="1:0:0:0:8"
   ^trc("fortypes","fortypes",28,"FOR_LOOP",1)=2
   ^trc("fortypes","fortypes",29)="2:0:0:0:26"
   ^trc("fortypes","fortypes",29,"FOR_LOOP",1)=6
   ^trc("fortypes","fortypes",30)="6:0:0:0:4"
   ^trc("fortypes","fortypes",32)="1:0:0:0:8"
   ^trc("fortypes","fortypes",32,"FOR_LOOP",1)=5
   ^trc("fortypes","fortypes",34)="1:0:0:0:5"
   ^trc("fortypes","fortypes",34,"FOR_LOOP",1)=2
   ^trc("fortypes","fortypes",36)="1:0:0:0:8"
   ^trc("fortypes","fortypes",36,"FOR_LOOP",1)=3
   ^trc("fortypes","fortypes",36,"FOR_LOOP",2)=3
   ^trc("fortypes","fortypes",38)="1:0:0:0:14"
   ^trc("fortypes","fortypes",38,"FOR_LOOP",1)=3
   ^trc("fortypes","fortypes",38,"FOR_LOOP",2)=3
   ^trc("fortypes","fortypes",38,"FOR_LOOP",3)=7

**"ZDATE_FORM":"value"**

Determines whether four digit year code is active for $ZDATE() function. GT.M defaults to zero (0), that is, two digit output. For more usage information, refer to “$ZDate()”.

If no value is given with the VIEW command, it turns four digit code on. It is equivalent to the intrinsic special variable $ZDATEFORM. Use $ZDATEFORM to set this VIEW keyword. Also, logical name environment variable gtm_zdate_form may be used to set the initial value to this factor.

++++++++++++++++++++++++
Examples of VIEW
++++++++++++++++++++++++

Example:

.. parsed-literal::
   GTM>Kill A
                   
   GTM>View "NOUNDEF"
   GTM>Write A,?10,$L(A)
         0
   GTM>

This demonstrates how a VIEW that specifies NOUNDEF prevents UNDEFined errors.

Example 2:

.. parsed-literal::
   GTM>ZLink "NOSENSE"
   %GTM-E-LABELMISSING Label referenced but
   not defined:lab
   %GTM-I-SRCNAM in source module /home/gtmuser1/.fis-gtm/V5.4-002B_x86/r/
   NOSENSE.m
   GTM>ZPrint ^NOSENSE
   NOSENSE;
           Do lab
           Quit
   LAB  Write !,"THIS IS NOSENSE"
           Quit
   GTM>View "LABELS":"UPPER"
   GTM>ZLink "NOSENSE.m"
   GTM>Do ^NOSENSE
   THIS IS NOSENSE
   GTM>

This demonstrates use of VIEW "LABELS" to make label handling case insensitive. Notice that the routine was ZLINKed with an extension of .m to force a recompile and ensure that the object code and the run-time handling of labels is the same.

------------------
Write
------------------

The WRITE command transfers a character stream specified by its arguments to the current device.

The format of the WRITE command is:

.. parsed-literal::
   W[RITE][:tvexpr] expr\|\*intexpr\|fcc[,...]

* The optional truth-valued expression immediately following the command is a command postconditional that controls whether or not YottaDB/GT.M executes the command.
* An expression argument supplies the text of a WRITE.
* When a WRITE argument consists of a leading asterisk (*) followed by an integer expression, WRITE outputs one ASCII character associated with the ASCII code specified by the integer evaluation of the expression.
* WRITE arguments may also be format control characters; format control characters modify the position of a virtual cursor: an exclamation point (!) produces a new line, a number-sign (#) produces a new page and a question-mark (?) followed by an expression moves the virtual cursor to the column specified by the integer evaluation of the expression provided that the virtual cursor is to the "left" of the specified column; if the virtual cursor is not to the left of the specified column, then the text is printed at the current cursor position.
* An indirection operator and an expression atom evaluating to a list of one or more WRITE arguments form a legal argument for a WRITE.
* In the UTF-8 mode, the WRITE command uses the character set specified on the device OPEN as the character encoding of the output device. If character set specifies "M" or "UTF-8", YottaDB/GT.M WRITEs the data with no transformation. If character set specifies "UTF-16", "UTF-16LE" or "UTF-16BE", the data is assumed to be encoded in UTF-8 and WRITE transforms it to the character encoding specified by character set device parameter.
* If a WRITE command encounters an illegal character in UTF-8 mode, it produces a run-time error irrespective of the setting of VIEW "BADCHAR".

-----------------------
Xecute
-----------------------

The XECUTE command makes an entry in the YottaDB/GT.M invocation stack and executes the argument as YottaDB/GT.M code.

The format of the XECUTE command is:

.. parsed-literal::
   X[ECUTE]:tvexpr expr[:tvexpr][,...]

* The optional truth-valued expression immediately following the command is a command postconditional that controls whether or not YottaDB/GT.M executes the command.
* The required expression specifies a fragment of YottaDB/GT.M source code. The maximum length of the expression is 8192 bytes.
* The optional truth-valued expression immediately following the argument expression specifies the argument postconditional and controls whether YottaDB/GT.M performs an XECUTE with that argument.
* An indirection operator and an expression atom evaluating to a list of one or more XECUTE arguments form a legal argument for an XECUTE.
* Run-time errors from indirection or XECUTEs maintain $STATUS and $ZSTATUS related information and cause normal error handling but do not provide compiler supplied information on the location of any error within the code fragment.

An explicit or implicit QUIT within the scope of the XECUTE, but not within the scope of any closer DO, FOR, XECUTE or extrinsic, returns execution to the instruction following the calling point. This may be the next XECUTE argument or another command. At the end of the code specified by the XECUTE argument expression, YottaDB/GT.M performs an implicit QUIT.

Because XECUTE causes run-time compilation in YottaDB/GT.M, and because it tends to obscure code, use XECUTE only when other approaches clearly do not meet your particular requirement.

YottaDB/GT.M compiles XECUTE <literal> at compile time when the literal is valid YottaDB/GT.M code that has minimal impact on the M virtual machine. An XECUTE literal containing GOTO, NEW, QUIT, (nested) XECUTE and indirection can't be precompiled because of the interaction of those features with the stack architecture of the M virtual machine. Precompiled XECUTE literals do not show up in $STATCK() as having a separate stack level, but rather "disappear" into the stack level of the original XECUTE. Please observe the following cautions: 

* ensure you compile with the same YottaDB/GT.M version, $gtm_chset, $gtm_local_collate, $gtm_patnumeric, $gtm_pattern_file and $gtm_pattern_table values (or lack thereof) as those used to run your application.
* If the application changes the run time values controlled by those environment variables, use variable operands or indirection, rather than literals for operands with pattern match (?) or sorts-after (]]).

Note that indirection almost always performs better than an XECUTE that can't be precompiled. Note also that adding a QUIT at the end of an XECUTE that does not contain a FOR will leave it for run time compilation.

+++++++++++++++++++++
Examples of XECUTE
+++++++++++++++++++++

Example:

.. parsed-literal::
   GTM>Xecute "Write ""HELLO"""
   HELLO
   GTM>

This demonstrates a simple use of Xecute.

Example:

.. parsed-literal::
   Set x="" For Set x=$Order(^%x(x)) Quit:x=""  Xecute x

This $ORDER() loop XECUTEs code out of the first level of the global array ^%x. Note that, in most cases, having the code in a YottaDB/GT.M source file, for example TMPX.m, and using a Do ^TMPX improves efficiency.

--------------------
ZAllocate
--------------------

The ZALLOCATE command reserves the specified name without releasing previously reserved names. Other YottaDB/GT.M processes cannot reserve the ZALLOCATEd name with a ZALLOCATE or LOCK command.

The ZALLOCATE command provides compatibility with some other YottaDB/GT.M implementations. The M Development Committee chose to add the + and - delimiters to the LOCK command (incremental locking) rather than adopt the ZALLOCATE and ZDEALLOCATE approach. Therefore, when a design requires an incremental lock mechanism, LOCK +/- has the advantage over ZALLOCATE / ZDEALLOCATE of being part of the M standard. LOCK +/- also has the advantage of working symmetrically when routines using LOCKs are nested. That is, a ZALLOCATE command issued by a process for a named resource already ZALLOCATEd by that process results in no change of state. This means that routines that do ZALLOCATE followed by a ZDEALLOCATE on a named resource that is already ZALLOCATEd by the same process (at routine entry time), will end up ZDEALLOCATEing the named resource (which might not be desired). On the other hand, a LOCK + command issued by a process for a named resource already LOCKed by that process causes the LEVEL of the LOCK to be incremented (as seen in a ZSHOW "L" output). Every LOCK - command on that named resource causes the LEVEL to be decremented. When the LEVEL becomes 0, the named resource is no longer LOCKed.

For more information on troubleshooting LOCKs with the M Lock Utility (LKE), refer to the appropriate chapter of the Administration and Operations Guide.

The format of the ZALLOCATE command is:

.. parsed-literal::
   ZA[LLOCATE][:tvexpr] [(]nref[,...][)][:intexpr][,...]

* The optional truth-valued expression immediately following the command is a command postconditional that controls whether or not YottaDB/GT.M executes the command.
* The nref argument specifies a name in the format of a YottaDB/GT.M name with or without subscripts, and with or without a preceding caret (^).
* Outside of transactions, only one process in an environment can ZALLOCATE (or LOCK) a particular resource name at any given time.
* Because the data storage in YottaDB/GT.M uses hierarchical sparse arrays and ZALLOCATE may serve to protect that data from inappropriate "simultaneous" access by multiple processes, ZALLOCATE treats resource names in a hierarchical fashion; a ZALLOCATE protects not only the named resource, but also its ancestors and descendants.
* When one or more nrefs are enclosed in parentheses (), ZALLOCATE reserves all the enclosed names "simultaneously," that is, it reserves none of them until all become available.
* The optional numeric expression specifies a time in seconds after which the command should timeout if unsuccessful; choosing 0 results in a single attempt. If a ZALLOCATE command specifies a timeout that do not exceed $ZMAXTPTIME and the resource name is locked on the final retry, the process may generate TPNOACID messages while it tries to ensure there is no possibility of a deadlock.
* An indirection operator and an expression atom evaluating to a list of one or more ZALLOCATE arguments form a legal argument for a ZALLOCATE.

For additional information on the locking mechanism, refer to the "LOCK" section in the M LOCK Utility chapter of the Administration and Operations Guide.

If a ZALLOCATE command specifies a timeout, and YottaDB/GT.M acquires ownership of the named resource before the timeout elapses, ZALLOCATE sets $TEST to TRUE (1). If YottaDB/GT.M cannot acquire ownership of the named resource within the specified timeout, ZALLOCATE sets $TEST to FALSE (0). If a ZALLOCATE command does not specify a timeout, the execution of the command does not affect $TEST.

When given a list of nrefs, ZALLOCATE tries to reserve each nref from left to right in the order specified taking into account the timeout specified for each. If the timeout elapses before reserving an nref, YottaDB/GT.M terminates the ZALLOCATE command. Any nrefs already acquired as part of the current ZALLOCATE command stay acquired.

+++++++++++++++++++++++
Examples of ZALLOCATE
+++++++++++++++++++++++

Example:

.. parsed-literal::
   ZAllocate A
   ZAllocate ^A
   ZAllocate ^A(1)
   ZAllocate (^B("smith"),^C("jones"))
   ZAllocate @A

The first command ZALLOCATEs A; the second, ^A; the third, ^A(1) and the fourth, both ^B("smith") and ^C("jones") simultaneously. The last command ZALLOCATEs the resources named by the value of the variable A.

Example:

.. parsed-literal::
   ZAllocate A,^B,@C
   ZALLOCATE (A,B,C)

If ZALLOCATE arguments are enclosed in parentheses, the command waits until all names in the argument list become available before reserving any of the names. For example, in the statement ZA (A,B,C), if the resource named C is not available, ZALLOCATE waits until C becomes available before reserving A and B. Using the format illustrated in the first line above, can cause deadlocks because the resource names are reserved as they come available.

When a process attempts to ZALLOCATE a name currently ZALLOCATEd or LOCKed (with the LOCK command) by another process, the ZALLOCATEing process hangs until the other process releases the name. In the event that names remain unavailable for significant periods of time, timeouts allow the process issuing a ZALLOCATE to regain program control.

Example:

.. parsed-literal::
   ZAllocate ^D:5

This example specifies a timeout of five seconds. If YottaDB/GT.M reserves ^D before the five seconds elapses, ZALLOCATE sets $TEST to TRUE. If YottaD/GT.M cannot reserve ^D within the five second timeout, ZALLOCATE sets $TEST to FALSE.

At the time of ZALLOCATEing a name, no names previously reserved with ZALLOCATE or the LOCK command are released (similarly, LOCKing a name does not release names that have been ZALLOCATEd). For example, after ZALLOCATEing A and LOCKing B, LOCKing B does not release A, and ZALLOCATEing C does not release A or B.

ZDEALLOCATE releases ZALLOCATED resource names. The ZDEALLOCATE command can only release previously ZALLOCATEd (not LOCKed) names.

Resource name arguments for LOCKs and ZALLOCATEs intersect. That is, if one process holds a LOCK or ZALLOCATE, another process can neither LOCK nor ZALLOCATE any name falling in the hierarchy of the resource name held by the first process. When a process holds a LOCK or ZALLOCATE, that same process may also LOCK or ZALLOCATE resource names falling in the hierarchy of the currently held resource name. When a single process holds both LOCKs and ZALLOCATEs, a LOCK does not release the ZALLOCATEd resource(s) and a ZDEALLOCATE does not release the LOCKed resource(s).

Also see the description of the ZDEALLOCATE command described later in this chapter.

Example:

.. parsed-literal::
   Lock ^AR(PNT)
   .
   .
   .
   ZAllocate ^AR(PNT,SUB)
   .
   .
   .
   Lock ^TOT(TDT)
   .
   .
   ZDEALLOCATE ^AR(PNT,SUB)

This LOCKs ^AR(PNT)and all its descendents, then, after performing some unspecified commands, it ZALLOCATEs ^AR(PNT,SUB). ZALLOCATE does not imply any change to LOCKs or existing ZALLOCATEd resource names, therefore, the LOCK of ^AR(PNT) remains in effect. ^AR(PNT,SUB) is already protected by the LOCK. Next, because an unsigned LOCK releases all resource names currently LOCKed by the process, the routine releases ^AR(PNT) with the LOCK of ^TOT(TDT). This leaves the ZALLOCATE of ^AR(PNT,SUB). The name ^AR and all its subscripts except for ^AR(PNT) and those that begin with ^AR(PNT,SUB) are now available for LOCKing by other processes. Finally the routine releases ^AR(PNT,SUB) with a ZDEALLOCATE command. The ZDEALLOCATE does not affect the LOCK on ^TOT(TDT). Note that this example was constructed to illustrate the interaction between LOCK, ZALLOCATE and ZDEALLOCATE, and not to illustrate sound programming practice..

Because the ZALLOCATE command reserves names without releasing previously reserved names, it can lead to deadlocks. For example, a deadlock occurs if two users ZALLOCATE names A and B in the following sequence:

**Deadlock Situation**

+------------------------+-------------------------------+
| User X                 | User Y                        |
+========================+===============================+
| ZAllocate A            | ZAllocate B                   |
+------------------------+-------------------------------+
| ZAllocate B            | ZAllocate A                   |
+------------------------+-------------------------------+

To avoid deadlocks, use a timeout. Because unsigned LOCKs always release previously reserved names, such LOCKs inherently prevent deadlocks.

**ZAllocate Operation Summary**

+----------------------------+-------------------------------------+----------------------------------------------------------------------------------------------------------------------+
| Preexisting Condition      | Command Issued                      | Result                                                                                                               |
+============================+=====================================+======================================================================================================================+
| Another user reserved M    | ZA M                                | Your process waits                                                                                                   |
+                            +-------------------------------------+----------------------------------------------------------------------------------------------------------------------+
|                            | LOCK M                              | Your process waits                                                                                                   |
+                            +-------------------------------------+----------------------------------------------------------------------------------------------------------------------+
|                            | ZD M                                | No effect                                                                                                            |
+----------------------------+-------------------------------------+----------------------------------------------------------------------------------------------------------------------+
| You reserved M with LOCK M | ZA M                                | M is ZALLOCATEd and LOCKed; use both ZDEALLOCATE and LOCK (L or L -M) to clear M                                     |
+                            +-------------------------------------+----------------------------------------------------------------------------------------------------------------------+
|                            | LOCK M                              | Release M and reserve M again                                                                                        |
+                            +-------------------------------------+----------------------------------------------------------------------------------------------------------------------+
|                            | ZD M                                | No effect                                                                                                            |
+----------------------------+-------------------------------------+----------------------------------------------------------------------------------------------------------------------+
| You reserved M with ZA M   | ZA M                                | No effect                                                                                                            |
+                            +-------------------------------------+----------------------------------------------------------------------------------------------------------------------+
|                            | LOCK M                              | M is ZALLOCATEd and LOCKed; use both ZDEALLOCATE and LOCK (L or L -M) to clear M                                     |
+                            +-------------------------------------+----------------------------------------------------------------------------------------------------------------------+
|                            | ZD M                                | No effect                                                                                                            |
+----------------------------+-------------------------------------+----------------------------------------------------------------------------------------------------------------------+


