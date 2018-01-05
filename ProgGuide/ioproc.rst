
.. index::
   Input/Output Processing

========================
Input/Output Processing
========================

.. contents::
   :depth: 2

This chapter describes the following topics which relate to input and output processing:

* Input/Output Intrinsic Special Variables, and their Maintenance : YottaDB/GT.M provides several intrinsic special variables that allow processes to examine, and in some cases change, certain aspects of the input/output (I/O) processing. The focus in this chapter is how YottaDB/GT.M handles the standard ones, such as $IO, $X, $Y, and those that are YottaDB/GT.M-specific (for example, $ZA, $ZB).

* Input/Output Devices: Each device type supported by YottaDB/GT.M responds to a particular subset of deviceparameters, while ignoring others. Devices may be programmed in a device-specific manner, or in a device-independent manner. This chapter discusses each device type, and provides tables of their deviceparameters.

* Input/Output Commands and their Deviceparameters: YottaDB/GT.M bases its I/O processing on a simple character stream model. YottaDB/GT.M does not use any pre-declared formats. This chapter describes the YottaDB/GT.M I/O commands OPEN, USE, READ, WRITE, and CLOSE.

OPEN, USE, and CLOSE commands accept deviceparameters, which are keywords that permit a YottaDB/GT.M program to control the device state. Some deviceparameters require arguments. The current ANSI standard for YottaDB/GT.M does not define the deviceparameters for all devices. This chapter includes descriptions of the YottaDB/GT.M deviceparameters in the sections describing each command.

.. note::
   The term "device" can refer to an entity manipulated by application code using Open, Use, Close, Read and Write commands as well as a device from the perspective of the operating system.  We endeavor herein to always make it clear from the context which meaning is intended.

----------------------------------
I/O Intrinsic Special Variables
----------------------------------

YottaDB/GT.M intrinsic special variables provide a mean for application code to communicate and manage the state of a device.

++++++++++++++++++++++++
Device Name Variables
++++++++++++++++++++++++

YottaDB/GT.M provides three intrinsic special variables that identify devices.

**$IO**

$I[O] contains the name of the current device specified by the last USE command. A SET command cannot modify $IO. USE produces the same $IO as USE $PRINCIPAL, but $P is the preferred construct.

**$PRINCIPAL**

A process inherits three open file descriptors from its parent - STDIN, STDOUT and STDERR - which can all map to different files or devices. YottaDB/GT.M provides no way for M application to access STDERR. Although STDIN and STDOUT may map to different devices, files, sockets, pipes, etc. in the operating system, M provides for only device $PRINCIPAL, to refers to both. At process startup, and when $PRINCIPAL is selected with a USE command, READ commands apply to STDIN and WRITE commands apply to STDOUT. The device type of the standard input determines which USE deviceparameters apply to $PRINCIPAL.

For an interactive process, $PRINCIPAL is the user's terminal. YottaDB/GT.M ignores a CLOSE of the principal device. YottaDB/GT.M does not permit a SET command to modify $PRINCIPAL.

0 is an alternate for $PRINCIPAL (for example, USE 0). YottaDB/FIS recommends that application code use $PRINCIPAL. The environment variable gtm_principal can be used to set a string reported by YottaDB/GT.M for $PRINCIPAL and which can be used in lieu of $PRINCIPAL for the USE command.

**$ZIO**

$ZIO contains the translated name of the current device, in contrast to $IO, which contains the name as specified by the USE command.

++++++++++++++++++++++++++
Cursor Position Variables
++++++++++++++++++++++++++

YottaDB/GT.M provides two intrinsic special variables for determining the virtual cursor position. $X refers to the current column, while $Y refers to the current row.

**$X**

$X contains an integer value ranging from 0 to 65,535, specifying the horizontal position of a virtual cursor in the current output record. $X=0 represents the initial position on a new record or row.

Every OPENed device has a $X. However, YottaDB/GT.M only has access to $X of the current device.

Generally, in M mode YottaDB/GT.M increments $X for every character written to and read from the current device; see below for behavior of a UTF-8 mode device. YottaDB/GT.M format control characters, FILTER, and the device WIDTH and WRAP also have an effect on $X.

As $X is only a counter to help a program track output, SET $X does not reposition the cursor or perform any other IO. Conversely, if a sequence of characters sent to a terminal or other device with a WRITE causes it to be repositioned except as described below, $X will not reflect this change.

**$Y**

$Y contains an integer value ranging from 0 to 65,535, specifying the vertical position of a virtual cursor in the current output record. $Y=0 represents the top row or line.

Every OPEN device has a $Y. However, YottaDB/GT.M only accesses $Y of the current device.

When YottaDB/GT.M finishes the logical record in progress, it generally increments $Y. YottaDB/GT.M recognizes the end of a logical record when it processes certain YottaDB/GT.M format control characters, or when the record reaches its maximum size, as determined by the device WIDTH, and the device is set to WRAP. The definition of "logical record" varies from device to device. For an exact definition, see the sections on each device type. FILTER and the device LENGTH also have an effect on $Y.

As $Y is only a counter to help a program track output, SET $Y does not reposition the cursor or perform any other IO. Conversely, if a sequence of characters sent to a terminal or other device with a WRITE causes it to be repositioned except as described below, $Y will not reflect this change. 

**Maintenance of $X and $Y**

The following factors affect the maintenance of the virtual cursor position ($X and $Y):

* The bounds of the virtual "page"
* Format control characters
* YottaDB/GT.M character filtering

Each device has a WIDTH and a LENGTH that define the virtual "page." The WIDTH determines the maximum size of a record for a device, while the LENGTH determines how many records fit on a page. YottaDB/GT.M starts a new record when the current record size ($X) reaches the maximum WIDTH and the device has WRAP enabled. When the current line ($Y) reaches the maximum LENGTH, YottaDB/GT.M starts a new page.

YottaDB/GT.M has several format control characters (used in the context of a WRITE command) that allow the manipulation of the virtual cursor. For all I/O devices, the YottaDB/GT.M format control characters do the following:

* ! Sets $X to zero (0) and increments $Y, and terminates the logical record in progress. The definition of "logical record" varies from device to device, and is discussed in each device section.
* # Sets $X and $Y to zero (0), and terminates the logical record in progress.
* ?n If n is greater than $X, writes n-$X spaces to the device, bringing $X to n. If n is less than or equal to $X, ?n has no effect. When WRAP is enabled and n exceeds the WIDTH of the line, WRITE ?n increments $Y and sets $X equal to n#WIDTH, where # is the YottaDB/GT.M modulo operator.

In UTF-8 mode, YottaDB/GT.M maintains $X in the following measurement units:

+--------------------------------------------+--------------------------------------------------+---------------------------------------------------+
| Devices                                    | Input                                            | Output                                            |
+============================================+==================================================+===================================================+
| FIFO                                       | code points                                      | display columns                                   |
+--------------------------------------------+--------------------------------------------------+---------------------------------------------------+
| PIPE                                       | code points                                      | display columns                                   |
+--------------------------------------------+--------------------------------------------------+---------------------------------------------------+
| SD                                         | code points                                      | display columns                                   |
+--------------------------------------------+--------------------------------------------------+---------------------------------------------------+
| SOC                                        | code points                                      | code points                                       |
+--------------------------------------------+--------------------------------------------------+---------------------------------------------------+
| TRM                                        | display columns                                  | display columns                                   |
+--------------------------------------------+--------------------------------------------------+---------------------------------------------------+

YottaDB/GT.M provides two modes of character filtering. When filtering is enabled, certain <CTRL> characters and/or escape sequences have special effects on the cursor position (for example, <BS> (ASCII 8) may decrement $X, if $X is non-zero). For more information on write filtering, refer to “FILTER”.

+++++++++++++++++++++++++++++++++
Status Variables
+++++++++++++++++++++++++++++++++

**$DEVICE**

If the last commanded resulted in no error-condition, the value of $DEVICE, when interpreted as a truth-value is 0 (FALSE). If the status of the device reflect any error-condition, the value of $DEVICE, when interpreted as a truth-value is 1 (TRUE).

For PIPE :

0 indicates for READ with a zero (0) timeout that available data has been read.

"1,Resource temporarily unavailable" indicates no input available for a READ with a zero (0) timeout.

"1,<error signature>" indicates a read error.

0 indicates for a WRITE that it was successful.

"1,Resource temporarily unavailable" indicates a failure of a WRITE where the pipe is full and the WRITE would block.

This condition also causes an exception.

"1,<error signature>" indicates a write error 

**$KEY**

$K[EY] contains the string that terminated the most recent READ command from the current device (including any introducing and terminating characters). If no READ command is issued to the current device or if no terminator is used, the value of $KEY is an empty string.

For PIPE:

$KEY contains the UNIX process id of the created process shell which executes the command connected to the PIPE.

For more information, refer to “$Key”.

**$ZA**

$ZA contains the status of the last read on the device. The value is a decimal integer with a meaning as follows:

For Terminal I/O:

0: Indicates normal termination of a read operation

1: Indicates a parity error

2: Indicates the terminator sequence was too long

9: Indicates a default for all other errors

For Sequential Disk :

0: Indicates normal termination of a read operation

9: Indicates a failure of a read operation

For FIFO:

0: Indicates normal termination or time out

9: Indicates a failure of a read operation

For SOCKET:

0: Indicates normal termination or time out

9: Indicates failure of a read operation

For PIPE:

0: Indicates normal termination or time out when using READ x:n, where n >0

9: Indicates failure of a READ x or READ x:n, where n>0

9: Indicates failure of a WRITE where the pipe is full and the WRITE would block

.. note::
   $ZA refers to the status of the current device. Therefore, exercise care in sequencing USE commands and references to $ZA.

**$ZB**

$ZB contains a string specifying the input terminator for the last terminal READ. $ZB is null, and it is not maintained for devices other than terminals. $ZB may contain any legal input terminator, such as <CR> (ASCII 13) or an escape sequence starting with <ESC> (ASCII 27), from zero (0) to 15 bytes in length. $ZB is null for any READ terminated by a timeout or any fixed-length READ terminated by input reaching the maximum length.

$ZB contains the actual character string, not a sequence of numeric ASCII codes.

If a device is opened with CHSET set to UTF-8 or UTF-16*, $ZB contains the bad character if one is encountered. This holds true for sockets, sequential files (and thus FIFOs and PIPEs) and terminals.

Example:

.. parsed-literal::
   set zb=$zb for i=1:1:$length(zb) write !,i,?5,$ascii(zb,i)

This example displays the series of ASCII codes for the characters in $ZB.

$ZB refers to the last READ terminator of the current device. Therefore, be careful when sequencing USE commands and references to $ZB. 

**$ZEOF**

$ZEOF contains a truth-valued expression indicating whether the last READ operation reached the end-of-file. $ZEOF is TRUE(1) at EOF and FALSE (0) at other positions. GT.M does not maintain $ZEOF for terminal devices.

$ZEOF refers to the end-of-file status of the current device. Therefore, be careful when sequencing USE commands and references to $ZEOF.

$ZEOF is set for terminals if the connection dropped on read. 

**$ZPIN**

When $PRINCIPAL has different input/output devices, the USE command recognizes intrinsic special variable $ZPIN to apply appropriate deviceparameters to the input side of $PRINCIPAL. A USE with $ZPIN sets $IO to $PRINCIPAL for READs and WRITEs from the input and output side of $PRINCIPAL. $ZSOCKET() also accepts $ZPIN as its first argument and, if the device is a split SOCKET device, supplies information on the input SOCKET device. In any context other than USE or $ZSOCKET(), or if $PRINCIPAL is not a split device, $PRINCIPAL, $ZPIN and $ZPOUT are synonyms. In the case of a split $PRINCIPAL, $ZPIN returns the value of $PRINCIPAL followed by the string "< /" Any attempt to OPEN $ZPIN results in a DEVOPENFAIL error. 

**$ZPOUT**

When $PRINCIPAL has different input/output devices, the USE command recognizes intrinsic special variables $ZPOUT to apply appropriate deviceparameters to the output side of $PRINCIPAL. A USE with $ZPOUT sets $IO to $PRINCIPAL for READs and WRITEs from the input and output side of $PRINCIPAL. $ZSOCKET() also accepts $ZPOUT as its first argument and, ifthe device is a split SOCKET device, supplies information on the output SOCKET device. In any context other than USE or $ZSOCKET(), or if $PRINCIPAL is not a split device, $PRINCIPAL, $ZPIN and $ZPOUT are synonyms. In the case of a split $PRINCIPAL, $ZPOUT returns the value of $PRINCIPAL followed by the string "> /" Any attempt to OPEN $ZPOUT results in a DEVOPENFAIL error.

-------------------
I/O Devices
-------------------

Each device type supported by YottaDB/GT.M responds to a particular subset of deviceparameters, while ignoring others. Devices may be programmed in a device-specific manner, or in a device-independent manner. Device-specific I/O routines are intended for use with only one type of device. Device-independent I/O routines contain appropriate deviceparameters for all devices to be supported by the function, so the user can redirect to a different device output while using the same program.

YottaDB/GT.M supports the following I/O device types:

* Terminals and Printers
* Sequential Disk Files
* FIFOs
* Null Devices
* Socket Devices
* PIPE Devices

++++++++++++++++++++++++
I/O Device Recognition
++++++++++++++++++++++++

YottaDB/GT.M OPEN, USE, and CLOSE commands have an argument expression specifying a device name.

During an OPEN, YottaDB/GT.M attempts to resolve the specified device names to physical names. When YottaDB/GT.M successfully resolves a device name to a physical device, that device becomes the target of the OPEN. If the device name contains a dollar sign ($), YottaDB/GT.M attempts an environment variable translation; the result becomes the name of the device. If it does not find such an environment variable, it assumes that the dollar sign is a part of the filename, and opens a file by that name.

.. note::
   Note: YottaDB/GT.M resolves the device name argument for menemonicspace devices (SOCKET or PIPE) to a arbitrary handle instead of a physical name.

Once a device is OPEN, YottaDB/GT.M establishes an internal correspondence between a name and the device or file. Therefore, while the device is OPEN, changing the translation of an environment variable in the device specification does not change the device.

The following names identify the original $IO for the process:

* $PRINCIPAL
* 0

++++++++++++++++++++++++++++++
Device Specification Defaults
++++++++++++++++++++++++++++++

YottaDB/GT.M uses standard filenames for device specifiers.

The complete format for a filename is:

.. parsed-literal::
   /directory/file

If the expression specifying a device does not contain a complete filename, the expression may start with an environment variable that translates to one or more leading components of the filename. YottaDB/GT.M applies default values for the missing components.

If the specified file is not found, it is created unless READONLY is specified.

The YottaDB/GT.M filename defaults are the following:

Directory: Current working directory

File: No default (user-defined filename)

Filetype: No default (user-defined filetype)

+++++++++++++++++++++++++++++++
How I/O Device Parameters Work
+++++++++++++++++++++++++++++++

I/O deviceparameters either perform actions that cause the device to do something (for example, CLEARSCREEN), or specify characteristics that modify the way the device subsequently behaves (for example, WIDTH). When an I/O command has multiple action deviceparameters, YottaDB/GT.M performs the actions in the order of the deviceparameters within the command argument. When a command has characteristic deviceparameters, the last occurrence of a repeated or conflicting deviceparameter determines the characteristic.

Deviceparameters often relate to a specific device type. YottaDB/GT.M ignores any deviceparameters that do not apply to the type of the device specified by the command argument. Specified device characteristics are in force for the duration of the YottaDB/GT.M image, or until modified by an OPEN, USE, or CLOSE command.

When reopening a device that it previously closed, a YottaDB/GT.M process restores all characteristics not specified on the OPEN to the values the device had when it was last CLOSEd. YottaDB/GT.M treats FIFO, PIPE, and SD differently and uses defaults for unspecified device characteristics on every OPEN (that is, YottaDB/GT.M does not retain devices characteristics on a CLOSE of SD, FIFO, and PIPE).

The ZSHOW command with an argument of "D" displays the current characteristics for all devices OPENed by the process. ZSHOW can direct its output into a YottaDB/GT.M variable. For more information on ZSHOW, refer to “ZSHow”.

+++++++++++++++++++++++++++++++
Abbreviating Device Parameters
+++++++++++++++++++++++++++++++

.. note::
   Most Z* deviceparameters have the same functionality as their counterparts and are supported for compatibility reasons.

YottaDB/GT.M deviceparameters do not have predefined abbreviations. YottaDB/GT.M recognizes deviceparameters using a minimum recognizable prefix technique. Most deviceparameters may be represented by four leading characters, except ERASELINE, all deviceparameters starting with WRITE, and Z* deviceparameters in a mnemonicspace (such as SOCKET). The four leading characters recognized do not include a leading NO for negation.

For compatibility with previous versions, YottaDB/GT.M may recognize certain deviceparameters by abbreviations shorter than the minimum. While it is convenient in Direct Mode to use shorter abbreviations, YottaDB/FIS may add additional deviceparameters, and therefore, recommends all programs use at least four characters. Because YottaDB/GT.M compiles the code, spelling out deviceparameters completely has no performance penalty, except when used with indirection or XECUTEd arguments.

+++++++++++++++++++++++++++
Document Conventions
+++++++++++++++++++++++++++

This chapter uses the following mnemonics to describe when a deviceparameter applies:

TRM: Valid for terminals

SD: Valid for sequential disk files

FIFO: Valid for FIFOs

NULL: Valid for null devices

SOC: Valid for both socket devices (TCP and LOCAL)

SOC(LOCAL): Valid for LOCAL sockets devices

SOC(TCP): Valid for TCP sockets devices

PIPE: Valid for PIPE devices

.. note::
   Lower case "pipe" refers to a UNIX pipe and the upper case "PIPE" to the YottaDB/GT.M device.

Some of the deviceparameter defaults shown are the basic operating system defaults, and may be subject to modification before the invocation of YottaDB/GT.M.

+++++++++++++++++++++++++++++++
Device-Independent Programming
+++++++++++++++++++++++++++++++

When a user may choose a device for I/O, YottaDB/GT.M routines can take one of two basic programming approaches.

* The user selection directs the program into different code branches, each of which handles a different device type.
* The user selection identifies the device. There is a single code path written with a full complement of deviceparameters to handle all selectable device types.

The latter approach is called device-independent programming. To permit device independent programming, YottaDB/GT.M uses the same deviceparameter for all devices that have an equivalent facility, and ignores deviceparameters applied to a device that does not support that facility.

Example:

.. parsed-literal::
   OPEN dev:(EXCE=exc:REWIND:VARIABLE:WRITEONLY)

This example OPENs a device with deviceparameters that affect different devices. The EXCEPTION has an effect for all device types. When dev is a terminal or a null device, YottaDB/GT.M ignores the other deviceparameters. When dev is a sequential file on disk, YottaDB/GT.M uses REWIND and VARIABLE. This command performs a valid OPEN for all the different device types.

------------------------------
Using Terminals
------------------------------

A YottaDB/GT.M process assigns $PRINCIPAL to the UNIX standard input of the process (for READ) and standard output (for WRITE). For a local interactive process, $PRINCIPAL identifies the "terminal" from which the user is signed on.

While all terminals support the CTRAP deviceparameter, only $PRINCIPAL supports CENABLE. While CTRAP allows terminal input to redirect program flow, CENABLE allows the terminal user to invoke the Direct Mode.

Directly connected printers often appear to YottaDB/GT.M as a terminal (although printers generally do not provide input) regardless of whether the printer is connected to the computer with a high speed parallel interface, or an asynchronous terminal controller. 

+++++++++++++++++++++++++++++++++
Setting Terminal Characteristics
+++++++++++++++++++++++++++++++++

YottaDB/GT.M does not isolate its handling of terminal characteristics from the operating system environment at large. YottaDB/GT.M inherits the operating system terminal characteristics in effect at the time the YottaDB/GT.M image is invoked. When YottaDB/GT.M exits, the terminal characteristics known by the operating system are restored.

However, if the process temporarily leaves the YottaDB/GT.M environment with a ZSYSTEM command , YottaDB/GT.M does not recognize any changes to the terminal characteristics left by the external environment. This may cause disparities between the physical behavior of the terminal, and the perceived behavior by YottaDB/GT.M.

UNIX enforces standard device security for explicit OPENs of terminals other than the sign-in terminal ($PRINCIPAL). If you are unable to OPEN a terminal, contact your system manager.

USE of a terminal causes the device driver to flush the output buffer. This feature of the USE command provides routine control over the timing of output, which is occasionally required. However, it also means that redundant USE commands may induce an unnecessary performance penalty. Therefore, YottaDB/FIS recommends restricting USE commands to redirecting I/O, modifying deviceparameters, and initiating specifically required flushes.

The terminal input buffer size is fixed at 1024 on UNIX and a variable read terminates after 1023 characters. 

**Setting the Environment Variable TERM**

The environment variable $TERM must specify a terminfo entry that accurately matches the terminal (or terminal emulator) settings. Refer to the terminfo man pages for more information on the terminal settings of the platform where YottaDB/GT.M needs to run.

Some terminfo entries may seem to work properly but fail to recognize function key sequences or position the cursor properly in response to escape sequences from YottaDB/GT.M. YottaDB/GT.M itself does not have any knowledge of specific terminal control characteristics. Therefore, it is important to specify the right terminfo entry to let YottaDB/GT.M communicate correctly with the terminal. You may need to add new terminfo entries depending on their specific platform and implementation. The terminal (emulator) vendor may also be able to help.

YottaDB/GT.M uses the following terminfo capabilities. The full variable name is followed by the capname in parenthesis:

.. parsed-literal::
   auto_right_margin(am), clr_eos(ed), clr_eol(el), columns(cols), cursor_address(cup), cursor_down(cud1),cursor_left(cub1), cursor_right(cuf1), cursor_up(cuu1), eat_newline_glitch(xenl), key_backspace(kbs), key_dc(kdch1),key_down(kcud1), key_left(kcub1), key_right(kcuf1), key_up(kcuu1), key_insert(kich1), keypad_local(rmkx),keypad_xmit(smkx), lines(lines). 

YottaDB/GT.M sends keypad_xmit before terminal reads for direct mode and READs (other than READ \*) if EDITING is enabled. YottaDB/GT.M sends keypad_local after these terminal reads.

++++++++++++++++++++++++++++++
Logical Records for Terminals
++++++++++++++++++++++++++++++

A logical record for a terminal equates to a line on the physical screen. The WIDTH device characteristic specifies the width of the screen, while the LENGTH device characteristic specifies the number of lines on the screen. 

+++++++++++++++++++++++++++++
Read \* Command for Terminals
+++++++++++++++++++++++++++++

If the terminal has ESCAPE sequencing enabled, and the input contains a valid escape sequence or a terminator character, YottaDB/GT.M stores the entire sequence in $ZB and returns the ASCII representation of the first character.

Example:

.. parsed-literal::
   GTM>kill
   GTM>use $principal:escape
   GTM>read \*x set zb=$zb zwrite
   (Press the F11 key on the VT220 terminal keyboard)
   x=27
   zb=$C(27)_"[23~"

This enters an escape sequence in response to a READ \*. The READ * assigns the code for <ESC> to the variable X. YottaDB/GT.M places the entire escape sequence in $ZB. As some of the characters are not graphic, that is, visible on a terminal, the example transfers the contents of $ZB to the local variable ZB and uses a ZWRITE so that the non-graphic characters appear in $CHAR() format.

When escape processing is disabled, READ \*x returns 27 in x for an <ESC>. If the escape introducer is also a TERMINATOR, $ZB has a string of length one (1), and a value of the $ASCII() representation of the escape introducer; otherwise, $ZB holds the empty string. YottaDB/GT.M stores the remaining characters of the escape sequence in the input stream. A READ command following a READ * command returns the remaining characters of the escape sequence.

Example:

.. parsed-literal::
   GTM>kill
   GTM>use $principal:(noescape:term=$char(13))
   GTM>read \*x set zb=$zb read y:0 zwrite
   (Press the F11 key on the terminal keyboard)
   [23~x=27
   y="[23~"
   zb=""
   GTM>use $principal:noecho read \*x set zb=$zb read y:0 use $principal:echo zwrite
   x=27
   y="[23~"
   zb=""
   GTM>read \*x set zb=$zb use $principal:flush read y:0 zwrite
   x=27
   y=""
   zb=""

While the first READ Y:0 picks up the sequence after the first character, notice how the graphic portion of the sequence appears on the terminal – this is because the READ \*X separated the escape character from the rest of the sequence thus preventing the terminal driver logic from recognizing it as a sequence, and suppressing its echo. The explicit suppression of echo removes this visual artifact. In the case of the final READ \*X, the FLUSH clears the input buffer so that it is empty by the time of the READ Y:0.

++++++++++++++++++++++++++++++++++++
READ X#maxlen Command for Terminals
++++++++++++++++++++++++++++++++++++

Generally, YottaDB/GT.M performs the same maintenance on $ZB for a READ X#maxlen as for a READ. However, if the READ X#maxlen terminates because the input has reached the maximum length, YottaDB/GT.M sets $ZB to null. When the terminal has ESCAPE sequencing enabled, and the input contains an escape sequence, YottaDB/GT.M sets $ZB to contain the escape sequence.

+++++++++++++++++++++++++++++++++
Terminal Deviceparameter Summary
+++++++++++++++++++++++++++++++++

The following tables provide a brief summary of deviceparameters for terminals, grouped into related areas. For detailed information, refer to “Open”, “Use”, and “Close”.

**Error Processing Deviceparameters**

+-----------------------------------------+-----------------------------+------------------------------------------------+
| Device Parameter                        | Command                     | Comment                                        |
+=========================================+=============================+================================================+
| EXCEPTION=expr                          | O/U/C                       | Controls device-specific error handling.       |
+-----------------------------------------+-----------------------------+------------------------------------------------+

**Interaction Management Deviceparameters**

+-------------------------------+--------------------------+---------------------------------------------------------------------------------------------------+
| Device Parameter              | Command                  | Comment                                                                                           |
+===============================+==========================+===================================================================================================+
| [NO]CENABLE                   | U                        | Controls whether <CTRL-C> on $PRINCIPAL causes YottaDB/GT.M to go to direct mode.                 |
+-------------------------------+--------------------------+---------------------------------------------------------------------------------------------------+
| CTRAP=expr                    | U                        | Controls vectoring on trapped <CTRL> characters.                                                  |
+-------------------------------+--------------------------+---------------------------------------------------------------------------------------------------+
| [NO]EDITING                   | U                        | Controls the editing mode for $PRINCIPAL.                                                         |
+-------------------------------+--------------------------+---------------------------------------------------------------------------------------------------+
| [NO]EMPTERM                   | U                        | Control whether an "Erase" character on an empty input line should terminate a READ or READ #     |
|                               |                          | command.                                                                                          |
+-------------------------------+--------------------------+---------------------------------------------------------------------------------------------------+
| [NO]ESCAPE                    | U                        | Controls escape sequence processing.                                                              |
+-------------------------------+--------------------------+---------------------------------------------------------------------------------------------------+
| [NO]INSERT                    | U                        | Controls insert or overstrike on input.                                                           |
+-------------------------------+--------------------------+---------------------------------------------------------------------------------------------------+
| [NO]PASTHRU                   | U                        | Controls interpretation by the operating system of special control characters (for example        |
|                               |                          | <CTRL-B>).                                                                                        |
+-------------------------------+--------------------------+---------------------------------------------------------------------------------------------------+
| [NO]TERMINATOR[=expr]         | U                        | Controls characters that end a READ                                                               |
+-------------------------------+--------------------------+---------------------------------------------------------------------------------------------------+

**Flow Control Deviceparameters**

+-------------------------------+--------------------------+---------------------------------------------------------------------------------------------------+
| Device Parameter              | Command                  | Comment                                                                                           |
+===============================+==========================+===================================================================================================+
| [NO]CONVERT                   | U                        | Controls forcing input to uppercase.                                                              |
+-------------------------------+--------------------------+---------------------------------------------------------------------------------------------------+
| [NO]FILTER                    | U                        | Controls some $X, $Y maintenance.                                                                 |
+-------------------------------+--------------------------+---------------------------------------------------------------------------------------------------+
| FLUSH                         | U                        | Clears the typeahead buffer.                                                                      |
+-------------------------------+--------------------------+---------------------------------------------------------------------------------------------------+
| [NO]HOSTSYNC                  | U                        | Controls host's use of XON/XOFF.                                                                  |
+-------------------------------+--------------------------+---------------------------------------------------------------------------------------------------+
| [NO]READSYNC                  | U                        | Controls wrapping READs in XON/XOFF.                                                              |
+-------------------------------+--------------------------+---------------------------------------------------------------------------------------------------+
| [NO]TTSYNC                    | U                        | Controls input response to XON/XOFF.                                                              |
+-------------------------------+--------------------------+---------------------------------------------------------------------------------------------------+
| [NO]TYPEAHEAD                 | U                        | Controls unsolicited input handling.                                                              |
+-------------------------------+--------------------------+---------------------------------------------------------------------------------------------------+

**Screen Management Deviceparameters**

+-------------------------------+--------------------------+---------------------------------------------------------------------------------------------------+
| Device Parameter              | Command                  | Comment                                                                                           |
+===============================+==========================+===================================================================================================+
| CLEARSCREEN                   | U                        | Clears from cursor to end-of-screen.                                                              |
+-------------------------------+--------------------------+---------------------------------------------------------------------------------------------------+
| DOWNSCROLL                    | U                        | Moves display down one line.                                                                      |
+-------------------------------+--------------------------+---------------------------------------------------------------------------------------------------+
| [NO]ECHO                      | U                        | Controls the host echo of input.                                                                  |
+-------------------------------+--------------------------+---------------------------------------------------------------------------------------------------+
| ERASELINE                     | U                        | Clears from cursor to end-of-line.                                                                |
+-------------------------------+--------------------------+---------------------------------------------------------------------------------------------------+
| [Z]LENGTH=intexpr             | U                        | Controls maximum number of lines on a page ($Y).                                                  |
+-------------------------------+--------------------------+---------------------------------------------------------------------------------------------------+
| UPSCROLL                      | U                        | Moves display up one line.                                                                        |
+-------------------------------+--------------------------+---------------------------------------------------------------------------------------------------+
| [Z]WIDTH=intexpr              | U                        | Controls the maximum width of an output line ($X).                                                |
+-------------------------------+--------------------------+---------------------------------------------------------------------------------------------------+
| [Z][NO]WRAP                   | U                        | Controls handling of output lines longer than the maximum width.                                  |
+-------------------------------+--------------------------+---------------------------------------------------------------------------------------------------+
| X=intexpr                     | U                        | Positions the cursor to column intexpr.                                                           |
+-------------------------------+--------------------------+---------------------------------------------------------------------------------------------------+
| Y=intexpr                     | U                        | Positions the cursor to row intexpr.                                                              |
+-------------------------------+--------------------------+---------------------------------------------------------------------------------------------------+

**O** : Applies to the OPEN command

**U** : Applies to the USE command

**C** : Applies to the CLOSE command

+++++++++++++++++++++
Terminal Examples
+++++++++++++++++++++

This section contains examples of YottaDB/GT.M terminal handling.

Example:

.. parsed-literal::
   use $principal:(exception="zg "_$zl\_":C^MENU")

This example USEs the principal device, and sets up an EXCEPTION handler. When an error occurs, it transfers control to label C in the routine ^MENU at the process stack level where the EXCEPTION was established.

Example:

.. parsed-literal::
   use $principal:(x=0:y=0:clearscreen)

This example positions the cursor to the upper left-hand corner and clears the entire screen.

Example:

.. parsed-literal::
   use $principal:(noecho:width=132:wrap)

This example disables ECHOing, enables automatic WRAPping, and sets the line width to 132 characters.

Note that YottaDB/GT.M enables WRAP automatically when you specify the WIDTH deviceparameter.

Example:

.. parsed-literal::
   use $principal:nocenable

This example disables <CTRL-C>.



