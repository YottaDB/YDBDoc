
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


--------------------------
Using Sequential Files
--------------------------

YottaDB/GT.M provides access to sequential files. These files allow linear access to records. Sequential files are used to create programs, store reports, and to communicate with facilities outside of YottaDB/GT.M.

+++++++++++++++++++++++++++++++++++++++++
Setting Sequential File Characteristics
+++++++++++++++++++++++++++++++++++++++++

The ANSI standard specifies that when a process CLOSEs and then reOPENs a device, YottaDB/GT.M restores any characteristics not explicitly specified with deviceparameters to the values they had prior to the last CLOSE. However, because it is difficult for a large menu-driven application to ensure the previous OPEN state, YottaDB/GT.M always sets unspecified sequential file characteristics to their default value on OPEN. This approach also reduces potential memory overhead imposed by OPENing and CLOSEing a large number of sequential files during the life of a process.

YottaDB/GT.M does not restrict multiple OPEN commands. However, if a file is already open, YottaDB/GT.M ignores attempts to modify sequential file OPEN characteristics, except for RECORDSIZE and for deviceparameters that also exist for USE.

Sequential files can be READONLY, or read/write (NOREADONLY).

Sequential files can be composed of either FIXED or VARIABLE (NOFIXED) length records. By default, records have VARIABLE length.

UNIX enforces its standard security when YottaDB/GT.M OPENs a sequential file. This includes any directory access required to locate or create the file. If you are unable to OPEN a file, contact your system manager.

++++++++++++++++++++++++++++++
Sequential File Pointers
++++++++++++++++++++++++++++++

Sequential file I/O operations use a construct called a file pointer. The file pointer logically identifies the next record to read or write. OPEN commands position the file pointer at the beginning of the file (REWIND) or at the end-of-file (APPEND). APPEND cannot reposition a file currently open. Because the position of each record depends on the previous record, a WRITE destroys the ability to reliably position the file pointer to subsequent records in a file. Therefore, by default (NOTRUNCATE), YottaDB/GT.M permits WRITEs only when the file pointer is positioned at the end of the file.

A file that has been previously created and contains data that should be retained can also be opened with the device parameter APPEND.

If a device has TRUNCATE enabled, a WRITE issued when the file pointer is not at the end of the file causes all contents after the current file pointer to be discarded. This effectively moves the end of the file to the current position and permits the WRITE.

++++++++++++++++++++++++
Line Terminators
++++++++++++++++++++++++

LF ($CHAR(10)) terminates the logical record for all M mode sequential files, TRM, PIPE, and FIFO. For non FIXED format sequential files and terminal devices for which character set is not M, all the standard Unicode line terminators terminate the logical record. These are U+000A (LF), U+0000D (CR), U+000D followed by U+000A (CRLF), U+0085 (NEL), U+000C (FF), U+2028 (LS) and U+2029 (PS). 

++++++++++++++++++++++++
READ/WRITE Operations
++++++++++++++++++++++++

The following table describes all READ and WRITE operations for STREAM, VARIABLE, and FIXED format sequential files having automatic record termination enabled (WRAP) or disabled (NOWRAP).

+-------------------------------+-------------------------------+-------------------------------------------------------------------------------------------------+------------------------------------------------------------------+
| Command                       | WRAP or NOWRAP                | STREAM or VARIABLE format file behavior                                                         | FIXED format file behavior                                       |
+===============================+===============================+=================================================================================================+==================================================================+
| READ format or WRITE or WRITE | WRAP                          | Write the entire argument, but anytime $X is about to exceed WIDTH: insert a <LF> character,    | Similar to VARIABLE but no <LF>                                  |
| \*                            |                               | set $X to 0, increment $Y                                                                       |                                                                  |
+-------------------------------+-------------------------------+-------------------------------------------------------------------------------------------------+------------------------------------------------------------------+
| READ format or WRITE or WRITE | NOWRAP                        | Update $X based on STREAM or VARIABLE format as described below:                                | Same as VARIABLE                                                 |
| \*                            |                               |                                                                                                 |                                                                  |
|                               |                               | STREAM: Write all of the argument with no truncation nor with a line terminator being inserted. |                                                                  |
|                               |                               | Add length of argument to $X.                                                                   |                                                                  |
|                               |                               |                                                                                                 |                                                                  |
|                               |                               | VARIABLE ($X=WIDTH): Write up to WIDTH-$X characters. Write no more output to the device until a|                                                                  |
|                               |                               | WRITE ! or a SET $X makes $X less than WIDTH.                                                   |                                                                  |
+-------------------------------+-------------------------------+-------------------------------------------------------------------------------------------------+------------------------------------------------------------------+
| READ or WRITE !               | either                        | Write <LF>, set $X to 0, increment $Y                                                           | Write PAD bytes to bring the current record to WIDTH             |
+-------------------------------+-------------------------------+-------------------------------------------------------------------------------------------------+------------------------------------------------------------------+
| WRITE #                       | either                        | Write <FF>,<LF>, set $X to 0, increment $Y                                                      | Write PAD bytes to bring the current record to WIDTH, then a <FF>|
|                               |                               |                                                                                                 | followed by WIDTH-1 PAD bytes                                    |
+-------------------------------+-------------------------------+-------------------------------------------------------------------------------------------------+------------------------------------------------------------------+
| CLOSE                         | either                        | After a WRITE, if $X > 0, Write <LF>                                                            | After a WRITE, if $X >0, perform an implicit "WRITE !" adding PAD|
|                               |                               |                                                                                                 | bytes to create a full record. If you need to avoid trailing PAD |
|                               |                               |                                                                                                 | bytes set $X to 0 before closing a FIXED format file.            |
+-------------------------------+-------------------------------+-------------------------------------------------------------------------------------------------+------------------------------------------------------------------+
| READ X                        | either                        | Return characters up to $X=WIDTH, or until encountering an <LF> or EOF. If <LF> encountered, set| Return WIDTH characters; no maintenance of $X and $Y, except that|
|                               |                               | $X to 0, increment $Y                                                                           | EOF increments $Y                                                |
+-------------------------------+-------------------------------+-------------------------------------------------------------------------------------------------+------------------------------------------------------------------+
| READ X#len                    | either                        | Return characters up to the first of $X=WIDTH or len characters, or encountering a <LF> or EOF; | Return MIN(WIDTH, len) characters; no maintenance of $X and $Y,  |
|                               |                               | if up to len characters or EOF update $X, otherwise set $X to 0 and increment $Y                | except that EOF increments $Y                                    |
+-------------------------------+-------------------------------+-------------------------------------------------------------------------------------------------+------------------------------------------------------------------+
| READ \*X                      | either                        | Return the code for one character and increment $X, if WIDTH=$X or <LF> encountered, set $X=0,  | Return the code for one character, if EOF return -1; no          |
|                               |                               | increment $Y; if EOF return -1                                                                  | maintenance of $X and $Y, except that EOF increments $Y          |
+-------------------------------+-------------------------------+-------------------------------------------------------------------------------------------------+------------------------------------------------------------------+

**Notes**

* EOF == end-of-file; <FF>== ASCII form feed; <LF> == ASCII line feed; 
* In M mode, and by default in UTF-8 mode PAD == <SP> == ASCII space.
* "READ format" in this table means READ ? or READ <strlit>
* A change to WIDTH implicitly sets WRAP unless NOWRAP follows in the deviceparameter list
* In VARIABLE and STREAM mode, READ (except for READ \*) never returns <LF> characters
* In M mode, the last setting of RECORDSIZE or WIDTH for the device determines WIDTH
* In M Mode, a WRITE to a sequential device after setting $X to a value greater than the device WIDTH or a reducing WIDTH to less than the current $X acts as if the first character caused $X to exceed the WIDTH induces an immediate WRAP, if WRAP is enabled
* In UTF-8 mode, RECORDSIZE is in bytes and WIDTH is in characters and the smaller acts as the WIDTH limit in the table.
* In UTF-8 mode, FIXED mode writes <SP> to the RECORDSIZE when the next character won't fit.
* In UTF-8 mode, all READ forms do not return trailing  PAD characters.
* In UTF-8 mode, all characters returned by all forms of FIXED mode READ are from a single record. 
* WRITE for a Sequential Disk (SD) device works at the current file position, whether attained with APPEND, REWIND or SEEK.
* YottaDB/GT.M manages any BOM for UTF mode files by ensuring they are at the beginning of the file and produces a BOMMISMATCH error for an attempt to change the byte-ordering on OPEN for an existing file.
* An attempt to OPEN a non-zero length file WRITEONLY without either NEWVERSION or TRUNCATE in UTF mode produces an OPENDEVFAIL due to the fact that any existing BOM information cannot be verified.
* Note that with YottaDB/GT.M SD encryption, because of the state information associated with encryption processing, encrypted files require the file to be WRITEn or READ from the beginning rather than from an arbitrary position. 


++++++++++++++++++++++++++++
Writing Binary Files
++++++++++++++++++++++++++++

To write a binary data file, open it with FIXED:WRAP:CHSET="M" and set $X to zero before the WRITE to avoid filling the last record with spaces (the default PAD byte value). 

.. note::
   With CHSET not "M", FIXED has a different definition. Each record is really the same number of bytes as specified by RECORDSIZE. Padding bytes are added as needed to each record.

Example:

.. parsed-literal::
   bincpy(inname,outname); YottaDB/GT.M routine to do a binary copy from file named in argument 1 to file named in argument 2
           ;
     new adj,nrec,rsize,x
     new $etrap
     set $ecode="",$etrap="goto error",$zstatus=""
     set rsize=32767                          ; max recordsize that keeps $X on track
     open inname:(readonly:fixed:recordsize=rsize:exception="goto eof")
     open outname:(newversion:stream:nowrap:chset="M")
     for nrec=1:1 use inname read x use outname write x
   eof     
     if $zstatus["IOEOF" do  quit
     . set $ecode=""
     . close inname
     . use outname
     . set adj=$x
     . set $x=0 close outname
     . write !,"Copied ",$select((nrec-1)<adj:adj,1:((nrec-1)*rsize)+adj)," bytes from ",inname," to ",outname
     else  use $principal write !,"Error with file ",inname,":"
  error   
     write !,$zstatus
     close inname,outname
     quit


++++++++++++++++++++++++++++++++++++++++
Sequential File Deviceparameter Summary
++++++++++++++++++++++++++++++++++++++++

The following tables provide a brief summary of deviceparameters for sequential files grouped into related areas. For more detailed information, refer to “Open”, “Use”, and “Close”.

**Error Processing Deviceparameters**

+-----------------------------------+--------------------------+---------------------------------------------------------------------+
| Deviceparameter                   | Command                  | Comment                                                             |
+===================================+==========================+=====================================================================+
| EXCEPTION=expr                    | O/U/C                    | Controls device-specific error handling.                            |
+-----------------------------------+--------------------------+---------------------------------------------------------------------+

**File Pointer Positioning Deviceparameters**

+-----------------------------------+--------------------------+------------------------------------------------------------------------------------------------------------------------------------------------------+
| Deviceparameter                   | Command                  | Comment                                                                                                                                              |
+===================================+==========================+======================================================================================================================================================+
| APPEND                            | O                        | Positions file pointer at EOF.                                                                                                                       |
+-----------------------------------+--------------------------+------------------------------------------------------------------------------------------------------------------------------------------------------+
| REWIND                            | O/U/C                    | Positions file pointer at start of the file.                                                                                                         |
+-----------------------------------+--------------------------+------------------------------------------------------------------------------------------------------------------------------------------------------+
| SEEK=strexpr                      | O/U                      | Positions the current file pointer to the location specified in strexpr. The format of strexpr is a string of the form "[+|-]integer" where unsigned |
|                                   |                          | value specifies an offset from the beginning of the file, and an explicitly signed value specifies an offset relative to the current file position.  |
|                                   |                          | For STREAM or VARIABLE format, the positive intexpr after any sign is a byte offset, while for a FIXED format, it is a record offset. In order to    |
|                                   |                          | deal with the possible presence of a Byte Order Marker (BOM), SEEK for a FIXED format file written in a UTF character set must follow at least one   |
|                                   |                          | prior READ since the device was created.                                                                                                             |
+-----------------------------------+--------------------------+------------------------------------------------------------------------------------------------------------------------------------------------------+

**File Format Deviceparameters**

+-----------------------------------+--------------------------+------------------------------------------------------------------------------------------------------------------------------------------------------+
| Deviceparameter                   | Command                  | Comment                                                                                                                                              |
+===================================+==========================+======================================================================================================================================================+
| [NO]FIXED                         | O                        | Controls whether records have fixed length.                                                                                                          |
+-----------------------------------+--------------------------+------------------------------------------------------------------------------------------------------------------------------------------------------+
| [Z]LENGTH=intexpr                 | U                        | Controls virtual page length.                                                                                                                        |
+-----------------------------------+--------------------------+------------------------------------------------------------------------------------------------------------------------------------------------------+
| RECORDSIZE=intexpr                | O                        | Specifies maximum record size.                                                                                                                       |
+-----------------------------------+--------------------------+------------------------------------------------------------------------------------------------------------------------------------------------------+
| STREAM                            | O                        | Specifies the STREAM format.                                                                                                                         |
+-----------------------------------+--------------------------+------------------------------------------------------------------------------------------------------------------------------------------------------+
| VARIABLE                          | O                        | Controls whether records have variable length.                                                                                                       |
+-----------------------------------+--------------------------+------------------------------------------------------------------------------------------------------------------------------------------------------+
| [Z]WIDTH=intexpr                  | U                        | Controls maximum width of an output line.                                                                                                            |
+-----------------------------------+--------------------------+------------------------------------------------------------------------------------------------------------------------------------------------------+
| [Z][NO]WRAP                       | O/U                      | Controls handling of records longer than device width.                                                                                               |
+-----------------------------------+--------------------------+------------------------------------------------------------------------------------------------------------------------------------------------------+

**File Access Deviceparameters**

+-----------------------------------+--------------------------+------------------------------------------------------------------------------------------------------------------------------------------------------+
| Deviceparameter                   | Command                  | Comment                                                                                                                                              |
+===================================+==========================+======================================================================================================================================================+
| DELETE                            | C                        | Specifies file be deleted by CLOSE.                                                                                                                  |
+-----------------------------------+--------------------------+------------------------------------------------------------------------------------------------------------------------------------------------------+
| GROUP=expr                        | O/C                      | Specifies file permissions for other users in the owner's group.                                                                                     |
+-----------------------------------+--------------------------+------------------------------------------------------------------------------------------------------------------------------------------------------+
| NEWVERSION                        | O                        | Specifies YottaDB/GT.M create a new version of file.                                                                                                 |
+-----------------------------------+--------------------------+------------------------------------------------------------------------------------------------------------------------------------------------------+
| OWNER=expr                        | O/C                      | Specifies file permissions for the owner of file.                                                                                                    |
+-----------------------------------+--------------------------+------------------------------------------------------------------------------------------------------------------------------------------------------+
| [NO]READONLY                      | O                        | Controls read-only file access.                                                                                                                      |
+-----------------------------------+--------------------------+------------------------------------------------------------------------------------------------------------------------------------------------------+
| RENAME=expr                       | C                        | Specifies CLOSE replace name of a disk file with name specified by expression.                                                                       |
+-----------------------------------+--------------------------+------------------------------------------------------------------------------------------------------------------------------------------------------+
| SYSTEM=expr                       | O/C                      | Specifies file permissions for the owner of the file (same as OWNER).                                                                                |
+-----------------------------------+--------------------------+------------------------------------------------------------------------------------------------------------------------------------------------------+
| [NO]TRUNCATE                      | O/U                      | Controls overwriting of existing data in file.                                                                                                       |
+-----------------------------------+--------------------------+------------------------------------------------------------------------------------------------------------------------------------------------------+
| UIC=expr                          | O/C                      | Specifies file's owner ID.                                                                                                                           |
+-----------------------------------+--------------------------+------------------------------------------------------------------------------------------------------------------------------------------------------+
| WORLD=expr                        | O/C                      | Specifies file permissions for users not in the owner's group.                                                                                       |
+-----------------------------------+--------------------------+------------------------------------------------------------------------------------------------------------------------------------------------------+

O: Applies to the OPEN command

U: Applies to the USE command

C: Applies to the CLOSE command

+++++++++++++++++++++++++++
Sequential File Examples
+++++++++++++++++++++++++++

This section contains a few brief examples of YottaDB/GT.M sequential file handling.

Example:

.. parsed-literal::
   GTM>do ^FREAD
   FREAD;
    zprint ^FREAD 
    read "File > ",sd
    set retry=0
    set $ztrap="BADAGAIN"
    open sd:(readonly:exception="do BADOPEN")
    use sd:exception="goto EOF"
    for  use sd read x use $principal write x,!
   EOF;
    if '$zeof zmessage +$zstatus
    close sd
    quit
   BADOPEN;
    set retry=retry+1 
    if retry=2 open sd
    if retry=4 halt
    if $piece($zstatus,",",1)=2 do  
    . write !,"The file ",sd," does not exist. Retrying in about 2 seconds ..."
    . hang 2.1
    . quit 
    if $piece($zstatus,",",1)=13 do  
    . write !,"The file ",sd," is not accessible. Retrying in about 3 seconds ..."
    . hang 3.1
    . quit
    quit
   BADAGAIN;
    w !,"BADAGAIN",!
                           
  File >

This example asks for the name of the file and displays its contents. It OPENs that file as READONLY and specifies an EXCEPTION. The exception handler for the OPEN deals with file-not-found and file-access errors and retries the OPEN command on error. The first USE sets the EXCEPTION to handle end-of-file. The FOR loop reads the file one record at a time and transfers each record to the principal device. The GOTO in the EXCEPTION terminates the FOR loop. At label EOF, if $ZEOF is false, the code reissues the error that triggered the exception. Otherwise, the CLOSE releases the file.

Example:

.. parsed-literal::
   GTM>do ^formatACCT
   formatACCT;
    zprint ^formatACCT; 
    set sd="temp.dat",acct=""
    open sd:newversion 
    use sd:width=132
    for  set acct=$order(^ACCT(acct)) quit:acct=""  do  
    . set rec=$$FORMAT(acct)
    . write:$y>55 #,hdr write !,rec
    close sd
    quit

This OPENs a NEWVERSION of file temp.dat. The FOR loop cycles through the ^ACCT global formatting (not shown in this code fragment) lines and writing them to the file. The FOR loop uses the argumentless DO construct to break a long line of code into more manageable blocks. The program writes a header record (set up in initialization and not shown in this code fragment) every 55 lines, because that is the application page length, allowing for top and bottom margins.


------------------------
FIFO Characteristics
------------------------

FIFOs have most of the same characteristics as other sequential files, except that READs and WRITEs can occur in any order.

The following characteristics of FIFO behavior may be helpful in using them effectively.

With READ:

* If a READ is done while there is no data in the FIFO: 
* The process hangs until data is put into the FIFO by another process, or the READ times out, when a timeout is specified.

The following table shows the result and the values of I/O status variables for different types of READ operations on a FIFO device. 

+-------------------------+-----------------------------------------+----------------------------------+----------------------------+-------------------+------------------+------------------+
| Operation               | Result                                  | $DEVICE                          | $ZA                        | $TEST             | X                | $ZEOF            |
+=========================+=========================================+==================================+============================+===================+==================+==================+
| READ X:n                | Normal Termination                      | 0                                | 0                          | 1                 | DATA READ        | 0                |
+-------------------------+-----------------------------------------+----------------------------------+----------------------------+-------------------+------------------+------------------+
| READ X:n                | Timeout with no data read               | 0                                | 0                          | 0                 | empty string     | 0                |
+-------------------------+-----------------------------------------+----------------------------------+----------------------------+-------------------+------------------+------------------+
| READ X:n                | Timeout with partial data read          | 0                                | 0                          | 0                 | partial data     | 0                |
+-------------------------+-----------------------------------------+----------------------------------+----------------------------+-------------------+------------------+------------------+
| READ X:n                | End of File                             | 1,Device detected EOF            | 9                          | 1                 | empty string     | 1                |
+-------------------------+-----------------------------------------+----------------------------------+----------------------------+-------------------+------------------+------------------+
| READ X:0                | Normal Termination                      | 0                                | 0                          | 1                 | DATA READ        | 0                |
+-------------------------+-----------------------------------------+----------------------------------+----------------------------+-------------------+------------------+------------------+
| READ X:0                | No data available                       | 0                                | 0                          | 0                 | empty string     | 0                |
+-------------------------+-----------------------------------------+----------------------------------+----------------------------+-------------------+------------------+------------------+
| READ X:0                | Timeout with partial data read          | 0                                | 0                          | 0                 | Partial data     | 0                | 
+-------------------------+-----------------------------------------+----------------------------------+----------------------------+-------------------+------------------+------------------+
| READ X:0                | End of File                             | 1,Device detected EOF            | 9                          | 1                 | empty string     | 1                |
+-------------------------+-----------------------------------------+----------------------------------+----------------------------+-------------------+------------------+------------------+
| READ X                  | Error                                   | 1,<error signature>              | 9                          | n/c               | empty string     | 0                |
+-------------------------+-----------------------------------------+----------------------------------+----------------------------+-------------------+------------------+------------------+

With WRITE:

* The FIFO device does non-blocking writes. If a process tries to WRITE to a full FIFO and the WRITE would block, the device implicitly tries to complete the operation up to a default of 10 times. If the gtm_non_blocked_write_retries environment variable is defined, this overrides the default number of retries. If the retries do not succeed (remain blocked), the WRITE sets $DEVICE to "1,Resource temporarily unavailable", $ZA to 9, and produces an error. If the YottaDB/GT.M process has defined an EXCEPTION, $ETRAP or $ZTRAP, the error trap may choose to retry the WRITE after some action or delay that might remove data from the FIFO device.
* While it is hung, the process will not respond to <CTRL-C>.

With CLOSE:

* The FIFO is not deleted unless the DELETE qualifier is specified.
* If a process closes the FIFO with the DELETE qualifier, the FIFO becomes unavailable to new users at that time.
* All processes currently USEing the FIFO may continue to use it, until the last process attached to it CLOSES it, and is destroyed.
* Any process OPENing a FIFO with the same name as a deleted FIFO creates a new one to which subsequent OPENs attach.
* The default access permissions on a FIFO are the same as the mask settings of the process that created the FIFO. Use the SYSTEM, GROUP, WORLD, and UIC deviceparameters to specify FIFO access permissions. File permissions have no affect on a process that already has the FIFO open. 

++++++++++++++++++++++++++++++++++++++
Considerations in implementing FIFOs
++++++++++++++++++++++++++++++++++++++

As you establish FIFOs for interprocess communication, consider whether, and how, the following issues will be addressed:

* Do READs occur immediately, or can the process wait?
* Are timed READs useful to avoid system hangs and provide a way to remove the process?
* Does the WRITE process need to know whether the READ data was received?
* Will there be multiple processes READing and WRITEing into a single FIFO?

+++++++++++++++++++++++++++++
Error Handling for FIFOs
+++++++++++++++++++++++++++++

Deleting devices (or files) created by an OPEN which has an error has deeper implications when that device, especially a FIFO, serves as a means of communications between a two processes. If one process OPENs a FIFO device for WRITE, there is an interval during which another process can OPEN the same device for READ. During that interval the writer process can encounter an error (for example, an invalid parameter) causing YottaDB/GT.M to delete the device, but the reader process can complete its OPEN successfully. This sequence results in a process with an orphaned device open for READ. Any other process that OPENs the same device for WRITE creates a new instance of it, so the reader can never find data to READ from the orphaned device. Since YottaDB/GT.M has insufficient context to enforce process synchronization between reader and writer, the application must use appropriate communication protocols and error handling techniques to provide synchronization between processes using files and FIFOs for communication.

+++++++++++++++++++++++++++++++++
YottaDB/GT.M Recognition of FIFOs
+++++++++++++++++++++++++++++++++

Like a sequential file, the path of a FIFO is specified as an argument expression to the OPEN, USE, and CLOSE commands. A device OPENed with a FIFO deviceparameter becomes a FIFO unless another device of that name is already OPEN. In that case, OPENing a device that has previously been OPENed by another process as a FIFO causes the process (the process here is the process trying to open the FIFO) to attach to the existing FIFO.

.. note::
   If an existing named pipe (aka fifo special file) is OPENed even without specifying the FIFO deviceparameter, it is treated as if FIFO had been specified.

+++++++++++++++++++++++++++++
FIFO Device Examples
+++++++++++++++++++++++++++++

The following two examples represent a master/slave arrangement where the slave waits in a read state on the FIFO until the master sends it some data that it then processes.

Example:

.. parsed-literal::
   set x="named.pipe"
   open x:fifo
   do getres
   use x write res,!

This routine opens the FIFO, performs its own processing which includes starting the slave process (not shown in this code fragment).

Example:

.. parsed-literal::
   set x="named.pipe"
   open x:fifo
   use x read res
   do process(res)

This routine waits for information from the master process, then begins processing.

+++++++++++++++++++++++++++++++++
FIFO Deviceparameter Summary
+++++++++++++++++++++++++++++++++

The following table summarizes the deviceparameters that can be used with FIFOs.

**File Format Deviceparameters**

+---------------------------+-------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------+
| Deviceparameter           | Command                       | Description                                                                                                                             |
+===========================+===============================+=========================================================================================================================================+
| [NO]FIXED                 | O                             | Controls whether records have fixed length.                                                                                             |
+---------------------------+-------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------+
| [Z]LENGTH=intexpr         | U                             | Controls the virtual page length.                                                                                                       |
+---------------------------+-------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------+
| RECORDSIZE=intexpr        | O                             | Specifies the maximum record size                                                                                                       |
+---------------------------+-------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------+
| VARIABLE                  | O                             | Controls whether records have variable length.                                                                                          |
+---------------------------+-------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------+
| [Z]WIDTH=intexpr          | U                             | Sets the device's logical record size and enables WRAP.                                                                                 |
+---------------------------+-------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------+
| [Z][NO]WRAP               | O/U                           | Controls the handling of records longer than the device width.                                                                          |
+---------------------------+-------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------+

**File Access Deviceparameters**

+---------------------------+-------------------------------+------------------------------------------------------------------------------------------------------------------------------------------+
| Deviceparameter           | Command                       | Description                                                                                                                              |
+===========================+===============================+==========================================================================================================================================+
| DELETE                    | C                             | Specifies that the FIFO should be deleted when the last user closes it. If specified on an OPEN, DELETE is activated only at the time of |
|                           |                               | the close. No new attachments are allowed to a deleted FIFO and any new attempt to use a FIFO with the name of the deleted device creates|
|                           |                               | a new device.                                                                                                                            |
+---------------------------+-------------------------------+------------------------------------------------------------------------------------------------------------------------------------------+
| GROUP=expr                | O/C                           | Specifies file permissions for other users in owner's group.                                                                             |
+---------------------------+-------------------------------+------------------------------------------------------------------------------------------------------------------------------------------+
| [NO]READONLY              | O                             | OPENs a device for reading only (READONLY) or reading and writing (NOREADONLY).                                                          |
+---------------------------+-------------------------------+------------------------------------------------------------------------------------------------------------------------------------------+
| OWNER=expr                | O/C                           | Specifies file permissions for owner of file.                                                                                            |
+---------------------------+-------------------------------+------------------------------------------------------------------------------------------------------------------------------------------+
| RENAME=expr               | C                             | Specifies that CLOSE replace the name of a disk file with the name specified by the expression.                                          |
+---------------------------+-------------------------------+------------------------------------------------------------------------------------------------------------------------------------------+
| SYSTEM=expr               | O/C                           | Specifies file permissions for owner of file (same as OWNER).                                                                            |
+---------------------------+-------------------------------+------------------------------------------------------------------------------------------------------------------------------------------+
| UIC=expr                  | O/C                           | Specifies the file's owner ID.                                                                                                           |
+---------------------------+-------------------------------+------------------------------------------------------------------------------------------------------------------------------------------+
| WORLD=expr                | O/C                           | Specifies file permissions for users not in the owner's group.                                                                           |
+---------------------------+-------------------------------+------------------------------------------------------------------------------------------------------------------------------------------+

-----------------------------------
Using NULL Devices
-----------------------------------

Null devices comprise of a collection of system purpose devices that include /dev/null, /dev/zero, /dev/random, and /dev/urandom.

* /dev/null returns a null string on READ and sets $ZEOF
* /dev/random and /dev/urandom return a random value on READ and set $ZEOF
* /dev/zero returns 0's on READ and does not set $ZEOF

A null device discards all output. YottaDB/GT.M maintains a virtual cursor position for null devices as it does for terminals on output. Use null devices for program testing and debugging, or for jobs that permit I/O to be discarded under certain circumstances. For example, JOB processes must have input and output devices associated with them, even though they do not use them. Null devices are low overhead never-fail alternatives for certain classes of I/O.

++++++++++++++++++++++++++++
NULL Deviceparameter Summary
++++++++++++++++++++++++++++

The following table provides a brief summary of deviceparameters for null devices. For more detailed information, refer to “Open”, “Use”, and “Close”.

+----------------------------+------------------------------+---------------------------------------------------------------------------------------------------------------------------------------------+
| Deviceparameter            | Command                      | Comment                                                                                                                                     |
+============================+==============================+=============================================================================================================================================+
| EXCEPTION=expr             | O/U/C                        | Controls device-specified error handling. For the null device this is only EOF handling and therefore exceptions can never be invoked except|
|                            |                              | by a READ.                                                                                                                                  |
+----------------------------+------------------------------+---------------------------------------------------------------------------------------------------------------------------------------------+
| [NO]FILTER[=expr]          | U                            | Controls some $X,$Y maintenance.                                                                                                            |
+----------------------------+------------------------------+---------------------------------------------------------------------------------------------------------------------------------------------+
| [Z]LENGTH=intexpr          | U                            | Controls the length of the virtual page.                                                                                                    |
+----------------------------+------------------------------+---------------------------------------------------------------------------------------------------------------------------------------------+
| [Z]WIDTH=intexpr           | U                            | Controls maximum size of a record.                                                                                                          |
+----------------------------+------------------------------+---------------------------------------------------------------------------------------------------------------------------------------------+
| [Z][NO]WRAP                | O/U                          | Controls handling of records longer than the maximum width.                                                                                 |
+----------------------------+------------------------------+---------------------------------------------------------------------------------------------------------------------------------------------+
| X=intexpr                  | U                            | Sets $X to intexpr.                                                                                                                         |
+----------------------------+------------------------------+---------------------------------------------------------------------------------------------------------------------------------------------+
| Y=intexpr                  | U                            | Sets $Y to intexpr.                                                                                                                         |
+----------------------------+------------------------------+---------------------------------------------------------------------------------------------------------------------------------------------+

O: Applies to the OPEN command

U: Applies to the USE command

C: Applies to the CLOSE command

++++++++++++++++++++++++
NULL Device Examples
++++++++++++++++++++++++

This section contains examples of null device usage.

Example:

.. parsed-literal::
   GTM>do ^runrep
   runrep;
    zprint ^runrep
    set dev="/dev/null"
    set hdr="********* REPORT HEADER ************"
    open dev use dev
    set x="" write hdr,!,$zdate($horolog),?30,$job,!
    for  set x=$order(^tmp($job,x)) quit:x=""  do REPORT
    quit
   REPORT;
    ;large amount of code
    quit;

This program produces a report derived from the information in the global variable ^tmp. The unspecified routine REPORT may potentially contain a large amount of code. To see that the basic program functions without error, the programmer may discard the output involved in favor of watching the function. To run the program normally, the programmer simply has to change the variable dev to name another device and the routine REPORT writes to the dev device.

Example:

.. parsed-literal::
   job ^X:(in="/dev/null":out="/dev/null":err="error.log")
   JOB ^X:(IN="/dev/null":OUT="/dev/null":ERR="error.log") 

This example issues a YottaDB/GT.M JOB command to execute the routine ^X in another process. This routine processes a large number of global variables and produces no output. In the example, the JOBbed process takes its input from a null device, and sends its output to a null device. If the JOBbed process encounters an error, it directs the error message to error.log.

---------------------------
Using PIPE Devices
---------------------------

A PIPE device is used to access and manipulate the input and/or output of a shell command as a YottaDB/GT.M I/O device. YottaDB/GT.M maintains I/O status variables for a PIPE device just as it does for other devices. An OPEN of the device starts a sub-process. Data written to the device by the M program is available to the process on its STDIN. The M program can read the STDOUT and STDERR of the sub-process. This facilitates output only applications, such as printing directly from a YottaDB/GT.M program to an lp command; input only applications, such as reading the output of a command such as ps; and co-processing applications, such as using iconv to convert data from one encoding to another.

A PIPE is akin to a FIFO device. Both FIFO and PIPE map YottaDB/GT.M devices to UNIX pipes, the conceptual difference being that whereas a FIFO device specifies a named pipe, but does not specify the process on the other end of the pipe, a PIPE device specifies a process to communicate with, but the pipes are unnamed. Specifically, an OPEN of a PIPE creates a subprocess with which the YottaDB/GT.M process communicates.

A PIPE device is specified with a "PIPE" value for mnemonicspace on an OPEN command. 

.. note::
   YottaDB/GT.M ignores the mnemonicspace specification on an OPEN of a previously OPEN device and leaves the existing device with its original characteristics.

++++++++++++++++++++++++
Modes of PIPE Operation
++++++++++++++++++++++++


