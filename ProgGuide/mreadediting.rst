
.. index::
   M Read Editing for UNIX

=====================================
Appendix B : M Read Editing for UNIX
=====================================

.. contents::
   :depth: 2

-------------------
Overview
-------------------

An editing capability similar to that available at the direct mode prompt has been added to the READ statement when reading from the $PRINCIPAL device if that device is a terminal. In addition to the functions currently available in direct mode, it is possible to select whether characters typed other than at the end of the current input will overwrite existing characters (overwrite mode) or be inserted at the cursor as is the case currently (insert mode). Only the previous input may be recalled during a READ unlike the multiple lines available during direct mode.

The default EDITING and INSERT modes can be specified with an environment variable. Unless these features are enabled, the current functionality, which provides no line editing during M READ and insert mode during direct mode, will be retained.

Like the direct mode line editing functions, the proper functioning of editing for READ depends on setting the TERM environment variable to select a terminfo entry which matches the terminal (or terminal emulator) settings. There are some terminfo entries that may seem to work but which will fail to properly recognize function key sequences or fail to properly position the cursor in response to escape sequences from YottaDB. YottaDB itself does not have any knowledge of specific terminal functions. In some cases, new terminfo entries may need to be added. The terminfo man pages for the specific platform should be consulted for the details if this is needed. The terminal (emulator) manufacturer may be able to help.

------------------
User Interface
------------------

+++++++++++++++++++++++
Editing Functions
+++++++++++++++++++++++

+----------------------------------------------------------+-------------------------------------------------------+---------------------------------------------------------------+
| Editing Function                                         | Key / Character                                       | Direct Mode Difference                                        |
+==========================================================+=======================================================+===============================================================+
| Delete character to left of cursor                       | Delete character [a]_ [b]_ (ASCII 127)                | Backspace character (ASCII 8) is also recognized.             |
+----------------------------------------------------------+-------------------------------------------------------+---------------------------------------------------------------+
| Move left one character                                  | Left arrow key or <ctrl> B character                  | \-                                                            |
+----------------------------------------------------------+-------------------------------------------------------+---------------------------------------------------------------+
| Move right one character                                 | Right arrow key or <ctrl> F character                 | \-                                                            |
+----------------------------------------------------------+-------------------------------------------------------+---------------------------------------------------------------+
| Insert/overstrike toggle within a direct mode line or    | Insert key                                            | Not previously available in direct mode                       |
| READ argument                                            |                                                       |                                                               |
+----------------------------------------------------------+-------------------------------------------------------+---------------------------------------------------------------+
| Recall previous input string                             | Up Arrow key                                          | Multiple lines can be scrolled through using both up and down |
|                                                          |                                                       | arrow keys                                                    |
+----------------------------------------------------------+-------------------------------------------------------+---------------------------------------------------------------+
| Erase entire text to left of cursor                      | <ctrl>-U character (erases entire line)               | \-                                                            |
+----------------------------------------------------------+-------------------------------------------------------+---------------------------------------------------------------+
| Position cursor at start of line                         | <ctrl>-A character                                    | \-                                                            |
+----------------------------------------------------------+-------------------------------------------------------+---------------------------------------------------------------+
| Position cursor at end of line                           | <ctrl>-E character                                    | \-                                                            |
+----------------------------------------------------------+-------------------------------------------------------+---------------------------------------------------------------+
| Deletes all characters from the cursor to the end of the | <ctrl>-K character                                    | \-                                                            |
| line                                                     |                                                       |                                                               |
+----------------------------------------------------------+-------------------------------------------------------+---------------------------------------------------------------+
| Delete character at the cursor position                  | <ctrl>-D character                                    | \-                                                            |
+----------------------------------------------------------+-------------------------------------------------------+---------------------------------------------------------------+

.. [a] The terminal or terminal emulator may remap the backspace key to send the delete character.

.. [b] The actual character recognized for this function depends on the setting of the erase character as shown by "stty -a" which is usually the delete character.

+++++++++++++++++++++++++++++
New USE Device Parameters
+++++++++++++++++++++++++++++

**[NO]EDITING**

Applies to : TRM

When EDITING mode is enabled for the $PRINCIPAL device, the use of the left and right cursor movement keys and certain <CTRL> characters are allowed within the current input line. The last input can be recalled using the up or down arrow key. The editing functions are the same as during direct mode command input as described in the `"Line Editing" section of the "Operating & Debugging in Direct Mode" chapter of the M Programmer's Guide <https://docs.yottadb.com/ProgrammersGuide/opdebug.html#functionality-available-in-direct-mode>`_ except backspace is not treated the same as the erase character from termcap which is usually delete (ASCII 127).

When EDITING mode is enabled, escape sequences do not terminate READs.

Enabling PASTHRU mode supersedes EDITING mode.

NOECHO disables EDITING mode.

If any of the EDITING <CTRL> characters are in the CTRAP list, their editing functions will not be available since CTRAP takes precedence. However the EDITING <CTRL> characters will take precedence over the TERMINATOR list.

By default, EDITING mode is disabled.

.. note::
   M READ EDITING depends on the values of $X and $Y being correct. If the application sends its own escape sequences or control characters, which change the cursor position, it must properly update $X and $Y before doing a M READ with EDITING enabled to ensure correct formatting during input.

**[NO]INSERT**

Applies to : TRM

Enables or disables insert mode for the $PRINCIPAL device. When a terminal has INSERT mode enabled, input characters are inserted at the logical position in the input stream designated by the cursor, for example in the middle of the line/record. When a terminal has INSERT mode disabled, input characters overwrite existing characters in the input stream at the logical position designated by the cursor. The insert mode can be toggled within a direct mode line or if EDITING is enabled, a single READ argument’s input using the terminal’s INSERT key. The INSERT mode is reset to the default or what was last specified with USE at the beginning of each direct mode line or READ argument.

By default, INSERT mode is enabled.

**OVERSTRIKE**

Applies to : TRM

Disables INSERT mode for a terminal. OVERSTRIKE is a synonym for NOINSERT.

By default, INSERT mode is enabled.

++++++++++++++++++++++++++++++++
Modified USE Device Parameters
++++++++++++++++++++++++++++++++

**[NO]ECHO**

Applies to: TRM

Disabling ECHO disables the EDITING functions and any input is not available for later recall.

**[NO]PASTHRU**

Applies to: TRM

PASTHRU supersedes line editing.


+++++++++++++++++++++++++
Environment Variable
+++++++++++++++++++++++++

By defining the environment variable "gtm_principal_editing", the defaults for EDITING and INSERT modes can be changed for the $PRINCIPAL device when it is a terminal. The value of the variable can be [NO]EDITING and/or [NO]INSERT. If both modes are specified they should be separated by a colon (i.e. “:”) and can be in any order.

Examples:

.. parsed-literal::
   gtm_principal_editing=”NOINSERT:EDITING”
   export gtm_principal_editing

   gtm_principal_editing=”EDITING”
   export gtm_principal_editing


+++++++++++++++++++++
Direct Mode Editing
+++++++++++++++++++++

When entering commands at the direct mode prompt, the insert mode can be toggled for that line by using the insert key. When YottaDB starts, insert mode is enabled unless the value of the gtm_principal_editing environment variable includes the string NOINSERT. If insert mode is disabled or enabled for the $PRINCIPAL device by a USE statement before returning to direct mode, it will remain disabled or enabled at direct mode. The insert mode can be toggled within a direct mode line using the terminal’s INSERT key.


+++++++++++++++++++
ZSHOW "D"
+++++++++++++++++++

When EDITING mode is enabled or INSERT mode is disabled, the ZSHOW “D” output for the $PRINCIPAL device will include this information.

Example:

.. parsed-literal::
   YDB>zshow “D”
   /dev/pts/4 OPEN TERMINAL NOPAST NOESCA NOREADS TYPE WIDTH=80 LENG=24 EDIT NOINSE

---------------------------
Typographical Conventions
---------------------------

Command Syntax: UNIX syntax (i.e., lowercase text and "-" for flags/qualifiers) is used throughout this document. 

Reference Number: The reference numbers used to track software enhancements and customer support requests appear in parentheses ( ).

Platform Identifier: If a new feature or software enhancement does not apply to all platforms, the relevant platform or platforms appear in brackets [ ].
