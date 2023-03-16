.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2018-2023 YottaDB LLC and/or its subsidiaries.#
.. # All rights reserved.                                        #
.. #                                                             #
.. #     This document contains the intellectual property        #
.. #     of its copyright holder(s), and is made available       #
.. #     under a license.  If you do not know the terms of       #
.. #     the license, please stop and do not read further.       #
.. #                                                             #
.. ###############################################################

.. index::
   Utility Routines

======================
10. Utility Routines
======================

.. contents::
   :depth: 5

YottaDB provides library utilities to perform frequently used tasks, and to access frequently used information. Most of the utilities are for YottaDB programmers, but some provide tools for system administration and operation.

The YottaDB utilities fall into the following general categories:

* Date and time utilities
* Conversion utilities
* Mathematic utilities
* Global utilities
* Routine utilities
* Internationalization utilities
* System Management utilities
* Unicode Utility Routines
* Miscellaneous utilities

The YottaDB distribution includes the source files for these utilities. The default installation compiles them to produce object modules in the $ydb_dist distribution library.

You may wish to examine the utilities and include some of them in your programs if the programs access the function frequently or you may want to modify the utilities to better fit your particular needs. If you modify a utility, store your copy in a directory that precedes ydb_dist in the search list $ZROUTINES to prevent a new release of YottaDB from overwriting your copy.

-------------------------------
Using the Utilities
-------------------------------

You can either use a utility in Direct Mode or include it in a source application program with one or more of the following formats.

* DO ^%UTILITYNAME
* DO LABEL^%UTILITYNAME
* $$FUNC^%UTILITYNAME[(para1,...)]

Many utilities contain labels that invoke variations of the basic utility functionality. Some also provide the label FUNC to invoke an extrinsic function with optional or required parameters.

YottaDB passes input to non-extrinsic forms of the utilities interactively or by using "input" variables. YottaDB passes output from non-extrinsic forms of the utilities using "output" variables. For extrinsic entry points, the utilities receive input as parameters and pass output as the returned result. For other entry points, YottaDB uses predefined "input" and "output" variables to pass information. Some utilities interactively request user inputs and display their results. Each utility is described individually in this chapter where appropriate labels, input, and output variables are identified.

By convention, the utilities use upper-case variables for external input and output. Since M is case-sensitive, when an invocation uses a lower-case or misspelled variable name, the routine does not output the expected information. Instead, it supplies a default value, if one exists, or produces an error message.

Example:

.. code-block:: bash

   YDB>SET %ds="11/22/2018"
   YDB>DO INT^%DATE
   YDB>ZWRITE
   %DN=62047
   %ds="11/22/2018"

This example sets the lowercase variable %ds to the date 11/22/2018. Since the %DATE routine expects the input to be provided in the uppercase %DS variable, it returns a default value in the output variable $DN. The default is the $HOROLOG format of the current date, which is 11/17/2018 in the example.

.. note::
   Utility programs written in M (such as %GO) run within yottadb processes and behave like any other code written in M. Encryption keys are required if the yottadb process accesses encrypted databases. A process running a utility program written in M that does not access encrypted databases (such as %RSEL) does not need encryption keys just to run the utility program.

-------------------------
Date and Time Utilities
-------------------------

The date and time utilities are:

%D: Displays the current date using the [d]d-mmm-[yy]yy format.

%DATE: Converts input date to the $HOROLOG format.

%H: Converts date and time to and from $HOROLOG format.

%T: Displays the current time in [h]h:mm AM/PM format.

%TI: Converts time to $HOROLOG format.

%TO: Converts the current time from $HOROLOG format to [h]h:mm AM/PM format.

The "%" sign has been removed from the topic headings below, intentionally.

The Intrinsic Special Variable $ZDATEFORM interprets year inputs with two digits as described in the following table:

+-------------------------------+------------------------------------------------------------------------------+---------------------------------------+
| $ZDATEFORM                    | INTERPRETATION OF 2 DIGIT YEAR                                               | OUTPUT OF %D                          |
+===============================+==============================================================================+=======================================+
| 0:                            | 20th century (1900 - 1999)                                                   | 2 digits                              |
+-------------------------------+------------------------------------------------------------------------------+---------------------------------------+
| 1:                            | current century (2000 - 2099)                                                | 4 digits                              |
+-------------------------------+------------------------------------------------------------------------------+---------------------------------------+
| (1841-9999):                  | the next 99 years starting from $ZDATEFORM (x - x+99)                        | 4 digits                              |
+-------------------------------+------------------------------------------------------------------------------+---------------------------------------+
| other:                        | current century (2000 - 2099)                                                | 4 digits                              |
+-------------------------------+------------------------------------------------------------------------------+---------------------------------------+

Example:

If $ZDATEFORM is 1965, an input year of 70 would be interpreted as 1970, whereas an input year of 10 would be taken as 2010.

.. _d-util:

+++
%D
+++

The %D utility displays the current date using the [d]d-mmm-[yy]yy format. If a routine uses this function repetitively, put the utility code directly into the M program.

~~~~~~~~~~~~~~~~
Utility Labels
~~~~~~~~~~~~~~~~

INT: Sets variable %DAT to current date.

FUNC[()]: Invokes an extrinsic function returning today's date.

~~~~~~~~~~~~~~~~
Output Variables
~~~~~~~~~~~~~~~~

%DAT: Contains the current date.

~~~~~~~~~~~~~~~~
Examples of %D
~~~~~~~~~~~~~~~~

For the following examples, $ZDATEFORM is assumed to be one (1).

Example:

.. code-block:: bash

   YDB>DO ^%D
   08-FEB-2018

This example invokes %D in Direct Mode. Then %D displays the current date.

Example:

.. code-block:: bash

   YDB>DO INT^%D
   YDB>ZWRITE
   %DAT="08-FEB-2018"

This example invokes %D with the label INT (INT^%D). The variable %DAT contains the current date. ZWRITE displays the contents of the output variable.

Example:

.. code-block:: bash

   YDB>WRITE $$FUNC^%D
   08-FEB-2018

This example invokes %D as an extrinsic function with the label FUNC. $$FUNC^%D returns today's date.

.. _date-util:

++++++++++++
%DATE
++++++++++++

The %DATE utility converts an input date to the $HOROLOG format. The $HOROLOG format represents time as the number of days since December 31, 1840. The routine has entry points for interactive or non-interactive use.

~~~~~~~~~~~~~~~~
Utility Labels
~~~~~~~~~~~~~~~~

INT: Converts %DS input non-interactively (if defined), otherwise the current date.

FUNC(t): Invokes an extrinsic function returning $HOROLOG format of the argument.

~~~~~~~~~
Prompts
~~~~~~~~~

Date: Interactively requests a date for conversion to $HOROLOG format.

~~~~~~~~~~~~~~~~~
Input Variables
~~~~~~~~~~~~~~~~~

%DS: Contains input date; refer to %DATE Input Formats table.

~~~~~~~~~~~~~~~~~~
Output Variables
~~~~~~~~~~~~~~~~~~

%DN: Contains output date in $HOROLOG format

~~~~~~~~~~~~~~~~~~~~~~~~~~
DATE Input Formats Table
~~~~~~~~~~~~~~~~~~~~~~~~~~

+----------------------+------------------------------------------------------------------------------------+------------------------------------+
| Element              | Description                                                                        | Examples                           |
+======================+====================================================================================+====================================+
| DAYS                 | 1 or 2 digits                                                                      | 1,01,24                            |
+----------------------+------------------------------------------------------------------------------------+------------------------------------+
| MONTHS               | 1 or 2 digits                                                                      | 3,03,12                            |
+----------------------+------------------------------------------------------------------------------------+------------------------------------+
|                      | Abbreviations accepted                                                             | MAR                                |
+----------------------+------------------------------------------------------------------------------------+------------------------------------+
|                      | Numeric months precede days                                                        | 1/5 is 5 Jan                       |
+----------------------+------------------------------------------------------------------------------------+------------------------------------+
|                      | Alpha months may precede or follow days                                            | 3 MAR or MAR 3                     |
+----------------------+------------------------------------------------------------------------------------+------------------------------------+
| YEARS                | 2 or 4 digits                                                                      | 11/22/98 or 11/22/2002             |
+----------------------+------------------------------------------------------------------------------------+------------------------------------+
|                      | A missing year defaults to current year                                            | 11/22                              |
+----------------------+------------------------------------------------------------------------------------+------------------------------------+
| TODAY                | Abbreviation Accepted                                                              | T[ODAY]                            |
+----------------------+------------------------------------------------------------------------------------+------------------------------------+
|                      | t+/- N. no. of days                                                                | t+1 or t-3                         |
+----------------------+------------------------------------------------------------------------------------+------------------------------------+
| TOMORROW             | Abbreviation Accepted                                                              | TOM[ORROW]                         |
+----------------------+------------------------------------------------------------------------------------+------------------------------------+
| YESTERDAY            | Abbreviation Accepted                                                              | Y[ESTERDAY]                        |
+----------------------+------------------------------------------------------------------------------------+------------------------------------+
| NULL INPUT           | Defaults to Today                                                                  |                                    |
+----------------------+------------------------------------------------------------------------------------+------------------------------------+
| DELIMITERS           | All non-alphanumeric character(s) except the + or - offset                         | 11/22/98 or 11 NOV 98 or 22 Nov,   |
|                      |                                                                                    | 2002 or 11-22-2002                 |
+----------------------+------------------------------------------------------------------------------------+------------------------------------+

~~~~~~~~~~~~~~~~~~~
Examples of %DATE
~~~~~~~~~~~~~~~~~~~

Example:

.. code-block:: bash

   YDB>DO ^%DATE
   Date:
   YDB>ZWRITE
   %DN=62047

This example invokes %DATE at the YDB> prompt. After pressing <RETURN> at the Date: prompt, %DATE converts today's date (for example, 02/08/2018) to the $HOROLOG format. ZWRITE displays the contents of the output variable.

Example:

.. code-block:: bash

   YDB>DO INT^%DATE
   YDB>ZWRITE
   %DN=59105

This example invokes INT^%DATE, which converts the current date non-interactively into $HOROLOG format. ZWRITE displays the contents of the output variable.

Example:

.. code-block:: bash

   YDB>SET %DS="02/08/2018"
   YDB>DO INT^%DATE
   YDB>ZWRITE
   %DN=62019
   %DS="02/08/2018"

This example sets the input variable %DS prior to invoking INT^%DATE, which converts that date non-interactively to $HOROLOG format.

Example:

.. code-block:: bash

   YDB>WRITE $$FUNC^%DATE("02/08/2018")
   62010

This example invokes %DATE with the label FUNC as an extrinsic function to convert an input date to $HOROLOG. If the invocation does not supply a date for $$FUNC^%DATE, FUNC converts the current date.

Example:

.. code-block:: bash

   YDB>WRITE $ZDATEFORM
   1975
   YDB>WRITE $$FUNC^%DATE("10/20/80")
   51062
   YDB>WRITE $ZDATE(51062)
   10/20/1980
   YDB>WRITE $$FUNC^%DATE("10/20/10")
   62019
   YDB>WRITE $ZDATE(62019)
   10/20/2010

This example shows the use of a year limit in $ZDATEFORM. Two digit years are interpreted to be in the interval (1975, 2074) since $ZDATEFORM is 1975; the input year "80" is interpreted as the year "1980" and "10" is interpreted as the year "2010". The example invokes FUNC^%DATE to convert the input date to $HOROLOG format. $ZDATE() is used to convert the $HOROLOG format date to mm/dd/yyyy format.

.. _h-util:

+++++++++
%H
+++++++++

The %H utility converts date and time to and from $HOROLOG format.

~~~~~~~~~~~~~~~~
Utility Labels
~~~~~~~~~~~~~~~~

%CDS: Converts %DT $HOROLOG input date to mm/dd/yyyy format.

%CTS: Converts %TM $HOROLOG input time to external format.

%CDN: Converts %DT input date to $HOROLOG format.

%CTN: Converts %TM input time to $HOROLOG format.

CDS(dt): Extrinsic entry that converts the $HOROLOG argument to external date format.

CTS(tm): Extrinsic entry that converts the $HOROLOG argument to external time format.

CDN(dt): Extrinsic entry that converts the argument to $HOROLOG format.

CTN(tm): Extrinsic entry that converts the argument to $HOROLOG format.

~~~~~~~~~~~~~~~~~
Input Variables
~~~~~~~~~~~~~~~~~

%DT: Contains input date in either $HOROLOG or mm/dd/[yy]yy format, depending on the format expected by the utility entry point.

%TM: Contains input time in either $HOROLOG or [h]h:mm:ss format, depending on the format expected by the utility entry point.

~~~~~~~~~~~~~~~~~~
Output Variables
~~~~~~~~~~~~~~~~~~

%DAT: Contains converted output date

%TIM: Contains converted output time

~~~~~~~~~~~~~~~~
Examples of %H
~~~~~~~~~~~~~~~~

Example:

.. code-block:: bash

   YDB>SET %DT=+$H DO %CDS^%H
   YDB>ZWRITE
   %DAT="10/20/2010"
   %DT=62047

This example sets %DT to the current date in $HOROLOG format and converts it to mm/dd/yyyy format by invoking %H at the label %CDS. %H returns the converted date in the variable %DAT. ZWRITE displays the contents of the variables.

Example:

.. code-block:: bash

   YDB>SET %DT="10/20/2002" DO %CDN^%H
   YDB>ZWRITE
   %DAT=59097
   %DT="10/20/2002"

This example sets the variable %DT to a date in mm/dd/yyyy format and invokes %H at the label %CDN. %H returns the converted date in the variable %DAT. ZWRITE displays the contents of the variables.

Example:

.. code-block:: bash

   YDB>SET %TM=$P($H,",",2) DO %CTS^%H
   YDB>ZWRITE
   %TIM="17:41:18"
   %TM=63678

This example sets the variable %TM to the current time in $HOROLOG format using a $PIECE() function to return only those digits of the $HOROLOG string that represent the time. The example then invokes %H at the label %CTS. %H returns the converted time in the variable %TIM. ZWRITE displays the contents of the variables.

Example:

.. code-block:: bash

   YDB>SET %TM="17:41:18" DO %CTN^%H
   YDB>ZWRITE
   %TIM=63678
   %TM="17:41:18"

This example sets the variable %TM to a time in hh:mm:ss format, and invokes %H at the label %CTN. %H returns the converted time in the variable %TIM. ZWRITE displays the contents of the variables.

Example:

.. code-block:: bash

   YDB>WRITE $$CDS^%H(62019)
   11/17/2010

This invokes CDS^%H as an extrinsic function to convert the external argument to external date format.

Example:

.. code-block:: bash

   YDB>WRITE $ZDATEFORM
   1980
   YDB>WRITE $$CDN^%H("10/20/02")
   59097
   YDB>WRITE $ZDATE(59097)
   10/20/2002
   YDB>WRITE $$CDN^%H("10/20/92")
   55445
   YDB>WRITE $ZDATE(55445)
   10/20/1992

This example shows the use of a year limit in $ZDATEFORM. Two digit years are interpreted to be in the interval of 1980 - 2079; since $ZDATEFORM is 1980, the input year "02" is interpreted as "2002" and "92" is interpreted as "1992". This example invokes CDN^%H to convert the argument in mm/dd/yy format to $HOROLOG format. $ZDATE() is used to convert the $HOROLOG format date to mm/dd/yyyy format.

.. _t-util:

++++
%T
++++

The %T utility displays the current time in [h]h:mm AM/PM. If a routine uses this function repetitively, put the utility code directly into the M program.

~~~~~~~~~~~~~~~~
Utility Labels
~~~~~~~~~~~~~~~~

INT: Sets %TIM to current time in [h]h:mm AM/PM format.

FUNC[()]: Invokes an extrinsic function returning the current time.

~~~~~~~~~~~~~~~~~~
Output Variables
~~~~~~~~~~~~~~~~~~

%TIM: Contains current time in [h]h:mm AM/PM format.

~~~~~~~~~~~~~~~~
Examples of %T
~~~~~~~~~~~~~~~~

Example:

.. code-block:: bash

   YDB>DO ^%T
   8:30 AM

This example invokes %T, which prints the current time and does not set %TIM.

Example:

.. code-block:: bash

   YDB>DO INT^%T
   YDB>ZWRITE
   %TIM="8:30 AM"

This example invokes INT^%T, which sets the variable %TIM to the current time. ZWRITE displays the contents of the variable.

Example:

.. code-block:: bash

   YDB>WRITE $$FUNC^%T
   8:30 AM

This example invokes FUNC as an extrinsic function, which returns the current time.

.. _ti-util:

+++++
%TI
+++++

The %TI utility converts time to $HOROLOG format. The $HOROLOG format represents time as the number of seconds since midnight. %TI returns the converted time in the variable %TN. The routine has entry points for interactive or non-interactive use.

~~~~~~~~~~~~~~~~
Utility Labels
~~~~~~~~~~~~~~~~

INT Non-interactively converts %TS to $HOROLOG format; if %TS is not defined, then current time is converted.

FUNC[(ts)] Invokes an extrinsic function returning $HOROLOG format of the argument, or if no argument, the $HOROLOG format of the current time.

~~~~~~~~~
Prompts
~~~~~~~~~

Time: Requests time in [h]h:mm:ss format to convert to $HOROLOG format.

~~~~~~~~~~~~~~~~~
Input Variables
~~~~~~~~~~~~~~~~~

%TS Contains input time.

The following table summarizes input formats accepted by %TI.

~~~~~~~~~~~~~~~~~~~
%TI Input Formats
~~~~~~~~~~~~~~~~~~~

+-----------------------------+--------------------------------------------------------------+----------------------------------------+
| Element                     | Description                                                  | Examples                               |
+=============================+==============================================================+========================================+
| HOURS                       | 1 or 2 digits                                                | 3,03,12                                |
+-----------------------------+--------------------------------------------------------------+----------------------------------------+
| MINUTES                     | 2 digits                                                     | 05,36                                  |
+-----------------------------+--------------------------------------------------------------+----------------------------------------+
| AM or PM                    | AM or PM required                                            | 9:00 AM/am or 9:00 PM/pm               |
+-----------------------------+--------------------------------------------------------------+----------------------------------------+
|                             | Abbreviation accepted                                        | 9:00 A/a or 9:00 P/p                   |
+-----------------------------+--------------------------------------------------------------+----------------------------------------+
| NOON                        | Abbreviation accepted                                        | N[OON]                                 |
+-----------------------------+--------------------------------------------------------------+----------------------------------------+
| MIDNIGHT or MIDNITE         | Abbreviation accepted                                        | M[IDNIGHT]/m[idnight] or               |
|                             |                                                              | M[IDNITE]/m[idnite]                    |
+-----------------------------+--------------------------------------------------------------+----------------------------------------+
| MILITARY                    | No punctuation (hhmm)                                        | 1900, 0830                             |
+-----------------------------+--------------------------------------------------------------+----------------------------------------+
| NULL INPUT                  | Defaults to current time                                     |                                        |
+-----------------------------+--------------------------------------------------------------+----------------------------------------+
| DELIMITERS                  | Colon between hours and minutes                              | 3:00                                   |
+-----------------------------+--------------------------------------------------------------+----------------------------------------+

~~~~~~~~~~~~~~~~~~
Output Variables
~~~~~~~~~~~~~~~~~~
%TN: Contains output time in $HOROLOG format

~~~~~~~~~~~~~~~~~
Examples of %TI
~~~~~~~~~~~~~~~~~

Example:

.. code-block:: bash

   YDB>DO ^%TI
   Time: 4:02 PM
   YDB>ZWRITE
   %TN=57720

This example invokes %TI, which prompts for an input time. Press <RETURN> to convert the current time. ZWRITE displays the contents of the output variable.

Example:

.. code-block:: bash

   YDB>ZWRITE
   YDB>DO INT^%TI
   YDB>ZWRITE
   %TN=40954

This example invokes INT^%TI to convert the current time non-interactively. ZWRITE displays the contents of the output variable %TN.

Example:

.. code-block:: bash

   YDB>SET %TS="8:30AM"
   YDB>DO INT^%TI
   YDB>ZWRITE
   %TN=30600
   %TS="8:30AM"

This example sets the variable %TS prior to invoking INT^%TI. %TI uses %TS as the input time. ZWRITE displays the contents of the variables.

Example:

.. code-block:: bash

   YDB>WRITE $$FUNC^%TI("8:30AM")
   30600

This example invokes %TI as an extrinsic function to convert the supplied time to $HOROLOG format. If there is no argument (i.e., $$FUNC^%TI), %TI converts the current time.

.. _to-util:

++++
%TO
++++

The %TO utility converts the input time from $HOROLOG format to [h]h:mm AM/PM format. Put the utility code directly into the M program if the routine uses this function repetitively.

~~~~~~~~~~~~~~~~
Utility Labels
~~~~~~~~~~~~~~~~

INT: Converts non-interactively %TS or if %TS is not defined, the current time to [h]h:mm AM/PM format.

~~~~~~~~~~~~~~~~~
Input Variables
~~~~~~~~~~~~~~~~~

%TN: Contains input time in $HOROLOG format.

~~~~~~~~~~~~~~~~~~
Output Variables
~~~~~~~~~~~~~~~~~~

%TS: Contains output time in [h]h:mm AM/PM format.

~~~~~~~~~~~~~~~~~
Examples of %TO
~~~~~~~~~~~~~~~~~

Example:

.. code-block:: bash

   YDB>DO INT^%TI,^%TO
   YDB>ZWRITE
   %TN=62074
   %TS="5:14 PM"

This example invokes INT^%TI to set %TN to the current time and invokes %TO to convert the time contained in %TN to the [h]h:mm AM/PM format. %TO returns the converted time in the variable %TS. ZWRITE displays the contents of the variables.

-------------------------
Conversion Utilities
-------------------------

The conversion utilities are:

%DH: Decimal to hexadecimal conversion.

%DO: Decimal to octal conversion.

%HD: Hexadecimal to decimal conversion.

%HO: Hexadecimal to octal conversion.

%LCASE: Converts a string to all lower case.

%OD: Octal to decimal conversion.

%OH: Octal to hexadecimal conversion.

%UCASE: Converts a string to all upper case.

The conversion utilities can be invoked as extrinsic functions.

.. note::

   All base conversion utility programs (%DH, %DO, %HD, %HO, %OD, and %OH) convert input until the first invalid character and then ignore the remaining characters.

.. _dh-util:

++++++++++++++
%DH
++++++++++++++

The %DH utility converts numeric values from decimal to hexadecimal. %DH defaults the length of its output to eight digits. However, the input variable %DL overrides the default and controls the length of the output. The routine has entry points for interactive or non-interactive use.

~~~~~~~~~~~~~~~~
Utility Labels
~~~~~~~~~~~~~~~~

INT: Converts interactively entered decimal number to hexadecimal number with the number of digits specified.

FUNC(d[,l]): Invokes %DH as an extrinsic function returning the hexadecimal equivalent of the argument.

~~~~~~~~~~~~~~~~~
Input Variables
~~~~~~~~~~~~~~~~~

%DH: As input, contains input decimal number.

%DL: Specifies how many digits appear in the output, defaults to eight.

~~~~~~~~~
Prompts
~~~~~~~~~

Decimal: Requests a decimal number for conversion to hexadecimal.

Digits: Requests the length of the output in digits; eight by default.

~~~~~~~~~~~~~~~~~~
Output Variables
~~~~~~~~~~~~~~~~~~

%DH: As output, contains the converted number in hexadecimal.

~~~~~~~~~~~~~~~~~
Examples of %DH
~~~~~~~~~~~~~~~~~

Example:

.. code-block:: bash

   YDB>DO INT^%DH
   Decimal: 12
   Digits: 1
   YDB>ZWRITE
   %DH="C"

This example invokes %DH interactively with INT^%DH. %DH prompts for a decimal number and output length, then returns the result in the variable %DH. ZWRITE displays the contents of the variables.

Example:

.. code-block:: bash

   YDB>SET %DH=12
   YDB>DO ^%DH
   YDB>ZWRITE
   %DH="0000000C"
   %DL=8

This example sets the read-write variable %DH to 12 and invokes %DH to convert the number to a hexadecimal number. Because the number of digits was not specified, %DH used the default of 8 digits. Set %DL to specify the number of output digits.

Example:

.. code-block:: bash

   YDB>WRITE $$FUNC^%DH(12,4)
   000C

This example invokes %DH as an extrinsic function using the FUNC label. The first argument specifies the input decimal number and the optional, second argument specifies the number of output digits. If the extrinsic does not have a second argument, the length of the output defaults to eight characters.

.. _do-util:

+++++
%DO
+++++

The %DO utility converts numeric values from decimal to octal. The default length of its output is 12 digits. The value assigned to the input variable %DL overrides the default and controls the length of the output. The routine has entry points for interactive or non-interactive use.

~~~~~~~~~~~~~~~~
Utility Labels
~~~~~~~~~~~~~~~~

INT: Converts the specified decimal number to an octal number with the specified number of digits, interactively.

FUNC(d[,ln]): Invokes %DO as an extrinsic function, returning the octal equivalent of the argument.

~~~~~~~~~
Prompts
~~~~~~~~~

Decimal: Requests a decimal number for conversion to octal.

Digits: Requests the length of the output in digits; 12 by default.

~~~~~~~~~~~~~~~~~
Input Variables
~~~~~~~~~~~~~~~~~

%DO: As input, contains input decimal number.

%DL: Specifies the number of digits in the output, defaults to 12.

~~~~~~~~~~~~~~~~~~
Output Variables
~~~~~~~~~~~~~~~~~~

%DO: As output, contains the converted number in octal.

~~~~~~~~~~~~~~~~~
Examples of %DO
~~~~~~~~~~~~~~~~~

Example:

.. code-block:: bash

   YDB>DO INT^%DO
   Decimal: 12
   Digits: 4
   YDB>ZWRITE
   %DO="0014"

This example invokes %DO interactively with INT^%DO. %DO prompts for a decimal number and an output length. If the output value of %DO has leading zeros, the value is a string. ZWRITE displays the contents of the variables.

Example:

.. code-block:: bash

   YDB>SET %DO=12
   YDB>DO ^%DO
   YDB>ZWRITE
   %DO="000000000014"

This example sets the read-write variable %DO to 12 and invokes %DO to convert the number non-interactively. Because the number of digits was not specified, %DO used the default of 12 digits. Set %DL to specify the number of output digits. ZWRITE displays the contents of the variables.

Example:

.. code-block:: bash

   YDB>WRITE $$FUNC^%DO(12,7)
   0000014

This example invokes %DO as an extrinsic function with the label FUNC. The first argument specifies the number to be converted and the optional, second argument specifies the number of output digits. If the second argument is not specified, %DO uses the default of 12 digits.

.. _hd-util:

++++
%HD
++++

The %HD utility converts numeric values from hexadecimal to decimal, accepting strings starting with a case independent :code:`"0x"`. %HD returns the decimal number in the read-write variable %HD. %HD rejects input numbers beginning with a minus (-) sign and returns null (""). The routine has entry points for interactive or non-interactive use.

~~~~~~~~~~~~~~~~
Utility Labels
~~~~~~~~~~~~~~~~

INT: Converts hexadecimal number entered interactively to decimal number.

FUNC(h): Invokes %HD as an extrinsic function returning the decimal equivalent of the argument.

~~~~~~~~~
Prompts
~~~~~~~~~

Hexadecimal: Requests a hexadecimal number for conversion to decimal.

~~~~~~~~~~~~~~~~~
Input Variables
~~~~~~~~~~~~~~~~~

%HD: As input, contains input hexadecimal number.

~~~~~~~~~~~~~~~~~~
Output Variables
~~~~~~~~~~~~~~~~~~

%HD: As output, contains the converted number in decimal.

~~~~~~~~~~~~~~~~~
Examples of %HD
~~~~~~~~~~~~~~~~~

Example:

.. code-block:: bash

   YDB>DO INT^%HD
   Hexadecimal:E
   YDB>ZWRITE
   %HD=14

This example invokes %HD in interactive mode with INT^%HD. %HD prompts for a hexadecimal number, then returns the converted number in the variable %HD. ZWRITE displays the contents of the variable.

Example:

.. code-block:: bash

   YDB>SET %HD="E"
   YDB>DO ^%HD
   YDB>ZWRITE
   %HD=14

This example sets the read-write variable %HD to "E" and invokes %HD to convert non-interactively the value of %HD to a decimal number. %HD places the converted value into the read-write variable %HD.

Example:

.. code-block:: bash

   YDB>WRITE $$FUNC^%HD("E")
   14

This example invokes %HD as an extrinsic function with the label FUNC and writes the results.

.. _ho-util:

++++
%HO
++++

The %HO utility converts numeric values from hexadecimal to octal, accepting strings starting with a case independent :code:`"0x"`. %HO returns the octal number in the read-write variable %HO. %HO rejects input numbers beginning with a minus (-) sign and returns null (""). The routine has entry points for interactive or non-interactive use.

~~~~~~~~~~~~~~~~
Utility Labels
~~~~~~~~~~~~~~~~

INT: Converts hexadecimal number entered interactively to octal number.

FUNC(h): Invokes %HO as an extrinsic function returning the octal equivalent of the argument.

**Prompts**

Hexadecimal: Requests a hexadecimal number for conversion to octal.

**Input Variables**

%HO: As input, contains input hexadecimal number.

**Output Variables**

%HO: As input, contains input hexadecimal number.

**Examples of %HO**

Example:

.. code-block:: bash

   YDB>DO INT^%HO
   Hexadecimal:C3
   YDB>ZWRITE
   %HO=303

This example invokes %HO in interactive mode using INT^%HO. %HO prompts for a hexadecimal number that it converts to an octal number. ZWRITE displays the contents of the variable.

Example:

.. code-block:: bash

   YDB>SET %HO="C3"
   YDB>DO ^%HO
   YDB>ZWRITE
   %HO=303

This example sets the read-write variable %HO to "C3" and invokes %HO to convert the value of %HO non-interactively. ZWRITE displays the contents of the variable.

Example:

.. code-block:: bash

   YDB>WRITE $$FUNC^%HO("C3")
   303

This example invokes %HO as an extrinsic function with the FUNC label.

.. _lcase-util:

+++++++++
%LCASE
+++++++++

The %LCASE utility converts a string to all lower-case letters. If a routine uses this function repetitively, put the utility code directly into the M program.

~~~~~~~~~~~~~~~~
Utility Labels
~~~~~~~~~~~~~~~~

INT: Converts interactively a string to lower-case.

FUNC(s): Invokes %LCASE as an extrinsic function returning the lower-case form of the argument.

~~~~~~~~~
Prompts
~~~~~~~~~

String: Requests a string for conversion to lower case.

~~~~~~~~~~~~~~~~~
Input Variables
~~~~~~~~~~~~~~~~~

%S: As input, contains string to be converted to lower case.

~~~~~~~~~~~~~~~~~~
Output Variables
~~~~~~~~~~~~~~~~~~

%S: As output, contains the converted string in lower case.

~~~~~~~~~~~~~~~~~~~~
Examples of %LCASE
~~~~~~~~~~~~~~~~~~~~

Example:

.. code-block:: bash

   YDB>DO INT^%LCASE
   String: LABEL
   Lower: label

This example invokes %LCASE in interactive mode using INT^%LCASE. %LCASE prompts for a string that it converts to all lower case.

Example:

.. code-block:: bash

   YDB>SET %S="Hello"
   YDB>do ^%LCASE
   YDB>zwrite
   %S="hello"

This example sets the variable %S to the string "Hello" and invokes %LCASE non-interactively to convert the string.

Example:

.. code-block:: bash

   YDB>SET ^X="Hello"
   YDB>WRITE $$FUNC^%LCASE(^X)
   hello

This example sets the variable ^X to the string "Hello" and invokes %LCASE as an extrinsic function that returns "hello" in lower case.

.. _od-util:

+++++
%OD
+++++

The %OD utility converts numeric values from octal to decimal. %OD returns the decimal number in the read-write variable %OD. %OD rejects input numbers beginning with a minus (-) sign and returns null (""). The routine has entry points for interactive or non-interactive use.

~~~~~~~~~~~~~~~~
Utility Labels
~~~~~~~~~~~~~~~~

INT: Converts octal number entered interactively to decimal number.

FUNC(oct): Invokes %OD as an extrinsic function returning the decimal equivalent of the argument.

~~~~~~~~~
Prompts
~~~~~~~~~

Octal: Requests an octal number for conversion to decimal.

~~~~~~~~~~~~~~~~~
Input Variables
~~~~~~~~~~~~~~~~~

%OD: As input, contains input octal number.

~~~~~~~~~~~~~~~~~~
Output Variables
~~~~~~~~~~~~~~~~~~

%OD: As output, contains the converted number in decimal.

~~~~~~~~~~~~~~~~~
Examples of %OD
~~~~~~~~~~~~~~~~~

Example:

.. code-block:: bash

   YDB>DO INT^%OD
   Octal:14
   YDB>ZWRITE
   %OD=12

This example invokes INT^%OD to interactively convert the octal number entered. %OD prompts for an octal number that it converts to a decimal. %OD returns the converted value in the variable %OD.

Example:

.. code-block:: bash

   YDB>SET %OD=14
   YDB>DO ^%OD
   YDB>ZWRITE
   %OD=12

This example sets the read-write variable %OD to 14 and invokes %OD to convert the number non-interactively. ZWRITE displays the contents of the variables.

Example:

.. code-block:: bash

   YDB>WRITE $$FUNC^%OD(14)
   12

This example invokes %OD as an extrinsic function with the FUNC label. The argument specifies the number to be converted.

.. _oh-util:

+++++
%OH
+++++

The %OH utility converts numeric values from octal to hexadecimal. %OH returns the hexadecimal number in the read-write variable %OH. %OH rejects input numbers beginning with a minus (-) sign. The routine has entry points for interactive or non-interactive use. In interactive mode, %OH rejects non-octal numbers with the following message, "Input must be an octal number". In non-interactive mode, %OH returns a null string ("") upon encountering a non-octal number.

~~~~~~~~~~~~~~~~
Utility Labels
~~~~~~~~~~~~~~~~

INT: Converts interactively octal number entered to hexadecimal number.

FUNC(oct): Invokes %OH as an extrinsic function returning the hexadecimal equivalent of the argument.

~~~~~~~~~
Prompts
~~~~~~~~~

Octal: Requests an octal number for conversion to hexadecimal.

~~~~~~~~~~~~~~~~~
Input Variables
~~~~~~~~~~~~~~~~~

%OH: As input, contains input octal number.

~~~~~~~~~~~~~~~~~~
Output Variables
~~~~~~~~~~~~~~~~~~

%OH: As output, contains the converted number in hexadecimal.

~~~~~~~~~~~~~~~~~
Examples of %OH
~~~~~~~~~~~~~~~~~

Example:

.. code-block:: bash

   YDB>DO INT^%OH
   Octal:16
   YDB>ZWRITE
   %OH="E"

This example invokes %OH in interactive mode using INT^%OH. %OH prompts for an octal number that it converts to a hexadecimal number. ZWRITE displays the contents of the variable.

Example:

.. code-block:: bash

   YDB>SET %OH=16
   YDB>DO ^%OH
   YDB>ZWRITE
   %OH="E"

This example sets the read-write variable %OH to 16 and invokes %OH to convert the value of %OH non-interactively. ZWRITE displays the contents of the variable.

Example:

.. code-block:: bash

   YDB>WRITE $$FUNC^%OH(16)
   E

This example invokes %OH as an extrinsic function with the FUNC label.

.. _ucase-util:

++++++++++++++
%UCASE
++++++++++++++

The %UCASE utility converts a string to all upper-case letters. If a routine uses this function repetitively, put the utility code directly into the M program.

~~~~~~~~~~~~~~~~
Utility Labels
~~~~~~~~~~~~~~~~

INT: Converts a string to upper case interactively.

FUNC(s): Invokes %UCASE as an extrinsic function, returning the upper-case form of the argument.

~~~~~~~~~
Prompts
~~~~~~~~~

String: Requests a string for conversion to upper case.

~~~~~~~~~~~~~~~~~
Input Variables
~~~~~~~~~~~~~~~~~

%S: As input, contains string to be converted to upper case.

~~~~~~~~~~~~~~~~~~
Output Variables
~~~~~~~~~~~~~~~~~~

%S: As output, contains the converted string in upper case.

~~~~~~~~~~~~~~~~~~~~
Examples of %UCASE
~~~~~~~~~~~~~~~~~~~~

Example:

.. code-block:: bash

   YDB>DO INT^%UCASE
   String: test
   Upper: TEST

This example invokes %UCASE in interactive mode using INT^%UCASE. %UCASE prompts for a string that it converts to all upper case.

Example:

.. code-block:: bash

   YDB>SET ^X="hello"
   YDB>WRITE $$FUNC^%UCASE(^X)
   HELLO

This example sets the variable X to the string "hello" and invokes %UCASE as an extrinsic function that returns "HELLO" in upper case.

---------------------------
Mathematic Utilities
---------------------------

The mathematic utilities are:

%EXP: Raises one number to the power of another number.

%SQROOT: Calculates the square root of a number.

The mathematic utilities can be invoked as extrinsic functions.

.. _exp-util:

++++++++
%EXP
++++++++

The %EXP utility raises one number provided to the power of another number provided. While this utility provides an interactive interface for exponential calculations, most production code would perform inline calculation with the "**" operator. The routine has entry points for interactive or non-interactive use.

~~~~~~~~~~~~~~~~
Utility Labels
~~~~~~~~~~~~~~~~

INT: Calculates a number to the power of another number interactively.

FUNC(i,j): Invokes %EXP as an extrinsic function returning the first argument raised to the power of the second argument.

~~~~~~~~~
Prompts
~~~~~~~~~

Power: Requests an exponent or power.

Number: Requests a base number to raise by the power.

~~~~~~~~~~~~~~~~~
Input Variables
~~~~~~~~~~~~~~~~~

%I: As input, contains number to be raised to a power.

%J: Contains exponential power by which to raise %I.

~~~~~~~~~~~~~~~~~~
Output Variables
~~~~~~~~~~~~~~~~~~

%I: As output, contains the result of the exponential calculation.

~~~~~~~~~~~~~~~~~~
Examples of %EXP
~~~~~~~~~~~~~~~~~~

Example:

.. code-block:: bash

   YDB>DO INT^%EXP
   Power: 3
   Number: 12
   12 raised to 3 is 1728

This example invokes %EXP in interactive mode using INT^%EXP. %EXP prompts for an exponent (power) and a base number.

Example:

.. code-block:: bash

   YDB>SET %I=2,%J=9
   YDB>DO ^%EXP
   YDB>ZWRITE
   %I=512
   %J=9

This example sets the read-write variable %I to 2, variable %J to 9, and invokes %EXP to calculate the result. ZWRITE displays the contents of the variables. %I contains the result.

Example:

.. code-block:: bash

   YDB>WRITE $$FUNC^%EXP(2,9)
   512

This example invokes %EXP as an extrinsic function with the label FUNC.

.. _sqroot-util:

++++++++++++++
%SQROOT
++++++++++++++

The %SQROOT utility calculates the square root of a number provided. While this utility provides an interactive interface for taking square roots, most production code would perform inline calculation by raising a number to the .5 power (n**.5). The routine has entry points for interactive or non-interactive use.

~~~~~~~~~~~~~~~~
Utility Labels
~~~~~~~~~~~~~~~~

INT: Calculates the square root of a number interactively.

FUNC(s): Invokes %SQROOT as an extrinsic function returning the square root of the argument.

~~~~~~~~~
Prompts
~~~~~~~~~

The square root of: Requests a number.

~~~~~~~~~~~~~~~~~
Input Variables
~~~~~~~~~~~~~~~~~

%X: Contains the number for which to calculate the square root.

~~~~~~~~~~~~~~~~~~
Output Variables
~~~~~~~~~~~~~~~~~~

%Y: Contains the square root of %X.

~~~~~~~~~~~~~~~~~~~~~
Examples of %SQROOT
~~~~~~~~~~~~~~~~~~~~~

Example:

.. code-block:: bash

   YDB>SET %X=81
   YDB>DO ^%SQROOT
   YDB>ZWRITE
   %X=81
   %Y=9

This example sets the variable %X to 81 and invokes %SQROOT to calculate the square root non-interactively. ZWRITE displays the contents of the variables.

Example:

.. code-block:: bash

   YDB>DO INT^%SQROOT
   The square root of: 81 is: 9
   The square root of: <RETURN>
   YDB>

This example invokes INT^%SQROOT interactively that prompts for a number. The square root of the number appears on the same line. %SQROOT then prompts for another number. Press <RETURN> to exit.

Example:

.. code-block:: bash

   YDB>WRITE $$FUNC^%SQROOT(81)
   9

This example invokes %SQROOT as an extrinsic function with the label FUNC.

----------------------
String Utilities
----------------------

.. _mpiece-util:

+++++++++++
%MPIECE
+++++++++++

The %MPIECE utility replaces one or more consecutive occurrences of the second argument in the first argument with one occurrence of the third argument. This lets $PIECE operate on the resulting string like UNIX `awk <https://en.wikipedia.org/wiki/AWK>`_.

You can use the %MPIECE utility in Direct Mode or include it in a source application program in the following format:

.. code-block:: none

   $$^%MPIECE(str,expr1,expr2)

If expr1 and expr2 are not specified, %MPIECE assumes expr1 to be one or more consecutive occurrences of whitespaces and expr2 to be one space.

%MPIECE removes all leading occurrences of expr1 from the result.

~~~~~~~~~~~~~~~~
Utility Labels
~~~~~~~~~~~~~~~~

$$SPLIT^%MPIECE(str,expr1): Invokes %MPIECE as an extrinsic function that returns an alias local array of string divided into pieces by expr1. If expr1 is not specified, MPIECE assumes expr1 to be one or more consecutive occurrences of whitespaces.

Example:

.. code-block:: bash

   YDB>set strToSplit=" please split this string into six"
   YDB>set piecestring=$$^%MPIECE(strToSplit," ","|") zwrite strToSplit,piecestring write $length(piecestring,"|")
   strToSplit=" please split this string into six"
   piecestring="please|split|this|string|into|six
   6
   YDB>set \*fields=$$SPLIT^%MPIECE(strToSplit) zwrite fields
   fields(1)="please"
   fields(2)="split"
   fields(3)="this"
   fields(4)="string"
   fields(5)="into"
   fields(6)="six"

.. _randstr-util:

+++++++++++++++++++++
%RANDSTR
+++++++++++++++++++++

%RANDSTR generates a random string.

The format of %RANDSTR is:

.. code-block:: none

   %RANDSTR (strlen,charranges,patcodes,charset)

The random string is of length strlen from an alphabet defined by charset or by charranges and patcodes.

strlen: the length of the random string.

charranges: Range of alphabets defined by charset. By default charranges is 1:1:127. charranges uses the same syntax used for FOR loop ranges, for example, 48:2:57 to select the even decimal digits or 48:1:57,65:1:70 to select hexadecimal digits.

patcodes: specifies pattern codes used to restrict the characters to those that match the selected codes. By default, patcodes is "AN".

charset: Specifies a string of non-zero length. If specified, %RANDSTR generates the random string using the characters in charset, otherwise it takes its alphabet as specified by charranges and patcodes. If charset is of zero length, and is passed by reference, %RANDSTR() initializes it to the alphabet of characters defined by charranges and patcodes. If not specified, strlen defaults to 8, charranges defaults to 1:1:127 and patcodes to "AN".

.. _trim-util:

+++++++++++
%TRIM
+++++++++++

%TRIM removes leading and trailing characters from a string. The format of the %TRIM utility function is:

.. code-block:: none

   $$FUNC|$$L|$$R^%TRIM(expr1[,expr2])


* The first expression specifies the string. The optional second expression specifies a list of trailing and leading characters to remove from expr1. When expr2 is not specified, ^%TRIM assumes expr2 as $char(9,32) which removes all trailing and leading whitespaces (spaces and tabs) from expr1. Note that ^%TRIM treats expr2 as a list of characters (not a substring).

* The $$FUNC label trims leading and trailing characters.

* The $$L label trims leading characters.

* The $$R label trims trailing characters.

You can also use %TRIM as a command line utility to read from STDIN and write to STDOUT in the following format:

.. code-block:: bash

   echo "  string with leading and trailing spaces  " | $ydb_dist/yottadb -r ^%TRIM

Example:

.. code-block:: bash

   YDB>set strToTrim=$char(9,32)_"string with spaces and tabs"_$char(32,32,32) write $length(strToTrim)
   32
   YDB>write "strToTrim=",?24,"""",strToTrim,"""",!,"$$L^%TRIM(strToTrim)=",?24,"""",$$L^%TRIM(strToTrim),"""",!,"$$R^%TRIM(strToTrim)=",?24,"""",$$R^%TRIM(strToTrim),"""",!,"$$FUNC^%TRIM(strToTrim)=",?24,"""",$$FUNC^%TRIM(strToTrim),""""
   strToTrim=              "        string with spaces and tabs   "
   $$L^%TRIM(strToTrim)=   "string with spaces and tabs   "
   $$R^%TRIM(strToTrim)=   "        string with spaces and tabs"
   $$FUNC^%TRIM(strToTrim)="string with spaces and tabs"

This example invokes %TRIM as an extrinsic function and demonstrates the use of its $$L,$$R, and $$FUNC labels.

Example:

.. code-block:: bash

   $ echo " YottaDB Rocks! " | $ydb_dist/yottadb -r ^%TRIM
   YottaDB Rocks!
   $

This example invokes %TRIM as a command line utility which reads STDIN and writes the trimmed output to STDOUT.

--------------------
Global Utilities
--------------------

The Global utilities are:

%G: Displays global variables and their values.

%GC: Copies a global or global sub-tree.

%GCE: Replaces a specified value or part of a value in a set of variables.

%GD: Displays existing globals in the current global directory without displaying their values or descendants.

%GED: Provides full-screen editing capabilities for global variables and values.

%GI: Loads global data from a sequential file into a YottaDB database.

%GO: Extracts global data from a YottaDB database into a sequential file.

%GSE: Displays global variables and their values when the values contain a specified string or number.

%GSEL: Selects globals.

%ZSHOWVTOLCL: Restores ZSHOW "V":gvn data into its original local variables.

.. _g-util:

+++
%G
+++

The %G utility displays names, descendants and values of globals currently existing in the database. Use %G to examine global variables and their values. Enter a question mark (?) at any prompt to display help information.

~~~~~~~~~
Prompts
~~~~~~~~~

Output Device: <terminal>:

Requests a destination device; defaults to the principal device.

List ^Requests the name, in ZWRITE format, of a global to display.

For descriptions of valid input to the List ^ prompt, see the following table.

Arguments for %G and %GED:

+----------------------------------------+-------------------------------------------------------------------------------------+------------------------------------+
| Item                                   | Description                                                                         | Examples                           |
+========================================+=====================================================================================+====================================+
| Global Name                            | M Name                                                                              | SQL, %5                            |
+----------------------------------------+-------------------------------------------------------------------------------------+------------------------------------+
|                                        | M pattern form to match several globals                                             | ?1"A".E, ?1A1"TMP"                 |
+----------------------------------------+-------------------------------------------------------------------------------------+------------------------------------+
|                                        | asterisk to match all global names                                                  | \*                                 |
+----------------------------------------+-------------------------------------------------------------------------------------+------------------------------------+
|                                        | global directory lists request                                                      | ?D                                 |
+----------------------------------------+-------------------------------------------------------------------------------------+------------------------------------+
| Subscripts following a global name in  | M Expr                                                                              | "rick",599,X, or $e(a,7)*10        |
| parenthesis                            |                                                                                     |                                    |
+----------------------------------------+-------------------------------------------------------------------------------------+------------------------------------+
|                                        | [expr]:[expr] for a range                                                           | 1:10, "A":"F", or :4, PNT:, :      |
+----------------------------------------+-------------------------------------------------------------------------------------+------------------------------------+
|                                        | M pattern form to match certain subscripts                                          | 1"E"3N, or ?1"%F".E                |
+----------------------------------------+-------------------------------------------------------------------------------------+------------------------------------+
|                                        | \* descendants                                                                      | \*                                 |
+----------------------------------------+-------------------------------------------------------------------------------------+------------------------------------+

~~~~~~~~~~~~~~~~
Examples of %G
~~~~~~~~~~~~~~~~

Example:

.. code-block:: bash

   YDB>do ^%G
   Output Device: <terminal>: <RETURN>
   List ^C
   ^C="CLASS"
   ^C(1)="MARY"
   ^C(1,2)="MATH"
   ^C(1,2,1)=80
   ^C(1,3)="BIO"
   ^C(1,3,1)=90
   ^C(2)="JOHN"
   ^C(3)="PETER"
   List ^ <RETURN>
   YDB>

This example lists the nodes of global ^C. %G displays the global and its descendants and values, if the node exists.

Example:

.. code-block:: bash

   YDB>do ^%G
   Output Device: <terminal>: <RETURN>
   List ^C(1)
   ^C(1)="MARY"

This example lists only the node entered and its value.

Example:

.. code-block:: bash

   YDB>do ^%G
   Output Device: <terminal>: <RETURN>
   List ^C(1,*)
   ^C(1)="MARY"
   ^C(1,2)="MATH"
   ^C(1,2,1)=80
   ^C(1,3)="BIO"
   ^C(1,3,1)=90
   List ^ <RETURN>
   YDB>

This example uses the asterisk (*) wildcard to list node ^C(1), its descendants and values.

Example:

.. code-block:: bash

   YDB>do ^%G
   Output Device: <terminal>: <RETURN>
   List ^?D
   Global Directory
   Global ^ <RETURN>
   ^C ^D ^S ^Y ^a
   Total of 5 globals.
   List ^
   YDB>

This example specifies "?D" as the global that invokes the %GD utility. %GD displays existing globals in the current global directory without displaying their values or descendants.

.. _gc-util:

+++
%GC
+++

The %GC utility copies values of globals from one global to another. It is useful for testing and for moving misfiled data.

~~~~~~~~~
Prompts
~~~~~~~~~

Show copied nodes <Yes>?:

Asks whether to display the "source nodes" on the principal device.

From global: ^Requests a global variable name from which to copy variable and descendants.

To global: ^Request a global variable name to receive the copy.

~~~~~~~~~~~~~~~~~
Examples of %GC
~~~~~~~~~~~~~~~~~

Example:

.. code-block:: bash

   YDB>do ^%GC
   Global copy
   Show copied nodes <Yes>? <RETURN>
   From global ^b
   To global ^g
   ^g(1)=1
   ^g(2)=2
   ^g(3)=3
   Total 3 nodes copied.
   From global ^<RETURN>
   YDB>

This example makes a copy of the nodes and values of global ^b to global ^g.

.. _gce-util:

+++++
%GCE
+++++

The %GCE utility changes every occurrence of a string within the data of selected global nodes to a replacement string. ^%GCE changes the string in each place it occurs, even if it forms part of a longer string. For example, changing the string 12 to 55 changes 312 to 355.

%GCE displays the name of each global as it is processed. You can suppress the output of the names of globals in which no changes are made by using the QUIET utility label.

~~~~~~~~~
Prompts
~~~~~~~~~

Global^: Requests (using %GSEL) the name(s) of the globals to change; <RETURN> ends selection.

Old string: Requests an existing string to find.

New string: Requests the replacement string.

Show changed nodes <Yes>?:

Asks whether to display the before and after versions of modified nodes on the current device.

Output Device: <terminal>:

Requests a destination device; defaults to the principal device.

QUIET: Only displays the names of globals in which changes are made.

~~~~~~~~~~~~~~~~~~
Examples of %GCE
~~~~~~~~~~~~~~~~~~

Example:

.. code-block:: bash

   YDB>DO ^%GCE
   Global Change Every occurrence
   Global ^a:^b
   ^a ^b
   Current total of 2 globals.
   Global ^ <RETURN>
   Old String: hello
   New String: good-bye
   Show changed nodes <Yes>?: <RETURN>
   Output Device: <terminal>: <RETURN>
   ^a
   No changes made in total 1 nodes.
   ^b
   ^b(10)
   Was : hello Adam
   Now : good-bye Adam
   1 changes made in total 25 nodes.
   Global ^ <RETURN>
   YDB>

This example searches a range of globals and its nodes for the old string value entered. YottaDB searches each global and displays the changes and number of nodes changed and checked.

Example:

.. code-block:: bash

   YDB>set ^b(12)=12
   YDB>set ^b(122)=122
   YDB>set ^b(30)=656
   YDB>set ^b(45)=344
   YDB>set ^b(1212)=012212
   YDB>DO ^%GCE
   Global Change Every occurrence
   Global ^b
   Current total of 1 global.
   Global ^ <RETURN>
   Old String: 12
   New String: 35
   Show changed nodes <Yes>?: <RETURN>
   Output Device: <terminal>: <RETURN>
   ^b(12)
   Was : 12
   Now : 35
   ^b(122)
   Was : 122
   Now : 352
   ^b(1212)
   Was : 12212
   Now : 35235
   5 changes made in total 5 nodes
   Global ^ <RETURN>
   YDB>DO ^%G
   Output device: <terminal>: <RETURN>
   List ^b
   ^b(12)=35
   ^b(30)=656
   ^b(45)=344
   ^b(122)=352
   ^b(1212)=35235

This example shows that executing %GCE replaces all occurrences of "12" in the data stored in the global ^b with "35" and displays the affected nodes before and after the change. Then the %G demonstrates that "12" as data was changed, while "12" in the subscripts remained untouched.

.. _gd-util:

+++++
%GD
+++++

The %GD utility displays existing globals in the current global directory without displaying their values or descendants.

%GD prompts for a global name and redisplays the name if that global exists.

%GD interprets a percent sign (%) in the first position of a global name literally.

%GD allows the wildcard characters asterisk (*) and question mark (?). The wildcards carry their usual meanings, an asterisk (*) denotes a field or a portion of a field, and a question mark (?) denotes a single character.

A colon (:) between two globals specifies a range. %GD displays existing globals within that range.

After each selection %GD reports the number of globals selected by the input.

A question mark (?) entered at a prompt displays help information. Pressing <RETURN> exits %GD.

~~~~~~~~~
Prompts
~~~~~~~~~

Global^: Requests (using %GSEL) a global name with optional wildcards or a range of names; <RETURN> terminates %GD.

~~~~~~~~~~~~~~~~~
Examples of %GD
~~~~~~~~~~~~~~~~~

Example:

.. code-block:: bash

   YDB>DO ^%GD
   Global directory
   Global ^k
   ^k
   Total of 1 global.
   Global ^ <RETURN>
   YDB>

This example verifies that ^k exists in the global directory.

Example:

.. code-block:: bash

   YDB>DO ^%GD
   Global directory
   Global ^C:S
   ^C ^D ^S
   Total of 3 globals
   Global ^ <RETURN>
   YDB>

This example displays a range of globals that exist from ^C to ^S.

Example:

.. code-block:: bash

   YDB>DO ^%GD Global directory
   Global ^*
   ^C ^D ^S ^Y ^a
   Total of 5 globals
   Global ^ <RETURN>
   YDB>

The asterisk (*) wildcard at the Global ^ prompt displays all globals in the global directory.

.. _ged-util:

+++++
%GED
+++++

The %GED utility enables you to edit the globals in a full-screen editor environment. %GED invokes your default editor as specified by the EDITOR environment variable. When you finish the edit, use the [save and] exit command(s) of the editor you are using, to exit.

~~~~~~~~~
Prompts
~~~~~~~~~

Edit^: Requests the name, in ZWRITE format, of a global to edit.

Only one global can be edited at a time with %GED, see “Prompts” above for descriptions of valid input for subscripts.

~~~~~~~~~~~~~~~~~~
Examples of %GED
~~~~~~~~~~~~~~~~~~

Example:

.. code-block:: bash

   YDB>DO ^%GED
   edit ^ b
   Beginning screen:
   ^b(1)="melons"
   ^b(2)="oranges"
   ^b(3)="bananas"
   Screen with a change to ^b(1), elimination of ^b(3), and two new entries ^b(4) and ^b(5):
   ^b(1)="apples"
   ^b(2)="oranges"
   ^b(4)=pears
   ^b(5)="grapes"
   %GED responds:
   Invalid syntax: b(4)=pears
   return to continue:
   After screen:
   ^b(1)="apples"
   ^b(2)="oranges"
   ^b(4)="pears"
   ^b(5)="grapes"
   %GED responds:
   node: ^b
   selected: 3
   changed: 1
   added: 2
   killed: 1
   Edit ^ <RETURN>
   YDB>

This example shows the use of the full-screen editor to change, add, and delete (kill) nodes. When you exit from the editor, %GED checks the syntax and reports any problems. By pressing <RETURN>, return to the full-screen editor to fix the error. At the end of the session, %GED reports how many nodes were selected, changed, killed, and added.

.. _gi-util:

++++
%GI
++++

%GI loads global variable names and their corresponding data values into a YottaDB database from a sequential file. %GI uses the global directory to determine which database files to use. %GI may operate concurrently with normal YottaDB database access. However, a %GI does not use M LOCKs and may produce application-level integrity problems if run concurrently with many applications.

In many ways, %GI is similar to MUPIP LOAD. The format of the input file (GO or ZWRITE) is automatically detected. Like MUPIP LOAD, %GI does not load YottaDB trigger definitions. Unlike MUPIP LOAD, %GI invokes triggers just like any other M code, which may yield results other than those expected or intended.

^%GI loads records having up to 1MiB string length.

~~~~~~~~~
Prompts
~~~~~~~~~

Enter input file:

Requests name of a file; file should be in standard Global Output (GO) format or Zwrite (ZWR) format.

OK <Yes>?: Asks for confirmation.

~~~~~~~~~~~~~~~~~
Examples of %GI
~~~~~~~~~~~~~~~~~

Example:

.. code-block:: bash

   YDB>DO ^%GI
   Global Input Utility
   Input device <terminal>: DATA.GBL
   Saved from users development area
   YottaDB 08-FEB-2018 14:14:09
   OK <Yes>? <RETURN>
   ^IB ^INFO
   Restored 10 nodes in 2 globals
   YDB>

.. _go-util:

++++
%GO
++++

%GO copies specified globals from the current database to a sequential output file in either GO or ZWR format. Use %GO to back up specific globals or when extracting data from the database for use by another system. %GO uses the global directory to determine which database files to use. %GO may operate concurrently with normal YottaDB database access. To ensure that a %GO reflects a consistent application state, suspend database updates to all regions involved in the extract.

In many ways, the %GO utility is similar to MUPIP EXTRACT (-FORMAT=GO or -FORMAT=ZWR). Like MUPIP EXTRACT, %GO does not extract and load YottaDB trigger definitions.

~~~~~~~~~
Prompts
~~~~~~~~~

Global^: Requests (using %GSEL) the name(s) of the globals to search; <RETURN> ends selection.

Header label: Requests text describing contents of extract file.

Output Format: GO or ZWR:

Requests the format to output the data. Defaults to ZWR.

Output Device: <terminal>:

Requests destination device, which may be any legal filename.

~~~~~~~~~~~~~~~~~
Examples of %GO
~~~~~~~~~~~~~~~~~

Example:

.. code-block:: bash

   YDB>DO ^%GO
   Global Output Utility
   Global ^A
   ^A
   Current total of 1 global
   Global ^<RETURN>
   Header label: Revenues May, 2010
   Output Format: GO or ZWR: ZWR
   Output device: /usr/dev/out.go
   ^A
   Total of 1 node in 1 global.
   YDB>

.. _gse-util:

+++++
%GSE
+++++

The %GSE utility finds occurrences of a string within the data values for selected global nodes and displays the variable name and data on a specified output device.

%GSE displays the name of each global as it is processed. You can suppress the output of the names of globals in which the search string is not found by using the QUIET utility label.

~~~~~~~~~
Prompts
~~~~~~~~~

Output Device: <terminal>:

Requests a destination device; defaults to the principal device.

Global^: Requests (using %GSEL) the name(s) of the globals to search; <RETURN> ends selection.

String: Requests a search string.

~~~~~~~~~~~~~~~~~~
Examples of %GSE
~~~~~~~~~~~~~~~~~~

Example:

.. code-block:: bash

   YDB>do ^%GSE
   Global Search For Every Occurence
   Output device: <terminal>: Test.dat
   Global ^a <RETURN>
   ^a
   Current total of 1 global.
   Global ^ <RETURN>
   String: Hello
   ^a
   ^a(10) Hello Adam
   Total 1 matches found in 25 nodes.
   Global ^ <RETURN>
   YDB>

This example searches global ^a for the string "Hello" and displays all nodes that contain that string.

.. _gsel-util:

++++++
%GSEL
++++++

The %GSEL utility selects globals. %GSEL creates a variable %ZG that is a local array of the selected globals. After each selection %GSEL displays the number of globals in %ZG.

%GSEL accepts the wildcard characters asterisk (*), percent sign (%) and question mark (?). The wildcards carry their usual meanings, asterisk (*) denoting a field or a portion of a field, and question mark (?) or percent sign (%) denoting a single character.

* The wildcards question mark (?) and percent sign (%) lose their meanings when in the first position of a global name.

  * When '%' is in the first position of a global name, %GSEL interprets it literally. For example, "%*" means all global names starting with '%'.
  * When you specify only '?' as a global name, %GSEL displays the online help.
  * When you specify a '?' followed by a 'D' or 'd', %GSEL displays the global names currently in the %ZG array.
  * %GSEL produces an error if there is '?' at the first position of a global name followed by any character other than 'D' or 'd'. For example, "?a" produces an error.

* A colon (:) between two globals specifies a range. %GSEL produces an error if you specify a '?' as the first character after a colon (:).

* A minus sign (-) or quotation mark (')  as the first character will cause the search to remove the proceding global or range from the %ZG array.

~~~~~~~~~~~~~~~~
Utility Labels
~~~~~~~~~~~~~~~~

CALL: Runs %GSEL without reinitializing %ZG.

~~~~~~~~~~~~~~~~~~
Output Variables
~~~~~~~~~~~~~~~~~~

%ZG Contains array of all globals selected.

~~~~~~~~~
Prompts
~~~~~~~~~

Global^: Requests a global name with optional wildcards or a range of names.

~~~~~~~~~~~~~~~~~~~
Examples of %GSEL
~~~~~~~~~~~~~~~~~~~

Example:

.. code-block:: none

   YDB>DO ^%GSEL
   Global ^C
   ^C
   Current total of 1 global
   Global ^*
   ^S ^Y ^c ^class
   Current total of 5 globals
   Global ^-S
   ^S
   Current total of 4 globals
   Global ^'Y
   ^Y
   Current total of 3 globals
   Global ^?D
   ^C ^c ^class
   Current total of 3 globals
   Global ^
   YDB>ZWRITE
   %ZG=3
   %ZG("^C")=""
   %ZG("^c")=""
   %ZG("^class")=""
   YDB>

This example adds and subtracts globals from the list of selected globals. "?D" displays all globals selected. ZWRITE displays the contents of the %ZG array.

Example:

.. code-block:: bash

   YDB>DO ^%GSEL
   Global ^a
   ^a
   Current total of 1 global.
   Global ^
   YDB>ZWRITE
   %ZG=1
   %ZG("^a")=""
   YDB>DO CALL^%GSEL
   Global ^?d
   ^a
   Global ^iv
   ^iv
   Current total of 2 globals.
   Global ^
   YDB>ZWRITE
   %ZG=2
   %ZG("^a")=""
   %ZG("^iv")=""
   YDB>

This example uses CALL^%GSEL to add to an existing %ZG array of selected globals.

.. _zshowvtolcl-util:

++++++++++++++++
%ZSHOWVTOLCL
++++++++++++++++

The %ZSHOWVTOLCL utility restores ZSHOW "V":gvn data into its original local variables. Invoke this utility with $ECODE set to the empty string. This utility facilitates automated restoration even of nodes exceeding the maximum record size of the global.

~~~~~~~~~~~~~~~~~
Input Variables
~~~~~~~~~~~~~~~~~

%ZSHOWvbase: The name of the global variable destination for ZSHOW "V". Note that %ZSHOWVTOLCL cannot restore a local variable with the name %ZSHOWvbase.

-------------------------
Routine Utilities
-------------------------

The routine utilities are:

%FL: Lists the comment lines at the beginning of source programs.

%RCE: Replaces every occurrence of a text string with another text string in a routine or a list of routines.

%RD: Lists routine names available through $ZROUTINES.

%RI: Loads routines from RO file to \*.m files in YottaDB format.

%RO: Writes M source code for one or more routines to a sequential device such as a terminal, or a disk file.

%RSE: Searches for every occurrence of a text string in a routine or a list of routines.

%RSEL: Selects M routines and places their directories and shared libraries and names in a local array.

.. _fl-util:

++++
%FL
++++

The %FL utility lists the comment lines at the beginning of source programs. %FL writes the routines in alphabetical order to the specified device. If the output device is not the principal device, %FL displays the name of each routine on the principal device as it writes the routine to the output device.

%FL uses %RSEL to select routines. For more information, see :ref:`rsel-util`.

~~~~~~~~~
Prompts
~~~~~~~~~

Routine: Requests the name(s) of the routines (using %RSEL); <RETURN> ends the selection.

Output Device: <terminal>:

Requests a destination device; defaults to the principal device.

~~~~~~~~~~~~~~~~~
Examples of %FL
~~~~~~~~~~~~~~~~~

Example:

.. code-block:: bash

   YDB>DO ^%FL
   First Line Lister
   Routine: %D
   %D
   Current total of 1 routine.
   Routine: %GS*
   %GSE %GSEL
   Current total of 3 routines.
   Routine: - %D
   %D
   Current total of 2 routines.
   Routine: ?D
   %GSE %GSEL
   Routine: <RETURN>
   Output Device: <RETURN>
   Routine First Line Lister Utility
   YottaDB 08-FEB-2018 16:44:09
   %GSE
   %GSE;YottaDB %GSE utility - global search
   ;
   %GSEL;
   %GSEL;YottaDB %GSEL utility - global select into a local array
   ;
   ;invoke ^%GSEL to create %ZG - a local array of existing globals, interactively
   ;
   Total 5 lines in of 2 routines.
   YDB>

This example selects %D, then selects %GSE and %GSEL and deselects %D. Because the example enters <RETURN> at the Output Device: <terminal>: prompt, the output goes to the principal device.

.. _rce-util:

+++++++++++
%RCE
+++++++++++

The %RCE utility replaces every occurrence of a text string with another text string in a routine or a list of routines.

%RCE uses %RSEL to select routines. For more information, see :ref:`rsel-util`.

%RCE prompts for a text string to replace and its replacement. %RCE searches for text strings in a case-sensitive manner. %RCE issues a warning message if you specify a control character such as a <TAB> in the text string or its replacement. %RCE confirms your selection by displaying the text string and its replacement between a left and right arrow. The arrows highlight any blank spaces that you might have included in the text string or its replacement.

Regardless of whether you select a display of every change, %RCE displays the name of each routine as it is processed. You can suppress the output of the names of routines in which no changes are made by using the QUIET and QCALL utility labels. %RCE completes processing with a count of replacements and routines changed.

~~~~~~~~~
Prompts
~~~~~~~~~

Routine: Requests (using %RSEL) the name(s) of the routines to change; <RETURN> ends the selection.

Old string: Requests string to be replaced.

New string: Requests replacement string.

Show changed lines <Yes>?:

Asks whether to display the before and after versions of the modified lines on an output device.

Output Device: <terminal>:

Requests a destination device; defaults to the principal device.

~~~~~~~~~~~~~~~~
Utility Labels
~~~~~~~~~~~~~~~~

CALL: Works without user interaction unless %ZR is not defined.

~~~~~~~~~~~~~~~~~
Input Variables
~~~~~~~~~~~~~~~~~

The following input variables are only applicable when invoking CALL^%RCE.

%ZR: Contains an array of routines provided or generated with %RSEL.

%ZF: Contains string to find.

%ZN: Contains a replacement string.

%ZD: Identifies the device to display the change trail, defaults to principal device. Make sure you open the device if the device is not the principal device.

%ZC: Truth-value indicating whether to display the change trail, defaults to 0 (no).

~~~~~~~~~~~~~~~~~~
Examples of %RCE
~~~~~~~~~~~~~~~~~~

Example:

.. code-block:: bash

   YDB>DO ^%RCE
   Routine Change Every occurrence
   Routine: BES*
   BEST BEST2 BEST3 BEST4
   Current total of 4 routines
   Routine: <RETURN>
   Old string:^NAME
   New string:^STUDENT
   Replace all occurrences of:
   >^NAME<
   With
   >^STUDENT<
   Show changed lines <Yes>?: <RETURN>
   Output Device: <RETURN>
   /usr/smith/work/BEST.m
   Was: S ^NAME=SMITH
   Now: S ^STUDENT=SMITH
   Was: S ^NAME(1)=JOHN
   Now: S ^STUDENT(1)=JOHN
   /usr/smith/work/BEST2.m
   /usr/smith/work/BEST3.m
   Was: S ^NAME=X
   Now: S ^STUDENT=X
   Was: W ^NAME
   Now: W ^STUDENT
   /usr/smith/work/BEST4.m
   Total of 4 routines parsed.
   4 occurrences changed in 2 routines.
   YDB>

This example selects a list of routines that change the string "^NAME" to the string "^STUDENT," and displays a trail of the changes.

Example:

.. code-block:: bash

   YDB>DO ^%RCE
   Routine Change Every occurrence
   Routine: BES*
   BEST BEST2 BEST3 BEST4
   Current total of 4 routines
   Routine: <RETURN>
   Old String:<TAB>
   The find string contains control characters
   New string: <RETURN>
   Replace all occurrences of:
   ><TAB><
   With:
   ><
   Show changed lines <Yes>?: N
   BEST BEST2 BEST3 BEST4
   Total 4 routines parsed.
   4 occurrences changed in 2 routines.
   YDB>

This example removes all occurrences of the <TAB> key from specified routines and suppresses the display trail of changes.

.. _rd-util:

+++
%RD
+++

The %RD utility lists routine names accessible through the current $ZROUTINES. %RD calls %RSEL and displays any routines accessible through %RSEL. Use %RD to locate routines.

%RD accepts the wildcard characters asterisk (*) and question mark (?). The wildcards carry their usual meanings, an asterisk (*) denotes a field or a portion of a field, and a question mark (?) denotes a single character in positions other than the first.

A colon (:) between two routine names specifies a range of routines. %RD displays only those routine names accessible through the current $ZROUTINES.

After each selection %RD displays the total number of routines listed.

Pressing <RETURN> exits %RD.

~~~~~~~~~
Prompts
~~~~~~~~~

Routine: Requests (using %RSEL) the name(s) of the routines to list; <RETURN> ends the selection.

~~~~~~~~~~~~~~~~
Utility Labels
~~~~~~~~~~~~~~~~

OBJ: Lists object modules accessible through the current $ZROUTINES.

LIB: Lists percent (%) routines accessible through the current $ZROUTINES.

SRC: Lists the source modules accessible through the current $ZROUTINES (same as %RD).

~~~~~~~~~~~~~~~~~
Examples of %RD
~~~~~~~~~~~~~~~~~

Example:

.. code-block:: bash

   YDB>DO ^%RD
   Routine directory
   Routine: TAXES
   TAXES
   Total of 1 routine
   Routine:*
   EMP FICA PAYROLL TAXES YTD
   Total of 5 Routines
   Routine: <RETURN>
   YDB>

This example invokes %RD that prompts for routine TAXES and the wildcard (*). %RD lists five routines accessible through the current $ZROUTINES.

Example:

.. code-block:: bash

   YDB>DO OBJ^%RD
   Routine directory
   Routine:*
   EMP FICA
   Total of 2 routines
   Routine: <RETURN>
   YDB>

This example invokes %RD with the label OBJ that lists only object modules accessible through the current $ZROUTINES.

Example:

.. code-block:: bash

   YDB>DO LIB^%RD
   Routine directory
   %D %DATE %DH %G %GD %GSEL
   YDB>

This example invokes %RD with the LIB label that lists all the % routines accessible through the current $ZROUTINES.

Example:

.. code-block:: bash

   YDB>DO SRC^%RD
   Routine directory
   Routine:*
   DATACHG
   Total of 1 routines
   Routine: <RETURN>
   YDB>

This example invokes %RD with the label SRC that lists only source modules accessible through the current $ZROUTINES.

.. _ri-util:

+++
%RI
+++

%RI transforms M routines in the sequential format described in the ANSI standard into individual .m files in YottaDB format. Use %RI to make M RO format accessible as YottaDB routines.

~~~~~~~~~
Prompts
~~~~~~~~~

Formfeed delimited <No>?

Requests whether lines should be delimited by formfeed characters rather than carriage returns.

Input Device: <terminal>:

Requests name of RO file containing M routines.

Output Directory:

Requests name of directory to output M routines.

~~~~~~~~~~~~~~~~~
Examples of %RI
~~~~~~~~~~~~~~~~~

Example:

.. code-block:: bash

   YDB>DO ^%RI
   Routine Input utility - Converts RO file to \*.m files
   Formfeed delimited <No>? <RETURN>
   Input device: <terminal>: file.ro
   Files saved from FILEMAN directory
   YottaDB 07-MAY-2002 15:17:54
   Output directory: /usr/smith/work/
   DI DIA DIAO DIAI DIB DIBI
   Restored 753 lines in 6 routines.
   YDB>

.. _ro-util:

++++
%RO
++++

The %RO utility writes M source code for one or more routines to a sequential device such as, a disk file or a printer. .

%RO uses %RSEL to select routines. For more information, see :ref:`rsel-util`.

%RO writes the routines in alphabetical order to the specified device. %RO displays the name of each routine as it writes the routine to the device.

~~~~~~~~~
Prompts
~~~~~~~~~

Routine: Requests (using %RSEL) the name(s) of the routines to output; <RETURN> ends selection.

Output device: <terminal>:

Requests a destination device; defaults to the principal device.

Header label: Requests text to place in the first of the two header records.

Strip comments <No>?:

Asks whether to remove all comment lines except those with two adjacent semicolons.

~~~~~~~~~~~~~~~~
Utility Labels
~~~~~~~~~~~~~~~~

CALL: Works without user interaction unless %ZR is not defined.

~~~~~~~~~~~~~~~~~
Input Variables
~~~~~~~~~~~~~~~~~

The following input variables are only applicable when invoking CALL^%RO.

%ZR: Contains an array of routines provided or generated with %RSEL.

%ZD: Identifies the device to display output, defaults to principal device.

~~~~~~~~~~~~~~~~~
Examples of %RO
~~~~~~~~~~~~~~~~~

Example:

.. code-block:: bash

   YDB>DO ^%RO
   Routine Output - Save selected routines into RO file.
   Routine: %D
   %D
   Current total of 1 routines.
   Routine: -%D
   %D
   Current total of 0 routines.
   Routine: BEST*
   BEST BEST1 BEST2
   Current total of 3 routines.
   Routine: ?D
   BEST BEST1 BEST2
   Routine: <RETURN>
   Output Device: <terminal>: output.txt
   Header Label: Source code for the BEST modules.
   Strip comments <No>?:<RETURN>
   BEST BEST1 BEST2
   Total of 53 lines in 3 routines
   YDB>

This example adds and subtracts %D from the selection, then adds all routines starting with "BEST" and confirms the current selection. The example sends output to the designated output file output.txt. %RO displays the label at the beginning of the output file. The first record of the header label is the text entered at the prompt. The second record of the header label consists of the word "YottaDB" and the current date and time.

.. _rse-util:

+++++++
%RSE
+++++++

The %RSE utility searches for every occurrence of a text string in a routine or a list of routines.

%RSE uses %RSEL to select routines. For more information, see :ref:`rsel-util`.

%RSE searches for text strings are case-sensitive. %RSE issues a warning message if you specify a control character such as a <TAB> in the text string. %RSE confirms your selection by displaying the text string between a left and right arrow. The arrows display any blank spaces included in the text string.

%RSE displays the name of each routine as it is processed. You can suppress the output of the names of routines in which the search string is not found by using the QUIET and QCALL utility labels.

%RSE completes processing with a count of occurrences found.

~~~~~~~~~
Prompts
~~~~~~~~~

Routine: Requests (using %RSEL) the name(s) of the routines to search; <RETURN> ends selection.

Find string: Requests string for which to search.

Output device: <terminal>:

Requests a destination device; defaults to the principal device.

~~~~~~~~~~~~~~~~
Utility Labels
~~~~~~~~~~~~~~~~

CALL: Works without user interaction unless %ZR is not defined.

~~~~~~~~~~~~~~~~~
Input Variables
~~~~~~~~~~~~~~~~~

The following input variables are only applicable when invoking CALL^%RSE.

%ZR: Contains an array of routines provided or generated with %RSEL.

%ZF: Contains the string to find.

%ZD: Identifies the device to display the results, defaults to principal device. Make sure you open the device if the device is not the principal device.

~~~~~~~~~~~~~~~~~~
Examples of %RSE
~~~~~~~~~~~~~~~~~~

Example:

.. code-block:: bash

   YDB>DO ^%RSE
   Routine Search for Every occurrence
   Routine: BES*
   BEST BEST2 BEST3 BEST4
   Current total of 4 routines
   Routine: <RETURN>
   Find string:^NAME
   Find all occurrences of:
   >^NAME<
   Output device: <terminal>:
   /usr/smith/work/BEST.m
   S ^NAME=SMITH
   S ^NAME(1)=JOHN
   /usr/smith/work/BEST2.m
   /usr/smith/work/BEST3.m
   S ^NAME=X
   W ^NAME
   /usr/smith/work/BEST4.m
   Total of 4 routines parsed.
   4 occurrences found in 2 routines.
   YDB>

This example invokes %RSE that searches and finds a given string. The output device specifies a terminal display of all lines where the text string occurs.

Example:

.. code-block:: bash

   YDB>DO ^%RSE
   Routine Search for Every occurrence
   Routine: BEST
   BEST
   Current total of 1 routine
   Routine: <RETURN>
   Find string:^NAME
   Find all occurrences of:
   >^NAME<
   Output Device: out.lis
   BEST
   YDB>

This example instructs ^%RSE to write all lines where the text string occurs to an output file, out.lis.

.. _rsel-util:

++++++++++
%RSEL
++++++++++

The %RSEL utility selects M routines. %RSEL selects routines using directories and shared libraries specified by the YottaDB special variable $ZROUTINES. $ZROUTINES contains an ordered list of directories that certain YottaDB functions use to locate source and object files. If $ZROUTINES is not defined, YottaDB sets it in the environment to :code:`$ydb_dist/libyottadbutil.so` in M mode or to :code:`$ydb_dist/utf8/libyottadbutil.so` in UTF-8 mode, if it exists, and to :code:`$ydb_dist` if it does not, and then uses that value. Other YottaDB utilities call %RSEL.

%RSEL prompts for the name of a routine(s).

%RSEL accepts the wildcard characters asterisk (*) and question mark (?). The wildcards carry their usual meanings: an asterisk (*) denotes a field or a portion of a field, and a question mark (?) denotes a single character in positions other than the first.

A colon (:) between two routines specifies a range.

%RSEL creates a read-write variable %ZR, which is a local array of selected routines. After each selection, %RSEL reports the number of routines in %ZR. A minus sign (-) or an apostrophe (') character preceding a routine name removes that routine from the %ZR array. A question mark (?) provides online help, and "?D" displays M routines currently in the array.

.. note::
   If a local variable %ZRSET is defined, %RSEL places the output information into a global variable (^%RSET) instead of the local variable %ZR.

~~~~~~~~~
Prompts
~~~~~~~~~

Routine: Requests the name(s) of the routines; <RETURN> ends selection.

~~~~~~~~~~~~~~~~
Utility Labels
~~~~~~~~~~~~~~~~

CALL: Performs %RSEL without reinitializing %ZR.

OBJ: Searches only object files.

SILENT: Provides non-interactive (batch) access to the functionality of %RSEL. The syntax is SILENT^%RSEL(pattern,label) where pattern is a string that specifies the routine names to be searched, label can be "OBJ", "SRC" or "CALL". The default value of "SRC" corresponds to ^%RSEL if invoked interactively.

SRC: Searches only source files (same as %RSEL).

SRCDIR: Returns a space separated list of directories in $ZROUTINES that can contain source code. The syntax is SRCDIR^%RSEL. Typically, the first source directory is the location where code generators should place generated source code. If there are no source directories, for example, if $ZROUTINES contains only shared libraries, SRCDIR^%RSEL returns an empty string (:code:`""`).

~~~~~~~~~~~~~~~~~
Input Variables
~~~~~~~~~~~~~~~~~

The following input variables are only valid when invoking CALL^%RSEL:

%ZE: Contains the file extension, usually either .m for source files or .o for object files.

%ZR: As input, contains an existing list of routines to be modified.

%ZRSET: On being set, requests %RSEL to place the output in the global variable ^%RSET.

~~~~~~~~~~~~~~~~~~
Output Variables
~~~~~~~~~~~~~~~~~~

%ZR: As output, contains list of directories and shared libraries indexed by selected routine names.

^%RSET($JOB): The output global variable ^%RSET is used instead of the local variable %RD if the input variable %ZRSET is set. It is indexed by job number $JOB and the selected routine names.

~~~~~~~~~~~~~~~~~~~
Examples of %RSEL
~~~~~~~~~~~~~~~~~~~

Example:

.. code-block:: bash

   YDB>DO ^%RSEL
   Routine: TES*
   TEST2 TEST3
   Current total of 2 routines
   Routine: <RETURN>
   YDB>DO OBJ^%RSEL
   Routine:TEST?
   Current total of 0 routines
   Routine: <RETURN>
   YDB>ZWRITE
   %ZR=0

This example selects two source routines starting with "TES" as the first three characters. Then, the example invokes %RSEL at the OBJ label to select object modules only. OBJ^%RSEL returns a %ZR=0 because object modules for the TEST routines do not exist.

Example:

.. code-block:: none

   YDB>DO ^%RSEL
   Routine: BES*
   BEST BEST2 BEST3 BEST4
   Current total of 4 routines
   Routine: - BEST
   BEST
   Current total of 3 routines
   Routine: ?D
   BEST2 BEST3 BEST4
   Routine: 'BEST2
   BEST2
   Current total of 2 routines
   Routine: ?D
   BEST3 BEST4
   Routine: <RETURN>
   YDB>ZWRITE
   %ZR=2
   %ZR("BEST3")="/usr/smith/work/"
   %ZR("BEST4")="/usr/smith/test/"
   YDB>

This example selects the routines using the asterisk (*) wildcard and illustrates how to tailor your selection list. Note that %ZR contains two routines from different directories.

By default, %RSEL bases the contents of %ZR on source files that have a .m extension.

Example:

.. code-block:: bash

   YDB>DO ^%RSEL
   Routine:BEST*
   BEST2 BEST3
   Current total of 2 routines
   Routine: <RETURN>
   YDB>ZWRITE
   %ZR=2
   %ZR("BEST2")="/usr/smith/test/"
   %ZR("BEST3")="/usr/smith/test/"

This example creates a %ZR array with BEST2 and BEST3.

Example:

.. code-block:: bash

   YDB>DO ^%RSEL
   Routine:LOCK
   LOCK
   Current total of 1 routine
   Routine: <RETURN>
   YDB>ZWRITE
   %ZR=1
   %ZR("LOCK")="/usr/smith/work/"
   YDB>DO CALL^%RSEL
   Routine:BEST*
   BEST2 BEST3
   Current total of 2 routines
   Routine: <RETURN>
   YDB>ZWRITE
   %ZR=3
   %ZR("BEST2")="/usr/smith/work/"
   %ZR("BEST3")="/usr/smith/work/"
   %ZR("LOCK")="/usr/smith/work/"
   YDB>

This example creates a %ZR array with LOCK and adds to it using CALL%RSEL.

Example:

.. code-block:: bash

   YDB>do SILENT^%RSEL("myroutine","OBJ")

.. code-block:: bash

   YDB>ZWRITE
   %ZR=1
   %ZR("myroutine")="/usr/smith/work"

This example invokes %RSEL non-interactively and creates a %ZR array for myroutine using OBJ%RSEL.

------------------------------
Internationalization Utilities
------------------------------

The internationalization utilities are:

.. _gbldef-util:

%GBLDEF: Manipulates the collation sequence assigned to a global. For more information and usage examples, refer to :ref:`use-gbldef-util`.

.. _lclcol-util:

%LCLCOL: Manipulates the collation sequence assigned to local variables in an active process. For more information and usage examples, refer to :ref:`establish-local-colln-seq`.

.. _patcode-util:

%PATCODE: Loads pattern definition files for use within an active database.

These utilities are an integral part of the YottaDB functionality that permits you to customize your applications for use with other languages. For a description of these utilities, refer to `Chapter 12: “Internationalization” <./internatn.html>`_.

----------------------------
System Management Utilities
----------------------------

The System Management utilities are:

.. _dsewrap-util:

+++++++++++++++
%DSEWRAP
+++++++++++++++

The %DSEWRAP utility provides a programmatic interface that drives DSE either through a PIPE device or through generated command files. The current implementation only provides access to dumping the database file header.

.. note::
   %DSEWRAP is currently deprecated. Please use the %PEEKBYNAME utility to programatically read database file header information. MUPIP DUMPFHEAD and/or the %DUMPFHEAD utility provide additional alternatives.

~~~~~~~~~~~~~~~~
Utility Labels
~~~~~~~~~~~~~~~~

DUMP^%DSEWRAP(regions,.fdump,"fileheader","all") : Retrieve and parse the result of the DSE's DUMP -FILEHEADER -ALL command into the second parameter (passed by reference) for the regions contained in the local variable 'regions'. If invoked as an extrinsic function, %DSEWRAP returns the status of DUMP -FILEHEADER -ALL command.

The first parameter 'regions' can be undefined, "", "*" or "all" to mean all available regions.

The second parameter is a required passed-by-reference variable that the caller uses to retrieve data.

The third optional parameter defaults to DUMP -FILEHEADER. Using any other command dump command has not been tested.

The fourth optional parameter indicates the level of detail, -ALL, for the DUMP -FILEHEADER command. Fore more information on other -FILEHEADER qualifiers, please refer to the `DSE chapter in the Administration and Operations Guide <../AdminOpsGuide/dse.html>`_.

The format of the output array is fdump(<REGION NAME>,<FIELD NAME>). In the event of a field collision, dump^%DSEWRAP avoids overwriting existing data by creating number descendants.

The default $ETRAP handler for %DSEWRAP terminates the application if it detects an error during input validation. Application developers must define $ETRAP prior to calling %DSEWRAP.

Example:

.. code-block:: bash

   $ydb -run ^%XCMD 'do dump^%DSEWRAP("DEFAULT",.dsefields,"","all") zwrite dsefield'

.. _dumpfhead-util:

+++++++++++++
%DUMPFHEAD
+++++++++++++

The %DUMPFHEAD utility provides a programmatic interface to the functionality of MUPIP DUMPFHEAD. This routine reads the database file header directly, rather than opening it as a database and reading values mapped into memory. This means that it is lighter weight in some senses than ^%PEEKBYNAME, but it also means that the information it retrieves is more limited, and possibly less current.

~~~~~~~~~~~~~~~~
Utility Labels
~~~~~~~~~~~~~~~~

getfields^%DUMPFHEAD(fldarray,dbname) : Retrieve the file header fields provided by the MUPIP DUMPFHEAD command for the database file specified by the second parameter into the array passed by reference to the first parameter.

The first parameter is a required pass-by-reference variable that the caller uses to retrieve data.

The second parameter is the path and name for the database file on which to report information.

The format of the output array is fdump(sgmnt_data.<FIELD NAME>)=<value>; refer to :ref:`peekbyname-util` for additional information on the names and values.

The $ETRAP handler simply QUITs as it defers error handling to the caller. Application developers should define an appropriate $ETRAP prior to calling %DUMPFHEAD.

Example:

.. code-block:: bash

   $ydb -run ^%XCMD 'do getfields^%DUMPFHEAD(.fields,"yottadb.dat") zwrite fields'

.. _freecnt-util:

+++++++++++++++
%FREECNT
+++++++++++++++

The %FREECNT utility displays the number of free blocks in the database files associated with the current global directory.

Example:

.. code-block:: bash

   YDB>DO ^%FREECNT
   Region          Free     Total          Database file
   ------          ----     -----          -------------
   DEFAULT           81       100 ( 81.0%) /home/yottadbuser1/.yottadb/r1.20_x86/g/yottadb.dat

   YDB>

This example invokes %FREECNT at the YDB> prompt that displays the number of free blocks and percentage of free space available in the current global directory.

.. _peekbyname-util:

++++++++++++++++++
%PEEKBYNAME()
++++++++++++++++++

%PEEKBYNAME() provides a stable interface to $ZPEEK() that uses control structure field names. $ZPEEK() provides a read-only mechanism to access selected fields in selected control structures in the address space of a process, including process private memory, database shared memory segments and Journal Pools. Although application code can call $ZPEEK() directly, such direct access must use numeric arguments that can vary from release to release. Access by name using %PEEKBYNAME makes application code more stable across YottaDB releases. For more information, refer to :ref:`zpeek-function`. YottaDB intends to maintain the stability of a name from release to release where that name refers to the same data item; however, we may add or obsolete names, or modify the type and size associated with existing names at our discretion, to reflect changes in the implementation. The format of the %PEEKBYNAME() function is:

.. code-block:: none

   %PEEKBYNAME(field[,regindex[,format[,gldpath]]])

* The first expression specifies the memory location to access in the format: CONTROL_BLOCK[.FIELD].* (For example, "gd_region.max_key_size").
* The optional second expression specifies a region name, structure index or a base address associated with the first (field name) argument. The choice is governed by the following rules applied in the following order:

  1. If the value is a hex value in the form of 0xhhhhhhhh[hhhhhhhh], then PEEKBYNAME uses it as the base address of the data to fetch. Also in this case, the offset, length, and type are taken from the field specified in the first expression (field). For more information, see the description of the "PEEK" mnemonic in :ref:`zpeek-function`.
  2. If the first expression refers to one of the region-related structures supported by the $ZPEEK() function, %PEEKBYNAME() treats this second expression as a region name. Always specify region names to %PEEKBYNAME() in uppercase. As YottaDB uses lower case region names for :ref:`ygblstat-util`, using lowercase names may yield unexpected results.
  3. If the first expression refers to one of the replication related structures supported by the $ZPEEK() function that are indexed, PEEKBYNAME treats this second expression as a numerical (base 10) index value.
  4. For those structures supported by the $ZPEEK() function that do not accept an argument, this second expression must be NULL or not specified.

* The optional third expression specifies the output format in one character as defined in the "format" argument in the $ZPEEK() documentation. This argument overrides the automatic format detection by the %PEEKBYNAME utility.
* The optional fourth argument is a global directory referencing the :code:`ydbhelp.gld` for accesssing the :code:`ydbhelp.dat` file.

Example:

.. code-block:: none

   ; Print the maximum key size for the DEFAULT region
   YDB>write $$^%PEEKBYNAME("gd_region.max_key_size","DEFAULT")
   64
   ; Print the journaling state (non-zero value means it is on)
   YDB>write $$^%PEEKBYNAME("gd_region.jnl_state","DEFAULT")
   0
   ; Save the base address of the database file header
   YDB>set base=$$^%PEEKBYNAME("sgmnt_addrs.hdr","DEFAULT")
   ; Print the file header label
   YDB>write $$^%PEEKBYNAME("sgmnt_data.label",base)
   GDSDYNUNX03

~~~~~~~~~~~~~~~~~~~~~
LISTALL^%PEEKBYNAME
~~~~~~~~~~~~~~~~~~~~~

Prints all the field mnemonics acceptable as the first argument to %PEEKBYNAME().

~~~~~~~~~~~~~~~~~~~~~~~~~~~
LIST^%PEEKBYNAME(.output)
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Populates output variable with the type and size information indexed by the field mnemonics for all %PEEKBYNAME()-acceptable fields. For example, output("gd_region.jnl_file_name")="unsigned-char,256".

~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Labels for selected fields
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Below are selected fields for which you may find %PEEKBYNAME to be a useful alternative to running a DSE or MUPIP command in a PIPE device, and parsing the output. If there is a field that you wish to access using %PEEKBYNAME, please send questions to your YottaDB support channel. We will get you an answer, and if it seems to us to be of general interest, we will add it to the %PEEKBYNAME user documentation.

~~~~~~~~~~~~~~~~~~~
Region Parameters
~~~~~~~~~~~~~~~~~~~

Calls to %PEEKBYNAME with the listed string as value of the first parameter, and the region name as the value of the second parameter, return the value. For example:

.. code-block:: bash

   YDB>write $$^%PEEKBYNAME("sgmnt_data.n_bts","DEFAULT") ; How many global buffers there are
   1000
   YDB>write $$^%PEEKBYNAME("node_local.wcs_active_lvl","DEFAULT") ; How many of them are dirty
   0
   YDB>for i=1:1:10000 set ^x($$^%RANDSTR(8))=$$^%RANDSTR(64)
   YDB>write $$^%PEEKBYNAME("node_local.wcs_active_lvl","DEFAULT") ; And now, how many of them are dirty
   377
   YDB>write $ZPIECE($$^%PEEKBYNAME("gd_region.open","DEFAULT"),$ZCHAR(0),1) ; display if DEFAULT region is open (1) or not (0)
   1
   YDB>write $ZPIECE($$^%PEEKBYNAME("gd_segment.fname","AREG"),$ZCHAR(0),1) ; display database file name for AREG region
   YDB>

When using the following, remember to write code that allows for values other than those listed, e.g., if writing code to check whether before image journaling is in use, make sure it can deal with values other than 0 and 1, because a future release of YottaDB can potentially introduce a new return value for a field.

+------------------------------+-----------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| Parameter                    | ^%PEEKBYNAME() Parameter                      | Value                                                                                                                       |
+==============================+===============================================+=============================================================================================================================+
| Asynchronous I/O             |  "sgmnt_data.asyncio"                         | TRUE (1) if the region has asynchronous I/O enabled and FALSE (0) if it does not                                            |
+------------------------------+-----------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| Block size                   |  "sgmnt_data.blk_size"                        | Integer number of bytes                                                                                                     |
+------------------------------+-----------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| Commit wait spin count       |  "sgmnt_data.wcs_phase2_commit_wait_spincnt"  | Integer Count                                                                                                               |
+------------------------------+-----------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| Current transaction          |  "sgmnt_data.trans_hist.curr_tn"              | Integer Count                                                                                                               |
+------------------------------+-----------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| Database file FTOK           |  "unix_db_info.ftok_semid"                    | Id of the semaphore used when creating and releasing the shared memory and private semaphore for each database file         |
+------------------------------+-----------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| Database file inode          |  "unix_db_info.fileid.inode"                  | Database file's inode value                                                                                                 |
+------------------------------+-----------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| Database file name           |  "gd_segment.fname"                           | String of text. See example above for how to use.                                                                           |
+------------------------------+-----------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| Defer allocate               |  "sgmnt_data.defer_allocate"                  | Integer - 1 means DEFER_ALLOCATE, 0 means NODEFER_ALLOCATE                                                                  |
+------------------------------+-----------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| Encryption key hash          |  "sgmnt_data.encryption_hash"                 | String of binary bytes                                                                                                      |
+------------------------------+-----------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| Extension size               |  "sgmnt_data.extension_size"                  | Integer number of blocks                                                                                                    |
+------------------------------+-----------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| Flush trigger                |  "sgmnt_data.flush_trigger"                   | Integer number of blocks (not meaningful for MM)                                                                            |
+------------------------------+-----------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| Journal align size           |  "sgmnt_data.alignsize"                       | Integer number of bytes                                                                                                     |
+------------------------------+-----------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| Journal autoswitch limit     |  "sgmnt_data.autoswitchlimit"                 | Integer number of bytes for maximum size of each journal file                                                               |
+------------------------------+-----------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| Journal before imaging       |  "sgmnt_data.jnl_before_image"                | Integer - 1 means BEFORE image journaling, 0 means NOBEFORE (meaningful only if journaling is on)                           |
+------------------------------+-----------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| Journal buffer size          |  "sgmnt_data.jnl_buffer_size"                 | Integer number of journal buffers                                                                                           |
+------------------------------+-----------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| Journal epoch interval       |  "sgmnt_data.epoch_interval"                  | Integer number of seconds                                                                                                   |
+------------------------------+-----------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| Journal next write offset    |  "jnl_buffer.dskaddr"                         | Integer number of bytes from beginning of journal file                                                                      |
+------------------------------+-----------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| Journal next epoch time      |  "jnl_buffer.next_epoch_time"                 | Integer number of seconds since January 1, 1970 00:00:00 UTC                                                                |
+------------------------------+-----------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| Journal state                |  "sgmnt_data.jnl_state"                       | Integer 0 means disabled, 1 means enabled but off, 2 means on                                                               |
+------------------------------+-----------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| Journal SYNCIO               |  "sgmnt_data.jnl_sync_io"                     | Integer - 1 means SYNC_IO, 0 means NOSYNC_IO                                                                                |
+------------------------------+-----------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| Journal yield limit          |  "sgmnt_data.yield_lmt"                       | Integer count                                                                                                               |
+------------------------------+-----------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| Lock space                   |  "sgmnt_data.lock_space_size"                 | Integer number of bytes                                                                                                     |
+------------------------------+-----------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| Maximum key size             |  "sgmnt_data.max_key_size"                    | Integer number of bytes                                                                                                     |
+------------------------------+-----------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| Maximum record size          |  "sgmnt_data.max_rec_size"                    | Integer number of bytes                                                                                                     |
+------------------------------+-----------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| Mutex hard spin count        |  "sgmnt_data.mutex_spin_parms.mutex_hard_spi  | Integer count                                                                                                               |
|                              |  n_count"                                     |                                                                                                                             |
+------------------------------+-----------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| Mutex sleep spin count       | "sgmnt_data.mutex_spin_parms.mutex_sleep_spi  | Integer count                                                                                                               |
|                              | n_count"                                      |                                                                                                                             |
+------------------------------+-----------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| Null subscripts              | "sgmnt_data.null_subs"                        | Integer - 0 means disabled, 1 means enabled, 2 means existing null subscripts are respected but new ones cannot be created  |
|                              |                                               |                                                                                                                             |
+------------------------------+-----------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| Number of global buffers     | "node_local.wcs_active_lvl"                   | Integer Count                                                                                                               |
| (dirty)                      |                                               |                                                                                                                             |
+------------------------------+-----------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| Number of global buffers     |  "sgmnt_data.n_bts"                           | Integer Count                                                                                                               |
| (total)                      |                                               |                                                                                                                             |
+------------------------------+-----------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| Number of processes accessing|  "node_local.ref_cnt"                         | Integer count (always greater than zero, because the process running %PEEKBYNAME has the database open)                     |
| the database                 |                                               |                                                                                                                             |
+------------------------------+-----------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| Region open                  |  "gd_region.open"                             | Boolean. See example above for how to use.                                                                                  |
+------------------------------+-----------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| Region replication sequence  |  "sgmnt_data.reg_seqno"                       | Integer Count                                                                                                               |
| number                       |                                               |                                                                                                                             |
+------------------------------+-----------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| Report number of maximum     |  "node_local.max_procs"                       | Two comma separated integers. First is the integer count of the maximum number of processes that concurrently accessed that |
| processes concurrently       |                                               | database region, and the second is the integer count of the number of seconds since January 1, 1970 00:00:00 UTC at which   |
| accessing a database region  |                                               | that number of processes was recorded.                                                                                      |
+------------------------------+-----------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| Spanning nodes absent        |  "sgmnt_data.span_node_absent"                | Integer - 1 means that no global variable nodes span multiple database blocks, 0 means YottaDB does not know (in the        |
|                              |                                               | past, at least one global variable node spanned multiple blocks, but it may since have been overwritten or KILL'd)          |
+------------------------------+-----------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| Write errors                 |  "sgmnt_data.wcs_wterror_invoked_cntr"        | Integer count of errors trying to write database blocks - barring problems with the storage subsystem, hitting an           |
|                              |                                               | asynchronous write limit constitute the primary (probably only) cause                                                       |
+------------------------------+-----------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
| Writes in progress           |  "node_local.wcs_wip_lvl"                     | Integer count of blocks for which YottaDB has issued writes that have not yet been recognized as complete                   |
+------------------------------+-----------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+

~~~~~~~~~~~~~~~~~~~~~~~~
Replication Parameters
~~~~~~~~~~~~~~~~~~~~~~~~

Calls to %PEEKBYNAME with the listed parameter as the first or only parameter return replication fields as described. For example:

.. code-block:: bash

   YDB>write $$^%PEEKBYNAME("repl_inst_hdr.inst_info.this_instname") ; Name of this instance
   Collegeville
   YDB>write $$^%PEEKBYNAME("gtmsource_local_struct.secondary_instname",0) ; Name of instance in slot 0 of replication instance file
   Malvern
   YDB>set x=$$^%PEEKBYNAME("jnlpool_ctl_struct.jnl_seqno") ; Sequence number in Journal Pool of Collegeville
   YDB>set y=$$^%PEEKBYNAME("gtmsource_local_struct.read_jnl_seqno",0) ; Next sequence number to send to Malvern
   YDB>write x-y ; Current replication backlog from Collegeville to Malvern
   2
   YDB>

+--------------------------------------+-----------------------------------------------------------+-------------------------------------------------------------+
| Replication Parameter                | ^%PEEKBYNAME() Parameter                                  | Value                                                       |
+======================================+===========================================================+=============================================================+
| Instance Freeze                      |  "jnlpool_ctl_struct.instfreeze_environ_inited"           | Integer 1 means Instance Freeze is enabled; 0 means Instance|
|                                      |                                                           | Freeze is disabled                                          |
+--------------------------------------+-----------------------------------------------------------+-------------------------------------------------------------+
| Journal sequence number              |  "jnlpool_ctl_struct.jnl_seqno"                           | Integer                                                     |
+--------------------------------------+-----------------------------------------------------------+-------------------------------------------------------------+
| Journal sequence number to send to   | "gtmsource_local_struct.read_jnl_seqno",i where i is the  | Integer                                                     |
| receiving instance in replication    | slot number in the replication instance file              |                                                             |
| file slot                            |                                                           |                                                             |
+--------------------------------------+-----------------------------------------------------------+-------------------------------------------------------------+
| Name of receiving instance in        | "gtmsource_local_struct.secondary_instname",i where i is  | String of text                                              |
| replication instance file slot       | the slot number in the replication instance file          |                                                             |
+--------------------------------------+-----------------------------------------------------------+-------------------------------------------------------------+
| Name of this instance                |  "repl_inst_hdr.inst_info.this_instname"                  | String of text                                              |
+--------------------------------------+-----------------------------------------------------------+-------------------------------------------------------------+
| Supplementary Replication            |  "repl_inst_hdr.is_supplementary"                         | Integer - 1 means supplementary instance; 0 means not       |
|                                      |                                                           | supplementary instance                                      |
+--------------------------------------+-----------------------------------------------------------+-------------------------------------------------------------+
| Updates disabled                     |  "jnlpool_ctl_struct.upd_disabled"                        | Integer - 1 means updates disabled; 0 means updates         |
|                                      |                                                           | permitted                                                   |
+--------------------------------------+-----------------------------------------------------------+-------------------------------------------------------------+

.. note::
   %PEEKBYNAME opens the -READ_ONLY help database as part of its operation, and in so doing causes each process using it to create a private semaphore. In addition, the first process to access a database creates an ftok related semaphore, which in the case of a -READ_ONLY database remains until deleted by a MUPIP RUNDOWN.

.. _xcmd-util:

+++++++++++++++++
%XCMD
+++++++++++++++++

The ^%XCMD utility XECUTEs input from the shell command line and returns any error status (truncated to a single byte on UNIX) generated by that code.

~~~~~~~~~~~~~~~
Utility Labels
~~~~~~~~~~~~~~~
.. code-block:: none

   LOOP^%XCMD [--before=/<XECUTE_code>/] --xec=/<XECUTE_code>/ [--after=/<XECUTE_code>/]

LOOP^%XCMD: XECUTEs the arguments specified in --xec=/arg1/ as YottaDB code for each line of standard input that it reads. The currently read line is stored in the variable %l; its line number is stored in %NR (starts from 1). It returns any error status (truncated to a single byte on UNIX) generated by that code.

--before=/arg0/ specifies the YottaDB code that LOOP^%XCMD must XECUTE before executing --xec.

--after=/arg2/ specifies the YottaDB code that LOOP^%XCMD must XECUTE after executing the last --xec.

For all qualifiers, always wrap YottaDB code specified two forward slashes (/) to denote the start and end of the YottaDB code. YottaDB strongly recommends enclosing the code within single quotation marks to prevent inappropriate expansion by the shell. LOOP^%XCMD's command line parser ignores these forward slashes.

Example:

.. code-block:: bash

   /usr/local/lib/yottadb/r120/ydb -run %XCMD 'write "hello world",!'


produces the following output:

.. code-block:: bash

   "hello world"

Example:

.. code-block:: bash

   $ ps -ef | $gtm_exe/yottadb -run LOOP^%XCMD --before='/set user=$ztrnlnm("USER") write "Number of processes owned by ",user," : "/' --xec='/if %l[user,$increment(x)/' --after='/write x,\!/'
   Number of processed owned by jdoe: 5
   $
   $ cat somefile.txt | $gtm_exe/yottadb -run LOOP^%XCMD --before='/write "Total number of lines : "/' --xec='/set total=$increment(x)/' --after='/write total,\!/'
   Total number of lines: 9
   $
   $ cat somefile.txt | $gtm_exe/yottadb -run LOOP^%XCMD --after='/write "Total number of lines : ",%NR,\!/'
   Total number of lines: 9
   $
   $ $gtm_exe/yottadb -run LOOP^%XCMD --before='/set f="somefile.txt" open f:readonly use f/' --after='/use $p write "Total number of lines in ",f,": ",%NR,\!/'
   Total number of lines in somefile.txt: 9
   $

.. _ydbjnlf-util:

++++++++++
%YDBJNLF
++++++++++

The %YDBJNLF utility routine loads journal extracts into global variables, allowing software to answer questions such as which process(es) updated a certain global, in what sequence and when; that global variable updates a process made; etc.

~~~~~~~~~~~~~~~
Utility Labels
~~~~~~~~~~~~~~~

INGEST^%YDBJNLF(jnlfile[,label]) uses `MUPIP JOURNAL EXTRACT FORWARD SHOW=ALL FENCES=NONE DETAIL FULL NOVERIFY <../AdminOpsGuide/ydbjournal.html#journal-selection-qualifiers>`_ to extract journal file jnlfile into global variables as described below. Since troubleshooting and forensics may need damaged journal files to be ingested, %YDBJNLF uses the NOVERIFY option.

  * If :code:`label` is specified, it is used to identify the extract; otherwise the journal file name :code:`jnlfile` is the identifying label.
  * INGEST deletes any existing :code:`^%ydbJNLF*(label)` global variables. Use a unique label for each call to INGEST if the journal file name is not unique, e.g., current generation journal files like :code:`yottadb.mjl`, because they will be renamed when the journal file is switched, and ingesting a subsequent file with same name will replace previously ingested data. For prior generation journal files, e.g., :code:`yottadb.mjl_2022341165323` using a label is less important as the file name is permanent. If you are ingesting a journal file from another machine, e.g., on a machine designated for forensics, then a label is required to distinguish journal files from different machines.

Global variables store both journal extract records as well as metadata. The general format of global variables used to store journal extract records is:

  * a prefix of :code:`^%ydbJNLF` and a suffix of the record type, as discussed below, e.g., :code:`^%ydbJNLFTYPE1`;
  * a first subscript, which is the label;
  * a second subscript, which is the offset in the journal file at which the record begins; and
  * a third subscript, which is the size of the record, in bytes.

Since journal files are written sequentially, the offset states definitively whether, for example, a SET was executed before or after a KILL.

The data at each node is a ``\`` separated record, as documented in `MUPIP JOURNAL EXTRACT formats <../AdminOpsGuide/ydbjournal.html#journal-extract-formats>`_ with the following changes to expedite analysis:

  * The first piece, preceding the record, is the record type, KILL, PBLK, PINI, SET, etc.
  * The `$HOROLOG <https://docs.yottadb.com/ProgrammersGuide/isv.html#horolog>`_ timestamp of the record is stored in two separate pieces, date and time.
  * :code:`node` fields in extract records are stored as two pieces, the global variable (e.g., :code:`^x`) and the complete global reference (e.g., :code:`^x(2)`).
  * :code:`node=sarg` fields in extract records are stored as three pieces, the global variable, the global reference, and the value. So :code:`^x(2)=ABC` would be stored as :code:`…\^x\^x(2)\"ABC"`.
  * Extracts for SET and KILL type records are all stored in the same :code:`^%ydbJNLFTYPE1` global variable, except that the KILL type records do not have the last piece, which stores the value for SET type records.

For example, the MUPIP JOURNAL EXTRACT format for SET records is:

.. code-block:: none

   time\tnum\chksum\pid\clntpid\token_seq\strm_num\strm_seq\updnum\nodeflags\node=sarg

^%ydbJNLFTYPE1 nodes store SET records as

.. code-block:: none

   op\horologdate\horologtime\tnum\chksum\pid\clntpid\token_seq\strm_num\strm_seq\updnum\nodeflags\gvname\gvref\nodeval

where :code:`op` is :code:`SET`.

Metadata for the journal extract data is stored in global variables as follows:

* :code:`^%ydbJNLF`, with records:

  * :code:`^%ydbJNLF(label,"extrfmt")` contains the format of the journal extract, e.g., :code:`"YDBJDX09 UTF-8"`.

  * :code:`^%ydbJNLF(label)` contains metadata about the extract, in semicolon (`;`) separated pieces, with strings quoted. Fields of interest are:

    * The journal file name, e.g., :code:`"/tmp/test/r1.36_x86_64/g/yottadb.mjl"`.
	* The journal file format, e.g., :code:`"YDBJNL44"`.
	* The database file name, e.g., :code:`"/tmp/test/r1.36_x86_64/g/yottadb.dat"`.
	* The prior generation journal file name, if any, e.g., :code:`"/tmp/test/r1.36_x86_64/g/yottadb.mjl_2022277114311"`.
	* A subsequent journal file name if any. This field is typically blank, except for journal files involved in a MUPIP JOURNAL BACKWARD RECOVER/ROLLBACK.
	* If before-image journaling is enabled, the string :code:`"ENABLED"`.
	* The remaining fields are primarily of interest to system administrators. Consult your YottaDB support channel or the source code for more information.

* :code:`^%ydbJNLFACTIVE(label,counter)` has records of processes that had the journal file open at the time of the EXTRACT, where counter is simply an incrementing integer. Semicolon separated pieces are as follows:

  * The process pid.
  * The node name of the computer, e.g., :code:`mylaptop`.
  * The user name of the process, e.g., :code:`ydbuser`.
  * The terminal or pseudo-terminal of the session, if any, e.g., :code:`3`.
  * The $HOROLOG date when the process opened the journal file, e.g., :code:`66386`.
  * The $HOROLOG time of day when the process opened the journal file, e.g., :code:`42207`.

* :code:`^%ydbJNLFCOMPLETE(label,counter)` has records of processes that previously had the journal file open, but no longer do. The fields are the same as for :code:`^%ydbJNLFACTIVE`, except that the pid may not be unique, in the event the operating system recycled pids while the journal file was active.

* :code:`^%ydbJNLFOPCOUNT(label,op)` has count of each opcode, e.g., if label is :code:`"TEST1"`, the node :code:`^%ydbJNLFOPCOUNT("TEST1","SET")` reports the number of SET records ingested.

* Since the number of record formats is smaller than the number of opcodes, e.g., all SET and KILL opcodes use the same record type, :code:`^%ydbJNLFRECTBL(format,op)` specifies the record type for each opode. For example, :code:`^%ydbJNLFRECTBL(44,"SET")="TYPE1"` means that global variables nodes that store ingested SET records where the :code:`format` was :code:`44` are :code:`^%ydbJNLFTYPE1("TEST1",offset,recsize)`. :code:`format` is the integer part of the journal format, e.g., 44.

If a YDBJNLF region does not exist in the global directory, INGEST creates an `AutoDB <../AdminOpsGuide/gde.html#no-au-todb>`_ region that uses the MM access method, mapped to database file :code:`%ydbjnlf.dat` in the same directory as the DEFAULT region (:code:`$ydb_dir/$ydb_rel/g` if the global directory was created by sourcing `ydb_env_set <../AdminOpsGuide/basicops.html#ydb-env-set>`_). Global variables of the form :code:`^%ydbJNLF` with all case combinations of :code:`JNLF` are mapped to the YDBJNLF region.

To avoid impacting response times in production environments, we recommend analyzing elsewhere the large journal files generated by production environments.

Although we have attempted to make INGEST as efficient as we can, ingesting large journal files is inherently not a fast operation and the time taken will be comparable to the time taken for the underlying MUPIP JOURNAL EXTRACT operation. When ingesting a journal file, avoid journaling the database region where `^%ydbJNLF*` global variables are mapped, as the journal file from ingesting a journal file is likely to be comparable to, and potentially even larger than, the journal file being ingested.

When ingesting journal files on systems where the corresponding database file is not present:

* If the database file does not use custom collation, set the `ydb_extract_nocol <../AdminOpsGuide/basicops.html#ydb-extract-nocol>`_ environment variable.
* If the database file does use custom collation, create an empty database file with the same filename as the original database, and with the same custom collation.

:code:`PURGE^%YDBJNLF(label)` purges all ingested %YDBJNLF data with the specified :code:`label`. If :code:`label` is omitted, it purges all ingested %YDBJNLF data.

~~~~~~~~~~
Octo DDL
~~~~~~~~~~

:code:`OCTODDL^%YDBJNLF([rectype])` outputs CREATE TABLE statements in a format suitable for Octo, which then allows the journal file data identified by label to be queried through Octo using SQL.
If rectype is omitted, OCTODDL, outputs CREATE TABLE statements for all record types and all metadata, e.g.,

.. code-block:: bash

   YDB>do OCTODDL^%YDBJNLF
   DROP TABLE IF EXISTS YDBJNLF KEEPDATA;
   CREATE TABLE YDBJNLF -- Metadata for ingested journal files
     (label VARCHAR,
     jnlfilename VARCHAR, -- Journal file name
     …
     openprocjpvtime INTEGER,
     PRIMARY KEY (label))
   Delim ";"
   Global "^%ydbJNLF";
   …
   DROP TABLE IF EXISTS YDBJNLFRECTYPE KEEPDATA;
   CREATE TABLE YDBJNLFRECTYPE -- Table for record types
     (format INTEGER,
     rectype VARCHAR,
     tbltype VARCHAR, -- table name is YDBJNLF followed by tbltype,
     PRIMARY KEY (format, rectype))
   GLOBAL "^%ydbJNLFRECTBL";

   YDB>


If :code:`rectype` is one or more opcodes, OCTODDL outputs the CREATE TABLE statements for those record types, e.g.,

.. code-block:: bash

   YDB>do OCTODDL^%YDBJNLF("SET,AIMG,KILL")
   DROP TABLE IF EXISTS YDBJNLFTYPE1 KEEPDATA;
   CREATE TABLE YDBJNLFTYPE1 -- FKILL,FSET,FZKILL,GKILL,GSET,GZKILL,KILL,SET,TKILL,TSET,TZKILL,TZTRIG,UKILL,USET,UZKILL,UZTRIG,ZKILL,ZTRIG
     (label VARCHAR,
     offset INTEGER,
     recsize INTEGER,
     op VARCHAR,
     horologdate INTEGER,
     horologtime INTEGER,
     tnum INTEGER,
     chksum INTEGER,
     pid INTEGER,
     clntpid INTEGER,
     token_seq INTEGER,
     strm_num INTEGER,
     strm_seq INTEGER,
     updnum INTEGER,
     nodeflags INTEGER,
     gvname VARCHAR,
     gvref VARCHAR,
     nodeval VARCHAR,
     PRIMARY KEY (label, offset, recsize))
   Delim "\"
   Global "^%ydbJNLFTYPE1";
   DROP TABLE IF EXISTS YDBJNLFTYPE5 KEEPDATA;
   CREATE TABLE YDBJNLFTYPE5 -- AIMG,PBLK
     (label VARCHAR,
     offset INTEGER,
     recsize INTEGER,
     op VARCHAR,
     horologdate INTEGER,
     horologtime INTEGER,
     tnum INTEGER,
     chksum INTEGER,
     pid INTEGER,
     clntpid INTEGER,
     blknum INTEGER,
     bsiz INTEGER,
     blkhdrtn INTEGER,
     ondskbver INTEGER,
     dsecmdline VARCHAR,
     PRIMARY KEY (label, offset, recsize))
   Delim "\"
   Global "^%ydbJNLFTYPE5";

   YDB>

.. _ydbprocstuckexec-util:

+++++++++++++++++++
%YDBPROCSTUCKEXEC
+++++++++++++++++++

%YDBPROCSTUCKEXEC is a standard utility program to capture diagnostics when invoked by the `ydb_procstuckexec <../AdminOpsGuide/basicops.html#ydb-procstuckexec-env-var>`_ mechanism. To use it, set the :code:`ydb_procstuckexec` environment variable to :code:`$ydb_dist/yottadb -run %YDBPROCSTUCKEXEC` with :code:`$ydb_dist` expanded to the actual directory where YottaDB is installed, or to :code:`yottadb -run %YDBPROCSTUCKEXEC` when :code:`yottadb` is located by :code:`$PATH`. Also :code:`$ydb_dist/libyottadbutil.so` must be in the routine search path defined by :code:`$ydb_routines` at process startup, or in the :code:`$ZROUTINES` intrinsic special variable.

When invoked by :code:`yottadb -run %YDBPROCSTUCKEXEC <msg> <callingpid> <blockingpid> <count>` as described in the ydb_procstuckexec documentation, it creates in the directory specified by :code:`$ydb_log`, :code:`$gtm_log`, :code:`$ydb_tmp`, or :code:`$gtm_tmp` (defaulting to :code:`/tmp`) a file whose name is :code:`%YDBPROCSTUCKEXEC_<date>,<time>_<msg>_<callingpid>_<blockingpid>_<count>_<%YDBPROCSTUCEXECpid>.out` where the , timestamp is generated by :ref:`zdate-function` with a format string of :code:`"YEAR.MM.DD,24.60.SS"`, e.g., :code:`%YDBPROCSTUCKEXEC_2020.05.22,17.58.38_ABCD_91998_91987_1_92045.out`.

The file contains:

 - A summary of the invocation, e.g., Invoked on 91987 by 91998 for 1st time; reason: MUTEXLCKALERT.
 - The command line of the blocking process.
 - The environment of the blocking process.
 - The value of /proc/sys/kernel/yama/ptrace_scope (if non-zero, gdb may not be able to capture the blocking process for a snapshot of its state).
 - A snapshot of the state of the blocking process, captured by gdb.
 - The value returned by :ref:`zsigproc-function` used to send SIGUSR1 (a `MUPIP INTRPT <../AdminOpsGuide/dbmgmt.html#mupip-intrpt>`_) to the blocking pid. If appropriately configured (with `ydb_zinterrupt <../AdminOpsGuide/basicops.html#ydb-zinterrupt-env-var>`_ / :ref:`zinterrupt-isv`, the blocking process will create a dump file of its process state.

If the calling pid and blocking pid are of different uids, the functionality of %YDBPROCSTUCKEXEC is reduced. Depending on security and system administration considerations of the system, it may be appropriate in such cases to create a small setuid root shell script to invoke %YDBPROCSTUCKEXEC. Depending your specific requirements, it may be appropriate to copy and adapt the standard routine for your environment.

%YDBPROCSTUCKEXEC was added to YottaDB effective release `r1.30 <https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.30>`_.

%YDBPROCSTUCKEXEC also captures the file header data. The global directory used for capturing the database file header is the :code:`$ydb_gbldir` of the blocked process, rather than either that of the blocking process or :code:`$ZGBLDIR` of the blocked process.

The :code:`%YDBPROCSTUCKEXEC_<date>,<time>_<msg>_<callingpid>_<blockingpid>_<count>_<%YDBPROCSTUCEXECpid>_dse.out` file contains the file header data whereas the :code:`%YDBPROCSTUCKEXEC_<date>,<time>_<msg>_<callingpid>_<blockingpid>_<count>_<%YDBPROCSTUCEXECpid>_dse.err` file contains any errors while getting the file header data. This functionality was added to YottaDB effective release `r1.36 <https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.36>`_.

.. _ygblstat-util:

++++++++++++++++++++
%YGBLSTAT()
++++++++++++++++++++

Labels in the ^%YGBLSTAT utility program gather and report statistics, offering both a high level API and a low level API. While we intend to preserve backward compatibility of the high level API in future YottaDB releases, we may change the low level API if and, when, we change the underlying implementation. A call to a label in ^%YGBLSTAT does not in any way slow the execution of other processes. Because the gathering of statistics is not instantaneous, and processes concurrently open database files as well as close them on exit and may turn their participation in statistics monitoring on and off, statistics typically do not show a single moment in time, as they change during the short time interval over which they are gathered.

In the following, an omitted response or argument is equivalent to "*".

The high level API implemented by $$STAT^%YGBLSTAT(expr1[,expr2[,expr3[,expr4]]]) reports global variable statistics and has arguments as follows:

* expr1 (treated as an intexpr - coercing an expr to an integer is equivalent to +(expr)) specifies the PID of a process on which to report; if such a process does not exist, has not opted in, or no database file mapped by expr3 and expr4 includes statistics for such a process, the function returns an empty string. Specifying "*" as the value of expr1 returns the aggregate statistic(s) specified by expr2 for all processes whose statistics are included in the database file(s) of the region(s) specified by expr4 within the global directory specified by expr3, or the empty string if there are no statistics to report for any process.

* expr2 specifies the statistic(s) to report as follows:

  * If expr2 is a single statistic, for example, "LKF", the function returns the requested value as an integer
  * If expr2 is a series of comma-separated names of statistics, for example., "GET,DTA", the function returns a string with each requested statistic in ZSHOW "G" order, for example, "GET:3289,DTA:598...", rather than in the order in which they appear within the specifying argument.
  * If expr2 is omitted, or consists of the string "*", the return value reports all statistics formatted like the ZSHOW "G" statistics for a single region, for example, "SET:563,KIL:39,GET:3289,DTA:598...
  * The ^%YGBLSTAT utility uses the local variable structure %YGS. Application code may hide %YGS with NEW but it must not modify nodes within it.

* expr3 specifies a global directory file name (producing a ZGBLDIRACC error if such a global directory is not accessible); if unspecified, the utility defaults this value to $ZGBLDIR of the invoking process.

* expr4 specifies the name of a region (producing a NOREGION error if no such region exists in the global directory expr3); if expr4 is unspecified, or the string "*", the function returns statistics for the process or processes summed across all regions of the global directory explicitly or implicitly specified by expr3.

When invoked as an interactive utility program using DO, ^%YGBLSTAT, prompts for:

* the process id (respond * for all processes).
* a comma separated list of the statistics desired (respond * for all statistics).
* the global directory to use.
* region (respond * to report statistics summed across all regions).

When invoked from a shell, the command line is:

.. code-block:: bash

   $ yottadb -run %YGBLSTAT [--help] [--pid pidlist] [--reg reglist] [--stat statlist]

where

* pidlist is a single pid, or "*" (quoted to protect it from expansion by the shell) for all processes currently sharing statistics.
* reglist is a single region name in the global directory specified by $ydb_gbldir, or "*" to report statistics summed across all regions.
* statlist is one or more comma separated statistics, or "*".
* When statlist specifies a list of statistics, %YGBLSTAT reports them in the same order in which ZSHOW "G" reports those statistics, rather than in the order in which they appear within the specifying argument.

^%YGBLSTAT returns an empty string for any process that has not shared statistics on any region sought by the invoking arguments. The most interesting class of such processes are probably non-YottaDB processes, but it also includes YottaDB processes that are not sharing.

$$IN^%YGBLSTAT(<pid>,<global directory>,<region>) is used to query whether a process is sharing statistics, returning a TRUE (1) value if the process is sharing statistics in the region, a FALSE (0) if it is not, and an empty string if the pid is invalid or there is no sharing for a region of the specified name. If region is empty or an asterisk, the extrinsic returns a TRUE if the process is sharing statistics in any region, and a FALSE otherwise. If the global directory is not empty the function attempts to use it, but if it is unavailable the function fails into the invoking environment's specified $ETRAP or $ZTRAP.

$$IN^%YGBLSTAT() was implemented effective release `r1.30 <https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.30>`_.

$$ORDERPID^%YGBLSTAT(expr1[,expr2[,expr3]]) reports PIDs of processes that have opted in and recorded statistics. Its arguments are as follows:

* expr1 coerced to an intexpr specifies a PID such that the function returns the next PID after expr1 of a process that has opted in to be monitored and which has recorded statistics in any region(s) specified by expr3 from the global directory specified by expr2, or the empty string if expr1 is the last PID. A value of the empty string ("") for expr1 returns the first monitored PID meeting the specifications in expr2 and expr3.
* expr2 specifies a global directory file name (producing a ZGBLDIRACC error if such a global directory is not accessible); if unspecified or the empty string, the utility defaults this value to the $ZGBLDIR of the invoking process.
* expr3 evaluates to the name of a region (producing a NOREGION error if no such region exists in the global directory specified by expr2); of expr3 is unspecified, or the string "*", the function returns the PID for the next process after expr1 for any region of the global directory specified by expr2.
* Applications should not rely on YottaDB returning the PIDs in a sorted or other predictable order: the order in which PIDs are returned is at the discretion of the implementation, and may change from release to release.

The low level API implemented by $$SHOW^%YGBLSTAT(glvn[,strexp]) reports raw statistics of a process and has arguments as follows:

* glvn specifies a node containing raw statistics for a process

* the raw data is stored in uniquely managed database files as ^%YGS(expr1,expr2) where:

  * expr1 evaluates to the name of a region in the current global directory (or the global directory of an extended reference), producing an UNDEF error, or, in NOUNDEF mode, an empty string, if no such region exists
  * expr2 coerced to an intexpr is a PID.
  * The data in the node is a series of binary bytes which are the raw statistics shared by a process

* strexp specifies statistics to report with the same interpretation as the expr2 parameter of $$STAT^%YGBLSTAT.

* $$SHOW^%YGBLSTAT() reports a zero value for any statistic whose name is unrecognized. This facilitates application code written for a version of YottaDB that includes a statistic, but which also needs to run on an earlier version without that statistic.

* Because a process sharing statistics can exit, deleting its node, between the time a monitoring process decides to access its statistics, e.g., finding it using $$ORDERPID^%YGBLSTAT() or $ORDER(^%YGS()), and the time the monitoring process performs the database access, any direct access to ^%YGBLSTAT should be wrapped in $GET().

* As raw statistics are binary data, processes in UTF-8 mode that gather and monitor statistics should use code with appropriate BADCHAR handling. Note that processes sharing statistics and processes gathering statistics for monitoring and reporting need not run in the same UTF-8/M mode. Statistics sharing by processes is identical in M and UTF-8 modes. YottaDB suggests that processes gathering statistics run in M mode

YottaDB strongly recommends that except as documented here for sharing and gathering statistics, you not access statistics database files except under the direction of your YottaDB support channel.

As they do for unshared statistics, shared statistics reflect all database actions for a TP transaction, including those during RESTARTs. Because the sharing of statistics is not a database operation that modifies the relative time stamps that YottaDB uses to maintain serialized operation preserving the Consistency and Isolation aspects of ACID operation, statistics generated by a sharing process inside a transaction (TSTART/TCOMMIT) do not cause transaction restarts as a consequence of updates to shared statistics by other processes.

.. note::
   See also: ydb_statsdir (which specifies the directory for database files into which processes that have opted-in to sharing global statistics place their statistics as binary data) and ydb_statshare (which specifies an initial value for the characteristic controlled by VIEW “[NO]STATSHARE” in application code) in the `Environment Variables <../AdminOpsGuide/basicops.html#env-vars>`_ section of the documentation. VIEW “[NO]STATSHARE”[:<region-list>] enables or disables database statistics sharing for listed regions which permit such sharing, more information :ref:`here <keywords-view-command>`.

----------------------------
UTF-8 Mode Utility Routines
----------------------------

The %UTF2HEX and %HEX2UTF M utility routines provide conversions between UTF-8 and hexadecimal code-point representations. Both these utilities run only in UTF-8 mode; in M mode, they both trigger a run-time error.

.. _utf2hex-util:

++++++++++++++++
%UTF2HEX
++++++++++++++++

The YottaDB %UTF2HEX utility returns the hexadecimal notation of the internal byte encoding of a UTF-8 encoded YottaDB character string. This routine has entry points for both interactive and non-interactive use.

DO ^%UTF2HEX converts the string stored in %S to the hexadecimal byte notation and stores the result in %U.

DO INT^%UTF2HEX converts the interactively entered string to the hexadecimal byte notation and stores the result in %U.

$$FUNC^%UTF2HEX(s) returns the hexadecimal byte representation of the character string s.

Example:

.. code-block:: bash

   YDB>write $zchset
   UTF-8
   YDB>SET %S=$CHAR($$FUNC^%HD("0905"))_$CHAR($$FUNC^%HD("091A"))_$CHAR($$FUNC^%HD("094D"))_$CHAR($$FUNC^%HD("091B"))_$CHAR($$FUNC^%HD("0940"))
   YDB>zwrite
   %S="अच्छी"
   YDB>DO ^%UTF2HEX
   YDB>zwrite
   %S="अच्छी"
   %U="E0A485E0A49AE0A58DE0A49BE0A580"
   YDB>write $$FUNC^%UTF2HEX("ABC")
   414243
   YDB>

Note that %UTF2HEX provides functionality similar to the UNIX binary dump utility (od -x).

.. _hex2utf-util:

++++++++++++++
%HEX2UTF
++++++++++++++

The YottaDB %HEX2UTF utility returns the YottaDB encoded character string from the given bytestream in hexadecimal notation. This routine has entry points for both interactive and non-interactive use.

DO ^%HEX2UTF converts the hexadecimal byte stream stored in %U into a YottaDB character string and stores the result in %S.

DO INT^%HEX2UTF converts the interactively entered hexadecimal byte stream into a YottaDB character string and stores the result in %S.

$$FUNC^%HEX2UTF(s) returns the YottaDB character string specified by the hexadecimal character values in s (each character is specified by its Unicode code-point).

Example:

.. code-block:: bash

   YDB>set u="E0A485" write $$FUNC^%HEX2UTF(u)
   अ
   YDB>set u="40E0A485" write $$FUNC^%HEX2UTF(u)
   @अ
   YDB>

-------------------------
Miscellaneous utilities
-------------------------

.. _zmvalid-util:

+++++++++
%ZMVALID
+++++++++

$$^%ZMVALID(str) creates a temporary file containing the string, compiles it, and returns the result of the compilation as its output. If the parameter :code:`str` is a valid line of M code, the output is the empty string. If :code:`str` contains some syntax error, the output is a non-empty string containing the full output of the compilation.

Note:

 * As the utility program prefixes :code:`str` with a space, the column in which an error is reported will be one greater than the actual byte position in the string.

 * The output string will contain linefeed characters. When using M code to write the output to a file or terminal, set $X to 0 after writing each linefeed, in order to prevent unexpected line wrapping.

Example:

.. code-block:: bash

   $ yottadb -run %XCMD 'set x=$$^%ZMVALID("write 0") zwrite x'
   x=""
   $  yottadb -run %XCMD 'set x=$$^%ZMVALID("writ 0") use $principal:(width=65535) write x'
            writ 0
            ^-----
                   At column 2, line 1, source module /tmp/ydbZMVALIDpid60450count1.m
   %YDB-E-INVCMD, Invalid command keyword encountered

-------------------------------
Utilities Summary Table
-------------------------------

+-----------------------------------+----------------------------------------------------------------------------------------------------------+
| Utility Name                      | Description                                                                                              |
+===================================+==========================================================================================================+
| :ref:`d-util`                     | Displays the current date in [d]d-mmm-[yy]yy format.                                                     |
+-----------------------------------+----------------------------------------------------------------------------------------------------------+
| :ref:`date-util`                  | Converts input date to $HOROLOG format.                                                                  |
+-----------------------------------+----------------------------------------------------------------------------------------------------------+
| :ref:`dh-util`                    | Converts decimal numbers to hexadecimal.                                                                 |
+-----------------------------------+----------------------------------------------------------------------------------------------------------+
| :ref:`do-util`                    | Converts decimal numbers to octal.                                                                       |
+-----------------------------------+----------------------------------------------------------------------------------------------------------+
| :ref:`dsewrap-util`               | Provides a programmatic interface that drives DSE either through a PIPE device or through generated      |
|                                   | command files                                                                                            |
+-----------------------------------+----------------------------------------------------------------------------------------------------------+
| :ref:`dumpfhead-util`             | Provides a programmatic interface to the functionality of MUPIP DUMPFHEAD                                |
+-----------------------------------+----------------------------------------------------------------------------------------------------------+
| :ref:`exp-util`                   | Raises number to the power of another number.                                                            |
+-----------------------------------+----------------------------------------------------------------------------------------------------------+
| :ref:`fl-util`                    | Lists comment lines at the beginning of the source programs.                                             |
+-----------------------------------+----------------------------------------------------------------------------------------------------------+
| :ref:`freecnt-util`               | Displays the number of free blocks in the database files associated with the current global directory.   |
+-----------------------------------+----------------------------------------------------------------------------------------------------------+
| :ref:`g-util`                     | Displays global variables and their values.                                                              |
+-----------------------------------+----------------------------------------------------------------------------------------------------------+
| :ref:`%GBLDEF <gbldef-util>`      | Manipulates the collation sequence assigned to a global.                                                 |
+-----------------------------------+----------------------------------------------------------------------------------------------------------+
| :ref:`gc-util`                    | Copies a global or global sub-tree.                                                                      |
+-----------------------------------+----------------------------------------------------------------------------------------------------------+
| :ref:`gce-util`                   | Replaces a specified value or part of a value in a set of global variables.                              |
+-----------------------------------+----------------------------------------------------------------------------------------------------------+
| :ref:`gd-util`                    | Displays existing globals in the current global directory without displaying their values or descendants.|
+-----------------------------------+----------------------------------------------------------------------------------------------------------+
| :ref:`ged-util`                   | Provides full-screen editing capabilities for global variables and values.                               |
+-----------------------------------+----------------------------------------------------------------------------------------------------------+
| :ref:`gi-util`                    | Enters global variables and their values from a sequential file into a database.                         |
+-----------------------------------+----------------------------------------------------------------------------------------------------------+
| :ref:`go-util`                    | Copies globals from the current database to a sequential output file.                                    |
+-----------------------------------+----------------------------------------------------------------------------------------------------------+
| :ref:`gse-util`                   | Displays global variables and their values when the values contain a specified string or number.         |
+-----------------------------------+----------------------------------------------------------------------------------------------------------+
| :ref:`gsel-util`                  | Selects globals by name.                                                                                 |
+-----------------------------------+----------------------------------------------------------------------------------------------------------+
| :ref:`h-util`                     | Converts date and time to and from $HOROLOG format.                                                      |
+-----------------------------------+----------------------------------------------------------------------------------------------------------+
| :ref:`hd-util`                    | Converts hexadecimal numbers to decimal.                                                                 |
+-----------------------------------+----------------------------------------------------------------------------------------------------------+
| :ref:`hex2utf-util`               | Converts the given bytestream in hexadecimal notation to YottaDB encoded character string.               |
+-----------------------------------+----------------------------------------------------------------------------------------------------------+
| :ref:`ho-util`                    | Converts hexadecimal numbers to octal.                                                                   |
+-----------------------------------+----------------------------------------------------------------------------------------------------------+
| :ref:`lcase-util`                 | Converts a string to all lower case.                                                                     |
+-----------------------------------+----------------------------------------------------------------------------------------------------------+
| :ref:`%LCLCOL <lclcol-util>`      | Manipulates the collation sequence assigned to local variables.                                          |
+-----------------------------------+----------------------------------------------------------------------------------------------------------+
| :ref:`mpiece-util`                | Replaces one or more consecutive occurrences of the second argument in the first argument, with the      |
|                                   | occurrence of the third argument.                                                                        |
+-----------------------------------+----------------------------------------------------------------------------------------------------------+
| :ref:`od-util`                    | Converts octal numbers to decimal.                                                                       |
+-----------------------------------+----------------------------------------------------------------------------------------------------------+
| :ref:`oh-util`                    | Converts octal numbers to hexadecimal.                                                                   |
+-----------------------------------+----------------------------------------------------------------------------------------------------------+
| :ref:`%PATCODE <patcode-util>`    | Loads pattern definition files for use within an active database.                                        |
+-----------------------------------+----------------------------------------------------------------------------------------------------------+
| :ref:`peekbyname-util`            | Provides a stable interface to $ZPEEK() that uses control structure field names.                         |
+-----------------------------------+----------------------------------------------------------------------------------------------------------+
| :ref:`randstr-util`               | Generates a random string.                                                                               |
+-----------------------------------+----------------------------------------------------------------------------------------------------------+
| :ref:`rce-util`                   | Replaces every occurrence of a text string with another string in a routine or list of routines.         |
+-----------------------------------+----------------------------------------------------------------------------------------------------------+
| :ref:`rd-util`                    | Lists routine names available through your $ZROUTINES search list.                                       |
+-----------------------------------+----------------------------------------------------------------------------------------------------------+
| :ref:`ri-util`                    | Transfers routines from ANSI sequential format into individual .m files in YottaDB format.               |
+-----------------------------------+----------------------------------------------------------------------------------------------------------+
| :ref:`ro-util`                    | Writes M routines in ANSI transfer format.                                                               |
+-----------------------------------+----------------------------------------------------------------------------------------------------------+
| :ref:`rse-util`                   | Searches for every occurrence of a text string in a routine or a list of routines.                       |
+-----------------------------------+----------------------------------------------------------------------------------------------------------+
| :ref:`rsel-util`                  | Selects M routines and places their directories and shared libraries and names in a local array.         |
+-----------------------------------+----------------------------------------------------------------------------------------------------------+
| :ref:`sqroot-util`                | Calculates the square root of a number.                                                                  |
+-----------------------------------+----------------------------------------------------------------------------------------------------------+
| :ref:`t-util`                     | Displays the current time in [h]h:mm AM/PM format.                                                       |
+-----------------------------------+----------------------------------------------------------------------------------------------------------+
| :ref:`ti-util`                    | Converts time to $HOROLOG format.                                                                        |
+-----------------------------------+----------------------------------------------------------------------------------------------------------+
| :ref:`to-util`                    | Converts the current time from $HOROLOG format to [h]h:mm AM/PM format.                                  |
+-----------------------------------+----------------------------------------------------------------------------------------------------------+
| :ref:`trim-util`                  | Removes leading and trailing characters from a string.                                                   |
+-----------------------------------+----------------------------------------------------------------------------------------------------------+
| :ref:`ucase-util`                 | Converts a string to all upper case.                                                                     |
+-----------------------------------+----------------------------------------------------------------------------------------------------------+
| :ref:`utf2hex-util`               | Converts UTF-8 encoded YottaDB character string to bytestream in hexadecimal notation.                   |
+-----------------------------------+----------------------------------------------------------------------------------------------------------+
| :ref:`xcmd-util`                  | XECUTEs input from the shell command line and returns any error status generated by the code.            |
+-----------------------------------+----------------------------------------------------------------------------------------------------------+
| :ref:`ydbjnlf-util`               | Provides functionality to simplify analytics and forensics using journal files.                          |
+-----------------------------------+----------------------------------------------------------------------------------------------------------+
| :ref:`ydbprocstuckexec-util`      | Captures diagnostics when invoked by the                                                                 |
|                                   | `ydb_procstuckexec <../AdminOpsGuide/basicops.html#ydb-procstuckexec-env-var>`_ mechanism.               |
+-----------------------------------+----------------------------------------------------------------------------------------------------------+
| :ref:`ygblstat-util`              | Gathers and reports statistics, offering a high level and low level API.                                 |
+-----------------------------------+----------------------------------------------------------------------------------------------------------+
| :ref:`zmvalid-util`               | Checks whether a string is a syntactically correct line of M code.                                       |
+-----------------------------------+----------------------------------------------------------------------------------------------------------+
| :ref:`zshowvtolcl-util`           | Restores ZSHOW "V":gvn data into its original local variables.                                           |
+-----------------------------------+----------------------------------------------------------------------------------------------------------+
