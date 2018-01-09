
.. index::
   Utility Routines

======================
Utility Routines
======================

.. contents::
   :depth: 2

YottaDB/GT.M provides library utilities to perform frequently used tasks, and to access frequently used information. Most of the utilities are for YottaDB/GT.M programmers, but some provide tools for system administration and operation.

The YottaDB/GT.M utilities fall into the following general categories:

* Date and time utilities
* Conversion utilities
* Mathematic utilities
* Global utilities
* Routine utilities
* Internationalization utilities
* System Management utilities
* Unicode Utility Routines

The YottaDB/GT.M distribution includes the source files for these utilities. The default installation compiles them to produce object modules in the $gtm_dist distribution library.

You may wish to examine the utilities and include some of them in your programs if the programs access the function frequently or you may want to modify the utilities to better fit your particular needs. If you modify a utility, store your copy in a directory that precedes gtm_dist in the search list $ZROUTINES to prevent a new release of YottaDB/GT.M from overwriting your copy.

-------------------------------
Using the Utilities
-------------------------------

You can either use a utility in Direct Mode or include it in a source application program with one or more of the following formats. 

* DO ^%UTILITYNAME
* DO LABEL^%UTILITYNAME
* $$FUNC^%UTILITYNAME[(para1,...)]

Many utilities contain labels that invoke variations of the basic utility functionality. Some also provide the label FUNC to invoke an extrinsic function with optional or required parameters.

YottaDB/GT.M passes input to non-extrinsic forms of the utilities interactively or by using "input" variables. YottaDB/GT.M passes output from non-extrinsic forms of the utilities using "output" variables. For extrinsic entry points, the utilities receive input as parameters and pass output as the returned result. For other entry points, YottaDB/GT.M uses predefined "input" and "output" variables to pass information. Some utilities interactively request user inputs and display their results. Each utility is described individually in this chapter where appropriate labels, input, and output variables are identified.

By convention, the utilities use upper-case variables for external input and output. Since M is case-sensitive, when an invocation uses a lower-case or misspelled variable name, the routine does not output the expected information. Instead it supplies a default value, if one exists, or produces an error message.

Example:

.. parsed-literal::
   GTM>SET %ds="11/22/2010"
   GTM>DO INT^%DATE
   GTM>ZWRITE
   %DN=62047
   %ds="11/22/2010"

This example sets the lowercase variable %ds to the date 11/22/2010. Since the %DATE routine expects the input to be provided in the uppercase %DS variable, it returns a default value in the output variable $DN. The default is the $HOROLOG format of the current date, which is 11/17/2010 in the example.

.. note::
   Utility programs written in M (such as %GO) run within mumps processes and behave like any other code written in M. Encryption keys are required if the mumps process accesses encrypted databases. A process running a utility program written in M that does not access encrypted databases (such as %RSEL) does not need encryption keys just to run the utility program.

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

+++
%D
+++

The %D utility displays the current date using the [d]d-mmm-[yy]yy format. If a routine uses this function repetitively, put the utility code directly into the M program.

**Utility Labels**

INT: Sets variable %DAT to current date.

FUNC[()]: Invokes an extrinsic function returning today's date.

**Output Variables**

%DAT: Contains the current date..

**Examples of %D**

For the following examples, $ZDATEFORM is assumed to be one (1).

Example:

.. parsed-literal::
   GTM>DO ^%D
   22-NOV-2010

This example invokes %D in Direct Mode. Then %D displays the current date.

Example:

.. parsed-literal::
   GTM>DO INT^%D
   GTM>ZWRITE
   %DAT="22-NOV-2010"

This example invokes %D with the label INT (INT^%D). The variable %DAT contains the current date. ZWRITE displays the contents of the output variable.

Example:

.. parsed-literal::
   GTM>WRITE $$FUNC^%D 
   22-NOV-2010 

This example invokes %D as an extrinsic function with the label FUNC. $$FUNC^%D returns today's date.

++++++++++++
%DATE
++++++++++++

The %DATE utility converts an input date to the $HOROLOG format. The $HOROLOG format represents time as the number of days since December 31, 1840. The routine has entry points for interactive or non-interactive use.

**Utility Labels**

INT: Converts %DS input non-interactively, if defined, otherwise the current date.

FUNC(t): Invokes an extrinsic function returning $HOROLOG format of the argument.

**Prompts**

Date: Interactively requests a date for conversion to $HOROLOG format.

**Input Variables**

%DS: Contains input date; refer to %DATE Input Formats table.

**Output Variables**

%DN: Contains output date in $HOROLOG format

**DATE Input Formats Table**

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

**Examples of %DATE**

Example:

.. parsed-literal::
   GTM>DO ^%DATE
   Date:
   GTM>ZWRITE
   %DN=62047

This example invokes %DATE at the GTM> prompt. After pressing <RETURN> at the Date: prompt, %DATE converts today's date (for example, 11/22/2010) to the $HOROLOG format. ZWRITE displays the contents of the output variable.

Example:

.. parsed-literal::
   GTM>DO INT^%DATE
   GTM>ZWRITE
   %DN=59105

This example invokes INT^%DATE, which converts the current date non-interactively into $HOROLOG format. ZWRITE displays the contents of the output variable.

Example:

.. parsed-literal::
   GTM>SET %DS="10/20/2010"
   GTM>DO INT^%DATE
   GTM>ZWRITE
   %DN=62019
   %DS="10/20/2010"

This example sets the input variable %DS prior to invoking INT^%DATE, which converts that date non-interactively to $HOROLOG format.

Example:

.. parsed-literal::
   GTM>WRITE $$FUNC^%DATE("10/20/2010")
   62010 

This example invokes %DATE with the label FUNC as an extrinsic function to convert an input date to $HOROLOG. If the invocation does not supply a date for $$FUNC^%DATE, FUNC converts the current date.

Example:

.. parsed-literal::
   GTM>WRITE $ZDATEFORM
   1975
   GTM>WRITE $$FUNC^%DATE("10/20/80")
   51062
   GTM>WRITE $ZDATE(51062)
   10/20/1980
   GTM>WRITE $$FUNC^%DATE("10/20/10")
   62019
   GTM>WRITE $ZDATE(62019)
   10/20/2010

This example shows the use of a year limit in $ZDATEFORM. Two digit years are interpreted to be in the interval (1975, 2074) since $ZDATEFORM is 1975; the input year "80" is interpreted as the year "1980" and "10" is interpreted as the year "2010". The example invokes FUNC^%DATE to convert the input date to $HOROLOG format. $ZDATE() is used to convert the $HOROLOG format date to mm/dd/yyyy format.

+++++++++
%H
+++++++++

The %H utility converts date and time to and from $HOROLOG format.

**Utility Labels**

%CDS: Converts %DT $HOROLOG input date to mm/dd/yyyy format.

%CTS: Converts %TM $HOROLOG input time to external format.

%CDN: Converts %DT input date to $HOROLOG format.

%CTN: Converts %TM input time to $HOROLOG format.

CDS(dt): Extrinsic entry that converts the $HOROLOG argument to external date format.

CTS(tm): Extrinsic entry that converts the $HOROLOG argument to external time format.

CDN(dt): Extrinsic entry that converts the argument to $HOROLOG format.

CTN(tm): Extrinsic entry that converts the argument to $HOROLOG format.

**Input Variables**

%DT: Contains input date in either $HOROLOG or mm/dd/[yy]yy format, depending on the format expected by the utility entry point.

%TM: Contains input time in either $HOROLOG or [h]h:mm:ss format, depending on the format expected by the utility entry point.

**Output Variables**

%DAT: Contains converted output date,

%TIM: Contains converted output time,

**Examples of %H**

Example:

.. parsed-literal::
   GTM>SET %DT=+$H DO %CDS^%H
   GTM>ZWRITE
   %DAT="10/20/2010"
   %DT=62047

This example sets %DT to the current date in $HOROLOG format and converts it to mm/dd/yyyy format by invoking %H at the label %CDS. %H returns the converted date in the variable %DAT. ZWRITE displays the contents of the variables.

Example:

.. parsed-literal::
   GTM>SET %DT="10/20/2002" DO %CDN^%H
   GTM>ZWRITE
   %DAT=59097
   %DT="10/20/2002"

This example sets the variable %DT to a date in mm/dd/yyyy format and invokes %H at the label %CDN. %H returns the converted date in the variable %DAT. ZWRITE displays the contents of the variables.

Example:

.. parsed-literal::
   GTM>SET %TM=$P($H,",",2) DO %CTS^%H
   GTM>ZWRITE
   %TIM="17:41:18" 
   %TM=63678

This example sets the variable %TM to the current time in $HOROLOG format using a $PIECE() function to return only those digits of the $HOROLOG string that represent the time. The example then invokes %H at the label %CTS. %H returns the converted time in the variable %TIM. ZWRITE displays the contents of the variables.

Example:

.. parsed-literal::
   GTM>SET %TM="17:41:18" DO %CTN^%H
   GTM>ZWRITE
   %TIM=63678
   %TM="17:41:18"

This example sets the variable %TM to a time in hh:mm:ss format, and invokes %H at the label %CTN. %H returns the converted time in the variable %TIM. ZWRITE displays the contents of the variables.

Example:

.. parsed-literal::
   GTM>WRITE $$CDS^%H(62019)
   11/17/2010 

This invokes CDS^%H as an extrinsic function to convert the external argument to external date format.

Example:

.. parsed-literal::
   GTM>WRITE $ZDATEFORM
   1980
   GTM>WRITE $$CDN^%H("10/20/02")
   59097
   GTM>WRITE $ZDATE(59097)
   10/20/2002
   GTM>WRITE $$CDN^%H("10/20/92")
   55445
   GTM>WRITE $ZDATE(55445)
   10/20/1992 

This example shows the use of a year limit in $ZDATEFORM. Two digit years are interpreted to be in the interval of 1980 - 2079; since $ZDATEFORM is 1980, the input year "02" is interpreted as "2002" and "92" is interpreted as "1992". This example invokes CDN^%H to convert the argument in mm/dd/yy format to $HOROLOG format. $ZDATE() is used to conver the $HOROLOG format date to mm/dd/yyyy format.

++++
%T
++++

The %T utility displays the current time in [h]h:mm AM/PM. If a routine uses this function repetitively, put the utility code directly into the M program.

**Utility Labels**

INT: Sets %TIM to current time in [h]h:mm AM/PM format.

FUNC[()]: Invokes an extrinsic function returning the current time.

**Output Variables**

%TIM: Contains current time in [h]h:mm AM/PM format.

**Examples of %T**

Example:

.. parsed-literal::
   GTM>DO ^%T
   8:30 AM

This example invokes %T, which prints the current time and does not set %TIM.

Example:

.. parsed-literal::
   GTM>DO INT^%T
   GTM>ZWRITE
   %TIM="8:30 AM"

This example invokes INT^%T, which sets the variable %TIM to the current time. ZWRITE displays the contents of the variable.

Example:

.. parsed-literal::
   GTM>WRITE $$FUNC^%T
   8:30 AM 

This example invokes FUNC as an extrinsic function, which returns the current time.


