.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2018-2022 YottaDB LLC and/or its subsidiaries.#
.. # All rights reserved.                                        #
.. #                                                             #
.. #     This source code contains the intellectual property     #
.. #     of its copyright holder(s), and is made available       #
.. #     under a license.  If you do not know the terms of       #
.. #     the license, please stop and do not read further.       #
.. #                                                             #
.. ###############################################################

.. index::
   Functions

=======================
7. Functions
=======================

.. contents::
   :depth: 2

This chapter describes M language Intrinsic Functions implemented in YottaDB. Traditional string processing functions have parallel functions that start with the letter "z". The parallel functions extend the byte-oriented functionality of their counterparts to UTF-8 mode. They are helpful when applications need to process binary data including blobs, binary byte streams, bit-masks, and so on.

Other functions that start with the letter "z" and do not have counterparts implement new functionality and are YottaDB additions to the ANSI standard Intrinsic Functions. The M standard specifies standard abbreviations for Intrinsic Functions and rejects any non-standard abbreviations.

M Intrinsic Functions start with a single dollar sign ($) and have one or more arguments enclosed in parentheses () and separated by commas (,). These functions provide expression results by performing actions that are impossible or difficult to perform using M commands.

.. _ascii-function:

-----------------
$ASCII()
-----------------

Returns the integer ASCII code for a character in the given string. For a yottadb process started in UTF-8 mode, $ASCII() returns the integer Unicode® UTF-8 code-point value of a character in the given string.

The format for the $ASCII function is:

.. code-block:: none

   $A[SCII](expr[,intexpr])

* The expression is the source string from which $ASCII() extracts the character it decodes.
* intexpr contains the position within the expression of the character that $ASCII() decodes. If intexpr is missing, $ASCII() returns a result based on the first character position.
* If intexpr evaluates to before the beginning or after the end of the expression, $ASCII() returns a value of -1.

$ASCII() provides a means of examining non-graphic characters in a string. When used with $CHAR(), $ASCII() also provides a means to perform arithmetic operations on the codes associated with characters.

$ZASCII() is the parallel function of $ASCII(). $ZASCII() interprets the string argument as a sequence of bytes (rather than a sequence of characters) and can perform all byte-oriented $ASCII() operations. For more information, refer to :ref:`zascii-function`.

++++++++++++++++++++
Examples of $ASCII()
++++++++++++++++++++

Example:

.. code-block:: bash

   YDB>For i=0:1:3 Write !,$Ascii("Hi",i)
   -1
   72
   73
   -1
   YDB>

This loop displays the result of $ASCII() specifying a character position before, first and second positions, and after the string.

Example:

.. code-block:: bash

   YDB>Write $ZCHSET
   UTF-8
   YDB>Write $Ascii("主")
   20027
   YDB>Write $$FUNC^%DH("20027")
   00004E3B

In this example, 20027 is the integer equivalent of the hexadecimal value 4E3B. U+4E3B is a character in the CJK Ideograph block of the Unicode® Standard.

.. _char-function:

-----------------
$CHAR()
-----------------

Returns a string of one or more characters corresponding to integer ASCII codes specified in its argument(s). For a process started in UTF-8 mode, $CHAR() returns a string composed of characters represented by the integer equivalents of the Unicode® code-points specified in its argument(s).

The format for the $CHAR function is:

.. code-block:: none

   $C[HAR](intexpr[,...])

* The integer expression(s) specify the codes of the character(s) $CHAR() returns.
* The M standard does not restrict the number of arguments to $CHAR(). However, YottaDB does limit the number of arguments to a maximum of 254. $CHAR() provides a means of producing non-graphic characters, as such characters cannot appear directly within an M string literal. When used with $ASCII(), $CHAR() can also perform arithmetic operations on the codes associated with characters.
* With VIEW "BADCHAR" enabled, $CHAR() produces a run-time error if any expression evaluates to a code-point value that is not a character in Unicode. YottaDB determines from ICU which characters are illegal.
* $ZCHAR() is the parallel function of $CHAR(). $ZCHAR() returns a sequence of bytes (rather than a sequence of characters) and can perform all byte-oriented $CHAR() operations. For more information, refer to :ref:`zchar-function`.

++++++++++++++++++++
Examples of $CHAR()
++++++++++++++++++++

Example:

.. code-block:: bash

   YDB>write $char(77,7)
   M
   YDB>

This example uses $CHAR() to WRITE the letter M and signal the terminal "bell."

Example:

.. code-block:: none

   set nam=$extract(nam,1,$length(nam)-1)_$char($ascii(nam,$length(nam))-1)

This example uses $CHAR() and $ASCII() to set the variable nam to a value that immediately precedes its previous value in the set of strings of the same length as nam.

Example:

.. code-block:: bash

   YDB>write $zchset
   UTF-8
   YDB>write $char(20027)
   主
   YDB>write $char(65)
   A

In the above example, the integer value 20027 is the Unicode® character "主" in the CJK Ideograph block of Unicode. Note that the output of the $CHAR() function for values of integer expression(s) from 0 through 127 does not vary with choice of the character encoding scheme. This is because 7-bit ASCII is a proper subset of UTF-8 character encoding scheme. The representation of characters returned by the $CHAR() function for values 128 through 255 differ for each character encoding scheme.

----------------
$DATA()
----------------

Returns an integer code describing the value and descendent status of a local or global variable.

The format for the $DATA function is:

.. code-block:: none

   $D[ATA](glvn)

* The subscripted or unsubscripted global or local variable name specifies the target node.
* If the variable is undefined, $DATA() returns 0.
* If the variable has a value but no descendants, $DATA() returns 1.
* If the variable has descendants but no value, $DATA() returns 10.
* If the variable has a value and descendants, $DATA() returns 11.
* $ZDATA() extends $DATA() to reflect the current alias state of the lvn or name argument to identify alias and alias container variables. For more information, refer to :ref:`zdata-function`.

The following table summarizes $DATA() return values.

++++++++++++++++
$DATA() Results
++++++++++++++++

+----------+---------------------+-------------------+
| Value    | Descendants (No)    | Descendants (Yes) |
+==========+=====================+===================+
| NO       | 0                   | 10                |
+----------+---------------------+-------------------+
| YES      | 1                   | 11                |
+----------+---------------------+-------------------+

$DATA() return values can also be understood as a pair of truth-values where the left describes descendants and the right describes data and where M suppresses any leading zero (representing no descendants).

+++++++++++++++++++++
Examples of $DATA()
+++++++++++++++++++++

Example:

.. code-block:: bash

   YDB>Kill  Write $Data(a)
   0
   YDB>Set a(1)=1 Write $Data(a(1))
   1
   YDB>Write $Data(a)
   10
   YDB>Set a=0 Write $Data(a)
   11
   YDB>

This uses $DATA to display all possible $DATA() results.

Example:

.. code-block:: none

   lock ^ACCT(0)
   if '$data(^ACCT(0)) set ^ACCT(0)=0
   set (ACCT,^ACCT(0))=^ACCT(0)+1
   lock

This uses $DATA() to determine whether a global node requires initialization.

Example:

.. code-block:: none

   for  set cus=$O(^cus(cus)) quit:cus=""  if $data(^(cus))>1 do WORK

This uses $DATA() to determine whether a global node has descendants and requires additional processing.

.. _extract-function:

-------------------
$EXTRACT()
-------------------

Returns a substring of a given string.

The format for the $EXTRACT function is:

.. code-block:: none

   $E[XTRACT](expr[,intexpr1[,intexpr2]])

* The expression specifies a string from which $EXTRACT() derives a substring.
* The first optional integer expression (second argument) specifies the starting character position in the string. If the starting position is beyond the end of the expression, $EXTRACT() returns an empty string. If the starting position is zero (0) or negative, $EXTRACT() starts at the first character; if this argument is omitted, $EXTRACT() returns the first character of the expression. $EXTRACT() numbers character positions starting at one (1) (that is, the first character of a string is at position one (1)).
* The second optional integer expression (third argument) specifies the ending character position for the result. If the ending position is beyond the end of the expression, $EXTRACT() stops with the last character of the expression. If the ending position precedes the starting position, $EXTRACT() returns an empty string. If this argument is omitted, $EXTRACT() returns one character at most.

$EXTRACT() provides a tool for manipulating strings based on character positions.

For a yottadb process started in UTF-mode, $EXTRACT interprets the string arguments as UTF-8 encoded. With VIEW "BADCHAR" enabled, $EXTRACT() produces a run-time error when it encounters a character in the reserved range of the Unicode® Standard, but it does not process the characters that fall after the span specified by the arguments. The parallel function of $EXTRACT() is $ZEXTRACT(). Use $ZEXTRACT() for byte-oriented operations. For more information, refer to :ref:`zextract-function`.

$EXTRACT() can be used on the left-hand side of the equal sign (=) of a SET command to set a substring of a string. This construct permits easy maintenance of individual pieces within a string. It can also be used to right justify a value padded with blank characters. For more information on SET $EXTRACT(), refer to :ref:`set-command`.

++++++++++++++++++++++
Examples of $EXTRACT()
++++++++++++++++++++++

Example:

.. code-block:: bash

   YDB>for i=0:1:3 write !,$extract("HI",i),"<"
   <
   H<
   I<
   <
   YDB>

This loop displays the result of $EXTRACT(), specifying no ending character position and a beginning character position "before" first and second positions, and "after" the string.

Example:

.. code-block:: bash

   YDB>For i=0:1:3 write !,$extract("HI",1,i),"<"
   <
   H<
   HI<
   HI<
   YDB>

This loop displays the result of $EXTRACT() specifying a beginning character position of 1 and an ending character position "before, " first and second positions, and "after" the string.

Example:

.. code-block:: bash

   YDB>zprint ^trim
   trim(x)
       new i,j
       for i=1:1:$length(x) quit:" "'=$extract(x,i)
       for j=$length(x):-1:1 quit:" "'=$extract(x,j)
       quit $extract(x,i,j)
   YDB>set str=" M "
   YDB>write $length(str)
   3
   YDB>write $length($$^trim(str))
   1
   YDB>

This extrinsic function uses $EXTRACT() to remove extra leading and trailing spaces from its argument.

.. _find-function:

------------------
$FIND()
------------------

Returns an integer character position that locates the occurrence of a substring within a string.

The format for the $FIND function is:

.. code-block:: none

   $F[IND](expr1,expr2[,intexpr])

* The first expression specifies the string within which $FIND() searches for the substring.
* The second expression specifies the substring for which $FIND() searches.
* The optional integer expression identifies the starting position for the $FIND() search. If this argument is missing, zero (0), or negative, $FIND() begins its search in the first position of the string.
* If $FIND() locates the substring, it returns the position after the last character of the substring. If the end of the substring coincides with the end of the string (expr1), it returns an integer equal to the length of the string plus one ($L(expr1)+1).
* If $FIND() does not locate the substring, it returns zero (0).
* For a process started in UTF-8 mode, $FIND() interprets the string arguments as UTF-8 encoded. With VIEW "BADCHAR" enabled, $FIND() produces a run-time error when it encounters a malformed character, but it does not process the characters that fall after the span specified by the arguments.
* $ZFIND() is the Z equivalent function $FIND(). Irrespective of the settings of VIEW "BADCHAR" and $ZCHSET, $ZFIND() interprets argument as a sequence of bytes (rather than a sequence of characters) and can perform byte-oriented $FIND() operations. For more information, refer to :ref:`zfind-function`.

$FIND() provides a tool to locate substrings. The ([) operator and the two-argument $LENGTH() are other tools that provide related functionality.

+++++++++++++++++++++
Examples of $FIND()
+++++++++++++++++++++

Example:

.. code-block:: bash

   YDB>write $find("HIFI","I")
   3
   YDB>

This example uses $FIND() to WRITE the position of the first occurrence of the character "I." The return of 3 gives the position after the "found" substring.

Example:

.. code-block:: bash

   YDB>write $find("HIFI","I",3)
   5
   YDB>

This example uses $FIND() to WRITE the position of the next occurrence of the character "I" starting in character position three.

Example:

.. code-block:: none

   YDB>set t=1 for  set t=$find("BANANA","AN",t) quit:'t  write !,t
   4
   6
   YDB>

This example uses a loop with $FIND() to locate all occurrences of "AN" in "BANANA". $FIND() returns 4 and 6 giving the positions after the two occurrences of "AN".

Example:

.. code-block:: bash

   YDB>set str="M databases are hierarchical"
   YDB>Write $find(str," ")
   3
   YDB>Write $find(str,"Z")
   0
   YDB>Write $find(str,"d",1)
   4
   YDB>Write $find(str,"d",10)
   0

The above example searches a string for a sub string, and returns an integer value which corresponds to the next character position after locating the sub string.

----------------------
$FNUMBER()
----------------------

Returns a string containing a formatted number.

The format for the $FNUMBER function is:

.. code-block:: none

   $FN[UMBER](numexpr,expr[,intexpr])

* The numeric expression specifies the number that $FNUMBER() formats.
* The expression (second argument) specifies zero or more single character format control codes; if the expression contains any character other than the defined codes, $FNUMBER() generates a run-time error.
* The optional integer expression (third argument) specifies the number of digits after the decimal point. If the numeric expression has more digits than specified by this argument, $FNUMBER() rounds to obtain the result. If the numeric expression has fewer digits than specified by this argument, $FNUMBER() zero-fills to obtain the result.
* When the optional third argument is specified and the first argument evaluates to a fraction between -1 and 1, $FNUMBER() returns a number with a leading zero (0) before the decimal point (.).

$FNUMBER() formats or edits numbers, usually for reporting. For more information on rounding performed by $FNUMBER(), refer to :ref:`justify-function`.

The formatting codes are:

* \+ : Forces a "+" on positive values.
* \- : Suppresses the "-" on negative values.
* , : Inserts commas every third position to the left of the decimal within the number.
* . : inserts periods (".") every third position to the left of the decimal within the number, and uses a comma (",") as the decimal separator.
* T : Represents the number with a trailing, rather than a leading sign; positive numbers have a trailing space unless the expression includes a plus sign (+).
* P : Represents negative values in parentheses, positive values with a space on either side; combining with any other code except a comma (,) or a period (.) causes a run-time error.

+++++++++++++++++++++++
Examples of $FNUMBER()
+++++++++++++++++++++++

Example:

.. code-block:: bash

   YDB>do ^fnum
   fnum;
     zprint ^fnum
     set X=-100000,Y=2000
     write "SUPPRESS NEGATIVE SIGN:",?35,$FNumber(X,"-"),!
     write "TRAILING SIGN:",?35,$FNumber(X,"T"),!
     write "NEGATIVE NUMBERS IN ():",?35,$FNumber(X,"P"),!
     write "COMMAS IN NUMBER:",?35,$FNumber(X,","),!
     write "NUMBER WITH FRACTION:",?35,$FNumber(X,"",2),!
     write "FORCE + SIGN IF POSITIVE:",?35,$FNumber(Y,"+"),!
   SUPPRESS NEGATIVE SIGN:            100000
   TRAILING SIGN:                     100000-
   NEGATIVE NUMBERS IN ():            (100000)
   COMMAS IN NUMBER:                  -100,000
   NUMBER WITH FRACTION:              -100000.00
   FORCE + SIGN IF POSITIVE:          +2000

Example:

.. code-block:: none

   set x=$fnumber(x,"-")

This example uses $FNUMBER() to SET x equal to its absolute value.


------------------
$GET()
------------------

Returns the value of a local or global variable if the variable has a value. If the variable has no value, the function returns a value specified by an optional second argument, and otherwise returns an empty string.

The format for the $GET function is:

.. code-block:: none

   $G[ET](glvn[,expr])

* The subscripted or unsubscripted global or local variable name specifies the node for which $GET() returns a value.
* If the global or local variable has a data value, $GET() returns the value of the variable.
* If the global or local variable has no data value, $GET() returns the value of the optional expression (second argument), or an empty string if the expression is not specified.

M defines $GET(x,y) as equivalent to:

.. code-block:: none

   $Select($Data(x)[0:y,1:x)

and $GET(x) as equivalent to:

.. code-block:: none

   $GET(x,"")

$GET() provides a tool to eliminate separate initialization of variables. This technique may provide performance benefits when used to increase the density of a sparse global array by eliminating nodes that would otherwise hold absent optional information. On the other hand, some uses of one argument $GET() can mask logic problems.

YottaDB has a "NOUNDEF" mode of operation, which treats all variable references as if they were arguments to a one argument $GET(). The VIEW command controls "NOUNDEF" mode.

+++++++++++++++++++
Examples of $GET()
+++++++++++++++++++

Example:

.. code-block:: none

   setstatus;
            if '$data(^PNT(NAME,TSTR)) set STATUS="NEW TEST"
            else  if ^PNT(NAME,TSTR)="" set STATUS="WAITING FOR RESULT"
            else  set STATUS=^PNT(NAME,TSTR)

This example can be reduced to two lines of code by using $GET(), shown in the following example. However, by using $GET() in its one-argument form, the distinction between an undefined variable and one with a null value is lost:

.. code-block:: none

   set STATUS=$get(^PNT(NAME,TSTR))
   if STATUS="" set STATUS="WAITING FOR RESULT"

This is solved by using the two-argument form of $GET():

.. code-block:: none

   set STATUS=$get(^PNT(NAME,TSTR),"NEW TEST")
   if STATUS="" set STATUS="WAITING FOR RESULT"

------------------
$INCREMENT()
------------------

Atomically adds (increments) a global variable by a numeric value. Note that increment is atomic, but the evaluation of the expression is not, unless inside a transaction (TStart/TCommit). The function also works on local variables, but has less benefit for locals as it does not (need to) provide ACID behavior.

The format of the $INCREMENT function is:

.. code-block:: none

   $INCREMENT(glvn[,numexpr])

* $I, $INCR, $INCREMENT, $ZINCR, and $ZINCREMENT are considered as valid synonyms of the full function name.
* $INCREMENT() returns the value of the glvn after the increment.
* If not specified, numexpr defaults to 1. Otherwise, $INCREMENT() evaluates the "numexpr" argument before the "glvn" argument.
* numexpr can be a negative value.
* Since it performs an arithmetic operation, $INCREMENT() treats glvn as numeric value. $INCREMENT treats glvn as if it were the first argument of an implicit $GET() before the increment. If the value of glvn is undefined $INCREMENT treats it as having empty string , which means it treats it as a numeric zero (0) (even if glvn is a global variable that resides on a remote node and is accessed through a GT.CM GNP server).
* If $INCREMENT() occurs inside a transaction ($TLevel is non-zero), or if glvn refers to a local variable, it is equivalent to SET glvn=$GET(glvn)+numexpr.
* If $INCREMENT() occurs outside a transaction ($TLevel is zero) and glvn refers to a global variable, the function acts as a SET glvn=$GET(glvn)+numexpr performed as an Atomic, Consistent and Isolated operation. Note that $INCREMENT() performs the evaluation of numexpr before it starts the Atomic, Consistent, Isolated incrementing of the glvn. If the region containing the glvn is journaled, then the $INCREMENT() is also Durable. Only BG, MM and GT.CM GNP access methods are supported for the region containing the global variable (glvn). GT.CM OMI and GT.CM DDP access methods do not support this operation and there are no current plans to add such support.
* $INCREMENT() does not support global variables that have NOISOLATION turned ON (through the VIEW "NOISOLATION" command), and a $INCREMENT() on such a variable, triggers at YDB-E-GVINCRISOLATION run-time error.
* The naked reference is affected by the usage of global variables (with or without indirection) in the glvn and/or numexpr components. The evaluation of "numexpr" ahead of "glvn" determines the value of the naked reference after the $INCREMENT. If neither glvn or numexpr contain indirection, then $INCREMENT sets the naked reference as follows:

  * glvn, if glvn is a global, or
  * the last global reference in "numexpr" if glvn is a local, or
  * unaffected if neither glvn nor numexpr has any global reference.

+++++++++++++++++++++++++
Examples of $INCREMENT()
+++++++++++++++++++++++++

Example:

.. code-block:: bash

   YDB>set i=1
   YDB>write $increment(i)
   2
   YDB>write $increment(i)
   3
   YDB>write $increment(i)
   4
   YDB>write $increment(i)
   5
   YDB>write i
   5
   YDB>write $increment(i,-2)
   3
   YDB>write I
   3
   YDB>

This example increments the value of i by 1 and at the end decrements it by 2. Note that the default value for incrementing a variable is 1.

.. _justify-function:

-------------------
$JUSTIFY()
-------------------

Returns a formatted string.

The format for the $JUSTIFY function is:

.. code-block:: none

   $J[USTIFY](expr,intexpr1[,intexpr2])

* The expression specifies the string to be formatted by $JUSTIFY().
* The first integer expression (second argument) specifies the minimum size of the resulting string. If the first integer expression is larger than the length of the expression, $JUSTIFY() right justifies the expression to a string of the specified length by adding leading spaces. Otherwise, $JUSTIFY() returns the expression unmodified unless specified by the second integer argument.
* The optional second integer expression (third argument) specifies the number of digits to follow the decimal point in the result, and forces $JUSTIFY() to evaluate the expression as numeric. If the numeric expression has more digits than this argument specifies, $JUSTIFY() rounds to obtain the result. If the expression had fewer digits than this argument specifies, $JUSTIFY() zero-fills to obtain the result.
* When the second argument is specified and the first argument evaluates to a fraction between -1 and 1, $JUSTIFY() returns a number with a leading zero (0) before the decimal point (.).

$JUSTIFY() fills expressions to create fixed length values. However, if the length of the specified expression exceeds the specified field size, $JUSTIFY() does not truncate the result (although it may still round based on the third argument). When required, use $EXTRACT() to perform truncation.

$JUSTIFY() optionally rounds the portion of the result after the decimal point. In the absence of the third argument, $JUSTIFY() does not restrict the evaluation of the expression. In the presence of the third (rounding) argument, $JUSTIFY() evaluates the expression as a numeric value. The rounding algorithm can be understood as follows:

* If necessary, the rounding algorithm extends the expression to the right with 0s (zeros) to have at least one more digit than specified by the rounding argument.
* Then, it adds 5 (five) to the digit position after the digit specified by the rounding argument.
* Finally, it truncates the result to the specified number of digits. The algorithm rounds up when excess digits specify a half or more of the last retained digit and rounds down when they specify less than a half.
* For a process started in UTF-8 mode, $JUSTIFY() interprets the string argument as UTF-8 encoded. With VIEW "BADCHAR" enabled, $JUSTIFY() produces a run-time error when it encounters a malformed character.
* $ZJUSTIFY() is the parallel function of $JUSTIFY(). Irrespective of the settings of VIEW "BADCHAR" and $ZCHSET, $ZJUSTIFY() interprets argument as a sequence of bytes (rather than a sequence of characters) and can perform all byte-oriented $JUSTIFY() operations. For more information, refer to :ref:`zjustify-function`.

++++++++++++++++++++++++
Examples of $JUSTIFY()
++++++++++++++++++++++++

Example:

.. code-block:: bash

   YDB>write ":",$justify("HELLO",10),":",!,":",$justify("GOODBYE",5),":"
   :     HELLO:
   :GOODBYE:
   YDB>

This uses $JUSTIFY() to display "HELLO" in a field of 10 spaces and "GOODBYE" in a field of 5 spaces. Because the length of "GOODBYE" exceeds five spaces, the result overflows the specification.

Example:

.. code-block:: bash

   YDB>write "1234567890",!,$justify(10.545,10,2)
   1234567890
        10.55
   YDB>

This uses $JUSTIFY() to WRITE a rounded value right justified in a field of 10 spaces. Notice that the result has been rounded up.

Example:

.. code-block:: bash

   YDB>write "1234567890",!,$justify(10.544,10,2)
   1234567890
        10.54
   YDB>

Again, this uses $JUSTIFY() to WRITE a rounded value right justified in a field of 10 spaces. Notice that the result has been rounded down.

Example:

.. code-block:: bash

   YDB>write "1234567890",!,$justify(10.5,10,2)
   1234567890
        10.50
   YDB>

Once again, this uses $JUSTIFY() to WRITE a rounded value right justified in a field of 10 spaces. Notice that the result has been zero-filled to 2 places.

Example:

.. code-block:: bash

   YDB>write $justify(.34,0,2)
   0.34
   YDB>

This example uses $JUSTIFY to ensure that the fraction has a leading zero. Note the use of a second argument of zero in the case that rounding is the only function that $JUSTIFY is to perform.

.. _length-function:

-------------------------
$LENGTH()
-------------------------

Returns the length of a string measured in characters, or in "pieces" separated by a delimiter specified by one of its arguments.

The format for the $LENGTH function is:

.. code-block:: none

   $L[ENGTH](expr1[,expr2])

* The first expression specifies the string that $LENGTH() "measures".
* The optional second expression specifies the delimiter that defines the measure; if this argument is missing, $LENGTH() returns the number of characters in the string.
* If the second argument is present and not an empty string, $LENGTH returns one more than the count of the number of occurrences of the second string in the first string; if the second argument is an empty string, the M standard specifies that $LENGTH() returns a zero (0).
* $LENGTH() provides a tool for determining the lengths of strings in two ways, characters and pieces. The two argument $LENGTH() returns the number of existing pieces, while the one argument returns the number of characters.
* For a process started in UTF-8 mode, $LENGTH() interprets the string argument(s) as UTF-8 encoded. With VIEW "BADCHAR" enabled, $LENGTH() produces a run-time error when it encounters a malformed character.
* $ZLENGTH() is the parallel function of $LENGTH(). Irrespective of the setting of VIEW "BADCHAR" and $ZCHSET, $ZLENGTH() interpets string arguments as a sequence of bytes (rather than characters) and can perform all byte-oriented $LENGTH() operations. For more information, refer to :ref:`zlength-function`.

+++++++++++++++++++++++++++
Examples of $LENGTH()
+++++++++++++++++++++++++++

Example:

.. code-block:: bash

   YDB>Write $length("KINGSTON")
   8
   YDB>

This uses $LENGTH() to WRITE the length in characters of the string "KINGSTON".

Example:

.. code-block:: bash

   YDB>set x="Smith/John/M/124 Main Street/Ourtown/KA/USA"
   YDB>write $length(x,"/")
   7
   YDB>

This uses $LENGTH() to WRITE the number of pieces in a string, as delimited by /.

Example:

.. code-block:: bash

   YDB>write $length("/2/3/","/")
   4
   YDB>

This also uses $LENGTH() to WRITE the number of pieces in a string, as delimited by /. Notice that YottaDB adds one count to the count of delimiters (in this case 3), to get the number of pieces in the string (displays 4).

.. _name-function:

---------------------
$NAME()
---------------------

Returns an evaluated representation of some or all of a local or global variable name.

The format for the $NAME function is:

.. code-block:: none

   $NA[ME](glvn[,intexpr])

* The subscripted or unsubscripted global or local variable name, including naked references, specifies the name for which $NAME() returns an evaluated representation.
* When using NOUNDEF, $NAME() returns an empty string where appropriate for undefined variables.
* The optional integer expression (second argument) specifies the maximum number of subscript levels in the representation. If the integer expression is not provided or exceeds the actual number of subscript levels, $NAME() returns a representation of the whole name. If the integer expression is zero (0), $NAME() returns only the name. A negative integer expression produces a run-time error.

+++++++++++++++++++++++
Examples of $NAME()
+++++++++++++++++++++++

Example:

.. code-block:: bash

   YDB>set X="A""B",^Y(1,X,"B",4)=""
   YDB>write $name(^(3),3)
   ^Y(1,"A""B","B")
   YDB>

This example sets up a naked reference and then uses $NAME() to display the first three levels of that four-level reference.

Example:

.. code-block:: bash

   YDB>write $name(^(3),0)
   ^Y
   YDB>

This example shows the name level for the same naked reference.

----------------------
$NEXT()
----------------------

Returns the next subscripted local or global variable name in collation sequence within the array level specified by its argument.

$NEXT() has been replaced by $ORDER(). $NEXT has been retained in the current standard only for compatibility with earlier versions of the standard. $NEXT() is similar to $ORDER(). However, $NEXT() has the deficiency that when it encounters negative one (-1) as a subscript, it returns the same result as when it finds no other data at the level. This deficiency is particularly disruptive because it occurs in the middle of the M collating sequence.

.. note::
   As $NEXT() has been removed from the standard in the MDC, you should use $ORDER.

The format for the $NEXT function is:

.. code-block:: none

   $N[EXT](glvn)

* The subscripted global or local variable name specifies the node following which $NEXT() searches for the next node with data and/or descendants; the number of subscripts contained in the argument implicitly defines the array level.
* If $NEXT() finds no node at the specified level after the specified global or local variable, it returns negative one (-1).
* If the last subscript in the subscripted global or local variable name is null or negative one (-1), $NEXT() returns the first node at the specified level.

----------------------
$ORDER()
----------------------

Returns the subscript of the next or prior local or global variable name in collation sequence within the array level specified by its first argument. In doing so, it moves in the direction specified by the second argument. In YottaDB, when $ORDER() has an unsubscripted argument, it returns the next or previous unsubscripted local or global variable name in collating sequence.

The format for the $ORDER function is:

.. code-block:: none

   $O[RDER](glvn[,expr])

* The subscripted global or local variable name specifies the node from which $ORDER() searches for the next or previous node that has data and/or descendants. The number of subscripts contained in the argument implicitly defines the array level.
* The optional expression (second argument) specifies the direction for the $ORDER(); 1 specifies forward operation and -1 specifies reverse operation. Any other values for the expression will cause an error.
* YottaDB extends the M standard to allow unsubscripted names. In this case, $ORDER() returns the next or previous unsubscripted name.
* If $ORDER() finds no node (or name) at the specified level after (or before) the specified global or local variable, it returns an empty string (" ").
* If the last subscript in the subscripted global or local variable name is null and the corresponding subscripted global or local variable has a matching null subscript, $ORDER() returns the next node after that with the null subscript at the specified level.
* If the last subscript in the subscripted global or local variable name is null and the corresponding subscripted global or local variable has no matching null subscript , $ORDER() returns first node at the specified level. If the last subscript in the subscripted global or local variable name is null and second argument is -1, $ORDER() always returns the last node at the specified level regardless of the existence of a null subscript at the specified level. However, when a global or local variable level includes a null subscript and $ORDER(glvn,-1) returns an empty string result, users must test separately for the existence of the node with the null subscript.
* $ORDER() can be used as a tool for retrieving data from M sparse arrays in an ordered fashion, independent of the order in which it was entered. In M, routines generally sort by SETting data into an array with appropriate subscripts and then retrieving the information with $ORDER().
* $ORDER() returns subscripts, not data values, and does not discriminate between nodes that have data values and nodes that have descendants. Once $ORDER() provides the subscript, the routine must use the subscript to access the data value, if appropriate. Using $ORDER() maintains the naked reference indicator, even if $ORDER() returns a null.
* YottaDB optionally permits the use of null subscripts. This feature is enabled via the VIEW command for local variables and a REGION qualifier in GDE for global variables. When an application uses null subscripts, they are "invisible" in a $ORDER() loop so the application must test for them as a special case, perhaps using $DATA().
* $ORDER() returns local array subscripts with values that are numeric, but non-canonical (over 18 digit), as strings.

.. note::
   Name-level $ORDER() always returns an empty string when used with extended references.

++++++++++++++++++++++
Examples of $ORDER()
++++++++++++++++++++++

Example:

.. code-block:: bash

   YDB>zwrite
   lcl(1)=3
   lcl("x")=4
   YDB>write $order(lcl(""))
   1

This example returns the first node, that is 1, because the specified last subscript of the argument is null and lcl has no null subscript.

Example:

.. code-block:: bash

   YDB>write $order(lcl(1))
   x

This example returns the first node after lcl(1) that is x because lcl has no null subscript.

Example:

.. code-block:: bash

   YDB>write $order(lcl(""),-1)
   x

This example returns the last node that is, x, because the last subscript of the first argument is null and second argument is -1.

.. code-block:: bash

   YDB>set lcl("")=2
   YDB>zwrite
   lcl("")=2
   lcl(1)=3
   lcl("x")=4
   YDB>write $order(lcl(""))
   1

This example returns the second node at the specified level because the null subscript at the end of the argument is ambiguous (does it specify starting at the beginning or starting at the real node with the null subscript?) and returning the subscript of the first node (an empty string) would tend to create an endless loop.

Example:

.. code-block:: bash

   YDB>write $order(lcl(""),-1)
   x
   YDB>write $order(lcl("x"),-1)
   1

Example:

.. code-block:: bash

   YDB>kill  set (a(1),a(2000),a("CAT"),a("cat"),a("ALF"),a(12))=1
   YDB>set x="" for  set x=$order(a(x)) quit:x=""  write !,x
   1
   12
   2000
   ALF
   CAT
   cat
   YDB>kill a("CAT") set a(5,10)="woolworths",a("cat")="last"
   YDB>set x="" for  set x=$order(a(x),-1) quit:x=""  write !,x
   cat
   ALF
   2000
   12
   5
   1
   YDB>

This example uses a $ORDER() loop to display all the subscripts at the first level of local variable a, make some changes in a, and then display all the subscripts in reverse order. Notice that $ORDER() returns only the existing subscripts in the sparse array and returns them in M collation sequence, regardless of the order in which they were entered. Also, $ORDER() does not differentiate between node A(5), which has only descendants (no data value), and the other nodes, which have data values.

Example:

.. code-block:: bash

   YDB>kill set (%(1),tiva(2),A(3),tiv(4),Q(5),%a(6))=""
   YDB>set x="%"
   YDB>write:$data(@x) !,x for  set x=$order(@x) quit:x=""  write !,x
   %
   %a
   A
   Q
   tiv
   tiva
   x
   YDB>set $piece(x,"z",32)=""
   YDB>write:$data(@x) !,x for  set x=$order(@x,-1) quit:x=""  write !,x
   x
   tiva
   tiv
   Q
   A
   %a
   %
   YDB>

This example uses $ORDER() to display the current local variable names in both forward and reverse order. Notice that the first ([^]%) and last ([^]zzzzzzzz) names require handling as special cases and require a $DATA() function.

Example:

.. code-block:: none

   set acct="",cntt=""
   for  fet acct=$order(^acct(acct)) quit:acct=""  do
   . for  set cntt=$order(^acct(acct,cntt)) do WORK
   quit

This uses two nested $ORDER() loops to cycle through the ^acct global array and perform some action for each second level node.

.. _piece-function:

---------------------
$PIECE()
---------------------

Returns a substring delimited by a specified string delimiter made up of one or more characters. In M, $PIECE() returns a logical field from a logical record.

The format for the $PIECE function is:

.. code-block:: none

   $P[IECE](expr1,expr2[,intexpr1[,intexpr2]])

* The first expression specifies the string from which $PIECE() computes its result.
* The second expression specifies the delimiting string that determines the piece "boundaries"; if this argument is an empty string, $PIECE() returns an empty string.
* If the second expression does not appear anywhere in the first expression, $PIECE() returns the entire first expression (unless forced to return an empty string by the second integer expression).
* The optional first integer expression (third argument) specifies the beginning piece to return; if this argument is missing, $PIECE() returns the first piece.
* The optional second integer expression (fourth argument) specifies the last piece to return. If this argument is missing, $PIECE() returns only one piece unless the first integer expression is zero (0) or negative, in which case it returns a null string. If this argument is less than the first integer expression, $PIECE() returns an empty string.
* If the second integer expression exceeds the actual number of pieces in the first expression, $PIECE() returns all of the expression after the delimiter selected by the first integer expression.
* The $PIECE() result never includes the "outside" delimiters; however, when the second integer argument specifies multiple pieces, the result contains the "inside" occurrences of the delimiter.
* $PIECE() can also be used as tool for efficiently using values that contain multiple elements or fields, each of which may be variable in length.
* Applications typically use a single character for a $PIECE() delimiter (second argument) to minimize storage overhead, and increase efficiency at run-time. The delimiter must be chosen so the data values never contain the delimiter. Failure to enforce this convention with edit checks may result in unanticipated changes in the position of pieces within the data value. The caret symbol (^), backward slash (\\), and asterisk (*) characters are examples of popular visible delimiters. Multiple character delimiters may reduce the likelihood of conflict with field contents. However, they decrease storage efficiency, and are processed with less efficiency than single character delimiters. Some applications use control characters, which reduce the chances of the delimiter appearing in the data but sacrifice the readability provided by visible delimiters.
* A SET command argument can have something that has the format of a $PIECE() on the left-hand side of its equal sign (=). This construct permits easy maintenance of individual pieces within a string. It also can be used to generate a string of delimiters. For more information on SET $PIECE(), refer to :ref:`set-command`.
* $PIECE() can also be used as target in a SET command to change part of the value of a node. Also, when SET arguments have multiple parenthesized (set-left) targets and a target is used as a subscript in more than one item in the list of targets that follow, all the targets use the before-SET value (not the after-SET value) in conformance to the M-standard. For more information on SET $PIECE(), refer to :ref:`set-command`.
* For a process started in UTF-8 mode, $PIECE() interprets the string arguments as UTF-8 encoded. With VIEW "BADCHAR" enabled, $PIECE() produces a run-time error when it encounters a malformed character, but it does not process the characters that fall after the span specified by the arguments.
* $ZPIECE() is the parallel function of $PIECE(). Irrespective of the settings of VIEW "BADCHAR" and $ZCHSET, $ZPIECE() interprets string arguments as a sequence of bytes (rather than a sequence of characters) and can perform all byte-oriented $PIECE() operations. For more information, refer to :ref:`zpiece-function`.

++++++++++++++++++++++++++++
Examples of $PIECE()
++++++++++++++++++++++++++++

Example:

.. code-block:: bash

   YDB>for i=0:1:3 write !,$piece("1 2"," ",i),"<"
   <
   1<
   2<
   <
   YDB>

This loop displays the result of $PIECE(), specifying a space as a delimiter, a piece position "before," first and second, and "after" the string.

Example:

.. code-block:: bash

   YDB>for i=-1:1:3 write !,$piece("1 2"," ",i,i+1),"<"
   <
   1<
   1 2<
   2<
   <
   YDB>

This example is similar to the previous example except that it displays two pieces on each iteration. Notice the delimiter (a space) in the middle of the output for the third iteration, which displays both pieces.

Example:

.. code-block:: none

   for p=1:1:$length(x,"/") write ?p-1*10,$piece(x,"/",p)

This example uses $LENGTH() and $PIECE() to display all the pieces of x in columnar format.

Example:

.. code-block:: bash

   YDB>set $piece(x,".",25)="" write x
   ........................

This SETs the 25th piece of the variable x to null, with a delimiter of a period. This produces a string of 24 periods preceding the null.

Example:

.. code-block:: bash

   YDB>set ^x=1,$piece(^a,";",3,2)=^b

This example leaves the naked indicator to pointing to the global ^b.

----------------------
$QLENGTH()
----------------------

Returns the number of subscripts in a variable name. The format is:

.. code-block:: none

   $QL[ENGTH](namevalue)

* The namevalue has the form of an evaluated subscripted or unsubscripted global variable.
* $QLENGTH() returns a value which is derived from namevalue. If namevalue has the form NAME(s1, s2,..., sn), then the function returns n; if the name is unsubscripted, $QLENGTH() yields a length of zero (0).
* $QLENGTH() only affects the naked indicator if the string in question is stored in a global variable.

.. _examples-of-qlength:

++++++++++++++++++++++++++
Examples of $QLENGTH()
++++++++++++++++++++++++++

Example:

.. code-block:: bash

   YDB>write $data(^|"XXX"|ABC(1,2,3,4))
   0
   YDB>set X=$name(^(5,6))
   YDB>write $qlength(X)
   5

The number of subscripts in x is 5. Notice that the name and the environment preceding it do not contribute to the count. Refer to :ref:`name-function` section earlier in this chapter for an understanding of the $NAME function.

-------------------------
$QSUBSCRIPT()
-------------------------

Returns a component of a variable name.

The format of the $QSUBSCRIPT function is:

.. code-block:: none

   $QS[UBSCRIPT](namevalue, intexpr)

* The namevalue has the form of an evaluated subscripted or unsubscripted global or local variable name.
* The intexpr selects the component of the name as follows:

  * -2 : is reserved but may be "error",
  * -1 : for environment,
  * 0 : for the unsubscripted name,
  * 1 : for the first subscript,
  * 2 : for the second subscript, and so on.

If the second argument selects a component that is not part of the specified name, $QSUBSCRIPT() returns an empty string ("").

+++++++++++++++++++++++++++
Examples of $QSUBSCRIPT()
+++++++++++++++++++++++++++

Example:

Assume that X is defined as in the :ref:`examples-of-qlength` earlier in this chapter;

.. code-block:: none

   write X
   X="^|""XXX""|ABC(1,2,3,5,6)"
   YDB>write $qsubscript(X,-2)
   error
   YDB>WRITE $qsubscript(X,-1)
   XXX
   YDB>WRITE $qsubscript(X,0)
   ^ABC
   YDB>WRITE $qsubscript(X,1)
   1
   YDB>WRITE $qsubscript(X,4)
   5
   YDB>WRITE $qsubscript(X,7)
   ""

.. _query-function:

------------------
$QUERY()
------------------

Returns the next or previous subscripted local or global variable node name, independent of level, which follows or precedes the node specified by its argument in M collating sequence and has a data value.

The format for the $QUERY function is:

.. code-block:: none

   $Q[UERY](glvn[,expr])

* The subscripted or unsubscripted global or local variable name specifies the starting node from which $QUERY() searches for the next or previous node with a data value.
* The optional expression (second argument) specifies the direction for the $QUERY(); 1 specifies forward operation and -1 specifies reverse operation. Any other values for the expression will cause an error.
* If $QUERY() finds no node after the specified global or local variable, it returns an empty string.
* With stdnullcoll, if $DATA(glvn(""))=1 (or 11), $QUERY(glvn("")) returns glvn(1) (assuming glvn(1) exists). Applications looking for a node with a "null" subscript must use $D(glvn("")) to test the existence of glvn(""). $Q(glvn("...")) never returns the starting-point (glvn("")) even though glvn("") may exist.

$QUERY() can be used as a tool for scanning an entire array for nodes that have data values. Because $QUERY() can return a result specifying a different level than its argument, the result provides a full variable name. This contrasts with $ORDER(), which returns a subscript value. To access the data value at a node, a $ORDER() return can be used as a subscript; however, a $QUERY() return must be used with indirection. Because arrays tend to have homogeneous values within a level but not between levels, $QUERY() is more useful as a tool in utility programs than in application programs. The $QUERY() can be useful in avoiding nested $ORDER loops.

Note that the standard does not unambiguously define the state of the naked reference indicator after a $QUERY(). While in YottaDB after $QUERY(), the naked reference indicator reflects the $QUERY() argument, NOT its result.

If the byte length of the string returned by $QUERY() exceeds 1,048,576 bytes, $QUERY() returns a `YDB-E-MAXSTRLEN <../MessageRecovery/errors.html#maxstrlen-error>`_ error.

+++++++++++++++++++++++++++
Examples of $QUERY()
+++++++++++++++++++++++++++

Example:

.. code-block:: none

   set ^X(1,2,3)="123"
   set ^X(1,2,3,7)="1237"
   set ^X(1,2,4)="124"
   set ^X(1,2,5,9)="1259"
   set ^X(1,6)="16"
   set ^X("B",1)="AB"

The tree diagram below represents the structure produced by the preceding routine.

.. image:: querytree.svg

The following routine:

.. code-block:: none

   set y="^X"
   for  set y=$query(@y) quit:y=""  write !,y,"=",@y

produces the results:

.. code-block:: none

   ^X(1,2,3)=123
   ^X(1,2,3,7)=1237
   ^X(1,2,4)=124
   ^X(1,2,5,9)=1259
   ^X(1,6)=16
   ^X("B",1)=AB

And the following routine (reverse $QUERY):

.. code-block:: none

   set y="^X(""B"",1)"
   for  do  quit:y=""  write !,y,"=",@y set y=$query(@y,-1)

produces the following results:

.. code-block:: none

   ^X("B",1)=AB
   ^X(1,6)=16
   ^X(1,2,5,9)=1259
   ^X(1,2,4)=124
   ^X(1,2,3,7)=1237
   ^X(1,2,3)=123

Example:

.. code-block:: bash

   YDB>zwrite lcl
   lcl("")=1
   lcl(1)=1
   lcl(1,2)=2
   lcl(1,2,"")=3
   lcl(1,2,"","")=4
   lcl(1,2,"","",4)=5
   lcl(1,2,0)=6
   lcl(1,2,"abc",5)=7
   lcl("x")=1
   YDB>set y="lcl"
   YDB>for  set y=$query(@y) quit:y=""  write !,y,"=",@y

This example produces the results:

.. code-block:: none

   lcl("")=1
   lcl(1)=1
   lcl(1,2)=2
   lcl(1,2,"")=3
   lcl(1,2,"","")=4
   lcl(1,2,"","",4)=5
   lcl(1,2,0)=6
   lcl(1,2,"abc",5)=7
   lcl("x")=1

Note that the result is the same as the ZWRITE output.

----------------------
$RANDOM()
----------------------

Returns a pseudo-random integer from a range specified by its argument.

The format for the $RANDOM function is:

.. code-block:: none

   $R[ANDOM](intexpr)

* The integer expression specifies the upper exclusive limit of a range of integers from which $RANDOM() may pick a result; $RANDOM() never returns a number less than zero (0).
* If $RANDOM() has an argument less than one (1), it generates a run-time error.
* $RANDOM can generate numbers up to 2147483646 (that is 2GB - 2).

$RANDOM() returns an integer between zero (0) and one less than the argument. $RANDOM() provides a tool for generating pseudo-random patterns useful in testing or statistical calculations. You should ensure that the statistical properties of $RANDOM() are adequate for your application needs.

.. note::
   $RANDOM() should never be used when cryptographic quality random numbers are needed.

++++++++++++++++++++++++++++
Examples of $RANDOM()
++++++++++++++++++++++++++++

Example:

.. code-block:: bash

   YDB>for i=1:1:10 write $random(1)
   0000000000
   YDB>

This shows that when $RANDOM() has an argument of one (1), the result is too confined to be random.

Example:

.. code-block:: none

   set x=$random(100)+1*.01

This $RANDOM() example produces a number between 0 and 99. The example then shifts with addition, and scales with multiplication to create a value between .01 and 1.

--------------------------
$REVERSE()
--------------------------

Returns a string with the characters in the reverse order from that of its argument.

The format for the $REVERSE function is:

.. code-block:: none

   $RE[VERSE](expr)

* The expr in the syntax is the string to be reversed.

++++++++++++++++++++++++++
Examples of $REVERSE()
++++++++++++++++++++++++++

Example:

.. code-block:: bash

   YDB>write $reverse(123)
   321
   YDB>write $reverse("AbCDe")
   "eDCbA"

---------------------
$SELECT()
---------------------

Returns a value associated with the first true truth-valued expression in a list of paired expression arguments.

The format for the $SELECT function is:

.. code-block:: none

   $S[ELECT](tvexpr:expr[,...])

* $SELECT() evaluates expressions from left to right.
* If a truth-valued expression is TRUE (1), $SELECT() returns the corresponding expression after the colon (:) delimiter.
* Once $SELECT() finds a TRUE, the function does not process any remaining arguments.
* If $SELECT() finds no TRUE truth-value in its list of arguments, the function generates a run-time error.
* $SELECT() does not have any effect on $TEST.

$SELECT() is one of a limited set of functions that permit an indefinite number of arguments. $SELECT() provides a means of selecting from a list of alternatives.

Generally, the last $SELECT() argument has numeric literal one (1) for a truth-value to prevent run-time errors, and to provide a "default" value.

++++++++++++++++++++++++
Examples of $SELECT()
++++++++++++++++++++++++

Example:

.. code-block:: bash

   YDB>for i=3:-1:0 write !,$select(i=1:"here",i=2:"come",i=3:"Watson")
   Watson
   come
   here
   %YDB-E-SELECTFALSE, No argument to $SELECT was true
   YDB>

This loop uses $SELECT() to WRITE a series of strings. Because there is no true argument on the fourth iteration, when i=0, $SELECT() produces an error.

Example:

.. code-block:: none

   set name=$select(sex="M":"Mr. ",sex="F":"Ms. ",1:"")_name

This example uses $SELECT() to add a prefix to the name based on a sex code held in the variable sex. Notice that the default handles the case of a missing or incorrect code.

Example:

.. code-block:: none

   if $select(x=+x:x,x="":0,"JANAPRJULOCT"[x:1,1:0) do THING

This uses $SELECT() to perform complex logic as the truth-valued expression argument to an IF command.

.. note::
   When extrinsics are within a $SELECT expression, boolean short-circuiting does not prevent them from being evaluated during the execution of the statement.

.. code-block:: none

   echoAndRet(A,B)
    write A,!
    quit B

   YDB>write 1!$$^echoAndRet("Hello",0)!$S($$^echoAndRet("World",0):5)
       World
   %YDB-E-SELECTFALSE, No argument to $SELECT was true

   YDB>

.. _stack-function:

--------------------
$STACK()
--------------------

Returns strings describing aspects of the execution environment.

The format for the $STACK function is:

.. code-block:: none

   $ST[ACK](intexpr[,expr])

* The intexpr identifies the M virtual machine stack level (as described by the standard), on which the function is to provide information.
* The optional second argument is evaluated as a keyword that specifies a type of information to be returned as follows:

  * "MCODE" the line of code that was executed.
  * "PLACE" the address of the above line of code or the symbol at ("@") to indicate code executed from a string value.
  * "ECODE" either an empty string, or the error code(s) that was added at this execution level.

  .. note::
     For run-time errors, YottaDB does not provide a "PLACE" within a line (unlike it does for compilation errors), but it reports a label, offset, and routine.

* When $STACK has only one argument, values corresponding to available stack levels specify a return value that indicates how the level was created, as follows:
  * If intexpr is zero (0), the function returns information on how YottaDB was invoked.
  * If intexpr is minus one (-1), the function returns the highest level for which $STACK can return information. Note that, if $ECODE="", $STACK(-1) returns the same value as the $STACK ISV.
  * If intexpr is greater than zero (0) and less than or equal to $STACK(-1), indicates how this level of process stack was created ("DO", "TRIGGER" - for a stack level invoked by a trigger, "XECUTE", or "$$" - for an extrinsic function).
* $STACK(lvl) reports "ZINTR" for a stack level invoked by MUPIP INTRPT.
* If intexpr is greater than $STACK (-1), the function returns an empty string.
* During error handling, $STACK() return a snapshot of the state of the stack at the time of error. Even if subsequent actions add stack levels, $STACK() continues to report the same snapshot for the levels as of the time of the error. $STACK() reports the latest stack information only after the code clears $ECODE.
* $STACK() assists in debugging programs.

.. note::
   $STACK() returns similar information to ZSHOW "S" when ""=$ECODE, but when $ECODE contains error information, $STACK() returns information as of the time of a prior error, generally the first entry in $ECODE. For $STACK() to return current information, be sure that error handling code does a SET $ECODE="" before restoring the normal flow of control.

+++++++++++++++++++++++++
Examples of $STACK()
+++++++++++++++++++++++++

Example:

.. code-block:: bash

   /usr/local/lib/yottadb/r120/ydb -run ^dstackex
   dstackex;
     zprint ^dstackex
     write !,$STACK
     xecute "WRITE !,$STACK"
     do Label
     write !,$$ELabel
     write !,$STACK
     quit

   Label
     write !,$STACK
     do DLabel
     quit

   ELabel()
     quit $STACK

   DLabel
     write !,$STACK
     quit
   0
   1
   1
   2
   1

Example for error processing:

.. code-block:: bash

   YDB>zprint ^debugerr
   debugerr;
    set dsm1=$stack(-1)
    write !,"$stack(-1):",dsm1
    for l=dsm1:-1:0 do
    . write !,l
    . for i="ecode","place","mcode" write ?5,i,?15,$stack(l,i),!
   YDB>

The above example can be used to display a trace of the code path that led to an error.

Example:

.. code-block:: bash

   YDB>zprint ^dstacktst
   dstacktst(x)       ; check $stack() returns with and without clearing $ecode
    set $etrap="do ^debugerr"
    label
     if x>0 set $ecode=",U1," ; if condition
     else  set $ecode=",U2," ;  else condition
     quit
   YDB>do ^dstacktst(0)
   $stack(-1):2
   2    ecode
        place     debugerr+3^debugerr
        mcode      for l=dsm1:-1:0 do
   1    ecode     ,U2,
        place     label+2^dstacktst
        mcode      else  set $ecode=",U2," ;  else condition
   0    ecode
        place     +1^GTM$DMOD
        mcode
   %YDB-E-SETECODE, Non-empty value assigned to $ECODE (user-defined error trap)
   YDB>do ^dstacktst(1)
   $stack(-1):1
   1    ecode     ,U2,
        place     label+2^dstacktst
        mcode      else  set $ecode=",U2," ;  else condition
   0    ecode
        place     +1^GTM$DMOD
        mcode
   %YDB-E-SETECODE, Non-empty value assigned to $ECODE (user-defined error trap)
   YDB>set $ecode=""
   YDB>do ^dstacktst(1)
   $stack(-1):2
   2    ecode
        place     debugerr+3^debugerr
        mcode      for l=dsm1:-1:0 do
   1    ecode     ,U1,
        place     label+1^dstacktst
        mcode      if x>0 set $ecode=",U1," ; if condition
   0    ecode
        place     +1^GTM$DMOD
        mcode
   %YDB-E-SETECODE, Non-empty value assigned to $ECODE (user-defined error trap)
   YDB>

This example shows how SETing $ECODE=.. makes $STACK() report current information. Notice how ^do dstacktst(0) and ^dostacktst(1) without clearing $ECODE in between displays information frozen at the time of the first error (else condition).

--------------------
$TEXT()
--------------------

Returns source text for the line specified by its argument.

The format for the $TEXT function is:

.. code-block:: none

   $T[EXT](entryref)

* The entryref specifies the label, offset, and routine (or trigger name) of the source line that $TEXT() returns.
* If the label+offset combination do not fall within the routine, $TEXT returns a null string.
* If the entryref explicitly or implicitly specifies an offset of zero (0) from the beginning of the routine (or trigger name), $TEXT() returns the routine name or trigger name.
* If the entryref does not specify a routine/trigger, YottaDB assumes the current routine/trigger, that is, the routine/trigger at the top of a ZSHOW "S."
* A YottaDB extension to $TEXT() permits negative offsets; however, every offset must still be preceded by a plus sign (+) delimiter, (for example, LABEL+-3). If a negative offset points to a line prior to the zero line, $TEXT() generates a run-time error.

$TEXT() provides a tool for examining routine source code and the name of the current routine or trigger. $TEXT() assists, along with the ZPRINT command, in debugging programs. $TEXT() also allows the insertion of small tables of driver information into a routine. Because $TEXT() is not very efficient and the table-driven technique is generally best suited to minimal program changes, this approach is best used for prototyping and the tables should reside in global variables for production.

If $TEXT() cannot access the source file for the current object, either because it is not in the location from which it was compiled or because the process does not have access to some piece of the path to the source, or if the located source does not match the object currently in use by the process, $TEXT() returns an empty string.

++++++++++++++++++++++
Examples of $TEXT()
++++++++++++++++++++++

Example:

.. code-block:: none

   for i=1:1 set x=$text(+i) quit:x=""  write !,x

This loop uses $TEXT() to write out the entire source for the current routine.

Example:

.. code-block:: bash

   YDB>write $text(+0)
   GTM$DMOD
   YDB>write $text(+1)
   YDB>

This uses $TEXT() to WRITE the name of the current routine, then it tries to access the source and returns an empty string. This occurs because the default Direct Mode image is compiled by YottaDB and delivered without source. The exact failure message may vary.

.. _translate-function:

---------------------
$TRANSLATE()
---------------------

Returns a string that results from replacing or dropping characters in the first of its arguments as specified by the patterns of its other arguments.

The format for the $TRANSLATE function is:

.. code-block:: none

   $TR[ANSLATE](expr1[,expr2[,expr3]])


* The first expression specifies the string on which $TRANSLATE() operates. If the other arguments are omitted, $TRANSLATE() returns this expression.
* The optional second expression specifies the characters for $TRANSLATE() to replace. If a character occurs more than once in the second expression, the first occurrence controls the translation, and $TRANSLATE() ignores subsequent occurrences. If this argument is omitted, $TRANSLATE() returns the first expression without modification.
* The optional third expression specifies the replacement characters for positionally corresponding characters in the second expression. If this argument is omitted or shorter than the second expression, $TRANSLATE() drops all occurrences of characters in the second expression that have no replacement in the corresponding position of the third expression.
* For a process started in UTF-8 mode, the algorithm of $TRANSLATE() treats the string arguments as UTF-8 encoded. With VIEW "BADCHAR" enabled, $TRANSLATE() produces a run-time error when it encounters a malformed character.
* Irrespective of the settings of VIEW "BADCHAR" and $ZCHSET, $ZTRANSLATE() interprets argument as a sequence of bytes (rather than a sequence of characters) and performs all byte-oriented $TRANSLATE() operations. For more information, refer to “$ZTRanslate()”.
* $TRANSLATE() provides a tool for tasks such as changing case and doing encryption. For examples of case translation, refer to the ^%LCASE and ^%UCASE utility routines.

The $TRANSLATE() algorithm can be understood as follows:

* $TRANSLATE() evaluates each character in the first expression, comparing it character by character to the second expression looking for a match. If there is no match in the second expression, the resulting expression contains the character without modification.
* When it locates a character match, $TRANSLATE() uses the position of the match in the second expression to identify the appropriate replacement for the original expression. If the second expression has more characters than the third expression, $TRANSLATE() replaces the original character with a null, thereby deleting it from the result. By extension of this principle, if the third expression is missing, $TRANSLATE() deletes all characters from the first expression that occur in the second expression.

++++++++++++++++++++++++++
Examples of $TRANSLATE()
++++++++++++++++++++++++++

Example:

.. code-block:: bash

   YDB>write $translate("ABC","CB","1")
   A1
   YDB>

* First, $TRANSLATE() searches for "A" (the first character in the first expression, "ABC") within the second expression ("CB"). Since "A" does not exist in the second expression, it appears unchanged in the result.
* Next, $TRANSLATE() searches for "B" (the second character in the first expression) within the second expression ("CB"). Because "B" holds the second position in the second expression ("CB"), $TRANSLATE() searches for the character holding the second position in the third expression. Since there is no second character in the third expression, $TRANSLATE() replaces "B" with a null, effectively deleting it from the result.
* Finally, $TRANSLATE() searches for "C" (the third character in the first expression) within the second expression ("CB"), finds it in the first position, and replaces it with the number 1, which is in the first position of the third expression. The translated result is "A1."

.. note::
   While this example provides an explanation for the work done by $TRANSLATE(), it does not necessarily correspond to how YottaDB implements $TRANSLATE().

Example:

.. code-block:: bash

   YDB>write $translate("A","AA","BC")
   B
   YDB>

This $TRANSLATE() example finds the first occurrence of "A" in the second expression, which holds the first character position, and substitutes the character in the first position of the third expression.

Example:

.. code-block:: bash

   YDB>write $translate("BACKUP","AEIOU")
   BCKP
   YDB>

Because the $TRANSLATE() has only two parameters in this example, it finds the characters in the first expression that also exist in the second expression and deletes them from the result.

.. _view-function:

---------------------
$VIEW()
---------------------

Returns information about an environmental factor selected by the arguments. In YottaDB, the first argument contains a keyword identifying the environmental factor and, where appropriate, subsequent arguments select among multiple possible occurrences of that factor.

The format for the $VIEW function is:

.. code-block:: none

   $V[IEW](expr1[,expr2])

* The first expression specifies a keyword identifying the target factor for $VIEW() to examine.
* The second expression differentiates between multiple possible targets for some keywords. $VIEW() requires the second expression for some keywords and does not permit it for others.

.. _arg-kwrds-view:

+++++++++++++++++++++++++++++
Argument Keywords of $VIEW()
+++++++++++++++++++++++++++++

$VIEW() provides a means to access YottaDB environmental information. When YottaDB permits modification of the factors accessible with $VIEW(), the VIEW command generally provides the means for effecting the change.

**$VIEW() Argument Keywords**

+---------------+------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Arg 1         | Arg 2            | Return Value                                                                                                                                                        |
+===============+==================+=====================================================================================================================================================================+
| "BADCHAR"     | none             | In UTF-8 mode processes, enables or disable the generation of an error when character-oriented functions encounter malformed byte sequences (illegal characters).   |
|               |                  | The default is 1.                                                                                                                                                   |
+---------------+------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| "BREAKMSG"    | none             | Value of the break message mask; YottaDB defaults this to 31.                                                                                                       |
+---------------+------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| "FREEBLOCKS"  | region           | Number of free database blocks in a given region.                                                                                                                   |
+---------------+------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| "FREEZE"      | region           | Process-id of a process that has frozen the database associated with the region specified (using DSE or MUPIP). If the region is currently not frozen, returns zero.|
+---------------+------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| "FULL_BOOLEAN"| none             | Returns a string describing the current compiler setting. The default is "YottaDB Boolean short-circuit". $VIEW("FULL_BOOLEAN") reports "Standard Boolean           |
|               |                  | evaluation side effects" when it is not explicitly set, but that mode of operation is required by the setting of ydb_side_effects, and "Standard Boolean side-effect|
|               |                  | warning" when warnings have been specified.                                                                                                                         |
+---------------+------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| "GDSCERT"     | none             | Truth Value indicating whether Database block certification is currently enabled or disabled. To enable or disable Database block certification, use the VIEW       |
|               |                  | "GDSCERT" command.                                                                                                                                                  |
+---------------+------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| "GVACCESS_METH| none             | Access method of the region.                                                                                                                                        |
| OD"           |                  |                                                                                                                                                                     |
+---------------+------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| "GVFILE"      | region           | Name of the database associated with the region.                                                                                                                    |
+---------------+------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| "GVFIRST"     | none             | Name of the first database region in the current global directory; functionally equivalent to $VIEW("GVNEXT","").                                                   |
+---------------+------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| "GVNEXT"      | region           | Name of the next database region after the given one in alphabetical order (or M collation sequence); "" for region starts with the first region. A return value of |
|               |                  | "" means that the global directory defines no additional regions.                                                                                                   |
+---------------+------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| "GVSTAT"      | region           | A read-only process cannot update the database including the database file header where GVSTATS are stored. Another process with write access to a database, such as|
|               |                  | MUPIP RUNDOWN, can flush its read statistics from the associated shared memory to GVSTATS.                                                                          |
+---------------+------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| "ICHITS"      | none             | Number of indirection cache hits since YottaDB process startup. Indirection cache is a pool of compiled expressions that YottaDB maintains for indirection          |
|               |                  | and XECUTE.                                                                                                                                                         |
+---------------+------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| "ICMISS"      | none             | Number of indirection cache misses since YottaDB process startup.                                                                                                   |
+---------------+------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| "JNLACTIVE"   | region           | can return the following values:                                                                                                                                    |
|               |                  |                                                                                                                                                                     |
|               |                  | * -1 (internal error)                                                                                                                                               |
|               |                  |                                                                                                                                                                     |
|               |                  | * 0 journaling is disabled                                                                                                                                          |
|               |                  |                                                                                                                                                                     |
|               |                  | * 1 journaling is enabled but closed (OFF)                                                                                                                          |
|               |                  |                                                                                                                                                                     |
|               |                  | * 2 journaling is enabled and open (ON)                                                                                                                             |
+---------------+------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| "JNLFILE"     | region           | Journal file name associated with the region.                                                                                                                       |
+---------------+------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| "JNLTRANSACTI | none             | Index showing how many ZTSTART transaction fences have been opened (and not closed).                                                                                |
| ON"           |                  |                                                                                                                                                                     |
+---------------+------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| "LABELS"      | none             | Truth value showing whether label case sensitivity is ON (1 for "LOWER") or OFF (0 for "UPPER"); YottaDB defaults to 1.                                             |
+---------------+------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| "LINK"        | none             | Returns the current relink recursive setting of ZLINK.                                                                                                              |
+---------------+------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| "LV_CREF"     | local variable   | returns the total number of references to the data-space associated with an unsubscripted local variable name specified as a second expr (for example a quoted      |
|               | name (lvn)       | string). it returns a zero for a variable without any associated alias container.                                                                                   |
+---------------+------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| "LV_GCOL"     | none             | returns the number of data-spaces recovered during a local variable data-space garbage collection it triggers; such collections normally happen automatically at    |
|               |                  | appropriate times.                                                                                                                                                  |
+---------------+------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| "LV_REF"      | local variable   | returns the total number of references to the data-space associated with an unsubscripted local variable name specified as a second expr (for example a quoted      |
|               | name (lvn)       | string).                                                                                                                                                            |
+---------------+------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| "LVNULLSUBS"  | none             | Truth value showing whether null subscripts are permitted in local arrays (1 for "LVNULLSUBS") or not (0 for "NOLVNULLSUBS"); YottaDB defaults to 1.                |
+---------------+------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| "NOISOLATION" | global           | The current isolation-status of the specified global variable which must have a leading "^" in its specification.                                                   |
|               |                  |                                                                                                                                                                     |
|               |                  | This function returns 1 if YottaDB has been instructed to not enforce the ACID property of Isolation (i.e., "NOISOLATION" has been specified) and 0 otherwise.      |
|               |                  |                                                                                                                                                                     |
|               |                  | By default, YottaDB ensures Isolation, that is, a $VIEW command will return 0. The isolation-status of a global variable can be turned on and off by the VIEW       |
|               |                  | "NOISOLATION" command.                                                                                                                                              |
+---------------+------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| "PATCODE"     | none             | Name of the active patcode table; YottaDB defaults this to "M".                                                                                                     |
+---------------+------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| "POOLLIMIT"   | region           | The current limit on global buffers for the region .                                                                                                                |
+---------------+------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| "PROBECRIT"   | region           | Acquires and releases a critical section for the region (the "probe"), returning a string with the following field,                                                 |
|               |                  | some of of which always have zero (0) values because they are no longer used:                                                                                       |
|               |                  |                                                                                                                                                                     |
|               |                  | * CPT - nanoseconds for the probe to get the critical section                                                                                                       |
|               |                  |                                                                                                                                                                     |
|               |                  | * CFN - 0                                                                                                                                                           |
|               |                  |                                                                                                                                                                     |
|               |                  | * CQN - 0                                                                                                                                                           |
|               |                  |                                                                                                                                                                     |
|               |                  | * CYN - 0                                                                                                                                                           |
|               |                  |                                                                                                                                                                     |
|               |                  | * CQF - 0                                                                                                                                                           |
|               |                  |                                                                                                                                                                     |
|               |                  | * CQE - 0                                                                                                                                                           |
|               |                  |                                                                                                                                                                     |
|               |                  | * CAT - total of critical section acquisitions successes                                                                                                            |
+---------------+------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| "REGION"      | gvn              | Name of the region(s) holding the specified gvn. The region names are case-insensitive. The specified region name is converted to upper case before processing.     |
|               |                  | If gvn is :code:`"^*"`, the name of the default region.                                                                                                             |
|               |                  |                                                                                                                                                                     |
|               |                  | If gvn spans more than one region, this function returns region name in an order where the first region is the region to which the unsubscripted global variable    |
|               |                  | name maps; and other regions are in the order in which they would be encountered by traversing the subscripts of gvn in order (with duplicates removed).            |
|               |                  |                                                                                                                                                                     |
|               |                  | gvn is a subscripted or unsubscripted global variable name in the same form as that generated by $NAME(). You can use $NAME() inside $VIEW() to ensure that         |
|               |                  | subscripts are in a correct form, for example, $VIEW("REGION",$NAME(^abcd(1,2E4))) instead of $VIEW("REGION","^abcd(1,20000)").                                     |
+---------------+------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| "RTNCHECKSUM" | routine name     | Source code check-sum for the most recently ZLINK'd version of the specified routine name (these check-sums use a 128 bit hash based on the MurmurHash3 algorithm). |
+---------------+------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| "RTNNEXT"     | routine name     | Name of the next routine in the image after the given one; "" (empty string) for routinename starts with the first routine in ASCII collating sequence and a return |
|               |                  | value of the empty string indicates the end of the list.                                                                                                            |
+---------------+------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| "SPSIZE"      | none             | Returns a string with three comma separated values: Number of bytes currently allocated as process working storage: YottaDB manages this space as what is           |
|               |                  | commonly called a heap, and uses the term stringpool to refer to it. The YottaDB garbage collector reclaims unused space from the stringpool from time to time,     |
|               |                  | and YottaDB automatically expands the stringpool as needed by the application program; Number of bytes currently used by the process; Number of bytes reserved:     |
|               |                  | The reserved space is used to reduce the active memory usage, for example, when a process uses a large amount of memory then subsequently uses a significantly      |
|               |                  | reduced amount.                                                                                                                                                     |
+---------------+------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| "STATSHARE"   | region           | Returns 0 when the process has sharing disabled, 1 when it has sharing enabled, and 2 when sharing is enabled selectively for regions. For a process to store       |
|               |                  | statistics in the stats db, the database must be enabled for sharing and the process must have opted in to share. VIEW "STATSHARE" with no region argument enables  |
|               |                  | sharing for all regions and VIEW "STATSHARE":"REGION_NAME" enables sharing selectively for a region. $VIEW("STATSHARE","REGION_NAME") returns whether a process has |
|               |                  | opted to share statistics for a region.                                                                                                                             |
+---------------+------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| "STKSIZ"      | none             | Returns the YottaDB stack size in bytes.                                                                                                                            |
+---------------+------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| "TOTALBLOCKS" | region           | Total number of database blocks in a given region.                                                                                                                  |
+---------------+------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| "TRANSACTIONI | NULL or          | Transaction ID specified in the particular level (when the transaction level is specified). The first level TSTART is returned if the level is not specified as     |
| D"            | transaction level| second argument. A NULL string is returned if the specified level (explicitly or implicitly) is greater than the current value of $TLEVEL.                          |
+---------------+------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| "UNDEF"       | none             | Truth value showing whether undefined variables should be treated as having a null value (1 for "UNDEF"; 0 for "NOUNDEF"); YottaDB defaults to 0.                   |
+---------------+------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| "YGVN2GDS"    | string           | When string is the name of a global variable node, e.g., "^ACN(""NAME"",""TYPE"")", returns the bytes in a database block that store the name,                      |
|               | [,<collation>]   | e.g., "ACN"_$C(0,255)_"NAME"_$C(0,255)_"TYPE"_$C(0,0). An optional additional parameter is an alternative collation sequence number, which specifies the type of    |
|               |                  | collation desired. Refer to :ref:`colln-seq-defn` for more details on                                                                                               |
|               |                  | specifying alternative collation.                                                                                                                                   |
+---------------+------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| "YGDS2GVN"    | string           | When string contains the subscript representation of a global variable, returns the name of the global. An optional additional parameter is an alternative          |
|               | [,<collation>]   | collation sequence number, which specifies the type of collation desired. Refer to                                                                                  |
|               |                  | :ref:`colln-seq-defn`  for more details on specifying alternative collation.                                                                                        |
+---------------+------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| "ZDATE_FORM"  | none             | Integer value showing whether four digit year code is active for $ZDATE(); YottaDB defaults to 0 (for "YY" format). Use the environment variable ydb_zdate_form     |
|               |                  | to set the initial value of this factor. For usage examples, refer to “$ZDate()”.                                                                                   |
+---------------+------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------+

.. note::
   YottaDB uses the LC_CREF, LV_GCOL, LV_REF keywords in testing and is documenting them to ensure completeness in product documentation. They may (or may not) be useful during application development for debugging or performance testing implementation alternatives.

++++++++++++++++++++++++
Examples of $VIEW()
++++++++++++++++++++++++

Example:

.. code-block:: bash

   YDB>Set a=1,*b(1)=a
   YDB>write $view("LV_CREF","a")," ",$view("LV_CREF","b")
   1 0
   YDB>write $view("LV_REF","a")," ",$view("LV_REF","b")
   2 1
   YDB>

This example creates an alias variable and an alias container variable and checks the number of both container references and total references to the cells associated with both a and b.

Example:

.. code-block:: bash

   YDB>Set *a(1)=b,*b(1)=a
   YDB>kill *a,*b
   YDB>write $view("LV_GCOL")
   2
   YDB>

This example creates two cross associated alias containers, destroys their ancestor nodes with KILL * and uses $VIEW("LV_GCOL") to force a clean-up of the abandoned data-spaces. In the absence of the $VIEW("LV_GCOL"), YottaDB would do this automatically at some subsequent convenient time.

Example:

.. code-block:: bash

   YDB>write $view("GVSTAT","DEFAULT")
   SET:203,KIL:12,GET:203,DTA:2,ORD:23,ZPR:21,QRY:0,LKS:0,LKF:0,CTN:44,DRD:103,DWT:59,
   NTW:24,NTR:55,NBW:27,NBR:138,NR0:0,NR1:0,NR2:0,NR3:0,TTW:17,TTR:5,TRB:0,TBW:32,
   TBR:80,TR0:0,TR1:0,TR2:0,TR3:0,TR4:0,TC0:0,TC1:0,TC2:0,TC3:0,TC4:0,ZTR:7,DFL:9,
   DFS:0,JFL:0,JFS:0,JBB:0,JFB:0,JFW:0,JRL:0,JRP:0,JRE:0,JRI:0,JRO:0,JEX:0,DEX:0,
   CAT:35,CFE:0,CFS:0,CFT:0,CQS:0,CQT:0,CYS:0,CYT:0,BTD:13
   YDB>

These are statistics associated with the DEFAULT region. Refer to :ref:`zshow-info-codes` for information on the parameters.

Example:

Given the following global directory configuration:

.. code-block:: bash

   GDE>add -name a(1:10)      -region=a1
   GDE>add -name a(10,1)      -region=a2
   GDE>add -name a(10,2)      -region=a3
   GDE>add -name a(120:300)   -region=a4
   GDE>add -name a(60:325)    -region=a5
   GDE> show -name
    *** NAMES ***
   Global        Region
   ------------------------------------------------------------------------------
   *             DEFAULT
   a(1:10)       A1
   a(10,1)       A2
   a(10,2)       A3
   a(60:120)     A5
   a(120:300)    A4
   a(300:325)    A5

Here are some $VIEW("REGION",gvn) outputs:

.. code-block:: bash

   YDB>write $view("REGION","^a(1)")
   A1
   YDB>write $view("REGION","^a(10)")
   DEFAULT,A2,A3
   YDB>w $view("REGION","^a(60)")
   A5
   YDB>w $view("REGION","^a")
   DEFAULT,A1,A2,A3,A5,A4

Support for $VIEW("REGION","^*"), which returns the name of the region in the global directory mapped to by the * namespace, was added effective release `r1.30 <https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.30>`_.

.. _zahandle-function:

------------------------
$ZAHANDLE()
------------------------

$ZAHANDLE() returns a unique identifier (handle) for the array associated with a name or an alias container; for an subscripted lvn, it returns an empty string. To facilitate debugging, the handle is a printable string representation of a hexadecimal number. The only meaningful operation on the value returned by a call to $ZAHANDLE() is to compare it for equality with the value returned by another call. Changing nodes within the array doesn't change its handle. $ZAHANDLE() returns different results for copies of an array.

Example:

.. code-block:: bash

   YDB>set A=1,*B(1)=A
   YDB>write "$zahandle(A)=""",$zahandle(A),""" $zahandle(B(1))=""",$zahandle(B(1)),""""
   $zahandle(A)="17B8810" $zahandle(B(1))="17B8810"
   YDB>set A("Subscript")="Value" ; Change array - but $ZAHandle() does not change
   YDB>write "$zahandle(A)=""",$zahandle(A),""" $zahandle(B(1))=""",$zahandle(B(1)),""""
   $zahandle(A)="17B8810" $zahandle(B(1))="17B8810"
   YDB>merge D=A ; A copy of the data has a different $zahandle()
   YDB>Write "$ZAHandle(A)=""",$ZAHandle(A),""" $ZAHandle(D)=""",$ZAHandle(D),""""
   $zahandle(A)="17B8810" $zahandle(D)="17B8C10"
   YDB>

Since YottaDB does not provide a way for a function to return an array or alias variable as its result, the uniqueness of $ZAHandle() can be exploited to effect this capability, by placing the result in a local variable with an agreed prefix (e.g., "%") and its $ZAHANDLE() as a suffix. The handle can be returned as the value.

.. code-block:: none

   $ /usr/local/lib/yottadb/r120/ydb -run retval
   retval        ; Return an array / object from a function
      ;;Data for the object array
      ;;Albert Einstein,14-March-1879
      ;;Arthur Eddington,28-December-1882
      ;;
      zprint    ; Print this program
      new tmp1,tmp2,tmp3
      for i=3:1 set tmp1=$text(+i),tmp2=$piece(tmp1,";;",2) quit:'$length(tmp2)  do
      .set tmp3="%"_$$NewPerson($piece(tmp2,",",1),$piece(tmp2,",",2))
      .set @("*Relativists("_(i-2)_")="_tmp3)
      .kill @("*"_tmp3)
      kill tmp1,tmp2,tmp3
      write "------------",!
      write "Array of objects of relativists:",!
      zwrite
      quit
      ;
   NewPerson(name,birthdate)    ; Create new person object
      new lname,fname,dob,tmp1,tmp2 ; New variables used by this function
      set lname=$Piece(name," ",2),fname=$Piece(name," ",1)
      set dob=$$FUNC^%DATE(birthdate)
      set tmp1("fname")=fname,tmp1("lname")=lname,tmp1("dob")=dob
      set tmp2=$ZAHandle(tmp1)
      set @("*%"_tmp2_"=tmp1")
      quit tmp2
   ------------
   Array of objects of relativists:
   $ZWRTAC=""
   *Relativists(1)=$ZWRTAC1
   $ZWRTAC1("dob")=13952
   $ZWRTAC1("fname")="Albert"
   $ZWRTAC1("lname")="Einstein"
   *Relativists(2)=$ZWRTAC2
   $ZWRTAC2("dob")=15337
   $ZWRTAC2("fname")="Arthur"
   $ZWRTAC2("lname")="Eddington"
   i=5
   $ZWRTAC=""
   $

.. _zascii-function:

---------------------
$ZASCII()
---------------------

Returns the numeric byte value (0 through 255) of a given sequence of octets (8-bit bytes).

The format for the $ZASCII function is:

.. code-block:: none

   $ZA[SCII](expr[,intexpr])

* The expression is the sequence of octets (8-bit bytes) from which $ZASCII() extracts the byte it decodes.
* The optional integer expression contains the position within the expression of the byte that $ZASCII() decodes. If this argument is missing, $ZASCII() returns a result based on the first byte position. $ZASCII() starts numbering byte positions at one (1), (the first byte of a string is at position one (1)).
* If the explicit or implicit position is before the beginning or after the end of the expression, $ZASCII() returns a value of negative one (-1).
* $ZASCII() provides a means of examining bytes in a byte sequence. When used with $ZCHAR(), $ZASCII() also provides a means to perform arithmetic operations on the byte values associated with a sequence of octets (8-bit bytes).

+++++++++++++++++++++
Examples of $ZASCII()
+++++++++++++++++++++

Example:

.. code-block:: bash

   YDB>for i=0:1:4 write !,$zascii("主",i)

   -1
   228
   184
   187
   -1
   YDB>

This UTF-8 mode example displays the result of $ZASCII() specifying a byte position before, first, second and third positions, and after the sequence of octets (8-bit bytes) represented by 主. In the above example, 228, 184, and 187 represents the numeric byte value of the three-byte in the sequence of octets (8-bit bytes) represented by 主.

-----------------------
$ZATRANSFORM()
-----------------------

Returns the transformed representation of the first argument expr in a normalized form using the alternative transform specified by the second argument intexpr; the return can be used as an operand to the follows (]) or sorts-after (]]) operator such that, if both operands are in the normalized form, the result is independent of alternative collation. The format for the $ZATRANSFORM function is:


.. code-block:: none

   $ZATRANSFORM(expr,intexpr[,{0|1|2|-2}][,{0|1}])

* The expression specifies the string to transform.
* The intexpr specifies the ID of the alternative transform to use.
* The optional third argument specifies :

     * zero (0): the transform is to normalized form
     * one (1): the reverse transform from the normalized to the native form
     * two (2): the character which collates immediately after the first character of the first argument, or the empty string if no character does
     * minus two (-2): the character which collates immediately before the first character of the first argument, or the empty string if no character does
* The optional fourth argument specifes whether to use standard M collation of numbers before strings, the default or zero (0), or to sort all values as strings (1).

$ZATRANSFORM() options were modified in YottaDB effective release `r1.32 <https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.32>`_.

Please see the section on $ZCOLLATE() for a similar alternative.

+++++++++++++++++++++++++++
Examples of $ZATRANSFORM()
+++++++++++++++++++++++++++

Example:

.. code-block:: bash

   YDB>write $zatransform("John Smythe",1)]$zatransform("Jane Smith",2)
   0
   YDB>

This example uses $ZATRANSFORM() and two (here unspecified) collation definitions to compare the ordering of two (literal) expressions as YottaDB would collate them if there was a way to collate them together. The result indicates that the first would collate before the second.

---------------------
$ZBIT Functions
---------------------

A series of functions beginning with $ZBIT lets you manipulate a bit stream. Internally, YottaDB stores a bit stream in the form of a bit string. A bit string embeds a bit stream in such a way that the first byte specifies the number of trailing bits in the last byte that are not part of the bit-stream. In this way, YottaDB is able to store bit-streams of lengths other than multiples of 8 bits in byte format. So for example, a first byte of value of zero (0) indicates that all of the bits in the last byte belong to the bit-stream, while a one (1) indicates the last bit is excluded and a seven (7) indicates that only the first bit in the last byte belongs to the bit-stream.

If you have to convert a character string into a bit string then add a leading byte to that character string so that all $ZBIT functions can recognize it. The most common and straightforward way of doing this is to concatenate a $CHAR(n) on the front of the character string, where the value of n is zero through seven (0-7) – most commonly zero (0). If you pass a bit string as an argument to a routine that is expecting a character string, then that caller routine must strip off the first (and possibly the last) byte so that it can recognize the character string.

This section contains the description of all $ZBIT function and an example of using $ZBIT functions to turn a character into a bit stream and return a coded value. However, the most appropriate use of these functions may include the formation of checksums, handling of bit-data (say pixels from a scan), or interfacing with a routine that requires bit-oriented arguments.

++++++++++++++
$ZBITAND()
++++++++++++++

Performs a logical AND function on two bit strings and returns a bit string equal in length to the shorter of the two arguments (containing set bits in those positions where both of the input strings have set bits). Positions corresponding to positions where either of the input strings have a cleared bit, also have cleared bits in the resulting string.

The format for the $ZBITAND function is:

.. code-block:: none

   $ZBITAND(expr1,expr2)

* The first expression specifies one of the bit strings that is input to the AND operation.
* The second expression specifies the other bit string that is input to the AND operation.

**Example of $ZBITAND()**

.. code-block:: bash

   YDB>
   ; The binary representation of A is 01000001
   YDB>Set BITSTRINGB=$zbitset($zbitset($zbitstr(8,0),2,1),7,1)
   ; The binary representation of B is 01000010
   YDB>set BITSTRINGAB=$zbitand(BITSTRINGA,BITSTRINGB)
   YDB>for i=1:1:8 write $zbitget(BITSTRINGAB,I)
   01000000

This examples uses $ZBITAND to perform a bitwise AND operation on A and B.

.. code-block:: none

   A= 01000001
   B= 01000010
   A bitwise AND B=0100000

++++++++++++++++
$ZBITCOUNT()
++++++++++++++++

Returns the number of ON bits in a bit string.

The format for the $ZBITCOUNT function is:

.. code-block:: none

   $ZBITCOUNT(expr)

The expression specifies the bit string to examine.

**Example of $ZBITCOUNT()**

Example:

.. code-block:: bash

   YDB>set BITSTRINGA=$ZBITSET($ZBITSET($ZBITSTR(8,0),2,1),8,1)
   ; The binary representation of A is 01000001
   YDB>set BITSTRINGB=$zbitset($zbitset($zbitstr(8,0),2,1),7,1)
   ; The binary representation of B is 01000010
   YDB>Set BITSTRINGC=$zbitor(BITSTRINGA,BITSTRINGB)
   ; A OR B=01000011
   YDB>write $zbitcount(BITSTRINGA)
   2
   YDB>write $zbitcount(BITSTRINGB)
   2
   YDB>write $zbitcount(BITSTRINGC)
   3
   YDB>

This example displays the number of ON bits in BITSTRINGA, BITSTRINGB, and BITSTRINGC.

+++++++++++++++++++
$ZBITFIND()
+++++++++++++++++++

Performs the analog of $FIND() on a bit string. It returns an integer that identifies the position after the first position equal to a truth-valued expression that occurs at, or after, the specified starting position.

The format for the $ZBITFIND function is:

.. code-block:: none

   $ZBITFIND(expr,tvexpr[,intexpr])

* The expression specifies the bit string to examine.
* The truth-valued expression specifies the bit value for which $ZBITFIND() searches (1 or 0).
* The optional integer argument specifies the starting position at which to begin the search. If this argument is missing, $ZBITFIND() begins searching at the first position of the string. $ZBIT functions count the first bit as position one (1).

If the optional integer argument exceeds the length of the string, or if the function finds no further bits, $ZBITFIND() returns a zero value.

**Examples of $ZBITFIND()**

Example:

.. code-block:: bash

   YDB>Set BITSTRINGA=$ZBITSET($ZBITSET($ZBITSTR(8,0),2,1),8,1)
   ; The binary representation of A is 01000001
   YDB>write $zbitfind(BITSTRINGA,1,3)
   9
   YDB>

This example searches for bit value 1 starting from the 3rd bit of BITSTRINGA.

+++++++++++++++++++++++
$ZBITGET()
+++++++++++++++++++++++

Returns the value of a specified position in the bit string.

The format for the $ZBITGET function is:

.. code-block:: none

   $ZBITGET(expr,intexpr)

* The expression specifies the bit string to examine.
* The integer argument specifies the position in the string for which the value is requested. If the integer argument is negative, zero, or exceeds the length of the bit string, it is rejected with a run-time error. $ZBIT functions count the first bit as position one (1).

**Examples of $ZBITGET()**

Example:

.. code-block:: bash

   YDB>set BITSTRINGA=$zbitset($zbitset($zbitstr(8,0),2,1),8,1)
   ; The binary representation of A is 01000001
   YDB>for i=1:1:8 write $zbitget(BITSTRINGA,I)
   01000001
   YDB>

This examples uses $ZBITGET() to display the binary representation of A.

++++++++++++++
$ZBITLEN()
++++++++++++++

Returns the length of a bit string, in bits.

The format for the $ZBITLEN function is:

.. code-block:: none

   $ZBITLEN(expr)

The expression specifies the bit string to examine.

**Examples of $ZBITLEN()**

Example:

.. code-block:: bash

   YDB>set BITSTR=$zbitstr(6,1)

   YDB>write $zbitlen(BITSTR)
   6
   YDB>

This example displays the length of a bit string of 6 bits.

+++++++++++++++++++++
$ZBITNOT()
+++++++++++++++++++++

Returns a copy of the bit string with each input bit position inverted.

The format for the $ZBITNOT function is:

.. code-block:: none

   $ZBITNOT(expr)

The expression specifies the bit string whose inverted bit pattern becomes the result of the function.

**Examples of $ZBITNOT()**

.. code-block:: bash

   YDB>set BITSTRINGA=$zbitset($zbitset($zbitstr(8,0),2,1),8,1)
   ; The binary representation of A is 01000001
   YDB>for i=1:1:8 write $zbitget($zbitnot(BITSTRINGA),I)
   10111110
   YDB>

This example displays inverted bits for all the bits in BITSTRINGA.

++++++++++++++
$ZBITOR()
++++++++++++++

Performs a bitwise logical OR on two bit strings, and returns a bit string equal in length to the longer of the two arguments (containing set bits in those positions where either or both of the input strings have set bits). Positions that correspond to positions where neither input string has a set bit have cleared bits in the resulting string.

The format for the $ZBITOR function is:

.. code-block:: none

   $ZBITOR(expr1,expr2)

* The first expression specifies one of the bit strings that is input to the OR operation.
* The second expression specifies the other bit string that is input to the OR operation.

**Examples of $ZBITOR()**

Example:

.. code-block:: bash

   YDB>set BITSTRINGA=$zbitset($zbitset($zbitstr(8,0),2,1),8,1)
   ; The binary representation of A is 01000001
   YDB>set BITSTRINGB=$zbitset($zbitset($zbitstr(8,0),2,1),7,1)
   ; The binary representation of B is 01000010
   YDB>set BITSTRINGC=$zbitor(BITSTRINGA,BITSTRINGB)
   ; A OR B=01000011
   YDB>write BITSTRINGC
   C
   YDB>

This example displays the result of BITSTRINGA bitwise ORed with BITSTRINGB.

+++++++++++++++++++++
$ZBITSET()
+++++++++++++++++++++

Returns an edited copy of the input bit string with a specified bit set to the value of the truth-valued expression.

The format for the $ZBITSET function is:

.. code-block:: none

   $ZBITSET(expr,intexpr,tvexpr)

* The expression specifies the input bit string.
* The integer expression specifies the position of the bit to manipulate. Arguments that are negative, zero, or exceed the length of the bit string produce a run-time error. $ZBIT functions count the first bit as position one (1).
* The truth-valued expression specifies the value to which to set the specified bit (0 or 1).

**Examples of $ZBITSET()**

Example:

.. code-block:: bash

   YDB>set X="A",Y=$extract($zbitset($char(0)_X,3,1),2) zwrite
   X="A"
   Y="a"

This example changes the case of the ASCII letter A to the corresponding lowercase version.

+++++++++++++++
$ZBITSTR()
+++++++++++++++

Returns a bit string of a specified length with all bit positions initially set to either zero or one.

The format for the $ZBITSTR function is:

.. code-block:: none

   $ZBITSTR(intexpr[,tvexpr])

* The integer expression specifies the length of the bit string to return; arguments that exceed the maximum length of 253,952 produce a run-time error.
* The optional truth-valued expression specifies the value to which all bit positions should initially be set (0 or 1). If this argument is missing, the bits are set to zero.

**Examples of $ZBITSTR()**

.. code-block:: bash

   YDB>set BITSTR=$zbitstr(6,1)

This example sets the value of expression BITSTR to 6 bit with all bits set to 1.

+++++++++++++++
$ZBITXOR()
+++++++++++++++

Performs a bitwise exclusive OR on two bit strings, and returns a bit string equal in length to the shorter of the two arguments (containing set bits in those positions where either (but not both) of the input strings have set bits). Positions that correspond to positions where neither or both input strings have a set bit have cleared bits in the resulting string.

The format for the $ZBITXOR function is:

.. code-block:: none

   $ZBITXOR(expr1,expr2)

* The first expression specifies one of the bit strings that is input to the XOR operation.
* The second expression specifies the other bit string that is input to the XOR operation.

**Examples of $ZBITXOR()**

.. code-block:: bash

   YDB>set BITSTRINGA=$zbitset($zbitset($zbitstr(8,0),2,1),8,1) ; The binary representation of A is 01000001
   YDB>set BITSTRINGB=$zbitset($zbitset($zbitstr(8,0),2,1),7,1); The binary representation of B is 01000010
   YDB>set BITSTRINGC=$zbitor(BITSTRINGA,BITSTRINGB) ; A XOR B=00000011
   YDB>for I=1:1:8 write $zbitget(BITSTRINGC,I)
   00000011
   YDB>

This example displays the result of the bitwise XOR of A and B.

++++++++++++++++++++++++++++
Examples of $ZBIT Functions
++++++++++++++++++++++++++++

Example:

.. code-block:: none

   ZCRC(X)
    new R,I,J,B,X1,K
    set R=$zbitstr(8,0)
    for I=1:1:$length(X) Set R=$zbitxor(R,$$bitin($A(X,I)))
    quit $$bitout(R)

   bitin(X) ;CONVERT A BYTE TO A BIT STRING
     set X1=$zbitstr(8,0)
     for J=1:1:8 set B=X#2,X=X\2 if B set X1=$zbitset(X1,J,1)
     quit X1

   bitout(X) ; CONVERT A BITSTRING TO A NUMBER
     set X1=0
     for K=1:1:8 I $zbitget(X,K) set X1=X1+(2**(K-1))
     quit X1

This uses several $ZBIT functions to turn a character into a bit stream and return a coded value.

While this example illustrates the use of several of the $ZBIT functions, the following example produces identical results if you need to code the function illustrated above for production.

.. code-block:: none

   ZCRC(X)
    new R,I,J,B,X1,K
    set R=$zbitstr(8,0)
    for I=1:1:$length(X) Set R=$zbitxor(R,$char(0)_$extract(X,I))
    quit $ascii(R,2)

This example illustrates the use of $Char() to specify the number of invalid bits that exist at the end of the character string. In this case there are zero invalid bits.

.. _zchar-function:

---------------
$ZCHAR()
---------------

Returns a string composed of bytes represented by the integer octet values specified in its argument(s).

The format for the $ZCHAR function is:

.. code-block:: none

   $ZCH[AR](intexpr[,...])

* The integer expression(s) specify the numeric byte value of the byte(s) $ZCHAR() returns.
* YottaDB limits the number of arguments to a maximum of 254. $ZCHAR() provides a means of producing byte sequences. In the UTF-8 mode, $ZCHAR() returns a malformed characters for numeric byte values 128 to 255. In the M mode, $ZCHAR() can create valid UTF-8 characters that includes bytes in the range 128-255.

.. note::
   The output of $ZCHAR() for values of integer expression(s) from 0 through 127 does not vary with choice of the character encoding scheme. This is because 7-bit ASCII is a proper subset of UTF-8 character encoding scheme. The representation of characters returned by $ZCHAR() for values 128 through 255 differ for each character encoding scheme.

* When used with $ZASCII(), $ZCHAR() can also perform arithmetic operations on the byte values of the bytes associated with a sequence of octets (8-bit bytes).

++++++++++++++++++++
Example of $ZCHAR()
++++++++++++++++++++

Example:

.. code-block:: bash

  YDB>write $zchar(228,184,187,7)
  主
  YDB>

This example WRITEs the byte sequence represented by 主 and signals the terminal bell.

-----------------
$ZCOLLATE()
-----------------

Returns the transformed representation of the first argument glvn in a normalized form using the alternative transform specified by the second argument intexpr; the return can be used as an operand to the follows (]) or sorts-after (]]) operator such that, if both operands are in the normalized form, the result is independent of alternative collation.

The format for the $ZCOLLATE function is:

.. code-block:: none

   $ZCO[LLATE](glvn,intexpr[,{0|1}])

* The subscripted or unsubscripted global or local variable name specifies the key to transform.
* The integer expression specifies the ID of the alternative transform to use.
* The optional third argument specifies whether the transform is to normalized form, by default or if zero (0), or, if one (1), the reverse transform from the normalized to the native form.

Note that because the forward transform is to the GDS global storage format, the reverse transform always shows a global form. This is not material when the result is used for most comparisons, but for some uses the application might need to remove the leading up-arrow (^).

Please see the section on $ZATRANSFORM() for a similar alternative.

++++++++++++++++++++++++
Examples of $ZCOLLATE()
++++++++++++++++++++++++

Example:

.. code-block:: bash

   YDB>write $zwrite($zcollate("A(""foo"")",0))
   "A"_$C(0,255)_"foo"_$C(0,0)
   YDB>write $zcollate($zcollate("A(""foo"")",0),0,1)
   ^A("foo")
   YDB>

The first WRITE in this example shows the readable form or the value produced by the $ZCOLLATE(), revealing the details of how YottaDB internally represents this key using default (M) collation. The second WRITE shows how the combination of the transform and reverse transform restores the value to the original representation.

.. _zconvert-function:

-------------------
$ZCONVERT()
-------------------

Returns its first argument as a string or value converted to a different encoding or numeric base. The two argument form changes the encoding for case within the ASCII character set. The three argument form changes the encoding scheme or base. Supported bases are decimal (:code:`"DEC"` and :code:`"HEX"`, case insensitive).

The format for the $ZCONVERT function is:

.. code-block:: none

   $ZCO[NVERT](expr1, expr2,[expr3])

* The first expression is the string or value to convert. $ZCONVERT() generates a run-time error if for Unicode conversion if the string contains a code-point value that is not in the character set, or for base conversion if the value to be converted is out of range.
* In the two argument form, the second expression specifies a code that determines the form of the result.
* In the three-argument form
  * The second expression is a code that specifies the character set or base of the first argument.
  * The third expression is a code that specifies the character set or base of the result. If the expression does not evaluate to one of the defined codes, $ZCONVERT() generates a run-time argument. The three-argument form for character set conversion is supported only in UTF-8 mode.

$ZCONVERT() generates a run-time error if the second or third expression is not a valid code or a supported base. Valid bases are case-insensitive :code:`"DEC"` and :code:`"HEX"`. The valid (case insensitive) character codes for expr2 in the two-argument form are:

* U converts the string to UPPER-CASE. "UPPER-CASE" refers to words where all the characters are converted to their "capital letter" equivalents. $ZCONVERT() retains characters already in UPPER-CASE "capital letter" form unchanged.
* L converts the string to lower-case. "lower-case" refers to words where all the letters are converted to their "small letter" equivalents. $ZCONVERT() retains characters already in lower-case or having no lower-case equivalent unchanged.
* T converts the string to title case. "Title case" refers to a string with the first character of each word in upper-case and the remaining characters in the lower-case. $ZCONVERT() retains characters already conforming to "Title case" unchanged. "T" (title case) is not supported in M mode.

.. note::
   When UTF-8 mode is enabled, YottaDB uses the ICU Library to perform case conversion. As mentioned in the Theory of Operation section, the case conversion of the strings occurs according to Unicode code-point values. This may not be the linguistically or culturally correct case conversion. Therefore, you must ensure that the actual case conversion is linguistically and culturally correct for your specific needs. The two-argument form of the $ZCONVERT() function in M mode does not use the ICU Library to perform operations related to the case conversion of the strings.

The valid (case insensitive) codes for character set encoding for expr2 and expr3 in the three-argument form are:

* "UTF-8"-- a multi-byte variable length Unicode® encoding form.
* "UTF-16LE"-- a multi-byte 16-bit Unicode® encoding form in little-endian.
* "UTF-16BE"-- a multi-byte 16-bit Unicode® encoding form in big-endian.
* "UTF-16"-- a multi-byte 16-bit Unicode® encoding form which uses the same endian level as that of the current system.

.. note::
   As YottaDB Unicode support uses UTF-8, and not other encodings, invoking functions such as $LENGTH() on UTF-16 strings are likely to result in BADCHAR errors. Conversion to and from UTF-16 encodings exists primarily to support input and output of UTF-16 data.

For numeric conversion:

* Unsigned numbers in the range 0 through 0xFFFFFFFFFFFFFFFF (64-bit unsigned integers) can be converted. Decimal return values greater than 999999999999999999 (18 decimal digits, YottaDB's maximum numeric size) are returned as strings.
* Hexadecimal numbers are always converted to positive decimal numbers.
* As conversion from hexadecimal numbers preceded by "-" to decimal is not considered meaningful, if the number to be converted is a “negative” hexadecimal number (e.g., "-F"), the result is 0.
* Conversion from negative decimal numbers to hexadecimal returns the hexadecimal value of the 2's complement of the number, e.g., the value of $ZCONVERT(-23,"DEC","HEX") is "E9"

++++++++++++++++++++++++
Examples of $ZCONVERT()
++++++++++++++++++++++++

Example:

.. code-block:: bash

   YDB>write $zchset
   M
   YDB>write $zconvert("Happy New Year","U")
   HAPPY NEW YEAR
   YDB>

Example:

.. code-block:: bash

   YDB>write $zchset
   UTF-8
   YDB>write $zconvert("HAPPY NEW YEAR","T")
   Happy New Year
   YDB>

Example:

.. code-block:: bash

   YDB>set x="FFFF" write $zconvert(x,"hex","dec")
   65535
   YDB>kill x,y set $piece(x,"F",17)="" set y=$zconvert(x,"hex","dec") write x," ",y
   FFFFFFFFFFFFFFFF 18446744073709551615
   YDB>kill x,y set $piece(x,"9",19)="" set y=$zconvert(x,"dec","hex") write x," ",y
   999999999999999999 DE0B6B3A763FFFF
   YDB>

Base conversion was added to $ZCONVERT() effective release `r1.30 <https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.30>`_.

.. _zdata-function:

-------------------
$ZDATA()
-------------------

Extends $DATA() to reflect the current alias state of the lvn or name argument to identify alias and alias container variables. It treats variables joined through pass-by-reference as well as TP RESTART variables within a transaction as alias variables. However, it does not distinguish nodes having alias containers among their descendants.

In addition to the four standard M results from $DATA(), $ZDATA() returns:

* 100 for an uninitialized alias or alias container
* 101 for an alias or alias container with no descendants
* 111 for an alias or alias container with descendants

Existing $DATA() tests for data and descendants report on alias and alias container variables, as well as other variables in the standard fashion. When an application uses alias and alias container variables $ZDATA() supplies additional information when needed.

++++++++++++++++++++++
Examples for $ZDATA()
++++++++++++++++++++++

Example:

.. code-block:: bash

   YDB>set a=1,*b(1)=a,*c=d
   YDB>write $data(a)," ",$zdata(a)
   1 101
   YDB>write $data(b)," ",$zdata(b)
   10 10
   YDB>write $data(c)," ",$zdata(c)
   0 100
   YDB>write $data(d)," ",$zdata(d)
   0 100
   YDB>write $data(b(1))," ",$zdata(b(1))
   1 101
   YDB>set b(1,2)=2
   YDB>write $data(b(1))," ",$zdata(b(1))
   11 111
   YDB>write $data(b(1,2))," ",$zdata(b(1,2))
   1 1
   YDB>

.. _zdate-function:

-----------------
$ZDATE()
-----------------

Returns a date and/or time formatted as text based on an argument formatted in the manner of $HOROLOG. For information on the format of $HOROLOG, refer to `Chapter 8: “Intrinsic Special Variables” <./isv.html>`_.

The format for the $ZDATE function is:

.. code-block:: none

   $ZD[ATE](expr1[,expr2[,expr3[,expr4]]]])

* The first expression specifies in $HOROLOG format the date and/or time that $ZDATE() returns in text format. If the output requires only the date or the time, the other piece of the argument that is delimited by a comma (,) may be null.
* The optional second expression specifies a string providing $ZDATE() with a "picture" of the desired output format. If this argument is missing or null, $ZDATE() uses the default format string "MM/DD/YY". If the optional second expression exceeds 64 characters, $ZDATE() generates a run-time error.
* The optional third expression specifies a list of 12 month codes, separated by commas (,), that $ZDATE() uses in formatting text months called for by the "MON" picture, (that is, $ZDATE() outputs $PIECE(expr3,",",month-number) when "MON" appears in the second expression). If this argument is missing or null, $ZDATE() uses three-character English abbreviations for months.
* The optional fourth expression specifies a list of seven day codes, separated by commas (,), which $ZDATE() uses in formatting text days of the week called for by the "DAY" picture, $ZDATE() outputs $PIECE (expr4,",",day-of-week-number) when "DAY" appears in the second expression; if this argument is missing or null, $ZDATE() uses three-character English abbreviations for days of the week.
* $ZDATE() returns 31-Dec-1840 as a date representation of day 0.

$ZDATE() provides an easy and flexible tool for putting M internal date/time ($HOROLOG) formats into more user-friendly formats.

.. note::
   $ZDATE() generates an error for input date values greater than 31-Dec-999999 (364570088) or less than 01-JAN-1840 (-365) and for time values greater than a second before midnight (86399) or less than 0 (zero).

The Intrinsic Special Variable $ZDATEFORM determines the output format for years. The default value is zero (0), in which case $ZDATE() with one argument (no format specification) uses a "YY" (two digit) format for all years. If $ZDATEFORM is one (1), a "YYYY" (four digit) format is used for years later than 1999. For all other values of $ZDATEFORM, "YYYY" (four digit) format is used for all years. $ZDATEFORM does not affect $ZDATE() when the format argument is specified.

The following table summarizes the usage of $ZDATE() when only first argument is specified.

+-------------------------------------------+---------------------------------------------------------------+
| Value of $DATEFORM                        | $ZDATE() Output Format                                        |
+===========================================+===============================================================+
| 0                                         | 2 digits                                                      |
+-------------------------------------------+---------------------------------------------------------------+
| 1                                         | 4 digits for years 2000 and after                             |
|                                           |                                                               |
|                                           | 2 digits otherwise (for years ranging between 1840, 1999)     |
+-------------------------------------------+---------------------------------------------------------------+
| other                                     | 4 digits                                                      |
+-------------------------------------------+---------------------------------------------------------------+

++++++++++++++++++++++++++++++++++++++++++
$ZDATE() Format Specifification Elements
++++++++++++++++++++++++++++++++++++++++++

This section lists the $ZDATE format specification elements. $ZDATE() format specifications must appear in upper case. When any alphabetic characters in format specifications are in lower case, $ZDATE() generates a run-time error.

YY: Outputs the rightmost two digits of the year.

YEAR: Outputs the year as a four-digit number.

YYYYYY: Outputs the year as a six-digit number.

MM: Outputs the month as a two-digit zero-filled number between 01 and 12.

MON: Outputs the month as a three-letter abbreviation. (You can modify the output further using expr3).

DD: Outputs the day of the month as a two-digit zero-filled number between 01 and 31.

DAY: Outputs the day of the week as a three-letter abbreviation. (You can modify the output further using expr4).

24: Outputs the hour of the day as a zero-filled number between 00 and 23.

12: Outputs the hour of the day as a zero-filled number between 01 and 12.

60: Outputs the minute of the hour as a zero-filled number between 00 and 59.

SS: Outputs the second of the minute as a zero-filled number between 00 and 59.

AM: Outputs the letters AM and PM depending on the time.

\+: Inserts a plus sign (+) in the output string

\-: Inserts a minus sign (-) in the output string.

\.: Inserts a period (.) in the output string.

,: Inserts a comma (,)in the output string.

/: Inserts a slash (/) in the output string.

\:: Inserts a colon (:) in the output string.

;: Inserts a semi-colon (;) in the output string.

\*: Inserts an asterisk (*) in the output string.

.. note::
   A blank space inserts a blank space in the output string.

+++++++++++++++++++++
Examples of $ZDATE()
+++++++++++++++++++++

Example:

.. code-block:: bash

   YDB>write $horolog,!,$zdate($H)
   62109,60946
   01/18/18
   YDB>

This displays $HOROLOG and then uses $ZDATE() to display today's date. The output shown would appear if today were the eighteenth day of January, 2018.

Example:

.. code-block:: bash

   YDB>write $zdate($H,"DD-MON-YEAR")
   18-JAN-2018
   YDB>

This uses the second argument to specify a text format different from the default.

Example:

.. code-block:: bash

   YDB>set m="Januar,Februar,Marz,April,Mai,Juni,Juli,August,"
   YDB>set m=m_"September,October,November,Dezember"
   YDB>write $zdate($horolog,"DD-MON-YEAR",m)
   18-Januar-2018
   YDB>

This is similar to the prior example, however it uses the third argument to specify the months in German.

Example:

.. code-block:: bash

   YDB>set d="Dimanche,Lundi,Mardi,Mercredi,Jeudi,Vendredi,Samedi"
   YDB>write $zdate($H,"DAY, DD/MM/YY","",d)
   Mardi, 18/01/2018
   YDB>

This example displays the eighteenth of January, however it uses the fourth argument to specify the days of the week in French.

Example:

.. code-block:: bash

   YDB>write !,$zdate($H,"12:60:SS AM")
   10:35:51 PM
   YDB>

This example shows hours, minutes, and seconds in a 12 hour clock with an AM/PM indicator.

Example:

.. code-block:: bash

   YDB>write !,$zdate(",36524","24-60")
   10-08
   YDB>

This example shows hours and minutes on a 24 hour clock. Notice that the first argument must provide the time in the second comma delimiter piece to match $HOROLOG format.

Example:

.. code-block:: bash


  YDB>write $zdateform
  0
  YDB>write $zdate($H)
  01/18/18
  YDB>set $zdateform=1
  YDB>write $zdate($horolog)
  01/18/2018
  YDB>write $zdate($horolog,"MM/DD/YY")
  01/18/18

This example converts the output format for years from the default ("YY") format to the four digit format ("YYYY") using the Intrinsic Special Variable $ZDATEFORM.

Example:

.. code-block:: bash

   YDB>write $zdate(123456789,"DAY MON DD, YYYYYY")
   FRI MAR 17, 339854
   YDB>

This example displays year as a six-digit number.

.. _zextract-function:

---------------------
$ZEXTRACT()
---------------------

Returns a byte sequence from a given sequence of octets (8-bit bytes).

The format for the $ZEXTRACT function is:

.. code-block:: none

   $ZE[XTRACT](expr[,intexpr1[,intexpr2]])

* The expression specifies a sequence of octets (8-bit bytes) from which $ZEXTRACT() derives a byte sequence.
* The first optional integer expression (second argument) specifies the starting byte position in the byte string. If the starting position is beyond the end of the expression, $ZEXTRACT() returns an empty string. If the starting position is zero (0) or negative, $ZEXTRACT() starts at the first byte position in the expression; if this argument is omitted, $ZEXTRACT() returns the first byte. $ZEXTRACT() numbers byte positions starting at one (1) (the first byte of a sequence of octets (8-bit bytes) is at position one (1)).
* The second optional integer expression (third argument) specifies the ending byte position for the result. If the ending position is beyond the end of the expression, $ZEXTRACT() stops with the last byte of the expression. If the ending position precedes the starting position, $ZEXTRACT() returns null. If this argument is omitted, $ZEXTRACT() returns one byte.
* $ZEXTRACT() provides a tool for manipulating strings based on byte positions.
* As $ZEXTRACT() operates on bytes, it can produce a string that is not well-formed according to the UTF-8 character set.

+++++++++++++++++++++++++++++
Examples of $ZEXTRACT()
+++++++++++++++++++++++++++++

Example:

.. code-block:: bash

   YDB>Set A="主要雨在西班牙停留在平原"

   YDB>For i=0:1:$zlength(A)
   YDB>write !,$zascii($zextract(A,i)),"|"
   YDB>

This example displays the numeric byte sequence of the sequence of octets ("主要雨在西班牙停留在平原").

.. _zfind-function:

-----------------------
$ZFIND()
-----------------------

Returns an integer byte position that locates the occurrence of a byte sequence within a sequence of octets(8-bit bytes).

The format of the $ZFIND function is:

.. code-block:: none

   $ZF[IND](expr1,expr2[,intexpr])

* The first expression specifies the sequence of octets (8-bit bytes) in which $ZFIND() searches for the byte sequence.
* The second expression specifies the byte sequence for which $ZFIND() searches.
* The optional integer expression identifies the starting byte position for the $ZFIND() search. If this argument is missing, zero (0), or negative, $ZFIND() begins to search from the first position of the sequence of octets (8-bite bytes).
* If $ZFIND() locates the byte sequence, it returns the position after its last byte. If the end of the byte sequence coincides with the end of the the sequence of octets (expr1), it returns an integer equal to the byte length of the expr1 plus one ($L(expr1)+1).
* If $ZFIND() does not locate the byte sequence, it returns zero (0).
* $ZFIND() provides a tool to locate byte sequences. The ([) operator and the two-argument $ZLENGTH() are other tools that provide related functionality.

+++++++++++++++++++++++
Examples of $ZFIND()
+++++++++++++++++++++++

Example:

.. code-block:: bash

   YDB>write $zfind("主要雨",$zchar(187))
   4
   YDB>

This example uses $ZFIND() to WRITE the position of the first occurrence of the numeric byte code 150. The return of 3 gives the position after the "found" byte.

Example:

.. code-block:: bash

   YDB>write $zfind("新年好",$zchar(229),5)
   8
   YDB>

This example uses $ZFIND() to WRITE the position of the next occurrence of the byte code 229 starting in byte position five.

Example:

.. code-block:: none

   YDB>set t=1 for  set t=$zfind("新年好",$zchar(230,150,176),t) quit:'t  write !,t
   4
   YDB>

This example uses a loop with $ZFIND() to locate all the occurrences of the byte sequence $ZCHAR(230,150,176) in the sequence of octets ("新年好"). The $ZFIND() returns 4 giving the position after the occurrence of byte sequence $ZCHAR(230,150,176).

----------------
$ZGETJPI()
----------------

Returns job or process information of the specified process. The format for the $ZGETJPI function is:

.. code-block:: none

   $ZGETJPI(expr1,expr2)

* expr1 identifies the PID of the target job. If expr1 is an empty string (""), $ZGETJPI() returns information about the current process.
* expr2 specifies the item keyword identifying the type of information returned; keywords may be upper, lower, or mixed-case. The keywords are as follows:

**ZGETJPI()**

+----------------------------+--------------------------------------------------------------------------------------------------------------------------------------+
| Keywords                   | Data Returned                                                                                                                        |
+============================+======================================================================================================================================+
| ISPROCALIVE                | Determines whether the specified process is alive.                                                                                   |
+----------------------------+--------------------------------------------------------------------------------------------------------------------------------------+
| CPUTIM                     | Total process and child CPU time used in hundredths of a second.                                                                     |
+----------------------------+--------------------------------------------------------------------------------------------------------------------------------------+
| CSTIME                     | System time of child processes                                                                                                       |
+----------------------------+--------------------------------------------------------------------------------------------------------------------------------------+
| CUTIME                     | User time of child processes                                                                                                         |
+----------------------------+--------------------------------------------------------------------------------------------------------------------------------------+
| STIME                      | Process system time                                                                                                                  |
+----------------------------+--------------------------------------------------------------------------------------------------------------------------------------+
| UTIME                      | Process user time                                                                                                                    |
+----------------------------+--------------------------------------------------------------------------------------------------------------------------------------+

* Note that the $ZGETJPI() retrieves process time measurements (CPUTIM, CSTIME, CUTIME, STIME, and UTIME) only of the current process ($JOB). The "child" process time includes ZSYSTEM and PIPE device sub-processes (only after the PIPE CLOSEs), but excludes processes created by JOB commands.
* $ZGETJPI() provides a tool for examining the characteristics of a UNIX process. Accessing information about processes belonging to other users requires certain UNIX privileges. Consult your system manager if you require additional privileges.

+++++++++++++++++++++++
Examples of $ZGETJPI()
+++++++++++++++++++++++

Example:

.. code-block:: bash

   YDB>write $zgetjpi(1975,"isprocalive")
   1
   YDB>

This uses $ZGETJPI() to determine whether process 1975 is alive.

Example:

.. code-block:: bash

   YDB>set t=$zgetjpi("","cputim")
   YDB>do ^bench write $zgetjpi("","cputim")-t
   1738
   YDB>

This uses $ZGETJPI() to measure the actual CPU time, measured in hundredths of a second, consumed by performing the BENCH routine.

--------------------
$ZJOBEXAM()
--------------------

.. code-block:: none

   $ZJOBEXAM([expr1[,expr2]])

Returns the full specification of the file specified by the optional expr1 argument into which the function places a ZSHOW output specified by expr2. The return value is the name of the file. YottaDB reports each $ZJOBEXAM() to the operator log facility, along with its file specification.

The optional expr1 argument is a template output device specification. It can be a device, a file directory, or a file name. The template is an expression that is pre-processed to create a file specification as the target for the ZSHOW. The preprocessing is equivalent to $ZPARSE(), as illustrated by the following M code:

.. code-block:: none

   set deffn="YDB_JOBEXAM.ZSHOW_DMP\_"_$JOB\_"_"_<cntr>
   set filespec=$zparse(expr1,"",deffn)

The $ZJOBEXAM() does not trigger error processing except when there is a problem storing its return value, so no error is reported to the process until after any dump is complete. In the event of any error encountered during the $ZJOBEXAM(), YottaDB sends an appropriate message to operator log facility and returns control to the caller. Note that this special error handling applies only to the $ZJOBEXAM(), and is not a property of the $ZINTERRUPT interrupt handler, which uses $ZJOBEXAM() by default.

Defaulting to :code:`"*"`, expr2 specifies the :ref:`zshow-info-codes` of data to be included in the output. To specify expr2 while allowing expr1 to default, specify :code:`""` as the value of expr1. Invalid codes in expr2 are ignored, and can result in empty dump files.

.. note::
   As ZSHOW output files contain process-internal data that may include confidential information, e.g., in local variables, ensure that output files have access protection appropriate to their content.

++++++++++++++++++++++++
Examples of $ZJOBEXAM()
++++++++++++++++++++++++

Example:

.. code-block:: bash

   YDB>set x=$zjobexam()
   YDB>write x
   /home/ydbuser1/YDB_JOBEXAM.ZSHOW_DMP_28760_1
   YDB>set x=$zjobexam("isvonly.txt","I")
   YDB>write x
   /home/ydbuser1/isvonly.txt
   YDB>

Shows default file name and type of the files created containing the zshow dump information and the difference when the name and type are specified.

The second parameter was added to $ZJOBEXAM() effective release `r1.30 <https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.30>`_.

.. _zjustify-function:

-------------------
$ZJUSTIFY()
-------------------

Returns a formatted and fixed length byte sequence.

The format for the $ZJUSTIFY function is:

.. code-block:: none

   $ZJ[USTIFY](expr,intexpr1[,intexpr2])

* The expression specifies the sequence of octets formatted by $ZJUSTIFY().
* The first integer expression (second argument) specifies the minimum size of the resulting byte sequence.
* If the first integer expression is larger than the length of the expression, $ZJUSTIFY() right-justifies the expression to a byte sequence of the specified length by adding leading spaces. Otherwise, $ZJUSTIFY() returns the expression unmodified unless specified by the second integer argument.
* The behavior of the optional second expression (third argument) for $ZJUSTIFY() is the same at $JUSTIFY(). For more information, refer to “$Justify()”.
* When the second argument is specified and the first argument evaluates to a fraction between -1 and 1, $ZJUSTIFY() returns a number with a leading zero (0) before the decimal point (.).
* $ZJUSTIFY() fills a sequence of octets to create a fixed length byte sequence. However, if the length of the specified expression exceeds the specified byte size, $ZJUSTIFY() does not truncate the result (although it may still round based on the third argument). When required, $ZEXTRACT() performs truncation.

$ZJUSTIFY() optionally rounds the portion of the result after the decimal point. In the absence of the third argument, $ZJUSTIFY() does not restrict the evaluation of the expression. In the presence of the third (rounding) argument, $JUSTIFY() evaluates the expression as a numeric value. The rounding algorithm can be understood as follows:

* If necessary, the rounding algorithm extends the expression to the right with 0s (zeros) to have at least one more digit than specified by the rounding argument.
* Then, it adds 5 (five) to the digit position after the digit specified by the rounding argument.
* Finally, it truncates the result to the specified number of digits. The algorithm rounds up when excess digits specify a half or more of the last retained digit and rounds down when they specify less than a half.

++++++++++++++++++++++++
Examples of $ZJUSTIFY()
++++++++++++++++++++++++

Example:

.. code-block:: bash

   YDB>write "123456789012345",! write $zjustify("新年好",15),!,$zjustify("新年好",5)
   123456789012345
        新年好
   新年好
   YDB>

This example uses $ZJUSTIFY() to display the sequence of octets represented by "新年好" in fields of 15 space octets and 5 space octets. Because the byte length of "新年好" is 9, it exceeds 5 spaces, the result overflows the specification.

.. _zlength-function:

-------------------------
$ZLENGTH()
-------------------------

Returns the length of a sequence of octets measured in bytes, or in "pieces" separated by a delimiter specified by one of its arguments.

The format for the $ZLENGTH function is:

.. code-block:: none

   $ZL[ENGTH](expr1[,expr2])

* The first expression specifies the sequence of octets that $ZLENGTH() "measures".
* The optional second expression specifies the delimiter that defines the measure; if this argument is missing, $ZLENGTH() returns the number of bytes in the sequence of octets.
* If the second argument is present and not null, $ZLENGTH() returns one more than the count of the number of occurrences of the second byte sequence in the first byte sequence; if the second argument is null, the M standard for the analogous $LENGTH() dictates that $ZLENGTH() returns a zero (0).
* $ZLENGTH() provides a tool for determining the lengths of a sequence of octets in two ways - characters and delimited substrings(pieces). The two argument $ZLENGTH() returns the number of existing pieces, while the one argument returns the number of characters.

+++++++++++++++++++++++
Examples of $ZLENGTH()
+++++++++++++++++++++++

Example:

.. code-block:: bash

   YDB>write $zlength("主要雨在西班牙停留在平原")
   36
   YDB>

This uses $ZLENGTH() to WRITE the length in bytes of the sequence of octets "主要雨在西班牙停留在平原".

Example:

.. code-block:: bash

   YDB>set x="主"_$zchar(63)_"要"_$zchar(63)_"雨"
   YDB>write $zlength(x,$zchar(63))
   3
   YDB>

This uses $ZLENGTH() to WRITE the number of pieces in a sequence of octets, as delimited by the byte code $ZCHAR(63).

Example:

.. code-block:: bash

   YDB>set x=$zchar(63)_"主"_$zchar(63)_"要"_$zchar(63)_"雨"_$zchar(63)"
   YDB>write $zlength(x,$zchar(63))
   5
   YDB>

This also uses $ZLENGTH() to WRITE the number of pieces in a sequence of octets, as delimited by byte code $ZCHAR(63). Notice that YottaDB counts both the empty beginning and ending pieces in the string because they are both delimited.

.. _zmessage-function:

-------------------
$ZMESSAGE()
-------------------

Returns a message string associated with a specified status code .

The format for the $ZMESSAGE function is:

.. code-block:: none

   $ZM[ESSAGE](intexpr)

The integer expression specifies the status code for which $ZMESSAGE() returns error message text .

$ZMESSAGE() provides a tool for examining the message and/or mnemonic associated with a particular message code as reported in $ZSTATUS.

The :ref:`zstatus-isv` Intrinsic Special Variable holds the message code and the message of the last non-Direct Mode YottaDB error.

++++++++++++++++++++++++
Examples of $ZMESSAGE()
++++++++++++++++++++++++

Example:

.. code-block:: bash

   YDB>write $zmessage(150373210)
   %YDB-E-DIVZERO, Attempt to divide by zero
   YDB>

This uses $ZMESSAGE() to display the message string corresponding to code 150373210.

.. _zparse-function:

-------------------
$ZPARSE()
-------------------

Expands a file name to a full pathname and then returns the full pathname or one of its fields (directory, name, or extension).

The format for the $ZPARSE function is:

.. code-block:: none

   $ZPARSE(expr1[,expr2[,expr3[,expr4[,expr5]]]])

* The first expression specifies the file name; if the file name is not valid, $ZPARSE() returns a null string; if the file name contains a wildcard (* and/or ?), $ZPARSE() returns a file name containing the wildcard(s).
* The optional second expression specifies the field of the pathname that $ZPARSE() returns; if this argument is missing or null, $ZPARSE() returns a full pathname constructed using default values in place of any fields missing for directory, file and extension.
* The optional third and fourth expressions specify default values to use during file name expansion for missing fields (directory, name, or extension), if any, in the original file name. For any field missing in the original file name specified in expr1, $ZPARSE() will attempt to substitute the corresponding field from expr3; if that field is not present in expr3, $ZPARSE() will attempt to use the corresponding field from expr4.
* If the file extension is missing from all three of expr1, expr3, and expr4, $ZPARSE() will return a null string for the corresponding field. If the file or directory is missing from all three of expr1, expr3, and expr4, $ZPARSE() will substitute the information from your current working directory.
* The optional fifth expression specifies the mode or type of parse that $ZPARSE() performs.

$ZPARSE() provides a tool for verifying that a file name is syntactically correct, for examining specific fields of a file name, and for filling in missing pieces in a partial specification based on a hierarchy of defaults. For information about determining whether a file exists, see :ref:`zsearch-function`.

$ZPARSE() arguments, after the first, are optional. If you use no other arguments, a single argument is sufficient. However, if you use selected arguments $ZPARSE() requires that null strings ("") be filled in for the unspecified arguments.

The acceptable keywords for the second argument are:

"DIRECTORY": Directory name

"NAME": File name (excluding file extension)

"TYPE": File type extension

The keywords may be entered in either upper or lower case. Variables that evaluate to these strings and indirection are acceptable for argument two. When the keywords themselves appear as string literals, they must be enclosed in quotation marks (" ").

The following guidelines must be followed in constructing arguments one, three and four:

* Directory specifications must end in a slash; anything after the final slash in the directory specification is assumed to be part of the name specification.
* A file name with an extension must include at least one character to the left of the period (.). Thus, "/user/.login" refers to the file named ".login", while "/usr/taxes.c" refers to a file named "taxes" with the extension "c". If a file name includes more than one period, the extension includes all letters to the right of the rightmost period.

The keywords for the fifth argument $ZPARSE() are:

NULL (""): Returns a full file-specification or device

"SYNTAX_ONLY": Disables checking for the existence of the directory or device.

"SYMLINK": Returns the file name of the actual file rather than the symbolic link, with $ZPARSE() following the link or chain of links.

The SYMLINK keyword was added to YottaDB effective release `r1.32 <https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.32>`_.

++++++++++++++++++++++++++++++
Examples of $ZPARSE()
++++++++++++++++++++++++++++++

Example:

.. code-block:: bash

   YDB>write $zparse("test","","/usr/work/","dust.lis")
   /usr/work/test.lis
   YDB>

This uses $ZPARSE() to demonstrate defaulting using the third and fourth arguments. The result gets the directory field from the third expression, the name from the first expression, and the type from the fourth expression.

Example:

.. code-block:: bash

   YDB>r!,"file :",f w ?20,$zparse(f,"directory")
   file: test.list /usr/work/
   YDB>

This uses $ZPARSE() to display the directory for the file name entered as input at the prompt file: , in this case, the current working directory.

Example:

.. code-block:: bash

   $ cd /usr/work/me
   $ ydb
   YDB>write $zparse("test","","x.list","y.c")/usr/work/me/test.lis
   YDB>write $zparse("test","","/usr/work/","/dev/y.c")/usr/work/test.c
   YDB>write $zparse("test","","/usr/work","/dev/y.c")/usr/test.c
   YDB>

This example illustrates the use of the third and fourth arguments to $ZPARSE(). In the first statement, the first argument has no directory or extension field, so $ZPARSE() substitutes the extension field from the third argument. Since neither the third nor fourth argument specifies a directory, and because the fourth argument does not contain any fields that are not present in the third argument, the fourth argument is not used.

In the second statement, the first argument to $ZPARSE() is again missing both the directory and extension. In this instance, $ZPARSE() uses the directory specified in the third argument and, because neither the first nor third argument specifies a file extension, $ZPARSE() uses the file extension from the fourth argument.

In the third statement, because "/usr/work" does not end with a backward slash (/), $ZPARSE() interprets the substring "work" as a file name. Then, $ZPARSE() substitutes "/usr/" for the directory missing in the first argument and substitutes ".c" from the fourth argument for the extension missing from both the first and third arguments.

Example:

.. code-block:: bash

   $ cd /usr/work/me
   $ /usr/local/lib/yottadb/r120/ydb
   YDB>For i="DIRECTORY","NAME","TYPE","" Write $ZPARSE("test.m",i),!
   /usr/work/me/
   test
   .m
   /usr/work/me/test.m
   YDB>


This example illustrates the output produced for each of the possible values for the second argument.

.. _zpeek-function:

------------------
$ZPEEK()
------------------

Provides a way to examine memory in the current process address space. Use of this function requires information about YottaDB internals, which may change from release to release. Contact YottaDB support for information on techniques for using $ZPEEK() in largely release independent ways.

The $ZPEEK() function returns the contents of the memory requested as a string depending on the requested (or defaulted) formatting.

The format of the $ZPEEK() function is:

.. code-block:: none

   $ZPEEK("mnemonic[:argument]",offset,length[,format])

mnemonic specifies the memory area $ZPEEK() is to access. Some mnemonics have arguments separated from the mnemonic by a colon (":"). The mnemonics are case independent. Possible mnemonics, their possible abbreviations and their arguments are:

* CSA[REG]:region - returns a value from the sgmnt_addrs (process private) control block. Takes a case independent region name as an argument.
* FH[REG]:region - returns a value from the sgmnt_data (shared file header) control block. Takes a case independent region name as an argument.
* GDR[REG]:region - returns a value from the gd_region (process private) control block. Takes a case independent region name as an argument.
* GDS[SEG]:region - returns a value from the gd_segment (process private) control block. Takes a case independent region name as an argument.
* GLF[REPL]:n - returns a value from the jnlpool.gtmsrc_lcl_array[n] control block. Takes a numeric index (n) as an argument.
* GRL[REPL] - returns a value from the recvpool.gtmrecv_local control block. No argument allowed. Only available when run on a non-primary instance.
* GSL[REPL]:n - returns a value from the jnlpool.gtmsource_local_array[n] control block. Takes a numeric index (n) as an argument.
* JBF[REG]:region - obtains fields in shared jnl_buffer structure. Takes a case independent region name as an argument.
* JNL[REG]:region - obtains fields in the jnl_private_control structure. Takes a case independent region name as an argument.
* JPC[REPL] - returns a value from the jnlpool.jnlpool_ctl control block. No argument allowed.
* NL[REG]:region - returns a value from the node_local (shared) control block. Takes a case independent region name as an argument.
* NLREPL - returns a value from the node_local (shared) control block associated with replication. No argument allowed.
* PEEK:baseaddr - returns a value based on the supplied argument. Argument is the base address of the value to obtain in 0xhhhhhhh format where the h's are hex digits.
* RIH[REPL] - returns a value from the jnlpool.repl_inst_filehdr control block. No argument allowed.
* RPC[REPL] - returns a value from the recvpool.recvpool_ctl control block. No argument allowed. Only available when run on a non-primary instance.
* UHC[REPL] - returns a value from the recvpool.upd_helper_ctl control block. No argument allowed. Only available when run on a non-primary instance.
* UPL[REPL] - returns a value from the recvpool.upd_proc_local control block. No argument allowed. Only available when run on a non-primary instance.

offset (first integer expression) is a numeric value that specifies the offset from the address supplied or implied by the the mnemonic and argument. Specifying a negative offset results in a BADZPEEKARG error. Specifying too large an offset such that unavailable memory is specified results in a BADZPEEKRANGE error.

length (second integer expression) is a numeric value that specifies the length of the field to obtain. Specifying a negative length results in a BADZPEEKARG error. Specifying a length that exceeds the maximum string length results in a MAXSTRLEN error. Specifying too large a length such that unavailable memory is specified results in a BADZPEEKRANGE error.

format is an optional single case independent character formatting code for the retrieved data. The formatting codes are:

* C : returns a character representations of the memory locations; this is the DEFAULT if the fourth argument is not specified.
* I : returns a signed integer value - negative values have a preceding minus sign (-); the length can be 1, 2, 4, or 8 bytes.
* U : returns an unsigned integer value - all bits are part of the numeric value; the length can be 1, 2, 4, or 8 bytes.
* S : returns a character representation of the memory locations and the first NULL character found terminates the returned string; that is: the specified length is a maximum.
* T: Selects a $HOROLOG format for a field of 4 or 8 bytes which is intended for use on fields in UNIX time format (seconds since 01/01/1970)
* X : returns a hexadecimal value as 0xXXXXXX where XXXXXX is twice the specified length in bytes, so requested length 1 returns 0xXX and length 4 returns 0xXXXXXXXX; the length can be 1, 2, 4, or 8 bytes.
* Z : returns a hexadecimal representation of the memory locations as 'X' does, without regard to endianness, and with no length restriction other than max string length.
* $ZPEEK() function generates an UNDEF error when VIEW UNDEF is not set and a format parameter is specified but is undefined.

$ZPEEK() has no UTF-8 checking. It is possible for values returned by the 'C' and 'S' codes to have invalid UTF-8 values in them. Take care when processing values obtained by these codes to either use "VIEW NOBADCHAR" when dealing with such values and/or use the $Zxxx() flavors of functions like $ZPIECE(), $ZEXTRACT(),etc which also do not raise BADCHAR errors when encountering invalid UTF-8 encoded strings.

Note that $ZPEEK() with 8 byte numeric formatting can return numeric string values that exceed YottaDB's current limit of 18 digits of precision. If the values are used as strings, the extra digits are preserved, but if used arithmetically, the lower precision digits can be lost.

When values from replication structures are requested and the structures are not available due to replication not running or, in the case of the gtmrecv.* control block base options, if not running on a non-primary instance where the gtmrecv.* control are available, a ZPEEKNOREPLINFO error is raised.

The JNL[REG] and JBL[REG] mnemonics and characteristics are defined by the running the GTMDefinedTypesInit.m utility, which produces a cross-index in the form:

.. code-block:: none

   gtmtypfldindx(<structure-name>.<field-mnemonic>)=<n>

where gtmtypes(<structure-name>,<n>,*) nodes contain the field characteristics.

.. _zpiece-function:

--------------------------
$ZPIECE()
--------------------------

Return a sequence of bytes delimited by a specified byte sequence made up of one or more bytes.

The format for the $ZPIECE function is:

.. code-block:: none

   $ZPI[ECE](expr1,expr2[,intexpr1[,intexpr2]])

* The first expression specifies the sequence of octets from which $ZPIECE() takes its result.
* The second expression specifies the delimiting byte sequence that determines the piece "boundaries"; if this argument is a null string, $ZPIECE() returns a null string.
* If the second expression does not appear anywhere in the first expression, $ZPIECE() returns the entire first expression (unless forced to return null by the second integer expression).
* The optional first integer expression (third argument) specifies the beginning piece to return; if this argument is missing, $ZPIECE() returns the first piece.
* The optional second integer expression (fourth argument) specifies the last piece to return. If this argument is missing, $ZPIECE() returns only one piece unless the first integer expression is zero (0) or negative, in which case it returns a null string. If this argument is less than the first integer expression, $ZPIECE() returns null.
* If the second integer expression exceeds the actual number of pieces in the first expression, $ZPIECE() returns all of the expression after the delimiter selected by the first integer expression.
* The $ZPIECE() result never includes the "outside" delimiters; however, when the second integer argument specifies multiple pieces, the result contains the "inside" occurrences of the delimiter.
* $ZPIECE() provides a tool for efficiently using values that contain multiple elements or fields, each of which may be variable in length.
* Applications typically use a single byte for a $ZPIECE() delimiter (second argument) to minimize storage overhead, and increase efficiency at run-time. The delimiter must be chosen so the data values never contain the delimiter. Failure to enforce this convention with edit checks may result in unanticipated changes in the position of pieces within the data value. The caret symbol (^), backward slash (\\), and asterisk (*) characters are examples of popular visible delimiters. Multiple byte delimiters may reduce the likelihood of conflict with field contents. However, they decrease storage efficiency, and are processed with less efficiency than single byte delimiters. Some applications use control characters, which reduce the chances of the delimiter appearing in the data but sacrifice the readability provided by visible delimiters.
* A SET command argument can have something that has the format of a $ZPIECE() on the left-hand side of its equal sign (=). This construct permits easy maintenance of individual pieces within a sequence of octets. It also can be used to generate a byte sequence of delimiters. For more information on SET $ZPIECE(), refer to :ref:`set-command`.

+++++++++++++++++++++
Examples of $ZPIECE()
+++++++++++++++++++++

Example:

.. code-block:: bash

   YDB>for i=0:1:3 write !,$zpiece("主"_$zchar(64)_"要",$zchar(64),i),"|"
   |
   主|
   要|
   |
   YDB>

This loop displays the result of $ZPIECE(), specifying $ZCHAR(64) as a delimiter, a piece position "before," first and second, and "after" the sequence of octets.

Example:

.. code-block:: bash

   YDB>for i=-1:1:3 write !,$zpiece("主"_$zchar(64)_"要",$zchar(64),i,i+1),"|"
   |
   主|
   主@要|
   要|
   |
   YDB>

This example is similar to the previous example except that it displays two pieces on each iteration. Notice the delimiter () in the middle of the output for the third iteration, which displays both pieces.

Example:

.. code-block:: none

   For p=1:1:$ZLength(x,"/") Write ?p-1*10,$ZPIece(x,"/",p)

This loop uses $ZLENGTH() and $ZPIECE() to display all the pieces of x in columnar format.

Example:

.. code-block:: bash

   YDB>Set $piece(x,$zchar(64),25)="" write x
   @@@@@@@@@@@@@@@@@@@@@@@@

This SETs the 25th piece of the variable x to null, with delimiter $ZCHAR(64). This produces a byte sequence of 24 at-signs (@) preceding the null.

----------------------
$ZPREVIOUS()
----------------------

The $ZPREVIOUS function returns the subscript of the previous local or global variable name in collation sequence within the array level specified by its argument. When $ZPREVIOUS() has an unsubscripted argument, it returns the previous unsubscripted local or global variable name in collating sequence.

The $ZPREVIOUS function provides compatibility with some other M implementations. The M Development Committee chose to implement this functionality with the optional second -1 argument of $ORDER(). Therefore, when a design requires this functionality $ORDER() has the advantage over $ZPREVIOUS of being part of the M standard.

The format for the $ZPREVIOUS function is:

.. code-block:: none

   $ZP[REVIOUS](glvn)

* The subscripted or unsubscripted global or local variable name specifies the node prior to which $ZPREVIOUS() searches backwards for a defined node with data and/or descendants. The number of subscripts contained in the argument implicitly defines the array level.
* If $ZPREVIOUS() finds no node at the specified level before the specified global or local variable, it returns a null string.
* If the last subscript in the subscripted global or local variable name is null, $ZPREVIOUS() returns the last node at the specified level.

$ZPREVIOUS() is equivalent to $ORDER() with a second argument of -1.

.. _zqgblmod-function:

---------------------
$ZQGBLMOD()
---------------------

The $ZQGBLMOD function enables an application to determine whether it can safely apply a lost transaction to the database. A lost transaction is a transaction that must be rolled off a database to maintain logical multisite consistency. $ZQGBLMOD() always applies to data-level (level-0) nodes.

The format for the $ZQGBLMOD function is:

.. code-block:: none

   $ZQGBLMOD(gvn)

* The subscripted or non-subscripted global variable name (gvn) specifies the target node.
* A return value of zero (0) means the value of the global variable has not changed since the last synchronization of the originating and replicating instances.
* A return value of one (1) means the value of the global variable may have changed since the last synchronization of the originating and replicating instance.

$ZQGBLMOD function produces an error if you submit an argument that is not a global variable name.

Internally, $ZQGBLMOD (gvn) compares the YottaDB transaction number in the database block in which the global variable name is (or would be) stored with the value in the Zqgblmod_Trans field stored in the database file header.

For example, if x is the transaction number of the level-0 database block in which gvn resides, and y is the value of Zqgblmod_Trans of region reg containing gvn, then the following is true:

* If x <= y, no transaction modified the level-0 database block z in which gvn resides since the originating and replicating instances synchronized with each other. $ZQGBLMOD() returns a zero (0).
* If x > y, some transaction modified z, but not necessarily gvn, after the originating and replicating instances synchronized with each other. $ZQGBLMOD() returns a one (1).

If a transaction is a lost transaction that has been rolled back and it is determined that for all the M globals set and killed in the transaction $ZQGBLMOD() is zero (0), it is probably safe to apply the updates automatically. However, this determination of safety can only be made by the application designer and not by YottaDB. If the $ZQGBLMOD() is one (1) for any set or kill in the transaction, it is not safe to apply the update.

.. note::
   The test of $ZQGBLMOD() and applying the updates must be encapsulated inside a YottaDB transaction.

Another approach to handling lost transactions would be to store in the database the initial message sent by a client, as well as the outcome and the response, and to reprocess the message with normal business logic. If the outcome is the same, the transaction can be safely applied.

.. note::
   If restartable batch operations are implemented, lost batch transactions can be ignored since a subsequent batch restart will process them correctly.

.. _zsearch-function:

--------------------
$ZSEARCH()
--------------------

The $ZSEARCH function attempts to locate a file matching the specified file name. If the file exists, it returns the file name; if the file does not exist, it returns the null string.

The format for the $ZSEARCH function is:

.. code-block:: none

   $ZSEARCH(expr[,intexpr])

* The first parameter :code:`expr` contains a file name, with or without wildcards, for which $ZSEARCH() attempts to locate a matching file. Repeating $ZSEARCH with the same filename uses the same context and return a sequence of matching files when they exist; when the sequence is exhausted, $ZSEARCH() returns an empty string (""). Any change to the file name starts a new context.
* $ZSEARCH() uses the process current working directory, if :code:`expr` does not specify a directory.
* The optional :code:`intexpr` specifies an integer expression that is a :code:`stream` number. It can be any value from :code:`0` to :code:`255` for each search; streams provide a means of having up to 256 $ZSEARCH() contexts simultaneously in progress. If no :code:`intexpr` is specified, a default value of 0 is assumed.
* If a $ZSEARCH() stream has never been used or if the expression differs from the argument to the last $ZSEARCH() of the stream, the function resets the context and returns the first pathname matching the expression; otherwise, it returns the next matching file in collating sequence; if the last prior pathname returned for the same expression and same stream was the last one matching the argument, $ZSEARCH() returns a null string.
* The special :code:`stream` number of :code:`-1` always resets the context and returns the first pathname matching the expression. That is, it starts a fresh sequence of matching files even if the same file name is used in repeated calls.

$ZSEARCH() provides a tool for verifying that a file exists. For information to help determine the validity of a file name, see :ref:`zparse-function`.

.. note::
   You can call the POSIX stat() function to access metadata. The optional YottaDB POSIX plug-in packages the stat() function for easy access from M application code.

+++++++++++++++++++++++
Examples of $ZSEARCH()
+++++++++++++++++++++++

Example:

.. code-block:: bash

   YDB>write $zsearch("data.dat")
   /usr/staff/ccc/data.dat
   YDB>

This uses $ZSEARCH() to display the full file path name of "data.dat" in the process current default directory.

Example:

.. code-block:: bash

   YDB>set x=$zsearch("*.c")
   YDB>for  set x=$zsearch("*.m") quit:x=""  write !,$zparse(x,"NAME")

This FOR loop uses $ZSEARCH() and $ZPARSE() to display M source file names in the process current working directory. To ensure that the search starts at the beginning, the example resets the context by first searching with a different argument.

.. _zsigproc-function:

-------------------
$ZSIGPROC()
-------------------

Sends a signal to a process. The format for the $ZSIGPROC function is:

.. code-block:: none

   $ZSIGPROC(expr1,expr2)

* The first expression is the pid of the process to which the signal is to be sent.
* The second expression is the system signal name (e.g., :code:`"SIGUSR1"` or just :code:`"USR1"` - YottaDB accepts either) or number (e.g., 10 for SIGUSR1). YottaDB recommends using signal names to maintain code portability across different platforms.

If the second expression is 0, $ZSIGPROC() checks the validity of the pid specified in the first expression.

There are four possible return values from $ZSIGPROC():

+------------------------------------------------+---------------------------------------------------------------------------------------------------------------------------+
| Return Codes/ POSIX Error Definitions          | Description                                                                                                               |
+================================================+===========================================================================================================================+
| 0                                              | The specified signal number was successfully sent to the specified pid. Any return value other than 0 indicates an error. |
+------------------------------------------------+---------------------------------------------------------------------------------------------------------------------------+
| EPERM                                          | The process has insufficient permissions to send the signal to the specified pid.                                         |
+------------------------------------------------+---------------------------------------------------------------------------------------------------------------------------+
| ESRCH                                          | The specified pid does not exist.                                                                                         |
+------------------------------------------------+---------------------------------------------------------------------------------------------------------------------------+
| EINVAL                                         | Invalid expression(s).                                                                                                    |
+------------------------------------------------+---------------------------------------------------------------------------------------------------------------------------+

.. note::
   Although $ZSIGPROC() works today as a way to invoke the asynchronous interrupt mechanism of YottaDB processes to XECUTE $ZINTERRUPT because the underlying mechanism uses the USR1 signal, YottaDB reserves the right to change the underlying mechanism to suit its convenience and sending a SIGUSR1 may cease to work as a way to invoke the asynchronous interrupt mechanism. Use MUPIP INTRPT as the supported and stable API to invoke the asynchronous interrupt mechanism.

In release `r1.30 <https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.30>`_ $ZSIGPROC() was enhanced to allow signals to be specified by name.

.. note::

   $ZSIGPROC() is implemented using `kill(2) <https://man7.org/linux/man-pages/man2/kill.2.html>`_. If the pid of the process is zero or negative, $ZSIGPROC() may behave unexpectedly.

++++++++++++++++++++++++++
Examples of $ZSIGPROC()
++++++++++++++++++++++++++

Example:

.. code-block:: bash

   YDB>job ^Somejob
   YDB>set ret=$&ydbposix.signalval("SIGUSR1",.sigusr1) zwrite
       ret=0
       sigusr1=10
   YDB>write $zsigproc($zjob,sigusr1)
       0
   YDB>

This example sends the SIGUSR1 signal to the pid specified by $zjob.

.. _zsocket-function:

------------------------
$ZSOCKET()
------------------------

Returns information about a SOCKET device and its attached sockets. The format of the $ZSOCKET() function is:

.. code-block:: none

   $ZSOCKET(expr1,expr2[,[expr3][,expr4]])

The first expression specifies the SOCKET device name; an empty string returns the same result as the current device ($IO). If the first expression is not specified, $ZSOCKET() returns information about sockets in the socketpool. Specifying a device other than a SOCKET device for the $ZSOCKET() function produces a ZSOCKETNOTSOCK error. When a YottaDB process starts with different sockets for input and output on $PRINCIPAL, $ZSOCKET() accepts $ZPIN or $ZPOUT as its first argument and supplies information on the input or output side, respectively. The following is an example of getting the handles for the $PRINCIPAL input and output socket devices.

.. code-block:: none

   set handlein=$ZSOCKET($ZPIN,"SOCKETHANDLE",0)
   set handleout=$ZSOCKET($ZPOUT,"SOCKETHANDLE",0)

For more information, refer to :ref:`zpin-isv` and :ref:`zpout-isv`.

The second expression specifies a keyword identifying the type of information returned and the optional third expression usually specifies the index (starting at zero) of a socket attached to the device; if the index is outside the range of attached sockets, $ZSOCKET() returns an empty string. If the third expression is not specified, $ZSOCKET() returns information about the current socket. Using an invalid keyword produces a ZSOCKETATTR error. The fourth expression specifies an individual delimiter when the second expression specifies DELIMITER. For more information, see the following table. Note that changes to the socket collection for a SOCKET device using OPEN, CLOSE, USE :ATTACH, or USE :DETACH may change the index for a socket.

+----------------------------+----------------------------------+-----------------------------------------------------------------------------------------------------------------------------------+
| Keyword                    | Arguments                        | Returns                                                                                                                           |
+============================+==================================+===================================================================================================================================+
| CURRENTINDEX               |                                  | The index (starting at zero) of the current socket for the SOCKET device.                                                         |
+----------------------------+----------------------------------+-----------------------------------------------------------------------------------------------------------------------------------+
| DELIMITER                  | index[, delimiter]               | If only index is specified, the number of delimiters. If delimiter is also specified, selects which delimiter to return. The first|
|                            |                                  | delimiter is zero.                                                                                                                |
+----------------------------+----------------------------------+-----------------------------------------------------------------------------------------------------------------------------------+
| DESCRIPTOR                 | index                            | The OS socket descriptor for the socket.                                                                                          |
+----------------------------+----------------------------------+-----------------------------------------------------------------------------------------------------------------------------------+
| HOWCREATED                 | index                            | LISTEN, CONNECT, ACCEPTED, PRINCIPAL, or PASSED                                                                                   |
|                            |                                  |                                                                                                                                   |
|                            |                                  | ACCEPTED indicates a connection created from a LISTENing socket.                                                                  |
|                            |                                  |                                                                                                                                   |
|                            |                                  | PRINCIPAL indicates that the socket is the $PRINCIPAL of the process.                                                             |
|                            |                                  |                                                                                                                                   |
|                            |                                  | PASSED indicates a socket passed by WRITE /ACCEPT.                                                                                |
+----------------------------+----------------------------------+-----------------------------------------------------------------------------------------------------------------------------------+
| INDEX                      | handle                           | The current index of the socket named by handle.                                                                                  |
+----------------------------+----------------------------------+-----------------------------------------------------------------------------------------------------------------------------------+
| IOERROR                    | handle                           | 1 (TRUE) if IOERROR=TRAP otherwise 0 (FALSE).                                                                                     |
+----------------------------+----------------------------------+-----------------------------------------------------------------------------------------------------------------------------------+
| LOCALADDRESS               | index                            | The address of the local side of the socket. For TCP sockets: the IPv6 or IPv4 numeric address. For LOCAL sockets: the path.      |
+----------------------------+----------------------------------+-----------------------------------------------------------------------------------------------------------------------------------+
| LOCALPORT                  | index                            | The numeric port of the local side of a TCP socket.                                                                               |
+----------------------------+----------------------------------+-----------------------------------------------------------------------------------------------------------------------------------+
| MOREREADTIME               | index                            | The value of the MOREREADTIME device parameter if it was specified, otherwise an empty string.                                    |
+----------------------------+----------------------------------+-----------------------------------------------------------------------------------------------------------------------------------+
| NUMBER                     |                                  | The number of sockets in the SOCKET device.                                                                                       |
+----------------------------+----------------------------------+-----------------------------------------------------------------------------------------------------------------------------------+
| PARENT                     | index                            | If the socket was created from a LISTENing socket: the handle of the LISTENing socket.                                            |
+----------------------------+----------------------------------+-----------------------------------------------------------------------------------------------------------------------------------+
| PROTOCOL                   | index                            | TCP, TCP6, or LOCAL                                                                                                               |
+----------------------------+----------------------------------+-----------------------------------------------------------------------------------------------------------------------------------+
| REMOTEADDRESS              | index                            | The address of the remote side of the socket. For TCP sockets: the IPv6 or IPv4 numeric address. For LOCAL sockets: the path.     |
+----------------------------+----------------------------------+-----------------------------------------------------------------------------------------------------------------------------------+
| REMOTEPORT                 | index                            | The numeric port of the remote side of a TCP socket.                                                                              |
+----------------------------+----------------------------------+-----------------------------------------------------------------------------------------------------------------------------------+
| SOCKETHANDLE               | index                            | The handle for the selected socket.                                                                                               |
+----------------------------+----------------------------------+-----------------------------------------------------------------------------------------------------------------------------------+
| STATE                      | index                            | One of LISTENING, CONNECTED, BOUND, or CONNECTINPROGRESS                                                                          |
+----------------------------+----------------------------------+-----------------------------------------------------------------------------------------------------------------------------------+
| TLS                        | index[,expr4]                    | If the selected socket is using TLS, a string of the form: 1,{SERVER|CLIENT}[,tlsid], where the optional tlsid comes from the     |
|                            |                                  | WRITE /TLS which enabled TLS on the socket; otherwise an empty string. See the following table for a description of all options   |
|                            |                                  | for the fourth expression for the TLS keyword.                                                                                    |
+----------------------------+----------------------------------+-----------------------------------------------------------------------------------------------------------------------------------+
| ZBFSIZE                    | index                            | Size of the YottaDB buffer in bytes.                                                                                              |
+----------------------------+----------------------------------+-----------------------------------------------------------------------------------------------------------------------------------+
| ZFF                        | index                            | The value of the ZFF device parameter.                                                                                            |
+----------------------------+----------------------------------+-----------------------------------------------------------------------------------------------------------------------------------+
| ZIBFSIZE                   | index                            | Size of the OS buffer in bytes (SO_RCVBUF).                                                                                       |
+----------------------------+----------------------------------+-----------------------------------------------------------------------------------------------------------------------------------+
| ZDELAY                     | index                            | 1 if Nagle algorithm enabled, otherwise 0.                                                                                        |
+----------------------------+----------------------------------+-----------------------------------------------------------------------------------------------------------------------------------+

The following table describes the values for the fourth expression for the TLS keyword.

+------------------+-----------------------------------------------------------------------------------------------------------------------------------------------+
| expr4 (TLS)      | Description                                                                                                                                   |
+==================+===============================================================================================================================================+
| SESSION          | Returns information related to SSL sessions including information about renegotiations. Here is an example:                                   |
|                  |                                                                                                                                               |
|                  | \|S:RENSEC:1,RENTOT:1,SESSID:<SESSID>, SESEXP:Thu Jun 4 21:07:11 2015                                                                         |
|                  |                                                                                                                                               |
|                  | "\|S:" denotes this piece contains session information, "RENSEC:" indicates whether secure renegotiation is available (1) or not (0),         |
|                  | "RENTOT:" gives the current total number of renegotiations done on this socket, "SESSID:" shows the session id in hexadecimal, and "SESEXP:"  |
|                  | indicates when the session expires respresented as time in the local time zone.                                                               |
+------------------+-----------------------------------------------------------------------------------------------------------------------------------------------+
| OPTIONS          | the hexadecimal representation of the ssl-options selected by the combination of the OpenSSL defaults, options set by the YottaDB TLS         |
|                  | plugin, and options specified in the gtmcrypt_config configuration file prefixed by "O:", a comma, and the verify mode as two hexadecimal     |
|                  | digits. Here is an example:                                                                                                                   |
|                  |                                                                                                                                               |
|                  | \|O:0000000001520004,01                                                                                                                       |
|                  |                                                                                                                                               |
|                  | The values for the SSL_OP options and verify modes are defined in the include/openssl/ssl.h file provided by the OpenSSL development package. |
|                  |                                                                                                                                               |
|                  | Note: the TLS reference implementation plug-in disables SSLv3 by default.                                                                     |
+------------------+-----------------------------------------------------------------------------------------------------------------------------------------------+
| CIPHER           | The SSL protocol version prefixed by "P:" and the algorithm negotiated between the server and client prefixed by "C:". Here is an example:    |
|                  |                                                                                                                                               |
|                  | \|P:TLSv1.2|C:DHE-RSA-AES256-SHA                                                                                                              |
+------------------+-----------------------------------------------------------------------------------------------------------------------------------------------+
| ALL              | returns all available information. Here is an example:                                                                                        |
|                  |                                                                                                                                               |
|                  | \|P:TLSv1.2|C:AES256-GCM-SHA384|O:0000000001020004,01|S:RENSEC:1,RENTOT:0,SESEXP:Mon Jun 22 23:58:09 2015                                     |
+------------------+-----------------------------------------------------------------------------------------------------------------------------------------------+

.. _zsubstr-function:

-------------------------
$ZSUBSTR()
-------------------------

Returns a properly encoded string from a sequence of bytes.

.. code-block:: none

   $ZSUB[STR] (expr ,intexpr1 [,intexpr2])

* The first expression is an expression of the byte string from which $ZSUBSTR() derives the character sequence.
* The second expression is the starting byte position (counting from 1 for the first position) in the first expression from where $ZSUBSTR() begins to derive the character sequence.
* The optional third expression specifies the number of bytes from the starting byte position specified by the second expression that contribute to the result. If the third expression is not specified, the $ZSUBSTR() function returns the sequence of characters starting from the byte position specified by the second expression up to the end of the byte string.
* The $ZSUBSTR() function never returns a string with illegal or invalid characters. With VIEW "NOBADCHAR", the $ZSUBSTR() function ignores all byte sequences within the specified range that do not correspond to valid UTF-8 code-points, With VIEW "BADCHAR", the $ZSUBSTR() function triggers a run-time error if the specified byte sequence contains a code-point value that is not in the character set.
* The $ZSUBSTR() is similar to the $ZEXTRACT() byte equivalent function but differs from that function in restricting its result to conform to the valid characters in the current encoding.


+++++++++++++++++++++++++++
Examples of $ZSUBSTR()
+++++++++++++++++++++++++++

Example:

.. code-block:: bash

   YDB>write $ZCHSET
   M
   YDB>set char1="a" ; one byte character
   YDB>set char2="ç"; two-byte character
   YDB>set char3="新"; three-byte character
   YDB>set y=char1_char2_char3
   YDB>write $zsubstr(y,1,3)=$zsubstr(y,1,5)
   0

With character set M specified, the expression $ZSUBSTR(y,1,3)=$ZSUBSTR(y,1,5) evaluates to 0 or "false" because the expression $ZSUBSTR(y,1,5) returns more characters than $ZSUBSTR(y,1,3).

Example:

.. code-block:: bash

   YDB>write $zchset
   UTF-8
   YDB>set char1="a" ; one byte character
   YDB>set char2="ç"; two-byte character
   YDB>set char3="新"; three-byte character
   YDB>set y=char1_char2_char3
   YDB>write $zsubstr(y,1,3)=$zsubstr(y,1,5)
   1

For a process started in UTF-8 mode, the expression $ZSUBSTR(y,1,3)=$ZSUBSTR(y,1,5) evaluates to 1 or "true" because the expression $ZSUBSTR(y,1,5) returns a string made up of char1 and char2 excluding the three-byte char3 because it was not completely included in the specified byte-length.

In many ways, the $ZSUBSTR() function is similar to the $ZEXTRACT() function. For example, $ZSUBSTR(expr,intexpr1) is equivalent to $ZEXTRACT(expr,intexpr1,$L(expr)). Note that this means when using the M character set, $ZSUBSTR() behaves identically to $EXTRACT() and $ZEXTRACT(). The differences are as follows:

* $ZSUBSTR() cannot appear on the left of the equal sign in the SET command where as $ZEXTRACT() can.
* In both the modes, the third expression of $ZSUBSTR() is a byte, rather than character, position within the first expression.
* $EXTRACT() operates on characters, irrespective of byte length.
* $ZEXTRACT() operates on bytes, irrespective of multi-byte character boundaries.
* $ZSUBSTR() is the only way to extract as valid UTF-8 encoded characters from a byte string containing mixed UTF-8 and non UTF-8 data. It operates on Unicode® characters so that its result does not exceed the given byte length.

----------------------
$ZSYSLOG()
----------------------

Sends its string parameter to the system log and always returns TRUE (1). Syslog entries logged by $ZSYSLOG() show :code:`"YDB-"` followed by the process executable :code:`proc/<pid>/comm` converted to upper case as the SYSLOG_IDENTIFIER field (along with instance information where appropriate). The $ZSYSLOG() function sends the argument to syslog facility. The format of the $ZSYSLOG() function is:

.. code-block:: none

   $ZSYSLOG(expr)

.. _ztranslate-function:

-------------------------
$ZTRANSLATE()
-------------------------

Returns a byte sequence that results from replacing or dropping bytes in the first of its arguments as specified by the patterns of its other arguments.

The format for the $ZTRANSLATE function is:

.. code-block:: none

   $ZTR[ANSLATE](expr1[,expr2[,expr3]])

* The first expression specifies the sequence of octets on which $ZTRANSLATE() operates. If the other arguments are omitted, $ZTRANSLATE() returns this expression.
* The optional second expression specifies the byte for $TRANSLATE() to replace. If a byte occurs more than once in the second expression, the first occurrence controls the translation, and $ZTRANSLATE() ignores subsequent occurrences. If this argument is omitted, $ZTRANSLATE() returns the first expression without modification.
* The optional third expression specifies the replacement bytes for the positionally corresponding bytes in the second expression. If this argument is omitted or shorter than the second expression, $ZTRANSLATE() drops all occurrences of the bytes in the second expression that have no replacement in the corresponding position of the third expression.
* A first call to $ZTRANSLATE where the first argument is defined but the second and third are undefined results in an error.
* $ZTRANSLATE() provides a tool for tasks such as encryption.

The $ZTRANSLATE() algorithm can be understood as follows:

* $ZTRANSLATE() evaluates each byte in the first expression, comparing it byte by byte to the second expression looking for a match. If there is no match in the second expression, the resulting expression contains the byte without modification.
* When it locates a byte match, $ZTRANSLATE() uses the position of the match in the second expression to identify the appropriate replacement for the original expression. If the second expression has more bytes than the third expression, $ZTRANSLATE() replaces the original byte with a null, thereby deleting it from the result. By extension of this principle, if the third expression is missing, $ZTRANSLATE() deletes all bytes from the first expression that occur in the second expression.

+++++++++++++++++++++++++++++
Examples of $ZTRANSLATE()
+++++++++++++++++++++++++++++

Example:

.. code-block:: bash

   YDB>set hiraganaA=$char(12354) ; $zchar(227,129,130)
   YDB>set temp1=$zchar(130)
   YDB>set temp2=$zchar(140)
   YDB>set tr=$ztranslate(hiraganaA,temp1,temp2)
   YDB>w $ascii(tr)
   12364
   YDB>

In the above example, $ZTRANSLATE() replaces byte $ZCHAR(130) in first expression and matching the first (and only) byte in the second expression with byte $ZCHAR(140) - the corresponding byte in the third expression.

.. _ztrigger-function:

------------------------
$ZTRIGGER()
------------------------

Examine or load trigger definition. The format of the $ZTRIGGER() function is:

.. code-block:: none

   $ZTRIgger(expr1[,expr2])

* $ZTRIGGER() returns a truth value (1 or 0) depending on the success of the specified action.
* $ZTRIGGER() performs trigger maintenance actions similar those performed by MUPIP TRIGGER.
* If expr1 evaluates to case-insensitive "FILE", $ZTRIGGER() evaluates expr2 as the location of the trigger definition file. Then, it applies the trigger definitions in the file specified by expr2. If a file contains a delete all (-\*), that action produces no user confirmation.
* If expr1 evaluates to case-insensitive "ITEM", $ZTRIGGER() evaluates expr2 as a single line or multi-line trigger definition entry. A multi-line trigger definition or a multi-line XECUTE string starts with << and uses $char(10) to denote the newline separator. expr2 with ITEM does not permit a multi-line XECUTE string to end with the >> terminator. It does not require trigger logic to appear immediately after the -xecute=<<, but a $char(10) must prefix the logic as shown in the following examples:

 .. code-block:: none

    set trigstr="+^a -xecute=<< -commands=S"_$char(10)_" do ^twork1"_$char(10)_" do ^twork2"_$char(10) write $ztrigger("item",trigstr)
    set trigstr="+^a -xecute=<< -commands=S "_$c(10)_" do ^twork1"_$c(10)_" do ^twork2"_$c(10) write $ztrigger("item",trigstr)

* If expr1 evaluates to case-insensitive "SELECT", $ZTRIGGER() evaluates the optional expr2 as a trigger name or name wildcard, and directs its output to $IO. A FALSE result (0) indicates there are no matching triggers.
* $ZTRIGGER() always operates within a TP transaction even if it needs to implicitly create one to encapsulate its work. Trigger maintenance operations reserve their output until the transaction commits (TCOMMIT where $TLEVEL goes to zero), at which time they deliver their entire output to the current $IO containing consistent information for all $ZTRIGGER() invocations within the successful processing of a larger transaction at that ultimate TCOMMIT. If an explicit transaction ends with a TROLLBACK, it does not produce any $ZTRIGGER() output.
* $ZTRIGGER() can appear within a transaction as long as it does not update any triggers for globals which have had triggers invoked earlier in the same transaction.
* An attempt by a $ZTRIGGER() within a transaction to remove or replace a trigger on a global after the transaction has activated any trigger defined within the named global generates a TRIGMODINTP error.
* $ZTRIGGER() treats the deletion of a non-existent trigger as a success; if that is the only operation, or one of a set of successful operations, it return success (TRUE/1) to the YottaDB process. $ZTRIGGER() returns failure in case of trigger selection using trigger names where the number after the pound-sign (#) starts with a 0 (which is an impossible auto-generated trigger name).
* YottaDB maps trigger definitions to the region to which they apply.
* YottaDB allows defining triggers with the same name and signature in different regions, but does not allow defining triggers with the same name, but different signatures, in different regions within the same global directory. When loading a trigger definition, if a user-defined name conflicts with a name in any region to which the trigger applies, $ZTRIGGER() reports an error. However, when the name is auto-generated, it generates a name in every region, so if there are multiple (spanning) regions, the same trigger might have a differing auto-generated name in each region.

.. note::
   A $ZTRIGGER() action (delete or select) applies to all triggers in all regions matching the specified signature. If the argument specifies an incomplete trigger signature, for example, only the name, the specification may match multiple triggers and apply the delete or select to all of them. YottaDB recommends you run a select and analyze the scope of the signature before any signature limited delete.

++++++++++++++++++++++++++
Examples of $ZTRIGGER()
++++++++++++++++++++++++++

Example:

.. code-block:: bash

   YDB>set X=$ztrigger("S")
   YDB>

This example displays the current trigger definitions stored in the database.

.. code-block:: bash

   YDB>set X=$ztrigger("i","+^Acct(sub=:) -command=set -xecute=""set ^X($ztvalue)=sub""")
   YDB>

This example adds a trigger definition for the first level node of ^Acct.

Example:

.. code-block:: bash

   YDB>set trigstr="+^a -commands=S -xecute=<<"_$c(10)_" do ^twork1"_$c(10)_" do ^twork2"_$c(10) write $ztrigger("item",trigstr)

This example demonstrates the usage of the $ztrigger("ITEM",<multi-line-trigger-definition>> where <<denotes the definition of a multi-line -XECUTE string and $c(10) to denote the newline separator. Unlike the $ztrigger("FILE") form, $ztrigger("ITEM",<multi-line-trigger-definition>> does not require the trigger definition to terminate with >>.

Example:

.. code-block:: bash

   YDB>write $ztrigger("file","agbl.trg")
   1
   YDB>

This example is equivalent to the previous $ztrigger("ITEM") example. In this example, agbl.trg contains the following multi-line trigger definition:

.. code-block:: none

   +^a -commands=S -xecute=<<
   do ^twork1
   do ^twork2
   >>

Unlike $ztrigger("ITEM"), $ztrigger("FILE") usages require the trigger definition to terminate with >>

----------------------
$ZTRNLNM()
----------------------

The $ZTRNLNM function returns the value of an environment variable.

.. note::
   $ZTRNLNM() does not perform iterative translation.

The format for the $ZTRNLNM function is:

.. code-block:: none

   $ZTRNLNM(expr1[,expr2[,expr3[,expr4[,expr5[,expr6]]]]])

expr1 specifies the environment variable whose value needs to be returned.

expr2 to expr5 are OpenVMS-related expressions that specify logical name table(s), index (numbered from 0), initial mode of the look-up, and a value indicating whether the look-up is case sensitive. To ensure interoperability between UNIX and OpenVMS versions, $ZTRNLNM() on UNIX accepts these expressions and ignores them.

The optional expr6 specifies any one of the following keywords:

+---------------------------------+-------------------------------------------------+
| Item Keyword                    | Data Returned                                   |
+=================================+=================================================+
| FULL                            | Return the translation                          |
+---------------------------------+-------------------------------------------------+
| LENGTH                          | Length of the return value in bytes.            |
+---------------------------------+-------------------------------------------------+
| VALUE                           | Returns the translation.                        |
+---------------------------------+-------------------------------------------------+

+++++++++++++++++++++++
Examples of $ZTRNLNM()
+++++++++++++++++++++++

Example:

.. code-block:: bash

   YDB>write $ztrnlnm("ydb_dist","","","","","VALUE")
   /usr/local/lib/yottadb/r120/utf8
   YDB>write $ztrnlnm("ydb_dist")
   /usr/local/lib/yottadb/r120
   YDB>

This uses $ZTRNLNM() to display the translation value for ydb_dist.

.. _zwidth-function:

-----------------------
$ZWIDTH()
-----------------------

Returns the numbers of columns required to display a given string on the screen or printer. The format of the $ZWIDTH() function is:

.. code-block:: none

   $ZW[IDTH] (expr)

* The expression is the string which $ZWIDTH() evaluates for display length. If the expression contains a code-point value that is not a valid UTF-8 character, $ZWIDTH() generates a run-time error.
* If the expression contains any non-graphic characters, the $ZWIDTH() function does not count those characters.
* If the string contains any escape sequences containing graphical characters (which they typically do), $ZWIDTH() includes those characters in calculating its result, as it does not do escape processing. In such a case, the result may be larger than the actual display width.

.. note::
   When in "NOBADCHAR" mode, $ZWIDTH() returns give any bad characters a length of zero (0), which may or may not match the behavior of any device used to display the string.

With character set UTF-8 specified, the $ZWIDTH() function uses the ICU's glyph-related conventions to calculate the number of columns required to represent the expression.

+++++++++++++++++++++++++++
Examples of $ZWIDTH()
+++++++++++++++++++++++++++

Example:

.. code-block:: bash

   YDB>set NG=$char($$FUNC^%HD("200B"))
   YDB>set S=$char(26032)_NG_$CHAR(26033)
   YDB>W $ZWidth(STR)
   4
   YDB>

In the above example, the local variable NG contains a non-graphic character which does not display between two double-width UTF-8 characters.

Example:

.. code-block:: bash

   YDB>write $zwidth("The rain in Spain stays mainly in the plain.")
   44
   YDB>set A="主要雨在西班牙停留在平原"
   YDB>write $length(A)
   12
   YDB>write $zwidth(A)
   24

In the above example, the $ZWIDTH() function returns 24 because each character in A occupies 2 columns when they are displayed on the screen or printer.

-------------------
$ZWRITE()
-------------------

Converts its first string argument to or from ZWRITE format (quoted graphics characters concatenated with $CHAR() representations of any non-graphic characters). The second integer expression controls the direction of conversion. The format of the $ZWRITE() function is:

.. code-block:: none

   $ZWRITE(expr[,intexpr])

* The first argument specifies the string to convert to or from the ZWRITE format.
* The second argument specifies the direction of conversion. When intexpr is not specified or evaluates to zero, $zwrite() converts the first argument to the ZWRITE format. When intexpr evaluates to a non-zero value, $ZWRITE() treats the first argument as being in ZWRITE format and attempts to convert it to a string with embedded non-graphic characters; if it is not in ZWRITE format, it returns an empty string.
* Converting to zwrite format tends to produce a string that is longer than the input and therefore a $ZWRITE() result may exceed the maximum string length - the maximum input length that is guaranteed not to do so is a 116,510 byte string.
* If all its arguments are literals, $ZWRITE() evaluates to a literal constant at compile time.
* Note that non-graphic characters differ between M mode and UTF-8 mode.

+++++++++++++++++++++
Examples of $ZWRITE()
+++++++++++++++++++++

Example:

.. code-block:: bash

   YDB>set temp="X"_$char(10)_"X" ; $CHAR(10) is the linefeed character
   YDB>write temp
   X
   X
   YDB>write $zwrite(temp)
   "X"_$C(10)_"X"
   YDB>write $zwrite($zwrite(temp),1)
   X
   X
   YDB>

-----------------
$ZYHASH()
-----------------

Returns the 128-bit `MurmurHash3 <https://en.wikipedia.org/wiki/MurmurHash#MurmurHash3>`_ of a string as a hexadecimal string prefixed with :code:`"0x"`. This is equivalent to calling the C API function `ydb_mmrhash_128() <../MultiLangProgGuide/cprogram.html#ydb-mmrhash-128-fn>`_ and passing its return value to `ydb_mmrhash_128_hex() <../MultiLangProgGuide/cprogram.html#ydb-mmrhash-128-hex-fn>`_

.. code-block:: none

   $ZYHASH(string[,salt])

* The first argument is the string to be hashed.
* The second argument (salt) is an integer 0 through 4,294,967,295, and defaulting to zero to be used as a salt for the hash.

+++++++++++++++++++++
Examples of $ZYHASH()
+++++++++++++++++++++

.. code-block:: bash

   YDB>write $zyhash("YottaDB Rocks!")
   0xa91a6a91c8d3afa118ae643d7c08c007
   YDB>write $zyhash("YottaDB Rocks!",0)
   0xa91a6a91c8d3afa118ae643d7c08c007
   YDB>write $zyhash("YottaDB Rocks!",123456789)
   0xb931c7fa5746e15dc9fdaecfb29b8626
   YDB>

.. note::
   $ZYHASH() should never be used when cryptographic quality hashes are needed.

$ZYHASH() is in YottaDB effective release `r1.30 <https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.30>`_.

-----------------
$ZYISSQLNULL()
-----------------

Returns 1 if its sole argument has a value :ref:`zysqlnull-isv`, and 0 otherwise.

.. code-block:: none

   $ZYISSQLNULL(expr)

++++++++++++++++++++++++++
Examples of $ZYISSQLNULL()
++++++++++++++++++++++++++

.. code-block:: bash

   YDB>set x=$ZYSQLNULL write $ZYISSQLNULL(x)
   1
   YDB>set x="Something else" write $ZYISSQLNULL(x)
   0
   YDB>

$ZYISSQLNULL() is in YottaDB effective release `r1.30 <https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.30>`_.

-------------
$ZYSUFFIX()
-------------

Returns a 128-bit `MurmurHash3 <https://en.wikipedia.org/wiki/MurmurHash#MurmurHash3>`_ of its string argument rendered as a 22 character alphanumeric (i.e., 0-9, a-z, A-Z) sequence suitable for concatenation to an application identifier (e.g., :code:`"^%MyApp"`) to generate names for global variables, local variables, and routines that are unique for all practical purposes.

.. code-block:: none

   $ZYSU[FFIX](string)

.. note::

   * YottaDB supports names that are unique in the first 31 characters, and
   * The function may return different sequences for the same :code:`string` on different platforms

+++++++++++++++++++++++++
Examples of $ZYSUFFIX()
+++++++++++++++++++++++++

.. code-block:: bash

   YDB>write $zysuffix("abc")
   jKHjZ0MZwBJAqO6sGQV7F5
   YDB>

$ZYSUFFIX() was added in YottaDB effective release `r1.32 <https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.32>`_.
