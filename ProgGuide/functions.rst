
.. index::
   Functions

=======================
Functions
=======================

.. contents::
   :depth: 2

This chapter describes M language Intrinsic Functions implemented in YottaDB/GT.M. Traditional string processing functions have parallel functions that start with the letter "z". The parallel functions extend the byte-oriented functionality of their counterparts to UTF-8 mode. They are helpful when applications need to process binary data including blobs, binary byte streams, bit-masks, and so on.

Other functions that start with the letter "z" and do not have counterparts implement new functionality and are YottaDB/GT.M additions to the ANSI standard Intrinsic Functions. The M standard specifies standard abbreviations for Intrinsic Functions and rejects any non-standard abbreviations.

M Intrinsic Functions start with a single dollar sign ($) and have one or more arguments enclosed in parentheses () and separated by commas (,). These functions provide expression results by performing actions that are impossible or difficult to perform using M commands.

-----------------
$ASCII()
-----------------

Returns the integer ASCII code for a character in the given string. For a mumps process started in UTF-8 mode, $ASCII() returns the integer Unicode code-point value of a character in the given string.

The format for the $ASCII function is:

.. parsed-literal::
   $A[SCII](expr[,intexpr])

* The expression is the source string from which $ASCII() extracts the character it decodes.
* intexpr contains the position within the expression of the character that $ASCII() decodes. If intexpr is missing, $ASCII() returns a result based on the first character position.
* If intexpr evaluates to before the beginning or after the end of the expression, $ASCII() returns a value of negative one (-1).

$ASCII() provides a means of examining non-graphic characters in a string. When used with $CHAR(), $ASCII() also provides a means to perform arithmetic operations on the codes associated with characters.

$ZASCII() is the parallel function of $ASCII(). $ZASCII() interprets the string argument as a sequence of bytes (rather than a sequence of characters) and can perform all byte-oriented $ASCII() operations. For more information, refer to “$ZAscii()”.

++++++++++++++++++++
Examples of ASCII()
++++++++++++++++++++

Example:

.. parsed-literal::
   GTM>For i=0:1:3 Write !,$Ascii("Hi",i)
   -1
   72
   73
   -1
   GTM>

This loop displays the result of $ASCII() specifying a character position before, first and second positions, and after the string.

Example:

.. parsed-literal::
   GTM>Write $ZCHSET
   UTF-8
   GTM>Write $Ascii("主")
   20027
   GTM>Write $$FUNC^%DH("20027")
   00004E3B

In this example, 20027 is the integer equivalent of the hexadecimal value 4E3B. U+4E3B is a character in the CJK Ideograph block of the Unicode Standard.

-----------------
$CHAR()
-----------------

Returns a string of one or more characters corresponding to integer ASCII codes specified in its argument(s). For a process started in UTF-8 mode, $CHAR() returns a string composed of characters represented by the integer equivalents of the Unicode code-points specified in its argument(s).

The format for the $CHAR function is:

.. parsed-literal::
   $C[HAR](intexpr[,...])

* The integer expression(s) specify the codes of the character(s) $CHAR() returns.
* The M standard does not restrict the number of arguments to $CHAR(). However, YottaDB/GT.M does limit the number of arguments to a maximum of 254. $CHAR() provides a means of producing non-graphic characters, as such characters cannot appear directly within an M string literal. When used with $ASCII(), $CHAR() can also perform arithmetic operations on the codes associated with characters.
* With VIEW "BADCHAR" enabled, $CHAR() produces a run-time error if any expression evaluates to a code-point value that is not a character in Unicode. YottaDB/GT.M determines from ICU which characters are illegal.
* $ZCHAR() is the parallel function of $CHAR(). $ZCHAR() returns a sequence of bytes (rather than a sequence of characters) and can perform all byte-oriented $CHAR() operations. For more information, refer to “$ZCHar()”.

++++++++++++++++++++
Examples of $CHAR()
++++++++++++++++++++

Example:

.. parsed-literal::
   GTM>write $char(77,85,77,80,83,7)
   MUMPS
   GTM>

This example uses $CHAR() to WRITE the word MUMPS and signal the terminal "bell."

Example:

.. parsed-literal::
   set nam=$extract(nam,1,$length(nam)-1)_$char($ascii(nam,$length(nam))-1)

This example uses $CHAR() and $ASCII() to set the variable nam to a value that immediately precedes its previous value in the set of strings of the same length as nam.

Example:

.. parsed-literal::
   GTM>write $zchset
   UTF-8
   GTM>write $char(20027)
   主
   GTM>write $char(65)
   A

In the above example, the integer value 20027 is the Unicode character "主" in the CJK Ideograph block of Unicode. Note that the output of the $CHAR() function for values of integer expression(s) from 0 through 127 does not vary with choice of the character encoding scheme. This is because 7-bit ASCII is a proper subset of UTF-8 character encoding scheme. The representation of characters returned by the $CHAR() function for values 128 through 255 differ for each character encoding scheme.

----------------
$DATA()
----------------

Returns an integer code describing the value and descendent status of a local or global variable.

The format for the $DATA function is:

.. parsed-literal::
   $D[ATA](glvn)

* The subscripted or unsubscripted global or local variable name specifies the target node.
* If the variable is undefined, $DATA() returns 0.
* If the variable has a value but no descendants, $DATA() returns 1.
* If the variable has descendants but no value, $DATA() returns 10.
* If the variable has a value and descendants, $DATA() returns 11.
* $ZDATA() extends $DATA() to reflects the current alias state of the lvn or name argument to identify alias and alias container variables. For more information, refer to “$ZDATA()”.

The following table summarizes $DATA() return values.

**$DATA() Results**

+---------------+----------------------------------------------------------+--------------------------------------------------------------+
|                                        Value                                                                                            |
+===============+==========================================================+==============================================================+
|               | Descendants (No)                                         | Descendants (Yes)                                            |
+---------------+----------------------------------------------------------+--------------------------------------------------------------+
| NO            | 0                                                        | 10                                                           |
+---------------+----------------------------------------------------------+--------------------------------------------------------------+
| YES           | 1                                                        | 11                                                           |
+---------------+----------------------------------------------------------+--------------------------------------------------------------+

$DATA() return values can also be understood as a pair of truth-values where the left describes descendants and the right describes data 1 and where M suppresses any leading zero (representing no descendants).

+++++++++++++++++++++
Examples for $DATA()
+++++++++++++++++++++

Example:

.. parsed-literal::
   GTM>Kill  Write $Data(a)
   0
   GTM>Set a(1)=1 Write $Data(a(1))
   1
   GTM>Write $Data(a)
   10
   GTM>Set a=0 Write $Data(a)
   11
   GTM>

This uses $DATA to display all possible $DATA() results.

Example:

.. parsed-literal::
   lock ^ACCT(0)
   if '$data(^ACCT(0)) set ^ACCT(0)=0
   set (ACCT,^ACCT(0))=^ACCT(0)+1
   lock

This uses $DATA() to determine whether a global node requires initialization.

Example:

.. parsed-literal::
   for  set cus=$O(^cus(cus)) quit:cus=""  if $data(^(cus))>1 do WORK

This uses $DATA() to determine whether a global node has descendants and requires additional processing.

-------------------
$Extract()
-------------------

Returns a substring of a given string.

The format for the $EXTRACT function is:

.. parsed-literal::
   $E[XTRACT](expr[,intexpr1[,intexpr2]])

* The expression specifies a string from which $EXTRACT() derives a substring.
* The first optional integer expression (second argument) specifies the starting character position in the string. If the starting position is beyond the end of the expression, $EXTRACT() returns an empty string. If the starting position is zero (0) or negative, $EXTRACT() starts at the first character; if this argument is omitted, $EXTRACT() returns the first character of the expression. $EXTRACT() numbers character positions starting at one (1) (that is, the first character of a string is at position one (1)).
* The second optional integer expression (third argument) specifies the ending character position for the result. If the ending position is beyond the end of the expression, $EXTRACT() stops with the last character of the expression. If the ending position precedes the starting position, $EXTRACT() returns an empty string. If this argument is omitted, $EXTRACT() returns one character at most.

$EXTRACT() provides a tool for manipulating strings based on character positions.

For a mumps process started in UTF-mode, $EXTRACT interprets the string arguments as UTF-8 encoded. With VIEW "BADCHAR" enabled, $EXTRACT() produces a run-time error when it encounters a character in the reserved range of the Unicode Standard, but it does not process the characters that fall after the span specified by the arguments. The parallel function of $EXTRACT() is $ZEXTRACT(). Use $ZEXTRACT() for byte-oriented operations. For more information, refer to “$ZExtract()”.

$EXTRACT() can be used on the left-hand side of the equal sign (=) of a SET command to set a substring of a string. This construct permits easy maintenance of individual pieces within a string. It can also be used to right justify a value padded with blank characters. For more information on SET $EXTRACT(), refer to “Set” in the Commands chapter.

++++++++++++++++++++++
Examples of $EXTRACT()
++++++++++++++++++++++

Example:

.. parsed-literal::
   GTM>for i=0:1:3 write !,$extract("HI",i),"<"
   <
   H<
   I<
   <
   GTM>

This loop displays the result of $EXTRACT(), specifying no ending character position and a beginning character position "before" first and second positions, and "after" the string.

Example:

.. parsed-literal::
   GTM>For i=0:1:3 write !,$extract("HI",1,i),"<"
   <
   H<
   HI<
   HI<
   GTM>

This loop displays the result of $EXTRACT() specifying a beginning character position of 1 and an ending character position "before, " first and second positions, and "after" the string.

Example:

.. parsed-literal::
   GTM>zprint ^trim
   trim(x)
       new i,j
       for i=1:1:$length(x) quit:" "'=$extract(x,i)
       for j=$length(x):-1:1 quit:" "'=$extract(x,j)
       quit $extract(x,i,j)
   GTM>set str=" MUMPS "
   GTM>write $length(str)
   7
   GTM>write $length($$^trim(str))
   5
   GTM>

This extrinsic function uses $EXTRACT() to remove extra leading and trailing spaces from its argument.

------------------
$Find()
------------------

Returns an integer character position that locates the occurrence of a substring within a string.

The format for the $FIND function is:

.. parsed-literal::
   $F[IND](expr1,expr2[,intexpr])

* The first expression specifies the string within which $FIND() searches for the substring.
* The second expression specifies the substring for which $FIND() searches.
* The optional integer expression identifies the starting position for the $FIND() search. If this argument is missing, zero (0), or negative, $FIND() begins its search in the first position of the string.
* If $FIND() locates the substring, it returns the position after the last character of the substring. If the end of the substring coincides with the end of the string (expr1), it returns an integer equal to the length of the string plus one ($L(expr1)+1).
* If $FIND() does not locate the substring, it returns zero (0).
* For a process started in UTF-8 mode, $FIND() interprets the string arguments as UTF-8 encoded. With VIEW "BADCHAR" enabled, $FIND() produces a run-time error when it encounters a malformed character, but it does not process the characters that fall after the span specified by the arguments.
* $ZFIND() is the Z equivalent function $FIND(). Irrespective of the settings of VIEW "BADCHAR" and $ZCHSET, $ZFIND() interprets argument as a sequence of bytes (rather than a sequence of characters) and can perform byte-oriented $FIND() operations.For more information, refer to “$ZFind()”.

$FIND() provides a tool to locate substrings. The ([) operator and the two-argument $LENGTH() are other tools that provide related functionality.

+++++++++++++++++++++
Examples of $FIND()
+++++++++++++++++++++

Example:

.. parsed-literal::
   GTM>write $find("HIFI","I")
   3
   GTM>

This example uses $FIND() to WRITE the position of the first occurrence of the character "I." The return of 3 gives the position after the "found" substring.

Example:

.. parsed-literal::
   GTM>write $find("HIFI","I",3)
   5
   GTM>

This example uses $FIND() to WRITE the position of the next occurrence of the character "I" starting in character position three.

Example:

.. parsed-literal::
   GTM>set t=1 for  set t=$find("BANANA","AN",t) quit:'t  write !,t
   4
   6
   GTM>

This example uses a loop with $FIND() to locate all occurrences of "AN" in "BANANA". $FIND() returns 4 and 6 giving the positions after the two occurrences of "AN".

Example:

.. parsed-literal::
   GTM>set str="MUMPS databases are hierarchical"
   GTM>Write $find(str," ")
   7
   GTM>Write $find(str,"Z")
   0
   GTM>Write $find(str,"d",1)
   8
   GTM>Write $find(str,"d",10)
   0

The above example searches a string for a sub string, and returns an integer value which corresponds to the next character position after locating the sub string.

----------------------
$FNumber()
----------------------

Returns a string containing a formatted number.

The format for the $FNUMBER function is:

.. parsed-literal::
   $FN[UMBER](numexpr,expr[,intexpr])

* The numeric expression specifies the number that $FNUMBER() formats.
* The expression (second argument) specifies zero or more single character format control codes; if the expression contains any character other than the defined codes, $FNUMBER() generates a run-time error.
* The optional integer expression (third argument) specifies the number of digits after the decimal point. If the numeric expression has more digits than specified by this argument, $FNUMBER() rounds to obtain the result. If the numeric expression has fewer digits than specified by this argument, $FNUMBER() zero-fills to obtain the result.
* When the optional third argument is specified and the first argument evaluates to a fraction between -1 and 1, $FNUMBER() returns a number with a leading zero (0) before the decimal point (.).

$FNUMBER() formats or edits numbers, usually for reporting. For more information on rounding performed by $FNUMBER(), refer to “$Justify()”.

The formatting codes are:

* \+ : Forces a "+" on positive values.
* \- : Suppresses the "-" on negative values.
* , : Inserts commas every third position to the left of the decimal within the number.
* T : Represents the number with a trailing, rather than a leading sign; positive numbers have a trailing space unless the expression includes a plus sign (+).
* P : Represents negative values in parentheses, positive values with a space on either side; combining with any other code except comma (,) causes a run-time error.

+++++++++++++++++++++++
Examples of $FNUMBER()
+++++++++++++++++++++++

Example:

.. parsed-literal::
   GTM>do ^fnum
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

.. parsed-literal::
   set x=$fnumber(x,"-")

This example uses $FNUMBER() to SET x equal to its absolute value.


------------------
$Get()
------------------

Returns the value of a local or global variable if the variable has a value. If the variable has no value, the function returns a value specified by an optional second argument, and otherwise returns an empty string.

The format for the $GET function is:

.. parsed-literal::
   $G[ET](glvn[,expr])

* The subscripted or unsubscripted global or local variable name specifies the node for which $GET() returns a value.
* If the global or local variable has a data value, $GET() returns the value of the variable.
* If the global or local variable has no data value, $GET() returns the value of the optional expression (second argument), or an empty string if the expression is not specified.

M defines $GET(x,y) as equivalent to:

.. parsed-literal::
   $Select($Data(x)[0:y,1:x)

and $GET(x) as equivalent to:

.. parsed-literal::
   $GET(x,"")

$GET() provides a tool to eliminate separate initialization of variables. This technique may provide performance benefits when used to increase the density of a sparse global array by eliminating nodes that would otherwise hold absent optional information. On the other hand, some uses of one argument $GET() can mask logic problems.

YottaDB/GT.M has a "NOUNDEF" mode of operation, which treats all variable references as if they were arguments to a one argument $GET(). The VIEW command controls "NOUNDEF" mode.

+++++++++++++++++++
Examples of $GET()
+++++++++++++++++++

Example:

.. parsed-literal::
   setstatus;
            if '$data(^PNT(NAME,TSTR)) set STATUS="NEW TEST"
            else  if ^PNT(NAME,TSTR)="" set STATUS="WAITING FOR RESULT"
            else  set STATUS=^PNT(NAME,TSTR)

This example can be reduced to two lines of code by using $GET(), shown in the following example. However, by using $GET() in its one-argument form, the distinction between an undefined variable and one with a null value is lost:

.. parsed-literal::
   set STATUS=$get(^PNT(NAME,TSTR))
   if STATUS="" set STATUS="WAITING FOR RESULT"

This is solved by using the two-argument form of $GET():

.. parsed-literal::
   set STATUS=$get(^PNT(NAME,TSTR),"NEW TEST")
   if STATUS="" set STATUS="WAITING FOR RESULT"

------------------
$Increment()
------------------

Atomically adds (increments) a global variable by a numeric value. Note that increment is atomic, but the evaluation of the expression is not, unless inside a transaction (TStart/TCommit). The function also works on local variables, but has less benefit for locals as it does not (need to) provide ACID behavior.

The format of the $INCREMENT function is:

.. parsed-literal::
   $INCREMENT(glvn[,numexpr])

* $I, $INCR, $INCREMENT, $ZINCR, and $ZINCREMENT are considered as valid synonyms of the full function name.
* $INCREMENT() returns the value of the glvn after the increment.
* If not specified, numexpr defaults to 1. Otherwise, $INCREMENT() evaluates the "numexpr" argument before the "glvn" argument.
* numexpr can be a negative value.
* Since it performs an arithmetic operation, $INCREMENT() treats glvn as numeric value. $INCREMENT treats glvn as if it were the first argument of an implicit $GET() before the increment. If the value of glvn is undefined $INCREMENT treats it as having empty string , which means it treats it as a numeric zero (0) (even if glvn is a global variable that resides on a remote node and is accessed through a GT.CM GNP server).
* If $INCREMENT() occurs inside a transaction ($TLevel is non-zero), or if glvn refers to a local variable, it is equivalent to SET glvn=$GET(glvn)+numexpr.
* If $INCREMENT() occurs outside a transaction ($TLevel is zero) and glvn refers to a global variable, the function acts as a SET glvn=$GET(glvn)+numexpr performed as an Atomic, Consistent and Isolated operation. Note that $INCREMENT() performs the evaluation of numexpr before it starts the Atomic, Consistent, Isolated incrementing of the glvn. If the region containing the glvn is journaled, then the $INCREMENT() is also Durable. Only BG, MM (OpenVMS only) and GT.CM GNP access methods are supported for the region containing the global variable (glvn). GT.CM OMI and GT.CM DDP access methods do not support this operation and there are no current plans to add such support.
* $INCREMENT() does not support global variables that have NOISOLATION turned ON (through the VIEW "NOISOLATION" command), and a $INCREMENT() on such a variable, triggers at GTM-E-GVINCRISOLATION run-time error.
* The naked reference is affected by the usage of global variables (with or without indirection) in the glvn and/or numexpr components. The evaluation of "numexpr" ahead of "glvn" determines the value of the naked reference after the $INCREMENT. If neither glvn or numexpr contain indirection, then $INCREMENT sets the naked reference as follows:
  
  * glvn, if glvn is a global, or
  * the last global reference in "numexpr" if glvn is a local, or
  * unaffected if neither glvn nor numexpr has any global reference.

+++++++++++++++++++++++++
Examples of $INCREMENT()
+++++++++++++++++++++++++

Example:

.. parsed-literal::
   GTM>set i=1
   GTM>write $increment(i)
   2
   GTM>write $increment(i)
   3
   GTM>write $increment(i)
   4
   GTM>write $increment(i)
   5
   GTM>write i
   5
   GTM>write $increment(i,-2)
   3
   GTM>write I
   3
   GTM>

This example increments the value of i by 1 and at the end decrements it by 2. Note that the default value for incrementing a variable is 1.

-------------------
$Justify()
-------------------

Returns a formatted string.

The format for the $JUSTIFY function is:

.. parsed-literal::
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
* $ZJUSTIFY() is the parallel function of $JUSTIFY(). Irrespective of the settings of VIEW "BADCHAR" and $ZCHSET, $ZJUSTIFY() interprets argument as a sequence of bytes (rather than a sequence of characters) and can perform all byte-oriented $JUSTIFY() operations. For more information, refer to “$ZJustify()”.

++++++++++++++++++++++++
Examples of $JUSTIFY()
++++++++++++++++++++++++

Example:

.. parsed-literal::
   GTM>write ":",$justify("HELLO",10),":",!,":",$justify("GOODBYE",5),":"
   :     HELLO:
   \:GOODBYE\:
   GTM>

This uses $JUSTIFY() to display "HELLO" in a field of 10 spaces and "GOODBYE" in a field of 5 spaces. Because the length of "GOODBYE" exceeds five spaces, the result overflows the specification.

Example:

.. parsed-literal::
   GTM>write "1234567890",!,$justify(10.545,10,2)
   1234567890
        10.55
   GTM>

This uses $JUSTIFY() to WRITE a rounded value right justified in a field of 10 spaces. Notice that the result has been rounded up.

Example:

.. parsed-literal::
   GTM>write "1234567890",!,$justify(10.544,10,2)
   1234567890
        10.54
   GTM>

Again, this uses $JUSTIFY() to WRITE a rounded value right justified in a field of 10 spaces. Notice that the result has been rounded down.

Example:

.. parsed-literal::
   GTM>write "1234567890",!,$justify(10.5,10,2)
   1234567890
        10.50
   GTM>

Once again, this uses $JUSTIFY() to WRITE a rounded value right justified in a field of 10 spaces. Notice that the result has been zero-filled to 2 places.

Example:

.. parsed-literal::
   GTM>write $justify(.34,0,2)
   0.34
   GTM>

This example uses $JUSTIFY to ensure that the fraction has a leading zero. Note the use of a second argument of zero in the case that rounding is the only function that $JUSTIFY is to perform.

-------------------------
$Length()
-------------------------

Returns the length of a string measured in characters, or in "pieces" separated by a delimiter specified by one of its arguments.

The format for the $LENGTH function is:

.. parsed-literal::
   $L[ENGTH](expr1[,expr2])

* The first expression specifies the string that $LENGTH() "measures".
* The optional second expression specifies the delimiter that defines the measure; if this argument is missing, $LENGTH() returns the number of characters in the string.
* If the second argument is present and not an empty string, $LENGTH returns one more than the count of the number of occurrences of the second string in the first string; if the second argument is an empty string, the M standard specifies that $LENGTH() returns a zero (0).
* $LENGTH() provides a tool for determining the lengths of strings in two ways, characters and pieces. The two argument $LENGTH() returns the number of existing pieces, while the one argument returns the number of characters.
* For a process started in UTF-8 mode, $LENGTH() interprets the string argument(s) as UTF-8 encoded. With VIEW "BADCHAR" enabled, $LENGTH() produces a run-time error when it encounters a malformed character.
* $ZLENGTH() is the parallel function of $LENGTH(). Irrespective of the setting of VIEW "BADCHAR" and $ZCHSET, $ZLENGTH() interpets string arguments as a sequence of bytes (rather than characters) and can perform all byte-oriented $LENGTH() operations. For more information, refer to “$ZLength()”.

+++++++++++++++++++++++++++
Examples of $LENGTH()
+++++++++++++++++++++++++++

Example:

.. parsed-literal::
   GTM>Write $length("KINGSTON")
   8
   GTM>

This uses $LENGTH() to WRITE the length in characters of the string "KINGSTON".

Example:

.. parsed-literal::
   GTM>set x="Smith/John/M/124 Main Street/Ourtown/KA/USA"
   GTM>write $length(x,"/")
   7
   GTM>

This uses $LENGTH() to WRITE the number of pieces in a string, as delimited by /.

Example:

.. parsed-literal::
   GTM>write $length("/2/3/","/")
   4
   GTM>

This also uses $LENGTH() to WRITE the number of pieces in a string, as delimited by /. Notice that YottaDB/GT.M. adds one count to the count of delimiters (in this case 3), to get the number of pieces in the string (displays 4).

---------------------
$NAme()
---------------------

Returns an evaluated representation of some or all of a local or global variable name.

The format for the $NAME function is:

.. parsed-literal::
   $NA[ME](glvn[,intexpr])

* The subscripted or unsubscripted global or local variable name, including naked references, specifies the name for which $NAME() returns an evaluated representation.
* When using NOUNDEF, $NAME() returns an empty string where appropriate for undefined variables.
* The optional integer expression (second argument) specifies the maximum number of subscript levels in the representation. If the integer expression is not provided or exceeds the actual number of subscript levels, $NAME() returns a representation of the whole name. If the integer expression is zero (0), $NAME() returns only the name. A negative integer expression produces a run-time error.

+++++++++++++++++++++++
Examples of $NAME()
+++++++++++++++++++++++

Example:

.. parsed-literal::
   GTM>set X="A""B",^Y(1,X,"B",4)="" 
   GTM>write $name(^(3),3)
   ^Y(1,"A""B","B")
   GTM>

This example sets up a naked reference and then uses $NAME() to display the first three levels of that four-level reference.

Example:

.. parsed-literal::
   GTM>write $name(^(3),0)
   ^Y
   GTM>

This example shows the name level for the same naked reference.

----------------------
$Next()
----------------------

Returns the next subscripted local or global variable name in collation sequence within the array level specified by its argument.

$NEXT() has been replaced by $ORDER(). $NEXT has been retained in the current standard only for compatibility with earlier versions of the standard. $NEXT() is similar to $ORDER(). However, $NEXT() has the deficiency that when it encounters negative one (-1) as a subscript, it returns the same result as when it finds no other data at the level. This deficiency is particularly disruptive because it occurs in the middle of the M collating sequence.

.. note::
   As $NEXT() has been removed from the standard in the MDC, you should use $ORDER.

The format for the $NEXT function is:

.. parsed-literal::
   $N[EXT](glvn)

* The subscripted global or local variable name specifies the node following which $NEXT() searches for the next node with data and/or descendants; the number of subscripts contained in the argument implicitly defines the array level.
* If $NEXT() finds no node at the specified level after the specified global or local variable, it returns negative one (-1).
* If the last subscript in the subscripted global or local variable name is null or negative one (-1), $NEXT() returns the first node at the specified level.

----------------------
$Order()
----------------------

Returns the subscript of the next or prior local or global variable name in collation sequence within the array level specified by its first argument. In doing so, it moves in the direction specified by the second argument. In YottaDB/GT.M, when $ORDER() has an unsubscripted argument, it returns the next or previous unsubscripted local or global variable name in collating sequence.

The format for the $ORDER function is:

.. parsed-literal::
   $O[RDER](glvn[,expr])

* The subscripted global or local variable name specifies the node from which $ORDER() searches for the next or previous node that has data and/or descendants. The number of subscripts contained in the argument implicitly defines the array level.
* The optional expression (second argument) specifies the direction for the $ORDER(); 1 specifies forward operation and -1 specifies reverse operation. Any other values for the expression will cause an error.
* YottaDB/GT.M extends the M standard to allow unsubscripted names. In this case, $ORDER() returns the next or previous unsubscripted name.
* If $ORDER() finds no node (or name) at the specified level after (or before) the specified global or local variable, it returns an empty string (" ").
* If the last subscript in the subscripted global or local variable name is null and the corresponding subscripted global or local variable has a matching null subscript, $ORDER() returns the next node after that with the null subscript at the specified level.
* If the last subscript in the subscripted global or local variable name is null and the corresponding subscripted global or local variable has no matching null subscript , $ORDER() returns first node at the specified level. If the last subscript in the subscripted global or local variable name is null and second argument is -1, $ORDER() always returns the last node at the specified level regardless of the existence a null subscript at the specified level. However when a global or local variable level includes a null subscript and $ORDER(glvn,-1) returns an empty string result, users must test separately for the existence of the node with the null subscript.
* $ORDER() can be used as a tool for retrieving data from M sparse arrays in an ordered fashion, independent of the order in which it was entered. In M, routines generally sort by SETting data into an array with appropriate subscripts and then retrieving the information with $ORDER().
* $ORDER() returns subscripts, not data values, and does not discriminate between nodes that have data values and nodes that have descendants. Once $ORDER() provides the subscript, the routine must use the subscript to access the data value, if appropriate. Using $ORDER() maintains the naked reference indicator, even if $ORDER() returns a null.
* YottaDB/GT.M optionally permits the use of null subscripts. This feature is enabled via the VIEW command for local variables and a REGION qualifier in GDE for global variables. When an application uses null subscripts, they are "invisible" in a $ORDER() loop so the application must test for them as a special case, perhaps using $DATA().
* $Order() returns local array subscripts with values that are numeric, but non-canonical (over 18 digit), as strings.

.. note::
   Name-level $ORDER() always returns an empty string when used with extended references.

++++++++++++++++++++++
Examples of $ORDER()
++++++++++++++++++++++

Example:

.. parsed-literal::
   GTM>zwrite
   lcl(1)=3
   lcl("x")=4
   GTM>write $order(lcl(""))
   1

This example returns the first node, that is 1, because the specified last subscript of the argument is null and lcl has no null subscript.

Example:

.. parsed-literal::
   GTM>write $order(lcl(1))
   x

This example returns the first node after lcl(1) that is x because lcl has no null subscript.

Example:

.. parsed-literal::
   GTM>write $order(lcl(""),-1)
   x

This example returns the last node that is, x, because the last subscript of the first argument is null and second argument is -1.

.. parsed-literal::
   GTM>set lcl("")=2
   GTM>zwrite
   lcl("")=2
   lcl(1)=3
   lcl("x")=4
   GTM>write $order(lcl(""))
   1

This example returns the second node at the specified level because the null subscript at the end of the argument is ambiguous (does it specify starting at the beginning or starting at the real node with the null subscript?) and returning the subscript of the first node (an empty string) would tend to create an endless loop.

Example:

.. parsed-literal::
   GTM>write $order(lcl(""),-1)
   x
   GTM>write $order(lcl("x"),-1)
   1

Example:

.. parsed-literal::
   GTM>kill  set (a(1),a(2000),a("CAT"),a("cat"),a("ALF"),a(12))=1
   GTM>set x="" for  set x=$order(a(x)) quit:x=""  write !,x
   1
   12
   2000
   ALF
   CAT
   cat
   GTM>kill a("CAT") set a(5,10)="woolworths",a("cat")="last"
   GTM>set x="" for  set x=$order(a(x),-1) quit:x=""  write !,x
   cat
   ALF
   2000
   12
   5
   1
   GTM>

This example uses a $ORDER() loop to display all the subscripts at the first level of local variable a, make some changes in a, and then display all the subscripts in reverse order. Notice that $ORDER() returns only the existing subscripts in the sparse array and returns them in M collation sequence, regardless of the order in which they were entered. Also, $ORDER() does not differentiate between node A(5), which has only descendants (no data value), and the other nodes, which have data values.

Example:

.. parsed-literal::
   GTM>kill set (%(1),tiva(2),A(3),tiv(4),Q(5),%a(6))=""
   GTM>set x="%"
   GTM>write:$data(@x) !,x for  set x=$order(@x) quit:x=""  write !,x
   %
   %a
   A
   Q
   tiv
   tiva
   x
   GTM>set $piece(x,"z",32)=""
   GTM>write:$data(@x) !,x for  set x=$order(@x,-1) quit:x=""  write !,x
   x
   tiva
   tiv
   Q
   A
   %a
   %
   GTM>

This example uses $ORDER() to display the current local variable names in both forward and reverse order. Notice that the first ([^]%) and last ([^]zzzzzzzz) names require handling as special cases and require a $DATA() function.

Example:

.. parsed-literal::
   set acct="",cntt=""
   for  fet acct=$order(^acct(acct)) quit:acct=""  do
   . for  set cntt=$order(^acct(acct,cntt)) do WORK
   quit

This uses two nested $ORDER() loops to cycle through the ^acct global array and perform some action for each second level node.

---------------------
$Piece()
---------------------

Returns a substring delimited by a specified string delimiter made up of one or more characters. In M, $PIECE() returns a logical field from a logical record.

The format for the $PIECE function is:

.. parsed-literal::
   $P[IECE](expr1,expr2[,intexpr1[,intexpr2]])

* The first expression specifies the string from which $PIECE() computes its result.
* The second expression specifies the delimiting string that determines the piece "boundaries"; if this argument is an empty string, $PIECE() returns an empty string.
* If the second expression does not appear anywhere in the first expression, $PIECE() returns the entire first expression (unless forced to return an empty string by the second integer expression).
* The optional first integer expression (third argument) specifies the beginning piece to return; if this argument is missing, $PIECE() returns the first piece.
* The optional second integer expression (fourth argument) specifies the last piece to return. If this argument is missing, $PIECE() returns only one piece unless the first integer expression is zero (0) or negative, in which case it returns a null string. If this argument is less than the first integer expression, $PIECE() returns an empty string.
* If the second integer expression exceeds the actual number of pieces in the first expression, $PIECE() returns all of the expression after the delimiter selected by the first integer expression.
* The $PIECE() result never includes the "outside" delimiters; however, when the second integer argument specifies multiple pieces, the result contains the "inside" occurrences of the delimiter.
* $PIECE() can also be used as tool for efficiently using values that contain multiple elements or fields, each of which may be variable in length.
* Applications typically use a single character for a $PIECE() delimiter (second argument) to minimize storage overhead, and increase efficiency at run-time. The delimiter must be chosen so the data values never contain the delimiter. Failure to enforce this convention with edit checks may result in unanticipated changes in the position of pieces within the data value. The caret symbol (^), backward slash (\), and asterisk (*) characters are examples of popular visible delimiters. Multiple character delimiters may reduce the likelihood of conflict with field contents. However, they decrease storage efficiency, and are processed with less efficiency than single character delimiters. Some applications use control characters, which reduce the chances of the delimiter appearing in the data but sacrifice the readability provided by visible delimiters.
* A SET command argument can have something that has the format of a $PIECE() on the left-hand side of its equal sign (=). This construct permits easy maintenance of individual pieces within a string. It also can be used to generate a string of delimiters. For more information on SET $PIECE(), refer to “Set”.
* $PIECE() can also be used as target in a SET command to change part of the value of a node. Also, when SET arguments have multiple parenthesized (set-left) targets and a target is used as a subscript in more than one item in the list of targets that follow, all the targets use the before-SET value (not the after-SET value) in conformance to the M-standard. For more information on SET $PIECE(), refer to “Set”.
* For a process started in UTF-8 mode, $PIECE() interprets the string arguments as UTF-8 encoded. With VIEW "BADCHAR" enabled, $PIECE() produces a run-time error when it encounters a malformed character, but it does not process the characters that fall after the span specified by the arguments.
* $ZPIECE() is the parallel function of $PIECE(). Irrespective of the settings of VIEW "BADCHAR" and $ZCHSET, $ZPIECE() interprets string arguments as a sequence of bytes (rather than a sequence of characters) and can perform all byte-oriented $PIECE() operations. For more information, refer to “$ZPIece()”.

++++++++++++++++++++++++++++
Examples of $PIECE()
++++++++++++++++++++++++++++

Example:

.. parsed-literal::
   GTM>for i=0:1:3 write !,$piece("1 2"," ",i),"<"
   <
   1<
   2<
   <
   GTM>

This loop displays the result of $PIECE(), specifying a space as a delimiter, a piece position "before," first and second, and "after" the string.

Example:

.. parsed-literal::
   GTM>for i=-1:1:3 write !,$piece("1 2"," ",i,i+1),"<"
   <
   1<
   1 2<
   2<
   <
   GTM>

This example is similar to the previous example except that it displays two pieces on each iteration. Notice the delimiter (a space) in the middle of the output for the third iteration, which displays both pieces.

Example:

.. parsed-literal::
   for p=1\:1\:$length(x,"/") write ?p-1*10,$piece(x,"/",p)

This example uses $LENGTH() and $PIECE() to display all the pieces of x in columnar format.

Example:

.. parsed-literal::
   GTM>set $piece(x,".",25)="" write x
   ........................

This SETs the 25th piece of the variable x to null, with a delimiter of a period. This produces a string of 24 periods preceding the null.

Example:

.. parsed-literal::
   GTM>set ^x=1,$piece(^a,";",3,2)=^b

This example leaves the naked indicator to pointing to the global ^b.

----------------------
$Qlength()
----------------------

Returns the number of subscripts in a variable name. The format is:

.. parsed-literal::
   $QL[ENGTH] (namevalue)

* The namevalue has the form of an evaluated subscripted or unsubscripted global variable.
* $QLENGTH() returns a value which is derived from namevalue. If namevalue has the form NAME(s1, s2,..., sn), then the function returns n; if the name is unsubscripted, $QLENGTH() yields a length of zero (0).
* $QLENGTH() only affects the naked indicator if the string in question is stored in a global variable.

++++++++++++++++++++++++++
Examples of $QLENGTH()
++++++++++++++++++++++++++

Example:

.. parsed-literal::
   GTM>write $data(^|"XXX"\|ABC(1,2,3,4))
   0
   GTM>set X=$name(^(5,6))
   GTM>write $qlength(X)
   5

The number of subscripts in x is 5. Notice that the name and the environment preceding it do not contribute to the count. Refer to $NAme() section earlier in this chapter for an understanding of the $NAME function.

-------------------------
$QSubscript()
-------------------------

Returns a component of a variable name.

The format of the $QSUBSCRIPT function is:

.. parsed-literal::
   $QS[UBSCRIPT](namevalue, intexpr)

The namevalue has the form of an evaluated subscripted or unsubscripted global or local variable name.

The intexpr selects the component of the name as follows: 

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

Assume that X is defined as in the "Examples of $Qlength()" earlier in this chapter; 

.. parsed-literal::
   write X
   X="^|""XXX""\|ABC(1,2,3,5,6)"
   GTM>write $qsubscript(X,-2)
   error
   GTM>WRITE $qsubscript(X,-1)
   XXX
   GTM>WRITE $qsubscript(X,0)
   ^ABC
   GTM>WRITE $qsubscript(X,1)
   1
   GTM>WRITE $qsubscript(X,4)
   5
   GTM>WRITE $qsubscript(X,7)
   ""

------------------
$Query()
------------------

Returns the next subscripted local or global variable node name, independent of level, which follows the node specified by its argument in M collating sequence and has a data value.

The format for the $QUERY function is:

.. parsed-literal::
   $Q[UERY](glvn)

* The subscripted or unsubscripted global or local variable name specifies the starting node from which $QUERY() searches for a node with a data value.
* If $QUERY() finds no node after the specified global or local variable, it returns an empty string. 
* With stdnullcoll, if $Data(glvn(""))=1 (or 11), $Query(glvn("")) returns glvn(1) (assuming glvn(1) exists). Applications looking for a node with a "null" subscript must use $D(glvn("")) to test the existence of glvn(""). $Q(glvn("...")) never returns the starting-point (glvn("")) even though glvn("") may exist.

$QUERY() can be used as a tool for scanning an entire array for nodes that have data values. Because $QUERY() can return a result specifying a different level than its argument, the result provides a full variable name. This contrasts with $ORDER(), which returns a subscript value. To access the data value at a node, a $ORDER() return can be used as a subscript; however, a $QUERY() return must be used with indirection. Because arrays tend to have homogeneous values within a level but not between levels, $QUERY() is more useful as a tool in utility programs than in application programs. The $QUERY() can be useful in avoiding nested $ORDER loops.

Note that the standard does not unambiguously define the state of the naked reference indicator after a $QUERY(). While in YottaDB/GT.M after $QUERY(), the naked reference indicator reflects the $QUERY() argument, NOT its result.

+++++++++++++++++++++++++++
Examples of $QUERY()
+++++++++++++++++++++++++++

Example:

.. parsed-literal::
   set ^X(1,2,3)="123"
   set ^X(1,2,3,7)="1237"
   set ^X(1,2,4)="124"
   set ^X(1,2,5,9)="1259"
   set ^X(1,6)="16"
   set ^X("B",1)="AB"

The tree diagram below represents the structure produced by the preceding routine.

.. image:: querytree.gif

The following routine:

.. parsed-literal::
   set y="^X"
   for  set y=$query(@y) quit:y=""  write !,y,"=",@y

produces the results:

.. parsed-literal::
   ^X(1,2,3)=123
   ^X(1,2,3,7)=1237
   ^X(1,2,4)=124
   ^X(1,2,5,9)=1259
   ^X(1,6)=16
   ^X("B",1)=AB

Example:

.. parsed-literal::
   GTM>zwrite lcl
   lcl("")=1
   lcl(1)=1
   lcl(1,2)=2
   lcl(1,2,"")=3
   lcl(1,2,"","")=4
   lcl(1,2,"","",4)=5
   lcl(1,2,0)=6
   lcl(1,2,"abc",5)=7
   lcl("x")=1
   GTM>set y="lcl"
   GTM>for  set y=$query(@y) quit:y=""  write !,y,"=",@y

This example produces the results:

.. parsed-literal::
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
$Random()
----------------------

Returns a random integer from a range specified by its argument.

The format for the $RANDOM function is:

.. parsed-literal::
   $R[ANDOM](intexpr)

* The integer expression specifies the upper exclusive limit of a range of integers from which $RANDOM() may pick a result; $RANDOM() never returns a number less than zero (0).
* If $RANDOM() has an argument less than one (1), it generates a run-time error.
* $RANDOM can generate numbers up to 2147483646 (that is 2GB - 2).

$RANDOM() provides a tool for generating pseudo-random patterns useful in testing or statistical calculations. $RANDOM() results fall between zero (0) and one less than the argument.

Random number generators use factors from the environment to create sequences of numbers. True random number generation requires a source of what is known as "noise". Pseudo-random numbers appear to have no pattern, but are developed using interactions between factors that vary in ways not guaranteed to be entirely random. In accordance with the M standard, the YottaDB/GT.M implementation of $RANDOM() produces pseudo-random numbers.

++++++++++++++++++++++++++++
Examples of $RANDOM()
++++++++++++++++++++++++++++

Example:

.. parsed-literal::
   GTM>for i=1:1:10 write $random(1)
   0000000000
   GTM>

This shows that when $RANDOM() has an argument of one (1), the result is too confined to be random.

Example:

.. parsed-literal::
   set x=$random(100)+1*.01

This $RANDOM() example produces a number between 0 and 99. The example then shifts with addition, and scales with multiplication to create a value between .01 and 1.

--------------------------
$REverse()
--------------------------

Returns a string with the characters in the reverse order from that of its argument.

The format for the $REVERSE function is:

.. parsed-literal::
   $RE[VERSE](expr)

* The expr in the syntax is the string to be reversed.

++++++++++++++++++++++++++
Examples of $REVERSE()
++++++++++++++++++++++++++

Example:

.. parsed-literal::
   GTM>write $reverse(123)
   321
   GTM>write $reverse("AbCDe")
   "eDCbA"

---------------------
$Select()
---------------------

Returns a value associated with the first true truth-valued expression in a list of paired expression arguments.

The format for the $SELECT function is:

.. parsed-literal::
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

.. parsed-literal::
   GTM>for i=3:-1:0 write !,$select(i=1:"here",i=2:"come",i=3:"Watson")
   Watson
   come
   here
   %GTM-E-SELECTFALSE, No argument to $SELECT was true
   GTM>

This loop uses $SELECT() to WRITE a series of strings. Because there is no true argument on the fourth iteration, when i=0, $SELECT() produces an error.

Example:

.. parsed-literal::
   set name=$select(sex="M":"Mr. ",sex="F":"Ms. ",1:"")_name

This example uses $SELECT() to add a prefix to the name based on a sex code held in the variable sex. Notice that the default handles the case of a missing or incorrect code.

Example:

.. parsed-literal::
   if $select(x=+x:x,x="":0,"JANAPRJULOCT"[x:1,1:0) do THING

This uses $SELECT() to perform complex logic as the truth-valued expression argument to an IF command.

--------------------
$STack()
--------------------

Returns strings describing aspects of the execution environment.

The format for the $STACK function is:

.. parsed-literal::
   $ST[ACK](intexpr[,expr])

* The intexpr identifies the M virtual machine stack level (as described by the standard), on which the function is to provide information.
* The optional second argument is evaluated as a keyword that specifies a type of information to be returned as follows: 

  * "MCODE" the line of code that was executed.
  * "PLACE" the address of the above line of code or the symbol at ("@") to indicate code executed from a string value.
  * "ECODE" either an empty string, or the error code(s) that was added at this execution level.

  .. note::
     For run-time errors, YottaDB/GT.M does not provide a "PLACE" within a line (unlike it does for compilation errors), but it reports a label, offset, and routine.

* When $STACK has only one argument, values corresponding to available stack levels specify a return value that indicates how the level was created, as follows:
* If intexpr is zero (0), the function returns information on how GT.M was invoked.
* If intexpr is minus one (-1), the function returns the highest level for which $STACK can return information. Note that, if $ECODE="", $STACK(-1) returns the same value as the $STACK ISV.
* If intexpr is greater than zero (0) and less than or equal to $STACK(-1), indicates how this level of process stack was created ("DO", "TRIGGER" - for a stack level invoked by a trigger, "XECUTE", or "$$" - for an extrinsic function).
* $STACK(lvl) reports "ZINTR" for a stack level invoked by MUPIP INTRPT.
* If intexpr is greater than $STACK (-1), the function returns an empty string.
* During error handling, $STACK() return a snapshot of the state of the stack at the time of error. Even if subsequent actions add stack levels, $STACK() continues to report the same snapshot for the levels as of the time of the error. $STACK() reports the latest stack information only after the code clears $ECODE.
* $STACK() assists in debugging programs. 

.. note::
   $STACK() returns similar information to ZSHOW "S" when ""=$ECODE, but when $ECODE contains error information, $STACK() returns information as of the time of a prior error, generally the first entry in $ECODE. For $STACK() to return current information, be sure that error handing code does a SET $ECODE="" before restoring the normal flow of control.

+++++++++++++++++++++++++
Examples of $STACK() 
+++++++++++++++++++++++++

Example:

.. parsed-literal::
   /usr/lib/fis-gtm/V5.4-002B_x86/gtm -run ^dstackex
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

.. parsed-literal::
   GTM>zprint ^debugerr
   debugerr;
    set dsm1=$stack(-1)
    write !,"$stack(-1):",dsm1
    for l=dsm1:-1:0 do
    . write !,l
    . for i="ecode","place","mcode" write ?5,i,?15,$stack(l,i),!
   GTM>

The above example can be used to display a trace of the code path that led to an error.

Example:

.. parsed-literal::
   GTM>zprint ^dstacktst
   dstacktst(x)       ; check $stack() returns with and without clearing $ecode
    set $etrap="do ^debugerr"
    label
     if x>0 set $ecode=",U1," ; if condition
     else  set $ecode=",U2," ;  else condition
     quit
   GTM>do ^dstacktst(0)
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
   %GTM-E-SETECODE, Non-empty value assigned to $ECODE (user-defined error trap)
   GTM>do ^dstacktst(1)
   $stack(-1):1
   1    ecode     ,U2,
        place     label+2^dstacktst
        mcode      else  set $ecode=",U2," ;  else condition
   0    ecode
        place     +1^GTM$DMOD
        mcode
   %GTM-E-SETECODE, Non-empty value assigned to $ECODE (user-defined error trap)
   GTM>set $ecode=""
   GTM>do ^dstacktst(1)
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
   %GTM-E-SETECODE, Non-empty value assigned to $ECODE (user-defined error trap)
   GTM>

This example shows how SETing $ECODE=.. makes $STACK() reports current information. Notice how ^do dstacktst(0) and ^dostacktst(1) without clearing $ECODE in between displays information frozen at the time of the first error (else condition).

--------------------
$Text()
--------------------

Returns source text for the line specified by its argument.

The format for the $TEXT function is:

.. parsed-literal::
   $T[EXT](entryref)

* The entryref specifies the label, offset, and routine (or trigger name) of the source line that $TEXT() returns.
* If the label+offset combination do not fall within the routine, $TEXT returns a null string.
* If the entryref explicitly or implicitly specifies an offset of zero (0) from the beginning of the routine (or trigger name), $TEXT() returns the routine name or trigger name.
* If the entryref does not specify a routine/trigger, YottaDB/GT.M assumes the current routine/trigger, that is, the routine/trigger at the top of a ZSHOW "S."
* A YottaDB/GT.M extension to $TEXT() permits negative offsets; however, every offset must still be preceded by a plus sign (+) delimiter, (for example, LABEL+-3). If a negative offset points to a line prior to the zero line, $TEXT() generates a run-time error.

$TEXT() provides a tool for examining routine source code and the name of the current routine or trigger. $TEXT() assists, along with the ZPRINT command, in debugging programs. $TEXT() also allows the insertion of small tables of driver information into a routine. Because $TEXT() is not very efficient and the table-driven technique is generally best suited to minimal program changes, this approach is best used for prototyping and the tables should reside in global variables for production.

If $TEXT() cannot access the source file for the current object, either because it is not in the location from which it was compiled or because the process does not have access to some piece of the path to the source, or if the located source does not match the object currently in use by the process, $TEXT() returns an empty string.

++++++++++++++++++++++
Examples of $TEXT()
++++++++++++++++++++++

Example:

.. parsed-literal::
   for i=1:1 set x=$text(+i) quit:x=""  write !,x

This loop uses $TEXT() to write out the entire source for the current routine.

Example:

.. parsed-literal::
   GTM>write $text(+0)
   GTM$DMOD
   GTM>write $text(+1)
   GTM>

This uses $TEXT() to WRITE the name of the current routine, then it tries to access the source and returns an empty string. This occurs because the default Direct Mode image is compiled by YottaDB/FIS and delivered without source. The exact failure message may vary.



