.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2017-2022 YottaDB LLC and/or its subsidiaries.#
.. # All rights reserved.                                        #
.. #                                                             #
.. #     This document contains the intellectual property        #
.. #     of its copyright holder(s), and is made available       #
.. #     under a license.  If you do not know the terms of       #
.. #     the license, please stop and do not read further.       #
.. #                                                             #
.. ###############################################################

.. index::
   Language Features

==================================
5. General Language Features of M
==================================

.. contents::
   :depth: 5

This chapter describes general features of the M language, as well as general information about the operation of YottaDB. Commands, Functions, and Intrinsic Special Variables are each described in separate chapters. This chapter contains information about exceptions, as well as information about general M features.

M is a general purpose language with an embedded database system. This section describes the features of the language that are not covered in the `Commands <./commands.html>`_, `Functions <./functions.html>`_, or `Intrinsic Special Variables <./isv.html>`_ chapters.

---------------------------
Data Types
---------------------------

M operates with a single basic data type, string. However, M evaluates data using methods that vary according to context.

++++++++++++++++++++
Numeric Expressions
++++++++++++++++++++

When M syntax specifies a numexpr, M evaluates the data as a sequence of ASCII characters that specify a number. M stops the evaluation and provides the result generated from successfully evaluated characters when it encounters any character that is not the following:

* A digit 0-9
* A plus sign (+) or minus sign (-) and also the first character in the string
* The first decimal point (.) in the string

+++++++++++++++++++
Numeric Accuracy
+++++++++++++++++++

YottaDB provides 18 digits of accuracy, independent of the decimal point (.) placement, and a numeric range from 10**(-43) to (10**47). Numbers with three digits or fewer to the right of the decimal point are precise.

++++++++++++++++++++
Integer Expressions
++++++++++++++++++++

When M syntax specifies an intexpr, M evaluates the data as it would a numexpr except that it stops the evaluation at any decimal point including the first.

++++++++++++++++++++++++
Truth Valued Expressions
++++++++++++++++++++++++

When M syntax specifies a tvexpr, M evaluates the data as a numeric. However, it stops the evaluation and returns a true value (1) as soon as it encounters a non-zero digit, otherwise it returns a false value (0). In other words, M treats expressions that have a non-zero numeric value as true, and expressions that have a zero numeric value as false. The sign and/or decimal have no affect on the evaluation of a truth-valued expression.

------------------------------
M Names
------------------------------

M uses names for variables, LOCK command arguments, labels on lines, and routine names. M names are alphanumeric and must start with an alphabetic character or a percent sign (%).

The percent sign can only appear as the first character in a name. By convention, names starting with percent signs are generally application-independent or distinguished in some similar way.

M does not reserve any names. That is, M always distinguishes keywords by context. Therefore, M permits a variable or a label called SET even though the language has a command called SET.

M names are case sensitive. That is, M treats ABC, Abc, ABc, AbC ABC, and abc as six different names.

M does not restrict the length of names in the main body of the standard. However, the portability section of the standard recommends limiting names to a maximum of eight (8) characters. YottaDB's limit of 31 characters applies to:

* Local variable names
* Global variable names
* Routine names
* Source and object file names (not including the extension)
* Label names
* Local lock resource names
* Global lock resource names

A trigger name is up to 28 characters and a replication instance name is up to 15 characters.

--------------------------------
Variables
--------------------------------

M does not require predefinition of variable type or size. M variables are either local or global. Any variable may be unsubscripted or subscripted.

+++++++++++++++++++++++++
Arrays and Subscripts
+++++++++++++++++++++++++

In M, subscripted variables identify elements in sparse arrays. Sparse arrays comprise existing subscripts and data nodes -; no space is reserved for potential data nodes. These arrays generally serve logical, rather than mathematical, purposes.

M array subscripts are expressions, and are not restricted to numeric values.

The format for an M global or local variable is:

.. code-block:: none

   [^]name[(expr1[,...])]

* The optional leading caret symbol (^) designates a global variable.
* The name specifies a particular array.
* The optional expressions specify the subscripts and must be enclosed in parentheses and separated by commas (,).

Although there is no restriction on variable names in source code, the first 31 characters of a variable name are significant and subsequent characters are dropped internally. A variable can have up to 31 subscripts. The maximum size of a variable name and all its subscripts is `1,019 bytes <../AdminOpsGuide/gde.html#guidelines-for-mapping>`_.  As this limit is defined by the `internal representation <../AdminOpsGuide/gds.html#gds-keys>`_, it is not easily translated to a specific limit; however, in practice it appears to suffice for most applications. The value of a node can be 1MiB.

.. note:: As global variables that start with :code:`^%Y` are used by the
	  :ref:`ygblstat-util`
	  utility program, and global variables that start with
	  :code:`^%y` are reserved for use by YottaDB,
	  applications should not use them.

++++++++++++++++++++++++++
M Collation Sequences
++++++++++++++++++++++++++

M collates all canonic numeric subscripts ahead of all string subscripts, including strings such as those with leading zeros that represent non-canonic numbers. Numeric subscripts collate from negative to positive in value order. String subscripts collate in ASCII sequence. In addition, YottaDB allows the empty string subscript in most contexts (the null, or empty, string collates ahead of all canonic numeric subscripts).

YottaDB allows definition of alternative collation sequences. For complete information on enabling this functionality, See `Chapter 12: “Internationalization” <./internatn.html>`_.

++++++++++++++++++++++++++++++
Null Subscripts and Collation
++++++++++++++++++++++++++++++

~~~~~~~~~~~~~~~~
Null Subscripts
~~~~~~~~~~~~~~~~

YottaDB has an option to have databases where existing nodes with null subscripts are accepted, but updates with null subscripts (except Kill) are not allowed. At the same time, an option at database creation time exists to collate null subscripts before numeric and string subscripts, as specified by the M standard.

The NULL SUBSCRIPTS database file header field has the values TRUE, FALSE, ALWAYS (synonymous with TRUE, which is deprecated but continues to be supported), NEVER (synonymous with FALSE, which is also deprecated and also continues to be supported) and EXISTING. Please note that TRUE and ALWAYS are internally the same, as are FALSE and NEVER. This means that GDE and DSE will only display the values as ALWAYS and NEVER.

For any region for which the NULL SUBSCRIPTS field has the value EXISTING:

* SETs to nodes to that region behave as if the value of the file header field is NEVER/FALSE.

  * A SET operation to a global in that region where any subscript of the global is null generates a runtime error.
  * A MERGE operation into a global in that region terminates with an error if and when any subscript of any node being set is null.

* Reads (e.g., $GET) from nodes with null subscripts

  * If the node has a value, that value is returned. If the value does not exist, a null is returned if VIEW "NOUNDEF" is set, or a runtime error is generated if VIEW "UNDEF" is set.

* Removal of nodes with null subscripts behaves as if the value of the file header field is ALWAYS/TRUE.

  * A KILL or ZWITHDRAW operation of a global in that region is permitted, either a direct KILL/ZWITHDRAW of a global any subscript of which is null, or a KILL of a higher-level sub-tree in which one or more nodes have subscripts that are null.

The DSE FILEHEADER qualifier NULL_SUBSCRIPTS permits the new keywords for values in the NULL_SUBSCRIPTS field. Also, the DSE DUMP command displays the new keywords in the file header output.

The GDE REGION qualifier NULL_SUBSCRIPTS accepts the keywords ALWAYS, NEVER and EXISTING. The existing argument-less qualifiers NULL_SUBSCRIPTS (synonymous with NULL_SUBSCRIPTS=ALWAYS) and NONULL_SUBSCRIPTS (synonymous with -NULL_SUBSCRIPTS=NEVER) are deprecated but continue to be supported. Also, the GDE SHOW command displays the new keywords in the “Null Subs” column.

MUPIP CREATE creates database files with the new values for NULL_SUBSCRIPTS.

.. _null-subs-colltn:

~~~~~~~~~~~~~~~~~~~~~~~~
Null Subscript Collation
~~~~~~~~~~~~~~~~~~~~~~~~

The default collation (“standard null collation”) of local and global variable subscripts is that the null subscript collates first, followed by numeric subscripts in numeric order, and finally string subscripts in lexical order. YottaDB also supports a historical collation of null subscripts, between numeric subscripts and string subscripts. For global variables, the collation method must be specified at the time of database creation.

A read-only boolean parameter STDNULLCOLL in the database fileheader specifies the type of null collation:

* If STDNULLCOLL is TRUE, subscripts of globals in the database file place the null subscript before all other subscripts.
* If STDNULLCOLL is set to FALSE, subscripts of globals in the database file place the null subscript between numeric and string subscripts.

When `MUPIP CREATE <../AdminOpsGuide/dbmgmt.html#mupip-create>`_ creates a database file, it initializes the STDNULLCOLL parameter to the collation specified for that region in the global directory.

To establish the null collation method for a specified database, GDE supports a region parameter STDNULLCOLL that can be set to TRUE or FALSE using a region qualifier -STDNULLCOLL or -NOSTDNULLCOLL respectively. These qualifiers are supported with ADD, CHANGE and TEMPLATE commands. When MUPIP creates a new database, the STDNULLCOLL value is copied from the global directory into the database file header.

For M local variables, the null collation can be established either at startup or during run time. Since the same local collation method is established for all locals in a process, changing the null collation within the process is allowed only if there are no local variables defined at that time. At process startup, YottaDB uses the following:

* Standard null collation if the environment variable :code:`ydb_lct_stdnull` is undefined, set to either TRUE or YES (or a case-insensitive leading substring thereof), or a non-zero integer.
* Historical null collation if the environment variable :code:`ydb_lct_stdnull` is set to either FALSE or NO (or a case-insensitive leading substring thereof) or 0.

To establish a default collation version for local variables within the process, the percent utility %LCLCOL supports establishing the null collation method as well. set^%LCLCOL(col,ncol) accepts an optional parameter ncol that determines the null collation type to be used with the collation type col.

* If the truth value of ncol is TRUE(1), local variables use standard null collation.
* If the truth value of ncol is FALSE(0), local variables use historical null collation.
* If ncol is not supplied, there is no change to the already established null collation method.

Also using set^%LCLCOL(,ncol), the null collation order can be changed while keeping the alternate collation order unchanged. If subscripted local variables exist, null collation order cannot be changed. In this case, YottaDB issues YDB-E-COLLDATAEXISTS.

~~~~~~
GDE
~~~~~~

The -REGION qualifier –[NO]NULL_SUBCRIPTS accepts new values with change, add and template commands, default is –NONULL_SUBSCRIPTS, e.g.:

.. code-block:: bash

   GDE>add –region areg –dyn=aseg –null_subscripts=always
   GDE>change –region areg –null_subscripts=true
   GDE>change –region areg –null_subscripts=false
   GDE>change –region areg –null_subscripts=never
   GDE>change –region areg –null_subscripts=existing
   GDE>template –region –null_subscripts=existing
   GDE>template –region –nonull_subscripts

The other region qualifier is –[NO]STDNULLCOLL with add, change and template command, default is –NOSTDNULLCOLL.

.. code-block:: bash

   GDE> template -region -stdnullcoll
   GDE> change -region DEFAULT -stdnullcoll
   GDE> add -segment TEAGLOBALS -file=TEAGLOBALS.dat
   GDE> add -region TEAGLOBALS -dyn=TEAGLOBALS -null_subscripts=existing
   GDE> add -name LapsangSouchong -region=TEAGLOBALS
   GDE> add -name Darjeeling -region=TEAGLOBALS
   GDE> add -name Tea* -region=TEAGLOBALS
   GDE> show -all

                             *** Templates ***

   Region                             Def Coll     Rec Size    Key Size    Null Subs    Standard Null Coll   Journaling
   -----------------------------------------------------------------------------------------------------------------------
   <default>                             0          256          64        NEVER               Y                 N


   Segment             Active             Acc      Typ    Block                Alloc    Exten     Options
   ---------------------------------------------------------------------------------------------------------
   <default>             *                BG       DYN    1024                 100       100      GLOB=1024
                                                                                                  LOCK=40
   <default>                              MM       DYN    1024                 100       100      DEFER
                                                                                                  LOCK=40

                         *** Names ***

   Global                         Region
   ---------------------------------------
   *                            DEFAULT
   Darjeeling                   TEAGLOBALS
   LapsangSouchong              TEAGLOBALS
   Tea*                         TEAGLOBALS

                              *** REGIONS  ***

   Region                  Dynamic Segment         Def Coll   Rec Size   Key Size   Null Subs  Standard Null Coll   Journaling
   -----------------------------------------------------------------------------------------------------------------------------
   DEFAULT                   DEFAULT                 0          256         64        NEVER             Y                N
   TEAGLOBALS                TEAGLOBALS              0          256         64        EXISTING          Y                N


                              *** SEGMENTS ***

   Segment             File (def ext: .dat)     Acc  Typ   Block            Alloc   Exten     Options
   -----------------------------------------------------------------------------------------------------
   DEFAULT               yottadb.dat              BG   DYN   1024              100     100      GLOB=1024
                                                                                              LOCK=40
                                                                                              RES=0
   TEAGLOBALS            TEAGLOBALS.dat         BG   DYN   1024              100     100      GLOB=1024
                                                                                              LOCK=40
                                                                                              RES=0

                                 *** MAP ***

   ---------------------------------- Names --------------------------------------------------

   From                          Upto                      Region/Segment/File (def ext: .dat)
   ---------------------------------------------------------------------------------------------
    %                          Darjeeling                     REG=DEFAULT
                                                              SEG=DEFAULT
                                                              FILE=yottadb.dat
   Darjeeling                  Darjeeling0                    REG=TEAGLOBALS
                                                              SEG=TEAGLOBALS
                                                              FILE=TEAGLOBALS.dat
   Darjeeling0                 LapsangSouchong                REG=DEFAULT
                                                              SEG=DEFAULT
                                                              FILE=yottadb.dat
   LapsangSouchong             LapsangSouchong0               REG=TEAGLOBALS
                                                              SEG=TEAGLOBALS
                                                              FILE=TEAGLOBALS.dat
   LapsangSouchong0            Tea                            REG=DEFAULT
                                                              SEG=DEFAULT
                                                              FILE=yottadb.dat
   Tea                         Teb                            REG=TEAGLOBALS
                                                              SEG=TEAGLOBALS
                                                              FILE=TEAGLOBALS.dat
   Teb                         ...                            REG=DEFAULT
                                                              SEG=DEFAULT
                                                              FILE=yottadb.dat
   LOCAL LOCKS                                                REG=DEFAULT
                                                              SEG=DEFAULT
                                                              FILE=yottadb.dat
   GDE>

~~~~~~
DSE
~~~~~~

The -null_subscripts qualifier accepts never, always and existing. The default qualifier is never.

.. note::
   The null subscript collation order cannot be changed using DSE.

dump –fileheader output reflects this for null_subscripts as well as null collation order.

For a region, “Standard Null Collation” in DSE dump output corresponds to -stdnullcoll field in .gld file. DSE displays TRUE for “Standard Null Collation” if the region has –STDNULLCOLL, otherwise it displays FALSE.

From the example above, the output of dump –fileheader for TEAGLOBALS.dat will be as follows:

.. code-block:: bash

   DSE> dump -fileheader

   File            /tmp/yottadb.dat
   Region          DEFAULT
   Date/Time       19-FEB-2018 18:51:43 [$H = 60039,67903]
   Access method                   BG        Global Buffers                1024
   Reserved Bytes                   0        Block size (in bytes)         4096
   Maximum record size           4088        Starting VBN                    49
   Maximum key size               255        Total blocks            0x00000065
   Null subscripts           EXISTING        Free blocks             0x00000049
   Standard Null Collation      FALSE
   Last Record Backup      0x00000001        Extension Count                100
   Last Database Bckup     0x00000001        Number of local maps             1
   Last Bytestream Bckup   0x00000001        Lock space              0x00000028
   In critical section     0x00000000        Timers pending                   0
   Cache freeze id         0x00000000        Flush timer            00:00:01:00
   Freeze match            0x00000000        Flush trigger                  960
   Current transaction     0x000007CE        No. of writes/flush              7
   Create in progress           FALSE        Modified cache blocks            0
   Reference count                  1        Wait Disk                        0
   Journal State        [inactive] ON        Journal Before imaging        TRUE
   Journal Allocation             100        Journal Extension              100
   Journal Buffer Size           1000        Journal Alignsize              128
   Journal AutoSwitchLimit    8388600        Journal Epoch Interval         300
   Journal Yield Limit              8        Journal Sync IO              FALSE
   Journal File: /tmp/yottadb.mjl
   Mutex Hard Spin Count          128        Mutex Sleep Spin Count         128
   Mutex Spin Sleep Time         2048        KILLs in progress                0
   Replication State              OFF        Region Seqno    0x0000000000000001
   Resync Seqno    0x0000000000000001        Resync transaction      0x00000001

With Standard null collation, the null subscript is represented by 0x01 instead of 0xFF with historical null collation. So, the output of dse dump -block for a null subscript will also be different.

.. code-block:: bash

   DSE>dump -block=3
      File /testarea1/null_subs/yottadb.dat
      Region DEFAULT

      Block     3       Size    24  Level   0   TN  3
      Rec:1  Blk 3  Off 8  Size A  Cmpc 0  Key ^a("")
               8 : | 0  A  0  0 61  0  1  0  0 31              |
                   |  .  .  .  .  a  .  .  .  . 1              |

With historical null collation, for the same command output will be as follows:

.. code-block:: bash

   DSE>dump -block=3
       File /testarea1/null_subs/yottadb.dat
       Region DEFAULT

              3   Size 24   Level 0   TN 3
       Rec:1  Blk 3  Off 8  Size A  Cmpc 0  Key ^a("")
                8 : |  0  A  0  0 61  0  FF  0  0 31             |
                    |  .  .  .  .  a  .  .  .  .   1             |

~~~~~~~~~~~~~~~~~~~~~
M Commands/Functions
~~~~~~~~~~~~~~~~~~~~~

^^^^^^^
ZWRITE
^^^^^^^

Since with standard collation, null subscripts collate before numeric and string subscripts, ZWR output will be different if nodes with null subscripts exist.

.. code-block:: bash

   YDB>ZWR
   lcl("")=2
   lcl(1)=3
   lcl("x")=4

With the same data and historical null collation, the output of ZWR will be as follows:

.. code-block:: bash

   lcl(1)=3
   lcl("")=2
   lcl("x")=4

^^^^^^^^^
$ORDER()
^^^^^^^^^

If the last subscript in the subscripted global or local variable name passed as a parameter to $ORDER() is null and a subscripted global or local variable with a null subscript exists, $ORDER() returns the next node at the specified level.

If the last subscript in the subscripted global or local variable name passed as a parameter to $ORDER() is null and a subscripted global or local variable with a null subscript does not exist, $ORDER() returns the first node at the specified level.

If the last subscript in the subscripted global or local variable name is null and second argument of $ORDER() is -1, $ORDER() will always return the last node at the specified level regardless of the existence of a subscripted global or local variable (with null subscript). This allows the user to traverse all the nodes in a specified level starting from the last.

.. code-block:: bash

   YDB>ZWRITE
   lcl(1)=3
   lcl("x")=4

   YDB>WRITE $ORDER(lcl(""))
   1

   YDB>WRITE $ORDER(lcl(1))
   x

   YDB>WRITE $ORDER(lcl(""),-1)
   x

   YDB>SET lcl("")=2
   YDB>ZWRITE
   lcl("")=2
   lcl(1)=3
   lcl("x")=4

   YDB>WRITE $ORDER(lcl(""))
   1

   YDB>WRITE $ORDER(lcl(""),-1)
   x

   YDB>WRITE $ORDER(lcl("x"),-1)
   1

^^^^^^^^^^^^^
$ZPREVIOUS()
^^^^^^^^^^^^^

It is equivalent to $ORDER() with second argument -1.

^^^^^^^^^
$QUERY()
^^^^^^^^^

With stdnullcoll, if $D(glvn(""))=1 (or 11), $Q(glvn("")) will return glvn(1) [assuming glvn(1) exists]. Software should execute $D(glvn("")) to test the existence of glvn(""). $Q(glvn("...")) will never return the starting-point (glvn("")) even though glvn("") may exist.

.. code-block:: bash

   YDB>ZWRITE lcl
   lcl("")=1
   lcl(1)=1
   lcl(1,2)=2
   lcl(1,2,"")=3
   lcl(1,2,"","")=4
   lcl(1,2,"","",4)=5
   lcl(1,2,0)=6
   lcl(1,2,"abc",5)=7
   lcl("x")=1

   YDB>SET y="x”

   YDB>FOR SET y=$QUERY(@y) QUIT:y="" WRITE !,y,"=",@y

The output will be the same as the ZWRITE output.

For more details about the behavior of these functions with historical null collation, please consult the `M Programmer’s Guide <./index.html>`_.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
MUPIP Binary Extract and Load
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* MUPIP EXTRACT BINARY issues NULLCOLLDIFF error if it needs to extract from multiple databases with different STDNULCOLL settings.
* MUPIP EXTRACT BINARY writes a new field in the binary extract header to note down the first database's STDNULCOLL setting.
* MUPIP LOAD BINARY on a binary extract transforms the null subscripts appropriately if the STDNULCOLL setting of the target database is different from the setting in the binary extract header.
* MUPIP LOAD BINARY is able to successfully load onto multiple databases with different STDNULCOLL settings.
* MUPIP EXTRACT ZWR and MUPIP LOAD ZWR will work no matter what the YottaDB version of the source and destination databases, and no matter what the null (or other) collation setting of the source and destination databases.

~~~~~~~~~~~~~~~~~~
Replication
~~~~~~~~~~~~~~~~~~

In a replicated environment, all databases belonging to an instance should have the same null collation order. If this condition is not met, the source server issues the YDB-E-NULLCOLLDIFF error message on the primary. On the secondary, the update process issues the same error message if the condition is not satisfied.

Although all databases belonging to an instance must have the same collation method, YottaDB allows the primary and secondary to use different null collation methods. Any needed conversion is handled internally and transparently.

+++++++++++++++++++++++++
Local Variables
+++++++++++++++++++++++++

A local variable in M refers to a variable used solely within the scope of a single process. Local variable names have no leading delimiter.

M makes a local variable available and subject to modification by all routines executed within a process from the time that variable is first SET until it is KILLed, or until the process stops executing M. However, M "protects" a local variable after that variable appears as an argument to a NEW command, or after it appears as an element in a formallist used in parameter passing. When M protects a local variable, it saves a copy of the variable's value and makes that variable undefined. M restores the variable to its saved value during execution of the QUIT that terminates the process stack level associated with the "protecting" NEW or formallist. For more information on NEW and QUIT, see `Chapter 6: “Commands” <./commands.html>`_.

M restricts the following uses of variables to local variables:

* FOR command control variables.
* Elements within the parentheses of an "exclusive" KILL.
* TSTART [with local variables list].
* A KILL with no arguments removes all current local variables.
* NEW command arguments.
* Actualnames used by pass-by-reference parameter passing.

++++++++++++++++++++++++++++++++++++++++++++++++
Global Variables and Resource Name Environments
++++++++++++++++++++++++++++++++++++++++++++++++

M recognizes an optional environment specification in global names or in the LOCK resource names (nrefs), which have analogous syntax. Global variable names have a leading caret symbol (^) as a delimiter.

M makes a global variable available, and subject to modification by all routines executed within all processes in an environment, from the time that variable is first SET until it is KILLed.

++++++++++++++++++++++++++++
Naked References
++++++++++++++++++++++++++++

M accepts an abbreviation of the global name under some circumstances. When the leading caret symbol (^) immediately precedes the left parenthesis delimiting subscripts, the global variable reference is called a naked reference. M evaluates a naked reference by prefixing the last used global variable name, except for its last subscript, to the list of subscripts specified by the naked reference. The prefixed portion is known as the naked indicator. An attempt to use a naked reference when the prior global reference does not exist, or did not contain a subscript, generates an error.

Because M has only one process-wide naked indicator which it maintains as a side affect of every evaluation of a global variable, using the naked reference requires an understanding of M execution sequence. M execution generally proceeds from left to right within a line, subject to commands that change the flow of control. However, M evaluates the portion of a SET command argument to the right side of the equal sign before the left side. Also, M does not evaluate any further $SELECT() arguments within the function after it encounters a true selection argument.

In general, using naked references only in very limited circumstances prevents problems associated with the naked indicator.

+++++++++++++++++++++++++++++++++
Global Variable Name Environments
+++++++++++++++++++++++++++++++++

M recognizes an optional environment specification in global names. The environment specification designates one of some set of alternative database files.

The syntax for global variable names that include an environment specification is:

.. code-block:: none

   ^|expr|name[(subscript[,...])]

In YottaDB, the expression identifies the Global Directory for mapping the global variable.

Environment specifications permit easy access to global variables in alternative databases, including other "copies" of active variables in the current database. Environment specifications are sometimes referred to as extended global syntax or extended value syntax.

YottaDB also allows:

.. code-block:: none

   ^|expr1,expr2|name[(subscript[,...])]

Where the first expression identifies the Global Directory and the second expression is accepted but ignored by YottaDB.

To improve compatibility with some other M implementations, YottaDB also accepts another non-standard syntax. In this syntax, the leading and trailing up-bar (|) are respectively replaced by a left square-bracket ([) and a right square-bracket (]). This syntax also requires expratoms, rather than expressions. For additional information on expratoms, see :ref:`expressions`.

The formats for this non-standard syntax are:

.. code-block:: none

   ^[expratom1]name[(subscript...)]

or

.. code-block:: none

   ^[expratom1,expratom2]name[(subscript...)]

Where expratom1 identifies the Global Directory and expratom2 is a dummy variable. Note that the first set of brackets in each format is part of the syntax. The second set of square brackets is part of the meta-language identifying an optional element.

Example:

.. code-block:: bash

   $ ydb_gbldir=Test.GLD
   $ export ydb_gbldir
   $ YDB

   YDB>WRITE $ZGBLDIR
   TEST.GLD
   YDB>WRITE ^A
   THIS IS ^A IN DATABASE RED
   YDB>WRITE ^|"M1.GLD"|A
   THIS IS ^A IN DATABASE WHITE
   YDB>WRITE $ZGBLDIR
   TEST.GLD
   YDB>HALT

   $ echo ydb_gbldir
   TEST.GLD

The statement WRITE ^|"M1.GLD"\|A writes variable ^A using the Global Directory, M1.GLD, but does not change the current Global Directory.

Example:

.. code-block:: bash

   YDB>WRITE $ZGBLDIR
   M1.GLD
   YDB>WRITE ^A
   THIS IS ^A IN DATABASE WHITE
   YDB>WRITE ^|"M1.GLD"|A
   THIS IS ^A IN DATABASE WHITE

The statement WRITE ^|"M1.GLD"\|A is equivalent to WRITE ^A.

Specifying separate Global Directories does not always translate to using separate databases.

Example:

.. code-block:: bash

   YDB>WRITE ^|"M1.GLD"|A,!,^|"M2.GLD"|A,!,^|"M3.GLD"
   |A,!
   THIS IS ^A IN DATABASE WHITE
   THIS IS ^A IN DATABASE BLUE
   THIS IS ^A IN DATABASE WHITE

In this example, the WRITE does not display ^A from three YottaDB database files. Mapping specified by the Global Directory Editor (GDE) determines the database file to which a Global Directory points.

This result could have occurred under the following mapping:

.. code-block:: bash

   ^|"M1.GLD"|A --> REGIONA --> SEGMENTA --> FILE1.DAT
   ^|"M2.GLD"|A --> REGIONA --> SEGMENT1 --> FILE2.DAT
   ^|"M3.GLD"|A --> REGION3 --> SEGMENT3 --> FILE1.DAT

For more information on Global Directories, refer to the `"Global Directory Editor" <../AdminOpsGuide/gde.html>`_ chapter of the Administration and Operations Guide.

.. _opt-ydb-env-xltn-fac:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Optional YottaDB Environment Translation Facility
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To facilitate application migration to YottaDB from other M implementations (for example to convert UCI and VOL specifications to global directories) in the environment specification, YottaDB provides an interface to translate strings to global directory filenames.

.. note::
   Using this facility impacts the performance of every global access that uses environment specification. Make sure you use it only when static determination of the global directory is not feasible. When used, maximize the efficiency of the translation routines.

The use of this facility is enabled by the definition of the environment variable ydb_env_translate, which contains the path of a shared library with the following entry point:

^^^^^^^^^^^^^^
ydb_env_xlate
^^^^^^^^^^^^^^

If the shared object is not accessible or the entry point is not accessible, YottaDB reports an error.

The ydb_env_xlate() routine has the following C prototype:

.. code-block:: C

   int ydb_env_xlate(ydb_string_t *in1, ydb_st
      ring_t *in2, ydb_string *in3, ydb_string_t *out)

where ydb_string_t is a structure defined in libyottadb.h as follows:

.. code-block:: C

   typedef struct
   {
	unsigned long	length;
	char		*address;
   } ydb_string_t;

The purpose of the function is to use its three input arguments to derive and return an output argument that can be used as an environment specification by YottaDB. Note that the input values passed (in1, in2 and in3) are the result of M evaluation and must not be modified. The first two arguments are the expressions passed within the up-bars "| \|" or the square-brackets "[ ]", and the third argument is the current working directory as described by $ZDIRECTORY.

A return value other than zero (0) indicates an error in translation, and is reported by a YottaDB error.

If the length of the output argument is non-zero, YottaDB appends a secondary message of YDB-I-TEXT, containing the text found at the address of the output structure.

YottaDB does not do any memory management related to the output argument - space for the output should be allocated by the external routine. The routine must place the returned environment specification at the address it has allocated and adjust the length accordingly. On a successful return, the return value should be zero. If the translation routine must communicate an error to YottaDB, it must return a non-zero value, and if it is to communicate additional error information, place the error text at the address where the environment would normally go and adjust the length to match the length of the error text.

Length of the return value may range from 0-32767, otherwise YottaDB reports an error.

A zero-length (empty) string specifies the current value of $ZGBLDIR. Non-zero lengths must represent the actual length of the file specification pointed to by the address, excluding any <NUL> terminator. If the address field of the output argument is NULL, YottaDB issues an error.

The file specification may be absolute or relative and may contain an environment variable. If the file specified is not accessible, or is not a valid global directory, YottaDB reports errors in the same way it does for any invalid global directory.

It is possible to write this routine in M (as a call-in), however, global variables in such a routine would change the naked indicator, which environment references normally do not. Depending on the conventions of the application, there might be difficult name-space management issues such as protecting the local variables used by the M routine.

While it is possible for this routine to take any form that the application designer finds appropriate within the given interface definition, the following paragraphs make some recommendations based on the expectation that a routine invoked for any more than a handful of global references should be efficient.

It is expected that the routine loads one or more tables, either at compilation or the first time it is invoked. The logic of the routine performs a look up on the entry in the set of tables. The lookup might be based on the length of the strings and some unique set of characters in the names, or a hash, with collision provisions as appropriate.

The routine may have to deal with a case where one or both of the inputs have zero length. A subset of these cases may have the first string holding a comma limited string that needs to be re-interpreted as being equivalent to two input strings (note that the input strings must never be modified). The routine may also have to handle cases where a value (most likely the first) is accidentally or intentionally, already a global directory specification.

Example:

.. code-block:: bash

   $ cat ydb_env_xlate.c
   #include <stdio.h>
   #include <string.h>
   #include "libyottadb.h"
   static int init = 0;
   typedef struct
   {
     ydb_string_t field1, field2, ret;
   } line_entry ;
   static line_entry table[5], *line, linetmp;
   /* Since these errors may occur before setup is complete, they are statics */
   static char *errorstring1 ="Error in function initialization, environment variable GTM_CALLIN_START not defined. Environment translation failed.";
   static char *errorstring2 ="Error in function initialization, function pointers could not be determined. Environment translation failed.";
   #define ENV_VAR"GTM_CALLIN_START"
   typedef int(*int_fptr)();
   int_fptr GTM_MALLOC;
   int init_functable(ydb_string_t *ptr)
   {
   /* This function demonstrates the initialization of other function pointers as well (if the user-code needs them for any reason, they should be defined as globals) */
   char *pcAddress;
   long lAddress;
   void **functable;
   void (*setup_timer) ();
   void (*cancel_timer) ();
   pcAddress = getenv(ENV_VAR);
   if (pcAddress == NULL)
   {
   ptr->length = strlen(errorstring1);
   ptr->address = errorstring1;
   return 1;
   }
   lAddress = -1;
   lAddress = atol(pcAddress);
   if (lAddress == -1)
   {
   ptr->length = strlen(errorstring2);
   ptr->address = errorstring2;
   return 1;
   }
   functable = (void *)lAddress;
   setup_timer = (void(*)()) functable[2];
   cancel_timer = (void(*)()) functable[3];
   GTM_MALLOC = (int_fptr) functable[4];
   return 0;
   }
   void copy_string(char **loc1, char *loc2, int length)
   {
   char *ptr;
   ptr = (char *) ydb_malloc(length);
   strncpy( ptr, loc2, length);
   *loc1 = ptr;
   }
   int init_table(ydb_string_t *ptr)
   {
   int i = 0;
   char buf[100];
   char *buf1, *buf2;
   FILE *tablefile;
   char *space = " ";
   char *errorstr1 = "Error opening table file table.dat";
   char *errorstr2 = "UNDETERMINED ERROR FROM GTM_ENV_XLATE";
   if ((tablefile = fopen("table.dat","r")) == (FILE *)NULL)
   {
   ptr->length = strlen(errorstr1);
   copy_string(&(ptr->address), errorstr1, strlen(errorstr1));
   return 1;
   }
   while (fgets(buf, (int)sizeof(buf), tablefile) != (char *)NULL)
   {
   line= &table[i++];
   buf1 = buf;
   buf2 =strstr(buf1, space);
   line->field1.length = buf2 - buf1;
   copy_string( &(line->field1.address), buf1, line->field1.length);
   buf1 = buf2+1;
   buf2 = strstr(buf1, space);
   line->field2.length = buf2-buf1;
   copy_string( &(line->field2.address), buf1, line->field2.length);
   buf1 = buf2+1;
   line->ret.length = strlen(buf1) - 1;
   copy_string( &(line->ret.address), buf1, line->ret.length);
   }
   fclose(tablefile);
   /* In this example, the last entry in the table is the error string */
   line = &table[4];
   copy_string( &(line->ret.address), errorstr2, strlen(errorstr2));
   line->ret.length = strlen(errorstr2);
   return 0;
   }
   int cmp_string(ydb_string_t str1, ydb_string_t str2)
   {
   if (str1.length == str2.length)
   return strncmp(str1.address, str2.address, (int) str1.length);
   else
   return str1.length - str2.length;
   }
   int cmp_line(line_entry *line1, line_entry *line2)
   {
   return (((cmp_string(line1->field1, line2->field1))||(cmp_string(line1->field2, line2->field2))));
   }
   int look_up_table(line_entry *aline, ydb_string_t *ret_ptr)
   {
   int i;
   int ret_v;
   for(i=0;i<4;i++)
   {
   line = &table[i];
   ret_v = cmp_line( aline, line);
   if (!ret_v)
   {
   ret_ptr->length = line->ret.length;
   ret_ptr->address = line->ret.address;
   return 0;
   }
   }
   /*ERROR OUT*/
   line = &table[4];
   ret_ptr->length= line->ret.length;
   ret_ptr->address = line->ret.address;
   return 1;
   }
   int ydb_env_xlate(ydb_string_t *ptr1, ydb_string_t *ptr2, ydb_string_t *ptr_zdir, ydb_string_t *ret_ptr)
   {
   int return_val, return_val_init;
   if (!init)
   {
   return_val_init = init_functable(ret_ptr);
   if (return_val_init) return return_val_init;
   return_val_init = init_table(ret_ptr);
   if (return_val_init) return return_val_init;
   init = 1;
   }
   linetmp.field1.length= ptr1->length;
   linetmp.field1.address= ptr1->address;
   linetmp.field2.length= ptr2->length;
   linetmp.field2.address= ptr2->address;
   return_val = look_up_table(&linetmp, ret_ptr);
   return return_val;
   }
   > cat table.dat
   day1 week1 yottadb
   day2 week1 a
   day3 week2 b
   day4 week2 c.gld

This example demonstrates the mechanism. A table is set up the first time for proper memory management, and for each reference, a table lookup is performed. Note that for the purpose of simplicity, no error checking is done, so table.dat is assumed to be in the correct format, and have exactly four entries. This routine should be built as a shared library, see `Chapter 11: “Integrating External Routines” <./extrout.html>`_ for information on building as a shared library. The function init_functable is necessary to set up the YottaDB memory management functions.

----------------------------
Literals
----------------------------

M has both string and numeric literals.

+++++++++++++++++++++++++
String Literals
+++++++++++++++++++++++++

A string literal (strlit) is enclosed in quotation marks (" ") and can contain a sequence of ASCII and Unicode® UTF-8 characters. While the standard indicates the characters must be graphic, YottaDB accepts non-graphic characters and, at compile-time, gives a warning. Using $CHAR() and concatenate to represent non-graphic characters in strings not only avoids the warning but is less error prone and makes for easier understanding. M attempts to use character text that appears outside of quotation mark delimiters according to context, which generally means as a local variable name.

To include a quotation mark (") within a strlit, use a set of two quotation marks ("" "").

Example:

.. code-block:: bash

   YDB>write """"
   "
   YDB>

The WRITE displays a single quotation mark because the first quotation mark delimits the beginning of the string literal, the next two quotation marks denote a single quote within the string, and the last quotation mark delimits the end of the string literal.

Use the $CHAR function and the concatenation operator to include control characters within a string.

Example:

.. code-block:: bash

   YDB>WRITE "A"_$CHAR(9)_"B"
   A B
   YDB>

The WRITE displays an "A," followed by a tab (<HT>), followed by a "B" using $CHAR(), to introduce the non-graphic character.

+++++++++++++++++++++++++++
Numeric Literals
+++++++++++++++++++++++++++

In M, numeric literals (numlit) are entered without surrounding delimiters.

Example:

.. code-block:: bash

   YDB>WRITE 1
   1
   YDB> WRITE 1.1
   1.1

These display numeric literals that are integer and decimal.

M also accepts numeric literals in the form of a mantissa and an exponent, separated by a delimiter of "E" in uppercase. The mantissa may be an integer or a decimal fraction. The integer exponent may have an optional leading minus sign (-).

Example:

.. code-block:: bash

   YDB>WRITE 8E6
   8000000
   YDB> WRITE 8E-6
   .000008
   YDB>

.. note::
   The exponential numeric form may lead to ambiguities in the meaning of subscripts. Because numeric subscripts collate ahead of string subscripts, the string subscript "01E5" is not the same as the numeric subscript 01E5.

YottaDB handles numeric strings which are not canonical within the implementation as strings unless the application specifically requests they be treated as numbers. Any use in a context defined as numeric elicits numeric treatment; this includes operands of numeric operators, numeric literals, and some intrinsic function arguments. When the code creates a large number out of range, YottaDB gives a NUMOFLOW error. When the code creates a small fractional number out of range YottaDB treats it as zero (0). The YottaDB number range is (to the limit of accuracy) 1E-43 to 1E47. When the application creates an in-range number that exceeds the YottaDB numeric accuracy of 18 significant digits, YottaDB silently retains the most significant digits. With standard collation, YottaDB collates canonic numeric strings used as subscripts numerically, while it collates non-canonic numbers as strings.

.. _expressions:

----------------------------
Expressions
----------------------------

The following items are legal M expression atoms (expratoms). An expression atom is a component of an M expression.

* Local variables
* Global variables
* Intrinsic special variables
* Intrinsic functions
* Extrinsic functions
* Extrinsic special variables
* Numeric literals
* String literals
* An expression enclosed in parentheses
* Any of the above preceded by a unary operator

In addition, any of these items may be combined with a binary operator and another expression atom.

-----------------------------
Operators
-----------------------------

M has both unary and binary operators.

.. note::

   :code:`.` is not an operator. Please refer to the :ref:`parameter-passing` sub-section for more information on usage of :code:`.`

+++++++++++++++++++++++
Precedence
+++++++++++++++++++++++

All unary operations have right to left precedence.

All M binary operations have strict left to right precedence. This includes all arithmetic, string, and logical operations. Hierarchies of operations require explicit establishment of precedence using parentheses (). Although this rule is counterintuitive, it is easy to remember and has no exceptions.

.. _arithmetic-ops:

+++++++++++++++++++++++
Arithmetic Operators
+++++++++++++++++++++++

All arithmetic operators force M to evaluate the expressions to which they apply as numeric.

The arithmetic operators are:

+-----------+----------------------------------------------------------------------------------------------------------------------------------------------------------+
| Operator  | Description                                                                                                                                              |
+===========+==========================================================================================================================================================+
| \+        | as a unary operator it simply forces M to evaluate the expression following as numeric; as a binary operator it causes M to perform addition             |
+-----------+----------------------------------------------------------------------------------------------------------------------------------------------------------+
| \-        | as a unary operator it causes M to negate the expression following; as a binary operator it causes M to perform subtraction                              |
+-----------+----------------------------------------------------------------------------------------------------------------------------------------------------------+
| \*        | binary operator for multiplication                                                                                                                       |
+-----------+----------------------------------------------------------------------------------------------------------------------------------------------------------+
| \**       | binary operator for exponentiation                                                                                                                       |
+-----------+----------------------------------------------------------------------------------------------------------------------------------------------------------+
| /         | binary operator for fractional division                                                                                                                  |
+-----------+----------------------------------------------------------------------------------------------------------------------------------------------------------+
| \\        | binary operator for integer division                                                                                                                     |
+-----------+----------------------------------------------------------------------------------------------------------------------------------------------------------+
| #         | binary operator for modulo, that is, causes M to produce the remainder from integer division of the first argument by the second                         |
+-----------+----------------------------------------------------------------------------------------------------------------------------------------------------------+

Because of the practice of using it to intentionally induce an error, YottaDB does not produce a DIVZERO error at compile time, only at run time, for divide or integer divide by a literal expression that evaluates to zero (0).

Remember that precedence is left to right for all arithmetic operators.

Example:

.. code-block:: bash

   YDB>WRITE 1+1
   2
   YDB>WRITE 2-1
   1
   YDB>WRITE 2*2
   4
   YDB>WRITE 3**2
   9
   YDB>WRITE 4/2
   2
   YDB>WRITE 7
   2
   YDB>WRITE 7#3
   1
   YDB>

This simple example demonstrates how each arithmetic binary operation uses numeric literals.

Example:

.. code-block:: bash

   YDB>WRITE +"12ABC"
   12
   YDB>WRITE --"-3-4"
   -3
   YDB>

The first WRITE shows the unary plus sign (+) operation forcing the numeric evaluation of a string literal. The second WRITE demonstrates the unary minus sign (-). Note the second minus sign within the string literal does not cause subtraction, but rather, terminates the numeric evaluation with the result of negative three (-3). Each of the leading minus signs causes one negation and therefore, the result is negative three (-3).

+++++++++++++++++++++++++++++++
Logical Operators
+++++++++++++++++++++++++++++++

M logical operators always produce a result that is TRUE (1) or FALSE (0). All logical operators force M to evaluate the expressions to which they apply as truth-valued.

The logical operators are:

+-----------+----------------------------------------------------------------------------------------------------------------------------------------------------------+
| Operator  | Description                                                                                                                                              |
+===========+==========================================================================================================================================================+
| '         | unary NOT operator negates current truth-value;                                                                                                          |
|           | M accepts placement of the NOT operator next to a relational operator, for example, A'=B meaning '(A=B), i.e., NAND                                      |
|           | and next to logical operators, A'& B meaning '(A & B) i.e. NAND and A'! B meaning '(A ! B) i.e. NOR                                                      |
+-----------+----------------------------------------------------------------------------------------------------------------------------------------------------------+
| &         | binary AND operator produces a true result only if both of the expressions are true                                                                      |
+-----------+----------------------------------------------------------------------------------------------------------------------------------------------------------+
| !         | binary OR operator produces a true result if either of the expressions is true                                                                           |
+-----------+----------------------------------------------------------------------------------------------------------------------------------------------------------+

Remember that precedence is always left to right, and that logical operators have the same precedence as all other operators.

Example:

.. code-block:: none

   YDB>WRITE '0
   1
   YDB>WRITE '1
   0
   YDB>WRITE '5689
   0
   YDB>WRITE '-1
   0
   YDB>WRITE '"ABC"
   1
   YDB>

The above example demonstrates the unary NOT operation. Note that any non-zero numeric value is true and has a false negation.

Example:

.. code-block:: bash

   YDB>WRITE 0&0
   0
   YDB>WRITE 0'&0
   1
   YDB>WRITE 1&0
   0
   YDB>WRITE 0&1
   0
   YDB>WRITE 1&1
   1
   YDB>WRITE 1'&1
   0
   YDB>WRITE 2&1
   1
   YDB>WRITE 0!0
   0
   YDB>WRITE 0'!0
   1
   YDB>WRITE 1!0
   1
   YDB>WRITE 0!1
   1
   YDB>WRITE 1!1
   1
   YDB>WRITE 1'!1
   0
   YDB>WRITE 2!1
   1
   YDB>

The above example demonstrates cases covered by the binary logical operators.

.. _m-string-operators:

+++++++++++++++++++
String Operators
+++++++++++++++++++

All string operators force M to evaluate the expressions to which they apply as strings.

The string operator is:

+-----------+------------------------------------------------------------------------------------------------------------------------+
| Operator  | Description                                                                                                            |
+===========+========================================================================================================================+
| _         | binary operator causes M to concatenate the second expression with the first expression                                |
+-----------+------------------------------------------------------------------------------------------------------------------------+

Example:

.. code-block:: bash

   YDB>WRITE "B"_"A"
   BA
   YDB>WRITE "A"_1
   A1
   YDB>

The above example demonstrates M concatenation.

++++++++++++++++++++++++++++
Numeric Relational Operators
++++++++++++++++++++++++++++

M relational operators always generate a result of TRUE (1) or FALSE (0). All numeric relational operators force M to evaluate the expressions to which they apply as numeric.

The numeric relational operators are:

+-----------+------------------------------------------+
| Operator  | Description                              |
+===========+==========================================+
| >         | binary arithmetic greater than           |
+-----------+------------------------------------------+
| <         | binary arithmetic less than              |
+-----------+------------------------------------------+

The equal sign (=) does not force numeric evaluation, and should be viewed as a string operator. However, the equal sign between two numeric values tests for numeric equality.

Other numeric relations are formed using the logical NOT operator apostrophe (') as follows:

+-----------+----------------------------------------------------------------------+
| Operator  | Description                                                          |
+===========+======================================================================+
| '>        | not greater than, that is, less than or equal to                     |
+-----------+----------------------------------------------------------------------+
| '<        | not less than, that is, greater than or equal to                     |
+-----------+----------------------------------------------------------------------+
| >=        | greater than or equal to, that is, not less than                     |
+-----------+----------------------------------------------------------------------+
| <=        | less than or equal to, that is, not greater than                     |
+-----------+----------------------------------------------------------------------+
| '=        | not equal, numeric or string operation                               |
+-----------+----------------------------------------------------------------------+

Example:

.. code-block:: bash

   YDB>WRITE 1>2
   0
   YDB>WRITE 1<2
   1
   YDB>

The above example demonstrates the basic arithmetic relational operations.

Example:

.. code-block:: bash

   YDB>WRITE 1'<2
   0
   YDB>WRITE 2'<1
   1
   YDB>

The above example demonstrates combinations of arithmetic, relational operators with the logical NOT operator.

++++++++++++++++++++++++++++
String Relational Operators
++++++++++++++++++++++++++++

M relational operators always generate a result of TRUE (1) or FALSE (0). All string relational operators force M to evaluate the expressions to which they apply as strings.

The string relational operators are:

+-----------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Operator  | Description                                                                                                                                                     |
+===========+=================================================================================================================================================================+
| =         | binary operator causes M to produce a TRUE if the expressions are equal                                                                                         |
+-----------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------+
| [         | binary operator causes M to produce a TRUE if the first expression contains the ordered sequence of characters in the second expression                         |
+-----------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------+
| ]         | binary operator causes M to produce a TRUE if the first expression lexically follows the second expression in the character encoding sequence,                  |
|           | which by default is ASCII                                                                                                                                       |
+-----------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------+
| ]]        | binary operator causes M to produce a TRUE if the first expression lexically sorts after the second expression in the subscript collation sequence              |
+-----------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------+

Note that all non-empty strings lexically follow the empty string, and every string contains the empty string.

Other string relations are formed using the logical NOT operator apostrophe (') as follows:

+-----------+----------------------------------------------------------------------------------------------------+
| Operator  | Description                                                                                        |
+===========+====================================================================================================+
| '[        | does not contain                                                                                   |
+-----------+----------------------------------------------------------------------------------------------------+
| ']        | does not follow, that is, lexically less than or equal to                                          |
+-----------+----------------------------------------------------------------------------------------------------+
| ']]       | does not sort after, that is, lexically less than or equal to in the subscript collation sequence  |
+-----------+----------------------------------------------------------------------------------------------------+
| '=        | not equal, numeric or string operation                                                             |
+-----------+----------------------------------------------------------------------------------------------------+

Example:

.. code-block:: bash

   YDB>WRITE "A"="B"
   0
   YDB>WRITE "C"="C"
   1
   YDB>WRITE "A"["B"
   0
   YDB>WRITE "ABC"["C"
   1
   YDB>WRITE "A"]"B"
   0
   YDB>WRITE "B"]"A"
   1
   YDB>WRITE "A"]]"B"
   0
   YDB>WRITE "B"]]"A"
   1

These examples demonstrate the string relational operators using string literals.

Example:

.. code-block:: bash

   YDB>WRITE 2]10
   1
   YDB>WRITE 2]]10
   0
   YDB>WRITE 0]"$"
   1
   YDB>WRITE 0]]"$"
   0

These examples illustrate that when using the primary ASCII character set, the main difference in the "follows" (]) operator and the "sorts-after" (]]) operator is the way they treat numbers.

Example:

.. code-block:: bash

   YDB>WRITE 1=1
   1
   YDB>WRITE 1=2
   0
   YDB>WRITE 1="1"
   1
   YDB>WRITE 1=01
   1
   YDB>WRITE 1="01"
   0
   YDB>WRITE 1=+"01"
   1
   YDB>

These examples illustrate the dual nature of the equal sign operator. If both expressions are string or numeric, the results are straight forward. However, when the expressions are mixed, the native string data type prevails.

Example:

.. code-block:: none

   YDB>WRITE "a"'="A"
   1
   YDB>WRITE "FRED"'["RED"
   0
   YDB>WRITE "ABC"']""
   0

These examples demonstrate combinations of the string relational operators with the NOT operator.

.. _pattern-match-op:

++++++++++++++++++++++++
Pattern Match Operator
++++++++++++++++++++++++

The pattern match operator (?) causes M to return a TRUE if the expression ahead of the operator matches the characteristics described by the pattern following the operator. The pattern is not an expression.

Patterns are made up of two elements:

1. A repetition count
2. A pattern code, a string literal or an alternation list

The element following the pattern match operator may consist of an indirection operator, followed by an element that evaluates to a legitimate pattern.

The repetition count consists of either a single integer literal or a period (.) delimiter with optional leading and trailing integer literals. A single integer literal specifies an exact repetition count. The period syntax specifies a range of repetitions where the leading number is a minimum and the trailing number is a maximum. When the repetition count is missing the leading number, M assumes there is no minimum, (i.e., a minimum of zero). When the repetition count is missing the trailing number, M does not place a maximum on the number of repetitions.

The pattern codes are:

+----------+--------------------------------------------------------------------------------+
| Code     | Description                                                                    |
+==========+================================================================================+
| A        | alphabetic characters upper or lower case                                      |
+----------+--------------------------------------------------------------------------------+
| C        | control characters ASCII 0-31 and 127                                          |
+----------+--------------------------------------------------------------------------------+
| E        | any character; used to pass all characters in portions of the string where the |
|          | pattern is not restricted                                                      |
+----------+--------------------------------------------------------------------------------+
| L        | lower-case alphabetic characters, ASCII 97-122                                 |
+----------+--------------------------------------------------------------------------------+
| N        | digits 0-9, ASCII 48-57                                                        |
+----------+--------------------------------------------------------------------------------+
| P        | punctuation, ASCII 32-47, 58-64, 91-96, 123-126                                |
+----------+--------------------------------------------------------------------------------+
| U        | upper-case alphabetic characters, ASCII 65-90                                  |
+----------+--------------------------------------------------------------------------------+

Pattern codes may be upper or lower case and may be replaced with a string literal. YottaDB allows the M pattern match definition of patcodes A, C, N, U, L, and P to be extended or changed, (A can only be modified implicitly by modifying L or U) and new patcodes added. For detailed information on enabling this functionality, see `Chapter 12: “Internationalization” <./internatn.html>`_.

.. note::
   The YottaDB compiler accepts pattern codes other than those explicitly defined above. If, at run-time, the pattern codes come into use and no pattern definitions are available, YottaDB issues a run-time error (PATNOTFOUND). YottaDB does not currently implement a mechanism for Y and Z patterns and continues to treat those as compile-time syntax errors. YottaDB defers literal optimizations involving patterns within an XECUTE as well as evaluations that encounter issues with the pattern table.

Example:

.. code-block:: bash

   YDB>WRITE "ABC"?3U
   1
   YDB>WRITE "123-45-6789"?3N1"-"2N1"-"4N
   1

The first WRITE has a simple one-element pattern while the second has multiple elements including both codes and string literals. All the repetition counts are fixed.

Example:

.. code-block:: none

   I x?.E1C.E W !,"Must not contain a control character" Q

This example uses a pattern match to test for control characters.

Example:

.. code-block:: none

   I acn?1U.20A1","1U.10A D
   .S acn=$G((^ACX($P(acn,","),$P(acn,",",2)))

This example uses a pattern match with implicit minimums to determine that an "account number" is actually a name, and to trigger a look-up of the corresponding account number in the ^ACX cross index.

The pattern match operator accepts the alteration syntax. Alteration consists of a repeat count followed by a comma-delimited list of patatoms enclosed in parentheses "()". This is to check if any of the listed patterns matches the operand string. For example, ?1(2N1"-"7N,3N1"-"2N1"-"4N).1U might be a way to match either a social security number or a taxpayer ID. Since alteration is defined as one of the ways of constructing a patatom, alteration can nest (be used recursively).

.. note::
   Complex pattern matches may not be efficient to evaluate, so every effort should be made to simplify any commonly used pattern and to determine if more efficient alternative logic would be more appropriate.

--------------------------------
Commands
--------------------------------

M commands may be abbreviated to a defined prefix. Most commands have arguments. However, some commands have either optional arguments or no arguments. When a command has no argument and is followed by more commands on the same line, at least two spaces (<SP>) must follow the command without arguments. Commands that accept arguments generally accept multiple arguments on the same command. M treats multiple arguments the same as multiple occurrences of the same command, each with its own argument.

+++++++++++++++++++++++
Postconditionals
+++++++++++++++++++++++

M provides postconditionals as a tool for placing a condition on the execution of a single command and, in some cases, a single command argument. A postconditional consists of a colon (:) delimiter followed by a truth-valued expression. When the expression evaluates to true, M executes the command occurrence. When the expression evaluates to false, M does not execute the command occurrence.

**Command Postconditionals**

Command postconditionals appear immediately following a command and apply to all arguments for the command when it has multiple arguments. All commands except commands that themselves have a conditional aspect accept a command postconditional. Among the M standard commands, ELSE, FOR, and IF do not accept command postconditionals. All the YottaDB command extensions accept command postconditionals. When a postconditional evaluates to a literal FALSE (0), YottaDB discards the command and its arguments at compile time, which means it does not perform any validity checking on the arguments.

**Argument Postconditionals**

Commands that affect the flow of control may accept postconditionals on individual command arguments. Because multiple arguments act as multiple commands, this is a straight-forward application of the same principal as command postconditional. The only M standard commands that accept argument postconditionals are DO, GOTO, and XECUTE. The YottaDB command extensions that accept argument postconditionals are BREAK, ZGOTO, and ZSYSTEM.

+++++++++++++++++
Timeouts
+++++++++++++++++

M provides timeouts as a tool to retain program control over commands of indefinite duration. A timeout consists of a colon (:) delimiter on an argument, followed by a numeric expression specifying the number of seconds to millisecond (three decimal place) precision for M to attempt to execute the command. When the timeout is zero (0), M makes a single attempt to complete the command.

YottaDB caps the maximum timeout to 2,147,483.647 seconds (about 24.8 days), and converts values greater than the maximum timeout to that cap. When a command has a timeout, M maintains the $TEST intrinsic special variable as the command completes. If the command completes successfully, M sets $TEST to TRUE (1). If the command times out before successful completion, M sets $TEST to FALSE (0). When a command argument does not specify a timeout, M does not maintain $TEST.

The following commands accept timeouts:

* LOCK
* JOB
* OPEN
* READ
* ZALLOCATE

When a READ times out, M returns any characters that have arrived between the start of the command and the timeout. M does not produce any partial results for any of the other timed commands.

.. _m-locks:

----------------------------
M Locks
----------------------------

The LOCK command reserves one or more resource names. Only one process at a time can reserve a resource name. Resource names follow exactly the same formation rules as M variables. They may be unsubscripted or subscripted and may or may not have a leading caret (^) prefix. M code commonly uses LOCKs as flags that control access to global data. Generally, a LOCK specifies the resource with the same name as the global variable that requires protected access. However, this is only a convention. LOCKing does not keep two or more processes from modifying the same global variable. It only keeps another process from LOCKing the same resource name at the same time.

M LOCKs are hierarchical. If one process holds a LOCK on a resource, no other process can LOCK either an ancestor or a descendant resource. For example, a LOCK on ^A(1,2) blocks LOCKs on either ^A(1), or ^A(1,2,3), but not on, for example, ^A(2) or its descendants.

A LOCK argument may contain any subscripted or unsubscripted M variable name including a name without a preceding caret symbol (^). As they have the appearance of local variable names, resource names with no preceding caret symbol (^) are commonly referred to as "local LOCKs" even though these LOCKs interact with other processes. For more information on the interaction between LOCKs and processes, refer to the `LKE chapter in the Administration and Operations Guide <../AdminOpsGuide/mlocks.html>`_.

The YottaDB run-time system records LOCK information in memory associated with the region holding the global of the same name. However, YottaDB does not place LOCKs in the database structures that hold the globals. Instead the LOCK manager sets up a "LOCK database" associated with each database region. Only the M commands LOCK, ZALLOCATE, and ZDEALLOCATE and the LKE utility access the information in the LOCK database.

YottaDB distributes the LOCK database within space associated with the database files identified by the Global Directory (GD). The Global Directory Editor (GDE) enables you to create and maintain global directories. YottaDB associates LOCKs of resource names starting with a caret symbol (^) with the database region used to map variables with the same name. If the global directory maps the name ^A to file A.DAT, YottaDB maps all LOCKs on resource name ^A to LOCK space implemented in shared memory control structures associated with A.DAT. YottaDB maps LOCKs on names not starting with a caret symbol (^) to the region of the database specified with the GDE command LOCKS -REGION.

By default, GDE creates global directories mapping "local" LOCKs to the region DEFAULT.

^LOCKS automatically intersect for all users of the same data in any database file, because YottaDB associates the ^LOCKs with the same region as the global variables with the same name.

"Local" LOCK intersections are dependent on the global directory, because users may access the database through different global directories. The "local" LOCKs of two processes interact with each other only when the same lock resource names map to the same database region.

----------------------------
Intrinsic Functions
----------------------------

M Intrinsic Functions start with a single dollar sign ($) and have one or more arguments enclosed in parentheses () and separated by commas (,). These functions provide an expression result by performing actions that would be impossible or difficult to perform using M commands. It is now possible to invoke a C function in a package via the external call mechanism. For information on the functions, see `Chapter 7: “Functions” <./functions.html>`_.

----------------------------
Intrinsic Special Variables
----------------------------

M Intrinsic Special Variables start with a single dollar sign ($). YottaDB provides such variables for program examination. In some cases, the Intrinsic Special Variables may be SET to modify the corresponding part of the environment. For information, see `Chapter 8: “Intrinsic Special Variables” <./isv.html>`_.

-------------------------
Routines
-------------------------

M routines have a name and consist of lines of code followed by a formfeed. M separates the name of a routine from the body of the routine with an end-of-line which is a line-feed. This form is mostly used for interchange with other M implementations and can be read and written by the %RI and %RO utility routines.

YottaDB stores routine sources in UNIX text files.

In M, a routine has no particular impact on variable management and may include code that is invoked at different times and has no logical intersection.

++++++++++++++++
Lines
++++++++++++++++

A line of M code consists of the following elements in the following order:

* An optional label.
* A line-start delimiter. The standard defines the line-start delimiter as a space (<SP>) character. In order to enhance routine readability, YottaDB extends M by accepting one or more tab (<HT>) characters as line-start delimiters.
* Zero or more level indicators, which are periods (.). The level indicators show the level of nesting for argumentless DO commands: the more periods, the deeper the nesting. M ignores lines that contain level indicators unless they directly follow an argumentless DO command with a matching level of nesting. For more information on the DO command, see `Chapter 6: “Commands” <./commands.html>`_.
* Zero or more commands and their arguments. M accepts multiple commands on a line. The argument(s) of one command are separated from the next command by a command-start delimiter, consisting of one or more spaces (<SP>).
* A terminating end-of-line, which is a line feed.

**Labels**

In addition to labels that follow the rules for M names, M accepts labels consisting only of digits. In a label consisting only of digits, leading zeros are considered significant. For example, labels 1 and 01 are different. Formallists may immediately follow a label. A Formallist consists of one or more names enclosed in parentheses (). Formallists identify local variables that "receive" passed values in M parameter passing. For more information, see :ref:`parameter-passing`.

In YottaDB, a colon (:) delimiter may be appended to the label, which causes the label to be treated as "local." Within the routine in which they appear, they perform exactly as they would without the trailing colon but they are available only during compilation and inaccessible to other routines and to indirection or XECUTE. Because references to local labels preceding their position in a routine produce a LABELUNKNOWN error at run-time, YottaDB recommends omitting the routinename from labelrefs to a local label. Using local labels reduces object size and linking overhead for all types of dynamic linking except indirection and XECUTE. Use of local labels may either improve or impair performance; typically any difference is modest. The more likely they are to all be used within the code block at run-time, the more likely an improvement. In other words, conditional code paths which prevent all references to local variables appearing in the block may actually impair performance.

**Comments**

In addition to commands, a line may also contain a comment that starts with a leading semi-colon (;) delimiter. The scope of a comment is the remainder of the line. In other words, M ignores anything to the right of the comment delimiter. The standard defines the comment delimiter (;) as it would a command, and therefore requires that it always appear after a linestart. YottaDB extends the standard to permit comments to start at the first character of a line or in an argument position.

+++++++++++++++++
Entry References
+++++++++++++++++

M entryrefs provide a generalized target for referring to a line within a routine. An entryref may contain some combination of a label, an offset, and a routine name (in that order). The offset is delimited by a plus sign (+) and the routinename is delimited by a caret symbol(^). When an entryref does not contain a label, M assumes the offset is from the beginning of the routine. When an entryref does not contain an offset, M uses an offset of zero (0). When an entryref does not contain a routine name, M assumes the routine that is currently executing.

M permits every element in an entryref to have the form of an indirection operator, followed by an element that evaluates to a legitimate occurrence of that portion of the entryref.

.. note::
   YottaDB accepts an offset without a label (for example +3^RTN) for an entryref argument to DO, GOTO and ZGOTO but prohibits the same during parameter passing with the JOB command.

Offsets provide an extremely useful tool for debugging. However, avoid their use in production code because they generally produce maintenance problems.

+++++++++++++++++
Label References
+++++++++++++++++

M labelrefs are a subset of entryrefs that exclude offsets and separate indirection. Labelrefs are used with parameter passing.

----------------------------
Indirection
----------------------------

M provides indirection as a means to defer definition of elements of the code until run-time. Indirection names a variable that holds or "points" to the element. The indirection operator is the "at" symbol (@).

++++++++++++++++++++++++++
Argument Indirection
++++++++++++++++++++++++++

Most commands accept indirection of their entire argument.

Example:

.. code-block:: bash

   YDB>set x="^INDER"
   YDB>do @x

This example is equivalent to do ^INDER.

++++++++++++++++++++++++
Atomic Indirection
++++++++++++++++++++++++

Any expratom or any local or global variable name may be replaced by indirection.

Example:

.. code-block:: bash

   YDB>set x="HOOP",b="x"
   YDB>set a="HULA "_@b
   YDB>write a
   HULA HOOP
   YDB>

This example uses indirection within a concatenation operation.

+++++++++++++++++++++
Entryref Indirection
+++++++++++++++++++++

Any element of an entryref may be replaced by indirection.

Example:

.. code-block:: bash

   YDB>set lab="START",routine="PROG"
   YDB>do @lab^@routine

This example is equivalent to do START^PROG.

++++++++++++++++++++++++++
Pattern Code Indirection
++++++++++++++++++++++++++

A pattern code may be replaced by indirection.

Example:

.. code-block:: bash

   YDB>FOR p="1U.20A1"",""1U.20A","5N" IF x?@p QUIT
   YDB>ELSE WRITE !,"Incorrect format" QUIT

This example uses pattern code indirection to test x for either a five-digit number, or a name consisting of two comma separated pieces, each starting with an upper case letter and followed by up to twenty alphabetic characters.

++++++++++++++++++++++++
Name Indirection
++++++++++++++++++++++++

Indirection may replace the prefix of a subscripted global or local variable name. This "name" indirection requires two indirection operators, a leading operator similar to the other forms of indirection, and a trailing operator marking the transition to those subscripts that are not specified by indirection.

Example:

.. code-block:: bash

   YDB>SET from="B",to="^A(15)",x=""
   YDB>FOR SET x=$O(@from@(x)) Q:x="" S @to@(x)=@from@(x)

This example uses name indirection to copy the level contents of a local array to a part of a global array. The example assumes that all existing first level nodes of variable B have data.

+++++++++++++++++++++++
Indirection Concerns
+++++++++++++++++++++++

M indirection provides a very powerful tool for allowing program abstraction. However, because indirection is frequently unnecessary and has some disadvantages, use it carefully.

Because routines that use indirection in some ways do not contain adequate information for easy reading, such routines tend to be more difficult to debug and maintain.

To improve run-time performance, YottaDB tends to move work from run-time to compile-time. Indirection forces compiler actions to occur at run-time, which minimizes the benefits of compilation.

M allows most forms of indirection to be recursive. However, in real applications, recursive indirection typically makes the code obscure and slow.

There are circumstances where indirection serves a worthwhile purpose. For instance, certain utility functions with a general nature may be clearly abstracted and coded using indirection. Because M has no "CASE" command, DO (or GOTO) with argument indirection provides a clear solution to the problem of providing complex branching.

Some M users prototype with indirection and then replace indirection with generated code that reduces run-time overhead. In any case, always consider whether indirection can be replaced with a clearer or more efficient approach.

Run-time errors from indirection or XECUTEs maintain $STATUS and $ZSTATUS related information and cause normal error handling but do not provide compiler supplied information on the location of any error within the code fragment.

.. _parameter-passing:

----------------------------------
Parameter Passing
----------------------------------

Parameter passing provides a way of explicitly controlling some or all of the variable context transferred between M routines.

M uses parameter passing for:

* A DO command with parameters
* Extrinsic functions and special variables

Parameter passing is optional on DO commands.

Parameter passing uses two argument lists: the actuallist that specifies the parameters that M passes to an invoked routine, and the formallist that specifies the local variables to receive or associate with the parameters.

++++++++++++++
Actuallists
++++++++++++++

An actuallist specifies the parameters M passes to the invoked routine. The actuallist contains a list of zero or more parameters enclosed in parentheses, immediately following a DO or extrinsic function.

An actuallist:

* Is made up of items separated by commas
* Contains expressions and/or actualnames. Items may be missing, that is, two commas may appear next to each other, with nothing between them.
* Must be used in an invocation of a label with a formallist, except in the case of extrinsic special variables.
* Must not contain undefined variables.
* Must not have more items than a formallist with which it is used.
* May contain the same item in more than one position.

Example:

.. code-block:: bash

   YDB>DO MULT(3,X,.RESULT)

This example illustrates a DO with parameters. The actuallist contains:

* 3 - a numeric literal
* X - a local variable
* .RESULT - an actualname

++++++++++++++++++++
Actualnames
++++++++++++++++++++

An actualname starts with a leading period (.) delimiter, followed by an unsubscripted local variable name. Actualnames identify variables that are passed by reference, as described in a subsequent section. While expressions in an actualname are evaluated when control is transferred to a formallabel, the variables identified by actualnames are not; therefore, they do not need to be defined at the time control is transferred.

++++++++++++++++++++
Formallists
++++++++++++++++++++

A formallist specifies the variables M uses to hold passed values. A formallist contains a list of zero or more parameters enclosed in parentheses, immediately following a label.

A formallist:

* Is made up of items separated by commas.
* Contains unsubscripted local variable names.
* Must be used and only used with a label invoked with an actuallist or an extrinsic.
* May contain undefined variables.
* May have more items than an actuallist with which it is used.
* Must not contain the same item in more than one position.
* Must contain at least as many items as the actuallist with which it is used.

Example:

.. code-block:: none

   MULT(MP,MC,RES)
   SET RES=MP*MC
   QUIT RES

In this example, illustrating a simple parameterized routine, the formallist contains the following items:

* MP
* MC
* RES

An example in the section describing "Actuallists" shows an invocation that matches this routine.

++++++++++++++++++
Formallabel
++++++++++++++++++

A label followed by a formallist is called a formallabel.

++++++++++++++++++++++++++++
Parameter Passing Operation
++++++++++++++++++++++++++++

M performs an implicit NEW on the formallist names and replaces the formallist items with the actuallist items.

M provides the actuallist values to the invoked procedure by giving each element in the formallist the value or reference provided by the corresponding element in the actuallist. M associates the first name in the formallist with the first item in the actuallist, the second name in the formallist with the second item in the actuallist and so on. If the actuallist is shorter than the formallist, M ensures that the formallist items with no corresponding value are in effect NEWed. If the formallist item has no corresponding item in the actuallist (indicated by two adjacent commas in the actuallist), that item in the formallist becomes undefined.

If the actuallist item is an expression and the corresponding formallist variable is an array, parameter passing does not affect the subscripted elements of the array. If an actualname corresponds to a formallist variable, M reflects array operations on the formallist variable, by reference, in the variable specified by the actualname.

M treats variables that are not part of the formallist as if parameter passing did not exist (i.e., M makes them available to the invoked routine).

M initiates execution at the first command following the formallabel.

A QUIT command terminates execution of the invoked routine. At the time of the QUIT, M restores the formallist items to the values they had at the invocation of the routine.

.. note::
   In the case where a variable name appears as an actualname in the actuallist, and also as a variable in the formallist, the restored value reflects any change made by reference.

A QUIT from a DO does not take an argument, while a QUIT from an extrinsic must have an argument. This represents one of the two major differences between the DO command with parameters and the extrinsics. M returns the value of the QUIT command argument as the value of the extrinsic function or special variable. The other difference is that M stacks $TEST for extrinsics.

For more information, see :ref:`extrinsic-functions` and :ref:`extrinsic-special-vars`.

Example:

.. code-block:: none

   SET X=30,Z="Hello"
   DO WRTSQR(X)
   ZWRITE
   QUIT
   WRTSQR(Z)
   SET Z=Z*Z
   WRITE Z,!
   QUIT

produces

.. code-block:: none

   900
   X=30
   Z="Hello"

++++++++++++++++++++++++++++
Parameter Passing Mechanisms
++++++++++++++++++++++++++++

M passes the actuallist values to the invoked routine using two parameter-passing mechanisms:

* Call-by-Value - where expressions appear
* Call-by-Reference - where actualnames appear

A call-by-value passes a copy of the value of the actuallist expression to the invoked routine by assigning the copy to a formallist variable. If the parameter is a variable, the invoked routine may change that variable. However, because M constructs that variable to hold the copy, it deletes the variable holding the copy when the QUIT restores the prior formallist values. This also means that changes to the variable by the invoked routine do not affect the value of the variable in the invoking routine.

Example:

.. code-block:: none

   SET X=30
   DO SQR(X)
   ZWRITE
   QUIT
   SQR(Z)SET Z=Z*Z
   QUIT

produces:

.. code-block:: none

   X=30

A period followed by a name identifies an actualname and causes a call-by-reference.

A call-by-reference passes a pointer to the variable of the invoked routine so operations on the assigned formallist variable also act on the actualname variable. Changes, including KILLs to the formallist variable, immediately have the same affect on the corresponding actualname variable. This means that M passes changes to formallist variables in the invoked routine back to the invoking routine as changes in actualname variables.

Example:

.. code-block:: none

   SET X=30
   DO SQR(.X)
   ZWRITE
   QUIT
   SQR(Z)SET Z=Z*Z
   QUIT

produces:

.. code-block:: none

   X=900

+++++++++++++++++++++++++++++
Parameter Passing Extensions
+++++++++++++++++++++++++++++

The standard does not provide for indirection of a labelref because the syntax has an ambiguity.

Example:

.. code-block:: none

   DO @X(1)

This example could be:

* An invocation of the label specified by X with a parameter of 1.
* An invocation of the label specified by X(1) with no parameter list.

YottaDB processes the latter interpretation as illustrated in the following example.

Example:

The syntax:

.. code-block:: none

   SET A(1)="CUBE",X=5
   DO @A(1)(.X)
   WRITE X,!
   QUIT
   CUBE(C);cube a variable
   SET C=C*C*C
   QUIT

Produces the result:

.. code-block:: none

   125

YottaDB follows analogous syntax for routine indirection:

**DO ^@X(A)** invokes the routine specified by X(A).

**DO ^@(X)(A)** invokes the routine specified by X and passes the parameter A.

**DO ^@X(A)(A)** invokes the routine specified by X(A) and passes the parameter A.

.. _ext-calls:

---------------------------
External Calls
---------------------------

YottaDB allows references to a YottaDB database from programs written in other programming languages that run under UNIX.

In YottaDB, calls to C language routines may be made with the following syntax:

.. code-block:: none

   DO &[packagename.]name[^name][parameter-list]

or as an expression element,

.. code-block:: none

   $&[packagename.]name[^name][parameter-list]

Where packagename, like the name elements is a valid M name. Because of the parsing conventions of M, the identifier between the ampersand (&) and the optional parameter-list has precisely constrained punctuation – a later section describes how to transform this into a more richly punctuated name should that be appropriate for the called function. While the intent of the syntax is to permit the name^name to match an M labelref, there is no semantic implication to any use of the caret (^).

.. note::
   For more information on external calls, see `Chapter 11: “Integrating External Routines” <./extrout.html>`_.

.. _extrinsic-functions:

---------------------------
Extrinsic Functions
---------------------------

An extrinsic function is an M subroutine that another M routine can invoke to return a value.

The format for extrinsic functions is:

.. code-block:: none

   $$[label][^routinename]([expr|.lname[,...]])


* The optional label and optional routinename make up the formallabel that specifies the name of the subroutine performing the extrinsic function. The formallabel must contain at least one of its optional components.
* The optional expressions and actualnames make up the actuallist that specifies the list of actual parameters M passes to the invoked routine.

M stacks $TEST for extrinsic functions. This is one of the two major differences between the DO command with parameters and extrinsics. On return from an extrinsic function, M restores the value of $TEST to what it was before the extrinsic function, regardless of the actions executed by the invoked routine.

M requires a routine that implements an extrinsic function to terminate with an explicit QUIT command which has an argument. M returns the value of the QUIT command argument as the value of the extrinsic function. This is the other major difference between the DO command with parameters and extrinsics. It is now possible to invoke a C function in a package via the external call mechanism.

Example:

.. code-block:: none

   POWER(V,X,S,T);extrinsic to raise to a power
   ;ignores fractional powers
   SET T=1,S=0
   IF X<0 SET X=-X,S=1
   FOR X=1:1:X S T=T*V
   QUIT $S(S:1/T,1:T)
   YDB> WRITE $$^POWER(3,4)
   81
   YDB>

.. note::
   The POWER routine uses a formallist that is longer than the "expected" actuallist to protect local working variables. Such a practice may be encouraged or discouraged by your institution's standards.

.. _extrinsic-special-vars:

--------------------------------
Extrinsic Special Variables
--------------------------------

An extrinsic special variable is a user-written M subroutine that another M routine can invoke to return a value.

The format for extrinsic special variables is:

.. code-block:: none

   $$[label][^routinename]

* The optional label and optional routinename make up the formallabel, which specifies the name of the subroutine performing the extrinsic function. The formallabel must contain at least one of its optional component.

An extrinsic special variable can be thought of as an extrinsic function without input parameters. \$ \$ x is equivalent in operation to \$ \$ x(). Extrinsic special variables are the only case where invocation of a formallabel does not require an actuallist. M stacks $TEST for extrinsic special variables.

M requires that a routine that implements an extrinsic special variable terminate with an explicit QUIT command which has an argument. M returns the value of the QUIT command argument as the value of the extrinsic special variable.

Example:

.. code-block:: bash

   YDB>ZPRINT ^DAYOWEEK
   DAYOWEEK();extrinsic special variable to
   ;provide the day of the week
   QUIT $ZD($H,"DAY")
   YDB>WRITE $$DAYOWEEK^DAYOWEEK
   MON

--------------------------------
Transaction Processing
--------------------------------

Transaction Processing (TP) provides a way for M programs to organize database updates into logical groups that occur as a single event (i.e., either all the database updates in a transaction occur, or none of them occur). With a properly constructed transaction, no other actor or process behaves as if it observed any intermediate state. Transaction processing has been designed to improve throughput and minimize the possibility and impact of "live lock" conditions.

++++++++++++++++
TP Definitions
++++++++++++++++

In M, a transaction is a sequence of commands that begins with a TSTART command, ends with a TCOMMIT command, and is not within the scope of another transaction. Applications can nest TSTART/TCOMMIT commands to create sub-transactions, but sub-transactions only commit at the outer-most TCOMMIT. $TLEVEL greater than 1 indicates sub-transaction nesting.

A successful transaction ends with a COMMIT that is triggered by the TCOMMIT command at the end of the transaction. A COMMIT causes all the database updates performed within the transaction to become available to other processes.

An unsuccessful transaction ends with a ROLLBACK. ROLLBACK is invoked explicitly by the TROLLBACK command, or implicitly at a process termination that occurs during a transaction in progress. An error within a transaction does not cause an implicit ROLLBACK. A ROLLBACK removes any database updates performed within the transaction before they are made available to other processes. ROLLBACK also releases all resources LOCKed since the start of the transaction, and makes the naked reference undefined.

A RESTART is a transfer of control to the TSTART at the beginning of the transaction. RESTART implicitly includes a ROLLBACK and may optionally restore local variables to the values they had when the initial TSTART was originally executed. A RESTART always restores $TEST and the naked reference to the values they had when the initial TSTART was executed. RESTART does not manage device state information. A RESTART is invoked by the TRESTART command or by M if it is determined that the transaction is in conflict with other database updates. RESTART can only successfully occur if the initial TSTART includes an argument that enables RESTART.

+++++++++++++++++++++++++++++++++++++
Key Considerations - Writing TP Code
+++++++++++++++++++++++++++++++++++++

Some key considerations for writing application code between TSTART and TCOMMIT are as follows:

* Do not use BREAK, CLOSE, JOB, OPEN, READ, USE, WRITE, LOCK, HANG, and ZSYSTEM as they violate the ACID principal of Isolation. Using these commands inside a transaction may lead to longer than usual response time, high CPU utilization, repeat execution due to transaction restart, and/or TPNOTACID messages in the operator log. If application logic requires their use, put them before TSTART or after TCOMMIT so that they do not interfere with the transaction processing mechanism.

* Keep your transaction code "pure" . By "pure" we mean that you restrict code to only perform database updates (SET, MERGE, and so on). The primary purpose of a YottaDB transaction is to perform database updates that commit in entirety or do not commit at all. Perform external interaction like invoking an external call before or after the transaction.

* Design transactions to minimize the number of regions they use, particularly update. Like keeping transactions small, this minimizes contention and improves performance.

* Keep transactions as short as possible.

* Code for handling errors during transactions must include a TROLLBACK. A TROLLBACK should appear as early as possible in the error handling code. You can run commands like WRITE, OPEN, etc. after TROLLBACK because the TROLLBACK releases resources held by the transaction.

* Remember that trigger code executes within an implicit transaction. So, trigger code is always subject to transaction considerations.

Most transaction processing systems try to have transactions that meet the "ACID" test – Atomic, Consistent, Isolated, and Durable.

To provide ACID transactions, YottaDB uses a technique called optimistic concurrency control. Each block has a transaction number that YottaDB sets to the current database transaction number when updating a block. Application logic, brackets transactions with TSTART and TCOMMIT commands. Once inside a transaction, a YottaDB process tracks each database block that it reads (any database block containing existing data that it intends to update has to be read first) and in process private memory keeps a list of updates that it intends to apply - application logic within the process views the database with the updates; application logic in other processes does not see states internal to the transaction. At TCOMMIT time, the process checks whether any blocks have changed since it read them, and if none have changed, it commits the transaction, making its changes visible to other processes Atomically with Isolation and Consistency (Durability comes from the journal records written at COMMIT time). Optimistic concurrency attempts to exploit the odds that two processes need access to the same resource at the same time. If the chances are small, it permits many processes to work concurrently, particularly in a system with multiple CPUs. If the changes are not small the penalty is repeated execution of the same transaction logic.

If one or more blocks have changed, the process reverts its state to the TSTART and re-executes the application code for the transaction. If it fails to commit the second time, it tries yet again. If it fails to commit on the third attempt, it locks other processes out of the database and executes the transaction as the sole process (that is, on the fourth attempt, it switches to a from an optimistic approach to a pessimistic one).

This technique normally works very well and is one of the factors that allow YottaDB to excel at transaction processing throughput.

.. note::
    YottaDB uses implicit transaction processing when it needs to ensure complex operations, including spanning block actions, spanning region actions and trigger actions preserve Atomicity. Of these, triggers involve application code and therefore are most subject to the following discussion.

Pathological cases occur when processes routinely modify blocks that other processes have read (called "collisions"), resulting in frequent transaction restarts. Collisions can be legitimate or accidental. Importantly, the longer that a transaction is "open" (the "collision window," when the application logic is between TSTART and TCOMMIT), the greater the probability that a collision will require a transaction restart.

Legitimate collisions can result from normal business activity, for example, if two joint account holders make simultaneous ATM withdrawals from a joint account. When the time an application takes to process each transaction is a minuscule fraction of a second, the probability of a collision is very low, and in the rare case where one occurs, the restart mechanism handles it well. An example with a higher probability of collision comes from commercial accounts, where a large enterprise may have tens to hundreds of accounts, individual transactions may hit multiple accounts, and during the business day many people may execute transactions against those accounts. Again, the small collision window means that collisions remain rare and the restart mechanism handles them well when they occur.

Legitimate (from a YottaDB point of view) collisions can also occur as a consequence of application design. For example, if an application has an application level transaction journal that every process appends to then that design will likely result in high rates of collisions, creating a pathological case where every transaction fails three times and then commits on the fourth attempt with all other processes locked out. The way to avoid these is to adjust the application design, either to use M LOCKs to gate such "hot spots" or, better, to give each process its own update space which, at some event, a single process then consolidates.

Accidental collisions result when two processes access unrelated data that happens to reside on the same data block (for example, some globals indexed by last name can result in an accidental collision - for two account holders whose last names start with the same letter, the global data nodes may reside in the same block). Because the path to many data blocks typically pass though one index block, data additions cause changes in index blocks and can generate accidental collisions. While it is not possible to avoid accidental collisions (especially in blocks containing metadata such as index blocks), they are typically rare and the occasional collision is handled well by the restart mechanism. Because the application is rarely in a position to efficiently prevent accidental collisions, YottaDB strongly recommends using TCOMMIT forms that allow YottaDB to use restarts and thus relieve the application logic of having to manage TRESTNOT errors. YottaDB uses the database block as the granularity for concurrency control because it is generally an efficient and successful compromise between a more granular and expensive lock and a less granular but more likely to conflict lock. It also simplifies some things by aligning with the unit of transfer to non-volatile storage.

Application design that keeps transactions open for long periods of time can cause pathological rates of accidental collision. When a process tries to run an entire report in a transaction, instead of the transaction taking a fraction of a second (remember that transactions are intended to be atomic), the report takes seconds or even minutes and effectively ensures collisions and restarts. Furthermore, since the probability of collisions is high, the probability of these long-running transactions executing the fourth retry (with other processes shut out) goes up, and when that happens, the system appears to respond erratically, or hang temporarily.

Non-isolated actions are another consideration in the design of wholesome transactions. Because M permits all language features with a transaction, an application may use actions that interact with actors outside of the transaction. Such actions violate the ACID principle of Isolation, which states that to be wholesome, a transaction must not interact with other agents or processes until it commits (see below for a more detailed discussion). While there may be reasons drawn from the larger application model that justify violations of Isolation, doing so carries risks. One problem is time: external interactions typically have a longer duration, and in the worst case, may have an indefinite duration. The JOB, LOCK, OPEN, and READ commands have an optional timeout to place time limits on external interactions as do some WRITE format arguments. The HANG command induces a potentially arbitrary delay. In addition, BREAK, WRITE, ZSYSTEM and external calls also involve external interaction. Except for WRITE and external calls, in order to minimize the potential impact of non-ACID transactions, YottaDB limits the duration of database locks for transactions that use these non-isolated commands, and records that limitation as a TPNOTACID message in the operator log. However, that time limit, managed with the ydb_tpnotacidtime environment variable, can be long enough, depending on its value, to permit noticeable processing disruptions. Further, processes denied a long lock may have trouble completing and consume system resources with repeated unsuccessful attempts. External calls are excluded from this protection because they are the domain of more sophisticated design and may actually remain isolated (see the tip below on Implementing Web Services). WRITE is currently excluded because most WRITE commands are non-blocking, but applications should avoid blocking WRITEs within a transaction. Beyond the issue of duration, because the application can repeat due to a restart or rollback because of an error or application logic, non-isolated actions require management to appropriately manage their external interactions; this is discussed in more detail below. In summary, put external interactions before or after transactions rather than within them. If the application requires a non-isolated action within a transaction, be aware of the risks, design, implement and test very carefully.

YottaDB provides a transaction timeout feature that interrupts long-running transactions in order to limit their impact on the system, and the consequent user perception of system erratic response times and temporary hangs. Calls to an external library, say to access a web service, can subvert the timeout mechanism when the external library uses an uninterruptable system call. If such a web service uses an adjacent server that responds immediately, the web service is wholesome. But if the web service accesses a remote server without a guaranteed short response time, then collisions may be frequent, and if a process in the fourth retry waits for a web service that never responds, it brings the entire application to a standstill.

.. note::
   To safely implement web services inside a transaction, an application must implement a guaranteed upper bound on the time taken by the service. The story or use case for each circumstance determines the appropriate timeout for the corresponding transaction. For example, if the web service is to authorize a transaction, there might be a 500 millisecond timeout with the authorization refused if the approval service does not respond within that time. There are two approaches to implementing web services with a timeout.For applications that call out to C code, the C code guarantee a return within a time limit, using a wrapper if necessary. YottaDB provides functions that external C code can use to implement timers. If the call is to an unknown library, or one without a way to guarantee a timeout, the external C code may need to create an intermediate proxy that can provide a timeout to YottaDB. Because web services are usually implemented by a known protocol layered on TCP/IP and YottaDB provides a SOCKET device for TCP/IP connections, implement the call out to the web service using a SOCKET device. YottaDB can then enforce the TP timeout mechanism, which it cannot for an external call, especially one that calls via a library into an uninterruptible OS service.

To conform with the M approach of providing maximum flexibility and, when possible, backwards compatibility with older versions of the standard, M transaction processing requires the use of programming conventions that meet the ACID test.

For example, some effects of the BREAK, CLOSE, JOB, OPEN, READ, USE WRITE, and ZSYSTEM commands may be observed by parties to the system. Because the effects of these commands might cause an observing process or person to conclude that a transaction executing them was in progress and perhaps finished, they violate, in theory, the principle of Isolation.

The LOCK command is another example. A program may attempt to use a LOCK to determine if another process has a transaction in progress. The answer would depend on the management of LOCKs within transactions, which is implementation-specific. This would therefore clearly violate the principle of Isolation. The LOCK command is discussed later in this section.

The simplest way to construct a transaction that meets the ACID test is not to use any commands within a transaction whose effects may be immediately "visible" outside the transaction. Unfortunately, because M applications are highly interactive, this is not entirely straightforward. When a user interaction relies on database information, one solution is for the program to save the initial values of any global values that could affect the outcome, in local variables. Then, once the interaction is over and the transaction has been initiated, the program checks the saved values against the corresponding global variables. If they are the same, it proceeds. If they differ, some other update has changed the information, and the program must issue a TROLLBACK, and initiate another interaction as a replacement.

Even when the "visible" commands appear within a transaction, an M application may provide wholesome operation by relying on additional programming or operating conventions.

A program using M LOCKs to serialize transactions relies on properly designed and universally followed LOCKing conventions to achieve Isolation with respect to database operations. We recommend using transaction processing instead of M LOCKs.

In YottaDB the Durability aspect of the ACID properties relies on the journaling feature. When journaling is on, every transaction is recorded in the journal file as well as in the database. The journal file constitutes a serial record of database actions and states. It is always written before the database updates and is designed to permit recovery of the database if the database should be damaged. By default, when a process commits a transaction, it does not return control to the application code until the transaction has reached the journal file. The exception to this is that when the TSTART specifies TRANSACTIONID="BATCH" the process resumes application execution without waiting for the file system to confirm the successful write of the journal record. The idea of the TRANSACTIONID="BATCH" has nothing inherently to do with "batch" processing - it is to permit maximum throughput for transactions where the application has its own check-pointing mechanism, or method of recreating the transaction in case of a failure. The real durability of transactions is a function of the durability of the journal files. Putting journal files on reliable devices (RAID with UPS protection) and eliminating common points of failure with the path to the database (separate drives, controllers cabling) improve durability. The use of the replication feature can also improve durability by moving the data to a separate site in real time.

Attempting to QUIT (implicitly or explicitly) from code invoked by a DO, XECUTE, or extrinsic after that code issued a TSTART not yet matched by a TCOMMIT, produces an error. Although this is a consequence of the RESTART capability, it is true even when that capability is disabled. For example, this means that an XECUTE containing only a TSTART fails, while an XECUTE that performs a complete transaction succeeds.

++++++++++++++++++++++++++
TP Performance
++++++++++++++++++++++++++

To achieve the best YottaDB performance, transactions should:

* be as short as possible
* consist, as much as possible, only of global updates
* be SERIAL with no associated LOCKs
* have RESTART enabled with a minimum of local variables protected by a restart portion of the TSTART argument.
* Large concurrent transactions using TCOMMIT may result in repeated and inefficient attempts by competing processes to capture needed scarce resources, resulting in poor performance.

Example:

.. code-block:: none

   TSTART ():SERIAL
   SET (ACCT,^M(0))=^M(0)+1
   SET ^M(ACCT)=PREC,^PN(NAM)=ACCT
   TCOMMIT

This transaction encapsulates these two SETs. The first increments the tally of patients registered, storing the number in local variable ACCT for faster access in the current program, and in global variable ^M(0). The second SET stores a patient record by account number and the third cross-references the account number with the patient name. Placing the SETs within a single transaction ensures that the database always receives either all of the SETs or none of them, thus protecting database integrity against process or system failure. Similarly, another concurrent process, whether using transactions or not, never finds one of the SETs in place without also finding the other one.

Example:

.. code-block:: none

   TSTART ():SERIAL
   IF $TRESTART>3 DO QUIT
   .TROLLBACK
   .WRITE !,"Too many RESTARTs"
   .QUIT
   SET (NEXT,^ID(0))=^ID(0)+1
   SET ^ID(NEXT)=RECORD,^XID(ZIP,NEXT)=""
   TCOMMIT

This transaction will automatically restart if it cannot serialize the SETs to the database, and will terminate with a TROLLBACK if more than 3 RESTARTs occur.

YottaDB provides a way to monitor transaction restarts by reporting them to the operator logging facility. If the environment variable ydb_tprestart_log_delta is defined, YottaDB reports every Nth restart where N is the numeric evaluation of the value of ydb_tprestart_log_delta. If the environment variable ydb_tprestart_log_first is defined, the restart reporting begins after the number of restarts specified by the value of ydb_tprestart_log_first. For example, defining both the environment variable to the value 1, causes all TP restarts to be logged. When ydb_tprestart_log_delta is defined, leaving ydb_tprestart_log_first undefined is equivalent to giving it the value 1.

Here is an example message:

.. code-block:: none

   %YDB-I-TPRESTART, Database /gbls/dtx/dtx.dat; code: L; blk: 0x00BA13DD in glbl: ^DTX; pvtmods: 0, blkmods: 1, blklvl: 1, type: 4, readset: 3, writeset: 1, local_tn: 0x00000000000002D0, zpos: LABEL+108^ROUTINENAME

* pvtmods - Is always less than or equal to blkmods. This means it can be 1 only if "blkmods" is also 1. If it is 1, it means that process P1 was planning to UPDATE (not just READ) the block number (indicated as "blk: ..." in the TPRESTART message) as part of its TP transaction.

* blkmods - Is either 1 or 0. 1 implies the transaction restarted because this process (P1) is attempting to READ/UPDATE a block that has concurrently been updated by another process (P2) since P1 access the block as part of its TP transaction. This means the "code: ..." output in the TPRESTART message will have L as the last letter. 0 implies the restart occurred because of a different reason. The "code: ..." then has something other than "L" as the last letter. Note that each letter in "code: ..." corresponds to the failure code in each try/retry in the order of occurrence.

* blklvl - Is the level in the GDS structure of the block ("blk: ..." field in the TPRESTART message) that caused the TP restart.

* type - A value of 0,1,2,4 shows the restart occurred in the TP transaction BEFORE executing TCOMMIT; whether it is a 0 or 1 or 2 or 4 should not matter to the user. These values would typically be used for debugging by your YottaDB support channel. A value of 3 shows the restart occurred at TCOMMIT time.

* readset - The number of GDS blocks that are accessed as part of this TP transaction in the region containing the global ("glbl: ..." in the TPRESTART message).

* writeset - Out of the readset number, the number of GDS blocks this process was attempted to UPDATE as part of this TP transaction in the region containing the global ("glbl: ..." in the TPRESTART message).

* local_tn - This is a never-decreasing counter (starting at 1 at process startup) incremented for every new TP transaction, TP restart, and TP rollback. Two TPRESTART messages by the same process should never have the same value of local_tn. The difference between the local_tn values of two messages from the same process indicates the number of TP transactions done by that process in the time interval between the two messages.

.. note::
   Use VIEW [NO]LOGT[PRESTART][=intexpr] to enable or disable the logging of TPRESTART messages. Note that you can use the ydb_tprestart_log_delta and ydb_tprestart_log_first environment variables to set the frequency of TPRESTART messages. Use VIEW [NO]LOGN[ONTP][=intexpr] to enable or disable the logging of NONTPRESTART messages. This facility is the analog of TPRESTART tracking, but for non-TP mini-transacstions. Note that you can use the ydb_nontprestart_log_delta and ydb_nontprestart_log_first environment variables to set the frequency of the NONTPRESTART messages.For more information, refer to `“Key Words in VIEW Command” <./commands.html#keywords-in-view-command>`_ and the `Environment Variables <../AdminOpsGuide/basicops.html#env-vars>`_ section of the Administration and Operations Guide.


++++++++++++++++++++++
TP Example
++++++++++++++++++++++

Here is a transaction processing example that lets you exercise the concept. If you use this example, be mindful that the functions "holdit" and "trestart" are included as tools to allow you access to information within a transaction which would normally be hidden from users. These types of functions would not normally appear in production code. Comments have been inserted into the code to explain the function of various segments.

.. code-block:: none

   trans
   ;This sets up the program constants
   ;for doit and trestart
   new
   set $piece(peekon,"V",51)=""
   set $piece(peekon,"V",25)="Peeking inside Job "_$job
   set $piece(peekoff,"^",51)=""
   set $piece(peekoff,"^",25)="Leaving peeking Job "_$job
   ;This establishes the main loop
   set CNFLTMSG="Conflict, please reenter"
   for read !,"Name: ",nam quit:'$length(nam) do
   .if "?"=nam do quit
   ..write !,"Current data in ^trans:",! do:$data(^trans) quit
   ...zwrite ^trans
   .for set ok=1 do quit:ok write !,$char(7),CNFLTMSG,$char(7),!
   ..set old=$get(^trans(nam),"?")
   ..if "?"=old write !,"Not on file" do quit
   ...;This is the code to add a new name
   ...for do quit:"?"'=data
   ....read !,"Enter any info using '#' delimiter: ",!,data
   ...if ""=data write !,"No entry made for ",nam quit
   ...TSTART ():SERIAL if $$trestart ;$$trestart for demo
   ...if $data(^trans(nam)) set ok=^trans(nam)=data TROLLBACK quit
   ...set ^trans(nam)=data
   ...TCOMMIT:$$doit ;$$doit for demo
   ..;This is the beginning of the change and delete loop
   ..for do quit:+fld=fld!'$length(fld) write " must be numeric"
   ...write !,"Current data: ",!,old
   ...read !,"Piece no. (negative to delete record) : ",fld
   ..if 'fld write !,"no change made" quit
   ..;This is the code to delete a new name
   ..if fld<0 do quit ; delete record
   ...for do quit:"YyNn"[x
   ....write !,"Ok to delete ",nam," Y(es) or N(o) <N>? "
   ....read x set x=$extract(x)
   ...if "Yy"'[x!'$length(x) write !,"No change made" quit
   ...TSTART ():SERIAL if $$trestart ;$$trestart for demo
   ...if $get(^trans(nam),"?")'=old TROLLBACK set ok=0 quit
   ...kill ^trans(nam)
   ...TCOMMIT:$$doit; $$doit for demo
   ..;This is the code to change a field
   ..for read !,"Data: ",data quit:("?"'=data)&(data'["#") do
   ...write " must not be a single '?' or contain any '#'"
   ..TSTART ():SERIAL if $$trestart ;$$trestart for demo
   ..if '$data(^trans(nam)) set ok=0 TROLLBACK q
   ..if $piece(^trans(nam),"#",fld)=$piece(old,"#",fld) do quit
   ...set ok=$piece(^trans(nam),"#",fld)=data TROLLBACK
   ..set $piece(^trans(nam),"#",fld)=data
   ..TCOMMIT:$$doit; $$doit for demo
   quit
   doit()
   ;This inserts delay and an optional
   ;rollback only to show how it works
   write !!,peekon do disp
   for do quit:"CR"[act
   .read !,"C(ommit), R(ollback), or W(ait) <C>? ",act
   .set act=$translate($extract(act),"cr","CR")
   .if "?"=act do disp
   if "R"=act TROLLBACK write !,"User requested DISCARD"
   write !,peekoff,!
   quit $TLEVEL
   trestart()
   ;This is only to show what is happening
   if $TRESTART do
   .write !!,peekon,!,">>>RESTART<<<",! do disp write !,peekoff,!
   quit 1
   disp
   write !,"Name: ",nam
   write !,"Original data: ",!,old,!,"Current data: "
   write !,$get(^trans(nam),"KILLED!")
   quit

Generally, this type of program would be receiving data from multiple sessions into the same global.



