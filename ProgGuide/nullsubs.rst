
.. index::
   Null Subscripts and Collation

=============================================
Appendix D: Null Subscripts and Collation
=============================================

.. contents::
   :depth: 2

----------------------
Null Subscripts
----------------------

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

--------------------------------
Null Subscript Collation
--------------------------------

The default collation of local and global variable subscripts has been that the null subscript collates between numeric and string subscripts whereas the M standard collation requires the null subscript to collate before any other subscript. YottaDB supports both null collation methods. The collation method must be specified at the time of database creation.

A read-only boolean parameter STDNULLCOLL is in the database fileheader to specify the type of null collation:

* If STDNULLCOLL is set to FALSE, subscripts of globals in the database continue the previous practice where the null subscript collates between numeric and string subscripts.
* If STDNULLCOLL is set to TRUE, subscripts of globals in the database follow the M standard where the null subscript collates before all other subscripts.

When a database is created, the STDNULLCOLL parameter is initialized to the collation specified for that region in the global directory.

To establish the null collation method for a specified database, GDE supports a region parameter STDNULLCOLL that can be set to TRUE or FALSE using a region qualifier -STDNULLCOLL or -NOSTDNULLCOLL respectively. These qualifiers are supported with ADD, CHANGE and TEMPLATE commands. When MUPIP creates a new database, the STDNULLCOLL value is copied from the global directory into the database file header.

For M local variables, the null collation can be established either at startup or during run time. Since the same local collation method is established for all locals in a process, changing the null collation within the process is allowed only if there are no local variables defined at that time. At process startup, YottaDB uses the following:

* The M standard null collation if an environment variable gtm_lct_stdnull is defined to either TRUE or YES (or a case-insensitive leading substring thereof) or a non-zero integer.
* The YottaDB standard null collation if the environment variable or the logical name is not defined or defined to any other value.

To establish a default collation version for local variables within the process, the percent utility %LCLCOL supports establishing the null collation method as well. set^%LCLCOL(col,ncol) accepts an optional parameter ncol that determines the null collation type to be used with the collation type col.

* If the truth value of ncol is FALSE(0), local variables use the YottaDB standard null collation.
* If the truth value of ncol is TRUE(1), local variables use the M standard null collation.
* If ncol is not supplied, there is no change to the already established null collation method.

Also using set^%LCLCOL(,ncol), the null collation order can be changed while keeping the alternate collation order unchanged. If subscripted local variables exist, null collation order cannot be changed. In this case, YottaDB issues GTM-E-COLLDATAEXISTS.

++++++
GDE
++++++

The -REGION qualifier –[NO]NULL_SUBCRIPTS accepts new values with change, add and template commands, default is –NONULL_SUBSCRIPTS, e.g.:

.. parsed-literal::
   GDE>add –region areg –dyn=aseg –null_subscripts=always
   GDE>change –region areg –null_subscripts=true
   GDE>change –region areg –null_subscripts=false
   GDE>change –region areg –null_subscripts=never
   GDE>change –region areg –null_subscripts=existing
   GDE>template –region –null_subscripts=existing
   GDE>template –region –nonull_subscripts

The other region qualifier is –[NO]STDNULLCOLL with add, change and template command, default is –NOSTDNULLCOLL.

.. parsed-literal::
   GDE> template -region -stdnullcoll
   GDE> change -region DEFAULT -stdnullcoll
   GDE> add -segment TEAGLOBALS -file=TEAGLOBALS.dat
   GDE> add -region TEAGLOBALS -dyn=TEAGLOBALS -null_subscripts=existing
   GDE> add -name LapsangSouchong -region=TEAGLOBALS
   GDE> add -name Darjeeling -region=TEAGLOBALS
   GDE> add -name Tea* -region=TEAGLOBALS
   GDE> show -all

                               \*\*\* Templates \*\*\*

   Region                             Def Coll     Rec Size    Key Size    Null Subs    Standard Null Coll   Journaling
   -----------------------------------------------------------------------------------------------------------------------
   <default>                             0          256          64        NEVER               Y                 N


   Segment             Active             Acc      Typ    Block                Alloc    Exten     Options
   ---------------------------------------------------------------------------------------------------------
   <default>            \*                BG       DYN    1024                 100       100      GLOB=1024
                                                                                                  LOCK=40
   <default>                              MM       DYN    1024                 100       100      DEFER
                                                                                                  LOCK=40


                \*\*\* Names \*\*\*

   Global                         Region
   ---------------------------------------
   \*                           DEFAULT
   Darjeeling                   TEAGLOBALS
   LapsangSouchong              TEAGLOBALS
   Tea*                         TEAGLOBALS


                                  \*\*\* REGIONS  \*\*\*

   Region                  Dynamic Segment         Def Coll   Rec Size   Key Size   Null Subs  Standard Null Coll   Journaling
   -----------------------------------------------------------------------------------------------------------------------------
   DEFAULT                   DEFAULT                 0          256         64        NEVER             Y                N
   TEAGLOBALS                TEAGLOBALS              0          256         64        EXISTING          Y                N


                                    \*\*\* SEGMENTS \*\*\*

   Segment             File (def ext: .dat)     Acc  Typ   Block            Alloc   Exten     Options
   -----------------------------------------------------------------------------------------------------
   DEFAULT               mumps.dat              BG   DYN   1024              100     100      GLOB=1024
                                                                                              LOCK=40
                                                                                              RES=0
   TEAGLOBALS            TEAGLOBALS.dat         BG   DYN   1024              100     100      GLOB=1024
                                                                                              LOCK=40
                                                                                              RES=0

                                   \*\*\* MAP \*\*\*

     ---------------------------------- Names --------------------------------------------------

     From                          Upto                      Region/Segment/File (def ext: .dat)
    ---------------------------------------------------------------------------------------------
      %                          Darjeeling                     REG=DEFAULT
                                                                SEG=DEFAULT
                                                                FILE=mumps.dat
     Darjeeling                  Darjeeling0                    REG=TEAGLOBALS
                                                                SEG=TEAGLOBALS
                                                                FILE=TEAGLOBALS.dat
     Darjeeling0                 LapsangSouchong                REG=DEFAULT
                                                                SEG=DEFAULT
                                                                FILE=mumps.dat
     LapsangSouchong             LapsangSouchong0               REG=TEAGLOBALS
                                                                SEG=TEAGLOBALS
                                                                FILE=TEAGLOBALS.dat
     LapsangSouchong0            Tea                            REG=DEFAULT
                                                                SEG=DEFAULT
                                                                FILE=mumps.dat
     Tea                         Teb                            REG=TEAGLOBALS
                                                                SEG=TEAGLOBALS
                                                                FILE=TEAGLOBALS.dat
     Teb                         ...                            REG=DEFAULT
                                                                SEG=DEFAULT
                                                                FILE=mumps.dat
     LOCAL LOCKS                                                REG=DEFAULT
                                                                SEG=DEFAULT
                                                                FILE=mumps.dat
     GDE>

++++++++
DSE
++++++++

The -null_subscripts qualifier accepts never, always and existing. The default qualifier is never.

.. note::
   The null subscript collation order cannot be changed using DSE.

dump –fileheader output reflects this for null_subscripts as well as null collation order.

For a region, “Standard Null Collation” in DSE dump output corresponds to -stdnullcoll field in .gld file. DSE displays TRUE for “Standard Null Collation” if the region has –STDNULLCOLL, otherwise it displays FALSE.

From the example above, the output of dump –fileheader for TEAGLOBALS.dat will be as follows:

.. parsed-literal::
   DSE> dump -fileheader

   File            /tmp/mumps.dat
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
    Journal File: /tmp/mumps.mjl
    Mutex Hard Spin Count          128        Mutex Sleep Spin Count         128
    Mutex Spin Sleep Time         2048        KILLs in progress                0
    Replication State              OFF        Region Seqno    0x0000000000000001
    Resync Seqno    0x0000000000000001        Resync transaction      0x00000001

With Standard null collation, the null subscript is represented by 0x01 instead of 0xFF with YottaDB null collation. So, the output of dse dump -block for a null subscript will also be different.

.. parsed-literal::
   DSE>dump -block=3
   File /testarea1/null_subs/mumps.dat
   Region DEFAULT
                                                    
              Block     3       Size    24  Level   0   TN  3
              Rec:1  Blk 3  Off 8  Size A  Cmpc 0  Key ^a("")
                  8 : | 0  A  0  0 61  0  1  0  0 31              |
                      |  .  .  .  .  a  .  .  .  . 1              |

With YottaDB null collation, for the same command output will be as follows:

.. parsed-literal::
   DSE>dump -block=3
   File /testarea1/null_subs/mumps.dat
   Region DEFAULT
                                                           
             3   Size 24   Level 0   TN 3
             Rec:1  Blk 3  Off 8  Size A  Cmpc 0  Key ^a("")
                 8 : |  0  A  0  0 61  0  FF  0  0 31             |
                     |  .  .  .  .  a  .  .  .  .   1             |   


++++++++++++++++++++++
M Commands/ Functions
++++++++++++++++++++++

**ZWRITE**:

Since with standard collation, null subscripts collate before numeric and string subscripts, ZWR output will be different if nodes with null subscripts exist.

.. parsed-literal::
   YDB>ZWR
   lcl("")=2
   lcl(1)=3
   lcl("x")=4

With the same data and YottaDB null collation, the output of ZWR will be as follows:

.. parsed-literal::
   lcl(1)=3
   lcl("")=2
   lcl("x")=4


**$ORDER()**:

If the last subscript in the subscripted global or local variable name passed as a parameter to $Order() is null and a subscripted global or local variable with a null subscript exists, $ORDER() returns the next node at the specified level.

If the last subscript in the subscripted global or local variable name passed as a parameter to $Order() is null and a subscripted global or local variable with a null subscript does not exist, $ORDER() returns first node at the specified level.

If the last subscript in the subscripted global or local variable name is null and second argument of $ORDER() is -1, $ORDER() will always return the last node at the specified level regardless of the existence of subscripted global or local variable (with null subscript). This allows the user to traverse all the nodes in a specified level starting from the last.

.. parsed-literal::
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

**$ZPREVIOUS()** : is equivalent to $ORDER() with second argument -1.

**$QUERY()**: With stdnullcoll, if $D(glvn(""))=1 (or 11), $Q(glvn("")) will return glvn(1) [assuming glvn(1) exists]. Software should execute $D(glvn("")) to test the existence of glvn(""). $Q(glvn("...")) will never return the starting-point (glvn("")) even though glvn("") may exist. 

.. parsed-literal::
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

For more details about the behavior of these functions with YottaDB Null Collation, please consult the `M Programmer’s Guide <https://docs.yottadb.com/ProgrammersGuide/index.html>`_.

++++++++++++++++++++++++++++++
MUPIP Binary Extract and Load
++++++++++++++++++++++++++++++

* MUPIP EXTRACT -BINARY issues NULLCOLLDIFF error if it needs to extract from multiple databases with different STDNULCOLL settings.
* MUPIP EXTRACT -BINARY writes a new field in the binary extract header to note down the first database's STDNULCOLL setting.
* MUPIP LOAD –BINARY on a binary extract transforms the null subscripts appropriately if the STDNULCOLL setting of the target database is different from the setting in the binary extract header.
* MUPIP LOAD –BINARY is able to successfully load onto multiple databases with different STDNULCOLL settings.
* MUPIP EXTRACT -ZWR and MUPIP LOAD -ZWR will work no matter what the YottaDB version of the source and destination databases, and no matter what the null (or other) collation setting of the source and destination databases.

++++++++++++++++++
Replication
++++++++++++++++++

In a replicated environment, all databases belonging to an instance should have the same null collation order. If this condition is not met, the source server issues the GTM-E-NULLCOLLDIFF error message on the primary. On the secondary, the update process issues the same error message if the condition is not satisfied.

Although all databases belonging to an instance must have the same collation method, YottaDB allows the primary and secondary to use different null collation methods. Any needed conversion is handled internally and transparently.

