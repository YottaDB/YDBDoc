
.. index::
   Triggers

===================
Triggers
===================

.. contents::
   :depth: 2

--------------------
Triggers
--------------------

YottaDB/GT.M allows you to set up a trigger mechanism that automatically executes a defined action in response to a database update operation on a matching global node. The trigger mechanism executes a fragment of M code (trigger code) "before" or "as part of" a database update. You can define the specifications of this mechanism in a Trigger Definition File. For a trigger on KILL (and ZKILL), YottaDB/GT.M executes trigger code "before" the KILL operation. For example, a trigger on KILL ^CIF(:,1) might clear old cross references. For a trigger on SET, YottaDB/GT.M executes trigger code "as part of" the SET operation. Within trigger logic, the ISV $ZTOLDVAL provides read access to the value of global node prior to the update and $ZTVALUE provides read/write access to the tentative SET value. This allows you to modify the tentative SET value before YottaDB/GT.M commits it to the database. The term "as part of" means that SET triggers execute intertwined with the SET operation. Although it is not yet committed the database, the tentative new value appears to the process as assigned but the process must SET $ZTVALUE to make any revision to the tentative value, because a SET of the global node would nest the trigger recursively - a pathological condition. YottaDB/GT.M executes SET triggers during a MERGE update where YottaDB/GT.M internally performs a series of SET operations and while performing a $INCREMENT() operation where YottaDB/GT.M internally performs a SET operation.For all triggers, YottaDB/GT.M handles the database update event and the triggered actions as an Atomic (all or nothing) transaction. Because triggers use application code and are always part of an implicit or explicit TP transaction, trigger code must conform to ACID conventions discussed in the TP documentation.

Triggers meet many application needs including (but not limited to) the following:

* **Enforce schema-level consistency**: Since database schema created in a normal M application are implicit, M applications implement logic to maintain and enforce conformance with an application schema. Using triggers to enforce schema-level consistency ensures all processes invoke the code uniformly, and increases code modularity and maintainability.
* **Allow an application to maintain one or more non-primary key indexes**. For example, a trigger on updates to global nodes containing a customer id can maintain an index on the last name.
* **Implement business logic**: For example, an update to an account could automatically trigger updates to related accounts.
* **Reducing replication traffic**: Since the YottaDB/GT.M replication stream carries only the triggering updates, not the triggered updates, triggers reduce network traffic.
* **Automate application defined logging or journaling of updates or maintaining historical records**. Triggers can be used to control these.
* **Implement referential integrity**: For example, a trigger can prevent the posting of a bank transaction for an inactive account and display a rule violation message.
* **Debugging**: Debugging an application with multiple concurrent accesses is hard. You can use triggers to establish "watch points" on global variable updates to trap incorrect accesses. For example, if an application is failing because certain global variable nodes either have incorrect values or when previously set values disappear. A trigger can be used to trap all such accesses.
* **Implement a dataflow based programming paradigm**. Although not a primary goal of the implementation of triggers, you can use them to implement applications that use a dataflow programming paradigm.

-------------------------
Trigger Definition File
-------------------------

A trigger definition file is a text file used for adding new triggers, modifying existing triggers, or removing obsolete triggers. A trigger definition file consists of one or more trigger definitions. A trigger definition includes the following information:

* **Trigger signature**: A trigger signature consists of global variable, subscripts, value, command, and trigger code. YottaDB/GT.M uses a combination of global variable, subscripts, value, and command to find the matching trigger to invoke for a database update.

  * Global Variable: The name of a specific global to which this trigger applies.
  * Subscripts: Subscripts for global variable nodes of the named global, specified using the same patterns as the ZWRITE command.
  * Value: For commands that SET or update the value at a node, YottaDB/GT.M honors an optional pattern to screen for changes to delimited parts of the value. A value pattern includes a piece separator and a list of pieces of interest.
  * Command: There are four commands: SET, KILL, ZTRIGGER, and ZKILL (ZWITHDRAW is identical to ZKILL) the shorter name for the command is used when specifying triggers. MERGE is logically treated as equivalent to a series of SET operations performed in a loop. YottaDB/GT.M handles $INCREMENT() of a global matching a SET trigger definition as a triggering update.
  * Trigger code: A string containing M code that YottaDB/GT.M executes when application code updates, including deletions by KILL and like commands, a global node with a matching trigger. The specified code can invoke additional routines and subroutines.

.. note::
   While YottaDB/GT.M does not restrict trigger code from performing I/O operations, YottaDB/FIS recommends against using OPEN, USE, READ, WRITE and CLOSE within trigger application code. Such operations may be useful for development and diagnostic purposes. However, triggers implicitly run as TP transactions and I/O violates the ACID property of Isolation. In addition, MUPIP has somewhat different I/O handling characteristics than the main YottaDB/GT.M run-time, so I/O within triggers run by MUPIP may behave differently than within the originating application environment.

* **ACID property modifiers for triggered database updates**: Currently, YottaDB/GT.M merely performs a syntax check on this part of a trigger definition. YottaDB/GT.M ensures the triggering database update, and any updates generated by trigger logic executed with transaction semantics. With the VIEW "NOISOLATION" command, YottaDB/GT.M transaction processing has long provided a mechanism for an application to inform the YottaDB/GT.M runtime system that it need not enforce Isolation. In such a case, the application and schema design provides Isolation by ensuring only one process ever updates nodes in a particular global at any given time, say by using $JOB as a subscript. This property anticipates a time when a trigger specification can provide NOISOLATION for particular nodes, in contrast to entire globals, and for every update to that node, in contrast to by process use of a VIEW command. Currently, the YottaDB/GT.M runtime system enforces Consistency for application logic inside a transaction and for triggered updates. This property anticipates a time when a trigger specification permits an application to inform the runtime system the application and schema design ensures appropriate Consistency for a trigger and its logic, thus relieving the YottaDB/GT.M runtime system from that task.

* **Trigger Name**: You can optionally specify a trigger name that uniquely identifies each trigger. YottaDB/GT.M uses a trigger name for error reporting and configuration management of triggers - for example, a ZSHOW "S" reports the name of each trigger on the stack. If you do not specify a trigger name, YottaDB/GT.M automatically generates one using the global name as a base. User-specified trigger names and automatically generated trigger names occupy different name space; both last for the life of the definition. A user-specified trigger name is an alphanumeric string of up to 28 characters. It must start with an alphabetic character or a percent sign (%). For a trigger name, YottaDB/GT.M uses the same naming convention as an M name. In other contexts, YottaDB/GT.M truncates M names at 31 characters. However, YottaDB/GT.M treats a trigger name of over 28 characters as an error. This is because a trigger name uniquely identifies a trigger and truncation may cause duplication.

An automatically generated trigger name is a string comprised of two parts. Using the global name as a base, YottaDB/GT.M takes the first part as an alphanumeric string of up to 21 characters starting with an alphabetic character or a percent sign (%). The trailing part consists of an automatically incremented number in the form of #n# where n is a whole number that monotonically increases from 1 to 999999 that uniquely identifies a trigger for the same update. For example, if no trigger names are specified in the trigger definition file, YottaDB/GT.M automatically generates trigger names Account#1#, Account#2#, and Account#3# for the first three triggers defined for global variable ^Account. An attempt to use automatic assignment for more than a million triggers produces an error. Once the numeric portion of the auto generated names reaches 999999, you must reload all triggers associated with the global variables that use the auto generated name space. At run-time YottaDB/GT.M generates a trailing suffix of a hash-sign (#) followed by up to two characters to ensure that every trigger has a unique designation, even when the environment is complex. The run-time suffix applies to both user-specified and automatically generated trigger names. It helps in differentiating triggers with the same name in different database files.

Suppose you want to set up a trigger called TrigAcct on every s ^Acct("ID") to invoke the routine ^OpenAccount. Your trigger definition file may have an entry like +^Acct("ID") -command=S -xecute="do ^OpenAccount" -name=TrigAcct. The following diagram identifies the different parts of this trigger definition:

.. image:: accttrig.gif

To apply this trigger definition file to YottaDB/GT.M, all you do is to load it using MUPIP TRIGGER -TRIGGERFILE or $ZTRIGGER(). YottaDB/GT.M would invoke trigger name TrigAcct on every SET operation on ^Acct("ID"). Internally, YottaDB/GT.M stores trigger TrigAcct in the same database file where ^Acct is stored. The syntax of an entry in a trigger definition file is:

{-triggername\|-triggername-prefix\*\|-\*\|{+|-}trigvn -commands=cmd[,...] -xecute=strlit1 [-[z]delim=expr][-pieces=[lvn=]int1[:int2][;...]] [-options={[no]i[solation]|[no]c[onsistencycheck]}...] [-name=strlit2]}

**-triggername\|-trigger-name-prefix\*\|-\* .**

-triggername deletes a user-specified trigger name called triggername from the database. -triggername* deletes all those user-defined triggers whose starting name match triggername. -* deletes all triggers; if the MUPIP TRIGGER command does not specify -NOPROMPT , YottaDB/GT.M displays a warning and asks for user confirmation before deleting all triggers. If MUPIP TRIGGER command specifies -NOPROMPT and the definition file includes a -* line, YottaDB/GT.M deletes all the triggers without user confirmation. $ZTRIGGER() performs deletions -NOPROMPT.+triggername issues an error; to add a new user-specified trigger name, use -name=strlit2.

**\{\+\|-\}trigvn**

trigvn is a global node on which you set up a trigger.-trigvn deletes any triggers in the database that match the specified trigger. +trigvn adds or replaces the specified trigger. If the specified trigger exists (with a matching specification), MUPIP TRIGGER or $ZTRIGGER() treats the matching definition as a no-op, resulting in no database update. If you want to specify more than one global node for the same trigger code, the following rules apply:

1. You can use patterns and ranges for subscripts.
2. You can specify a semicolon (;) separated list for subscripts.
3. You can specify a selection list that includes a mix of points, ranges and patterns, but a pattern cannot serve as either end of a range. For example, :,"a":"d";?1U is a valid specification but :,"a":?1A is not.
4. You can specify a local variable name for each subscript. For example instead of ^X(1,:,:), you can specify ^X(1,lastname=:,firstname=:). This causes YottaDB/GT.M to define local variables lastname and firstname to the actual second and third level subscripts respectively from the global node invoking this trigger. The trigger code can then use these variables just like any other M local variable. As described in the Trigger Execution Environment section, trigger code executes in a clean environment - as if all code is preceded by an implicit NEW - the implicit assignments apply only within the scope of the trigger code and don't conflict or affect any run-time code or other triggers.
5. You cannot use the @ operator, unspecified subscripts (for example, ^A() or ^A(:,)) or local or global variable names as subscripts.
6. You cannot use patterns and ranges for the global variable name. Therefore, you cannot set a single trigger for ^Acct*.

In order to account for any non-standard collation, YottaDB/GT.M evaluates string subscript ranges using the global specific collation when an application update first invokes a trigger - as a consequence, it detects and reports range issues at run-time rather than from MUPIP TRIGGER or $ZTRIGGER(), so test appropriately. For example, YottaDB/GT.M reports a run-time error for an inverted subscript range such as (ASCII) C:A.

**-command=cmd**

cmd is the trigger invocation command. Currently, you can specify one or more of S[ET], K[ILL], ZTR[IGGER], or ZK[ILL]. A subsequent YottaDB/GT.M release may support ZTK[ILL] for triggering on descendent nodes of a KILLed ancestor, but, while current versions accept ZTK, they convert it into K. If cmd specifies multiple command values, YottaDB/GT.M treats each M command as a separate trigger. Note that even if you specify both SET and KILL, only one M command matches at any given time. Trigger code is not executed in the following conditions:

* A KILL of a node that does not exist.
* A KILL of a node that has a cmd=ZK trigger, but no cmd=K trigger.
* A ZKILL or ZWITHDRAW of a node that has descendents but no data and a trigger with cmd=ZK.
* The trigger uses the "piece" syntax (described below) and no triggering piece changes in the update.

**-xecute="|<<strlit1"|>>**

strlit1 specifies the trigger code that is executed when an update matches trigvn. If strlit1 is a single line, enclose it with quotes (") and make sure that the quotes inside strlit1 are doubled as in normal M syntax.

If strlit1 is in multiple lines, mark the beginning with << which must immediately follow the = after the -xecute. A newline must immediately follow the <<. >> should mark the end of multiple-line strlit1 and must be at the beginning of a line. The lines in strlit1 follow the standard conventions of a YottaDB/GT.M program, that is, optional label, line start, and M code.

The maximum length of strlit1 (even if multi-line) is 1048576 (ASCII) characters or 4096 DB records, whichever is smaller.

To validate strlit1, MUPIP TRIGGER or $ZTRIGGER() compiles it before applying the trigger definition to the database and issues a TRGCOMPFAIL error if it contains any invalid code.

.. note::
   Trigger compilation detects compilation errors, but not run-time errors. Therefore, you should always test your trigger code before applying trigger definitions to the database. 

.. note::
   As stated in the Trigger Definition File section, the text of trigger code is a part of the trigger signature. If you use two trigger signatures that have the same semantics (global variable, subscript, value, and command) but different text (for example: set foo=$ztoldval, s foo=$ztoldval, and set foo=$ztol), their signatures become different and YottaDB/GT.M treats them as different triggers. YottaDB/FIS recommends you to use comprehensive and strong coding conventions for trigger code or rely on user-specified names in managing the deletion and replacement of triggers.

Example:

.. parsed-literal::
   +^multi -commands=set -name=example -xecute=<<
    do ^test1
    do stop^test2
    >>

**[-pieces=int1[:int2][;...]]**

f cmd is S[et], you can specify an optional piece list sequence where int2>int1 and int1:int2 denotes a integer range from int1 to int2. The trigger gets executed only when any piece from the specified piece list changes. Suppose your trigvn has a list "Window|Chair|Table|Door" and you want to execute the trigger only when the value of the 3rd or 4th piece changes so you might specify the following trigger definition:

.. parsed-literal::
   +^trigvn -commands=S -pieces=3;4 -delim="|" -options=NOI,NOC -xecute="W ""3rd or 4th element updated."""
   GTM>W ^trigvnWindow\|Chair\|Table\|Door\|
   GTM>s $Piece(^trigvn,"|",3)="Dining Table"
   3rd or 4th element updated.

This trigger is not executed if you change the first element. For example:

S $Piece(^trigvn,"|",1)="Chandelier"

does not invoke the trigger.

You can also specify a range for your piece sequence. For example, 3:5;7;9:11 specifies a trigger on pieces 3 through 5,7 and 9 through 11. YottaDB/GT.M merges any overlapping values or ranges - for example, 3:6;7 is the same as 3:7.

**[-[z]delim=expr]**

If cmd is S[ET] , you can specify an optional piece delimiter using -[z]delim=expr where expr is a string literal or an expression (with very limited syntax) evaluating to a string separating the pieces (e.g., "|") in the values of nodes, and is interpreted as an ASCII or UTF-8 string based on the environment variable gtm_chset. To allow for unprintable delimiters in the delimiter expression, MUPIP TRIGGER only accepts $CHAR() and $ZCHAR() and string concatenation (_) as embellishments to the string literals. If zdelim specifies a delimiter, YottaDB/GT.M uses the equivalent of $ZPIECE() to match pieces and to identify changes in $ZTUPDATE() (refer to the ISV description for additional information); otherwise, if delim specifies a delimiter, YottaDB/GT.M uses the equivalent of $PIECE() for the current mode (M or UTF-8). Specifying a delimiter for cmd other than S[ET] or specifying both delim and zdelim for the same trigger each produce an error.

**[-options= {no]i[solation]\|[[no]c[onsistencycheck]}...**

You can specify [NO] ISOLATION or [NO]CONSISTENCYCHECK as a property of the triggered database updates. NOISOLATION is a facility for your application to instruct YottaDB/GT.M where the application logic and database schema take responsibility for ensuring the ACID property of ISOLATION, and that any apparent collisions are purely coincidental from multiple global nodes resident in the same physical block which serves as the YottaDB/GT.M level of granularity in conflict checking. In the current release this trigger designation is notational only - you must still implement NOISOLATION at the process level with the VIEW command, but you can use the trigger designation in planning to move to schema-based control of this facility. NOCONSISTENCYCHECK is a facility for your application to instruct YottaDB/GT.M that application logic and schema take responsibility for ensuring the ACID property of CONSISTENCY. The [NO]CONSISTENCYCHECK feature is not yet implemented and will be made available in a future YottaDB/GT.M release. For now, you can plan to move CONSISTENCY responsibility from your application to a trigger and implement it later when this feature becomes available. Note: -options are not part of the trigger signature and so can be modified without deleting an existing trigger.

**[-name=strlit2]**

strlit2 is a user-specified trigger name. It is an alphanumeric string of up to 28 characters. It must start with an alphabetic character or a percent sign (%). Note: -name is not part of the trigger signature and so can be modified without deleting an existing trigger. Note also that the name can be used to delete a trigger - this alternative avoids potential issues with text variations in the code associated with the -xecute qualifier which is part of the trigger signature when the variations do not have semantic significance.

---------------------------------------
Trigger ISVs Summary
---------------------------------------

The following table briefly describes all ISVs (Intrinsic Special Variables) available for use by application logic using triggers. With the exception of $ZTWORMHOLE they return zero (0) if they have numeric values or an empty string when referenced by code outside of a trigger context. For more comprehensive description and usage examples of these ISVs, refer to “Trigger ISVs”.

+-----------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------+
| Trigger ISV           | Description                                                                                                                                               |
+=======================+===========================================================================================================================================================+
| $ZTNAME               | Within a trigger context, $ZTNAME returns the trigger name. Outside a trigger context, $ZTNAME returns an empty string.                                   |
+-----------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZTDATA               | A fast path alternative to $DATA(@$REFERENCE)#2 for a SET or $DATA(@$REFERENCE) of the node for a KILL update.                                            |
+-----------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZTDELIM              | Within a SET trigger context, $ZTDE[LIM] returns the piece separator, as specified by -delim in the trigger definition. This allows triggers to extract   |
|                       | updated pieces defined in $ZTUPDATE without having the piece separator hard coded into the routine. Outside of a SET trigger context, $ZTDELIM is null.   |
+-----------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZTLEVEL              | Returns the current level of trigger nesting (invocation by an update in trigger code of an additional trigger).                                          |
+-----------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZTOLDVAL             | Returns the prior (old) value of the node whose update caused the trigger invocation or an empty string if node had no value; refer to $ZTDATA to         |
|                       | determine if the node had a data value.                                                                                                                   |
+-----------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZTRIGGEROP           | For SET (including MERGE and $INCREMENT() operations), $ZTRIGGEROP returns the value "S". For KILL, $ZTRIGGEROP returns the value "K". For ZKILL or       |
|                       | ZWITHDRAW, $ZTRIGGEROP returns the value "ZK". For ZTR, $ZTRIGGEROP returns the value "ZTR"                                                               |
+-----------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZTSLATE              | $ZTSLATE allows you to specify a string that you want to make available in chained or nested triggers invoked for an outermost transaction (when a TSTART |
|                       | takes $TLEVEL from 0 to 1).                                                                                                                               |
+-----------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZTVALUE              | For SET, $ZTVALUE has the value assigned to the node which triggered the update. Initially this is the value specified by the explicit (triggering) SET   |
|                       | operation. Modifying $ZTVALUE within a trigger modifies the value YottaDB/GT.M eventually assigns to the node.                                            |
+-----------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZTUPDATE             | For SET commands where the YottaDB/GT.M trigger specifies a piece separator, $ZTUPDATE provides a comma separated list of ordinal piece numbers of pieces |
|                       | that differ between the current values of $ZTOLDVAL and $ZTVALUE.                                                                                         |
+-----------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------+
| $ZTWORMHOLE           | $ZTWORMHOLE allows you to specify a string up to 128KB that you want to make available during trigger execution. You can use $ZTWORMHOLE to supply        |
|                       | application context or process context to your trigger logic. Because $ZTWORMHOLE is retained throughout the duration of the process, you can read/write  |
|                       | $ZTWORMHOLE both from inside and outside a trigger. Note that if trigger code does not reference $ZTWORMHOLE, YottaDB/GT.M does not make it available to  |
|                       | MUPIP (via the journal files or replication stream). Therefore, if a replicating secondary has different trigger code than the initiating primary (an     |
|                       | unusual configuration) and the triggers on the replicating node require information from $ZTWORMHOLE, the triggers on the initiating node must reference  |
|                       | $ZTWORMHOLE to ensure YottaDB/GT.M maintains the data it contains for use by the update process on the replicating node. YottaDB/GT.M allows you to change|
|                       | $ZTWORMHOLE within trigger code so that a triggered update can trigger other updates but because of the arbitrary ordering of triggers matching the same  |
|                       | node (refer to the discussion on trigger chaining below), such an approach requires careful design and implementation.                                    |
+-----------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------+

The Trigger Execution Environment section describes the interactions of the following ISVs with triggers: $ETRAP, $REFERENCE, $TEST, $TLEVEL, and $ZTRAP.

-----------------------------------
Chained and Nested Triggers
-----------------------------------

Triggers are chained or nested when a database update sets off more than one trigger. A nested trigger is a trigger set off by another trigger. YottaDB/GT.M assigns a nesting level to each nested trigger to up to 127 levels. While nested triggers are always Atomic with their triggering update YottaDB/GT.M gives each nested trigger a new trigger context rather than a part of the triggering update. A chained trigger is an arbitrary sequence of matching triggers for the same database update. Consider the following trigger definition entries:

.. parsed-literal::
   +^Acct("ID") -commands=Set -xecute="Set ^Acct(1)=$ZTVALUE+1"
   +^Acct(sub=:) -command=Set -xecute="Set ^X($ZTVALUE)=sub"

This example sets off a chained sequence of two triggers and one nested trigger. On Set ^Acct("ID")=10, YottaDB/GT.M chains together an arbitrary sequence of triggers for ^Acct("ID") and ^Acct(sub:). It is possible for either the ^Acct(sub=:) trigger or the ^Acct("ID") trigger to execute first and the other to follow because the trigger execution sequence is arbitrary. Whenever YottaDB/GT.M invokes the trigger for ^Acct("ID"), the Set ^Acct(1)=$ZTVALUE+1 code sets off the trigger for ^Acct(sub=:) as a nested trigger.

.. note::
   YottaDB/FIS recommends against using chained and nested triggers that potentially update the same piece of a global variable. You should always assess the significance of having chained triggers for a database update especially because of the arbitrary trigger execution order. The following table shows the stacking behavior of some Intrinsic Special Variables in chained and nested triggers.

+----------------------------+------------------------------------+------------------------------------------+
| ISV                        | Chained Triggers                   | Nested Triggers                          |
+============================+====================================+==========================================+
| $REFERENCE                 | Shared                             | Stacked                                  |
+----------------------------+------------------------------------+------------------------------------------+
| $TEST                      | Stacked                            | Stacked                                  |
+----------------------------+------------------------------------+------------------------------------------+
| $ZTVALUE                   | Shared (updatable)                 | Stacked                                  |
+----------------------------+------------------------------------+------------------------------------------+
| $ZTOLDVAL                  | Shared                             | Stacked                                  |
+----------------------------+------------------------------------+------------------------------------------+
| $ZTDATA                    | Shared                             | Stacked                                  |
+----------------------------+------------------------------------+------------------------------------------+
| $ZTSLATE                   | Not Stacked                        | Not Stacked                              |
+----------------------------+------------------------------------+------------------------------------------+
| $ZTRIGGEROP                | Shared                             | Stacked                                  |
+----------------------------+------------------------------------+------------------------------------------+
| $ZTWORMHOLE                | Not Stacked                        | Not Stacked                              |
+----------------------------+------------------------------------+------------------------------------------+
| $ZTLEVEL                   | Shared                             | Stacked                                  |
+----------------------------+------------------------------------+------------------------------------------+
| $ZTUPDATE                  | depends on $ZTVALUE when trigger   | Stacked                                  |
|                            | starts                             |                                          |
+----------------------------+------------------------------------+------------------------------------------+

*Stacked* denotes an ISV whose value is restored at the completion of the trigger.

*Not Stacked* denotes an ISV whole value is retained after the completion of the trigger.

*Shared* denotes an ISV whose value is the same, possibly subject to updates, across chained updates

Note that a trigger that is both nested and chained has the characteristics from both columns - the "Chained" column is really about the relationship between triggers invoked by the same update and the "Nested" is really about the isolation of a trigger from the context that invoked it, whether or not that context is inside the context of another trigger.

--------------------------------
A Simple Example
--------------------------------

This section contains a simple example showing how a YottaDB/GT.M trigger can automatically maintain cross references in response to a SET or KILL operation on ^CIF(ACN,1). It also reinforces the basic trigger concepts explained above. Global nodes in ^CIF(ACN,1) have a structure ^CIF(ACN,1)=NAM|XNAME| where the vertical-bars are delimiters and XNAME is a customer's canonical name (e.g., "Doe, Johnny"). The application schema has one cross reference index, ^XALPHA("A",XNAME,ACN)="". A YottaDB/GT.M trigger specified for ^CIF(:,1) nodes can automatically maintain the cross references.

Using your editor, create a trigger definition file called triggers.trg with the following entry:

+^CIF(acn=:,1) -delim="|" -pieces=2 -commands=SET,KILL -xecute="Do ^XNAMEinCIF"

In this definition: 

* ^CIF - specifies the global variable to which the trigger applies.
* acn=: - in ZWRITE syntax, ":" specifies any value for the first subscript.
* acn= prefix requests YottaDB/GT.M assign the value of the first subscript (ACN) to the local variable acn before invoking the trigger logic.
* 1 - specifies that the trigger matches only if the second subscript is 1 (one).
* -delim="|" - specifies that YottaDB/GT.M use "\|" as the piece separator when checking the value of the node to see whether to invoke the trigger. The use of the keyword delim tells YottaDB/GT.M to use $PIECE() semantics for the value at the node; zdelim, instead, would instruct YottaDB/GT.M to use $ZPIECE() semantics.
* -pieces=2 - specifies that YottaDB/GT.M should only invoke the trigger when the update changes the second piece (XNAME) not for a change to the first piece (NAM), or any other piece without a change to XNAME.
* -commands=SET,KILL - specifies that YottaDB/GT.M invoke the trigger for SET and KILL updates (but not a ZKILL/ZWITHDRAW command).
* -xecute="Do ^XNAMEinCIF" - provides code for YottaDB/GT.M to invoke to perform the trigger logic.

Execute a command like the following:

.. parsed-literal::
   $ mupip trigger -triggerfile=triggers.trg

This command adds a trigger for ^CIF(:,1). On successful trigger load, this command displays an output like the following:

.. parsed-literal::
   File triggers.trg, Line 1: ^CIF trigger added with index 1
   =========================================
   1 triggers added
   0 triggers deleted
   0 trigger file entries not changed
   0 triggers modified
   =========================================

Now, every SET and KILL operation on the global node ^CIF(:,1) executes the routine XNAMEinCIF.

Using your editor, create an M routine called XNAMEinCIF.m with the following code:

.. parsed-literal::
   XNAMEinCIF ; Triggered Update for XNAME change in ^CIF(:,1)
       Set oldxname=$Piece($ZTOLDval,"|",2) Set:'$Length(oldxname) oldxname=$zchar(254); old XNAME 
       Kill ^XALPHA("A",oldxname,acn); remove any old xref 
                                     ; Create a new cross reference if the command is a Set
       Do:$ZTRIggerop="S" 
       . Set xname=$Piece($ZTVALue,"|",2) Set:'$Length(xname) xname=$zchar(254)              ; new XNAME
       . Set^XALPHA("A",xname,acn)=""                                                                                                         ; create new xref
       ;

When the XNAME piece of a ^CIF(:,1) node is SET to a new value or KILLed, after obtaining the values, an unconditional KILL command deletes the previous cross reference index, if it exists. The deletion can be unconditional, because if the node did not previously exist, then the KILL is a no-op. Then, only if a SET invoked the trigger (determined from the ISV $ZTRIGGEROP), the trigger invoked routine creates a new cross reference index node. Note that because YottaDB/GT.M implicitly creates a new context for the trigger logic we do not have to worry about our choice of names or explicitly NEW any variables.

After obtaining the values, an unconditional KILL command deletes the previous cross reference index, if it exists. Then, only if a SET invoked the trigger (determined from the ISV $ZTRIGGEROP), the trigger invoked routine creates a new cross reference index node. Note that because YottaDB/GT.M implicitly creates a new context for the trigger logic we do not have to worry about out choice of names or explicitly NEW any variables.

The following illustration shows the flow of control when the trigger is executed for Set ^CIN(ACN,1)="Paul|John, Doe, Johnny|". The initial value of ^CIN(ACN,1) is "Paul|Doe, John|" and ACN is set to "NY". 

.. image:: setcin.gif

.. note::
   Within trigger context, if you modify the value of $ZTVALUE, YottaDB/GT.M now commits it to the database. YottaDB/GT.M executes all steps as an Atomic (all or nothing) transactional unit.

The following illustration shows the flow of control when the trigger is executed for Kill ^CIN(ACN,1).

.. image:: kcin.gif


