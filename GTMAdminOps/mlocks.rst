
.. index::
   M Lock Utility (LKE)

====================================
M Lock Utility (LKE)
====================================

.. contents::
   :depth: 2

-----------------------------
Introduction
-----------------------------

The M Lock Utility (LKE) is a tool for examining and changing the YottaDB/GT.M LOCK environment. For a description of M LOCKs, refer to the LOCKs section in the General Language Features of M chapter and the description of the LOCK command in the Commands chapter of the Programmer's Guide.

The two primary functions of the M Lock Utility (LKE) are:

1. SHOW all or specified LOCKs currently active

2. CLEAR all or specified LOCKs currently active

When debugging an M application, you may use LKE to identify a possible deadlock situation, that is, two or more processes have LOCKs and are waiting to add resource names LOCKed by the other(s).

.. parsed-literal::
   Process 1   Process 2
   LOCK A     
            LOCK B
            LOCK +A
   LOCK +B

Process 1 has A LOCKed and attempts to LOCK B. Process 2 has B LOCKed and attempts to LOCK A. Because these processes do not release their current LOCKs before adding additional LOCKs, nor do they provide a timeout to detect the problem, they are deadlocked. Neither process can proceed normally. You can use LKE to release one of the LOCKs so both processes may execute. However, because releasing a LOCK may cause the process to violate its design assumptions, terminating one process is generally a safer way to break the deadlock.

.. note::
   When a process leaves M, YottaDB/GT.M normally releases any LOCKs or ZALLOCATEs held by that process. If a YottaDB/GT.M process terminates abnormally, or if the system "crashes" while a YottaDB/GT.M process is active, YottaDB/GT.M cannot perform normal clean-up. However, as soon as any other process waits several seconds for a LOCK owned by a process that no longer exists, YottaDB/GT.M automatically clears the "orphaned" LOCK.

+++++++++++++++++++++++++++++
To Invoke and Exit LKE
+++++++++++++++++++++++++++++

YottaDB/GT.M installation procedure places the LKE utility package in a directory specified by the environment variable gtm_dist.

LKE requires that the environment variable gtmgbldir be defined.

Invoke LKE using the following command at the shell prompt. If this does not work, consult your system manager to investigate setup and file access issues.

.. parsed-literal::
   $gtm_dist/lke LKE>

.. note::
   Always run LKE on the node where the lock is held.

When LKE is ready to accept commands, it displays the LKE> prompt. To leave LKE, enter the EXIT command at the LKE> prompt.

When additional information is entered on the command line after the LKE command, LKE processes the additional information as its command.

.. parsed-literal::
   $gtm_dist/lke show -all

This command displays all current LOCKs and then returns to the shell prompt.

If your LKE argument contains quotes, precede each quote in the argument by a back-slash (\) or enclose the entire argument in a set of quotes (matching single or double). Apply this convention only for those LKE commands that you run from the shell.

.. parsed-literal::
   $gtm_dist/lke show -lock="^Account(\"Name\")"
   $gtm_dist/lke show -lock='^Account("Name")'

Both these commands display the status of LOCK ^Account("Name") in the default region.

++++++++++++++++++++++++++++++++++
To Establish a Global Directory
++++++++++++++++++++++++++++++++++

LKE uses the environment variable gtmgbldir to identify the active global directory. gtmgbldir should be defined by individual users in their login files.

.. parsed-literal::
   $ gtmgbldir=prod.gld 
   $ export gtmgbldir

------------------------------------
LKE Commands and Qualifiers
------------------------------------

The general format of LKE commands is:

.. parsed-literal::
   command [-qualifier[=qualifier-value]]

LKE accepts command and qualifier abbreviations. The section describing each command provides the minimal abbreviation that can be used for that command, and the command qualifiers, if any. YottaDB/FIS recommends the use of a minimum of four characters for key words in scripts to ensure new keywords do not conflict with older scripts.

+++++++++++++
CLEAR
+++++++++++++

Use the CLEAR command to remove active LOCKs.

.. note::
   YottaDB/FIS recommends restricting the use of the LKE CLEAR facility to debugging environments; removing LOCKs in a production environment typically violates application design assumptions and can cause aberrant process behavior. YottaDB/GT.M automatically removes abandoned LOCKs so it is typically safer to MUPIP STOP a process that is inappropriately hanging on to a LOCK.

The format of the CLEAR command is:

.. parsed-literal::
   C[LEAR] [-qualifier...]

The optional qualifiers are:

.. parsed-literal::
   -A[LL] 
   -L[OCK] 
   -[NO]C[RIT] 
   -[NO]EXACT
   -[NO]I[NTERACTIVE] 
   -O[UTPUT]="file-name" 
   -P[ID]=pid
   -R[EGION]=region-name

By default, CLEAR operates interactively (-INTERACTIVE).

Qualifiers for CLEAR:

.. parsed-literal::
   -A[LL]

Specifies all current LOCKs.

* -ALL removes all current LOCKs.

* If used, CLEAR and -REGION qualifier, -ALL removes all LOCKs in that region.

* Issue a CLEAR - ALL only when there are no active YottaDB/GT.M processes using LOCKs, or when you can predict the effect on the application.

* By default, CLEAR -ALL operates interactively (-INTERACTIVE).

.. parsed-literal::
   -[NO]C[RIT]

Allows LKE CLEAR to work even if another process is holding a critical section.

.. note::
   This can damage current LOCKs and the LOCK mechanism. It is intended for use only under the direction of YottaDB/FIS.

By default LKE operates in CRIT mode and ensures a consistent view of LOCKs by using the database critical section(s).

.. parsed-literal::
   -[NO]EXACT

Limits the CLEAR command to the exact resource name specified with -LOCK=resource_name. NOEXACT (the default) treats the specified resource name as a prefix and works not only on it, but also on any of its descendants, since their existence effectively LOCK their parent tree.

.. parsed-literal::
   -L[OCK]=""resource_name""

Unless used with -EXACT, specifies the leading prefix for an implicit wild card search of all locks that start with the resource_name.

* The resource_name is enclosed in two double quotation marks ("" ""). Because M resource names are formatted the same as global nodes with punctuation characters, in this context they are usually enclosed in sets of double quotation marks with string subscripts enclosed in sets of two double quotations.

* When used with CLEAR, -LOCK removes the locks that start with resource_name.

*  When used with SHOW,-LOCK provides a precise way to examine the specified lock.

.. parsed-literal::
   -[NO]I[NTERACTIVE]

Interactively clears one LOCK at a time. LKE displays each current LOCK with the PID of the owner process and prompts for verification that the LOCK should be cleared. LKE retains the LOCK for any response other than Y[ES].

* By default, CLEAR operates interactively (-INTERACTIVE).

* To avoid holding a lock resource too long, LKE skips to the next matching LOCK if there is no operator response for several seconds.

* -NOINTERACTIVE forces the action to take place without user confirmation of each change. Using -NOINTERACTIVE prevents the LKE operator from controlling the LOCK subsystem for potentially long periods of time when many locks are held. To do this, it limits the amount of time it waits for each response.

.. parsed-literal::
   -O[UTPUT]="file-name"

Directs the reporting of all specified LOCKs to a file.

* If you specify an existing file, LKE creates a new version and overwrites that file.

* If file-name has permission issues, OUTPUT reports the cause of the error.

* The -OUTPUT qualifier is compatible with all other qualifiers.

* By default, CLEAR sends output messages to stdout.

.. parsed-literal::
   -P[ID]=pid

Specifies the process identification number that holds a LOCK on a resource name.

* LKE interprets pid as a decimal number.

* PID clears LOCKs held by the process with the specified process identification number.

* Provides a means for directing CLEAR to LOCKs held by a process that is behaving abnormally.

* The -PID qualifier is compatible with all other qualifiers.

.. parsed-literal::
   -R[EGION]=region-name

region-namespecifies the region that holds the locked resource names.

* REGION clears LOCKs mapped by the current global directory to a region specified by the region-name.

* The -REGION qualifier is compatible with all other qualifiers.

* By default, CLEAR -REGION= operates interactively (-INTERACTIVE).

Example:

.. parsed-literal::
   LKE>CLEAR -ALL

This command clears all current LOCKs.

Example:

.. parsed-literal::
   LKE>clear -pid=2325 -interactive

This command presents all LOCKs held by the process with PID equal to 2325. You can choose whether or not to clear each LOCK.

.. parsed-literal::
   LKE>clear -reg=areg -interactive

This command produces an output like the following:

.. parsed-literal::
   AREG ^a Owned by PID= 2083 which is an existing
   process Clear lock ?

Type Yes or Y in response to the prompt.

LKE responds with an informational message:

.. parsed-literal::
   %GTM-S-LCKGONE, Lock removed : ^a

Type Yes or N or No or N until all LOCKs are displayed and acted upon.

.. parsed-literal::
    LKE> clear -pid=4208 -nointeractive

This command clears the lock held by a process with PID 4208. This command produces an output like the following:

.. parsed-literal::
   DEFAULT Lock removed : ^A

Note that -NOINTERACTIVE forced the action without asking for a confirmation.

Example:

.. parsed-literal::
   LKE>clear -lock="^a("b") 
   Clear lock ? y 
   Lock removed : ^a("b") 
   LKE>

This command clears lock ^a("b") in the default region.

Example:

.. parsed-literal::
   LKE>clear -lock="^a" -nointeractive

This command clears all the locks that start with "^a" in the default region. -NOINTERACTIVE qualifier instructs LKE to clear these locks without further user intervention.

Example:

.. parsed-literal::
   LKE>clear -lock="^a" -exact -nointeractive

This command clears lock ^a in the default region. -NOINTERACTIVE instructs LKE to clear lock ^a without further user intervention.

Example:

.. parsed-literal::
   LKE>CLEAR -PID=4109 -LOCK=""^A"" 
   Clear lock ? Y
   Lock removed : ^A 
   LKE>

This command clears LOCK ^A held by process with PID 4109.

++++++++++++++++++++
SHOW
++++++++++++++++++++

Use the SHOW command to get status of the LOCK mechanism and the LOCK database. The format of the SHOW command is:

.. parsed-literal::
   SH[OW] [-qualifier...]

The optional qualifiers are:

.. parsed-literal::
   -A[LL]
   -L[OCK] 
   -[NO]C[RIT] 
   -O[UTPUT]="file-name"
   -P[ID]=pid 
   -R[EGION]=region-name 
   -W[AIT]

* By default, SHOW displays -A[LL].

* The SHOW command reports active LOCKs. Information includes the LOCK resource name and the process identification (PID) of the LOCK owner.

* LKE SHOW displays lock space usage with a message in the form of: "%GTM-I-LOCKSPACEUSE, Estimated free lock space: xxx% of pppp pages." If the lock space is full, it also displays a LOCKSPACEFULL error.

* A LOCK command which finds no room in LOCK_SPACE to queue a waiting LOCK, does a slow poll waiting for LOCK_SPACE to become available. If LOCK does not acquire the ownership of the named resource with the specified timeout, it returns control to the application with $TEST=0. If timeout is not specified, the LOCK command continues to do a slow poll till the space becomes available.

* LOCK commands which find no available lock space send a LOCKSPACEFULL message to the operator log. To prevent flooding the operator log, GT.M suppresses further such messages until the lock space usage drops below 75% full.

* The results of a SHOW may be immediately "outdated" by M LOCK activity.

* If the LOCK is owned by a GT.CM server on behalf of a client GT.M process, then LKE SHOW displays the client NODENAME (limited to the first 15 characters) and clientPID. The client PID (CLNTPID) is a decimal value in UNIX

.. note::
   GT.CM is an RPC-like way of remotely accessing a YottaDB/GT.M database.

.. parsed-literal::
   -ALL

Specifies all current LOCKs.

* -ALL displays all current LOCKs in all regions and information about the state of processes owning these LOCKs.

* The -ALL qualifier is compatible with all other qualifiers.

* When -ALL is combined with -PID or -REGION, the most restrictive qualifier prevails.

* SHOW -ALL and -WAIT displays both -ALL and -WAIT information.

.. parsed-literal::
   -L[OCK]=resource_name

resource_name specifies a single lock.

* The resource_name is enclosed in double quotation marks ("" ""). Because M resource names are formatted the same as global nodes with punctuation characters, in this context they are usually enclosed in sets of double quotation marks with string subscripts enclosed in sets of two double quotations.

* When used with the CLEAR command, the LOCK qualifier removes the specified lock.

* When used with the SHOW command, the LOCK qualifier provides a precise way to examine the specified lock and any descendant LOCKed resources.

.. parsed-literal::
   -[NO]C[RIT]

Allows the SHOW command to work even if another process is holding a critical section.

* By default LKE operates in CRIT mode and ensures a consistent view of LOCKs by using the database critical section(s).

* Use NOCRIT with SHOW only when normal operation is unsuccessful, as NOCRIT may cause LKE to report incomplete or inconsistent information.

.. parsed-literal::
   -O[UTPUT]="file-name"

Directs the reporting of all specified LOCKs to a file.

* If you specify an existing file, LKE creates a new version and overwrites that file.

* The -OUTPUT qualifier is compatible with all other qualifiers.

* By default, the SHOW command send output messages to stdout.

.. parsed-literal::
   -P[ID]=pid

Specifies the process identification number that holds a LOCK on a resource name.

* LKE interprets pid as a decimal number.

* PID displays all LOCKs owned by the specified process identification number.

* The -PID qualifier is compatible with all other qualifiers; the most restrictive of the qualifiers prevails.

* By default, SHOW displays the LOCKs for all PIDs.

.. parsed-literal::
   -R[EGION]=region-name

Specifies the region that holds the locked resource names.

* The REGION qualifier displays LOCKs of that specified region.

* The REGION qualifier is compatible with all other qualifiers; the most restrictive of the qualifiers prevails.

* By default, SHOW displays the LOCKs for all regions.

.. parsed-literal::
   -W[AIT]

Displays the LOCK resource name and the process state information of all processes waiting for the LOCK to be granted.

* SHOW -WAIT does not display the owner of the LOCK.

* SHOW -ALL -WAIT displays both -ALL and -WAIT information.

* When a process abandons a "wait" request, that request may continue to appear in LKE SHOW -WAIT displays. This appearance is harmless, and is automatically eliminated if the YottaDB/GT.M lock management requires the space which it occupies.

Use the following procedure to display all LOCKs active in the database(s) defined by the current global directory.

.. parsed-literal::
   LKE> SHOW -ALL -WAIT

This produces an output like the following:

.. parsed-literal::
   No locks were found in DEFAULT 
   AREG 
   ^a Owned by PID=2080 which is an existing process 
   BREG 
   ^b(2) Owned by PID= 2089 which is a nonexistent process 
   No locks were found in CREG

Example:

.. parsed-literal::
   LKE>SHOW -ALL

This command displays all LOCKs mapped to all regions of the current global directory. It produces an output like the following:

.. parsed-literal::
   DEFAULT
   ^A Owned by PID= 5052 which is an existing process
   ^B Owned by PID= 5052 which is an existing process
   %GTM-I-LOCKSPACEUSE, Estimated free lock space: 99% of 40 pages

Example:

.. parsed-literal::
   LKE>show -lock="^a"(""b"")"

This command shows lock ^a("b") in the default region.

Example:

.. parsed-literal::
   LKE>SHOW -CRIT

This command displays all the applicable locks held by a process that is holding a critical section.

Example:

.. parsed-literal::
   LKE>show -all -output="abc.lk"

This command create a new file called abc.lk that contains the output of the SHOW -ALL command.

Example

.. parsed-literal::
   LKE>show -pid=4109

This command displays all locks held by process with PID 4109 and the total lock space usage.

Example:

.. parsed-literal::
   LKE>show -region=DEFAULT -lock=""^A""

This command displays the lock on ^A in the region DEFAULT. It produces an output like the following:

.. parsed-literal::
   DEFAULT
   ^A Owned by PID= 5052 which is an existing process
   %GTM-I-LOCKSPACEUSE, Estimated free lock space: 99% of 40 pages

+++++++++
EXIT
+++++++++

The EXIT command ends an LKE session. The format of the EXIT command is:

.. parsed-literal::
   E[XIT]

+++++++
HELP
+++++++

The HELP command explains LKE commands. The format of the HELP command is:

.. parsed-literal::
   H[ELP] [options...]

Enter the LKE command for which you want information at the Topic prompt(s) and then press RETURN or CTRL-Z to return to the LKE prompt.

Example:

.. parsed-literal::
   LKE> HELP SHOW

This command displays help for the SHOW command.

++++++++++++
SPAWN
++++++++++++

Use the SPAWN command to create a sub-process for access to the shell without terminating the current LKE environment. Use the SPAWN command to suspend a session and issue shell commands such as ls or printenv.

The format of the SPAWN command is:

.. parsed-literal::
   SP[AWN]

The SPAWN command has no qualifiers.

Example:

.. parsed-literal::
   LKE>spawn

This command creates a sub-process for access to the current shell without terminating the current LKE environment. Type exit to return to LKE.

--------------------
Summary
--------------------

+------------------------------+----------------------------------------------------------+---------------------------------------------------------+
| Command                      | Qualifier                                                | Comments                                                |
+==============================+==========================================================+=========================================================+
| C[LEAR]                      | -ALL, -L[OCK], -[NO]CRIT, -[NO]EXACT, -[NO]I[NTERACTIVE],| Use CLEAR with care and planning.                       |
|                              | -O[UTPUT]=file-name, -P[ID]=pid, -R[EGION]=name          |                                                         |
+------------------------------+----------------------------------------------------------+---------------------------------------------------------+
| E[XIT]                       | None                                                     | \-                                                      |
+------------------------------+----------------------------------------------------------+---------------------------------------------------------+
| H[ELP]                       | [option]                                                 | \-                                                      |
+------------------------------+----------------------------------------------------------+---------------------------------------------------------+
| SH[OW]                       | -ALL, -L[OCK], -[NO]CRIT, -N[OINTERACTIVE],              | \-                                                      |
|                              | -O[UTPUT]=file-name, -P[ID]=pid, -R[EGION]=name, -W[AIT] |                                                         |
+------------------------------+----------------------------------------------------------+---------------------------------------------------------+
| SP[AWN]                      | none                                                     | shellcommand                                            |
+------------------------------+----------------------------------------------------------+---------------------------------------------------------+

-------------------------------
LKE Exercises
-------------------------------

When using M Locks, you must use a well designed and defined locking protocol. Your locking protocol must specify guidelines for acquiring LOCKs, selecting and using timeout, releasing M Locks, defining a lock s
trategy according the given situation, identifying potential deadlock situations, and providing ways to avoid or recover from them. This section contains two exercises. The first exercise reinforces the concepts
 of YottaDB/GT.M LOCKs previously explained in this chapter. The second exercise describes a deadlock situation and demonstrates how one can use LKE to identify and resolve it.

++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Exercise 1: Preventing concurrent updates using M Locks
++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Consider a situation when two users (John and Tom) have to exclusively update a global variable ^ABC.

.. note::
   Transaction Processing may offer a more efficient and more easily managed solution to the issue of potentially conflicting updates. For more information, see General Language Features of M chapter of the Programmer's Guide.

At the prompt of John, execute the following commands:

.. parsed-literal::
   GTM>lock +^ABC

This command places a YottaDB/GT.M LOCK on "^ABC " (not the global variable^ABC). Note: LOCKs without the +/- always release all LOCKs held by the process, so they implicitly avoid dead locks. With LOCK +, a pro
tocol must accumulate LOCKs in the same order ( to avoid deadlocks).

Then execute the following command to display the status of the LOCK database.

.. parsed-literal::
   GTM>zsystem "lke show -all"

This command produces an output like the following:

.. parsed-literal::
   DEFAULT ^ABC Owned by PID= 3657 which is an existing process

Now, without releasing lock^ABC, execute the following commands at the prompt of Tom.

.. parsed-literal::
   GTM>lock +^ABC

This command wait for the lock on resource "^ABC " to be released. Note that that the LOCK command does not block global variable ^ABC in any way. This command queues the request for locking resource "^ABC" in the LOCK database. Note that you can still modify the value of global variable ^ABC even if it is locked by John.

Now, at the prompt of John, execute the following command:

.. parsed-literal::
   GTM>zsystem "LKE -show -all -wait"

This command produces an output like the following:

.. parsed-literal::
   DEFAULT ^ABC Owned by PID= 3657 which is an existing process 
   Request PID= 3685 which is an existing process

This output shows that the process belonging to John with PID 3657 currently owns the lock for global variable ^ABC and PID of Tom has requested the ownership of that lock. You can use this mechanism to create an application logic that adhere to your concurrent access protocols.

++++++++++++++++++++++++++++++++++++++++++++
Exercise 2: Rectifying a deadlock situation
++++++++++++++++++++++++++++++++++++++++++++

Now, consider another situation when both these users (John and Tom) have to update two text files. While an update is in progress, a YottaDB/GT.M LOCK should prevent the other user from LOCKing that file. In some cases, a deadlock occurs when both users cannot move forward because they do not release their current LOCKs before adding additional LOCKs.

A deadlock situation can occur in the following situation:

.. parsed-literal::
   John           Tom 
   LOCK +file_1   LOCK +file_2 
   LOCK +file_2   LOCK +file_1

Here both the users are deadlocked and neither can move forward. Note that a deadlock situation does not actually block the underlying resource.

Let us now create this situation.

At the prompt of John, execute the following commands:

.. parsed-literal::
   GTM>set file1="file_1.txt"
   GTM>lock +file1
   GTM>open file1:APPEND 
   GTM>use file1 
   GTM>write "John",!
   GTM>close file1

Note that John has not released the LOCK on resource "file1".

At the prompt of Tom, execute the following commands:

.. parsed-literal::
   GTM> set file2="file_2.txt" 
   GTM> lock +file2
   GTM> open file2:APPEND 
   GTM> use file2 
   GTM>write "Tom",!
   GTM>close file2

Note that Tom has not released the LOCK on resource "file2".

Now, at the prompt of John, execute the following commands.

.. parsed-literal::
   GTM>set file2="file_2.txt" 
   GTM>lock +file2

The latter command attempts to acquire a lock on resource file2 that is already locked by Tom. Therefore, this results in a deadlock situation. Repeat the same process for Tom and attempt to lock resource file1.

Execute the following command at LKE prompt to view this deadlock situation.

.. parsed-literal::
   LKE>show -all -wait 
   file1 Owned by PID= 2080 which is an existing process 
   Request PID= 2089 which is an existing process 
   file2 Owned by PID= 2089 which is an existing process 
   Request PID=2080 which is an existing process

This shows a deadlock situation where neither user can proceed forward because it is waiting for the other user to release the lock. You can resolve this situation by clearing the locks using the LKE CLEAR -PID command.

.. note::
   Avoid using the  LKE CLEAR command to clear a deadlock in a production environment as it may lead to unpredictable application behavior. Always use the  MUPIP STOP command to clear a deadlock situation in your production environment. However, in a debugging environment, you can use LKE to debug LOCKs, analyze the status of the LOCK database and even experiment with LKE CLEAR.





