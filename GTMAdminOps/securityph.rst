
.. index::
   YottaDB/ GT.M Security Philosophy

===============================================
Appendix E : YottaDB/GT.M Security Philosophy
===============================================

.. contents::
   :depth: 2

-------------------------
Philosophy
-------------------------

The general YottaDB/GT.M philosophy is to use the security of the underlying operating system, and to neither subvert it nor extend it. The purpose of this document is to discuss the implications of, exceptions to, and limitations of this philosophy (the "security model"). This appendix reflects YottaDB/GT.M as of V5.5-000.

.. note::
    YottaDB/GT.M is not intended to operate robustly on a machine that is potentially subject to direct attack, such as a firewall, or a machine operating in a "DMZ."

++++++++++++++++++++++++++++++
Normal User and Group ID Rule
++++++++++++++++++++++++++++++

YottaDB/GT.M processes run with normal UNIX user and group ids. YottaDB/GT.M has no database daemon that needs to run with elevated privileges. Process code written in M will be able to read a database file if and only if the process has read permission for that database file, and to update that database file if and only if the process has read/write permission for that database file. ( The concept of write-only access to a database file is not meaningful for YottaDB/GT.M).

There are two exceptions to this rule. Also, special mention is made of YottaDB/GT.M triggers, which require awareness of their behavior even though they comply with the Normal User and Group ID Rule.

**Exceptions**

Exceptions to the Normal User and Group IDs Rule exist for:

* Shared Memory when the Buffered Global (BG) access method is used, and
* gtmsecshr.

+++++++++++++++++++++++++++++++++
Shared Memory Exception for BG
+++++++++++++++++++++++++++++++++

With the BG access method, each open database file has a shared memory segment associated with it. This shared memory contains a pool of disk blocks (the global buffers) as well as associated control structures (for example, for concurrency control). Even a process that has read-only permission to the database file requires read-write access to the associated shared memory in order to use the control structures. It is therefore possible for a cached disk block in shared memory to be modified by one process and for the actual write of that dirty block to disk to be performed by another. Thus, a "rogue" process with read-only access to the database file but read-write access to shared memory can modify the cached copy of a disk block and effect a permanent change to the database when that block is written to disk by another process that has read-write access to the database file.

Comments on the Shared Memory Exception for BG:

* This only applies if a mumps process contains non-M code. If a mumps process has only M code, the YottaDB/GT.M run-time environment will not allow a process to modify a database for which it lacks write permission.
* This only applies if a database file has mixed read-only and read-write access, that is, some mumps processes have read-only access and others have read-write access. If all processes have read-only access, although the database may appear to be temporarily modified when copies of blocks in shared memory are modified, the database file on disk cannot be permanently modified because no process will have the required permission to flush dirty blocks to disk.
* Where processes that contain C code and have read-only database access must co-exist with processes that have read-write access, YottaDB/GT.M will only "keep honest processes honest." [See below for recommendations where read-only access is required by processes that cannot be considered trustworthy.]

+++++++++++++++++++++++++++
gtmsecshr Exception
+++++++++++++++++++++++++++

Processes with normal user and group ids do not have adequate permissions to effect necessary GT.M interprocess communication and cleanup after abnormal process termination. A process called gtmsecshr runs as root in order to effect the following functionality:

* Interprocess communication, including sending SIGALARM and SIGCONT between processes where normal UNIX permissions do not permit such signals to be sent.
* Cleanup after processes that terminate abnormally, including removing semaphores, shared memory segments, and flushing database file headers (but not database blocks) from shared memory segments to disk.

Whenever a YottaDB/GT.M process lacks adequate permissions to effect any of the above operations, it automatically invokes gtmsecshr if it is not already running. A complete list of gtmsecshr functionality appears in “gtmsecshr commands” .

In order to run as root, and to be invoked by a process that has normal user and group ids, the invocation chain for gtmsecshr requires an executable image that is owned by root and which has the setuid bit turned on in its file permissions.

Once started and running, gtmsecshr records information in a log file gtm_secshr_log (in a directory specified by $gtm_log), creating it if it does not exist. $gtm_log is inherited from the environment of the YottaDB/GT.M process (mumps, mupip or dse) that first invokes the gtmsecshr process. If the environment variable $gtm_log is undefined, if its value is longer than YottaDB/GT.M can handle, or if it is defined to a value that is not an absolute pathname (starting with a /), $gtm_log is assumed to be the directory /tmp (AIX, GNU/Linux).

Communication between YottaDB/GT.M processes and gtmsecshr uses socket files in $gtm_tmp, which is also inherited from the YottaDB/GT.M process that first invokes gtmsecshr. If the environment variable $gtm_tmp is undefined, if its value is longer than YottaDB/GT.M can handle, or if it is defined to a value that is not an absolute pathname (starting with a /), $gtm_tmp is assumed to be the directory /tmp (AIX, GNU/Linux).

The gtmsecshr process receives messages via a socket file owned by root with a name of the form gtm_secshrnnnnnnnn, the nnnnnnnn being replaced by the hexadecimal ftok value of the gtmsecshr executable file. This value is reported by the YottaDB/GT.M ftok utility on the gtmsecshr file, for example, $gtm_dist/ftok $gtm_dist/gtmsecshr

YottaDB/GT.M processes receive responses from gtmsecshr via socket files owned by the userid of the process with names of the form gtm_secshrnnnnnnnn, where nnnnnnnn is a hexadecimal version of the client's process id, padded with leading zeroes. When a client process terminates abnormally, or is killed before it cleans up its socket file, it is possible for a subsequent client with the same process id but a different userid to be unable to delete the leftover socket file. In this case, it tries to send a message to gtmsecshr using a slightly modified client socket file of the form gtm_secshrnnnnnnnnx where x starts with "a" whose corresponding socket file does not already exist or is removable by the current client process (if all suffixes "a" through "z" are unavailable, the client process errors out with a "Too many leftover client socket files" message). gtmsecshr recognizes this special modified socket file name, and as part of servicing the client's request deletes the gtm_secshrnnnnnnnn socket file and all gtm_secshrnnnnnnnnx files that exist. The client process expects this file removal and creates a new gtm_secshrnnnnnnnn file for subsequent communications with gtmsecshr.

* When there is no gtmsecshr process running, by starting one up with incorrect values of $gtm_log and $gtm_tmp, a gtmsecshr process can be made to create a file called gtm_secshr_log in any directory. Having incorrect values can also interfere with normal YottaDB/GT.M operations until the incorrect gtmsecshr process times out and terminates, because YottaDB/GT.M processes and gtmsecshr will be unable to communicate with one another.
* gtmsecshr can be made to delete client socket files by a rogue process. If a socket file is deleted under a running YottaDB/GT.M process, gtmsecshr will be unable to reply to the process. It will timeout, create another and proceed. Thus, while YottaDB/GT.M performance of a single process may temporarily be slowed, system operation will not be disrupted.

**Triggers**

A YottaDB/GT.M trigger is a code fragment stored in the database file that all processes performing a matching update to a global variable in that file execute automatically, for example, to maintain cross-reference indices and referential integrity. Any process that has read-write permission for a database file can change the triggers in that database file, which can in turn force other processes updating that database to execute the changed triggers.

++++++++++++++++++++++++++
Recommendations
++++++++++++++++++++++++++

Based on the security model, the following are recommended best practices for securing YottaDB/GT.M:

* Secure the machine on which YottaDB/GT.M operates behind layers of defenses that permit only legitimate accesses.
* Restrict access to a system on which YottaDB/GT.M runs to those who legitimately need it.
* If not all users who have access to a system require the ability to run YottaDB/GT.M, limit access to YottaDB/GT.M to a group to which all users who need access belong, and remove world access to YottaDB/GT.M.( The GT.M installation script presents an option to restrict access to GT.M to members of a group). If such a group is called gtmusers, the following command executed as root will accomplish this, if access was not restricted when YottaDB/GT.M was installed: 
  
  .. parsed-literal::
      chgrp -R gtmusers $gtm_dist ; chmod -R o-rwx $gtm_dist

* Ensure that database file ownership (user and group), UNIX user and group ids, and permissions at the UNIX level match the intended access. If finer grained access controls than those provided by user and group ids and permissions are needed, consider using, where appropriate and available, security products layered on top of the operating system.
* Under typical conditions, YottaDB/GT.M shared resources - journal files, shared memory, and semaphores - have the same group ids and access permissions as their database files, but may not be owned by the same userid, since the process creating the shared resource may have a different userid from the one that created the database. There are two edge cases to consider:

  * Where the owner of the database file is not a member of the group of the database file, but is a member of the group YottaDB/GT.M's libgtmshr.so file. In this case, if a process with a userid other than the owner were to create a shared resource, a process with the userid of the owner would not have access to them. Therefore, YottaDB/GT.M uses the group id of the libgtmshr.so file if the process creating the shared resource is also a member of that group. In this case it would also restrict access to the resource to members of that group. If the process creating this resource is not a member of the libgtmshr.so group, the group id of the shared resource remains that of the creating resource but the permissions allow world access. YottaDB/FIS advises against using a database file whose owner is not a member of the group of that file.
  * Where the owner of the database file is neither a member of the group nor a member of the group of libgtmshr.so. In this case, YottaDB/GT.M uses world read-write permissions for the shared resources. YottaDB/FIS advises against the use of a database file whose owner is neither a member of the group of the file nor a member of the group of libgtmshr.so.


* The Mapped Memory (MM) access method does not use a shared memory segment for a buffer pool for database blocks - shared memory is used only for control structures. Therefore, consider using MM if there are processes that are are not considered trustworthy but which need read-only access to database files. Even with MM, processes that have read-only access to the database file still have read-write access to the control structures (for example, for M locks). It is conceivable that a rogue process with read-only access may somehow place information in the control structures (for example, bad M lock information) to induce a normal process with read-write access to record inconsistent information in the database.
* If MM cannot be used, and processes that are not considered trustworthy need read-only access to database files, run those processes on a replicating instance specifically set up for that purpose.
* If a database file does not change during normal operation (for example, it contains configuration parameters), make its permissions read only for everyone. On rare occasions when they need to be changed, shut down the application to get stand-alone access, temporarily make it read-write, make the changes, and then make it read-only once more.
* YottaDB/GT.M by default uses a wrapper for gtmsecshr. Source code for the wrapper is published. If processes that startup gtmsecshr cannot be trusted or coerced to have the correct values of $gtm_log and $gtm_tmp, modify the source code to set $gtm_log and $gtm_tmp to required values, recompile and reinstall your modified wrapper.
* Consider implementing layered security software if it exists for your platform, for example, SELinux, Trusted AIX.

.. note::
   YottaDB/FIS neither endorses nor has tested any specific layered security product.

------------------------------
gtmsecshr Commands
------------------------------

+---------------------+-----------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------+
| Commands            | Action                                                                            | Comments                                                                                                       |
+=====================+===================================================================================+================================================================================================================+
| WAKE_MESSAGE        | Sends SIGALRM to specified process.                                               | Used to inform receiving process that a resource (such as a critical section) it awaits has become available.  |
+---------------------+-----------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------+
| CONTINUE_PROCESS    | Sends SIGCONT to specified process.                                               | Used to awake a process that has been suspended while holding a resource. (Please do not ever suspend a        |
|                     |                                                                                   | YottaDB/GT.M process. In the event YottaDB/GT.M finds a process suspended while holding a resource, it is sent |
|                     |                                                                                   | a SIGCONT.                                                                                                     |
+---------------------+-----------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------+
| CHECK_PROCESS_ALIVE | Test sending a signal to specified process. (no longer needed)                    | Used to determine if a process owning a resource still exists; if not, the resource is available to be grabbed |
|                     |                                                                                   | by another process that needs it.                                                                              |
+---------------------+-----------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------+
| REMOVE_SEM          | Remove a specified POSIX semaphore.                                               | Used to remove an abandoned semaphore (for example, if the last attached process terminated abnormally).       |
+---------------------+-----------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------+
| REMOVE_SHMMEM       | Remove a specified shared memory segment.                                         | Used to remove an abandoned shared memory segment. Before removing the segment, gtmsecshr checks that there are|
|                     |                                                                                   | no processes attached to it.                                                                                   |
+---------------------+-----------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------+
| REMOVE_FILE         | Remove a specified file.                                                          | Used to remove an abandoned socket file (for example, as a result of abnormal process termination) used for    |
|                     |                                                                                   | interprocess communication on platforms that do not support memory semaphores (msems); unused on other         |
|                     |                                                                                   | platforms. Before removal, gtmsecshr verifies that the file is a socket file, in directory $gtm_tmp, and its   |
|                     |                                                                                   | name matches YottaDB/GT.M socket file naming conventions.                                                      |
+---------------------+-----------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------+
| FLUSH_DB_IPCS_INFO  | Writes file header of specified database file to disk.                            | The ipc resources (shared memory and semaphore) created for a database file are stored in the database file    |
|                     |                                                                                   | header. The first process opening a database file initializes these fields while the last process to use the   |
|                     |                                                                                   | database clears them. If neither of them has read-write access permissions to the database file, they set/reset|
|                     |                                                                                   | these fields in shared memory and gtmsecshr will write the database file header from shared memory to disk on  |
|                     |                                                                                   | their behalf.                                                                                                  |
+---------------------+-----------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------+

------------------------------------------
Shared Resource Authorization Permissions
------------------------------------------

YottaDB/GT.M uses several types of shared resources to implement concurrent access to databases. The first YottaDB/GT.M process to open a database file creates IPC resources (semaphores and shared memory) required for concurrent use by other YottaDB/GT.M processes, and in the course of operations YottaDB/GT.M processes create files (journal, backup, snapshot) which are required by other YottaDB/GT.M processes. In order to provide access to database files required by M language commands and administration operations consistent with file permissions based on the user, group and world classes, the shared resources created by YottaDB/GT.M may have different ownership, groups and permissions from their associated database files as described below. As an example of the complexity involved, consider a first process opening a database based on its group access permissions. In other words, the database file is owned by a different userid from the semaphores and shared memory created by that first process. Now, if the userid owning the database file is not a member of the database file's group, a process of the userid owning the database file can only have access to the shared resources if the shared resources have world access permissions or if they have a group that is guaranteed to be shared by all processes accessing the database file, even if that group is different from the database file's own group. Again, although YottaDB/FIS strongly recommends against running YottaDB/GT.M processes as root, a root first process opening the database file must still be able to open it although it may not be the owner of the database file or even in its group - but it must ensure access to other, non-root processes. Some things to keep in mind:

* Even a process with read-only access to the database file requires read-write access to the shared memory control structures and semaphores.
* Creating and renaming files (for example, journal files) requires write access to both the files and the directories in which they reside.
* If you use additional layered security (such as Access Control Lists or SELinux), you must ensure that you analyze these cases in the context of configuring that layered security.

YottaDB/GT.M takes a number of factors into account to determine the resulting permissions:

* The owner/group/other permissions of the database file or object directory
* The owner of the database file or object directory
* The group of the database file or object directory
* The group memberships of the database file's or object directory's owner
* The owner/group/other permissions of the libgtmshr file
* The group of the libgtmshr file
* The effective user id of the creating process
* The effective group id of the creating process
* The group memberships of the creating process' user

The following table describes how these factors are combined to determine the permissions to use:

+---------------------------+-------------------------------+------------------------------------+---------------------------------+-------------------------------------------------------------------+
| Database File* Permissions| Opening Process is owner of   | Owner is member of group of        | Opening Process is a member of  | Execution of YottaDB/GT.M restricted to members of a group?       |
|                           | database file* ?              | database file* ?                   | database file* group?           |                                                                   |
+===========================+===============================+====================================+=================================+===================================================================+
| **Group of Resource**                                     | **IPC Permissions** \*\*                                             | **File Permissions** \*\*\*                                       |
+---------------------------+-------------------------------+------------------------------------+---------------------------------+-------------------------------------------------------------------+
| -r--r--rw-                | N                             | Y                                  | N                               | N                                                                 |
+---------------------------+-------------------------------+------------------------------------+---------------------------------+-------------------------------------------------------------------+
| Current Group of Process                                  |  -rw-rw-rw-                                                          |  -rw-rw-rw-                                                       |
+---------------------------+-------------------------------+------------------------------------+---------------------------------+-------------------------------------------------------------------+
| -\*--rw----               | N                             | Y                                  | Y                               | \-                                                                |
+---------------------------+-------------------------------+------------------------------------+---------------------------------+-------------------------------------------------------------------+
| Group of Database File                                    |  -rw-rw----                                                          |  -rw-rw----                                                       |
+---------------------------+-------------------------------+------------------------------------+---------------------------------+-------------------------------------------------------------------+
| -r*-r*-r*-                | \-                            | \-                                 | Y                               | \-                                                                |
+---------------------------+-------------------------------+------------------------------------+---------------------------------+-------------------------------------------------------------------+
| Group of Database File                                    |  -rw-rw-rw                                                           |  -r*-r*-r*                                                        |
+---------------------------+-------------------------------+------------------------------------+---------------------------------+-------------------------------------------------------------------+
| -rw-rw-r*                 | \-                            | \-                                 | N                               | \-                                                                |
+---------------------------+-------------------------------+------------------------------------+---------------------------------+-------------------------------------------------------------------+
| Current Group of Process                                  |  -rw-rw-rw                                                           |  -rw-rw-rw                                                        |
+---------------------------+-------------------------------+------------------------------------+---------------------------------+-------------------------------------------------------------------+
| -rw-rw-rw                 | \-                            | \-                                 | N                               | \-                                                                |
+---------------------------+-------------------------------+------------------------------------+---------------------------------+-------------------------------------------------------------------+
| Current Group of Process                                  |  -rw-rw-rw                                                           |  -rw-rw-rw                                                        |
+---------------------------+-------------------------------+------------------------------------+---------------------------------+-------------------------------------------------------------------+
| -rw-rw-rw                 | Y                             | Y                                  | \-                              | \-                                                                |
+---------------------------+-------------------------------+------------------------------------+---------------------------------+-------------------------------------------------------------------+
| Group of Database File                                    |  -rw-rw-rw                                                           |  -r*-r*----                                                       |
+---------------------------+-------------------------------+------------------------------------+---------------------------------+-------------------------------------------------------------------+
|  -r*-r*----               | Y                             | N                                  | \-                              | N                                                                 |
+---------------------------+-------------------------------+------------------------------------+---------------------------------+-------------------------------------------------------------------+
| Current Group of Process                                  |  -rw-rw-rw-                                                          |  -rw-rw-rw-                                                       |
+---------------------------+-------------------------------+------------------------------------+---------------------------------+-------------------------------------------------------------------+
| -r*-r*----                | Y                             | N                                  | \-                              | Y                                                                 |
+---------------------------+-------------------------------+------------------------------------+---------------------------------+-------------------------------------------------------------------+
| Group to which YottaDB/GT.M is restricted                 |  -rw-rw----                                                          |  -rw-rw----                                                       |
+---------------------------+-------------------------------+------------------------------------+---------------------------------+-------------------------------------------------------------------+
| -r*-r*----                | \-                            | Y                                  | \-                              | \-                                                                |
+---------------------------+-------------------------------+------------------------------------+---------------------------------+-------------------------------------------------------------------+
| Group of Database File                                    |  -rw-rw----                                                          |  -r*-r*----                                                       |
+---------------------------+-------------------------------+------------------------------------+---------------------------------+-------------------------------------------------------------------+
|  -r*-r*----               | \-                            | N                                  | \-                              | N                                                                 |
+---------------------------+-------------------------------+------------------------------------+---------------------------------+-------------------------------------------------------------------+
| Group of Database File                                    |  -rw-rw-rw-                                                          | -rw-rw-rw-                                                        |
+---------------------------+-------------------------------+------------------------------------+---------------------------------+-------------------------------------------------------------------+
| -r*-r*----                | \-                            | N                                  | \-                              | Y                                                                 |
+---------------------------+-------------------------------+------------------------------------+---------------------------------+-------------------------------------------------------------------+
| Group to which YottaDB/GT.M is restricted                 |  -rw-rw----                                                          |  -rw-rw----                                                       |
+---------------------------+-------------------------------+------------------------------------+---------------------------------+-------------------------------------------------------------------+
| ----r*----                | \-                            | N                                  | \-                              | \-                                                                |
+---------------------------+-------------------------------+------------------------------------+---------------------------------+-------------------------------------------------------------------+
| Group of database file                                    |  -rw-rw----                                                          |  ----r*----                                                       |
+---------------------------+-------------------------------+------------------------------------+---------------------------------+-------------------------------------------------------------------+
| -r*-------                | Y                             | \-                                 | \-                              | \-                                                                |
+---------------------------+-------------------------------+------------------------------------+---------------------------------+-------------------------------------------------------------------+
| Current Group of Process                                  |  -rw-------                                                          |  -rw-------                                                       |
+---------------------------+-------------------------------+------------------------------------+---------------------------------+-------------------------------------------------------------------+

**For Autorelink permissions:**

\* : Routine directory

\*\* : rtnobj shared memory and relinkctl shared memory permissions. Note that rtnobj shared memory permissions have the x bit set wherever r or w are set.

\*\*\* : relinkctl file permissions 

* The resulting group ownership and permissions are found by matching the database file permissions, then determining which question columns produce the correct "Y" or "N" answer; "-" answers are "don't care".
* An asterisk ("*") in the Database File Permissions matches writable or not writable. An asterisk in the Resulting File Permissions means that YottaDB/GT.M uses the write permissions from the database file.
* YottaDB/GT.M determines group restrictions by examining the permissions of the libgtmshr file. If it is not executable to others, YottaDB/GT.M treats it as restricted to members of the group of the libgtmshr file.
* Group membership can either be established by the operating system's group configuration or by the effective group id of the process.
* A YottaDB/GT.M process requires read access in order to perform write access to database file - a file permission of write access without read access is an error.
* YottaDB/GT.M treats the "root" user the same as other users, except that when it is not the file owner and not a member of the group, it is treated as if it were a member of the group.
* "Execution of YottaDB/GT.M restricted to members of a group" may remove "other" permissions.



