
.. index:: About This Manual

=================
About This Manual
=================

.. contents::
    :depth: 2

The YottaDB/GT.M Message and Recovery Procedures Reference Manual describes the meaning of messages issued by YottaDB and GT.M components, including the M compiler, the run-time system, and utility programs. It serves OpenVMS and UNIX versions of YottaDB/GT.M.

This manual is organized alphabetically according to the message identifier. The meaning of each message is provided as well as suggestions for locating and addressing the cause of error-type messages.

------------------
Intended Audience
------------------

This manual is for programmers and system managers who use YottaDB or GT.M.

---------------------
Purpose of the Manual
---------------------

The YottaDB/GT.M Message and Recovery Procedures Reference Manual helps you understand and act on YottaDB and GT.M messages. This manual complements the other YottaDB/GT.M manuals.

-----------------------
How to Use This Manual
-----------------------

The YottaDB/GT.M Message and Recovery Procedures Reference Manual is intended to be used primarily to determine the nature of a message and interpret its meaning. Therefore, messages are listed in alphabetical order, according to the mnemonic that precedes them. Cross references to additional information are provided in individual entries, as appropriate. The manual does not include a table of contents or index.

-------------------------------
Conventions Used in this Manual
-------------------------------

YottaDB/GT.M messages are identified by a signature of the form GTM-s-abcdef where -s- is a severity indicator and abcdef is an identifier. The severity indicators are: -I- for informational messages, -W- for warnings, -E- for errors and -F- for events that cause a YottaDB/GT.M process to terminate abnormally. For more information on monitoring YottaDB/GT.M messages, refer to "Appendix B: Monitoring YottaDB/GT.M Messages" in the Administration and Operations Guide.

Each entry in this manual is presented in the following format as illustrated by the NOPRINCIO message.

.. parsed-literal::
   1. NOTPRINCIO
   2. NOTPRINCIO, Output currently directed to device xxxx
   3. Run Time Warning: This message displays the current device when the process enters Direct Mode and the current device xxxx is not the principal device.
   4. Action: To redirect all I/O to the terminal, note the current device or save it in a temporary variable and USE $P. If you decide to resume program execution, restore the current device with a USE command.

where
 1 indicates the unique mnemonic preceding the error message and is the component by which the entry is alphabetized.
 2 indicates the mnemonic and the actual message that accompanies it.
 3 indicates the GT.M component that generates the message, its severity, and a short description of its implication(s).
 4 suggests action(s) to take when the message appears.

This manual can be used with YottaDB/GT.M on any of its supported platforms. However, because in some instances the suggested actions are more useful when platform-specific information is provided, the following conventions are used, as necessary.

UNIX: The term UNIX is used here in the general sense of all platforms for which GT.M uses a POSIX API. As of this date, this includes: AIX; HP-UX on IA64 and PA-RISC; GNU/Linux on IA64, x86 and x86_64; Solaris on SPARC; z/OS.

Platform Identifier: If an entire statement is either UNIX- or OpenVMS- specific, it is preceded by the phrase "In UNIX," or "In OpenVMS,". The following example illustrates this convention. Action: In OpenVMS, relink your image to include the missing routine. In UNIX, relink your external call descriptor image to include the missing routine.

If a phrase or word is either UNIX- or OpenVMS- specific, it is suffixed by "(UNIX)" or "(VMS)". The following examples illustrates this convention.

    Run Time Error: Strings longer than 32,767 (UNIX) or 65,535 (OpenVMS) bytes have been used with alternative collation and only gtm_ac_xform and gtm_ac_xback are defined in the collation library.
    Compile Time Error: This indicates that translation of the indicated environment variable (UNIX) or logical name (OpenVMS) failed. Examine the accompanying message to find out the failure type and the reason of the error.

Although the terms "host shell command" and "file-specification" have some platform-specification connotations, they are used in their most generic sense throughout this manual. The former describes commands that originate from the host operating system, rather than from YottaDB/GT.M. The latter may refer to a simple file name or a full directory path to that file.

Henceforth, the term "originating instance" is used where previously "primary instance" or "originating primary instance" were used, and the term "replicating instance" is used where previously "secondary instance" and "originating secondary instance" were used. Since it is easier to change documentation than it is to change software, and given our tradition of maintaining compatibility especially for existing programs and scripts, the former terms will remain in the software for a long time to come, even as they are supplemented with the new terms in the code, and replaced in the documentation.

We have changed the terminology because "initiating" and "replicating" describe roles that currently exist and will continue to exist as GT.M evolves, while "primary" and "secondary" are both more and less precise and will become less useful as we add capabilities to YottaDB/GT.M. An "initiating instance" is always the instance that first records a transaction (including non-TP mini-transactions). A "replicating instance" is always following the action of an "originating instance", previously called "root primary". We have called secondaries that replicate propagating primaries, but they are not originating instances.

The circle-shaped icon with the "+" sign denotes messages that newly introduced in or since the last major release.

.. image:: plus.png

The octagon-shaped icon with the "-" sign denotes deprecated messages.

.. image:: deprecated.png

The square-shaped icon with the delta symbol denotes messages revised in or since the last major release.

.. image:: revised.png

At a major release, square-shaped and circle-shaped icons of and since the preceding major release are removed.

---------------------------------
Following Up on Suggested Actions
---------------------------------

When the suggested action is to "Report the error to the group responsible for database integrity at your operation," you may also refer to the Maintaining Database Integrity chapter in the Administration and Operations Guide.

For information about utility-generated messages, refer to the chapter that describes that utility in the Administration and Operations Guide.

The OpenVMS edition of YottaDB/GT.M uses the OpenVMS standard message facility as well as other OpenVMS system components. Therefore, some errors issued by YottaDB/GT.M originate in OpenVMS and are described in this manual.

-----------------
MUPIP LOAD Errors
-----------------

If a MUPIP LOAD error occurs, ensure that the proper media is loaded and that the command input includes the proper file-specification.

If the input file is FORMAT=GO or ZWR, the database should contain the correct content to the point where the failure occurred and should be usable. You can edit and possibly correct the input file.

If the input file is FORMAT=BIN, the database is probably corrupt. Fix the database intergrity issues and EXTRACT the file again.

For more information on LOAD and EXTRACT, refer to the General Database Management chapter in the Administration and Operations Guide.

For information on salvaging damaged extracts, refer to the Maintaining Database Integrity chapter in the Administration and Operations Guide.

For details on the internals of spanning nodes, refer to the YottaDB/GT.M Database Structure (GDS) chapter in the Administration and Operations Guide.

--------------
Plug-In Errors
--------------

The plug-in architecture of YottaDB and GT.M allows you to choose your preferred encryption software. Some plugin errors that you may encounter are as follows:

**Database file <path> not found**

Plugin error: The plugin is unable to find the specified database file.

Action: Verify that the database file exists, the corresponding entry in the master key file points to the database file, and appropriate authorizations exist in the directory path and the database file.

**Encryption handle corrupted**

Plugin error: The plugin detected an internal error.

Action: This error indicates that there is a communication error between GT.M and the gtmcrypt plug-in. Replace the process with undamaged one. Report the entire incident context to your YottaDB or GT.M support channel.

**Encryption key file <path> not found**

Plugin error: The plugin was not able to find the key file on the specified path.

Action: Verify that the master key file entry for this key file points to the correct path. Verify that the key file itself exists. Verify proper authorizations on directory path and file.

**Encryption library has not been initialized**

Plugin error: A gtmcrypt function was called before gtmcrypt_init().

Action: Call gtmcrypt_init() before calling any other encryption functions.

For more information on the plug-in errors and their fixes, see the documentation of your preferred encryption software.

**Appendix B: Reference Implementation Error messages** lists some errors that the YottaDB and GT.M team encountered while testing YottaDB/GT.M's plug-in architecture with `GNU Privacy Guard <http://gnupg.org/>`_, the widely available implementation of Pretty Good Privacy (see "PGP: Pretty Good Privacy" by Simson Garfinkel).

------------------
MUPIP Integ Errors
------------------

Database errors reported by MUPIP INTEG differ in impact and severity. Some require an immediate action to prevent extending the damage, action on other less severe errors may be delayed.

The following table outlines the MUPIP INTEG error messages with their severity using the codes as listed below:

+------------------------+---------------------------------------------------+
| A                      | Access: Prevents Database Access                  |
+------------------------+---------------------------------------------------+
| B                      | Benign: Presents no risk of additional damage and |
|                        | has little or no effect on database performance   |
+------------------------+---------------------------------------------------+
| D                      | Dangerous: Presents a high risk that continuing   |
|                        | updates may cause significant additional damage   |
+------------------------+---------------------------------------------------+
| I                      | Index: If the block is an index block, continuing |
|                        | updates will be quite dangerous: treat as a D; if |
|                        | the block is a datablock, continuing updates can  |
|                        | only cause limited additional damage.             |
+------------------------+---------------------------------------------------+
| T                      | Transient: Usually cleared by an update to the    |
|                        | database.                                         |
+------------------------+---------------------------------------------------+

**MUPIP INTEG Error Messages**

+-------------------+-------------------------------+-----------------------------+---------------------------------+
| Error Name        | Message                       |  Severity                   |    Section *                    |
+===================+===============================+=============================+=================================+
| DBBADKYNM         | Bad Key Name                  |  I                          |    K1                           |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBBADNSUB         | Bad numeric subscript         |  I                          |    K1                           |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBBADPNTR         | Bad pointer value in directory|  D                          |    K4                           |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBBDBALLOC        | Block doubly allocated        |  D                          |    K3                           |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBBFSTAT          | Block busy/free status unknown|  D                          |    M1                           |
|                   | (local bitmap corrupted)      |                             |                                 |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBBNPNTR          | Bit map block number as       |  D                          |    K4                           |
|                   | pointer                       |                             |                                 |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBBPLMGT2K        | Blocks per local map is       |  D                          |    I3                           |
|                   | greater than 2K               |                             |                                 |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBBPLMLT512       | Blocks per local map is less  |  D                          |    I3                           |
|                   | than 512                      |                             |                                 |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBBPLNOT512       | Blocks per local map is not   |  D                          |    I3                           |
|                   | 512                           |                             |                                 |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBBSIZZRO         | Block size equals zero        |  A                          |    I3                           |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBCOMPTOOLRG      | Record has too large          |  I                          |    O2                           |
|                   | compression count             |                             |                                 |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBDATAMX          | Record too large              |  B                          |    O5                           |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBFGTBC           | File size larger than block   |  B                          |    I4                           |
|                   | count would indicate          |                             |                                 |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBFSTBC           | File size smaller than block  |  D                          |    I4                           |
|                   | count would indicate          |                             |                                 |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBFSTHEAD         | File smaller than database    |  A                          |    I3                           |
|                   | header                        |                             |                                 |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBGTDBMAX         | Key larger than database      |  I                          |    K7                           |
|                   | maximum                       |                             |                                 |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBHEADINV         | Header size not valid for     |  A                          |    I3                           |
|                   | database                      |                             |                                 |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBINCLVL          | Block at incorrect level      |  D                          |    O1                           |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBINCRVER         | Incorrect version of GT.M     |  A                          |    I2                           |
|                   | database                      |                             |                                 |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBINVGBL          | Invalid mixing of global names|  D                          |    K3                           |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBKEYGTIND        | Key greater than index key    |  I or B                     |    K2 or O5                     |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBKGTALLW         | Key larger than maximum       |  I                          |    K1                           |
|                   | allowed length                |                             |                                 |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBLOCMBINC        | Local bitmap incorrect        |  B                          |    M1                           |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBLRCINVSZ        | Last record of block has      |  I                          |    K5                           |
|                   | invalid size                  |                             |                                 |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBLTSIBL          | Key less than sibling’s index |  I                          |    K2                           |
|                   | key                           |                             |                                 |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBLVLINC          | Local map block level         |  B                          |    M2                           |
|                   | incorrect                     |                             |                                 |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBMAXKEYEXC       | Maximum key size for database |  D                          |    I3                           |
|                   | exceeds design maximum        |                             |                                 |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBMAXRSEXBL       | Maximum record size for       |  D                          |    I3                           |
|                   | database exceeds what the     |                             |                                 |
|                   | block size can support        |                             |                                 |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBMBMINCFREZ      | Master bit map incorrectly    |  B                          |    M1                           |
|                   | asserts this local map has    |                             |                                 |
|                   | free space.                   |                             |                                 |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBMBPFLDIS        | Master bit map shows this map |  B                          |    M1                           |
|                   | full, in disagreement with    |                             |                                 |
|                   | both disk and INTEG results   |                             |                                 |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBMBPFLDLBM       | Master bit map shows this map |  B                          |    M1                           |
|                   | full, agreeing with disk local|                             |                                 |
|                   | map                           |                             |                                 |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBMBPFLINT        | Master bitmap shows this map  |  B                          |    M1                           |
|                   | full, agreeing with MUPIP     |                             |                                 |
|                   | INTEG                         |                             |                                 |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBMBPFRDLBM       | Master bit map shows this map |  B                          |    M1                           |
|                   | has space, agreeing with disk |                             |                                 |
|                   | local map                     |                             |                                 |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBMBPFRINT        | Master bit map shows this map |  B                          |    M1                           |
|                   | has space, agreeing with MUPIP|                             |                                 |
|                   | INTEG                         |                             |                                 |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBMBPINCFL        | Master bit map incorrectly    |  B                          |    M1                           |
|                   | marks this local map full     |                             |                                 |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBMBSIZMN         | Map block too small           |  B                          |    M2                           |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBMBSIZMX         | Map block too large           |  B                          |    M2                           |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBMBTNSIZMX       | Map block transaction         |  T                          |    I6                           |
|                   | number too large              |                             |                                 |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBMRKBUSY         | Block incorrectly marked busy |  B                          |    M1                           |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBMRKFREE         | Block incorrectly marked free |  D                          |    M1                           |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBMXRSEXCMIN      | Maximum record size for       |  D                          |    I3                           |
|                   | database is less than the     |                             |                                 |
|                   | design minimum                |                             |                                 |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBNOTDB           | File does not have a valid    |  A                          |    I3                           |
|                   | GDS file header               |                             |                                 |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBNOTMLTP         | Block size not a multiple of  |  A                          |    I3                           |
|                   | 512 bytes.                    |                             |                                 |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBRBNLBMN         | Root block number is a local  |  D                          |    K4                           |
|                   | bit map number                |                             |                                 |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBRBNNEG          | Root block number negative    |  D                          |    K4                           |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBRBNTOOLRG       | Root block number greater     |  D                          |    K4                           |
|                   | than last block number in file|                             |                                 |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBREADBM          | Read error on bitmap          |  D                          |    H7                           |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBRLEVLTONE       | Root level less than one      |  D                          |    O1                           |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBRLEVTOOHI       | Root level higher than max    |  D                          |    O1                           |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBSPANCHUNKORD    | Chunk of blocks is out of     |  B                          |    O5                           |
|                   | order                         |                             |                                 |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBSPANGLOINCMP    | Spanning node is missing      |  B                          |    O5                           |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBSVBNMIN         | Start VBN smaller than        |  A                          |    I3                           |
|                   | possible                      |                             |                                 |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBSZGT64K         | Block size greater than 64K   |  A                          |    I3                           |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBTNNEQ           | Current tn and early tn are   |  T                          |    I6                           |
|                   | not equal                     |                             |                                 |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBTNTOOLG         | Block transaction number too  |  T                          |    I6                           |
|                   | large                         |                             |                                 |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBTTLBLK0         | Total blocks equal zero       |  A                          |    I4                           |
+-------------------+-------------------------------+-----------------------------+---------------------------------+
| DBUNDACCMT        | Cannot determine access method|  T                          |    I6                           |
|                   | ; Trying with BG              |                             |                                 |
+-------------------+-------------------------------+-----------------------------+---------------------------------+


.. note::
   Section * refers to the specified section in *Managing Database Integrity* chapter of Administration and Operations Guide. The section details a description along with the action item to be taken on encountering the error message.


