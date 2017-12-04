.. header::
   YottaDB- GT.M - About GT.M

.. footer::
   Page ###Page### of ###Total###

.. index::
   GT.M - About GT.M

==========================
GT.M Ch 1. About GT.M
==========================

.. contents::
   :depth: 2

GT.M is a high-end performance database application development and deployment platform from Fidelity National Information Services (FIS). GT.M provides an M language subsystem, a database engine, and a complete set of utilities for administration and operation. One can install and manage GT.M using the utilities described in this manual and standard operating system tools. This chapter provides an overview of the GT.M system environment and utilities. 

--------------------------------------
Hardware/ Operating System Environment
--------------------------------------

GT.M runs on a variety of UNIX/Linux implementations. Consult FIS for currently supported versions. Each GT.M release is extensively tested by FIS on a set of specific versions of operating systems on specific hardware architectures (the combination of operating system and hardware architecture is referred to as a platform). This set of specific versions is considered Supported. There will be other versions of the same operating systems on which a GT.M release may not have been tested, but on which the FIS GT.M support team knows of no reason why GT.M would not work. This larger set of versions is considered Supportable. There is an even larger set of platforms on which GT.M may well run satisfactorily, but where the FIS GT.M team lacks the knowledge to determine whether GT.M is Supportable. These are considered Unsupported. Contact FIS GT.M Support with inquiries about your preferred platform.

System requirements vary widely depending on application needs, and should be empirically determined for each situation. 

.. note::
   The 32-bit version of GT.M is compiled to require the 586 ("Pentium compatible") instruction set. 

------------
Installation
------------

GT.M installation is semi-automatic, using the system administration scripts provided with GT.M. The installation procedure is described in Chapter 2: “Installing GT.M”. 

--------
Security
--------

Users require no special privileges to run GT.M beyond standard system access and suitable access to relevant application and database files. All the standard UNIX security features are available to protect GT.M resources.

FIS strongly recommends a security design where each user has no more authorizations than they require to do their assigned roles and each user's actions are distinguishable in an audit.

.. note::
  Root or superuser access is required to install GT.M, but not to use it. FIS recommends against routine use of the root userid to run GT.M.

-------------------------------
Program Development Environment
-------------------------------

GT.M provides a compiler and run-time environment for the M language. Program development uses standard editors and utility programs.

GT.M requires minimal administrative effort.

M routines are stored as text files and object files in the native file system. Compilation of source text into object code is typically automatic, but may be invoked explicitly. A user may store routines in multiple libraries and/or directories organized into search hierarchies, and change the search paths as needed.

For more information on the GT.M language and programming environment, see GT.M Programmer's Guide.

------------------
Database Subsystem
------------------

The GT.M database subsystem consists of a run-time library and a set of utilities which operate on one or more user-specified Global Directories (GD) and database files. GT.M stores M global variables in database files, which are ordinary UNIX files. Internally, the UNIX files are organized as balanced trees (B-trees) in GT.M Data Structures (GDS). See "GT.M Database Structure" chapter for more information on B-trees and the GDS file structure.

A directory maps global names to a database file. GT.M processes use this mapping when storing and retrieving globals from the database. Multiple global directories can reference a single database file, and a database file can be referenced in multiple global directories, with some exceptions, as discussed in Chapter 4: “Global Directory Editor”. Use the Global Directory Editor (GDE) to create and maintain global directories.

In addition to mapping global variables to database files, global directories also store initial parameters used by the MUPIP CREATE command when creating new database files. GT.M uses environment variables to locate the global directory or, optionally database files.

---------------------
GT.M Utility Programs
---------------------

GT.M provides utility programs to administer the system. Each utility is summarized below, and described later in this manual.

~~~~~~
1. GDE
~~~~~~

The Global Directory Editor (GDE) is a GT.M utility program that creates and maintains global directories. GDE provides commands for operating on the global directory.

~~~~~~~~
2. MUPIP
~~~~~~~~

MUPIP (M Peripheral Interchange Program) is the GT.M utility program for general database operations, GT.M Journaling, Multi-site Database Replication, and some non-database operations. 

~~~~~~
3. LKE
~~~~~~

The M Lock Utility (LKE) is the GT.M utility program that examines and modifies the lock space where GT.M maintains the current M LOCK state. LKE can monitor the locking mechanism and remove locks. See Chapter 8: “M Lock Utility (LKE)” for more information.

~~~~~~
4. DSE
~~~~~~

The Database Structure Editor (DSE) is the GT.M utility program to examine and alter the internal database structures. DSE edits GT.M Database Structure (GDS) files. It provides an extensive database "patch" facility (including block integrity checks), searches for block numbers and nodes, and provides symbolic examination and manipulation facilities. See Chapter 10: “Database Structure Editor” for more information.

~~~~~~~~~~~~~~~~~~~~~
5. Command Qualifiers
~~~~~~~~~~~~~~~~~~~~~

ach utility program has its own set of commands. Qualifiers are used as arguments for a command. A qualifier is always prefixed with a hypen (-). Some qualifier allow assigning values with an equal (=) sign where as some allow the use of sub-qualifiers as their arguments. If you specify the same qualifier more than once, MUPIP, DSE, and LKE acts upon the qualifier that appears latest. However, you cannot specify qualifiers that have sub-qualifiers more than once. With GDE, specifying the same qualifier more than once produces an error.

------------------
Database Integrity
------------------

GT.M tools verify and maintain database integrity. As described in Chapter 11: “Maintaining Database Integrity”, database integrity refers to a state of logical and physical consistency in the database when all of the globals and pointers are correct, thereby making all data accessible. Chapter 11 describes how to use the MUPIP INTEG command and the DSE utility to detect and repair integrity problems, and supplies procedures for avoiding such problems.

--------------------------
Interprocess Communication
--------------------------

GT.M uses UNIX Interprocess Communication (IPC) resources to coordinate access to the database. Additionally, GT.M includes a daemon process gtmsecshr that implements process wake-up for M locks and clean-up of IPC resources after certain types of abnormal process termination. See Appendix A: “GT.M's IPC Resource Usage” for more information.
