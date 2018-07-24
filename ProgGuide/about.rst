
.. index::
   About YottaDB

===========================
1. About YottaDB
===========================

.. contents::
   :depth: 2

YottaDB runs on a wide variety of computer platforms. Consult YottaDB for the current list of supported platforms.

In addition to preserving the traditional features of M, YottaDB also offers an optimized compiler that produces object code that does not require internal interpreters during execution.

On all platforms, the YottaDB dynamic linking mechanism activates compiled objects. On some platforms, you can link the object modules into shared object libraries.

In keeping with the focus on creating fully compiled code, YottaDB is tightly integrated with the operating system environment and permits the use of operating system utilities for program development.

YottaDB also provides a full complement of M tools for creating, compiling, and debugging source code. Many of these tasks are accomplished from the facility called Direct Mode, which offers the look and feel of an interpreted language that is familiar to the traditional M programmer.

-----------------------------
Programming Environment
-----------------------------

The Programming Environment is described in the following sections.

++++++++++++++++++++++
Managing Data
++++++++++++++++++++++

The scope of M data is either process local or global.

* Local variables last only for the duration of the current session; YottaDB deletes them when the M process terminates.
* Global variables contain data that persists beyond the process. YottaDB stores global variables on disk. A Global Directory organizes global variables and describes the organization of a database. The YottaDB administrator uses the Global Directory Editor (GDE) to create and manage Global Directories. A Global Directory maps global names to a database file. YottaDB uses this mapping when it stores and retrieves globals from the database. Several Global Directories may refer to a single database file.

For more information about the data management system, refer to the `"Global Directory Editor" <https://docs.yottadb.com/AdminOpsGuide/gde.html>`_, `"MUPIP" <https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html>`_ and `"YottaDB Journaling" <https://docs.yottadb.com/AdminOpsGuide/ydbjournal.html>`_ chapters in the Administration and Operations Guide.

**Database Management Utilities**

The Global Directory Editor (GDE) creates, modifies, maintains, and displays the characteristics of Global Directories. GDE also maps LOCKs on resource names to the region of the database specified for the corresponding global variables.

The M Peripheral Interchange Program (MUPIP) creates database files and provides tools for database management and database journaling.

For more information regarding database utilities, refer to the `"Global Directory Editor" <https://docs.yottadb.com/AdminOpsGuide/gde.html>`_, `"MUPIP" <https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html>`_ and `"YottaDB Journaling" <https://docs.yottadb.com/AdminOpsGuide/ydbjournal.html>`_ chapters in the Administration and Operations Guide.

++++++++++++++++++++++
Managing Source Code
++++++++++++++++++++++

In the YottaDB programming environment, source routines are generated and stored as standard UNIX files. They are created and edited with standard UNIX text editors. YottaDB accepts source lines of up to 8192 bytes. When YottaDB encounters a line with a length greater than 8192 bytes in a source file, it emits a %YDB-W-LSEXPECTED warning. This warning identifies cases where a line greater than 8192 bytes is split into multiple lines, which causes statements beyond the character prior to the limit to execute irrespective of any starting IF, ELSE or FOR commands. The 8192 byte limit applies to XECUTE command arguments and Direct Mode input as well.

YottaDB is designed to work with the operating system utilities and enhances them when beneficial. The following sections describe the process of programming and debugging with YottaDB and from the operating system.

**Source File Management**

In addition to standard M "percent" utilities, YottaDB permits the use of the standard UNIX file manipulation tools, for example, the diff, grep, cp, and mv commands. The programmer can also use the powerful facilities provided by the UNIX directory structure, such as time and date information, tree-structured directories, and file protection codes.

YottaDB programs are compatible with most source management software, for example, RCS and SCCS.

**Programming and Debugging Facilities**

The programmer can use any UNIX text editor to create M source files. If you generate a program from within the Direct Mode, it also accesses the UNIX text editor specified by the environment variable EDITOR and provides additional capabilities to automate and enhance the process.

The programmer also uses the Direct Mode facility to interactively debug, modify, and execute M routines. In Direct Mode, YottaDB executes each M command immediately, as if it had been in-line at the point where YottaDB initiated Direct Mode.

The following is a list of additional enhancements available from the Direct Mode:

* The capability to issue commands from Direct Mode to the shell
* A command recall function to display and reuse previously entered commands
* Many language extensions that specifically optimize the debugging environment

**The Compiler**

The compiler operates on source files to produce object files consisting of position-independent, native object code, which on some platforms can be linked into shared object libraries. YottaDB provides syntax error checking at compile-time and allows you to enable or disable the compile-as-written mode. By default, YottaDB produces an object file even if the compiler detects errors in the source code. This compile-as-written mode facilitates a flexible approach to debugging.

**The Run-Time System**

A YottaDB programmer can execute an M routine from the shell or interactively, using the M commands from Direct Mode.

The run-time system executes compile-as-written code as long as it does not encounter the compile-time errors. If it detects an error, the run-time system suspends execution of a routine immediately and transfers control to Direct Mode or to a user-written error routine.

++++++++++++++++++++++++++++++++++
Automatic and Incremental Linking
++++++++++++++++++++++++++++++++++

The run-time system utilizes a YottaDB facility called ZLINK to link in an M routine. When a program or a Direct Mode command refers to an M routine that is not part of the current process, YottaDB automatically uses the ZLINK facility and attempts to link the referenced routine (auto-ZLINK). The ZLINK facility also determines whether recompilation of the routine is necessary. When compiling as a result of a ZLINK, YottaDB typically ignores errors in the source code.

The run-time system also provides incremental linking. The ZLINK command adds an M routine to the current image. This feature facilitates the addition of code modifications during a debugging session. The programmer can also use the feature to add patches and generated code to a running M process.

**Error Processing**

The compiler detects and reports syntax errors at the following times:

* Compile-time - while producing the object module from a source file
* Run-time - while compiling code for M indirection and XECUTEs
* Run-time - when the user is working in Direct Mode.

The compile-time error message format displays the line containing the error and the location of the error on the line. The error message also indicates what was incorrect about the M statement.

YottaDB can not detect certain types of errors associated with indirection, the functioning of I/O devices, and program logic until run-time.

The compile-as-written feature allows compilation to continue and produces an object module despite errors in the code. This permits testing of other pathways through the code. The errors are reported at run-time, when YottaDB encounters them in the execution path.

The run-time system recognizes execution errors and reports them when they occur. It also reports errors flagged by the compiler when they occur in the execution path.

For more information, see `Chapter 13: “Error Processing” <https://docs.yottadb.com/ProgrammersGuide/errproc.html>`_.

**Input-Output Processing**

YottaDB supports input and output processing with the following system components:

* Terminals
* Sequential disk files
* Mailboxes
* FIFOs
* Null devices
* Socket devices

YottaDB input/output processing is device-independent. Copying information from one device to another is accomplished without reformatting.

YottaDB has special terminal-handling facilities. YottaDB performs combined QIO operations to enhance terminal performance. The terminal control facilities that YottaDB provides include escape sequences, control character traps, and echo suppression.

YottaDB supports RMS sequential disk files that are accessed using a variety of deviceparameters.

YottaDB supports block I/O with fixed and variable length records for file-structured (FILES-11) tapes and non-file-structured unlabeled (FOREIGN) tapes. YottaDB supports the ASCII character set for unlabeled FOREIGN and FILES-11 tapes. YottaDB supports the EBCDIC character set for FOREIGN tapes only. YottaDB also supports FOREIGN DOS-11 and ANSI labelled tapes or stream format records. It also supports ASCII and EBCDIC character sets.

YottaDB uses permanent or temporary mailboxes fifos for interprocess communication. YottaDB treats mailboxes as record-structured I/O devices.

YottaDB provides the ability to direct output to a null device. This is an efficient way to discard unwanted output.

YottaDB provides device-exception processing so that I/O exception handling need not be combined with process-related exception conditions. The OPEN, USE, and CLOSE EXCEPTION parameters define an XECUTE string as an error handler for an I/O device.

+++++++++++++++++++++++++++++++++++++++++++++++
Integrating YottaDB with Other Languages
+++++++++++++++++++++++++++++++++++++++++++++++

YottaDB offers capabilities that allow you to optimize your programming environment. These include allowing you to call into M routines from programs written in other programming languages, access your M databases with interfaces that provide functionality equivalent to M intrinsic database functions, and to alter your programming environment when working with languages other than American English. These include allowing you to call programs written in other programming languages that support C-like interfaces and to alter your programming environment when working with languages other than American English. This capability is described in more detail in chapters throughout this manual.

+++++++++++++++++++++++++++++++++
Access to non- M routines
+++++++++++++++++++++++++++++++++

YottaDB routines can call external (non-M) routines using the external call function. This permits access to functions implemented in other programming languages. For more information, see `Chapter 11: “Integrating External Routines” <https://docs.yottadb.com/ProgrammersGuide/extrout.html>`_.

++++++++++++++++++++++++++++++++
Internationalization
++++++++++++++++++++++++++++++++

YottaDB allows the definition of alternative collation sequences and pattern matching codes for use with languages other than English. `Chapter 12: “Internationalization” <https://docs.yottadb.com/ProgrammersGuide/internatn.html>`_ describes the details and requirements of this functionality.


