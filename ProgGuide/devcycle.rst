
.. index::
   Development Cycle

=======================
Development Cycle
=======================

.. contents::
   :depth: 2

This chapter introduces program development in the YottaDB/GT.M environment. The YottaDB/GT.M environment differs from other M implementations in a number of ways. These differences, which include maintaining data and code in separate files and compiling rather than interpreting source code, allow greater programmer control over the development cycle.

In contrast to M environments that interpret M code, YottaDB/GT.M compiles M code from source files into the target machine language. The YottaDB/GT.M compiler produces object files, which are dynamically linked into an image. Source files and object files may be managed independently, or placed together in a specific directory. YottaDB/GT.M permits access to source and object files in multiple directories.

YottaDB/GT.M databases are UNIX files identified by a small file called a Global Directory. Global Directories allow management of the database files to be independent of the placement of files containing M routines. By changing the Global Directory, you can use the same programs to access different databases.

Program development may utilize both YottaDB/GT.M and UNIX development tools. The development methodology and environment chosen for a particular installation, and tailored by the individual user, determines the actual mix of tools. These tools may vary from entirely YottaDB/GT.M with little UNIX, to mostly UNIX with a modest use of YottaDB/GT.M.

Direct Mode serves as an interactive interface to the YottaDB/GT.M run-time environment and the compiler. In Direct Mode, the user enters M commands at the YottaDB/GT.M prompt, and YottaDB/GT.M compiles and executes the command. This feature provides immediate turnaround for rapid program development and maintenance.

This chapter is based on the tasks that a programmer might perform while developing an application. It provides a "road map" for programmers of varying levels. Some steps may be unnecessary in your environment, so feel free to skip sections that do not apply to your situation.

-----------------------------------------
Overview of the Program Development Cycle
-----------------------------------------

This section provides an overview of the steps involved in generating executable programs in YottaDB/GT.M.

The steps begin with your initial use of YottaDB/GT.M. The first two steps are part of your initial setup and will generally be performed only the first time you use YottaDB/GT.M. The remaining steps are those you will use regularly when generating your programs.

Each of these remaining steps can be performed either from the YottaDB/GT.M prompt or the shell prompt. To clearly describe the two ways to perform each step, this section is set up in the format of a table with one column illustrating the YottaDB/GT.M method, and one column illustrating the shell method.

+------------------------------------------------------+-----------------------------------------------+---------------------------------------------------+
| Creating a YottaDB/GT.M Routine                                                                                                                          | 
+======================================================+===============================================+===================================================+
| Define Environment Variables (shell)                 | define                                                                                            |
|                                                      |                                                                                                   |
|                                                      | gtm_dist                                                                                          |
|                                                      |                                                                                                   |
|                                                      | gtmgbldir                                                                                         |
|                                                      |                                                                                                   |
|                                                      | gtmroutines                                                                                       |
+------------------------------------------------------+-----------------------------------------------+---------------------------------------------------+
| Prepare database                                     | define Global Directory with GDE                                                                  |
|                                                      |                                                                                                   |
|                                                      | create database with MUPIP CREATE                                                                 |
+------------------------------------------------------+-----------------------------------------------+---------------------------------------------------+
| \-                                                   | SHELL                                         | YottaDB/GT.M                                      |
+------------------------------------------------------+-----------------------------------------------+---------------------------------------------------+
| Create/Edit Routine                                  | Create file with UNIX editor; assign .m       | ZEDIT "routine" .m extension added by YottaDB/GT.M|
|                                                      | extension                                     |                                                   |
+------------------------------------------------------+-----------------------------------------------+---------------------------------------------------+
| Compile Routine                                      | invoke mumps routine.m                        | ZLINK "routine"                                   |
+------------------------------------------------------+-----------------------------------------------+---------------------------------------------------+
| Execute Routine                                      | invoke mumps -run routine                     | Do ^routine calls from other routines invoke      |
|                                                      |                                               | auto-ZLINK                                        |
|                                                      | calls from other routines invoke auto-ZLINK   |                                                   |
+------------------------------------------------------+-----------------------------------------------+---------------------------------------------------+
| Debug Routine                                        | edit file with UNIX editor; compile and       | utilize debug commands such as:                   |
|                                                      | execute                                       |                                                   |
|                                                      |                                               | ZGOTO                                             |
|                                                      |                                               |                                                   |
|                                                      |                                               | ZLINK                                             |
|                                                      |                                               |                                                   |
|                                                      |                                               | ZMESSAGE                                          |
|                                                      |                                               |                                                   |
|                                                      |                                               | ZPRINT                                            |
|                                                      |                                               |                                                   |
|                                                      |                                               | ZSHOW                                             |
|                                                      |                                               |                                                   |
|                                                      |                                               | ZSTEP                                             |
|                                                      |                                               |                                                   |
|                                                      |                                               | ZSYSTEM                                           |
|                                                      |                                               |                                                   |
|                                                      |                                               | ZWRITE                                            |
|                                                      |                                               |                                                   |
|                                                      |                                               | compile and execute                               |
+------------------------------------------------------+-----------------------------------------------+---------------------------------------------------+

The table is presented as an overview of the YottaDB/GT.M routine generation process, and as a comparison of the available methods. More complete information on each of the steps can be found in the following parts of this manual set.

* Debugging routines: Chapter 4: “Operating and Debugging in Direct Mode”.
* Defining environment variables: “Defining Environment Variables”.
* Defining/creating Global Directories: “Preparing the Database” and the Administration and Operations Guide, "Global Directory Editor" and "MUPIP" chapters.
* Creating/editing routines: “Creating and Editing a Source Program”.
* Compiling routines: “Compiling a Source Program”.
* Executing routines: “Executing a Source Program”.

---------------------------------
Defining Environment Variables
---------------------------------

YottaDB/GT.M requires the definition of certain environment variables as part of setting up the environment. These environment variables are used for the following purposes:

* To locate the files that YottaDB/FIS provides as part of YottaDB/GT.M
* To hold some user-controlled information which YottaDB/GT.M uses for run-time operation

YottaDB/GT.M limits environment variables to 8192 bytes, but items they specify such as a path may have a lower limit.

The procedure below describes how to define an environment variable. Use this procedure to define an environment variable either at the shell prompt or in your shell startup file. If you define the variable at the shell prompt, it will be effective only until you logout. If you define it in your .profile file (.cshrc, if using a C shell variant), it will be in effect whenever you log in. Your system manager may have already defined some of these variables.

.. note::
   Each environment variable required by YottaDB/GT.M is described and illustrated in individual sections following the procedure. Only gtm_dist, and in some cases gtmgbldir, gtm_principal and gtmroutines, are required by users who do not perform programming activities.

To define an environment variable type the following commands:

.. parsed-literal::
   $ env_variable=env_variable_value
   $ export env_variable

The example above may differ from the syntax supported by some shells

The following environment variables hold information that determines some details of YottaDB/GT.M run-time operation, over which the user has control.

+++++++++
gtm_dist
+++++++++

gtm_dist is used to establish the location of the installed YottaDB/GT.M program and support files.

The syntax for gtm_dist is as follows:

.. parsed-literal::
   $ gtm_dist=<distribution-directory>

The standard installation places these files in /usr/lib/fis-gtm or /usr/lib/yottadb.

Example:

.. parsed-literal::
   $ gtm_dist=/usr/lib/fis-gtm/V6.0-002_x86_64
   $ export gtm_dist

This identifies /usr/lib/fis-gtm/V6.0-002_x86_64 as the location of the installed YottaDB/GT.M files.

Add gtm_dist to your PATH environment variable to have UNIX search the YottaDB/GT.M installation directory (when processing a command to activate or run an image). This allows you to activate YottaDB/GT.M and the utilities without explicitly specifying a path.

To add gtm_dist to your PATH type the following commands:

.. parsed-literal::
   $ PATH=$PATH:$gtm_dist
   $ export PATH

.. note::
   Most of the examples in this manual assume that you have added gtm_dist to your PATH.

++++++++++
gtmgbldir
++++++++++

gtmgbldir defines the path to a Global Directory. A Global Directory maps global variables to physical database files, and is required to locate M global variables. gtmgbldir provides the initial value for $ZGBLDIR, the intrinsic special variable that connects the YottaDB/GT.M run-time system to the Global Directory. It also connects the Global Directory to the utilities requiring one.

If you maintain multiple global directories, define gtmgbldir to the Global Directory you currently want to use.

The syntax of a gtmgbldir definition is:

.. parsed-literal::
   $ gtmgbldir=/directory/filename.gld

Example:

.. parsed-literal::
   $ gtmgbldir=/usr/staff/mumps.gld
   $ export gtmgbldir

This specifies /usr/staff as the directory containing the Global Directory file named mumps.gld.

+++++++++++++++
gtm_principal
+++++++++++++++

The gtm_principal environment variable specifies the value for $principal, which designates the absolute pathname of the principal $IO device. This is an MDC Type A enhancement to standard M.

The following is an example of gtm_principal definition:

.. parsed-literal::
   $ gtm_principal=/usr/filename
   $ export gtm_principal

This specifies the /usr/filename as the principal $IO device, effective until changed further or until you logout of the particular session.

+++++++++++++++
gtmroutines
+++++++++++++++

The gtmroutines environment variable specifies a search list of possible locations for M routines. This value is used to initialize $ZROUTINES, (the intrinsic special variable that enables YottaDB/GT.M to find the routine (program) you want to run). gtmroutines is required for ZLINKing. gtmroutines is particularly helpful in calling percent utilities and the Global Directory Editor (GDE), which are in gtm_dist.

.. parsed-literal::
   $ gtmroutines="directories in search list"

The directories in the search list must be separated by a space and enclosed in quotation marks (" "). Environment variables are accepted in the search list.

The following is an example of gtmroutines definition:

.. parsed-literal::
   $ gtmroutines=". $gtm_dist"
   $ export gtmroutines

This specifies that YottaDB/GT.M search for a routine first in the current directory (.), then in the distribution directory (which is identified by the environment variable gtm_dist). The distribution directory is included in the list because it contains the percent routines. You will probably want the search list to contain these two items at a minimum. In addition, you may want to add directories of your own.

For additional information about how YottaDB/GT.M uses the routine search list, see “$ZROutines”.

++++++++++++++++
Editor
++++++++++++++++

The EDITOR environment variable specifies the UNIX text editor used when editing a routine either from the shell or with ZEDIT. Since this is a standard part of establishing your UNIX environment, you will probably only need to define this when you want to use a different editor than the one defined in your shell startup file.

Example:

.. parsed-literal::
   $ EDITOR=/usr/bin/vi
   $ export EDITOR

This defines the current text editor to vi.

--------------------------
Preparing the Database
--------------------------

YottaDB/GT.M databases consist of one or more UNIX files. Most database files have a UNIX file structure externally and a YottaDB/GT.M Database Structure (GDS) internally. Management of the GDS files by the YottaDB/GT.M run-time system assures high performance and integrity. YottaDB/GT.M database files are coordinated by a Global Directory. The Global Directory identifies which global names belong in which files, and specifies the creation characteristics for each file. To specify access to a database, each M process must define the gtmgbldir environment variable to point to the associated Global Directory.

To define and maintain a Global Directory, use the Global Directory Editor (GDE) utility. The GDE utility automatically upgrades existing global directories to the current format. The MUPIP command CREATE uses the characteristics as defined in the Global Directory to create the associated database. In a production environment, the system manager typically maintains Global Directories.

For more information on GDE and MUPIP refer to the "Global Directory Editor" and "MUPIP" chapters in the Administration and Operations Guide.

Example:

This example is a sequence of events that illustrate steps you might typically perform in creating a new global directory, in our example PAYROLL.GLD. 

.. parsed-literal::
   $ ls payroll.gld
   payroll.gld not found

The ls command verifies that there are no existing files with the name payroll.gld.

.. parsed-literal::
   $ gtmgbldir=payroll.gld 
   $ export gtmgbldir

This establishes the current value of the environment variable gtmgbldir as payroll.gld. YottaDB/GT.M uses gtmgbldir to identify the current Global Directory. When defined at the shell prompt, gtmgbldir maintains the defined value only for the current login session. The next time you log into UNIX, you must again define the value of gtmgbldir as payroll.gld to use it as the current Global Directory.

This example defines gtmgbldir without a full pathname. The environment variable points to the payroll.gld file in the current working directory. Therefore if the default directory changes, YottaDB/GT.M attempts to locate the Global Directory in the new default directory and cannot use the original file. If you intend for the Global Directory to consistently point to this file, even if the default directory changes, use a full file-specification for gtmgbldir.

.. parsed-literal::
   $ /usr/lib/fis-gtm/V6.0-0001_x86/gtm
   GTM>do ^GDE
   %GDE-I-LOADGD, Loading Global Directory file 
           /home/jdoe/.fis-gtm/V6.0-001_x86/g/payroll.gld
   %GDE-I-VERIFY, Verification OK
   GDE>

This invokes the Global Directory Editor by entering GDE from the YottaDB/GT.M prompt and produces an informational message.

.. parsed-literal::
   GDE> show all

                              \*\*\* Templates \*\*\*
    Region                        Def Coll    Rec Size   Key Size   Null Subs    Std Null Coll   Journaling
    --------------------------------------------------------------------------------------------------------
    <default>                       0           4080       255       NEVER             Y             Y

                                 Jnl File (def ext: .mjl)    Before   Buff    Alloc   Exten
    ------------------------------------------------------------------------------------------------
    <default>                    <based on DB file-spec>      Y       128     2048    2048


    Segment              Active          Acc   Typ   Block   Alloc   Exten     Options
    --------------------------------------------------------------------------------------
    <default>             *              BG    DYN   4096    5000    10000     GLOB=1000
                                                                               LOCK = 40
                                                                               RES = 0
                                                                               ENCR = OFF
    <default>                            MM    DYN   4096    5000    10000     DEFER
                                                                               LOCK=40


                                \*\*\* Names \*\*\*
    Global                                              Region
    ----------------------------------------------------------------
    *                                                  DEFAULT


                                \*\*\* Regions \*\*\*
    Region          Dynamic Segment     Def Coll    Rec Size   Key Size   Null Subs   Std Null Coll   Journaling
    ---------------------------------------------------------------------------------------------------------------
    DEFAULT         DEFAULT               0          4080       255        NEVER          Y              Y


                                \*\*\* Journaling Information \*\*\*
    Region                          Jnl File (def ext: .mjl)     Before   Buff   Alloc   Exten
    ------------------------------------------------------------------------------------------------------
    DEFAULT                     $gtmdir/$gtmver/g/payroll.mjl    Y        128    2048     2048

                                 
                                 \*\*\* Segments \*\*\*
    Segment                      File (def ext: .dat)           Acc  Typ  Block   Alloc  Exten   Options
    ---------------------------------------------------------------------------------------------------------
    DEFAULT                      $gtmdir/$gtmver/g/payroll.dat  BG   DYN  4096    5000   10000   GLOB=1000
                                                                                                 LOCK=40
                                                                                                 RES=0
                                                                                                 ENCR=OFF


                                  \*\*\* MAP \*\*\*
    --------------------------------Names----------------------------------------------
    From              Up to              Region / Segment / File(def ext: .dat)
    ---------------------------------------------------------------------------------------
    %                 ...                REG = DEFAULT
                                         SEG = DEFAULT
                                         FILE = $gtmdir/$gtmver/g/payroll.dat

    LOCAL LOCKS                          REG = DEFAULT
                                         SEG = DEFAULT
                                         FILE = $gtmdir/$gtmver/g/payroll.dat


The GDE SHOW command displays the default Global Directory.

.. parsed-literal::
   GDE> change -segment default -allocation=1000 file=payroll.dat

The GDE CHANGE command sets the database file name to payroll.dat, and specifies a file size of 1000 blocks (of 1024 bytes).

.. parsed-literal::
   GDE>exit
   %GDE-I-VERIFY, Verification OK
   %GDE-I-GDCREATE, Creating Global Directory file /usr/lib/fis-gtm/V6.0-001_x86/payroll.gld
   %GDE-I-GDEIS, Global Directory

The GDE EXIT command terminates GDE. The Global Directory Editor creates a default Global Directory and displays a confirmation message.

.. parsed-literal::
   $ ls payroll.gld
   payroll.gld

This ls command shows the new Global Directory has been created.

In the simplest case, running the Global Directory Editor and immediately EXITing creates a Global Directory with a default single file database.

To create the database file payroll.dat, use the MUPIP CREATE utility.

Example:

.. parsed-literal:: 
   $ mupip create
   Created file payroll.dat

The MUPIP CREATE command generates the database file. Notice that the MUPIP CREATE syntax does not include the file name. MUPIP uses the environment variable gtmgbldir to find the Global Directory payroll.dat and obtains the file name from that Global Directory. MUPIP then checks to make sure that payroll.dat does not already exist and creates payroll.dat with the characteristics described in payroll.dat.

Example:

.. parsed-literal::
   $ mupip load payroll.gld
   GT.M MUPIP EXTRACT
   02-MAY-2013  22:21:37 ZWR
   Beginning LOAD at record number: 3
   LOAD TOTAL                Key Cnt: 279  Max Subsc Len: 28  Max Data Len: 222
   Last LOAD record number: 281

This uses the MUPIP LOAD command to load a sequential file into the database.

Because MUPIP uses the environment variable gtmgbldir to locate a Global Directory, which identifies the database file(s), the LOAD command does not require any information about the target database. With few exceptions, the YottaDB/GT.M utilities work in the same way.

--------------------------------------
Creating and Editing a Source Program
--------------------------------------

The first step in developing a YottaDB/GT.M program is to create a source file. In most cases, the user can create and modify YottaDB/GT.M source programs using UNIX text editors.

When the program is very simple (and its lines do not need revision after they are entered), you can use the cat command to direct input from your terminal to your source file.

+++++++++++++++++++++++++
Editing from YottaDB/GT.M
+++++++++++++++++++++++++

If you focus on program development outside the YottaDB/GT.M environment, skip this section and continue with the section "Editing from the Shell".

ZEDIT <filename>

Invoke Direct Mode to create and edit a source program in YottaDB/GT.M. At the GTM> or YDB> prompt, invoke the editor by typing:

ZEDIT <filename>

ZEDIT invokes the editor specified by the EDITOR environment variable, which creates a seperate file for each M source module.

The YottaDB/GT.M environment works most efficiently if the file has the same name as the M routine it contains, and has an .m extension. Since ZEDIT automatically defaults the.m extension, it is not necessary to specify an extension unless you require a different one. If you use another extension, you must specify that extension with every reference to the file. Multiple character file extensions are permitted for M source file names.

Example:

.. parsed-literal::
   $ /usr/lib/.fis-gtm/V5.4-002B_x86/gtm
   GTM>ZEDIT "payroll"

This syntax uses the gtm script to enter YottaDB/GT.M from the shell, and uses ZEDIT to initiate an editing session on payroll.m Because ZEDIT defaults the extension to .m, it is not necessary to provide an extension. If payroll.m does not already exist, YottaDB/GT.M creates it in the first source directory identified by $ZROUTINES. If $ZROUTINES is null, ZEDIT places the source file in the process's current working directory.

$ZROUTINES is a read-write special variable containing an ordered list of directories that certain YottaDB/GT.M functions use to locate source and object files. Generally, a system manager sets up the environment to define the environment variable gtmroutines. At image invocation, YottaDB/GT.M initializes $ZROUTINES to the value of gtmroutines. Once you are running M, you can SET and refer to $ZROUTINES using the format:

.. parsed-literal::
   GTM>SET $ZROUTINES=expr

Where:

* The expression may contain a list of UNIX directories and/or file-specifications delimited by spaces.
* The expression specifies one or more directories to search.
* An element of the expression contains an environment variable evaluating to a directory specification.
* If $ZROUTINES contains an environment variable that evaluates to a list, YottaDB/GT.M uses the first name in that list.

For more information on $ZROUTINES, see Chapter 8: “Intrinsic Special Variables”.

+++++++++++++++++++++++++
Editing from the Shell
+++++++++++++++++++++++++

To create and edit a source program from the shell, invoke any text editor at the shell prompt and specify a UNIX file as the source. The YottaDB/GT.M environment works best when you give a file the name of the M routine that it contains, and an .m extension.

Example:

.. parsed-literal::
   $ vi payroll.m

The vi command initiates an editing session for payroll.m from the shell prompt. If payroll.m does not already exist, vi creates it. Because this example uses UNIX rather than YottaDB/GT.M tools, we must specify the .m file extension.

----------------------------
Compiling a Source Program
----------------------------

If you wish to focus on program development outside the YottaDB/GT.M environment, skip the next section and continue with the section "Compiling from the Shell".

YottaDB/GT.M compiles M source code files and produces object files for complete integration into the UNIX enviroment. The object modules have the same name as the compiled M source file with an .o file extension, unless otherwise specified. The object files contain machine instructions and information necessary to connect the routine with other routines, and map it into memory. An M routine source file must be compiled after it is created or modified. You can compile explicitly with the ZLINK command or implicitly with auto-ZLINK. At the shell command line, compile by issuing the mumps command.

The compiler checks M code for syntax errors and displays error messages on the terminal, when processing is complete. Each error message provides the source line in error with an indicator pointing to the place on the line where the error is occurring. For a list and description of the compiler error messages, refer to the Message and Recovery Procedures Reference Manual.

You can generate a listing file containing the compile results by including the -list qualifier as a modifier to the argument to the ZLINK command in Direct Mode. This can also be done by redirecting the compiler messages to a file by adding >filename 2>&1 to the mumps command when compiling a program from the shell. See “Compiling from the Shell” for an explanation of the M command describing -list, and other valid qualifiers for the M and ZLINK commands.

The compiler stops processing a routine line when it detects an error on that line. Under most conditions the compiler continues processing the remaining routine lines. This allows the compiler to produce a more complete error analysis of the routine and to generate code that may have valid executable paths. The compiler does not report multiple syntax errors on the same line. When it detects more than 127 syntax errors in a source file, the compiler ceases to process the file.

++++++++++++++++++++++++++++
Compiling from YottaDB/GT.M
++++++++++++++++++++++++++++

In Direct Mode, YottaDB/GT.M provides access to the compiler explicitly through the ZLINK and ZCOMPILE commands, and implicitly through automatic invocation of ZLINK functionality (auto-ZLINK) to add required routines to the image. ZCOMPILE is a YottaDB/GT.M routine compilation command, it compiles the routine and creates a new object module. The primary task of ZLINK is to place the object code in memory and "connect" it with other routines. However, under certain circumstances, ZLINK may first use the YottaDB/GT.M compiler to create a new object module.

The difference between ZCOMPILE and ZLINK is that ZCOMPILE creates a new object module on compiling, whereas the ZLINK command links the object module with other routines and places the object code in memory.

ZLINK compiles under these circumstances:

* ZLINK cannot locate a copy of the object module but can locate a copy of the source module.
* ZLINK can locate both object and source module, and finds the object module to be older than the source module.
* The file-specification portion of the ZLINK argument includes an explicit extension of .m.

Auto-ZLINK compiles under the first two circumstances, but can never encounter the last one.

When a command refers to an M routine that is not part of the current image, YottaDB/GT.M automatically attempts to ZLINK and, if necessary, compile that routine. In Direct Mode, the most common method to invoke the compiler through an auto-ZLINK is to enter DO ^routinename at the GTM> or YDB> prompt. When the current image does not contain the routine, YottaDB/GT.M does the following:

* Locates the source and object
* Determines whether the source has been edited since it was last compiled
* Compiles the routine, if appropriate
* Adds the object to the image

By using the DO command, you implicitly instruct YottaDB/GT.M to compile, link, and execute the program. With this method, you can test your routine interactively.

For complete descriptions of ZLINK and auto-ZLINK, see Chapter 6: “Commands” .

Example:

.. parsed-literal::
   GTM>do ^payroll
   GTM>do ^taxes

This uses the M DO command to invoke the YottaDB/GT.M compiler implicitly from the YDB> or GTM> prompt if the routine requires new object code. When the compiler runs, it produces two object module files, payroll.o and taxes.o.

If you receive error messages from the compilation, you may fix them immediately by returning to the editor and correcting the source. By default, the YottaDB/GT.M compiler operates in "compile-as-written" mode, and produces object code even when a routine contains syntax errors. This code includes all lines that are correct and all commands on a line with an error, up to the error. Therefore, you may decide to tailor the debugging cycle by running the program without removing the syntax errors.

.. note::
   The DO command does not add an edited routine to the current image if the image already includes a routine matching the DO argument routine name. When the image contains a routine, YottaDB/GT.M simply executes the routine without examining whether a more recent version of the module exists. If you have a routine in your image, and you wish to change it, you must explicitly ZLINK that routine.

Example:

.. parsed-literal::
   GTM>zlink "payroll"
   GTM>zlink "taxes.m"

The first ZLINK compiles payroll.m if it cannot locate payroll, or if it finds that payroll.m has a more recent date/time stamp than payroll.o. The second ZLINK always compiles taxes.m producing a new taxes.o.

For more information on debugging in Direct Mode, see Chapter 4: “Operating and Debugging in Direct Mode”.

+++++++++++++++++++++++++++++++++
Compiling from the Shell
+++++++++++++++++++++++++++++++++

From the shell, invoke the compiler by entering mumps file-name at the shell prompt.

Example:

.. parsed-literal::
   $ mumps payroll.m
   $ mumps taxes.m

This uses the mumps command to invoke the YottaDB/GT.M compiler from the shell prompt, and creates .o versions of these files.

Use the mumps command at the shell prompt to:

* Check the syntax of a newly entered program.
* Optionally, get a formatted listing of the program.
* Ensure that all object code is up to date before linking.

The mumps command invokes the compiler to translate an M source file into object code.

The format for the MUMPS command is:

.. parsed-literal::
   MUMPS [-qualifier[...]] pathname

* Source programs must have an extension of .m.
* Each pathname identifies an M source program to compile.
* Qualifiers determine characteristics of the compiler output.
* Qualifiers must appear after the command, but before the file name to be properly applied.
* YottaDB/GT.M allows the UNIX * and ? wildcards in a file name.
* The MUMPS command returns a status of 1 after any error in compilation.

The * wildcard accepts any legal combination of numbers and characters including a null, in the position the wildcard holds.

The ? wildcard accepts exactly one legal character in its position.

For example, mumps \*.m compiles all files in the current default directory with an .m extension. mumps \*pay?.m compiles .m files with names that contain any characters followed by pay, followed by one character. Unlike when using ZLINK or ZCOMPILE, the filename must be fully specified when compiling from the shell.

.. note::
   When forming routine names, the compiler truncates object filenames to a maximum length of 31 characters. For example, for a source file called Adatabaseenginewithscalabilityproven.m the compiler generates an object file called Adatabaseenginewithscalabilityp.o. Ensure that the first 31 characters of source file names are unique.


+++++++++++++++++++++++++++++++++
Qualifiers for the mumps command
+++++++++++++++++++++++++++++++++

The mumps command allows qualifiers that customize the type and form of the compiler output to meet specific programming needs. MUMPS command qualifiers may also appear as a modifier to the argument to the ZLINK and ZCOMPILE commands. The following section describes the mumps command qualifiers. Make sure the arguments are specified ahead of file name and after the command itself.

**-di[rect_mode]**

Invokes a small YottaDB/GT.M image that immediately initiates Direct Mode.

-direct_mode does not invoke the M compiler.

The -direct_mode qualifier is incompatible with a file specification and with all other qualifiers.

**-dy[namic_literals]**

Compiles certain data structures associated with literals used in the source code in a way that they are dynamically loaded and unloaded from the object code. The dynamic loading and unloading of these data structures:

* Supersedes any specification of -NOINLINE_LITERALS.
* Reduces the amount of private memory required by each process which in turn allows more processes to execute with the same memory.
* In some circumstances, increases application performance by making more memory available for file system buffers.
* Increases the CPU and stack costs of local variable processing

With no -DYNAMIC_LITERALS specified, these data structures continue to be generated when a routine is linked and stay in the private memory of each process. As the use of -DYNAMIC_LITERALS increases object code size, and as the dynamic loading and unloading only saves memory when the object code is in shared libraries, YottaDB/FIS recommends restricting the use of -DYNAMIC_LITERALS to only when compiling object code to be loaded into shared libraries or executed from an auto relink enabled directory.

**-[no]embed_source**

Instructs YottaDB/GT.M to embeds routine source code in the object code. The default is NOEMBED_SOURCE. Like other YottaDB/GT.M compilation qualifiers, this qualifier can be specified through the $ZCOMPILE intrinsic special variable and gtmcompile environment variable. EMBED_SOURCE provides $TEXT and ZPRINT access to the correct source code, even if the original M source file has been edited or removed. Where the source code is not embedded in the object code, YottaDB/GT.M attempts to locate the source code file. If it cannot find source code matching the object code, $TEXT() returns a null string. ZPRINT prints whatever source code found and also prints a TXTSRCMAT message in direct mode; if it cannot find a source file, ZPRINT issues a FILENOTFND error. 

**-[no]i[gnore]**

Instructs the compiler to produce an object file even when the compiler detects errors in the source code (-ignore), or not to produce an object file when the compiler encounters an error (-noignore).

When used with the -noobject qualifier, the -ignore qualifier has no effect.

Execution of a routine that compiles with errors produces run-time errors when the execution path encounters any of the compile time errors.

This compile-as-written mode facilitates a flexible approach to debugging and expedites conversion to YottaDB/GT.M from an interpreted environment. Many M applications from an interpreted environment contain syntax abnormalities. This feature of compiling and later executing a routine provides the feel of developing in an interpreted environment.

By default, the compiler operates in -ignore mode and produces an object module even when the source routine contains errors.

**-le[ngth]=lines**

Controls the page length of the listing file.

The M compiler ignores the -length qualifier unless it appears with the -list qualifier.

By default, the listing has -length=66.

**-[no]li[st][=filename]**

Instructs the compiler to produce a source program listing file, and optionally specifies a name for the listing file. The listing file contains numbered source program text lines. When the routine has errors, the listing file also includes an error count, information about the location, and the cause of the errors.

When you do not specify a file name for the listing file, the compiler produces a listing file with the same name as the source file with a .lis file extension.

The -length and -space qualifiers modify the format and content of the listing file. The M compiler ignores these qualifiers unless the command includes the -list qualifier.

By default, the compiler operates -nolist and does not produce listings.

**-noin[line_literals]**

Compiles routines to use library code in order to load literals instead of generating in-line code thereby reducing the routine size. At the cost of a small increase in CPU, the use of -NOINLINE_LITERAL may help counteract growth in object size due to -DYNAMIC_LITERALS.

.. note::
   Both -DYNAMIC_LITERALS and -NOINLINE_LITERALS help optimize performance and virtual memory usage for applications whose source code includes literals. As the scalability and performance from reduced per-process memory usage may or may not compensate for the incremental cost of dynamically loading and unloading the data structures, and as the performance of routines vs. inline code can be affected by the availability of routines in cache, YottaDB/FIS suggests benchmarking to determine the combination of qualifiers best suited to each workload. Note that applications can freely mix routines compiled with different combinations of qualifiers.

**-[no]o[bject][=filename]**

Instructs the compiler to produce an output object file and optionally specifies a name for the object file using the optional filename argument.

When you do not specify a file name, the compiler produces an object file with the same file name as the source file and an .o file extension.

When forming routine names, the compiler truncates object filenames to a maximum length of 31 characters. For example, for a source file called Adatabaseenginewithscalabilityproven.m the compiler generates an object file called Adatabaseenginewithscalabilityp.o. Ensure that first 31 characters of source file names are unique.

The -noobject qualifier suppresses the production of an object file and is usually used with the -list qualifier to produce only a listing file.

By default, the compiler produces object modules.

**-[no]w[arning]**

Instructs the compiler to suppress error output; the default is -warning.

When used with the -list qualifier, the -nowarning qualifier does not affect errors in the listing.

.. note::
   When used with the -noobject qualifier, the -nowarning qualifier instructs the compiler to produce no object with no indication of the fact or the cause of any errors.

**-r[un]**

Invokes YottaDB/GT.M in Autostart Mode.

The next argument is taken to be an M entryref. That routine is immediately executed, bypassing Direct Mode. Depending on the shell, you may need to put the entryref in quotation marks (""). This qualifier does not invoke the M compiler and is not compatible with any other qualifier.

**-s[pace]=lines**

Controls the spacing of the output in the listing file. -space=n specifies n-1 blank lines separating every source line in the listing file. If n<1, the M command uses single spacing in the listing.

If this qualifier appears without the -list qualifier, the M compiler ignores the -space qualifier.

By default, listings use single spaced output (-space=1).

**MUMPS Command Qualifiers Summary**

+----------------------------------------------+--------------------------------------------+
| Qualifier                                    | Default                                    |
+==============================================+============================================+
| “-di[rect_mode]”                             | N/A                                        |
+----------------------------------------------+--------------------------------------------+
| “-dy[namic_literals]”                        | N/A                                        |
+----------------------------------------------+--------------------------------------------+
| “-[no]embed_source”                          | -noembedsource                             |
+----------------------------------------------+--------------------------------------------+
| “-[no]i[gnore]”                              | -ignore                                    |
+----------------------------------------------+--------------------------------------------+
| “-le[ngth]=lines”                            | -length=66                                 |
+----------------------------------------------+--------------------------------------------+
| “-[no]li[st][=filename]”                     | -nolist                                    |
+----------------------------------------------+--------------------------------------------+
| “-noin[line_literals]”                       | N/A                                        |
+----------------------------------------------+--------------------------------------------+
| “-[no]o[bject][=filename]”                   | -object                                    |
+----------------------------------------------+--------------------------------------------+
| “-r[un]”                                     | N/A                                        |
+----------------------------------------------+--------------------------------------------+
| “-s[pace]=lines”                             | -space=1                                   |
+----------------------------------------------+--------------------------------------------+

-------------------------------
Executing a Source Program
-------------------------------

M source programs can be executed either from the shell or from YottaDB/GT.M (Direct Mode).

++++++++++++++++++++++++++++
Executing in the Direct Mode
++++++++++++++++++++++++++++

As discussed in the section on compiling source programs, the GT.M command ZLINK compiles the source code into an object module and adds the object module to the current image.

The run-time system also invokes auto-ZLINKing when an M command, in a program or in Direct Mode, refers to a routine that is not part of the current image.

M commands and functions that may initiate auto-ZLINKing are:

* DO
* GOTO
* ZBREAK
* ZGOTO
* ZPRINT
* $TEXT()

YottaDB/GT.M auto-ZLINKs the routine only under these conditions:

* The routine has the same name as the source file.
* ZLINK can locate the routine file using $ZROUTINES, or the current directory if $ZROUTINES is null.

$ZROUTINES is a read-write special variable that contains a directory search path used by ZLINK and auto-ZLINK to locate source and object files.

When the argument to a ZLINK command includes a pathname, $ZSOURCE maintains that pathname as a default for ZEDIT and ZLINK. $ZSOURCE is a read-write special variable.

Once you use the ZEDIT or ZLINK commands, $ZSOURCE can contain a partial file specification. The partial file specification can be a directory path (full or relative), a file name, and a file extension. You can set $ZSOURCE with an M SET command. A ZLINK without an argument is equivalent to ZLINK $ZSOURCE.

For additional information on $ZSOURCE and $ZROUTINES, refer to Chapter 8: “Intrinsic Special Variables”.

Example:

.. parsed-literal::
   GTM>ZLINK "taxes"

If ZLINK finds taxes.m or taxes.o, the command adds the routine taxes to the current image. When ZLINK cannot locate taxes.o, or when it finds taxes.o is older than taxes.m, it compiles taxes.m, producing a new taxes.o. Then, it adds the contents of the new object file to the image.

++++++++++++++++++++++++++++++++++++
Locating the Source File Directory
++++++++++++++++++++++++++++++++++++

A ZLINK command that does not specify a directory uses $ZROUTINES to locate files. When $ZROUTINES is null, ZLINK uses the current directory. $ZROUTINES is initialized to the value of the gtmroutines environment variable.

When the file being linked includes an explicit directory, ZLINK and auto-ZLINK searches only that directory for both the object and the source files. If compilation is required, ZLINK places the new object file in the named directory.

A subsequent ZLINK searching for this object file will never find the object file in the specified directory unless the directory is added to the search path in $ZROUTINES, or the object file is moved to another directory already in the search path.

ZLINK cannot change a currently active routine, (e.g., a routine displayed in a ZSHOW "S" of the stack). ZLINK a currently active routine by first removing it from the M stack, using ZGOTO, or one or more QUITs. For additional information on the functionality of ZGOTO and ZSHOW, see their entries in Chapter 6: “Commands”.

To maintain compatibility with other editions of YottaDB/GT.M that do not permit the percent sign (%) in a file name, YottaDB/GT.M uses an underscore (_) in place of the percent in the file name.

Example:

.. parsed-literal::
   GTM>zlink "_MGR"

This ZLINK links the M routine %MGR into the current image.

---------------------------------
Executing from the Shell
---------------------------------

You can run a program from the shell prompt using the following command:

.. parsed-literal::
   $ mumps -run ^filename

The mumps command searches the directories specified by the environment variable gtmroutines to locate the specified file name.

Example:

.. parsed-literal::
   $ mumps -run ^payroll

This executes a routine named payroll.

---------------------------------------------
Processing Errors from Direct Mode and Shell
---------------------------------------------

+----------------------------+---------------------------------------------------------------------------+-------------------------------------------------------------------------+
|                            | Executing in Direct Mode                                                  | Executing from the Shell (mumps -run ^routine)                          |
+============================+===========================================================================+=========================================================================+
| Usage                      | Suitable for Development and Debugging                                    | Suitable for production                                                 |
+----------------------------+---------------------------------------------------------------------------+-------------------------------------------------------------------------+
| Error Handling             | Not invoked for code entered at the direct mode prompt; Note that XECUTE  | Errors are suppressed and cause a silent process exit. Set the          |
|                            | code is treated as not entered at the Direct Mode prompt.                 | environment variable gtm_etrap which overrides the default $ZTRAP="B".  |
|                            |                                                                           |                                                                         |
|                            | The default $ZTRAP="B" brings a process to the Direct Mode for debugging. | If needed, error handlers can include appropriate error notification to |
|                            |                                                                           | $PRINCIPAL. For example, the gtmprofile script sets a default $ETRAP    |
|                            |                                                                           | value of "Write:(0=$STACK) ""Error occurred: "",$ZStatus,!" which you   |
|                            |                                                                           | can customize to suit your needs.                                       |
+----------------------------+---------------------------------------------------------------------------+-------------------------------------------------------------------------+
| stderr                     | YottaDB/GT.M processes send error messages to stderr only under the following conditions:                                                           |
|                            |                                                                                                                                                     |
|                            | * The error is fatal which means that the process is about to terminate                                                                             |
|                            | * During compilation except of indirection or XECUTE                                                                                                |
|                            | * The process is about to enter direct mode due to a BREAK command                                                                                  |
|                            | * The erroneous code was entered at the direct mode prompt                                                                                          |
|                            |                                                                                                                                                     |
+----------------------------+---------------------------------------------------------------------------+-------------------------------------------------------------------------+

For more information, see Chapter 13: “Error Processing”.
