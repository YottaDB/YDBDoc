
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




