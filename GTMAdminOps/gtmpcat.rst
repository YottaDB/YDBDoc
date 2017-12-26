
.. index::
   GTMPCAT - YottaDB/GT.M Process/Core Analysis Tool

==============================================================
Appendix F: GTMPCAT - YottaDB/GT.M Process/Core Analysis Tool
==============================================================

.. contents::
   :depth: 2

----------------------------
Overview
----------------------------

gtmpcat is a diagnostic tool for YottaDB/FIS to provide YottaDB/GT.M support. It gathers extensive diagnostic context from process dump (core) files and from running processes. This diagnostic information permits YottaDB/FIS to respond more quickly and successfully with root cause analysis of issues for which you engage with YottaDB/GT.M support.

Starting with YottaDB/GT.M V6.3-002, each YottaDB/GT.M release includes a gtmpcat_for_{release} kit which contains the files required to run gtmpcat for the released YottaDB/GT.M version. install_gtmpcat.sh script installs the gtmpcat files in $gtm_dist/tools/gtmpcat. After installation, use $gtm_dist/gtmpcat to run gtmpcat. Note that the gtmpcat_for_{release} kit does not contain the files necessary for analyzing other YottaDB/GT.M releases. Full gtmpcat releases contain these files for all supported production YottaDB/GT.M releases as of the gtmpcat release date.

.. note::
   The local variables of a YottaDB/GT.M process may contain restricted information, such as protected health care or financial data. Under your direction, gtmpcat either ignores (the default) or accesses the contents of local variables

The gtmpcat program is itself written using YottaDB/GT.M and requires:

* YottaDB/GT.M V5.3-004 or later when run in M mode (may misrepresent UTF-8 information if present), or
* YottaDB/GT.M V5.4-002 or later when run in UTF-8 mode

However, gtmpcat can analyze core files and processes from YottaDB/GT.M V5.1-000 or later - there need not be a relationship between the YottaDB/GT.M release used to run gtmpcat, and the YottaDB/GT.M release of a core file or process analyzed by gtmpcat.

gtmpcat requires the presence of an appropriate system debugger (for example, gdb on Linux (x86 and x86_64) or dbx on AIX). In operation, gtmpcat attaches to a system debugger, and directs it to extract information from a core file or live process.

This appendix discusses gtmpcat and how to use it.

.. note::
   Albeit small, there is a non-zero risk of premature process termination when attaching to a live process with a debugger which is what gtmpcat does when it is not directed to a core file. This risk cannot be eliminated. If problem diagnosis requires attaching to a live process, YottaDB/FIS recommends attaching to an expendable process, or to one in a testing environment. Use gtmpcat on a production process when you have considered all your options and carefully determined that is the best alternative.

-------------------------------
Usage
-------------------------------

gtmpcat consists of two routine files -- a platform-independent main program (gtmpcat.m), and an initialization file, which is specific to the CPU architecture, operating system and GT.M version. Both the main program and the appropriate initialization file must be available in the $gtmroutines search path. Run gtmpcat as follows:

.. parsed-literal::
   $gtm_dist/mumps -run gtmpcat <options> corename|processid

As it executes, gtmpcat provides a running account of its actions on its stdout, as well as a detailed report in a file whose name it provides on stdout at the end of its processing.

Option values are separated from option flags by whitespace. The options are:

+-------------------------------------------+------------+-------------------+---------------------------------------------------------------------------------------------------------+
| Options                                   |            | Abbr              | Description                                                                                             |
+===========================================+============+===================+=========================================================================================================+
| --asver <V6.X-XXX>                        |            | -a                | Specify an explicit YottaDB/GTM version for the core/process, if it is not the same as that specified by|
|                                           |            |                   | the -version option (which in turn defaults to that of the gtmpcat process).                            |
+-------------------------------------------+------------+-------------------+---------------------------------------------------------------------------------------------------------+
| --callin <path>                           |            | n/a               | Specifies the path of the executable that created the core or the path to load in the given process to  |
|                                           |            |                   | run against. Typically, this is a call-in type module that can also be used to diagnose LKE or DSE cores|
|                                           |            |                   | or processes.                                                                                           |
+-------------------------------------------+------------+-------------------+---------------------------------------------------------------------------------------------------------+
| --cmdscript <entryref>                    |            | -s                | Use this option for the prototyping of additional information extraction. This option executes once     |
|                                           |            |                   | every time you run GTMPCAT. This option also implies interactive mode. As soon as interactive mode setup|
|                                           |            |                   | is complete, GTMPCAT invokes the specified entryref. This options is like a gtmpcat-plugin in a way that|
|                                           |            |                   | it runs with full access to GTMPCAT routines, variables, and the debugger.                              |
+-------------------------------------------+------------+-------------------+---------------------------------------------------------------------------------------------------------+
| --cmdfile </path/to/file-name>            |            | -c                | Specifies the path of the file containing interactive mode commands. It is ignored unless specified with|
|                                           |            |                   | --interactive (i).                                                                                      |
+-------------------------------------------+------------+-------------------+---------------------------------------------------------------------------------------------------------+
| --debug                                   | \*         | -d                | Enable interactive debugging of gtmpcat itself. On encountering an error, gtmpcat executes a BREAK      |
|                                           |            |                   | command before gtmpcat either continues or exits depending on the error severity and whether an error   |
|                                           |            |                   | resume point is defined. This enables some interactive debugging. Also, for a successful run, a ZSHOW   |
|                                           |            |                   | dump is created with the name gtmpcat-DEBUG.zshowdump.txt. This file is overwritten/recreated as needed.|
|                                           |            |                   | The default is -nodebug.                                                                                |
+-------------------------------------------+------------+-------------------+---------------------------------------------------------------------------------------------------------+
| --debuglines                              | \*         | n/a               | Enables debugging of the lines written to and read from the debugger. Used for debugging gtmpcat. The   |
|                                           |            |                   | default is --nodebuglines.                                                                              |
+-------------------------------------------+------------+-------------------+---------------------------------------------------------------------------------------------------------+
| --devicedetail                            | \*         | -e                | Include a separate section in the report to show the actual control blocks in use and their addresses.  |
+-------------------------------------------+------------+-------------------+---------------------------------------------------------------------------------------------------------+
| --gtmdeftypes                             |            | -g                | Specifies the location of GTMDefinedTypesInit.m when it is not there in the current path.               |
+-------------------------------------------+------------+-------------------+---------------------------------------------------------------------------------------------------------+
| --ignoreunexprslt                         | \*         | -u                | When a YottaDB/GT.M version is built with debugging flags, YottaDB/GT.M can output extra lines when, for|
|                                           |            |                   | example, starting up and/or displaying the results of simple commands like Write $Zversion. As part of  |
|                                           |            |                   | determining what version a particular install directory has, gtmpcat executes the Write $ZVersion       |
|                                           |            |                   | command in a mumps process. If this command prints extra lines, it can cause gtmpcat initialization to  |
|                                           |            |                   | fail. This option can be used to ignore the extra lines returned. The default is --noignoreunexprslt .  |
+-------------------------------------------+------------+-------------------+---------------------------------------------------------------------------------------------------------+
| --interactive                             | \*         | -i                | Tells gtmpcat to enter interactive mode, as described below. Use this only under the direction of       |
|                                           |            |                   | YottaDB/FIS Support. The default is --nointeractive .                                                   |
+-------------------------------------------+------------+-------------------+---------------------------------------------------------------------------------------------------------+
| --localvar                                | \*         | -l                | Include local variables, both the current local vars plus any saved (NEW'd) vars on the M stack (either |
|                                           |            |                   | explicit or implicit) in the report. Since the local variables of a process are likely to contain       |
|                                           |            |                   | protected (confidential) information that is being processed, the default is ---nolocalvar to omit them.|
|                                           |            |                   | Before sharing a gtmpcat report with anyone, you must determine whether the report contains protected   |
|                                           |            |                   | information and whether the recipient is permitted to view the information in the report. YottaDB/GT.M  |
|                                           |            |                   | Support does not accept protected information.                                                          |
+-------------------------------------------+------------+-------------------+---------------------------------------------------------------------------------------------------------+
| --lockdetail                              | \*         | n/a               | Include a detailed dump of M lock related control blocks showing the block addresses and relationships. |
|                                           |            |                   | The default is --nolockdetail. This option is useful only to debug YottaDB/GT.M itself.                 |
+-------------------------------------------+------------+-------------------+---------------------------------------------------------------------------------------------------------+
| --lvdetail                                | \*         | n/a               | Include a detailed dump of the actual local variable structures. As this option can produce a report    |
|                                           |            |                   | with protected information in local variable subscripts, please review the warnings above in the        |
|                                           |            |                   | -localvar option. The default is --nolvdetail. This option is useful only to debug YottaDB/GT.M itself. |
+-------------------------------------------+------------+-------------------+---------------------------------------------------------------------------------------------------------+
| --memorydump                              | \*         | -m                | Includes a memory map dump of all allocated storage. Only available when the core file or process is    |
|                                           |            |                   | running YottaDB/GT.M V5.3-001 or later, and then only if $gtmdbglvl is non-zero in the process. The     |
|                                           |            |                   | default is -nodump. Use this only under the direction of YottaDB/FIS Support.                           |
+-------------------------------------------+------------+-------------------+---------------------------------------------------------------------------------------------------------+
| --mprof                                   | \*         | -p                | Enable M-profiling across the gtmpcat run. After loading the initialization file, gtmpcat turns on      |
|                                           |            |                   | M-profiling, Just before gtmpcat completes execution, it turns off M-profiling and dumps the result in a|
|                                           |            |                   | global called ^trace. This option requires a YottaDB/GT.M database and global directory be available.   |
+-------------------------------------------+------------+-------------------+---------------------------------------------------------------------------------------------------------+
| --msdetail                                |            | n/a               | Includes additional fields from the M stack-frame. The default is --msdetail.                           |
+-------------------------------------------+------------+-------------------+---------------------------------------------------------------------------------------------------------+
| -mumps                                    |            | n/a               | the core or process is a mumps executable (default).                                                    |
+-------------------------------------------+------------+-------------------+---------------------------------------------------------------------------------------------------------+
| -mupip                                    |            | n/a               | the core or process is a mupip executable.                                                              |
+-------------------------------------------+------------+-------------------+---------------------------------------------------------------------------------------------------------+
| --output <file/directory>                 |            | -o                | Specifies the desired output file/directory. If the value given is a directory (relative or absolute),  |
|                                           |            |                   | the default file name is created in the given directory. If the value is a file, that is the file-name  |
|                                           |            |                   | used to hold the report.                                                                                |
+-------------------------------------------+------------+-------------------+---------------------------------------------------------------------------------------------------------+
| --ppi                                     | \*         | n/a               | Specifies whether to extract personally identifying information from the core. If --ppi is not          |
|                                           |            |                   | specified, GTMPCAT does not: invoke the -l option, extract local var information on the M stack, or     |
|                                           |            |                   | dump database clues in the region output.                                                               |
+-------------------------------------------+------------+-------------------+---------------------------------------------------------------------------------------------------------+
| --regiondetail                            |            | n/a               | Collects spanning regions information. Currently, this option collects data but does not display the    |
|                                           |            |                   | output.                                                                                                 |
+-------------------------------------------+------------+-------------------+---------------------------------------------------------------------------------------------------------+
| --tracedump                               |            | -t                | Read and format the internal YottaDB/GT.M trace table. Default is --notracedump. This is useful only to |
|                                           |            |                   | debug YottaDB/GT.M itself.                                                                              |
+-------------------------------------------+------------+-------------------+---------------------------------------------------------------------------------------------------------+
| --version <location of the YottaDB/GT.M   |            | -v                | Specifies the directory with the YottaDB/GT.M version of the core/process. The default is the version   |
| version of the core process>              |            |                   | used by gtmpcat itself, that is, in $gtm_dist.                                                          |
+-------------------------------------------+------------+-------------------+---------------------------------------------------------------------------------------------------------+


Abbr specifies the single character abbreviation for an option. You can combine two or more options using their single character abbreviations with a common "-" prefix and no white space in between. Specify values in the same order in which you combine the abbreviations. For example, -lov output.txt /usr/lib/fis-gtm/V6.0-001_x86_64 means --localvar --output output.txt --version /usr/lib/fis-gtm/V6.0-001_x86_64.

\* specifies options that can be negated. To negate a single character abbreviation, use its upper case equivalent or use the full option name prefixed by "no" . For example, -P means --nomprof.

When gtmpcat runs, it needs to know how the structures and fields for a given version of YottaDB/GT.M are defined. There is one of these initialization files for each OS, architecture, and YottaDB/GT.M version. Once gtmpcat knows the architecture and YottaDB/GT.M version of the process/core, it invokes the proper initialization file to define the layout of everything it is interested in. The format of the gtmpcat initialization file is:

.. parsed-literal::
   gtmpcat<OS>On<architecture><gtmversion>.m 

For example, the name of gtmpcat initialization file on Linux x86_64 running YottaDB/GT.M V6.0-000 is gtmpcatLinuxOnX8664V60000.m


-----------------------------------
Interactive Mode
-----------------------------------

gtmpcat has an interactive mode. Instead of producing a report, the interactive mode acts as a front-end that enhances the use of the debugger from the underlying OS.

.. note::
   As interactive mode is still under development, it is likely to have rough edges. For example, M terminal IO is limited to that provided by YottaDB/GT.M . you can edit input using the left and right arrow keys but command retrieval is limited to the last command entered (with the up arrow key, and the down arrow key has no effect).

The help gtmpcat command describes currently supported commands . Any command that is not recognized in this mode, or any command prefixed with a "\" char, is sent to the native debugger and its output displayed on the console.

All of the information from the reports is available in this mode. Each "section" of the report can be printed, either the entire report or one or more at a time with the report command.

There are commands that give additional information not available in the standard report. For example:

* *cache*: The cache command gives information similar to the DSE CACHE command.
* *dmp*: The dmp command dumps a data structure, given just its address as long as the structure is defined in the initialization file.
* *dmparray*: The dmparray command can dump an array of control blocks in a formatted fashion. The array of control blocks are typdef structures defined in the YottaDB/GT.M header files and whose layouts are defined in GTMDefinedTypesInit.m. The initialization file for interactive mode is GTMDefinedTypesInit.m which is platform and release specific. It is available as a separate tarball in YottaDB/GT.M release directories (older versions have GTMDefinedTypesInit.m packaged with the YottaDB/GT.M tarball itself). 
* *dmplist*: The dmplist command can dump a linked list of blocks terminating when a maximum number of such blocks is processed or if the list cycles around to the beginning or if it hits a NULL forward link.


