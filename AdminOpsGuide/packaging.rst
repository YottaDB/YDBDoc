.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2017-2022 YottaDB LLC and/or its subsidiaries.#
.. # All rights reserved.                                        #
.. #                                                             #
.. #     This document contains the intellectual property        #
.. #     of its copyright holder(s), and is made available       #
.. #     under a license.  If you do not know the terms of       #
.. #     the license, please stop and do not read further.       #
.. #                                                             #
.. ###############################################################

.. index::
   Packaging YottaDB Applications

=================================================
Appendix E : Packaging YottaDB Applications
=================================================

.. contents::
   :depth: 2

YottaDB provides the yottadb -run shell command to invoke application entryrefs directly from the shell. YottaDB recognizes a number of environment variables to determine the starting characteristics of a process; these are described in the documentation. In order to ensure that environment variables are set correctly, you should create a shell script that appropriately sets (and clears where needed) environment variables before invoking YottaDB. When users should not get to the shell prompt from the application (either for lack of skill or because they are not sufficiently trusted), or when the application needs more access to the shell command line than that provided by the $ZCMDLINE ISV, you may need to package a YottaDB application using techniques beyond, or in addition to, invocation from a shell script.

Since YottaDB is designed to integrate with the underlying OS, you should consider the entire range of services provided by operating systems when packaging a YottaDB application. For example, you can use the host based access control provided by TCP wrappers, or various controls provided by xinetd (including per_source, cps, max_load protection, only_from, no_access, and access_times).

This appendix has two examples that illustrate techniques for packaging YottaDB applications, neither of which excludes the other.

1. “Setting up a Captive User Application with YottaDB”, such that when captive users login, they are immediately taken to the application, have no access to the shell or programmer prompt, and when they exit, are logged off the system.
2. “Invoking YottaDB through a C main() program”: in addition to providing another technique for creating captive applications, invoking through a C main() is the only way that application code has access to the original argc and argv of a shell command used to start the application (the $ZCMDLINE ISV provides an edited version of the command line).

--------------------------------------------------------
Setting Up a Captive User Application with YottaDB
--------------------------------------------------------

This section discusses wholesome practices in setting up a YottaDB based application on UNIX/Linux such that, when "captive" users log in to the system, they are taken directly into the application, and when they exit the application, they are logged off the system. Unless part of the application design, a captive user should not get to a shell or YottaDB prompt.

The example in “Sample .profile” is for /bin/sh on GNU/Linux, and may need to be adapted for use with other shells on other platforms.

At a high level, preventing a captive user from getting to a shell or YottaDB prompt involves:

* trapping signals that may cause the login shell to give the user interactive access, for example, by pressing <CTRL-Z> to suspend the yottadb application;
* preventing a yottadb process from responding to a <CTRL-C> until the application code sets up a handler; and
* preventing an error in the application, or a bug in an error handler, from putting a captive user into direct mode.

Note that other users on the system who have appropriate privileges as managed by the operating system can still interfere with captive users. In order to secure a system for captive applications, you must protect it from untrusted other users. Users should only have credentials that permit them the level of access appropriate to their level of trustworthiness, thus: untrusted users should not have credentials to access a system with captive applications.

Defensive configuration implies setting up layers of defenses, so that an error in one layer does not compromise the system.

+++++++++++++++++++++
Sample .profile
+++++++++++++++++++++

After initialization common to all users of a system, a login shell sources the .profile file in the user's home directory. A captive user's .profile might look something like this, where "..." denotes a value to be provided.

.. code-block:: bash

   trap "" int quit        # terminate on SIGINT and SIGQUIT
   stty susp \000         # prevent <CTRL-Z> from sending SIGSUSP
   # set environment variables needed by YottaDB and by application, for example
   export ydb_dist=...
   export ydb_gbldir=...
   export ydb_routines=...
   export ydb_repl_instance=...
   export ydb_tmp=...
   # disable yottadb ^C until application code sets up handler
   export ydb_nocenable=1
   # override default of $ZTRAP="B"
   export ydb_etrap='I 0=$ST W "Process terminated by: ",$ZS,! ZHALT 1'
   # set other environment variables as appropriate, for example
   export EDITOR=...        # a preferred editor for ZEDIT
   export TZ=...          # a timezone different from system default
   export HUGETLB_SHM=yes      # example of a potential performance setting
   export PATH=/usr/bin:/bin    # only the minimum needed by application
   export SHELL=/bin/false     # disable ZSYSTEM from command prompt
   # execute captive application starting with entryref ABC^DEF then exit
   exec $ydb_dist/yottadb -run ABC^DEF

Note the use of exec to run the application - this terminates the shell and disconnects users from the system when they exit the YottaDB application.

If an incoming connection is via an Internet superserver such as xinetd, some of these are not applicable, such as disabling <CTRL-C> and <CTRL-Z>

--------------------------------------------
Invoking YottaDB in a C main() program
--------------------------------------------

There are several circumstances when it is desirable to invoke a YottaDB application with a top-level C main() program rather than with yottadb -run. Examples include:

* A need to ensure correct values for environment variables, and a shell script cannot be used (for example, when there is a specific operational need to install an application with the setuid bit).
* Programs that show up on a process display with meaningful names (like friday instead of yottadb -run monthstarting friday, in the following example).

To compile and run the monthstarting.zip example, perform the following steps:

Download monthstarting.zip.

monthstarting.zip contains monthstarting.m, month_starting.c, and monthstarting.ci. To download monthstarting.zip, go to `GitLab <https://gitlab.com/YottaDB/DB/YDBDoc/blob/master/AdminOpsGuide/monthstarting.zip>`_.

Run the monthstarting.m program that lists months starting with the specified day of the week and year range.

.. code-block:: bash

   $ yottadb -run monthstarting Friday 1986 1988
   FRI AUG 01, 1986
   FRI MAY 01, 1987
   FRI JAN 01, 1988
   FRI APR 01, 1988
   FRI JUL 01, 1988
   $

Notice that this program consists of a main program that reads the command line in the intrinsic special variable $ZCMDLINE, and calls calcprint^monthstarting(), providing as its first parameter the day of the week to be reported.

This step is optional as there is no need to explicitly compile monthstarting.m because YottaDB autocompiles it as needed.

On x86 GNU/Linux (64-bit Ubuntu 12.04), execute the following command to compile month_starting.c and create an executable called friday.

.. code-block:: bash

   $ gcc -c month_starting.c -I$ydb_dist
   $ gcc month_starting.o -o friday -L $ydb_dist -Wl,-rpath=$ydb_dist -lyottadb

For compiling the month_starting.c program on other platforms, refer to the `Integrating External Routines chapter of the Programmer's Guide <../ProgrammersGuide/extrout.html>`_.

Execute the following command:

.. code-block:: bash

   $ ./friday 1986 1988
   FRI AUG 01, 1986
   FRI MAY 01, 1987
   FRI JAN 01, 1988
   FRI APR 01, 1988
   FRI JUL 01, 1988
   $

You can also execute the same program with the name monday. In doing so, the program displays months starting with Monday.

.. code-block:: bash

   $ ln -s friday monday
   $ ./monday 1986 1988
   MON SEP 01, 1986
   MON DEC 01, 1986
   MON JUN 01, 1987
   MON FEB 01, 1988
   MON AUG 01, 1988
   $

The month_starting.c program accomplishes this by calling the same YottaDB entryref calcprint^monthstarting(), and passing in as the first parameter the C string argv[0], which is the name by which the program is executed. If there are additional parameters, month_starting.c passes them to the M function; otherwise it passes pointers to null strings:

.. code-block:: C

   /* Initialize and call calcprint^monthstarting() */
   if ( 0 == ydb_init() ) ydb_ci("calcprint", &status, argv[0], argc>1 ? argv[1] : "", argc>2 ? argv[2] : "");

Prior to calling the YottaDB entryref, the C program also needs to set environment variables if they are not set: ydb_dist to point to the directory where YottaDB is installed, ydb_routines to enable YottaDB to find the monthstarting M routine as well as YottaDB's %DATE utility program, and ydb_ci to point to the call-in table:

.. code-block:: C

   /* Define environment variables if not already defined */
           setenv( "ydb_dist", "/usr/local/lib/yottadb/r120", 0 );
           if (NULL == getenv( "ydb_routines" ))
           {
             tmp1 = strlen( getenv( "PWD" ));
             strncpy( strbuf, getenv( "PWD"), tmp1 );
             strcpy( strbuf+tmp1, " " );
             tmp2 = tmp1+1;
             tmp1 = strlen( getenv( "ydb_dist" ));
             strncpy( strbuf+tmp2, getenv( "ydb_dist" ), tmp1 );
             tmp2 += tmp1;
             if ( 8 == sizeof( char * ))
             {
               tmp1 = strlen( "/libgtmutil.so" );
               strncpy( strbuf+tmp2, "/libgtmutil.so", tmp1 );
               tmp2 += tmp1;
             }
             strcpy( strbuf+tmp2, "" );
            setenv( "ydb_routines", strbuf, 1 );
            }
            setenv( "ydb_ci", "monthstarting.ci", 0 );
            if ( 0 == ydb_init() ) ydb_ci("calcprint", &status, argv[0], argc>1 ? argv[1] : "", argc>2 ? argv[2] : "");
            ydb_exit(); /* Discard status from ydb_exit and return status from function call */


Note that on 32-bit platforms, the last element of ydb_routines is $ydb_dist, whereas on 64-bit platforms, it is $ydb_dist/libgtmutil.so. If you are creating a wrapper to ensure that environment variables are set correctly because their values cannot be trusted, you should also review and set the environment variables discussed in “Setting up a Captive User Application with YottaDB” above.

All the C program needs to do is to set environment variables and call a YottaDB entryref. A call-in table is a text file that maps C names and parameters to M names and parameters. In this case, the call-in table is just a single line to map the C function calcprint() to the YottaDB entryref calcprint^monthstarting():

.. code-block:: none

   calcprint : ydb_int_t* calcprint^monthstarting(I:ydb_char_t*, I:ydb_char_t*, I:ydb_char_t*)

--------------------------------
Defensive Practices
--------------------------------

The following practices, some of which are illustrated in “Sample .profile”, help provide layered defenses:

1. Setting the ydb_nocenable environment variable to a value to specify that <CTRL-C> should be ignored by the application, at least until it sets up a <CTRL-C> handler. As part of its startup, the application process might execute:

   .. code-block:: none

      USE $PRINCIPAL:(EXCEPTION="ZGOTO"_$ZLEVEL_":DONE":CTRAP=$CHAR(3):CENABLE)

to set up a handler such as:

DONE: QUIT ; or HALT or ZHALT, as appropriate

2. Providing a value to the ydb_etrap environment variable, as illustrated “Sample .profile”. This overrides YottaDB's default value of "B" for $ZTRAP, which puts the application into direct mode. Of course, in a development environment, going to direct mode may be the correct behavior, in which case there is no need to set ydb_etrap.

3. Providing a value to the ydb_zinterrupt environment to override the default of "IF $ZJOBEXAM()" which causes the process to create a text file of its state in response to a MUPIP INTRPT (or SIGUSR1 signal). Such a text file may contain confidential information that the process is actively computing. Note that a user can only send INTRPT signals as permitted by the configuration of system security for the user. If your application uses INTRPT signals, review the code they invoke carefully to ensure processes respond appropriately to the signal. If any response produces an output file, be sure they have write access to the destination; restrict read access to such files appropriately. The “Sample .profile” example does not illustrate an alternative value for ydb_interrupt.

4. Setting the SHELL environment variable to /bin/false disables the ZSYSTEM command, which if executed without an argument takes the user to a shell prompt. While a correctly coded application might not have a ZSYSTEM without an argument, setting SHELL to a value such as /bin/false, as illustrated above, adds a layer of defense against a possible application bug. Of course, if an application uses the ZSYSTEM command, then an executable SHELL is required. If your application uses ZSYSTEM to run a command, consider whether a PIPE device might provide a better alternative.

5. Setting the PATH environment explicitly to only those directories that contain executable files that the yottadb process will need to execute, with a ZSYSTEM command or a PIPE device.

6. Because some text editors include functionality to run a shell in an edit buffer, setting the EDITOR variable to an editor which does not have such functionality is a way to block shell access in the event that the application uses the ZEDIT command to edit a text file. Note that if an application allows users to edit text files, they can also edit YottaDB program source files, and application configuration should ensure that such program files cannot be accessed by the $ZROUTINES of the process unless that is the desired behavior.

---------------------------------
Other
---------------------------------

Depending on application requirements, other packaging technologies for consideration include:

* Choosing a restricted shell for the login of a captive user, such as rbash, instead of /bin/sh (for example, see http://en.wikipedia.org/wiki/Restricted_shell).
* Setting up a chroot environment for an application used by captive users (for example, see http://en.wikipedia.org/wiki/Chroot).
* Using TCP wrappers to filter incoming requests (for example, see https://www.cyberciti.biz/faq/tcp-wrappers-hosts-allow-deny-tutorial/).
* Configuring mandatory access controls, such as SELinux (for example, http://opensource.com/business/13/11/selinux-policy-guide).


