.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2018-2022 YottaDB LLC and/or its subsidiaries.#
.. # All rights reserved.                                        #
.. #                                                             #
.. #     This document contains the intellectual property        #
.. #     of its copyright holder(s), and is made available       #
.. #     under a license.  If you do not know the terms of       #
.. #     the license, please stop and do not read further.       #
.. #                                                             #
.. ###############################################################

============
YDB Posix
============

.. contents::
   :depth: 5

--------
Overview
--------

YDBPosix is a plugin that allows M application code to use selected POSIX functionality; it does not implement the underlying functionality. A set of low level C functions closely matching their corresponding POSIX functions act as a software shim to connect M code to POSIX functions. A set of higher level entryrefs make the functionality available in form more familiar to M programmers. M application code is free to call either level.

As C application code can call POSIX functions directly, the plugin has no value to C application code.

When installed in the :code:`$ydb_dist/plugin` directory, YDBPosix consists of the following files:

- :code:`libydbposix.so` – a shared library with the C software shims

- :code:`ydbposix.xc` – a call-out table to allow M code to call the functions in :code:`libydbposix.so`

- :code:`r/_ydbposix.m` – M source code for higher level :code:`^%ydbposix` entryrefs that M application code can call.

- :code:`r/_ydbposixtest.m` – M source code for :code:`%ydbposixtest` routine to test plugin with :code:`yottadb -run %ydbposix`

- :code:`o/_ydbposix.so` – a shared library with M mode object code for :code:`^%ydbposix` & :code:`^%ydbposixtest` entryrefs

- :code:`o/utf8/_ydbposix.so` – if YottaDB is installed with UTF-8 support, a shared library with UTF-8 mode object code

------------
Installation
------------

YottaDB must be installed and available before installing the POSIX plugin. https://yottadb.com/product/get-started/ has instructions on installing YottaDB. Download and unpack the POSIX plugin in a temporary directory, and make that the current directory. Then:

.. code-block:: bash

    mkdir build && cd build
    cmake ..
    make && sudo make install

The POSIX plugin can also be installed when installing YottaDB, by adding the :code:`--posix` option to the :code:`ydbinstall.sh` command:

.. code-block:: bash

   sudo ./ydbinstall.sh --utf8 default --verbose --posix

-------
Testing
-------

Testing the POSIX plugin needs an environment to be created. This can be done with the following commands:

.. code-block:: bash

   export ydb_dir=`mktemp -d`
   source $(pkg-config --variable=prefix yottadb)/ydb_env_set

Once the environment has been set up :code:`yottadb -run %ydbposixtest` can be executed to test the POSIX plugin installation.
The expected output of :code:`yottadb -run %ydbposixtest` is as below; manually verify whether the statement about Daylight Savings Time is correct.

.. code-block:: none

    PASS Invocation
    PASS $zhorolog
    PASS $ZHOROLOG
    Daylight Savings Time is not in effect
    PASS mktime()
    PASS Microsecond resolution
    PASS regmatch^%ydbposix 1
    PASS regfree^%ydbposix
    PASS REGMATCH^%ydbposix 1
    PASS REGFREE^%ydbposix
    PASS regmatch^%ydbposix 2
    PASS REGMATCH^%ydbposix 2
    PASS regmatch^%ydbposix 3
    PASS REGMATCH^%ydbposix 3
    PASS regmatch^%ydbposix 3
    PASS REGMATCH^%ydbposix 3
    PASS regmatch^%ydbposix 4
    PASS REGMATCH^%ydbposix 4
    PASS regmatch^%ydbposix 5
    PASS REGMATCH^%ydbposix 5
    PASS mktmpdir
    PASS statfile.times
    PASS statfile.ids
    PASS filemodeconst^%ydbposix
    PASS signal
    PASS STATFILE.times
    PASS STATFILE.ids
    PASS syslog1
    PASS syslog2
    PASS setenv
    PASS unsetenv
    FAIL rmdir
    PASS MKTMPDIR
    PASS mkdir
    PASS MKDIR
    PASS UTIMES
    PASS UMASK
    PASS CHMOD
    PASS SYMLINK
    PASS REALPATH
    PASS CP
    PASS Nanosecond resolution
    PASS SYSCONF

---
Use
---

For use by YottaDB, the environment variable :code:`ydb_xc_ydbposix` must point to :code:`ydbposix.xc` (which is installed at :code:`$ydb_dist/plugin/ydbposix.xc` by :code:`make install`); and the environment variable :code:`ydb_routines` must allow YottaDB processes to find the %ydbposix entryrefs. This includes a :code:`$ydb_routines` term of the form :code:`$ydb_dist/plugin/o/_ydbposix.so` for M mode processes and :code:`$ydb_dist/plugin/o/utf8/_ydbposix.so` for UTF-8 mode processes.

The :code:`$ydb_dist/ydb_env_set` file that you can source to set environment variables and the :code:`$ydb_dist/ydb` script to run YottaDB automatically define appropriate values for :code:`$ydb_xc_ydbposix` and :code:`$ydb_routines` to allow processes to execute ydbposix.

---------------------------------
(High level) ^%ydbposix entryrefs
---------------------------------

Except for any entryrefs starting with $$, which must be called as functions, ^%ydbposix entryrefs as described below can be called either as functions or with a DO. Except where noted, each entryref can be invoked in either all upper-case or all lower-case, but not with mixed case. These entryrefs have no abbreviations.

++++++++++++++++++++++++++
chmod^%ydbposix(name,mode)
++++++++++++++++++++++++++

Changes the permissions of a file to those specified, whether in symbolic or numeric representation.

++++++++++++++++++++++++++++++++++++++++
clockgettime^%ydbposix(clock,.sec,.nsec)
++++++++++++++++++++++++++++++++++++++++

Retrieves the time of the specified clock, in symbolic or numeric representation, with nanosecond resolution. Note that nanosecond resolution does not mean nanosecond accuracy.

++++++++++++++++++++++++++++++
$$clockval^%ydbposix(clockval)
++++++++++++++++++++++++++++++

Given a symbolic clock ID as a string,, e.g., "CLOCK_REALTIME", returns the numeric value of that clock. See also the description of :code:`$&ydbposix.clockval()`.

+++++++++++++++++++++++++
cp^%ydbposix(source,dest)
+++++++++++++++++++++++++

Copy a file, preserving its permissions.

++++++++++++++++++++++++++++++
$$filemodeconst^%ydbposix(sym)
++++++++++++++++++++++++++++++

Given a symbolic file mode as a string,, e.g., "S_IRWXU", returns the numeric value of that mode. See also the description of :code:`$&ydbposix.filemodeconst()`.

+++++++++++++++++++++++++++++
mkdir^%ydbposix(dirname,mode)
+++++++++++++++++++++++++++++

Given a directory name as a string, and a mode, as either a symbolic or numeric value, creates the directory.

+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
mktime^%ydbposix(year,mon,mday,hour,min,sec,.wday,.yday,.isdst,.unixtime)
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Converts a broken-down time structure to calendar time representation, populating variables to contain the day of the week, day of the year, daylight saving status, and UNIX time.

+++++++++++++++++++++++++++++
mktmpdir^%ydbposix(.template)
+++++++++++++++++++++++++++++

With a directory name template ending in "XXXXXX" creates a directory with a unique name, replacing the "XXXXXX" to return the name of the directory created in template.

++++++++++++++++++++++++++++++++++
realpath^%ydbposix(name,.realpath)
++++++++++++++++++++++++++++++++++

Retrieves the canonicalized absolute pathname to the file specified by name and stores it in realpath.

++++++++++++++++++++++++++++++
regfree^%ydbposix(pregstrname)
++++++++++++++++++++++++++++++

Given the name of a variable with a compiled regular expression as a string, frees the memory and ZKILLs the variable. Note that regfree() requires a variable name to be passed in as a string. For example, after :code:`regmatch^%ydbposix("AIXHP-UXLinuxSolaris","ux","REG_ICASE",,.matches,1)`, the call to regfree to release the memory would be :code:`regfree^%ydbposix("%ydbposix(""regmatch"",""ux"",%ydbposix(""regmatch"",""REG_ICASE""))")`.

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
regmatch^%ydbposix(str,patt,pattflags,matchflags,.matchresults,maxresults)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

* Regular expression matching in string **str** for pattern **patt**, compiling the pattern if needed using :code:`regcomp()` and matching using :code:`regmatch()`.

* **pattflags** condition the pattern compilation with :code:`regcomp()`.

* **matchflags** condition the matching performed by :code:`regexec()`. To pass multiple flags, simply add the numeric values of the individual flags as provided by :code:`$$regsymval^%ydbposix()`.

* **maxresults** specifies the maximum number of matches.

* The function returns results as an array, where the value of :code:`matchresults(n,"start")` provides the starting character position for the nth match, and the value of :code:`matchresults(n,"end")` provides the character position for the first character after a match; e.g. :code:`$extract(str,matchresults(2,"start"),matchresults(2,"end")-1)` returns the second matching substring.

When called as a function, :code:`regmatch^%ydbposix` returns 1 on successful match and 0 if there was no match. On a successful match, the function KILLs all prior data in matchresults and otherwise leaves it unchanged. After a failed compilation, :code:`%ydbposix("regcomp","errno")` contains the error code from errlog(). When the match encounters an error (as opposed to a failure to match), :code:`%ydbposix("regexec","errno")` contains the value of errno. Local variable nodes :code:`%ydbposix("regmatch",patt,pattflags)` contain descriptors of compiled patterns and *must not be modified by your application code*. Be sure to read Memory Usage Considerations, below. Refer to :code:`man regex` for more information about regular expressions and pattern matching.

++++++++++++++++++++++++++
$$regsymval^%ydbposix(sym)
++++++++++++++++++++++++++

Returns the numeric value of a symbolic constant used in regular expression pattern matching, such as "REG_ICASE". Also, it provides the sizes of certain structures that M code needs to have access to, when provided as strings, such as :code:`sizeof(regex_t)`, :code:`sizeof(regmatch_t)`, and :code:`sizeof(regoff_t)`.

++++++++++++++++++++++++
rmdir^%ydbposix(dirname)
++++++++++++++++++++++++

Removes a directory. For the call to succeed, the directory must be empty.

++++++++++++++++++++++++++++++++++++++
setenv^%ydbposix(name,value,overwrite)
++++++++++++++++++++++++++++++++++++++

Sets an environment variable to the specified value, overwriting or preserving the existing value as indicated. Note that this function is deprecated and retained for backward compatibility. Use `VIEW SETENV <https://docs.yottadb.com/ProgrammersGuide/commands.html#key-words-in-view-command>`_ instead.

++++++++++++++++++++++++
statfile^%ydbposix(f,.s)
++++++++++++++++++++++++

Provides information about file **f** in nodes of local variable **s**. All prior nodes of **s** are deleted. When called as a function, **statfile** returns 1 unless the underlying call to **stat()** failed. Refer to :code:`man 2 stat` for more information.

++++++++++++++++++++++++++++++
symlink^%ydbposix(target,name)
++++++++++++++++++++++++++++++

Creates a symbolic link to a file with the specified name.

++++++++++++++++++++++++++++++
sysconf^%ydbposix(name,.value)
++++++++++++++++++++++++++++++

Obtains the value of the specified configuration option and saves it into the provided container.

++++++++++++++++++++++++++++++
$$sysconfval^%ydbposix(option)
++++++++++++++++++++++++++++++

Given a symbolic configuration option as a string,, e.g., "ARG_MAX", returns the numeric value of that option. See also the description of :code:`$&ydbposix.sysconfval()`.

+++++++++++++++++++++++++++++++++++++++++++++++
syslog^%ydbposix(message,format,facility,level)
+++++++++++++++++++++++++++++++++++++++++++++++

Provides a mechanism to log messages to the system log. format defaults to "%s", facility to "LOG_USER" and level to "LOG_INFO". When called as a function, syslog returns 1. Refer to :code:`man syslog` for more information. Unless you really need the fine-grained control this offers, `$ZSYSLOG() <https://docs.yottadb.com/ProgrammersGuide/functions.html#zsyslog>`_ should suffice for most needs.

++++++++++++++++++++++++
syslogval^%ydbposix(msg)
++++++++++++++++++++++++

Given a symbolic syslog priority as a string,, e.g., "LOG_ALERT", returns the numeric value of that priority. See also the description of :code:`$&ydbposix.syslogval()`.

++++++++++++++++++++++++
unsetenv^%ydbposix(name)
++++++++++++++++++++++++

Unsets an environment variable. Note that this function is deprecated and retained for backward compatibility. Use `VIEW UNSETENV <https://docs.yottadb.com/ProgrammersGuide/commands.html#key-words-in-view-command>`_ instead.

++++++++++++++++++++++++++++++
umask^%ydbposix(mode,.oldMode)
++++++++++++++++++++++++++++++

Sets the current user's file mode creation mask, passed in as a symbolic or numeric value, and returns the previous mask's numeric value in the second argument.

++++++++++++++++++++++
utimes^%ydbposix(name)
++++++++++++++++++++++

Updates the access and modification timestamps of a file. The implemented functionality is equivalent to a "touch" command.

+++++++++++++++++++
$$version^%ydbposix
+++++++++++++++++++

Returns the version of the ydbposix plugin.

++++++++++++++++++++
$$zhorolog^%ydbposix
++++++++++++++++++++

Provides the time in $horolog format, but with microsecond resolution of the number of seconds since midnight. Note that microsecond resolution does not mean microsecond accuracy. This function is deprecated and retained for backward compatibility. Consider using `$ZHOROLOG <https://docs.yottadb.com/ProgrammersGuide/isv.html#zhorolog>`_ instead.

----------------------------
Examples of ^%ydbposix usage
----------------------------

Below are examples of usage of high level entryrefs in ^%ydbposix. The file _ydbposixtest.m contains examples of use of the functions in ydbposix.

.. code-block:: none

    YDB>set str="THE QUICK BROWN FOX JUMPS OVER the lazy dog"

    YDB>write:$$regmatch^%ydbposix(str,"the",,,.result) $extract(str,result(1,"start"),result(1,"end")-1)
    the
    YDB>write:$$regmatch^%ydbposix(str,"the","REG_ICASE",,.result) $extract(str,result(1,"start"),result(1,"end")-1)
    THE
    YDB>

    YDB>set retval=$$statfile^%ydbposix($ztrnlnm("ydb_dist")_"/yottadb",.stat) zwrite stat
    stat("atime")=1332555721
    stat("blksize")=4096
    stat("blocks")=24
    stat("ctime")=1326986163
    stat("dev")=2052
    stat("gid")=0
    stat("ino")=6567598
    stat("mode")=33133
    stat("mtime")=1326986160
    stat("nlink")=1
    stat("rdev")=0
    stat("size")=8700
    stat("uid")=0

    YDB>write stat("mode")\$$filemodeconst^%ydbposix("S_IFREG")#2 ; It is a regular file
    1
    YDB>

    YDB>write $$version^%ydbposix
    v4.0.0
    YDB>

--------------------------
(Low Level) ydbposix calls
--------------------------

The high level entryrefs in ^%ydbposix access low level functions in ydbposix.c that directly wrap POSIX functions. Unless otherwise noted, functions return 0 for a successful completion, and non-zero otherwise. Note that some POSIX functions only return success, and also that a non-zero return value triggers a "%YDB-E-ZCSTATUSRET, External call returned error status" YottaDB runtime error for your $ETRAP or $ZTRAP error handler. Where :code:`errno` is the last argument passed by reference, it takes on the value of the errno from the underlying system call.

.. note::

   The ydbposix YottaDB interface to call out to POSIX functions is a low-level interface designed for use by programmers rather than end-users. Misuse, abuse and bugs can result in programs that are fragile, hard to troubleshoot and potentially insecure.

++++++++++++++++++++++++++++++++++
$&ydbposix.chmod(file,mode,.errno)
++++++++++++++++++++++++++++++++++

Changes the permissions of a file to those specified. See :code:`man 2 chmod` for more infornmation.

++++++++++++++++++++++++++++++++++++++++++++++++++++
$&ydbposix.clockgettime(clock,.tvsec,.tvnsec,.errno)
++++++++++++++++++++++++++++++++++++++++++++++++++++

Returns the time of the specified clock in seconds and nanoseconds. See :code:`man clock_gettime` on your POSIX system for more information.

+++++++++++++++++++++++++++++++++++++++
$&ydbposix.clockval(fmsymconst,.symval)
+++++++++++++++++++++++++++++++++++++++

Takes a symbolic clock ID constant in fmsymconst and returns the numeric value in symval. If no such constant exists, the return value is non-zero. Please see the :code:`clock_gettime()` function man page for the list of available clocks.

+++++++++++++++++++++++++++++++++
$&ydbposix.cp(source,dest,.errno)
+++++++++++++++++++++++++++++++++

Copy file source to dest, preserving its permissions. Note that this function is not a wrapper to a single POSIX function but a basic POSIX-conformant implementation of the cp command available on most UNIX OSs.

++++++++++++++++++++++++++++++++++++++++++++
$&ydbposix.filemodeconst(fmsymconst,.symval)
++++++++++++++++++++++++++++++++++++++++++++

Takes a symbolic regular file mode constant in fmsymconst and returns the numeric value in symval. If no such constant exists, the return value is non-zero. Currently supported fmsymconst constants are the following. Please see :code:`stat()` function man page for their meaning.

.. code-block:: none

        "S_IFBLK",  "S_IFCHR", "S_IFDIR", "S_IFIFO", "S_IFLNK", "S_IFMT",  "S_IFREG",
        "S_IFSOCK", "S_IRGRP", "S_IROTH", "S_IRUSR", "S_IRWXG", "S_IRWXO", "S_IRWXU",
	"S_ISGID",  "S_ISUID", "S_ISVTX", "S_IWGRP", "S_IWOTH", "S_IWUSR", "S_IXGRP",
	"S_IXOTH",  "S_IXUSR"

++++++++++++++++++++++++++++++++++++++++++++++
$&ydbposix.gettimeofday(.tvsec,.tvusec,.errno)
++++++++++++++++++++++++++++++++++++++++++++++

Returns the current time as the number of seconds since the UNIX epoch (00:00:00 UTC on 1 January 1970) and the number of microseconds within the current second. See :code:`man gettimeofday` on your POSIX system for more information.

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
$&ydbposix.localtime(tvsec,.sec,.min,.hour,.mday,.mon,.year,.wday,.yday,.isdst,.errno)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Takes a time value in tvsec represented as a number of seconds from the epoch - for example as returned by gettimeofday() - and returns a number of usable fields for that time value. See :code:`man localtime` for more information.

++++++++++++++++++++++++++++++++++++++
$&ydbposix.mkdir(.dirname,mode,.errno)
++++++++++++++++++++++++++++++++++++++

Creates a directory dirname with the specified permissions. See :code:`man 2 mkdir` for more information.

+++++++++++++++++++++++++++++++++++
$&ydbposix.mkdtemp(template,.errno)
+++++++++++++++++++++++++++++++++++

With a template for a temporary directory name - the last six characters must be "XXXXXX" - creates a unique temporary directory and updates template with the name. See :code:`man mkdtemp` for more information.

+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
$&ydbposix.mktime(year,month,mday,hour,min,sec,.wday,.yday,.isdst,.unixtime,.errno)
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Takes elements of POSIX broken-down time and returns time since the UNIX epoch in seconds in unixtime. Note that year is the offset from 1900 (i.e, 2014 is 114) and month is the offset from January (i.e., December is 11). wday is the day of the week offset from Sunday and yday is the day of the year offset from January 1 (note that the offsets of dates starting with March 1 vary between leap years and non-leap years). isdst should be initialized to one of 0, 1, or -1 as required by the POSIX mktime() function. If a $horolog value is the source of broken-down time, isdst should be -1 since YottaDB $horolog reflects the state of Daylight Savings time in the timezone of the process, but the M application code does not know whether or not Daylight Savings Time is in effect; on return from the call, it is 0 if Daylight Savings Time is in effect and 1 if it is not. See man mktime for more information.

++++++++++++++++++++++++++++++++++++++++
$&ydbposix.realpath(file,.result,.errno)
++++++++++++++++++++++++++++++++++++++++

Retrieves the canonicalized absolute pathname to the specified file and stores it in result. See :code:`man realpath` for more information.

++++++++++++++++++++++++++++++++++++++++++++++++
$&ydbposix.regcomp(.pregstr,regex,cflags,.errno)
++++++++++++++++++++++++++++++++++++++++++++++++

Takes a regular expression regex, compiles it and returns a pointer to a descriptor of the compiled regular expression in pregstr. Application code *must not* modify the value of pregstr. cflags specifies the type of regular expression compilation. See :code:`man regex` for more information.

++++++++++++++++++++++++++++++++++++++++
$&ydbposix.regconst(regsymconst,.symval)
++++++++++++++++++++++++++++++++++++++++

Takes a symbolic regular expression constant in regsymconst and returns the numeric value in symval. If no such constant exists, the return value is non-zero. The $$regsymval^%ydbposix() function uses :code:`$&ydbposix.regconst()`. Currently supported values of regsymconst are

.. code-block:: none

	"REG_BADBR",      "REG_BADPAT",      "REG_BADRPT",         "REG_EBRACE",       "REG_EBRACK",    "REG_ECOLLATE",
	"REG_ECTYPE",     "REG_EESCAPE",     "REG_EPAREN",         "REG_ERANGE",       "REG_ESPACE",    "REG_ESUBREG",
	"REG_EXTENDED",   "REG_ICASE",       "REG_NEWLINE",        "REG_NOMATCH",      "REG_NOSUB",     "REG_NOTBOL",
	"REG_NOTEOL",     "sizeof(regex_t)", "sizeof(regmatch_t)", "sizeof(regoff_t)"

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
$&ydbposix.regexec(pregstr,string,nmatch,.pmatch,eflags,.matchsuccess)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Takes a string in string and matches it against a previously compiled regular expression whose descriptor is in pregstr with matching flags in eflags, for which numeric values can be obtained from symbolic values with :code:`$$regconst^%ydbposix()`. nmatch is the maximum number of matches to be returned and pmatch is a predefined string in which the function returns information about substrings matched. pmatch must be initialized to at least nmatch times the size of each match result which you can effect with: :code:`set $zpiece(pmatch,$zchar(0),nmatch*$$regsymval("sizeof(regmatch_t)")+1)=""` matchsuccess is 1 if the match was successful, 0 if not. The return value is 0 for both successful and failing matches; a non-zero value indicates an error. See :code:`man regex` for more information.

+++++++++++++++++++++++++++
$&ydbposix.regfree(pregstr)
+++++++++++++++++++++++++++

Takes a descriptor for a compiled regular expression, as provided by :code:`$&ydbposix.regcomp()` and frees the memory associated with the compiled regular expression. After executing :code:`$&ydbposix.regfree()`, the descriptor can be safely deleted; deleting a descriptor prior to calling this function results in a memory leak because deleting the descriptor makes the memory used for the compiled expression unrecoverable.

++++++++++++++++++++++++++++++++++++++++++++++++
$&ydbposix.regofft2int(regofftbytes,.regofftint)
++++++++++++++++++++++++++++++++++++++++++++++++

On both little- and big-endian platforms, takes a sequence of bytes of size sizeof(regoff_t) and returns it as an integer. :code:`$$regsconst^%ydbposix("sizeof(regoff_t)")` provides the size of regoff_t. Always returns 0.

+++++++++++++++++++++++++++++++++
$&ydbposix.rmdir(pathname,.errno)
+++++++++++++++++++++++++++++++++

Removes a directory, which must be empty. See :code:`man 2 rmdir` for more information.

++++++++++++++++++++++++++++++++++++++++++++++
$&ydbposix.setenv(name,value,overwrite,.errno)
++++++++++++++++++++++++++++++++++++++++++++++

Sets the value of an environment variable. name is the name of an environment variable (i.e., without a leading "$") and value is the value it is to have ($char(0) cannot be part of the value). If the name already has a value, then overwrite must be non-zero in order to replace the existing value. See :code:`man setenv` for more information.

+++++++++++++++++++++++++++++++++++++
$&ydbposix.signalval(signame,.sigval)
+++++++++++++++++++++++++++++++++++++

Takes a signal name (such as "SIGUSR1") and provides its value in sigval. A non-zero return value means that no value was found for the name. Currently supported signames are

.. code-block:: none

	"SIGABRT", "SIGALRM", "SIGBUS",  "SIGCHLD", "SIGCONT", "SIGFPE",  "SIGHUP",  "SIGILL",
	"SIGINT",  "SIGKILL", "SIGPIPE", "SIGQUIT", "SIGSEGV", "SIGSTOP", "SIGTERM", "SIGTRAP",
	"SIGTSTP", "SIGTTIN", "SIGTTOU", "SIGURG",  "SIGUSR1", "SIGUSR2", "SIGXCPU", "SIGXFSZ"

+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
$&ydbposix.stat(fname,.dev,.ino,.mode,.nlink,.uid,.gid,.rdev,.size,.blksize,.blocks,.atime,.atimen,.mtime,mtimen,.ctime,.ctimen,.errno)
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Takes the name of a file in fname, and provides information about it. See :code:`man 2 stat` for more information.

++++++++++++++++++++++++++++++++++++++
$&ydbposix.symlink(target,name,.errno)
++++++++++++++++++++++++++++++++++++++

Creates a symbolic link to a file with the specified name. See :code:`man symlink` for more information.

++++++++++++++++++++++++++++++++++++++
$&ydbposix.sysconf(name,.value,.errno)
++++++++++++++++++++++++++++++++++++++

Obtains the value of the specified configuration option and saves it to value. The name argument needs to be a valid int understandable by sysconf() rather than a corresponding system-defined constant. For instance, _SC_ARG_MAX and _SC_2_VERSION's values should be used for ARG_MAX and POSIX2_VERSION options, respectively. Note that for certain limits the value of -1 can be legitimately returned, indicating that there is no definite limit. See :code:`man sysconf` for more information.

+++++++++++++++++++++++++++++++++++++++++
$&ydbposix.sysconfval(fmsymconst,.symval)
+++++++++++++++++++++++++++++++++++++++++

Takes a sysconf option name (such as "PAGESIZE") and provides the corresponding _SC... value in sigval. A non-zero return value means that no value was found for the name. Currently supported sysconf options are

.. code-block:: none

        "ARG_MAX",          "BC_BASE_MAX",   "BC_DIM_MAX",      "BC_SCALE_MAX",    "BC_STRING_MAX",   "CHILD_MAX",
       	"COLL_WEIGHTS_MAX", "EXPR_NEST_MAX", "HOST_NAME_MAX",   "LINE_MAX",        "LOGIN_NAME_MAX",  "OPEN_MAX",
       	"PAGESIZE",         "POSIX2_C_DEV",  "POSIX2_FORT_DEV", "POSIX2_FORT_RUN", "POSIX2_SW_DEV",   "POSIX2_VERSION",
       	"RE_DUP_MAX",       "STREAM_MAX",    "SYMLOOP_MAX",     "TTY_NAME_MAX",    "TZNAME_MAX",      "_POSIX2_LOCALEDEF",
       	"_POSIX_VERSION"

++++++++++++++++++++++++++++++++++++++++++
$&ydbposix.syslog(priority,message,.errno)
++++++++++++++++++++++++++++++++++++++++++

Takes a priority, format and message to log on the system log. Priority is itself an OR of a facility and a level. See :code:`man syslog` for more information.

++++++++++++++++++++++++++++++++++++++++++++++++++++
$&ydbposix.syslogconst(syslogsymconst,.syslogsymval)
++++++++++++++++++++++++++++++++++++++++++++++++++++

Takes a symbolic syslog facility or level name (e.g., "LOG_USER") in syslogsymconst and returns its value in syslogsymval. A non-zero return value means that a value was not found. Currently supported values of syslogsymconst are

.. code-block:: none

        "LOG_ALERT",  "LOG_CRIT",   "LOG_DEBUG",  "LOG_EMERG",  "LOG_ERR",    "LOG_INFO",   "LOG_LOCAL0",
	"LOG_LOCAL1", "LOG_LOCAL2", "LOG_LOCAL3", "LOG_LOCAL4", "LOG_LOCAL5", "LOG_LOCAL6", "LOG_LOCAL7",
	"LOG_NOTICE", "LOG_USER",   "LOG_WARNING"

+++++++++++++++++++++++++++++++++++++++
$&ydbposix.umask(mode,.prevMode,.errno)
+++++++++++++++++++++++++++++++++++++++

Sets the current user's file mode creation mask and returns the previous mask in the second argument. See :code:`man umask` for more information.

++++++++++++++++++++++++++++++++
$&ydbposix.unsetenv(name,.errno)
++++++++++++++++++++++++++++++++

Unsets the value of an environment variable. See :code:`man umask` for more information.

++++++++++++++++++++++++++++++
$&ydbposix.utimes(file,.errno)
++++++++++++++++++++++++++++++

Updates the access and modification timestamps of a file. See :code:`man utimes` for more information.

:code:`_ydbposixtest.m` contains examples of use of the low level ydbposix interfaces.

----------------------------
The %ydbposix local variable
----------------------------

The ydbposix plugin uses the :code:`%ydbposix` local variable to store information pertaining to POSIX external calls. For example, a call to :code:`$&regsymval^%ydbposix("REG_NOTBOL")` that returns a numeric value also sets the node :code:`%ydbposix("regmatch","REG_NOTBOL")` to that value. Subsequent calls to :code:`$$regsymval^%ydbposix("REG_NOTBOL")` return the value stored in %ydbposix rather than calling out the low level function. This means that KILLs or NEWs that remove the value in :code:`%ydbposix`, result in a call to the low level function, and SETs of values may cause inappropriate results from subsequent invocations.

If your application already uses :code:`%ydbposix` for another purpose, you can edit :code:`_ydbposix.m` and replace all occurrences of %ydbposix with another available local variable name.

---------------------------
Memory Usage Considerations
---------------------------

When :code:`$&ydbposix.regcomp()` is called to compile a regular expression, it allocates needed memory, and returns a descriptor to the compiled code. Until a subsequent call to :code:`$&ydbposix.regfree()` with that descriptor, the memory is retained. The high level :code:`regmatch^%ydbposix()` entryref stores descriptors in :code:`%ydbposix("regmatch",...)` nodes. If an application deletes or modifies these nodes prior to calling :code:`$&ydbposix.regfree()` to release compiled regular expressions, that memory cannot be released during the life of the process. If your application uses scope management (using KILL and/or NEW) that adversely interacts with this, you should consider modifying _ydbposix.m to free the cached compiled regular expression immediately after the call to :code:`$&ydbposix.regexec()`, or to store the descriptors in a global variable specific to the process, rather than in a local variable.

--------------
Error Handling
--------------

Entryrefs within :code:`^%ydbposix` except the top one (calling which is not meaningful), raise errors but do not set their own error handlers with $ETRAP or $ZTRAP. Application code error handlers should deal with these errors. In particular, note that non-zero function return values from :code:`$&ydbposix` functions result in ZCSTATUSRET errors.

Look at the end of :code:`_ydbposix.m` for errors raised by entryrefs in %ydbposix.
