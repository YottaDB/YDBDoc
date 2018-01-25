
.. index:: Basic Operations

=======================
3. Basic Operations
=======================

.. contents::
   :depth: 2

------------------------------
YottaDB Environment Setup
------------------------------

Several environment variables control the operation of YottaDB. Some of them must be set up for normal operation, where as for others YottaDB assumes a default value if they are not set.

Your YottaDB distribution comes with many scripts that set up a default YottaDB environment for the shell of your choice. These scripts are as follows: 


**gtmprofile**: uses reasonable defaults to set up a system and YottaDB application development environment for POSIX shells. The gtmprofile script sets default values for environment variables gtm_dist, gtmgbldir, gtm_icu_version, gtm_log, gtm_principal_editing, gtm_prompt, gtm_retention, gtmroutines, gtm_tmp, and gtmver. When you source the gtmprofile script, it creates a default execution environment (global directory and a default database file with BEFORE_IMAGE journaling) if none exists.

* **gtmcshrc**: sets up a default YottaDB environment for C-shell compatible shells. It sets up default values for gtm_dist, gtmgbldir, gtm_chset and gtmroutines. It also creates aliases so you can execute YottaDB and its utilities without typing the full path.

* **gtmbase**: detects the shell type and adds gtmprofile to .profile or gtmchsrc to .cshrc so the shell automatically sources gtmprofile or gtmschrc on a subsequent login operation. YottaDB does not recommend using gtmbase as is - use it as an example of a script for you to develop suitable for your systems. It is not as actively maintained as the gtmprofile script.

* **gtm**: starts YottaDB in direct mode on POSIX shells. The gtm script sources gtmprofile. It also deletes prior generation journal and temporary files older than the number of days specified by the environment variable gtm_retention. It attempts to automatically recover the database when it runs and as such is suitable for "out of the box" usage of YottaDB. Although it will work for large multi-user environments, you may want to modify or replace it with more efficient scripting.

* **gdedefaults**: a GDE command file that specifies the default values for database characteristics defined by GDE.

These scripts are designed to give you a friendly out-of-the-box YottaDB experience. Even though you can set up an environment for normal YottaDB operation without using these scripts, it is important to go through these scripts to understand how to configure environments. 


++++++++++
gtmprofile
++++++++++

On POSIX shells, gtmprofile helps you set an environment for single-user, non-replicated use of YottaDB.

gtmprofile sets reasonable defaults for the following environment variables for normal YottaDB operation:

.. parsed-literal::
   gtmdir, gtm_dist, gtm_etrap, gtmgbldir, gtm_icu_version, gtm_log, gtm_principal_editing, gtm_prompt, gtm_repl_instance, gtm_retention, gtmroutines, gtm_tmp, gtmver 

You can set the following environment variables before sourcing gtmprofile or running the gtm script;


* **gtm_chset** - set this to "UTF-8" to run YottaDB in UTF-8 mode; it defaults to M mode. As UTF-8 mode requires a UTF-8 locale to be set in LC_CTYPE or LC_ALL, if a locale is not specified, gtmprofile also attempts to set a UTF-8 locale. Since YottaDB in UTF-8 mode often requires gtm_icu_version to be set, if it is not set, gtmprofile attempts to determine the ICU version on the system and set it. This requires the icu-config program to be installed and executable by gtmprofile.

* **gtmdir** - set this to define a directory for the environment set by gtmprofile.

The following shell variables are used by the script and left unset at its completion: 

.. parsed-literal::
   old_gtm_dist, old_gtmroutines, old_gtmver, tmp_gtm_tmp, tmp_passwd. 

The $gtmroutines value set by the gtmprofile script enables auto-relink by default for object files in the $gtmdir/$gtmver/o directory in M mode and $gtmdir/$gtmver/o/utf8 in UTF-8 mode. Auto-relink requires shared memory resources and limits beyond those for database operation. If your system has inadequate shared memory configured, YottaDB displays messages along the lines of:

.. parsed-literal::
   %GTM-E-SYSCALL, Error received from system call shmget() failed

Refer to your OS documentation to configure shared memory limits (for example, on common Linux systems, the kernel.shmmax parameter in /etc/sysctl.conf).

The gtmprofile (and gtm) scripts, by design, are idempotent so that calling them repeatedly is safe. The YottaDB installation process ensures that gtmprofile always sets gtm_dist correctly. Idempotency is implemented by checking the value of $gtm_dist and skipping all changes to environment variables if gtm_dist is already defined.

When gtm sources gtmprofile, it provides a default execution environment (global directory and a default database (with BEFORE_IMAGE journaling) if none exists. By default, it creates the database in $HOME/.fis-gtm with a structure like the following; note that this directory structure has different locations for YottaDB routines (r), object files (o), and database-related files (g):

.. parsed-literal::

   .fis-gtm
      | -- r
      | -- r1.00
      | | -- g 
      | | | -- gtm.dat 
      | | | -- gtm.gld 
      | | ` -- gtm.mjl 
      | | -- o 
      | | ` -- utf8 
      | ` -- r 
      | -- r1.10
       | -- g 
       | | -- gtm.dat 
       | | -- gtm.gld 
          | ` -- gtm.mjl 
       | -- o 
       | ` -- utf8 
         ` -- r


where r1.10 represents the current release and platform information and r1.00 represents a previously used YottaDB release.

On 64-bit platforms in M mode, gtmprofile sets the environment variable gtmroutines to something like the following (where $gtm_dist and $gtmver are as discussed above):

.. parsed-literal::
   $gtmdir/$gtmver/o*($gtmdir/$gtmver/r $gtmdir/r) $gtm_dist/plugin/o($gtm_dist/plugin/r) $gtm_dist/libgtmutil.so $gtm_dist

$gtmdir/$gtmver/o*($gtmdir/$gtmver/r $gtmdir/r) specifies that YottaDB searches for routines in $gtmdir/$gtmver/r, then $gtmdir/r, using $gtmdir/$gtmver/o for object code, then for routines in the plugin subdirectory of $gtm_dist, then in $gtm_dist, looking first for a shared library of routines distributed with YottaDB and then for other routines subsequently installed there. The * -suffix after the object directory enables the auto-relink facility.

For a comprehensive discussion of YottaDB source and object file management, refer to the $ZROUTINES section in the Programmer's Guide.

When $gtm_chset is set to UTF-8, gtmprofile sets gtmroutines to something like this:

.. parsed-literal::
   $gtmdir/$gtmver/o/utf8*($gtmdir/$gtmver/r $gtmdir/r) $gtm_dist/plugin/o/utf8($gtm_dist/plugin/r) $gtm_dist/libgtmutil.so $gtm_dist

Note that gtmprofile sets $gtm_dist in UTF-8 mode to the utf8 subdirectory of the YottaDB installation directory. If you have installed any plugins that include shared libraries, the gtmprofile script includes those. For example, with the POSIX and ZLIB plugins installed on a 64-bit platform, gtmdir set to /home/jdoe1 and YottaDB installed in /opt/yottadb/r1.10, gtmprofile would set gtmroutines to:

.. parsed-literal::
   /home/jdoe1/.yottadb/r1.10/o*(/home/jdoe1/.yottadb/r1.10/r /home/jdoe1/.yottadb/r) /usr/lib/yottadb/r1.10/plugin/o/_POSIX.so /usr/lib/yottadb/r1.10/plugin/o/_ZLIB.so /usr/lib/yottadb/r1.10/plugin/o(/usr/lib/yottadb/r1.10/plugin/r) /usr/lib/yottadb/r1.10/libgtmutil.so /usr/lib/yottadb/r1.10

.. note::
   This scenario of sourcing gtmprofile is only for the sake of example. Consult your system administrator before implementing gtmprofile for a multi-user environment.

gtmprofile creates the following aliases:

.. parsed-literal::
   alias dse="$gtm_dist/dse"
   alias gde="$gtm_dist/mumps -run GDE"
   alias gtm="$gtm_dist/gtm"
   alias lke="$gtm_dist/lke"
   alias mupip="$gtm_dist/mupip"

If /var/log/yottadb/$gtmver directory exists, gtmprofile sets it as the value for $gtm_log. If gtmprofile does not find /var/log/yottadb/$gtmver, it uses $gtm_tmp to set the value of $gtm_log.

++++++++++
gtmscshrc
++++++++++

Sets a default YottaDB environment for C type shell. It sets the $gtm_dist, $gtmgbldir, $gtm_chset, $gtmroutines, and adds $gtm_dist to the system environment variable PATH.

To source the gtmcshrc script, type:

.. parsed-literal::
   $ source <path_to_YottaDB_installation_directory>/gtmcshrc 

You can also run the gtmbase script which places the above command in the .cshrc file so the script will get automatically sourced the next time you log in.

gtmcshrc also creates the following aliases. 

.. parsed-literal::
   alias gtm '$gtm_dist/mumps -direct'
   alias mupip '$gtm_dist/mupip'
   alias lke '$gtm_dist/lke'
   alias gde '$gtm_dist/mumps -r ^GDE'
   alias dse '$gtm_dist/dse'

Now you run can YottaDB and its utilities without specifying a full path to the directory in which YottaDB was installed.

++++++++++++++++
 gtmbase 
++++++++++++++++

Adds the following line to .profile or .cshrc file depending on the shell.

In the POSIX shell, gtmbase adds the following line to .profile:

.. parsed-literal::
   . <gtm_dist pathname>/gtmprofile
   
In the C shell, adds the following line to .cshrc:

.. parsed-literal::
   source <gtm_dist pathname>/gtmcshrc 

+++++++++++++
 gdedefaults 
+++++++++++++

Specifies default or template values for database characteristics defined by GDE.

+++
gtm
+++

The gtm script starts with #!/bin/sh so it can run with any shell. Also, you can use it to both run a program and run in direct mode. It sources gtmprofile and sets up default YottaDB database files with BEFORE_IMAGE journaling. It automatically recovers the database on startup. This script sets up everything you need to run YottaDB for a simple out-of-box experience.

For multi-user multi-environment systems, you should modify or replace the gtm script for your configuration.

The gtm script deletes all prior generation journal files (\*_<time and date stamp> files) older than $gtm_retention days from the directory that contains the global directory (as pointed to by $gtmgbldir) and any subdirectories of that directory. By default, $gtm_retention is 42. However, you might want to align it with the backup frequency of your database.

Note that the removal of prior generation journal files is not specific to the database/journal files indicated by the current $gtmgbldir but the directory from where you run the gtm script.

If you plan to use YottaDB in UTF-8 mode, set $gtm_chset to UTF-8 and LC_CTYPE to a UTF-8 locale and then run the gtm script.

If you intend to use Database Encryption, set the gtm_passwd and gtmcrypt_config environment variables first and then run the gtm script.

**To run the gtm script type:**

.. parsed-literal::
   $ <path to your YottaDB Distribution>/gtm

**To invoke the help to assist first-time users, type:**

.. parsed-literal::
   $ <path to your YottaDB Distribution>/gtm -help
   gtm -dir[ect] to enter direct mode (halt returns to shell)
   gtm -run <entryref> to start executing at an entryref
   gtm -help / gtm -h / gtm -? to display this text


----------------------
Environment Variables
----------------------

A comprehensive list of environment variables that are directly or indirectly used by YottaDB follows:

**EDITOR** is a standard system environment variable that specifies the full path to the editor to be invoked by YottaDB in response to the ZEDit command (defaults to vi, if $EDITOR is not set).

**GTMCI** specifies the call-in table for function calls from C code to M code.

**GTMXC_gpgagent** specifies the location of gpgagent.tab. By default, YottaDB places gpgagent.tab in the $gtm_dist/plugin/ directory. GTMXC_gpgagent is used by pinentry-gtm.sh and is meaningful only if you are using Gnu Privacy Guard version 2.

**LC_CTYPE** is a standard system environment variable used to specify a locale. When $gtm_chset has the value "UTF-8", $LC_CTYPE must specify a UTF-8 locale (e.g., "en_US.utf8").

**LC_ALL** is a standard system environment variable used to select a locale with UTF-8 support. LC_ALL is an alternative to LC_TYPE, which overrides LC_TYPE and has a more pervasive effect on other aspects of the environment beyond YottaDB.

**LD_LIBRARY_PATH** (LIBPATH on AIX) is a standard system environment variable used to modify the default library search path. Use this extension when YottaDB relies on custom compiled libraries that do not reside in the default library search path. For example ICU, GPG, OpenSSL and/or zlib libraries.

**TZ** is a standard system environment variable that specifies the timezone to be used by a YottaDB process, if they are not to use the default system timezone. YottaDB uses the system clock for journal time stamps on the assumption it reflects UTC time.

**gtmcompile** specifies the initial value of the $ZCOmpile ISV. The SET command can alter the value of $ZCOMPILE in an active process.

**gtmcrypt_config** specifies the location of the configuration file required for database encryption, Sequential file, PIPE, and FIFO device encryption and/or TLS support. A configuration file is divided into two sections: the database encryption section and the TLS section. The database encryption section contains a list of database files and their corresponding key files. You do not need to add a database encryption section if you are not using an encrypted database, or a TLS section if you are not using TLS for replication or sockets. The TLS section provides information needed for OpenSSL (in the reference plugin implementation) or other encryption package, such as the location of the root certification authority certificate in PEM format and leaf-level certificates with their corresponding private key files. Note that the use of the gtmcrypt_config environment variable requires prior installation of the libconfig package.

**gtmcrypt_FIPS** specifies whether the plugin reference implementation should attempt to use either OpenSSL or Libgcrypt to provide database encryption that complies with FIPS 140-2. When the environment variable $gtmcrypt_FIPS is set to 1 (or evaluates to a non-zero integer, or any case-independent string or leading substring of "TRUE" or "YES"), the plugin reference implementation attempts to use libgcrypt (from GnuPG) and libcrypto (OpenSSL) in "FIPS mode." Note that to comply with FIPS 140-2 you should be knowledgeable with that standard and take many steps beyond setting this environment variable. By default YottaDB does not enforce "FIPS mode."

**gtmgbldir** specifies the initial value of the $ZGBLDIR ISV. $ZGBLDIR identifies the global directory. A global directory maps global variables to physical database files, and is required to access M global variables. Users who maintain multiple global directories use this environment variable to conveniently choose one to use from the time of process startup. To automate this definition, define gtmgbldir in the user's login file. The SET command can alter the value of $ZGBLDIR in an active process.

**gtmroutines** specifies the initial value of the $ZROutines ISV, which specifies where to find object and source code. The SET command can alter the value of $ZROUTINES in an active process.

**gtmtls_passwd_<label>** specifies the obfuscated password of the encrypted private key pair. You can obfuscate passwords using the 'maskpass' utility provided along with the encryption plugin. If you choose to use unencrypted private keys, set the gtmtls_passwd_<label> environment variable to a non-null dummy value; this prevents inappropriate prompting for a password.

**gtmver** (not used by YottaDB directly) - The current YottaDB version. The gtmprofile script uses $gtmver to set other environment variables.

**gtm_aio_nr_events**: For Linux x86_64, the gtm_aio_nr_events environment variable controls the number of structures a process has per global directory to manage asynchronous writes, and therefore determines the number of concurrent writes a process can manage across all regions within a global directory. If not specified, the value controlled by gtm_aio_nr_events defaults to 128. If a process encounters a situation where it needs to perform an asynchronous write, but has no available slots with which to manage an additional one, it either falls back to synchronous writing if the write is blocking other actions, and otherwise defers the write until a slot becomes available as other writes complete. Linux allocates the structures on a system-wide basis with the setting of /proc/sys/fs/aio-max-nr. Therefore, you should configure this parameter to account for the needs (as determined by gtm_aio_nr_events or the default) of all processes using asynchronous I/O. When processes use multiple global directories with asynchronous I/O, their need for system resources increases accordingly. For example, if an environment runs 10,000 processes, each of which open two global directories and /proc/sys/fs/aio-max-nr is set to a value of 200,000 then gtm_aio_nr_events needs to be set to a value <= 200,000 / (10,000 * 2) = 10. Conversely if gtm_aio_nr_events is set to a value of 20, then aio-max-nr needs to be bumped up to (10,000 * 2 * 20) = 400,000. YottaDB captures the number of errors encountered when attempting to write database blocks for a region, and, barring problems with the storage subsystem, hitting an asynchronous write limit would constitute primary (probably only) contribution to that value, which you can access with $$^%PEEKBYNAME("sgmnt_data.wcs_wterror_invoked_cntr",<region>)

**gtm_autorelink_ctlmax** specifies the maximum number of entries for unique routine names in the relink control file created by a process for any directory, with a minimum of 1,000, a maximum of 16,000,000 and a default of 50,000 if unspecified. If a specified value is above or below the allowed range, the process logs the errors ARCTLMAXHIGH or ARCTLMAXLOW respectively in the syslog, and uses the nearest acceptable limit instead. MUPIP RCTLDUMP and ZSHOW "A" outputs include the maximum number of unique routine names available in a relink control file.

**gtm_autorelink_keeprtn**: When gtm_autorelink_keeprtn is set to 1, t[rue], or y[es] , exiting processes leave auto-relinked routines in shared memory. When the environment variable gtm_autorelink_keeprtn is undefined, 0, f[alse] or n[o], exiting processes purge auto-relinked routines in shared memory if no other processes are using them. Regardless of the value of gtm_autorelink_keeprtn, the Operating System removes an auto-relink shared memory repository when there are no processes accessing it. 

All values are case-independent. When gtm_autorelink_keeprtn is defined and TRUE:

* Processes do less work on exiting, with some performance gain - faster process termination - likely only observable when a large number of processes exit concurrently.

* In a production environment, an application that frequently invokes YottaDB routines in short running processes (such as YottaDB routines invoked by web servers using interfaces such as CGI) may give better performance when setting gtm_autorelink_keeprtn or using at least one long running auto-relink process that remains attached to the shared memory to keep routines available in shared memory for use when short running processes need them.

**gtm_autorelink_shm** specifies the size (in MiB) of an initial Rtnobj shared memory segment used by the auto-relink facility. If the value of gtm_autorelink_shm is not a power of two, YottaDB rounds the value to the next higher integer power of two. If the first object (.o) file does not fit in a new Rtnobj segment, YottaDB rounds the allocation up to the smallest integer power of two required to make it fit. When YottaDB needs room for object files, and existing Rtnobj segments have insufficient free space, it creates an additional shared memory segment, double the size of the last. Note that when hugepages are enabled, the actual Rtnobj shared memory size might be more than that requested implicitly or explicitly through $gtm_autorelink_shm.

**gtm_badchar** specifies the initial setting that determines whether YottaDB should raise an error when it encounters an illegal UTF-8 character sequence. This setting can be changed with a VIEW "[NO]BADCHAR" command, and is ignored for I/O processing and in M mode.

**gtm_baktmpdir** specifies the directory where mupip backup creates temporary files. If $gtm_baktmpdir is not defined, YottaDB currently uses the deprecated $GTM_BAKTMPDIR environment variable if defined, and otherwise uses /tmp. All processes performing updates during an online IBACKUP must have the use the same directory and have write access to it.

**gtm_boolean** specifies the initial setting that determines how YottaDB compiles Boolean expression evaluation (expressions evaluated as a logical TRUE or FALSE). If gtm_boolean is undefined or evaluates to an integer zero (0), YottaDB behaves as it would after a VIEW "NOFULL_BOOLEAN" and compiles such that it stops evaluating a Boolean expression as soon as it establishes a definitive result . Note that:

*  $gtm_side_effects has an analogous impact on function argument evaluation order and implies "FULLBOOLEAN" compilation, so VIEW "NOFULLBOOLEAN" produces an error when $gtm_side_effects is on.

* If gtm_boolean evaluates to an integer one (1), YottaDB enables VIEW "FULL_BOOLEAN" compilation, which means that YottaDB ensures that within a Boolean expression, all side effect expression atoms, extrinsic functions ($$), external functions ($&), and $INCREMENT() execute in left-to-right order.

* If gtm_boolean evaluates to an integer two (2), YottaDB enables VIEW "FULL_BOOLWARN" behavior, which means that YottaDB not only evaluates Boolean expressions like "FULL_BOOLEAN" but produces a BOOLSIDEFFECT warning when it encounters Boolean expressions that may induce side-effects; that is: expressions with side effects after the first Boolean operator - extrinsic functions, external calls, and $INCREMENT().

**gtm_chset** determines the mode in which YottaDB compiles and operates. If it has a value of "UTF-8", YottaDB assumes that strings are encoded in UTF-8. In response to a value of "M" (or indeed anything other than "UTF-8"), YottaDB treats all 256 combinations of the 8 bits in a byte as a single character.

**gtm_chset_locale** (z/OS only) specifies the locale for UTF-8 operations on z/OS.

**gtm_collate_n** specifies the shared library holding an alternative sequencing routine when using non-M standard (ASCII) collation. The syntax is gtm_collate_n=pathname where n is an integer from 1 to 255 that identifies the collation sequence, and pathname identifies the shared library containing the routines for that collation sequence.

**gtm_crypt_plugin**: If the environment variable gtm_crypt_plugin is defined and provides the path to a shared library relative to $gtm_dist/plugin, YottaDB uses $gtm_dist/plugin/$gtm_crypt_plugin as the shared library providing the plugin. If $gtm_crypt_plugin is not defined, YottaDB expects $gtm_dist/plugin/libgtmcrypt.so to be a symbolic link to a shared library providing the plugin. The expected name of the actual shared library is libgtmcrypt_cryptlib_CIPHER.so (depending on your platform, the actual extension may differ from .so), for example, libgtmcrypt_openssl_AESCFB. YottaDB cannot and does not ensure that the cipher is actually AES CFB as implemented by OpenSSL.

**gtm_custom_errors** specifies the complete path to the file that contains a list of errors that should automatically stop all updates on those region(s) of an instance which have the Instance Freeze mechanism enabled.

**gtm_db_startup_max_wait** specifies how long to wait for a resolution of any resource conflict when they first access a database file. YottaDB uses semaphores maintained using UNIX Inter-Process Communication (IPC) services to ensure orderly initialization and shutdown of database files and associated shared memory. Normally, the IPC resources are held in an exclusive state only for very brief intervals. However, under unusual circumstances that might include extremely large numbers of simultaneous database initializations, a long-running MUPIP operation involving standalone access (like INTEG -FILE or RESTORE), an OS overload or an unpredicted process failure, the resources might remain unavailable for an unanticipated length of time. $gtm_db_startup_max_wait specifies how long to wait for the resources to become available:

* -1 - Indefinite wait until the resource becomes available; the waiting process uses the gtm_procstuckexec mechanism at approximately 48 and 96 seconds.

* 0 - No wait - if the resource is not immediately available, give a DBFILERR error with an associated SEMWT2LONG

* > 0 - Seconds to wait - rounded to the nearest multiple of eight (8); if the specification is 96 or more seconds, the waiting process uses the gtm_procstuckexec mechanism at one half the wait and at the end of the wait; if the resource remains unavailable, the process issues DBFILERR error with an associated SEMWT2LONG

**gtm_dist** specifies the path to the directory containing the YottaDB system distribution. gtm_dist must be defined for each user. If you are not using the gtm script or sourcing gtmprofile, consider defining gtm_dist in the login file or as part of the default system environment. In UTF-8 mode, the gtm_dist environment variable specifies the path to the directory containing the YottaDB system distribution for Unicode. The distribution for Unicode is located in subdirectory utf8 under the YottaDB distribution directory. For example, if the YottaDB distribution is in /usr/local/lib/yottadb/r110, set gtm_dist to point to /usr/local/lib/yottadb/r110/utf8 for UTF-8 mode. Correct operation of YottaDB executable programs requires gtm_dist to be set correctly.

**gtm_dmterm** specifies a [NO]DMTERM state at process initiation where application setting applied to $PRINCIPAL also apply to direct mode interactions; a case-insensitive value of "1", "yes", or "true" establishes a DMTERM state at process initiation where direct mode uses default terminal characteristics and ignores application settings for $PRINCIPAL; all other values, including no value, result in the default VIEW "NODMTERM" behavior.

**gtm_env_translate** specifies the path to a shared library to implement the optional YottaDB environment translation facility that can assist in resolving extended global references.

**gtm_etrap** specifies an initial value of $ETRAP to override the default value of "B" for $ZTRAP as the base level error handler. The gtmprofile script sets gtm_etrap to "Write:(0=$STACK) ""Error occurred: "",$ZStatus,!" which you can customize to suit your needs.

**gtm_extract_nocol** specifies whether a MUPIP JOURNAL -EXTRACT (when used without -RECOVER or -ROLLBACK) on a database with custom collation should use the default collation if it is not able to read the database file. In a situation where the database file is inaccessible or the replication instance is frozen with a critical section required for the access held by another process and the environment variable gtm_extract_nocol is defined and evaluates to a non-zero integer or any case-independent string or leading substrings of "TRUE" or "YES", MUPIP JOURNAL -EXTRACT issues the DBCOLLREQ warning and proceeds with the extract using the default collation. If gtm_extract_nocol is not set or evaluates to a value other than a positive integer or any case-independent string or leading substrings of "FALSE" or "NO", MUPIP JOURNAL -EXTRACT exits with the SETEXTRENV error.

.. note::
    If default collation is used for a database with custom collation, the subscripts reported by MUPIP JOURNAL -EXTRACT are those stored in the database, which may differ from those used by application logic.

**gtm_fullblockwrites** specifies whether a YottaDB process should write a full filesystem, or full database block, worth of bytes when writing a database block that is not full. Depending on your IO subsystem, writing a full block worth of bytes (even when there are unused garbage bytes at the end) may result in better database IO performance by replacing a low level read-modify-read IO operation with a single write operation.

**gtm_gdscert** specifies the initial setting that controls whether YottaDB  processes should test updated database blocks for structural damage. If it is defined, and evaluates to a non-zero integer or any case-independent string or leading substrings of "TRUE" or "YES", YottaDB performs a block-level integrity check on every block as a process commits it. Within a running process, VIEW "GDSCERT":value controls this setting By default, YottaDB does not check database blocks for structural damage, because the impact on performance is usually unwarranted.

**gtm_gvdupsetnoop** specifies the initial value that determines whether a YottaDB process should enable duplicate SET optimization. If it is defined, and evaluates to a non-zero integer or any case-independent string or leading substrings of "TRUE" or "YES", when a SET command does not change the value of an existing node, YottaDB does not perform the update or execute any trigger code specified for the node. Within a running process, VIEW "GVDUPSETNOOP":value controls this behavior. By default, YottaDB enables this optimization.

**gtm_icu_version** specifies the MAJOR VERSION and MINOR VERSION numbers of the desired ICU. For example "3.6" denotes ICU-3.6. If $gtm_chset has the value "UTF-8", YottaDB requires libicu with version 3.6 or higher. If you must chose between multiple versions of libicu or if libicu has been compiled with symbol renaming enabled, YottaDB requires gtm_icu_version to be explicitly set. Please see the section on "Configuring and operation YottaDB with Unicode Support" for more information.

**gtm_ipv4_only** specifies whether a Source Server should establish only IPv4 connections with a Receiver Server or sockets associated with a SOCKET device. If it is defined, and evaluates to a non-zero integer, or any case-independent string or leading substring of "TRUE" or "YES", the Source Server establishes only IPv4 connections with the Receiver Server. gtm_ipv4_only is useful for environments where different server names are not used for IPv4 and IPv6 addresses and the Source Server connects to a Receiver Server running a YottaDB version prior to V6.0-003.

**gtm_jnl_release_timeout** specifies the number of seconds that a replicating Source Server waits when there is no activity on an open journal file before closing it. The default wait period is 300 seconds (5 minutes). If $gtm_jnl_release_timeout specifies 0, the Source Server keeps the current journal files open until shutdown. The maximum value for $gtm_jnl_release_timeout is 2147483 seconds.

**gtm_keep_obj** specifies whether the gtminstall script should delete the object files from the YottaDB installation directory. If gtm_keep_obj is set to "Y", the gtminstall script leaves object files; by default, gtminstall deletes object files after archiving them in a shared library.

**gtm_lct_stdnull** specifies whether a YottaDB process should use standard collation for local variables with null subscripts or legacy YottaDB collation.

**gtm_link** specifies the initial setting that determines whether YottaDB permits multiple versions of the same routine to be active at different stack levels of the M virtual machine. The VIEW "LINK":"[NO]RECURSIVE" command modifies this in an active process. If gtm_link is set to "RECURSIVE", auto-relink and explicit ZLINK commands links a newer object even when a routine with the same name is active and available in the current stack. When a process links a routine with the same name as an existing routine, future calls use the new routine. Prior versions of that routine referenced by the stack remain tied to the stack until they QUIT, at which point they become inaccessible. This provides a mechanism to patch long-running processes. If gtm_link is undefined or set to NORECURSIVE, or any value other than "RECURSIVE", auto-zlink defers replacing older routines until they no longer have an invoking use by the process and a ZLINK command produces a LOADRUNNING error when it attempts to relink an active routine on the YottaDB invocation stack.

**gtm_local_collate** specifies an alternative collation sequence for local variables.

**gtm_log** specifies a directory where the gtm_secshr_log file is stored. The gtm_secshr_log file stores information gathered in the gtmsecshr process. YottaDB recommends that a system-wide default be established for gtm_log so that gtmsecshr always logs its information in the same directory, regardless of which user's YottaDB process invokes gtmsecshr. In conformance with the Filesystem Hierarchy Standard, YottaDB recommends /var/log/yottadb/$gtmver as the value for $gtm_log unless you are installing the same version of YottaDB in multiple directories. Note that $gtmver can be in the form of the current YottaDB release and platform. If you do not set $gtm_log, YottaDB creates log files in a directory in /tmp (AIX, GNU/Linux). However, this is not recommended because it makes YottaDB log files vulnerable to the retention policy of a temporary directory.

.. note::
   In the latest versions, gtmsecshr logs its messages in the system log and the environment variable gtm_log is ignored.

**gtm_max_sockets** specifies the maximum number of client connections for socket devices. The default is 64. While it must be large enough to accommodate the actual need, each reservation requires some memory in socket structures, so setting this number unnecessarily high causes requires a bit of additional memory for no benefit.

**gtm_memory_reserve** specifies the size in kilobytes of the reserve memory that YottaDB should use in handling and reporting an out-of-memory condition. The default is 64 (KiB). Setting this too low can impede investigations of memory issues, but YottaDB only uses this reserve when a process runs out of memory so it almost never requires actual memory, only address space.

**gtm_mupjnl_parallel** defines the number of processes or threads used by MUPIP JOURNAL -RECOVER/-ROLLBACK when the invoking command does not have a -PARALLEL qualifier. When defined with no value, it specifies one process or thread per region. When undefined or defined to one (1), it specifies MUPIP should process all regions without using additional processes or threads. When defined with an integer value greater than one (1), it specifies the maximum number of processes or threads for MUPIP to use. If the value is greater than the number of regions, MUPIP never uses more processes or threads than there are regions. If it is less than the number of regions, MUPIP allocates work to the additional processes or threads based on the time stamps in the journal files.

**gtm_nocenable** specifies whether the $principal terminal device should ignore <CTRL-C> or use <CTRL-C> as a signal to place the process into direct mode; a USE command can modify this device characteristic. If gtm_nocenable is defined and evaluates to a non-zero integer or any case-independent string or leading substrings of "TRUE" or "YES", $principal ignores <CTRL-C>. If gtm_nocenable is not set or evaluates to a value other than a positive integer or any case-independent string or leading substrings of "FALSE" or "NO", <CTRL-C> on $principal places the process into direct mode at the next opportunity (usually at a point corresponding to the beginning of the next source line).

**gtm_non_blocked_write_retries** modifies FIFO or PIPE write behavior. A WRITE which would block is retried up to the number specified with a 100 milliseconds delay between each retry. The default value is 10 times.

**gtm_nontprestart_log_delta** specifies the number of non-transaction restarts for which YottaDB should wait before reporting a non-transaction restart to the operator logging facility. If gtm_nontprestart_log_delta is not defined, YorttaDB initializes gtm_nontprestart_log_delta to 0.

**gtm_nontprestart_log_first** specifies the initial number of non-transaction restarts which YottaDB should report before placing non-transaction restart reports to the operator logging facility using the gtm_nontprestart_log_delta value. If gtm_nontprestart_log_delta is defined and gtm_nontprestart_log_first is not defined, YottaDB initializes gtm_nontprestart_log_first to 1.

**gtm_noundef** specifies the initial setting that controls whether a YottaDB process should treat undefined global or local variables as having an implicit value of an empty string. If it is defined, and evaluates to a non-zero integer or any case-independent string or leading substring of "TRUE" or "YES", then YottaDB treats undefined variables as having an implicit value of an empty string. The VIEW "[NO]UNDEF" command can alter this behavior in an active process. By default, YottaDB signals an error on an attempt to use the value of an undefined variable.

**gtm_obfuscation_key**: If $gtm_obfuscation_key specifies the name of file readable by the process, the encryption reference plug-in uses an SHA-512 hash of the file's contents as the XOR mask for the obfuscated password in the environment variable gtm_passwd. When gtm_obfuscation_key does not point to a readable file, the plug-in creates an XOR mask based on the userid and inode of the mumps executable and then computes an SHA-512 hash of the XOR mask to use as a mask.

gtm_obfuscation_key can be used as a mechanism to pass an obfuscated password between unrelated processes (for example, a child process with a different userid invoked via a sudo mechanism), or even from one system to another (for example, over an ssh connection).

**gtm_passwd** used by the encryption reference plugin (not used by YottaDB directly) for the obfuscated (not encrypted) password to the GNU Privacy Guard key ring. If the environment variable gtm_patnumeric is not defined or set to a value other than "UTF-8", YottaDB initializes $ZPATNUMERIC to "M".

**gtm_patnumeric** specifies the value of the read-only ISV $ZPATNUMERIC that determines how YottaDB interprets the patcode "N" used in the pattern match operator. The SET command can alter the value of $ZPATNUMERIC in an active process.

**gtm_pattern_file** and **gtm_pattern_table** specify alternative patterns for the pattern (?) syntax. Refer to the Internationalization chapter in the Programmer's Guide for additional information.

**gtm_poollimit** restricts the number of global buffers a process uses in order to limit the potential impact on other processes. It is intended for use by a relatively small subset of processes when those processes have the potential to "churn" global buffers; the value is of the form n[%] when it ends with a per-cent sign (%), the number is taken as an as a percentage of the configured global buffers and otherwise as an ordinal number of preferred buffers; standard M parsing and integer conversions apply. Note that this environment variable applies to all regions accessed by a process; the VIEW command for this feature allows finer grained control. MUPIP REORG uses this facility to limit its buffers with a default of 64 if gtm_poollimit is not specified. Note that this may slightly slow a standalone REORG but can be overridden by defining gtm_poollimit as 0 or "100%".

**gtm_principal** specifies the value for $PRINCIPAL, which designates an alternative name (synonym) for the principal $IO device.

**gtm_principal_editing** specifies the initial settings for $PRINCIPAL of the following colon-delimited deviceparameters: [NO]EDITING [NO]EMPTERM and [NO]INSERT; in an active process the USE command can modify these device characteristics. gtm_max_sockets specifies the maximum number of client connections for socket devices. The default is 64. While it must be large enough to accommodate the need, each reservation requires some memory in the socket structures, so setting this number unnecessarily high requires additional memory for no benefit.

.. note::
   The YottaDB direct mode commands have a more extensive capability in this regard, independent of the value of this environment variable.

**gtm_procstuckexec** specifies a shell command or a script to execute when any of the following conditions occur:

* A one minute wait on a region due to an explicit MUPIP FREEZE or an implicit freeze, such as BACKUP, INTEG -ONLINE, and so on.

* MUPIP actions find kill_in_prog (KILLs in progress) to be non-zero after a one minute wait on a region. Note that YottaDB internally maintains a list of PIDs (up to a maximum of 8 PIDs) currently doing a KILL operation.

* A process encounters conditions that produce the following operator log messages: BUFOWNERSTUCK, INTERLOCK_FAIL, JNLPROCSTUCK, SHUTDOWN, WRITERSTUCK, MAXJNLQIOLOCKWAIT, MUTEXLCKALERT, SEMWT2LONG, and COMMITWAITPID.

You can use this as a monitoring facility for processes holding a resource for an unexpected amount of time. Typically, for the shell script or command pointed to by gtm_procstuckexec, you would write corrective actions or obtain the stack trace of the troublesome processes (using their PIDs). YottaDB passes arguments to the shell command / script in the order specified as follows:

* *condition* is the name of the condition. For example, BUFOWNERSTUCK, INTERLOCK_FAIL, and so on.

* *waiting_pid* is the PID of the process reporting the condition.

* *blocking_pid* is the PID of the process holding a resource.

* *count* is the number of times the script has been invoked for the current condition (1 for the first occurrence).

Each invocation generates an operator log message and if the invocation fails, an error message to the operator log. The shell script should start with a line beginning with #! that designates the shell.

.. note::
   Make sure that user processes have sufficient space and permissions to run the shell command / script. For example - for the script to invoke the debugger, the process must be of the same group or have a way to elevate privileges.

**gtm_prompt** specifies the initial value of the ISV $ZPROMPT, which controls the YottaDB direct mode prompt. The SET command can alter the value of $ZPROMPT in an active process. By default, the direct mode prompt is "YDB>".

**gtm_quiet_halt** specifies whether YottaDB should disable the FORCEDHALT message when the process is stopped via MUPIP STOP or by a SIGTERM signal (as sent by some web servers).

**gtm_repl_instance** specifies the location of the replication instance file when database replication is in use.

**gtm_repl_instname** specifies a replication instance name that uniquely identifies an instance. The replication instance name is immutable. The maximum length of a replication instance name is 15 bytes. Note that the instance name is not the same as the name of the replication instance file (gtm_repl_instance). You need to specify a replication instance name at the time of creating an replication instance file. If you do not define gtm_repl_instname, you need to specify an instance name using -NAME=<instance_name> with MUPIP REPLICATE -INSTANCE_CREATE.

**gtm_repl_instsecondary** specifies the name of the replicating instance in the current environment. YottaDB uses $gtm_repl_instsecondary if the -instsecondary qualifer is not specified.

**gtm_retention** (not used by YottaDB directly) - used by the gtm script to delete old journal files and old temporary files it creates.

**gtm_side_effects**: When the environment variable gtm_side_effects is set to one (1) at process startup, YottaDB generates code that performs left to right evaluation of actual list arguments, function arguments, operands for non-Boolean binary operators, SET arguments where the target destination is an indirect subscripted glvn, and variable subscripts. When the environment variable is not set, or set to zero (0), YottaDB retains its traditional behavior, which re-orders the evaluation of operands using rules intended to improve computational efficiency. This reordering assumes that functions have no side effects, and may generate unexpected behavior (x+$increment(x) is a pathological example). When gtm_side_effects is set to two (2), YottaDB generates code with the left-to-right behavior, and also generates SIDEEFFECTEVAL warning messages for each construct that potentially generates different results depending on the order of evaluation. As extrinsic functions and external calls are opaque to the compiler at the point of their invocation, it cannot statically determine whether there is a real interaction. Therefore SIDEEFFECTEVAL warnings may be much more frequent than actual side effect interactions and the warning mode may be most useful as a diagnostic tool to investigate problematic or unexpected behavior in targeted code rather than for an audit of an entire application. Note that a string of concatenations in the same expression may generate more warnings than the code warrants. Other values of the environment variable are reserved for potential future use by YottaDB. It is important to note that gtm_side_effects affects the generated code, and must be in effect when code is compiled - the value when that compiled code is executed is irrelevant. Note also that XECUTE and auto-ZLINK, explicit ZLINK and ZCOMPILE all perform run-time compilation subject to the characteristic selected when the process started. Please be aware that the programming style where one term of an expression changes a prior term in the same expression is an unsafe programming practice. The environment variable gtm_boolean may separately control short-circuit evaluation of Boolean expressions but a setting of 1 (or 2) for gtm_side_effects causes the same boolean evaluations as setting gtm_boolean to 1 (or 2). Note that warning reports for the two features are separately controlled by setting their values to 2. The differences in the compilation modes may include not only differences in results, but differences in flow of control when the code relies on side effect behavior.

**gtm_snaptmpdir** specifies the location to place the temporary "snapshot" file created by facilities such as on-line mupip integ. If $gtm_snaptmpdir is not defined, YottaDB uses the $gtm_baktmpdir environment variable if defined, and otherwise uses the current working directory. All processes performing updates during an online INTEG must have the use the same directory and have write access to it.

**gtm_statsdir** specifies the directory for database files into which processes opted-in to sharing global statistics place their statistics as binary data. If you do not explicitly define this environment variable for a process, YottaDB defines this to the evaluation of $gtm_tmp, which defaults to /tmp. All processes that share statistics MUST use the same value for $gtm_statsdir. YottaDB suggests that you point gtm_statsdir at a tmpfs or ramfs on Linux, and a filesystem in a ram disk on AIX. These database files have a name derived from the user defined database file name and a .gst extension. They are not usable as normal database files by application code, except to read statistics. YottaDB automatically creates and deletes these database files as needed. Under normal operation, applications do not need to manage them explicitly. The mapping of ^%YGS to statistics database files is managed by YottaDB within global directories, transparently to applications. The ^%YGBLSTAT utility program gathers and reports statistics from nodes of ^%YGS(region,pid).

**gtm_statshare** specifies an initial value for the characteristic controlled by VIEW "[NO]STATSHARE" in application code. A value of 1, or any case-independent string or leading substrings of "TRUE" or "YES" in the environment variable gtm_statshare provides the equivalent of VIEW "STATSHARE" as the initial value. Leaving the gtm_statshare undefined or defined to another value, typically 0, "FALSE" or "NO" provides the equivalent of VIEW "NOSTATSHARE" as the initial value.

**gtm_stdxkill** enables the standard-compliant behavior to kill local variables in the exclusion list if they had an alias that as not in the exclusion list. By default, this behavior is disabled.

**gtm_sysid** specifies the value for the second piece of the $SYSTEM ISV. $SYSTEM contains a string that identifies the executing M instance. The value of $SYSTEM is a string that starts with a unique numeric code that identifies the manufacturer. Codes were originally assigned by the MDC (MUMPS Development Committee). $SYSTEM in YottaDB starts with "47" followed by a comma and $gtm_sysid.

**gtm_tmp** specifies a directory where socket files used for communication between gtmsecshr and YottaDB processes are stored. All processes using the same YottaDB should have the same $gtm_tmp.

**gtm_tpnotacidtime** specifies the maximum time that a YottaDB process waits for a non-isolated timed command (HANG, JOB, LOCK, OPEN, READ, WRITE /* or ZALLOCATE) running within a transaction to complete before it releases all critical sections it owns and sends a TPNOTACID information message to the system log. A YottaDB process owns critical sections on all or some of the regions participating in a transactions only during final retry attempts (when $TRETRY>2). gtm_tpnotacidtime specifies time in seconds to millisecond precision (three decimal places); the default is 2 seconds. The maximum value of gtm_tpnotacidtime is 30 and the minimum is 0. If gtm_tpnotacidtime specifies a time outside of this range, YottaDB uses the default value. YottaDB releases critical sections in a final retry attempt to provide protection from certain risky coding patterns which, because they are not isolated, can cause deadlocks (in the worst case) and long hangs (in the best case). As ZSYSTEM and BREAK are neither isolated nor timed, YottaDB initiates TPNOTACID behavior for them immediately as it encounters them during execution in a final retry attempt (independent of gtm_tpnotacidtime). Rapidly repeating TPNOTACID messages are likely associated with live-lock, which means that a process is consuming critical resources repeatedly within a transaction, and is unable to commit because the transaction duration is too long to commit while maintaining ACID transaction properties.

**gtm_tprestart_log_delta** specifies the number of transaction restarts for which YottaDB should wait before reporting a transaction restart to the operator logging facility. If gtm_tprestart_log_delta is not defined, YottaDB initializes gtm_tp_restart_log_delta to 0.

**gtm_tprestart_log_first** specifies the initial number of transaction restarts which YottaDB should report before pacing transaction restart reports to the operator logging facility using the gtm_tprestart_log_delta value. If gtm_tprestart_log_delta is defined and gtm_tprestart_log_first is not defined, YottaDB initializes gtm_tprestart_log_first to 1.

**gtm_trace_gbl_name** enables YottaDB tracing at process startup. Setting gtm_trace_gbl_name to a valid global variable name instructs YottaDB to report the data in the specified global when a VIEW command disables the tracing, or implicitly at process termination. This setting behaves as if the process issued a VIEW "TRACE" command at process startup. However, gtm_trace_gbl_name has a capability not available with the VIEW command, such that if the environment variable is defined but evaluates to zero (0) or to the empty string, YottaDB collects the M-profiling data in memory and discards it when the process terminates (this feature is mainly used for in-house testing). Note that having this feature activated for processes that otherwise don't open a database file (such as GDE) can cause them to encounter an error.

**gtm_trigger_etrap** provides the initial value for $ETRAP in trigger context; can be used to set trigger error traps for trigger operations in both mumps and MUPIP processes.

**gtm_zdate_form** specifies the initial value for the $ZDATE ISV. The SET command can alter the value of $ZDATE in an active process.

**gtm_zinterrupt** specifies the initial value of the ISV $ZINTERRUPT which holds the code that YottaDB executes (as if it is the argument for an XECUTE command) when a process receives a signal from a MUPIP INTRPT command. The SET command can alter the value of $ZINTERRUPT in an active process.

**gtm_zlib_cmp_level** specifies the zlib compression level used in the replication stream by the source and receiver servers. By default, replication does not use compression.

**gtm_zmaxtptime** specifies the initial value of the $ZMAXTPTIME Intrinsic Special Variable, which controls whether and when YottaDB issues a TPTIMEOUT error for a TP transaction that runs too long. gtm_zmaxtptime specifies time in seconds and the default is 0, which indicates "no timeout" (unlimited time). The maximum value of gtm_zmaxtptime is 60 seconds and the minimum is 0; YottaDB ignores gtm_zmaxtptime if it contains a value outside of this recognized range. This range check does not apply to SET $ZMAXTPTIME.

**gtm_zquit_anyway** specifies whether the code of the form QUIT <expr> execute as if it were SET <tmp>=<expr> QUIT:$QUIT tmp QUIT, where <tmp> is a temporary local variable in the YottaDB runtime system that is not visible to application code. This setting is a run-time setting, rather than a compiler-time setting. If gtm_zquit_anyway is defined and evaluates to 1 or any case-independent string or leading substrings of "TRUE" or "YES", code of the form QUIT <expr> executes as if it were SET <tmp>=<expr> QUIT:$QUIT tmp QUIT. If gtm_zquit_anyway is not defined or evaluates to 0 or any case-independent string or leading substrings of "FALSE" or "NO", YottaDB executes QUIT <expr> as specified by the standard.

**gtm_zstep** specifies the initial value of $ZSTEP, which defines the ZSTEP action; if gtm_zstep is not defined, $ZSTEP defaults to "B".

**gtm_ztrap_form** and **gtm_zyerror** specify the behavior of error handling specified by $ZTRAP as described in the Error Processing chapter of the Programmer's Guide.

**gtm_ztrap_new** specifies whether a SET $ZTRAP also implicitly performs a NEW $ZTRAP before the SET.

**old_gtm_dist** (not used by YottaDB directly) - The path of the prior YottaDB distribution. The gtmprofile script uses this value to set other environment variables.

**old_gtmroutines** (not used by YottaDB directly) - The prior routine search path. The gtmprofile script uses this value to set other environment variables.

**old_gtmver** (not used by YottaDB directly) - The value of gtmver that was set when the gtmprofile script was last sourced. The gtmprofile script uses this value to set other environment variables.

**tmp_gtm_tmp** (not used by YottaDB directly) - It is used by the gtmprofile script in maintaining gtm_tmp.

**tmp_passw** (not used by YottaDB directly) - It is used by the gtmprofile script in maintaining gtm_passwd.

The gtmprofile and gtmschrc scripts sets the following environment variables. YottaDB recommends using the gtmprofile script (or the gtm script which sources gtmprofile) to set up an environment for YottaDB.

+------------------------------------------------+--------------------------------------------------------+
| Environment Variables                          | Set up by YottaDB shell scripts                        |
+================================================+========================================================+
| LC_CTYPE                                       | gtmprofile                                             |
+------------------------------------------------+--------------------------------------------------------+
| gtmgbldir*                                     | gtmprofile, gtmcshrc                                   |
+------------------------------------------------+--------------------------------------------------------+
| gtmroutines*                                   | gtmprofile, gtmcshrc                                   |
+------------------------------------------------+--------------------------------------------------------+
| gtmver                                         | gtmprofile                                             |
+------------------------------------------------+--------------------------------------------------------+
| gtm_dist*                                      | gtmprofile, gtmschrc                                   |
+------------------------------------------------+--------------------------------------------------------+
| gtm_icu_version                                | gtmprofile                                             |
+------------------------------------------------+--------------------------------------------------------+
| gtm_log*                                       | gtmprofile                                             |
+------------------------------------------------+--------------------------------------------------------+
| gtm_principal_editing                          | gtmprofile                                             |
+------------------------------------------------+--------------------------------------------------------+
| gtm_prompt                                     | gtmprofile                                             |
+------------------------------------------------+--------------------------------------------------------+
| gtm_repl_instance                              | gtmprofile                                             |
+------------------------------------------------+--------------------------------------------------------+
| gtm_retention                                  | gtmprofile                                             |
+------------------------------------------------+--------------------------------------------------------+
| gtm_tmp                                        | gtmprofile                                             |
+------------------------------------------------+--------------------------------------------------------+
| old_gtmroutines                                | gtmprofile                                             |
+------------------------------------------------+--------------------------------------------------------+
| old_gtm_dist                                   | gtmprofile                                             |
+------------------------------------------------+--------------------------------------------------------+
| old_gtmver                                     | gtmprofile                                             |
+------------------------------------------------+--------------------------------------------------------+
| tmp_gtm_tmp                                    | gtmprofile                                             |
+------------------------------------------------+--------------------------------------------------------+
| tmp_passw                                      | gtmprofile                                             |
+------------------------------------------------+--------------------------------------------------------+

\* denotes environment variables that must be defined for normal YottaDB operation.

While creating an environment for multiple processes accessing the same version of YottaDB, bear in mind the following important points:

* A YottaDB version has an associated gtmsecshr (located by $gtm_dist). If multiple processes are accessing the same YottaDB version, each process must use the same combination of $gtm_tmp and $gtm_log.

* In conformance with the Filesystem Hierarchy Standard, YottaDB recommends /var/log/yottadb/$gtmver as the value for $gtm_log. Note that $gtmver can be in the form of the current YottaDB release and platform information.

* YottaDB recommends setting $gtm_tmp to a temporary directory /tmp (AIX, GNU/Linux). The gtmprofile script sets $gtm_tmp to /tmp/yottadb/$gtmver.

* If you do not set $gtm_log, YottaDB creates log files in a directory in /tmp (AIX, GNU/Linux). However, this is not recommended because it makes YottaDB log files vulnerable to the retention policy of a temporary directory. 

Always set the same value of $gtm_tmp for all processes using the same YottaDB version. Having different $gtm_tmp for multiple processes accessing the same YottaDB version may prevent processes from being able to communicate with gtmsecshr and cause performance issues.

-------------------------------------------------------------------
 Configuring and operating YottaDB with Unicode™ support (optional) 
-------------------------------------------------------------------

The configure script provides the option to install YottaDB with or without Unicode™ support for encoding international character sets. This section describes the system environment required to install and operate YottaDB with Unicode™ support. Users who handle data in ASCII or other single-byte character sets such as one of the ISO-8859 representations and do not foresee any use of character sets beyond single byte character sets, may proceed to the next section.

++++++++++++++++++++++++++++++++
M mode and UTF-8 mode 
++++++++++++++++++++++++++++++++

A YottaDB process can operate in either M mode or UTF-8 mode. In certain circumstances, both M mode and UTF-8 mode may concurrently access the same database.

$gtm_chset determines the mode in which a process operates. If it has a value of M, YottaDB treats all 256 combinations of the 8 bits in a byte as a character, which is suitable for many single-language applications.

If $gtm_chset has a value of UTF-8, YottaDB (at process startup) interprets strings as being encoded in UTF-8. In this mode, all functionality related to Unicode™ becomes available and standard string-oriented operations operate with UTF-8 encoding. In this mode, YottaDB detects character boundaries (since the size of a character is variable length), calculates glyph display width, and performs string conversion between UTF-8 and UTF-16.

If you install YottaDB with Unicode™ support, all YottaDB components related to M mode reside in your YottaDB distribution directory and Unicode™-related components reside in the utf8 subdirectory of your YottaDB distribution. For processes in UTF-8 mode, in addition to gtm_chset, ensure that $gtm_dist points to the utf8 subdirectory, that $gtmroutines includes the utf8 subdirectory (or the libgtmutil.so therein) rather than its parent directory.

In addition to $gtm_chset, recent versions use $gtm_icu_version to choose an ICU library library version other than the default. For ICU libraries built with symbol renaming enabled, $gtm_icu_version becomes a required setting.

$gtm_icu_version specifies the ICU version that YottaDB should use for Unicode operations. It is in the form of MajorVersion.MinorVersion where MajorVersion and MinorVersion specify the desired major verison and minor version of ICU. For example, 3.6 refers to ICU version 3.6. If $gtm_icu_version is defined, YottaDB works regardless of whether or not symbols are renamed in ICU. If $gtm_icu_version is not defined or does not evaluate to an installed ICU version, YottaDB look for non-renamed symbols in the default ICU version. Note that display widths for a few characters are different starting in ICU 4.0. 

.. note::
   The gtmprofile script defines $gtm_icu_version as necessary.

+++++++++++++++++
Compiling ICU
+++++++++++++++++

YottaDB uses ICU 3.6 (or above) to perform Unicode™-related operations. YottaDB generates the distribution for Unicode™ only if ICU 3.6 (or above) is installed on the system. Therefore, install an appropriate ICU version before installing YottaDB to perform functionality related to Unicode™.

Note that the ICU installation instructions may not be the same for every platform. If libicu has been compiled with symbol renaming enabled, YottaDB requires $gtm_icu_version be explicitly set. Please see the above section for more information.

After installing ICU 3.6 (or above), you also need to set the following environment variables to an appropriate value.

1. LC_CTYPE
2. LC_ALL
3. LD_LIBRARY_PATH
4. TERM

------------------------------
Starting YottaDB
------------------------------

**To start YottaDB from a POSIX shell**:

Execute ydb from your shell prompt:

.. parsed-literal::
   $ <path_to_ydb_installation_directory>/ydb 

**To start YottaDB in UTF-8 mode from a POSIX shell**:

First, set $gtm_chset to UTF-8 and LC_CTYPE or LC_ALL to any usable UTF-8 locale.

.. parsed-literal::
   $ export gtm_chset="UTF-8"
   $ export LC_CTYPE="en_US.utf8"

Execute the ydb script.

.. parsed-literal::
   $ <path_to_ydb_installation_directory>/ydb

**To start YottaDB from a C-type shell**:

First source the gtmschrc script to set up a default YottaDB environment. At your shell prompt, type:

.. parsed-literal::
   $ source <path_to_ydb_installation_directory>/gtmcshrc

Run the gtm alias to start YottaDB in direct mode.

.. parsed-literal::
   $ ydb 

**To start YottaDB in UTF-8 mode from a C-type shell**:

Set the environment variable gtm_chset to UTF-8 and LC_CTYPE or LC_ALL to any usable UTF-8 locale.

.. parsed-literal::
   $ setenv gtm_chset UTF-8
   $ setenv LC_CTYPE en_US.utf8 

Source the gtmchrc script to set up default YottaDB Unicode environment.

.. parsed-literal::
   $ source <path_to_ydb_installation_directory>/gtmcshrc

Run the ydb alias to start YottaDB in direct mode.

.. parsed-literal::
   $ ydb

**To start YottaDB without using any script**:

* Define gtm_dist, gtm_log, gtm_tmp, gtmgbldir, and gtmroutines. Ensure that gtm_dist points to the location of your YottaDB distribution.

* Add gtm_dist to the system environment variable PATH.

* Ensure that you have set an appropriate value for TERM.

* Consider adding these environment variables in your login file so you do not have to create them again the next time you start your shell.

* Set up the following aliases to run YottaDB and its utilities.

 .. parsed-literal::
    alias dse="$gtm_dist/dse"
    alias gde="$gtm_dist/mumps -run ^GDE" 
    alias gtm="$gtm_dist/mumps -direct" 
    alias lke="$gtm_dist/lke" 
    alias mupip="$gtm_dist/mupip" 

* Run the ydb alias to start YottaDB in direct mode. 

  .. parsed-literal::
     $ ydb


**To start YottaDB in UTF-8 mode without using any script**:

* Define gtm_dist, gtm_log, gtmgbldir, and gtmroutines. Ensure that gtm_dist points to the uft8 subdirectory of your YottaDB distribution.

* Set gtm_routines to include the utf8 subdirectory of your YottaDB distribution. Note that the utf8 subdirectory includes all Unicode-related YottaDB functionality.

* Ensure that you have installed ICU 3.6 (or above) and have LC_CTYPE or LC_ALL set to a usable UTF-8 locale.

* Set LD_LIBRARY_PATH and TERM to appropriate values.

* If you have built ICU with symbol renaming enabled, set gtm_icu_version to an appropriate ICU version.

* Add gtm_dist to the system environment variable PATH.

* Set gtm_chset to UTF-8.

* Consider adding these environment variables in your login file so you do not have to create them again the next time you start your shell.

* Set up the following aliases to run YottaDB and its utilities.

  .. parsed-literal::
     alias dse="$gtm_dist/dse"
     alias gde="$gtm_dist/mumps -run ^GDE"
     alias gtm="$gtm_dist/mumps -direct"
     alias lke="$gtm_dist/lke" 
     alias mupip="$gtm_dist/mupip"

* Type the following command to start YottaDB in direct mode.

  .. parsed-literal::
     $ ydb

* At the YottaDB prompt, type the following command. 

  .. parsed-literal::
    YDB>w $ZCHSET 
    UTF-8 ; the output confirms UTF-8 mode. 

.. note::
    If you are configuring a YottaDB environment without using the gtmprofile script (or the ydb script which sources gtmprofile), bear in mind the following recommendation: All YottaDB processes should use the same settings for gtm_log and gtm_tmp, especially for production environments. This is because gtmsecshr inherits these values from whichever YottaDB process first uses its services. If there are multiple YottaDB versions active on a system, YottaDB recommends different sets of gtm_log and gtm_tmp values for each version as using the same values for different distributions can cause significant performance issues. 
     
YottaDB has three invocation modes: compiler, direct, and auto-start. To invoke YottaDB in these modes, provide the following arguments to the ydb script or the mumps command.

* **-direct**: Invokes YottaDB in direct mode where you can enter M commands interactively.

* **<list of M source files>**: Invokes YottaDB in compiler mode, invoke YottaDB by entering a list of file names to compile as a argument. YottaDB then compiles the specified programs into .o files. UNIX shell globbing to resolve wild-cards (* and ?) in names.

* **-run ^routine_name**: -r invokes YottaDB in auto-start mode. The second argument is taken to be an M entryref, and that routine is automatically executed, bypassing direct mode. Depending on your shell, you may need to put the entryref in quotes.

When executing M programs, YottaDB incrementally links any called programs. For example, the command YDB> do ^TEST links the object file TEST.o and executes it; if the TEST.m program calls other M routines, those are automatically compiled and linked. 

.. note::
   When possible, YottaDB verifies that MUMPS, MUPIP, DSE and LKE reside in $gtm_dist. If the path to the executable and the path to $gtm_dist do not match each executable issues an error. In cases where the executable path could not be determined, each executable defers issuing an error until it is required.

--------------------------------------------------
 Configuring huge pages for YottaDB on Linux
--------------------------------------------------

Huge pages are a Linux feature that may improve the performance of YottaDB applications in production. Huge pages create a single page table entry for a large block (typically 2MiB) of memory in place of hundreds of entries for many smaller (typically 4KiB) blocks. This reduction of memory used for page tables frees up memory for other uses, such as file system caches, and increases the probability of TLB (translation lookaside buffer) matches - both of which can improve performance. The performance improvement related to reducing the page table size becomes evident when many processes share memory as they do for global buffers, journal buffers, and replication journal pools. Configuring huge pages on Linux for x86 or x86_64 CPU architectures help improve:

* YottaDB shared memory performance: When your YottaDB database uses journaling, replication, and the BG access method.

* YottaDB process memory performance: For your process working space and dynamically linked code.

  .. note::
     At this time, huge pages have no effect for MM databases; the text, data, or bss segments for each process; or for process stack.

While YottaDB recommends you configure huge pages for shared memory, you need to evaluate whether or not configuring huge pages for process-private memory is appropriate for your application. Having insufficient huge pages available during certain commands (for example, a JOB command - see complete list below) can result in a process terminating with a SIGBUS error. This is a current limitation of Linux. Before you use huge pages for process-private memory on production systems, YottaDB recommends that you perform appropriate peak load tests on your application and ensure that you have an adequate number of huge pages configured for your peak workloads or that your application is configured to perform robustly when processes terminate with SIGBUS errors. 

The following YottaDB features fork processes and may generate SIGBUS errors when huge pages are not available - JOB, OPEN a PIPE device, ZSYSTEM, interprocess signaling that requires the services of gtmsecshr when gtmsecshr is not already running, SPAWN commands in DSE, GDE, and LKE, argumentless MUPIP RUNDOWN, and replication-related MUPIP commands that start server processes and/or helper processes. As increasing the available huge pages may require a reboot, an interim workaround is to unset the environment variable HUGETLB_MORECORE for YottaDB processes until you are able to reboot or otherwise make available an adequate supply of huge pages.

Consider the following example of a memory map report of a Source Server process running at peak load:

.. parsed-literal::
   $ pmap -d 18839
   18839: /usr/lib/fis-gtm/V6.2-000_x86_64/mupip replicate -source -start -buffsize=1048576 -secondary=melbourne:1235 -log=/var/log/.fis-gtm/mal2mel.log -instsecondary=melbourne
   Address   Kbytes Mode Offset   Device Mapping
   --- lines removed for brevity -----
   mapped: 61604K writeable/private: 3592K shared: 33532K
   $

Process id 18839 uses a large amount of shared memory (33535K) and can benefit from configuring huge pages for shared memory. Configuring huge pages for shared memory does not cause a SIGBUS error when a process does a fork. For information on configuring huge pages for shared memory, refer to the "Using huge pages" and "Using huge pages for shared memory" sections. SIGBUS errors only occur when you configure huge pages for process-private memory; these errors indicate you have not configured your system with an adequate number of huge pages. To prevent SIGBUS errors, you should perform peak load tests on your application to determine the number of required huge pages. For information on configuring huge pages for process-private memory, refer to the "Using huge pages" and "Using huge pages for process working space" sections.

As application response time can be adversely affected if processes and database shared memory segments are paged out, YottaDB recommends configuring systems for use in production with sufficient RAM so as to not require swap space or a swap file. While you must configure an adequate number of huge pages for your application needs as empirically determined by benchmarking / testing, and there is little downside to a generous configuration to ensure a buffer of huge pages available for workload spikes, an excessive allocation of huge pages may affect system throughput by reserving memory for huge pages that could otherwise be used by applications that cannot use huge pages.


++++++++++++++++++++++++++++++++++
Using huge pages
++++++++++++++++++++++++++++++++++

+----------------------------------------------------------------------+---------------------------------------------------------------------------------------------------------------------------------------+
| Prerequisites                                                        | Notes                                                                                                                                 |
+======================================================================+=======================================================================================================================================+
| A 32- or 64-bit x86 CPU running a Linux kernel with huge pages       | All currently Supported Linux distributions appear to support huge pages; to confirm, use the command: grep hugetlbfs                 |
| enabled                                                              | /proc/filesystems which should report: nodev hugetlbfs                                                                                |
+----------------------------------------------------------------------+---------------------------------------------------------------------------------------------------------------------------------------+
| libhugetlbfs.so                                                      | Use your Linux system's package manager to install the libhugetlbfs.so library in a standard location. Note that libhugetlbfs is not  |
|                                                                      | in Debian repositories and must be manually installed; YottaDB on Debian releases is Supportable, not Supported.                      |
+----------------------------------------------------------------------+---------------------------------------------------------------------------------------------------------------------------------------+
| Have sufficient number of huge pages available.                      | To reserve Huge Pages boot Linux with the hugepages=num_pages kernel boot parameter; or, shortly after bootup when unfragmented       |
|                                                                      | memory is still available, with the command: hugeadm --pool-pages-min DEFAULT:num_pages                                               |
|                                                                      | For subsequent on-demand allocation of Huge Pages, use: hugeadm --pool-pages-max DEFAULT:num_pages                                    |
|                                                                      | These delayed (from boot) actions do not guarantee availability of the requested number of huge pages; however, they are safe as, if a|
|                                                                      | sufficient number of huge pages are not available, Linux simply uses traditional sized pages.                                         |
+----------------------------------------------------------------------+---------------------------------------------------------------------------------------------------------------------------------------+

**Using Huge Pages for Shared Memory**

To use huge pages for shared memory (journal buffers, replication journal pool and global buffers):

* Permit YottaDB processes to use huge pages for shared memory segments (where available, YottaDB recommends option 1 below; however not all file systems support extended attributes). Either:
 
 1. Set the CAP_IPC_LOCK capability needs for your mumps, mupip and dse processes with a command such as:

    .. parsed-literal::
       setcap 'cap_ipc_lock+ep' $gtm_dist/mumps

.

 2. Permit the group used by YottaDB processes needs to use huge pages with the following command, which requires root privileges: 

    .. parsed-literal::
       echo gid >/proc/sys/vm/hugetlb_shm_group

* Set the environment variable HUGETLB_SHM for each process to "yes". 

**Using huge pages for YottaDB process private memory**

To use huge pages for process working space and dynamically linked code:

* Set the environment variable HUGETLB_MORECORE for each process to "yes". 

Although not required to use huge pages, your application is also likely to benefit from including the path to libhugetlbfs.so in the LD_PRELOAD environment variable.

If you enable huge pages for all applications (by setting HUGETLB_MORECORE, HUGETLB_SHM, and LD_PRELOAD as discussed above in /etc/profile and/or /etc/csh.login), you may find it convenient to suppress warning messages from common applications that are not configured to take advantage of huge pages by also setting the environment variable HUGETLB_VERBOSE to zero (0).

Refer to the documentation of your Linux distribution for details. Other sources of information are: 

* http://www.kernel.org/doc/Documentation/vm/hugetlbpage.txt

* http://lwn.net/Articles/374424/

* https://www.ibm.com/developerworks/community/blogs/fe313521-2e95-46f2-817d-44a4f27eba32/entry/backing_guests_with_hugepages?lang=en 

* the HOWTO guide that comes with libhugetlbfs (http://sourceforge.net/projects/libhugetlbfs/files/)

.. note::
   Since the memory allocated by Linux for shared memory segments mapped with huge pages is rounded up to the next multiple of huge pages, there is potentially unused memory in each such shared memory segment. You can therefore increase any or all of the number of global buffers, journal buffers, and lock space to make use of this otherwise unused space. You can make this determination by looking at the size of shared memory segments using ipcs. Contact YottaDB support for a sample program to help you automate the estimate. Transparent huge pages may further improve virtual memory page table efficiency. Some supported releases automatically set transparent_hugepages to "always"; others may require it to be set at or shortly after boot-up. Consult your Linux distribution's documentation.

-------------------------------------
Configuring the Restriction Facility
-------------------------------------

Post installation, a system administrator can optionally add a restrict.txt file in $gtm_dist to restrict the use of certain YottaDB facilities to a group-name. The owner and group for $gtm_dist/restrict.txt can be different from those used to install YottaDB. The file may contain zero or more of the following case-insensitive lines in any order:

.. parsed-literal::
   BREAK[:<group-name>]
   ZBREAK[:<group-name>]
   ZCMDLINE[:<group-name>]
   ZEDIT[:<group-name>]
   ZSYSTEM[:<group-name>]
   CENABLE[:<group-name>]
   PIPE_OPEN[:<group-name>]
   DIRECT_MODE[:<group-name>]
   DSE[:<group-name>]
   TRIGGER_MOD[:<group-name>]


If the file $gtm_dist/restrict.txt does not exist, YottaDB does not restrict any facilities.

Any non-empty lines that do not match the above format cause processes with read-only permission to behave as if they could not read the file, and YottaDB enforces all restrictions.

Restrictions apply as follows:

+---------------------------------------------------------+----------------------------------------------------------------------------------------------------+
| YottaDB Facility                                        | Behavior                                                                                           |
+=========================================================+====================================================================================================+
| BREAK                                                   | YottaDB ignores any break command                                                                  |
+---------------------------------------------------------+----------------------------------------------------------------------------------------------------+
| ZBREAK                                                  | any ZBREAK produces a RESTRICTEDOP error                                                           |
+---------------------------------------------------------+----------------------------------------------------------------------------------------------------+
| ZCMDLINE                                                | YottaDB returns an empty string for all references to $ZCMDLINE                                    |
+---------------------------------------------------------+----------------------------------------------------------------------------------------------------+
| ZEDIT                                                   | any ZEDIT produces a RESTRICTEDOP error                                                            |
+---------------------------------------------------------+----------------------------------------------------------------------------------------------------+
| ZSYSTEM                                                 | any ZSYSTEM produces a RESTRICTEDOP error                                                          |
+---------------------------------------------------------+----------------------------------------------------------------------------------------------------+
| CENABLE                                                 | the process acts like $gtm_nocenable is TRUE and ignores any CENABLE deviceparameter               |
+---------------------------------------------------------+----------------------------------------------------------------------------------------------------+
| PIPE_OPEN                                               | any OPEN of a PIPE device produces a RESTRICTEDOP error                                            |
+---------------------------------------------------------+----------------------------------------------------------------------------------------------------+
| DIRECT_MODE                                             | mumps -direct terminates immediately with a RESTRICTEDOP error                                     |
+---------------------------------------------------------+----------------------------------------------------------------------------------------------------+
| DSE                                                     | terminates immediately with a RESTRICTEDOP error                                                   |
+---------------------------------------------------------+----------------------------------------------------------------------------------------------------+
| TRIGGER_MOD                                             | any $ZTRIGGER() or MUPIP TRIGGER that attempts a change or delete produces a RESTRICTEDOP error    |
+---------------------------------------------------------+----------------------------------------------------------------------------------------------------+

If the file exists, a process:

* that has write authorization to restrict.txt has no restrictions

* that has no read access to restrict.txt is restricted from all facilities for which YottaDB supports a restriction (currently the above list)

* that has read-only access to restrict.txt is restricted from any listed facility unless it is a member of the group specified in the optional group-id following the facility name

Note that restricting $ZCMDLINE prevents things like: mumps -run %XCMD 'for read x xecute x' which can act as substitutes for Direct Mode. 
