
.. index:: Basic Operations

=======================
Basic Operations
=======================

.. contents::
   :depth: 2

------------------------------
YottaDB Environment Setup
------------------------------

Several environment variables control the operation of YottaDB. Some of them must be set up for normal operation, where as for others YottaDB assumes a default value if they are not set.

Your YottaDB distribution comes with many scripts that set up a default YottaDB environment for the shell of your choice. These scripts are as follows: 


**gtmprofile**: uses reasonable defaults to set up a system and YottaDB application development environment for POSIX shells. The gtmprofile script sets default values for environment variables gtm_dist, gtmgbldir, gtm_icu_version, gtm_log, gtm_principal_editing, gtm_prompt, gtm_retention, gtmroutines, gtm_tmp, and gtmver. When you source the gtmprofile script, it creates a default execution environment (global directory and a default database file with BEFORE_IMAGE journaling) if none exists.

* **gtmcshrc**: sets up a default YottaDB environment for C-shell compatible shells. It sets up default values for gtm_dist, gtmgbldir, gtm_chset and gtmroutines. It also creates aliases so you can execute YottaDB and its utilities without typing the full path. While gtmprofile is current with YottaDB releases, gtmcshrc is at the same level of sophistication as gtmprofile_preV54000. It is not as actively maintained as the gtmprofile script.

* **gtmprofile_preV54000**: This script was provided as gtmprofile in YottaDB/GT.M distributions prior to V5.4-000. This script is a POSIX shell equivalent of gtmschrc.

* **gtmbase**: detects the shell type and adds gtmprofile to .profile or gtmchsrc to .cshrc so the shell automatically sources gtmprofile or gtmschrc on a subsequent login operation. YottaDB does not recommend using gtmbase as is - use it as an example of a script for you to develop suitable for your systems. It is not as actively maintained as the gtmprofile script.

* **gtm**: starts YottaDB in direct mode on POSIX shells. The gtm script sources gtmprofile. It also deletes prior generation journal and temporary files older than the number of days specified by the environment variable gtm_retention. It attempts to automatically recover the database when it runs and as such is suitable for "out of the box" usage of YottaDB. Although it will work for large multi-user environments, you may want to modify or replace it with more efficient scripting.

* **gdedefaults**: a GDE command file that specifies the default values for database characteristics defined by GDE.

These scripts are designed to give you a friendly out-of-the-box YottaDB experience. Even though you can set up an environment for normal YottaDB operation without using these scripts, it is important to go through these scripts to understand the how to manage environment configuration. 


++++++++++
gtmprofile
++++++++++

On POSIX shells, gtmprofile helps you set an environment for single-user, non-replicated use of YottaDB.

gtmprofile sets reasonable defaults for the following environment variables for normal YottaDB operation:

.. parsed-literal::
   gtmdir, gtm_dist, gtm_etrap, gtmgbldir, gtm_icu_version, gtm_log, gtm_principal_editing, gtm_prompt, gtm_repl_instance, gtm_retention, gtmroutines, gtm_tmp, gtmver 

You can set the following environment variables before sourcing gtmprofile or running the gtm script;


* **gtm_chset** - set this to "UTF-8" to run YottaDB in UTF-8 mode; it defaults to M mode. As UTF-8 mode requires a UTF-8 locale to be set in LC_CTYPE or LC_ALL, if a locale is not specified, gtmprofile also attempts to set a UTF-8 locale. Since YottaDB in UTF-8 mode often requires gtm_icu_version to be set, if it is not set, gtmprofile attempts to determine the ICU version on the system and set it. This requires the icu-config program to be installed and executable by gtmprofile.

* **gtmdir** - set this to define a directory for the environment set by gtmprofile

The following shell variables are used by the script and left unset at its completion: 

.. parsed-literal::
   old_gtm_dist, old_gtmroutines, old_gtmver, tmp_gtm_tmp, tmp_passwd. 

The $gtmroutines value set by the gtmprofile script enables auto-relink by default for object files in the $gtmdir/$gtmver/o directory in M mode and $gtmdir/$gtmver/o/utf8 in UTF-8 mode. Auto-relink requires shared memory resources and limits beyond those for database operation. If your system has inadequate shared memory configured, YottaDB displays messages along the lines of:

.. parsed-literal::
   %GTM-E-SYSCALL, Error received from system call shmget() failed

Refer to your OS documentation to configure shared memory limits (for example, on common Linux systems, the kernel.shmmax parameter in /etc/sysctl.conf).

The gtmprofile (and gtm) scripts, by design, are idempotent so that calling them repeatedly is safe. The YottaDB installation process ensures that gtmprofile always sets gtm_dist correctly. Idempotency is implemented by checking the value of $gtm_dist and skipping all changes to environment variables if gtm_dist is already defined.

When gtm sources gtmprofile, it provides a default execution environment (global directory and a default database (with BEFORE_IMAGE journaling) if none exists. By default, it creates the database in $HOME/.yottadb with a structure like the following; note that this directory structure has different locations for YottaDB routines (r), object files (o), and database-related files (g):

.. parsed-literal::

   .yottadb
      | -- r
      | -- V6.2-000_x86_64
      | | -- g 
      | | | -- gtm.dat 
      | | | -- gtm.gld 
      | | ` -- gtm.mjl 
      | | -- o 
      | | ` -- utf8 
      | ` -- r 
      | -- V6.2-001_x86_64
       | -- g 
        | | -- gtm.dat 
         | | -- gtm.gld 
          | ` -- gtm.mjl 
           | -- o 
            | ` -- utf8 
             ` -- r


where V6.2-001_x86_64 represents the current release and platform information and V6.2-000_x86_64 represents a previously used YottaDB/GT.M release.

On 64-bit platforms in M mode, gtmprofile sets the environment variable gtmroutines to something like the following (where $gtm_dist and $gtmver are as discussed above):

.. parsed-literal::
   $gtmdir/$gtmver/o*($gtmdir/$gtmver/r $gtmdir/r) $gtm_dist/plugin/o($gtm_dist/plugin/r) $gtm_dist/libgtmutil.so $gtm_dist

$gtmdir/$gtmver/o*($gtmdir/$gtmver/r $gtmdir/r) specifies that YottaDB searches for routines in $gtmdir/$gtmver/r, then $gtmdir/r, using $gtmdir/$gtmver/o for object code, then for routines in the plugin subdirectory of $gtm_dist, then in $gtm_dist, looking first for a shared library of routines distributed with YottaDB and then for other routines subsequently installed there. The * -suffix after the object directory enables the auto-relink facility.

For a comprehensive discussion of YottaDB source and object file management, refer to the $ZROUTINES section in the Programmer's Guide.

When $gtm_chset is set to UTF-8, gtmprofile sets gtmroutines to something like this:

.. parsed-literal::
   $gtmdir/$gtmver/o/utf8*($gtmdir/$gtmver/r $gtmdir/r) $gtm_dist/plugin/o/utf8($gtm_dist/plugin/r) $gtm_dist/libgtmutil.so $gtm_dist

Note that gtmprofile sets $gtm_dist in UTF-8 mode to the utf8 subdirectory of the YottaDB installation directory. If you have installed any plugins that include shared libraries, gtmprofile script includes those. For example, with the POSIX and ZLIB plugins installed on a 64-bit platform, gtmdir set to /home/jdoe1 and YottaDB installed in /opt/yottadb/V1.10, gtmprofile would set gtmroutines to:

.. parsed-literal::
   /home/jdoe1/.yottadb/V1.10/o*(/home/jdoe1/.yottadb/V1.10/r /home/jdoe1/.yottadb/r) /usr/lib/yottadb/V1.10/plugin/o/_POSIX.so /usr/lib/yottadb/V1.10/plugin/o/_ZLIB.so /usr/lib/yottadb/V1.10/plugin/o(/usr/lib/yottadb/V1.10/plugin/r) /usr/lib/yottadb/V1.10/libgtmutil.so /usr/lib/yottadb/V1.10

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
   lias gtm '$gtm_dist/mumps -direct'
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



