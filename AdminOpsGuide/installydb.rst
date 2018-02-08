
.. index::
    Installing YottaDB

========================
2. Installing YottaDB
========================

.. contents::
    :depth: 2

This chapter describes the installation procedure for YottaDB. Always read the release notes for special instructions before installing YottaDB.

-------------------------------------
Obtaining YottaDB Distribution Media
-------------------------------------

YottaDB LLC prefers to distribute YottaDB online via the Internet. YottaDB for selected platforms, including GNU/Linux on the popular x86 architecture and Raspberry Pi hardware, can be downloaded under the terms of the Affero GNU General Public License (AGPL) version 3, from Github (https://raw.githubusercontent.com/YottaDB/YottaDB/master/sr_unix/ydbinstall.sh). Contact YottaDB Support (info@yottadb.com) to obtain a copy of a YottaDB distribution for other platforms or on physical media. 


---------------------------
Before You Begin
---------------------------

Before you begin installing YottaDB, perform the following tasks:

* Read the YottaDB Release Notes Documentation. The release documents contain the latest information that may be critical for installing and configuring YottaDB. They are located under the `Releases tab on the YottaDB Github repository <https://github.com/YottaDB/YottaDB/releases>`_ and can also be reached from the `Documentation page on the YottaDB website <https://yottadb.com/resources/documentation/>`_

* Determine whether or not YottaDB access is restricted to a specific group. Keep the group name handy as you will have to enter it during the installation process.

* Set the environment variable gtm_log to a directory where YottaDB should create log files. If you do not set gtm_log, YottaDB creates log files in a directory in /tmp (AIX, GNU/Linux). However, this is not recommended because it makes YottaDB log files vulnerable to the retention policy of a temporary directory.

.. note::
   In the latest version, gtmsecshr logs its messages in the system log and the environment variable gtm_log is ignored.

* If you need to perform Unicode™-related operations in YottaDB, you must have at least ICU version 3.6 installed. YottaDB uses ICU 3.6 (or above) to provide support for Unicode. YottaDB generates the distribution for Unicode only if ICU 3.6 (or above) is installed on your system. By default, YottaDB uses the most current version of ICU. YottaDB expects ICU to have been built with symbol renaming disabled and issues an error at startup if the currently installed version of ICU has been built with symbol renaming enabled. If you intend to use a version of ICU built with symbol renaming enabled or any version other than the default, keep the MAJOR VERSION and MINOR VERSION numbers ready as you will have to enter it as MajorVersion.MinorVersion (for example "3.6" to denote ICU-3.6) during the installation process.

.. note::
  Installing YottaDB on an NFS mounted directory is not recommended. Several NFS characteristics violate YottaDB database design assumptions which may manifest themselves as hard to diagnose problems. If you still choose to install and operate YoottaDB from an NFS mounted directory, there are chances that at some point you will face significant problems with performance and response time. While you should never operate the YottaDB database and journal files from an NFS mounted directory you can safely, except on Linux, use an NFS mounted directory for keeping source and object code modules and performing sequential file IO. While NFS mounted files may work for you, historically they have not provided sufficient support for file locking over NFS to prevent intermittent errors when you have  significant concurrent file activity.

-------------------------
Installation Procedure
-------------------------



* Create a temporary directory and change to it, e.g.: mkdir /tmp/tmp ; cd /tmp/tmp

* Get the YottaDB installer: wget https://raw.githubusercontent.com/YottaDB/YottaDB/master/sr_unix/ydbinstall.sh

* Make it executable: chmod +x ydbinstall.sh

* Run it with your choice of directory where you want it installed (omit the –verbose option for less output): 

 .. parsed-literal::

    sudo ./ydbinstall.sh --installdir /opt/yottadb/ --utf8 default --verbose

+++++++++++++++++++++++++++++++++++++++++++++
Compiling the Reference Implementation Plugin
+++++++++++++++++++++++++++++++++++++++++++++

Compile the reference implementation plugin as follows:

* Install the development headers and libraries for libgcrypt, libgpgme, libconfig, and libssl. On Linux, the package names of development libraries usually have a suffix such as -dev or -devel and are available through the package manager. For example, on Ubuntu_x86_64 a command like the following installs the required development libraries:

.. parsed-literal::
   sudo apt-get install libgcrypt11-dev libgpgme11-dev libconfig-dev libssl-dev

The package names vary by distribution / version.

* Unpack $gtm_dist/plugin/gtmcrypt/source.tar to a temporary directory, for example: 

 .. parsed-literal::
   mkdir /tmp/plugin-build
   cd /tmp/plugin-build
   cp $gtm_dist/plugin/gtmcrypt/source.tar .
   tar -xvf source.tar

* Follow the instructions in the README.

  * Open Makefile with your editor; review and edit the common header (IFLAGS) and library paths (LIBFLAGS) in the Makefile to reflect those on your system.
  
  * Define the gtm_dist environment variable to point to the absolute path for the directory where YottaDB is installed
  
  * Copy and paste the commands from the README to compile and install the encryption plugin with the permissions defined at install time

* Compare the permissions of $gtm_dist/libgtmshr.so to the newly installed shared libraries in $gtm_dist/plugin. Adjust the permission of the newly installed libraries as necessary.

---------------------
ydbinstall Script
---------------------

ydbinstall is a stand-alone YottaDB installation script that installs YottaDB using reasonable defaults. ydbinstall is a part of the YottaDB binary distribution and you can also use it to install YottaDB from the temporary directory in which you unpack the YottaDB distribution. It allows considerable customization using the following command line switches:

+-------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| Command Line Switches                                 | \* | Description                                                                                                            |
+=======================================================+====+========================================================================================================================+
| --build-type buildtype                                | \* | Type of YottaDB build, default is pro                                                                                  |
+-------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| --copyenv dirname                                     |    | Copy gtmprofile and gtmcshrc files to dirname; incompatible with linkenv                                               |
+-------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| --copyexec dirname                                    |    | Copy gtm script to dirname; incompatible with linkexec                                                                 |
+-------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| --debug                                               | \* | Turn on  debugging option with set -x                                                                                  |
+-------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| --distrib dirname or URL                              |    | Source directory for YottaDB distribution tarball, local or remote                                                     |
+-------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| --dry-run                                             |    | Do everything short of installing YottaDB, including downloading the distribution                                      |
+-------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| --group group                                         |    | Group that should own the YottaDB installation                                                                         |
+-------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| --group-restriction                                   |    | Limit execution to a group; defaults to unlimited if not specified                                                     |
+-------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| --gtm                                                 |    | Install GT.M instead of YottaDB                                                                                        |
+-------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| --help                                                |    | Print this usage information                                                                                           |
+-------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| --installdir dirname                                  |    | Directory where YottaDB is to be installed (defaults to /usr/local/lib/yottadb/version)                                |
+-------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| --keep-obj                                            |    | Keep .o files of M routines (normally deleted on platforms with YottaDB support for routines in shared libraries);     |
+-------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| --linkenv dirname                                     |    | Create link in dirname to gtmprofile and gtmcshrc files; incompatible with copyenv                                     |
+-------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| --linkexec dirname                                    |    | Create link in dirname to gtm script; incompatible with copyexec                                                       |
+-------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| --overwrite-existing                                  |    | Install into an existing directory, overwriting contents; defaults to requiring new directory                          |
+-------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| --prompt-for-group                                    | \* | YottaDB installation script prompts for group; default is yes                                                          |
+-------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| --ucaseonly-utils                                     |    | Install only upper case utility program names; defaults to both if not specified                                       |
+-------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| --user username                                       |    | User who should own YottaDB installation; default is root                                                              |
+-------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| --utf8 ICU_version                                    |    | Install UTF-8 support using specified major.minor ICU version; specify default to use default version                  |
+-------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| --verbose                                             | \* | Output diagnostic information as the script executes; default is to run quietly                                        |
+-------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+



* Options that take a value (e.g, --group) can be specified as either --option=value or --option value

* Options marked with \* are likely to be of interest primarily to YottaDB developers
        
* Version is defaulted from the mumps file if one exists in the same directory as the installer

* This version must run as root.


To run the ydbinstall script, run it as root.

**Examples**

.. parsed-literal::
   sudo ./ydbinstall.sh

This example installs the latest YottaDB release at /usr/local/lib/yottadb/version.

.. parsed-literal::
   sudo ./ydbinstall.sh --utf8 default --verbose

This example installs the latest YottaDB release with added support for UTF-8 and outputs diagnostic information as the script executes.

.. parsed-literal::
   sudo ./ydbinstall.sh --installdir /r110 r1.10

This example installs YottaDB release r1.10 in the r110 directory.

.. parsed-literal::
   sudo ./ydbinstall.sh --gtm

This example installs the latest GT.M version at /usr/local/lib/fis-gtm/version.





