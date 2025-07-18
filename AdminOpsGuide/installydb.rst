.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2017-2025 YottaDB LLC and/or its subsidiaries.#
.. # All rights reserved.                                        #
.. #                                                             #
.. # Portions Copyright (c) Fidelity National                    #
.. # Information Services, Inc. and/or its subsidiaries.         #
.. #                                                             #
.. #     This document contains the intellectual property        #
.. #     of its copyright holder(s), and is made available       #
.. #     under a license.  If you do not know the terms of       #
.. #     the license, please stop and do not read further.       #
.. #                                                             #
.. ###############################################################

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

YottaDB distributions for selected platforms, including GNU/Linux on standard x86_64 and ARM architectures, can be downloaded under the terms of the Affero GNU General Public License (AGPL) version 3, from the `YottaDB releases page on GitLab <https://gitlab.com/YottaDB/DB/YDB/-/releases>`_. Each distribution is a tarball that includes a :code:`ydbinstall` script. Unpack the tarball in a temporary directory, change to that directory, and run the script. For Supported platforms, and selected Supportable platforms, we recommend using the `ydbinstall.sh <https://download.yottadb.com/ydbinstall.sh>`_ script, which will download, unpack, and install YottaDB for the platform on which it is run. The instructions for :ref:`ydbinstall script <ydbinstall-script>` and ydbinstall.sh script are identical.

---------------------------
Before You Begin
---------------------------

Before you begin installing YottaDB, perform the following tasks:

* Read the YottaDB Release Notes Documentation. The release documents contain the latest information that may be critical for installing and configuring YottaDB. They are located under the `Tags in the YottaDB GitLab repository <https://gitlab.com/YottaDB/DB/YDB/tags>`_ and can also be reached from the `Documentation page on the YottaDB website <https://yottadb.com/resources/documentation/>`_

* Determine whether or not YottaDB access is restricted to a specific group. Keep the group name handy as you will have to enter it during the installation process.

* Set the environment variable ydb_log to a directory where YottaDB should create log files. If you do not set ydb_log, YottaDB creates log files in a directory in /tmp. However, this is not recommended because it makes YottaDB log files vulnerable to the retention policy of a temporary directory.

.. note::
   In the latest version, gtmsecshr logs its messages in the system log and the environment variable ydb_log is ignored.

* If you need to perform UTF-8 mode operations in YottaDB, you must have at least ICU version 3.6 installed. YottaDB uses ICU 3.6 (or above) to provide support for Unicode. YottaDB generates the distribution for Unicode only if ICU 3.6 (or above) is installed on your system. By default, YottaDB uses the most current version of ICU. YottaDB expects ICU to have been built with symbol renaming disabled and issues an error at startup if the currently installed version of ICU has been built with symbol renaming enabled. If you intend to use a version of ICU built with symbol renaming enabled or any version other than the default, keep the MAJOR VERSION and MINOR VERSION numbers ready as you will have to enter it as MajorVersion.MinorVersion (for example "3.6" to denote ICU-3.6) during the installation process.

.. note::
  Installing YottaDB on an NFS mounted directory is not recommended. Several NFS characteristics violate YottaDB database design assumptions which may manifest themselves as hard to diagnose problems. If you still choose to install and operate YottaDB from an NFS mounted directory, there are chances that at some point you will face significant problems with performance and response time. While you should never operate the YottaDB database and journal files from an NFS mounted directory you can safely, except on Linux, use an NFS mounted directory for keeping source and object code modules and performing sequential file IO. While NFS mounted files may work for you, historically they have not provided sufficient support for file locking over NFS to prevent intermittent errors when you have  significant concurrent file activity.


-------------------------
Installation Procedure
-------------------------

+++++++++++++++++++++++++++++++++
Installing prerequisite packages
+++++++++++++++++++++++++++++++++

Install the following packages:

.. code-block:: bash

   # Ubuntu/Debian
   apt-get install -y --no-install-recommends file binutils libelf-dev libicu-dev nano wget

   # RHEL/Rocky Linux
   yum install -y file binutils findutils elfutils-libelf libicu wget procps-ng nano gzip

.. _installation-procedure-installing-yottadb:

+++++++++++++++++++
Installing YottaDB
+++++++++++++++++++

* Create a temporary directory and change to it, e.g.: mkdir /tmp/tmp ; cd /tmp/tmp

* Get the YottaDB installer: wget https://download.yottadb.com/ydbinstall.sh

* Make it executable: chmod +x ydbinstall.sh

* Run it with your choice of directory where you want it installed (omit the -verbose option for less output):

  .. code-block:: bash

    sudo --preserve-env=ydb_icu_version ./ydbinstall.sh --installdir /opt/yottadb/ --utf8 --verbose

  The :code:`--preserve-env=ydb_icu_version` is needed in case the :code:`ydb_icu_version` env var is set to a value
  other than what :code:`pkg-config --modversion icu-io` would return (seen as necessary for example, on a :code:`SLED 15`
  or :code:`openLeap 15` SUSE Linux system). It is not needed otherwise but does not hurt either.

* :code:`yottadb -version` provides a detailed report on the YottaDB build, e.g.,

  .. code-block:: bash

     $ yottadb -version
     YottaDB release:         r1.30
     Upstream base version:   GT.M V6.3-008
     Platform:                Linux x86_64
     Build date/time:         2020-08-11 20:55
     Build commit SHA:        177eb8e48098204dafe564cac2bcb84312b2853a
     $

Note that the ``ydbinstall`` script explicitly removes ``setgid``, ``setuid`` and sticky bits from the target installation directory if it exists with
those bits set. Previously the sub-directories created by the installation script inappropriately carried the ``setgid`` settings.

Additionally, the ``ydbinstall`` script records ownership, permissions and OpenSSH SHA256 checksum values of all installed files for future reference in ``$ydb_dist/install_permissions.log`` and ``$ydb_dist/install_sha256_checksum.log``.

+++++++++++++++++++++++++++++++++++++++++++++
Compiling the Reference Implementation Plugin
+++++++++++++++++++++++++++++++++++++++++++++

Compile the reference implementation plugin as follows:

* Install the development headers and libraries for libgcrypt, libgpgme, libconfig, and libssl. On Linux, the package names of development libraries usually have a suffix such as -dev or -devel and are available through the package manager. For example, on Ubuntu_x86_64, the following command installs the required development libraries:

  .. code-block:: bash

   sudo apt-get install libgcrypt11-dev libgpgme11-dev libconfig-dev libssl-dev

  The package names vary by distribution/version.

  .. note::

   :code:`$ydb_dist` points to the absolute path for the directory where YottaDB is installed.

* Unpack $ydb_dist/plugin/gtmcrypt/source.tar to a temporary directory, for example:

 .. code-block:: bash

   mkdir /tmp/plugin-build
   cd /tmp/plugin-build
   cp $ydb_dist/plugin/gtmcrypt/source.tar .
   tar -xvf source.tar


* Follow the instructions in the README.

  * Open Makefile with your editor; review and edit the common header (IFLAGS) and library paths (LIBFLAGS) in the Makefile to reflect those on your system.

  * Define the ydb_dist environment variable to point to the absolute path for the directory where YottaDB is installed

  * Copy and paste the commands from the README to compile and install the encryption plugin with the permissions defined at install time

* Compare the permissions of $ydb_dist/libyottadb.so to the newly installed shared libraries in $ydb_dist/plugin. Adjust the permission of the newly installed libraries as necessary.

.. _ydbinstall-script:

---------------------
ydbinstall Script
---------------------

`ydbinstall.sh <https://download.yottadb.com/ydbinstall.sh>`_ is a stand-alone YottaDB installation script that installs YottaDB using reasonable defaults. It deduces the distribution from :code:`/etc/os-release` and downloads the appropriate binary distribution. For selected derivatives of Supported distributions (e.g., `Rocky Linux <https://rockylinux.org/>`_ as a derivative of `Red Hat Enterprise Linux <https://www.redhat.com/en/technologies/linux-platforms/enterprise-linux>`_), it downloads the YottaDB binary distribution of the Supported distribution. The derivative distributions are Supportable, not Supported. :code:`ydbinstall.sh` then invokes the :code:`ydbinstall` in the unpacked distribution, passing to it command line switches as discussed below.

:code:`ydbinstall` in each YottaDB binary distribution installs YottaDB from the temporary directory in which the distribution is unpacked. It allows considerable customization using the following command line switches:

+---------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| Command Line Switches                                   | \* | Description                                                                                                            |
+=========================================================+====+========================================================================================================================+
| \-\-aim                                                 |    | Download and install the `YottaDB AIM plugin <https://docs.yottadb.com/Plugins/ydbaim.html>`_.                         |
+---------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| \-\-allplugins                                          |    | Installs all YottaDB plugins that :code:`ydbinstall` knows about.                                                      |
+---------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| \-\-branch branchname                                   |    | Builds YottaDB from a specific git branch; use with \-\-from-source.                                                   |
+---------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| \-\-build-type buildtype                                | \* | Type of YottaDB build, default is pro.                                                                                 |
+---------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| \-\-copyenv [dirname]                                   |    | Copy ydb_env_set, ydb_env_unset, and gtmprofile files to dirname, default :code:`/usr/local/etc`; incompatible with    |
|                                                         |    | linkenv.                                                                                                               |
+---------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| \-\-copyexec [dirname]                                  |    | Copy ydb and gtm scripts to dirname, default :code:`/usr/local/bin`; incompatible with linkexec.                       |
+---------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| \-\-curl                                                |    | Installs the YottaDB `plugin <https://gitlab.com/YottaDB/Util/YDBCurl>`_ for `curl <https://curl.se/libcurl/>`_.       |
+---------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| \-\-debug                                               | \* | Turn on debugging option with set -x.                                                                                  |
+---------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| \-\-distrib dirname or URL                              |    | Source directory for YottaDB distribution tarball, local or remote.                                                    |
+---------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| \-\-dry-run                                             |    | Do everything short of installing YottaDB, including downloading the distribution.                                     |
+---------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| \-\-encplugin                                           | \† | Download and install the `YottaDB encryption plugin <./encryption.html>`_.                                             |
+---------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| \-\-filename filename                                   |    | Name of the YottaDB distribution tarball.                                                                              |
+---------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| \-\-force-install                                       |    | Install even if the current platform is not supported.                                                                 |
+---------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| \-\-from-source reponame                                |    | Clone the repository specified by reponame in current directory and change to YDB/ subdirectory.                       |
+---------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| \-\-group group                                         |    | Group that should own the YottaDB installation.                                                                        |
+---------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| \-\-group-restriction                                   |    | Limit execution to a group; defaults to unlimited if not specified.                                                    |
+---------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| \-\-gui                                                 |    | Download and install the `YottaDB GUI <https://gitlab.com/YottaDB/UI/YDBGUI/>`_.                                       |
+---------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| \-\-help                                                |    | Print this usage information.                                                                                          |
+---------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| \-\-installdir dirname                                  |    | Directory where YottaDB is to be installed (defaults to :code:`/usr/local/lib/yottadb/<release>`), e.g.                |
|                                                         |    | :code:`/usr/local/lib/yottadb/r202`.                                                                                   |
+---------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| \-\-keep-obj                                            |    | Keep .o files of M routines (normally deleted on platforms with YottaDB support for routines in shared libraries).     |
+---------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| \-\-linkenv [dirname]                                   |    | Create or update links in dirname to ydb_env_set, ydb_env_unset and gtmprofile files, default :code:`/usr/local/etc`;  |
|                                                         |    | incompatible with copyenv. Linkenv is the default.                                                                     |
+---------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| \-\-linkexec [dirname]                                  |    | Create or update links in dirname to ydb and gtm scripts, default :code:`/usr/local/bin`; incompatible with copyexec.  |
|                                                         |    | Linkexec is the default.                                                                                               |
+---------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| \-\-nocopyenv                                           |    | Do not copy ydb_env_set, ydb_env_unset, and gtmprofile to another directory.                                           |
+---------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| \-\-nocopyexec                                          |    | Do not copy ydb and gtm scripts to another directory.                                                                  |
+---------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| \-\-nodeprecated                                        |    | Do not install deprecated components, especially %DSEWRAP.                                                             |
+---------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| \-\-nolinkenv                                           |    | Do not create link to ydb_env_set, ydb_env_unset, and gtmprofile from another directory.                               |
+---------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| \-\-nolinkexec                                          |    | Do not create link to ydb and gtm scripts from another directory.                                                      |
+---------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| \-\-nopkg-config                                        |    | Do not create yottadb.pc for pkg-config, or update an existing file.                                                   |
+---------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| \-\-octo parameters                                     | \† | Download and install `Octo® <https://docs.yottadb.com/Octo/>`_ a YottaDB plugin for SQL access to databases.           |
|                                                         |    | Also installs required POSIX and AIM plugins. Specify optional cmake parameters for Octo as necessary.                 |
+---------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| \-\-overwrite-existing                                  |    | Install into an existing directory, overwriting contents; defaults to requiring new directory.                         |
+---------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| \-\-plugins-only                                        |    | Add or update specified plugins to an existing YottaDB installation. This option requires the \-\-overwrite-existing   |
|                                                         |    | option as the YottaDB directory must already exist.                                                                    |
+---------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| \-\-posix                                               | \† | Download and install the `YottaDB POSIX plugin <https://docs.yottadb.com/Plugins/ydbposix.html>`_.                     |
+---------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| \-\-preserveRemoveIPC                                   |    | Do not allow changes to RemoveIPC in :code:`/etc/systemd/logind.conf` if needed; defaults to allow changes.            |
+---------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| \-\-prompt-for-group                                    | \* | YottaDB installation script prompts for group; default is yes.                                                         |
+---------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| \-\-ucaseonly-utils                                     |    | Install only upper case utility program names; defaults to both if not specified.                                      |
+---------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| \-\-user username                                       |    | User who should own YottaDB installation; default is root.                                                             |
+---------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| \-\-utf8                                                |    | Install UTF-8 support using the default ICU version of the system.                                                     |
+---------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| \-\-webserver                                           | \† | Install the YottaDB web server plugin.                                                                                 |
+---------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| \-\-verbose                                             | \* | Output diagnostic information as the script executes; default is to run quietly.                                       |
+---------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| \-\-ydbsupport                                          |    | Download and install the `YDBSupport plugin <https://docs.yottadb.com/Plugins/ydbsupport.html>`_.                      |
+---------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+
| \-\-zlib                                                |    | Download and install the `YDBzlib plugin <https://docs.yottadb.com/Plugins/ydbzlib.html>`_.                            |
+---------------------------------------------------------+----+------------------------------------------------------------------------------------------------------------------------+

* Options that take a value (e.g, \-\-group) can be specified as either \-\-option=value or \-\-option value
* Options marked with \* are likely to be of interest primarily to YottaDB developers
* Options marked with † require Internet access as well as that :code:`gcc` and required libraries be installed.
* Version is defaulted from the yottadb file if one exists in the same directory as the installer
* This script must run as root.

:code:`ydbinstall` / :code:`ydbinstall.sh` creates :code:`usr/share/pkgconfig` (the directory where :code:`pkg-config` by default tracks packages) if it does not exist. Note that this was added effective release `r1.34 <https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.34>`_.

When the script installs a plugin, it records a 1-line summary of the last commit for that plugin, including the hash and a summary of the commit in the :code:`$ydb_dist/plugin/version` directory. Since plugins are `continuously released <https://yottadb.com/yottadb-continuous-integration-continuous-delivery/>`_, this makes it easier to identify the exact commit of an installed plugin.

++++++++
Examples
++++++++

.. code-block:: bash

   sudo --preserve-env=ydb_icu_version ./ydbinstall.sh

This example installs the latest YottaDB release in a subdirectory of :code:`/usr/local/lib/yottadb`, e.g., :code:`/usr/local/lib/yottadb/r134`.

.. code-block:: bash

   sudo --preserve-env=ydb_icu_version ./ydbinstall.sh --utf8 --verbose

This example installs the latest YottaDB release with added support for UTF-8 and outputs diagnostic information as the script executes.

.. code-block:: bash

   sudo --preserve-env=ydb_icu_version ./ydbinstall.sh --installdir /r120 r1.20

This example installs YottaDB release r1.20 in the r120 directory.

The :code:`--encplugin`, :code:`--octo` and :code:`--posix` options were added to the :code:`ydbinstall` / :code:`ydbinstall.sh` script effective release `r1.30 <https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.30>`_.

If the build of any plugin (those installed by :code:`--encplugin`, :code:`--octo`, :code:`--posix`, and :code:`--zlib` command line options) does not succeed, the :code:`ydbinstall` script retains the directory where it built the plugin.

The :code:`--plugins-only` option was added to the :code:`ydbinstall` / :code:`ydbinstall.sh` script effective release `r1.34 <https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.34>`_. It adds or updates specified plugins to an existing YottaDB installation. Note that this option requires the :code:`--overwrite-existing` option as the YottaDB directory must already exist.

With the :code:`--from-source <repo>` option, the :code:`ydbinstall` / :code:`ydbinstall.sh` script clones the repository specified by :code:`<repo>` using :code:`git clone <repo>` in the current directory, and changes to the :code:`YDB/` subdirectory.
If :code:`--branch <branch>` is specified, it executes :code:`git checkout -B <branch>` to specify a branch other than the default. Then it builds YottaDB, and if successful, installs the built YottaDB using :code:`sudo ydbinstall` of the :code:`ydbinstall` script of the built YottaDB, passing it all command line options except the :code:`--from-source` and :code:`--branch` options. The :code:`sudo ydbinstall` prompts for a password as required.
For example, :code:`ydbinstall --from-source https://gitlab.com/ydbuser/YDB.git --branch working --utf8 --aim --install-directory /usr/local/lib/yottadb/devel_$(date +%Y%m%d)` will checkout, build, and install the :code:`working` branch of YottaDB from the YDB repository of GitLab user :code:`ydbuser` in a date-stamped directory, along with the `Application Independent Metadata plugin <https://gitlab.com/YottaDB/Util/YDBAIM/>`_.
This was added effective release `r1.34 <https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.34>`_.

For YottaDB to correctly operate on Linux systems using `systemd <https://systemd.io>`_, the :code:`RemoveIPC=no` setting is required in :code:`/etc/systemd/logind.conf`. The :code:`ydbinstall`/ :code:`ydbinstall.sh` script checks this setting:

* If :code:`RemoveIPC` is set to no, it proceeds with the installation.

* If :code:`RemoveIPC` is set to yes, the script checks the command line option :code:`--preserveRemoveIPC`:

  * If :code:`--preserveRemoveIPC` is set to no, the script changes the setting in :code:`/etc/systemd/logind.conf` and outputs a message to indicate the change. A restart of :code:`systemd-logind` is required to complete the installation.
  * If :code:`--preserveRemoveIPC` is set to yes, then change :code:`RemoveIPC` to no and restart :code:`systemd-logind` to complete the installation.

.. raw:: html

    <img referrerpolicy="no-referrer-when-downgrade" src="https://download.yottadb.com/AdminOpsGuide.png" />
