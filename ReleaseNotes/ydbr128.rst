.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2019-2021 YottaDB LLC and/or its subsidiaries.#
.. # All rights reserved.                                        #
.. #                                                             #
.. #     This source code contains the intellectual property     #
.. #     of its copyright holder(s), and is made available       #
.. #     under a license.  If you do not know the terms of       #
.. #     the license, please stop and do not read further.       #
.. #                                                             #
.. ###############################################################

=================
YottaDB r1.28
=================

.. contents::
   :depth: 2

------------------------------
Release Note Revision History
------------------------------

+-------------------------------+---------------------------------------+----------------------------------------------------------------------+
| Revision                      | Date                                  | Summary                                                              |
+===============================+=======================================+======================================================================+
| 1.00                          | September 11, 2019                    | r1.28 Initial Release                                                |
+-------------------------------+---------------------------------------+----------------------------------------------------------------------+

-----------------------------
Contact Information
-----------------------------

++++++++++++
YottaDB LLC
++++++++++++

| 40 Lloyd Avenue, Suite 104
| Malvern, PA 19355, USA
| info@yottadb.com
| +1 (610) 644-1898

++++++++++++
Support
++++++++++++

**Customers**

Contact your YottaDB support channel.

**Others**

For free (to you) support from members of communities who run widely available applications on YottaDB, please use an application-specific list where appropriate.

* For requests other than to the communities below, post an Issue at `https://gitlab.com/YottaDB/DB/YDB/issues <https://gitlab.com/YottaDB/DB/YDB/issues>`_ and include the words "Help Wanted" in the summary.

* For issues specific to the use of YottaDB from node.js via `nodem <https://github.com/dlwicksell/nodem>`_, `QewdJS <http://qewdjs.com/>`_ or `Enterprise Web Developer <http://ewdjs.com/>`_, post to the `Enterprise Web Developer community <http://groups.google.com/group/enterprise-web-developer-community>`_.

* For issues specific to the use of YottaDB with `VistA <https://en.wikipedia.org/wiki/VistA>`_ flavors, post to the `Hardhats <http://groups.google.com/group/hardhats>`_ list.

* For issues specific to the use of YottaDB with M other than for applications above, post to the `comp.lang.mumps <http://groups.google.com/group/comp.lang.mumps>`_ list.

----------------------
r1.28
----------------------

+++++++++++++
Overview
+++++++++++++

For users of YottaDB using the `Go API <https://docs.yottadb.com/MultiLangProgGuide/goprogram.html>`_, r1.28 is a major release because it is required for production grade access to YottaDB from `Go <https://golang.org>`_. For others, it is a minor release with a small set of enhancements anf fixes as detailed in the `complete release notes <https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.28>`_.

YottadB r1.28 is upward compatible with `YottaDB r1.26 <https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.26>`_, and thus upward compatible with `GT.M V6.3-006 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-006_Release_Notes.html>`_, and `GT.M V6.3-007 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-007_Release_Notes.html>`_.


++++++++++++++++++++++
Platforms
++++++++++++++++++++++

A platform is a combination of a CPU architecture and an operating system. A platform is Supported, Supportable, or Unsupported. Supported means that we have the platform in our development environment and test each release on that platform. Supportable means that although we do not necessarily have such a platform in our environment, we have no reason to believe that the software will not run on it. All others are Unsupported.

+---------------------------------------------------------+----------------------------------------------------+-------------------------------------------------------------------------------------+
| CPU Architecture                                        | Supported OS Version(s)                            | Notes                                                                               |
+=========================================================+====================================================+=====================================================================================+
| 64-bit x86                                              | Ubuntu 18.04 LTS; Red Hat Enterprise Linux 7.6;    | Note that there are separate binary distributions for Ubuntu and Red Hat, owing to  |
|                                                         | Debian GNU/Linux 10 (Buster)                       | differences in library versions of those distributions.                             |
+---------------------------------------------------------+----------------------------------------------------+-------------------------------------------------------------------------------------+
| 64-bit ARM (Raspberry Pi 3 Model B)                     | Ubuntu 18.04 LTS                                   | While YottaDB r1.28 is Supportable on other                                         |
|                                                         |                                                    | `ARMv8-A CPUs <https://en.wikipedia.org/wiki/Comparison_of_ARMv8-A_cores>`_,        |
|                                                         |                                                    | owing to variations in the implementations of ARM microarchitectures, we recommend  |
|                                                         |                                                    | that you ensure the software runs correctly before committing to any specific       |
|                                                         |                                                    | hardware other than those Supported. Please contact info@yottadb.com if you want a  |
|                                                         |                                                    | specific combination of OS and CPU microarchitecture to be Supported.               |
+---------------------------------------------------------+----------------------------------------------------+-------------------------------------------------------------------------------------+
| 32-bit ARM (Raspberry Pi 3 Model B)                     | Raspbian GNU/Linux 10 (Buster)                     | While YottaDB is Supportable on other                                               |
|                                                         |                                                    | `ARMv7-A CPUs <https://en.wikipedia.org/wiki/Comparison_of_ARMv7-A_cores>`_,        |
|                                                         |                                                    | owing to variations in the implementations of ARM microarchitectures, we recommend  |
|                                                         |                                                    | that you ensure the software runs correctly before committing to any specific       |
|                                                         |                                                    | hardware other than those Supported. Please contact info@yottadb.com if you want a  |
|                                                         |                                                    | specific combination of OS and CPU microarchitecture to be Supported.               |
+---------------------------------------------------------+----------------------------------------------------+-------------------------------------------------------------------------------------+
| 32-bit ARM (Raspberry Pi Zero)                          | Raspbian GNU/Linux 10 (Buster)                     | While YottaDB r1.28 is Supportable on other ARMv6 CPUs, owing to variations in the  |
|                                                         |                                                    | implementations of ARM microarchitectures, we recommend that you ensure the software|
|                                                         |                                                    | runs correctly before committing to any specific hardware other than those          |
|                                                         |                                                    | Supported. Please contact info@yottadb.com if you want a specific combination of OS |
|                                                         |                                                    | and CPU microarchitecture to be Supported.                                          |
+---------------------------------------------------------+----------------------------------------------------+-------------------------------------------------------------------------------------+

Recent releases of major GNU/Linux distributions with contemporary kernels, glibc and ncurses are Supportable. Specific notes:

- On Ubuntu releases after 18.04 LTS, YottaDB needs the libtinfo5 package to be installed.
- On `Arch Linux <https://www.archlinux.org/>`_ and possibly other leading edge distributions, YottaDB may need to be recompiled from source code owing to library and tool chain versions significantly more recent than those used in building the distribution.

+++++++++++++++
Getting Started
+++++++++++++++

See our `Get Started <https://yottadb.com/product/get-started/>`_ page to use YottaDB.

We **Strongly recommend** that you install YottaDB r1.28 in a newly created directory, different from those of YottaDB r1.26 and any GT.M versions you may have installed on the system.

+++++++++++++++++++++++++++++++++++++
Removing an installed YottaDB release
+++++++++++++++++++++++++++++++++++++

Assuming $ydb_dist points to the directory where YottaDB is installed:

* Cleanly shut down all application processes using that release.
* Execute mupip rundown && mupip rundown -relinkctl.
* Ensure that there are no gtcm* or gtmsecshr processes active.
* Use sudo lsof | grep $ydb_dist to ensure there are no open files.
* Delete the directory with sudo rm -rf $ydb_dist.

----------------------------
Upgrading to YottaDB r1.28
----------------------------

As YottaDB r1.28 is upward compatible from both YottaDB r1.26 and GT.M V6.3-006, the minimal upgrade steps are:

* Install YottaDB r1.28.
* Install plugins you use.
* Recompile any object code, and recreate shared libraries where appropriate.
* If you are using encryption, compile and install the reference implementation plugin or your customized plugin.
* Cleanly shut down the application and ensure that the database files are shut down using mupip rundown from the prior release.
* Switch journal files with the new YottaDB release.
* Start using the new YottaDB release.

To upgrade from older GT.M releases, first upgrade to GT.M V6.0-000 or later and follow the steps above, or contact your YottaDB support channel for assistance and guidance.

A more sophisticated upgrade technique is:

* Install YottaDB r1.28.
* Create a new replicated instance of your application (on the same system or a different system).
* Assuming the existing instance is A, and the new instance is B, uopgrade B to r1.28 and start replicating from A to B.
* Once B catches up, switchover so that B is in a primary role replicating to A.
* Once you are satisfied with B, remove (or upgrade) A.

------------------------
Change History
------------------------

++++++++++++++
r1.28
++++++++++++++

YottaDB r1.28 includes the following changes from `r1.26 <https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.26>`_.

+-------------------------------------------------------------------------+-------------------+-------------------------------------------------------------------------------------------+
| ID                                                                      | Category          | Summary                                                                                   |
+=========================================================================+===================+===========================================================================================+
| `#469 <https://gitlab.com/YottaDB/DB/YDB/issues/469>`_                  | Language          | $FNUMBER() formatting code "." facilitates number formatting                              |
+-------------------------------------------------------------------------+-------------------+-------------------------------------------------------------------------------------------+
| `#471 <https://gitlab.com/YottaDB/DB/YDB/issues/471>`_                  | Other             | Sourcing ydb_env_set and ydb_env_unset provides more information in the event of an error |
+-------------------------------------------------------------------------+-------------------+-------------------------------------------------------------------------------------------+
| `#472 <https://gitlab.com/YottaDB/DB/YDB/issues/472>`_                  | Language          | ydb_data_s()/ydb_data_st() return YDB_DATA_ERROR in case of error                         |
+-------------------------------------------------------------------------+-------------------+-------------------------------------------------------------------------------------------+
| `#475 <https://gitlab.com/YottaDB/DB/YDB/issues/475>`_                  | Language          | ydb_lock_incr_s(), ydb_lock_decr_s(), and ydb_lock_s() check for too many subscripts      |
+-------------------------------------------------------------------------+-------------------+-------------------------------------------------------------------------------------------+
| `#477 <https://gitlab.com/YottaDB/DB/YDB/issues/477>`_                  | Language          | NEW accepts $TEST as an argument                                                          |
+-------------------------------------------------------------------------+-------------------+-------------------------------------------------------------------------------------------+
| `#480 <https://gitlab.com/YottaDB/DB/YDB/issues/480>`_                  | Language          | Incrementing a variable whose initial value is zero or undefined with a seven or more     |
|                                                                         |                   | digit increment returns the correct string value                                          |
+-------------------------------------------------------------------------+-------------------+-------------------------------------------------------------------------------------------+

++++++++++++
Database
++++++++++++

+++++++++++
Language
+++++++++++

* To facilitate application internationalization, $FNUMBER() accepts the following formatting code (i.e., as the second argument):

  * ".": inserts periods (".") every third position to the left of the decimal within the number and uses a comma (",") as the decimal separator.

  (`#469 <https://gitlab.com/YottaDB/DB/YDB/issues/469>`_)

* When ydb_data_s() or ydb_data_st() get an error, the return value in \*ret_value is set to YDB_DATA_ERROR. Previously the return value was zero which is a valid return value. (`#472 <https://gitlab.com/YottaDB/DB/YDB/issues/472>`_)

* ydb_lock_incr_s(), ydb_lock_decr_s(), and ydb_lock_s() validate that the number of subscripts provided does not exceed the maximum.
  Previously specifying too many subscripts resulted in uninformative assert failures and buffer overflows. (`#475 <https://gitlab.com/YottaDB/DB/YDB/issues/475>`_)

* The NEW command accepts $TEST as an argument. Previously, it did not. YottaDB would like to thank SP.ARM for contributing this enhancement. (`#477 <https://gitlab.com/YottaDB/DB/YDB/issues/477>`_)

* Using $INCREMENT(), ydb_incr_s(), or ydb_incr_st() to increment a variable whose initial value is zero or undefined with an
  increment of seven digits or more returns the correct string value. Previously, while the numeric value of the result was correct,
  the string value was the empty string (""). Depending on how the result was used, it could have been wrong.
  (`#480 <https://gitlab.com/YottaDB/DB/YDB/issues/480>`_)

++++++++++++++++++++++++++++++++
System Administration
++++++++++++++++++++++++++++++++

+++++++++++
Other
+++++++++++

* With the %YDBENV program (invoked while sourcing the ydb_env_set and ydb_env_unset files), the error output includes the
  output of ZSHOW "*" in addition to the single line error message. Previously, it was only the single line error message,
  which provided less information when troubleshooting. (`#471 <https://gitlab.com/YottaDB/DB/YDB/issues/471>`_)

------------------------
More Information
------------------------

-------------------------
Error Messages
-------------------------

------------------------
Tarball Hashes
------------------------

+----------------------------------------------------------------------------------------------+------------------------------------------------+
| sha256sum                                                                                    | File                                           |
+==============================================================================================+================================================+
| 3f1968f882e6f54f89a12b3eb8a4bbf6d769e5fe46c69272fcddc93cb0e102d3                             | yottadb_r128_debian10_x8664_pro.tgz            |
+----------------------------------------------------------------------------------------------+------------------------------------------------+
| 6a44f544dfa3b753bee7f13480699cc5b6f5aa108af06f99d872bd6f50c9f078                             | yottadb_r128_linux_aarch64_pro.tgz             |
+----------------------------------------------------------------------------------------------+------------------------------------------------+
| 4c04380fd35f15a1fc1669e9d0519d770b834f2335db4a3b2250611bf99f43ef                             | yottadb_r128_linux_armv6l_pro.tgz              |
+----------------------------------------------------------------------------------------------+------------------------------------------------+
| 6c8e61c37d63eae071f2e8a2d57f232237dff7f9e19f286c125e4faab7b01243                             | yottadb_r128_linux_armv7l_pro.tgz              |
+----------------------------------------------------------------------------------------------+------------------------------------------------+
| f5df150d7659fc4b050b89cbc2e9f6a01e117ed1b5e1ccb133b9ebb70d9d40d3                             | yottadb_r128_linux_x8664_pro.tgz               |
+----------------------------------------------------------------------------------------------+------------------------------------------------+
| 6200c81f349cf9c4e7ee5120530248bcdffff611621416d57b3cd8f9693c29c2                             | yottadb_r128_rhel7_x8664_pro.tgz               |
+----------------------------------------------------------------------------------------------+------------------------------------------------+

-------------------------
Legal Stuff
-------------------------

Copyright © 2019 YottaDB LLC

Permission is granted to copy, distribute and/or modify this document under the terms of the `GNU Free Documentation License, Version 1.3 <http://www.gnu.org/licenses/fdl.txt>`_ or any later version published by the Free Software Foundation; with no Invariant Sections, no Front-Cover Texts and no Back-Cover Texts.

YottaDB® and Octo® are registered trademarks of YottaDB LLC.
GT.M™ is a trademark of Fidelity National Information Services, Inc.
Other trademarks belong to their respective owners.

This document contains a description of YottaDB and the operating instructions pertaining to the various functions that comprise the software. This document does not contain any commitment of YottaDB LLC. YottaDB LLC believes the information in this publication is accurate as of its publication date; such information is subject to change without notice. YottaDB LLC is not responsible for any errors or defects.
