=================
YottaDB r1.20
=================

.. contents::
   :depth: 2

------------------------------
Release Note Revision History
------------------------------

+-------------------------------+---------------------------------------+----------------------------------------------------------------------+
| Revision                      | Date                                  | Summary                                                              |
+===============================+=======================================+======================================================================+
| 1.00                          |                                       | r1.20 Initial Release                                                |
+-------------------------------+---------------------------------------+----------------------------------------------------------------------+

-----------------------------
Contact Information
-----------------------------

++++++++++++
YottaDB LLC
++++++++++++

40 Lloyd Avenue, Suite 104

Malvern, PA 19355, USA

info@yottadb.com

+1 (610) 644-1898

++++++++++++
Support
++++++++++++

**Customers**

Contact your YottaDB support channel.

**Others**

For free (to you) support from members of communities who run widely available applications on YottaDB, please use an application-specific list where appropriate.

* For issues specific to the use of YottaDB from node.js via `nodem <https://github.com/dlwicksell/nodem>`_, `QewdJS <http://qewdjs.com/>`_ or `Enterprise Web Developer <http://ewdjs.com/>`_, post to the `Enterprise Web Developer community <http://groups.google.com/group/enterprise-web-developer-community>`_.

* For issues specific to the use of YottaDB with `VistA <https://en.wikipedia.org/wiki/VistA>`_ flavors, post to the `Hardhats <http://groups.google.com/group/hardhats>`_ list.

* For issues specific to the use of YottaDB with M other than for applications above, post to the `comp.lang.mumps <http://groups.google.com/group/comp.lang.mumps>`_ list.

* If you are not sure where to post, or for requests other than to the above communities, post an issue at https://github.com/YottaDB/YottaDB/issues and include the words "help wanted" in the summary.

----------------------
r1.20
----------------------

+++++++++++++
Overview
+++++++++++++

YottaDB r1.20 is a major release. The ydbinstall.sh script automatically downloads and installs the current YottaDB release. As always, there as other enhancements and fixes, as noted below.

YottaDB r1.20 is built on (and except as noted, upward compatible with) both `YottaDB r1.10 <https://github.com/YottaDB/YottaDB/releases/tag/r1.10>`_ and `GT.M V6.3-003 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-003_Release_Notes.html>`_.

++++++++++++++++++++++
Platforms
++++++++++++++++++++++

A platform is a combination of a CPU architecture and an operating system. A platform is Supported, Supportable, or Unsupported. Supported means that we have the platform in our development environment and test each release on that platform. Supportable means that although we do not necessarily have such a platform in our environment, we have no reason to believe that the software will not run on it. All others are Unsupported.

+---------------------------------------------------------+----------------------------------------------------+-------------------------------------------------------------------------------------+
| CPU Architecture                                        | Supported OS Version(s)                            | Notes                                                                               |
+=========================================================+====================================================+=====================================================================================+
| 64-bit x86                                              | Ubuntu 16.04 LTS; Red Hat Enterprise Linux 7.4     | Note that there are separate binary distributions for Ubuntu and Red Hat, owing to  |
|                                                         |                                                    | differences in library versions of those distributions.                             |
+---------------------------------------------------------+----------------------------------------------------+-------------------------------------------------------------------------------------+
| Raspberry Pi 3 Model B; BeagleBone Black Wireless       | Raspbian GNU/Linux 9.1; Stretch IoT (non GUI)      | While YottaDB r1.20 is Supportable on other ARMv7-A CPUs, owing to variations in the|
|                                                         |                                                    | implementations of ARM microarchitectures, we recommend that you ensure the software|
|                                                         |                                                    | runs correctly before committing to any specific hardware other than those Supported|
|                                                         |                                                    | Please contact info@yottadb.com if you want a specific combination of OS and CPU    |
|                                                         |                                                    | microarchitecture to be Supported.                                                  |
+---------------------------------------------------------+----------------------------------------------------+-------------------------------------------------------------------------------------+

Recent releases of major GNU/Linux distributions with contemporary kernels, glibc and ncurses are Supportable. Running on Arch Linux requires the ncurses5-compat-libs package to be installed.

+++++++++++++++++++
Installation
+++++++++++++++++++

The simplest way to install the current release of YottaDB for your platform is the `ydbinstall.sh <https://github.com/YottaDB/YottaDB/blob/master/sr_unix/ydbinstall.sh>`_ script.
Download it, make the file executable, and execute it:

* sudo ./ydbinstall.sh downloads and installs the current YottaDB release with a reasonable set of defaults.

* sudo ./ydbinstall.sh --utf8 default downloads and installs the current YottaDB release with UTF-8 support, using the default ICU version on the operating system.

* ./ydbinstall.sh --help prints a list of command line options.

* sudo ./ydbinstall.sh --verbose outputs information as it executes, for troubleshooting purposes (for example if Internet access is stalled by a proxy server, the script will print the wget command it is trying to execute and hang).

You can also download the YottaDB binary distribution tarball for your platform from the `release notes for the latest release <https://github.com/YottaDB/YottaDB/releases/latest>`_, unpack it, change to the directory with the unpacked files, and (a) run the ydbinstall therein or (b) install the historical way per the instructions for installing GT.M in the Installation Procedure section of Chapter 2 (Installing GT.M) in the `GT.M Administration and Operations Guide <http://tinco.pair.com/bhaskar/gtm/doc/books/ao/UNIX_manual/index.html>`_.

We **strongly recommend** that you install YottaDB r1.20 in a newly created directory, different from those of YottaDB r1.10 and any GT.M versions you may have installed on the system.

+++++++++++++++++++++++++++++++++++++
Removing an installed YottaDB release
+++++++++++++++++++++++++++++++++++++

Assuming $gtm_dist points to the directory where YottaDB is installed:

* Cleanly shut down all application processes using that release.
* Execute mupip rundown && mupip rundown -relinkctl.
* Ensure that there are nogtcm* or gtmsecshr processes active.
* Use sudo lsof | grep $gtm_dist to ensure there are no open files.
* Delete the directory with sudo rm -rf $gtm_dist.

----------------------------
Upgrading to YottaDB r1.20
----------------------------

As YottaDB r1.20 is upward compatible from both YottaDB r1.10 and GT.M V6.3-003, the minimal upgrade steps are:

* Install YottaDB r1.10.
* Recompile any object code, and recreate shared libraries where appropriate.
* If you are using encryption, compile and install the reference implementation plugin or your customized plugin.
* Cleanly shut down the application and ensure that the database files are shut down using mupip rundown from the prior release.
* Switch journal files with the new YottaDB release.
* Start using the new YottaDB release.

To upgrade from older GT.M releases, first upgrade to GT.M V6.0-000 or later and follow the steps above, or contact your YottaDB support channel for assistance and guidance.

A more sophisticated upgrade technique is:

* Install YottaDB r1.20.
* Create a new replicated instance of your application (on the same system or a different system).
* Assuming the existing instance is A, and the new instance is B, start replicating from A to B.
* Once B catches up, switchover so that B is in a primary role replicating to A.
* Once you are satisfied with B, remove (or upgrade) A.

------------------------
Change History
------------------------

++++++++
r1.20
++++++++

YottaDB r1.20 includes the following changes from `YottaDB r1.10 <https://github.com/YottaDB/YottaDB/releases/tag/r1.10>`_.

+++++++++++++++
GT.M V6.3-003A
+++++++++++++++

+-------------------------------------------------------------------------------------------------------+--------------------------------------+-------------------------------------------------------------------------------+
| ID                                                                                                    | Category                             | Summary                                                                       |
+=======================================================================================================+======================================+===============================================================================+
| `GTM-8880 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-003_Release_Notes.html#GTM-8880>`_ | Language                             | Fix issue with (non-default) Standard Boolean evaluation with side-effects    |
|                                                                                                       |                                      | and certain patterns                                                          |
+-------------------------------------------------------------------------------------------------------+--------------------------------------+-------------------------------------------------------------------------------+
| `GTM-8887 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-003_Release_Notes.html#GTM-8887>`_ | Other                                | Fix rare timer issue                                                          |
+-------------------------------------------------------------------------------------------------------+--------------------------------------+-------------------------------------------------------------------------------+
| `GTM-8889 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-003_Release_Notes.html#GTM-8889>`_ | Other                                | Prevent UNDEF error after <CTRL-C> within ZHELP navigation                    |
+-------------------------------------------------------------------------------------------------------+--------------------------------------+-------------------------------------------------------------------------------+

++++++++++++++
GT.M V6.3-003
++++++++++++++

+-------------------------------------------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------+
| ID                                                                                                    | Category                              | Summary                                                                       |
+=======================================================================================================+=======================================+===============================================================================+
| `GTM-4212 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-003_Release_Notes.html#GTM-4212>`_ | Admin                                 | MUPIP better deals with over length file names                                |
+-------------------------------------------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------+
| `GTM-6115 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-003_Release_Notes.html#GTM-6115>`_ | Language                              | Please see `GTM-8792                                                          |
|                                                                                                       |                                       | <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-003_Release_Notes.htm|
|                                                                                                       |                                       | l#GTM-8792>`_                                                                 |
+-------------------------------------------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------+
| `GTM-7986 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-003_Release_Notes.html#GTM-7986>`_ | Language                              | Warning on implicit wrapping of source lines exceeding maximum supported      |
|                                                                                                       |                                       | length                                                                        |
+-------------------------------------------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------+
| `GTM-8182 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-003_Release_Notes.html#GTM-8182>`_ | DB                                    | Allow updating globals belonging to different instances                       |
+-------------------------------------------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------+
| `GTM-8186 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-003_Release_Notes.html#GTM-8186>`_ | Language                              | Accept offset alone for an entryref in DO, GOTO and ZGOTO                     |
+-------------------------------------------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------+
| `GTM-8587 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-003_Release_Notes.html#GTM-8587>`_ | Language                              | Maintain $DEVICE and $KEY for all supported devices                           |
+-------------------------------------------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------+
| `GTM-8617 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-003_Release_Notes.html#GTM-8617>`_ | Admin                                 | MUPIP SET supports N[ULL_SUBSCRIPTS] and STD[NULLCOLL] qualifiers.            |
+-------------------------------------------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------+
| `GTM-8680 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-003_Release_Notes.html#GTM-8680>`_ | DB                                    | LOCK Improvements                                                             |
+-------------------------------------------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------+
| `GTM-8732 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-003_Release_Notes.html#GTM-8732>`_ | Admin                                 | Better validation for MUPIP REPLICATE -LOG_INTERVAL and -HELPER, and MUPIP    |
|                                                                                                       |                                       | SET -DEFER_TIME                                                               |
+-------------------------------------------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------+
| `GTM-8735 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-003_Release_Notes.html#GTM-8735>`_ | Admin                                 | READ_ONLY characteristic to prevent state changes to MM databases             |
+-------------------------------------------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------+
| `GTM-8754 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-003_Release_Notes.html#GTM-8754>`_ | Other                                 | Prevent odd ASYNCIO deadlock                                                  |
+-------------------------------------------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------+
| `GTM-8767 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-003_Release_Notes.html#GTM-8767>`_ | Admin                                 | MUPIP SET -HARD_SPIN_COUNT and -SPIN_SLEEP_MASK support                       |
+-------------------------------------------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------+
| `GTM-8769 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-003_Release_Notes.html#GTM-8769>`_ | Language                              | Syntax check $ETRAP, $ZSTEP, $ZTRAP, and EXCEPTION when specified             |
+-------------------------------------------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------+
| `GTM-8779 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-003_Release_Notes.html#GTM-8779>`_ | Admin                                 | Freeze Notification                                                           |
+-------------------------------------------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------+
| `GTM-8780 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-003_Release_Notes.html#GTM-8780>`_ | Language                              | Fix $SELECT() handling of certain syntax errors                               |
+-------------------------------------------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------+
| `GTM-8781 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-003_Release_Notes.html#GTM-8781>`_ | Other                                 | Prevent memory leak in ZSYSTEM                                                |
+-------------------------------------------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------+
| `GTM-8786 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-003_Release_Notes.html#GTM-8786>`_ | Language                              | $NAME() of a naked reference returns any current extended reference           |
+-------------------------------------------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------+
| `GTM-8787 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-003_Release_Notes.html#GTM-8787>`_ | Admin                                 | MUPIP JOURNAL -EXTRACT='-stdout' doesn't explode at termination if stdout is  |
|                                                                                                       |                                       | gone                                                                          |
+-------------------------------------------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------+
| `GTM-8788 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-003_Release_Notes.html#GTM-8788>`_ | Language                              | The compiler excludes BLKTODEEP lines from the object files                   |
+-------------------------------------------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------+
| `GTM-8789 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-003_Release_Notes.html#GTM-8789>`_ | Language                              | Prevent NEW $ZGBLDIR from setting up an Update Process failure                |
+-------------------------------------------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------+
| `GTM-8790 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-003_Release_Notes.html#GTM-8790>`_ | DB                                    | Retain any extended first reference in $REFERENCE when sharing statistics     |
+-------------------------------------------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------+
| `GTM-8792 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-003_Release_Notes.html#GTM-8792>`_ | Language                              | Prevent keys that exceed the supported maximum string length                  |
+-------------------------------------------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------+
| `GTM-8794 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-003_Release_Notes.html#GTM-8794>`_ | Admin                                 | MUPIP RUNDOWN -OVERRIDE works on a non-MUPIP backup made during an Instance   |
|                                                                                                       |                                       | Freeze                                                                        |
+-------------------------------------------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------+
| `GTM-8795 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-003_Release_Notes.html#GTM-8795>`_ | DB                                    | Journal Updates promptly during MUPIP FREEZE -ONLINE                          |
+-------------------------------------------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------+
| `GTM-9796 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-003_Release_Notes.html#GTM-8796>`_ | DB                                    | Improved error handling during TP and mini transaction commits                |
+-------------------------------------------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------+
| `GTM-8797 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-003_Release_Notes.html#GTM-8797>`_ | Admin                                 | Installation script fixes                                                     |
+-------------------------------------------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------+
| `GTM-8798 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-003_Release_Notes.html#GTM-8798>`_ | Admin                                 | MUPIP ENDIANCVT converts Mutex Queue Slots                                    |
+-------------------------------------------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------+
| `GTM-8799 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-003_Release_Notes.html#GTM-8799>`_ | Other                                 | Improve performance for a pattern of local variable creation                  |
+-------------------------------------------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------+
| `GTM-8801 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-003_Release_Notes.html#GTM-8801>`_ | Other                                 | cmake build produces appropriate support for the ^%YGBLSTATS utility.         |
+-------------------------------------------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------+
| `GTM-8804 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-003_Release_Notes.html#GTM-8804>`_ | Language                              | ZSHOW "T" option to return summary for ZSHOW "GL"                             |
+-------------------------------------------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------+
| `GTM-8805 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-003_Release_Notes.html#GTM-8805>`_ | DB                                    | Fix to havesting of LOCKs abandoned by an abnormally terminated process       |
+-------------------------------------------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------+
| `GTM-8832 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-003_Release_Notes.html#GTM-8832>`_ | Language                              | Appropriately report NUMOFLOW for string literal with a huge value when used  |
|                                                                                                       |                                       | as a number                                                                   |
+-------------------------------------------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------+
| `GTM-8839 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-003_Release_Notes.html#GTM-8839>`_ | Language                              | $DEVICE shows the full error message                                          |
+-------------------------------------------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------+
| `GTM-8840 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-003_Release_Notes.html#GTM-8840>`_ | Admin                                 | Normalized gtmsecshr message severities                                       |
+-------------------------------------------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------+
| `GTM-8842 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-003_Release_Notes.html#GTM-8842>`_ | Admin                                 | ZBREAK and ZSTEP restricted in triggers when TRIGGER_MOD is restricted        |
+-------------------------------------------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------+
| `GTM-8844 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-003_Release_Notes.html#GTM-8844>`_ | Admin                                 | Restriction available for HALT and ZHALT; ZGOTO 0 can return a non-zero status|
|                                                                                                       |                                       | to the shell                                                                  |
+-------------------------------------------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------+
| `GTM-8846 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-003_Release_Notes.html#GTM-8846>`_ | Admin                                 | GT.M accepts multi-slash journal file names                                   |
+-------------------------------------------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------+
| `GTM-8847 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-003_Release_Notes.html#GTM-8847>`_ | Language                              | Provide a way to detect and limit process private heap storage                |
+-------------------------------------------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------+
| `GTM-8849 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-003_Release_Notes.html#GTM-8849>`_ | Other                                 | Help databases built with make files have QDBRUNDOWN and NOGVSTATS            |
|                                                                                                       |                                       | characteristics                                                               |
+-------------------------------------------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------+
| `GTM-8850 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-003_Release_Notes.html#GTM-8850>`_ | DB                                    | Allow process exit when MUPIP FREEZE -ONLINE is in place                      |
+-------------------------------------------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------+
| `GTM-8854 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-003_Release_Notes.html#GTM-8854>`_ | Language                              | Compiler handles a syntax error after a literal postconditional that's FALSE  |
+-------------------------------------------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------+
| `GTM-8855 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-003_Release_Notes.html#GTM-8855>`_ | Other                                 | Prevent memory leak from an error locating a global directory                 |
+-------------------------------------------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------+
| `GTM-8856 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-003_Release_Notes.html#GTM-8856>`_ | Language                              | Defer failing evaluations of literal pattern matches to run time              |
+-------------------------------------------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------+
| `GTM-8857 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-003_Release_Notes.html#GTM-8857>`_ | Language                              | Improve error detection for certain pattern match cases                       |
+-------------------------------------------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------+
| `GTM-8858 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-003_Release_Notes.html#GTM-8858>`_ | DB                                    | Improve available information in cases of apparent database integrity issues  |
+-------------------------------------------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------+
| `GTM-8859 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-003_Release_Notes.html#GTM-8859>`_ | Admin                                 | MUPIP ROLLBACK handles idle regions better                                    |
+-------------------------------------------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------+
| `GTM-8866 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-003_Release_Notes.html#GTM-8866>`_ | Language                              | Prevent timeouts with more than three decimal digits from being too long      |
+-------------------------------------------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------+
| `GTM-8873 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-003_Release_Notes.html#GTM-8873>`_ | DB                                    | Prevent occasional $ORDER(,-1) problem                                        |
+-------------------------------------------------------------------------------------------------------+---------------------------------------+-------------------------------------------------------------------------------+

-------------------------
More Information
-------------------------

(Section blank for this release)

----------------------
Messages
----------------------



+++++++++++++++
From GT.M
+++++++++++++++

**DBFREEZEOFF**, Region rrrr is UNFROZEN ([NO]OVERRIDE [NO]AUTOREL)

Operator log/MUPIP Information: The database region rrrr is no longer frozen, most likely due to a MUPIP FREEZE -OFF, with the selected options. [NO]AUTOREL indicates whether an autorelease of the region occurred prior to the MUPIP FREEZE -OFF command.

Action: Confirm that this was the desired action.

**DBFREEZEON**, Region rrrr is FROZEN ([NO]OVERRIDE [NO]ONLINE [NO]AUTOREL)

Operator log/MUPIP Information: The database region rrrr is frozen, most likely due to a MUPIP FREEZE -ON, with the reported options.

Action: Confirm that this was the desired action.

**DBNONUMSUBS**, XXXX Key contains a numeric form of subscript in a global defined to collate all subscripts as strings

Run Time/MUPIP Error: The record has a numeric subscript but the collation setting for the global or region indicates all subscripts are filed as strings. The leading context (XXXX) identifies the block and offest of the problematic record. This can arise if an operator uses DSE to force a change to a collation setting or to modify a key when the global already has content.

Action: If you can determine the cause of, and reason for, the change and you may choose to reverse it. If you need to change the collation, the appropriate procedure is to EXTRACT the data, KILL the global, or remove and recreate the database file, and them LOAD the extracted data.

**DBNULCOL**, XXXX NULL collation representation differs from the database file header setting

DSE/MUPIP/Run Time Error: This indicates the database contains a record with an empty subscript ("Null" subscript) representation that is incompatible with the current setting database file header setting for such a representation. The leading context (XXXX) specifies the block number and offset of the problematic record. This can only arise if someone changes the setting for the database while it contains one or more such subscripts. YottaDB recommends against making such a change. This message can originate from MUPIP INTEG, DSE INTEG or from running with VIEW "GDSCERT"

Action: Use the record and block information to remove the problematic record with DSE and restore the data appropriately, typically with a SET command. Note that the record and block of the record many change due to ongoing updates, so this operation requires great care and familiarity with DSE.

**GBLOFLOW**, Database segment is full

Run Time/MUPIP Error: This indicates that an error was encountered while extending the database file.

Action: Examine the accompanying message(s) for the cause of the error. If the error is due to insufficient authorization, address that. If the error is due to TOTALBLKMAX (refer to the explaination of that message) or a lack of enough free space on the disk to fit the size of a database file, try performing a KILL of some nodes in the database to get free blocks in the existing allocated space (you may need to KILL several subscripted nodes before you can KILL a name node).

**LSINSERTED**, Line YYYY, source module XXXX exceeds maximum source line length; line seperator inserted, terminating scope of any prior IF, ELSE, or FOR.

Compile Time Warning: Indicates that source XXXX line YYYY exceeded the maximum line length and YottaDB separated it into multiple lines to allow continued parsing. Internally, YottaDB represents the generated code as N lines for this source line, where N is the number of segments extracted from this source line. Be aware that as a result of this, source lines containing a command whose scope is rest of the line (IF, ELSE, FOR), are now split into multiple lines, each with a separate scope.

Action: Consider refactoring code to avoid source line lengths in excess of 8192 characters.

**MUTEXFRCDTERM**, Mutual Exclusion subsystem detected forced termination of process pppp. Crit salvaged from database file dddd.

Run Time Warning: This indicates that YottaDB confirmed inappropriate termination of the process pppp, while holding crit on database file dddd.

Action: Determine the cause of the termination and take appropriate action.

**NULSUBSC**, XXXX Null subscripts are not allowed for current region.

Run Time/MUPIP Error: This indicates that a global variable specified a null subscript in a database file which does not accept null subscripts. The leading context (XXXX) specifies more about the event or location of the issue.

Action: Look for the source of the null subscript(s) and consider whether they are appropriate or due to a coding error. If they are appropriate, use MUPIP SET -NULL_SUBSCRIPTS, and remember to make the same adjustment with GDE CHANGE REGION -NULL_SUBSCRIPTS to ensure the next time you recreate a database that the characteristic persists.

**READONLYNOBG**, Read-only cannot be enabled on non-MM databases

MUPIP Error: This indicates an attempt to change a BG database to -READ_ONLY or to change a -READ_ONLY to MM access method; -READ_ONLY only compatible with the MM access mode.

Action: Verify whether the database should not be read only and adjust, if appropriate. Alternatively, set the database to MM access mode then mark it as read-only.

**REPLINSTACC**, Error accessing replication instance file xxxx

Run Time/MUPIP Error: This indicates that some errors were encountered while accessing the specified replication instance file defined by $gtm_repl_instance or the relevant global directory.

Action: Refer to the accompanying message(s) for additional information.

**REPLINSTMISMTCH**, Process has replication instance file ffff (jnlpool shmid = ssss) open but database dddd is bound to instance file gggg (jnlpool shmid =tttt)

Run Time Error: The process attempted an update on the replicated database dddd associated with the replication instance file ffff and journal pool shared memory id ssss; however, the process has already associated the database with a different replication instance file gggg or journal pool shmid tttt.

Action: A replicated database can only accept updates by processes that have the same replication instance file (defined by the environment variable gtm_repl_instance or in the global directory) open for that database. Ensure the same replication instance file is used for all processes that update the same replicated database file. This error can also occur if the replication instance file was recreated (while processes were still accessing the replication instance). In this case, the name ffff and gggg would be the same but the corresponding journal pool shared memory ids would be different. To recover from this situation, shut down all processes accessing the instance from before and after the instance file recreate. Run an argumentless MUPIP RUNDOWN to clean up the older journal pool tttt and restart the instance. The Source Server (which is the first process to start on a replicated instance) only binds replicated databases from its global directory to the journal pool that it creates. No other replicated database file can be bound with this journal pool.

**REPLMULTINSTUPDATE**, Previous updates in the current transaction are to xxxx so updates to yyyy (in rrrr) not allowed

Run Time Error: Previous updates in the current TP transaction mapped to database files associated with replication instance file xxxx, so it cannot make updates to database file yyyy which is associated with replication instance file rrrr.

Action: Modify the application so all updates in a TP transaction to replicated regions are associated with a single replication instance.

**STACKCRIT**, Stack space critical

Run Time Error: This indicates that the process has consumed almost all of the available stack space.

Action: Look for infinite recursion. If you do not take immediate action to reduce your stack, YottaDB is likely to produce a STACKOFLOW error, which terminates the process. Examine the stack with ZSHOW. Trim the stack using QUIT, ZGOTO, HALT or ZHALT.

**STACKOFLOW**, Stack overflow

Run Time Fatal: This indicates that the process required more stack space than was available in memory.

Action: Reduce the stack when you get a STACKCRIT error. This error terminates the process.

**STPCRIT**, String pool space critical

Run Time Error: This indicates that the process has exceeded the heap (string pool) limit specified in the $ZSTRPLLIM ISV. If you do not take prompt action to reduce the process memory requirements, at the next heap expansion, YottaDB produces an STPOFLOW error, which terminates the process.

Action: Investigate whether the process memory usage is appropriate, and if so, increase or remove the limit. Otherwise correct the cause(s) of the excessive memory consumption.

**STPOFLOW**, String pool space overflow

Run Time Fatal: This indicates that the process has previously exceeded the heap (string pool) limit specified in the $ZSTRPLLIM ISV and still needs more memory, so YottaDB terminates the process.

Action: Investigate whether the process memory usage is appropriate, and if so, increase or remove the limit. Otherwise correct the cause(s) of the excessive memory consumption.

----------------------------
Tarball hashes
----------------------------


-----------------------
Legal Stuff
-----------------------

Copyright © 2017 YottaDB LLC

Permission is granted to copy, distribute and/or modify this document under the terms of the `GNU Free Documentation License, Version 1.3 <http://www.gnu.org/licenses/fdl.txt>`_ or any later version published by the Free Software Foundation; with no Invariant Sections, no Front-Cover Texts and no Back-Cover Texts.

YottaDB™ is a trademark of YottaDB LLC.
GT.M™ is a trademark of Fidelity National Information Services, Inc.
Other trademarks belong to their respective owners.

This document contains a description of YottaDB and the operating instructions pertaining to the various functions that comprise the software. This document does not contain any commitment of YottaDB LLC. YottaDB LLC believes the information in this publication is accurate as of its publication date; such information is subject to change without notice. YottaDB LLC is not responsible for any errors or defects.


----------------------------
Downloads
----------------------------


