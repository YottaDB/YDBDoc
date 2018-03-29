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

YottaDB r1.20 is the most significant release to date from the YottaDB team. With the ability to call the data management engine directly from C, and eliminating the need for any application code to be written in M (by separating the M language from the database without compromising either in any way), YottaDB r1.20 represents a historic milestone for the YottaDB/GT.M code base.

YottaDB r1.20 is built on (and except where explicitly noted, upward compatible with) both `YottaDB r1.10 <https://github.com/YottaDB/YottaDB/releases/tag/r1.10>`_ and `GT.M V6.3-003A <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-003_Release_Notes.html>`_. Highlights of enhancements and fixes made by the YottaDB team include:

* The C API mentioned above to call the database management engine directly (link to Issue 59 in the release notes): As C is the *lingua franca* of computer languages, this makes the engine accessible from other languages that can call C APIs. In the future, we anticipate creating standard wrappers to the engine from other languages, and we invite members of the community to do so as well.

* An all-new manual, the `Multi-Language Programmers Guide to access the C API <https://docs.yottadb.com/MultiLangProgGuide/>`_.

* At US $5 in retail quantities, a new Supported platform, the `Raspberry Pi Zero <https://www.raspberrypi.org/products/raspberry-pi-zero/>`_ is the lowest cost platform ever for the code base. This and the halving of the storage footprint of the database engine itself (14-15MB) (link to Issue 33 in release notes), makes YottaDB attractive for Internet of Things applications. Our blog post `Edge Computing and YottaDB Everywhere <https://yottadb.com/edge-computing-and-yottadb-everywhere/>`_ touches on the benefits of YottaDB on low cost computing platforms.

* `Docker containers <https://www.docker.com/what-container>`_ make it easy to get up and running with YottaDB for experimentation, DevOps and Microservices.

* Performance improvements (up to two orders of magnitude in specific test cases) in large local arrays (link to Issue 80 in release notes) and garbage collection in applications with large numbers of strings that change often (link to Issue 85 in release notes).

* The READ command supports a buffer of the same number of prior inputs that direct mode does. (link to Issue 83 in release notes)

Highlights from the upstream  V6.3-003A code base, thanks to the GT.M team include:

* Performance improvements when there are a large number of concurrent locks (link to GTM-8680 in release notes).

* Introduced as field test grade functionality in a production release, the ability for a process to associated itself with different instances at run-time (link to GTM-8182 in release notes).

As is the case in every release, YottaDB r1.20 brings other enhancements, e.g., callin when an applicaiton is already inside a transaction processing fence (link to Issue 188 in release notes), and sending syslog messages to stderr when there is no syslog (link to Issue 189 in release notes), as well as fixes to issues as detailed below.

As always, there as other enhancements and fixes, as noted below. See our `Get Started <https://yottadb.com/product/get-started/>`_ page to use YottaDB.


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
| Raspberry Pi Zero                                       | Raspbian GNU/Linux 9.1                             | While YottaDB r1.20 is Supportable on other ARMv6 CPUs, owing to variations in the  |
|                                                         |                                                    | implementations of ARM microarchitectures, we recommend that you ensure the software|
|                                                         |                                                    | runs correctly before committing to any specific hardware other than those Supported|
|                                                         |                                                    | Please contact info@yottadb.com if you want a specific combination of OS and CPU    |
|                                                         |                                                    | microarchitecture to be Supported.                                                  |
+---------------------------------------------------------+----------------------------------------------------+-------------------------------------------------------------------------------------+

Recent releases of major GNU/Linux distributions with contemporary kernels, glibc and ncurses are Supportable. Running on Arch Linux requires the ncurses5-compat-libs package to be installed.

+++++++++++++++
Getting Started
+++++++++++++++

See our `Get Started <https://yottadb.com/product/get-started/>`_ page to use YottaDB.

We **strongly recommend** that you install YottaDB r1.20 in a newly created directory, different from those of YottaDB r1.10 and any GT.M versions you may have installed on the system.

+++++++++++++++++++++++++++++++++++++
Removing an installed YottaDB release
+++++++++++++++++++++++++++++++++++++

Assuming $ydb_dist points to the directory where YottaDB is installed:

* Cleanly shut down all application processes using that release.
* Execute mupip rundown && mupip rundown -relinkctl.
* Ensure that there are nogtcm* or gtmsecshr processes active.
* Use sudo lsof | grep $ydb_dist to ensure there are no open files.
* Delete the directory with sudo rm -rf $ydb_dist.

----------------------------
Upgrading to YottaDB r1.20
----------------------------

As YottaDB r1.20 is upward compatible from both YottaDB r1.10 and GT.M V6.3-003/-003A, the minimal upgrade steps are:

* Install YottaDB r1.20.
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

+-------------------------------------------------------------------------------------------------------+-------------------------------------+--------------------------------------------------------------------------------+
| ID                                                                                                    | Category                            | Summary                                                                        |
+=======================================================================================================+=====================================+================================================================================+
| `#23 <https://github.com/YottaDB/YottaDB/issues/23>`_                                                 | Admin                               | Change references to GT.M into references to YottaDB - shared libraries        |
+-------------------------------------------------------------------------------------------------------+-------------------------------------+--------------------------------------------------------------------------------+
| `#33 <https://github.com/YottaDB/YottaDB/issues/33>`_                                                 | Other                               | Reduce footprint of engine                                                     |
+-------------------------------------------------------------------------------------------------------+-------------------------------------+--------------------------------------------------------------------------------+
| `#59 <https://github.com/YottaDB/YottaDB/issues/59>`_                                                 | Data                                | Directly access YottaDB data from C                                            |
+-------------------------------------------------------------------------------------------------------+-------------------------------------+--------------------------------------------------------------------------------+
| `#77 <https://github.com/YottaDB/YottaDB/issues/77>`_                                                 | Other                               | Fix reporting glitch in Indirection cache hit ratio when cache hits are greater|
|                                                                                                       |                                     | than 43 million                                                                |
+-------------------------------------------------------------------------------------------------------+-------------------------------------+--------------------------------------------------------------------------------+
| `#80 <https://github.com/YottaDB/YottaDB/issues/80>`_                                                 | Data                                | Improve performance of large local arrays                                      |
+-------------------------------------------------------------------------------------------------------+-------------------------------------+--------------------------------------------------------------------------------+
| `#83 <https://github.com/YottaDB/YottaDB/issues/83>`_                                                 | Language                            | Recall history for READ command to match direct mode                           |
+-------------------------------------------------------------------------------------------------------+-------------------------------------+--------------------------------------------------------------------------------+
| `#85 <https://github.com/YottaDB/YottaDB/issues/85>`_                                                 | Data                                | Stringpool garbage collector performance enhancements                          |
+-------------------------------------------------------------------------------------------------------+-------------------------------------+--------------------------------------------------------------------------------+
| `#90 <https://github.com/YottaDB/YottaDB/issues/90>`_                                                 | Language                            | YottaDB correctly runs M programs which had PATNOTFOUND errors at compile time |
+-------------------------------------------------------------------------------------------------------+-------------------------------------+--------------------------------------------------------------------------------+
| `#94 <https://github.com/YottaDB/YottaDB/issues/94>`_                                                 | Admin                               | Ignore gtm_dist environment variable                                           |
+-------------------------------------------------------------------------------------------------------+-------------------------------------+--------------------------------------------------------------------------------+
| `#97 <https://github.com/YottaDB/YottaDB/issues/97>`_                                                 | Admin                               | Customize YottaDB message prefix                                               |
+-------------------------------------------------------------------------------------------------------+-------------------------------------+--------------------------------------------------------------------------------+
| `#99 <https://github.com/YottaDB/YottaDB/issues/99>`_                                                 | Other                               | ydbinstall.sh correctly reports "YottaDB" or "GT.M" on successful installation |
+-------------------------------------------------------------------------------------------------------+-------------------------------------+--------------------------------------------------------------------------------+
| `#100 <https://github.com/YottaDB/YottaDB/issues/100>`_                                               | Language                            | Timeouts specifying sub-millisecond resolutions time out correctly             |
+-------------------------------------------------------------------------------------------------------+-------------------------------------+--------------------------------------------------------------------------------+
| `#109 <https://github.com/YottaDB/YottaDB/issues/109>`_                                               | Admin                               | ydb_repl_filter_timeout environment variable to control replication filter     |
|                                                                                                       |                                     | timeout                                                                        |
+-------------------------------------------------------------------------------------------------------+-------------------------------------+--------------------------------------------------------------------------------+
| `#115 <https://github.com/YottaDB/YottaDB/issues/115>`_                                               | Admin                               | Environment variables ydb_dbglvl, ydb_gbldir, ydb_maxtptime are initialized    |
+-------------------------------------------------------------------------------------------------------+-------------------------------------+--------------------------------------------------------------------------------+
| `#118 <https://github.com/YottaDB/YottaDB/issues/118>`_                                               | Admin                               | MUPIP SET JOURNAL issues JNLCRESTATUS error when unable to rename current      |
|                                                                                                       |                                     | journal file                                                                   |
+-------------------------------------------------------------------------------------------------------+-------------------------------------+--------------------------------------------------------------------------------+
| `#120 <https://github.com/YottaDB/YottaDB/issues/120>`_                                               | Language                            | $ZEOF set correctly for files in /proc filesystem                              |
+-------------------------------------------------------------------------------------------------------+-------------------------------------+--------------------------------------------------------------------------------+
| `#122 <https://github.com/YottaDB/YottaDB/issues/122>`_                                               | Admin                               | Simpler MUPIP JOURNAL ROLLBACK recovery following abnormal termination of      |
|                                                                                                       |                                     | process inside transaction commit logic                                        |
+-------------------------------------------------------------------------------------------------------+-------------------------------------+--------------------------------------------------------------------------------+
| `#126 <https://github.com/YottaDB/YottaDB/issues/126>`_                                               | Other                               | ydb script to run YottaDB, and files ydb_env_set & ydb_env_unset to source to  |
|                                                                                                       |                                     | set & clear environment                                                        |
+-------------------------------------------------------------------------------------------------------+-------------------------------------+--------------------------------------------------------------------------------+
| `#136 <https://github.com/YottaDB/YottaDB/issues/136>`_                                               | Admin                               | Establish replication connections more efficiently in an edge case             |
+-------------------------------------------------------------------------------------------------------+-------------------------------------+--------------------------------------------------------------------------------+
| `#141 <https://github.com/YottaDB/YottaDB/issues/141>`_                                               | Language                            | ZSTEP OVER and ZSTEP OUTOF work correctly across extrinsic function returns    |
|                                                                                                       |                                     | using QUIT @ syntax                                                            |
+-------------------------------------------------------------------------------------------------------+-------------------------------------+--------------------------------------------------------------------------------+
| `#143 <https://github.com/YottaDB/YottaDB/issues/143>`_                                               | Other                               | libyottadbutil.so created when YottaDB for Linux/ARM is installed with UTF8    |
|                                                                                                       |                                     | support                                                                        |
+-------------------------------------------------------------------------------------------------------+-------------------------------------+--------------------------------------------------------------------------------+
| `#145 <https://github.com/YottaDB/YottaDB/issues/145>`_                                               | Language                            | DIVZERO error correctly issued in certain edge cases of dividing by zero       |
+-------------------------------------------------------------------------------------------------------+-------------------------------------+--------------------------------------------------------------------------------+
| `#150 <https://github.com/YottaDB/YottaDB/issues/150>`_                                               | Admin                               | Fixes to multiple issues affecting databases with READ_ONLY set                |
+-------------------------------------------------------------------------------------------------------+-------------------------------------+--------------------------------------------------------------------------------+
| `#160 <https://github.com/YottaDB/YottaDB/pull/160>`_                                                 | Other                               | Use ydb and ydb_env_set scripts instead of gtm and gtmprofile scripts, and     |
|                                                                                                       |                                     | ydb_prefix environment variables                                               |
+-------------------------------------------------------------------------------------------------------+-------------------------------------+--------------------------------------------------------------------------------+
| `#161 <https://github.com/YottaDB/YottaDB/pull/161>`_                                                 | Other                               | Change relative path to ydbmerrors to use src directory                        |
+-------------------------------------------------------------------------------------------------------+-------------------------------------+--------------------------------------------------------------------------------+
| `#162 <https://github.com/YottaDB/YottaDB/pull/162>`_                                                 | Other                               | Create Docker Images                                                           |
+-------------------------------------------------------------------------------------------------------+-------------------------------------+--------------------------------------------------------------------------------+
| `#169 <https://github.com/YottaDB/YottaDB/issues/169>`_                                               | Data                                | Certain edge cases of nested triggers work correctly without abnormal process  |
|                                                                                                       |                                     | termination                                                                    |
+-------------------------------------------------------------------------------------------------------+-------------------------------------+--------------------------------------------------------------------------------+
| `#184 <https://github.com/YottaDB/YottaDB/issues/184>`_                                               | Language                            | C program that invokes call-in leaves terminal characteristics in sane state   |
|                                                                                                       |                                     | on exit                                                                        |
+-------------------------------------------------------------------------------------------------------+-------------------------------------+--------------------------------------------------------------------------------+
| `#186 <https://github.com/YottaDB/YottaDB/issues/186>`_                                               | Language                            | EXCEPTION handler correctly executes when Ctrl-C entered                       |
+-------------------------------------------------------------------------------------------------------+-------------------------------------+--------------------------------------------------------------------------------+
| `#188 <https://github.com/YottaDB/YottaDB/issues/188>`_                                               | Language                            | Call-ins permitted inside TP                                                   |
+-------------------------------------------------------------------------------------------------------+-------------------------------------+--------------------------------------------------------------------------------+
| `#189 <https://github.com/YottaDB/YottaDB/issues/189>`_                                               | Admin                               | Send messages to stderr if syslog not present or not functional                |
+-------------------------------------------------------------------------------------------------------+-------------------------------------+--------------------------------------------------------------------------------+

~~~~~~~~~~
Admin
~~~~~~~~~~

* The file libyottadb.so contains the runtime logic that was previously in libgtmshr.so, which is now a relative symbolic link to libyottadb.so. Similarly, libyottadbutil.so contains the object code for utility routines, and libgtmutil.so is a relative symbolic link to libyottadbutil.so. If UTF-8 support is installed, a similar change also occurs in the utf8 subdirectory. There should be no change to the behavior of any application program or scripting that does not explicitly check the nature of libgtmshr.so and libgtmutil.so. [`#23 <https://github.com/YottaDB/YottaDB/issues/23>`_]
* YottaDB ignores the environment variable gtm_dist, deriving any needed information from within the running process. Previously, it required $gtm_dist to contain the name of the directory from which it ran. [`#94 <https://github.com/YottaDB/YottaDB/issues/94>`_]
* The environment variable ydb_msgprefix specifies a prefix for YottaDB messages generated by a process, with the prefix defaulting to "YDB", e.g., YDB-I-DBFILEXT. Previously, the prefix was always "GTM". A value of "GTM" retains the previous format. [`#97 <https://github.com/YottaDB/YottaDB/issues/97>`_]
* An integer value in seconds for the environment variable ydb_repl_filter_timeout sets a limit for the Source Server to await a response from an external filter program. The default value is 64 seconds; a value less than 32 is treated as 32 seconds; and a value greater than 131072 (2**17) is treated as 131,072 seconds. Set a value for ydb_repl_filter_timeout if, for example, your filter program is functionally correct, but needs more time to respond. Previously, the value was always 64 seconds. [`#109 <https://github.com/YottaDB/YottaDB/issues/109>`_]
* At process startup, YottaDB initializes the following intrinsic special variables: (1) $zgbldir to the global directory file pointed to by $ydb_gbldir. If ydb_gbldir is not defined, YottaDB uses the gtmgbldir environment variable instead. (2) $zmaxtptime to the number of seconds specified by $ydb_maxtptime. If ydb_maxtptime is not defined, YottaDB uses the gtm_zmaxtptime environment variable instead, defaulting to 0 seconds (infinite timeout).To facilitate debugging application memory allocation bugs, the environment variable ydb_dbglvl optionally provides debugging flags as specified in the file gtmdbglvl.h. If ydb_dbglvl is not defined, YottaDB uses the gtmdbglvl environment variable instead. If neither is defined, no memory allocation debugging is turned on. Previously YottaDB ignored the ydb_dbglvl, ydb_gbldir, and ydb_maxtptime environment variables. [`#115 <https://github.com/YottaDB/YottaDB/issues/115>`_]
* MUPIP SET JOURNAL issues a JNLCRESTATUS error in case it is not able to rename the current journal file (for example, because of read-only permissions on the directory containing the journal file) before creating the new journal file. In YottaDB r1.10, the MUPIP command used to abnormally terminate with a SIG-11. <`#118 <https://github.com/YottaDB/YottaDB/issues/118>`_]
* MUPIP JOURNAL ROLLBACK works correctly in case a process updating multiple regions in a TP transaction terminates abnormally (e.g., kill -9). In YottaDB r1.10 (and GT.M V6.3-002), it was possible for the rollback to fail with a DUPTOKEN error in rare cases, depending on where in the transaction commit logic the process was killed. The workaround was to rerun the MUPIP JOURNAL ROLLBACK with a non-zero ERROR_LIMIT qualifier value to allow DUPTOKEN errors. In addition, NULL records are placed in the correct file (journal extract file, broken transaction file, lost transaction file). Previously, they could be incorrectly placed in the lost transaction file instead of the broken transaction file. Finally, the NULL records are extracted in the correct order (journal sequence number) in the extract file (or broken transaction or lost transaction file). Previously, the extract file would be sorted in terms of journal sequence number except for the NULL type of journal records (00 record type in the first column of the extract file) which could be placed in arbitrary order. Note that such a transaction remains Atomic, because the process termination means the transaction was never committed. Note also that YottaDB recommends terminating processes with MUPIP STOP and not kill -9. [`#122 <https://github.com/YottaDB/YottaDB/issues/122>`_]
* Initiating replication connections between Source and Receiver Servers is more efficient. Previously, in rare cases, the Source Server unnecessarily disconnect the connection and reconnected. [`#136 <https://github.com/YottaDB/YottaDB/issues/136>`_]
* Multiple issues with READ_ONLY features from `GTM-8735 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-003_Release_Notes.html#GTM-8735>`_ in the upstream code base that generated errors while accessing the help database were addressed and corrected. More details: [`#150 <https://github.com/YottaDB/YottaDB/issues/150>`_]
* In environments without a syslog facility (such as default Docker containers), messages intended for the syslog go to stderr of the YottaDB process. Previously, they appeared on the user's interactive session. YottaDB uses the existence of /dev/log to decide whether a syslog faclity exists. As syslog messages record important information about the operation of the YottaDB engine, we strongly recommend a syslog facility in all cases except single-user development environments. [`#189 <https://github.com/YottaDB/YottaDB/issues/189>`_]

~~~~~~~~~~~~~
Data
~~~~~~~~~~~~~

* The YottaDB engine is directly accessible using a C-callable API. See https://docs.yottadb.com/MultiLangProgGuide/index.html for user documentation. [`#59 <https://github.com/YottaDB/YottaDB/issues/59>`_]
* Local arrays with large number of subscripts scale much better. When the number of nodes in a local array is in the millions, node creation time is now noticeably faster [`#80 <https://github.com/YottaDB/YottaDB/issues/80>`_]
* Applications with large numbers of strings, which can occur with large numbers of local variables, local variables with many nodes, or both, and whose performance is limited by garbage collection, i.e. applications where the strings change frequently, run much faster. In one test case, the improvement was two orders of magnitude. [`#85 <https://github.com/YottaDB/YottaDB/issues/85>`_]
* Nested database triggers (i.e. database triggers that invoke code to update global variables, which in turn invoke other triggers) work correctly. Previously, it was possible in rare cases involving multiple processes loading triggers at the same time for the processes to abnormally terminate with fatal SIGABRT (SIG-6) errors. [`#169 <https://github.com/YottaDB/YottaDB/issues/169>`_]

~~~~~~~~~~~~
Language
~~~~~~~~~~~~

* With the [NO]EDITING deviceparameter set to EDITING, the number of previous inputs to the READ command from a terminal device that can be recalled is a circular buffer of 99 entries, matching the size of the RECALL command buffer for direct mode. READ X#, READ \*X, and READ X all share the same history. Empty inputs are excluded from the buffer, and multiple consecutive occurrences of the same input are stored once. The up-arrow key goes back in history, and the down-arrow key goes forward, towards more recent inputs. Previously the READ buffer was limited to just the previous line. [`#83 <https://github.com/YottaDB/YottaDB/issues/83>`_]
* YottaDB correctly runs M programs which had PATNOTFOUND errors at compile time. Previously, in r1.10 it was possible for mumps processes to terminate abnormally with a SIG-11 as a consequence of a defect in the GT.M V6.3-002 code base.[`#90 <https://github.com/YottaDB/YottaDB/issues/90>`_]
* Timeouts in JOB, LOCK, OPEN, READ, WRITE /WAIT, WRITE /LISTEN, WRITE /ACCEPT, and WRITE /TLS commands with more than three digits after the decimal point time out shortly after the requested time has elapsed. Previously, in r1.10, timeouts with more than three digits after the decimal point would be treated as 2Gi msec (≃24.8 days), owing to a defective enhancement to allow fractional timeouts introduced in the GT.M V6.3-002 code base (`GTM-5250 <http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V6.3-002_Release_Notes.html#GTM-5250>`_) [`#100 <https://github.com/YottaDB/YottaDB/issues/100>`_]
* $ZEOF is set correctly for files in the /proc file system. Previously, $ZEOF used to incorrectly return 1 after the first line. [`#120 <https://github.com/YottaDB/YottaDB/issues/120>`_]
* ZSTEP OVER and ZSTEP OUTOF work correctly across extrinsic function calls which return using the QUIT @ syntax. Previously, the ZSTEP would not pause (and execute the ZSTEP action) after the return from such function calls. [`#141 <https://github.com/YottaDB/YottaDB/issues/141>`_]
* YottaDB issues a DIVZERO error when the divisor in a division operation is 0. In previous versions (r1.10 and r1.00 as well as all GT.M versions up to V6.3-003A), dividing by 0 in some numeric expressions resulted in a fatal KILLBYSIGSINFO1/SIGINTDIV error (for example set x=2E20 write 1/(x*0)). [`#145 <https://github.com/YottaDB/YottaDB/issues/145>`_]
* A C program that invokes a call-in leaves the terminal in a sane state when it exits. Previously, some terminal characteristics would be changed (for example, character echo would be disabled) on exit, requiring an stty sane command to restore them. Additionally, YottaDB now changes the terminal characteristics only when necessary (i.e. a READ or WRITE from/to the terminal, or direct mode). Previously, it used to change them unconditionally at process startup. [`#184 <https://github.com/YottaDB/YottaDB/issues/184>`_]
* A USE statement for a terminal device with parameters (CTRAP=$C(3):EXCEPTION="…") executes the specified M code fragment when a Ctrl-C is entered. Previously, the Ctrl-C was ignored. A workaround was to also specify the NOCENABLE option. [`#186 <https://github.com/YottaDB/YottaDB/issues/186>`_]
* Call-ins are allowed even if a TP transaction is active (process is within a TSTART/TCOMMIT fence). Previously, a call-in while already in TP was not permitted, resulting in a CITPNESTED error. [`#188 <https://github.com/YottaDB/YottaDB/issues/188>`_]

~~~~~~~~~~~
Other
~~~~~~~~~~~

* The YottaDB install directory size is 14-15Mb (down from 34Mb in prior versions). [`#33 <https://github.com/YottaDB/YottaDB/issues/33>`_]
* When run with ydb_dbglvl / gtmdbglvl set as described in [`#115 <https://github.com/YottaDB/YottaDB/issues/115>`_], YottaDB reports correct ratios when the number of indirection cache hits exceeds 43 million. Previously, it could report a negative number. Note that ydb_dbglvl / gtmdgblvl is not part of the published and supported API whose stability we strive to maintain, and exists to assist YottaDB in supporting customers. [`#77 <https://github.com/YottaDB/YottaDB/issues/77>`_]
* When asked to install GT.M using the gtm qualifier, the ydbinstall.sh script reports "GT.M" on a successful install. Previously, it reported "YottaDB" unconditionally, whether it installed YottaDB or GT.M. [`#99 <https://github.com/YottaDB/YottaDB/issues/99>`_]
* For "out of the box" use of YottaDB, ydb_env_set is a file you can source with a POSIX or compatible shell to configure an environment with a default structure and required environment variables, creating a default environment if one does not exist.  At this time, support has not been implemented in YottaDB for all ydb\_ prefixed environment varables, but each release will increase that set. So as to not require changes as future YottaDB releases add YottaDB counterparts to GT.M environment variables, sourcing ydb_env_set sets both sets of environment variables to appropriate values, which are usually, but not always, the same. Sourcing ydb_env_unset unsets the above environment variables, unsets the aliases, and removes any occurrence of $ydb_dist in $LD_LIBRARY_PATH. For more information, see Issue [`#126 <https://github.com/YottaDB/YottaDB/issues/126>`_]
* Installing YottaDB with UTF8 support builds $gtm_dist/utf8/libyottadbutil.so (previously named libgtmutil.so) on the Linux/ARM platform. In r1.10, libgtmutil.so was built only in the $gtm_dist (non-utf8) directory. [`#143 <https://github.com/YottaDB/YottaDB/issues/143>`_]
* When installing YottaDB, gtm is created as a symbolic link to ydb, and gtmprofile is a symbolic link to ydb_env_set. The default names for global directory, database files, and journal files are changed, but if current files exist, their environment setting is done with an M program rather than with shell commands. [`#160 <https://github.com/YottaDB/YottaDB/pull/160>`_]
* The YDB_SOURCE_DIR cmake variable is used to get the path to ydbmerrors.h instead of a hardcoded relative path [`#161 <https://github.com/YottaDB/YottaDB/pull/161>`_]
* A Docker image to build and deploy a YottaDB runtime environment is available. [`#162 <https://github.com/YottaDB/YottaDB/pull/162>`_]


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

+++++++++++++
From YottaDB
+++++++++++++

**CALLINTCOMMIT**, TCOMMIT at call-in-level=xxxx not allowed as corresponding TSTART was done at lower call-in-level=yyyy.

Run Time Error: This indicates that at least one call-in invocation happened in between when the TP transaction started (either through a ydb_tp_s() call in C or a TSTART command in M) and when the corresponding transaction commit is attempted (through a TCOMMIT command in M).

Action: If a TP transaction is started using SimpleAPI, and the user function driven by ydb_tp_s() does a call-in invocation, care should be taken to ensure the call-in code does not do a TCOMMIT.

**CALLINTROLLBACK**, TROLLBACK at call-in-level=xxxx not allowed as corresponding TSTART was done at lower call-in-level=yyyy

Run Time Error: This indicates that at least one call-in invocation happened in between when the TP transaction started (either through a ydb_tp_s() call in C or a TSTART command in M) and when the corresponding transaction rollback is attempted (through a TROLLBACK command in M).

Action: If a TP transaction is started using SimpleAPI, and the user function driven by ydb_tp_s() does a call-in invocation, care should be taken to ensure the call-in code does not do a TROLLBACK.

**FATALERROR1**, Fatal error raised. Generating core and terminating process. Error: <error>.

Run Time Error: This indicates that there was a fatal error in a SimpleAPI call that resulted in the termination of the running process and the generation of a core file. Appears in the system log.

Action: Look up the error indicated in the secondary message text in the documentation to correct the cause of the fatal error.

**FATALERROR2**, Fatal error raised. Bypassing core generation and terminating process. Error: <error>

Run Time Error: This indicates that there was a fatal error in a SimpleAPI call that resulted in the termination of the running process, and no core file was generated as a result of this. Appears in the system log.

Action: Look up the error indicated in the secondary message text in the documentation to correct the cause of the fatal error.

**INSUFFSUBS**, Return subscript array for an API call too small.

Run Time Error: This indicates that the return subscript array needs more entries for the ydb_node_next_s() or ydb_node_previous_s() SimpleAPI call than is currently allocated (specified by the input/output parameter \*ret_subs_used). In this case \*ret_subs_used is set to the needed entries.

Action: Ensure the return subscript array ("ret_subsarray" parameter of ydb_node_next_s() or ydb_node_previous_s()) is allocated with at least \*ret_subs_used entries and retry the ydb_node_next_s() or ydb_node_previous_s() call.

**INVNAMECOUNT**, Number of varnames (namecount parameter in a rrrr call) cannot be less than zero.

Runtime Error: This indicates that the number of variable names specified in a SimpleAPI call (identified in the message text) is less than zero.

Action: Redo the SimpleAPI call with a number of variable names that is greater than or equal to zero.

**INVVARNAME**, Invalid local/global/ISV variable name supplied to API call.

Run Time Error: This indicates that a SimpleAPI call received an invalid variable name. The invalidity can be one of the following types:

a) The ydb_buffer_t structure corresponding to the variable name has a "len_used" field greater than "alloc_len" OR
b) The ydb_buffer_t structure corresponding to the variable name has a zero value of "len_used" OR
c) The ydb_buffer_t structure corresponding to the variable name has a non-zero value of "len_used" but a NULL value of "buf_addr" OR
d) The variable name starts with a ^ (i.e. is a global variable name), but the second character is not a % or an alpha character (lower or upper case) or at least one of the following characters is not an alphanumeric character (lower or upper case alphabet or a decimal digit) OR
e) The variable name starts with a $ (i.e. is an intrinsic special variable name), but is not followed by any other character (i.e. "len_used" has a value of 1) OR
f) The variable name starts with a character other than a % or an alpha character (lower or upper case) OR
g) The variable name starts with a % or alpha character (lower or upper case) but at least one of the following characters is not an alphanumeric character (lower or upper case alphabet or a decimal digit)

Action: Determine which of the described failures scenarios is the issue and accordingly fix the variable name passed in to the SimpleAPI call

**LIBYOTTAMISMTCH**, $ydb_dist/libyottadb.so does not match the shared library path.

Runtime Error: This indicates that the full path of the currently running libyottadb.so shared library does not match the path described by $ydb_dist. This is possible for example if a C program tries to directly invoke a base image function (e.g. gtm_main, dse_main, mupip_main etc.) for more than one build/release of YottaDB in the same process.

Action:  Make sure a C program invokes a base image function of only one libyottadb.so executable.

**MINNRSUBSCRIPTS**, Number of subscripts cannot be a negative number.

Run Time Error: This indicates that the number of subscripts in an input array (usually the "subs_used" parameter in various SimpleAPI calls) is a negative number.

Action: Redo the SimpleAPI call with a subscript count that is greater than or equal to zero.

**MIXIMAGE**, Cannot load more than one base image function on a process.

Run Time Error: This indicates that a C function tries to invoke more than one base image function included in libyottadb.so (e.g. gtm_main, dse_main, mupip_main etc.). Only one base image function can be invoked and only once for the lifetime of the process.

Action: Make sure only one base image function is invoked for the lifetime of one process.

**NAMECOUNT2HI**, Number of varnames specified (namecount parameter in a rrrr call)  exceeds maximum cccc allowed.

Runtime Error: This indicates that the number of variable names specified in a SimpleAPI call (identified in the message text) exceeds the maximum number of allowed variable names (also identified in the message text).

Action: Redo the SimpleAPI call with a fewer number of variable names specified.

**PARAMINVALID**, Invalid parameter dddd specified in an API (rrrr) call.

Run Time Error: This indicates that a parameter in a SimpleAPI call was not properly specified. The function name (e.g. ydb_set_s()) and the name of the invalid parameter (e.g. subsarray) along with the type of the invalidity is identified in the error message text. If the parameter is an array, the index of the element where the invalidity is detected is also identified. If the parameter is an input parameter of type ydb_buffer_t it is invalid if "len_used" is greater than "alloc_len" OR if it has a "len_used value of 0 but a NULL value of "buf_addr". If the parameter is an output parameter, it is invalid if the ydb_buffer_t pointer is NULL or if the "buf_addr" field in the ydb_buffer_t structure is NULL. Note that no error checks are done if an input ydb_buffer_t typed pointer parameter is NULL (the process would get a SIG-11 and dump core in that case).

Action: Fix the cause of the invalidity and pass in a valid parameter to the SimpleAPI call.

**QUERY2**, Invalid second argument to $QUERY. Must be -1 or 1.

Run Time Error: This indicates that there is an invalid second argument passed to the function $QUERY. It must be either -1 or 1.

Action: Refer to `$QUERY in the Programmer's Guide <https://docs.yottadb.com/ProgrammersGuide/functions.html#query>`_ for correct usage.

**READONLYLKFAIL**, Failed to get a lock on READ_ONLY database file.

Run Time Error: This error is issued by a MUPIP command that requires standalone access (e.g. MUPIP SET -NOREAD_ONLY) to a database file (which has Read-only mode turned on) if other processes are still accessing the database OR by any process that tries to open a database file (which again has Read-only mode turned on) while a MUPIP command that has standalone access on the same database file is concurrently running.

Action: If the error is from the MUPIP command which requires standalone access, ensure all processes which have the database file open are shut down and reattempt the command. If the error is from a process trying to open the database file, wait for the concurrent MUPIP command requiring standalone access to finish and reattempt to open the database.

**READONLYNOSTATS**, Read-only and Statistics sharing cannot both be enabled on database.

Run Time Error: This error is issued if if one tries to enable the Read-only mode on a database that has Statistics sharing turned on OR if one tries to enable Statistics sharing on a database that has Read-only mode turned on OR if one tries to enable both at the same time.

Action: Make sure at most one of Read-only or Statistics sharing is turned on in the database at any point in time.

**SIMPLEAPINEST**, Attempt to nest a SimpleAPI call (rrrr) with another SimpleAPI call (RRRR) - - nesting calls is not permitted in simpleAPI.

Run Time Error: This indicates that a SimpleAPI call (function name identified in the message text) was attempted while another SimpleAPI call (whose function name is also identified in the message text) is still running (possible for example through a call-in or trigger invocation). Nesting of such SimpleAPI calls is not currently permitted.

Action: Avoid nesting SimpleAPI calls. Finish one SimpleAPI call before attempting another.

**SUBSARRAYNULL**, Non-zero number of subscripts xxxx specified but subscript array parameter is NULL in API call.

Run Time Error: This indicates that the value of the subscript array parameter is NULL, meaning there are no subscripts specified, but the parameter specifying the number of subscripts (usually the "subs_used" parameter) has a non-zero value.

Action: Redo the SimpleAPI call with a non-NULL subscript array parameter or with a zero value for the parameter specifying the number of subscripts.

**TIME2LONG**, Specified time value exceeds supported maximum limit xxxx allowed.
Run Time Error: This indicates that a timer value specified in a SimpleAPI call (e.g. ydb_lock_s(), ydb_lock_incr_s() etc.) exceeded the maximum allowed limit. Both the specified time value and the maximum allowed limit are indicated in the message text.

Action: Specify a time value below the maximum limit and retry the SimpleAPI call.

**VARNAME2LONG**, Variable name length exceeds maximum allowed length xxxx.

Run Time Error: This indicates that the length of a variable name specified in a SimpleAPI call exceeded the maximum limit. The maximum value is identified in the message text.

Action: Specify the variable name within the maximum length limit and retry the SimpleAPI call.

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

+----------------------------------------------------------------------------+--------------------------------------------------+
| sha256sum                                                                  | File                                             |
+============================================================================+==================================================+
| cd26897549405b33e63966df52aefb8ad581afd1633db1cb2723ff2c12acce25           | yottadb_r120_linux_armv6l_pro.tgz                |
+----------------------------------------------------------------------------+--------------------------------------------------+
| 8993fbb7300cb732da06e90bc7cb1334e9ab5318da7d0b7427900be8919aa640           | yottadb_r120_linux_armv7l_pro.tgz                |
+----------------------------------------------------------------------------+--------------------------------------------------+
| 6e7bf4c1fa0b12e29fa2b0e1629bfdaaeebd0541c458eaf561d5676d1f0fc5e6           | yottadb_r120_linux_x8664_pro.tgz                 |
+----------------------------------------------------------------------------+--------------------------------------------------+
| e32dc5ffbdd1e8fd17d4ed2f1df97145f05d5748489f2b5d8322ad9ee33008ce           | yottadb_r120_rhel7_x8664_pro.tgz                 |
+----------------------------------------------------------------------------+--------------------------------------------------+
| f4310725ff72ff6bd5da41fc0b3eaf5ab918978ce33d08878ed717c1d1cf04c4           | yottadb_r120_src.tgz                             |
+----------------------------------------------------------------------------+--------------------------------------------------+

-----------------------
Legal Stuff
-----------------------

Copyright © 2018 YottaDB LLC

Permission is granted to copy, distribute and/or modify this document under the terms of the `GNU Free Documentation License, Version 1.3 <http://www.gnu.org/licenses/fdl.txt>`_ or any later version published by the Free Software Foundation; with no Invariant Sections, no Front-Cover Texts and no Back-Cover Texts.

YottaDB™ is a trademark of YottaDB LLC.
GT.M™ is a trademark of Fidelity National Information Services, Inc.
Other trademarks belong to their respective owners.

This document contains a description of YottaDB and the operating instructions pertaining to the various functions that comprise the software. This document does not contain any commitment of YottaDB LLC. YottaDB LLC believes the information in this publication is accurate as of its publication date; such information is subject to change without notice. YottaDB LLC is not responsible for any errors or defects.
