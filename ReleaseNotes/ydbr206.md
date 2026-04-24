<!---
.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2024-2026 YottaDB LLC and/or its subsidiaries.#
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
-->

# YottaDB r2.06

## Binary Distributions

| sha256sum | file |
|-----------|------|
|           |      |

## Release Note Revision History

| Revision | Date           | Summary               |
|----------|----------------|-----------------------|
| 1.0      | April 27, 2026 | Initial Release Notes |

## Contact Information

### YottaDB LLC

https://yottadb.com /  <info@yottadb.com>

### Support

#### Customers

Contact YottaDB or your YottaDB support channel for support with assured service levels on commercial terms from knowledgeable staff.

#### Others

For free (to you) support from members of communities who run widely available applications on YottaDB, please use an application-specific list where appropriate.

 - Join the discussion on the [YottaDB Discord channel](https://discord.gg/bFHmDdzcqY).
 - For issues specific to the use of YottaDB from node.js via [Nodem](https://github.com/dlwicksell/nodem), [post an Issue on the Nodem project](https://github.com/dlwicksell/nodem/issues/new/).
 - For issues specific to the use of YottaDB from [QewdJS](http://qewdjs.com/), [EWD.js](https://github.com/robtweed/ewd.js), node.js via [mg-dbx](https://github.com/chrisemunt/mg-dbx), or [mg-dbx-napi](https://github.com/chrisemunt/mg-dbx-napi) post to the [Enterprise Web Developer community](https://groups.google.com/forum/#!forum/enterprise-web-developer-community).
 - For issues specific to the use of YottaDB with [VistA](https://en.wikipedia.org/wiki/VistA) flavors, post to the [Hardhats](https://groups.google.com/forum/#!forum/hardhats) list.
 - For issues specific to use of YottaDB with the [M Language](https://docs.yottadb.com/ProgrammersGuide/), post to the [Everything MUMPS](https://groups.google.com/g/everythingmumps) list.
 - For requests other than to the communities above [post an Issue](https://gitlab.com/YottaDB/DB/YDB/-/issues) and include the words "Help Wanted" in the Title.

## r2.06

### Overview

r2.06 provides a timely fix to a bug in r2.04, which the compiler generates incorrect code for an unusual code pattern. We recommend that you use r2.06 instead of r2.04. If you are upgrading from a YottaDB release prior to r2.04, please refer to the [r2.04 Release Notes](https://gitlab.com/YottaDB/DB/YDB/-/releases/r2.04).

Details are in the [Change History](#changehistory) section.

### Platforms

A platform is a combination of a CPU architecture and an operating system. A platform is Supported, Supportable, or Unsupported.

* Supported means that we have the platform in our development environment, test each release on that platform, and provide a binary distribution for it.
* Supportable means that although we may not have such a platform in our environment, we have no reason to believe that the software will not run on it.
* All others are Unsupported.

| CPU Architecture      | Supported OS Version(s)              | Notes                                        |
|-----------------------|--------------------------------------|----------------------------------------------|
| 64-bit x86            | * Ubuntu 24.04 LTS<br>* Red Hat Enterprise Linux 9.x & 10.x<br>* SUSE Linux Enterprise 15.x<br>* Debian GNU/Linux 13 (Trixie) | There are separate binary distributions for each OS version, owing to differences in the library versions of the distributions. |
| AARCH64 (64-bit ARM   | * Ubuntu 24.04 LTS<br>* Debian GNU Linux 13 (Trixie) | See below. |

 Recent releases of major 64-bit GNU/Linux distributions with contemporary kernels, glibc and ncurses are Supportable; however you may have to build YottaDB from source using the `--from-source` option of `ydbinstall.sh`. Specific notes:

- Supported filesystems are ext4 and xfs. f2fs is Supportable. btrfs, zfs, and NFS are Unsupported, and known to have issues.
- [ydbinstall.sh](https://gitlab.com/YottaDB/DB/YDB/-/blob/master/sr_unix/ydbinstall.sh) recognizes [Rocky Linux](https://rockylinux.org/) as equivalent to RHEL, and [OpenSUSE Leap](https://www.opensuse.org/#Leap) as equivalent to SUSE Linux Enterprise, installing the releases for the corresponding Supported distributions. Note that Rocky Linux and OpenSUSE Leap are Supportable, not Supported.
- On [Arch Linux](https://www.archlinux.org/) and other leading edge distributions such as [OpenSUSE Tumbleweed](https://www.opensuse.org/#Tumbleweed), as well as new versions of Linux distributions, YottaDB will need to be recompiled from source code owing to library and tool chain versions newer than those used in building Supported distributions. The `--from-source` option of [ydbinstall.sh](https://gitlab.com/YottaDB/DB/YDB/-/blob/master/sr_unix/ydbinstall.sh) simplifies the process.
- Owing to variations in the implementations of [AARCH66](https://en.wikipedia.org/wiki/AArch64) microarchitectures, we recommend that you ensure the software runs correctly before committing to any specific hardware. Please [contact us](mailto:info@yottadb.com?subject=AARCH64%20CPU%20and%20OS%20support) if you want a specific combination of OS and CPU microarchitecture to be Supported.
- The 64-bit ARM Ubuntu 24.04 LTS distributions are built on [Ubuntu Asahi](https://ubuntuasahi.org/) on Apple Mac Mini M2 hardware. See our blog post [The Strong ARM of Apple](https://yottadb.com/the-strong-arm-of-apple/) for more information.

### Installation
See our [Get Started page](https://yottadb.com/product/get-started) to use YottaDB.

We **strongly recommend** that you install YottaDB r2.06 in a newly created directory, different from those of prior YottaDB releases and any GT.M versions you may have installed on the system.

### Removing an installed YottaDB release
Assuming `$ydb_dist` points to the directory where YottaDB is installed:

 - Cleanly shut down all application processes using that release.
 - Execute `mupip rundown && mupip rundown -relinkctl` and review the output to ensure successful completion.
 - Ensure that there are no `gtcm*` or `gtmsecshr` processes active.
 - Use `sudo lsof | grep $ydb_dist` to ensure there are no open files.
 - If there are links to files in `$ydb_dist`, e.g., from `/usr/local/bin/` or `/usr/local/etc/`, remove the links.
 - Delete the directory with `sudo rm -rf $ydb_dist`

## Upgrading to YottaDB r2.06

YottaDB r2.06 is upward compatible from YottaDB [r2.04](https://gitlab.com/YottaDB/DB/YDB/-/releases/r2.024). The minimal upgrade steps are:

* Install YottaDB r2.06 using the [ydbinstall.sh](https://download.yottadb.com/ydbinstall.sh) script; or download the YottaDB distribution for your platform, and use the included `ydbinstall` script to install YottaDB. The scripts can install the YottaDB plugins that you use, along with installing YottaDB, or you can install or upgrade them later. The `--help` option of the script lists its options.
* Install plugins you need in addition to those installed in the previous step, e.g., non-YottaDB plugins or customized plugins.
* Cleanly shut down the application, as well as replication servers if replication is in use. Ensure that the database files are shut down using [MUPIP RUNDOWN](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#rundown) and routine shared memory with [MUPIP RUNDOWN RELINKCTL](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#relinkctl) from the prior release.
* Recompile object code, and recreate shared libraries if your application uses shared libraries. If you are not using shared libraries, YottaDB will atomatically recompile routines and replace object code files compiled with prior releases.
* Switch journal files with the new YottaDB release.
* Start using the new YottaDB release. If the instance is replicated, remember to start a Source Server process first.

If the database has triggers defined with non-canonical numbers, or numbers specified as strings with any version prior to [r1.28](https://gitlab.com/YottaDB/DB/YDB/-/releases/r1.28), or if you are unsure, extract the trigger definitions, delete existing triggers, and reload the trigger definitions. Issue [[#430](https://gitlab.com/YottaDB/DB/YDB/-/work_items/430)] from [r1.28](https://gitlab.com/YottaDB/DB/YDB/-/releases/r1.28) has a series of steps you can copy and execute. There is no need to do this if upgrading from [r1.28](https://gitlab.com/YottaDB/DB/YDB/-/releases/r1.28) or later.

To upgrade from older GT.M releases, first upgrade to GT.M V6.0-000 or later and follow the steps above, or contact your YottaDB support channel for assistance and guidance.

Use a rolling technique to upgrade production installations to r2.06.

* Create a new replicated instance of your application (on the same system or a different system). Let's call the live production system A and the replicated instance B.
* Install YottaDB r2.06 on B, and upgrade the replica on B to r2.06. Resume replicating from A to B.
* Once B catches up, switchover so that B is in a primary role replicating to A.
* Once you are satisfied with B, remove (or upgrade) A.

### Upgrading Global Directories

Opening a global directory file with [GDE](https://docs.yottadb.com/AdminOpsGuide/gde.html) and exiting automatically upgrades it to the latest format. As there is no way to downgrade the format of a global directory file, we recommend taking a backup before you upgrade it. The [GDE SHOW COMMAND](https://docs.yottadb.com/AdminOpsGuide/gde.html#show) outputs a text version of the global directory that you can use to recreate it, and which you can also use for global directory version control.

### Upgrading Database Files

Database files created with prior r2.x releases of YottaDB do not need to be explicitly upgraded. YottaDB r2.06 fully supports and uses database files created with r1.x releases of YottaDB.

> [!note]
> A database file can only be opened by processes of one YottaDB release at any given time.

In the event you need database files that need the 16Gi block limit or the 11 tree levels of r2.x (r1.x database files have a 992Mi block limit and 7 tree levels), you have two options:

* After a [MUPIP FREEZE](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#mupip-freeze) of the database, use [MUPIP EXTRACT](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#extract) to extract the database nodes, and [MUPIP LOAD](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#load) to load them into a new database file created by [MUPIP CREATE](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#create). A binary format will give you the fastest extract and load times. Note that you should use a MUPIP FREEZE across all regions of a database to ensure consistency of the database. The MUPIP EXTRACT and MUPIP LOAD processes can run concurrently for the regions.

* After a [MUPIP INTEG](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#integ) to verify the structural integrity of database files, use [MUPIP UPGRADE](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#mupip-upgrade) to upgrade the database file header to the 2.x format, followed by a [MUPIP REORG UPGRADE](https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#upgrade) to upgrade the individual blocks. MUPIP REORG UPGRADE can run concurrently with normal database usage.

If you wish to use a database file created or used by r2.06 on a prior release of YottaDB or a GT.M version, contact your YottaDB support channel.

<a name="changehistory"></a>
## Change History

### r2.06

YottaDB r2.06 includes the following enhancements and fixes beyond YottaDB [r2.04](https://gitlab.com/YottaDB/DB/YDB/-/releases/r2.04).

| ID               | Category  | Summary                                                           |
|------------------|-----------|-------------------------------------------------------------------|
| ([982](#x982))   | Languages | Subscript functions recognize ^#t global references               |
| ([1221](#x1221)) | Languages | Correct JSON encoding when data includes ASCII NULL               |
| ([1223](#x1223)) | Languages | Generate correct global reference code in an unusual case         |
| ([1224](#x1224)) | Languages | Lock acquisition with zero timeout reliably works in an edge case |

### Languages

* <a name="x982"></a>The [\$QLENGTH()](https://docs.yottadb.com/ProgrammersGuide/functions.html#qlength), [\$QSUBSCRIPT()](https://docs.yottadb.com/ProgrammersGuide/functions.html#qsubscript), [\$VIEW("YGVN2GDS")](https://docs.yottadb.com/ProgrammersGuide/functions.html#view), [ydb_node_next_s() / ydb_node_next_st()](https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#ydb-node-next-s-ydb-node-next-st), and [ydb_node_previous_s() / ydb_node_previous_st()](https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#ydb-node-previous-s-ydb-node-previous-st) functions accept `^#t` gvrefs as valid. Previously, they generated [NOCANONICNAME](https://docs.yottadb.com/MessageRecovery/errors.html#nocanonicname) errors. Note that `^#t` globals store [triggers](https://docs.yottadb.com/ProgrammersGuide/triggers.html) in database files, and are not valid M global variable names.  [Indirection](https://docs.yottadb.com/ProgrammersGuide/langfeat.html#indirection) and [XECUTE](https://docs.yottadb.com/ProgrammersGuide/commands.html#xecute) continue to generate NOCANONICNAME errors if they encounter `^#t` gvrefs. [#982](https://gitlab.com/YottaDB/DB/YDB/-/work_items/982)

* <a name="x1221"></a>[ydb_encode_s() / ydb_encode_st()](https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#ydb-encode-s-ydb-encode-st) and [ZYENCODE](https://docs.yottadb.com/ProgrammersGuide/commands.html#zyencode) work correctly when encoding an ASCII NULL ($CHAR(0)). Previously it was possible for them to randomly generate incorrect encoding in rare cases. [#1221](https://gitlab.com/YottaDB/DB/YDB/-/work_items/1221)

* <a name="x1223"></a>The M compiler generates correct code in an unusual case where an extrinsic function is called which returns after setting one global node, follows in the source code another that is not called but which also returns after setting a global node of the same variable. Previously, in r2.04, in this unusual case, the generated code set an incorrect global variable node or incorrectly returned a [GVNAKED](https://docs.yottadb.com/MessageRecovery/errors.html#gvnaked). This unusual case is best illustrated with an example:

  ```
  YDB1223	;
	  kill ^x(1)
	  set x=$$x
	  zwrite ^x
	  quit

  y()	;
	  set y=^y(0)
	  quit "y"

  x()	;
	  set ^y(1)=2
	  quit "x"
  ```

  The correct behavior is:

  ```
  $ yottadb -run YDB1223
  Error occurred: 150372994,YDB1223+3^YDB1223,%YDB-E-GVUNDEF, Global variable undefined: ^x
  $
  ```

  The previous incorrect behavior was:

  ```
  $ yottadb -run YDB1223
  ^x(1)=2
  $
  ```

  We thank [Konstantin](https://gitlab.com/littlecat) for bringing this bug to our attention. [#1223](https://gitlab.com/YottaDB/DB/YDB/-/work_items/1223)

* <a name="x1224"></a>[Locking](https://docs.yottadb.com/ProgrammersGuide/commands.html#lock) a resource with a zero timeout (e.g. `LOCK +^GBL:0` in M; or equivalent using [ydb_lock_s() / ydb_lock_st()](https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#ydb-incr-s-ydb-incr-st) in C and equivalent language wrapper functions) acquires the lock if no process holds the lock, and another process is waiting for the lock. If the lock acquisition does not succeed, it means that another process is holding the lock. Previously, it was possible for the acquisition to not succeed (i.e, resource `^GBL` not locked by the process, with [$TEST](https://docs.yottadb.com/ProgrammersGuide/isv.html#test) set to 0 in M or a return of [YBD_LOCK_TIMEOUT](https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#ydb-lock-timeout) in C), if the process attempted to acquire the lock in the brief period after a process holding the lock released it, and a waiting process had not yet acquired it. Note that the previous behavior was not wrong, because the process waiting for the lock would acquire it - if a lock is released with multiple processes wanting it, there is nothing that specifies which of those processes wanting the lock should acquire it. We opted to make this change because the process with a zero timeout makes just one attempt to acquire the lock, and since it was available, it should get the lock even if another process is waiting for it. [#1224](https://gitlab.com/YottaDB/DB/YDB/-/work_items/1224)

## Additional Information

(There is no Additional Informaton for r2.06.)

## Messages

(There are no new, revised, or obsolete messages in r2.06.)

## Legal Stuff

Copyright © 2026 YottaDB LLC. Portions Copyright Fidelity National Information Services, Inc. and/or its subsidiaries.

Permission is granted to copy, distribute and/or modify this document under the terms of the [GNU Free Documentation License, Version 1.3](https://www.gnu.org/licenses/fdl-1.3.txt) or any later version published by the [Free Software Foundation](https://www.fsf.org/); with no Invariant Sections, no Front-Cover Texts and no Back-Cover Texts.

YottaDB®, Octo®, and Xider® are registered trademarks, of YottaDB LLC. GT.M™ is a trademark of Fidelity National Information Services, Inc. Other trademarks belong to their respective owners.

This document contains a description of YottaDB and the operating instructions pertaining to the various functions that comprise the software. This document does not contain any commitment of YottaDB LLC. YottaDB LLC believes the information in this publication is accurate as of its publication date; such information is subject to change without notice. YottaDB LLC is not responsible for any errors or defects.
