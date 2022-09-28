<!---
.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2023 YottaDB LLC and/or its subsidiaries.     #
.. # All rights reserved.                                        #
.. #                                                             #
.. #     This document contains the intellectual property        #
.. #     of its copyright holder(s), and is made available       #
.. #     under a license.  If you do not know the terms of       #
.. #     the license, please stop and do not read further.       #
.. #                                                             #
.. ###############################################################
-->

# YottaDB r2.00

### Release Note Revision History

| Revision  | Date              | Summary               |
| --------- | ----------------- | --------------------- |
| 1.00      |                   | r2.00 Initial Release |

### Contact Information
#### YottaDB LLC
https://yottadb.com /  <info@yottadb.com>

#### Support
##### Customers
Contact YottaDB or your YottaDB support channel for support with assured service levels from knowledgeable staff.

##### Others
For free (to you) support from members of communities who run widely available applications on YottaDB, please use an application-specific list where appropriate.

 - For requests other than to the communities below, [post an Issue](https://gitlab.com/YottaDB/DB/YDB/-/issues) and include the words "Help Wanted" in the Title.
 - For access from node.js via [Nodem](https://github.com/dlwicksell/nodem), [post an Issue on the Nodem project](https://github.com/dlwicksell/nodem/issues/new/).
 - For access from [QewdJS](http://qewdjs.com/), or [EWD.js](https://github.com/robtweed/ewd.js), or from node.js via [mg-dbx](https://github.com/chrisemunt/mg-dbx) post to the [Enterprise Web Developer community](https://groups.google.com/forum/#!forum/enterprise-web-developer-community).
 - For requests specific to the use of YottaDB with [VistA](https://en.wikipedia.org/wiki/VistA) flavors, post to the [Hardhats](https://groups.google.com/forum/#!forum/hardhats) list.
 - For requests specific to the use of YottaDB with M other than for applications above, post to the [comp.lang.mumps](https://groups.google.com/forum/#!forum/comp.lang.mumps) list.

### r2.00

#### Overview

Inherited from the upstream GT.M V7.0-000, YottaDB r2.00 supports database files of up to 16Gi blocks. For example, the maximum size of a database file with 4KiB blocks is 64TiB, which means you can use fewer regions for extremely large databases. With YottaDB r2.00, you can continue to use database files created by r1.x releases, except that the maximum size of a database file created with prior YottaDB releases remains unchanged.

r2.00 also includes other fixes and enhancements including changes aimed a making management of replication more straightforward.

As with all YottaDB releases, there are a number of fixes and smaller enhancements, as described in the [Change History](#change-history).

#### Platforms
A platform is a combination of a CPU architecture and an operating system. A platform is Supported, Supportable, or Unsupported.

* Supported means that we have the platform in our development environment and test each release on that platform.
* Supportable means that although we do not necessarily have such a platform in our environment, we have no reason to believe that the software will not run on it.
* All others are Unsupported.

| CPU Architecture                    | Supported OS Version(s)                                                              | Notes                                                                                                                         |
| ----------------------------------- | ------------------------------------------------------------------------------------ | ----------------------------------------------------------------------------------------------------------------------------- |
| 64-bit x86                          | Ubuntu 22.04 LTS; Red Hat Enterprise Linux 8.x; SUSE Linux Enterprise 15.x; Debian GNU/Linux 11 (Bullseye) | There are separate binary distributions for each OS version, owing to differences in library versions of those distributions. |
| 64-bit ARM (Raspberry Pi 3 Model B) | Debian GNU/Linux 11 (Bullseye)                                                       | See below.                                                                                                                    |
| 32-bit ARM (Raspberry Pi Zero)      | Debian GNU/Linux 11 (Bullseye)                                                       | See below.                                                                                                                    |

Recent releases of major GNU/Linux distributions with contemporary kernels, glibc and ncurses are Supportable. Specific notes:

- [ydbinstall.sh](https://gitlab.com/YottaDB/DB/YDB/-/blob/master/sr_unix/ydbinstall.sh) recognizes [Rocky Linux](https://rockylinux.org/) 8 as equivalent to RHEL 8, and [OpenSUSE Leap](https://www.opensuse.org/#Leap) 15.x as equivalent to SUSE Linux Enterprise 15.x, and installs the corresponding versions. Note that Rocky Linux and OpenSUSE Leap are Supportable, not Supported.
- On [Arch Linux](https://www.archlinux.org/) and other leading edge distributions such as OpenSUSE Tumbleweed, and newer OS versions such as Red Hat Enterprise Linux 9.x, YottaDB will need to be recompiled from source code owing to library and tool chain versions newer than those used in building Supported distributions. The --from-source option of [ydbinstall.sh](https://gitlab.com/YottaDB/DB/YDB/-/blob/master/sr_unix/ydbinstall.sh) simplifies the process. ([754](https://gitlab.com/YottaDB/DB/YDB/-/issues/754))
- While YottaDB is Supportable on other ARMv6, [ARMv7-A](https://en.wikipedia.org/wiki/Comparison_of_ARMv7-A_cores), and [ARMv8-A](https://en.wikipedia.org/wiki/Comparison_of_ARMv8-A_cores) CPUs, owing to variations in the implementations of ARM microarchitectures, we recommend that you ensure the software runs correctly before committing to any specific hardware other than those listed above. Please contact <info@yottadb.com> if you want a specific combination of OS and CPU microarchitecture to be Supported.

#### Installation
See our [Get Started page](https://yottadb.com/product/get-started) to use YottaDB.

We **strongly recommend** that you install YottaDB r2.00 in a newly created directory, different from those of prior YottaDB releases and any GT.M versions you may have installed on the system.

#### Removing an installed YottaDB release
Assuming `$ydb_dist` points to the directory where YottaDB is installed:

 - Cleanly shut down all application processes using that release.
 - Execute `mupip rundown && mupip rundown -relinkctl`
 - Ensure that there are no `gtcm*` or `gtmsecshr` processes active.
 - Use `sudo lsof | grep $ydb_dist` to ensure there are no open files.
 - Delete the directory with `sudo rm -rf $ydb_dist`

### Upgrading to YottaDB r2.00
As YottaDB r2.00 is upward compatible from YottaDB [r1.38](https://gitlab.com/YottaDB/DB/YDB/-/releases/r1.38). The minimal upgrade steps are:

* Install YottaDB r2.00. Use `ydbinstall` / `ydbinstall.sh` to install plugins you use.
* Install plugins you need in addition to those installed in the previous step.
* Recompile object code, and recreate shared libraries if your application uses shared libraries.
* If your application uses encryption, compile and install the reference implementation plugin (if not done by the `ydbinstall` / `ydbinstall.sh` script) or your customized plugin.
* Cleanly shut down the application and ensure that the database files are shut down using MUPIP RUNDOWN from the prior release.
* Switch journal files with the new YottaDB release.
* Start using the new YottaDB release.

If the database has triggers defined with non-canonical numbers, or numbers specified as strings with any version prior to [r1.28](https://gitlab.com/YottaDB/DB/YDB/-/releases/r1.28), or if you are unsure, extract the trigger definitions, delete existing triggers, and reload the trigger definitions. Issue [[#430](https://gitlab.com/YottaDB/DB/YDB/-/issues/430)] from [r1.28](https://gitlab.com/YottaDB/DB/YDB/-/releases/r1.28) has a series of steps you can copy and execute. There is no need to do this if upgrading from [r1.28](https://gitlab.com/YottaDB/DB/YDB/-/releases/r1.28) or later.

To upgrade from older GT.M releases, first upgrade to GT.M V6.0-000 or later and follow the steps above, or contact your YottaDB support channel for assistance and guidance.

A more sophisticated upgrade technique is:

* Install YottaDB r2.00.
* Create a new replicated instance of your application (on the same system or a different system).
* Assuming the existing instance is A, and the new instance is B, upgrade B to r2.00 and start replicating from A to B.
* Once B catches up, switchover so that B is in a primary role replicating to A.
* Once you are satisfied with B, remove (or upgrade) A.

### Change History

#### r2.00

YottaDB r2.00 includes the following enhancements and fixes beyond YottaDB [r1.38](https://gitlab.com/YottaDB/DB/YDB/-/releases/r1.38).

| ID             | Category  | Summary                                                                                                 |
|----------------|-----------|---------------------------------------------------------------------------------------------------------|
| ([996](#x996)) | Languages | WRITE /PASS and WRITE /ACCEPT allow also LISTENING TCP sockets to be passed from one process to another |


#### GT.M V7.0-000

YottaDB r2.0 incorporates enhancements and fixes from [GT.M V7.0-000](http://tinco.pair.com/bhaskar/gtm/doc/articles/GTM_V7.0-000_Release_Notes.html).

| ID                      | Category              | Summary                                                                                                                                                   |
|-------------------------|-----------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------|
| ([GTM-6952](#GTM-6952)) | System Administration | Utilites accept decimal or hexidecimal input in many places                                                                                               |
| ([GTM-8262](#GTM-8262)) | System Administration | The installation script prints proper error messages if UTF-8 dependencies are not met                                                                    |
| ([GTM-8263](#GTM-8263)) | System Administration | UTF8 installation simplifications                                                                                                                         |
| ([GTM-8288](#GTM-8288)) | System Administration | Install configuration leave clean file authorizations                                                                                                     |
| ([GTM-8517](#GTM-8517)) | System Administration | YottaDB installation script records ownership, permissions, and OpenSSH SHA256 checksum values of files                                                   |
| ([GTM-8913](#GTM-8913)) | Database              | Revised database format supports up to 16Gi blocks in a region                                                                                            |
| ([GTM-9268](#GTM-9268)) | System Administration | JNLFLUSHLOG in the syslog if journal buffer flushing fails										                                                          |
| ([GTM-9302](#GTM-9302)) | System Administration | Acknowledged sequence number in MUPIP REPLICATE SOURCE SHOWBACKLOG and available with ^%PEEKBYNAME                                                        |
| ([GTM-9340](#GTM-9340)) | Languages             | VIEW and $VIEW() use the first 31 characters of overly long region names; and $VIEW() rejects multiple region names with a more appropriate error message |
| ([GTM-9343](#GTM-9343)) | System Administration | MUPIP FTOK JNLPOOL or RECVPOOL show the entire path for the replication instance file                                                                     |
| ([GTM-9344](#GTM-9344)) | System Administration | MUPIP FREEZE (ONLINE) with AIO freezes all regions at the same point in time                                                                              |
| ([GTM-9346](#GTM-9346)) | System Administration | Correct sleep period between soft tries connection attempts                                                                                               |
| ([GTM-9347](#GTM-9347)) | Languages             | Compiler optimization of out-of-range literals produces a NUMOFLOW error                                                                                  |
| ([GTM-9349](#GTM-9349)) | System Administration | YottaDB installation script does not attempt to install semstat2 and ftok utilities.                                                                      |
| ([GTM-9357](#GTM-9357)) | Other                 | ^%DSEWRAP removed from the distribution                                                                                                                   |
| ([GTM-9358](#GTM-9358)) | System Administration | MUPIP REPLICATE SOURCE ZEROBACKLOG SHUTDOWN appropriately cleans up IPC resources                                                                         |
| ([GTM-9361](#GTM-9361)) | System Administration | MUPIP REPLICATE ZEROBACKLOG uses sequence number from Receiver Server while determining the backlog                                                       |
| ([GTM-9362](#GTM-9362)) | Languages             | $TRANSLATE() with prior compilation errors does not terminate with segmentation violation                                                                 |
| ([GTM-9368](#GTM-9368)) | System Administration | A Source Server shutdown process exits with CTRL\_C                                                                                                       |
| ([GTM-9370](#GTM-9370)) | Languages             | All DIVZERO errors deferred to run-time                                                                                                                   |
| ([GTM-9371](#GTM-9371)) | Languages             | Ensure appropriate compilation of +<literal><nonrelational-Boolean-operator><gvn|@>                                                                     |

#### Database

* <a name="GTM-8913"></a> r2.00 creates database files that can hold up to 16Gi blocks. This requires a new database format using 64-bit rather than 32-bit pointers. The impact on storage efficiency is a function of the ratio of key size to pointer size. Prior versions used 32-bit block pointers and recent r1.x versions supported a maximum of 992Mi blocks.

r2.00 supports databases created using prior YottaDB releases, which appear the same within the global directory mapping. However, migrating existing data requires MUPIP EXTRACT and LOAD. YottaDB recommends using a rolling upgrade process to maximize application availability.

YottaDB r2.00 supports database files created with any prior YottaDB release.

#### Languages

* <a name="x996"></a> WRITE /PASS and WRITE /ACCEPT allow also LISTENING TCP sockets to be passed from one process to another. Previously, only CONNECTED TCP sockets could be passed, and an attempt to pass a LISTENING TCP socket resulted in the WRITE /ACCEPT command raising a GETSOCKNAMERR error.

* <a name="GTM-9340"></a> VIEW and $VIEW() use the first 31 characters of overly long region names. Previously, such an invalid name could cause the process to fail. This behavior was seen only seen in in-house testing and was never reported by a customer.

Also, $VIEW() produces a VIEWREGLIST error when it detects an attempt to specify more than one region, as $VIEW() does not accept region lists. Previously, it produced a NOREGION error for the second region even if that region existed.

* <a name="GTM-9347"></a> During compilation, YottaDB now reports NUMOFLOW errors for constants that exceed its numeric range. Such errors typically arise from use of the exponential ("E") notation. Previously, YottaDB suppressed such errors, which could lead to incorrect results.

* <a name="GTM-9362"></a>This issue was previously addressed in YottaDB r1.28 as part of the V6.3-013 code merge, and the following release note is included here for completeness.

$TRANSLATE() appropriately handles UTF-8 mode processing. Starting with r1.28 a recent prior compilation error could induce the process to fail with a segmentation violation (SIG-11).

The root cause was a longstanding defect in YottaDB management of memory in the transition from compilation back to execution and it is possible there were other symptoms. The workarounds are to avoid compilation errors in indirection and XECUTE or follow such an error with a successful compilation or a BREAK to direct mode.

* <a name="GTM-9370"></a> The YottaDB compiler defers optimization of expressions containing modulo by zero (#0) or exponentiation of zero using a negative exponent (e.g. 0\*\*-1) to run time, when, if executed, they produce DIVZERO errors. This is consistent with YottaDB's treatment of divide by zero (/0) and integer divide by zero (\0).

Note that except when it evaluates to match a recent and still cached object, XECUTE induces a mini-compilation with all detected errors deferred to run time in order to prevent double reporting. Previously, the modulo divide by zero produced a segmentation violation (SIG11) when executed, and the negative exponentiation of zero typically failed in compilation with a GTMASSERT.

* <a name="GTM-9371"></a> The following issue in the GT.M V7.0-000 release notes was previously addressed in YottaDB release r1.34 ([YDB#828](https://gitlab.com/YottaDB/DB/YDB/-/issues/828)). The following release note is included here for completeness.

YottaDB appropriately compiles Boolean expressions of the form literal-operator-<2nd operand> where a plus-sign (+) precedes the literal, the operator is non-relational (AND/OR/NAND/NOR), and the second operand contains a global reference or indirection.

The literal may not appear obvious because it may exist as an expression of literal elements, which the YottaDB compiler reduces to a single literal. Also, the global and/or indirection may appear anywhere in the right-hand operand or in later in an argument containing the Boolean.

Previously, such a construct tended to fail with a GTMASSERT, looping or out of memory during compilation. Note that XECUTE might make the failure appear while executing at run time.

#### System Administration

* <a name="GTM-6952"></a> All command line arguments across YottaDB utilities now accept hexadecimal numbers as inputs in addition to decimal numbers. Hexadecimal numbers must be prefixed with a 0x or 0X and the digits above nine are case insensitive.

Furthermore, all the command line arguments across YottaDB utilities which previously accepted only hexadecimal number inputs continue to accept only hexadecimal number inputs. However, YottaDB is deprecating the use of HEX values without a leading '0x' or '0X' and may cease to support them in the future.

* <a name="GTM-8262"></a> The installation script checks dependencies if requested to run YottaDB in UTF-8 mode, prints proper error messages and exits if any of the dependencies are not met. Previously it did not provide explicit error messages if dependencies were not met.

* <a name="GTM-8263"></a> The ydbinstall script wrapper (ydbinstall.sh) picks up the default ICU version of the system. Previously, it accepted ICU version specification with a −−utf8 option. The manual ydbinstall script no longer prompts for non-default ICU versions.

When requesting UTF-8 support, if the dependencies are not met, the installation exits with an appropriate error. Previously, the installer accepted and prompted for non-default ICU versions and continued installation without UTF-8 support if the dependencies have not been met.

* <a name="GTM-8288"></a> The ydbinstall script explicitly removes setgid, setuid and sticky bits from the target installation directory if it exists with those bits set. Previously, the sub-directories created by the installation script inappropriately carried the setgid settings.

* <a name="GTM-8517"></a> The ydbinstall script records ownership, permissions and openssh sha256 checksum values of all installed files for future reference in $ydb_dist/install_permissions.log and $ydb_dist/install_sha256_checksum.log.

* <a name="GTM-9268"></a> YottaDB issues a JNLFLUSHLOG error message to the system log when the journal buffer flushing fails. Previously, YottaDB issued only the BUFFLUFAILED error message.

* <a name="GTM-9302"></a> The MUPIP REPLICATE SOURCE SHOWBACKLOG reports the sequence number acknowledged from the Receiver Server. Previously, MUPIP REPLICATE SOURCE SHOWBACKLOG reported information only from the Source Server which did not include an acknowledgement of updates that have reached the Receiver Server.

The acknowledged sequence number from the replicating (secondary) instance can also be accessed from the originating (primary) instance using the gtmsource_local_struct.heartbeat_jnl_seqno field of the %PEEKBYNAME utility function. For an example of the gtmsource_local_struct.heartbeat_jnl_seqno field, refer to the *Additional Information* section below.

* <a name="GTM-9343"></a> MUPIP FTOK JNLPOOL or RECVPOOL use the entire replication instance file path. Previously, it assumed the current working directory of the process.

* <a name="GTM-9344"></a> MUPIP FREEZE ONLINE institutes the FREEZE on all regions in parallel to produce a consistent snapshot for a multiple-region database when ASYNCIO is ON. Previously, MUPIP FREEZE ONLINE with ASYNCIO froze each region serially which could result in an inconsistent snapshot with the final regions have sequence numbers higher than the first regions.

* <a name="GTM-9346"></a> During soft tries connection attempts, the Source Server waits for the specified soft tries period when it encounters a network error for the host name specified with the SECONDARY qualifier. Previously, the Source Server skipped waiting which caused the Source Server log to record such attempts in quick succession.

* <a name="GTM-9349"></a> YottaDB installation script does not attempt to install the deprecated semstat2 and ftok utilities. When using existing directories the installation script force creates new soft links in utf8 directories if the destination files exist.

Previously, creating such softlinks would throw a File exists error. When the installation fails, execute permissions of only the installed files are removed, previously execute permissions of even the directories were inappropriately removed. YottaDB strongly recommends against installing YottaDB into existing directories. YottaDB strongly recommends installing YottaDB in a new directory rather than re-using an existing directory.

* <a name="GTM-9358"></a>  MUPIP REPLICATE SOURCE ZEROBACKLOG SHUTDOWN clears all IPC resources associated with the replication instance file. Also, MUPIP REPLICATE SOURCE ZEROBACKLOG SHUTDOWN TIMEOUT displays Initiating ZEROBACKLOG shutdown operation. Waiting for up to N seconds for backlog to clear at the start of the command.

Previously, MUPIP REPLICATE SOURCE ZEROBACKLOG SHUTDOWN did not clear the IPC resources associated with the replication instance file and did not display the Initiating ZEROBACKLOG shutdown operation message.

* <a name="GTM-9361"></a> MUPIP REPLICATE SOURCE SHUTDOWN ZEROBACKLOG accounts for the acknowledged sequence number received from the Receiver Server while determining whether the Source Server has a backlog. It returns the REPL0BACKLOG message when there is no backlog and the Receiver Server has acknowledged all sequence number updates. Alternatively, it returns REPLBACKLOG when there is a backlog, unacknowledged updates, or the timeout expires.

Previously it did not include the sequence number acknowledgement from the Receiver Server to confirm that there is no backlog which could lead to in-flight updates and generate a lost transaction file during a switchover.

To reduce the possibility of a generating a lost transaction file during switchover, YottaDB recommends using SOURCE SHUTDOWN ZEROBACKLOG and a check for the REPL0BACKLOG message during a planned switchover to help confirm that there is no backlog, and all in-flight updates have reached the Receiver Pool.

The maximum TIMEOUT specified with MUPIP REPICATE SOURCE SHUTDOWN (with or without ZEROBACKLOG) is 3600 seconds (1 hour). MUPIP produces the INVSHUTDOWN error if the TIMEOUT is higher than the maximum shutdown timeout. If TIMEOUT is not specified, the default timeout for MUPIP REPLICATE SOURCE SHUTDOWN is 120 seconds.

Previously, the maximum timeout for both these cases was 30 seconds and MUPIP automatically adjusted the timeout downwards to 30 seconds when a higher timeout was specified.

* <a name="GTM-9368"></a> A Source Server shutdown process (started with or without ZEROBACKLOG) exits gracefully with CTRL-C; previously the Source Server did
not respond to CTRL-C during the shutdown process.

#### Other

* <a name="GTM-9357"></a> ^%DSEWRAP, which was previously deprecated, is removed from the release package. Use MUPIP DUMPFHEAD,
%PEEKBYNAME or DSE DUMP FILEHEADER as alternatives to ^%DSEWRAP.

### Additional Information

Here are some examples of the new gtmsource_local_struct.heartbeat_jnl_seqno ^%PEEKBYNAME field.

This routine returns the replication speed, that is the number of seqno updates per second acknowledged by the replicating instance during heartbeat intervals.

Example:

```
replspeed
    ; This routine returns the replication speed, that is the number of seqno updates per second acknowledged by the
    replicating instance during the heartbeat intervals.
    ; Usage: yottadb -run ^replspeed INSTANCE3 20
    ; The second parameter is the sampling size
    set $etrap="write ""REPLSPEED-E-ACKSMPL : unable to fetch sampling data due to "",$zstatus halt "
    set $ztimeout="300:write ""Timeout occurred out after 5 minutes"",! zwrite halt"
    new hrtbtperiod,instance,samplingsize,hrtbts,slot,i,hrtbtdiffs,diff,dump
    set instance=$piece($zcmdline," ",1),slot=0
    set samplingsize=$piece($zcmdline," ",2),hrtbtdiffs=0
    set:$length(samplingsize)=0 samplingsize=10
    if '($length(instance)) write "REPLSPEED-E-ARGS : ",$zcmdline," was specified. This routine requires specifying
    an instance name.",! halt
    for i=0:1:15 set instname=$$^%PEEKBYNAME("gtmsource_local_struct.secondary_instname",i),instname=$piece(instname,$char(0),1) set:(instname=instance) slot=i
    ; capture heartbeat_jnl_seqno samplingsize times. Wait for hrtbtperiod after every capture of heartbeat_jnl_seqno
    set hrtbtperiod=$piece($$^%PEEKBYNAME("gtmsource_local_struct.connect_parms",slot),",",5)
    for j=1:1:samplingsize do
    . set hrtbts(j)=$$^%PEEKBYNAME("gtmsource_local_struct.heartbeat_jnl_seqno",slot,"I")
    . do:(j>1)
    . . set diff=hrtbts(j)-hrtbts(j-1)
    . . do:(diff<0)
    . . . write "REPLSPEED-E-ACKSEQNO : acknowledgement sequence number received is lower than previous
    acknowledgement seqno."
    . . . set dump="replspeed.dump" open dump use dump zwrite hrtbts close dump
    . . . halt
    . . set hrtbtdiffs=hrtbtdiffs+diff
    . hang hrtbtperiod
    set hrtbtdiffs=hrtbtdiffs/samplingsize
    write $justify(hrtbtdiffs/hrtbtperiod,0,0)
    quit
```

Use the following example confirm that an update corresponding to a Source Server sequence number has reached the Receiver Pool. This
can be used as a tool to help confirm that there are no in-flight updates.

Example:

```
waitforhrtbt
    ; This routine returns 0 when the acknowledged seqno from the specified instance matches or exceeds the specified
    seqno
    ; If there is no confirmation (network issues etc) from the Receiver Server for 300 seconds, this routines
    returns 1.
    ; Usage: yottadb -run ^waitforhrtbt <instname> <seqnocheckpoint>
    set $etrap="write ""WAITFORHRTBT-E-ERROR, Error occurred while waiting for ackseqno confirmation due to "",$zstatus halt "
    new heartbeatseqno,hangduration,instance,checkseqno,i,instname,slot
    ; set $ztimeout to align with the REPLALERT threshold for the test system
    set $ztimeout="900:write 1 halt"
    set hangduration=1,slot=""
    set instance=$piece($zcmdline," ",1)
    set checkseqno=$piece($zcmdline," ",2)
    if '($length(instance)&$length(checkseqno)) write "WAITFORHRTBT-E-ARGS : ",$zcmdline," was specified. This
    routine requires specifying instance name and seqno.",! halt
    for i=0:1:15 set instname=$$^%PEEKBYNAME("gtmsource_local_struct.secondary_instname",i),instname=$piece(instname,$char(0),1) set:(instname=instance) slot=i
    if '($length(slot)) write "WAITFORHRTBT-E-INSTNOMATCH : No matching instance name for ",instance,! halt
    for do
    . set heartbeatseqno=$$^%PEEKBYNAME("gtmsource_local_struct.heartbeat_jnl_seqno",slot,"I")-1
    . if (heartbeatseqno>=checkseqno) write 0 halt
    . hang hangduration
    quit
```

Example:

```
waitforseqnosync
    ; This routine returns 0 when there the sequence numbers of the Source Server and the acknowledged sequence
    number from the Receiver Server is the same.
    ; It returns a non-zero value when there is no confirmation of the receipt of the latest seqno from the secondary
    even when there is no backlog.
    ; If there is no confirmation (network issues etc) from the Receiver Server for 150 seconds, this routines
    returns 1.
    ; Usage: yottadb -run ^waitforseqnosync <instname>
    set $etrap="write ""WAITFORSEQNOSYNC-E-SRCBACKLOG : unable to get current Source Server backlog and seqno updates
    status due to "",$zstatus halt "
    new readseqno,heartbeatseqno,instance,i,instname,slot,hrtbtperiod
    set $ztimeout="150:write 1 halt"
    set slot=""
    ; hrtbtperiod: the heartbeat period (the fifth parameter of -CONNECTPARAMS)
    set instance=$zcmdline
    if '$length(instance) write "WAITFORSEQNOSYNC-E-ARGS : This routine requires specifying an instance name.",! halt
    for i=0:1:15 set instname=$$^%PEEKBYNAME("gtmsource_local_struct.secondary_instname",i),instname=$piece(instname,
    char(0),1) set:(instname=instance) slot=i
    if '($length(slot)) write "WAITFORSEQNOSYNC-E-INSTNOMATCH : No matching instance name for ",instance halt
    set hrtbtperiod=$piece($$^%PEEKBYNAME("gtmsource_local_struct.connect_parms",slot),",",5)
    for do
    . set seqno=$$^%PEEKBYNAME("jnlpool_ctl_struct.jnl_seqno","","I")
    . set readseqno=$$^%PEEKBYNAME("gtmsource_local_struct.read_jnl_seqno",i,"I")
    . set heartbeatseqno=$$^%PEEKBYNAME("gtmsource_local_struct.heartbeat_jnl_seqno",i,"I")
    . if (seqno=readseqno=heartbeatseqno) write 0 halt
    . hang hrtbtperiod
    quit
```

### Messages

#### New messages

The following new messages have been added:

**GTMCURUNSUPP**, The requested operation is unsupported in this version of YottaDB

All YottaDB Components Error: YottaDB tried to perform an operation that is unsupported in the current version. Currently this is only thrown by YottaDB when trying to perform an upgrade/downgrade operation.

Action: YottaDB does not support upgrade/downgrade between r1.x and r2.x databases.

**HEX64ERR**, Error: cannot convert VVVV value to 64 bit hexadecimal number

All YottaDB Components Error: The entered value does not correspond to a valid hexadecimal representation of a 64-bit number.

Action: Enter an appropriate hexadecimal value starting with 0X.

**HEXERR**, Error: cannot convert VVVV value to hexadecimal number

All YottaDB Components Error: The entered value does not correspond to a valid hexadecimal number.

Action: Enter an appropriate hexadecimal value starting with 0X.

**INVSHUTDOWN**, Shutdown timeout should be from 0 to 3600 seconds

MUPIP Error: This error appears when the TIMEOUT specified with SOURCE SHUTDOWN exceeds 3600 seconds (1 hour).

Action: Specify TIMEOUT between 0 to 3600 seconds

**NUM64ERR**, Error: cannot convert VVVV value to 64 bit decimal or hexadecimal number

All YottaDB Components Error: The entered value does not correspond to a valid decimal or hexidecimal representation of a 64-bit number.

Action: Enter an appropriate decimal value or hexadecimal value starting with 0X.

**NUMERR**, Error: cannot convert VVVV value to 64 bit decimal or hexadecimal number

All YottaDB Components Error: The entered value does not correspond to a valid decimal number or hexadecimal number.

Action: Enter an appropriate decimal value or hexadecimal value starting with 0X.

**REPL0BACKLOG**, Total backlog for the specified replicating instance(s) is 0

MUPIP Success: This message indicates a successful ZEROBACKLOG SHUTDOWN. It means that there was no backlog for the specified replicating instance(s), no inflight updates, and all updates were successfully acknowledged by the Receiver Server.

Action: None.

**REPLBACKLOG**, Timeout occurred while there was a backlog

MUPIP Error: This error occurs when the TIMEOUT specified with SOURCE ZEROBACKLOG SHUTDOWN expires and there is a either a backlog and/or there was a failure to receive an acknowledgement of the latest sequence number on the Source Server by the Receiver Server. If REPLNORESP also accompanies this error, it means that the Source Server did not receive a response from the Receiver Server acknowledging sequence number confirmation.

Action: This error means that the ZEROBACKLOG checks did not pass. Restart the Source Server to clear any backlog. The presence of a REPL0BACKLOG success message for ZEROBACKLOG SHUTDOWN confirms that there are no inflight updates and all updates are acknowledged by the Receiver Server.

**REPLNORESP**, No sequence number confirmation from the replicating instance xxxx after waiting for nnnn second(s)

MUPIP Warning: This message appears when the Source Server fails to receive a response from the Receiver Server during a ZEROBACKLOG SHUTDOWN. The presence of a REPLNORESP indicates that a ZEROBACKLOG SHUTDOWN check failed. This warning is accompanied by the REPLBACKLOG error message.

Action: This warning means that the ZEROBACKLOG checks did not pass. Restart the Source Server to clear any backlog. The presence of a REPL0BACKLOG success message for ZEROBACKLOG SHUTDOWN confirms that there are no inflight updates and all updates are acknowledged by the Receiver Server.

**SHUT2QUICK**, Shutdown timeout ssss shorter than the heartbeat period SSSS; cannot confirm the backlog at the replicating instance iiii

MUPIP Warning: This warning appears when the TIMEOUT=ssss specified with ZEROBACKLOG is less than the heartbeat period SSSS (the fifth parameter of CONNECTPARAMS). If TIMEOUT is less than the heartbeat period, ZEROBACKLOG cannot confirm that there is zero backlog as it cannot obtain the acknowledgement of the latest sequence number from the Receiver Server of instance iiii in such a short time.

Action: Specify TIMEOUT that is larger or equal to the heartbeat period.

**UNUM64ERR**, Error: cannot convert VVVV value to 64 bit unsigned decimal or hexadecimal number

All YottaDB Components Error: The entered value does not correspond to a valid unsigned decimal or hexidecimal representation of a 64-bit number.

Action: Enter an appropriate decimal value or hexadecimal value starting with 0X.

**VIEWREGLIST**, $VIEW() only handles the first region subparameter

Run Time Warning: $VIEW() with a region subparameter only operates on a single region. This differs from the VIEW command which has similar arguments and accepts region-lists for regions. This error is a warning and the function attempts to act on the first region.

Action: If the requirement is for multiple regions, use multiple $VIEW() invocations, perhaps in a loop.

**WCSFLUFAIL**, Error flushing buffers -- called from module MMMM at line LLLL

All YottaDB Components Error: This indicates that an attempt to flush a buffer to disk failed.

Action: For a BG database file, this means that a process attempting to flush modified global buffers to disk encountered an error. EEEE is the error it encountered, for database file DDDD when attempting to flush the blocks for database transaction number TTTT. This is usually accompanied by other messages that can together help provide more information and context. If you need further assistance and have purchased support, contact your YottaDB support channel.

Action: Refer to the description of error EEEE and take appropriate action.

#### Revised messages

The following messages have been modified for correctness and/or clarity:

**BOVTMGTEOVTM**, Journal file xxxx has beginning timestamp aaaa greater than end timestamp bbbb

MUPIP Error: This indicates that the beginning time stamp aaaa of the journal file xxxx is greater than the ending timestamp bbbb. This could be due to something that changed the system time,such as a daylight savings time change or a testing time reset, while YottaDB was journaling. YottaDB recommends against changing system time during YottaDB Run-time as a matter of course, as this disruption is not heavily tested.

Action: Changing system time during YottaDB run-time is not allowed. Contact your YottaDB support channel for further assistance.

**DBADDRANGE**, Database file rrrr element location aaaa: control vvvv was outside qqqq range bbbb to tttt

Run Time Information: This indicates that a process was abnormally terminated while updating the database. Database control structures may be damaged.

Action: This typically indicates a process terminated abnormally while updating the database. YottaDB often fixes such an error unless there is a serious problem causing this error. If YottaDB cannot correct the issue, the accompanying messages should expand on the situation. You are advised to report such a database error to the group responsible for database integrity at your operation.

**DBADDRANGE8**, Database file rrrr element location aaaa: control vvvv was outside qqqq range bbbb to tttt

Run Time Error: This indicates a database control structure for database region rrrr at memory location aaaa contains a value vvvv outside range bbbb to tttt for quantity qqqq.This message is the same as a DBADDRANGE message except that vvvv, bbbb and tttt are 8-byte quantities (as opposed to 4-byte quantitites in DBADDRANGE).

Action: This typically indicates a process terminated abnormally while updating the database. YottaDB often fixes such an error unless there is a serious problem causing this error. If YottaDB cannot correct the issue, the accompanying messages should expand on the situation; and you should report such database error to the group responsible for database integrity at your operation.

### Legal Stuff

Copyright © 2023 YottaDB LLC

Permission is granted to copy, distribute and/or modify this document under the terms of the GNU Free Documentation License, Version 1.3 or any later version published by the Free Software Foundation; with no Invariant Sections, no Front-Cover Texts and no Back-Cover Texts.

YottaDB® and Octo® are registered trademarks of YottaDB LLC. GT.M™ is a trademark of Fidelity National Information Services, Inc.
Other trademarks belong to their respective owners.

This document contains a description of YottaDB and the operating instructions pertaining to the various functions that comprise the software. This document does not contain any commitment of YottaDB LLC. YottaDB LLC believes the information in this publication is accurate as of its publication date; such information is subject to change without notice. YottaDB LLC is not responsible for any errors or defects.
