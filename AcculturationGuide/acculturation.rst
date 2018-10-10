
.. index::
   Acculturation Workshop

====================================
Acculturation Workshop
====================================

Welcome to the YottaDB Acculturation Workshop!

Copyright © 2017 YottaDB LLC and/or its subsidiaries. All Rights Reserved.

Permission is granted to copy, distribute and/or modify this document under the terms of the `GNU Free Documentation License <http://www.gnu.org/licenses/fdl.txt>`_, Version 1.3 or any later version published by the Free Software Foundation; with no Invariant Sections, no Front-Cover Texts and no Back-Cover Texts.

Copyright © 2014 Fidelity National Information Services, Inc. and/or its subsidiaries. All Rights Reserved.

YottaDB® is a trademark of YottaDB LLC. GT.M™ is a trademark of Fidelity National Information Services, Inc. Other trademarks are the property of their respective owners.

.. contents:: Navigation

-----------------------------------
Acculturation Workshop Expectations
-----------------------------------

The Acculturation Workshop is a hands-on “boot camp” for those interested in the configuration, administration and operation of applications on YottaDB. This file is the script (or workbook) for the workshop, consisting of the exercises below.

At the end of these exercises, you will have a basic working knowledge of the essential aspects of YottaDB Administration and Operation. While this workshop alone will not by any means make you a YottaDB expert, the basic working knowledge it will give you will help you to quickly understand the concepts explained in the user documentation and put you on the path to becoming an expert.

The workshop is not a course in M programming. Familiarity with Linux® (or at least UNIX®) will allow you to move faster through the material, but is not absolutely required. If you have no experience whatsoever with Linux or UNIX, supplementary tutorial material on the side will increase your level of comfort.

As the differences between YottaDB and other M implementations are more in the area of configuration and systems administration rather than M language features, the former topic is the major thrust of the workshop.

-------------
YottaDB
-------------

`YottaDB <http://yottadb.com>`_ is an implementation of the ISO standard scripting & application development language M, commonly known as `MUMPS <https://en.wikipedia.org/wiki/MUMPS>`_, developed and released by YottaDB LLC. The codebase YottaDB is built on is the most widely used M implementation in banking and finance, including several of the largest real time core processing systems that are live at any bank anywhere in the world. The codebase is also increasingly used in healthcare. The implementation of YottaDB on the GNU/Linux operating system on industry standard x86_64 architecture and Raspberry Pi hardware is the M implementation used for the FOSS (Free/Open Source Software) stack for `VistA <http://worldvista.org/AboutVistA>`_.

YottaDB is architected with the following objectives:

- Without compromise, the robustness, security and integrity of the information entrusted to it.
- Open architecture, with easy, standards-based access to the information in the database.
- Continuity of business – YottaDB has unique functionality for the deployment of mission-critical applications that must be available 24 hours a day, 365 days a year, with no down time even for planned events.
- Throughput, performance and scalability to meet the needs of the largest institutions in the world.

Free support for YottaDB is available from the community on `various mailing lists and forums <https://yottadb.com/resources/communities/>`_. Support for YottaDB with assured service levels is available from YottaDB LLC on a commercial basis.

YottaDB provides:

- Full `ACID (Atomic, Consistent, Isolated, Durable) <https://en.wikipedia.org/wiki/ACID>`_ transaction semantics
- Throughput that scales to the needs of enterprise-wide applications
- Unique functionality for creating logical multi-site configurations for mission critical applications that must always be available - including upgrades that involve changes to the database schema.

With the exception of Structured System Variable Names (SSVNs), YottaDB mostly implements ISO standard M (ISO/IEC 11756:1999), including a full implementation of transaction processing (TP) that provides ACID (Atomic, Consistent, Isolated, Durable) transactions. As with any M implementation, there are extensions. Commands and variables that start with the letter Z, IO parameters and VIEW command parameters are implementation specific.

Despite the fact that the dialect of M implemented by YottaDB has much in common with other M implementations, operationally, YottaDB is unlike any other M implementation.

This Acculturation Workshop is largely based on YottaDB r1.22, but any more recent version of YottaDB should also work well.

---------
Packaging
---------

**The exercises in this workshop are carried out by booting guest virtual machines (also called software appliances) on your host computer**. Think of a virtual machine as a “computer within a computer”. A guest virtual machine can run a different operating system from that of the host computer. The host computer might itself run Linux, Windows, OS X, or any other operating system and the guest can run Linux with YottaDB as well as other applications. "Emulation" or "virtualization" software helps you set up a guest system on a host computer. On the host computer, the disk images of the Acculturation Workshop guide look like ordinary files in the file system.

**Make sure to download the virtual machine before you begin the exercises**

+++++
Linux
+++++

Linux is the common name for the GNU/Linux operating system, consisting of the GNU utilities and libraries on the Linux kernel, available across the broadest range of hardware of any operating system. It is most widely used on industry standard architecture x86_64 hardware (i.e., based on popular CPUs from Intel, AMD and other vendors), and is increasingly popular around the world for applications that include embedded computing (appliances); personal desktops; file, print & web servers; supercomputing; and to deploy mission critical software. Linux is the operating system for the VistA FOSS stack.

Free support for Linux is available on numerous mailing lists and electronic forums. Commercial support is widely available from multiple vendors.

The Acculturation Workshop is on a virtual machine which starts with the disk image of a minimal `Ubuntu <https://www.ubuntu.com/>`_ 12.04 LTS (“Precise Pangolin”) Linux distribution, with `additional documentation resources <https://help.ubuntu.com/>`_.

For other documentation resources, although dated, `Linux: Rute User's Tutorial and Exposition <https://rlworkman.net/howtos/rute/>`_ is still a very useful tutorial for anyone getting started with Linux. The `Debian Project <https://www.debian.org/>`_ maintains a `page of books <https://www.debian.org/doc/books>`_ on Linux. The `Debian Wiki <https://wiki.debian.org/>`_ has useful reference information and having a paper copy of the `Debian Reference Card <https://www.debian.org/doc/manuals/refcard/>`_ (available in several languages) would be useful for anyone not entirely comfortable with Linux.

+++++++++++++++++++++++++++++++
Control of the Keyboard & Mouse
+++++++++++++++++++++++++++++++

When you boot a guest virtual machine, booting it “headless” (i.e., without a console - no keyboard and mouse attached), means that the host (you) always has control of the keyboard and mouse. If it is not headless, ownership of the keyboard or mouse may need to toggle between the host and guest. The software you use for virtualization determines how to transfer control.

With QEMU/kvm virtual machines, use the Ctrl-Alt key combination to toggle ownership of the mouse and keyboard between host and guest. Even if the host owns the keyboard, you can type into the guest console when it has focus, but not the other way around. Mouse clicks are visible to only the machine, host or guest, that owns the mouse.

++++++++++++++++++
Terminal Emulation
++++++++++++++++++

Even when running with a console, we recommend that you boot and minimize the virtual machine, and connect to your virtual machines with terminal sessions from a terminal emulator. On Windows, you can use a terminal emulator such as `putty <https://www.chiark.greenend.org.uk/~sgtatham/putty/>`_. Linux distributions include terminal emulation. Terminal emulators are available for, and frequently included with, other computer platforms.

For the Unicode exercises, you will either need a terminal emulator that can be switched between UTF-8 and single-byte characters, or you will need two emulators. If you intend to use languages that write right to left, you will need a terminal emulator with bidirectional capabilities.

+++++++++++++++
Virtualization
+++++++++++++++

The software used for virtualization and used for the examples in this document is `QEMU <https://www.qemu.org/>`_ which is available for many popular computing platforms, including Linux, Windows, and more. Instructions are provided below for Windows and Linux hosts. On Linux hosts, `kvm <https://www.linux-kvm.org/page/Main_Page>`_ may be the preferred choice (kvm and QEMU provide a very similar user interface - kvm is a fork of QEMU focusing on the kernel module). `VirtualBox <https://www.virtualbox.org/>`_ (by Oracle) is another popular FOSS (Free and Open Source Software) virtualization application. There is also proprietary virtualization software. Even though the examples used below are QEMU, you should be able to use the virtualization software of your choice.

You are at liberty to use a Linux host, or any Linux virtual machine of your choice. The virtual machine used to develop the exercises is a 64-bit Ubuntu Linux 16.04 LTS, using QEMU on a 64-bit Ubuntu Linux 14.10 host.

++++++++++++
Disk Formats
++++++++++++

The Acculturation Workshop is distributed as a `vmdk format <https://en.wikipedia.org/wiki/VMDK>`_ disk image file that should work with most virtualization software, both FOSS and proprietary.

+++++++++++++++++++++++++++
Virtual Machine Download
+++++++++++++++++++++++++++

Download: (`ubuntu-16.04_yottadbworkshop10.vmdk <https://docs.yottadb.com/ubuntu-16.04_yottadbworkshop10.zip>`_) 


+++++++++++++++++++++++++++++
Virtual Machine Configuration
+++++++++++++++++++++++++++++

Virtualization software configures virtual machines either with their own IP addresses where the network connection (wired or wireless) of the host has multiple IP addresses, or, more commonly - using network address translation (NAT). In the latter case, the network connection of the host has one IP address that it presents to the outside world, but each virtual machine has an IP address in a subnet within the host (the host acts just like a home wifi access point / router).

You will need to configure your virtual machine for outbound and inbound network access. While outbound access should require no configuration to work with either type of virtual machine network connection, inbound network access in a NAT'd environment will require a TCP port on the host to be forwarded to the virtual machine for each port at which a service on the virtual machine needs to respond. For example, each virtual machine has a secure shell (ssh) server listening at port 22 for incoming connections, and you might choose to forward port 2222 on the host to port 22 on your virtual machine.

Refer to the user documentation for your virtualization software to set up virtual machine networking.

Using qemu-system-x86_64 on a Linux host, the following boots the vmdk image with port 2222 on the host forwarded to port 22 on the guest for ssh sessions.

.. parsed-literal::
   
   qemu-system-x86_64 -enable-kvm -cpu host -m 256 -net nic -net user,hostfwd=tcp::2222-:22 -hda ubuntu-16.04_yottadbworkshop10.vmdk

Using kvm on a Linux host, the following command boots the vmdk image with port 2222 on the host forwarded to port 22 on the guest for ssh sessions:

.. parsed-literal::

    kvm -enable-kvm -cpu host -m 256 -display none -net nic -net user,hostfwd=tcp::2222-:22 -hda ubuntu-16.04_yottadbworkshop10.vmdk

   
+++++++++++
Legal Stuff
+++++++++++

YottaDB® is owned and copyrighted by `YottaDB LLC <http://yottadb.com/>`_ and is available for the GNU/Linux platforms on x86_64 and Raspberry Pi hardware under the terms of the `GNU Affero General Public License Version 3 <http://www.gnu.org/licenses/agpl.txt>`_ . Source and binary can be downloaded from the `YottaDB project page at GitLab <https://gitlab.com/YottaDB/DB/YDB>`_ .

GT.M™ is owned and copyrighted by `Fidelity Information Services, LLC <http://www.fisglobal.com/>`_, and is available for the x86_64 GNU/Linux platform under the terms of the `GNU Affero General Public License version 3 <http://www.gnu.org/licenses/agpl.txt>`_. Source and binary can be downloaded from the `GT.M project page at Source Forge <http://sourceforge.net/projects/fis-gtm>`_ .

The core of VistA (so called `FOIA VistA <https://www.osehra.org/content/foia-vista>`_) is in the public domain through the US Freedom of Information Act. Source and object code are available on one of the hard drive images. As noted above, no understanding of VistA itself is required or assumed for the workshop.

The Linux kernel, GNU utilities, the WorldVistA EHR extensions to VistA and all other software on the CD-ROM and hard drive images are FOSS and available under their respective FOSS licenses. Copyrights and trademarks of all content are hereby acknowledged as being held by their owners.

---------------
Getting Started
---------------

With a terminal emulator, initiate an ssh connection to port 2222 on localhost and login with userid 'yottadbuser' and password 'YottaDB Rocks!' (including a space and an exclamation point). For example, on Linux, you can use the command: ``ssh -p 2222 -X yottadbuser@localhost`` to connect as user yottadbuser to port 2222 on the host which is forwarded to port 22 on the guest.

.. parsed-literal::

 $ ssh -p 2222 -X yottadbuser@localhost
 Welcome to Ubuntu 16.04.1 LTS (GNU/Linux 3.13.0-39-generic x86_64)
 * Documentation:  https://help.ubuntu.com/
 System information as of Mon Jan 22 18:08:22 EST 2018
 System load: 0.48              Memory usage: 52%   Processes:       81
 Usage of /:  12.3% of 9.99GB   Swap usage:   0%    Users logged in: 0

 Graph this data and manage this system at: https://landscape.canonical.com/
 Last login: Fri Jan 19 16:51:37 2018 from 10.0.2.2
 yottadbuser@yottadbworkshop:~$ 


++++++++++++++++
Install YottaDB
++++++++++++++++

- Get the YottaDB install script: ``wget https://gitlab.com/YottaDB/DB/YDB/raw/master/sr_unix/ydbinstall.sh``
- Make it executable: ``chmod +x ydbinstall.sh``
- Run it where you want it installed (omit the --verbose option for less output): The below command installs YottaDB in /usr/local/lib/. The rest of the document assumes installation in this directory.
  
 .. parsed-literal::
     sudo ./ydbinstall.sh --utf8 default --verbose

- You can specify a choice of directory if you desire: The below command installs YottaDB in /opt/yottadb/

  .. parsed-literal::
     sudo ./ydbinstall.sh --installdir /opt/yottadb/ --utf8 default --verbose

+++++++++++++++++
Run YottaDB
+++++++++++++++++

**Default Environment**

YottaDB needs several environment variables to be set up. YottaDB provides a script that sets up reasonable defaults and allows you to start using YottaDB immediately. When you set up environments in YottaDB, you can set up your own scripting, but the default is a good place to start. You can source the ydb_env_set file in the directory in which you have installed YottaDB (e.g, /usr/local/lib/yottadb/r122/ydb_env_set) to set up reasonable defaults or simply execute the script ydb to execute YottaDB. A default environment is created only if it does not exist already.

.. parsed-literal::
   yottadbuser@yottadbworkshop:~$ source /usr/local/lib/yottadb/r122/ydb_env_set

Or you can run the script that runs YottaDB as follows:

.. parsed-literal::
   yottadbuser@yottadbworkshop:~$ ydb

Now you are in the “direct mode” where you can execute commands interactively. For example, type:

.. parsed-literal::
   YDB>set ^Animal("Mammal")="Bear"

   YDB>set ^Animal("Bird")="Eagle"

   YDB>set ^Animal("Insect")="Butterfly"

The commands perform database updates, which are shared between processes. You can see this if you start a new terminal session, start a new YottaDB process and ask it to dump the “global variable” (a key-value association) ^Animal. The halt command takes you back to the Linux shell.

.. parsed-literal::
   YDB>zwrite ^Animal
   ^Animal("Mammal")="Bear"
   ^Animal("Bird")="Eagle"
   ^Animal("Insect")="Butterfly"
   YDB>halt
   yottadbuser@yottadbworkshop:~$

The operation of YottaDB is controlled by a number of environment variables. In our exercise, the ydb_env_set script automatically sets a number of environment variables:

.. parsed-literal::
   yottadbuser@yottadbworkshop:~$ env | grep ^ydb
   ydb_retention=42
   ydb_log=/tmp/yottadb/r1.22_x86_64
   ydb_gbldir=/home/yottadbuser/.yottadb/r1.22_x86_64/g/yottadb.gld
   ydb_etrap=Write:(0=$STACK) "Error occurred: ",$ZStatus,!
   gtmdir=/home/yottadbuser/.yottadb
   ydb_rel=r1.22_x86_64
   ydb_routines=/home/yottadbuser/.yottadb/r1.22_x86_64/o*(/home/yottadbuser/.yottadb/r1.22_x86_64/r /home/yottadbuser/.yottadb/r) /usr/local/lib/yottadb/r1.22_x86_64/libyottadbutil.so
   ydb_repl_instance=/home/yottadbuser/.yottadb/r1.22_x86_64/g/yottadb.repl
   ydb_tmp=/tmp/yottadb/r1.22_x86_64
   ydb_dist=/usr/local/lib/yottadb/r1.22_x86_64/
   yottadbuser@yottadbworkshop:~$ 

YottaDB databases can be configured so that they can be recovered after a system crash. Simulate a crash by either clicking on the “X” in the top right corner of your virtual machine console window to instantly “power down” your virtual machine, or, if you started it headless, perform a hard power-down using a command on the host (in the case of virtualization using qemu/kvm on Linux, a kill -9 of the virtual machine process). Then reboot the virtual machine, run ydb and use a zwrite ^Animal command to confirm that the data in the database is still intact.

The tree program shows the default environment YottaDB creates in your home directory. 

.. parsed-literal::
   yottadbuser@yottadbworkshop:~$ tree .yottadb/
   .yottadb/
   ├── r
   └── r1.22_x86_64
       ├── g
       │ ├── yottadb.dat
       │ ├── yottadb.gld
       │ ├── yottadb.mjl
       │ ├── yottadb.mjl_2018317131528
       │ ├── yottadb.mjl_2018317131909
       │ └── yottadb.mjl_2018317134042
       ├── o
       │ └── utf8
       └── r

    6 directories, 6 files
    yottadbuser@yottadbworkshop:~$ 

Note that you may have to install the program 'tree' before running the above command, and make sure you provide the correct path to the .yottadb directory (now and in the future). We will get into the environment in more detail below.

**UTF-8 Mode**

With YottaDB, you can write applications that implement international character sets using Unicode or ISO/IEC-10646 (the two standards track each other). Connect to the virtual machine with your terminal emulator configured to support the UTF-8 character set. Execute the following commands (the non-printable characters may look different on your session from the screen here, depending on how your terminal emulator renders them):

.. parsed-literal::
   yottadbuser@yottadbworkshop:~$ source /usr/local/lib/yottadb/r122/ydb_env_unset
   yottadbuser@yottadbworkshop:~$ export ydb_chset=UTF-8 LC_CTYPE=en_US.utf8
   yottadbuser@yottadbworkshop:~$ source /usr/local/lib/yottadb/r122/ydb_env_set
   yottadbuser@yottadbworkshop:~$ ydb
   YDB>write $zchset
   UTF-8
   YDB>for i=1040:16:1072 write ! for j=0:1:15 write $char(i+j)," "

   А Б В Г Д Е Ж З И Й К Л М Н О П
   Р С Т У Ф Х Ц Ч Ш Щ Ъ Ы Ь Э Ю Я
   а б в г д е ж з и й к л м н о п
   YDB>

Note that Unicode support requires additional infrastructure, such as Unicode enabled terminal emulators and is likely to require custom collation modules to be written to ensure that strings such as names are sorted in linguistically and culturally correct order.

In the exercises below, we will set up environments for use with Unicode.

----------
The Basics
----------

To use YottaDB, at a minimum you need:

- `User documentation <https://yottadb.com/resources/documentation/>`_
- To specify the location of YottaDB on your computer in the ydb_dist environment variable
- To provide a search path to routines for a YottaDB process - the ydb_routines environment variable and the `$zroutines intrinsic special variable <https://docs.yottadb.com/ProgrammersGuide/isv.html#zroutines>`_ (or "ISV" - all ISVs are case insensitive, as are YottaDB commands).
- To map its global variables to database files - the ydb_gbldir environment variable and the `$zgbldir ISV <https://docs.yottadb.com/ProgrammersGuide/isv.html#zgbldir>`_ point to a global directory file with the mapping.

**User Documentation**

YottaDB user documentation is organized into Manuals and Release Notes. Current YottaDB documentation is available on the `YottaDB Documentation page <https://yottadb.com/resources/documentation/>`_.

- Each software release has accompanying Release Notes to document changes between that release and its immediate predecessor, as well as release-specific information such as supported platforms. While a software release is frozen for all time, e.g., there will never be another YottaDB r1.22, release notes may be updated from time to time to correct and clarify the information within.
- Manuals are published periodically. The content within manuals is updated frequently, reflecting information that is current as of the latest software release.

**Routines in the File System**

Routines in YottaDB are simply files in the file system; they do not reside in databases. You can edit routines from the YDB> prompt. Start ydb and at the YDB> prompt, type zedit "greeting" and hit ENTER. This starts the vi editor editing the source routine for ^greeting, /home/yottadbuser/.yottadb/r1.22_x86_64/r/greeting.m. Use the five key-sequence ESCAPE:q!ENTER to exit vi without changing the file.

.. note::
  although vi always puts a newline at the end of your file; other editors may not. A YottaDB program file should always end with a newline.

The philosophy of YottaDB is to focus on what it does well, providing a robust, scalable, transaction processing database and a compiler for the M language, and to leverage tools and capabilities of the underlying operating system for the rest. This is powerful because whenever there are enhancements to the underlying operating environment, YottaDB can benefit from them. This can be a little uncomfortable for M programmers migrating to YottaDB, because traditional M implementations carry their environments around with them.

As you saw when executing M commands interactively, even though YottaDB is a true compiler it still provides an interactive direct mode – YottaDB simply compiles and executes each line.

**Exercise - Compiling and Linking**

The purpose of this exercise is to understand compiling and linking routines. Use the command find .yottadb -iname greeting.[mo] to confirm that your default YottaDB environment does not have a program called greeting.

.. parsed-literal::
   yottadbuser@yottadbworkshop:~$ find .yottadb -iname greeting.[mo]
   yottadbuser@yottadbworkshop:~$ 

You can perform the same operation from inside YottaDB:

.. parsed-literal::
   YDB>zsystem "find .yottadb -iname greeting.[mo]"
   YDB>

or

.. parsed-literal::
   YDB>do SILENT^%RSEL("greeting") zwrite %ZR
   %ZR=0

   YDB>

Had there been a routine, the response might look like this:

.. parsed-literal::
   YDB>do SILENT^%RSEL("greeting") zwrite %ZR    
   %ZR=1
   %ZR("greeting")="/home/yottadbuser/.yottadb/r/"

   YDB>

If you are not comfortable with the terse commands of the default vi editor, you can install your preferred editor. Other editors that are installed on the virtual machine are fte, jed, joe and nano. Nano may be the easiest editor for you to use if you are not familiar with any editor included with the virtual machine. In nano, Ctrl-G provides a screen with keyboard shortcuts.

To switch editors:

.. parsed-literal::
   yottadbuser@yottadbworkshop:~$ export EDITOR=`which nano`
   yottadbuser@yottadbworkshop:~$ ydb
   YDB>

Instruct YottaDB to run the routine ^greeting and note that it reports an error:

.. parsed-literal::
   YDB>do ^greeting
   %YDB-E-ZLINKFILE, Error while zlinking "greeting"
   %YDB-E-FILENOTFND, File greeting not found
   YDB>do SILENT^%RSEL("greeting") zwrite %ZR
   %ZR=0
   YDB>

Within YottaDB, use zedit "greeting" to start the editor. Create a simple program that says "Hello!",save it and return to YottaDB. Now notice that the source file exists (you can use the arrow key to recall the previous command within YottaDB) but there is no object file.

.. parsed-literal::
   YDB>do SILENT^%RSEL("greeting") zwrite %ZR
   %ZR=1
   %ZR("greeting")="/home/yottadbuser/.yottadb/r1.22_x86_64/r/"

   YDB>do SILENT^%RSEL("greeting","OBJ") zwrite %ZR
   %ZR=0
   
   YDB>

Now run the program - it runs as expected.

.. parsed-literal::
   YDB>do ^greeting
   Hello!

   YDB>

Now you now also have an object file. YottaDB dynamically, and automatically, compiles the source program into the object program when you execute do ^greeting.

.. parsed-literal::
   YDB>do SILENT^%RSEL("greeting","OBJ") zwrite %ZR
   %ZR=1
   %ZR("greeting")="/home/yottadbuser/.yottadb/r1.22_x86_64/o/"

   YDB>

.. note::
   Since YottaDB is a compiler, it can generate error messages at compile time as well as at run time. Indeed, when compiling an application such as VistA, there may be hundreds of lines of error messages triggered by lines of code that are legal for other M implementations but not for YottaDB. These lines are protected in VistA and are inside conditional statements that are executed only on the appropriate M implementation, so they are nothing to be concerned about.

Let's also get the time stamps of the files; notice that the source code file is older than the object code file:

.. parsed-literal::
   YDB>zsystem "find .yottadb -name greeting.[mo] -exec ls -l {} \;"
   -rw-rw-r-- 1 yottadbuser yottadbuser 1048 Jan 22 10:16 .yottadb/r1.22_x86_64/o/greeting.o
   -rw-rw-r-- 1 yottadbuser yottadbuser 35 Jan 22 10:14 .yottadb/r1.22_x86_64/r/greeting.m

   YDB>

Now edit the program with zedit "greeting" then change it, e.g., make it print "Goodbye!" instead and save it.

Again execute do ^greeting and note that YottaDB still prints "Hello!". This is because YottaDB already has a greeting module linked in its address space, and does not go out every time to check if there is a new version. This is \"`clobber <https://en.wikipedia.org/wiki/Clobbering>`_ protection" and a YottaDB feature.

.. note::
   YottaDB currently provides a mechanism for processes to indicate that instead of explicitly relinking newer versions of routines, they would like to “subscribe” to and automatically execute the latest updated (“published”) object code of routines. Because of this, this example may not work with the latest version of YottaDB.

Execute zLink "greeting" which tells YottaDB to re-link greeting even if it already has one linked in its address space, followed by do ^greeting and note that it now prints "Goodbye!" . Verify that the source file is newer and that YottaDB has created a new object file.

.. parsed-literal::
   YDB>zedit "greeting"

   YDB>do ^greeting
   Hello!

   YDB>zlink "greeting"

   YDB>do ^greeting
   Goodbye!

   YDB>zsystem "find .yottadb -name greeting.[mo] -exec ls -l {} \;"
   -rw-rw-r-- 1 yottadbuser yottadbuser 1048 Jan 22 10:20 .yottadb/r1.22_x86_64/o/greeting.o
   -rw-rw-r-- 1 yottadbuser yottadbuser 35 Jan 22 10:20 .yottadb/r1.22_x86_64/r/greeting.m

   YDB>

.. note::
    To avoid being surprised by running an old version of a routine that you have just edited, it is important to understand how dynamic compilation and linking work on YottaDB.

The $zroutines ISV tells YottaDB where to find routines:

.. parsed-literal::
   YDB>write $zroutines
   /home/yottadbuser/.yottadb/r1.22_x86_64/o*(/home/yottadbuser/.yottadb/r1.22_x86_64/r /home/yottadbuser/.yottadb/r) usr/local/lib/yottadb/r1.22_x86_64/libyottadbutil.so 
   YDB>

At process startup, $zroutines is initialized from the environment variable $ydb_routines, but it can be altered from within the YottaDB process.

.. parsed-literal::
   YDB>set $zroutines=". "_$ztrnlnm("ydb_dist")

   YDB>write $zroutines
   . /usr/local/lib/yottadb/r122
   YDB>write $ztrnlnm("ydb_routines")
   /home/yottadbuser/.yottadb/r1.22_x86_64/o*(/home/yottadbuser/.yottadb/r1.22_x86_64/r /home/yottadbuser/.yottadb/r) /usr/local/lib/yottadb/r1.22_x86_64/libyottadbutil.so
   YDB>

The ZEDIT command always puts new routines in the first source directory in the search path. Use it to create a new routine to print the current date and time at the Universal Time Coordinate. After the change to $zroutines above, notice how a newly created program and object file are created in the current directory (.).

.. parsed-literal::
   YDB>zedit "UTC"
   YDB>zprint ^UTC
   UTC     zsystem "TZ=UTC date"
           quit

   YDB>do ^UTC
   Mon Jan 22 10:40:01 UTC 2018

   YDB>zsystem "find . -name UTC\* -exec ls -l {} \;"
   -rw-rw-r-- 1 yottadbuser yottadbuser 32 Jan 22 10:39 ./UTC.m
   -rw-rw-r-- 1 yottadbuser yottadbuser 1000 Jan 22 10:40 ./UTC.o

   YDB>

YottaDB also provides a mechanism for processes to indicate that instead of explicitly relinking newer versions of routines, they would like to “subscribe” to and automatically execute the latest updated (“published”) object code of routines. Processes indicate this interest by appending an asterisk (“*”) to each directory name from which they wish to execute the latest object code.

**The ydb_env_set script would have already appended the requisite asterisk to $zroutines.** 


.. parsed-literal::
   yottadbuser@yottadbworkshop:~$ ydb

   YDB>write $zroutines
   /home/yottadbuser/.yottadb/r1.22_x86_64/o*(/home/yottadbuser/.yottadb/r1.22_x86_64/r /home/yottadbuser/.yottadb/r) /usr/local/lib/yottadb/r1.22_x86_64/libyottadbutil.so 

   YDB>

In a different YottaDB process in a different shell session, modify the “greeting” program to say “Goodbye!”. Note the use of the environment variable ydb_prompt to differentiate it from the original session. After editing it, run the routine, which will compile the new version. 

.. parsed-literal::
   YDB2>zedit "greeting" ; modify it to print Goodbye!

   YDB2>do ^greeting ; this ensures that the new version is compiled
   Goodbye!
   YDB2>


In the original session, again run the greeting program, and notice that even without an explicit zlink, it has the latest version of the program:

.. parsed-literal::
   YDB>do ^greeting
   Goodbye!

   YDB>

The `Programmer's Guide <https://docs.yottadb.com/ProgrammersGuide/isv.html#zroutines>`_ explains the use of $ZROUTINES in more detail.

**Exercise - Default Directory Structure for an Application**

Use the tree -d .yottadb command from the shell to look at the default directory structure under .yottadb. What is the purpose of each directory?

--------------------------------------------
Global Directories Point to Global Variables
--------------------------------------------

Routines in YottaDB reside in the file system rather than in the database, whereas global variables reside in the database. Routines are completely independent of global variables. In this respect, YottaDB may be different from other M implementations.

Given a person's name, a telephone directory helps you find the person by giving you their phone number, and sometimes their address as well. Analogously, given an M global variable name, a global directory helps a YottaDB process find the variable by giving it the database file where that variable resides, as well as other pertinent information.

The global directory is a binary file pointed to by the ISV $zgbldir. The GDE utility program (invoked with do "^GDE" inside YottaDB or "mumps -run ^GDE" from the shell) is used to manage global directories. [Note that the input to GDE can be a text file. In a production environment, YottaDB recommends that text files be used to define database configurations, and that these text files be put under version control.]

In YottaDB, sets of M global variables (Names or Name spaces) are mapped to Regions that define properties relating to the M global. Each Region is mapped to a Segment that defines properties relating to the file system. Consider the example in the figure below:

.. image:: globaldir.png

In this example, there are four M global variables that we would like to separate from the rest (e.g., for purposes of sharing globals between applications, or for reasons of protection – perhaps they contain special information, so that only mammalogists are to have access to globals ^Horse and ^Platypus, and only carcinologists are to have access to globals ^Crab and ^Lobster). This is accomplished by creating five name spaces (note that a name space can contain a single variable, as in this example, or a range of global variables, e.g., everything starting with ^A through ^Horse). There is always a default (*) name space.

One or more name spaces are mapped to a Region. All global variables in a region share a common set of M global variable properties, such as the maximum record length, whether null subscripts are permitted, etc. In this case ^Horse and ^Platypus are mapped to the region "Mammals", whereas ^Crab and ^Lobster are mapped to the region "Crustaceans". The default name space * is mapped to a region called "Default".

Each region is mapped to a Segment. Just as a region defines properties pertaining to M global variables, the segment defines properties pertaining to the database file for that region, such as the file name, the initial allocation, number of global buffers, etc. The database file is just an ordinary file in the file system of the underlying operating system.

Each database file can have a single active journal file. A journal file can be linked to a previous journal files to form a chain of journal files.

The ISV $zgbldir points a YottaDB process to the global directory. $zgbldir is initialized from $ydb_gbldir at process startup, but it can be modified by the process during execution.

.. parsed-literal::
   YDB>write $zgbldir
   /home/yottadbuser/.yottadb/r1.22_x86_64/g/yottadb.gld
   YDB>

GDE, the Global Directory Editor, is a program used to manipulate global directories. GDE is itself written in M, and you can invoke it from the shell with "mumps -run GDE" or from inside the direct mode with "do ^GDE".

.. parsed-literal::
   yottadbuser@yottadbworkshop:~$ $ydb_dist/mumps -run GDE
   %GDE-I-LOADGD, Loading Global Directory file /home/yottadbuser/.yottadb/r1.22_x86_64/g/yottadb.gld
   %GDE-I-VERIFY, Verification OK


   GDE>

You can use the show command to examine name spaces, regions and segments.

.. parsed-literal::
   GDE> show -name

         \*\*\* NAMES \*\*\*
   Global                             Region
   ------------------------------------------------------------------------------
   *                                  DEFAULT
   GDE>


In this case, there is only one name space, the default. There is also only one region, DEFAULT. Region and segment names are case insensitive, but name spaces are case sensitive, since M variable names are case sensitive.

.. parsed-literal::
   GDE> show -region
   
    ======================================
                  REGIONS
    -------------------------------------
    Region  Dynamic Segment  Def Coll Rec Size  Key Size  Null Subs Std Null Col Jnl Inst Freeze on Error Qdb Rndwn
    ======  ===============  ======== ========  ========  ========= ============ === ==================== =========
    DEFAULT   DEFAULT            0          4080    255      NEVER      Y         Y      DISABLED         DISABLED
    =======   =======        =======   ========  =======     ======  ============ ===    ========         ========

Notice the region parameters – review them in the `Administration and Operations Guide <https://docs.yottadb.com/AdminOpsGuide/index.html>`_. Since there is one region, there is also one segment, also called DEFAULT. (the region and segment names can be different; it is good practice to keep them the same).

.. parsed-literal::
   
   GDE> show -segment

   =======================================
          SEGMENTS
   ---------------------------------------
   Segment   File (def ext: .dat)Acc Typ Block      Alloc Exten Options
   =======   =================================      ======================
   DEFAULT   $gtmdir/$ydb_rel/g/yottadb.dat              
                   BG  DYN  4096                     5000 10000 GLOB=1000
                                                                LOCK=  40
                                                                RES =   0
                                                                ENCR=OFF
                                                                MSLT=1024
   =======   ==================================     ======================

   GDE>

Notice how the database file is defined using the environment variables $gtmdir and $ydb_rel. This means that, as long as the environment variables are defined, one global directory can point to a database file wherever it happens to be in the system. This can allow two processes to share a global directory, but to have different database files.

.. note:: 
   The parameters in the global directory are used only by mupip create to create a new database file. At other times, the global directory is used only to map global variable names to database files. So, if you change the global directory, existing database files are not changed. If you change a parameter in a database file, unless you also change the global directory used to create the database file, the next time you create that file, it will use old parameters in the global directory.

The show map command gives a good visualization of the mapping of names to database files in the global directory.

.. parsed-literal::
   GDE> show -map

   =====================================
               MAP
   ------------------------------------
               Names
   ------------------------------------
   From           Up to     Region / Segment / File(def ext: .dat)
   ====           =====     ======================================
   %               ...        REG= DEFAULT
                              SEG= DEFAULT
                              FILE = $gtmdir/$ydb_rel/g/yottadb.dat
   LOCAL LOCKS                REG= DEFAULT
                              SEG= DEFAULT
                              FILE = $gtmdir/$ydb_rel/g/yottadb.dat
   ============   ======    ======================================

   GDE>

**Exercise- Set up the Global Directory for Mammalogists and Carcinologists**

Start from the shell. Assign a value to $ydb_gbldir so as to not overwrite any existing global directory in the Acculturation Workshop and then invoke GDE.

.. parsed-literal::
   yottadbuser@yottadbworkshop:~$ export ydb_gbldir=/home/yottadbuser/yottadb.gld
   yottadbuser@yottadbworkshop:~$ mumps -run GDE
   %GDE-I-GDUSEDEFS, Using defaults for Global Directory
           /home/yottadbuser/yottadb.gld

   GDE>

While not essential, it may be conceptually helpful to build the global directory from the bottom up – first create the segments, then the regions, and then the name spaces. First, edit the default to make the parameters more useful – the out-of-the-box defaults are suitable for experimentation but not real use. Using a template reduces the work needed to create multiple regions and segments. Notice the use of different access methods for MAMMALS and CRUSTACEANS.

.. parsed-literal::
   GDE> change -segment DEFAULT -block_size=4096 -allocation=1000 -extension=1000 -global_buffer_count=1000 -file_name=/home/yottadbuser/mumps.dat
   GDE> template -segment -access_method=bg -block_size=4096 -allocation=1000 -extension=1000 -global_buffer_count=1000
   GDE> template -segment -access_method=mm -block_size=4096 -allocation=1000 -extension=1000 -global_buffer_count=1000
   GDE> add -segment MAMMALS -access_method=mm -file_name=/home/yottadbuser/linnaeus.dat
   GDE> add -segment CRUSTACEANS -access_method=bg -file_name=/home/yottadbuser/brunnich.dat
   GDE> show -segment

   ==================================
          SEGMENTS
   ----------------------------------
    Segment    File (def ext: .dat)Acc Typ Block  Alloc Exten Options
    =======    =================================  ===================
    MAMMALS      /home/yottadbuser/linnaeus.dat               
                                    MM DYN 4096   1000 1000 DEFER
                                                            LOCK=40
                                                            RES= 0
                                                            ENCR=OFF
                                                            MSLT=1024
    DEFAULT   /home/yottadbuser/mumps.dat
                                   BG DYN 4096    1000 1000 GLOB=100
                                                            LOCK=40
                                                            RES=0
                                                            ENCR=OFF
                                                            MSLT=1024
    CRUSTACEANS /home/yottadbuser/brunnich.dat
                                   BG DYN 4096    1000 1000 GLOB=100
                                                            LOCK=40
                                                            RES=0
                                                            ENCR=OFF
                                                            MSLT=1024
    ========  =================================   ====================

    GDE>

Then we can map the regions to the segments. Notice that the segment names (specified with the -dynamic qualifier) are converted to and displayed in upper case.

.. parsed-literal::
   GDE> change -region DEFAULT -stdnull -key_size=255 -record_size=4080 -journal=(before,file="/home/yottadbuser/mumps.mjl")
   GDE> template -region -stdnull -key_size=255 -record_size=4080 -journal=nobefore
   GDE> add -region MAMMALS -dynamic=mammals -journal=(nobefore,file="/home/yottadbuser/linnaeus.mjl")
   GDE> add -region CRUSTACEANS -dynamic=crustaceans -journal=(before,file="/home/yottadbuser/brunnich.mjl")
   GDE> show -region
   
   ==============================
            REGIONS
   ------------------------------
   Region       Dynamic Segment    Def Coll   Rec Size   Key Size   Null Subs  Std Null Coll  Jnl  Inst Freeze on Error  Qdb Rundown
   ======       ===============    ========   ========   ========   =========  =============  ===  ====================  ===========
   MAMMALS      MAMMALS              0        4080        255       NEVER           Y         Y     DISABLED             DISABLED
   DEFAULT      DEFAULT              0        4080        255       NEVER           Y         Y     DISABLED             DISABLED
   CRUSTACEANS  CRUSTACEANS          0        4080        255       NEVER           Y         Y     DISABLED             DISABLED
   ===========  ===============    ========   =======    =========  ========   =============  ===   ==================   ===========


   ==================================
         JOURNALING INFORMATION
   ----------------------------------
   Region      Jnl File (def ext: .mjl)       Before  Buff   Alloc  Exten   Autoswitch
   ======      ===========================   ======   ====   =====  =====   ==========
   MAMMALS     /home/yottadbuser/linnaeus.mjl      N      2308   2048   2048    8386560
   DEFAULT     /home/yottadbuser/mumps.dat         Y      2308   2048   2048    8386560
   CRUSTACEANS /home/yottadbuser/brunnich.mjl      Y      2308   2048   2048    8386560
   ========    ==========================    ======  ======  =====  ====   ===========

   GDE>

Now we map the name spaces to the regions.

.. parsed-literal::
   GDE> add -name Horse -region=mammals
   GDE> add -name Platypus -region=mammals
   GDE> add -name Crab -region=crustaceans
   GDE> add -name Lobster -region=crustaceans
   GDE> show -name

   ===============================
                  NAMES
   -------------------------------
   Global             Region
   ======             =======
   *                  DEFAULT
   Crab               CRUSTACEANS
   Horse              MAMMALS
   Lobster            CRUSTACEANS
   Platypus           MAMMALS
   =======            =======

  GDE>

You can examine the entire map, and ask GDE to perform a check for consistency.

.. parsed-literal::
   GDE> show -map

   =================================
             MAP
   ---------Names-------------------
   From       Up To         Region/Segment/File (def ext: .dat)
   =====     =======        ===================================
   %         Crab             REG= DEFAULT
                              SEG= DEFAULT
                              FILE= /home/yottadbuser/mumps.dat
   Crab      Crab0            REG= CRUSTACEANS
                              SEG= CRUSTACEANS
                              FILE= /home/yottadbuser/brunnich.dat
   Crab0     Horse            REG= DEFAULT
                              SEG= DEFAULT
                              FILE= /home/yottadbuser/mumps.dat
   Horse    Horse0            REG= MAMMALS
                              SEG= MAMMALS
                              FILE= /home/yottadbuser/linnaeus.dat
   Horse0   Lobster           REG= DEFAULT
                              SEG= DEFAULT
                              FILE= /home/yottadbuser/mumps.dat
   Lobster  Lobster0          REG= CRUSTACEANS
                              SEG= CRUSTACEANS
                              FILE= /home/yottadbuser/brunnich.dat
   Lobster0  Platypus         REG= DEFAULT
                              SEG= DEFAULT
                              FILE= /home/yottadbuser/mumps.dat
   Platypus  Platypus0        REG= MAMMALS
                              SEG= MAMMALS
                              FILE= /home/yottadbuser/linnaeus.dat
   Platypus0 .....            REG= DEFAULT
                              SEG= DEFAULT
                              FILE= /home/yottadbuser/mumps.dat
   LOCAL LOCKS                REG= DEFAULT
                              SEG= DEFAULT
                              FILE= /home/yottadbuser/mumps.dat
   ======     =========       ==============================

   GDE> verify
   %GDE-I-VERIFY, Verification OK

   GDE>

Exiting GDE creates the global directory. You can then use a mupip create command to create the database files. Notice that journal files must be separately created.

.. parsed-literal::
   GDE> exit
   %GDE-I-VERIFY, Verification OK

   %GDE-I-GDCREATE, Creating Global Directory file /home/yottadbuser/yottadb.gld
   yottadbuser@yottadbworkshop:~$ ls -l * .dat * .mjl
   ls: cannot access * .dat: No such file or directory
   ls: cannot access * .mjl: No such file or directory
   yottadbuser@yottadbworkshop:~$ mupip create
   Created file /home/yottadbuser/linnaeus.dat
   Created file /home/yottadbuser/mumps.dat
   Created file /home/yottadbuser/brunnich.dat
   yottadbuser@yottadbworkshop:~$ ls -l * .dat * .mjl
   ls: cannot access * .mjl: No such file or directory
   -rw-rw-rw- 1 yottadbuser yottadbuser 4366848 Jan 22 12:15 linnaeus.dat
   -rw-rw-rw- 1 yottadbuser yottadbuser 4366848 Jan 22 12:15 brunnich.dat
   -rw-rw-rw- 1 yottadbuser yottadbuser 4366848 Jan 22 12:15 mumps.dat
   yottadbuser@yottadbworkshop:~$

Then you can turn on journaling. As YottaDB requires you to explicitly specify the type of journaling to be used, you need separate commands depending on the type of journaling – before image and no-before image journaling.

.. parsed-literal::
   yottadbuser@yottadbworkshop:~$ mupip set -journal=nobefore -region CRUSTACEANS
   %YDB-I-JNLCREATE, Journal file /home/yottadbuser/brunnich.mjl created for region CRUSTACEANS with NOBEFORE_IMAGES
   %YDB-I-JNLSTATE, Journaling state for region CRUSTACEANS is now ON
   yottadbuser@yottadbworkshop:~$ mupip set -journal=before -region MAMMALS,DEFAULT
   %YDB-I-JNLCREATE, Journal file /home/yottadbuser/mumps.mjl created for region DEFAULT with BEFORE_IMAGES
   %YDB-I-JNLSTATE, Journaling state for region DEFAULT is now ON
   %YDB-I-JNLCREATE, Journal file /home/yottadbuser/linnaeus.mjl created for region MAMMALS with BEFORE_IMAGES
   %YDB-I-JNLSTATE, Journaling state for region MAMMALS is now ON
   yottadbuser@yottadbworkshop:~$ ls -l * .dat * .mjl
   -rw-rw-rw- 1 yottadbuser yottadbuser 4366848 Jan 22 12:22 brunnich.dat
   -rw-rw-rw- 1 yottadbuser yottadbuser   69632 Jan 22 12:22 brunnich.mjl
   -rw-rw-rw- 1 yottadbuser yottadbuser 4366848 Jan 22 12:24 linnaeus.dat
   -rw-rw-rw- 1 yottadbuser yottadbuser   69632 Jan 22 12:24 linnaeus.mjl
   -rw-rw-rw- 1 yottadbuser yottadbuser 4366848 Jan 22 12:24 mumps.dat
   -rw-rw-rw- 1 yottadbuser yottadbuser   69632 Jan 22 12:24 mumps.mjl
   yottadbuser@yottadbworkshop:~$

For production environments, we suggest that you put your GDE commands in a text file and invoke them with a heredoc or using GDE's @ command. Put the text file under version control.

**$zroutines and $zgbldir vs. UCI & Volume set**

The YottaDB environment is defined by $ZROUTINES (initialized from $ydb_routines) and $zgbldir (initialized from $ydb_gbldir). Concepts from other M implementations such as UCI and Volume Sets do not exist on YottaDB.

The YottaDB separation between routines and the database is very powerful, especially in real-world environments. Apart from the flexibility this offers, it enables the practice of “defensive programming”, not unlike defensive driving. This is desirable as defensive practices reduce the probability of errors.

**Exercise - Set Up a Simulated ASP Environment**

In an Application Service Provider (ASP) environment, the same application code can be used for a number of sites, but each site has its own database. Sometimes parts of the database may also be common and used on a read-only basis for normal operation, such as a data dictionary, an approved budget, or a table of sales tax rates for a specific location. Each site may also have a small set of custom routines. Let us consider an ASP serving two financial institutions, called fi (for Financial Institution) and cb (for Credit Bank).

The majority of routines are shared, with:

- source routines that are independent of the YottaDB version in /opt/bank/yottadb/r,
- source routines that are dependent on the YottaDB version in /opt/bank/yottadb/r122/r (note that in the typical case, this directory will be empty, but if a release of YottaDB has a new feature that a routine XYZ.m can take advantage of, you would put the new XYZ.m in this directory and leave the old XYZ.m in the previous directory), and
- object files in /opt/bank/yottadb/r122/o.

Custom routines for Financial Institution in /var/opt/bank/yottadb/fi/r and /var/opt/bank/yottadb/fi/r122/r with object code in /var/opt/bank/yottadb/fi/r122/o.

Similarly, custom routines for the Credit Bank are in /var/opt/bank/yottadb/cb/r and /var/opt/bank/yottadb/cb/r122/r with object code in /var/opt/bank/yottadb/cb/r122/o.

What should $ydb_routines be for an FI user and what should it be for a CB user? Create a shell script to set ydb_routines to be sourced by an FI user and another to be sourced by a CB user. [The shell scripts can reside in /var/opt/bank/yottadb/cb/r122 and /var/opt/bank/yottadb/fi/r122.]

The approved Tax Rate is in the global variable ^TXR and is shared by both institutions with read only access to users. The Tax Rate is in the database file /opt/bank/yottadb/r122/g/txr.dat. All other globals are in database files that are specific to FI and CB, in /var/opt/bank/yottadb/fi/r122/g/main.dat and /var/opt/bank/yottadb/cb/r122/g/main.dat.

First, create the directory structure.

.. parsed-literal::
   yottadbuser@yottadbworkshop:~$ sudo mkdir -p /opt/bank/yottadb
   yottadbuser@yottadbworkshop:~$ sudo chown -R yottadbuser.users /opt/bank/yottadb
   yottadbuser@yottadbworkshop:~$ cd /opt/bank/yottadb ; mkdir -p r r122/r r122/o r122/g
   yottadbuser@yottadbworkshop:/opt/bank/yottadb$ sudo mkdir -p /var/opt/bank/yottadb
   yottadbuser@yottadbworkshop:opt/bank/yottadb$ sudo chown -R yottadbuser.users /var/opt/bank/yottadb
   yottadbuser@yottadbworkshop:opt/bank/yottadb$ cd /var/opt/bank/yottadb ; mkdir -p fi/r fi/r122/r fi/r122/o fi/r122/g
   yottadbuser@yottadbworkshop:/var/opt/bank/yottadb$ mkdir -p cb/r cb/r122/r cb/r122/o cb/r122/g
   yottadbuser@yottadbworkshop:/var/opt/bank/yottadb$ tree -d
   .
   ├── cb
   │ ├── r
   │ └── r122
   │     ├── g
   │     ├── o
   │     └── r
   └── fi
    ├── r
    └── r122
          ├── g
          ├── o
          └── r
 12 directories
 yottadbuser@yottadbworkshop:~$

What should $ydb_gbldir be for an FI user and what should it be for a CB user? Add these to the command files you created earlier. Create a file of commands to be fed to GDE either with a heredoc or with GDE's @ command that will create the global directories and then create the global directories.

Create the three database files with mupip create (remember that the database file /opt/bank/yotadb/r122/g/txr.dat will be created by the first mupip create, and the second mupip create will only create the institution specific database file.

In one environment (fi or cb) , assign values to the global variables ^TXR and ^X. In the other environment, confirm that you are able to read the value of ^TXR (i.e., it is shared), but not the value in ^X (i.e., it is not shared).

Set a value for ^X in the second environment, and in the first environment confirm that you still see the original value of ^X that you set up in that environment.

Create a program ABC.m to write “Hello!” in /opt/bank/yottadb/r and two programs with the same name DEF.m in /var/opt/bank/yottadb/fi to write “Hello, Financial Institution!” and in /var/opt/bank/yottadb/cb to say “Hello, Credit Bank!”. Verify that a process in either environment gets “Hello!” when it executes ABC.m and either “Hello, Financial Institution!” or “Hello, Credit Bank!” depending on its environment when it executes DEF.m.

**No Special Startup or Shut Down**

The first process to open a database file sets up all the shared memory control structures needed. The last one out tears it down. There is no daemon that needs to run with elevated privileges or can be a single point of failure, a performance bottleneck or a potential security vulnerability. Note that if replication is in use, then at least one Source Server process (see the section on Replication further on) must be brought up first, but that is not a database daemon.

Upon bringing the system back up, if the system crashes, or is forcibly brought down: 

* If journaling is in use, mupip journal -recover (or mupip journal -rollback if replication is in use) will recover the database. 
* If journaling is not in use, mupip rundown -region "*" will clean up the database control structures in the file header, but cannot fix any integrity errors resulting from shutting down a computer without cleanly terminating YottaDB processes.

.. note::
   Do not use mupip rundown if journaling is in use and you plan to recover the database after a crash with a mupip journal operation.

--------------------------
Environment Variables
--------------------------

The operation of YottaDB is controlled by a number of environment variables. The most important ones are ydb_dist, ydb_routines and ydb_gbldir, which are discussed above. The file ydb_env_set that is supplied with YottaDB, and which must be sourced rather than executed, attempts to provide reasonable default values. By setting environment variables either before sourcing it or after (the former is preferred, because ydb_env_set can attempt to deal with interactions), you can provide your own values instead of using the defaults.

Review the file /usr/local/lib/yottadb/r122/ydb_env_set to see how the environment variables are set. Study the order in which they are set and see if you can understand why.

The following environment variable is explicitly set by ydb_env_set:

- **ydb_dist** - points to the directory where YottaDB is installed.

The following must be set before ydb_env_set is sourced if you want to run YottaDB in UTF-8 mode:

- **ydb_chset** - when it has the value "UTF-8", YottaDB  operates in UTF-8 mode,

When possible, ydb_env_set provides reasonable defaults for any of the following that are not set:

- **gtmdir** (not used by YottaDB directly) – part of a default YottaDB environment set by ydb_env_set. ydb_env_set uses this to create a default directory structure underneath, and sets other environment variables relative to $gtmdir and assuming a default directory structure underneath.

- **ydb_gbldir** - points to the global directory.

- **ydb_icu_version** - this is meaningful only when $ydb_chset is "UTF-8". YottaDB requires libicu version 3.6 or higher. If libicu has been compiled with symbol renaming enabled (as is the case with Ubuntu Linux), YottaDB requires ydb_icu_version to be explicitly set (see the release notes for your YottaDB release). Note that ICU changed its version numbering system so that the version after 4.8 was 49. As YottaDB retains the old numbering scheme, for ICU versions after 4.8, please set ydb_icu_version using the old scheme, e.g., if your Linux system has ICU version 55, set ydb_icu_version to 5.5.

- **ydb_log** - this is where the gtmsecshr process creates log files and all processes that use an installation of YottaDB (from one directory) should have the same value of this environment variable. In conformance with the `Filesystem Hierarchy Standard <http://www.pathname.com/fhs/>`_ /var/log/yottadb/$ydb_rel is suggested (unless the same version of YottaDB is installed in multiple directories).

- **ydb_principal_editing** - determines whether the previous input to a Read command can be recalled and edited before ENTER is pressed to submit it. Note: direct mode commands have a more extensive capability in this regard, independent of the value of this environment variable.

- **ydb_prompt** - if set, this is the YottaDB direct mode prompt. If not set, the direct mode prompt is "YDB>". If you routinely work in different environments, you can use this to remind yourself which environment you are in, e.g., "DEV>" for development, "TEST>" for testing and "PROD>" for production.

- **ydb_repl_instance** - specifies the path to the replication instance file when database replication is in use. We suggest putting this file in the same directory as your global directory.

- **gtm_retention** (not used by YottaDB directly) – used by the ydb script to delete old journal files and old temporary files it creates.

- **ydb_routines** - routine search path.

- **ydb_tmp** - socket files used for communication between gtmsecshr and YottaDB processes go here. All processes that use an installation of YottaDB should have the same value of this environment variable. We suggest /tmp/yottadb/$ydb_rel or /var/tmp/yottadb/$ydb_rel depending on your operating system and your local standards.

- **ydb_rel** (not used by YottaDB directly) – part of a default YottaDB environment set by ydb_env_set.

- **LC_CTYPE** - a standard system environment variable used to specify a locale. When $ydb_chset has the value "UTF-8", $LC_CTYPE must specify a UTF-8 locale (e.g., "en_US.utf8").

YottaDB directly or indirectly uses a number of other environment variables that are not touched by ydb_env_set (they can be set before or after ydb_env_set is sourced). These are documented in the `Administration and Operations Guide <https://docs.yottadb.com/AdminOpsGuide/index.html>`_. Some worth noting are:

- **ydb_badchar** is used to initialize the setting of the VIEW command that determines whether YottaDB should raise an error when it encounters an illegal UTF-8 character sequence.

- **ydb_baktmpdir** is used by mupip to create temporary files for backup in the directory where it is. Mupip online integ also creates temporary files in this directory if ydb_snaptmpdir is not defined.

- **gtm_dbkeys** (not used by YottaDB directly) – used by the encryption reference plugin for the name of a file providing a list of database files and their corresponding key files.

- **ydb_fullblockwrites** specifies whether a YottaDB process should write a full database block worth of bytes when writing a database block that is not full. Depending on your IO subsystem, writing a full block worth of bytes (even when there are unused garbage bytes at the end) may result in better database IO performance by replacing a read-modify-write low level IO operation with a single write operation.

- **ydb_nocenable** is used to specify that a Control-C on a terminal $Principal device should not cause the process to enter direct mode.

- **ydb_passwd** (not used by YottaDB directly) – used by the encryption reference plugin to store the obfuscated (not encrypted) password to the GNU Privacy Guard key ring.

- **EDITOR** - a standard system environment variable that specifies the editor invoked by YottaDB in response to the ZEDIT command (defaults to vi, if $EDITOR is not set).

- **TZ** - a standard system environment variable that specifies the timezone to be used by YottaDB processes, if they are not to use the default system timezone (YottaDB assumes the system clock is set to UTC).

Here are the environment variables set by the default ydb_env_set file (which the ydb script sources).

.. parsed-literal::
   yottadbuser@yottadbworkshop:~$ env | grep ^ydb # No ydb environment variables defined initially
   yottadbuser@yottadbworkshop:~$ source /usr/local/lib/yottadb/r122/ydb_env_set
   yottadbuser@yottadbworkshop:~$ env | grep ^ydb
   ydb_retention=42
   ydb_log=/tmp/yottadb/r1.22_x86_64
   ydb_gbldir=/home/yottadbuser/.yottadb/r1.22_x86_64/g/yottadb.gld
   ydb_etrap=Write:(0=$STACK) "Error occurred: ",$ZStatus,!
   gtmdir=/home/yottadbuser/.yottadb
   ydb_rel=r1.22_x86_64
   ydb_routines=/home/yottadbuser/.yottadb/r1.22_x86_64/o*(/home/yottadbuser/.yottadb/r1.22_x86_64/r /home/yottadbuser/.yottadb/r) /usr/local/lib/yottadb/r1.22_x86_64/libyottadbutil.so
   ydb_repl_instance=/home/yottadbuser/.yottadb/r1.22_x86_64/g/yottadb.repl
   ydb_tmp=/tmp/yottadb/r1.22_x86_64
   ydb_dist=/usr/local/lib/yottadb/r122/
   yottadbuser@yottadbworkshop:~$ 

While ydb_env_set and ydb are good resources when you initially start with YottaDB, once you get to a certain level of expertise, you may prefer to create your own scripting.

--------
Security
--------

YottaDB was designed from the very beginning to be secure. 

.. note::
 Absolute security does not exist in this universe. For a discussion that bridges philosophy and technology, we highly recommend `Bruce Schneier's Secrets and Lies, ISBN 0-471-25311-1 <http://www.schneier.com/book-sandl.html>`_.

A YottaDB process can access a database file only if the file ownership and permissions allow. Under normal operation, there is only one small component of YottaDB that operates as the super user (root) – the gtmsecshr helper process. The YottaDB security model is simple, well understood and documented.

Review `Appendix E of the Administration and Operations Guide <https://docs.yottadb.com/AdminOpsGuide/securityph.html>`_.

**Exercise - Access Controls with Ownership and Permissions**

Start with a fresh session to discard environment variables from the last exercise. In the following, notice how Linux file permissions are used to allow user yottadbuser full access to the database, preventing another user from updating a database, while allowing that user to read from it.

Verify that you can read from and write to your default database and change the permissions to make it not accessible to the world, and to make it read-only by others in the group.

.. parsed-literal::
   yottadbuser@yottadbworkshop:~$ ls -l .yottadb/r1.22_x86_64/g/yottadb.dat
   -rw-rw-rw- 1 yottadbuser gtmuser 20783616 Jan 22 13:56 .yottadb/r1.22_x86_64/g/yottadb.dat
   yottadbuser@yottadbworkshop:~$ chmod o-rw,g-w .yottadb/r1.22_x86_64/g/yottadb.dat
   yottadbuser@yottadbworkshop:~$ ls -l .yottadb/r1.22_x86_64/g/yottadb.dat
   -rw-r----- 1 yottadbuser gtmuser 20783616 Jan 22 13:56 .yottadb/r1.22_x86_64/g/yottadb.dat
   yottadbuser@yottadbworkshop:~$ ydb

   YDB>set ^X=1

   YDB>zwrite ^X
   ^X=1

   YDB>halt

Create another user who is also a member of the group. See that the user can read from the database owned by yottadbuser, but cannot update it.

.. parsed-literal::
   yottadbuser@yottadbworkshop:~$ sudo useradd -g gtmuser -m staffuser
   yottadbuser@yottadbworkshop:~$ sudo su - staffuser
   staffuser@yottadbworkshop:~$ pwd
   /home/staffuser
   staffuser@yottadbworkshop:~$ export ydb_dist=/usr/local/lib/yottadb/r122/
   staffuser@yottadbworkshop:~$ export ydb_rel=r1.22_x86_64
   staffuser@yottadbworkshop:~$ export gtmdir=/home/yottadbuser/.yottadb
   staffuser@yottadbworkshop:~$ export ydb_gbldir=$gtmdir/$ydb_rel/g/yottadb.gld
   staffuser@yottadbworkshop:~$ $ydb_dist/mumps -dir

   YDB>zwrite ^X
   ^X=1

   YDB>set ^X=2
   %YDB-E-DBPRIVERR, No privilege for attempted update operation for file: /home/yottadbuser/.yottadb/r1.22_x86_64/g/ydb.dat

   YDB>halt
   staffuser@yottadbworkshop:~$ exit
   yottadbuser@yottadbworkshop:~$ sudo userdel staffuser
   yottadbuser@yottadbworkshop:~$ grep staffuser /etc/passwd
   yottadbuser@yottadbworkshop:~$ sudo rm -rf /home/staffuser

There is an installation option to restrict access to YottaDB to a group. If you use this option, only those in the specified group will be able to use YottaDB.

It is extremely straightforward to create a userid that can only login, run an application and log out.

**Exercise - Simulated ASP Environment with Isolation**

For this exercise look at the instructions for the `WorldVistA EHR Four Slice Toaster MSC Fileman 1034 edition <http://tinyurl.com/yjgub6f>`_ (you may need to download the file and open it in your browser). Alternatively go to the `WorldVistA project at Source Forge <http://sourceforge.net/projects/worldvista>`_. Click on “View all files”, open WorldVistA EHR VOE_1.0 and then open 2008-06 Four Slice Toaster MSC FM 1034 and download the file WVEHRVOE10Release6-08Toaster4SliceMSCFM1034Readme.html. Also, download the file WVEHRVOE10Release6-08Toaster4SliceMSCFM1034.zip, unzip it and open it according to the instructions in the Readme.

Login as *vistaadmin / vistaadmin* for administrator access. Note how Clinic P users are members of the gtm group and also members of the clinicp group and Clinic Q users are members of the gtm group and the clinicq group and neither has access to the databases of the other.

----------
Journaling
----------

You should journal any databases whose integrity you care about. Conversely, you need not journal any database that you are prepared to delete & recreate anew in the event of an untoward event like a system crash.

YottaDB, like virtually all high performance databases, uses journaling (called “logging” by some databases) to restore data integrity and provide continuity of business after an unplanned event such as a system crash.

There are two switches to turn on journaling – ENABLE / DISABLE and ON/OFF. Enabling or disabling journaling requires stand alone access to the database. Turning journaling on and off can be done when the database is in use. 

**Exercise - Journaling with the Existing Database**

In this exercise, we will crash your virtual machine and then recover the database. First, we'll just do it on the existing database; then we will set up journaling from scratch.

First, clean out old journal files. Verify that there are no shared memory segments in use. Then go into YottaDB and perform a database operation and verify that there is now a new shared memory segment.

.. parsed-literal::
   yottadbuser@yottadbworkshop:~$ rm -f .yottadb/r1.22_x86_64/g/yottadb.mjl_*
   yottadbuser@yottadbworkshop:~$ ipcs -m

   ------ Shared Memory Segments --------
   key        shmid      owner      perms      bytes      nattch     status

   yottadbuser@yottadbworkshop:~$ /usr/local/lib/yottadb/r1.22_x86_64/yottadb

   YDB>set ^X=$zdate($horolog,"MON DD, YEAR") ; opens database file and creates a shared memory segment

   YDB>zwrite ^X
   ^X="JAN 22, 2018"

   YDB>zsystem "ipcs -m"

   ------ Shared Memory Segments --------
   key        shmid      owner      perms      bytes      nattch     status
   0x00000000 65536      yottadbuser    660        7208960    1


   YDB>

Now kill the virtual machine by clicking on the “X” of the console window, or with a kill -9 of the virtual machine process, and then reboot it. Go back into YottaDB and verify that the data is still there. Instead of running the ydb script (which performs an automatic recovery), run mumps and try to access the database. Note: you should not run the ydb script for this exercise, since it performs a recovery as part of its operation.

.. parsed-literal::
   yottadbuser@yottadbworkshop:~$ source /usr/local/lib/yottadb/r122/ydb_env_set
   yottadbuser@yottadbworkshop:~$ mumps -dir

   YDB>zwrite ^X
   %YDB-E-REQRECOV, Error accessing database /home/yottadbuser/.yottadb/r1.22_x86_64/g/yottadb.dat.  Must be recovered on cluster node yottadbworkshop.
   %YDB-I-TEXT, Error with database control shmctl
   %SYSTEM-E-ENO22, Invalid argument

   YDB>zsystem "ls -l $gtmdir/$ydb_rel/g" ; notice the journal file
   total 1008
   -rw-r----- 1 yottadbuser yottadbuser 20783616 Jan 22 14:22 yottadb.dat
   -rw-rw-r-- 1 yottadbuser yottadbuser     1536 Jan 15 13:14 yottadb.gld
   -rw-r----- 1 yottadbuser yottadbuser    69632 Jan 22 14:22 yottadb.mjl
   -rw-r----- 1 yottadbuser yottadbuser    69632 Jan 15 17:29 yottadb.mjl_2018335172932

   YDB>zsystem "ipcs -m" ; and there are no shared memory segments indicating an open database


   ------ Shared Memory Segments --------
   key        shmid      owner      perms      bytes      nattch     status      


   YDB>zsystem "ls -lR $ydb_tmp" ; and no log files from the ydb script
   /tmp/yottadb/r1.22_x86_64:
   total 0

  YDB>halt

Now, try the ydb script instead of running the mumps executable directly.

.. parsed-literal::
   yottadbuser@yottadbworkshop:~$ ydb

   YDB>zwrite ^X ; database access works
   ^X="JAN 22, 2018"

   YDB>zsystem "ls -l $gtmdir/$ydb_rel/g" ; there are two new journal files
   total 1144
   -rw-r----- 1 yottadbuser yottadbuser 20783616 Jan 22 14:22 yottadb.dat
   -rw-rw-r-- 1 yottadbuser yottadbuser     1536 Jan 15 13:14 yottadb.gld
   -rw-r----- 1 yottadbuser yottadbuser    69632 Jan 22 14:22 yottadb.mjl
   -rw-r----- 1 yottadbuser yottadbuser    69632 Jan 15 17:29 yottadb.mjl_2018335172932
   -rw-r----- 1 yottadbuser yottadbuser    69632 Jan 22 14:30 yottadb.mjl_2018335143031
   -rw-r----- 1 yottadbuser yottadbuser    69632 Jan 22 14:30 yottadb.mjl_2018335143032

   YDB>zsystem "ipcs -m" ; there is a shared memory segment for the open database file

   ------ Shared Memory Segments --------
   key        shmid      owner      perms      bytes      nattch     status
   0x00000000 65536      yottadbuser    660        7208960    1 


   YDB>zsystem "ls -lR $ydb_tmp" ; and log files from the commands in the ydb script
   /tmp/yottadb/r1.22_x86_64:
   total 8
   -rw-rw-r-- 1 yottadbuser yottadbuser 617 Jan  22 14:30 yottadbuser_20181201165831UTC_mupip_recover
   -rw-rw-r-- 1 yottadbuser yottadbuser 339 Jan  22 14:30 yottadbuser_20181201165831UTC_mupip_set

   YDB>halt

How did the recovery happen? The answer is in the ydb script.

.. parsed-literal::
   yottadbuser\@yottadbworkshop:/usr/local/lib/yottadb/r122$ vim ydb
   #!/bin/sh
   #################################################################
   #       Copyright 2014-2018 Fidelity National Information       #
   #        Services, Inc. and/or its subsidiaries.                #
   #        All rights reserved.                                   #
   #                                                               #
   #       Copyright 2017-2018 YottaDB LLC., and/or its            #
   #       subsidiaries. All rights reserved                       #
   #                                                               #
   #       This source code contains the intellectual property     #
   #       of its copyright holder(s), and is made available       #
   #       under a license.  If you do not know the terms of       #
   #       the license, please stop and do not read further.       #
   #                                                               #
   #################################################################

   if [ ! -f "/usr/local/lib/yottadb/r122"/ydb_env_set ] ; then echo Cannot find file "/usr/local/lib/yottadb/r122"/ydb_env_set to source
   else
       . "/usr/local/lib/yottadb/r122"/ydb_env_set
       timestamp=`date -u +%Y%m%d%H%M%S`"UTC"
       ( cd `dirname $ydb_gbldir` ; \\
          $ydb_dist/mupip journal -recover -backward "*" 2>$ydb_tmp/"$USER"_$timestamp"_mupip_recover" && \\
          $ydb_dist/mupip set -journal="on,before" -region "*" 2>$ydb_tmp/"$USER"_$timestamp"_mupip_set" && \\
          find . -name \\*.mjl _\\* -mtime +$gtm_retention -exec rm -vf {} \\; )
       if [ 0 = $# ] ; then
          $ydb_dist/mumps -direct
       elif [ "-help" = "$1" -o "-h" = "$1" -o "-?" = "$1" ] ; then
          echo "ydb -dir[ect] to enter direct mode (halt returns to shell)"
          echo "ydb -run <entryref> to start executing at an entryref"
          echo "ydb -help / ydb -h / ydb -? to display this text"
       else                                           
       $ydb_dist/mumps $\* 
       fi   
       ( cd `dirname $ydb_gbldir` \\
           $ydb_dist/mupip rundown -region "*" 2>$ydb_tmp/"$USER"_$timestamp"-"`date -u +%Y%m%d%H%M%S`"UTC_mupip_rundown" )
       find $ydb_tmp -name "$USER"_\\* -mtime +$gtm_retention -exec rm -f {} \\;
 fi
 yottadbuser\@yottadbworkshop:~$

The mupip journal recover command performs the recovery. Review the output of the mupip commands – as new journal files are created, older journal files are being renamed. Each journal file has a back-pointer to its predecessor. The ydb script removes non-current journal files and temporary files, those older than the number of days specified by the $gtm_retention environment variable.

.. parsed-literal::
   yottadbuser@yottadbworkshop:~$ cat $ydb_tmp/yottadbuser_20111107223555UTC_mupip_recover
   %YDB-I-MUJNLSTAT, Initial processing started at Mon Jan  1 11:58:31 2018
   %YDB-I-MUJNLSTAT, Backward processing started at Mon Jan  1 11:58:31 2018
   %YDB-I-MUJNLSTAT, Before image applying started at Mon Jan  1 11:58:31 2018
   %YDB-I-FILERENAME, File /home/yottadbuser/.yottadb/r1.22_x86_64/g/gtm.mjl is renamed to /home/yottadbuser/.yottadb/r1.22_x86_64/g/yottadb.mjl_2018335115831
   %YDB-I-MUJNLSTAT, Forward processing started at Mon Jan  1 11:58:32 2018
   %YDB-S-JNLSUCCESS, Show successful
   %YDB-S-JNLSUCCESS, Verify successful
   %YDB-S-JNLSUCCESS, Recover successful
   %YDB-I-MUJNLSTAT, End processing at Mon Jan  1 11:58:32 2018
   yottadbuser@yottadbworkshop:~$ cat $ydb_tmp/yottadbuser_20111107223555UTC_mupip_set 
   %YDB-I-FILERENAME, File /home/yottadbuser/.yottadb/r1.22_x86_64/g/gtm.mjl is renamed to /home/yottadbuser/.yottadb/r1.22_x86_64/g/yottadb.mjl_2018335115832
   %YDB-I-JNLCREATE, Journal file /home/yottadbuser/.yottadb/r1.22_x86_64/g/yottadb.mjl created for region DEFAULT with BEFORE_IMAGES
   %YDB-I-JNLSTATE, Journaling state for region DEFAULT is now ON
   yottadbuser@yottadbworkshop:~$ 

Look at the animation of journaling in action at the beginning of `Chapter 6: YottaDB Journaling <https://docs.yottadb.com/AdminOpsGuide/ydbjournal.html>`_ in the Administration and Operations Guide.

**Note on File System Configuration**

Robust operation of YottaDB recovery after a crash requires robust recovery of the file system. If your file system requires an option to ensure that meta-data is written to disk only after the corresponding data is written, ensure that it is set.

**Exercise - Journaling from Scratch**

*Create a directory (e.g., exDir) in your home directory. In it, create a global directory that maps all variables starting with A or a in aA.dat and others in others.dat. Create the database files. Then enable and turn on before image journaling for both files. Start a process and update both databases. With the process open, kill the virtual machine. Reboot the virtual machine, see for yourself that you cannot access the database, then recover the database (which consists of two database files) and demonstrate that you can now access the database.*

Hints:

- Start with an environment that does not have YottaDB environment variables already defined, e.g., from sourcing the ydb_env_set file. You can always logout and login to get a fresh session

- Create an ydbenv file in the directory to set up the environment variables. You can then source it with a command such as source ./ydbenv to set up the environment. Set up the environment variables yourself and do not source /usr/local/lib/yottadb/r1.22_x86_64/ydb_env_set because it will recover the database when you source it and you will miss the point of the exercise. At a minimum, the env file should specify values for the following environment variables: ydb_dist (set to /usr/local/lib/yottadb/r1.22_x86_64), ydb_gbldir (set to $HOME/exDir/yottadb.gld), ydb_log and ydb_tmp (set to /tmp/yottadb/r1.22_x86_64; make sure it exists), ydb_principal_editing (set to EDITING), ydb_routines (set to "$HOME/exDir* $ydb_dist/libyottadbutil.so"). Make sure the directory /tmp/yottadb/r122 exists by creating it in the ydbenv file with a mkdir -p command. It may be convenient to alias mumps to $ydb_dist/mumps and mupip to $ydb_dist/mupip. [Hint: if you read a little further, you may find a ydbenv file that you can copy and paste into an editor.]

- In GDE, source the commands in the file /usr/local/lib/yottadb/r122/gdedefaults to set reasonable defaults for the global directory and then change the database file names in the segment and the journal file names in the region to place the database and journal files in /home/yottadbuser/exDir.

- Look at the example with mammalogists and carcinologists for commands to set up journaling.

- You do not have to specify the journal file names for recovery – you can simply specify "*".

----------------------
Database Replication
----------------------

When an application must have the best possible continuity of business, use database replication in addition to before-image journaling to create a logical multi-site configuration. A major restriction of YottaDB replication today is the 20,000 kilometer distance restriction on replication (since the circumference of Planet Earth is approximately 40,000 kilometers, it is difficult to place data centers more than 20,000 kilometers apart). In our example, we will simulate data centers in Santiago (33°S, 70°W), Paris (49°N, 2°E) and Melbourne (37°S, 144°E). Santiago to Paris is 11,642 kilometers, Paris to Melbourne is 16,781 kilometers, and Melbourne to Santiago is 11,269 kilometers (approximately).

**Exercise - Replication**

Because replication builds on journaling, use the directory exDir created above. Enhance the shell script ydbenv to assign values to two more environment variables, ydb_repl_instance and ydb_repl_instname. ydb_repl_instance is the name of a replication instance file where a replicated instance stores information about the state of replication and ydb_repl_instance is the name of an instance – in this case, dummy, but we will change it as we create copies of the instances.

.. parsed-literal::
   yottadbuser@yottadbworkshop:~$ cd exDir ; cat ydbenv
   export ydb_dist=/usr/local/lib/yottadb/r122
   export ydb_gbldir=$HOME/exDir/gtm.gld
   export ydb_log=/tmp/yottadb/r122
   export ydb_tmp=$ydb_log
   export ydb_principal_editing=EDITING
   export ydb_repl_instance=$HOME/exDir/yottadb.repl
   export ydb_repl_instname=dummy
   export ydb_routines="$HOME/exDir* $ydb_dist/libyottadbutil.so"
   mkdir -p $ydb_tmp
   alias mumps=$ydb_dist/mumps
   alias mupip=$ydb_dist/mupip

Turn on replication and journaling (remember to source ydbenv to set the environment variables first)

.. parsed-literal::
   yottadbuser@yottadbworkshop:~$ mupip set -replication=on -region "*"
   %YDB-I-FILERENAME, File /home/yottadbuser/exDir/aA.mjl is renamed to /home/yottadbuser/exDir/aA.mjl_2018335181257
   %YDB-I-JNLCREATE, Journal file /home/yottadbuser/exDir/aA.mjl created for region A with BEFORE_IMAGES
   %YDB-I-PREVJNLLINKCUT, Previous journal file name link set to NULL in new journal file /home/yottadbuser/exDir/aA.mjl created for database file /home/yottadbuser/exDir/aA.dat
   %YDB-I-JNLSTATE, Journaling state for region A is now ON
   %YDB-I-REPLSTATE, Replication state for region A is now ON
   %YDB-I-FILERENAME, File /home/yottadbuser/exDir/others.mjl is renamed to /home/yottadbuser/exDir/others.mjl_2018335181257
   %YDB-I-JNLCREATE, Journal file /home/yottadbuser/exDir/others.mjl created for region DEFAULT with BEFORE_IMAGES
   %YDB-I-PREVJNLLINKCUT, Previous journal file name link set to NULL in new journal file /home/yottadbuser/exDir/others.mjl created for database file /home/yottadbuser/exDir/others.dat
   %YDB-I-JNLSTATE, Journaling state for region DEFAULT is now ON
   %YDB-I-REPLSTATE, Replication state for region DEFAULT is now ON

Create the following shell scripts inside exDir and make them executable:

.. parsed-literal::
   yottadbuser@yottadbworkshop:~$ cat originating_stop 
   #!/bin/sh
   $ydb_dist/mupip replicate -source -shutdown -timeout=0
   $ydb_dist/mupip rundown -region "*"
   
   yottadbuser@yottadbworkshop:~$ cat replicating_start 
   #!/bin/sh
   $ydb_dist/mupip replicate -source -start -passive -instsecondary=dummy -buffsize=1048576 -log=$HOME/exDir/source_dummy.log
   $ydb_dist/mupip replicate -receive -start -listenport=3000 -buffsize=1048576 -log=$HOME/exDir/receive.log
   
   yottadbuser@yottadbworkshop:~$ cat replicating_stop
   #!/bin/sh
   $ydb_dist/mupip replicate -receive -shutdown -timeout=0
   $ydb_dist/mupip replicate -source -shutdown -timeout=0
   $ydb_dist/mupip rundown -region "*"
   
   yottadbuser@yottadbworkshop:~$ chmod +x {originating_stop*,replicating_*}
   
   yottadbuser@yottadbworkshop:~$ ls -l {originating_stop*,replicating_*}
   -rwxrwxr-x 1 yottadbuser yottadbuser  81 Jan  23 10:17 originating_stop
   -rwxrwxr-x 1 yottadbuser yottadbuser 219 Jan  23 10:19 replicating_start
   -rwxrwxr-x 1 yottadbuser yottadbuser 127 Jan  23 10:16 replicating_stop
   yottadbuser@yottadbworkshop:~$ 

You can delete the prior generation journal files, just to keep the directory clean:

.. parsed-literal::
   yottadbuser\@yottadbworkshop:~$ rm \*.mjl\_ *
   yottadbuser\@yottadbworkshop:~$ 

Shut down the Acculturation Workshop virtual machine and make three copies of the Acculturation Workshop called Paris.vmdk, Melbourne.vmdk and Santiago.vmdk. Alternatively, if your host system is short of disk space, make two copies and rename the original ubuntu-16.04_yottadbworkshop10.vmdk file.

If you are using qcow2 or vmdk disk images with QEMU/kvm on Linux, you can use a feature that allows a disk image to be created off a base image so that the base image does not change and all changes go to the new disk image. Check with your virtualization software to determine whether it supports this feature. Execute commands such as the following on the host (with the guest shut down) – depending on the version of QEMU/kvm on your PC, the exact command may vary.

.. parsed-literal::
   $ qemu-img create -f vmdk -o zeroed_grain,backing_file=ubuntu-16.04_yottadbworkshop10.vmdk Paris.vmdk
   Formatting ' Paris.vmdk', fmt=vmdk size=10737418240 backing_file='ubuntu-16.04_yottadbworkshop10.vmdk' compat6=off zeroed_grain=on
   $

Now boot the three virtual machines. Each virtual machine will need two ports to be forwarded from the host, one for ssh access forwarded to port 22 on each virtual machine and one for replication forwarded to port 3000 on each virtual machine (i.e., a total of six ports on the host for the three instances). The examples here use host ports 2221 & 4000 for Santiago, 2222 & 5000 for Paris, and 2223 & 6000 for Melbourne. The commands given here use kvm on Linux – use the commands appropriate to virtualization on your host).

.. parsed-literal::
   kvm -enable-kvm -cpu host -m 256 -display none -net nic -net user,hostfwd=tcp::2221-:22,hostfwd=tcp::4000-:3000 -hda Santiago.vmdk &
   kvm -enable-kvm -cpu host -m 256 -display none -net nic -net user,hostfwd=tcp::2222-:22,hostfwd=tcp::5000-:3000 -hda Paris.vmdk &
   kvm -enable-kvm -cpu host -m 256 -display none -net nic -net user,hostfwd=tcp::2223-:22,hostfwd=tcp::6000-:3000 -hda Melbourne.vmdk &

To avoid confusion when you are working with multiple machines, change the name of each machine from yottadbworkshop to its location. The examples here are from the Santiago machine. You should do likewise with Paris and Melbourne. To effect a name change will need to (as root) edit the files /etc/hosts and /etc/hostname to change yottadbworkshop to santiago and then reboot.

.. parsed-literal::
   yottadbuser@santiago:~$ cat /etc/hosts
   127.0.0.1       localhost
   127.0.1.1       santiago

   # The following lines are desirable for IPv6 capable hosts
   ::1     ip6-localhost ip6-loopback
   fe00::0 ip6-localnet
   ff00::0 ip6-mcastprefix
   ff02::1 ip6-allnodes
   ff02::2 ip6-allrouters
   
   yottadbuser@santiago:~$ cat /etc/hostname
   santiago
   yottadbuser@santiago:~$ 

You may also want to change the window/tab labels on your terminal emulator on the host to show which machine each session is connected to.

To make it more realistic (and to reduce the probability of operator error) on each machine, execute sudo dpkg-reconfigure tzdata to specify the “local” time zone. Select Paris and Santiago.

On each machine, edit exDir/env in each instance and change the line export ydb_repl_instname=dummy and the line export ydb_repl_instance=/home/yottadbuser/exDir/yottadb.repl to an instance file name for that instance. For example, on the Santiago instance:

.. parsed-literal::
   yottadbuser@santiago:~/exDir$ cat ydbenv 
   export ydb_dist=/usr/local/lib/yottadb/r122
   export ydb_gbldir=$HOME/exDir/yottadb.gld
   export ydb_log=/tmp/yottadb/r122
   export ydb_tmp=$ydb_log
   export ydb_principal_editing=EDITING
   export ydb_repl_instance=$HOME/exDir/yottadb.repl
   export ydb_repl_instname=Santiago
   export ydb_routines="$HOME/exDir* $ydb_dist/libyottadbutil.so"
   mkdir -p $ydb_tmp
   alias mumps=$ydb_dist/mumps
   alias mupip=$ydb_dist/mupip
   yottadbuser@santiago:~$

Then on each instance, create a replication instance file.

.. parsed-literal::
   yottadbuser\@santiago:~/exDir$ ls -l \*.repl
   ls: cannot access exDir/\*.repl: No such file or directory
   yottadbuser\@santiago:~/exDir$ source ydbenv 
   yottadbuser\@santiago:~/exDir$ mupip replicate -instance_create
   yottadbuser\@santiago:~/exDir$ ls -l \*.repl
   -rw-rw-r-- 1 yottadbuser yottadbuser 1536 2011-11-10 00:47 exDir/santiago.repl
   yottadbuser\@santiago:~/exDir$

Start the configuration with Paris as the originating primary instance, and Santiago and Melbourne in replicating secondary roles. The following commands, on the three instances can be executed in any order.

Start Melbourne as a replicating instance.

.. parsed-literal::
   yottadbuser@melbourne:~/exDir$ ./replicating_start 
   Tue Jan  23 11:08:09 2018 : Initiating START of source server for secondary instance [dummy]
   yottadbuser@melbourne:~/exDir$

Start Santiago as a replicating instance.

.. parsed-literal::
   yottadbuser@santiago:~/exDir$ ./replicating_start
   Tue Jan  23 12:08:01 2018 : Initiating START of source server for secondary instance [dummy]
   yottadbuser@santiago:~/exDir$

Start Paris as an originating instance replicating to Santiago and Melbourne (notice the use of ports on the host to reach the different replicating instances in the virtual machines).

.. parsed-literal::
   yottadbuser@paris:~/exDir$ mupip replicate -source -start -instsecondary=Santiago -secondary=10.0.2.2:4000 -buffsize=1048576 -log=/home/yottadbuser/exDir/source_Santiago.log
   Tue Jan  23 12:12:25 2018 : Initiating START of source server for secondary instance [Santiago]
   yottadbuser@paris:~/exDir$ mupip replicate -source -start -instsecondary=Melbourne -secondary=10.0.2.2:6000 -buffsize=1048576 -log=/home/yottadbuser/exDir/source_Melbourne.log
   Tue Jan  23 12:12:53 2018 : Initiating START of source server for secondary instance [Melbourne]
   yottadbuser@paris:~/exDir$


Start a YottaDB process in Paris and perform some database updates:

.. parsed-literal::
   yottadbuser@paris:~/exDir$ mumps -dir

   YDB>set ^Weather("Paris",$Piece($Horolog,",",1),$Piece($Horolog,",",2))="Rainy"

Verify that the data is replicated at Santiago and Melbourne. Execute the following at both instances:

.. parsed-literal::
   yottadbuser@melbourne:~/exDir$ mumps -dir

   YDB>zwrite ^Weather
   ^Weather("Paris",63523,51308)="Rainy"

   YDB>

Bring down Melbourne (simulating system maintenance, or a network outage), but leave Santiago untouched.

.. parsed-literal::
   yottadbuser@melbourne:~/exDir$ ./replicating_stop 
   Signalling immediate shutdown
   Tue Jan  23 03:24:46 2018 : Initiating shut down
   Tue Jan  23 03:24:47 2018 : Receive pool shared memory removed
   Tue Jan  23 03:24:47 2018 : Receive pool semaphore removed
   Tue Jan  23 03:24:47 2018 : Signalling shutdown immediate
   Tue Jan  23 03:24:47 2018 : Initiating SHUTDOWN operation on source server pid [1009] for secondary instance [dummy]
   Tue Jan  23 03:24:47 2018 : Waiting for upto [180] seconds for the source server to shutdown
   Tue Jan  23 03:24:49 2018 : Journal pool shared memory removed
   Tue Jan  23 03:24:49 2018 : Journal pool semaphore removed
   %YDB-I-MUFILRNDWNSUC, File /home/yottadbuser/exDir/aA.dat successfully rundown
   %YDB-I-MUFILRNDWNSUC, File /home/yottadbuser/exDir/others.dat successfully rundown
   yottadbuser@melbourne:~/exDir$ 

Create another update in Paris.

.. parsed-literal::
   YDB>set ^Weather("Paris",$Piece($Horolog,",",1),$Piece($Horolog,",",2))="Sunny"

Verify that this is updated in Santiago.

.. parsed-literal::
   YDB>zwrite ^Weather
   ^Weather("Paris",63523,51308)="Rainy"
   ^Weather("Paris",63523,51921)="Sunny"

   YDB>

But it is not replicated in Melbourne.

.. parsed-literal::
   YDB>zwrite ^Weather
   ^Weather("Paris",63523,51308)="Rainy"

   YDB>

Restart Melbourne as a replicating instance and notice that it catches up with updates at the originating instance when replication was not active in Melbourne.

.. parsed-literal::
   yottadbuser@melbourne:~/exDir$ exDir/replicating_start 
   Thu Nov 10 07:33:47 2011 : Initiating START of source server for secondary instance [dummy]
   yottadbuser@melbourne:~/exDir$ mumps -dir

   YDB>zwrite ^Weather
   ^Weather("Paris",63523,51308)="Rainy"
   ^Weather("Paris",63523,51921)="Sunny"

   YDB>

Now, simulate an unplanned outage of Paris by clicking on the “X” of the virtual machine console window, kill -9 of the process on the host, or otherwise powering down the virtual machine. Make Melbourne the new originating instance and Santiago its replicating instance. (In a controlled switchover/planned outage, bringing down the originating primary first helps to ensure that you do not have two concurrently operating originating primary instances.)

Bring down Melbourne as a replicating instance and bring it up as the originating instance. Notice that you can bring up the Source Server process to replicate to Paris – it will make the connection when Paris comes up.

.. parsed-literal::
   yottadbuser@melbourne:~/exDir$ ./replicating_stop
   Signalling immediate shutdown
   Tue Jan  23 12:30:01 2018 : Initiating shut down
   Tue Jan  23 12:30:02 2018 : Receive pool shared memory removed
   Tue Jan  23 12:30:02 2018 : Receive pool semaphore removed
   Tue Jan  23 12:30:02 2018 : Signalling shutdown immediate
   Tue Jan  23 12:30:02 2018 : Initiating SHUTDOWN operation on source server pid [1025] for secondary instance [dummy]
   Tue Jan  23 12:30:02 2018 : Waiting for upto [180] seconds for the source server to shutdown
   Tue Jan  23 12:30:03 2018 : Journal pool shared memory removed
   Tue Jan  23 12:30:03 2018 : Journal pool semaphore removed
   %YDB-I-MUFILRNDWNSUC, File /home/yottadbuser/exDir/aA.dat successfully rundown
   %YDB-I-MUFILRNDWNSUC, File /home/yottadbuser/exDir/others.dat successfully rundown
   yottadbuser@melbourne:~/exDir$ mupip replicate -source -start -instsecondary=Santiago -secondary=10.0.2.2:4000 -buffsize=1048576 -log=/home/yottadbuser/exDir/source_Santiago.log
   Tue Jan  23 12:30:42 2018 : Initiating START of source server for secondary instance [Santiago]
   yottadbuser@melbourne:~/exDir$ mupip replicate -source -start -instsecondary=Paris -secondary=10.0.2.2:5000 -buffsize=1048576 -log=/home/yottadbuser/exDir/source_Paris.log
   Tue Jan  23 12:31:06 2018 : Initiating START of source server for secondary instance [Paris]
   yottadbuser@melbourne:~/exDir$ 

Both Santiago and Paris should perform a rollback fetchresync before they become secondary instances to Melbourne. First Santiago (since Paris has crashed and is down; notice that the times look very different because they show times in their local timezones).

.. parsed-literal::
   yottadbuser@santiago:~/exDir$ ./replicating_stop
   Signalling immediate shutdown
   Tue Jan  23 21:31:42 2018 : Initiating shut down
   Tue Jan  23 21:31:43 2018 : Receive pool shared memory removed
   Tue Jan  23 21:31:43 2018 : Receive pool semaphore removed
   Tue Jan  23 21:31:43 2018 : Signalling shutdown immediate
   Tue Jan  23 21:31:43 2018 : Initiating SHUTDOWN operation on source server pid [1024] for secondary instance [dummy]
   Tue Jan  23 21:31:43 2018 : Waiting for upto [180] seconds for the source server to shutdown
   Tue Jan  23 21:31:44 2018 : Journal pool shared memory removed
   Tue Jan  23 21:31:44 2018 : Journal pool semaphore removed
   %YDB-I-MUFILRNDWNSUC, File /home/yottadbuser/exDir/aA.dat successfully rundown
   %YDB-I-MUFILRNDWNSUC, File /home/yottadbuser/exDir/others.dat successfully rundown

   yottadbuser@santiago:~/exDir$ mupip journal -rollback -backward -fetchresync=3000 -losttrans=/home/yottadbuser/exDir/Unreplic_Trans_Report_`date +%Y%m%d%H%M%S`.txt "*"
   %YDB-I-MUJNLSTAT, Initial processing started at Tue Jan  23 21:32:10 2018
   %YDB-I-MUJNLSTAT, FETCHRESYNC processing started at Tue Jan  23 21:32:10 2018
   Tue Jan  23 21:32:10 2018 : Assuming primary supports multisite functionality. Connecting using multisite communication protocol.
   Tue Jan  23 21:32:10 2018 : Waiting for a connection...
   Tue Jan  23 21:32:10 2018 : Connection established, using TCP send buffer size 87040 receive buffer size 374400
   Tue Jan  23 21:32:10 2018 : Connection information:: Local: ::ffff:10.0.2.15:3000 Remote: ::ffff:10.0.2.2:48768
   Tue Jan  23 21:32:10 2018 : Sending REPL_FETCH_RESYNC message with seqno 3 [0x3]
   Tue Jan  23 21:32:10 2018 : Source and Receiver sides have same endianness
   Tue Jan  23 21:32:10 2018 : Remote side source log file path is /home/yottadbuser/exDir/source_Santiago.log; Source Server PID = 1035
   Tue Jan  23 21:32:10 2018 : Received REPL_NEED_INSTINFO message from primary instance [Melbourne]
   Tue Jan  23 21:32:10 2018 : Sending REPL_INSTINFO message
   Tue Jan  23 21:32:10 2018 : Received REPL_NEED_HISTINFO message for Seqno 3 [0x3]
   Tue Jan  23 21:32:10 2018 : Sending REPL_HISTINFO message with seqno 1 [0x1]
   Tue Jan  23 21:32:10 2018 : History sent : Start Seqno = 1 [0x1] : Stream Seqno = 0 [0x0] : Root Primary = [Paris] : Cycle = [1] : Creator pid = 1007 : Created time = 1417547545 [0x547e0f19] : History number = 0 : Prev History number = -1 : Stream # = 0 : History type = 1
   Tue Jan  23 21:32:10 2018 : Received REPL_RESYNC_SEQNO message
   Tue Jan  23 21:32:10 2018 : Received RESYNC SEQNO is 3 [0x3]
   %YDB-I-MUJNLSTAT, Backward processing started at Tue Jan  2 21:32:10 2018
   %YDB-I-RESOLVESEQNO, Resolving until sequence number 3 [0x0000000000000003]
   %YDB-I-MUJNLSTAT, Before image applying started at Tue Jan  2 21:32:10 2018
   %YDB-I-FILERENAME, File /home/yottadbuser/exDir/aA.mjl is renamed to /home/yottadbuser/exDir/aA.mjl_2018336213210
   %YDB-I-FILERENAME, File /home/yottadbuser/exDir/others.mjl is renamed to /home/yottadbuser/exDir/others.mjl_2018336213210
   %YDB-I-MUJNLSTAT, Forward processing started at Tue Jan  2 21:32:10 2018
   %YDB-I-FILECREATE, Lost transactions extract file /home/yottadbuser/exDir/Unreplic_Trans_Report_20181202213210.txt created
   %YDB-I-RLBKJNSEQ, Journal seqno of the instance after rollback is 3 [0x0000000000000003]
   %YDB-S-JNLSUCCESS, Show successful
   %YDB-S-JNLSUCCESS, Verify successful
   %YDB-S-JNLSUCCESS, Rollback successful
   %YDB-I-MUJNLSTAT, End processing at Tue Jan  23 21:32:11 2018

   yottadbuser@santiago:~/exDir$ ./replicating_start
   Tue Jan  23 21:33:22 2018 : Initiating START of source server for secondary instance [dummy]
   yottadbuser@santiago:~/exDir$

Note that the Unreplicated Transaction File has no transactions rolled back:

.. parsed-literal::
   yottadbuser\@santiago:~/exDir$ cat Unreplic_Trans_Report_20181202213210.txt 
   GDSJEX07 ROLLBACK SECONDARY Santiago
   02\63523,77504\4\1024\0
   03\63523,77504\4\1024\0\3
   yottadbuser\@santiago:~/exDir$

Now reboot Paris to simulate its recovery. When the system comes up (before performing any other database access), perform a rollback fetchresync.

.. parsed-literal::
   yottadbuser@paris:~/exDir$ mupip journal -rollback -backward -fetchresync=3000 -losttrans=/home/yottadbuser/exDir/Unreplic_Trans_Report_`date +%Y%m%d%H%M%S`.txt "*"
   %YDB-I-MUJNLSTAT, Initial processing started at Tue Jan  23 14:35:55 2018
   %YDB-I-MUJNLSTAT, FETCHRESYNC processing started at Tue Jan  23 14:35:55 2018
   Tue Jan  23 14:35:55 2018 : Assuming primary supports multisite functionality. Connecting using multisite communication protocol.
   Tue Jan  23 14:35:55 2018 : Waiting for a connection...
   Tue Jan  23 14:35:56 2018 : Connection established, using TCP send buffer size 87040 receive buffer size 374400
   Tue Jan  23 14:35:56 2018 : Connection information:: Local: ::ffff:10.0.2.15:3000 Remote: ::ffff:10.0.2.2:49353
   Tue Jan  23 14:35:56 2018 : Sending REPL_FETCH_RESYNC message with seqno 3 [0x3]
   Tue Jan  23 14:35:56 2018 : Source and Receiver sides have same endianness
   Tue Jan  23 14:35:56 2018 : Remote side source log file path is /home/yottadbuser/exDir/source_Paris.log; Source Server PID = 1037
   Tue Jan  23 14:35:56 2018 : Received REPL_NEED_INSTINFO message from primary instance [Melbourne]
   Tue Jan  23 14:35:56 2018 : Sending REPL_INSTINFO message
   Tue Jan  23 14:35:56 2018 : Received REPL_NEED_HISTINFO message for Seqno 3 [0x3]
   Tue Jan  23 14:35:56 2018 : Sending REPL_HISTINFO message with seqno 1 [0x1]
   Tue Jan  23 14:35:56 2018 : History sent : Start Seqno = 1 [0x1] : Stream Seqno = 0 [0x0] : Root Primary = [Paris] : Cycle = [1] : Creator pid = 1007 : Created time = 1417547545 [0x547e0f19] : History number = 0 : Prev History number = -1 : Stream # = 0 : History type = 1
   Tue Jan  23 14:35:56 2018 : Received REPL_RESYNC_SEQNO message
   Tue Jan  23 14:35:56 2018 : Received RESYNC SEQNO is 3 [0x3]
   %YDB-I-MUJNLSTAT, Backward processing started at Tue Jan  23 14:35:56 2018
   %YDB-I-RESOLVESEQNO, Resolving until sequence number 3 [0x0000000000000003]
   %YDB-I-MUJNLSTAT, Before image applying started at Tue Jan  23 14:35:56 2018
   %YDB-I-FILERENAME, File /home/yottadbuser/exDir/aA.mjl is renamed to /home/yottadbuser/exDir/aA.mjl_2018336143556
   %YDB-I-FILERENAME, File /home/yottadbuser/exDir/others.mjl is renamed to /home/yottadbuser/exDir/others.mjl_2018336143556
   %YDB-I-MUJNLSTAT, Forward processing started at Tue Jan  23 14:35:56 2018
   %YDB-I-FILECREATE, Lost transactions extract file /home/yottadbuser/exDir/Unreplic_Trans_Report_20181202143555.txt created
   %YDB-I-RLBKJNSEQ, Journal seqno of the instance after rollback is 3 [0x0000000000000003]
   %YDB-S-JNLSUCCESS, Show successful
   %YDB-S-JNLSUCCESS, Verify successful
   %YDB-S-JNLSUCCESS, Rollback successful
   %YDB-I-MUJNLSTAT, End processing at Tue Jan  23 14:35:57 2018

   yottadbuser@paris:~/exDir$ ./replicating_start
   Tue Jan  23 14:37:14 2018 : Initiating START of source server for secondary instance [dummy]
   yottadbuser@paris:~/exDir$

Now, create a database update in Melbourne.

.. parsed-literal::
   YDB>set ^Weather("Melbourne",$Piece($Horolog,",",1),$Piece($Horolog,",",2))="Stormy"

And confirm that it is replicated to both Santiago and Paris.

.. parsed-literal::
   YDB>zwrite ^Weather
   ^Weather("Paris",63523,51308)="Rainy"
   ^Weather("Paris",63523,51921)="Sunny"
   ^Weather("Melbourne",63524,13176)="Stormy"

Shut down all three instances cleanly to end the exercise. On the originating instance in Melbourne:

.. parsed-literal::
   yottadbuser@melbourne:~/exDir$ ./originating_stop
   Wed Jan  24 03:42:11 2018 : Signalling shutdown immediate
   Wed Jan  24 03:42:11 2018 : Initiating SHUTDOWN operation on source server pid [1049] for secondary instance [Santiago]
   Wed Jan  24 03:42:11 2018 : Initiating SHUTDOWN operation on source server pid [1051] for secondary instance [Paris]
   Wed Jan  24 03:42:11 2018 : Waiting for upto [180] seconds for the source server to shutdown
   Wed Jan  24 03:42:12 2018 : Journal pool shared memory removed
   Wed Jan  24 03:42:12 2018 : Journal pool semaphore removed
   %YDB-I-MUFILRNDWNSUC, File /home/yottadbuser/exDir/aA.dat successfully rundown
   %YDB-I-MUFILRNDWNSUC, File /home/yottadbuser/exDir/others.dat successfully rundown
   yottadbuser@melbourne:~/exDir$

And on the replicating instances in Santiago and Paris, execute replicating_stop to stop the replication.

.. parsed-literal::
   yottadbuser@paris:~/exDir$ ./replicating_stop
   Signalling immediate shutdown
   Tue Jan  23 14:42:53 2018 : Initiating shut down
   Tue Jan  23 14:42:54 2018 : Receive pool shared memory removed
   Tue Jan  23 14:42:54 2018 : Receive pool semaphore removed
   Tue Jan  23 14:42:54 2018 : Signalling shutdown immediate
   Tue Jan  23 14:42:54 2018 : Initiating SHUTDOWN operation on source server pid [1018] for secondary instance [dummy]
   Tue Jan  23 14:42:54 2018 : Waiting for upto [180] seconds for the source server to shutdown
   Tue Jan  23 14:42:55 2018 : Journal pool shared memory removed
   Tue Jan  23 14:42:55 2018 : Journal pool semaphore removed
   %YDB-I-MUFILRNDWNSUC, File /home/yottadbuser/exDir/aA.dat successfully rundown
   %YDB-I-MUFILRNDWNSUC, File /home/yottadbuser/exDir/others.dat successfully rundown
   yottadbuser@paris:~/exDir$

**Replication and Backlogs**

In an ideal world, an originating instance never goes down with a backlog. In the real world, it may well go down with a backlog of updates that have not been replicated. Asynchronous replication is a consequence of the fact that committing an update requires a remote commit and a local commit, that the loss of the network between the remote instance and the local instance stops the local instance; and network delays slow down the local instance.

In order to provide continuity of business, when an originating primary instance goes down, a former replicating secondary instance can be brought up as the new originating primary instance to keep the application available. When the former originating primary instance comes up, it is in a secondary role, and the updates that were part of the backlog must be handled. YottaDB provides the hooks needed to create applications that are continuously available, but the application must take advantage of these hooks. Consider the following two-instance example (the notation P: 100 means that the site is operating as the primary and has committed update number 100):

+------------------------------+----------------------------------------------------------------------------------------------------------+
| Santiago                     |        Melbourne                                                                                         |
+==============================+==========================================================================================================+
| P:100                        |        S:95 (backlog of 5 updates)                                                                       |
+------------------------------+----------------------------------------------------------------------------------------------------------+
| Crashes                      |        P:95 (becomes the originating instance, starts processing, and keeps the organization operational)|
+------------------------------+----------------------------------------------------------------------------------------------------------+
| Repaired and brought back up |        P:120 (processing has moved it ahead)                                                             |
+------------------------------+----------------------------------------------------------------------------------------------------------+

This situation needs to be remedied, because updates (transactions) 96-100 on Santiago are different from updates 96-100 on Melbourne. This has a YottaDB part and an application software part. The YottaDB part is to rollback the transactions on the former originating primary instance with the mupip journal -rollback -fetchresync command. These rolled back updates (“unreplicated” or “lost” transactions) are placed in a file and must be transmitted to the new originating instance for reprocessing/reconciliation by application logic.

+---------------------------------------------------------------------------------+-------------------------------------------------------------------+
| Santiago                                                                        |      Melbourne                                                    |
+=================================================================================+===================================================================+
| S: 95 (database rolled back; updates 96-100 in unreplicated transaction file)   |      P:120                                                        |
+---------------------------------------------------------------------------------+-------------------------------------------------------------------+
| S: 120 (catches up with Melbourne once replication resumes)                     |      P: 120 (receives unreplicated transaction file)              |
+---------------------------------------------------------------------------------+-------------------------------------------------------------------+
| S: 125 (unreplicated transactions make it back after reprocessing)              |      P: 125 (processing unreplicated transactions moves it ahead) |
+---------------------------------------------------------------------------------+-------------------------------------------------------------------+

Adding Paris to the example above complicates it only slightly. There are two cases to consider when Santiago crashes:

- Paris is at transaction 95 or less. In this case, Paris simply becomes a replicating instance to Melbourne and there is no need for Paris to rollback any transactions.

- Paris is at a transaction 96 or more. In this case, when Paris becomes a replicating instance to Melbourne, it performs a rollback to transaction 95 before starting replication. These transactions in the unreplicated transaction file do not need to be sent to Melbourne for reprocessing because they will be in the unreplicated transaction file from Santiago.

**Exercise - Replication and Backlogs**

This exercise simulates a replication with a backlog (smaller than the five updates in the example above). Start with Santiago as the originating instance replicating to Paris and Melbourne as replicating instances. Since Santiago most recently was a secondary instance, you should start with mupip journal -rollback -fetchresync in Melbourne and Paris.

Start Santiago as the originating instance:

.. parsed-literal::
   yottadbuser@santiago:~/exDir$ mupip replicate -source -start -instsecondary=Paris -secondary=10.0.2.2:5000 -buffsize=1048576 -log=/home/yottadbuser/exDir/source_Paris.log
   Tue Jan  2 23:42:32 2018 : Initiating START of source server for secondary instance [Paris]

   yottadbuser@santiago:~/exDir$ mupip replicate -source -start -instsecondary=Melbourne -secondary=10.0.2.2:6000 -buffsize=1048576 -log=/home/yottadbuser/exDir/source_Melbourne.log
   Tue Jan  2 23:43:02 2018 : Initiating START of source server for secondary instance [Melbourne]
   yottadbuser@santiago:~/exDir$

At Paris (and also in Melbourne) perform the fetchresync operation and then start replication. You can ask YottaDB to tell you the health of replication and also the replication backlog.

.. parsed-literal::
   yottadbuser@paris:~/exDir$ mupip journal -rollback -backward -fetchresync=3000 -losttrans=/home/yottadbuser/exDir/Unreplic_Trans_Report_`date +%Y%m%d%H%M%S`.txt "*"
   %YDB-I-MUJNLSTAT, Initial processing started at Tue Jan  2 16:43:40 2018
   %YDB-I-MUJNLSTAT, FETCHRESYNC processing started at Tue Jan  2 16:43:40 2018
   Tue Jan  23 16:43:40 2018 : Assuming primary supports multisite functionality. Connecting using multisite communication protocol.
   Tue Jan  23 16:43:40 2018 : Waiting for a connection...
   Tue Jan  23 16:43:41 2018 : Connection established, using TCP send buffer size 87040 receive buffer size 374400
   Tue Jan  23 16:43:41 2018 : Connection information:: Local: ::ffff:10.0.2.15:3000 Remote: ::ffff:10.0.2.2:49967
   Tue Jan  23 16:43:41 2018 : Sending REPL_FETCH_RESYNC message with seqno 4 [0x4]
   Tue Jan  23 16:43:41 2018 : Source and Receiver sides have same endianness
   Tue Jan  23 16:43:41 2018 : Remote side source log file path is /home/yottadbuser/exDir/source_Paris.log; Source Server PID = 1063
   Tue Jan  23 16:43:41 2018 : Received REPL_NEED_INSTINFO message from primary instance [Santiago]
   Tue Jan  23 16:43:41 2018 : Sending REPL_INSTINFO message
   Tue Jan  23 16:43:41 2018 : Received REPL_NEED_HISTINFO message for Seqno 4 [0x4]
   Tue Jan  23 16:43:41 2018 : Sending REPL_HISTINFO message with seqno 3 [0x3]
   Tue Jan  23 16:43:41 2018 : History sent : Start Seqno = 3 [0x3] : Stream Seqno = 0 [0x0] : Root Primary = [Melbourne] : Cycle = [1] : Creator pid = 1035 : Created time = 1417548642 [0x547e1362] : History number = 1 : Prev History number = 0 : Stream # = 0 : History type = 1
   Tue Jan  23 16:43:41 2018 : Received REPL_RESYNC_SEQNO message
   Tue Jan  23 16:43:41 2018 : Received RESYNC SEQNO is 4 [0x4]
   %YDB-I-MUJNLSTAT, Backward processing started at Tue Jan  23 16:43:41 2018
   %YDB-I-RESOLVESEQNO, Resolving until sequence number 4 [0x0000000000000004]
   %YDB-I-MUJNLSTAT, Before image applying started at Tue Jan  23 16:43:41 2018
   %YDB-I-FILERENAME, File /home/yottadbuser/exDir/aA.mjl is renamed to /home/yottadbuser/exDir/aA.mjl_2018336164341
   %YDB-I-FILERENAME, File /home/yottadbuser/exDir/others.mjl is renamed to /home/yottadbuser/exDir/others.mjl_2018336164341
   %YDB-I-MUJNLSTAT, Forward processing started at Tue Jan  23 16:43:41 2018
   %YDB-I-FILECREATE, Lost transactions extract file /home/yottadbuser/exDir/Unreplic_Trans_Report_20181202164340.txt created
   %YDB-I-RLBKJNSEQ, Journal seqno of the instance after rollback is 4 [0x0000000000000004]
   %YDB-S-JNLSUCCESS, Show successful
   %YDB-S-JNLSUCCESS, Verify successful
   %YDB-S-JNLSUCCESS, Rollback successful
   %YDB-I-MUJNLSTAT, End processing at Tue Jan  23 16:43:41 2018

   yottadbuser@paris:~/exDir$ ./replicating_start
   Tue Jan  23 16:47:50 2018 : Initiating START of source server for secondary instance [dummy]

   yottadbuser@paris:~/exDir$ mupip replicate -receiver -checkhealth
   PID 1040 Receiver server is alive
   PID 1041 Update process is alive

   yottadbuser@paris:~/exDir$ mupip replicate -receiver -showbacklog
   0 : number of backlog transactions received by receiver server and yet to be processed by update process
   3 : sequence number of last transaction received from Source Server and written to receive pool
   3 : sequence number of last transaction processed by update process
   yottadbuser@paris:~/exDir$

You can also check replication health and the backlog on the originating instance, Santiago. Notice that if you do not specify which replication connection you want details for, you get information on all of them.

.. parsed-literal::
   yottadbuser@santiago:~/exDir$ mupip replicate -source -checkhealth
   Tue Jan  23 23:51:20 2018 : Initiating CHECKHEALTH operation on source server pid [1063] for secondary instance name [Paris]
   PID 1063 Source server is alive in ACTIVE mode
   Tue Jan  23 23:51:20 2018 : Initiating CHECKHEALTH operation on source server pid [1065] for secondary instance name [Melbourne]
   PID 1065 Source server is alive in ACTIVE mode

   yottadbuser@santiago:~/exDir$ mupip replicate -source -showbacklog
   Tue Jan  23 23:51:35 2018 : Initiating SHOWBACKLOG operation on source server pid [1063] for secondary instance [Paris]
   0 : backlog number of transactions written to journal pool and yet to be sent by the source server
   3 : sequence number of last transaction written to journal pool
   3 : sequence number of last transaction sent by source server
   Tue Jan  2 23:51:35 2018 : Initiating SHOWBACKLOG operation on source server pid [1065] for secondary instance [Melbourne]
   0 : backlog number of transactions written to journal pool and yet to be sent by the source server
   3 : sequence number of last transaction written to journal pool
   3 : sequence number of last transaction sent by source server
   yottadbuser@santiago:~/exDir$

Now create an update in Santiago.

.. parsed-literal::
   YDB>set ^Weather("Santiago",$Piece($Horolog,",",1),$Piece($Horolog,",",2))="Snowing"

Verify that it is replicated in Paris and Melbourne.

.. parsed-literal::
   YDB>zwrite ^Weather
   ^Weather("Santiago",63523,86064)="Snowing"
   ^Weather("Paris",63523,51308)="Rainy"
   ^Weather("Paris",63523,51921)="Sunny"
   ^Weather("Melbourne",63524,13176)="Stormy"

Now simulate a failure with a backlog by first shutting down replication in Melbourne, and then making an update in Santiago. In Melbourne:

.. parsed-literal::
   yottadbuser@melbourne:~/exDir$ ./replicating_stop
   Signalling immediate shutdown
   Wed Jan  24 05:56:09 2018 : Initiating shut down
   Wed Jan  24 05:56:10 2018 : Receive pool shared memory removed
   Wed Jan  24 05:56:10 2018 : Receive pool semaphore removed
   Wed Jan  24 05:56:10 2018 : Signalling shutdown immediate
   Wed Jan  24 05:56:10 2018 : Initiating SHUTDOWN operation on source server pid [1075] for secondary instance [dummy]
   Wed Jan  24 05:56:10 2018 : Waiting for upto [180] seconds for the source server to shutdown
   Wed Jan  24 05:56:11 2018 : Journal pool shared memory removed
   Wed Jan  24 05:56:11 2018 : Journal pool semaphore removed
   %YDB-I-MUFILRNDWNSUC, File /home/yottadbuser/exDir/aA.dat successfully rundown
   %YDB-I-MUFILRNDWNSUC, File /home/yottadbuser/exDir/others.dat successfully rundown
   yottadbuser@melbourne:~/exDir$

In Santiago:

.. parsed-literal::
   YDB>set ^Weather("Santiago",$Piece($Horolog,",",1),$Piece($Horolog,",",2))="Blizzards"

   YDB>zsystem "$ydb_dist/mupip replicate -source -showbacklog"
   Tue Jan  23 23:57:00 2018 : Initiating SHOWBACKLOG operation on source server pid [1063] for secondary instance [Paris]
   0 : backlog number of transactions written to journal pool and yet to be sent by the source server
   5 : sequence number of last transaction written to journal pool
   5 : sequence number of last transaction sent by source server
   Tue Jan  23 23:57:00 2018 : Initiating SHOWBACKLOG operation on source server pid [1065] for secondary instance [Melbourne]
   1 : backlog number of transactions written to journal pool and yet to be sent by the source server
   5 : sequence number of last transaction written to journal pool
   4 : sequence number of last transaction sent by source server

   YDB>

Notice that there is a backlog to Melbourne, but none to Paris. Now shut down replication in Paris and make another update in Santiago. Verify that there is a backlog of 1 to Paris and 2 to Melbourne.

In Paris:

.. parsed-literal::
   yottadbuser@paris:~/exDir$ ./replicating_stop
   Signalling immediate shutdown
   Tue Jan  23 16:57:32 2018 : Initiating shut down
   Tue Jan  23 16:57:33 2018 : Receive pool shared memory removed
   Tue Jan  23 16:57:33 2018 : Receive pool semaphore removed
   Tue Jan  23 16:57:33 2018 : Signalling shutdown immediate
   Tue Jan  23 16:57:33 2018 : Initiating SHUTDOWN operation on source server pid [1038] for secondary instance [dummy]
   Tue Jan  23 16:57:33 2018 : Waiting for upto [180] seconds for the source server to shutdown
   Tue Jan  23 16:57:34 2018 : Journal pool shared memory removed
   Tue Jan  23 16:57:34 2018 : Journal pool semaphore removed
   %YDB-I-MUFILRNDWNSUC, File /home/yottadbuser/exDir/aA.dat successfully rundown
   %YDB-I-MUFILRNDWNSUC, File /home/yottadbuser/exDir/others.dat successfully rundown
   yottadbuser@paris:~/exDir$

In Santiago:

.. parsed-literal::
   YDB>set ^Weather("Santiago",$Piece($Horolog,",",1),$Piece($Horolog,",",2))="Cloudy"

   YDB>zwrite ^Weather
   ^Weather("Santiago",63523,86064)="Snowing"
   ^Weather("Santiago",63523,86207)="Blizzards"
   ^Weather("Santiago",63523,86342)="Cloudy"
   ^Weather("Paris",63523,51308)="Rainy"
   ^Weather("Paris",63523,51921)="Sunny"
   ^Weather("Melbourne",63524,13176)="Stormy"

   YDB>zsystem "$ydb_dist/mupip replicate -source -showbacklog"
   Tue Jan  23 23:59:27 2018 : Initiating SHOWBACKLOG operation on source server pid [1063] for secondary instance [Paris]
   1 : backlog number of transactions written to journal pool and yet to be sent by the source server
   6 : sequence number of last transaction written to journal pool
   5 : sequence number of last transaction sent by source server
   Tue Jan  23 23:59:27 2018 : Initiating SHOWBACKLOG operation on source server pid [1065] for secondary instance [Melbourne]
   2 : backlog number of transactions written to journal pool and yet to be sent by the source server
   6 : sequence number of last transaction written to journal pool
   4 : sequence number of last transaction sent by source server

   YDB>

Now crash Santiago. You have a choice of bringing up Paris or Melbourne. If you don't have time to make a decision as to which replicating instance to make the new primary, just choose the most convenient. If you have time to make a decision, you can see which one is further ahead by looking at the “Region Seqno” field in the database file header with DSE (in a multi-region database, you need to look at all replicated regions and take the maximum).

In Paris (note the use of the find -region DSE command):

.. parsed-literal::
   yottadbuser@paris:~/exDir$ $ydb_dist/dse


   File    /home/yottadbuser/exDir/aA.dat
   Region  A

   DSE> dump -fileheader

   File            /home/yottadbuser/exDir/aA.dat
   Region          A
   Date/Time       23-JAN-2018 17:11:44 [$H = 63523,61904]

    Access method                          BG  Global Buffers                1000
    Reserved Bytes                          0  Block size (in bytes)         4096
    Maximum record size                  4080  Starting VBN                   513
    Maximum key size                      255  Total blocks            0x00001392
    Null subscripts                     NEVER  Free blocks             0x00001384
    Standard Null Collation              TRUE  Free space              0x00000000
    Last Record Backup     0x0000000000000001  Extension Count              10000
    Last Database Backup   0x0000000000000001  Number of local maps            10
    Last Bytestream Backup 0x0000000000000001  Lock space              0x00000028
    In critical section            0x00000000  Timers pending                   0
    Cache freeze id                0x00000000  Flush timer            00:00:01:00
    Freeze match                   0x00000000  Flush trigger                  938
    Current transaction    0x0000000000000002  No. of writes/flush              7
    Maximum TN             0xFFFFFFFF83FFFFFF  Certified for Upgrade to        V6
    Maximum TN Warn        0xFFFFFFFD93FFFFFF  Desired DB Format               V6
    Master Bitmap Size                    496  Blocks to Upgrade       0x00000000
    Create in progress                  FALSE  Modified cache blocks            0
    Reference count                         1  Wait Disk                        0
    Journal State               [inactive] ON  Journal Before imaging        TRUE
    Journal Allocation                   2048  Journal Extension             2048
    Journal Buffer Size                  2312  Journal Alignsize             4096
    Journal AutoSwitchLimit           8386560  Journal Epoch Interval         300
    Journal Yield Limit                     8  Journal Sync IO              FALSE
    Journal File: /home/yottadbuser/exDir/aA.mjl
    Mutex Hard Spin Count                 128  Mutex Sleep Spin Count         128
    Mutex Queue Slots                    1024  KILLs in progress                0
    Replication State                      ON  Region Seqno    0x0000000000000004
    Zqgblmod Seqno         0x0000000000000003  Zqgblmod Trans  0x0000000000000002
    Endian Format                      LITTLE  Commit Wait Spin Count          16
    Database file encrypted             FALSE  Inst Freeze on Error         FALSE
    Spanning Node Absent                 TRUE  Maximum Key Size Assured      TRUE

    DSE> find -region
    List of global directory:       


    File    /home/yottadbuser/exDir/aA.dat
    Region  A

    File    /home/yottadbuser/exDir/yottadb.dat
    Region  DEFAULT
    DSE> find -region=DEFAULT

    File    /home/yottadbuser/exDir/yottadb.dat
    Region  DEFAULT

    DSE> dump -fileheader

    File            /home/yottadbuser/exDir/others.dat
    Region          DEFAULT
    Date/Time       23-JAN-2018 17:12:57 [$H = 63523,61977]

      Access method                          BG  Global Buffers                1000
      Reserved Bytes                          0  Block size (in bytes)         4096
      Maximum record size                  4080  Starting VBN                   513
      Maximum key size                      255  Total blocks            0x00001392
      Null subscripts                     NEVER  Free blocks             0x00001382
      Standard Null Collation              TRUE  Free space              0x00000000
      Last Record Backup     0x0000000000000001  Extension Count              10000
      Last Database Backup   0x0000000000000001  Number of local maps            10
      Last Bytestream Backup 0x0000000000000001  Lock space              0x00000028
      In critical section            0x00000000  Timers pending                   0
      Cache freeze id                0x00000000  Flush timer            00:00:01:00
      Freeze match                   0x00000000  Flush trigger                  938
      Current transaction    0x0000000000000007  No. of writes/flush              7
      Maximum TN             0xFFFFFFFF83FFFFFF  Certified for Upgrade to        V6
      Maximum TN Warn        0xFFFFFFFD93FFFFFF  Desired DB Format               V6
      Master Bitmap Size                    496  Blocks to Upgrade       0x00000000
      Create in progress                  FALSE  Modified cache blocks            0
      Reference count                         1  Wait Disk                        0
      Journal State               [inactive] ON  Journal Before imaging        TRUE
      Journal Allocation                   2048  Journal Extension             2048
      Journal Buffer Size                  2312  Journal Alignsize             4096
      Journal AutoSwitchLimit           8386560  Journal Epoch Interval         300
      Journal Yield Limit                     8  Journal Sync IO              FALSE
      Journal File: /home/yottadbuser/exDir/others.mjl
      Mutex Hard Spin Count                 128  Mutex Sleep Spin Count         128
      Mutex Queue Slots                    1024  KILLs in progress                0
      Replication State                      ON  Region Seqno    0x0000000000000006
      Zqgblmod Seqno         0x0000000000000003  Zqgblmod Trans  0x0000000000000004
      Endian Format                      LITTLE  Commit Wait Spin Count          16
      Database file encrypted             FALSE  Inst Freeze on Error         FALSE
      Spanning Node Absent                 TRUE  Maximum Key Size Assured      TRUE

      DSE> exit
      yottadbuser@paris:~/exDir$

And in Melbourne:

.. parsed-literal::
   yottadbuser@melbourne:~/exDir$ $ydb_dist/dse

   File    /home/yottadbuser/exDir/aA.dat
   Region  A

   DSE> dump -fileheader

   File            /home/yottadbuser/exDir/aA.dat
   Region          A
   Date/Time       24-JAN-2018 06:15:09 [$H = 63524,22509]

     Access method                          BG  Global Buffers                1000
     Reserved Bytes                          0  Block size (in bytes)         4096
     Maximum record size                  4080  Starting VBN                   513
     Maximum key size                      255  Total blocks            0x00001392
     Null subscripts                     NEVER  Free blocks             0x00001384
     Standard Null Collation              TRUE  Free space              0x00000000
     Last Record Backup     0x0000000000000001  Extension Count              10000
     Last Database Backup   0x0000000000000001  Number of local maps            10
     Last Bytestream Backup 0x0000000000000001  Lock space              0x00000028
     In critical section            0x00000000  Timers pending                   0
     Cache freeze id                0x00000000  Flush timer            00:00:01:00
     Freeze match                   0x00000000  Flush trigger                  938
     Current transaction    0x0000000000000002  No. of writes/flush              7
     Maximum TN             0xFFFFFFFF83FFFFFF  Certified for Upgrade to        V6
     Maximum TN Warn        0xFFFFFFFD93FFFFFF  Desired DB Format               V6
     Master Bitmap Size                    496  Blocks to Upgrade       0x00000000
     Create in progress                  FALSE  Modified cache blocks            0
     Reference count                         1  Wait Disk                        0
     Journal State               [inactive] ON  Journal Before imaging        TRUE
     Journal Allocation                   2048  Journal Extension             2048
     Journal Buffer Size                  2312  Journal Alignsize             4096
     Journal AutoSwitchLimit           8386560  Journal Epoch Interval         300
     Journal Yield Limit                     8  Journal Sync IO              FALSE
     Journal File: /home/yottadbuser/exDir/aA.mjl
     Mutex Hard Spin Count                 128  Mutex Sleep Spin Count         128
     Mutex Queue Slots                    1024  KILLs in progress                0
     Replication State                      ON  Region Seqno    0x0000000000000004
     Zqgblmod Seqno         0x0000000000000001  Zqgblmod Trans  0x0000000000000002
     Endian Format                      LITTLE  Commit Wait Spin Count          16
     Database file encrypted             FALSE  Inst Freeze on Error         FALSE
     Spanning Node Absent                 TRUE  Maximum Key Size Assured      TRUE

   DSE> find -region=DEFAULT

   File    /home/yottadbuser/exDir/yottadb.dat
   Region  DEFAULT

   DSE> dump -fileheader

   File            /home/yottadbuser/exDir/others.dat
   Region          DEFAULT
   Date/Time       24-JAN-2018 06:16:12 [$H = 63524,22572]
      Access method                          BG  Global Buffers                1000
      Reserved Bytes                          0  Block size (in bytes)         4096
      Maximum record size                  4080  Starting VBN                   513
      Maximum key size                      255  Total blocks            0x00001392
      Null subscripts                     NEVER  Free blocks             0x00001382
      Standard Null Collation              TRUE  Free space              0x00000000
      Last Record Backup     0x0000000000000001  Extension Count              10000
      Last Database Backup   0x0000000000000001  Number of local maps            10
      Last Bytestream Backup 0x0000000000000001  Lock space              0x00000028
      In critical section            0x00000000  Timers pending                   0
      Cache freeze id                0x00000000  Flush timer            00:00:01:00
      Freeze match                   0x00000000  Flush trigger                  938
      Current transaction    0x0000000000000006  No. of writes/flush              7
      Maximum TN             0xFFFFFFFF83FFFFFF  Certified for Upgrade to        V6
      Maximum TN Warn        0xFFFFFFFD93FFFFFF  Desired DB Format               V6
      Master Bitmap Size                    496  Blocks to Upgrade       0x00000000
      Create in progress                  FALSE  Modified cache blocks            0
      Reference count                         1  Wait Disk                        0
      Journal State               [inactive] ON  Journal Before imaging        TRUE
      Journal Allocation                   2048  Journal Extension             2048
      Journal Buffer Size                  2312  Journal Alignsize             4096
      Journal AutoSwitchLimit           8386560  Journal Epoch Interval         300
      Journal Yield Limit                     8  Journal Sync IO              FALSE
      Journal File: /home/yottadbuser/exDir/others.mjl
      Mutex Hard Spin Count                 128  Mutex Sleep Spin Count         128
      Mutex Queue Slots                    1024  KILLs in progress                0
      Replication State                      ON  Region Seqno    0x0000000000000005
      Zqgblmod Seqno         0x0000000000000001  Zqgblmod Trans  0x0000000000000002
      Endian Format                      LITTLE  Commit Wait Spin Count          16
      Database file encrypted             FALSE  Inst Freeze on Error         FALSE
      Spanning Node Absent                 TRUE  Maximum Key Size Assured      TRUE

   DSE> exit
   yottadbuser@melbourne:~/exDir$ 

Since the largest Region Seqno is 6 (region DEFAULT in Paris), that is the preferred new originating primary instance. So, make it the new originating primary.

.. parsed-literal::
   yottadbuser@paris:~/exDir$ mupip replicate -source -start -instsecondary=Melbourne -secondary=10.0.2.2:6000 -buffsize=1048576 -log=/home/yottadbuser/exDir/source_Melbourne.log
   Tue Jan  2 17:20:13 2018 : Initiating START of source server for secondary instance [Melbourne]
   yottadbuser@paris:~/exDir$ mupip replicate -source -start -instsecondary=Santiago -secondary=10.0.2.2:4000 -buffsize=1048576 -log=/home/yottadbuser/exDir/source_Santiago.log
   Tue Jan  2 17:20:39 2018 : Initiating START of source server for secondary instance [Santiago]
   yottadbuser@paris:~/exDir$

On Melbourne, perform the mupip journal -rollback -fetchresync operation and start operation as a replicating instance.

.. parsed-literal::
   yottadbuser@melbourne:~/exDir$ mupip journal -rollback -backward -fetchresync=3000 -losttrans=/home/yottadbuser/exDir/Unreplic_Trans_Report_`date +%Y%m%d%H%M%S`.txt "*"
   %YDB-I-MUJNLSTAT, Initial processing started at Wed Jan  24 06:21:18 2018
   %YDB-I-MUJNLSTAT, FETCHRESYNC processing started at Wed Jan  24 06:21:18 2018
   Wed Jan  24 06:21:18 2018 : Assuming primary supports multisite functionality. Connecting using multisite communication protocol.
   Wed Jan  24 06:21:18 2018 : Waiting for a connection...
   Wed Jan  24 06:21:19 2018 : Connection established, using TCP send buffer size 87040 receive buffer size 374400
   Wed Jan  24 06:21:19 2018 : Connection information:: Local: ::ffff:10.0.2.15:3000 Remote: ::ffff:10.0.2.2:53668
   Wed Jan  24 06:21:19 2018 : Sending REPL_FETCH_RESYNC message with seqno 5 [0x5]
   Wed Jan  24 06:21:19 2018 : Source and Receiver sides have same endianness
   Wed Jan  24 06:21:19 2018 : Remote side source log file path is /home/yottadbuser/exDir/source_Melbourne.log; Source Server PID = 1057
   Wed Jan  24 06:21:19 2018 : Received REPL_NEED_INSTINFO message from primary instance [Paris]
   Wed Jan  24 06:21:19 2018 : Sending REPL_INSTINFO message
   Wed Jan  24 06:21:19 2018 : Received REPL_NEED_HISTINFO message for Seqno 5 [0x5]
   Wed Jan  24 06:21:19 2018 : Sending REPL_HISTINFO message with seqno 4 [0x4]
   Wed Jan  24 06:21:19 2018 : History sent : Start Seqno = 4 [0x4] : Stream Seqno = 0 [0x0] : Root Primary = [Santiago] : Cycle = [1] : Creator pid = 1063 : Created time = 1417556552 [0x547e3248] : History number = 2 : Prev History number = 1 : Stream # = 0 : History type = 1
   Wed Jan  24 06:21:19 2018 : Received REPL_RESYNC_SEQNO message
   Wed Jan  24 06:21:19 2018 : Received RESYNC SEQNO is 5 [0x5]
   %YDB-I-MUJNLSTAT, Backward processing started at Wed Jan  24 06:21:19 2018
   %YDB-I-RESOLVESEQNO, Resolving until sequence number 5 [0x0000000000000005]
   %YDB-I-MUJNLSTAT, Before image applying started at Wed Jan  24 06:21:19 2018
   %YDB-I-FILERENAME, File /home/yottadbuser/exDir/aA.mjl is renamed to /home/yottadbuser/exDir/aA.mjl_2018337062119
   %YDB-I-FILERENAME, File /home/yottadbuser/exDir/others.mjl is renamed to /home/yottadbuser/exDir/others.mjl_2018337062119
   %YDB-I-MUJNLSTAT, Forward processing started at Wed Jan  24 06:21:20 2018
   %YDB-I-FILECREATE, Lost transactions extract file /home/yottadbuser/exDir/Unreplic_Trans_Report_20181203062118.txt created
   %YDB-I-RLBKJNSEQ, Journal seqno of the instance after rollback is 5 [0x0000000000000005]
   %YDB-S-JNLSUCCESS, Show successful
   %YDB-S-JNLSUCCESS, Verify successful
   %YDB-S-JNLSUCCESS, Rollback successful
   %YDB-I-MUJNLSTAT, End processing at Wed Jan  24 06:21:20 2018
   yottadbuser@melbourne:~/exDir$ ./replicating_start
   Wed Jan  24 06:21:59 2018 : Initiating START of source server for secondary instance [dummy]
   yottadbuser@melbourne:~/exDir$

Perform an update in Paris and verify that there is a backlog to Santiago (the actual number may not be correct because Santiago was not recently a replicating instance to Paris, but it shows a non-zero value), but there is no backlog to Melbourne.

.. parsed-literal::
   yottadbuser@paris:~/exDir$ mumps -dir

   YDB>set ^Weather("Paris",$Piece($Horolog,",",1),$Piece($Horolog,",",2))="Heat Wave"

   YDB>zsystem "$ydb_dist/mupip replicate -source -showbacklog" 
   Tue Jan  23 17:24:09 2018 : Initiating SHOWBACKLOG operation on source server pid [1059] for secondary instance [Santiago]
   4 : backlog number of transactions written to journal pool and yet to be sent by the source server
   6 : sequence number of last transaction written to journal pool
   2 : sequence number of last transaction sent by source server
   Tue Jan  23 17:24:09 2018 : Initiating SHOWBACKLOG operation on source server pid [1057] for secondary instance [Melbourne]
   0 : backlog number of transactions written to journal pool and yet to be sent by the source server
   6 : sequence number of last transaction written to journal pool
   6 : sequence number of last transaction sent by source server

   YDB>

Now boot the Santiago machine and perform a fetchresync:

.. parsed-literal::
   yottadbuser\@santiago:~/exDir$ source ydbenv
   yottadbuser\@santiago:~/exDir$ mupip journal -rollback -backward -fetchresync=3000 -losttrans=/home/yottadbuser/exDir/Unreplic_Trans_Report_`date +%Y%m%d%H%M%S`.txt "*"
   %YDB-I-MUJNLSTAT, Initial processing started at Wed Jan  3 00:27:54 2018
   %YDB-I-MUJNLSTAT, FETCHRESYNC processing started at Wed Jan  3 00:27:54 2018
   Wed Jan  24 00:27:54 2018 : Assuming primary supports multisite functionality. Connecting using multisite communication protocol.
   Wed Jan  24 00:27:54 2018 : Waiting for a connection...
   Wed Jan  24 00:27:55 2018 : Connection established, using TCP send buffer size 87040 receive buffer size 374400
   Wed Jan  24 00:27:55 2018 : Connection information:: Local: ::ffff:10.0.2.15:3000 Remote: ::ffff:10.0.2.2:51387
   Wed Jan  24 00:27:55 2018 : Sending REPL_FETCH_RESYNC message with seqno 7 [0x7]
   Wed Jan  24 00:27:55 2018 : Source and Receiver sides have same endianness
   Wed Jan  24 00:27:55 2018 : Remote side source log file path is /home/yottadbuser/exDir/source_Santiago.log; Source Server PID = 1059
   Wed Jan  24 00:27:55 2018 : Received REPL_NEED_INSTINFO message from primary instance [Paris]
   Wed Jan  24 00:27:55 2018 : Sending REPL_INSTINFO message
   Wed Jan  24 00:27:55 2018 : Received REPL_NEED_HISTINFO message for Seqno 7 [0x7]
   Wed Jan  24 00:27:55 2018 : Sending REPL_HISTINFO message with seqno 4 [0x4]
   Wed Jan  24 00:27:55 2018 : History sent : Start Seqno = 4 [0x4] : Stream Seqno = 0 [0x0] : Root Primary = [Santiago] : Cycle = [1] : Creator pid = 1063 : Created time = 1417556552 [0x547e3248] : History number = 2 : Prev History number = 1 : Stream # = 0 : History type = 1
   Wed Jan  24 00:27:55 2018 : Received REPL_RESYNC_SEQNO message
   Wed Jan  24 00:27:55 2018 : Received RESYNC SEQNO is 6 [0x6]
   %YDB-I-MUJNLSTAT, Backward processing started at Wed Jan  24 00:27:55 2018
   %YDB-I-RESOLVESEQNO, Resolving until sequence number 6 [0x0000000000000006]
   %YDB-I-MUJNLSTAT, Before image applying started at Wed Jan  24 00:27:55 2018
   %YDB-I-FILERENAME, File /home/yottadbuser/exDir/aA.mjl is renamed to /home/yottadbuser/exDir/aA.mjl_2018337002755
   %YDB-I-FILERENAME, File /home/yottadbuser/exDir/others.mjl is renamed to /home/yottadbuser/exDir/others.mjl_2018337002755
   %YDB-I-MUJNLSTAT, Forward processing started at Wed Jan  24 00:27:55 2018
   %YDB-I-FILECREATE, Lost transactions extract file /home/yottadbuser/exDir/Unreplic_Trans_Report_20181203002754.txt created
   %YDB-I-RLBKJNSEQ, Journal seqno of the instance after rollback is 6 [0x0000000000000006]
   %YDB-S-JNLSUCCESS, Show successful
   %YDB-S-JNLSUCCESS, Verify successful
   %YDB-S-JNLSUCCESS, Rollback successful
   %YDB-I-MUJNLSTAT, End processing at Wed Jan  24 00:27:56 2018
   yottadbuser@santiago:~/exDir$ cat /home/yottadbuser/exDir/Unreplic_Trans_Report_20181203002754.txt 
   GDSJEX07 ROLLBACK PRIMARY Santiago
   05\63523,86342\7\1068\0\6\0\0\0\0\^Weather("Santiago",63523,86342)="Cloudy"
   02\63524,251\8\1068\0
   yottadbuser@santiago:~/exDir$

Now, notice that the Unreplicated Transaction File has meaningful content – the update that was made when Santiago was an originating instance, but which did not get replicated. This file will now need to be processed by the new originating instance in Paris.

Santiago can now start as a replicating instance:

.. parsed-literal::
   yottadbuser@santiago:~/exDir$ ./replicating_start
   Wed Jan  24 00:31:44 2018 : Initiating START of source server for secondary instance [dummy]
   yottadbuser@santiago:~/exDir$

Notice that Paris now reports no backlog:

.. parsed-literal::
   YDB>zsystem "$ydb_dist/mupip replicate -source -showbacklog"
   Tue Jan  23 17:32:01 2018 : Initiating SHOWBACKLOG operation on source server pid [1059] for secondary instance [Santiago]
   0 : backlog number of transactions written to journal pool and yet to be sent by the source server
   6 : sequence number of last transaction written to journal pool
   6 : sequence number of last transaction sent by source server
   Tue Jan  23 17:32:01 2018 : Initiating SHOWBACKLOG operation on source server pid [1057] for secondary instance [Melbourne]
   0 : backlog number of transactions written to journal pool and yet to be sent by the source server
   6 : sequence number of last transaction written to journal pool
   6 : sequence number of last transaction sent by source server

   YDB>

If your application uses the $ZQGBLMOD() function to process unreplicated transactions, read the `Database Replication chapter in the Administration and Operations Guide <https://docs.yottadb.com/AdminOpsGuide/dbrepl.html#processing-lost-transactions-file>`_ for information about the ``mupip replicate source -losttncomplete command`` to be executed after processing unreplicated transactions from all originating instances.

Shut down whichever were the replicating secondary instances - Santiago and Melbourne in the example above – and use the originating primary instance (Paris) for the backup exercises.

------
Backup
------

Backup **when an application is not running** is straightforward – just copy the database files. (However, remember that the copy will have the same journal file name in the database file header and the system now potentially has two database files pointing to the same journal file. Before using that file on the same computer system as the original database file, disable journaling and re-enable it if appropriate (do not simply switch journal files).)

Backup **when an application is operating normally**, without impacting the application (except of course for the additional IO load of the backup activity) is easy with YottaDB, and can be accomplished in two ways, one non-YottaDB and other YottaDB:

- The non-YottaDB way is to use a disk mirror (e.g., RAID or SAN). Issue a mupip freeze to momentarily freeze updates and flush updates to disk, break the mirror; then release the freeze. After backing up the mirror, rebuild it, and let it “catch up.” This is not discussed further here.

- The YottaDB way: a transaction-consistent backup of an entire multi-region database can be accomplished with a single YottaDB command: mupip backup. There are numerous options to satisfy virtually every type of backup need.

**Exercise - Backup**

Work with whichever instance was your last originating instance (although it does not really matter, since you can always bring the others up as replicating instances no matter which was the last originating instance because you will always start with a mupip journal rollback fetchresync step; in this case the example shows Paris).

Create a directory where you can put your backups: mkdir backup

In the exDir directory of that instance, create a program XYZ in file XYZ.m that creates updates, e.g.:

.. parsed-literal::
   XYZ    Set (^a,^b)=0
          For  Do
          . Hang 0.1
          . Tstart ()
          .   Set r=$Random(2147483646)
          .   Set ^a($horolog)=^a-r
          .   Set ^b($horolog)=^b+r
          . Tcommit
          Quit

Note that the global variables ^a and ^b go into different database files, but the use of transaction processing provides ACID properties across regions.

Since the database is a replicated environment, if no Source Servers are running, start at least one. (A replicated environment needs at least one source server to be running before updates are permitted. You can certainly start the second Source Server now – or you can start it up later for the **Replication Briefly Revisited** exercise below.)

Start the program as a background process from the shell: ``mumps -run XYZ </dev/null 1>/dev/null 2>&1 &``

Notice that the journal files are growing, indicating that the program is running:

.. parsed-literal::
   yottadbuser\@paris:~/exDir$ ls -l \*.mjl
   -rw-rw-rw- 1 yottadbuser yottadbuser 81920 Jan  23 17:52 aA.mjl
   -rw-rw-rw- 1 yottadbuser yottadbuser 81920 Jan  23 17:52 others.mjl
   yottadbuser\@paris:~/exDir$ ls -l \*.mjl
   -rw-rw-rw- 1 yottadbuser yottadbuser 86016 Jan  23 17:52 aA.mjl
   -rw-rw-rw- 1 yottadbuser yottadbuser 90112 Jan  23 17:52 others.mjl
   yottadbuser\@paris:~/exDir$ ls -l \*.mjl
   -rw-rw-rw- 1 yottadbuser yottadbuser 94208 Jan  23 17:53 aA.mjl
   -rw-rw-rw- 1 yottadbuser yottadbuser 98304 Jan  23 17:53 others.mjl
   yottadbuser\@paris:~/exDir$ 

Take a backup of the entire database (a "comprehensive backup"):

.. parsed-literal::
   yottadbuser@paris:~/exDir$ mupip backup -nojournal "*" backup/
   %YDB-I-FILERENAME, File /home/yottadbuser/exDir/aA.mjl is renamed to /home/yottadbuser/exDir/aA.mjl_2018336175401
   %YDB-I-JNLCREATE, Journal file /home/yottadbuser/exDir/aA.mjl created for region A with BEFORE_IMAGES
   %YDB-I-FILERENAME, File /home/yottadbuser/exDir/others.mjl is renamed to /home/yottadbuser/exDir/others.mjl_2018336175401
   %YDB-I-JNLCREATE, Journal file /home/yottadbuser/exDir/others.mjl created for region DEFAULT with BEFORE_IMAGES
   %YDB-I-JNLSTATE, Journaling state for database file backup//aA.dat is now DISABLED
   %YDB-I-JNLSTATE, Journaling state for database file backup//others.dat is now DISABLED
   DB file /home/yottadbuser/exDir/aA.dat backed up in file backup//aA.dat
   Transactions up to 0x00000000000001E7 are backed up.
   DB file /home/yottadbuser/exDir/others.dat backed up in file backup//others.dat
   Transactions up to 0x00000000000001ED are backed up.

   BACKUP COMPLETED.

   yottadbuser@paris:~/exDir$

Take a backup of the part of the database that has changed (a "bytestream" backup). Note:

- The use of the ``-since=database`` qualifier to only backup those database blocks that have changed since the last backup of the entire database).

- The fact that the backup files for others.dat have the name yottadb*.bck – YottaDB does not care what you name the files, but maps the targets in the alphabetic order of region name.

.. parsed-literal::
   yottadbuser@paris:~/exDir$ mupip backup -bytestream -since=database "*" backup/aA`date +%Y%m%d%H%M%S`.bck,backup/yottadb`date +%Y%m%d%H%M%S`.bck
   %YDB-I-FILERENAME, File /home/yottadbuser/exDir/aA.mjl is renamed to /home/yottadbuser/exDir/aA.mjl_2018336175445
   %YDB-I-JNLCREATE, Journal file /home/yottadbuser/exDir/aA.mjl created for region A with BEFORE_IMAGES
   %YDB-I-FILERENAME, File /home/yottadbuser/exDir/others.mjl is renamed to /home/yottadbuser/exDir/others.mjl_2018336175445
   %YDB-I-JNLCREATE, Journal file /home/yottadbuser/exDir/others.mjl created for region DEFAULT with BEFORE_IMAGES
   MUPIP backup of database file /home/yottadbuser/exDir/aA.dat to backup/aA20181202175445.bck
   DB file /home/yottadbuser/exDir/aA.dat incrementally backed up in file backup/aA20181202175445.bck
   13 blocks saved.
   Transactions from 0x00000000000001E7 to 0x00000000000002B7 are backed up.
   MUPIP backup of database file /home/yottadbuser/exDir/others.dat to backup/yottadb20181202175445.bck
   DB file /home/yottadbuser/exDir/others.dat incrementally backed up in file backup/yottadb20181202175445.bck
   13 blocks saved.
   Transactions from 0x00000000000001ED to 0x00000000000002BD are backed up.

   BACKUP COMPLETED.

   yottadbuser@paris:~/exDir$ ls -l backup/
   total 2704
   -rw-rw-rw- 1 yottadbuser yottadbuser   324608 Jan  23 17:54 aA20181202175445.bck
   -rw-rw-rw- 1 yottadbuser yottadbuser 20783616 Jan  23 17:54 aA.dat
   -rw-rw-rw- 1 yottadbuser yottadbuser   324608 Jan  23 17:54 yottadb20181202175445.bck
   -rw-rw-rw- 1 yottadbuser yottadbuser 20783616 Jan  23 17:54 others.dat
   yottadbuser@paris:~/exDir$

Take further bytestream backups of the part of the database that has changed – as many as desired (note the use of the ``-since=bytestream`` qualifier to backup only those blocks that have changed since the last bytestream backup):

.. parsed-literal::
   yottadbuser@paris:~/exDir$ mupip backup -bytestream -since=bytestream "*" backup/aA`date +%Y%m%d%H%M%S`.bck,backup/yottadb`date +%Y%m%d%H%M%S`.bck
   %YDB-I-FILERENAME, File /home/yottadbuser/exDir/aA.mjl is renamed to /home/yottadbuser/exDir/aA.mjl_2018336175610
   %YDB-I-JNLCREATE, Journal file /home/yottadbuser/exDir/aA.mjl created for region A with BEFORE_IMAGES
   %YDB-I-FILERENAME, File /home/yottadbuser/exDir/others.mjl is renamed to /home/yottadbuser/exDir/others.mjl_2018336175610
   %YDB-I-JNLCREATE, Journal file /home/yottadbuser/exDir/others.mjl created for region DEFAULT with BEFORE_IMAGES
   MUPIP backup of database file /home/yottadbuser/exDir/aA.dat to backup/aA20181202175610.bck
   DB file /home/yottadbuser/exDir/aA.dat incrementally backed up in file backup/aA20181202175610.bck
   13 blocks saved.
   Transactions from 0x00000000000002B7 to 0x0000000000000439 are backed up.
   MUPIP backup of database file /home/yottadbuser/exDir/others.dat to backup/yottadb20181202175610.bck
   DB file /home/yottadbuser/exDir/others.dat incrementally backed up in file backup/yottadb20181202175610.bck
   13 blocks saved.
   Transactions from 0x00000000000002BD to 0x000000000000043F are backed up.

   BACKUP COMPLETED.

   yottadbuser@paris:~/exDir$

Take as many additional bytestream backups as you want, in each case specifying that the updates since the preceding bytestream backup should be backed up.

.. parsed-literal::
   yottadbuser@paris:~/exDir$ mupip backup -bytestream -since=bytestream "*" backup/aA`date +%Y%m%d%H%M%S`.bck,backup/yottadb`date +%Y%m%d%H%M%S`.bck
   %YDB-I-FILERENAME, File /home/yottadbuser/exDir/aA.mjl is renamed to /home/yottadbuser/exDir/aA.mjl_2018336175650
   %YDB-I-JNLCREATE, Journal file /home/yottadbuser/exDir/aA.mjl created for region A with BEFORE_IMAGES
   %YDB-I-FILERENAME, File /home/yottadbuser/exDir/others.mjl is renamed to /home/yottadbuser/exDir/others.mjl_2018336175650
   %YDB-I-JNLCREATE, Journal file /home/yottadbuser/exDir/others.mjl created for region DEFAULT with BEFORE_IMAGES
   MUPIP backup of database file /home/yottadbuser/exDir/aA.dat to backup/aA20181202175650.bck
   DB file /home/yottadbuser/exDir/aA.dat incrementally backed up in file backup/aA20181202175650.bck
   15 blocks saved.
   Transactions from 0x0000000000000439 to 0x00000000000004F3 are backed up.
   MUPIP backup of database file /home/yottadbuser/exDir/others.dat to backup/yottadb20181202175650.bck
   DB file /home/yottadbuser/exDir/others.dat incrementally backed up in file backup/yottadb20181202175650.bck
   15 blocks saved.
   Transactions from 0x000000000000043F to 0x00000000000004F9 are backed up.

   BACKUP COMPLETED.

   yottadbuser@paris:~/exDir$

When you are satisfied, terminate the mumps process updating the database:

.. parsed-literal::
   yottadbuser@paris:~/exDir$ ps -ef | grep mumps | grep -v grep
   yottadbuser   1080   988  0 17:52 pts/0    00:00:02 /usr/local/lib/yottadb/r1.22_x86_64/mumps -run XYZ
   yottadbuser@paris:~/exDir$ mupip stop 1080
   STOP issued to process 1080
   yottadbuser@paris:~/exDir$

Take a final backup, and note the values of the final nodes ^a and ^b (and verify that they still sum to zero). After the final restore, we will verify that the values restored from the backup is the same as these values.

.. parsed-literal::
   yottadbuser@paris:~/exDir$ mupip backup -bytestream -since=bytestream "*" backup/aA`date +%Y%m%d%H%M%S`.bck,backup/yottadb`date +%Y%m%d%H%M%S`.bck
   %YDB-I-FILERENAME, File /home/yottadbuser/exDir/aA.mjl is renamed to /home/yottadbuser/exDir/aA.mjl_2018336175946
   %YDB-I-JNLCREATE, Journal file /home/yottadbuser/exDir/aA.mjl created for region A with BEFORE_IMAGES
   %YDB-I-FILERENAME, File /home/yottadbuser/exDir/others.mjl is renamed to /home/yottadbuser/exDir/others.mjl_2018336175946
   %YDB-I-JNLCREATE, Journal file /home/yottadbuser/exDir/others.mjl created for region DEFAULT with BEFORE_IMAGES
   MUPIP backup of database file /home/yottadbuser/exDir/aA.dat to backup/aA20181202175946.bck
   DB file /home/yottadbuser/exDir/aA.dat incrementally backed up in file backup/aA20181202175946.bck
   13 blocks saved.
   Transactions from 0x00000000000004F3 to 0x0000000000000745 are backed up.
   MUPIP backup of database file /home/yottadbuser/exDir/others.dat to backup/yottadb20181202175946.bck
   DB file /home/yottadbuser/exDir/others.dat incrementally backed up in file backup/yottadb20181202175946.bck
   13 blocks saved.
   Transactions from 0x00000000000004F9 to 0x000000000000074B are backed up


   BACKUP COMPLETED.

   yottadbuser@paris:~/exDir$ mumps -run %XCMD 'set x=$order(^a(""),-1) write x," ",^a(x)," ",^b(x)," ",^a(x)+^b(x),!'
   63523,64737 -778521900 778521900 0
   yottadbuser@paris:~/exDir$

Now it's time to work on restoring the backup. The first backup (the database backup) provides a complete, ready-to-run database. The subsequent bytestream backups can be applied to the database backup using the mupip restore command.

Create an environment to restore the backup. It may be easiest if you simply use the backup directory that you created, and working in a new shell session, copy exDir/env and exDir/yottadb.gld to the backup directory and edit them to reflect their new locations. (Note that since global directories are used only to create databases, there is no reason to change the default journal file names in the regions of the global directory file in backup. Nevertheless, it is good practice to keep global directories correct, since that global directory may be used at a future time to create new database files.)

.. parsed-literal::
   yottadbuser\@paris:~/exDir$ cp ydbenv yottadb.gld backup/
   yottadbuser\@paris:~/exDir$ cd backup; vim backup/ydbenv # edit to point to backup directory
   yottadbuser\@paris:~/exDir/backup$ cat ydbenv
   export ydb_dist=/usr/local/lib/yottadb/r1.22_x86_64/
   export ydb_gbldir=$HOME/exDir/backup/yottadb.gld
   export ydb_log=/tmp/yottadb/r1.22_x86_64
   export ydb_tmp=$ydb_log
   export ydb_principal_editing=EDITING
   export ydb_repl_instance=$HOME/exDir/backup/yottadb.repl
   export ydb_repl_instname=Paris
   export ydb_routines="$HOME/exDir/backup* $ydb_dist/libyottadbutil.so"
   mkdir -p $ydb_tmp
   alias mumps=$ydb_dist/mumps
   alias mupip=$ydb_dist/mupip
   yottadbuser\@paris:~/exDir/backup$ source ydbenv
   yottadbuser\@paris:~/exDir/backup$ mumps -run GDE
   %GDE-I-LOADGD, Loading Global Directory file /home/yottadbuser/exDir/backup/yottadb.gld
   %GDE-I-VERIFY, Verification OK

   GDE> show -segment

                     \*\*\* SEGMENTS \*\*\*
    Segment           File( def ext: .dat)   Acc Typ Block       Alloc Exten  Options
    ----------------------------------------------------------------------------------
    A                $HOME/exDir/aA.dat       BG DYN 4096        5000  10000  GLOB=1000
                                                                              LOCK=40
                                                                              RES=0
                                                                              ENCR=OFF
                                                                              MSLT=1024
    DEFAULT          $HOME/exDir/others.dat   BG DYN 4096        5000  10000  GLOB=1000
                                                                              LOCK=40
                                                                              RES=0
                                                                              ENCR=OFF
                                                                              MSLT=1024

   GDE> change -segment A -file=$HOME/exDir/backup/aA.dat
   GDE> change -segment DEFAULT -file=$HOME/exDir/backup/others.dat
   GDE> show -segment

                         \*\*\* SEGMENTS \*\*\*
    Segment              File( def ext:.dat)       Acc Typ Block       Alloc Exten  Options
    ---------------------------------------------------------------------------------------
    A                    $HOME/exDir/backup/aA.dat BG  DYN  4096       5000  10000  GLOB=1000
                                                                                    LOCK=40
                                                                                    RES=0
                                                                                    ENCR=OFF
                                                                                    MSLT=1024
    DEFAULT              $HOME/exDir/backup/others.dat
                                                   BG   DYN 4096       5000  10000  GLOB=1000
                                                                                    LOCK=40
                                                                                    RES=0
                                                                                    ENCR=OFF
                                                                                    MSLT=1024


   GDE> show -region

                           \*\*\* REGIONS \*\*\*
    Region       Dynamic Segment    Def Coll Rec Size  Key Size  Null Subs   Std Null Coll  Jnl  Inst Freeze on Error Qdb Rundown
   ------------------------------------------------------------------------------------------------------------------------------
    A                 A                0      4080      255       NEVER            Y         Y     DISABLED            DISABLED
    DEFAULT          DEFAULT           0      4080      255       NEVER            Y         Y     DISABLED            DISABLED

                         \*\*\*JOURNALING INFORMATION\*\*\*
    Region       Jnl file (def ext:.mjl)   Before  Buff  Alloc  Exten  Autoswitch
    ------------------------------------------------------------------------------
    A             $HOME/exDir/aA.mjl         Y    2308   2048   2048    8386560
    DEFAULT       $HOME/exDir/others.mjl     Y    2308   2048   2048    8386560

   GDE> change -region A -journal=file=$HOME/exDir/backup/aA.mjl
   GDE> change -region DEFAULT -journal=file=$HOME/exDir/backup/others.mjl
   GDE> show -region
   
                         \*\*\*  REGIONS \*\*\*
    Region       Dynamic Segment   Def Coll  Rec Size  Key Size  Null Subs  Std Null Coll  Jnl  Inst Freeze on Error  Qdb Rundown
    ------------------------------------------------------------------------------------------------------------------------------
    A                 A               0        4080     255       NEVER           Y         Y         DISABLED         DISABLED
    DEFAULT        DEFAULT            0        4080     255       NEVER           Y         Y         DISABLED         DISABLED

                         \*\*\* JOURNALING INFORMATION \*\*\*
    Region        Jnl file (def ext:.mjl)        Before  Buff  Alloc   Exten  Autoswitch
    ------------------------------------------------------------------------------------
    A             $HOME/exDir/backup/aA.mjl        Y     2308   2048   2048    8386560
    DEFAULT       $HOME/exDir/backup/others.mjl    Y     2308   2048   2048    8386560

   GDE> exit
   %GDE-I-VERIFY, Verification OK

   %GDE-I-GDUPDATE, Updating Global Directory file /home/yottadbuser/exDir/backup/yottadb.gld
   yottadbuser@paris:~/exDir/backup$

Confirm that the values of the last global nodes of ^a and ^b are equal and opposite (demonstrating the value of transaction processing):

.. parsed-literal::
   yottadbuser@paris:~$ mumps -run %XCMD 'set x=$order(^a(""),-1) write x," ",^a(x)," ",^b(x)," ",^a(x)+^b(x),!'
   63523,64441 -910877047 910877047 0
   yottadbuser@paris:~/exDir/backup$

Notice the file names of the bytestream backups that we will restore one by one.
 
.. parsed-literal::
   yottadbuser\@paris:~/exDir/backup$ ls -l backup/\*.bck
   -rw-rw-rw- 1 yottadbuser yottadbuser 324608 Jan  23 17:54 aA20181202175445.bck
   -rw-rw-rw- 1 yottadbuser yottadbuser 324608 Jan  23 17:56 aA20181202175610.bck
   -rw-rw-rw- 1 yottadbuser yottadbuser 332800 Jan  23 17:56 aA20181202175650.bck
   -rw-rw-rw- 1 yottadbuser yottadbuser 320512 Jan  23 17:59 aA20181202175946.bck
   -rw-rw-rw- 1 yottadbuser yottadbuser 324608 Jan  23 17:54 yottadb20181202175445.bck
   -rw-rw-rw- 1 yottadbuser yottadbuser 324608 Jan  23 17:56 yottadb20181202175610.bck
   -rw-rw-rw- 1 yottadbuser yottadbuser 332800 Jan  23 17:56 yottadb20181202175650.bck
   -rw-rw-rw- 1 yottadbuser yottadbuser 320512 Jan  23 17:59 yottadb20181202175946.bck
   yottadbuser\@paris:~/exDir/backup$

Restore the first bytestream backup (with the since=database qualifier), and check the values of ^a and ^b:

.. parsed-literal::
   yottadbuser@paris:~/exDir/backup$ mupip restore aA.dat aA20181202175445.bck 

   RESTORE COMPLETED
   14 blocks restored
   yottadbuser@paris:~/exDir/backup$ mupip restore others.dat yottadb20181202175445.bck 

   RESTORE COMPLETED
   14 blocks restored
   yottadbuser@paris:~/exDir/backup$ 

Confirm new values for ^a and ^b. Notice that the time stamp is later than that of the database before the first bytestream backup (with a since=database qualifier) was restored and the values of the nodes in ^a and ^b still add up to zero.

.. parsed-literal::
   yottadbuser@paris:~/exDir/backup$ mumps -run %XCMD 'set x=$order(^a(""),-1) write x," ",^a(x)," ",^b(x)," ",^a(x)+^b(x),!'
   63523,64485 -1612950378 1612950378 0
   yottadbuser@paris:~/exDir/backup$

Restore the second bytestream backup (the first with a since=bytestream qualifier), and check the values. Repeat for any additional bytestream backups. Notice that the final values are the same as the final values in exDir.

.. parsed-literal::
   yottadbuser@paris:~/exDir/backup$ mupip restore aA.dat aA20181202175610.bck 

   RESTORE COMPLETED
   14 blocks restored
   yottadbuser@paris:~/exDir/backup$ mupip restore others.dat yottadb20181202175610.bck 

   RESTORE COMPLETED
   14 blocks restored
   yottadbuser@paris:~/exDir/backup$ mumps -run %XCMD 'set x=$order(^a(""),-1) write x," ",^a(x)," ",^b(x)," ",^a(x)+^b(x),!'
   63523,64570 -1558529283 1558529283 0
   yottadbuser@paris:~/exDir/backup$ mupip restore aA.dat aA20181202175650.bck 

   RESTORE COMPLETED
   16 blocks restored
   yottadbuser@paris:~/exDir/backup$ mupip restore others.dat yottadb20181202175650.bck 

   RESTORE COMPLETED
   16 blocks restored
   yottadbuser@paris:~/exDir/backup$ mumps -run %XCMD 'set x=$order(^a(""),-1) write x," ",^a(x)," ",^b(x)," ",^a(x)+^b(x),!'
   63523,64610 -2074012206 2074012206 0
   yottadbuser@paris:~/exDir/backup$ mupip restore aA.dat aA20181202175946.bck 

   RESTORE COMPLETED
   13 blocks restored
   yottadbuser@paris:~/exDir/backup$ mupip restore others.dat yottadb20181202175946.bck 

   RESTORE COMPLETED
   13 blocks restored
   yottadbuser@paris:~$ mumps -run %XCMD 'set x=$order(^a(""),-1) write x," ",^a(x)," ",^b(x)," ",^a(x)+^b(x),!'
   63523,64737 -778521900 778521900 0
   yottadbuser@paris:~$

Note that ending values of ^a and ^b in the environment with the restored backups is exactly the same as in the original database. Also, the values of ^a and ^b always add to zero even though the global variables reside in different databases, demonstrating transaction processing.

**Replication Briefly Revisited**

We have been ignoring Santiago and Melbourne during the backup exercise, and we have created quite a backlog. Here is the backlog report in the exDir environment in Paris. (If you are using the same shell session to restore backups, you will need to source exDir/ydbenv to get the environment variables for the original environment, and restart Source Servers if they are not running.)

.. parsed-literal::
   yottadbuser@paris:~/exDir$ mupip replicate -source -showbacklog
   Fri Jan  26 04:50:18 2018 : Initiating SHOWBACKLOG operation on source server pid [1068] for secondary instance [Santiago]
   1860 : backlog number of transactions written to journal pool and yet to be sent by the source server
   1866 : sequence number of last transaction written to journal pool
   6 : sequence number of last transaction sent by source server
   Fri Jan  26 04:50:18 2018 : Initiating SHOWBACKLOG operation on source server pid [1064] for secondary instance [Melbourne]
   1860 : backlog number of transactions written to journal pool and yet to be sent by the source server
   1866 : sequence number of last transaction written to journal pool
   6 : sequence number of last transaction sent by source server
   yottadbuser@paris:~/exDir$

Boot Melbourne and check the state of the database with respect to ^a and ^b – notice the error because no node of ^a is defined. 

.. parsed-literal::
   yottadbuser@melbourne:~/exDir$ source ydbenv
   yottadbuser@melbourne:~/exDir$ mumps -run %XCMD 'set x=$order(^a(""),-1) write x," ",^a(x)," ",^b(x)," ",^a(x)+^b(x),!'
   %YDB-E-NULSUBSC, Null subscripts are not allowed for region: A
   %YDB-I-GVIS,            Global variable: ^a("")
   yottadbuser@melbourne:~/exDir$

Then execute the exDir/replicating_start script and notice that Melbourne catches up.

.. parsed-literal::
   yottadbuser@melbourne:~/exDir$ ./replicating_start 
   Fri Jan 26 07:12:07 2018 : Initiating START of source server for secondary instance [dummy]
   yottadbuser@melbourne:~/exDir$ mumps -run %XCMD 'set x=$order(^a(""),-1) write x," ",^a(x)," ",^b(x)," ",^a(x)+^b(x),!'
   63523,64737 -778521900 778521900 0
   yottadbuser@melbourne:~/exDir$

Notice that Paris now shows a zero backlog for Melbourne, but still has a large backlog for Santiago.

.. parsed-literal::
   yottadbuser@paris:~/exDir$ mupip replicate -source -showbacklog
   Fri Jan  26 05:05:04 2018 : Initiating SHOWBACKLOG operation on source server pid [1068] for secondary instance [Santiago]
   1860 : backlog number of transactions written to journal pool and yet to be sent by the source server
   1866 : sequence number of last transaction written to journal pool
   6 : sequence number of last transaction sent by source server
   Fri Jan  26 05:05:04 2018 : Initiating SHOWBACKLOG operation on source server pid [1064] for secondary instance [Melbourne]
   0 : backlog number of transactions written to journal pool and yet to be sent by the source server
   1866 : sequence number of last transaction written to journal pool
   1866 : sequence number of last transaction sent by source server
   yottadbuser@paris:~/exDir$

If you like, you can similarly boot Santiago and show that it also clears the backlog automatically. Shut down the instances when you are done.

-----------------------
Unicode (ISO/IEC-10646)
-----------------------

YottaDB supports international character sets using Unicode. A mumps process can operate in one of two modes: M mode and UTF-8 mode, which is specified by the environment variable ydb_chset at process startup and which is immutable for the life of the process.

In M mode, the process interprets strings as single byte characters with characters from $Char(0) through $Char(127) encoded as ASCII (M mode). A YottaDB process in M mode places no interpretation on characters $Char(128) through $Char(255) – the application is free to use any interpretation it chooses, and ISO-8859 variants are commonly used. There is no distinction between a string of bytes and a string of characters and all bytes are valid characters – the concept of an illegal or a non-canonical character is a non-sequitur in M mode.

In UTF-8 mode, the process by default interprets strings as multi-byte characters encoded using UTF-8 (UTF-8 mode). Some sequences of bytes are not defined by the standard, and are considered illegal characters – for example $char(55296), U+D800, is an illegal character. The process can also interpret a string as a sequence of bytes, and the same string can have different properties (such as its length) when it is considered a sequence of bytes than as a sequence of characters.

Except when triggers are used, this interpretation is at the level of the process: the database treats strings as binary data and does not care how they are encoded. So, it is possible for the same database to be concurrently accessed by mumps processes operating in both M mode and UTF-8 mode. Mupip and other processes are not concerned with this distinction. If the database has triggers, since triggers are compiled code, all processes that use a database must have the same mode.

The mode of a process is controlled by the environment variable ydb_chset. If it is not set, or has a value other than "UTF-8", the process operates in M mode. Within a process the ISV $ZCHset can be used to test the mode.

For a process to operate in UTF-8 mode requires:

- ICU with a level of 3.6 or higher packaged as libicu. YottaDB may also require the environment variable ydb_icu_version to be defined (the ydb_env_set script attempts to detect and set it, if it is not set, but is not guaranteed to succeed).

- The environment variable LC_CTYPE (or the environment variable LC_ALL) to specify a UTF-8 locale available on the system.

The locale command provides a list of available locales; the locale-gen command generates additional locales. In the example below, the locale command is used to list available locales, and the locale-gen command is used to generate the fr_BE.utf8 locale for French as used in Belgium:

.. parsed-literal::
   yottadbuser@yottadbworkshop:~$ locale -a
   C
   C.UTF-8
   en_AG
   en_AG.utf8
   en_AU.utf8
   en_BW.utf8
   en_CA.utf8
   en_DK.utf8
   en_GB.utf8
   en_HK.utf8
   en_IE.utf8
   en_IN
   en_IN.utf8
   en_NG
   en_NG.utf8
   en_NZ.utf8
   en_PH.utf8
   en_SG.utf8
   en_US.utf8
   en_ZA.utf8
   en_ZM
   en_ZM.utf8
   en_ZW.utf8
   POSIX
   yottadbuser@yottadbworkshop:~$ sudo locale-gen fr_BE.utf8
   Generating locales...
     fr_BE.UTF-8... done
   Generation complete.
   yottadbuser@yottadbworkshop:~$ locale -a | grep fr_BE.utf8
   fr_BE.utf8
   yottadbuser@yottadbworkshop:~$

For interactive usage, the terminal emulator must be configured to display characters in UTF-8 mode; otherwise you will become very confused.

This exercise uses two sessions, one with a mumps process in UTF-8 mode and the other using a mumps process in M mode. Create a utf8demo directory and two environment files within it, ydbenv_m and ydbenv_utf8 as follows:

.. parsed-literal::
   yottadbuser@yottadbworkshop:~/utf8demo$ cd utf8demo; vim ydbenv_m ; cat ydbenv_m
   export ydb_dist=/usr/local/lib/yottadb/r122
   export ydb_gbldir=$HOME/utf8demo/yottadb.gld
   export ydb_log=/tmp/yottadb/r122
   export ydb_tmp=$ydb_log
   export ydb_principal_editing=EDITING
   export ydb_repl_instance=$HOME/utf8demo/yottadb.repl
   export ydb_repl_instname=dummy
   export ydb_routines="$HOME/utf8demo* $ydb_dist/libyottadbutil.so"
   mkdir -p $ydb_tmp
   alias mumps=$ydb_dist/mumps
   alias mupip=$ydb_dist/mupip
   yottadbuser@yottadbworkshop:~/utf8demo$ cp ydbenv_m ydbenv_utf8 ; vim ydbenv_utf8 ; cat ydbenv_utf8 
   export ydb_dist=/usr/local/lib/yottadb/r122/utf8
   export ydb_gbldir=$HOME/utf8demo/yottadb.gld
   export ydb_log=/tmp/yottadb/r122
   export ydb_tmp=$ydb_log
   export ydb_principal_editing=EDITING
   export ydb_repl_instance=$HOME/utf8demo/yottadb.repl
   export ydb_repl_instname=dummy
   export ydb_routines="$HOME/utf8demo* $ydb_dist/libyottadbutil.so"
   export ydb_chset=UTF-8
   export LC_CTYPE=en_US.utf8 # or other UTF-8 locale
   export ydb_icu_version=5.5 # or other version that you are using
   export ydb_prompt="UTF8>"
   mkdir -p $ydb_tmp
   alias mumps=$ydb_dist/mumps
   alias mupip=$ydb_dist/mupip
   yottadbuser@yottadbworkshop:~/utf8demo$

Create a global directory and database file, as you did before. Only now, do this in the UTF-8 session to so that you can see that the operation of the GDE utility program is the same.

.. parsed-literal::
   yottadbuser\@yottadbworkshop:~/utf8demo$ source ydbenv_utf8
   yottadbuser\@yottadbworkshop:~/utf8demo$ mumps -run GDE
   %GDE-I-GDUSEDEFS, Using defaults for Global Directory 
           /home/yottadbuser/utf8demo/yottadb.gld

   GDE> @/usr/local/lib/yottadb/r122/utf8/gdedefaults
   %GDE-I-EXECOM, Executing command file /usr/local/lib/yottadb/r122/utf8/gdedefaults

   GDE> show -segment

                            \*\*\* SEGMENTS \*\*\*
     Segment            File (def ext: .dat)           Acct  Type  Block   Alloc Exten Options
     ------------------------------------------------------------------------------------------
     DEFAULT            $gtmdir/$ydb_rel/g/yottadb.dat      BG    DYN   4096    5000 10000 GLOB=1000
                                                                                      LOCK=40
                                                                                      RES=0
                                                                                      ENCR=OFF
                                                                                      MSLT=1024

   GDE> change -segment DEFAULT -file=$HOME/utf8demo/yottadb.dat
   GDE> show -segment


                           \*\*\* SEGMENTS \*\*\*
   Segment             File (def ext: .dat)           Acct   Type   Block  Alloc Exten Options
   --------------------------------------------------------------------------------------------
   DEFAULT             $HOME/utf8demo.dat              BG    DYN    4096   5000 10000 GLOB=1000
                                                                                      LOCK=40
                                                                                      RES=0
                                                                                      ENCR=OFF
                                                                                      MSLT=1024

   GDE> exit
   %GDE-I-VERIFY, Verification OK

   %GDE-I-GDCREATE, Creating Global Directory file 
           /home/yottadbuser/utf8demo/yottadb.gld
   yottadbuser\@yottadbworkshop:~/utf8demo$ mupip create
   Created file /home/yottadbuser/utf8demo/yottadb.dat
   yottadbuser\@yottadbworkshop:~/utf8demo$

We use the ydb_prompt environment variable to avoid confusion as to which mode a direct mode prompt is in.

.. parsed-literal::
   yottadbuser\@yottadbworkshop:~/utf8demo$ mumps -dir

   UTF8>write $zchset
   UTF-8
   UTF8>for i=0:1:255 set ^Ch(i)=$char(i)

   UTF8>for i=0:16:240 write ! for j=0:1:15 write ^Ch(i+j)," : "

   :  :  :  :  :  :  :  : :        : 
   : 
      : 
   :  :  : 
   :  :  :  :  :  :  :  : ▒ :  : ▒ : :  :  :  :  : 
      : ! : " : # : $ : % : & : ' : ( : ) : * : + : , : - : . : / : 
   0 : 1 : 2 : 3 : 4 : 5 : 6 : 7 : 8 : 9 : : : ; : < : = : > : ? : 
   @ : A : B : C : D : E : F : G : H : I : J : K : L : M : N : O : 
   P : Q : R : S : T : U : V : W : X : Y : Z : [ : \ : ] : ^ : _ : 
   ` : a : b : c : d : e : f : g : h : i : j : k : l : m : n : o : 
   p : q : r : s : t : u : v : w : x : y : z : { : | : } : ~ :  : 
    :  :  :  :  :  :  :  :  :  :  :  :  :  :  :  : 
    :  :  :  :  :  :  :  :  :  :  : :  :  :  :  : 
    : ¡ : ¢ : £ : ¤ : ¥ : ¦ : § : ¨ : © : ª : « : ¬ :  : ® : ¯ : 
   ° : ± : ² : ³ : ´ : µ : ¶ : · : ¸ : ¹ : º : » : ¼ : ½ : ¾ : ¿ : 
   À : Á : Â : Ã : Ä : Å : Æ : Ç : È : É : Ê : Ë : Ì : Í : Î : Ï : 
   Ð : Ñ : Ò : Ó : Ô : Õ : Ö : × : Ø : Ù : Ú : Û : Ü : Ý : Þ : ß : 
   à : á : â : ã : ä : å : æ : ç : è : é : ê : ë : ì : í : î : ï : 
   ð : ñ : ò : ó : ô : õ : ö : ÷ : ø : ù : ú : û : ü : ý : þ : ÿ :

   UTF8>halt
   yottadbuser\@yottadbworkshop:~/utf8demo$ 

Start a shell in M mode and display in M mode the characters that you just set in UTF-8 mode (remember to set your terminal emulator to a single-byte encoding such as ISO-8859-1, or use a different terminal emulator, otherwise you will get confused):

.. parsed-literal::
   yottadbuser\@yottadbworkshop:~/utf8demo$ mumps -dir

   YDB>write $zchset
   M
   YDB>for i=0:16:240 write ! for j=0:1:15 write ^Ch(i+j)," : "

   :  :  :  :  :  :  :  : :        : 
   : 
     : 
   :  :  : 
   :  :  :  :  :  :  :  : ▒ :  : ▒ : :  :  :  :  : 
    : ! : " : # : $ : % : & : ' : ( : ) : * : + : , : - : . : / : 
  0 : 1 : 2 : 3 : 4 : 5 : 6 : 7 : 8 : 9 : : : ; : < : = : > : ? : 
  @ : A : B : C : D : E : F : G : H : I : J : K : L : M : N : O : 
  P : Q : R : S : T : U : V : W : X : Y : Z : [ : \ : ] : ^ : _ : 
  ` : a : b : c : d : e : f : g : h : i : j : k : l : m : n : o : 
  p : q : r : s : t : u : v : w : x : y : z : { : | : } : ~ :  : 
  Â : Â : Â : Â : Â : Â : Â : Â : Â : Â : Â : Â : Â : Â : Â : Â : 
  Â : Â : Â : Â : Â : Â : Â : Â : Â : Â : Â : Â: Â : Â : Â : Â : 
  Â : Â¡ : Â¢ : Â£ : Â¤ : Â¥ : Â¦ : Â§ : Â¨ : Â© : Âª : Â« : Â¬ : Â : Â® : Â¯ : 
  Â° : Â± : Â² : Â³ : Â´ : Âµ : Â¶ : Â· : Â¸ : Â¹ : Âº : Â» : Â¼ : Â½ : Â¾ : Â¿ : 
  Ã : Ã : Ã : Ã : Ã : Ã : Ã : Ã : Ã : Ã : Ã : Ã : Ã : Ã : Ã : Ã : 
  Ã : Ã : Ã : Ã : Ã : Ã : Ã : Ã : Ã : Ã : Ã : Ã: Ã : Ã : Ã : Ã : 
  Ã : Ã¡ : Ã¢ : Ã£ : Ã¤ : Ã¥ : Ã¦ : Ã§ : Ã¨ : Ã© : Ãª : Ã« : Ã¬ : Ã : Ã® : Ã¯ : 
  Ã° : Ã± : Ã² : Ã³ : Ã´ : Ãµ : Ã¶ : Ã· : Ã¸ : Ã¹ : Ãº : Ã» : Ã¼ : Ã½ : Ã¾ : Ã¿ : 
  YDB>

Notice that the lengths of the strings are different – the process in UTF-8 mode reports all as being of length 1, whereas the M mode process reports some as being of length 2.

.. parsed-literal::
   UTF8>for i=0:16:240 write ! for j=0:1:15 write $length(^Ch(i+j))," : "

   1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 :
   1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 :
   1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 :
   1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 :
   1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 :
   1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 :
   1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 :
   1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 :
   1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 :
   1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 :
   1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 :
   1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 :
   1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 :
   1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 :
   1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 :
   1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 :
   UTF8>

   YDB>for i=0:16:240 write ! for j=0:1:15 write $length(^Ch(i+j))," : "

   1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 :
   1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 :
   1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 :
   1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 :
   1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 :
   1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 :
   1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 :
   1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 :
   2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 :
   2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 :
   2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 :
   2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 :
   2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 :
   2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 :
   2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 :
   2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 :
   YDB>

However, both report the same string lengths in bytes using the $Zlength() function instead of the $Length() function.

.. parsed-literal::
   UTF8>for i=0:16:240 write ! for j=0:1:15 write $zlength(^Ch(i+j))," : "

   1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 :
   1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 :
   1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 :
   1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 :
   1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 :
   1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 :
   1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 :
   1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 :
   2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 :
   2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 :
   2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 :
   2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 :
   2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 :
   2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 :
   2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 :
   2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 :
   UTF8>

   YDB>for i=0:16:240 write ! for j=0:1:15 write $zlength(^Ch(i+j))," : "

   1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 :
   1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 :
   1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 :
   1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 :
   1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 :
   1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 :
   1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 :
   1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 : 1 :
   2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 :
   2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 :
   2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 :
   2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 :
   2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 :
   2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 :
   2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 :
   2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : 2 :
   YDB>

Remember that Unicode only provides a character encoding. The ordering of strings encoded in UTF-8 may not be linguistically or culturally correct for your language. For example, Chinese names may be encoded in UTF-8 using characters in either the simplified or the traditional Chinese script, but the linguistically and culturally appropriate ordering of names (for example in the Beijing phone book) is the order in pinyin, which uses the Latin script. In order to use YottaDB to store strings in the correct order, there may be an additional requirement to provide a collation module.

--------------------
Pulling It Together
--------------------

Thus far, the Acculturation Workshop has taken you through different core concepts. Now, it is time to pull these concepts together. In order to do that, we will go through a series of installations, each more sophisticated than its predecessor, of the VistA application.

No knowledge of VistA is assumed or required for the Acculturation Workshop – VistA is simply used as a freely usable sample application to explore configuring an application on YottaDB.

**About VistA**

The `US Department of Veterans Affairs (VA) <http://va.gov/>`_ operates one of the largest integrated healthcare networks in the world. Delivering legally mandated high quality care to veterans of the US armed forces, it has repeatedly been recognized not only for the quality, but also for the cost-effectiveness of the care that it provides. VistA is a healthcare information system (HIS) developed and maintained by the VA. The software is in the public domain and freely available. Many providers support VistA on a commercial basis, and there is an `active online VistA community <http://groups.google.com/group/hardhats>`_.

For all healthcare organizations, VistA can provide a cost-effective enterprise resource planning (ERP) system. It is written in the ANSI/ISO standard programming language M (also known as MUMPS), With origins in the field of healthcare informatics, M is the de facto standard for healthcare software.

The Acculturation Workshop uses the `WorldVistA EHR® <http://worldvista.org/World_VistA_EHR>`_ flavor of VistA as a sample YottaDB application, and can be downloaded from the `WorldVistA project at Source Forge <http://sourceforge.net/projects/worldvista>`_.

**Download VistA**

To get VistA, you will need to download two archive files : routines and global variables. First, create a directory to store the archive files, then download the files into that directory.

.. parsed-literal::
   yottadbuser@yottadbworkshop:~$ sudo mkdir -p /Distrib/VistA
   [sudo] password for yottadbuser:
   yottadbuser@yottadbworkshop:~$ sudo chown yottadbuser.gtmuser /Distrib/VistA/
   yottadbuser@yottadbworkshop:~$ wget -P /Distrib/VistA/ http://tinyurl.com/WVEHRVOE10Routines

   …

   Length: 33432464 (32M) [application/octet-stream]
   Saving to: ‘/Distrib/VistA/WVEHRVOE10Routines’

   100%[===================================================>] 33,432,464   970KB/s   in 36s    

   2018-01-25 15:05:10 (895 KB/s) - ‘/Distrib/VistA/WVEHRVOE10Routines’ saved [33432464/33432464]

   yottadbuser@yottadbworkshop:~$ wget -P /Distrib/VistA/ http://tinyurl.com/WVEHRVOE10Globals

   …

   Length: 140631465 (134M) [application/x-gzip]
   Saving to: ‘/Distrib/VistA/WVEHRVOE10Globals’

   100%[========================================================================================>] 140,631,465  690KB/s   in 2m 49s 

   2018-01-25 15:10:08 (813 KB/s) - ‘/Distrib/VistA/WVEHRVOE10Globals’ saved [140631465/140631465]

   yottadbuser@yottadbworkshop:~$

**Simple Environment**

The very simplest environment is one where you put everything – source, object, global directory, database and journal files – in one directory (as you did for the environments in the exercises above). But for any real application, this would rapidly become unwieldy.

The first step up from the simplest environment is to have separate sub-directories for the source files, object files, and database; with shell scripts in the parent directory. Note that in production environments, you should consider putting the journal files elsewhere, on a file system on disks different from the database, and ideally even on a separate disk controller.

Create a VistA directory. Use a g subdirectory for global variables, an o subdirectory for object files and an r subdirectory for routines. Create a file to source to set up environment variables. Create a global directory and a database file into which to load the global variables. We will not set ydb_principal_editing since VistA has a mode where it manages its own screen cursors (Screenman) which requires VistA application code to receive terminal escape sequences.

.. parsed-literal::
   yottadbuser\@yottadbworkshop:~$ mkdir -p VistA/{g,o,r}
   yottadbuser\@yottadbworkshop:~$ vim VistA/ydbenv ; cat VistA/ydbenv
   export ydb_dist=/usr/local/lib/yottadb/r122/
   export ydb_gbldir=$HOME/VistA/g/yottadb.gld
   export ydb_log=/tmp/yottadb/r122
   export ydb_tmp=$ydb_log
   export ydb_repl_instance=$HOME/VistA/g/yottadb.repl
   export ydb_repl_instname=dummy
   export ydb_routines="$HOME/VistA/o*($HOME/VistA/r) $ydb_dist/libyottadbutil.so"
   mkdir -p $ydb_tmp
   alias mumps=$ydb_dist/mumps
   alias mupip=$ydb_dist/mupip
   yottadbuser\@yottadbworkshop:~$ source VistA/ydbenv
   yottadbuser\@yottadbworkshop:~$ mumps -run GDE
   %GDE-I-GDUSEDEFS, Using defaults for Global Directory 
           /home/yottadbuser/VistA/g/yottadb.gld

   GDE> @/usr/local/lib/yottadb/r122/gdedefaults
   %GDE-I-EXECOM, Executing command file /usr/local/lib/yottadb/r122/gdedefaults

   GDE> change -segment DEFAULT -file=$HOME/VistA/g/yottadb.dat
   GDE> show -segment
   

                    *** SEGMENTS ***
   Segment          File (ext def: .dat)  Acc  Type  Block   Alloc  Exten  Options
   -------------------------------------------------------------------------------
   DEFAULT      $HOME/VistA/g/yottadb.dat BG   DYN   4096    5000   10000  GLOB=1000
                                                                           LOCK=40
                                                                           RES=0
                                                                           ENCR=OFF
                                                                           MSLT=1024

   GDE> change -region DEFAULT -journal=(before,file="$HOME/VistA/g/yottadb.mjl")
   GDE> show -region

                    *** REGIONS ***
   Region    Dynamic Segment    Def Coll  Rec Size  Key Size  Null Subs  Std Null Coll  Jnl  Inst Freeze on Error  Qdb Rundown
   ---------------------------------------------------------------------------------------------------------------------------
   DEFAULT     DEFAULT            0         4080      255      NEVER           Y         Y      DISABLED            DISABLED

                    *** JOURNALING INFORMATION ***
   Region    Jnl File (def ext: .mjl)    Before  Buff  Alloc  Exten  Autoswitch
   -----------------------------------------------------------------------------
   DEFAULT   $HOME/VistA/g/yottadb.mjl      Y     2308  2048   2048    8386560

   GDE> exit
   %GDE-I-VERIFY, Verification OK

   %GDE-I-GDCREATE, Creating Global Directory file 
           /home/yottadbuser/VistA/g/yottadb.gld
   yottadbuser\@yottadbworkshop:~$ mupip create
   Created file /home/yottadbuser/VistA/g/yottadb.dat
   yottadbuser\@yottadbworkshop:~$


Load the global variables into the database.

.. parsed-literal::
   yottadbuser@yottadbworkshop:~$ gzip -d </Distrib/VistA/WVEHRVOE10Globals | mupip load -stdin
   GT.M MUPIP EXTRACT
   26-JAN-2018  14:38:37 ZWR
   Beginning LOAD at record number: 3

   LOAD TOTAL              Key Cnt: 20337342  Max Subsc Len: 223  Max Data Len: 834
   Last LOAD record number: 20337344

   yottadbuser@yottadbworkshop:~$ ls -lhR VistA/
   VistA/:
   total 4.0K
   drwxrwxr-x 2 yottadbuser gtmuser  34 Jan 26 15:33 g
   -rw-rw-r-- 1 yottadbuser gtmuser 384 Jan 26 15:25 ydbenv
   drwxrwxr-x 2 yottadbuser gtmuser   6 Jan 26 15:17 o
   drwxrwxr-x 2 yottadbuser gtmuser   6 Jan 26 15:17 r

   VistA/g:
   total 421M
   -rw-rw-rw- 1 yottadbuser gtmuser 451M Jan 26 15:39 yottadb.dat
   -rw-rw-r-- 1 yottadbuser gtmuser 1.5K Jan 26 15:33 yottadb.gld

   VistA/o:
   total 0

   VistA/r:
   total 0
   yottadbuser@yottadbworkshop:~$

Notice that the database is 451MiB and contains 20,337,342 global variable nodes. Now, we can unpack the routines into the r directory.

.. parsed-literal::
   yottadbuser@yottadbworkshop:~$ tar -zxf /Distrib/VistA/WVEHRVOE10Routines -C VistA/
   yottadbuser@yottadbworkshop:~$ ls VistA/r | wc
   25163 25163 251422
   yottadbuser@yottadbworkshop:~$

This tells us that WorldVistA EHR has 25,163 source code modules. Now that the globals are loaded into the newly created database, we can turn on journaling so that we can recover the database in the event the system crashes. This is handy even for development environments – developers don't like to be kept waiting to recover environments and disk is inexpensive!

.. parsed-literal::
   yottadbuser@yottadbworkshop:~$ mupip set -journal="before,on" -region DEFAULT
   %YDB-I-JNLCREATE, Journal file /home/yottadbuser/VistA/g/yottadb.mjl created for region DEFAULT with BEFORE_IMAGES
   %YDB-I-JNLSTATE, Journaling state for region DEFAULT is now ON
   yottadbuser@yottadbworkshop:~$ ls -l VistA/g
   total 430948
   -rw-rw-rw- 1 yottadbuser gtmuser 472228352 Jan 26 15:51 yottadb.dat
   -rw-rw-r-- 1 yottadbuser gtmuser      1536 Jan 26 15:33 yottadb.gld
   -rw-rw-rw- 1 yottadbuser gtmuser     69632 Jan 26 15:51 yottadb.mjl
   yottadbuser@yottadbworkshop:~$

You can now run VistA – just enough to convince yourself that it is working, then exit.

.. parsed-literal::
   yottadbuser@yottadbworkshop:~$ mumps -dir

   YDB>set DUZ=1 do P^DI


   VA FileMan 22.0


   Select OPTION: ^
   YDB>halt
   yottadbuser@yottadbworkshop:~$

Note that YottaDB has dynamically compiled modules as needed.

.. parsed-literal::
   yottadbuser@yottadbworkshop:~$ ls -l VistA/o
   total 456
   -rw-rw-r-- 1 yottadbuser gtmuser 25752 Jan 26 15:53 DIALOG.o
   -rw-rw-r-- 1 yottadbuser gtmuser 11784 Jan 26 15:53 DIARB.o
   -rw-rw-r-- 1 yottadbuser gtmuser 22520 Jan 26 15:53 DIB.o
   -rw-rw-r-- 1 yottadbuser gtmuser 21528 Jan 26 15:53 DIC0.o
   -rw-rw-r-- 1 yottadbuser gtmuser 13496 Jan 26 15:53 DIC11.o
   -rw-rw-r-- 1 yottadbuser gtmuser 33160 Jan 26 15:53 DIC1.o
   -rw-rw-r-- 1 yottadbuser gtmuser 22520 Jan 26 15:53 DIC2.o
   -rw-rw-r-- 1 yottadbuser gtmuser 21176 Jan 26 15:53 DICATT2.o
   -rw-rw-r-- 1 yottadbuser gtmuser 14616 Jan 26 15:53 DICL.o
   -rw-rw-r-- 1 yottadbuser gtmuser 28808 Jan 26 15:53 DIC.o
   -rw-rw-r-- 1 yottadbuser gtmuser 18552 Jan 26 15:53 DICRW.o
   -rw-rw-r-- 1 yottadbuser gtmuser 15800 Jan 26 15:53 DICUIX1.o
   -rw-rw-r-- 1 yottadbuser gtmuser 20520 Jan 26 15:53 DICUIX2.o
   -rw-rw-r-- 1 yottadbuser gtmuser 19336 Jan 26 15:53 DICUIX.o
   -rw-rw-r-- 1 yottadbuser gtmuser 22472 Jan 26 15:53 DII.o
   -rw-rw-r-- 1 yottadbuser gtmuser 10424 Jan 26 15:53 DILF.o
   -rw-rw-r-- 1 yottadbuser gtmuser 29848 Jan 26 15:53 DILIBF.o
   -rw-rw-r-- 1 yottadbuser gtmuser  2904 Jan 26 15:53 DI.o
   -rw-rw-r-- 1 yottadbuser gtmuser 14104 Jan 26 15:53 DIQGU.o
   -rw-rw-r-- 1 yottadbuser gtmuser 27240 Jan 26 15:53 _DTC.o
   -rw-rw-r-- 1 yottadbuser gtmuser 23176 Jan 26 15:53 _ZOSV.o
   yottadbuser@yottadbworkshop:~$

**Pre-compiled Routines**

VistA is written to be portable across different MUMPS implementations. This means that it is guaranteed to contain code that is syntactically incorrect for every MUMPS implementation. As modules are dynamically compiled, they will generate compilation errors that are sent to STDERR. Since you may find these disconcerting, you can prevent them by dynamically compiling all the modules.

.. parsed-literal::
   yottadbuser\@yottadbworkshop:~$ cd VistA/o
   yottadbuser\@yottadbworkshop:~/VistA/o$ find ../r -name "\*.m" -print -exec $ydb_dist/mumps {} \\;

   …

   yottadbuser\@yottadbworkshop:~/VistA/o$

Ideally, we would simply compile with a mumps \*.m command, but 25,163 routines would make for a command line longer than the shell can handle. So, we use the find command; you can also compile using the xargs command. You should also confirm that all routines were compiled by counting the number of source modules and the number of object modules.

.. parsed-literal::
   yottadbuser@yottadbworkshop:~/VistA/o$ ls | wc
     25163   25163  251422
   yottadbuser@yottadbworkshop:~/VistA/o$ ls ../r|wc
     25163   25163  251422
   yottadbuser@yottadbworkshop:~/VistA/o$

Make sure you can start and run VistA after recompiling the modules.

**Multiple YottaDB Versions**

YottaDB object files are specific to each release of YottaDB – so r1.22 cannot use object files generated by r1.10, for example. Although the database format is more stable, a database file can only be concurrently open only by processes of one YottaDB. The same source code, however, can be used by an unlimited number of YottaDB releases. Also, even within a single YottaDB release, the same source code can be used by processes running in M mode and UTF-8 mode – but the object files are different. The directory tree structure implemented in the simple environment allows only processes of only one YottaDB release operating in only one mode to use a set of YottaDB source modules.

.. parsed-literal::
   yottadbuser@yottadbworkshop:~$ tree -d VistA/
   VistA/
   ├── g
   ├── o
   └── r

   3 directories
   yottadbuser@yottadbworkshop:~$

By creating another layer in the directory structure, the same VistA routines can be made to work in multiple YottaDB releases, for example, if we had:

.. parsed-literal::
   yottadbuser@yottadbworkshop:~$ mkdir VistA/r122
   yottadbuser@yottadbworkshop:~$ mupip set -journal=disable -region DEFAULT
   %YDB-I-JNLSTATE, Journaling state for region DEFAULT is now DISABLED
   yottadbuser@yottadbworkshop:~$ mv VistA/{g,o} VistA/r122/
   yottadbuser@yottadbworkshop:~$ tree -d VistA/
   VistA/
   ├── r
   └── r122
        ├── g
        └── o

   4 directories
   yottadbuser@yottadbworkshop:~$

Notice that we simply moved the object, journal, global directory and database files from one location to another, after disabling journaling. Here are some rules for when you move files:

- YottaDB assumes that source files have no embedded location information within (of course the programmer may make assumptions)

- Object files include the absolute paths to the source files they were compiled from (in order to implement the $text() function). So, although you can move object files freely without impacting functionality, if you move a source file, you should recompile object files compiled from it.

- Database files have pointers to their current journal files and journal files have pointers to their database files and prior generation journal files. The safe way to move database and journal files is to disable journaling, move the files, and re-enable them. This must be corrected.

- Global directories have pointers to database files. The global directory should be edited with GDE to correct this. Also, although the journal file information in the DEFAULT region is not meaningful once the database is created, it should be edited since it may be used in the future to create a database file, and the journal file name from the global is placed in the database file header when a new database file is created.

- Environment setup should be updated. Although our env file can exist anywhere, since it sets environment variables that include path information, it is probably best to move it to the r1.22 directory.

Fixing the above (starting with the environment file, since it points to the global directory, and then the global directory, since it points to the database file):

.. parsed-literal::
   yottadbuser@yottadbworkshop:~$ mv VistA/ydbenv VistA/r122/
   yottadbuser@yottadbworkshop:~$ vim VistA/r122/ydbenv ;  cat VistA/r122/ydbenv
   export gtmdir=$HOME/VistA
   export ydb_rel=r122
   export ydb_dist=/usr/local/lib/yottadb/$ydb_rel
   export ydb_gbldir=$gtmdir/$ydb_rel/g/yottadb.gld
   export ydb_log=/tmp/yottadb/$ydb_rel
   export ydb_tmp=$ydb_log
   export ydb_repl_instance=$gtmdir/$ydb_rel/g/yottadb.repl
   export ydb_repl_instname=dummy
   export ydb_routines="$gtmdir/$ydb_rel/o*($gtmdir/r) $ydb_dist/libyottadbutil.so"
   mkdir -p $ydb_tmp
   alias mumps=$ydb_dist/mumps
   alias mupip=$ydb_dist/mupip
   yottadbuser@yottadbworkshop:~$ 

Notice that since the YottaDB version occurs in multiple locations, it has been abstracted to the environment variable $ydb_rel. Also, /home/yottadbuser/VistA can be abstracted into an environment variable $gtmdir. By modifying the global directory to use the environment variables, the global directory becomes more portable.

.. parsed-literal::
   yottadbuser\@yottadbworkshop:~$ source VistA/r122/ydbenv
   yottadbuser\@yottadbworkshop:~$ mumps -run GDE
   %GDE-I-LOADGD, Loading Global Directory file
           /home/yottadbuser/VistA/r122/g/yottadb.gld
   %GDE-I-VERIFY, Verification OK
   
   GDE> change -segment DEFAULT -file=$gtmdir/$ydb_rel/g/yottadb.dat
   GDE> show -segment

                                   \*\*\*  SEGMENTS  \*\*\*
   Segment      File (def ext: .dat)          Acc   Type  Block   Alloc   Exten   Options
   ---------------------------------------------------------------------------------------
   DEFAULT   $gtmdir/$ydb_rel/g/yottadb.dat  BG    DYN   4096    5000    10000   GLOB=1000
                                                                                  LOCK=40
                                                                                  RES=0
                                                                                  ENCR=OFF
                                                                                  MSLT=1024

   GDE> change -region DEFAULT -journal=file=$gtmdir/$ydb_rel/g/yottadb.mjl
   GDE> show -region

                                   \*\*\* REGIONS  \*\*\*
   Region        Dynamic Segment          Def Coll   Rec Size   Key Size  Null Subs  Std Null Coll Jnl Inst Freeze on Error  Qdb Rundown
   --------------------------------------------------------------------------------------------------------------------------------------
   DEFAULT         DEFAULT                   0        4080        255         NEVER         Y       Y     DISABLED            DISABLED

                                   \*\*\*  JOURNALING INFORMATION  \*\*\*
   Region        Jnl File (def ext: .mjl)    Before   Buff   Alloc   Exten  Autoswitch
   ------------------------------------------------------------------------------------
   DEFAULT    $gtmdir/$ydb_rel/g/yottadb.mjl  Y      2308   2048    2048   8386560

   GDE> exit
   %GDE-I-VERIFY, Verification OK

   %GDE-I-GDUPDATE, Updating Global Directory file 
           /home/yottadbuser/VistA/r122/g/yottadb.gld
   yottadbuser\@yottadbworkshop:~$

Now re-enable journaling so that the database and journal pointers are correct. You will need to delete the prior journal file, because the database file does not have a pointer to it, and YottaDB will refuse to create a new journal file where a file already exists that is not pointed to by the database file.

.. parsed-literal::
   yottadbuser\@yottadbworkshop:~$ rm /home/yottadbuser/VistA/r122/g/yottadb.mjl
   yottadbuser\@yottadbworkshop:~$ mupip set -journal="enable,on,before,file=$gtmdir/$ydb_rel/g/yottadb.mjl" -region DEFAULT
   %YDB-I-JNLCREATE, Journal file /home/yottadbuser/VistA/r122/g/yottadb.mjl created for region DEFAULT with BEFORE_IMAGES
   %YDB-W-JNLBUFFREGUPD, Journal file buffer size for region DEFAULT has been adjusted from 2308 to 2312.
   %YDB-I-JNLSTATE, Journaling state for region DEFAULT is now ON
   yottadbuser@yottadbworkshop:~$

And now VistA is again ready for use with the new directory structure:

.. parsed-literal::
   yottadbuser@yottadbworkshop:~$ mumps -dir

   YDB>set DUZ=1 do P^DI


   VA FileMan 22.0


   Select OPTION: ^
   YDB>halt
   yottadbuser@yottadbworkshop:~$ 

We can add additional directories for other versions of YottaDB. e.g.,

.. parsed-literal::
   yottadbuser@yottadbworkshop:~$ mkdir -p VistA/r110/{g,o}
   yottadbuser@yottadbworkshop:~$ tree -d VistA
   
   VistA
   ├── r
   ├── r122
   │   ├── g
   │   └── o
   └── r110
       ├── g
       └── o

   7 directories
   yottadbuser@yottadbworkshop:~$
 
This facilitates simple upgrades. For example, if you wanted to migrate from r1.22 to (an as yet unreleased as of this writing) r1.30, you could effect a rolling upgrade using replicating between the r1.22 and r1.30 sub-directories within the same directory.

**A Minor Refinement – YottaDB Version Dependent Source**

In general, program source code is independent of the YottaDB version. On occasion, you may want to take advantage of an enhancement in a YottaDB version, with modified source code. You can augment the g and o subdirectories with an r subdirectory for such version specific source code modules. Of course, unless the env file is updated accordingly, YottaDB will never find the version specific routines.

.. parsed-literal::
   yottadbuser\@yottadbworkshop:~$ for i in VistA/r1* ; do mkdir $i/r ; done
   yottadbuser\@yottadbworkshop:~$ tree -d VistA/
   
   VistA
   ├── r
   ├── r122
   │   ├── g
   │   ├── o
   │   └── r
   └── r110
       ├── g
       ├── o
       └── r

    9 directories
    yottadbuser\@yottadbworkshop:~$ vim VistA/r122/ydbenv
    yottadbuser\@yottadbworkshop:~$ cat VistA/r122/ydbenv
    export gtmdir=$HOME/VistA
    export ydb_rel=r122
    export ydb_dist=/usr/local/lib/yottadb/$ydb_rel
    export ydb_gbldir=$gtmdir/$ydb_rel/g/yottadb.gld
    export ydb_log=/tmp/yottadb/$ydb_rel
    export ydb_tmp=$ydb_log
    export ydb_repl_instance=$gtmdir/$ydb_ver/yottadb.repl
    export ydb_repl_instname=dummy
    export ydb_routines="$gtmdir/$ydb_rel/o*($gtmdir/$ydb_rel/r $gtmdir/r) $ydb_dist/libyottadbutil.so"
    mkdir -p $ydb_tmp
    alias mumps=$ydb_dist/mumps
    alias mupip=$ydb_dist/mupip
    yottadbuser\@yottadbworkshop:~$ source VistA/r122/ydbenv
    yottadbuser\@yottadbworkshop:~$ mumps -dir

    YDB>write $zroutines
    /home/yottadbuser/VistA/r122_x86_64/o*(/home/yottadbuser/VistA/r1.22_x86_64/r /home/yottadbuser/VistA/r) /usr/local/lib/yottadb/r1.22_x86_64/libyottadbutil.so
    YDB>halt
    yottadbuser\@yottadbworkshop:~$

**Segregating Local Modifications**

Installations of large applications often have local or modifications. In such cases, it is important to be able to segregate the local patches from the standard application distribution. This can be effected by placing these routines in p subdirectories, and placing the p subdirectories ahead of the r subdirectories where the standard routines remain untouched.

.. parsed-literal::
   yottadbuser\@yottadbworkshop:~$ for i in VistA/r1* ; do mkdir $i/p ; done
   yottadbuser\@yottadbworkshop:~$ mkdir $gtmdir/p
   yottadbuser\@yottadbworkshop:~$ tree -d VistA
   
   VistA
   ├── p
   ├── r
   ├── r122
   │   ├── g
   │   ├── o
   │   ├── p
   │   └── r
   └── r110
       ├── g
       ├── o
       ├── p
       └── r

    12 directories
    yottadbuser\@yottadbworkshop:~$ vim VistA/r122/ydbenv ;  cat VistA/r122/ydbenv
    export gtmdir=$HOME/VistA
    export ydb_rel=r122
    export ydb_dist=/usr/local/lib/yottadb/$ydb_rel
    export ydb_gbldir=$gtmdir/$ydb_rel/g/yottadb.gld
    export ydb_log=/tmp/yottadb/$ydb_rel
    export ydb_tmp=$ydb_log
    export ydb_repl_instance=$gtmdir/$ydb_rel/g/yottadb.repl
    export ydb_repl_instname=dummy
    export ydb_routines="$gtmdir/$ydb_rel/o*($gtmdir/$ydb_rel/p $gtmdir/$ydb_rel/r $gtmdir/p $gtmdir/r) $ydb_dist/libyottadbutil.so"
    mkdir -p $ydb_tmp
    alias mumps=$ydb_dist/mumps
    alias mupip=$ydb_dist/mupip
    yottadbuser\@yottadbworkshop:~$

Now you can look at this in operation by applying some modifications to VistA that allow it to operate better with YottaDB to the /home/yottadbuser/VistA/p directory. Download the file KSBVistAPatches.zip from `YottaDB on GitLab <https://gitlab.com/YottaDB/DB/YDBDoc/blob/master/AcculturationGuide/KSBVistAPatches.zip>`_, and put it in the /Distrib/VistA directory. Then unpack it to /home/yottadbuser/VistA/p directory.

.. parsed-literal::
   yottadbuser@yottadbworkshop:~$ ls -l /Distrib/VistA/KSBVistAPatches.zip
   -rw-r--r-- 1 yottadbuser yottadbuser 20530 Jan 15 17:49 /Distrib/VistA/KSBVistAPatches.zip
   yottadbuser@yottadbworkshop:~$ unzip -d VistA/p /Distrib/VistA/KSBVistAPatches.zip
   Archive:  /Distrib/VistA/KSBVistAPatches.zip
   inflating: VistA/p/XPDR.m          
   inflating: VistA/p/XWBTCPM.m       
   inflating: VistA/p/ZOSV2GTM.m      
   inflating: VistA/p/_ZOSV2.m        
   inflating: VistA/p/ZOSVGUX.m       
   inflating: VistA/p/_ZOSV.m         
   inflating: VistA/p/ZTMGRSET.m 
   yottadbuser@yottadbworkshop:~$

Then compile it with the object files in the VistA/r122/o directory

.. parsed-literal::
   yottadbuser\@yottadbworkshop:~$ cd VistA/r122/o
   yottadbuser\@yottadbworkshop:~/VistA/r122/o$ mumps ../../p/\*.m
   yottadbuser\@yottadbworkshop:~/VistA/r122/o$

Now, you can run VistA with the local modifications. In this case, one of the modifications is a fix to a minor bug in VistA: it treats spaces separating source directories in a parenthesized list as part of the directory name, rather than as a separator. With the change, when you run a function - for example, to apply a patch - it correctly puts the new routine in the first source directory even if it is within a parenthesized list of directories. In this example, you will run the ^ZTMGRTSET function. Notice that the VistA/r110/p directory is initially empty, but has some tens of files afterwards.

.. parsed-literal::
   yottadbuser\@yottadbworkshop:~$ ls -l VistA/r110/p
   total 0
   yottadbuser\@yottadbworkshop:~$ mumps -dir

   YDB>do ^ZTMGRSET


   ZTMGRSET Version 8.0 Patch level \**34,36,69,94,121,127,136,191,275,355**
   HELLO! I exist to assist you in correctly initializing the current account.

   This is namespace or uci EHR,EHR.
   Should I continue? N//y
   I think you are using GT.M (Unix)
   Which MUMPS system should I install?

   1 = VAX DSM(V6), VAX DSM(V7)
   2 = MSM-PC/PLUS, MSM for NT or UNIX
   3 = Cache (VMS, NT, Linux), OpenM-NT
   4 = Datatree, DTM-PC, DT-MAX
   5 =
   6 =
   7 = GT.M (VMS)
   8 = GT.M (Unix)
   System: 8//

   I will now rename a group of routines specific to your operating system.
   Routine: ZOSVGUX      Loaded, Saved as %ZOSV
   Routine:
   Routine: ZIS4GTM      Loaded, Saved as %ZIS4
   Routine: ZISFGTM      Loaded, Saved as %ZISF
   Routine: ZISHGTM      Loaded, Saved as %ZISH
   Routine: XUCIGTM      Loaded, Saved as %XUCI
   Routine: ZISETGUX     Missing
   Routine: ZOSV2GTM     Loaded, Saved as %ZOSV2
   Routine: ZISTCPS      Loaded, Saved as %ZISTCPS

   NAME OF MANAGER'S UCI,VOLUME SET: EHR,EHR//
   The value of PRODUCTION will be used in the GETENV api.
   PRODUCTION (SIGN-ON) UCI,VOLUME SET: EHR,EHR//
   The VOLUME name must match the one in PRODUCTION.
   NAME OF VOLUME SET: EHR//
   The temp directory for the system: '/tmp/'//
   ^%ZOSF setup

   Now to load routines common to all systems.
   Routine: ZTLOAD       Loaded, Saved as %ZTLOAD
   Routine: ZTLOAD1      Loaded, Saved as %ZTLOAD1
   Routine: ZTLOAD2      Loaded, Saved as %ZTLOAD2
   Routine: ZTLOAD3      Loaded, Saved as %ZTLOAD3
   Routine: ZTLOAD4      Loaded, Saved as %ZTLOAD4
   Routine: ZTLOAD5      Loaded, Saved as %ZTLOAD5
   Routine: ZTLOAD6      Loaded, Saved as %ZTLOAD6
   Routine: ZTLOAD7      Loaded, Saved as %ZTLOAD7
   Routine: ZTM          Loaded, Saved as %ZTM
   Routine: ZTM0         Loaded, Saved as %ZTM0
   Routine: ZTM1         Loaded, Saved as %ZTM1
   Routine: ZTM2         Loaded, Saved as %ZTM2
   Routine: ZTM3         Loaded, Saved as %ZTM3
   Routine: ZTM4         Loaded, Saved as %ZTM4
   Routine: ZTM5         Loaded, Saved as %ZTM5
   Routine: ZTM6         Loaded, Saved as %ZTM6
   Routine: ZTMS         Loaded, Saved as %ZTMS
   Routine: ZTMS0        Loaded, Saved as %ZTMS0
   Routine: ZTMS1        Loaded, Saved as %ZTMS1
   Routine: ZTMS2        Loaded, Saved as %ZTMS2
   Routine: ZTMS3        Loaded, Saved as %ZTMS3
   Routine: ZTMS4        Loaded, Saved as %ZTMS4
   Routine: ZTMS5        Loaded, Saved as %ZTMS5
   Routine: ZTMS7        Loaded, Saved as %ZTMS7
   Routine: ZTMSH        Loaded, Saved as %ZTMSH
   Routine: ZTER         Loaded, Saved as %ZTER
   Routine: ZTER1        Loaded, Saved as %ZTER1
   Routine: ZIS          Loaded, Saved as %ZIS
   Routine: ZIS1         Loaded, Saved as %ZIS1
   Routine: ZIS2         Loaded, Saved as %ZIS2
   Routine: ZIS3         Loaded, Saved as %ZIS3
   Routine: ZIS5         Loaded, Saved as %ZIS5
   Routine: ZIS6         Loaded, Saved as %ZIS6
   Routine: ZIS7         Loaded, Saved as %ZIS7
   Routine: ZISC         Loaded, Saved as %ZISC
   Routine: ZISP         Loaded, Saved as %ZISP
   Routine: ZISS         Loaded, Saved as %ZISS
   Routine: ZISS1        Loaded, Saved as %ZISS1
   Routine: ZISS2        Loaded, Saved as %ZISS2
   Routine: ZISTCP       Loaded, Saved as %ZISTCP
   Routine: ZISUTL       Loaded, Saved as %ZISUTL
   Routine: ZTPP         Loaded, Saved as %ZTPP
   Routine: ZTP1         Loaded, Saved as %ZTP1
   Routine: ZTPTCH       Loaded, Saved as %ZTPTCH
   Routine: ZTRDEL       Loaded, Saved as %ZTRDEL
   Routine: ZTMOVE       Loaded, Saved as %ZTMOVE
   Want to rename the FileMan routines: No//y
   Routine: DIDT         Loaded, Saved as %DT
   Routine: DIDTC        Loaded, Saved as %DTC
   Routine: DIRCR        Loaded, Saved as %RCR
   Setting ^%ZIS('C')

   Now, I will check your % globals...........
   ALL DONE
   YDB>halt
   yottadbuser\@yottadbworkshop:~$ ls -l VistA/r110/p|wc
        57     506    3132
   yottadbuser\@yottadbworkshop:~$

**Creating a Development Environment**

When you work on an application, either to enhance it or to fix a bug, you typically modify only a small part of the application. With YottaDB, you do not need to make a copy of an entire application environment to work on your project. Nor do you need to work in the same environment as other developers, with the risk of stepping on one another's toes. All you need is to to set up your processes so that their $ZROUTINES search path finds your development routines before finding the main application routines. If your work involves changes to global variables, you can set up your own copy of the database – or, even, if it makes sense, a part of the database with the remaining globals mapped to the parent environment. Of course, in a large project, your environment's parent may itself have a parent.

Delete the r1.22 subdirectory, and obtain the files `inc <https://gitlab.com/YottaDB/DB/YDBDoc/blob/master/AcculturationGuide/inc>`_ and `install <https://gitlab.com/YottaDB/DB/YDBDoc/blob/master/AcculturationGuide/install>`_ from GitLab, and make install executable.

.. parsed-literal::
   yottadbuser@yottadbworkshop:~$ ls -l VistA/
   total 872
   -rw-r--r-- 1 yottadbuser yottadbuser    894 Jan 16 13:20 inc
   -rwxr-xr-x 1 yottadbuser yottadbuser   4416 Jan 16 13:20 install
   drwxrwxr-x 2 yottadbuser yottadbuser    114 Jan 15 17:54 p
   drwxr-x--- 2 yottadbuser yottadbuser 606208 Feb  6  2009 r
   drwxrwxr-x 6 yottadbuser yottadbuser    108 Jan 16 13:20 r110
   yottadbuser@yottadbworkshop:~$

Similarly, to VistA/r110 copy the files wvehrstop, wvehrstart, run, newjnls and ydbenv (yes, the latter overwrites the ydbenv file you already have there). Make wvehrstop, wvehrstart, run and newjnls executable. Also, replace the env in VistA/r110 with the new env file. Create a symbolic link called ydb to /usr/local/lib/yottadb/r110.

.. parsed-literal::
   yottadbuser@yottadbworkshop:~$ ls -l VistA/r110/
   total 896
   drwxrwxr-x 2 yottadbuser yottadbuser     48 Jan 15 16:47 g
   lrwxrwxrwx 1 yottadbuser yottadbuser     32 Jan 16 13:24 ydb -> /usr/local/lib/yottadb/r110
   -rw-rw-r-- 1 yottadbuser yottadbuser    735 Jan 16 13:20 ydbenv
   -rwxr-xr-x 1 yottadbuser yottadbuser    181 Jan 16 13:20 newjnls
   drwxrwxr-x 2 yottadbuser yottadbuser 610304 Jan 16 11:57 o
   drwxrwxr-x 2 yottadbuser yottadbuser   4096 Jan 16 12:01 p
   drwxrwxr-x 2 yottadbuser yottadbuser   4096 Jan 16 11:59 r
   -rwxr-xr-x 1 yottadbuser yottadbuser    340 Jan 16 13:20 run
   -rwxr-xr-x 1 yottadbuser yottadbuser    277 Jan 16 13:20 wvehrstart
   -rwxr-xr-x 1 yottadbuser yottadbuser    161 Jan 16 13:20 wvehrstop
   yottadbuser@yottadbworkshop:~$

Starting with a clean environment (no ydb* environment variables defined), create a child environment of VistA called dev using the install script.

.. parsed-literal::
   yottadbuser@yottadbworkshop:~$ VistA/install dev
   Creating environment in dev as child of environment in /home/yottadbuser/VistA
   Default permission for development environment is for all to read and group to write - please alter as needed
   yottadbuser@yottadbworkshop:~$ ls -lR dev
   dev:
   total 16
   -r--r--r-- 1 yottadbuser yottadbuser  894 2010-09-14 17:33 inc
   -r-xr-x--x 1 yottadbuser yottadbuser 4416 2010-09-14 17:33 install
   drwxrwxr-x 2 yottadbuser yottadbuser    1 2010-09-14 17:33 p
   lrwxrwxrwx 1 yottadbuser yottadbuser   19 2010-09-14 17:33 parent -> /home/yottadbuser/VistA
   drwxrwxr-x 2 yottadbuser yottadbuser    1 2010-09-14 17:33 r
   drwxrwxr-x 7 yottadbuser yottadbuser   88 2010-09-14 17:33 r110

   dev/p:
   total 0

   dev/r:
   total 0

   dev/V6.2-000_x86_64:
   total 20
   -r--r--r-- 1 yottadbuser yottadbuser 731 2010-09-14 17:33 env
   drwxrwxr-x 2 yottadbuser yottadbuser   8 2010-09-14 17:33 g
   lrwxrwxrwx 1 yottadbuser yottadbuser  36 2010-09-14 17:33 ydb -> /home/yottadbuser/VistA/r110/ydb
   -r-xr-xr-x 1 yottadbuser yottadbuser 181 2010-09-14 17:33 newjnls
   drwxrwxr-x 2 yottadbuser yottadbuser   1 2010-09-14 17:33 o
   drwxrwxr-x 2 yottadbuser yottadbuser   1 2010-09-14 17:33 p
   drwxrwxr-x 2 yottadbuser yottadbuser   1 2010-09-14 17:33 r
   -r-xr-x--x 1 yottadbuser yottadbuser 343 2010-09-14 17:33 run
   drwxrwxr-x 2 yottadbuser yottadbuser   1 2010-09-14 17:33 tmp
   -r-xr-xr-x 1 yottadbuser yottadbuser 277 2010-09-14 17:33 wvehrstart
   -r-xr-xr-x 1 yottadbuser yottadbuser 161 2010-09-14 17:33 wvehrstop

   dev/V6.2-000_x86_64/g:
   total 4
   -r--r--r-- 1 yottadbuser yottadbuser 1536 2010-09-13 18:22 yottadb.gld

   dev/V6.2-000_x86_64/o:
   total 0

   dev/V6.2-000_x86_64/p:
   total 0

   dev/V6.2-000_x86_64/r:
   total 0

   dev/V6.2-000_x86_64/tmp:
   total 0
   yottadbuser@yottadbworkshop:~$

Now run the dev environment and notice the values of the environment variables. In particular notice how the database used is that of the parent.

.. parsed-literal::

   yottadbuser\@yottadbworkshop:~$ dev/r110/run

   YDB>write $zgbldir
   /home/yottadbuser/dev/r110/g/yottadb.gld
   YDB>write $zroutines
   /home/yottadbuser/dev/r110/o(/home/yottadbuser/dev/r110/p /home/yottadbuser/dev/r110/r /home/yottadbuser/dev/p /home/yottadbuser/dev/r) /home/yottadbuser/dev/parent/r110/o(/home/yottadbuser/dev/parent/r110/p /home/yottadbuser/dev/parent/r110/r /home/yottadbuser/dev/parent/p /home/yottadbuser/dev/parent/r) /home/yottadbuser/dev/r110/ydb/libyottadbutil.so
   YDB>zsystem "env | grep ^ydb"
   ydb_repl_instance=/home/yottadbuser/dev/parent/r110/g/ydb.repl
   ydb_log=/tmp/yottadb/r110
   ydb_retention=42
   ydb_rel=110
   ydb_icu_version=5.5
   ydb_gbldir=/home/yottadbuser/dev/r110/g/yottadb.gld
   ydb_routines=/home/yottadbuser/dev/r110/o*(/home/yottadbuser/dev/r110/p /home/yottadbuser/dev/r110/r /home/yottadbuser/dev/p /home/yottadbuser/dev/r) /home/yottadbuser/dev/parent/r110/o*(/home/yottadbuser/dev/parent/r110/p /home/yottadbuser/dev/parent/r110/r /home/yottadbuser/dev/parent/p /home/yottadbuser/dev/parent/r) /home/yottadbuser/dev/r110/ydb/libyottadbutil.so
   gtmdir=/home/yottadbuser/dev/parent
   ydb_etrap=Write:(0=$STACK) "Error occurred: ",$ZStatus,!
   ydb_principal_editing=EDITING
   ydb_tmp=/tmp/yottadb/r110
   ydb_dist=/usr/local/lib/yottadb/r110

   YDB>do ^GDE
   %GDE-I-LOADGD, Loading Global Directory file
           /home/yottadbuser/dev/r110/g/yottadb.gld
   %GDE-I-VERIFY, Verification OK

   GDE> show -segment

                                \*\*\* SEGMENTS  \*\*\*
   Segment              File (def ext: .dat)        Acc  Type  Block   Alloc  Exten  Options
   ------------------------------------------------------------------------------------------
   DEFAULT      $gtmdir/$ydb_rel/g/yottadb.dat    BG   DYN   4096    5000   10000  GLOB=1000
                                                                                    LOCK=40
                                                                                    RES=0
                                                                                    ENCR=OFF
                                                                                    MSLT=1024

   GDE> quit
   %GDE-I-NOACTION, Not updating Global Directory /home/yottadbuser/dev/r110/g/yottadb.gld

   YDB>halt
   yottadbuser\@yottadbworkshop:~$


**Further Investigation on your own**

Use the ZEDIT command in the dev environment to create a “Hello, world” program and show that you can run it. Now, try executing that program in the parent VistA environment, and notice that it does not exist.

Investigate what happens if you use the --separate-globals flag of the install script. Work through the scripts to see how the environment variables are set up.

Examine the install script and see how it sets up a child environment.

