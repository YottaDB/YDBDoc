
.. index::
   Compiling ICU on supported platforms

==============================================================
Appendix C: Compiling ICU on YottaDB Supported Platforms
==============================================================

.. contents::
   : depth: 2

------------------------
Compiling ICU
------------------------

YottaDB recommends using the ICU libraries provided by the OS vendor. This appendix includes sample instructions to download ICU, configure it not to use multi-threading, and compile it for various platforms. Note that download sites, versions of compilers, and milli and micro releases of ICU may have changed ICU since the embedded dates when these instructions were tested making them out-of-date. Therefore, these instructions must be considered examples, not a cookbook.

Although YottaDB uses ICU, ICU is not YottaDB software and YottaDB does not support ICU. The sample instructions for installing and configuring ICU are merely provided as a convenience. All examples below are on older versions of ICU, you will be able to find the latest version on http://site.icu-project.org/download

+++++++++++++++++++++++++++++
Compiling ICU 4.4 on AIX 6.1
+++++++++++++++++++++++++++++

As of November 2011, ICU version 4.4 can be compiled on AIX with the following configuration:

Operating System: AIX

Version: AIX 6.1 64-bit

Compilers: IBM XL C/C++ 10, GNU make 3.80

**Installation Instructions**

Ensure that system environment variable PATH includes the location of all the compilers mentioned above.

Set the environment variable OBJECT_MODE to 64.

Download the source code of ICU version 4.4 for C from http://site.icu-project.org/download

At the shell prompt, execute the following commands:

.. parsed-literal::
   gunzip -d < icu4c-4_4-src.tgz | tar -xf -
   cd icu/source
   ./runConfigureICU AIX --disable-threads --enable-renaming=no --with-library-bits=64 CC=xlc
   gmake
   gmake -k check
   gmake install

Set the environment variable LIBPATH to point to the location of ICU. AIX uses the environment variable LIBPATH to search for dynamically linked libraries.

This installs ICU in the /usr/local directory.

.. note::
   By default, ICU is installed in the /usr/local directory. To install ICU in a different directory, use --prefix=<install_path> with the runConfigureICU command.

++++++++++++++++++++++++++++++++++
Compiling ICU 4.2 on AIX 6.1
++++++++++++++++++++++++++++++++++

As of December 2009, ICU version 4.2 can be compiled on AIX with the following configuration:

Operating System: UNIX

Version: AIX 6.1 64-bit

Compilers: IBM XL C/C++ 10, GNU make 3.80

**Installation Instructions**

Ensure that system environment variable PATH includes the location of all the compilers mentioned above.

Set the environment variable OBJECT_MODE to 64.

Download the source code of ICU version 4.2 for C from http://site.icu-project.org/download

At the shell prompt, execute the following commands:

.. parsed-literal::
   gunzip -d < icu4c-4_2_1-AIX6_1-VA9.tgz | tar -xf - 
   cd icu/source
   ./configure CC=/usr/vac/bin/cc CXX=/usr/vacpp/bin/xlc++ --disable-threads --disable-renaming 
   gmake
   gmake check 
   gmake install

Set the environment variable LIBPATH to point to the location of ICU. AIX uses the environment variable LIBPATH to search for dynamically linked libraries.

This installs ICU in the /usr/local directory.

.. note::
   By default, ICU is installed on /usr/local directory. 

+++++++++++++++++++++++++++++++++++++++++++++++++++
Compiling ICU 4.2 on HP Integrity IA64 HP-UX 11.31
+++++++++++++++++++++++++++++++++++++++++++++++++++

As of November 2009, ICU version 4.2 could be compiled on HP Integrity IA64 HP-UX with the following configuration:

Operating System: UNIX

Version: IA64 HP-UX 11.31

Compilers: HP C/aC++ B3910B A.06.15, GNU make (3.81)

**Installation Instructions**

Ensure that system environment variable PATH includes the location of all the compilers mentioned above.

Download the source code of ICU (in this example version 3.6 for C from http://icu.sourceforge.net/).

At the shell prompt, run the following commands: 

.. parsed-literal::
   gunzip -d < icu4c-4_2_1-src.tgz | tar -xf -
   cd icu/source/
   chmod +x runConfigureICU configure install-sh
   ./runConfigureICU HP-UX/ACC --disable-renaming --disable-threads --with-library-bits=64
   gmake
   gmake check
   gmake install

Set the environment variable LD_LIBRARY_PATH to point to the location of ICU. HP-UX uses the environment variable LD_LIBRARY_PATH to search for dynamically linked libraries.

This installs ICU in the /usr/local directory.

.. note::
   By default, ICU is installed in the /usr/local directory. To install ICU in a different directory, use --prefix=<install_path> with the runConfigureICU command.  

+++++++++++++++++++++++++++++++++++++++++++++++++++
Compiling ICU 3.6 on HP Integrity IA64 HP-UX 11.31
+++++++++++++++++++++++++++++++++++++++++++++++++++

As of November 2009, ICU version 3.6 could be compiled on HP Integrity IA64 HP-UX with the following configuration:

Operating System: UNIX

Version: IA64 HP-UX 11.31

Compilers: HP C/aC++ B3910B A.06.15, GNU make (3.81)

**Installation Instructions**

Ensure that system environment variable PATH includes the location of all the compilers mentioned above.

Download the source code of ICU (in this example version 3.6 for C from http://icu.sourceforge.net/download/3.6.html#ICU4C).

At the shell prompt, run the following commands: 

.. parsed-literal::
   gunzip -d <  icu4c-3_6-src.tgz | tar -xf -
   cd icu/source/
   chmod +x runConfigureICU configure install-sh
   runConfigureICU HP-UX/ACC --disable-threads
   gmake
   gmake check
   gmake install

Set the environment variable LD_LIBRARY_PATH to point to the location of ICU. HP-UX uses the environment variable LD_LIBRARY_PATH to search for dynamically linked libraries. 

This installs ICU in the /usr/local directory.

.. note::
   By default, ICU is installed in the /usr/local directory.  If you install ICU in a different directory, type: runConfigureICU HP-UX/ACC --prefix=<install_path> --disable-threads . Then run the gmake commands, and set the environment variable LD_LIBRARY_PATH to point to the appropriate location.
        
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Compiling ICU 4.2 on Red Hat Enterprise Linux 4 Update 2
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++

As of December 2009, ICU version 4.2 could be compiled on x86_64 Linux with the following configuration:

Operating System: x86_64 Linux

Version: Red Hat Enterprise Linux 4 Update 2

Compilers: gcc 3.4.4, GNU make (3.77+), ANSI C compiler

**Installation Instructions**

Ensure that system environment variable PATH includes the location of all the compilers mentioned above.

Download the source code of ICU version 4.2 for C from http://site.icu-project.org/download.

At the shell prompt, execute the following commands:

.. parsed-literal::
   gunzip -d < icu4c-3_6-src.tgz | tar -xf -
   cd icu/source/
   chmod +x runConfigureICU configure install-sh
   ./runConfigureICU Linux --disable-renaming --disable-threads --with-library-bits=64
   gmake
   gmake check
   gmake install

Set the environment variable LD_LIBRARY_PATH to point to the location of ICU. Linux uses the environment variable LD_LIBRARY_PATH to search for dynamically linked libraries to be loaded.

This installs ICU in the /usr/local directory.

.. note::
   By default, ICU is installed in the /usr/local directory. To install ICU in a different directory, use --prefix=<install_path> with the runConfigureICU command. 

+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Compiling ICU 3.6 on Red Hat Enterprise Linux 4 Update 2
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

As of January 2007, ICU version 3.6 could be compiled on x86 Linux with the following configuration:

Operating System: x86 Linux

Version: Red Hat Enterprise Linux 4 Update 2

Compilers: gcc 3.4.4, GNU make (3.77+), ANSI C compiler

**Installation Instructions**

Ensure that system environment variable PATH includes the location of all the compilers mentioned above.

Download the source code of ICU version 3.6 for C from http://icu.sourceforge.net/download/3.6.html#ICU4C

At the shell prompt, execute the following commands:

.. parsed-literal::
   gunzip -d < icu4c-3_6-src.tgz | tar -xf - 
   cd icu/source/ 
   chmod +x runConfigureICU configure install-sh
   runConfigureICU Linux --disable-64bit-libs --disable-threads
   gmake 
   gmake check 
   gmake install

Set the environment variable LD_LIBRARY_PATH to point to the location of ICU. Linux uses the environment variable LD_LIBRARY_PATH to search for dynamically linked libraries to be loaded.

This installs ICU in the /usr/local directory.

.. note::
   By default, ICU is installed on /usr/local directory. If you need to install ICU on a different directory type: runConfigureICU Linux --prefix=<install_path> --disable-64bit-libs --disable-threads. Then execute the gmake commands, and set the environment variable LD_LIBRARY_PATH to point to the appropriate location.

++++++++++++++++++++++++++++++++++++++++++++++++
Compiling ICU 4.2 on Solaris 9 (SunOS 5.9)
++++++++++++++++++++++++++++++++++++++++++++++++

As of December 2009, ICU version 4.2 could be compiled on Solaris with the following configuration:

Operating System: Solaris

Version: Solaris 9 (SunOS 5.9)

Compilers: Sun Studio 8 (Sun C++ 5.5), GNU make (3.77+), ANSI C compiler

**Installation Instructions**

Ensure that system environment variable PATH includes the location of all the compilers mentioned above.

Download the source code of ICU version 4.2 for C from http://site.icu-project.org/download

Add the following line in the configuration file source/config/mh-solaris to include the appropriate C++ runtime libraries:

.. parsed-literal::
   DEFAULT_LIBS = -lCstd -lCrun -lm -lc

At the shell prompt, execute the following commands:

.. parsed-literal::
   gunzip -d < icu4c-4_2_1-src.tgz | tar -xf -
   cd icu/source/
   chmod +x runConfigureICU configure install-sh
   ./configure --disable-renaming --disable-threads --enable-64bit-libs
   gmake
   gmake check
   gmake install

Set the environment variable LD_LIBRARY_PATH to point to the location of ICU. Solaris uses the environment variable LD_LIBRARY_PATH to search for dynamically linked libraries to be loaded.

ICU is now installed in the /usr/local directory.

.. note::
   By default, ICU is installed in the /usr/local directory. To to install ICU in a different directory, use --prefix=<install_path> with the runConfigure command. 

+++++++++++++++++++++++++++++++++++++++++++
Compiling ICU 3.6 on Solaris 9 (SunOS 5.9)
+++++++++++++++++++++++++++++++++++++++++++

As of January 2007, ICU version 3.6 could be compiled on Solaris with the following configuration:

Operating System: Solaris

Version: Solaris 9 (SunOS 5.9)

Compilers: Sun Studio 8 (Sun C++ 5.5), GNU make (3.77+), ANSI C compiler 

**Installation Instructions**

Ensure that system environment variable PATH includes the location of all the compilers mentioned above.

Download the source code of ICU version 3.6 for C from http://icu.sourceforge.net/download/3.6.html#ICU4C>

Add the following line in the configuration file source/config/mh-solaris to include the appropriate C++ runtime libraries:

.. parsed-literal::
   DEFAULT_LIBS = -lCstd -lCrun -lm -lc

At the shell prompt, execute the following commands:

.. parsed-literal::
   gunzip -d < icu4c-3_6-src.tgz | tar -xf - 
   cd icu/source/ 
   chmod +x runConfigureICU configure install-sh
   runConfigureICU Solaris --disable-64bit-libs --disable-threads
   gmake 
   gmake check 
   gmake install

Set the environment variable LD_LIBRARY_PATH to point to the location of ICU. Solaris uses the environment variable LD_LIBRARY_PATH to search for dynamically linked libraries to be loaded.

ICU is now installed in the /usr/local directory.

.. note::
   By default, ICU is installed in the /usr/local directory. If you need to install ICU on a different directory type: runConfigureICU Solaris --prefix=<install_path> --disable-64bit-libs --disable-threads. Then execute the gmake commands, and set the environment variable LD_LIBRARY_PATH to point to the appropriate location.




