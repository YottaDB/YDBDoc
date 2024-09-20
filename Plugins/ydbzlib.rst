.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2021-2024 YottaDB LLC and/or its subsidiaries.#
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

==========
YDB zlib
==========

.. contents::
   :depth: 3

----------
Overview
----------

YDBzlib is a simple plugin to allow M application code to use `zlib <http://zlib.net>`_ to compress and uncompress string data. YDBzlib is itself provided to you under the terms of the same license that YottaDB itself is provided to you. YDBzlib is simply a wrapper for zlib, and does not include zlib, which you must independently procure and install.

YDBzlib consists of an M module %ZLIB, which in turn calls zlib, to provide the following entryrefs:

.. code-block:: bash

  $$compress2^%ZLIB(origstr,.compstr,level)
  $$uncompress^%ZLIB(compstr,.uncompstr)
  $$version^%ZLIB
  $$zlibversion^%ZLIB

:code:`compress2` compresses :code:`origstr` and returns the compressed string in :code:`compstr`; :code:`level` is the string passed to zlib as the compression level (-1 through 9, defaulting to -1 if not specified, which in turn instructs zlib to use its default compression). :code:`uncompress` expands :code:`compstr` and provides the result in :code:`uncompstr`. Both :code:`compress2` and :code:`uncompress` return the status returned by the underlying zlib code; 0 for a normal return. For example:

.. code-block:: none

  YDB>set a="The quick brown fox"

  YDB>set s=$$compress2^%ZLIB(a,.b)

  YDB>zwrite
  a="The quick brown fox"
  b="x"_$C(156,11)_"�HU(,�L�VH*�/�SH˯"_$C(0,0)_"E."_$C(7,20)
  s=0

  YDB>set s=$$uncompress^%ZLIB(b,.c)

  YDB>zwrite
  a="The quick brown fox"
  b="x"_$C(156,11)_"�HU(,�L�VH*�/�SH˯"_$C(0,0)_"E."_$C(7,20)
  c="The quick brown fox"
  s=0

  YDB>


zlibversion returns the version of the zlib library, version returns the version of the plugin, which is a timestamp in $horolog format. For example:

.. code-block:: none

  YDB>set z=$$zlibversion^%ZLIB

  YDB>set p=$$version^%ZLIB

  YDB>zwrite
  p="62508,48729"
  z="1.2.3.4"

  YDB>

--------------
Quickstart
--------------

As a YottaDB plugin, YDBZlib requires YottaDB. Install YottaDB and YDBZlib together:

.. code:: bash

   mkdir /tmp/tmp ; wget -P /tmp/tmp https://gitlab.com/YottaDB/DB/YDB/raw/master/sr_unix/ydbinstall.sh
   cd /tmp/tmp ; chmod +x ydbinstall.sh
   sudo ./ydbinstall.sh --utf8 --zlib

Omit the ``--utf8`` option if you do not want UTF-8 support installed. If you already have YottaDB installed, use ``sudo $ydb_dist/ydbinstall --zlib --plugins-only --overwrite-existing`` to install or reinstall the YDBZlib plugin without reinstalling YottaDB.

--------------------------
Installation from Source
--------------------------

YDBzlib consists of three code files - :code:`gtmzlib.c`, :code:`gtmzlib.xc`, and :code:`_ZLIB.m` - and one readme.txt. It may also contain a COPYING file describing the terms of the license under which it is provided.

Compile the :code:`gtmzlib.c` source file to produce a :code:`libgtmzlib.so` shared library that can be called from M code. The following example is from Linux and assumes that a 64-bit version of YottaDB r1.32 for the x86 architecture is installed in :code:`/usr/local/lib/yottadb/r132`; refer to the `M Programmer's Guide <../ProgrammersGuide/index.html>`_ for the commands on your platform and adjust as needed for your specific directory structure.

.. code-block:: bash

  $ ls -l
  total 28
  -rw-r--r-- 1 ydbuser ydb 1358 2017-02-21 16:56 gtmzlib.c
  -rw-r--r-- 1 ydbuser ydb  282 2017-02-21 16:56 gtmzlib.xc
  -rw-r--r-- 1 ydbuser ydb 1471 2017-02-21 17:17 _ZLIB.m
  $ gcc -c -fPIC -I/usr/local/lib/yottadb/r132 gtmzlib.c
  $ gcc -o libgtmzlib.so -shared gtmzlib.o
  $ ls -l
  total 28
  -rw-r--r-- 1 ydbuser ydb 1358 2017-02-21 16:56 gtmzlib.c
  -rw-r--r-- 1 ydbuser ydb 1976 2017-02-21 18:00 gtmzlib.o
  -rw-r--r-- 1 ydbuser ydb  282 2017-02-21 16:56 gtmzlib.xc
  -rwxr-xr-x 1 ydbuser ydb 7997 2017-02-21 18:00 libgtmzlib.so
  -rw-r--r-- 1 ydbuser ydb 1471 2017-02-21 17:17 _ZLIB.m
  $


Copy the :code:`gtmzlib.xc` and :code:`libgtmzlib.so` files to the plugin subdirectory of your YottaDB directory; in this example, :code:`/usr/local/lib/yottadb/r132/plugin` (you will need to run this command as root, or other administrative userid needed to write into a sub-directory of the YottaDB installation):

.. code-block:: bash

  $ sudo cp gtmzlib.xc libgtmzlib.so /usr/local/lib/yottadb/r132/plugin/


Copy the _ZLIB.m file to the /usr/local/lib/yottadb/r132/plugin/r and compile it with an M-mode object module in /usr/local/lib/yottadb/r132/plugin/o and a UTF-8 mode object module in /usr/local/lib/yottadb/r132/plugin/o/utf8.

.. code-block:: bash

  $ find /usr/local/lib/yottadb/r132/plugin -iname \*zlib\*
  /usr/local/lib/yottadb/r132/plugin/libgtmzlib.so
  /usr/local/lib/yottadb/r132/plugin/r/_ZLIB.m
  /usr/local/lib/yottadb/r132/plugin/o/utf8/_ZLIB.o
  /usr/local/lib/yottadb/r132/plugin/o/_ZLIB.o
  /usr/local/lib/yottadb/r132/plugin/gtmzlib.xc
  $


As all YottaDB platforms support shared libraries, you can replace the .o object files with shared libraries for more efficient memory usage.

.. code-block:: bash

  $ find /usr/local/lib/yottadb/r132/plugin -iname \*zlib\*
  /usr/local/lib/yottadb/r132/plugin/libgtmzlib.so
  /usr/local/lib/yottadb/r132/plugin/r/_ZLIB.m
  /usr/local/lib/yottadb/r132/plugin/o/utf8/_ZLIB.o
  /usr/local/lib/yottadb/r132/plugin/o/_ZLIB.o
  /usr/local/lib/yottadb/r132/plugin/gtmzlib.xc
  $


When you run YottaDB, if your system has not been configured to automatically locate the zlib shared library on your system, you will need to do that (see `man ldconfig <https://man7.org/linux/man-pages/man8/ldconfig.8.html>`_ on Linux) or explicitly preload the library (e.g., with the LD_PRELOAD environment variable on Linux; the location of libz.so below is from Ubuntu Linux 11.10). The :code:`source ydb_env_set` command automatically includes the environment variables needed to access any plugin that follows these conventions.

.. code-block:: bash

  $ LD_PRELOAD=/lib/x86_64-linux-gnu/libz.so.1.2.3.4 /usr/local/lib/yottadb/r132


  YDB>set a="The quick brown fox jumps over the lazy dog"

  YDB>set s=$$compress2^%ZLIB(a,.b,9)

  YDB>set t=$$uncompress^%ZLIB(b,.c)

  YDB>zwrite
  a="The quick brown fox jumps over the lazy dog"
  b="x�"_$C(11)_"�HU(,�L�VH*�/�SH˯P�*�-(V�/K-R("_$C(1)_"J�$VU*���"_$C(3,0)_"[�"_$C(15)_"�"
  c="The quick brown fox jumps over the lazy dog"
  s=0
  t=0

  YDB>


.. note::

  zlib is not YottaDB software and is not supported as part of YottaDB support. YottaDB strongly encourages you to ensure that you have appropriate support for software that you rely on.

  The pre-allocation for return strings in :code:`gtmzlib.xc`, whether compressed or uncompressed, allows for strings up to 1048576 bytes (1MB) which is the longest string value currently supported by YottaDB. Extensive use of gtmzlib may therefore result in frequent garbage collection. If your application is guaranteed to use strings only smaller than 1MB, you can reduce this number accordingly.

  The YottaDB interface to call out to C libraries is a low-level interface designed for use by programmers rather than end-users. Misuse, abuse and bugs can result in applications that are fragile, hard to troubleshoot and with security vulnerabilities.

.. raw:: html

    <img referrerpolicy="no-referrer-when-downgrade" src="https://download.yottadb.com/plugins.png" />
