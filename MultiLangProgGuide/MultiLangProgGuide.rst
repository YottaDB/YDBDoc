.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2017-2021 YottaDB LLC and/or its subsidiaries.#
.. # All rights reserved.                                        #
.. #                                                             #
.. #     This source code contains the intellectual property     #
.. #     of its copyright holder(s), and is made available       #
.. #     under a license.  If you do not know the terms of       #
.. #     the license, please stop and do not read further.       #
.. #                                                             #
.. ###############################################################

.. index:: Multi-language Programming Guide

=================================
Multi-Language Programmer's Guide
=================================
.. contents::
   :depth: 4

---------
Overview
---------

YottaDB is a multi-language NoSQL database. The daemonless database engine resides in the address space of the process, and can be accessed from any supported language. Functions in the supported languages can call one another to the extent that such calling is permitted by the Supported language implementations.

As C is the *lingua franca* of programming, the C API provides access to the YottaDB engine from any language. As YottaDB adds standard APIs for other languages, additional sections will be added to the Programmers Guide.

.. _mlpg-quick-start:

------------
Quick Start
------------

+++++++++++++++++++
Local Installation
+++++++++++++++++++

#. Install prerequisites.

   - If not already installed, install *wget*, *binutils* and *pkg-config*: :code:`sudo apt install wget binutils pkg-config`

#. Install YottaDB.

   - Create a temporary directory and change to it, e.g.: :code:`mkdir /tmp/tmp ; cd /tmp/tmp`
   - Get the YottaDB installer: :code:`wget https://gitlab.com/YottaDB/DB/YDB/raw/master/sr_unix/ydbinstall.sh`
   - Make it executable: :code:`chmod +x ydbinstall.sh`
   - Run it with your choice of directory where you want it installed (omit the :code:`--verbose` option for less output): :code:`sudo ./ydbinstall.sh --utf8 default --verbose`. If you do not specify an installation directory with :code:`--installdir`, the script installs YottaDB in :code:`/usr/local/lib/yottadb/r###` where :code:`r###` is the release, e.g., :code:`r130`.

#. Set up the environment: :code:`source $(pkg-config --variable=prefix yottadb)/ydb_env_set`. This defaults to an environment in :code:`$HOME/.yottadb`; to use another directory, set the environment variable :code:`ydb_dir` to the desired directory.

#. Put your C program in the :code:`$ydb_dir` directory, :code:`#include` the file :code:`libyottadb.h` in your C program and compile it. As a sample program, you can download the `wordfreq.c <https://gitlab.com/YottaDB/DB/YDBTest/blob/master/simpleapi/inref/wordfreq.c>`_ program, with a `reference input file <https://gitlab.com/YottaDB/DB/YDBTest/blob/master/simpleapi/outref/wordfreq_input.txt>`_ and `corresponding reference output file <https://gitlab.com/YottaDB/DB/YDBTest/blob/master/simpleapi/outref/wordfreq_output.txt>`_ and compile it with :code:`gcc $(pkg-config --libs --cflags yottadb) -o wordfreq wordfreq.c -lyottadb`.

#. Run your program and verify that the output matches the reference output. For example:

   .. code-block:: bash

	$ cd $ydb_dir
	$ gcc $(pkg-config --libs --cflags yottadb) -o wordfreq wordfreq.c -lyottadb
	$ ./wordfreq <wordfreq_input.txt >wordfreq_output.tmp
	$ diff wordfreq_output.tmp wordfreq_output.txt
	$

Note that the :code:`wordfreq.c` program randomly uses local or global variables (see :ref:`lcl-gbl-var`).

+++++++++++++++++
Docker Container
+++++++++++++++++

You must have at least `Docker <https://www.docker.com/community-edition>`_ 17.05 installed. Pre-built images are available at `Docker Hub <https://hub.docker.com/r/yottadb/>`_. The Docker image is built to provide sane defaults to begin exploring YottaDB. It is not meant for production usage.

To run a pre-built image: :code:`docker run --rm -it yottadb/yottadb` to run the image but not persist any changes you make, and :code:`docker run -it yottadb/yottadb` for persistent changes.

Volumes are supported by mounting the :code:`/data` directory. To mount the local directory :code:`ydb-data` into the container to save your database and routines locally and use them in the container, add an appropriate command line parameter before the yottadb/yottadb argument, e.g., :code:`docker run -it -v \`pwd\`/ydb-data:/data yottadb/yottadb`

This creates a :code:`ydb-data` directory in your current working directory. After the container is shutdown and removed, delete the directory if you want to remove all data created in the YottaDB container (such as your database and routines).

.. _mlpg-concepts:

---------
Concepts
---------

+++++++++++++++++++++++++++++++++++++++++++++++
Keys, Values, Nodes, Variables, and Subscripts
+++++++++++++++++++++++++++++++++++++++++++++++

The fundamental core data structure provided by YottaDB is *key-value tuples*. For example, the following is a set of key value tuples:

.. code-block:: none

    ["Capital","Belgium","Brussels"]
    ["Capital","Thailand","Bangkok"]
    ["Capital","USA","Washington, DC"]

Note that data in YottaDB is *always* ordered. [#]_ Even if you input data out of order, YottaDB always stores them in order. In the discussion below, data is therefore always shown in order. For example, the data below may well have been loaded by country.

.. [#] The terms "collate", "order", and "sort" are equivalent.

Each of the above tuples is called a *node*. In an *n*-tuple, the first *n*-1 items can be thought of as the *keys*, and the last item is the *value* associated with the keys.

While YottaDB itself assigns no meaning to the data in each node, by convention, application maintainability is improved by using meaningful keys, for example:

.. code-block:: none

    ["Capital","Belgium","Brussels"]
    ["Capital","Thailand","Bangkok"]
    ["Capital","USA","Washington, DC"]
    ["Population","Belgium",1367000]
    ["Population","Thailand",8414000]
    ["Population","USA",325737000]

As YottaDB assigns no inherent meaning to the keys or values, its key value structure lends itself to implementing *Variety*. [#]_ For example, if an application wishes to add historical census results under "Population", the following is a perfectly valid set of tuples (source: `United States Census <https://en.wikipedia.org/wiki/United_States_Census>`_):

.. code-block:: none

    ["Capital","Belgium","Brussels"]
    ["Capital","Thailand","Bangkok"]
    ["Capital","USA","Washington, DC"]
    ["Population","Belgium",1367000]
    ["Population","Thailand",8414000]
    ["Population","USA",325737000]
    ["Population","USA",17900802,3929326]
    ["Population","USA",18000804,5308483]
    …
    ["Population","USA",20100401,308745538]

In the above, 17900802 represents August 2, 1790, and an application would determine from the number of keys whether a node represents the current population or historical census data.

.. [#] Variety is one of the *three "V"s* of "big data" — Velocity,
       Volume, and Variety. YottaDB handles all three very well.

In YottaDB, the first key is called a *variable*, and the remaining keys are called *subscripts* allowing for a representation both compact and familiar to a programmer, e.g., :code:`Capital("Belgium")="Brussels"`. The set of all nodes under a variable is called a *tree* (so in the example, there are two trees, one under :code:`Capital` and the other under :code:`Population`). The set of all nodes under a variable and a leading set of its subscripts is called a *subtree* (e.g., :code:`Population("USA")` is a subtree of the :code:`Population` tree). [#]_

.. |JSONM| raw:: html

   <a href="https://fwslc.blogspot.com/2014/10/json-m.html" target="_blank"> JSON-M</a>

.. [#] Of course, the ability to represent the data this way does not
       in any way detract from the ability to represent the same data
       another way with which you are comfortable, such as XML or
       JSON. However, note while any data that can be represented in
       JSON can be stored in a YottaDB tree not all trees that YottaDB
       is capable of storing can be represented in JSON, or at least,
       may require some encoding (for example, see |JSONM|) - in order to be represented in JSON.

With this representation, the :code:`Population` tree can be represented as follows:

.. code-block:: none

    Population("Belgium")=1367000
    Population("Thailand")=8414000
    Population("USA")=325737000
    Population("USA",17900802)=3929326
    Population("USA",18000804)=5308483
    …
    Population("USA",20100401)=308745538

YottaDB has functions for applications to traverse trees in both breadth-first and depth-first order.

If the application designers now wish to enhance the application to add historical dates for capitals, the :code:`Capital("Thailand")` subtree might look like this (source: `The Four Capitals of Thailand <https://blogs.transparent.com/thai/the-four-capitals-of-thailand/>`_).

.. code-block:: none

   Capital("Thailand")="Bangkok"
   Capital("Thailand",1238,1378)="Sukhothai"
   Capital("Thailand",1350,1767)="Ayutthaya"
   Capital("Thailand",1767,1782)="Thonburi"
   Capital("Thailand",1782)="Bangkok"

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Variables vs. Subscripts vs. Values
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When viewed as :code:`["Capital","Belgium","Brussels"]` each component is a string, and in an abstract sense they are all conceptually the same. When viewed as :code:`Capital("Belgium")="Brussels"` differences become apparent:

- Variables are ASCII strings from 1 to 31 characters, the first of which is "%", or a letter from "A" through "Z" and "a" through "z". Subsequent characters are alphanumeric ("A" through "Z", "a" through "z", and "0" through "9"). Variable names are case-sensitive, and variables of a given type are always in ASCII order (i.e., "Capital" always precedes "Population").
- Subscripts are sequences of bytes from 0 bytes (the null or empty string, "") to 1048576 bytes (1MiB). When a subscript is a :ref:`canonical number <canonical-numbers>`, YottaDB internally converts it to, and stores it as, a number. When ordering subscripts:

  - Empty string subscripts precede all numeric subscripts. By default, YottaDB prohibits empty string subscripts for global variables but permits them for local variables (see :ref:`lcl-gbl-var`). *Note: YottaDB recommends against the practice of using empty string subscripts in applications.* [#]_
  - Numeric subscripts precede string subscripts. Numeric subscripts are in numeric order.
  - String subscripts follow numeric subscripts and collate in byte order. Where the natural byte order does not result in linguistically and culturally correct ordering of strings, YottaDB has a framework for an application to create and use custom collation routines.

.. [#] The YottaDB code base includes code for a historical null
       collation in which empty strings collate after numeric
       subscripts and before non-empty strings. This is supported
       **only** in M code for backward compatibility reasons, and is
       not supported for use with C or any other language. Any attempt
       to bypass protections and use this historical null collation with new
       code will almost certainly result in buggy applications that
       are hard to debug.

Like subscripts, values are sequences of bytes, except that ordering of values is not meaningful unlike ordering of subscripts. YottaDB automatically converts between numbers and strings, depending on the type of operand required by an operator or argument required by a function (see :ref:`numeric-considerations`).

This means that if an application were to store the current capital of Thailand as :code:`Capital("Thailand","current")="Bangkok"` instead of :code:`Capital("Thailand")="Bangkok"`, the above subtree would have the following order:

.. code-block:: none

   Capital("Thailand",1238,1378)="Sukhothai"
   Capital("Thailand",1350,1767)="Ayutthaya"
   Capital("Thailand",1767,1782)="Thonburi"
   Capital("Thailand",1782)="Bangkok"
   Capital("Thailand","current")="Bangkok"

.. _lcl-gbl-var:

+++++++++++++++++++++++++++
Local and Global Variables
+++++++++++++++++++++++++++

YottaDB is a database, and data in a database must *persist* and *be shared*. The variables discussed above are specific to an application process (i.e., are not shared).

- *Local* variables reside in process memory, are specific to an application process, are not shared between processes, and do not persist beyond the lifetime of a process. [#]_
- *Global* variables reside in databases, are shared between processes, and persist beyond the lifetime of any individual process.

.. [#] In other words, what YottaDB calls a local variable, the C
       programming language calls a global variable. There is no C
       counterpart to a YottaDB global variable.

Syntactically, local and global variables look alike, with global variable names having a caret ("^") preceding their names. Unlike the local variables above, the global variables below are shared between processes and are persistent.

.. code-block:: none

    ^Population("Belgium")=1367000
    ^Population("Thailand")=8414000
    ^Population("USA")=325737000

Even though they may appear superficially similar, a local variable is distinct from a global variable of the same name. Thus :code:`^X` can have the value 1 and :code:`X` can at the same time have the value :code:`"The quick brown fox jumps over the lazy dog."` For maintainability *YottaDB strongly recommends that applications use different names for local and global variables, except in the special case where a local variable is an in-process cached copy of a corresponding global variable.*

.. note::

   As global variables that start with :code:`^%Y` are used by the `%YGBLSTAT() <../ProgrammersGuide/utility.html#ygblstat-util>`_ utility program, and global variables that start with :code:`^%y` are reserved for use by YottaDB, applications should not use them.

+++++++++++++++++++
Global Directories
+++++++++++++++++++

To application software, files in a file system provide persistence. This means that global variables must be stored in files for persistence. A *global directory file* provides a process with a mapping from the name of every possible global variable name to one or more *regions*. A *database* is a set of regions, which in turn map to *database files*. Global directories are created and maintained by a utility program, which is discussed at length in `Chapter 4 Global Directory Editor of the YottaDB Administration and Operations Guide <../AdminOpsGuide/gde.html>`_ and is outside the purview of this document.

The name of the global directory file required to access a global variable such as :code:`^Capital`, is provided to the process at startup by the environment variable :code:`ydb_gbldir`.

In addition to the implicit global directory an application may wish to use alternate global directory names. For example, consider an application that wishes to provide an option to display names in other languages while defaulting to English. This can be accomplished by having different versions of the global variable :code:`^Capital` for different languages, and having a global directory for each language. A global variable such as :code:`^Population` would be mapped to the same database file for all languages, but a global variable such as :code:`^Capital` would be mapped to a database file with language-specific entries. So a default global directory :code:`Default.gld` mapping a :code:`^Capital` to a database file with English names can be specified in the environment variable :code:`ydb_gbldir` but a different global directory file, e.g., :code:`ThaiNames.gld` can have the same mapping for a global variable such as :code:`^Population` but a different database file for :code:`^Capital`. The intrinsic special variable :ref:`zgbldir-isv` can be set to a global directory name to change the mapping from one global directory to another.

Thus, we can have:

.. code-block:: none

   $zgbldir="ThaiNames.gld"
   ^Capital("Thailand")="กรุ่งเทพฯ"
   ^Capital("Thailand",1238,1378)="สุโขทัย"
   ^Capital("Thailand",1350,1767)="อยุธยา"
   ^Capital("Thailand",1767,1782)="ธนบุรี"
   ^Capital("Thailand",1782)="กรุ่งเทพฯ"

.. _client-server-op:

~~~~~~~~~~~~~~~~~~~~~~~~
Client/Server Operation
~~~~~~~~~~~~~~~~~~~~~~~~

In common usage, database files reside on the same computer system as that running application code. However, as described in `Chapter 13 GT.CM Client/Server of the Administration and Operations Guide <../AdminOpsGuide/gtcm.html>`_, database files can reside on a computer system different from that running application code. This mapping of global variables to regions that map to remote files is also performed using global directories, and is transparent to application code except that YottaDB client/server operation does not support :ref:`txn-proc`.

Furthermore, there are configurations that impliticly invoke transaction processing logic, such as distributing a global variable over multiple database regions, or a trigger invocation (see `Chapter 14 Triggers of the YottaDB M Programmers Guide <../ProgrammersGuide/triggers.html>`_). Operations that invoke implicit transaction processing are not supported for global variables that reside on remote database files.

++++++++++++++++++++++++++++
Intrinsic Special Variables
++++++++++++++++++++++++++++

In addition to local and global variables, YottaDB also has a set of *Intrinsic Special Variables*. Just as global variables are distinguished by a "^" prefix, intrinsic special variables are distinguished by a "$" prefix.  Unlike local and global variable names, intrinsic special variable names are case-insensitive and so :code:`$zgbldir` and :code:`$ZGblDir` refer to the same intrinsic special variable. Intrinsic special variables have no subscripts.

While the majority of intrinsic special variables as enumerated in `Chapter 8 (Intrinsic Special Variables) of the YottaDB M Programmers Guide <../ProgrammersGuide/isv.html>`_ are useful to M application code, others are more generally useful and documented here.

~~~~~~~~
$tlevel
~~~~~~~~

Application code can read the intrinsic special variable :code:`$tlevel` to determine whether it is executing inside a transaction. :code:`$tlevel>0` means that it is inside a transaction, and :code:`$tlevel>1` means that it is inside a nested transaction. Note that a transaction can be started explicitly, e.g., by calling :ref:`ydb-tp-s-st-fn`,or implicitly by a trigger resulting from a :ref:`ydb-delete-s-st-fn`, :ref:`ydb-set-s-st-fn`.

~~~~~~~~~~
$trestart
~~~~~~~~~~

Application code inside a transaction can read the intrinsic special variable :code:`$trestart` to determine how many times a transaction has been restarted. Although YottaDB recommends against accessing external resources within a transaction, logic that needs to access an external resource (e.g., to read data in a file), can use :code:`$trestart` to restrict that access to the first time it executes (:code:`$trestart=0`).

.. _zgbldir-isv:

~~~~~~~~~
$zgbldir
~~~~~~~~~

:code:`$zgbldir` is the name of the current global directory file; any global variable reference that does not explicitly specify a global directory uses $zgbldir. For example, an application can set an intrinsic special variable :code:`$zgbldir="ThaiNames.gld"` to use the :code:`ThaiNames.gld` mapping. At process startup, YottaDB initializes :code:`$zgbldir` from the environment variable value :code:`$ydb_gbldir`.

~~~~~~~~~~~~
$zmaxtptime
~~~~~~~~~~~~

:code:`$zmaxtptime` provides a limit in seconds for the time that a transaction can be open (see :ref:`txn-proc`). :code:`$zmaxtptime` is initialized at process startup from the environment variable :code:`ydb_maxtptime`, with values greater than 60 seconds truncated to 60 seconds. In the unlikely event that an application legitimately needs a timeout greater than 60 seconds, use :ref:`ydb-set-s-st-fn` to set it.

.. _zstatus-isv:

~~~~~~~~~
$zstatus
~~~~~~~~~

:code:`$zstatus` provides additional details of the last error. Application code can retrieve :code:`$zstatus` using :code:`ydb_get_s-st-fn`. :code:`$zstatus` consists of several comma-separated substrings.

- The first is an error number.
- The second is always :code:`"(SimpleAPI)"`.
- The remainder is more detailed information about the error, and may contain commas within.

Note that a race condition exists for a multi-threaded application: after a call that returns an error, it is possible for another call from a different thread to perturb the value of :code:`$zstatus`. Use the :ref:`errstr <errstr>` parameter discussed in :ref:`threads` to get the correct :code:`$zstatus` in a multi-threaded application.

.. _zyrelease-isv:

~~~~~~~~~~~
$zyrelease
~~~~~~~~~~~

:code:`$zyrelease` identifies the YottaDB release in use. It consists of four space separated pieces:

#. Always “YottaDB”.
#. The release number, which starts with “r” and is followed by two numbers separated by a period (“.”), e.g., “r1.30”. The first is a major release number and the second is a minor release number under the major release. Even minor release numbers indicate formally released software. Odd minor release numbers indicate software builds from “in flight” code under development, between releases.
#. The operating system. e.g., “Linux”.
#. The CPU architecture, e.g., “x86_64”.

.. _txn-proc:

+++++++++++++++++++++++
Transaction Processing
+++++++++++++++++++++++

YottaDB provides a mechanism for an application to implement `ACID (Atomic, Consistent, Isolated, Durable) transactions <https://en.wikipedia.org/wiki/ACID>`_, ensuring strict serialization of transactions, using `optimistic concurrency control <http://sites.fas.harvard.edu/~cs265/papers/kung-1981.pdf>`_.

Here is a simplified view [#]_ of YottaDB's implementation of optimistic concurrency control:

- Each database file header has a field of the next *transaction number* for updates in that database.
- The block header of each database block in a database file has the transaction number when that block was last updated.
- When a process is inside a transaction, it keeps track of every database block it has read, and the transaction number of that block when read. Other processes are free to update the database during this time.
- The process retains updates in its memory, without committing them to the database, so that its own logic sees the updates, but no other process does. As every block that the process wishes to write must also be read, tracking the transaction numbers of blocks read suffices to track them for blocks to be written.
- To commit a transaction, a process checks whether any block it has read has been updated since it was read. If none has, the process commits the transaction to the database, incrementing the file header fields of each updated database file for the next transaction.
- If even one block has been updated, the process discards its work, and starts over. If after three attempts, it is still unable to commit the transaction, it executes the transaction logic on the fourth attempt with updates by all other processes blocked so that the transaction at commit time will not encounter database changes made by other processes.

.. [#] At the high level at which optimistic concurrency control is
       described here, a single logical database update (which can
       span multiple blocks and even multiple regions) is a
       transaction that contains a single update.

In YottaDB's API for transaction processing, an application packages the logic for a transaction into a function, passing the function to the :ref:`ydb-tp-s-st-fn` functions. YottaDB then calls that function.

- If the function returns a :CODE:`YDB_OK`, YottaDB attempts to commit the transaction. If it is unable to commit as described above, or if the called function returns a :CODE:`YDB_TP_RESTART` return code, it calls the function again.
- If the function returns a :CODE:`YDB_TP_ROLLBACK`, :ref:`ydb-tp-s-st-fn` return to the caller with that return code after discarding the uncommitted database updates and releasing any locks acquired within the transaction.
- To protect applications against poorly coded transactions, if a transaction takes longer than the number of seconds specified by the intrinsic special variable :code:`$zmaxtptime`, YottaDB aborts the transaction and the :ref:`ydb-tp-s-st-fn` functions return the :CODE:`YDB_ERR_TPTIMEOUT` error.

.. note::

   If the transaction logic receives a :code:`YDB_TP_RESTART` or :code:`YDB_TP_ROLLBACK` from a YottaDB function that it calls, it *must* return that value to :ref:`ydb-tp-s-st-fn`. Failure to do so could result in application level data inconsistencies and hard to debug application code.

Sections :ref:`threads` and :ref:`threads-txn-proc` provide important information pertinent to transaction processing in a multi-threaded application.

~~~~~~~~~~~~~~~~~~~~
Nested Transactions
~~~~~~~~~~~~~~~~~~~~

YottaDB allows transactions to be nested. In other words, code executing within a transaction may itself call :ref:`ydb-tp-s-st-fn`. Although ACID properties are only meaningful at the outermost level, nested transactions are nevertheless useful. For example:

- Application logic can be programmed modularly. Logic that requires ACID properties can be coded as a transaction, without the need to determine whether or not the caller of that logic is itself within a transaction.
- That local variables can be saved, and restored on transaction restarts, provides useful functionality that nested transactions can exploit.

++++++
Locks
++++++

YottaDB locks are a fast, lightweight tool for multiple processes to coordinate their work. An analogy with the physical world may help to explain the functionality. When it is locked, the lock on a door prevents you from going through it. In contrast, a traffic light does not stop you from driving through a street intersection: it works because drivers by convention stop when their light is red and drive when it is green.

YottaDB locks are more akin to traffic lights than door locks. Each lock has a name: as lock names have the same syntax as local or global variable names, :code:`Population`, :code:`^Capital`, and :code:`^Capital("Thailand",1350,1767)` are all valid lock names. Features of YottaDB locks include:

- Locks are exclusive: one and only one process can acquire a lock with the resource name. For example, if process P1 acquires lock :code:`Population("USA")`, process P2 cannot simultaneously acquire that lock. However, P2 can acquire lock :code:`Population("Canada")` at the same time that process P1 acquires :code:`Population("USA")`.
- Locks are hierarchical: a process that has a lock at a higher level blocks locks at lower levels and vice versa. For example, if a process P0 must wait for processes P1, P2, … to complete, each of P1, P2, … can acquire lock :code:`Process(pid)`. P0's subsequent attempt to acquire lock :code:`Process` is blocked till processes P1, P2, … complete.
- Locks include counters: a process that acquires :code:`^Capital("Belgium")` can acquire that lock again, incrementing its count to 2. This simplifies application code logic: for example, a routine in application code that requires :code:`^Capital("Belgium")` can simply incrementally acquire that lock without needing to test whether a higher level routine has already acquired it. More importantly, when it completes its work, the routine can decrementally release the lock without concern for whether or not a higher level routine needs that lock. When the count goes from 1 to 0, the lock becomes available for acquisition by another process.
- Locks are robust: while normal process exit releases locks held by that process, if a process holding a lock exits abnormally without releasing it, another process that needs the lock, and finding it held by a non-existent process will automatically scavenge the lock.

Although YottaDB lock names are the same as local and global variable names, YottaDB imposes no connection between a lock name and the same variable name. By convention, and for application maintainability, it is good practice to use lock names associated with the variables to which application code requires exclusive access, e.g., use a lock called :code:`^Population` to protect or restrict access to a global variable called :code:`^Population`. [#]_

.. [#] Since a process always has exclusive access to its local
       variables, access to them never needs protection from a
       lock. So, it would be reasonable to use a lock :code:`Population`
       to restrict access to the global variable :code:`^Population`.

Since YottaDB lock acquisitions are always timed for languages other than M, it is not in principle possible for applications to `deadlock <https://en.wikipedia.org/wiki/Deadlock>`_ on YottaDB locks. Consequently defensive application code must always validate the return code of calls to acquire locks. As a practical matter, it is possible to set timeouts that are long enough that users may perceive applications to be hung.

Since YottaDB resources such as locks belong to a process rather than a thread within a process (see discussion under :ref:`threads`), design rules to avoid deadlocks (such as acquiring locks in a predefined order that all processes must respect) must be respected by all threads in a process (or for a language such as Go, by all Goroutines in a process).

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Locks and Transaction Processing
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

:ref:`txn-proc` and Locks solve overlapping though not congruent use cases. For example, consider application code to transfer $100 from a customer's savings account to that same customer's savings account, which would likely include the requirement that business transactions on an account must be serializable. This can be implemented by acquiring a lock on that customer (with an application coded so that other accesses to that customer are blocked till the lock is released) or by executing the transfer inside a YottaDB transaction (which provides ACID properties). Unless the application logic or data force pathological transaction restarts that cannot be eliminated or worked around, transaction processing's optimistic concurrency control typically results in better application throughput than the pessimistic concurrency control that locks imply.

In general, we recommend using either transaction processing or locks, and not mixing them. However, there may be business logic that requires the use of locks for some logic, but otherwise permits the use of transaction processing. If an application must mix them, the following rules apply:

- A lock that a process acquires prior to starting a transaction cannot be released inside the transaction. It can only be released after the transaction is committed or rolled back. Locks acquired inside a transaction can be released either inside the transaction, or after the transaction is committed or rolled back.

.. _prog-in-m:

================
Programming in M
================

YottaDB includes a complete implementation of the `M <https://en.wikipedia.org/wiki/MUMPS>`_ programming language (also known as MUMPS - see `The Heritage and Legacy of M (MUMPS) – and the Future of YottaDB <https://yottadb.com/heritage-legacy-m-mumps-future-yottadb/>`_) that mostly conforms to `ISO/IEC 11756:1999 <http://www.iso.ch/iso/en/CatalogueDetailPage.CatalogueDetail?CSNUMBER=29268&ICS1=35&ICS2=60&ICS3=&scopelist>`_. The `YottaDB M Programmers Guide <../ProgrammersGuide/index.html>`_ documents programming YottaDB in M and is not duplicated here.

YottaDB supports calling between M and C application code, as documented in `Chapter 11 (Integrating External Routines) of the M Programmers Guide <../ProgrammersGuide/extrout.html>`_.

