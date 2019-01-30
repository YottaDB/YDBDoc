.. header::
   YottaDB — Multi-Language Programmers Guide

.. footer::
   Page ###Page### of ###Total###

.. index:: Multi-language Programming Guide

=================================
Multi-Language Programmer's Guide
=================================
.. contents::
   :depth: 3

========
Overview
========

YottaDB is a multi-language NoSQL database. The daemonless database
engine resides in the address space of the process, and can be
accessed from any supported language. Functions in the supported
languages can call one another to the extent that such calling is
permitted by the Supported language implementations.

As C is the *lingua franca* of programming, the C API provides access
to the YottaDB engine from any language. As YottaDB adds standard APIs
for other languages, additional sections will be added to the
Programmers Guide.

===========
Quick Start
===========

Local Installation
==================

1. Install YottaDB.

- Create a temporary directory and change to it, e.g.: :code:`mkdir /tmp/tmp ; cd /tmp/tmp`
- Get the YottaDB installer: :code:`wget
  https://gitlab.com/YottaDB/DB/YDB/raw/master/sr_unix/ydbinstall.sh`
- Make it executable: :code:`chmod +x ydbinstall.sh`
- Run it with your choice of directory where you want it installed
  (omit the :code:`--verbose` option for less output): :code:`sudo
  ./ydbinstall.sh --utf8 default
  --verbose`.
  If you do not specify an installation directory with
  :code:`--installdir`, the script installs YottaDB in
  :code:`/usr/local/lib/yottadb/r###` where :code:`r###` is
  the release, e.g., :code:`r122`.

2. Set up the environment: :code:`source $(pkg-config --variable=prefix yottadb)/ydb_env_set`.
   This defaults to an environment in :code:`$HOME/.yottadb`; to use
   another directory, set the environment variable :code:`ydb_dir` to
   the desired directory.

#. Put your C program in the :code:`$ydb_dir` directory,
   :code:`#include` the file :code:`libyottadb.h`
   in your C program and compile it. As a sample program, you can
   download the `wordfreq.c
   <https://gitlab.com/YottaDB/DB/YDBTest/raw/master/simpleapi/inref/wordfreq.c>`_
   program, with a `reference input file
   <https://gitlab.com/YottaDB/DB/YDBTest/raw/master/simpleapi/outref/wordfreq_input.txt>`_
   and `corresponding reference output file
   <https://gitlab.com/YottaDB/DB/YDBTest/raw/master/simpleapi/outref/wordfreq_output.txt>`_
   and compile it with :code:`gcc $(pkg-config --libs --cflags yottadb) -o wordfreq wordfreq.c -lyottadb`.

#. Run your program and verify that the output matches the reference output. For example:

.. code-block:: bash

	$ cd $ydb_dir
	$ gcc $(pkg-config --libs --cflags yottadb) -o wordfreq wordfreq.c -lyottadb
	$ ./wordfreq <wordfreq_input.txt >wordfreq_output.tmp
	$ diff wordfreq_output.tmp wordfreq_output.txt
	$

Note that the :code:`wordfreq.c` program randomly uses local or
global variables (see `Local and Global Variables`_).

Docker Container
================

You must have at least `Docker
<https://www.docker.com/community-edition>`_ 17.05
installed. Pre-built images are available at `Docker Hub
<https://hub.docker.com/r/yottadb/>`_. The Docker image is built to
provide sane defaults to begin exploring YottaDB. It is not meant for
production usage.

To run a pre-built image: :code:`docker run --rm -it yottadb/yottadb`
to run the image but not persist any changes you make, and
:code:`docker run -it yottadb/yottadb` for persistent changes.

Volumes are supported by mounting the :code:`/data` directory. To
mount the local directory :code:`ydb-data` into the container to save
your database and routines locally and use them in the container, add
an appropriate command line parameter before the yottadb/yottadb
argument, e.g., :code:`docker run -it -v \`pwd\`/ydb-data:/data
yottadb/yottadb`

This creates a :code:`ydb-data` directory in your current working
directory. After the container is shutdown and removed, delete the
directory if you want to remove all data created in the YottaDB
container (such as your database and routines).

========
Concepts
========

Keys, Values, Nodes, Variables, and Subscripts
==============================================

The fundamental core data structure provided by YottaDB is *key-value
tuples*. For example, the following is a set of key value tuples:

::

    ["Capital","Belgium","Brussels"]
    ["Capital","Thailand","Bangkok"]
    ["Capital","USA","Washington, DC"]

Note that data in YottaDB is *always* ordered. [#]_ Even if you input
data out of order, YottaDB always stores them in order. In the
discussion below, data is therefore always shown in order. For
example, the data below may well have been loaded by country.

.. [#] The terms "collate", "order", and "sort" are equivalent.

Each of the above tuples is called a *node*. In an *n*-tuple, the
first *n*-1 items can be thought of as the *keys*, and the last item is
the *value* associated with the keys.

While YottaDB itself assigns no meaning to the data in each node, by
convention, application maintainability is improved by using
meaningful keys, for example:

::

    ["Capital","Belgium","Brussels"]
    ["Capital","Thailand","Bangkok"]
    ["Capital","USA","Washington, DC"]
    ["Population","Belgium",1367000]
    ["Population","Thailand",8414000]
    ["Population","USA",325737000]

As YottaDB assigns no inherent meaning to the keys or values, its key
value structure lends itself to implementing *Variety*. [#]_ For
example, if an application wishes to add historical census results
under "Population", the following is a perfectly valid set of tuples
(source: `United States Census
<https://en.wikipedia.org/wiki/United_States_Census>`_):

::

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

In the above, 17900802 represents August 2, 1790, and an application
would determine from the number of keys whether a node represents the
current population or historical census data.

.. [#] Variety is one of the *three "V"s* of "big data" — Velocity,
       Volume, and Variety. YottaDB handles all three very well.

In YottaDB, the first key is called a *variable*, and the remaining
keys are called *subscripts* allowing for a representation both
compact and familiar to a programmer, e.g.,
:code:`Capital("Belgium")="Brussels"`. The set of all nodes under a
variable is called a *tree* (so in the example, there are two trees,
one under :code:`Capital` and the other under :code:`Population`). The set of
all nodes under a variable and a leading set of its subscripts is
called a *subtree* (e.g., :code:`Population("USA")` is a subtree of the
:code:`Population` tree). [#]_

.. |JSONM| raw:: html

   <a href="https://fwslc.blogspot.com/2014/10/json-m.html" target="_blank"> JSON-M</a>

.. [#] Of course, the ability to represent the data this way does not
       in any way detract from the ability to represent the same data
       another way with which you are comfortable, such as XML or
       JSON. However, note while any data that can be represented in
       JSON can be stored in a YottaDB tree not all trees that YottaDB
       is capable of storing can be represented in JSON, or at least,
       may require some encoding (for example, see |JSONM|) - in order to be represented in JSON.

With this representation, the :code:`Population` tree can be represented as
follows:

::

    Population("Belgium")=1367000
    Population("Thailand")=8414000
    Population("USA")=325737000
    Population("USA",17900802)=3929326
    Population("USA",18000804)=5308483
    …
    Population("USA",20100401)=308745538

YottaDB has functions for applications to traverse trees in both
breadth-first and depth-first order.

If the application designers now wish to enhance the application to
add historical dates for capitals, the :code:`Capital("Thailand")` subtree
might look like this (source: `The Four Capitals of Thailand
<https://blogs.transparent.com/thai/the-four-capitals-of-thailand/>`_).

::

   Capital("Thailand")="Bangkok"
   Capital("Thailand",1238,1378)="Sukhothai"
   Capital("Thailand",1350,1767)="Ayutthaya"
   Capital("Thailand",1767,1782)="Thonburi"
   Capital("Thailand",1782)="Bangkok"

-----------------------------------
Variables vs. Subscripts vs. Values
-----------------------------------

When viewed as :code:`["Capital","Belgium","Brussels"]` each component is
a string, and in an abstract sense they are all conceptually the
same. When viewed as :code:`Capital("Belgium")="Brussels"` differences
become apparent:

- Variables are ASCII strings from 1 to 31 characters, the first of
  which is "%", or a letter from "A" through "Z" and "a" through
  "z". Subsequent characters are alphanumeric ("A" through "Z", "a"
  through "z", and "0" through "9"). Variable names are
  case-sensitive, and variables of a given type are always in ASCII
  order (i.e., "Capital" always precedes "Population").
- Subscripts are sequences of bytes from 0 bytes (the null or empty
  string, "") to 1048576 bytes (1MiB). When a subscript is a
  `canonical number`_, YottaDB internally converts it to, and stores
  it as, a number. When ordering subscripts:

  - Empty string subscripts precede all numeric subscripts. By
    default, YottaDB prohibits empty string subscripts for global
    variables but permits them for local variables (see `Local and
    Global Variables`_). *Note: YottaDB recommends against the
    practice of using empty string subscripts in applications.* [#]_
  - Numeric subscripts precede string subscripts. Numeric subscripts
    are in numeric order.
  - String subscripts follow numeric subscripts and collate in byte
    order. Where the natural byte order does not result in
    linguistically and culturally correct ordering of strings, YottaDB
    has a framework for an application to create and use custom
    collation routines.

.. [#] The YottaDB code base includes code for a legacy subscript
       collation in which empty strings collate after numeric
       subscripts and before non-empty strings. This is supported
       **only** in M code for backward compatibility reasons, and is
       not supported for use with C or any other language. Any attempt
       to bypass protections and use this legacy collation with new
       code will almost certainly result in buggy applications that
       are hard to debug.

Like subscripts, values are sequences of bytes, except that ordering
of values is not meaningful unlike ordering of subscripts. YottaDB
automatically converts between numbers and strings, depending on the
type of operand required by an operator or argument required by a
function (see `Numeric Considerations`_).

This means that if an application were to store the current capital of
Thailand as :code:`Capital("Thailand","current")="Bangkok"` instead of
:code:`Capital("Thailand")="Bangkok"`, the above subtree would have the
following order:

::

   Capital("Thailand",1238,1378)="Sukhothai"
   Capital("Thailand",1350,1767)="Ayutthaya"
   Capital("Thailand",1767,1782)="Thonburi"
   Capital("Thailand",1782)="Bangkok"
   Capital("Thailand","current")="Bangkok"

Local and Global Variables
==========================

YottaDB is a database, and data in a database must *persist* and *be
shared*. The variables discussed above are specific to an application
process (i.e., are not shared).

- *Local* variables reside in process memory, are specific to an
  application process, are not shared between processes, and do not
  persist beyond the lifetime of a process. [#]_
- *Global* variables reside in databases, are shared between
  processes, and persist beyond the lifetime of any individual
  process.

.. [#] In other words, what YottaDB calls a local variable, the C
       programming language calls a global variable. There is no C
       counterpart to a YottaDB global variable.

Syntactically, local and global variables look alike, with global
variable names having a caret ("^") preceding their names. Unlike the
local variables above, the global variables below are shared between
processes and are persistent.

::

    ^Population("Belgium")=1367000
    ^Population("Thailand")=8414000
    ^Population("USA")=325737000

Even though they may appear superficially similar, a local variable is
distinct from a global variable of the same name. Thus :code:`^X` can have
the value 1 and :code:`X` can at the same time have the value :code:`"The quick
brown fox jumps over the lazy dog."` For maintainability *YottaDB
strongly recommends that applications use different names for local
and global variables, except in the special case where a local
variable is an in-process cached copy of a corresponding global
variable.*

Global Directories
==================

To application software, files in a file system provide
persistence. This means that global variables must be stored in files
for persistence. A *global directory file* provides a process with a
mapping from the name of every possible global variable name to one or
more *regions*. A *database* is a set of regions, which in turn map to
*database files*. Global directories are created and maintained by a
utility program, which is discussed at length in `Chapter 4 Global
Directory Editor of the YottaDB Administration and Operations Guide
<https://docs.yottadb.com/AdminOpsGuide/gde.html>`_ and is outside the
purview of this document.

The name of the global directory file required to access a global
variable such as :code:`^Capital`, is provided to the process at startup
by the environment variable :code:`ydb_gbldir`.

In addition to the implicit global directory an application may wish
to use alternate global directory names. For example, consider an
application that wishes to provide an option to display names in other
languages while defaulting to English. This can be accomplished by
having different versions of the global variable :code:`^Capital` for
different languages, and having a global directory for each
language. A global variable such as :code:`^Population` would be
mapped to the same database file for all languages, but a global
variable such as :code:`^Capital` would be mapped to a database file
with language-specific entries. So a default global directory
:code:`Default.gld` mapping a :code:`^Capital` to a database file with
English names can be specified in the environment variable
:code:`ydb_gbldir` but a different global directory file, e.g.,
:code:`ThaiNames.gld` can have the same mapping for a global variable
such as :code:`^Population` but a different database file for
:code:`^Capital`. The `intrinsic special variable`_ :code:`$zgbldir`
can be set to a global directory name to change the mapping from one
global directory to another.

Thus, we can have:

::

   $zgbldir="ThaiNames.gld"
   ^Capital("Thailand")="กรุ่งเทพฯ"
   ^Capital("Thailand",1238,1378)="สุโขทัย"
   ^Capital("Thailand",1350,1767)="อยุธยา"
   ^Capital("Thailand",1767,1782)="ธนบุรี"
   ^Capital("Thailand",1782)="กรุ่งเทพฯ"

-----------------------
Client/Server Operation
-----------------------

In common usage, database files reside on the same computer system as
that running application code. However, as described in `Chapter 13
GT.CM Client/Server of the Administration and Operations Guide
<https://docs.yottadb.com/AdminOpsGuide/gtcm.html>`_, database files
can reside on a computer system different from that running
application code. This mapping of global variables to regions that map
to remote files is also performed using global directories, and is
transparent to application code except that YottaDB client/server
operation does not support `transaction processing`_.

Furthermore, there are configurations that impliticly invoke
transaction processing logic, such as distributing a global variable
over multiple database regions, or a trigger invocation (see `Chapter
14 Triggers of the YottaDB M Programmers Guide
<https://docs.yottadb.com/ProgrammersGuide/triggers.html>`_). Operations
that invoke implicit transaction processing are not supported for
global variables that reside on remote database files.

.. _intrinsic special variable:

Intrinsic Special Variables
===========================

In addition to local and global variables, YottaDB also has a set of
*Intrinsic Special Variables*. Just as global variables are
distinguished by a "^" prefix, intrinsic special variables are
distinguished by a "$" prefix.  Unlike local and global variable
names, intrinsic special variable names are case-insensitive and so
:code:`$zgbldir` and :code:`$ZGblDir` refer to the same intrinsic special
variable. Intrinsic special variables have no subscripts.

While the majority of intrinsic special variables as enumerated in
`Chapter 8 (Intrinsic Special Variables) of the YottaDB M Programmers
Guide <https://docs.yottadb.com/ProgrammersGuide/isv.html>`_ are
useful to M application code, others are more generally useful and
documented here.

-------
$tlevel
-------

Application code can read the intrinsic special variable
:code:`$tlevel` to determine whether it is executing inside a
transaction. :code:`$tlevel>0` means that it is inside a transaction,
and :code:`$tlevel>1` means that it is inside a nested
transaction. Note that a transaction can be started explicitly, e.g.,
by calling `ydb_tp_s()`_ or `ydb_tp_st()`_,or implicitly by a trigger
resulting from a `ydb_delete_s()`_, `ydb_delete_st()`_, `ydb_set_s()`_
or `ydb_set_st()`_.

---------
$trestart
---------

Application code inside a transaction can read the intrinsic special
variable :code:`$trestart` to determine how many times a transaction has
been restarted. Although YottaDB recommends against accessing external
resources within a transaction, logic that needs to access an external
resource (e.g., to read data in a file), can use :code:`$trestart` to
restrict that access to the first time it executes (:code:`$trestart=0`).

--------
$zgbldir
--------

:code:`$zgbldir` is the name of the current global directory file; any
global variable reference that does not explicitly specify a global
directory uses $zgbldir. For example, an application can set an
intrinsic special variable :code:`$zgbldir="ThaiNames.gld"` to use the
:code:`ThaiNames.gld` mapping. At process startup, YottaDB initializes
:code:`$zgbldir` from the environment variable value
:code:`$ydb_gbldir`.

-----------
$zmaxtptime
-----------

:code:`$zmaxtptime` provides a limit in seconds for the time that a
transaction can be open (see `Transaction
Processing`_). :code:`$zmaxtptime` is initialized at process startup from
the environment variable :code:`ydb_maxtptime`, with values greater than
60 seconds truncated to 60 seconds. In the unlikely event that an
application legitimately needs a timeout greater than 60 seconds, use
`ydb_set_s()`_ or `ydb_set_st()`_ to set it.

--------
$zstatus
--------

:code:`$zstatus` provides additional details of the last
error. Application code can retrieve :code:`$zstatus` using
`ydb_get_s()`_ or `ydb_get_st()`_. :code:`$zstatus` consists of
several comma-separated substrings.

- The first is an error number.
- The second is always :code:`"(SimpleAPI)"`.
- The remainder is more detailed information about the error, and may
  contain commas within.

Note that a race condition exists for a multi-threaded application:
after a call that returns an error, it is possible for another call
from a different thread to perturb the value of :code:`$zstatus`. Use
the `errstr`_ parameter discussed in `Threads`_ to get the correct
:code:`$zstatus` in a multi-threaded application.

----------
$zyrelease
----------

:code:`$zyrelease` identifies the YottaDB release in use. It consists
of four space separated pieces:

1. Always “YottaDB”.
#. The release number, which starts with “r” and is followed by two
   numbers separated by a period (“.”), e.g., “r1.24”. The first is a
   major release number and the second is a minor release number under
   the major release. Even minor release numbers indicate formally
   released software. Odd minor release numbers indicate software
   builds from “in flight” code under development, between releases.
#. The operating system. e.g., “Linux”.
#. The CPU architecture, e.g., “x86_64”.

.. _transaction:
.. _transaction processing:

Transaction Processing
======================

YottaDB provides a mechanism for an application to implement `ACID
(Atomic, Consistent, Isolated, Durable) transactions
<https://en.wikipedia.org/wiki/ACID>`_, ensuring strict serialization
of transactions, using `optimistic concurrency control
<http://sites.fas.harvard.edu/~cs265/papers/kung-1981.pdf>`_.

Here is a simplified view [#]_ of YottaDB's implementation of
optimistic concurrency control:

- Each database file header has a field of the next *transaction
  number* for updates in that database.
- The block header of each database block in a database file has the
  transaction number when that block was last updated.
- When a process is inside a transaction, it keeps track of every
  database block it has read, and the transaction number of that
  block when read. Other processes are free to update the database
  during this time.
- The process retains updates in its memory, without committing them
  to the database, so that its own logic sees the updates, but no
  other process does. As every block that the process wishes to write
  must also be read, tracking the transaction numbers of blocks read
  suffices to track them for blocks to be written.
- To commit a transaction, a process checks whether any block it has
  read has been updated since it was read. If none has, the process
  commits the transaction to the database, incrementing the file
  header fields of each updated database file for the next
  transaction.
- If even one block has been updated, the process discards its work,
  and starts over. If after three attempts, it is still unable to
  commit the transaction, it executes the transaction logic on the
  fourth attempt with updates by all other processes blocked so that
  the transaction at commit time will not encounter database changes
  made by other processes.

.. [#] At the high level at which optimistic concurrency control is
       described here, a single logical database update (which can
       span multiple blocks and even multiple regions) is a
       transaction that contains a single update.

In YottaDB's API for transaction processing, an application packages
the logic for a transaction into a function, passing the function to
the `ydb_tp_s()`_ or `ydb_tp_st()`_ functions. YottaDB then calls that
function.

- If the function returns a :CODE:`YDB_OK`, YottaDB attempts to commit
  the transaction. If it is unable to commit as described above, or if
  the called function returns a :CODE:`YDB_TP_RESTART` return code, it
  calls the function again.
- If the function returns a :CODE:`YDB_TP_ROLLBACK`, `ydb_tp_s()`_ or
  `ydb_tp_st()`_ return to the caller with that return code after
  discarding the uncommitted database updates and releasing any locks
  acquired within the transaction.
- To protect applications against poorly coded transactions, if a
  transaction takes longer than the number of seconds specified by the
  intrinsic special variable :code:`$zmaxtptime`, YottaDB aborts the
  transaction and the `ydb_tp_s()`_ or `ydb_tp_st()`_ functions return
  the :CODE:`YDB_ERR_TPTIMEOUT` error.

Sections `Threads`_ and `Threads and Transaction Processing`_ provide
important information pertinent to transaction processing in a
multi-threaded application.

-------------------
Nested Transactions
-------------------

YottaDB allows transactions to be nested. In other words, code
executing within a transaction may itself call `ydb_tp_s()`_ or
`ydb_tp_st()`_. Although ACID properties are only meaningful at the
outermost level, nested transactions are nevertheless useful. For
example:

- Application logic can be programmed modularly. Logic that requires
  ACID properties can be coded as a transaction, without the need to
  determine whether or not the caller of that logic is itself within a
  transaction.
- That local variables can be saved, and restored on transaction
  restarts, provides useful functionality that nested transactions can
  exploit.


Locks
=====

YottaDB locks are a fast, lightweight tool for multiple processes to
coordinate their work. An analogy with the physical world may help to
explain the functionality. When it is locked, the lock on a door
prevents you from going through it. In contrast, a traffic light does
not stop you from driving through a street intersection: it works
because drivers by convention stop when their light is red and drive
when it is green.

YottaDB locks are more akin to traffic lights than door locks. Each
lock has a name: as lock names have the same syntax as local or global
variable names, :code:`Population`, :code:`^Capital`, and
:code:`^Capital("Thailand",1350,1767)` are all valid lock
names. Features of YottaDB locks include:

- Locks are exclusive: one and only one process can acquire a lock
  with the resource name. For example, if process P1 acquires lock
  :code:`Population("USA")`, process P2 cannot simultaneously acquire
  that lock. However, P2 can acquire lock :code:`Population("Canada")`
  at the same time that process P1 acquires :code:`Population("USA")`.
- Locks are hierarchical: a process that has a lock at a higher level
  blocks locks at lower levels and vice versa. For example, if a
  process P0 must wait for processes P1, P2, … to complete, each of
  P1, P2, … can acquire lock :code:`Process(`\ *pid*\ :code:`)`. P0's
  subsequent attempt to acquire lock :code:`Process` is blocked till
  processes P1, P2, … complete.
- Locks include counters: a process that acquires
  :code:`^Capital("Belgium")` can acquire that lock again, incrementing
  its count to 2. This simplifies application code logic: for example,
  a routine in application code that requires :code:`^Capital("Belgium")`
  can simply incrementally acquire that lock without needing to test
  whether a higher level routine has already acquired it. More
  importantly, when it completes its work, the routine can
  decrementally release the lock without concern for whether or not a
  higher level routine needs that lock. When the count goes from 1 to
  0, the lock becomes available for acquisition by another process.
- Locks are robust: while normal process exit releases locks held by
  that process, if a process holding a lock exits abnormally without
  releasing it, another process that needs the lock, and finding it
  held by a non-existent process will automatically scavenge the lock.

Although YottaDB lock names are the same as local and global variable
names, YottaDB imposes no connection between a lock name and the same
variable name. By convention, and for application maintainability, it
is good practice to use lock names associated with the variables to
which application code requires exclusive access, e.g., use a lock
called :code:`^Population` to protect or restrict access to a global
variable called :code:`^Population`. [#]_

.. [#] Since a process always has exclusive access to its local
       variables, access to them never needs protection from a
       lock. So, it would be reasonable to use a lock :code:`Population`
       to restrict access to the global variable :code:`^Population`.

Since YottaDB lock acquisitions are always timed for languages other
than M, it is not in principle possible for applications to `deadlock
<https://en.wikipedia.org/wiki/Deadlock>`_ on YottaDB
locks. Consequently defensive application code must always validate
the return code of calls to acquire locks. As a practical matter, it
is possible to set timeouts that are long enough that users may
perceive applications to be hung.

Since YottaDB resources such as locks belong to a process rather than
a thread within a process (see discussion under `Threads`_), design
rules to avoid deadlocks (such as acquiring locks in a predefined
order that all processes must respect) must be respected by all
threads in a process (or for a language such as Go, by all Goroutines
in a process).

--------------------------------
Locks and Transaction Processing
--------------------------------

`Transaction Processing`_ and Locks solve overlapping though not
congruent use cases. For example, consider application code to
transfer $100 from a customer's savings account to that same
customer's savings account, which would likely include the requirement
that business transactions on an account must be serializable. This
can be implemented by acquiring a lock on that customer (with an
application coded so that other accesses to that customer are blocked
till the lock is released) or by executing the transfer inside a
YottaDB transaction (which provides ACID properties). Unless the
application logic or data force pathological transaction restarts that
cannot be eliminated or worked around, transaction processing's
optimistic concurrency control typically results in better application
throughput than the pessimistic concurrency control that locks imply.

In general, we recommend using either transaction processing or locks,
and not mixing them. However, there may be business logic that
requires the use of locks for some logic, but otherwise permits the
use of transaction processing. If an application must mix them, the
following rules apply:

- A lock that a process acquires prior to starting a transaction
  cannot be released inside the transaction - it can only be released
  after the transaction is committed or rolled back. Locks acquired
  inside a transaction can be released either inside the transaction,
  or after the transaction is committed or rolled back.

================
Programming in C
================

Symbolic Constants
==================

The :code:`libyottadb.h` file defines several symbolic constants, which are
one of the following types:

- Function Return Codes, which in turn are one of:

  + Normal Return Codes
  + Error Return Codes

- Limits
- Other

Symbolic constants all fit within the range of a C :code:`int`.

---------------------
Function Return Codes
---------------------

Return codes from calls to YottaDB are usually of type :code:`int` and
occasionally other types. Normal return codes are non-negative
(greater than or equal to zero); error return codes are negative.


Normal Return Codes
-------------------

Symbolic constants for normal return codes have :CODE:`YDB_` prefixes
other than :CODE:`YDB_ERR_`.

:CODE:`YDB_LOCK_TIMEOUT` — This return code from lock acquisition
functions indicates that the specified timeout was reached without
the requested locks being acquired.

:CODE:`YDB_OK` — This the standard return code of all functions following
successful execution.

:CODE:`YDB_TP_RESTART` — Return code to YottaDB from an application
function that implements a transaction to indicate that it wishes
YottaDB to restart the transaction, or by a YottaDB function invoked
within a transaction to its caller that the database engine has
detected that it will be unable to commit the transaction and will
need to restart. Application code designed to be executed within a
transaction should be written to recognize this return code and in
turn perform any cleanup required and return to the YottaDB
`ydb_tp_s() / ydb_tp_st()`_ invocation from which it was called. See
`Transaction Processing`_ for a discussion of restarts.

:CODE:`YDB_TP_ROLLBACK` — Return code to YottaDB from an application
function that implements a transaction, and in turn returned to the
caller indicating that the transaction was not committed.

.. _error return code:

.. _error return codes:

Error Return Codes
------------------

Symbolic constants for error codes returned by calls to YottaDB are
prefixed with :CODE:`YDB_ERR_` and are all less than zero. The
symbolic constants below are not a complete list of all error messages
that YottaDB functions can return — error return codes can indicate
system errors and database errors, not just application errors. A
process that receives a negative return code, including one not listed
here, can call `ydb_get_s() / ydb_get_st()`_ to get the value of
`$zstatus`_.

Error messages can be raised by the YottaDB runtime system or by the
underlying operating system.

- A full set of YottaDB error messages and numbers is in the `YottaDB
  Messages and Recovery Procedures Manual
  <https://docs.yottadb.com/MessageRecovery/>`_.
- Linux error messages are described in Linux documentation,
  e.g. `errno <https://linux.die.net/man/3/errno>`_.

Remember that the error codes returned by YottaDB functions are the
negated numeric values of the error codes above.

:CODE:`YDB_ERR_CALLINAFTEREXIT` – A YottaDB function was called after
:code:`ydb_exit()` was called.

:CODE:`YDB_ERR_FATALERROR1` – A fatal error occurred. The process is
generating a core dump and terminating. As a process cannot receive a
fatal error code, this error appears in the syslog.

:CODE:`YDB_ERR_FATALERROR2` – A fatal error occurred. The process is
terminating without generating a core dump. As a process cannot
receive a fatal error code, this error appears in the syslog.

:CODE:`YDB_ERR_GVUNDEF` — No value exists at a requested global variable
node.

:CODE:`YDB_ERR_INVNAMECOUNT` – A :code:`namecount` parameter has an invalid
value.

:CODE:`YDB_ERR_INSUFFSUBS` — A call to `ydb_node_next_s() /
ydb_node_next_st()`_ or `ydb_node_previous_s() /
ydb_node_previous_st()`_ did not provide enough parameters for the
return values.

.. _YDB_ERR_INVSTRLEN:

:CODE:`YDB_ERR_INVSTRLEN` — A buffer provided by the caller is not long
enough for a string to be returned, or the length of a string passed
as a parameter exceeds :CODE:`YDB_MAX_STR`. In the event the return code
is :CODE:`YDB_ERR_INVSTRLEN` and if :code:`*xyz` is a :code:`ydb_buffer_t`
structure whose :code:`xyz->len_alloc` indicates insufficient space, then
:code:`xyz->len_used` is set to the size required of a sufficiently large
buffer. In this case the :code:`len_used` field of a :code:`ydb_buffer_t`
structure is greater than the :code:`len_alloc` field, and the caller is
responsible for correcting the :code:`xyz->len_used` field.

:CODE:`YDB_ERR_INVSVN` — A special variable name provided by the caller
is invalid.

:CODE:`YDB_ERR_INVVARNAME` — A variable name provided by the caller is
invalid.

:CODE:`YDB_ERR_KEY2BIG` — The length of a global variable name and
subscripts exceeds the limit configured for the database region to
which it is mapped.

:CODE:`YDB_ERR_LVUNDEF` — No value exists at a requested local variable
node.

:CODE:`YDB_ERR_MAXNRSUBSCRIPTS` — The number of subscripts specified in
the call exceeds :CODE:`YDB_MAX_SUBS`.

:CODE:`YDB_ERR_MINNRSUBSCRIPTS` – The number of subscripts cannot be
negative.

:CODE:`YDB_ERR_NAMECOUNT2HI` – The number of variable names specified
to `ydb_delete_excl_s() / ydb_delete_excl_st()`_ or `ydb_tp_s() /
ydb_tp_st()`_ exceeded the :CODE:`YDB_MAX_NAMES`.

:CODE:`YDB_ERR_NODEEND` — In the event a call to `ydb_node_next_s() /
ydb_node_next_st()`_, `ydb_node_previous_s() /
ydb_node_previous_st()`_, `ydb_subscript_next_s() /
ydb_subscript_next_st()`_, or `ydb_subscript_previous_s() /
ydb_subscript_previous_st()`_ wish to report that there no further
nodes/subscripts in their traversals, they return this value.

:code:`YDB_NOTOK` – `ydb_file_name_to_id()`_ was called with a NULL
pointer to a filename.

:CODE:`YDB_ERR_NUMOFLOW` — A `ydb_incr_s() / ydb_incr_st()`_ operation
resulted in a numeric overflow.

:CODE:`YDB_ERR_PARAMINVALID` — A parameter provided by the caller is
invalid.

:CODE:`YDB_ERR_SIMPLEAPINEST` – An attempt was made to nest Simple API
calls, which cannot be nested.

:CODE:`YDB_ERR_SUBSARRAYNULL` – The :code:`subs_used` parameter of a function
is greater than zero, but the :code:`subsarray` parameter is a NULL
pointer.

:CODE:`YDB_ERR_SVNOSET` — the application inappropriately attempted to
modify the value of an intrinsic special variable such as an attempt
to modify :code:`$trestart` using `ydb_set_s() / ydb_set_st()`_.

:CODE:`YDB_ERR_TIME2LONG` – This return code indicates that a value
greater than :CODE:`YDB_MAX_TIME_NSEC` was specified for a time duration.

:CODE:`YDB_ERR_TPTIMEOUT` — This return code from `ydb_tp_s() /
ydb_tp_st()`_ indicates that the transaction took too long to commit.

:CODE:`YDB_ERR_UNIMPLOP` — An operation that is not supported for an
intrinsic special variable – of the `Simple API`_ functions only
`ydb_get_s() / ydb_get_st()`_ and `ydb_set_s() / ydb_set_st()`_ are
supported – was attempted on an intrinsic special variable.

:CODE:`YDB_ERR_VARNAME2LONG` – A variable name length exceeds YottaDB's
limit.

------
Limits
------

Symbolic constants for limits are prefixed with :CODE:`YDB_MAX_` or
:code:`YDB_MIN_`.

:CODE:`YDB_MAX_IDENT` — The maximum space in bytes required to store a
complete variable name, not including the preceding caret for a global
variable. Therefore, when allocating space for a string to hold a
global variable name, add 1 for the caret.

:CODE:`YDB_MAX_NAMES` – The maximum number of variable names that can
be passed to `ydb_delete_excl_s() / ydb_delete_excl_st()`_ or
`ydb_tp_s() / ydb_tp_st()`_.

:CODE:`YDB_MAX_STR` — The maximum length of a string (or blob) in
bytes. A caller to `ydb_get_s() / ydb_get_st()`_ whose
:code:`*ret_value` parameter provides a buffer of :CODE:`YDB_MAX_STR`
will never get a :CODE:`YDB_ERR_INVSTRLEN` error.

:CODE:`YDB_MAX_SUBS` — The maximum number of subscripts for a local or
global variable.

:CODE:`YDB_MAX_TIME_NSEC` — The maximum value in nanoseconds that an
application can instruct libyottab to wait, e.g., until the process is
able to acquire locks it needs before timing out, or for
`ydb_hiber_start()`_.

:code:`YDB_MAX_YDBERR` – The absolute (positive) value of any YottaDB
function error return code. If the absolute value of an error return
code is greater than :code:`YDB_MAX_YDBERR`, then it is an error code
from elsewhere, e.g., e.g. `errno
<https://linux.die.net/man/3/errno>`_. Also, see :code:`YDB_IS_YDBERR()`.

:code:`YDB_MIN_YDBERR` - The absolute (positive) value of any YottaDB
function error return code. If the absolute value of an error return
code is less than :code:`YDB_MIN_YDBERR`, then it is an error code
from elsewhere, e.g., e.g. `errno
<https://linux.die.net/man/3/errno>`_. Also, see :code:`YDB_IS_YDBERR()`.

--------
Severity
--------

Symbolic constants for the severities of message numbers in return
codes and :code:`$zstatus` are prefixed with :CODE:`YDB_SEVERITY_`.

:CODE:`YDB_SEVERITY_ERROR` – The number corresponds to an error from which the
process can recover.

:CODE:`YDB_SEVERITY_FATAL` – The number corresponds to an error that terminated
the process.

:CODE:`YDB_SEVERITY_INFORMATION` – The number corresponds to an informational
message.

:CODE:`YDB_SEVERITY_SUCCESS` – The number corresponds to the successful
completion of a requested operation.

:CODE:`YDB_SEVERITY_WARNING` – The number corresponds to a warning, i.e.,
it indicates a possible problem.

-----
Other
-----

Other symbolic constants have a prefix of :CODE:`YDB_`.

:CODE:`YDB_DEL_NODE` and :CODE:`YDB_DEL_TREE` — As values of the
:code:`deltype` parameter, these values indicate to `ydb_delete_s() /
ydb_delete_st()`_ whether to delete an entire subtree or just the node
at the root, leaving the subtree intact.

:code:`YDB_NOTTP` – As a value of the :code:`tptoken` parameter of the
`Simple API`_ multi-threaded functions – those ending in
:code:`_st()`, indicates that the caller is not within a
`transaction`_.


Data Structures & Type Definitions
==================================

:code:`ydb_buffer_t` is a descriptor for a string [#]_ value, and consists of
the following fields:

- :code:`buf_addr` — pointer to an :code:`unsigned char`, the starting
  address of a string.
- :code:`len_alloc` and :code:`len_used` — fields of type :code:`unsigned int` where:

  - :code:`len_alloc` is the number of bytes allocated to store the
    string,
  - :code:`len_used` is the length in bytes of the currently stored
    string, and
  - :code:`len_alloc` ≥ :code:`len_used` except when a `YDB_ERR_INVSTRLEN`_
    occurs.

.. [#] Strings in YottaDB are arbitrary sequences of bytes that are not
       null-terminated. Other languages may refer to them as binary
       data or blobs.

:code:`ydb_string_t` is a descriptor for a string provided for
compatibility with existing code, and consists of the following
fields:

- :code:`address` — pointer to an :code:`unsigned char`, the starting
  address of a string.
- :code:`length` — the length of the string starting at the :code:`address` field.

:code:`ydb_tpfnptr_t` is a pointer to a function which returns an
integer, with one parameter, a pointer to an arbitrary structure:

.. code-block:: C
		
	typedef int (*ydb_tpfnptr_t)(void *tpfnparm);

:code:`ydb_tp2fnptr_t` is a pointer to a function which returns an
integer, with three parameters, a :code:`tptoken`, a :code:`*errstr`
pointer, and a pointer to an arbitrary structure:

.. code-block:: C

	typedef int (*ydb_tp2fnptr_t)(uint64_t tptoken,
		ydb_buffer_t *errstr, void *tpfnparm)

Functions to implement transaction processing logic for
single-threaded applications are referenced by :code:`ydb_tpfnptr_t`
and functions to implement transaction processing logic for
multi-threaded applications are referenced by :code:`ydb_tp2fnptr_t`.

Macros
======

:code:`YDB_ASSERT(x)` – Conditionally include this macro in code for
debugging and testing purposes. If :code:`x` is non-zero, it prints an
error message on :code:`stderr` and generates a core file by calling
`ydb_fork_n_core()`_.

:code:`YDB_BUFFER_IS_SAME(buffer1, buffer2)` – Use this macro to test
whether the memory locations (strings) pointed to by two
:code:`ydb_buffer_t` structures have the same content, returning :CODE:`FALSE`
(0) if they differ and a non-zero value if the contents are identical.

:code:`YDB_COPY_BUFFER_TO_BUFFER(source, destination, done)` – Use this
macro to copy the memory locations (strings) pointed to by :code:`source`
to the memory locations pointed to by :code:`destination` and set:

- :code:`destination->len_used` to :code:`source->len_used`; and
- :code:`done` to :CODE:`TRUE` if :code:`destination->len_alloc` ≥
  :code:`source->len_used` and the underlying :code:`memcpy()`
  completed successfully, and :CODE:`FALSE` otherwise.

:code:`YDB_COPY_LITERAL_TO_BUFFER(literal, buffer, done)` - Use this macro
to copy a literal string to previously allocated memory referenced by
a :code:`ydb_buffer_t` structure (for example, to set an initial subscript
to sequence through nodes). It sets:

- :code:`buffer->len_used` to the size of the literal; and
- :code:`done` to :CODE:`TRUE` if :code:`buffer->len_alloc` ≥ the size of the
  literal excluding its terminating null byte and the underlying
  :code:`memcpy()` completed successfully, and :CODE:`FALSE` otherwise.

:code:`YDB_COPY_STRING_TO_BUFFER(string, buffer, done)` – Use this
macro to copy a null-terminated string to previously allocated memory
referenced by a :code:`ydb_buffer_t` structure. This macro requires
the code to also :code:`#include <string.h>`. It sets:

- :code:`buffer->len_used` to the size of the copied string; and
- :code:`done` to :CODE:`TRUE` if :code:`buffer->len_alloc` ≥ the size
  of the string to be copied and the underlying :code:`memcpy()`
  completed successfully, and :CODE:`FALSE` otherwise.

:code:`YDB_FREE_BUFFER(BUFFERP)` - Use this macro to free the buffer malloced using :code:`YDB_MALLOC_BUFFER`.

- `free()` call is used on :code:`BUFFERP->buf_addr`.

:code:`YDB_LITERAL_TO_BUFFER(literal, buffer)` – Use this macro to set
:code:a `ydb_buffer_t` structure to refer to a literal (such as a
:code:variable name). With :code:`literal` a string literal, and
:code::code:`buffer` a pointer to a :code:`ydb_buffer_t` structure,
:code:set:

- :code:`buffer->buf_addr` to the address of :code:`literal`; and
- :code:`buffer->len_used` and :code:`buffer->len_alloc` to the length of
  :code:`literal` excluding the terminating null byte.

:code:`YDB_IS_YDBERR(msgnum)` – returns TRUE if the absolute value of
:code:`msgnum` lies between :code:`YDB_MIN_YDBERR` and
:code:`YDB_MAX_YDBERR`.

:code:`YDB_MALLOC_BUFFER(BUFFERP,LEN)` - Use this macro to to allocate a buffer using :code:`malloc()`
of length LEN and assign it to an already allocated :code:`ydb_buffer_t` structure.

- :code:`BUFFERP->buf_addr` is set to the malloced buffer.

- :code:`BUFFERP->len_alloc` is set to the malloced length.

- :code:`BUFFERP->len_used` is set to 0.

:code:`YDB_SEVERITY(msgnum, severity)` – The `error return code`_ from a
function indicates both the nature of an error as well as its
severity. For message :code:`msgnum`, the variable :code:`severity` is set to
one of the :CODE:`YDB_SEVERITY_*` symbolic
constants. :code:`YDB_SEVERITY()` is only meaningful for `error return
codes`_ and not other numbers. Use  :code:`YDB_IS_YDBERR()` to
determine whether a return code is a YottaDB `error return code`_.

YottaDB functions are divided into:

- Simple API — a core set of functions that provides easy-to-use
  access to the major features of YottaDB.
- Comprehensive API — a more elaborate set of functions for
  specialized or optimized access to additional functionality within
  :code:`libyottadb.so` that YottaDB itself uses. The Comprehensive API is
  a project for the future.
- Utility Functions — Functions useful to a C application using
  YottaDB.

Simple API
==========

As all subscripts and node data passed to YottaDB using the Simple API
are strings, use the :code:`sprintf()` and :code:`atoi()/strtoul()` family of
functions to convert between numeric values and strings which are
`canonical numbers`_.

To allow the YottaDB Simple API functions to handle a variable tree
whose nodes have varying numbers of subscripts, the actual number of
subscripts is itself passed as a parameter. In the prototypes of
functions, parameters of the form:

- :code:`ydb_buffer_t *varname` refers to the name of a variable;
- :code:`int subs_used` and :code:`int *subs_used` refer to an actual number
  of subscripts; and
- :code:`ydb_buffer_t *subsarray` refers to an array of :code:`ydb_buffer_t`
  structures used to pass subscripts whose actual number is defined by
  :code:`subs_used` or :code:`*subs_used` parameters.

To pass an intrinsic special variable, or unsubscripted local or
global variable, :code:`subs_used` should be zero and :code:`*subsarray`
should be NULL.

**Caveat:** Specifying a :code:`subs_used` that exceeds the actual number
of parameters passed in :code:`*subsarray` will almost certainly result in
an unpleasant bug that is difficult to troubleshoot.

Functions specific to the YottaDB Simple API for single-threaded
applications end in :code:`_s()` and those for multi-threaded
applications end in :code:`_st()`, with the latter functions typically
differing from their counterparts of the former type with two
additional parameters, :code:`tptoken`, and :code:`errstr`. The
discussion in `Threads`_ provides more detailed information.

.. _ydb_data_s():
.. _ydb_data_st():

----------------------------
ydb_data_s() / ydb_data_st()
----------------------------

.. code-block:: C

	int ydb_data_s(ydb_buffer_t *varname,
		int subs_used,
		ydb_buffer_t *subsarray,
		unsigned int *ret_value);

	int ydb_data_st(uint64_t tptoken,
		ydb_buffer_t *errstr,
		ydb_buffer_t *varname,
		int subs_used,
		ydb_buffer_t *subsarray,
		unsigned int *ret_value);

In the location pointed to by :code:`ret_value`, :code:`ydb_data_s()`
and :code:`ydb_data_st()` return the
following information about the local or global variable node
identified by :code:`*varname`, :code:`subs_used` and :code:`*subsarray`.

- 0 — There is neither a value nor a subtree, i.e., it is undefined.
- 1 — There is a value, but no subtree
- 10 — There is no value, but there is a subtree.
- 11 — There are both a value and a subtree.

It is an error to call :code:`ydb_data_s()` or :code:`ydb_data_st()`
on an intrinsic special variable; doing so results in the
:CODE:`YDB_ERR_UNIMPLOP` error. :code:`ydb_data_s() / ydb_data_st()`
returns:

- :code:`YDB_OK`; or
- an `error return code`_.

The error :CODE:`YDB_ERR_PARAMINVALID` is returned when

- :code:`ret_value` is NULL
- :code:`len_alloc` < :code:`len_used` or the :code:`len_used` is non-zero and :code:`buf_addr` is NULL in at least one subscript, in :code:`subsarray`.

.. _ydb_delete_s():
.. _ydb_delete_st():

--------------------------------
ydb_delete_s() / ydb_delete_st()
--------------------------------

.. code-block:: C

	int ydb_delete_s(ydb_buffer_t *varname,
		int subs_used,
		ydb_buffer_t *subsarray,
		int deltype);

	int ydb_delete_st(uint64_t tptoken,
		ydb_buffer_t *errstr,
		ydb_buffer_t *varname,
		int subs_used,
		ydb_buffer_t *subsarray,
		int deltype);

Delete nodes in the local or global variable tree or subtree
specified. A value of :CODE:`YDB_DEL_NODE` or :CODE:`YDB_DEL_TREE` for
:code:`deltype` specifies whether to delete just the node at the root,
leaving the (sub)tree intact, or to delete the node as well as the
(sub)tree.

Intrinsic special variables cannot be deleted.

:code:`ydb_delete_s()` and :code:`ydb_delete_st()` return :CODE:`YDB_OK`, a :CODE:`YDB_ERR_UNIMPLOP` if
:code:`deltype` is neither :CODE:`YDB_DEL_NODE` nor :CODE:`YDB_DEL_TREE`, :CODE:`YDB_ERR_PARAMINVALID` is returned when
:code:`len_alloc` < :code:`len_used` or the :code:`len_used` is non-zero
and :code:`buf_addr` is NULL in at least one subscript in :code:`subsarray`,
or another `error return code`_.

- :CODE:`YDB_OK`;
- :CODE:`YDB_ERR_UNIMPLOP` if :code:`deltype` is neither
  :CODE:`YDB_DEL_NODE` nor :CODE:`YDB_DEL_TREE`; or
- another `error return code`_.

.. _ydb_delete_excl_s():
.. _ydb_delete_excl_st():

------------------------------------------
ydb_delete_excl_s() / ydb_delete_excl_st()
------------------------------------------

.. code-block:: C

	int ydb_delete_excl_s(int namecount,
		ydb_buffer_t *varnames);

	int ydb_delete_excl_st(uint64_t tptoken,
		ydb_buffer_t *errstr,
		int namecount, ydb_buffer_t *varnames);

:code:`ydb_delete_excl_s()` and :code:`ydb_delete_excl_st()` delete
the trees of all local variables except those in the :code:`*varnames`
array. It is an error for :code:`*varnames` to include a global or
intrinsic special variable.

In the special case where :code:`namecount` is zero,
:code:`ydb_delete_excl_s()` and :code:`ydb_delete_excl_st()` delete
all local variables.

If your application mixes M and non M code, and you wish to use
:code:`ydb_delete_excl_s()` to delete local variables that are aliases,
formal parameters, or actual parameters passed by reference, make sure
you understand what (sub)trees are being deleted. This warning does
not apply to applications that do not include M code.

:code:`ydb_delete_excl_s()` and :code:`ydb_delete_excl_st()`return :CODE:`YDB_OK`,
:CODE:`YDB_ERR_NAMECOUNT2HI` if more
than :CODE:`YDB_MAX_NAMES` are specified, or another `error return
code`_. :CODE:`YDB_ERR_PARAMINVALID`
is returned when :code:`len_alloc` < :code:`len_used` or the :code:`len_used` is non-zero
and :code:`buf_addr` is NULL in at least one variable name in "code:`varnames`.

Note that specifying a larger value for :code:`namecount` than the
number of variable names actually provided in :code:`*varnames`
can result in a buffer overflow.

.. _ydb_get_s():
.. _ydb_get_st():

--------------------------
ydb_get_s() / ydb_get_st()
--------------------------

.. code-block:: C

	int ydb_get_s(ydb_buffer_t *varname,
		int subs_used,
		ydb_buffer_t *subsarray,
		ydb_buffer_t *ret_value);

	int ydb_get_st(uint64_t tptoken,
		ydb_buffer_t *errstr,
		ydb_buffer_t *varname,
		int subs_used,
		ydb_buffer_t *subsarray,
		ydb_buffer_t *ret_value);

To the location pointed to by :code:`ret_value->buf_addr`,
:code:`ydb_get_s()` and :code:`ydb_get_st()` copy the value of the
specified node or intrinsic special variable, setting
:code:`ret_value->len_used` on both normal and error returns (the
latter case as long as the data exists). Return values are:

- :CODE:`YDB_OK` for a normal return;
- :CODE:`YDB_ERR_GVUNDEF`, :CODE:`YDB_ERR_INVSVN`, or :CODE:`YDB_ERR_LVUNDEF` as
  appropriate if no such variable or node exists;
- :CODE:`YDB_ERR_INVSTRLEN` if :code:`ret_value->len_alloc` is insufficient for
  the value at the node;
- :CODE:`YDB_ERR_PARAMINVALID` when :code:`ret_value` is NULL or
  :code:`ret_value->buf_addr` is NULL and the return value has a non-zero :code:`len_used`; or
  :code:`len_alloc` < :code:`len_used` or the :code:`len_used` is non-zero
  and :code:`buf_addr` is NULL in at least one subscript in :code:`subsarray`; or
- another applicable `error return code`_.

Notes:

- In the unlikely event an application wishes to know the length of
  the value at a node, but not access the data, it can call
  :code:`ydb_get_s()` or :code:`ydb_get_st()` and provide an output
  buffer (:code:`retvalue->len_alloc`) with a length of zero, since
  even in the case of a :CODE:`YDB_ERR_INVSTRLEN` error,
  :code:`retvalue->len_used` is set.
- Within a transaction implemented by `ydb_tp_s() / ydb_tp_st()`_
  application code observes stable data at global variable nodes
  because YottaDB `transaction processing`_ ensures ACID properties,
  restarting the transaction if a value changes.
- Outside a transaction, a global variable node can potentially be
  changed by another, concurrent, process between the time that a
  process calls `ydb_data_s() / ydb_data_st()`_ to ascertain the
  existence of the data and a subsequent call to `ydb_get_s() /
  ydb_get_st()`_ to get that data. A caller of `ydb_get_s() /
  ydb_get_st()`_ to access a global variable node should code in
  anticipation of a potential :CODE:`YDB_ERR_GVUNDEF`, unless it is
  known from application design that this cannot happen.

.. _ydb_incr_s():
.. _ydb_incr_st():

----------------------------
ydb_incr_s() / ydb_incr_st()
----------------------------

.. code-block:: C

	int ydb_incr_s(ydb_buffer_t *varname,
		int subs_used,
		ydb_buffer_t *subsarray,
		ydb_buffer_t *increment,
		ydb_buffer_t *ret_value);

	int ydb_incr_st(uint64_t tptoken,
		ydb_buffer_t *errstr,
		ydb_buffer_t *varname,
		int subs_used,
		ydb_buffer_t *subsarray,
		ydb_buffer_t *increment,
		ydb_buffer_t *ret_value);

:code:`ydb_incr_s()` and :code:`ydb_incr_st()` atomically:

- convert the value in the specified node to a number if it is not
  one already, using a zero value if the node does not exist;
- increment it by the value specified by :code:`*increment`, converting
  the value to a number if it is not a `canonical number`_, defaulting to
  1 if the parameter is NULL; and
- store the value as a canonical number in :code:`*ret_value`.

Return values:

- The normal return value is :CODE:`YDB_OK`.
- If the atomic increment results in a numeric overflow, the function
  returns a :CODE:`YDB_ERR_NUMOFLOW` error; in this case, the value in the
  node is untouched and that in :code:`*ret_value` is unreliable.
- :CODE:`YDB_ERR_INVSTRLEN` if :code:`ret_value->len_alloc` is
  insufficient for the result. As with `ydb_get_s() / ydb_get_st()`_,
  in this case :CODE:`ret_value->len_used` is set to the required
  length.
- Other errors return the corresponding `error return code`_.

Notes:

- Intrinsic special variables cannot be atomically incremented, and an
  attempt to do so returns the :CODE:`YDB_ERR_UNIMPLOP` error.
- The value of the empty string coerced to a numeric value is 0.

.. _ydb_lock_s():
.. _ydb_lock_st():

----------------------------
ydb_lock_s() / ydb_lock_st()
----------------------------

.. code-block:: C

	int ydb_lock_s(unsigned long long timeout_nsec,
		int namecount[,
		[ydb_buffer_t *varname,
		int subs_used,
		ydb_buffer_t *subsarray], ...]);

	int ydb_lock_st(uint64_t tptoken,
		ydb_buffer_t *errstr,
		unsigned long long timeout_nsec,
		int namecount[,
		[ydb_buffer_t *varname,
		int subs_used,
		ydb_buffer_t *subsarray], ...]);

:code:`namecount` is the number of variable names in the call.

Release any locks held by the process, and attempt to acquire all the
requested locks. Except in the case of an error, the release is
unconditional. On return, the function will have acquired all
requested locks or none of them. If no locks are requested
(:code:`namecount` is zero), the function releases all locks and
returns :CODE:`YDB_OK`.

:code:`timeout_nsec` specifies a time in nanoseconds that the function
waits to acquire the requested locks. If :code:`timeout_nsec` is zero,
the function makes exactly one attempt to acquire the locks

Return values:

- If all requested locks are successfully acquired, the function
  returns :code:`YDB_OK`.
- If it is not able to acquire all requested locks in the specified
  time, it acquires no locks, returning with a
  :code:`YDB_LOCK_TIMEOUT` return value.
- If the requested :code:`timeout_nsec` exceeds
  :code:`YDB_MAX_TIME_NSEC`, the function immediately returns
  :code:`YDB_ERR_TIME2LONG`.
- :CODE:`YDB_ERR_PARAMINVALID`

is returned when :code:`len_alloc` < :code:`len_used` or the :code:`len_used` is non-zero
and :code:`buf_addr` is NULL in at least one subscript in :code:`subsarray`.
- In other cases, the function returns an `error return code`_.

.. _ydb_lock_decr_s():
.. _ydb_lock_decr_st():

--------------------------------------
ydb_lock_decr_s() / ydb_lock_decr_st()
--------------------------------------

.. code-block:: C

	int ydb_lock_decr_s(ydb_buffer_t *varname,
		int subs_used,
		ydb_buffer_t *subsarray);

	int ydb_lock_decr_st(uint64_t tptoken,
		ydb_buffer_t *errstr,
		ydb_buffer_t *varname,
		int subs_used,
		ydb_buffer_t *subsarray);

Decrements the count of the specified lock held by the process. As
noted in the `Concepts`_ section, a lock whose count goes from 1 to 0
is released. A lock whose name is specified, but which the process
does not hold, is ignored.

As releasing a lock cannot fail, the function returns :CODE:`YDB_OK`,
unless there is an error such as an invalid name that results in the
return of an error code such as :CODE:`YDB_ERR_INVVARNAME`. Errors
result in an appropriate `error return code`_. :CODE:`YDB_ERR_PARAMINVALID`
is returned when :code:`len_alloc` < :code:`len_used` or the :code:`len_used` is non-zero
and :code:`buf_addr` is NULL in at least one subscript in :code:`subsarray`.

.. _ydb_lock_incr_s():
.. _ydb_lock_incr_st():

--------------------------------------
ydb_lock_incr_s() / ydb_lock_incr_st()
--------------------------------------

.. code-block:: C

	int ydb_lock_incr_s(unsigned long long timeout_nsec,
		ydb_buffer_t *varname,
		int subs_used,
		ydb_buffer_t *subsarray);

	int ydb_lock_incr_st(uint64_t tptoken,
		ydb_buffer_t *errstr,
		unsigned long long timeout_nsec,
		ydb_buffer_t *varname,
		int subs_used,
		ydb_buffer_t *subsarray);

Without releasing any locks held by the process, attempt to acquire
the requested lock incrementing it if already held.

:code:`timeout_nsec` specifies a time in nanoseconds that the function
waits to acquire the requested locks. If :code:`timeout_nsec` is zero,
the function makes exactly one attempt to acquire the locks

Return values:

- If all requested locks are successfully acquired, the function
  returns :code:`YDB_OK`.
- If it is not able to acquire all requested locks in the specified
  time, it acquires no locks, returning with a
  :code:`YDB_LOCK_TIMEOUT` return value.
- If the requested :code:`timeout_nsec` exceeds
  :code:`YDB_MAX_TIME_NSEC`, the function immediately returns
  :code:`YDB_ERR_TIME2LONG`.
- :CODE:`YDB_ERR_PARAMINVALID`

is returned when :code:`len_alloc` < :code:`len_used` or the :code:`len_used` is non-zero
and :code:`buf_addr` is NULL in at least one subscript in :code:`subsarray`.
- In other cases, the function returns an `error return code`_.

.. _ydb_node_next_s():
.. _ydb_node_next_st():

--------------------------------------
ydb_node_next_s() / ydb_node_next_st()
--------------------------------------

.. code-block:: C

	int ydb_node_next_s(ydb_buffer_t *varname,
		int subs_used,
		ydb_buffer_t *subsarray,
		int *ret_subs_used,
		ydb_buffer_t *ret_subsarray);

	int ydb_node_next_st(uint64_t tptoken,
		ydb_buffer_t *errstr,
		ydb_buffer_t *varname,
		int subs_used,
		ydb_buffer_t *subsarray,
		int *ret_subs_used,
		ydb_buffer_t *ret_subsarray);

:code:`ydb_node_next_s()` and :code:`ydb_node_next_st()` facilitate
depth-first traversal of a local or global variable tree. As the
number of subscripts can differ between the input node of the call and
the output node reported by the call :code:`*ret_subs_used` is an
input as well as an output parameter:

- On input, :code:`*ret_subs_used` specifies the number of elements
  allocated for returning the subscripts of the next node.
- On normal output (:code:`YDB_OK` return code),
  :code:`*ret_subs_used` contains the actual number of subscripts
  returned. See below for error return codes

Return values of :code:`ydb_node_next_s()` and
:code:`ydb_node_next_st()` are:

- :CODE:`YDB_OK` with the next node, if there is one, changing
  :code:`*ret_subs_used` and :code:`*ret_subsarray` parameters to those of the
  next node. If there is no next node (i.e., the input node is the
  last), :code:`*ret_subs_used` on output is :CODE:`YDB_NODE_END`.
- :CODE:`YDB_ERR_INSUFFSUBS` if :code:`*ret_subs_used` specifies
  insufficient parameters to return the subscript. In this case
  :code:`*ret_subs_used` reports the actual number of subscripts required.
- :CODE:`YDB_ERR_INVSTRLEN` if one of the :code:`ydb_buffer_t` structures
  pointed to by :code:`*ret_subsarray` does not have enough space for the
  subscript. In this case, :code:`*ret_subs_used` is the index into the
  :code:`*ret_subsarray` array with the error, and the :code:`len_used` field
  of that structure specifies the size required.
- :CODE:`YDB_ERR_NODEEND` to indicate that that there are no more
  nodes. In this case, :code:`*ret_subs_used` is unchanged.
- :CODE:`YDB_ERR_PARAMINVALID` if :code:`ret_subs_used` is NULL or :code:`ret_subsarray`
  is NULL or one of the :code:`ydb_buffer_t` structures pointed to by :code:`*ret_subsarray`
  has a NULL buf_addr. In the last case, :code:`*ret_subs_used` is the index into the
  :code:`*ret_subsarray` array with the NULL buf_addr.
- Another `error return code`_, in which case the application should
  consider the values of :code:`*ret_subs_used` and the :code:`*ret_subsarray`
  to be undefined.

.. _ydb_node_previous_s():
.. _ydb_node_previous_st():

----------------------------------------------
ydb_node_previous_s() / ydb_node_previous_st()
----------------------------------------------

.. code-block:: C

	int ydb_node_previous_s(ydb_buffer_t *varname,
		int subs_used,
		ydb_buffer_t *subsarray,
		int *ret_subs_used,
		ydb_buffer_t *ret_subsarray);

	int ydb_node_previous_st(uint64_t tptoken,
		ydb_buffer_t *errstr,
		ydb_buffer_t *varname,
		int subs_used,
		ydb_buffer_t *subsarray,
		int *ret_subs_used,
		ydb_buffer_t *ret_subsarray);

Analogous to `ydb_node_next_s() / ydb_node_next_st()`_,
:code:`ydb_node_previous_s()` and :code:`ydb_node_previous_st()`
facilitate reverse breadth-first traversal of a local or global
variable tree, except that :code:`ydb_node_previous_s()` and
:code:`ydb_node_previous_st()` search for and report the predecessor
node. Unlike `ydb_node_next_s() / ydb_node_next_st()`_,
:code:`*ret_subs_used` can be zero if the previous node is the
unsubscripted root.

Return values of :code:`ydb_node_previous_s()` and
:code:`ydb_node_previous_st()` are:

- :CODE:`YDB_OK` with the previous node, if there is one, changing
  :code:`*ret_subs_used` and :code:`*ret_subsarray` parameters to those of the
  previous node.
- :CODE:`YDB_ERR_INSUFFSUBS` if :code:`*ret_subs_used` specifies
  insufficient parameters to return the subscript. In this case
  :code:`*ret_subs_used` reports the actual number of subscripts required.
- :CODE:`YDB_ERR_INVSTRLEN` if one of the :code:`ydb_buffer_t` structures
  pointed to by :code:`*ret_subsarray` does not have enough space for the
  subscript. In this case, :code:`*ret_subs_used` is the index into the
  :code:`*ret_subsarray` array with the error, and the :code:`len_used` field
  of that structure specifies the size required.
- :CODE:`YDB_ERR_NODEEND` to indicate that that there are no more
  nodes. In this case, :code:`*ret_subs_used` is unchanged.
- :CODE:`YDB_ERR_PARAMINVALID` if :code:`ret_subs_used` is NULL or :code:`ret_subsarray`
  is NULL or one of the :code:`ydb_buffer_t` structures pointed to by :code:`*ret_subsarray`
  has a NULL buf_addr. In the last case, :code:`*ret_subs_used` is the index into the
  :code:`*ret_subsarray` array with the NULL buf_addr.
- Another `error return code`_, in which case the application should
  consider the values of :code:`*ret_subs_used` and the :code:`*ret_subsarray`
  to be undefined.

.. _ydb_set_s():
.. _ydb_set_st():

--------------------------
ydb_set_s() / ydb_set_st()
--------------------------

.. code-block:: C

	int ydb_set_s(ydb_buffer_t *varname,
		int subs_used,
		ydb_buffer_t *subsarray,
		ydb_buffer_t *value);

	int ydb_set_st(uint64_t tptoken,
		ydb_buffer_t *errstr,
		ydb_buffer_t *varname,
		int subs_used,
		ydb_buffer_t *subsarray,
		ydb_buffer_t *value);

:code:`ydb_set_s()` and :code:`ydb_set_st()` copy the
:code:`value->len_used` bytes at :code:`value->buf_addr` as the value
of the specified node or intrinsic special variable specified. A NULL
:code:`value` parameter is treated as equivalent to one that points to
a :code:`ydb_buffer_t` specifying an empty string. Return values are:

- :CODE:`YDB_OK` for a normal return;
- :CODE:`YDB_ERR_INVSVN` if no such intrinsic special variable exists;
- :CODE:`YDB_ERR_PARAMINVALID` when :code:`len_alloc` < :code:`len_used` or the :code:`len_used` is non-zero
  and :code:`buf_addr` is NULL in at least one subscript in :code:`subsarray` or :code:`increment`; or
- another applicable `error return code`_.

.. _ydb_str2zwr_s():
.. _ydb_str2zwr_st():

----------------------------------
ydb_str2zwr_s() / ydb_str2zwr_st()
----------------------------------

.. code-block:: C

	int ydb_str2zwr_s(ydb_buffer_t *str, ydb_buffer_t *zwr);

	int ydb_str2zwr_st(uint64_t tptoken,
		ydb_buffer_t *errstr,
		ydb_buffer_t *str, ydb_buffer_t *zwr);

In the buffer referenced by :code:`*zwr`, :code:`ydb_str2zwr_s()` and
:code:`ydb_str2zwr_st()` provide the `zwrite formatted`_ version of
the string pointed to by :code:`*str`, returning:

- :CODE:`YDB_OK`;
- :CODE:`YDB_ERR_INVSTRLEN` if the :code:`*zwr` buffer is not long enough;
- :CODE:`YDB_ERR_PARAMINVALID` if :code:`zwr` is NULL or :code:`zwr->buf_addr` is
  NULL and the return value has a non-zero :code:`len_used`; or
- another applicable `error return code`_.

.. _ydb_subscript_next_s():
.. _ydb_subscript_next_st():

------------------------------------------------
ydb_subscript_next_s() / ydb_subscript_next_st()
------------------------------------------------

.. code-block:: C

	int ydb_subscript_next_s(ydb_buffer_t *varname,
		int subs_used,
		ydb_buffer_t *subsarray,
		ydb_buffer_t *ret_value);

	int ydb_subscript_next_st(uint64_t tptoken,
		ydb_buffer_t *errstr,
		ydb_buffer_t *varname,
		int subs_used,
		ydb_buffer_t *subsarray,
		ydb_buffer_t *ret_value);

:code:`ydb_subscript_next_s()` and :code:`ydb_subscript_next_st()`
provide a primitive for implementing breadth-first traversal of a tree
by searching for the next subscript at the level specified by
:code:`subs_used`, i.e., the next subscript after the one referred to
by :code:`subsarray[subs_used-1].buf_addr`. A node need not exist at
the subscripted variable name provided as input to the function. If
:code:`subsarray[subs_used-1].len_used` is zero,
:code:`ret_value->buf_addr` points to first node at that level with a
subscript that is not the empty string. :code:`ydb_subscript_next_s()`
and :code:`ydb_subscript_next_st()` return:

- :code:`YDB_OK`, in which case :code:`ret_value->buf_addr` points to
  the value of that next subscript;
- :code:`YDB_ERR_NODEEND` when there are no more subscripts at that
  level, in which case :code:`*ret_value` is unchanged;
- :code:`YDB_ERR_PARAMINVALID` when

  - :code:`ret_value` is NULL; 
  - :code:`ret_value->buf_addr` is NULL and the return value has a
    non-zero :code:`len_used`; or
  - :code:`len_alloc` < :code:`len_used` or the :code:`len_used` is
    non-zero and :code:`buf_addr` is NULL in at least one subscript in
    :code:`subsarray`

- or another `error return code`_.

In the special case where :code:`subs_used` is zero, and the function
returns :code:`YDB_OK`, :code:`ret_value->buf_addr` points to the next
local or global variable name, with :code:`YDB_ERR_NODEEND` indicating
an end to the traversal.

.. _ydb_subscript_previous_s():
.. _ydb_subscript_previous_st():

--------------------------------------------------------
ydb_subscript_previous_s() / ydb_subscript_previous_st()
--------------------------------------------------------

.. code-block:: C

	int ydb_subscript_previous_s(ydb_buffer_t *varname,
		int subs_used,
		ydb_buffer_t *subsarray,
		ydb_buffer_t *ret_value);

	int ydb_subscript_previous_st(uint64_t tptoken,
		ydb_buffer_t *errstr,
		ydb_buffer_t *varname,
		int subs_used,
		ydb_buffer_t *subsarray,
		ydb_buffer_t *ret_value);

:code:`ydb_subscript_previous_s()` and
:code:`ydb_subscript_previous_st()` provide a primitive for implementing
reverse breadth-first traversal of a tree by searching for the
previous subscript at the level specified by :code:`subs_used`. i.e. the
subscript preceding the one referred to by
:code:`subsarray[subs_used-1].buf_addr`. A node need not exist at the
subscripted variable name provided as input to the function. If
:code:`subsarray[subs_used-1].len_used` is zero, :code:`ret_value->buf_addr`
points to last node at that level with a subscript that is not the
empty string. :code:`ydb_subscript_previous_s()` and
:code:`ydb_subscript_previous_st()` return:

- :code:`YDB_OK`, in which case :code:`ret_value->buf_addr` points to
  the value of that previous subscript;
- :code:`YDB_ERR_NODEEND` when there are no more subscripts at that
  level, in which case :code:`*ret_value` is unchanged;
- :code:`YDB_ERR_PARAMINVALID` when

  - :code:`ret_value` is NULL; 
  - :code:`ret_value->buf_addr` is NULL and the return value has a
    non-zero :code:`len_used`; or
  - :code:`len_alloc` < :code:`len_used` or the :code:`len_used` is
    non-zero and :code:`buf_addr` is NULL in at least one subscript in
    :code:`subsarray`

- or another `error return code`_.

In the special case where :code:`subs_used` is zero, and the function
returns :code:`YDB_OK`, :code:`ret_value->buf_addr` points to the
previous local or global variable name, with :code:`YDB_ERR_NODEEND`
indicating an end to the traversal.

.. _ydb_tp_s():
.. _ydb_tp_st():

------------------------
ydb_tp_s() / ydb_tp_st()
------------------------

.. code-block:: C

	int ydb_tp_s(ydb_tpfnptr_t tpfn,
		void *tpfnparm,
		const char *transid,
		int namecount,
		ydb_buffer_t *varnames);

	int ydb_tp_st(uint64_t tptoken,
		ydb_buffer_t *errstr,
		ydb_tp2fnptr_t tpfn,
		void *tpfnparm,
		const char *transid,
		int namecount,
		ydb_buffer_t *varnames);

:code:`ydb_tp_s()` and :code:`ydp_tp_st()` call the function
:code:referenced by :code:`tpfn` passing it `tpfnparm` as a
:code:parameter. Additionally, :code:`ydb_tp_st()` also generates a
:code:new :code:`tptoken` that it passes as a parameter to the
:code:function referenced by its :code:`tpfn` parameter.

As discussed under `Transaction Processing`_, a function implementing
transaction processing logic should use the intrinsic special variable
:code:`$trestart` to manage any externally visible action (which
YottaDB recommends against, but which may be unavoidable). The
function referenced by :code:`tpfn` should return one of the
following:

- :CODE:`YDB_OK` — application logic indicates that the transaction can
  be committed (the YottaDB engine may still decide that a restart is
  required to ensure ACID transaction properties) as discussed under
  `Transaction Processing`_.
- :CODE:`YDB_TP_RESTART`  — application logic indicates that the
  transaction should restart.
- :CODE:`YDB_TP_ROLLBACK` — application logic indicates that the
  transaction should not be committed.
- :CODE:`YDB_ERR_PARAMINVALID` when :code:`len_alloc` < :code:`len_used` or the :code:`len_used` is non-zero
  and :code:`buf_addr` is NULL in at least one variable name in :code:`varnames`.
- An `error return code`_ returned by a YottaDB function called by the
  function.

:code:`transid` is a string, up to the first 8 bytes of which are recorded
in the commit record of journal files for database regions
participating in the transaction. If not NULL or the empty string, a
case-insensitive value of :CODE:`"BA"` or :CODE:`"BATCH"` indicates that at
transaction commit, YottaDB need not ensure Durability (it always
ensures Atomicity, Consistency, and Isolation). Use of this value may
improve latency and throughput for those applications where an
alternative mechanism (such as a checkpoint) provides acceptable
Durability. If a transaction that is not flagged as :CODE:`"BATCH"`
follows one or more transactions so flagged, Durability of the later
transaction ensures Durability of the the earlier :CODE:`"BATCH"`
transaction(s).

If :code:`namecount>0`, :code:`varnames[i]` where :code:`0≤i<namecount` specifies
local variable names whose values are restored to their original
values when the transaction is restarted. In the special case where
:code:`namecount=1` and :code:`varnames[0]` provides the value :code:`"*"`, all
local variables are restored on a restart. It is an error for a
:code:`varnames` to include a global or intrinsic special variable.

A top level :code:`ydb_tp_s()` and :code:`ydb-tp_st()` can return:

- :code:`YDB_OK`;
- :CODE:`YDB_TP_ROLLBACK`;
- :CODE:`YDB_ERR_TPTIMEOUT` (see `Transaction Processing`_); or
- an `error return code`_, including :CODE:`YDB_ERR_NAMECOUNT2HI`.

A :code:`ydb_tp_s()` or :code:`ydb_tp_st()` call that is within
another transaction (i.e., a nested transaction) can also return
:CODE:`YDB_TP_RESTART` to its caller. [#]_

.. [#] An enclosing transaction can result not just from another
       :code:`ydb_tp_s()` or :code:`ydb_tp_st()` higher in the stack,
       but also (for single-threaded applications) from an M
       :code:`tstart` command as well as a database trigger resulting
       from a `ydb_delete_s() / ydb_delete_st()`_, or `ydb_set_s() /
       ydb_set_st()`_.

.. _ydb_zwr2str_s():
.. _ydb_zwr2str_st():

----------------------------------
ydb_zwr2str_s() / ydb_zwr2str_st()
----------------------------------

.. code-block:: C

	int ydb_zwr2str_s(ydb_buffer_t *zwr, ydb_buffer_t *str);

	int ydb_zwr2str_st(uint64_t tptoken,
		ydb_buffer_t *errstr,
		ydb_buffer_t *zwr, ydb_buffer_t *str);

In the buffer referenced by :code:`*str`, :code:`ydb_zwr2str_s()` and
:code:`ydb_zwr2str_st()` provide the
string described by the `zwrite formatted`_ string pointed to by
:code:`*zwr`, returning 

- :CODE:`YDB_OK` (with :code:`str->len_used` set to zero if the zwrite formatted string has an error);
- :CODE:`YDB_ERR_INVSTRLEN` error if the :code:`*str` buffer is not long enough;
- :CODE:`YDB_ERR_PARAMINVALID` either if the :code:`*str` buffer is NULL or the return value contains a
  non-zero :code:`len_used`  and the :code:`str->buf_addr` is NULL.

Comprehensive API
=================

The Comprehensive API is a project for the future.

Utility Functions
=================

Utility functions are functions that are not core to YottaDB
functionality, but which are useful to application code.

Utility functions whose names end in :code:`_t()` are for use by
multi-threaded applications, and those which do not are for
single-threaded applications. The discussion in `Threads`_ provides
more detailed information.

Functions such as `ydb_exit()`_, `ydb_fork_n_core()`_, and
`ydb_init()`_, which do not have separate variants for single- and
multi-threaded applications, are suitable for both.

See also the description of the :code:`ydb_ci_t()` and
:code:`ydb_cip_t()` functions in the `Programmers Guide
<https://docs.yottadb.com/ProgrammersGuide/extrout.html#call-in-interface>`_.

----------------
ydb_child_init()
----------------

YottaDB r1.22 and before required the use of a function :code:`ydb_child_init()`
immediately after a :code:`fork()` to avoid database damage and other possible
side-effects.

Effective YottaDB r1.24, this function is not needed. It gets automatically
invoked by YottaDB as needed. Any existing usages of this function in an application
can be safely removed assuming YottaDB r1.24 or later is in use.

.. _ydb_exit():

----------
ydb_exit()
----------

.. code-block:: C

	int ydb_exit(void)

When a caller no longer wishes to use YottaDB, a call to
:code:`ydb_exit()` cleans up the process
connection/access to all databases and cleans up its data
structures. Therafter, any attempt to call a YottaDB function produces
a :code:`YDB_ERR_CALLINAFTEREXIT` error.

Note that a typical application should not need to call
:code:`ydb_exit()`, but should instead just terminate with a call to
:code:`exit()` which will perform any cleanup needed by YottaDB.

.. _ydb_file_id_free():
.. _ydb_file_id_free_t():

-----------------------------------------
ydb_file_id_free() / ydb_file_id_free_t()
-----------------------------------------

.. code-block:: C

	int ydb_file_id_free(ydb_fileid_ptr_t fileid)

	int ydb_file_id_free_t(uint64_t tptoken,
		ydb_buffer_t *errstr, ydb_fileid_ptr_t fileid)

Releases the memory used by a :code:`fileid` structure previously
generated by `ydb_file_name_to_id()`_ or
`ydb_file_name_to_id_t()`_. Calling the function twice for the same
pointer, unless it has been returned a second time by a different
`ydb_file_name_to_id()`_ or `ydb_file_name_to_id_t()`_ is an
application error with undefined consequences.

.. _ydb_file_is_identical():
.. _ydb_file_is_identical_t():

---------------------------------------------------
ydb_file_is_identical() / ydb_file_is_identical_t()
---------------------------------------------------

.. code-block:: C

	int ydb_file_is_identical(ydb_fileid_ptr_t fileid1,
		ydb_fileid_ptr_t fileid2)

	int ydb_file_is_identical_t(uint64_t tptoken,
		ydb_buffer_t *errstr,
		ydb_fileid_ptr_t fileid1,
		ydb_fileid_ptr_t fileid2)

Given two pointers to :code:`fileid` structures (see
`ydb_file_name_to_id()`_ / `ydb_file_name_to_id_t()`_),
:code:`ydb_file_is_identical()` and :code:`ydb_file_is_identical_t()`
return YDB_OK if the two :code:`fileid` structures are the same file
and YDB_NOTOK otherwise.

.. _ydb_file_name_to_id():
.. _ydb_file_name_to_id_t():

-----------------------------------------------
ydb_file_name_to_id() / ydb_file_name_to_id_t()
-----------------------------------------------

.. code-block:: C

	int ydb_file_name_to_id(ydb_string_t *filename,
		ydb_fileid_ptr_t *fileid)

	int ydb_file_name_to_id_t(uint64_t tptoken,
		ydb_buffer_t *errstr,
		ydb_string_t *filename,
		ydb_fileid_ptr_t *fileid)

As a file is potentially reachable through different paths, and
application code may need to check whether two paths do indeed lead to
the same file, YottaDB provides a mechanism to do so. Provided with a
path to a file, YottaDB creates an internal structure called a
:code:`fileid` that uniquely identifies the file if such a structure
does not already exist for that file, and provides the caller with a
pointer to that structure. The layout and contents of the fileid
structure are opaque to the caller, which **must not** modify the
pointer or the structure it points to.

When the :code:`fileid` structure for a file is no longer needed, an
application should call `ydb_file_id_free()`_ or
`ydb_file_id_free_t()`_ to release the structure and avoid a memory
leak.

:code:`ydb_file_name_to_id()` and :code:`ydb_file_name_to_id_t()`
:code:return :code:`YDB_OK`, `YDB_NOTOK` if the :code:`filename` is
:code:NULL, or an `error return code`_.

.. _ydb_fork_n_core():

-----------------
ydb_fork_n_core()
-----------------

.. code-block:: C

	void ydb_fork_n_core(void)

A core is a snapshot of a process, to help debug application code, for
example to troubleshoot an out-of-design condition. When a process
executes :code:`ydb_fork_n_core()`, it
forks. The child process sends itself a signal to generate a core and
terminate. On termination of the child process, the parent process
continues execution. Note that depending on the nature of the
condition necessitating a core, an :code:`exit()` may well be the
right action for the parent process. An :code:`exit()` call will drive
YottaDB exit handlers to perform clean shutdown of databases and
devices the process has open.

The content, location, and naming of cores is managed by the operating
system – see :code:`man 5 core` for details. We recommend that you set
:code:`kernel.core_uses_pid` to 1 to make it easier to identify and
track cores. As cores will likely contain protected confidential
information, you *must* ensure appropriate configuration and
management of cores.

In a multi-threaded environment, only the thread that executes
:code:`ydb_fork_n_core()` or :code:`ydb_fork_n_core()` survives in the
child and is dumped.

.. _ydb_free():

----------
ydb_free()
----------

.. code-block:: C

	void ydb_free(void *ptr)


Releases memory previously allocated by `ydb_malloc()`_. Passing :code:`ydb_free()` 
a pointer not previously provided to the
application by `ydb_malloc()`_ can result in
unpredictable behavior. The signature of :code:`ydb_free()` matches
that of the POSIX :code:`free()` call.

Just like other SimpleAPI functions, :code:`ydb_free()` should not be used in
multiple threads in multi-threaded programs. (See the `Threads`_ section for details). However, the :CODE:`YDB_FREE_BUFFER` macro is safe
to use in multiple threads.

.. _ydb_hiber_start():

-----------------
ydb_hiber_start()
-----------------

.. code-block:: C

	int ydb_hiber_start(unsigned long long sleep_nsec)

The process or thread sleeps for the time in nanoseconds specified by
:code:`sleep_nsec`. If a value greater than :code:`YDB_MAX_TIME_NSEC`
is specified, :code:`ydb_hiber_start()`
immediately returns with a :code:`YDB_ERR_TIME2LONG` error; otherwise
they return :code:`YDB_OK` after the elapsed time.

.. _ydb_hiber_start_wait_any():

--------------------------
ydb_hiber_start_wait_any()
--------------------------

.. code-block:: C

	int ydb_hiber_start_wait_any(unsigned long long sleep_nsec)

The process or thread sleeps for the time in nanoseconds specified by
:code:`sleep_nsec` or until it receives a signal. If a value greater
than :code:`YDB_MAX_TIME_NSEC` is specified, :code:`ydb_hiber_start_wait_any()`
immediately returns with a
:code:`YDB_ERR_TIME2LONG` error; otherwise they return :code:`YDB_OK`
after the elapsed time or when the wait is terminated by a signal.

.. _ydb_init():

----------
ydb_init()
----------

.. code-block:: C

	int ydb_init(void)

:code:`ydb_init()` initializes the YottaDB
runtime environment. As YottaDB automatically initializes the runtime
on the first call to its API or first M code invocation, there is
usually no need to explicitly call :code:`ydb_init()`.
The exception is when an application wishes to
set its own signal handlers (see `Signals`_): :code:`ydb_init()`
sets signal handlers, and in case an application
wishes to set its own signal handlers for signals not used by YottaDB,
it can call :code:`ydb_init()` before setting
its signal handlers.

.. _ydb_malloc():

------------
ydb_malloc()
------------

.. code-block:: C

	void *ydb_malloc(size_t size)

With a signature matching that of the POSIX :code:`malloc()` call,
:code:`ydb_malloc()` returns an address to a block of memory of the
requested size, or NULL if it is unable to satisfy the request.
:code:`ydb_malloc()` uses a `buddy system
<https://en.wikipedia.org/wiki/Buddy_memory_allocation>`_, and
provides debugging functionality under the control of the environment
variable :code:`ydb_dbglvl` whose values are a mask as described in
`gtmdbglvl.h
<https://gitlab.com/YottaDB/DB/YDB/blob/master/sr_port/gtmdbglvl.h>`_.

Just like other SimpleAPI functions, :code:`ydb_malloc()` should not be used in
multiple threads in multi-threaded programs. (See the `Threads`_ section for details). However, the :CODE:`YDB_MALLOC_BUFFER` macro is safe
to use in multiple threads.

.. _ydb_message():
.. _ydb_message_t():

-------------------------------
ydb_message() / ydb_message_t()
-------------------------------

.. code-block:: C

	int ydb_message(int errnum, ydb_buffer_t *msg_buff)

	int ydb_message_t(uint64_t tptoken, ydb_buffer_t *errstr,
		int errnum, ydb_buffer_t *msg_buff)

The functions return the error message text template for the error
number specified by :code:`errnum`.

- If :code:`errnum` does not correspond to an error that YottaDB
  recognizes, the return the error :code:`YDB_ERR_UNKNOWNSYSERR`,
  leaving the structures referenced by :code:`msg_buff` unaltered.
- Otherwise, if the length of the text exceeds
  :code:`msg_buff->len_alloc` they return the error
  :code:`YDB_ERR_INVSTRLEN`. In this case :code:`msg_buff->len_used` is
  greater than :code:`msg_buff->len_alloc`.
- Otherwise, if :code:`msg_buff->buf_addr` is NULL, they return the
  error :code:`YDB_ERR_PARAMINVALID`.
- Otherwise, the copy the text to the buffer specified by
  :code:`msg_buff->buf_addr`, set :code:`msg_buff->len_used` to its
  length, and return :code:`YDB_OK`.

.. _ydb_stdout_stderr_adjust():
.. _ydb_stdout_stderr_adjust_t():

---------------------------------------------------------
ydb_stdout_stderr_adjust() / ydb_stdout_stderr_adjust_t()
---------------------------------------------------------

.. code-block:: C

	int ydb_stdout_stderr_adjust(void)

	int ydb_stdout_stderr_adjust_t(uint64 tptoken,
		ydb_buffer_t *errstr)

The functions check whether stdout (file descriptor 1) and stderr
(file descriptor 2) are the same file, and if so, route stderr writes
to stdout instead. This ensures that output appears in the order in
which it was written; otherwise owing to IO buffering, output can
appear in an order different from that in which it was
written. Application code which mixes C and M code, and which
explicitly redirects stdout or stderr (e.g., using :code:`dup2()`),
should call one of these functions as soon as possible after the
redirection. :code:`ydb_stdout_stderr_adjust()` and
:code:`ydb_stdout_stderr_adjust_t()` return :code:`YDB_OK`.

.. _ydb_thread_is_main():

--------------------
ydb_thread_is_main()
--------------------

.. code-block:: C

	int ydb_thread_is_main(void)

The functions return :code:`YDB_OK` if the thread is the main thread
of the process, and another value if the thread is not. YottaDB
recommends against application code that requires use of these
functions, which exist only to provide backward compatibility to a
specific application code base (see discussion under `Threads`_).

.. _ydb_timer_cancel():
.. _ydb_timer_cancel_t():

-----------------------------------------
ydb_timer_cancel() / ydb_timer_cancel_t()
-----------------------------------------

.. code-block:: C

	void ydb_timer_cancel(intptr_t timer_id)

	void ydb_timer_cancel_t(uint64_t tptoken,
		ydb_buffer_t *errstr, intptr_t timer_id)

Cancel a timer identified by :code:`timer_id` and previously started with
`ydb_timer_start()`_ or `ydb_timer_start_t()`_.

.. _ydb_timer_start():
.. _ydb_timer_start_t():

---------------------------------------
ydb_timer_start() / ydb_timer_start_t()
---------------------------------------

.. code-block:: C

	typedef void (*ydb_funcptr_retvoid_t)(intptr_t timer_id,
		unsigned int handler_data_len,
		char *handler_data);

	int ydb_timer_start(intptr_t timer_id,
		unsigned long long limit_nsec,
		ydb_funcptr_retvoid_t handler,
		unsigned int handler_data_len
		char *handler_data);

	int ydb_timer_start_t(uint64_t tptoken,
		ydb_buffer_t *errstr,
		intptr_t timer_id,
		unsigned long long limit_nsec,
		ydb_funcptr_retvoid_t handler,
		unsigned int handler_data_len
		char *handler_data);

Start a timer. Unless canceled, when the timer expires,
:code:`ydb_timer_start()` and :code:`ydb_timer_start_t()` invoke a
handler function, providing that function with input data.

:code:`timer_id` is an identifier for the the timer. It is the
responsibility of application code to ensure that :code:`timer_id` is
different from those of any other active / pending timers.

:code:`limit_nsec` is the minimum number of nanoseconds before the timer
expires and invokes the handler function. Owing to overhead and system
load, the actual time will almost always be greater than this value.

:code:`handler` is a pointer to the function to be called when the timer
expires.

:code:`handler_data` is a pointer to the data to be passed to :code:`handler`
and :code:`handler_data_len` is the length of the data at
:code:`*handler_data`. Note that the data it points to **must** be on the
heap rather than on the stack, as the stack frame may no longer be
valid when the timer expires.

If the requested :code:`timeout_nsec` exceeds
:code:`YDB_MAX_TIME_NSEC`, the functions return
:code:`YDB_ERR_TIME2LONG`; otherwise they return :code:`YDB_OK`.

=================
Programming in Go
=================

Programming YottaDB in the `Go language <https://golang.org/>`_ is
accomplished through a wrapper for `Simple API`_ threaded functions
that uses `cgo <https://golang.org/cmd/cgo/>`_ to provide a “yottadb”
package for access from Go application code. The wrapper must be
installed on a system after YottaDB is installed.

There are two Go APIs:

- `Go Easy API`_ aims to be a straighforward, easy-to-use API to access
  YottaDB without limiting the functionality of YottaDB. The `Go Easy
  API`_ consists of `Go Easy API Functions`_ that use standard Go data
  types and structures.
- `Go Simple API`_ aims to improve performance by reducing copying
  between Go and YottaDB heaps by defining structures :code:`BufferT`,
  :code:`BufferTArray`, and :code:`KeyT` which contain pointers to
  structures and data in the YottaDB heap. `Go Simple API`_
  functionality is provided by Go methods where a method can
  meaningfully be associated with a structure, and by Go functions
  where there is no meaningful association with a structure.

Except for `triggers
<https://docs.yottadb.com/ProgrammersGuide/triggers.html>`_, which are
written in M and which can exist in the same process as Go code
because they run in a special, isolated, environment, Go code and M
code cannot co-exist in the same processs.

As the Go language has important differences from C (for example, it
has structures with methods but lacks macros), below are Go-specific
sections of the `Quick Start`_, `Concepts`_, `Symbolic Constants`_,
`Data Structures & Type Definitions`_, `Simple API`_ and `Utility
Functions`_ sections above. The sections below that are specific to Go
are intended to supplement, but not subsume, their C counterparts.

Go application code *must not* directly use the YottaDB C API
structures and functions (those prefixed by :code:`C.` or described in
the C `Simple API`_ above) as such usage bypasses important controls,
but should instead use the structures, methods and functions exposed
by the YottaDB Go wrapper. :code:`C.` prefixed structures and
functions are mentioned only for clarity in documentation and brevity
of explanation. For example, :code:`C.ydb_buffer_t` is the C
:code:`ydb_buffer_t` structure defined in `Data Structures & Type
Definitions`_.

All subsections of the `Programming in Go` section are prefixed with
“Go” to ensure unique names for hyperlinking.

As Go implementations are inherently multi-threaded, where the C
`Simple API`_ provides separate functions for use in multi-threaded
applications, e.g., `ydb_get_s()`_ vs. `ydb_get_st()`_), the Go wrapper
wraps the function for use in multi-threaded applications. Also, as
Go is multi-threaded, calls include a `errstr`_ parameter to get the
correct `$zstatus`_ for each call.

Go Quick Start
==============

The YottaDB Go wrapper requires a minimum YottaDB version of r1.24 and
is tested with a minimum Go version of 1.6.2. If the Golang packages
on your operating system are older, and the Go wrapper does not work,
please obtain and install a newer Golang implementation.

The `Go Quick Start`_ assumes that YottaDB has already been installed
as described in the `Quick Start`_ section. After completing step 1
(*Installing YottaDB*), download the Go wrapper, install it and
test it.

.. code-block:: bash

	$ go get lang.yottadb.com/go/yottadb
	$ go build lang.yottadb.com/go/yottadb
	$ source $(pkg-config --variable=prefix yottadb)/ydb_env_set
	$ go get -t lang.yottadb.com/go/yottadb
	$ go test lang.yottadb.com/go/yottadb
	ok  	lang.yottadb.com/go/yottadb	0.194s
	$

There are a number of programs in the
:code:`go/src/lang.yottadb.com/go/yottadb` directory that you can
look at.

3. Put your GO program in a directory of your choice, e.g.,
   :code:`$ydb_dir` directory and change to that directory.
   As a sample program, you can download the wordfreq.go program [XYZ
   – provide actual URL for wordfreq.go program when ready], with a
   `reference input file
   <https://gitlab.com/YottaDB/DB/YDBTest/raw/master/simpleapi/outref/wordfreq_input.txt>`_
   and `corresponding reference output file
   <https://gitlab.com/YottaDB/DB/YDBTest/raw/master/simpleapi/outref/wordfreq_output.txt>`_.
   Compile it thus: :code:`go build wordfreq.go`.

#. Run your program and verify that the output matches the reference output. For example:

.. code-block:: bash

	$ cd $ydb_dir
	$ # XYZ instructions to compile wordfreq.go to executable
	$ ./wordfreq <wordfreq_input.txt >wordfreq_output_go.txt
	$ diff wordfreq_output_go.txt wordfreq_output.txt
	$

Note that the :code:`wordfreq.go` program randomly uses local or
global variables (see `Local and Global Variables`_).

Go Concepts
===========

As the YottaDB wrapper is distributed as a Go package, function calls
to YottaDB are prefixed in Go code with :code:`yottadb.` (e.g.,
application code to call the :code:`GetET()` function is written
:code:`yottadb.GetET(…)`.

------------------
Go Error Interface
------------------

YottaDB has a comprehensive set of error return codes. Each has a
unique number and a mnemonic. Thus, for example, to return an error
that a buffer allocated for a return value is not large enough,
YottaDB uses the INVSTRLEN error code, which has the numeric value
:code:`C.YDB_ERR_INVSTRLEN`. YottaDB attempts to maintain stability of
the numeric values and mnemonics from release to release, to ensure
applications remain compatible when the underlying YottaDB releases
are upgraded. While the Go :code:`error` interface provides for a call
to return an error as a string (with :code:`nil` for a successful
return), applications in other languages, such as C, expect a numeric
return value.

Where C application code calling YottaDB functions will check the
return code, and if it is not :code:`YDB_OK` access the intrinsic
special variable `$zstatus`_ for more detailed information, Go
application code calling YottaDB methods and functions will check the
:code:`error` interface to determine whether it is :code:`nil`. If it
is not, the code has a choice of examining the string which is the
`$zstatus`_ for the error or accessing the numeric value. This means
that Go application code will never see a :code:`C.YDB_OK` return.

The YottaDB Go :code:`error` interface has a structure and a method.

.. code-block:: go

    type YDBError struct {
        errcode         int       // The error value (e.g. C.YDB_ERR_DBFILERR)
        errmsg          string    // The error string – $zstatus
    }

    func (err *YDBError) Error() string {
	return err.errmsg
    }

A routine used to find the error return code is:

.. code-block:: go 

    func ErrorCode(err error) int {
	yerr, ok := err.(*YDBError)
	if ok {
	    rc := yerr.errcode
	    return rc
	}
	return -1
    }

Note that the :code:`errcode` is the definitive return from the
call. The :code:`Error()` function accesses `$zstatus`_, which could
potentially be altered by a subsequent call to YottaDB from another
Goroutine, as a consequence of the race condition noted in the
description of `$zstatus`_. For an application where the race
condition can potentially occur, the `errstr`_ parameter provides
access to the actual `$zstatus`_ for each call. Pass a :code:`nil`
parameter for `errstr`_ where the text is not needed.

In the documentation:

- Error codes specific to each function are noted. However, common
  errors can also be returned. For example, while the `BufferT
  ValStr()`_ method can return INVSTRLEN, it can also return errors
  from the YottaDB engine, e.g., GVUNDEF.
- An error name such as INVSTRLEN refers to the underlying error,
  whether application code references the numeric value or the string.

Go Symbolic Constants
=====================

For modules that use `cgo <https://golang.org/cmd/cgo/>`_ to pull-in
:code:`libyott adb.h` by specifying the path to the file, Go symbolic
constants are the C `Symbolic Constants`_ with each C symbolic
constant prefixed with :code:`C.`. For example, the numeric C error
return value :code:`YDB_ERR_INVSTRLEN` is :code:`C.YDB_ERR_INVSTRLEN`
in Go.

:code:`yottadb.NOTTP` as a value for parameter :code:`tptoken`
indicates to the invoked YottaDB method or function that the caller is
not inside a `transaction`_.

Go Easy API
===========

A global or local variable node, or an intrinsic special variable, is
specified using the construct :code:`varname string, subary
[]string`. For an intrinsic special variable, :code:`subary` must be
the null array, :code:`[]string{}`. For a global or local variable, a
null array for :code:`subary` refers to the root node, the entire
tree, or both, depending on the function and context.

As the `Go Easy API`_ involves more copying of data between the Go and
YottaDB runtime systems, it requires the CPU to perform a little more
work than the `Go Simple API`_ does. Whether or not this has a
measurable impact on performance depends on the application and
workload.

Strings (values and subscripts) in YottaDB are variable length, as is
the number of subscripts a local or global variable can have. The `Go
Simple API`_ requires application code to allocate memory for buffers,
raising errors when allocated memory (either size or number of
buffers) is insufficient. Requiring application code using the `Go
Easy API`_ to similarly allocate memory would be at odds with our goal
of having it “just work”.  Although YottaDB provides functionality to
*a priori* determine the length of a value in order to allocate
required memory, doing this for every call would adversely affect
performance. The `Go Easy API`_ therefore allocates buffers initially
of a size and number we believe to be reasonable. Whenever a result
exceeds its allocation and returns an error, YottaDB expands the
allocation transparently to the caller, and repeats the operation,
remembering the expanded size for future allocations in the process.

---------------------
Go Easy API Functions
---------------------

DataE()
----------

.. code-block:: go

	func yottadb.DataE(tptoken uint64, errstr *BufferT,
		varname string, subary []string) (uint32, error)

Matching `DataST()`_, :code:`DataE()` function wraps and returns the
result of `ydb_data_st()`_. In the event of an error, the return
value is unspecified.

DeleteE()
------------

.. code-block:: go

	yottadb.DeleteE(tptoken uint64,  errstr *BufferT,
		deltype int, varname string, subary []string) error

Matching `DeleteST()`_, :code:`DeleteE()` wraps `ydb_delete_st()`_ to
delete a local or global variable node or (sub)tree, with a value of
:code:`C.YDB_DEL_NODE` for :code:`deltype` specifying that only the
node should be deleted, leaving the (sub)tree untouched, and a value
of :code:`C.YDB_DEL_TREE` specifying that the node as well as the
(sub)tree are to be deleted.

DeleteExclE()
----------------

.. code-block:: go

	func yottadb.DeleteExclE(tptoken uint64,
		 errstr *BufferT, varnames []string) error

Matching `DeleteExclST()`_, :code:`DeleteExclE()` wraps
`ydb_delete_excl_st()`_ to delete all local variables except those
specified. In the event :code:`varnames` has no elements (i.e.,
:code:`[]string{}`), :code:`DeleteExclE()` deletes all local
variables.

In the event that the number of variable names in :code:`varnames`
exceeds :code:`C.YDB_MAX_NAMES`, the error return is
ERRNAMECOUNT2HI. Otherwise, if `ydb_delete_excl_st()`_ returns an
error, the function returns the error.

As M and Go application code cannot be mixed in the same process, the
warning in `ydb_delete_excl_s()`_ does not apply.

IncrE()
----------

.. code-block:: go

	func yottadb.IncrE(tptoken uint64, errstr *BufferT,
		incr, varname string, subary []string) (string, error)

Matching `IncrST()`_, :code:`IncrE()` wraps `ydb_incr_st()`_ to
atomically increment the referenced global or local variable node
coerced to a number with :code:`incr` coerced to a number, with the
result stored in the node and returned by the function.

- If `ydb_incr_st()`_ returns an error such as NUMOFLOW or INVSTRLEN,
  the function returns the error.
- Otherwise, it returns the incremented value of the node.

With a :code:`nil` value for :code:`incr`, the default increment
is 1. Note that the value of the empty string coerced to an integer is
zero.

LockDecrE()
--------------

.. code-block:: go

	func yottadb.LockDecrE(tptoken uint64, errstr *BufferT,
		varname string, subary []string) error

Matching `LockDecrST()`_ :code:`LockDecrE()` wraps
`ydb_lock_decr_st()`_ to decrement the count of the lock name
referenced, releasing it if the count goes to zero or ignoring the
invocation if the process does not hold the lock.

LockE()
----------

.. code-block:: go

	func yottadb.LockE(tptoken uint64, errstr *BufferT,
		timeoutNsec uint64, namesnsubs ... interface{}) error

Matching `LockST()`_, :code:`LockE()` releases all lock resources
currently held and then attempt to acquire the named lock resources
referenced. If no lock resources are specified, it simply releases all
lock resources currently held and returns.

:code:`interface{}` is a series of pairs of :code:`varname string` and
:code:`subary []string` parameters, where a null `subary` parameter
(:code;`[]string{}`) specifies the unsubscripted lock resource
name.

If lock resources are specified, upon return, the process will have
acquired all of the named lock resources or none of the named lock
resources.

- If :code:`timeoutNsec` exceeds :code:`C.YDB_MAX_TIME_NSEC`, the
  function returns with an error return of TIME2LONG.
- If the lock resource names exceeds the maximum number supported
  (currently eleven), the function returns a PARMOFLOW error.
- If :code:`namesnsubs` is not a series of alternating :code:`string`
  and :code:`[]string` parameters, the function returns the
  INVLNPAIRLIST error.
- If it is able to aquire the lock resource(s) within
  :code:`timeoutNsec` nanoseconds, the function returns holding the lock
  resource(s); otherwise it returns LOCKTIMEOUT. If :code:`timeoutNsec`
  is zero, the function makes exactly one attempt to acquire the lock
  resource(s).

LockIncrE()
--------------

.. code-block:: go

	func yottadb.LockIncrE(tptoken uint64, errstr *BufferT,
		timeoutNsec uint64,
		varname string, subary []string) error

Matching `LockIncrST()`_, :code:`LockIncrE()` wraps
`ydb_lock_incr_st()`_ to attempt to acquire the referenced lock
resource name without releasing any locks the process already holds.

- If the process already holds the named lock resource, the function
  increments its count and returns.
- If :code:`timeoutNsec` exceeds :code:`C.YDB_MAX_TIME_NSEC`, the
  function returns with an error return TIME2LONG.
- If it is able to aquire the lock resource within :code:`timeoutNsec`
  nanoseconds, it returns holding the lock, otherwise it returns
  LOCKTIMEOUT. If :code:`timeoutNsec` is zero, the function makes
  exactly one attempt to acquire the lock.

NodeNextE()
--------------

.. code-block:: go

	func yottadb.NodeNextE(tptoken uint64, errstr *BufferT,
		varname string, subary []string) ([]string, error)

Matching `NodeNextST()`_, :code:`NodeNextE()` wraps
`ydb_node_next_st()`_ to facilitate depth first traversal of a local or
global variable tree.

- If there is a next node, it returns the subscripts of that next
  node.
- If the node is the last in the tree, the function returns the NODEEND error.

NodePrevE()
--------------

.. code-block:: go

	func yottadb.NodePrevE(tptoken uint64, errstr *BufferT,
		varname string, subary []string) ([]string, error)

Matching `NodePrevST()`_, :code:`NodePrevE()` wraps
`ydb_node_previous_st()`_ to facilitate reverse depth first traversal
of a local or global variable tree.

- If there is a previous node, it returns the subscripts of that
  previous node; an empty string array if that previous node is the root.
- If the node is the first in the tree, the function returns the NODEEND error.

SetValE()
------------

.. code-block:: go

	func yottadb.SetValE(tptoken uint64, errstr *BufferT,
		value, varname string, subary []string) error

Matching `SetValST()`_, at the referenced local or global variable
node, or the intrinsic special variable, :code:`SetValE()` wraps
`ydb_set_st()`_ to set the value specified by :code:`value`.

SubNextE()
-------------

.. code-block:: go

	func yottadb.SubNextE(tptoken uint64, errstr *BufferT,
		varname string, subary []string) (string, error)

Matching `SubNextST()`_, :code:`SubNextE()` wraps
`ydb_subscript_next_st()`_ to facilitate breadth-first traversal of a
local or global variable sub-tree.

- At the level of the last subscript, if there is a next subscript
  with a node and/or a subtree, it returns that subscript.
- If there is no next node or subtree at that level of the subtree,
  the function returns the NODEEND error.

In the special case where :code:`subary` is the null array,
:code:`SubNextE()` returns the name of the next global or local
variable, and the NODEEND error if :code:`varname` is the last global
or local variable.

SubPrevE()
-------------

.. code-block:: go

	func yottadb.SubPrevE(tptoken uint64, errstr *BufferT,
		varname string, subary []string) (string, error)

Matching `SubPrevST()`_, :code:`SubPrevE()` wraps
`ydb_subscript_previous_st()`_ to facilitate reverse breadth-first
traversal of a local or global variable sub-tree.

- At the level of the last subscript, if there is a previous subscript
  with a node and/or a subtree, it returns that subscript.
- If there is no previous node or subtree at that level of the
  subtree, the function returns the NODEEND error.

In the special case where :code:`subary` is the null array
:code:`SubNextE()` returns the name of the previous global or local
variable, and the NODEEND error if :code:`varname` is the first global
or local variable.

TpE()
--------

.. code-block:: go

	func yottadb.TpE(tptoken uint64, errstr *BufferT,
		tpfn unsafe.Pointer, tpfnparm unsafe.Pointer,
		transid string, varnames []string) error

Matching `TpST()`_, :code:`TpE()` wraps :code:`ydb_tp_st()` to
implement `Transaction Processing`_. The :code:`varnames` array
elements are local variable names whose values should be saved, and
restored to their original values when the transaction restarts. If
there are no :code:`varnames` array elements, or a sole
:code:`varnames` element is the empty string, no local variables are
saved and restored; and if a sole :code:`varnames` element is "*" all
local variables are saved and restored.

See `TpST()`_ for a more detailed discussion of YottaDB Go
transaction processing.

TpE2()
---------

.. code-block:: go

	func yottadb.TpE2(tptoken uint64, errstr *BufferT,
		tpfn func(uint64, *BufferT) int32, transid string,
		varnames []string) error

Matching `TpST2()`_, :code:`TpE()` wraps :code:`ydb_tp_st()` to
implement `Transaction Processing`_. The difference between
:code:`TpE()` and :code:`TpE2()` is that the former uses C glue code
to pass a parameter to the function implementing transaction logic,
whereas the latter is a pure Go function call (which may be a
closure).

Refer to `TpST()`_ for a more detailed discussion of YottaDB Go
transaction processing.

ValE()
---------

.. code-block:: go

	func yottadb.GetE(tptoken uint64, errstr *BufferT,
		varname string, subary []string) (string, error)

Matching `ValST()`_, :code:`ValE()` wraps `ydb_get_st()`_ to return
the value at the referenced global or local variable node, or
intrinsic special variable.

- If `ydb_get_s()`_ returns an error such as GVUNDEF, INVSVN, LVUNDEF,
  the function returns the error.
- Otherwise, it returns the value at the node.

Go Simple API
=============

The Go Simple API consists of `Go Data Structures & Type
Definitions`_, `Go Simple API BufferT Methods`_, `Go Simple API
BufferTArray Methods`_, `Go Simple API KeyT Methods`_ and `Go Simple
API Functions`_. Each of them wraps a function in the C `Simple API`_
– refer to the descriptions of those functions for more detailed
information. The majority of the functionality is in `Go Simple API
KeyT Methods`_.

Note that there is no :code:`MessageST()`. Applications which need
that functionality should simply call `MessageE()`.

-------------------------------------
Go Data Structures & Type Definitions
-------------------------------------

The :code:`C.ydb_buffer_t` structure, which is the
:code:`ydb_buffer_t` structure described in `Data Structures & Type
Definitions`_ is used to pass values between Go application code and
YottaDB. The design pattern is that the :code:`ydb_buffer_t`
structures are in memory managed by YottaDB. Go structures contain
pointers to the YottaDB structures so that when the Go garbage
collector moves Go structures, the pointers they contain remain valid.

There are three structures for the interface between YottaDB and Go:
:code:`BufferT` for data, :code:`BufferTArray` for a list of
subscripts or a set of variable names, :code:`KeyT` for keys where a
key in turn consists of a variable or lock resource name and
subscripts, as discussed in `Concepts`_.

.. code-block:: go

	type BufferT struct {
		cbuft      *C.ydb_buffer_t // Pointer to C structure describing data
	}

	type BufferTArray struct {
		elemsAlloc uint            // Number of elements allocated in array
		elemsUsed  uint            // Number of elements in use
		cbuftarray *C.ydb_buffer_t // Pointer to start of array of C structures describing data
	}

	type KeyT struct {
		Varnm      BufferT         // Pointer to variable name struct
		SubAry     BufferTArray    // Pointer to subscript struct
	}

As these structures contain pointers to storage allocated by YottaDB,
allowing a structure to go out of scope without first driving its
:code:`Free()` method introduces a storage leak.  Where possible, use
the Golang :code:`defer` statement to automatically drive the
appropriate free methods when these blocks go out of scope.

For those fields in the structures described here that are not
directly accessible (because they start with lower case letters),
there are methods associated with their containing structures to
access and modify them.

Methods for each structure are classified as either `Go Simple API
Access Methods`_ or `Go Simple API`_ methods. `Go Simple API Access
Methods`_ are methods implemented in the Go wrapper for managing the
structures for data interchange. `Go Simple API`_ methods wrap
functionality exposed by the YottaDB API.

----------------------------
Go Simple API Access Methods
----------------------------

Go Simple API Access Methods for BufferT
----------------------------------------

BufferT Alloc()
..................

.. code-block:: go

	func (buffer *BufferT) Alloc(nBytes uint32)

Allocate:

- a buffer in YottaDB heap space of size :code:`nBytes`; and
- a :code:`C.ydb_buffer_t` structure, also in YottaDB heap space, with
  its :code:`buf_addr` referencing the buffer, its :code:`len_alloc`
  set to :code:`nBytes` and its :code:`len_used` set to zero.

Set :code:`cbuft` in the :code:`BufferT`
structure to reference the :code:`C.ydb_buffer_t` structure.

BufferT Dump()
.................

.. code-block:: go

	func (buffer *BufferT) Dump()

For debugging purposes, dump on stdout:

- :code:`cbuft` as a hexadecimal address;
- for the :code:`C.ydb_buffer_t` structure referenced by
  :code:`cbuft`:

  - :code:`buf_addr` as a hexadecimal address, and
  - :code:`len_alloc` and :code:`len_used` as integers; and

- at the address :code:`buf_addr`, the lower of :code:`len_used` or
  :code:`len_alloc` bytes in `zwrite format`_.

BufferT DumpToWriter()
.........................

.. code-block:: go

	func (buffer *BufferT) DumpToWriter(writer io.writer)

For debugging purposes, dump on :code:`writer`:

- :code:`cbuft` as a hexadecimal address;
- for the :code:`C.ydb_buffer_t` structure referenced by
  :code:`cbuft`:

  - :code:`buf_addr` as a hexadecimal address, and
  - :code:`len_alloc` and :code:`len_used` as integers; and

- at the address :code:`buf_addr`, the lower of :code:`len_used` or
  :code:`len_alloc` bytes in `zwrite format`_.

BufferT Free()
.................

.. code-block:: go

	func (buffer *BufferT) Free()

The inverse of the :code:`Alloc()` method: release the buffer in
YottaDB heap space referenced by the :code:`C.ydb_buffer_t` structure,
release the :code:`C.ydb_buffer_t`, and set :code:`cbuft` in the
:code:`BufferT` structure to :code:`nil`.

BufferT FromPtr()
....................

.. code-block:: go

	func (buffer *BufferT) BufferTFromPtr(errstr unsafe.Pointer)

Intended for use by functions implementing transaction logic, the
method sets :code:`cbuft` in the :code:`BufferT` structure to
:code:`errstr`.

Note: Modifying :code:`errstr`, or accessing memory it references may
lead to code that behaves unpredictably and is hard to debug. Always
“wrap” it using :code:`FromPtr()` and use the methods for the
:code:`BufferT` structure.

BufferT LenAlloc()
.....................

.. code-block:: go

	func (buffer *BufferT) LenAlloc(tptoken uint64,
		errstr *BufferT) (uint32, error)

- If the :code:`C.ydb_buffer_t` structure referenced by :code:`cbuft`
  has not yet been allocated, return the STRUCTNOTALLOCD error.
- Otherwise, return the :code:`len_alloc` field of the
  :code:`C.ydb_buffer_t` structure referenced by :code:`cbuft`.

BufferT LenUsed()
....................

.. code-block:: go

	func (buffer *BufferT) LenUsed(tptoken uint64,
		errstr *BufferT) (uint32, error)

- If the :code:`C.ydb_buffer_t` structure referenced by :code:`cbuft`
  has not yet been allocated, return the STRUCTNOTALLOCD error.
- If the :code:`len_used` field of the :code:`C.ydb_buffer_t`
  structure is greater than its :code:`len_alloc` field (owing to a
  prior INVSTRLEN error), return an INVSTRLEN error and the
  :code:`len_used` field of the :code:`C.ydb_buffer_t` structure
  referenced by :code:`cbuft`.
- Otherwise, return the :code:`len_used` field of the
  :code:`C.ydb_buffer_t` structure referenced by :code:`cbuft`.

BufferT SetLenUsed()
.......................

.. code-block:: go

	func (buffer *BufferT) SetLenUsed(tptoken uint64,
		errstr *BufferT, newLen uint32) error

Use this method to change the length of a used substring of the
contents of the buffer referenced by the :code:`buf_addr` field of the
referenced :code:`C.ydb_buffer_t`.

- If the :code:`C.ydb_buffer_t` structure referenced by :code:`cbuft`
  has not yet been allocated, return the STRUCTNOTALLOCD error.
- If :code:`newLen` is greater than the :code:`len_alloc` field of the
  referenced :code:`C.ydb_buffer_t`, make no changes and return with
  an error return of INVSTRLEN.
- Otherwise, set the :code:`len_used` field of the referenced
  :code:`C.ydb_buffer_t` to :code:`newLen`.

Note that even if :code:`newLen` is not greater than the value of
:code:`len_alloc`, setting a :code:`len_used` value greater than the
number of meaningful bytes in the buffer will likely lead to
hard-to-debug errors.

BufferT SetValBAry()
.......................

.. code-block:: go

	func (buffer *BufferT) SetValBAry(tptoken uint64,
		errstr *BufferT, val *[]byte) error

- If the :code:`C.ydb_buffer_t` structure referenced by :code:`cbuft`
  has not yet been allocated, return the STRUCTNOTALLOCD error.
- If the length of :code:`val` is greater than the :code:`len_alloc`
  field of the :code:`C.ydb_buffer_t` structure referenced by
  :code:`cbuft`, make no changes and return INVSTRLEN.
- Otherwise, copy the bytes of :code:`val` to the location referenced
  by the :code:`buf_addr` field of the :code:`C.ydbbuffer_t`
  structure, set the :code:`len_used` field to the length of
  :code:`val`.

BufferT SetValStr()
......................

.. code-block:: go

	func (buffer *BufferT) SetValStr(tptoken uint64,
		errstr *BufferT, val *string) error

- If the :code:`C.ydb_buffer_t` structure referenced by :code:`cbuft`
  has not yet been allocated, return the STRUCTNOTALLOCD error.
- If the length of :code:`val` is greater than the :code:`len_alloc`
  field of the :code:`C.ydb_buffer_t` structure referenced by
  :code:`cbuft`, make no changes and return INVSTRLEN.
- Otherwise, copy the bytes of :code:`val` to the location referenced
  by the :code:`buf_addr` field of the :code:`C.ydbbuffer_t`
  structure, set the :code:`len_used` field to the length of
  :code:`val`.

BufferT SetValStrLit()
.........................

.. code-block:: go

	func (buffer *BufferT) SetValStrLit(tptoken uint64,
		errstr *BufferT, val string) error

- If the :code:`C.ydb_buffer_t` structure referenced by :code:`cbuft`
  has not yet been allocated, return the STRUCTNOTALLOCD error.
- If the length of :code:`val` is greater than the :code:`len_alloc`
  field of the :code:`C.ydb_buffer_t` structure referenced by
  :code:`cbuft`, make no changes and return INVSTRLEN.
- Otherwise, copy the bytes of :code:`val` to the location referenced
  by the :code:`buf_addr` field of the :code:`C.ydbbuffer_t`
  structure, set the :code:`len_used` field to the length of
  :code:`val`.

BufferT ValBAry()
....................

.. code-block:: go

	func (buffer *BufferT) ValBAry(tptoken uint64, errstr *BufferT) (*[]byte, error)

- If the :code:`C.ydb_buffer_t` structure referenced by :code:`cbuft`
  has not yet been allocated, return the STRUCTNOTALLOCD error.
- If the :code:`len_used` field of the :code:`C.ydb_buffer_t` structure
  is greater than its :code:`len_alloc` field (owing to a prior
  INVSTRLEN error), return an INVSTRLEN error.
- Otherwise, return :code:`len_used` bytes of the buffer as a byte
  array.

BufferT ValStr()
...................

.. code-block:: go

	func (buffer *BufferT) ValStr(tptoken uint64, errstr *BufferT) (*string, error)

- If the :code:`C.ydb_buffer_t` structure referenced by :code:`cbuft`
  has not yet been allocated, return the STRUCTNOTALLOCD error.
- If the :code:`len_used` field of the :code:`C.ydb_buffer_t` structure
  is greater than its :code:`len_alloc` field (owing to a prior
  INVSTRLEN error), return an INVSTRLEN error.
- Otherwise, return :code:`len_used` bytes of the buffer as a string.

Go Simple API Access Methods for BufferTArray
---------------------------------------------

BufferTArray Alloc()
.......................

.. code-block:: go

	func (buftary *BufferTArray) Alloc(numBufs, nBytes uint32)

Allocate:

- :code:`numSubs` buffers in YottaDB heap space, each of of size
  :code:`bufSiz`; and
- an array of :code:`numSubs` :code:`C.ydb_buffer_t` structures, also
  in YottaDB heap space.

Set:

- In each :code:`C.ydb_buffer_t` structure:

  - :code:`buf_addr` to the address of a buffer;
  - :code:`len_alloc` to :code:`bufSiz`; and
  - :code:`len_used` to zero.

- In the :code:`BufferTArray` structure:

  - :code:`cbuftary` to reference the beginning of the :code:`C.ydb_buffer_t` array;
  - :code:`elemsAlloc` field to :code:`numSubs`; and
  - :code:`elemsUsed` is to zero.

BufferTArray Dump()
......................

.. code-block:: go

	func (buftary *BufferTArray) Dump()

For debugging purposes, dump on stdout:

- :code:`cbuftary` as a hexadecimal address;
- :code:`elemsAlloc` and :code:`elemsUsed` as integers;
- for each element of the smaller of :code:`elemsAlloc` and
  :code:`elemsUsed` elements of the :code:`C.ydb_buffer_t` array
  referenced by :code:`cbuftary`:

  - :code:`buf_addr` as a hexadecimal address, and
  - :code:`len_alloc` and :code:`len_used` as integers; and
  - the smaller of :code:`len_used` and :code:`len_alloc` bytes at the
    address :code:`buf_addr`, in `zwrite format`_.

BufferTArray DumpToWriter()
..............................

.. code-block:: go

	func (buftary *BufferTArray) DumpToWriter(writer io.writer)

For debugging purposes, dump on :code:`writer`:

- :code:`cbuftary` as a hexadecimal address;
- :code:`elemsAlloc` and :code:`elemsUsed` as integers;
- for each element of the smaller of :code:`elemsAlloc` and
  :code:`elemsUsed` elements of the :code:`C.ydb_buffer_t` array
  referenced by :code:`cbuftary`:

  - :code:`buf_addr` as a hexadecimal address, and
  - :code:`len_alloc` and :code:`len_used` as integers; and
  - the smaller of :code:`len_used` and :code:`len_alloc` bytes at the
    address :code:`buf_addr`, in `zwrite format`_.

BufferTArray ElemAlloc()
...........................

.. code-block:: go

	func (buftary *BufferTArray) ElemAlloc() uint32

- If the :code:`C.ydb_buffer_t` structures referenced by
  :code:`cbuftary` have not yet been allocated, return the
  STRUCTNOTALLOCD error.
- Otherwise, return the :code:`elemsAlloc` field.

BufferTArray ElemLenAlloc()
..............................

.. code-block:: go

	func (buftary *BufferTArray)
		ElemLenAlloc(tptoken uint64) uint32

- If the :code:`C.ydb_buffer_t` structures referenced by
  :code:`cbuftary` have not yet been allocated, return the
  STRUCTNOTALLOCD error.
- Otherwise, return the :code:`len_alloc` from the
  :code:`C.ydb_buffer_t` structures referenced by :code:`cbuftary`,
  all of which have the same value.

BufferTArray ElemLenUsed()
.............................

.. code-block:: go

	func (buftary *BufferTArray) ElemLenUsed(tptoken uint64,
		errstr *BufferT, idx uint32) (uint32, error)

- If the :code:`C.ydb_buffer_t` structures referenced by
  :code:`cbuftary` have not yet been allocated, return the
  STRUCTNOTALLOCD error.
- If :code:`idx` is greater than the :code:`elemsAlloc` of the
  :code:`BufferTArray` structure, return with an error return of
  INSUFFSUBS.
- Otherwise, return the :code:`len_used` field of the array element
  specifed by :code:`idx` of the :code:`C.ydb_buffer_t` array referenced
  by :code:`cbuftary`.

BufferTArray ElemUsed()
..........................

.. code-block:: go

	func (buftary *BufferTArray) ElemUsed() uint32

- If the :code:`C.ydb_buffer_t` structures referenced by
  :code:`cbuftary` have not yet been allocated, return the
  STRUCTNOTALLOCD error.
- Otherwise, return the value of the :code:`elemsUsed` field.

BufferTArray Free()
......................

.. code-block:: go

	func (buftary *BufferTArray) Free()

The inverse of the :code:`Alloc()` method: release the :code:`numSubs`
buffers and the :code:`C.ydb_buffer_t` array. Set :code:`cbuftary` to
:code:`nil`, and :code:`elemsAlloc` and :code:`elemsUsed` to zero.

BufferTArray SetElemLenUsed()
................................

.. code-block:: go

	func (buftary *BufferTArray)
		SetElemLenUsed(tptoken uint64, errstr *BufferT,
		idx, newLen uint32) error

Use this method to set the number of bytes in :code:`C.ydb_buffer_t`
structure referenced by :code:`cbuft` of the array element specified
by :code:`idx`, for example to change the length of a used substring
of the contents of the buffer referenced by the :code:`buf_addr` field
of the referenced :code:`C.ydb_buffer_t`.

- If the :code:`C.ydb_buffer_t` structures referenced by
  :code:`cbuftary` have not yet been allocated, return the
  STRUCTNOTALLOCD error.
- If :code:`idx` is greater than :code:`elemsAlloc`, make no changes
  and return an INSUFFSUBS error.
- If :code:`newLen` is greater than the :code:`len_alloc` field of the
  referenced :code:`C.ydb_buffer_t`, make no changes and return an
  INVSTRLEN error.
- Otherwise, set the :code:`len_used` field of the referenced
  :code:`C.ydb_buffer_t` to :code:`newLen`.

Note that even if :code:`newLen` is not greater than the value of
:code:`len_alloc`, using a :code:`len_used` value greater than the
number of meaningful bytes in the buffer will likely lead to
hard-to-debug errors.

BufferTArray SetElemUsed()
.............................

.. code-block:: go

	func (buftary *BufferTArray)
		SetElemUsed(tptoken uint64, errstr *BufferT,
		newUsed uint32) error

Use this method to set the current number of valid strings (subscripts
or variable names) in the :code:`BufferTArray`.

- If the :code:`C.ydb_buffer_t` structures referenced by
  :code:`cbuftary` have not yet been allocated, return the
  STRUCTNOTALLOCD error.
- If :code:`newUsed` is greater than :code:`elemsAlloc`, make no
  changes and return with an error return of
  INSUFFSUBS.
- Otherwise, set :code:`elemsUsed` to :code:`newUsed`.

Note that even if :code:`newUsed` is not greater than the value of
:code:`elemsAlloc`, using an :code:`elemsUsed` value greater than the
number of valid values in the array will likely lead to hard-to-debug
errors.

BufferTArray SetValBAry()
............................

.. code-block:: go

	func (buftary *BufferTArray) SetValBAry(tptoken uint64,
		errstr *BufferT, idx uint32, val *[]byte) error

- If the :code:`C.ydb_buffer_t` structures referenced by
  :code:`cbuftary` have not yet been allocated, return the
  STRUCTNOTALLOCD error.
- If :code:`idx` is greater than :code:`elemsAlloc` make no changes
  and return with an error return of INSUFFSUBS.
- Otherwise, if the length of :code:`val` is greater than the
  :code:`len_alloc` field of the array element specified by :code:`idx`,
  set the :code:`len_used` field of that array element to the required
  length, and return INVSTRLEN.
- Otherwise, copy the bytes of :code:`val` to the location referenced
  by the :code:`buf_addr` field of the :code:`C.ydb_buffer_t`
  structure referenced, set its :code:`len_used` field to the number
  of bytes copied.

BufferTArray SetValStr()
...........................

.. code-block:: go

	func (buftary *BufferTArray)
		SetValStr(tptoken uint64, errstr *BufferT,
		idx uint32, value *string) error

- If the :code:`C.ydb_buffer_t` structures referenced by
  :code:`cbuftary` have not yet been allocated, return the
  STRUCTNOTALLOCD error.
- If :code:`idx` is greater than :code:`elemsAlloc` make no changes
  and return with an error return of INSUFFSUBS.
- Otherwise, if the length of :code:`val` is greater than the
  :code:`len_alloc` field of the array element specified by :code:`idx`,
  set the :code:`len_used` field of that array element to the required
  length, and return INVSTRLEN.
- Otherwise, copy the bytes of :code:`val` to the location referenced
  by the :code:`buf_addr` field of the :code:`C.ydb_buffer_t`
  structure referenced, set its :code:`len_used` field to the number
  of bytes copied.

BufferTArray SetValStrLit()
..............................

.. code-block:: go

	func (buftary *BufferTArray)
		SetValStrLit(tptoken uint64, errstr *BufferT,
		idx uint32, value string) error

- If the :code:`C.ydb_buffer_t` structures referenced by
  :code:`cbuftary` have not yet been allocated, return the
  STRUCTNOTALLOCD error.
- If :code:`idx` is greater than :code:`elemsAlloc` make no changes
  and return with an error return of INSUFFSUBS.
- If the length of :code:`val` is greater than the :code:`len_alloc`
  field of the :code:`C.ydb_buffer_t` structure indexed by :code:`idx`
  and referenced by :code:`cbuft`, make no changes and return
  INVSTRLEN.
- Otherwise, copy the bytes of :code:`val` to the location referenced by
  the :code:`buf_addr` field of the referenced :code:`C.ydbbuffer_t`
  structure, set the :code:`len_used` field to the length of
  :code:`val`.

BufferTArray ValBAry()
.........................

.. code-block:: go

	func (buftary *BufferTArray)
		ValBAry(tptoken uint64, errstr *BufferT,
		idx uint32) (*[]byte, error)

- If the :code:`C.ydb_buffer_t` structures referenced by
  :code:`cbuftary` have not yet been allocated, return the
  STRUCTNOTALLOCD error.
- If :code:`idx` is greater than :code:`elemsAlloc`, return a zero
  length byte array and an error return of INSUFFSUBS.
- If the :code:`len_used` field of the :code:`C.ydb_buffer_t`
  structure specified by :code:`idx` is greater than its
  :code:`len_alloc` field (owing to a previous INVSTRLEN error),
  return a byte array containing the :code:`len_alloc` bytes at
  :code:`buf_addr` and an INVSTRLEN error.
- Otherwise, return a byte array containing the :code:`len_used` bytes
  at :code:`buf_addr`.

BufferTArray ValStr()
...........................

.. code-block:: go

	func (buftary *BufferTArray)
		ValStr(tptoken uint64, errstr *BufferT,
		idx uint32) (*string, error)

- If the :code:`C.ydb_buffer_t` structures referenced by
  :code:`cbuftary` have not yet been allocated, return the
  STRUCTNOTALLOCD error.
- If :code:`idx` is greater than :code:`elemsAlloc`, return a zero
  length string and an error return of INSUFFSUBS.
- If the :code:`len_used` field of the :code:`C.ydb_buffer_t`
  structure specified by :code:`idx` is greater than its
  :code:`len_alloc` field (owing to a previous INVSTRLEN error),
  return a string containing the :code:`len_alloc` bytes at
  :code:`buf_addr` and the INVSTRLEN error.
- Otherwise, return a string containing the :code:`len_used` bytes at
  :code:`buf_addr`.

Go Simple API Access Methods for KeyT
-------------------------------------

As the members of :code:`KeyT` are visible to Go programs (they start
with upper-case letters), and application code can call the
:code:`BufferT` methods on :code:`Varnm` and :code:`BufferTArray`
methods on :code:`SubAry`, the `KeyT Alloc()`_, `KeyT Dump()`_
and `KeyT Free()`_ methods are available for programming
convenience.

KeyT Alloc()
...............

.. code-block:: go

	func (key *KeyT) Alloc(varSiz, numSubs, subSiz uint32)

Invoke :code:`Varnm.Alloc(varSiz)` (see `BufferT Alloc()`_) and
:code:`SubAry.Alloc(numSubs, subSiz)` (see `BufferTArray
Alloc()`_).

KeyT Dump()
..............

.. code-block:: go

	func (key *KeyT) Dump()

Invoke :code:`Varnm.Dump()` (see `BufferT Dump()`_) and
:code:`SubAry.Dump()` (see `BufferTArray Dump()`_).

KeyT DumpToWriter()
......................

.. code-block:: go

	func (key *KeyT) DumpToWriter(writer io.writer)

Invoke :code:`Varnm.Dump()` (see `BufferT Dump()`_) and
:code:`SubAry.Dump()` (see `BufferTArray Dump()`_), sending the
output to :code:`writer`.

KeyT Free()
..............

.. code-block:: go

	func (key *KeyT) Free()

Invoke :code:`Varnm.Free()` (see `BufferT Free()`_) and
:code:`SubAry.Free()` (see `BufferTArray Free()`_).

-----------------------------
Go Simple API BufferT Methods
-----------------------------

Str2ZwrST()
--------------

.. code-block:: go

	func (buft *BufferT) Str2ZwrST(tptoken uint64,
		errstr *BufferT, zwr *BufferT) error

The method wraps `ydb_str2zwr_st()`_ to provide the string in `zwrite
format`_.

- If the :code:`C.ydb_buffer_t` structure referenced by :code:`cbuft`
  has not yet been allocated, return the STRUCTNOTALLOCD error.
- If :code:`len_alloc` is not large enough, set :code:`len_used` to
  the required length, and return an INVSTRLEN error. In this case,
  :code:`len_used` will be greater than :code:`len_alloc` until
  corrected by application code.
- Otherwise, set the buffer referenced by :code:`buf_addr` to the
  `zwrite format`_ string, and set :code:`len_used` to the length.

Zwr2StrST()
--------------

.. code-block:: go

	func (buft *BufferT) Zwr2StrST(tptoken uint64,
		errstr *BufferT, str *BufferT) error

This method wraps `ydb_zwr2str_st()`_ and is the inverse of `Str2ZwrST()`_.

- If the :code:`C.ydb_buffer_t` structure referenced by :code:`cbuft`
  has not yet been allocated, return the STRUCTNOTALLOCD error.
- If :code:`len_alloc` is not large enough, set :code:`len_used` to
  the required length, and return an INVSTRLEN error. In this case,
  :code:`len_used` will be greater than :code:`len_alloc` until
  corrected by application code.
- If :code:`str` has errors and is not in valid `zwrite format`_, set
  :code:`len_used` to zero, and return the error code returned by
  `ydb_zwr2str_s()`_ e.g., INVZWRITECHAR`.
- Otherwise, set the buffer referenced by :code:`buf_addr` to the
  unencoded string, set :code:`len_used` to the length.

Note that the length of a string in `zwrite format`_ is always greater
than or equal to the string in its original, unencoded format.

----------------------------------
Go Simple API BufferTArray Methods
----------------------------------

DeleteExclST()
-----------------

.. code-block:: go

	func (buftary *BufferTArray)
		DeleteExclST(tptoken uint64, errstr *BufferT) error

:code:`DeleteExclST()` wraps `ydb_delete_excl_st()`_ to delete all
local variable trees except those of local variables whose names are
specified in the :code:`BufferTArray` structure. In the special case
where :code:`elemsUsed` is zero, the method deletes all local variable
trees.

In the event that the :code:`elemsUsed` exceeds
:code:`C.YDB_MAX_NAMES`, the error return is ERRNAMECOUNT2HI.

As M and Go application code cannot be mixed in the same process, the
warning in `ydb_delete_excl_s()`_ does not apply.

TpST()
---------

.. code-block:: go

	func (buftary *BufferTArray) TpST(tptoken uint64,
		errstr *BufferT, tpfn unsafe.Pointer,
		tpfnparm unsafe.Pointer, transid *string) error

:code:`TpST()` wraps `ydb_tp_st()`_ to implement `Transaction
Processing`_. :code:`tpfn` is a pointer to a C function with three
parameters, the first of which is a :code:`tptoken`, second is a :code:`errstr`,
and the third of which is :code:`tpfnparm`, a pointer to an arbitrary data structure in
YottaDB heap space. Please see both the description of `ydb_tp_st()`_
and the sections on `Transaction Processing`_ and `Threads and
Transaction Processing`_ for details.

The second parameter, :code:`errstr`, is passed in as an :code:`unsafe.Pointer`; this should be
immediately passed to BufferT.BufferTFromPtr, and the BufferT object used here passed to
all YottaDB wrapper calls as :code:`errstr`.
The raw :code:`unsafe.Pointer` should not be used or modified.

Since Go does not permit a pointer to a Go function to be passed as a
parameter to a C function, :code:`tpfn` is required to be a pointer to
a C function. For a pure Go application, the C function is a glue
routine that in turn calls the Go function. The Go utility
`ydb-dev-tools generate`_ generates glue routine functions.

Any function implementing logic for a transaction should return
:code:`int` with one of the following:

- A normal return (:code:`yottadb.YDB_OK`) to indicate that per application
  logic, the transaction can be committed. The YottaDB database engine
  will commit the transaction if it is able to, as discussed in
  `Transaction Processing`_, and if not, will call the function again.
- :code:`yottadb.YDB_TP_RESTART` to indicate that the transaction should restart, either
  because application logic has so determined or because a YottaDB
  function called by the function has returned TPRESTART.
- :code:`yottadb.YDB_TP_ROLLBACK` to indicate that :code:`TpST()` should not commit the
  transaction, and should return ROLLBACK to the caller.

In order to provide the function implementing the transaction logic
with a parameter or parameters, :code:`tpfnparm` is passed to the glue
routine, in turn be passed to the Go function called by the glue
routine. As :code:`tpfnparm` is passed from Go to YottaDB and back to
Go, the memory it references should be allocated using
`Go Malloc()`_ to protect it from the Go garbage collector.

The :code:`BufferTArray` receiving the :code:`TpST()` method is a list
of local variables whose values should be saved, and restored to their
original values when the transaction restarts. If the :code:`cbuftary`
structures have not been allocated or :code:`elemsUsed` is zero, no
local variables are saved and restored; and if :code:`elemsUsed` is 1,
and that sole element references the string "*" all local variables
are saved and restored.

A case-insensitive value of "BA" or "BATCH" for :code:`transid`
indicates to YottaDB that it need not ensure Durability for this
transaction (it continues to ensure Atomicity, Consistency, and
Isolation), as discussed under `ydb_tp_st()`_.

A special note: as the definition and implementation of Go protect
against dangling pointers in pure Go code, Go application code may not
be designed and coded with the same level of defensiveness against
dangling pointers that C applications are. In the case of
:code:`TpST()`, owing to the need to use :code:`unsafe.Pointer`
parameters, please take additional care in designing and coding your
application to ensure the validity of the pointers passed to
:code:`TpST()`.

TpST2()
----------

.. code-block:: go

	func (buftary *BufferTArray) TpST2(tptoken uint64,
		errstr *BufferT,
		tpfn func(uint64, *BufferT) int, transid string) error

Matching `TpE2()`_, :code:`TpST2()` wraps :code:`ydb_tp_st()` to
implement `Transaction Processing`_. The difference between
:code:`TpST()` and :code:`TpST2()` is that the former uses C glue code
to pass a parameter to the function implementing transaction logic,
whereas the latter is a pure Go function call (which may be a
closure).

Refer to `TpST()`_ for a more detailed discussion of YottaDB Go
transaction processing.

--------------------------
Go Simple API KeyT Methods
--------------------------

:code:`KeyT` methods return errors returned by methods that invoke the
underlying :code:`Varnm` and :code:`SubAry` members of :code:`KeyT`
structures, which can in turn originate in those methods or in YottaDB
functions invoked by them.

DataST()
-----------

.. code-block:: go

	func (key *KeyT) DataST(tptoken uint64,
		errstr *BufferT) (uint32, error)

Matching `DataE()`_, :code:`DataST()` returns the result of
`ydb_data_st()`_. In the event an error is returned, the return value
is unspecified.

DeleteST()
-------------

.. code-block:: go

	func (key *KeyT) DeleteS(tptoken uint64,
		errstr *BufferT, deltype int) error

Matching `DeleteE()`_, :code:`DeleteST()` wraps `ydb_delete_st()`_ to
delete a local or global variable node or (sub)tree, with a value of
:code:`C.YDB_DEL_NODE` for :code:`deltype` specifying that only the
node should be deleted, leaving the (sub)tree untouched, and a value
of :code:`C.YDB_DEL_TREE` specifying that the node as well as the
(sub)tree are to be deleted.

IncrST()
-----------

.. code-block:: go

	func (key *KeyT) IncrST(tptoken uint64,
		errstr *BufferT, incr, retval *BufferT) error

Matching `IncrE()`_, :code:`IncrST()` wraps `ydb_incr_st()`_ to
atomically increment the referenced global or local variable node
coerced to a number, with :code:`incr` coerced to a number. It stores
the result in the node and also returns it through the :code:`BufferT`
structure referenced by :code:`retval`.

- If `ydb_incr_st()`_ returns an error such as NUMOFLOW, INVSTRLEN, the
  method makes no changes to the structures under :code:`retval` and
  returns the error.
- If the length of the data to be returned exceeds
  :code:`retval.lenAlloc`, the method sets the :code:`len_used`
  of the :code:`C.ydb_buffer_t` referenced by :code:`retval`
  to the required length, and returns an INVSTRLEN error.
- Otherwise, it copies the data to the buffer referenced by the
  :code:`retval.buf_addr`, sets :code:`retval.lenUsed` to its
  length.

With a :code:`nil` value for :code:`incr`, the default increment
is 1. Note that the value of the empty string coerced to an integer is
zero.

LockDecrST()
---------------

.. code-block:: go

	func (key *KeyT) LockDecrS(tptoken uint64,
		errstr *BufferT) error

Matching `LockDecrE()`_ :code:`LockDecrST()` wraps
`ydb_lock_decr_st()`_ to decrement the count of the lock name
referenced, releasing it if the count goes to zero or ignoring the
invocation if the process does not hold the lock.

LockIncrST()
---------------

.. code-block:: go

	func (key *KeyT) LockIncrST(tptoken uint64,
		errstr *BufferT, timeoutNsec uint64) error

Matching `LockIncrE()`, :code:`LockIncrST()` wraps
`ydb_lock_incr_st()`_ to attempt to acquire the referenced lock
resource name without releasing any locks the process already holds.

- If the process already holds the named lock resource, the method
  increments its count and returns.
- If :code:`timeoutNsec` exceeds :code:`C.YDB_MAX_TIME_NSEC`, the
  method returns with an error return TIME2LONG.
- If it is able to aquire the lock resource within :code:`timeoutNsec`
  nanoseconds, it returns holding the lock, otherwise it returns
  LOCK_TIMEOUT. If :code:`timeoutNsec` is zero, the method makes
  exactly one attempt to acquire the lock.

NodeNextST()
---------------

.. code-block:: go

	func (key *KeyT) NodeNextST(tptoken uint64,
		errstr *BufferT, next *BufferTArray) error

Matching `NodeNextE()`_, :code:`NodeNextST()` wraps
`ydb_node_next_st()`_ to facilitate depth first traversal of a local or
global variable tree.

- If there is a next node:

  - If the number of subscripts of that next node exceeds
    :code:`next.elemsAlloc`, the method sets :code:`next.elemsUsed` to
    the number of subscripts required, and returns an INSUFFSUBS
    error. In this case the :code:`elemsUsed` is greater than
    :code:`elemsAlloc`.
  - If one of the :code:`C.ydb_buffer_t` structures referenced by
    :code:`next` (call the first or only element :code:`n`) has
    insufficient space for the corresponding subscript, the method sets
    :code:`next.elemsUsed` to :code:`n`, and the :code:`len_alloc` of
    that :code:`C.ydb_buffer_t` structure to the actual space
    required. The method returns an INVSTRLEN error. In this case the
    :code:`len_used` of that structure is greater than its
    :code:`len_alloc`.
  - Otherwise, it sets the structure :code:`next` to reference the
    subscripts of that next node, and :code:`next.elemsUsed` to the
    number of subscripts.

- If the node is the last in the tree, the method returns the NODEEND
  error, making no changes to the structures below :code:`next`.

NodePrevST()
---------------

.. code-block:: go

	func (key *KEyT) NodePrevST(tptoken uint64,
		errstr *BufferT, prev *BufferTArray) error

Matching `NodePrevE()`_, :code:`NodePrevST()` wraps
`ydb_node_previous_st()`_ to facilitate reverse depth first traversal
of a local or global variable tree.

- If there is a previous node:

  - If the number of subscripts of that previous node exceeds
    :code:`prev.elemsAlloc`, the method sets :code:`prev.elemsUsed` to
    the number of subscripts required, and returns an INSUFFSUBS
    error. In this case the :code:`elemsUsed` is greater than
    :code:`elemsAlloc`.
  - If one of the :code:`C.ydb_buffer_t` structures referenced by
    :code:`prev` (call the first or only element :code:`n`) has
    insufficient space for the corresponding subscript, the method sets
    :code:`prev.elemsUsed` to :code:`n`, and the :code:`len_alloc` of
    that :code:`C.ydb_buffer_t` structure to the actual space
    required. The method returns an INVSTRLEN error. In this case the
    :code:`len_used` of that structure is greater than its
    :code:`len_alloc`.
  - Otherwise, it sets the structure :code:`prev` to reference the
    subscripts of that prev node, and :code:`prev.elemsUsed` to the
    number of subscripts.

- If the node is the first in the tree, the method returns the NODEEND
  error making no changes to the structures below :code:`prev`.

SetValST()
-------------

.. code-block:: go

	func (key *KeyT) SetST(tptoken uint64,
		errstr *BufferT, value *BufferT) error

Matching `SetValE()`_, at the referenced local or global variable
node, or the intrinsic special variable, :code:`SetST()` wraps
`ydb_set_st()`_ to set the value specified by :code:`val`.

SubNextST()
--------------

.. code-block:: go

	func (key *KeyT) SubNextST(tptoken uint64,
		errstr *BufferT, retval *BufferT) error

Matching `SubNextE()`_, :code:`SubNextST()` wraps
`ydb_subscript_next_st()`_ to facilitate breadth-first traversal of a
local or global variable sub-tree.

- At the level of the last subscript, if there is a next subscript
  with a node and/or a subtree:

  - If the length of that next subscript exceeds
    :code:`sub.len_alloc`, the method sets :code:`sub.len_used` to the
    actual length of that subscript, and returns an INVSTRLEN error. In
    this case :code:`sub.len_used` is greater than
    :code:`sub.len_alloc`.
  - Otherwise, it copies that subscript to the buffer referenced by
    :code:`sub.buf_addr`, and sets :code:`sub.len_used` to its length.

- If there is no next node or subtree at that level of the subtree,
  the method returns the NODEEND error.

SubPrevST()
--------------

.. code-block:: go

	func (key *KeyT) SubPrevST(tptoken uint64,
		errstr *BufferT, retval *BufferT) error

:code:`SubPrevST()` wraps `ydb_subscript_previous_st()`_ to facilitate
reverse breadth-first traversal of a local or global variable sub-tree.

- At the level of the last subscript, if there is a previous subscript
  with a node and/or a subtree:

  - If the length of that previous subscript exceeds
    :code:`sub.len_alloc`, the method sets :code:`sub.len_used` to the
    actual length of that subscript, and returns an INVSTRLEN error. In
    this case :code:`sub.len_used` is greater than
    :code:`sub.len_alloc`.
  - Otherwise, it copies that subscript to the buffer referenced by
    :code:`sub.buf_addr`, and sets :code:`buf.len_used` to its length.

- If there is no previous node or subtree at that level of the
  subtree, the method returns the NODEEND error.

ValST()
----------

.. code-block:: go

	func (key *KeyT) ValST(tptoken uint64,
		errstr *BufferT, retval *BufferT) error

Matching `ValE()`_, :code:`ValST()` wraps `ydb_get_st()`_ to return
the value at the referenced global or local variable node, or
intrinsic special variable, in the buffer referenced by the
:code:`BufferT` structure referenced by :code:`retval`.

- If `ydb_get_st()`_ returns an error such as GVUNDEF, INVSVN, LVUNDEF,
  the method makes no changes to the structures under :code:`retval`
  and returns the error.
- If the length of the data to be returned exceeds
  :code:`retval.getLenAlloc()`, the method sets the :code:`len_used` of
  the :code:`C.ydb_buffer_t` referenced by :code:`retval` to the
  required length, and returns an INVSTRLEN error.
- Otherwise, it copies the data to the buffer referenced by the
  :code:`retval.buf_addr`, and sets :code:`retval.lenUsed` to its
  length.

-----------------------
Go Simple API Functions
-----------------------

LockST()
-----------

.. code-block:: go

	func yottadb.LockST(tptoken uint64, errstr *BufferT,
		timeoutNsec uint64, lockname ... *KeyT) error

Matching `LockE()`_, :code:`LockST()` wraps `ydb_lock_st()`_ to
release all lock resources currently held and then attempt to acquire
the named lock resources referenced. If no lock resources are
specified, it simply releases all lock resources currently held and
returns.

If lock resources are specified, upon return, the process will have
acquired all of the named lock resources or none of the named lock
resources.

- If :code:`timeoutNsec` exceeds :code:`C.YDB_MAX_TIME_NSEC`, the
  method returns with a TIME2LONG error.
- If the number of lock resource names exceeds the maximum number
  supported (currently eleven), the function returns a PARMOFLOW
  error.
- If it is able to aquire the lock resource(s) within
  :code:`timeoutNsec` nanoseconds, it returns holding the lock
  resource(s); otherwise it returns LOCKTIMEOUT. If
  :code:`timeoutNsec` is zero, the method makes exactly one attempt to
  acquire the lock resource(s).

Go Comprehensive API
====================

The Go Comprehensive API is a project for the future, to follow the C
`Comprehensive API`_

Go Utility Functions
====================

---------
Exit()
---------

.. code-block:: go

	func yottadb.Exit() error

For a process that wishes to close YottaDB databases and no longer use
YottaDB, the function wraps `ydb_exit()`_ so that any further calls to
YottaDB return a CALLINAFTEREXIT` error.

Although in theory typical processes should not need to call
:code:`Exit()` because normal process termination should close
databases cleanly, in practice thread shutdown may not always ensure
that databases are closed cleanly. So, application code should invoke
:code:`Exit()` prior to process exit, or when an application intends
to continue with other work beyond use of YottaDB.

-------------------
IsLittleEndian()
-------------------

.. code-block:: go

	func yottadb.IsLittleEndian() bool

The function returns :code:`true` if the underlying computing infrastructure
is little endian and :code:`false` otherwise.

-------------
MessageT()
-------------

.. code-block:: go

	func yottadb.Message(tptoken uint64, errstr *BufferT,
		status int) (string, error)

:code:`MessageT()` returns the text template for the error number
specified by :code:`status`.

- If :code:`status` does not correspond to an error that YottaDB
  recognizes, it returns the error UNKNOWNSYSERR.
- Otherwise, it returns the error message text template for the error
  number specified by :code:`status`.

-------------
ReleaseT()
-------------

.. code-block:: go

	func yottadb.ReleaseT(tptoken uint64, errstr *BufferT) string

Returns a string consisting of six space separated pieces to provide
version information for the Go wrapper and underlying YottaDB release:

- The first piece is always “gowr” to idenfify the Go wrapper.
- The Go wrapper release number, which starts with “r” and is followed
  by two numbers separated by a period (“.”), e.g., “r1.32”. The first
  is a major release number and the second is a minor release number
  under the major release. Even minor release numbers indicate
  formally released software. Odd minor release numbers indicate
  software builds from “in flight” code under development, between
  releases. Note that although they follow the same format, Go wrapper
  release numbers are different from the release numbers of the
  underlying YottaDB release as reported by `$zyrelease`_.
- The third through sixth pieces are `$zyrelease`_ from the underlying
  YottaDB relase.

Go Programming Notes
====================

These `Go Programming Notes`_ supplement rather than supplant the more
general `Programming Notes`_ for C.

----------
Goroutines
----------

In order to avoid restricting Go applications to calling the
single-threaded YottaDB engine from a single goroutine (which would be
unnatural to a Go programmer), the YottaDB Go wrapper calls the
functions of the C `Simple API`_ that support multi-threaded
applications, and includes logic to maintain the integrity of the
engine.

Directly calling YottaDB C API functions bypasses this protection, and
may result in unpredictable results (Murphy says that unpredictable
results will occur when you least expect them). Therefore, Go
application code should only call the YottaDB API exposed in this
`Programming in Go`_ section.

Goroutines in a process are dynamically mapped by the Go
implementation to run on threads within that process. Since YottaDB
resources are held by the process rather than by the thread or the
Goroutine, refer to the `Threads`_ discussion about the need for
applications to avoid race conditions when accessing YottaDB
resources.

----------------------
ydb-dev-tools generate
----------------------

As discussed in `TpST()`_ and referred to `TpE()`_, as Go does not
permit a pointer to a Go function to be passed as a parameter to a C
function, Go functions that encapsulate logic to be executed as an
ACID transaction cannot be passed as parameters to :code:`TpST()` and
:code:`yottadb.TpE()`. Instead, each Go function must have a C
"glue" routine whose address is passed to :code:`TpST()` or
:code:`yottadb.TpE()`.

Installing ydb-dev-tools can be done via :code:`go get lang.yottadb.com/go/yottadb/cmd/ydb-dev-tools`.
Ensure that :code:`$GOPATH/bin` is your :code:`$PATH`.
      
If an application has a callback routine called :code:`CallBackRtn()`
in package :code:`MyPackage`, executing
:code:`ydb-dev-tools generate -pkg MyPackage -func CallBackRtn` will generate a
routine :code:`CallBackRtn_cgo.go` which has the code to call
:code:`CallBackRtn()`. In addition to this file, one needs to add the
magic comment :code:`//export CallBackRtn` directly in front of the Go routine's
function definition. It is also essential that one adds :code:`import "C"` to this file
to ensure that Go generated C code for linking against.

In the following example,
:code:`/usr/local/lib/yottadb/r124` is the directory where YottaDB
r1.24 (the YottaDB release to be used) resides.

:code:`CallBackRtn_cgo.go` looks like this:

.. code-block:: go

	package MyPackage
	package MyPackage

	import "unsafe"

	/*
	#cgo pkg-config: yottadb
	#include <libyottadb.h>
	#include <inttypes.h>
	int CallBackRtn(uint64_t tptoken, ydb_buffer_t *errstr, void *tpfnparm);
	int CallBackRtn_cgo(uint64_t tptoken, ydb_buffer_t *errstr, void *tpfnparm) {
		return CallBackRtn(tptoken, errstr, tpfnparm);
	}
	*/
	import "C"

	// GetCallBackRtnCgo returns a pointer to the C wrapper for CallBackRtn to pass to yottadb.TpE
	func GetCallBackRtnCgo() unsafe.Pointer {
		return C.CallBackRtn_cgo
	}

The Go code module that includes the acutal transaction logic,
:code:`CallBackRtn()` should be structured thus, with no space between
:code:`//` and :code:`export`:

.. code-block:: go

	import "C"
	
	//export CallBackRtn
	func CallBackRtn(tptoken uint64, errptr *BufferT, tpfnparm unsafe.Pointer) int {
	    var errstr yottadb.BufferT
	    errstr.BufferTFromPtr(errptr)
	    defer errstr.Free()
	    // code for function
	}

:code:`CallBackRtn_cgo.go` and the module defining
:code:`CallBackRtn()` should reside in the same directory.

================
Programming in M
================

YottaDB includes a complete implementation of the `M
<https://en.wikipedia.org/wiki/MUMPS>`_ programming language (also
known as MUMPS - see `The Heritage and Legacy of M (MUMPS) – and the
Future of YottaDB
<https://yottadb.com/heritage-legacy-m-mumps-future-yottadb/>`_ ))
that mostly conforms to `ISO/IEC 11756:1999
<http://www.iso.ch/iso/en/CatalogueDetailPage.CatalogueDetail?CSNUMBER=29268&ICS1=35&ICS2=60&ICS3=&scopelist>`_.
The `YottaDB M Programmers Guide
<https://docs.yottadb.com/ProgrammersGuide/index.html>`_ documents
programming YottaDB in M and is not duplicated here.

YottaDB supports calling between M and C application code, as
 documented in `Chapter 11 (Integrating External Routines) of the M
 Programmers Guide
 <https://docs.yottadb.com/ProgrammersGuide/extrout.html>`_.

.. _Programming Notes:

============================================
Programming Notes (Avoiding Common Pitfalls)
============================================

As YottaDB is likely different from other data management and
persistence engines you have used, this section provides information
about features of YottaDB intended to help you avoid common pitfalls.

Numeric Considerations
======================

To ensure the accuracy of financial calculations, [#]_ YottaDB internally
stores numbers as, and performs arithmetic using, a scaled packed
decimal representation with 18 significant decimal digits, with
optimizations for values within a certain subset of its full
range. YottaDB efficiently converts between strings and numbers.

.. [#] For example, since a number such as .01 is not exactly
       representable as a binary or hexadecimal floating point number
       adding a list of currency values using floating point
       arithmetic does not guarantee that the result will be correct
       to the penny, which is a requirement for financial
       calculations.

When passed a string that is a `canonical number`_ for use as a subscript,
YottaDB automatically converts it to a number. This automatic
internal conversion is immaterial for applications:

- that simply store and retrieve data associated with subscripts,
  potentially testing for the existence of nodes; or
- whose subscripts are all numeric, and should be collated in numeric order.

This automatic internal conversion is material to applications that
use:

- numeric subscripts and expect the subscripts to be sorted in lexical order
  rather than numeric order; or
- mixed numeric and non-numeric subscripts, including subscripts that
  are not canonical numbers.

Applications that are affected by automatic internal conversion should
prefix their subscripts with a character such as "x" which ensures
that subscripts are not canonical numbers.

.. _canonical number:
.. _canonical numbers:

-----------------
Canonical Numbers
-----------------

Conceptually, a canonical number is a string from the Latin character
set that represents a decimal number in a standard, concise, form.

#. Any string of decimal digits, optionally preceded by a minus sign
   ("-"), the first of which is not "0" (except for the number zero
   itself), that represents an integer of no more than 18 significant
   digits.

   - The following are canonical numbers: "-1", "0", "3", "10",
     "99999999999999999999", "999999999999999999990". Note that the
     last string has only 18 significant digits even though it is 19
     characters long.
   - The following are not canonical numbers: "+1" (starts with "+"),
     "00" (has an extra leading zero), "999999999999999999999" (19
     significant digits), "-0" (the canonical representation of 0 is
     "0").

#. Any string of decimal digits, optionally preceded by a minus sign
   that includes one decimal point ("."), the first and last of which
   are not "0", that represents a number of no more than 18 significant
   digits.

   - The following are canonical numbers: "-.1", ".3",
     ".99999999999999999999".
   - The following are not canonical numbers "+.1" (starts with "+"),
     "0.3" (first digit is "0"), ".999999999999999999990" (last digit
     is "0"), ".999999999999999999999" (more than 18 significant
     digits).

#. Any of the above two forms followed by "E" (upper case only)
   followed by a canonical integer in the range -43 to 47 such
   that the magnitude of the resulting number is between 1E-43
   through .1E47.

.. _zwrite format:
.. _zwrite formatted:

Zwrite Format
=============

Strings used as subscripts and as values can include unprintable
bytes, for example control characters or binary data. YottaDB's zwrite
format is an encoding in printable ASCII of any sequence of
bytes. Unlike formats such as Base64, the zwrite format attempts to
preserve readability of printable ASCII characters. Note that a zwrite
formatted string is always longer than the original string (at the
very least, it has enclosing quotes).

Signals
=======

As YottaDB includes a real-time database engine that resides within
the address space of a process, applications that use signals *must*
not interfere with database operation.

When the YottaDB database engine initializes on the first call into
the API, it initializes signal handling as follows:

- :code:`SIGALRM` – YottaDB uses this signal extensively and sets its
  own signal handler for :code:`SIGALRM`. Application code should *not*
  use :code:`SIGALRM`, and must *never* replace YottaDB's
  handler. YottaDB provides an API for applications that need timing
  functionality (see `Utility Functions`_).
- :code:`SIGCHLD` (formerly :code:`SIGCLD`) – Set to :code:`SIG_DFL` for the
  default action.
- :code:`SIGTSTP`, :code:`SIGTTIN`, and :code:`SIGTTOU` – As
  suspending a real-time database engine at an inopportune moment is
  undesirable, YottaDB sets a signal handler for these signals that
  defers process suspension until the engine is in a state where it is
  safe to suspend.
- :code:`SIGCONT` - YottaDB sets a handler that continues a suspended
  process, and nothing if the process is not suspended.
- :code:`SIGINT` – YottaDB sets a handler for Ctrl-C. In call-in or
  simple API mode, this handler first calls the non-YDB main program's 
  Ctrl-C handler (if one exists) and if that call returns, exits with a
  -1 return code. Also, if a call-in is done in this environment and the
  M code uses either the CENABLE or NOCENABLE device parameters, those 
  parameters are IGNORED. In M mode with a mumps executable, behavior is
  as documented in the `M Programmer's Guide <https://docs.yottadb.com/ProgrammersGuide/index.html>`_.
- :code:`SIGUSR1` – As YottaDB uses this signal to asynchronously
  execute the M code in the `$zinterrupt intrinsic special variable
  <https://docs.yottadb.com/ProgrammersGuide/isv.html#zinterrupt>`_,
  it sets an appropriate handler. If non-M code is currently active
  when the process receives a :code:`SIGUSR1`, the handler defers the
  signal till such time as M code is active. If an application uses no
  M code whatsoever, and does not intend to, it can change the
  :code:`SIGUSR1` handler after the first call to YottaDB. If an
  application has, or in the future may have, M code, it is best to
  leave the YottaDB handler in place.
- :code:`SIGUSR2` – As YottaDB processes other than the servers for
  client/server operation do not use :code:`SIGUSR2`, YottaDB sets a
  :code:`SIG_IGN` handler. :code:`SIGUSR2` is available for
  applications to use. To do so, set a handler after the first call to
  YottaDB.
- :code:`SIGQUIT` – YottaDB sets a handler to terminate the process
  without generating a core dump.
- :code:`SIGABRT`, :code:`SIGBUS`, :code:`SIGFPE`, :code:`SIGILL`,
  :code:`SIGIOT`, :code:`SIGSEGV`, :code:`SIGTERM`, and
  :code:`SIGTRAP` – These signals are fatal, and the YottaDB handler
  terminates the process with a core dump. See the discussion about core
  dumps in the description of `ydb_fork_n_core()`_. Although YottaDB
  normally cleans up processes' interaction with databases on exit,
  these signals can indicate that the process is in a bad state and that
  its code and data cannot be trusted. The process therefore does
  not attempt to clean up before exit. After a fatal signal, *no*
  YottaDB functions can be called except `ydb_exit()`_.  In the
  event an application *must* use its own handler for one of
  these signals, it must either save YottaDB's handler, and drive
  it before process termination or call `ydb_exit()`_ prior to
  process exit. [#]_
- YottaDB saves an application's signal handler during
  initialization and restores it if :code:`ydb_exit()` is explicitly
  called prior to process exit. YottaDB does not reset existing signal handlers
  for signals it does not handle but calls the saved signal handler if the YottaDB handler returns (and doesn't exit).

.. [#] Other YottaDB processes will attempt to automatically clean up
       after a process terminates abnormally. However, this is not
       guaranteed. Also, if the abnormally terminating process is the
       last process accessing a database file, there are no remaining
       processes to attempt a cleanup. Avoid using these signals to
       terminate processes unless you know what you are doing.

As database operations such as `ydb_set_s()`_ set timers, subsequent
system calls can terminate prematurely with an EINTR. Such system
calls should be wrapped to restart them when this occurs. An example
from the file `eintr_wrappers.h
<https://gitlab.com/YottaDB/DB/YDB/blob/master/sr_port/eintr_wrappers.h>`_
demonstrates how YottaDB itself is coded to handle system calls that
terminate prematurely with an EINTR:

.. code-block:: c

    #define FGETS_FILE(BUF, LEN, FP, RC)                            \
    {                                                               \
	    do                                                      \
	    {                                                       \
		    FGETS(BUF, LEN, FP, RC);                        \
	    } while (NULL == RC && !feof(FP) && ferror(FP) && EINTR == errno);      \
    }

If YottaDB is used within a process with other code that cannot
co-exist, or be made to co-exist, with YottaDB, for example, by safely
saving and restoring handlers, separate the logic into multiple
processes or use a client/server database configuration to place
application logic and the database engine in separate processes (see
`Client/Server Operation`_).

To reiterate because of its importance: **never** replace YottaDB's
:code:`SIGALRM` handler.

Forking
=======

There are two considerations when executing :code:`fork()`.

- Before a process that performs buffered IO executes :code:`fork()`, it
  should execute :code:`fflush()`. Otherwise, the child process will
  inherit unflushed buffers from the parent, which the child process
  will flush when it executes an :code:`fflush()`. This is a general
  programming admonition, not specific to YottaDB except to the extent
  that M code within a parent process may have executed :code:`write`
  commands which are still buffered when C code within the same
  process calls :code:`fork()`.


Threads
=======

Important Notes:

- Local variables, locks and transaction contexts are held by the
  process and not by the thread. In other words, these resources are
  shared by threads in a multi-threaded application, and YottaDB
  assumes that the threads of an application cooperate to manage the
  resources, e.g.

  - One thread may set a local variable node, and another thread may
    delete it.
  - One thread may acquire a lock and another may release it.
  - A global variable update within a transaction by one thread is
    immediately visible to another thread within the process, but is
    not visible to other processes until the transaction commits.

- It is the responsibility of the application to avoid race conditions
  between threads in their use of resources managed by YottaDB at the
  level of the process. YottaDB does not ensure the absence of race
  conditions in accessing these resources because to do so would
  unduly restrict the freedom of application designers. For example,
  it is a legitimate design pattern to have one thread that provides
  one subscript of a node, and a different thread that provides a
  different subscript.

.. _errstr:
.. _*errstr:

- Simple API functions
  use an :code:`*errstr` parameter to avoid a race condition and
  ensure they get the correct `$zstatus`_ when function has an error
  return. If an application calls `ydb_get_s()`_ / `ydb_get_st()`_ for
  the value of `$zstatus`_ for the complete error text when a YottaDB
  function returns an `error return code`_, for a single-threaded
  application, `$zstatus`_ has correct and current information, since
  calls to YottaDB are entirely under the control of that single
  application thread. For a multi-threaded application, between the
  time a function returns with an `error return code`_, and a
  subsequent call to `ydb_get_st()`_ to get the value of `$zstatus`_,
  another thread may call YottaDB, and the `$zstatus`_ returned will
  be from that subsequent call. A :code:`*errstr` parameter in
  functions for multi-threaded applications provides the `$zstatus`_ for
  that call to the caller.

  - An application that does not want the `$zstatus`_ string can pass
    a :code:`NULL` value for :code:`*errstr`.

  - The string in `errstr->buf_addr` is always null terminated, which
    allows :code:`*errstr` to be passed to standard system functions
    like :code:`printf()`.

  - In the event a buffer provided by an application is not long
    enough for a `$zstatus`_, YottaDB truncates the string to be
    reported, rather than issuing an INVSTRLEN error (since a second
    error while attempting to report an error is likely to add
    confusion rather than enlightenment).

    - :code:`errstr->len_used` is always set to the length of `$zstatus`_,
      whether or not it is truncated.
    - If :code:`errstr->len_used` is greater than
      :code:`errstr->len_alloc-1` it means `$zstatus`_ has been
      truncated.

- A multi-threaded application is permitted to use the YottaDB
  single-thread functions *as long as the application ensures that all
  YottaDB access is performed only by one thread.* A thread may use
  the `ydb_thread_is_main()`_
  to determine whether it is the thread that is calling
  YottaDB. YottaDB strongly recommends against this application design
  pattern: this functionality only exists to provide backward
  compatibility to a specific existing application code base.

Even though the YottaDB data management engine is single-threaded and
operates in a single thread, [#]_ it supports both single- and
multi-threaded applications. Multi-threaded applications may call
multi-threaded `Simple API`_ functions – those whose names end in
:code:`_st()` – as well as utility functions – those whose names end
in :code:`_t()`. Single-threaded applications may call the `Simple
API`_ single-threaded functions – those whose names end in
:code:`_s()` – as well as utility functions – those whose names do not
end in :code:`_t()`. An application *must not* call both
single-threaded and multi-threaded Simple API functions, and any
attempt to do so results in a YottaDB error returned to the caller.

.. [#] Although there is functionality within YottaDB that may invoke
       multiple threads under the covers (such as asynchronous
       database IO), these perform certain very limited and specific
       operations. The YottaDB engine itself is single threaded.

When a single-threaded application calls a YottaDB function, the
application code blocks until YottaDB returns, the standard single
threaded application behavior for a function call, also known as
synchronous calls.

In a multi-threaded application, the YottaDB engine runs in its own
thread, which is distinct from any application thread. When a
multi-threaded application calls a YottaDB function, the function puts
a request on a queue for the YottaDB engine, and blocks awaiting a
response – in other words, any call to YottaDB is synchronous as far
as the caller is concerned, even if servicing that call results in
asynchronous activity within the process. Meanwhile, other application
threads continue to run, with the YottaDB engine handling queued
requests one at at time. An implication of this architecture is that
multi-threaded functions of the Simple API cannot recurse – a call to
a multi-threaded function when another is already on the C stack of a
thread results in a `SIMPLEAPINEST
<https://docs.yottadb.com/MessageRecovery/errors.html#simpleapinest>`_
error. While this is conceptually simple for
applications that do not use `Transaction Processing`_, transaction
processing in a threaded environment requires special consideration
(see `Threads and Transaction Processing`_).

`Programming in M`_ is single-threaded and single-threaded
applications can call into M code, and M code can call single threaded
C code as documented in `Chapter 11 (Integrating External Routines) of
the M Programmers Guide
<https://docs.yottadb.com/ProgrammersGuide/extrout.html>`_
Multi-threaded C applications are able to call M code through the
functions :code:`ydb_ci_t()` and :code:`ydb_cip_t()` functions as
documented `there
<https://docs.yottadb.com/ProgrammersGuide/extrout.html#call-in-interface>`_,
with the restriction that if M code called through :code:`ydb_ci_t()`
or :code:`ydb_cip_t()` calls out to C code, that C code is not
permitted to start a transaction using :code:`ydb_tp_st()`.

Note that triggers, which are written in M, run in the thread of the
YottaDB engine, and are unaffected by multi-threaded Simple API calls
already on an application process thread's stack. However, if a
trigger calls C code, and that C code calls :code:`ydb_ci_t()` or
:code:`ydb_cip_t()`, that C code is not permitted to call
:code:`ydb_tp_st()`.

.. _tptoken:

----------------------------------
Threads and Transaction Processing
----------------------------------

As discussed in `Transaction Processing`_, `ydb_tp_s()`_ or
`ydb_tp_st()`_ are called with a pointer to the function that is
called to execute an application's transaction logic.

In a single-threaded application, the YottaDB engine calls the TP
function and blocks until it returns. The function may itself call
YottaDB recursively, and the existence of a single thread ensures that
any call to YottaDB occurs at the correct transaction nesting level.

In a multi-threaded application, the YottaDB engine invokes the TP
function in another thread, but cannot block until it gets the message
that the function has terminated with a value to be returned, because
the engine must listen for messages from that function, as well as
threads it spawns. Furthermore, one of those threads may itself call
`ydb_tp_st()`_. Therefore

- The YottaDB engine must know the transaction nesting level at which
  it is operating, responding to requests for service at that level,
  and block any transaction invocations at a higher (enclosing) level
  until the current transactio is closed (committed or rolled back).
- After a transaction has closed, any further calls from threads
  invoking YottaDB for the closed transaction must receive errors.

To accomplish this, the `Simple API`_ functions for threaded
applications – those ending in :code:`_st()` – have a :code:`tptoken`
first parameter used as follows to provide the required transaction
context of a thread.

- When an application calls a `Simple API`_ function outside a
  transaction, it provides a value of :code:`YDB_NOTTP` for
  :code:`tptoken`.
- When an application calls `ydb_tp_st()`_, it generates provides a
  :code:`tptoken` as the first parameter when it calls the function
  that implements the logic for the transaction. Any threads that this
  function spawns must provide this :code:`tptoken` to
  YottaDB. Passing in a different or incorrect :code:`tptoken` can
  result in hard-to-debug application behavior, including deadlocks.
- When a `Simple API`_ function is called:

  - If :code:`tptoken` is that of the current transaction, the request
    is processed.
  - If :code:`tptoken` is that of a higher level transaction within
    which the current transaction is nested, the call blocks until the
    nested transaction completes (or nested transactions complete,
    since there may be multiple nesting levels).
  - If :code:`tptoken` does not correspond to a higher level
    transaction (e.g., if it corresponds to a closed transaction or a
    nonexistent one), YottaDB returns an error.

Note: if the function implementing a transaction spawns threads (or
coroutines executing in threads), those threads/coroutines must:

- terminate before the function returns to YottaDB;
- use a current :code:`tptoken` when invoking YottaDB (in effect,
  switching transaction contexts ­ technically this violates ACID
  transaction properties but perhaps reasonable in a few restricted
  cases, such as creating background worker threads); or
- not invoke YottaDB.

Should a thread/coroutine spawned in a function implementing
transaction logic invoke YottaDB after the function has returned, the
thread/coroutine will get an invalid token error message unless it
uses a current :code:`tptoken`.

Note: Sharing or passing :code:`tptoken` values between
threads/coroutines can lead to deadlocks and other hard-to-debug
situations. YottaDB strongly recommends against such usage. If you
have a legitimate use case, design it so that you can debug it when
the inevitable error condition occurs.

Timers and Timeouts
===================

Although the Simple API uses nanosecond resolution to specify all time
intervals, in practice underlying functions may have more granular
resolutions (microseconds or milliseconds). Furthermore, even with a
microsecond or millisecond resolution, the accuracy is always
determined by the underlying hardware and operating system, as well as
factors such as system load.

Memory Allocation
=================

Memory allocated by `ydb_malloc()`_ must be explicitly freed by
`ydb_free()`_. `ydb_exit()`_ does not free memory, and any
memory allocated but not freed prior to `ydb_exit()`_ is released
only on process exit.

Syslog
======

Issues that pertain to the application and on which application code
can take reasonable action are reported to the application
(:code:`YDB_ERR_GVUNDEF` being an example) and issues that pertain to
operations and which application code cannot take reasonable action
but which operations staff can (like running low on filesystem space,
which are not discussed here, as this is a Programmers Guide) are
reported to the syslog. In the event that a syslog does not exist
(e.g., in default Docker containers), a process' syslog messages go to
its stderr.

YottaDB uses the existence of :code:`/dev/log` as an indicator of the
existence of a syslog.

IO
==

Although YottaDB does not prohibit it, we recommend against performing
IO to the same device from M and non-M code in a process unless you
know exactly what you are doing and have the expertise to debug
unexpected behavior. Owing to differences in buffering, and in the
case of interactive sessions, setting terminal characteristics,
performing IO to the same device from both M and non-M code will
likely result in hard to troubleshoot race conditions and other
behavior.
