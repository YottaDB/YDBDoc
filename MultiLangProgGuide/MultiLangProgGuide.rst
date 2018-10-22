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
the YottaDB engine from any language. As YottaDB adds standard APIs
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

2. Choose a directory for your default environment and initialize it:
   :code:`export ydb_dir=$HOME/.yottadb ; . /usr/local/lib/yottadb/r122/ydb_env_set`
#. Put your C program in the :code:`$ydb_dir` directory,
   :code:`#include` the file :code:`/usr/local/lib/yottadb/r122/libyottadb.h`
   in your C program and compile it. As a sample program, you can
   download the `wordfreq.c
   <https://gitlab.com/YottaDB/DB/YDBTest/raw/master/simpleapi/inref/wordfreq.c>`_
   program, with a `reference input file
   <https://gitlab.com/YottaDB/DB/YDBTest/raw/master/simpleapi/outref/wordfreq_input.txt>`_
   and `corresponding reference output file
   <https://gitlab.com/YottaDB/DB/YDBTest/raw/master/simpleapi/outref/wordfreq_output.txt>`_
   and compile it with :code:`gcc -I $ydb_dist -L $ydb_dist -o wordfreq wordfreq.c -lyottadb`.

#. Run your program and verify that the output matches the reference output. For example:

.. code-block:: bash

	$ cd $ydb_dir
	$ gcc -I $ydb_dist -L $ydb_dist -o wordfreq wordfreq.c -lyottadb
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

  - Empty string subscripts precede all numeric subscripts. *Note:
    YottaDB recommends against applications that use empty string
    subscripts.* [#]_
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
operation does not support `transaction processing`_. This means that
within a `ydb_tp_s()`_ call, application code on a client machine is
not permitted to access a database region that resides in a file on a
remote server. Furthermore, there are configurations that impliticly
invoke transaction processing logic, such as distributing a global
variable over multiple database regions, or a trigger invocation (see
`Chapter 14 Triggers of the YottaDB M Programmers Guide
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

Application code can read the intrinsic special variable :code:`$tlevel`
to determine whether it is executing inside a
transaction. :code:`$tlevel>0` means that it is inside a transaction, and
:code:`$tlevel>1` means that it is inside a nested transaction. Note that
a transaction can be started explicitly, e.g., by calling
`ydb_tp_s()`_ ,or implicitly by a trigger resulting from a
`ydb_delete_s()`_, or `ydb_set_s()`_.

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
`ydb_set_s()`_ to set it.

--------
$zstatus
--------

:code:`$zstatus` provides additional details of the last
error. Application code can retrieve :code:`$zstatus` using
`ydb_get_s()`_. :code:`$zstatus` consists of several comma-separated
substrings.

- The first is an error number.
- The second is always :code:`"(SimpleAPI)"`.
- The remainder is more detailed information about the error, and may
  contain commas within.

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
  to the database, so that it's own logic sees the updates, but no
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

In YottaDB's API for transaction processing, an application
packages the logic for a transaction into a function with one
parameter, passing the function and its parameter as parameters to the
`ydb_tp_s()`_ function. YottaDB then calls that function.

- If the function returns a :CODE:`YDB_OK`, YottaDB attempts to commit
  the transaction. If it is unable to commit as described above, or if
  the called function returns a :CODE:`YDB_TP_RESTART` return code, it
  calls the function again.
- If the function returns a :CODE:`YDB_TP_ROLLBACK`, `ydb_tp_s()`_ returns
  to its caller with that return code after discarding the uncommitted
  database updates and releasing any locks acquired within the
  transaction.
- To protect applications against poorly coded transactions, if a
  transaction takes longer than the number of seconds specified by the
  intrinsic special variable :code:`$zmaxtptime`, YottaDB aborts the
  transaction and the `ydb_tp_s()`_ function returns the
  :CODE:`YDB_ERR_TPTIMEOUT` error.

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

------------------------
Function Return Codes
------------------------

Return codes from calls to YottaDB are usually of type :code:`int` and
occasionally other types. Normal return codes are non-negative
(greater than or equal to zero); error return codes are negative.

+++++++++++++++++++
Normal Return Codes
+++++++++++++++++++

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
`ydb_tp_s()`_ invocation from which it was called. See `Transaction
Processing`_ for a discussion of restarts.

:CODE:`YDB_TP_ROLLBACK` — Return code to YottaDB from an application
function that implements a transaction, and in turn returned to the
caller indicating that the transaction was not committed.

.. _error return code:

.. _error return codes:

++++++++++++++++++++
Error Return Codes
++++++++++++++++++++

Symbolic constants for error codes returned by calls to YottaDB are
prefixed with :CODE:`YDB_ERR_` and are all less than zero. The symbolic
constants below are not a complete list of all error messages that
YottaDB functions can return — error return codes can indicate system
errors and database errors, not just application errors. A process
that receives a negative return code, including one not listed here,
can call `ydb_get_s()`_ to get the value of `$zstatus`_.

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

:CODE:`YDB_ERR_INSUFFSUBS` — A call to :code:`ydb_node_next_s()` or
:code:`ydb_node_previous_s()` did not provide enough parameters for the
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
to `ydb_delete_excl_s()`_ or `ydb_tp_s()`_ exceeded the
:CODE:`YDB_MAX_NAMES`.

:code:`YDB_NOTOK` – `ydb_file_name_to_id()`_ was called with a NULL
pointer to a filename.

:CODE:`YDB_ERR_NUMOFLOW` — A `ydb_incr_s()`_ operation resulted in a
numeric overflow.

:CODE:`YDB_ERR_PARAMINVALID` — A parameter provided by the caller is
invalid.

:CODE:`YDB_ERR_SIMPLEAPINEST` – An attempt was made to nest Simple API
calls, which cannot be nested.

:CODE:`YDB_ERR_SUBSARRAYNULL` – The :code:`subs_used` parameter of a function
is greater than zero, but the :code:`subsarray` parameter is a NULL
pointer.

:CODE:`YDB_ERR_SVNOSET` — the application inappropriately attempted to
modify the value of an intrinsic special variable such as an attempt
to modify :code:`$trestart` using `ydb_set_s()`_.

:CODE:`YDB_ERR_TIME2LONG` – This return code indicates that a value
greater than :CODE:`YDB_MAX_TIME_NSEC` was specified for a time duration.

:CODE:`YDB_ERR_TPTIMEOUT` — This return code from `ydb_tp_s()`_ indicates
that the transaction took too long to commit.

:CODE:`YDB_ERR_UNIMPLOP` — An operation that is not supported for an
intrinsic special variable – of the `Simple API`_ functions only
`ydb_get_s()`_ and `ydb_set_s()`_ are supported – was attempted on an
intrinsic special variable.

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

:CODE:`YDB_MAX_NAMES` – The maximum number of variable names that can be
passed to `ydb_delete_excl_s()`_ or `ydb_tp_s()`_.

:CODE:`YDB_MAX_STR` — The maximum length of a string (or blob) in bytes. A
caller to :code:`ydb_get_s()` whose :code:`*ret_value` parameter provides a
buffer of :CODE:`YDB_MAX_STR` will never get a :CODE:`YDB_ERR_INVSTRLEN`
error.

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

---------
Severity
---------

Symbolic constants for the severities of message numbers in return
codes and :code:`$zstatus` are prefixed with :CODE:`YDB_SEVERITY_`.

:CODE:`YDB_SEVERITY_ERROR` – The number corresponds to an error from which the
process can recover.

:CODE:`YDB_SEVERITY_FATAL` – The number corresponds to an error that terminated
the process.

:CODE:`YDB_SEVERITY_INFORMATION` – The number corresponds to an informational
message.

:CODE:`YDB_SEVERITY_SUCCESS` – The number corresponds to the successful
completion of a requested opertion.

:CODE:`YDB_SEVERITY_WARNING` – The number corresponds to a warning, i.e.,
it indicates a possible problem.

------
Other
------

Other symbolic constants have a prefix of :CODE:`YDB_`.

:CODE:`YDB_DEL_NODE` and :CODE:`YDB_DEL_TREE` — As values of the :code:`deltype`
parameter, these values indicate to :code:`ydb_delete_s()` whether to
delete an entire subtree or just the node at the root, leaving the
subtree intact.

:CODE:`YDB_NODE_END` — In the event a call to :code:`ydb_node_next_s()` or
:code:`ydb_node_previous_s()` wish to report that there no further nodes,
the :code:`*ret_subs_used` parameter is set to this value. Application code
should make no assumption about this constant other than that it is
negative (<0).


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

:code:`ydb_tpfnptr_t` is a pointer to a function with one parameter, a
pointer, and which returns an integer, defined thus:

.. code-block:: C
		
	typedef int (*ydb_tpfnptr_t)(void *tpfnparm);


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

:code:`YDB_LITERAL_TO_BUFFER(literal, buffer)` – Use this macro to set a
:code:`ydb_buffer_t` structure to refer to a literal (such as a
:code:`variable name`). With a string :code:`literal`, and a
:code:`buffer` pointer to a :code:`ydb_buffer_t` structure,
:code:`set`:

- :code:`buffer->buf_addr` to the address of :code:`literal`; and
- :code:`buffer->len_used` and :code:`buffer->len_alloc` to the length of
  :code:`literal` excluding the terminating null byte.

:code:`YDB_IS_YDBERR(msgnum)` – returns TRUE if the absolute value of
:code:`msgnum` lies between :code:`YDB_MIN_YDBERR` and
:code:`YDB_MAX_YDBERR`.

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

Function names specific to the YottaDB Simple API end in :code:`_s`.

------------
ydb_data_s()
------------

.. code-block:: C

	int ydb_data_s(ydb_buffer_t *varname,
		int subs_used,
		ydb_buffer_t *subsarray,
		unsigned int *ret_value);

In the location pointed to by :code:`ret_value`, :code:`ydb_data_s()` returns the
following information about the local or global variable node
identified by :code:`*varname`, :code:`subs_used` and :code:`*subsarray`.

- 0 — There is neither a value nor a subtree, i.e., it is undefined.
- 1 — There is a value, but no subtree
- 10 — There is no value, but there is a subtree.
- 11 — There are both a value and a subtree.

It is an error to call :code:`ydb_data_s()` on an intrinsic special
variable; doing so results in the :CODE:`YDB_ERR_UNIMPLOP`
error. :code:`ydb_data_s()` returns :CODE:`YDB_OK` or an `error return code`_.

--------------
ydb_delete_s()
--------------

.. code-block:: C

	int ydb_delete_s(ydb_buffer_t *varname,
		int subs_used,
		ydb_buffer_t *subsarray,
		int deltype);

Deletes nodes in the local or global variable tree or subtree
specified. A value of :CODE:`YDB_DEL_NODE` or :CODE:`YDB_DEL_TREE` for
:code:`deltype` specifies whether to delete just the node at the root,
leaving the (sub)tree intact, or to delete the node as well as the
(sub)tree.

Intrinsic special variables cannot be deleted.

:code:`ydb_delete_s()` returns :CODE:`YDB_OK`, a :CODE:`YDB_ERR_UNIMPLOP` if
:code:`deltype` is neither :CODE:`YDB_DEL_NODE` nor :CODE:`YDB_DEL_TREE`, or
another `error return code`_.

-------------------
ydb_delete_excl_s()
-------------------

.. code-block:: C

	int ydb_delete_excl_s(int namecount,
		ydb_buffer_t *varnames);

:code:`ydb_delete_excl_s()` deletes the trees of all local variables
except those in the :code:`*varnames` array. It is an error for
:code:`*varnames` to include a global or intrinsic special variable.

In the special case where :code:`namecount` is zero,
:code:`ydb_delete_excl_s()` deletes all local variables.

If your application mixes M and non M code, and you wish to use
:code:`ydb_delete_excl_s()` to delete local variables that are aliases,
formal parameters, or actual parameters passed by reference, make sure
you understand what (sub)trees are being deleted. This warning does
not apply to applications that do not include M code.

:code:`ydb_delete_excl_s()` returns :CODE:`YDB_OK`,
:CODE:`YDB_ERR_NAMECOUNT2HI` if more
than :CODE:`YDB_MAX_NAMES` are specified, or another `error return
code`_.

Note that specifying a larger value for :code:`namecount` than the
number of variable names actually provided in :code:`*varnames`
can result in a buffer overflow.

-----------
ydb_get_s()
-----------

.. code-block:: C

	int ydb_get_s(ydb_buffer_t *varname,
		int subs_used,
		ydb_buffer_t *subsarray,
		ydb_buffer_t *ret_value);

To the location pointed to by :code:`ret_value->buf_addr`, :code:`ydb_get_s()`
copies the value of the specified node or intrinsic special variable,
setting :code:`ret_value->len_used` on both normal and error returns
(the latter case as long as the data exists). Return values are:

- :CODE:`YDB_OK` for a normal return;
- :CODE:`YDB_ERR_GVUNDEF`, :CODE:`YDB_ERR_INVSVN`, or :CODE:`YDB_ERR_LVUNDEF` as
  appropriate if no such variable or node exists;
- :CODE:`YDB_ERR_INVSTRLEN` if :code:`ret_value->len_alloc` is insufficient for
  the value at the node; or
- another applicable `error return code`_.

Notes:

- In the unlikely event an application wishes to know the length of
  the value at a node, but not access the data, it can call
  :code:`ydb_get_s()` and provide an output buffer
  (:code:`retvalue->len_alloc`) with a length of zero, since even in
  the case of a :CODE:`YDB_ERR_INVSTRLEN` error,
  :code:`retvalue->len_used` is set.
- Within a transaction implemented by `ydb_tp_s()`_ application
  code observes stable data at global variable nodes because YottaDB
  `transaction processing`_ ensures ACID properties.
- Outside a transaction, a global variable node can potentially be
  changed by another, concurrent, process between the time that a process
  calls :code:`ydb_data_s()` to ascertain the existence of the data and a
  subsequent call to :code:`ydb_get_s()` to get that data. A caller of
  :code:`ydb_get_s()` to access a global variable node should code in
  anticipation of a potential :CODE:`YDB_ERR_GVUNDEF`.

------------
ydb_incr_s()
------------

.. code-block:: C

	int ydb_incr_s(ydb_buffer_t *varname,
		int subs_used,
		ydb_buffer_t *subsarray,
		ydb_buffer_t *increment,
		ydb_buffer_t *ret_value);

:code:`ydb_incr_s()` atomically:

- converts the value in the specified node to a number if it is not
  one already, using a zero value if the node does not exist;
- increments it by the value specified by :code:`*increment`, converting
  the value to a number if it is not a `canonical number`_, defaulting to
  1 if the parameter is NULL; and
- storing the value as a canonical number in :code:`*ret_value`.

Return values:

- The normal return value is :CODE:`YDB_OK`.
- If the atomic increment results in a numeric overflow, the function
  returns a :CODE:`YDB_ERR_NUMOFLOW` error; in this case, the value in the
  node is untouched and that in :code:`*ret_value` is unreliable.
- :CODE:`YDB_ERR_INVSTRLEN` if :code:`ret_value->len_alloc` is
  insufficient for the result. As with `ydb_get_s()`_, in this case
  :CODE:`ret_value->len_used` is set to the required length.
- Other errors return the corresponding `error return code`_.

Notes:

- Intrinsic special variables cannot be atomically incremented, and an
  attempt to do so returns the :CODE:`YDB_ERR_UNIMPLOP` error.

------------
ydb_lock_s()
------------

.. code-block:: C

	int ydb_lock_s(unsigned long long timeout_nsec,
		int namecount[,
		[ydb_buffer_t *varname,
		int subs_used,
		ydb_buffer_t *subsarray], ...]);

:code:`namecount` is the number of variable names in the call.

Release any locks held by the process, and attempt to acquire all the
requested locks. Except in the case of an error or a
:CODE:`YDB_LOCK_TIMEOUT` return value, the release is unconditional. On
return, the function will have acquired all requested locks or none of
them. If no locks are requested (:code:`namecount` is zero), the function
releases all locks and returns :CODE:`YDB_OK`.

:code:`timeout_nsec` specifies a time in nanoseconds that the function waits
to acquire the requested locks. If it is not able to acquire all
requested locks, it acquires no locks, returning with a
:CODE:`YDB_LOCK_TIMEOUT` return value.

If :code:`timeout_nsec` is zero, the function makes exactly one attempt to
acquire the locks, and if it is unable to, it returns
:CODE:`YDB_LOCK_TIMEOUT`.

If all requested locks are successfully acquired, the function returns
:CODE:`YDB_OK`. If the requested :code:`timeout_nsec` exceeds
:code:`YDB_MAX_TIME_NSEC`, the function immediately returns
:code:`YDB_ERR_TIME2LONG`. In other cases, the function returns an
`error return code`_.

-----------------
ydb_lock_decr_s()
-----------------

.. code-block:: C

	int ydb_lock_decr_s(ydb_buffer_t *varname,
		int subs_used,
		ydb_buffer_t *subsarray);

Decrements the count of the specified lock held by the process. As
noted in the `Concepts`_ section, a lock whose count goes from 1 to 0
is released. A lock whose name is specified, but which the process
does not hold, is ignored.

As releasing a lock cannot fail, the function returns :CODE:`YDB_OK`,
unless there is an error such as an invalid name that results in the
return of an error code such as :CODE:`YDB_ERR_INVVARNAME`. Errors
result in an appropriate `error return code`_.

-----------------
ydb_lock_incr_s()
-----------------

.. code-block:: C

	int ydb_lock_incr_s(unsigned long long timeout_nsec,
		ydb_buffer_t *varname,
		int subs_used,
		ydb_buffer_t *subsarray);

Without releasing any locks held by the process, attempt to acquire
the requested lock incrementing it if already held.

:code:`timeout_nsec` specifies a time in nanoseconds that the function waits
to acquire the requested lock. If it is not able to acquire the lock,
it returns with a :CODE:`YDB_LOCK_TIMEOUT` return value.

If :code:`timeout_nsec` is zero, the function makes exactly one attempt to
acquire the lock, and if unable to, it returns :CODE:`YDB_LOCK_TIMEOUT`.

If the requested lock is successfully acquired, the function returns
:CODE:`YDB_OK`.  If the requested :code:`timeout_nsec` exceeds
:code:`YDB_MAX_TIME_NSEC`, the function immediately returns
:code:`YDB_ERR_TIME2LONG`. Errors result in an appropriate `error
return code`_.

-----------------
ydb_node_next_s()
-----------------

.. code-block:: C

	int ydb_node_next_s(ydb_buffer_t *varname,
		int subs_used,
		ydb_buffer_t *subsarray,
		int *ret_subs_used,
		ydb_buffer_t *ret_subsarray);

:code:`ydb_node_next_s()` facilitates depth-first traversal of a local or
global variable tree. As the number of subscripts can differ between
the input node of the call and the output node reported by the call
:code:`*ret_subs_used` is an input as well as an output parameter:

- On input, :code:`*ret_subs_used` specifies the number of elements
  allocated for returning the subscripts of the next node.
- On normal output (:CODE:`YDB_OK` return code),
  :code:`*ret_subs_used` contains the actual number of subscripts
  returned or is :CODE:`YDB_NODE_END`. See below for error return
  codes.

Return values of :code:`ydb_node_next_s()` are:

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
- Another `error return code`_, in which case the application should
  consider the values of :code:`*ret_subs_used` and the :code:`*ret_subsarray`
  to be undefined.

---------------------
ydb_node_previous_s()
---------------------

.. code-block:: C

	int ydb_node_previous_s(ydb_buffer_t *varname,
		int subs_used,
		ydb_buffer_t *subsarray,
		int *ret_subs_used,
		ydb_buffer_t *ret_subsarray);

Analogous to :code:`ydb_node_next(s)`, :code:`ydb_node_previous_s()`
facilitates reverse breadth-first traversal of a local or global
variable tree, except that :code:`ydb_node_previous_s()` searches for and
reports the predecessor node. Unlike :code:`ydb_node_next_s()`,
:code:`*ret_subs_used` can be zero if an expected previous node is the
unsubscripted root.

Return values of :code:`ydb_node_previous_s()` are:

- :CODE:`YDB_OK` with the previous node, if there is one, changing
  :code:`*ret_subs_used` and :code:`*ret_subsarray` parameters to those of the
  previous node. If there is no previous node (i.e., the input node is the
  first), :code:`*ret_subs_used` on output is :CODE:`YDB_NODE_END`.
- :CODE:`YDB_ERR_INSUFFSUBS` if :code:`*ret_subs_used` specifies
  insufficient parameters to return the subscript. In this case
  :code:`*ret_subs_used` reports the actual number of subscripts required.
- :CODE:`YDB_ERR_INVSTRLEN` if one of the :code:`ydb_buffer_t` structures
  pointed to by :code:`*ret_subsarray` does not have enough space for the
  subscript. In this case, :code:`*ret_subs_used` is the index into the
  :code:`*ret_subsarray` array with the error, and the :code:`len_used` field
  of that structure specifies the size required.
- Another `error return code`_, in which case the application should
  consider the values of :code:`*ret_subs_used` and the :code:`*ret_subsarray`
  to be undefined.

-----------
ydb_set_s()
-----------

.. code-block:: C

	int ydb_set_s(ydb_buffer_t *varname,
		int subs_used,
		ydb_buffer_t *subsarray,
		ydb_buffer_t *value);

Copies the :code:`value->len_used` bytes at :code:`value->buf_addr` as the
value of the specified node or intrinsic special variable specified. A
NULL :code:`value` parameter is treated as equivalent to one that points
to a :code:`ydb_buffer_t` specifying an empty string. Return values are:

- :CODE:`YDB_OK` for a normal return;
- :CODE:`YDB_ERR_INVSVN` if no such intrinsic special variable exists; or
- another applicable `error return code`_.

---------------
ydb_str2zwr_s()
---------------

.. code-block:: C

	int ydb_str2zwr_s(ydb_buffer_t *str, ydb_buffer_t *zwr);

In the buffer referenced by :code:`*zwr`, :code:`ydb_str2zwr_s()` provides the
`zwrite formatted`_ version of the string pointed to by :code:`*str`,
returning:

- :CODE:`YDB_OK`;
- :CODE:`YDB_ERR_INVSTRLEN` if the :code:`*zwr` buffer is not long enough;
- :CODE:`YDB_ERR_PARAMINVALID` if either :code:`zwr` or :code:`zwr->buf_addr` is
  null; or
- another applicable `error return code`_.

----------------------
ydb_subscript_next_s()
----------------------

.. code-block:: C

	int ydb_subscript_next_s(ydb_buffer_t *varname,
		int subs_used,
		ydb_buffer_t *subsarray,
		ydb_buffer_t *ret_value);

:code:`ydb_subscript_next_s()` provides a primitive for implementing
breadth-first traversal of a tree by searching for the next subscript
at the level specified by :code:`subs_used`, i.e., the next subscript
after the one referred to by :code:`subsarray[subs_used-1].buf_addr`. A
node need not exist at the subscripted variable name provided as input
to the function. If :code:`subsarray[subs_used-1].len_used` is zero,
:code:`ret_value->buf_addr` points to first node at that level with a
subscript that is not the empty string. :code:`ydb_subscript_next_s()`
returns :CODE:`YDB_OK` or an `error return code`_.

On return from :code:`ydb_subscript_next_s()` with a :CODE:`YDB_OK`, if
:code:`ret_value->len_used` is non-zero, :code:`ret_value->buf_addr` points to
the value of the next subscript. If it is zero, it means that there is
no node greater than the input node at that level.

In the special case where :code:`subs_used` is zero,
:code:`ret_value->buf_addr` points to the next local or global variable
name.

--------------------------
ydb_subscript_previous_s()
--------------------------

.. code-block:: C

	int ydb_subscript_previous_s(ydb_buffer_t *varname,
		int subs_used,
		ydb_buffer_t *subsarray,
		ydb_buffer_t *ret_value);

:code:`ydb_subscript_previous_s()` provides a primitive for implementing
reverse breadth-first traversal of a tree by searching for the
previous subscript at the level specified by :code:`subs_used`. i.e. the
subscript preceding the one referred to by
:code:`subsarray[subs_used-1].buf_addr`. A node need not exist at the
subscripted variable name provided as input to the function. If
:code:`subsarray[subs_used-1].len_used` is zero, :code:`ret_value->buf_addr`
points to last node at that level with a subscript that is not the
empty string. :code:`ydb_subscript_previous_s()` returns :CODE:`YDB_OK` or an
`error return code`_.

On return from :code:`ydb_subscript_previous_s()`, if
:code:`ret_value->len_used` is non-zero, :code:`ret_value->buf_addr` points to
the value of the previous subscript. If it is zero, it means that
there is no node less than the input node at that level.

Notes:

- If an application uses empty strings as subscripts, a subsequent
  call to :code:`ydb_data_s()` is required to determine whether the first
  subscript has been reached or whether the first subscript is a node
  with the empty string as a subscript.
- In the special case where :code:`subs_used` is zero,
  :code:`ret_value->buf_addr` points to the previous local or global
  variable name.

----------
ydb_tp_s()
----------

.. code-block:: C

	int ydb_tp_s(ydb_tpfnptr_t tpfn,
		void *tpfnparm,
		const char *transid,
		int namecount,
		ydb_buffer_t *varnames);

:code:`ydb_tp_s()` calls the function pointed to by :code:`tpfn` passing it
:code:`tpfnparm` as a parameter. As discussed under `Transaction
Processing`_, the function should use the intrinsic special variable
:code:`$trestart` to manage any externally visible action (which YottaDB
recommends against, but which may be unavoidable). The function
pointed to by :code:`tpfn` should return one of the following:

- :CODE:`YDB_OK` — application logic indicates that the transaction can
  be committed (the YottaDB engine may still decide that a restart is
  required to ensure ACID transaction properties) as discussed under
  `Transaction Processing`_.
- :CODE:`YDB_TP_RESTART`  — application logic indicates that the
  transaction should restart.
- :CODE:`YDB_TP_ROLLBACK` — application logic indicates that the
  transaction should not be committed.
- An `error return code`_ returned by a YottaDB function called by the
  function pointed to by :code:`tpfn`.

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

A top level :code:`ydb_tp_s()` can return :CODE:`YDB_OK`, :CODE:`YDB_TP_ROLLBACK`,
:CODE:`YDB_ERR_TPTIMEOUT` (see `Transaction Processing`_), or an `error
return code`_, including :CODE:`YDB_ERR_NAMECOUNT2HI`. A :code:`ydb_tp_s()`
call that is within another transaction can also return
:CODE:`YDB_TP_RESTART` to its caller. [#]_

.. [#] An enclosing transaction can result not just from another
       :code:`ydb_tp_s()` higher in the stack, but also from an M
       :code:`tstart` command as well as a database trigger resulting from
       a `ydb_delete_s()`_, or `ydb_set_s()`_.

---------------
ydb_zwr2str_s()
---------------

.. code-block:: C

	int ydb_zwr2str_s(ydb_buffer_t *zwr, ydb_buffer_t *str);

In the buffer referenced by :code:`*str`, :code:`ydb_zwr2str_s()` provides the
string described by the `zwrite formatted`_ string pointed to by
:code:`*zwr`, returning :CODE:`YDB_OK` (with :code:`str->len_used` set to zero if
the zwrite formatted string has an error), or the
:CODE:`YDB_ERR_INVSTRLEN` error if the :code:`*str` buffer is not long enough.

Comprehensive API
=================

The Comprehensive API is a project for the future.

Utility Functions
=================

Utility functions are functions that are not core to YottaDB
functionality, but which are useful to application code.

----------------
ydb_child_init()
----------------

.. code-block:: C

	int ydb_child_init(void *param)

As the YottaDB engine resides in the address space of the process,
child processes **must** call :code:`ydb_child_init()` to re-initialize
data structures after a :code:`fork()` or equivalent in other languages
(e.g., :code:`os.fork()` in Python).

Notes:

- A child process that fails to call :code:`ydb_child_init()` after a
  :code:`fork()` can cause structural damage to database files, as well as
  other possible side-effects.
- After a :code:`fork()`, a parent process should not exit until the child
  process has executed :code:`ydb_child_init()`. One way to
  implement this would be for the parent to set a node such as
  :code:`^Proc(ppid)=1` where :code:`ppid` is the parent's pid, and for the
  child to set it to zero or to delete the node. A parent process that
  wishes to :code:`fork()` a number of child processes can use
  :code:`ydb_incr_s()` to increment a node such as :code:`^Proc(ppid)` and
  each child can decrement it after executing
  :code:`ydb_child_init()`. When the value at the node is zero, the parent
  process knows that it is safe for it to exit.

The :code:`void *param` is reserved for future enhancements. As the
initial release of YottaDB ignores it, we recommend using
NULL. :code:`ydb_child_init()` returns :CODE:`YDB_OK` or an `error return
code`_.

----------
ydb_exit()
----------

.. code-block:: C

	int ydb_exit(void)

When a caller no longer wishes to use YottaDB, a call to
:code:`ydb_exit()` cleans up the process connection/access to all
databases and cleans up its data structures. Therafter, any attempt to
call a YottaDB function produces a :CODE:`YDB_ERR_CALLINAFTEREXIT`
error.

Note that a typical application should not need to call
:code:`ydb_exit()`, but should instead just terminate with a call to
:code:`exit()` which will perform any cleanup needed by YottaDB.

------------------
ydb_file_id_free()
------------------

.. code-block:: C

	int ydb_file_id_free(ydb_fileid_ptr_t fileid)

Releases the memory used by a :code:`fileid` structure previously
generated by `ydb_file_name_to_id()`_. Calling the function twice for
the same pointer, unless it has been returned a second time by a
different `ydb_file_name_to_id()`_ is an application error with
undefined consequences.

-----------------------
ydb_file_is_identical()
-----------------------

.. code-block:: C

	int ydb_file_is_identical(ydb_fileid_ptr_t fileid1,
		ydb_fileid_ptr_t fileid2)

Given two pointers to :code:`fileid` structures (see
`ydb_file_name_to_id()`_), :code:`ydb_file_is_identical` returns YDB_OK if
the two :code:`fileid` structures are the same file.

---------------------
ydb_file_name_to_id()
---------------------

.. code-block:: C

	int ydb_file_name_to_id(ydb_string_t *filename,
	ydb_fileid_ptr_t *fileid)

As a file is in principle reachable through different paths, and
application code may need to check whether two paths do indeed lead to
the same file, YottaDB provides a mechanism to do so. Provided with a
path to a file, YottaDB creates an internal structure called a
:code:`fileid` that uniquely identifies the file if such a structure does
not already exist for that file, and provides the caller with a
pointer to that structure. The layout and contents of the fileid
structure are opaque to the caller, which **must not** modify the
pointer or the structure it points to.

When the :code:`fileid` structure for a file is no longer needed, an
application should call `ydb_file_id_free()`_ to release the structure
and avoid a memory leak.

:code:`ydb_file_name_to_id()` returns :CODE:`YDB_OK`, `YDB_NOTOK` if
:code:the :code:`filename` is NULL, or an `error return code`_.

-----------------
ydb_fork_n_core()
-----------------

.. code-block:: C

	void ydb_fork_n_core(void)

A core is a snapshot of a process, to help debug application code, for
example to troubleshoot an out-of-design condition. When a process
executes :code:`ydb_fork_n_core()`, it forks. The child process sends
itself a signal to generate a core and terminate. On termination of
the child process, the parent process continues execution. Note that
depending on the nature of the condition necessitating a core, an
:code:`exit()` may well be the right action for the parent process. An
:code:`exit()` call will drive YottaDB exit handlers to perform clean
shutdown of databases and devices the process has open.

The content, location, and naming of cores is managed by the operating
system – see :code:`man 5 core` for details. We recommend that you set
:code:`kernel.core_uses_pid` to 1 to make it easier to identify and
track cores. As cores will likely contain protected confidential
information, you *must* ensure appropriate configuration and
management of cores.

----------
ydb_free()
----------

.. code-block:: C

	void ydb_free(void *ptr)

Releases memory previously allocated by :code:`ydb_malloc()`. Passing
:code:`ydb_free()` a pointer not previously provided to the application by
:code:`ydb_malloc()` can result in unpredictable behavior. The signature
of :code:`ydb_free()` matches that of the POSIX :code:`free()` call.

-----------------
ydb_hiber_start()
-----------------

.. code-block:: C

	int ydb_hiber_start(unsigned long long sleep_nsec)

The process sleeps for the time in nanoseconds specified by
:code:`sleep_nsec`. If a value greater than :CODE:`YDB_MAX_TIME_NSEC` is
specified, :code:`ydb_hiber_start()` immediately returns with a
:CODE:`YDB_ERR_TIME2LONG` error; otherwise it returns :CODE:`YDB_OK` after
the elapsed time.

--------------------------
ydb_hiber_start_wait_any()
--------------------------

.. code-block:: C

	int ydb_hiber_start_wait_any(unsigned long long sleep_nsec)

The process sleeps for the time in nanoseconds specified by
:code:`sleep_nsec` or until it receives a signal. If a value greater than
:CODE:`YDB_MAX_TIME_NSEC` is specified, :code:`ydb_hiber_start()` immediately
returns with a :CODE:`YDB_ERR_TIME2LONG` error; otherwise it returns
:CODE:`YDB_OK` after the elapsed time or when the wait is terminated by a
signal.

----------
ydb_init()
----------

.. code-block:: C

	int ydb_init(void)

:code:`ydb_init()` initializes the YottaDB runtime environment. As YottaDB
automatically initializes the runtime on the first call to its API or
first M code invocation, there is usually no need to explicitly call
:code:`ydb_init()`. The exception is when an application wishes to set
its own signal handlers (see `Signals`_).

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

--------------------------
ydb_stdout_stderr_adjust()
--------------------------

.. code-block:: C

	int ydb_stdout_stderr_adjust(void)

The function checks whether stdout (file descriptor 1) and stderr
(file descriptor 2) are the same file, and if so, routes writes to
stderr to stdout instead. This ensures that output appears in the
order in which it was written; otherwise owing to IO buffering, output
can appear in an order different from that in which it was
written. Application code which mixes C and M code, and which
explicitly redirects stdout or stderr (e.g., using :code:`dup2()`), should
call this function as soon as possible after the
redirection. :code:`ydb_stdout_stderr_adjust()` returns :CODE:`YDB_OK`.

--------------------
ydb_thread_is_main()
--------------------

.. code-block:: C

   int ydb_thread_is_main(void)

Returns :CODE:`YDB_OK` if the thread is the main thread of the process,
and another value if the thread is not.
	
------------------
ydb_timer_cancel()
------------------

.. code-block:: C

	void ydb_timer_cancel(intptr_t timer_id)

Cancel a timer identified by :code:`timer_id` and previously started with
`ydb_timer_start()`_.

-----------------
ydb_timer_start()
-----------------

.. code-block:: C

	typedef void (*ydb_funcptr_retvoid_t)(intptr_t timer_id,
		unsigned int handler_data_len,
		char *handler_data);
	int ydb_timer_start(intptr_t timer_id,
		unsigned long long limit_nsec,
		ydb_funcptr_retvoid_t handler,
		unsigned int handler_data_len
		char *handler_data);

Starts a timer. Unless canceled, when the timer expires,
:code:`ydb_timer_start()` invokes a handler function, providing that
function with input data.

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
:code:`YDB_MAX_TIME_NSEC`, the function returns
:code:`YDB_ERR_TIME2LONG`; otherwise it returns :code:`YDB_OK`.

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
- :code:`SIGINT` – If the top level invocation of the process is the
  :code:`mumps` executable, the handler is the YottaDB Ctrl-C handler
  for M. Otherwise, if the handler is :code:`SIG_DFL`, it is replaced
  by the YottaDB Ctrl-C handler for M, and if it is something else,
  YottaDB does not change it.
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
- Handlers for all signals other than those mentioned above are set to
  :code:`SIG_IGN`. If an application sets a signal hander for anther
  signal, it *must* ensure that :code:`ydb_exit()` is explicitly
  called prior to process exit. An application can set its own signal
  handler after the first YottaDB API call.

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
- After a :code:`fork()`, the child process must immediately execute
  `ydb_child_init()`_ (see discussion at `ydb_child_init()`_).

Threads
=======

As the YottaDB runtime system is single-threaded, application code
must ensure that only one thread executes the YottaDB runtime code at
any given time; to avoid unpredictable results, any additional thread
that attempts to enter the YottaDB runtime system **must** be blocked
till the first thread returns from YottaDB.

As YottaDB reserves the right to make the runtime system
multi-threaded at a future date, you should ensure that your
application code does not rely on the single-threadedness of the
YottaDB runtime system. Also, while local variables are shared by all
threads that call into YottaDB, this behavior may or may not continue
if and when YottaDB makes the runtime system multi-threaded in the
future.

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
