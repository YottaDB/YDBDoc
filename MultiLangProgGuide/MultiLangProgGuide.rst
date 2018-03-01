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
the YottaDB engine from any language. As YottaDB adds standard APIs()
for other languages, additional sections will be added to the
Programmers Guide.

**Caveat:** This code does not exist yet. The user documentation is
being written ahead of the code, and will change in the event the code
needs to differ from this document for a valid technical reason. Also,
this document itself is incomplete and still evolving.

===========
Quick Start
===========

**The Quick Start section needs to be fleshed out.**

1. Install YottaDB.

- Create a temporary directory and change to it, e.g.: ``mkdir
  /tmp/tmp ; cd /tmp/tmp``
- Get the YottaDB installer: ``wget
  https://raw.githubusercontent.com/YottaDB/YottaDB/master/sr_unix/ydbinstall.sh``
- Make it executable: ``chmod +x ydbinstall.sh``
- Run it with your choice of directory where you want it installed
  (omit the ``--verbose`` option for less output): ``sudo
  ./ydbinstall.sh --installdir /opt/yottadb/ --utf8 default
  --verbose``

2. Choose a directory for your default environment and initialize it:
   ``export ydb_dir=$HOME/.yottadb ; . /opt/yottadb/latest/yottadbprofile``
#. ``#include`` the file ``/opt/yottadb/latest/libyottadb.h`` in your C
   program and compile it.
#. Run your program, ensuring either that ``libyottadb.so`` is in the
   load path of your program (e.g., using ``ldcache`` or the
   ``LD_LIBRARY_PATH`` environment variable), or that it is
   preloaded using ``LD_PRELOAD``.

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
example, in the example below, data may well be loaded by country.

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
``Capital("Belgium")="Brussels"``. The set of all nodes under a
variable is called a *tree* (so in the example, there are two trees,
one under ``Capital`` and the other under ``Population``). The set of
all nodes under a variable and a leading set of its subscripts is
called a *subtree* (e.g., ``Population("USA")`` is a subtree of the
``Population`` tree). [#]_

.. [#] Of course, the ability to represent the data this way does not
       in any way detract from the ability to represent the same data
       another way with which you are comfortable, such as XML or
       JSON. However, note while any data that can be represented in
       JSON can be stored in a YottaDB tree not all trees that YottaDB
       is capable of storing can be represented in JSON, or at least,
       may require some encoding in order to be represented in JSON.

With this representation, the ``Population`` tree can be represented as
follows:

::

    Population("Belgium")=1367000
    Population("Thailand")=8414000
    Population("USA")=325737000
    Population("USA",17900802)=3929326
    Population("USA",18000804)=5308483
    …
    Population("USA",20100401)=308745538

Note that the trees are displayed in breadth-first order. YottaDB has
functions for applications to traverse trees in both breadth-first and
depth-first order.

If the application designers now wish to enhance the application to
add historical dates for capitals, the ``Capital("Thailand")`` subtree
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

When viewed as ``["Capital","Belgium","Brussels"]`` each component is
a string, and in an abstract sense they are all conceptually the
same. When viewed as ``Capital("Belgium")="Brussels"`` differences
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
Thailand as ``Capital("Thailand","current")="Bangkok"`` instead of
``Capital("Thailand")="Bangkok"``, the above subtree would have the
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
distinct from a global variable of the same name. Thus ``^X`` can have
the value 1 and ``X`` can at the same time have the value ``"The quick
brown fox jumps over the lazy dog."`` For maintainability **YottaDB
strongly recommends that applications use different names for local
and global variables, except in the special case where a local
variable is an in-process cached copy of a corresponding global
variable.**

Global Directories
==================

To application software, files in a file system provide
persistence. This means that global variables must be stored in files
for persistence. A *global directory file* provides a process with a
mapping from the name of every possible global variable name to a
*database file*. A *database* is a set of database files to which
global variables are mapped by a global directory. Global directories
are created and maintained by a utility program called the Global
Directory Editor, which is discussed at length in the `YottaDB
Administration and Operations Guide
<https://docs.yottadb.com/AdminOpsGuide/>`_ and is outside the purview
of this document.

The name of the global directory file required to access a global
variable such as ``^Capital``, is provided to the process at startup
by the environment variable ``ydb_gbldir``.

In addition to the implicit global directory an application may wish
to use alternate global directory names. For example, consider an
application that wishes to provide an option to display names in other
languages while defaulting to English. This can be accomplished by
having different versions of the global variable ``^Capital`` for
different languages, and having a global directory for each
language. A global variable such as ``^Population`` would be mapped to
the same database file for all languages, but a global variable such
as ``^Capital`` would be mapped to a database file with
language-specific entries. So a default global directory
``Default.gld`` mapping a ``^Capital`` to a database file with English
names can be specified in the environment variable ``ydb_gbldir`` but
a different global directory file, e.g., ``ThaiNames.gld`` can have
the same mapping for a global variable such as ``^Population`` but a
different database file for ``^Capital``. The intrinsic special
variable ``$zgbldir`` can be set to a global directory name to change
the mapping from one global directory to another.

Thus, we can have:

::

   $zgbldir="ThaiNames.gld"
   ^Capital("Thailand")="กรุ่งเทพฯ"
   ^Capital("Thailand",1238,1378)="สุโขทัย"
   ^Capital("Thailand",1350,1767)="อยุธยา"
   ^Capital("Thailand",1767,1782)="ธนบุรี"
   ^Capital("Thailand",1782)="กรุ่งเทพฯ"

Intrinsic Special Variables
===========================

In addition to local and global variables, YottaDB also has a set of
*Intrinsic Special Variables*. Just as global variables are
distinguished by a "^" prefix, intrinsic special variables are
distinguished by a "$" prefix.  Unlike local and global variable
names, intrinsic special variable names are case-insensitive and so
``$zgbldir`` and ``$ZGblDir`` refer to the same intrinsic special
variable. Intrinsic special variables have no subscripts.

While the majority of intrinsic special variables as enumerated in
Chapter 8 (Intrinsic Special Variables) of `YottaDB M Programmers Guide
<https://docs.yottadb.com/ProgrammersGuide/UNIX_manual/>`_ are
useful to M application code, others are more generally useful and
documented here.

-------
$tlevel
-------

Application code can read the intrinsic special variable ``$tlevel``
to determine whether it is executing inside a
transaction. ``$tlevel>0`` means that it is inside a transaction, and
``$tlevel>1`` means that it is inside a nested transaction. Note that
a transaction can be started explicitly, e.g., by calling
`ydb_tp_s()`_ ,or implicitly by a trigger resulting from a
`ydb_delete_s()`_, or `ydb_set_s()`_.

---------
$trestart
---------

Application code inside a transaction can read the intrinsic special
variable ``$trestart`` to determine how many times a transaction has
been restarted. Although YottaDB recommends against accessing external
resources within a transaction, logic that needs to access an external
resource (e.g., to read data in a file), or to acquire a lock, can use
``$trestart`` to restrict that access or acquisition to the first time
it executes (``$trestart=0``).

--------
$zgbldir
--------

``$zgbldir`` is the name of the current global directory file; any
global variable reference that does not explicitly specify a global
directory uses $zgbldir. For example, instead of using an extended
reference, an application can set an intrinsic special variable
``$zgbldir="ThaiNames.gld"`` to use the ``ThaiNames.gld`` mapping. At
process startup, YottaDB initializes ``$zgbldir`` from the environment
variable value ``$ydb_gbldir``.

--------
$zstatus
--------

``$zstatus`` provides additional details of the last
error. Application code can retrieve ``$zstatus`` using
`ydb_get_s()`_. ``$zstatus`` typically consists of three
comma-separated substrings.

- The first is an error number. Application code can use the
  `ydb_message()`_ function to get more detailed information.
- C application code should ignore the second substring.
- The third substring is more detailed information about the error.

After retrieving ``$zstatus`` and acting on the error, application
code should clear it (set it to the empty string using `ydb_set_s()`_)
in preparation for any subsequent error.

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

- If the function returns a ``YDB_OK``, YottaDB attempts to commit
  the transaction. If it is unable to commit as described above, or if
  the called function returns a ``YDB_TP_RESTART`` return code, it
  calls the function again.
- If the function returns a ``YDB_TP_ROLLBACK``, `ydb_tp_s()`_ returns
  to its caller with that return code.
- To protect applications against poorly coded transactions, if a
  transaction takes longer than the number of seconds specified by
  the environment variable ``ydb_maxtptime``, YottaDB aborts the
  transaction and the `ydb_tp_s()`_ function returns the
  ``YDB_ERR_TPTIMEOUT`` error.

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
lock has a name: as lock names have the same syntax local or global
variable names, ``Population``, ``^Capital``, and
``^Capital("Thailand",1350,1767)`` are all valid lock
names. Features of YottaDB locks include:

- Locks are exclusive: one and only process can acquire a lock with the
  resource name. For example, if process P1 acquires lock ``Population("USA")``,
  process P2 cannot simultaneously acquire that lock. However, P2 can acquire
  lock ``Population("Canada")`` at the same time that process P1 acquires
  ``Population("USA")``.
- Locks are hierarchical: a process that has a lock at a higher level
  blocks locks at lower levels and vice versa. For example, if a
  process P0 must wait for processes P1, P2, … to complete, each of
  P1, P2, … can acquire lock ``Process(``\ *pid*\ ``)``. P0's
  subsequent attempt to acquire lock ``Process`` is blocked till
  processes P1, P2, … complete.
- Locks include counters: a process that acquires
  ``^Capital("Belgium")`` can acquire that lock again, incrementing
  its count to 2. This simplifies application code logic: for example,
  a routine in application code that requires ``^Capital("Belgium")``
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
called ``^Population`` to protect or restrict access to a global
variable called ``^Population``. [#]_

.. [#] Since a process always has exclusive access to its local
       variables, access to them never needs protection from a
       lock. So, it would be reasonable to use a lock ``Population``
       to restrict access to the global variable ``^Population``.

Since YottaDB locks acquisitions are always timed for languages other
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
  after the transaction is committed or abandoned. Locks acquired
  inside a transaction can be released either inside the transaction,
  or after the transaction is committed or abandoned.
- As repeated acquisitions of the same lock during retries of a
  transaction will result in the lock count being incremented each
  time, we recommend either matching lock acquisition and releases
  within a transaction, or, for locks acquired within a transaction but
  released after the transaction is committed or abandoned, to
  acquisition only on the first attempt, using the intrinsic special
  variable `$trestart`_.

==================
Symbolic Constants
==================

The ``yottadb.h`` file defines several symbolic constants, which are
one of the following types:

- Function Return Codes, which in turn are one of:

  + Normal Return Codes
  + Error Return Codes

- Limits
- Other

Symbolic constants all fit within the range of a C ``int``.


Function Return Codes
=====================

Return codes from calls to YottaDB are of type
``int``. Normal return codes are non-negative (greater than
or equal to zero); error return codes are negative.

-------------------
Normal Return Codes
-------------------

Symbolic constants for normal return codes have ``YDB_`` prefixes
other than ``YDB_ERR_``.

``YDB_LOCK_TIMEOUT`` — This return code from lock acquisition
functions indicates that the specified timeout was reached without
requested locks being acquired.

``YDB_OK`` — This the standard return code of all functions following
successful execution.

``YDB_TP_RESTART`` — Code returned to YottaDB by an application
function that packages a transaction to indicate that it wishes
YottaDB to restart the transaction, or by a YottaDB function
invoked within a transaction to its caller that the database engine
has detected that it will be unable to commit the transaction and will
need to restart. Application code designed to be executed within a
transaction should be written to recognize this return code and in
turn return to the YottaDB `ydb_tp_s()`_ invocation from which it
was called. See `Transaction Processing`_ for a discussion of
restarts.

``YDB_TP_ROLLBACK`` — Code returned to YottaDB by an application
function that packages a transaction, and in turn returned to the
caller indicating that the transaction should not be committed.

.. _error return code:

.. _error return codes:

------------------
Error Return Codes
------------------

Symbolic constants for error codes returned by calls to YottaDB are
prefixed with ``YDB_ERR_`` and are all less than zero. [#]_ The
symbolic constants below are not a complete list of all error messages
that Simple API functions can return — error return codes can indicate
system errors and database errors, not just application errors. Also,
some of the errors listed below can be raised in other circumstances
as well. A full set of error messages is in the `YottaDB Messages and
Recovery Procedures Manual
<https://docs.yottadb.com/MessageRecovery/>`_.

The ``ydb_message()`` function provides a way to get more
detailed information about any error code returned by a Simple API
function, including error codes for return values without symbolic
constants.

.. [#] Note for implementers: the actual values are negated ZMESSAGE
       error codes.

``YDB_ERR_GVUNDEF`` — No value exists at a requested global variable
node.

``YDB_ERR_INSUFFSUBS`` — A call to ``ydb_node_next_s()`` or
``ydb_node_previous_s()`` did not provide enough parameters for the
return values. [#]_

.. [#] Note for implementers: this is a new error, not currently in
       the code base.

.. _YDB_ERR_INVSTRLEN:

``YDB_ERR_INVSTRLEN`` — A buffer provided by the caller is not long
enough for a string to be returned, or the length of a string passed
as a parameter exceeds ``YDB_MAX_STR``. In the event the return code
is ``YDB_ERR_INVSTRLEN`` and if ``*xyz`` is a ``ydb_buffer_t``
structure whose ``xyz->len_alloc`` indicates insufficient space, then
``xyz->len_used`` is set to the size required of a sufficiently large
buffer. In this case the ``len_used`` field of a ``ydb_buffer_t``
structure is greater than the ``len_alloc`` field, and the caller is
responsible for correcting the ``xyz->len_used`` field.

``YDB_ERR_INVSUB`` — A subscript provided by the caller is invalid. In
the case of a name with multiple subscripts, the intrinsic special
variable $zstatus acquired with a subsequent call to `ydb_get_s()`_
provides details on which subscript had the invalid value.

``YDB_ERR_INVSVN`` — A special variable name provided by the caller
is invalid.

``YDB_ERR_INVVARNAME`` — A variable name provided by the caller is
invalid. In the case of a call with multiple variable names, such as
`ydb_lock_s()`_, the intrinsic special variable $zstatus acquired with
a subsequent call to `ydb_get_s()`_ provides details on which variable
name was invalid.

``YDB_ERR_KEY2BIG`` — The length of a global variable name and
subscripts exceeds the limit configured for the database region to
which it is mapped.

``YDB_ERR_LVUNDEF`` — No value exists at a requested local variable
node. [#]_

.. [#] Note for implementers: under the covers, this is ``UNDEF`` but
       renamed to be more meaningful.

``YDB_ERR_MAXNRSUBSCRIPTS`` — The number of subscripts specified in
the call exceeds ``YDB_MAX_SUBS``.

``YDB_ERR_NUMOFLOW`` — a `ydb_incr_s()`_ operation resulted in a
numeric overflow.

``YDB_ERR_SVNOSET`` — the application inappropriately attempted to
modify the value of an intrinsic special variable such as an attempt
to increment ``$trestart`` using `ydb_incr_s()`_.

``YDB_ERR_TIMEOUT2LONG`` – This return code indicates that a value
greater than ``YDB_MAX_LOCKTIME`` was specified for a lock timeout.

``YDB_ERR_TPTIMEOUT`` — This return code from `ydb_tp_s()`_ indicates
that the transaction took too long to commit.

``YDB_ERR_UNIMPLOP`` — A `ydb_data_s()`_ or `ydb_incr_s()`_ was
attempted on an intrinsic special variable.

``YDB_ERR_UNKNOWN`` — A call to `ydb_message()`_ specified an
invalid message code.


Limits
======

Symbolic constants for limits are prefixed with ``YDB_MAX_``.

``YDB_MAX_IDENT`` — The maximum space in bytes required to store a
complete variable name, not including the preceding caret for a global
variable. Therefore, when allocating space for a string to hold a
global variable name, add 1 for the caret.

``YDB_MAX_LOCKTIME`` — The maximum value in nanoseconds that an
application can instruct libyottab to wait until the process is able
to acquire locks it needs before timing out.

``YDB_MAX_STR`` — The maximum length of a string (or blob) in
bytes. A caller to ``ydb_get()`` that provides a buffer of
``YDB_MAX_STR`` will never get a ``YDB_ERR_INVSTRLEN``
error.

``YDB_MAX_SUBS`` — The maximum number of subscripts for a local or
global variable.

Other
=====

Other symbolic constants have a prefix of ``YDB_``.

``YDB_DEL_NODE`` and ``YDB_DEL_TREE`` — As values of the ``deltype``
parameter, these values indicate to ``ydb_delete_s()`` and
``ydb_delete_multi_s()`` whether to delete an entire subtree or just
the node at the root, leaving the subtree intact.

``YDB_NODE_END`` — In the event a call to ``ydb_node_next_s()`` or
``ydb_node_previous_s()`` wish to report that there no further nodes,
the ``*ret_subs`` parameter is set to this value. Application code
should make no assumption about this constant other than that it is
negative (<0).

==================================
Data Structures & Type Definitions
==================================

``ydb_buffer_t`` is a descriptor for a string [#]_ value, and consists of
the following fields:

 - ``address`` — pointer to an ``unsigned char``, the starting
   address of a string.
 - ``len_alloc`` and ``len_used`` — fields of type ``unsigned int`` where
   ``len_alloc`` ≥ ``len_used`` except when a `YDB_ERR_INVSTRLEN`_ occurs.

.. [#] Strings in YottaDB are arbitrary sequences of bytes that are not
       null-terminated. Other languages may refer to them as binary
       data or blobs.

``ydb_string_t`` is a descriptor for a string provided for
compatibility with existing code, and consists of the following
fields: [#]_

- ``address`` — pointer to an ``unsigned char``, the starting
   address of a string.
- ``length`` — the length of the string starting at the ``address`` field.

.. [#] Note for implementers: ``ydb_string_t`` is the same structure
       as ``gtm_string_t``.

``ydb_tpfnptr_t`` is a pointer to a function with one parameter, a
pointer, and which returns an integer, defined thus:

.. code-block:: C
		
	typedef int (*ydb_tpfnptr_t)(void *tpfnparm);

======
Macros
======

``YDB_BUFFER_ALLOC_TO_STRING(ydbstring, ydbbuffer)`` — With
``ydbstring`` a pointer to a ``ydb_string_t`` structure and
``ydbbuffer`` a pointer to a ``ydb_buffer_t`` structure, set:

- ``ydbstring->address=ydbbuffer->buf_addr``, and
- ``ydb_string->length=ydbbuffer->len_alloc`` (i.e., no changes to
  ``ydbbuffer``).

``YDB_BUFFER_FREE(ydbbuffer)`` — using `ydb_free()`_ free the memory
at ``ydbbuffer->buf_addr`` and set ``ydbbuffer->buf_addr``,
``ydbbuffer->len_alloc``, and ``ydbbuffer->len)used`` to zero.

``YDB_BUFFER_NEW(ydbbuffer,size)`` — using `ydb_malloc()`_ allocate 
memory of ``size`` bytes and set:

- ``ydbbuffer->buf_addr`` to the address of the allocated memory,
- ``ydbbuffer->len_alloc`` to ``size``, and
- ``ydbbuffer->len_used`` to zero.

``YDB_BUFFER_USED_TO_STRING(ydbstring, ydbbuffer)`` — With
``ydbstring`` a pointer to a ``ydb_string_t`` structure and
``ydbbuffer`` a pointer to a ``ydb_buffer_t`` structure, set:

- ``ydbstring->address=ydbbuffer->buf_addr``, and
- ``ydb_string->length=ydbbuffer->len_used`` (i.e., no changes to
  ``ydbbuffer``).

``YDB_STRING_FREE(ydbstring)`` — using `ydb_free()`_ free the memory
at ``ydbstring->address`` and set ``ydbstring->address``,
``ydbstring->length`` to zero.

``YDB_STRING_NEW(ydbstring,size)`` — using `ydb_malloc()`_ allocate 
memory of ``size`` bytes and set:

- ``ydbstring->address`` to the address of the allocated memory, and
- ``ydbstring->length`` to ``size``.

``YDB_STRING_TO_BUFFER(ydbbuffer, ydbstring, used)`` — With ``ydbbuffer``
a pointer to a ``ydb_buffer_t`` structure, ``ydbstring`` a pointer to
a ``ydb_string_t`` structure, and ``used`` an unsigned integer, set:

- ``ydbbuffer->buf_addr=ydbstring->address``,
- ``ydbbuffer->len_alloc=ydbstring->used``, and
- ``ydbbuffer->len_used=used`` (i.e., no changes to ``ydbstring``).

``YDB_STRLIT_TO_BUFFER(ydbbuffer, strlit)`` — With ``ydbbuffer`` a
pointer to a ``ydb_buffer_t`` structure, and ``strlit`` a string
literal, set:

- ``ydbbuffer->buf_addr`` to the address of ``strlit``, and
- ``ydbbuffer->len_alloc`` and ``ydbbuffer->len_used`` to the length
  of the string literal excluding its terminating null character.

``YDB_STRLIT_TO_STRING(ydbstring,strlit)`` — With ``ydbstring`` a
pointer to a ``ydb_string_t`` structure, and ``strlit`` a string
literal, set

- ``ydbstring->address`` to the address of ``strlit``, and
- ``ydbstring->length`` to the length of the string literal excluding
  its terminating null character.

Note that the addresses of the ``strlit`` string literals set in
``*ydbbuffer`` by invocations of ``YDB_STRLIT_TO_BUFFER()`` and
``*ydbstring`` by invocations of ``YDB_STRLIT_TO_STRING()`` are almost
certainly pointers to read-only sections of memory, and any subsequent
attempt to modify the contents of ``ydbbuffer->buf_addr`` or
``ydb_string->address`` will thus result in abnormal process
termination with segmentation violation (SIG-11) that may be hard to
troubleshoot.

================
Programming in C
================

YottaDB functions are divided into:

- Simple API — a core set of functions that provides easy-to-use
  access to the major features of YottaDB.
- Comprehensive API — a more elaborate set of functions for
  specialized or optimized access to additional functionality within
  ``libyottadb.so`` that YottaDB itself uses. The Comprehensive API is
  a project for the future.
- Utility Functions — Functions useful to a C application using
  YottaDB.

Simple API
==========

As all subscripts and node data passed to YottaDB using the Simple
API are strings, use the ``printf()`` and ``scanf()`` family of
functions to convert between numeric values and strings which are
`canonical numbers`_.

To allow the YottaDB Simple API functions to handle a variable tree
whose nodes have varying numbers of subscripts, the actual number of
subscripts is itself passed as a parameter. In the prototypes of
functions, parameters of the form:

- ``ydb_buffer_t *varname`` refers to the name of a variable;
- ``int subs_used`` and ``int *subs_used`` refer to an actual number
  subscripts; and
- ``ydb_buffer_t *subsarray`` refers to an array of ``ydb_buffer_t``
  structures used to pass subscripts whose actual number is defined by
  ``subs_used`` or ``*subs_used`` parameters.

To pass an intrinsic special variable, ``subs_used`` should be zero
and ``*subsarray`` should be NULL.

**Caveat:** Specifying a ``subs_used`` that exceeds the actual number
of parameters passed in ``*subsarray`` will almost certainly result in
an unpleasant bug that is difficult to troubleshoot. [#]_

.. [#] Note for implementers: the implementation should attempt to
       limit the damage by not looking for more subscripts than are
       permitted by ``YDB_MAX_SUBS``.

Function names specific to the YottaDB Simple API end in ``_s``.

------------
ydb_data_s()
------------

.. code-block:: C

	int ydb_data_s(ydb_buffer_t *varname,
		int subs_used,
		ydb_buffer_t *subsarray,
		unsigned int *value);

In the location pointed to by ``value``, ``ydb_data_s()`` returns the
following information about the local or global variable node
identified by ``*varname``, ``subs_used`` and ``*subsarray``.

- 0 — There is neither a value nor a subtree, i.e., it is undefined.
- 1 — There is a value, but no subtree
- 10 — There is no value, but there is a subtree.
- 11 — There are both a value and a subtree.

It is an error to call ``ydb_data_s()`` on an intrinsic special
variable; doing so results in the ``YDB_ERR_UNIMPLOP``
error. ``ydb_data_s()`` returns ``YDB_OK`` or an `error return code`_.

--------------
ydb_delete_s()
--------------

.. code-block:: C

	int ydb_delete_s(ydb_buffer_t *varname,
		int subs_used,
		ydb_buffer_t *subsarray,
		int deltype);

Deletes nodes in the local or global variable tree or subtree
specified. A value of ``YDB_DEL_NODE`` or ``YDB_DEL_TREE`` for
``deltype`` specifies whether to delete just the node at the root,
leaving the (sub)tree intact, or to delete the node as well as the
(sub)tree.

In the special case where ``*varname`` is NULL, ``ydb_delete_s()``
deletes all local variables. Intrinsic special variables cannot be
deleted.

``ydb_delete_s()`` returns ``YDB_OK`` or an `error return code`_.

-------------------
ydb_delete_excl_s()
-------------------

.. code-block:: C

	int ydb_delete_excl_s(int namecount,
		ydb_buffer_t *varnames);

``ydb_delete_excl_s()`` deletes the trees of all local variables
except those in the ``*varnames`` array. It is an error for
``*varnames`` to include a global or intrinsic special variable. Note
that ``namecount`` must be greater than zero.

If your application mixes M and non M code, and you wish to use
``ydb_delete_excl_s()`` to delete local variables that are aliases,
formal parameters, or actual parameters passed by reference, make sure
you understand what (sub)trees are being deleted. This does not apply
to applications that do not include M code.

``ydb_delete_excl_s()`` returns ``YDB_OK`` or an `error return code`_.

--------------------
ydb_delete_multi_s()
--------------------

*This function is optional for the initial release of the Simple API,
time permitting.*

.. code-block:: C

	int ydb_delete_multi_s(int deltype,
		int namecount,
		ydb_buffer_t *varname,
		int subs_used,
		ydb_buffer_t *subsarray[, ...]);

``namecount`` (>0) is the number of nodes specified in the call.

Deletes nodes in each of the local or global variable trees or
subtrees specified.  A value of ``YDB_DEL_NODE`` or ``YDB_DEL_TREE``
for ``deltype`` specifies whether to delete just the node at each
root, leaving (sub)trees intact, or to delete nodes as well as
(sub)trees.

Intrinsic special variables cannot be deleted.

``ydb_delete_s()`` returns ``YDB_OK`` or an `error return code`_.

-----------
ydb_get_s()
-----------

.. code-block:: C

	int ydb_get_s(ydb_buffer_t *varname,
		int subs_used,
		ydb_buffer_t *subsarray,
		ydb_buffer_t *ret_value);

To the location pointed to by ``ret_value->buf_addr``, ``ydb_get_s()``
copies the value of the specified node or intrinsic special variable,
setting ``ret_value->len_used``. Return values are:

- ``YDB_OK`` for a normal return;
- ``YDB_ERR_GVUNDEF``, ``YDB_ERR_INVSVN``, or ``YDB_ERR_LVUNDEF`` as
  appropriate if no such variable or node exists;
- ``YDB_ERR_INVSTRLEN`` if ``ret_value->len_alloc`` is insufficient for
  the value at the node; or
- another applicable `error return code`_.

Notes:

- In the unlikely event an application wishes to know the length of
  the value at a node, but not access the data, it can call
  ``ydb_get_s()`` and provide an output buffer
  (``retvalue->len_alloc``) with a length of zero.
- Within a transaction implemented by `ydb_tp_s()`_ application
  code observes stable data at global variable nodes because YottaDB
  `transaction processing`_ ensures ACID properties.
- Outside a transaction, a global variable node can potentially be
  changed by another, concurrent, process between time that a process
  calls ``ydb_data_s()`` to ascertain the existence of the data and a
  subsequent call to ``ydb_get()`` to get that data. A caller of
  ``ydb_get_s()`` to access a global variable node should code in
  anticipation of a potential ``YDB_ERR_GVUNDEF``.

------------
ydb_incr_s()
------------

.. code-block:: C

	int ydb_incr_s(ydb_buffer_t *varname,
		int subs_used,
		ydb_buffer_t *subsarray,
		ydb_buffer_t *increment,
		ydb_buffer_t *ret_value);

``ydb_incr_s()`` atomically:

- converts the value in the specified node to a number if it is not
  one already, using a zero value if the node does not exist;
- increments it by the value specified by ``*increment``, converting
  the value to a number if it is not a canonical number, defaulting to
  1 if the parameter is NULL; and
- storing the value as a `canonical number`_ in ``*ret_value``.

Return values:

- The normal return value is ``YDB_OK``.
- If the atomic increment results in a numeric overflow, the function
  returns a ``YDB_ERR_NUMOFLOW`` error; in this case, the value in the
  node and ``*ret_value`` is unreliable.
- In the event the ``ydb_buffer_t`` structure pointed to by ``ret_value``
  is not large enough for the result, the function returns a
  ``YDB_ERR_INVSTRLEN`` error.

Notes:

- Intrinsic special variables cannot be atomically incremented, and an
  attempt to do so returns the ``YDB_ERR_UNIMPLOP`` error.
- Since it changes the value of the node, ``ydb_incr_s()`` is a
  function with a side effect.

------------
ydb_lock_s()
------------

.. code-block:: C

	int ydb_lock_s(unsigned long long timeout,
		int namecount[,
		[ydb_buffer_t *varname,
		int subs_used,
		ydb_buffer_t *subsarray], ...]);

``namecount`` is the number of variable names in the call.

Release any locks held by the process, attempt to acquire all the
requested locks. While the release is unconditional, on return, the
function will have acquired all requested locks or none of them. If no
locks are requested (``namecount`` is zero), the function releases all
locks and returns ``YDB_OK``.

``timeout`` specifies a time in nanoseconds that the function waits
to acquire the requested locks. If it is not able to acquire all
requested locks, it acquires no locks, returning with a
``YDB_LOCK_TIMEOUT`` return value.

If ``timeout`` is zero, the function makes exactly one attempt to
acquire the locks, and if it is unable to, it returns
``YDB_LOCK_TIMEOUT``.

If all requested locks are successfully acquired, the function returns
``YDB_OK``.

-----------------
ydb_lock_decr_s()
-----------------

.. code-block:: C

	int ydb_lock_decr_s(ydb_buffer_t *varname,
		int subs_used,
		ydb_buffer_t *subsarray);

Decrements the count held by the process of the specified lock. As
noted in the `Concepts`_ section, a lock whose count goes from 1 to 0
is released. A lock whose name is specified, but which the process
does not hold, is ignored.

As releasing a lock cannot fail, the function returns ``YDB_OK``,
unless there is an error such as an invalid name that results in the
return of an error code such as ``YDB_ERR_INVVARNAME``.

-----------------
ydb_lock_incr_s()
-----------------

.. code-block:: C

	int ydb_lock_incr_s(unsigned long long timeout,
		ydb_buffer_t *varname,
		int subs_used,
		ydb_buffer_t *subsarray);

Without releasing any locks held by the process, attempt to acquire
the requested lock incrementing it if already held.

``timeout`` specifies a time in nanoseconds that the function waits
to acquire the requested lock. If it is not able to acquire the lock,
it returns with a ``YDB_LOCK_TIMEOUT`` return value.

If ``timeout`` is zero, the function makes exactly one attempt to
acquire the lock, and if unable to, it returns ``YDB_LOCK_TIMEOUT``.

If the requested lock is successfully acquired, the function returns
``YDB_OK``.

-----------------
ydb_node_next_s()
-----------------

.. code-block:: C

	int ydb_node_next_s(ydb_buffer_t *varname,
		int subs_used,
		ydb_buffer_t *subsarray,
		int *ret_subs_used,
		ydb_buffer_t *ret_subsarray);

``ydb_node_next_s()`` facilitates depth-first traversal of a local or
global variable tree. As the number of subscripts can differ between
the input node of the call and the output node reported by the call
``*ret_subs_used`` is an input as well as an output parameter:

- On input, ``*ret_subs_used`` specifies the number of elements
  allocated for returning the subscripts of the next node.
- On output, ``*ret_subs_used`` contains the actual number of
  subscripts returned or is ``YDB_NODE_END``. If the actual number of
  subscripts to be returned exceeds the input value specified by
  ``*ret_subs_used``, the function returns the ``YDB_ERR_INSUFFSUBS``
  error (see below).

Return values of ``ydb_node_next_s()`` are:

- ``YDB_OK`` with the next node, if there is one, changing
   ``*ret_subs_used`` and ``*ret_subsarray`` parameters to those of
   the next node. If there is no next node (i.e., the input node is
   the last), ``*ret_subs_used`` on output is ``YDB_NODE_END``.
- ``YDB_ERR_INSUFFSUBS`` if ``*ret_subs_used`` specifies
  insufficient parameters to return the subscript. In this case
  ``*ret_subs_used`` reports the actual number of subscripts required.
- ``YDB_ERR_INVSTRLEN`` if one of the ``ydb_buffer_t`` structures
  pointed to by ``*ret_subsarray`` does not have enough space for the
  subscript. In this case, ``*ret_subs_used`` is the index into the
  ``*ret_subsarray`` array with the error, and the ``len_used`` field
  of that structure specifies the size required.
- Another `error return code`_, in which case the application should
  consider the values of ``*ret_subs_used`` and the ``*ret_subsarray``
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

Analogous to ``ydb_node_next(s)``, ``ydb_node_previous_s()``
facilitates reverse breadth-first traversal of a local or global
variable tree, except that ``ydb_node_previous_s()`` searches for and
reports the predecessor node. Unlike ``ydb_node_next_s()``,
``*ret_subs_used`` can be zero if an expected previous node is the
unsubscripted root. However ``*subs_used`` must be greater than zero.

``ydb_node_previous_s()`` returns ``YDB_OK``, ``YDB_ERR_INSUFFSUBS``,
``YDB_ERR_INVSTRLEN``, or an `error return code`_.

-----------
ydb_set_s()
-----------

.. code-block:: C

	int ydb_set_s(ydb_buffer_t *varname,
		int subs_used,
		ydb_buffer_t *subsarray,
		ydb_buffer_t *value);

Copies the ``value->len_used`` bytes at ``value->buf_addr`` as the
value of the specified node or intrinsic special variable specified,
returning ``YDB_OK`` or an `error return code`_. A NULL ``value``
parameter is treated as equivalent to one that points to a
``ydb_buffer_t`` specifying an empty string.

-----------------
ydb_set_multi_s()
-----------------

*This function is optional for the initial release of the Simple API,
time permitting.*

.. code-block:: C

	int ydb_set_multi_s(ydb_buffer_t *value,
		int namecount,
		ydb_buffer_t *varname,
		int subs_used,
		ydb_buffer_t *subsarray[, ...]);

Copies the ``value->len_used`` bytes at ``value->buf_addr`` as the
value of the specified nodes or intrinsic special variables specified,
returning ``YDB_OK`` or an `error return code`_. A NULL ``value``
parameter is treated as equivalent to one that points to a
``ydb_buffer_t`` specifying an empty string.

---------------
ydb_str2zwr_s()
---------------

.. code-block:: C

	int ydb_str2zwr_s(ydb_buffer_t *str, ydb_buffer_t *zwr);

In the buffer referenced by ``*zwr``, ``ydb_str2zwr_s()`` provides the
`zwrite formatted`_ version of the string pointed to by ``*str``,
returning ``YDB_OK``, or the ``YDB_ERR_INVSTRLEN`` error if the
``*zwr`` buffer is not long enough.

----------------------
ydb_subscript_next_s()
----------------------

.. code-block:: C

	int ydb_subscript_next_s(ydb_buffer_t *varname,
		int subs_used,
		ydb_buffer_t *subsarray,
		ydb_buffer_t *ret_value);

``ydb_subscript_next_s()`` provides a primitive for implementing
breadth-first traversal of a tree by searching for the next subscript
at the level specified by ``subs_used``, i.e., the next subscript
after ``*subsarray[subs_used].buf_addr``. A node need not exist at the
subscripted variable name provided as input to the function. If
``subsarray[subs_used].len_used`` is zero, ``ydb_subscript_next()``
returns the first node at that level with a subscript that is not the
empty string. ``ydb_subscript_next_s()`` returns ``YDB_OK`` or an
`error return code`_.

On return from ``ydb_subscript_next_s()`` with a ``YDB_OK``, if
``ret_value->len_used`` is non-zero, ``*ret_value->buf_addr`` contains
the value of the next subscript. If it is zero, it means that the
input node was the last at that level.

In the special case where ``subs_used`` is zero,
``ydb_subscript_next_s()`` returns the next local or global variable
name.

--------------------------
ydb_subscript_previous_s()
--------------------------

.. code-block:: C

	int ydb_subscript_previous_s(ydb_buffer_t *varname,
		int subs_used,
		ydb_buffer_t *subsarray,
		ydb_buffer_t *ret_value);

``ydb_subscript_previous_s()`` provides a primitive for implementing
reverse breadth-first traversal of a tree by searching for the
previous subscript at the level specified by ``subs_used``. i.e. the
subscript preceding ``*subsarray[subs_used].buf_addr``. A node need not
exist at the subscripted variable name provided as input to the
function. ``ydb_subscript_previous_s()`` returns ``YDB_OK`` or an
`error return code`_.

On return from ``ydb_subscript_previous_s()``, if
``ret_value->len_used`` is non-zero, ``*ret_value->buf_addr`` contains
the value of the previous subscript. If it is zero, and the
application does not use empty strings as subscripts, it means that
the input node was the first at that level. If an application uses
empty strings as subscripts, a subsequent call to ``ydb_data_s()`` is
required to determine whether the first subscript has been reached or
whether the first subscript is a node with the empty string as a
subscript.

In the special case where ``subs_used`` is zero,
``ydb_subscript_previous_s()`` returns the previous local or global
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

``ydb_tp_s()`` calls the function pointed to by ``tpfn`` passing it
``tpfnparm`` as a parameter. As discussed under `Transaction
Processing`_, the function should use the intrinsic special variable
``$trestart`` to manage any externally visible action (which YottaDB
recommends against, but which may be unavoidable). The function should
return one of the following:

- ``YDB_OK`` — application logic indicates that the transaction can
  be committed (the YottaDB engine may still decide that a restart is
  required to ensure ACID transaction properties) as discussed under
  `Transaction Processing`_.
- ``YDB_TP_RESTART``  — application logic indicates that the
  transaction should restart.
- ``YDB_TP_ROLLBACK`` — application logic indicates that the
  transaction should not be committed. Any return code from the
  function pointed to by ``tpfn`` other than ``YDB_OK`` or
  ``YDB_TP_RESTART`` results in ``ydb_tp_s()`` forthwith returning to
  its caller with that return code. The symbolic constant
  ``YDB_TP_ROLLBACK`` is provided to improve future code
  maintainability, and should be used when the intent is to rollback
  the transaction.

If not NULL or the empty string ``transid`` is case-insensitive
``"BA"`` or ``"BATCH"`` to indicate that at transaction commit,
YottaDB need not ensure Durability (it always ensures Atomicity,
Consistency, and Isolation). Use of this flag may improve latency and
throughput for those applications where an alternative mechanism (such
as a checkpoint) provides acceptable durability. If a transaction that
is not flagged as ``"BATCH"`` follows one or more transactions so
flagged, Durability of the later transaction ensures Durability of the
the earlier ``"BATCH"`` transaction(s).

If ``namecount>0``, the ``*varnames`` parameter specifies an array of
local variable names whose values are restored to their original
values when the transaction is restarted. In the special case where
``namecount=1`` and the sole ``*varnames`` provides the value ``"*"``,
all local variables are restored on a restart. It is an error for a
``*varnames`` to include a global or intrinsic special variable.

A ``ydb_tp_s()`` that is not itself within a transaction returns
``YDB_OK``, ``YDB_TP_ROLLBACK``, ``YDB_ERR_TPTIMEOUT`` (see
`Transaction Processing`_), or an `error return code`_ – a
``ydb_tp_s()`` that is the top level transaction handles restarts and
never returns a ``YDB_TP_RESTART``. A ``ydb_tp_s()`` call that is
within another transaction can also return ``YDB_TP_RESTART`` to its
caller. [#]_

.. [#] An enclosing transaction can result not just from another
       ``ydb_tp_s()`` higher in the stack, but also from an M
       ``tstart`` command as well as a database trigger resulting from
       a `ydb_delete_s()`_, or `ydb_set_s()`_.

---------------
ydb_zwr2str_s()
---------------

.. code-block:: C

	int ydb_zwr2str_s(ydb_buffer_t *zwr, ydb_buffer_t *str);

In the buffer referenced by ``*str``, ``ydb_zwr2str_s()`` provides the
string described by the `zwrite formatted`_ string pointed to by
``*zwr``, returning ``YDB_OK`` (with ``str->len_used`` set to zero if
the zwrite formatted string has an error), or the
``YDB_ERR_INVSTRLEN`` error if the ``*str`` buffer is not long enough.

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

As noted in the `Overview`_, the YottaDB database engine resides in
the address space of the process, and the data structures of the
database engine include information such as the pid, which differs
between parent and child processes.

Notes:

- If a process has open database files, after a ``fork()``, the child
  process **must** call ``ydb_child_init()`` in order to re-initialize
  the data structures of the database engine. No action is needed on
  the part of the parent process. *A child process that fails to call*
  ``ydb_child_init()`` *after a* ``fork()`` *or equivalent, such as*
  ``os.fork()`` *in Python, can cause structural damage to database
  files.* As ``ydb_child_init)`` is effectively a no-op if there are
  no open database files, a child process that is unsure whether there
  are open database files can unconditionally call the function.
- After a ``fork()`` or equivalent, a parent process should not exit
  until the child process has executed ``ydb_child_init()``. An
  efficient way to implement this would be for the parent to set a
  node such as ``^Proc(ppid)=1`` where ``ppid`` is the parent's pid,
  and for the child to set it to zero or to delete the node. A parent
  process that wishes to ``fork()`` a number of child processes can
  use ``ydb_incr_s()`` to increment a node such as ``^Proc(ppid)`` and
  each child can decrement it after executing
  ``ydb_child_init()``. When the value at the node is zero, the parent
  process knows that it is safe for it to exit.

The ``void *param`` is reserved for future enhancements. As the
initial release of YottaDB ignores it, we recommend using
NULL. ``ydb_child_init()`` returns ``YDB_OK`` or an `error return
code`_.

------------------
ydb_file_id_free()
------------------

.. code-block:: C

	int ydb_file_id_free(void *fileid)

Releases the memory used by a ``fileid`` structure previously
generated by `ydb_file_name_to_id()`_. Calling the function twice for
the same pointer, unless it has been returned a second time by a
different `ydb_file_name_to_id()`_ is an application error with
undefined consequences.

-----------------------
ydb_file_is_identical()
-----------------------

.. code-block:: C

	int ydb_file_is_identical(void *fileid1, void *fileid2)

Given two pointers to ``fileid`` structures (see
`ydb_file_name_to_id()`_), ``ydb_file_is_identical`` returns YDB_OK if
the two ``fileid`` structures are the same file.

---------------------
ydb_file_name_to_id()
---------------------

.. code-block:: C

	int ydb_file_name_to_id(ydb_string_t *filename,
		void *(*fileid))

As a file is in principal reachable through different paths, and
application code may need to check whether two paths do indeed lead to
the same file, YottaDB provides a mechanism to do so. Provided with a
path to a file, YottaDB creates an internal structure called a
``fileid`` that uniquely identifies the file if such a structure does
not already exist for that file, and provides the caller with a
pointer to that structure. The layout and contents of the fileid
structure are opaque to the caller, which **must not** modify the
pointer or the structure it points to.

When the ``fileid`` structure for a file is no longer needed, an
application should call `ydb_file_id_free()`_ to release the structure
and avoid a memory leak.

``ydb_file_name_to_id()`` returns ``YDB_OK`` or an `error return
code`_.

-----------------
ydb_fork_n_core()
-----------------

.. code-block:: C

	void ydb_fork_n_core(void)

The core of a process can help debug application code, for example to
troubleshoot an out-of-design condition. However, generating a core
terminates the process, and furthermore, if the process is last
process with a database file open, that file can be left in an
“unclean” state, even if a state that is easily recovered; and
cleaning up open database files before generating the core perturbs
the process state to be debugged.

When a process executes ``ydb_fork_n_core()``, the process forks. The
child process does essential cleanup (such as erasing encryption
passphrases) and sends itself a signal to generate a core and
terminate, while the parent process continues execution.

The content, location, naming, and management of cores is managed by
the operating system – see ``man 5 core`` for details. We recommend
that you set ``kernel.core_uses_pid`` to 1 to make it easier to
identify and track cores. As cores can contain protected confidential
information, you *must* ensure appropriate configuration and
management of cores.

----------
ydb_free()
----------

.. code-block:: C

	int ydb_free(void *ptr)

Releases memory previously allocated by ``ydb_malloc()``. Passing
``ydb_free()`` a pointer not previously provided to the application by
``ydb_malloc()`` can result in unpredictable behavior. The signature
of ``ydb_free()`` matches that of the POSIX ``free()`` call.

-----------------
ydb_hiber_start()
-----------------

.. code-block:: C

	void ydb_hiber_start(unsigned long long sleep_nsec)

The process sleeps for the time in nanoseconds specified by
``sleep_nsec``.

--------------------------
ydb_hiber_start_wait_any()
--------------------------

.. code-block:: C

	void ydb_hiber_start_wait_any(unsigned long long sleep_nsec)

The process sleeps for the time in nanoseconds specified by
``sleep_nsec`` or until it receives a signal.

------------
ydb_malloc()
------------

.. code-block:: C

	void *ydb_malloc(size_t size)

With a signature matching that of the POSIX ``malloc()`` call,
``ydb_malloc()`` returns an address to a block of memory of the
requested size, or NULL if it is unable to satisfy the request.
``ydb_malloc()`` uses a `buddy system
<https://en.wikipedia.org/wiki/Buddy_memory_allocation>`_, and
provides debugging functionality under the control of the environment
variable ``ydb_dbglevel``.

-------------
ydb_message()
-------------

.. code-block:: C

	int ydb_message(ydb_buffer_t *msgtext, int status)

Set ``msgtext->buf_addr`` to a location that has the text for the
condition corresponding to ``status``, and both ``msgtext->len_alloc`` and
``msgtext->len_used`` to its length (with no trailing null
character). Note: as ``msgtext->buf_addr`` points to an address in a
read-only region of memory, any attempt to modify the message will
result in a segmentation violation (SIGSEGV). ``ydb_message()``
returns ``YDB_OK`` for a valid ``status`` and
``YDB_ERR_UNKNOWN`` if ``status`` does not map to a known error.

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
explicitly redirects stdout or stderr (e.g., using ``dup2()``), should
call this function as soon as possible after the
redirection. ``ydb_stdout_stderr_adjust()`` returns ``YDB_OK``.

--------------------
ydb_thread_is_main()
--------------------

.. code-block:: C

   int ydb_thread_is_main(void)

Returns ``YDB_OK`` if the thread is the main thread of the process,
and another value if the thread is not.
	
------------------
ydb_timer_cancel()
------------------

.. code-block:: C

	void ydb_timer_cancel(int timer_id)

Cancel a timer identifier by ``timer_id`` and previously started with
`ydb_timer_start()`_.

-----------------
ydb_timer_start()
-----------------

.. code-block:: C

	typedef void (*handler_fun_ptr_t)(unsigned int timer_id,
		unsigned int handler_data_len,
		char *handler_data);
	void ydb_timer_start(unsigned int timer_id,
		unsigned long long limit_nsec,
		handler_fun_ptr_t handler,
		unsigned int handler_data_len
		char *handler_data);


Starts a timer. Unless canceled, when the timer expires,
``ydb_timer_start()`` invokes a handler function, providing that
function with input data.

``timer_id`` is an identifier for the the timer. It is the
responsibility of application code to ensure that ``timer_id`` is
different from those of any other active / pending timers.

``limit_nsec`` is the minimum number of nanoseconds before the timer
expires and invokes the handler function. Owing to overhead and system
load, the actual time will almost always be greater than this value.

``handler`` is a pointer to the function to be called when the timer
expires.

``handler_data`` is a pointer to the data to be passed to ``handler``
and ``handler_data_len`` is the length of the data at
``*handler_data``.

================
Programming in M
================

As YottaDB is built on `FIS GT.M <http://fis-gtm.com>`_ , it includes
a complete implementation of the `M
<https://en.wikipedia.org/wiki/MUMPS>`_ programming language (also
known as MUMPS) that mostly conforms to `ISO/IEC 11756:1999
<http://www.iso.ch/iso/en/CatalogueDetailPage.CatalogueDetail?CSNUMBER=29268&ICS1=35&ICS2=60&ICS3=&scopelist>`_.
The `YottaDB M Programmers Guide
<https://docs.yottadb.com/ProgrammersGuide/UNIX_manual/>`_ documents programming
YottaDB in M and is not duplicated here.

=================
Programming Notes
=================

Numeric Considerations
======================

To ensure the accuracy of financial calculations, [#]_ YottaDB internally
stores numbers as, and performs arithmetic using, a scaled packed
decimal representation with 18 significant decimal digits, with
optimizations for values within a certain subset of its full
range. Consequently, any number that is exactly represented in YottaDB
can be exactly represented as a string, with reasonably efficient
conversion back and forth.

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
   through.1E47.

.. _zwrite format:

.. _zwrite formatted:

Zwrite Format
=============

Strings used as subscripts and as values can include unprintable
bytes, for example control characters or binary data. YottaDB's zwrite
format is an encoding in printable ASCII of any sequence of
bytes. Unlike formats such as Base64, The zwrite format attempts to
preserve readability of printable ASCII characters. Note that a zwrite
formatted string is always longer than the original string (at the
very least, it has enclosing quotes).

Signals
=======

As ``libyottadb.so`` includes a database engine that uses timers and
signals, YottaDB uses signals, especially timers.  YottaDB strongly
discourages the use of signals, especially SIGALARM, in application
code functions. Use the exposed timer APIs for application timing
functionality (see `Utility Functions`_).

As the M language subsystem uses SIGUSR1 for asynchronous interrupts,
we strongly suggest that C applications use SIGUSR2 for this purpose.

Forking
=======

There are two considerations when executing ``fork()``.

- Before a process that performs buffered IO executes ``fork()``, it
  should execute ``fflush()``. Otherwise, the child process will
  inherit unflushed buffers from the parent, which the child process
  will flush when it executes an ``fflush()``. This is a general
  programming admonition, not specific to YottaDB except to the extent
  that M code within a parent process may have executed ``write``
  commands which are still buffered when C code within the same
  process calls ``fork()``.
- After a ``fork()``, the child process must immediately execute
  `ydb_child_init()`_ . Since the YottaDB database engine resides in
  the address space of the process, as noted in the `Overview`_, this
  is required to ensure a clean separation in the internal state of
  the database engine between parent and child processes.

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
