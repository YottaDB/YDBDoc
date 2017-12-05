.. header::
   YottaDB — Multi-Language Programmers Guide

.. footer::
   Page ###Page### of ###Total###

.. index:: Multi-language Programming Guide

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
for other languages, additional sections will be added to ths
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
   ``export ydbdir=$HOME/.yottadb ; . /opt/yottadb/latest/yottadbprofile``
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
    YottaDB strongly recommends against applications that use null
    subscripts.*
  - Numeric subscripts precede string subscripts. Numeric subscripts
    are in numeric order.
  - String subscripts follow numeric subscripts and collate in byte
    order. [#]_

- Like subscripts, values are sequences of bytes, except that ordering
  is not meaningful. YottaDB automatically converts between numbers
  and strings, depending on the type of operand required by an
  operator or argument required by a function (see `Numeric
  Considerations`_).

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

.. [#] Where the natural byte order does not result in linguistically
       and culturally correct ordering of strings, YottaDB has a
       framework for an application to create and use custom collation
       routines.

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
are created and maintaind by a utility program called the Global
Directory Editor, which is discussed at length in the `GT.M
Administration and Operations Guide
<http://tinco.pair.com/bhaskar/gtm/doc/books/ao/UNIX_manual/>`_ and is
outside the purview of this document.

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
distinguised by a "^" prefix, intrinsic special variables are
distinguished by a "$" prefix.  Unlike local and global variable
names, intrinsic special variable names are case-insensitive and so
``$zgbldir`` and ``$ZGblDir`` refer to the same intrinsic special
variable. Intrinsic special variables have no subscripts.

While the majority of intrinisic special variables as enumerated in
Chapter 8 (Intrinsic Special Variables) of `GT.M Programmers Guide
<http://tinco.pair.com/bhaskar/gtm/doc/books/pg/UNIX_manual/>`_ are
useful to M application code, others are more generally useful and
documented here.

---------
$tretries
---------

Application code inside a transaction can read the intrinsic special
variable ``$tretries`` to determine how many times a transaction has
been restarted. Although YottaDB recommends against accessing external
resources within a transaction, logic that needs to access an external
resource (e.g., to read data in a file), or to aquire a lock, can use
``$tretries`` to restrict that access or acquisition to the first time
it executes (``$tretries=0``).

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
  database block it has read, and the transaction numbner of that
  block when read. Other processes are free to update the database
  during this time.
- The process retains updates in its memory, without committing them
  to the database, so that it's own logic sees the updates, but no
  other process does. As every block that the process wishes to write
  must also be read, tracking the transaction numbers of blocks read
  suffices to track them for blocks to be writen.
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
  whether a higher level routine has already acqured it. More
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
than M, it is not possible for applications to `deadlock
<https://en.wikipedia.org/wiki/Deadlock>`_ on YottaDB
locks. Consequently defensive application code must always validate
the return code of calls to acquire locks.

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
  time, we recommend either matching lock acquition and releases
  within a transaction, or, for locks acquired within a transaction but
  released after the transaction is committed or abandoned, to
  acquisition only on the first attempt, using the intrinsic special
  variable `$tretries`_.

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
other than ``YDB_ERR_``

``YDB_OK`` — Normal return following successful execution.

``YDB_LOCK_TIMEOUT`` — This return code from lock acquisition
functions indicates that the specified timeout was reached without
requested locks being acquired.

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

------------------
Error Return Codes
------------------

Symbolic constants for error codes returned by calls to YottaDB are
prefixed with ``YDB_ERR_`` and are all less than zero. [#]_ The
symbolic constants below are not a complete list of all error messages
that Simple API functions can return — error return codes can
indicate system errors and database errors, not just application
errors. The ``ydb_message()`` function provides a way to get more
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
is ``YDB_ERR_INVSTRLEN`` and if ``*xyz`` is a ``ydb_string_t`` value
whose ``xyz->length`` indicates insufficient space, then ``xyz->used``
is set to the size required of a sufficiently large buffer, and
``xyz->address`` points to the first ``xyz->length`` bytes of the
value. In this case the ``used`` field of a ``ydb_string_t``
structure is greater than the ``length`` field.

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
the call exceeds ``YDB_MAX_SUB``.

``YDB_ERR_TPTMEOUT`` — This return code from `ydb_tp_s()`_ indicates
that the transaction took too long to commit.

``YDB_ERR_UNKNOWN`` — A call to `ydb_message()`_ specified an
invalid message code.


Limits
======

Symbolic constants for limits are prefixed with ``YDB_MAX_``.

``YDB_MAX_IDENT`` — The maximum space in bytes required to store a
complete variable name, not including the preceding caret for a global
variable. Therefore, when allocating space for a string to hold a
global variable name, add 1 for the caret.

``YDB_MAXLOCKTIME`` — The maximum value in microseconds that an
application can instruct libyottab to wait until the process is able
to acquire locks it needs before timing out. This value is guaranteed
to be no less than 2\ :superscript:`32`\ -1.

``YDB_MAX_STR`` — The maximum length of a string (or blob) in
bytes. A caller to ``ydb_get()`` that provides a buffer of
``YDB_MAX_STR`` will never get a ``YDB_ERR_INVSTRLEN``
error.

``YDB_MAX_SUB`` — The maximum number of subscripts for a local or
global variable.

Other
=====

Other symbolic constants have a prefix of ``YDB_``.

===============
Data Structures
===============

``ydb_string_t`` is a descriptor for a string [#]_ value, and consists of
the following fields:

 - ``length`` and ``used`` — fields of type ``unsigned int`` where
   ``length`` ≥ ``used`` except when a `YDB_ERR_INVSTRLEN`_ occurs.
 - ``address`` — pointer to an ``unsigned char``, the starting
   address of a string.

.. [#] Strings in YottaDB are arbitrary sequences of bytes that are not
       null-terminated. Other languages may refer to them as binary
       data or blobs.

======
Macros
======

``YDB_ALLOC_STRING(string[,actalloc])`` — Allocate a ``ydb_string_t``
structure and set its ``address`` field to point to ``string``, and
its ``used`` field to the length of string excluding the terminating
null character. Set its ``length`` field to ``actalloc`` if specified,
otherwise to ``used``. Return the address of the structure. Note that
if string is a ``const`` any code that attempts to change the value of
the string pointed to by this ``ydb_string_t`` structure will almost
certainly result in a segmentation violation (SIGSEGV). [#]_

.. [#] Note for implementers: under the covers, ``YDB_ALLOC_*()``,
       ``YDB_FREE_*()``, and ``YDB_NEW_*()`` macros should call the
       ``ydb_malloc()`` and ``ydb_free()`` functions, which are
       aliases for the ``gtm_malloc()`` and ``gtm_free()`` functions
       (i.e., either prefix calls the same function). Also, for
       efficiency reasons, we may want to have two macros,
       ``YDB_ALLOC_STRING()`` and ``YDB_ALLOC_STRLIT()``.

``YDB_COPY_STRING(dest,src)`` — Confirm that ``dest->length`` ≥
``src->used``, and if so copy ``src->used`` bytes from memory pointed
to by ``src->address`` to the memory pointed to by ``dest->address``,
returning ``YDB_OK``. If ``dest->length`` < ``src-used``, return
``YDB_ERR_INVSTRLEN``.

``YDB_FREE_STRING(x)`` — Free the ``ydb_string_t`` structure pointed
to by ``x``.

``YDB_FREE_STRING_DEEP(x)`` — Free the memory referenced by
``x->address`` and free the ``ydb_string_t`` structure pointed to by
``x``.

``YDB_NEW_STRING(string[,minalloc])`` — Allocate memory sufficient to
hold ``string`` (excluding the trailing null character) and copy
``string`` to that memory. If ``minalloc`` is specified, allocate at
least ``minalloc`` bytes. At the implementer's option, the allocation
may be further rounded up to a preferred size. Copy ``string`` to the
newly allocated memory. Allocate a ``ydb_string_t`` structure and set
its ``address`` field to point to the newly allocated memory, its
``length`` field to point to the size of allocated memory, and its
``used`` field to the length of ``string``. Return the address of the
new ``ydb_string_t`` structure. Use an empty string as the value of
``string`` to preallocate structures for use, e.g.,
``YDB_NEW_STRING("",YDB_MAX_IDENT)`` to create space for a local
variable name to be returned by a function such as
``ydb_subscript_next_s()``.

``YDB_SET_STRING(x, string)`` — Check whether the ``x->length`` has
sufficient space for ``string`` and if so, copy ``string`` excluding
the terminating null character to the memory pointed to
by ``x->address`` and set ``x->used`` to the length of ``string``.

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
subscripts is itself passed as a parameter. In the definitions of
functions:

- ``int count`` and ``int *count`` refer to an
  actual number subscripts,
- ``ydb_string_t *varname`` refers to the name of a variable, and
- ``[, ydb_string_t *subscript, ...]`` and ``ydb_string_t *subscript[,
  ydb_string_t *subscript, ...]`` refer to placeholders for subscripts
  whose actual number is defined by ``count`` or ``*count``.

**Caveat:** Specifying a count that exceeds the actual number of
parameters passed will almost certainly result in an unpleasant bug
that is difficult to troubleshoot. [#]_

.. [#] Note for implementers: the implementation should attempt to
       limit the damage by not looking for more subscripts than are
       permitted by ``YDB_MAX_SUB``.

Function names specific to the YottaDB Simple API end in ``_s``.

------------
ydb_data_s()
------------

.. code-block:: C

	int ydb_data_s(unsigned int *value,
		int count,
		ydb_string_t *varname[,
		ydb_string_t *subscript, ...]);

In the location pointed to by ``value``, ``ydb_data_s()`` returns the
following information about the local or global variable node
identified by ``*varname`` and the ``*subscript`` list.

- 0 — There is neither a value nor a subtree, i.e., it is undefined.
- 1 — There is a value, but no subtree
- 10 — There is no value, but there is a subtree.
- 11 — There are both a value and a subtree.

-----------
ydb_get_s()
-----------
 
.. code-block:: C

	int ydb_get_s(ydb_string_t *value,
		int count,
		ydb_string_t *varname[,
		ydb_string_t *subscript, ... ]);

If ``value->length`` is large enough to accommodate the result, to the
location pointed to by ``value->address``, ``ydb_get_s()`` copies the
value of the value of the data at the specified node or intrinsic
special variable, setting ``value->used``, and returning
``YDB_OK``; and ``YDB_ERR_INVSTRLEN`` otherwise.

If there is no value at the specified global or local variable node,
or if the intrinsic special variable does not exist,a non-zero return
value of YDB_ERR_GVUNDEF, YDB_ERR_INVSVN, or YDB_ERR_UNDEF indicates
the error.

Note: In a database application, a global variable node can
potentially be changed by another process between the time that a
process calls ``ydb_length()`` to get the length of the data in a node
and a subsequent call to ``ydb_get()`` to get that data. If a caller
cannot ensure from the application design that the size of the buffer
it provides is large enough for a string returned by ``ydb_get()``, it
should code in anticipation of a potential ``YDB_ERR_INVSTRLEN``
return code from ``ydb_get()``. See also the discussion at
`YDB_ERR_INVSTRLEN`_ describing the contents of ``*value`` when
``ydb_get_s()`` returns a ``YDB_ERR_INVSTRLEN`` return
code. Similarly, since a node can always be deleted between a call
such as ``ydb_node_next_s()`` and a call to ``ydb_get_s()``, a caller
of ``ydb_get_s()`` to access a global variable node should code in
anticipation of a potential ``YDB_ERR_GVUNDEF``.

------------
ydb_kill_s()
------------

.. code-block:: C

	int ydb_kill_s(int namecount,
		[int count,
		ydb_string_t *varname[,
		ydb_string_t *subscript, ...], ...,]);

``namecount`` is the number of variable names in the call.

Kills — deletes all nodes in — each of the local or global variable
trees or subtrees specified. In the special case where ``namecount``
is zero, ``ydb_kill_s()`` kills all local variables.

-----------------
ydb_kill_excl_s()
-----------------

.. code-block:: C

	int ydb_kill_excl_s(ydb_string_t *varnamelist);

``*varnamelist->address`` points to a comma separated list of local
variable names. ``ydb_kill_excl_s()`` kills the trees of all local
variable names except those on the list.

--------------
ydb_length_s()
--------------

.. code-block:: C

	int ydb_length_s(unsigned int *value,
		int count,
		ydb_string_t *varname[,
		ydb_string_t *subscript, ... ]);

In the location pointed to by ``*value``, ``ydb_length_s()`` reports
the length of the data in bytes. If the data is numeric, ``*value``
has the length of the canonical string representation of that value.

If there is no value at the requested global or local variable node,
or if the intrinsic special variable does not exist,a non-zero return
value of YDB_ERR_GVUNDEF, YDB_ERR_INVSVN, or YDB_ERR_UNDEF indicates
the error.

------------
ydb_lock_s()
------------

.. code-block:: C

	int ydb_lock_s(unsigned long long timeout, int namecount,
		[int count,
		ydb_string_t *varname[,
		ydb_string_t *subscript, ...], ...,]);

``namecount`` is the number of variable names in the call.

Release any locks held by the process, attempt to acquire all the
requested locks. While the release is unconditional, on return, the
function will have acquired all requested locks or none of them. If no
locks are requested, the function releases all locks and returns
``YDB_OK``.

``timeout`` specifies a time in microseconds that the function waits
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

	int ydb_lock_s(int namecount,
		[int count,
		ydb_string_t *varname[,
		ydb_string_t *subscript, ...], ...,]);

``namecount`` is the number of variable names in the call.

Decrements counts of the specified locks held by the process. As
noted in the `Concepts`_ section, a lock whose count goes from 1 to 0
is released. Any lock whose name is specified in the argument list,
but which the process does not hold, is ignored.

As releasing locks cannot fail, the function returns ``YDB_OK``,
unless there is an error such as an invalid name that results in the
return of an error code such as ``YDB_ERR_INVVARNAME``.

-----------------
ydb_lock_incr_s()
-----------------

.. code-block:: C

	int ydb_lock_s(unsigned long long timeout, int namecount,
		[int count,
		ydb_string_t *varname[,
		ydb_string_t *subscript, ...], ...,]);

``namecount`` is the number of variable names in the call.

Without releasing any locks held by the process, attempt to acquire
all the requested locks, and increment any locks already held by the
process. On return, the process will have acquired all requested
locks, and incremented those already held, or will have neither
acquired nor incremented any of them. If no locks are specified, the
function returns ``YDB_OK`` (i.e., it is a no-op).

``timeout`` specifies a time in microseconds that the function waits
to acquire the requested locks. If it is not able to acquire all
requested locks, it acquires no locks, returning with a
``YDB_LOCK_TIMEOUT`` return value.

If ``timeout`` is zero, the function makes exactly one attempt to
acquire the locks, and if it is unable to, it returns
``YDB_LOCK_TIMEOUT``.

If all requested locks are successfully acquired, the function returns
``YDB_OK``.

-----------------
ydb_node_next_s()
-----------------
		
.. code-block:: C

	int ydb_node_next_s(int *count,
		ydb_string_t *varname,
		ydb_string_t *subscript[, ... ]);

``ydb_node_next_s()`` facilitates depth-first traversal of a local or
global variable tree. Note that the parameters are both inputs to  the
function as well as outputs from the function, and that the number of
subscripts can differ between the input node of the call and the
output node reported by the call, which is the reason the number of
subscripts is passed by reference.

As an input parameter ``*count`` specifies the number of subscripts in
the input node, which does not need to exist — a value of 0 will
return the first node in the tree.

Except when the ``int`` value returned by
``ydb_node_next_s()`` returns an error code, ``*count`` on the return
from a call specifies the number of subscripts in the next node, which
will be a node with data unless there is no next node (i.e., the input
node is the last in the tree), in which case ``*count`` will be 0 on
output.

``ydb_node_next_s()`` does not change ``*varname``, but does change
the ``*subscript`` parameters.

- A ``YDB_ERR_INSUFFSUBS`` return code indicates an error if there are
  insufficient parameters to return the subscript. In this case
  ``*count`` reports the actual number of subscripts in the node, and
  the parameters report as many subscripts as can be reported.
- If one of the ``subscript->length`` values indicates insufficient
  space for an output value, the return code is the error
  ``YDB_ERR_INVSTRLEN``. See also the discussion at
  `YDB_ERR_INVSTRLEN`_ describing the contents of that ``*subscript``
  parameter. In the event of a ``YDB_ERR_INVSTRLEN`` error, the values
  in any subscripts beyond that identified by ``*count`` do not
  contain meaningful values.

Note that a call to ``ydb_node_next_s()`` must always have at least
one ``*subscript`` parameter, since it is a *non-sequitur* to call it
without subscripts and expect a return without subscripts.

---------------------
ydb_node_previous_s()
---------------------

.. code-block:: C

	int ydb_node_previous_s(int *count,
		ydb_string_t *varname,
		[ ydb_string_t *subscript, ... ]);

Analogous to ``ydb_node_next(s)``, ``ydb_node_previous_s()``
facilitates breadth-first traversal of a local or global variable
tree, except that:

- ``ydb_node_previous_s()`` reports the predecessor node,
- an input value of 0 for ``*value`` reports the last node in the tree
  on output, and 
- an output value of 0 for ``*value`` means there is no previous node.

Other behavior of ``ydb_node_previous_s()`` is the same as
`ydb_node_next_s()`_.

-----------
ydb_set_s()
-----------

.. code-block:: C

	int ydb_set_s(ydb_string_t *value,
		int count,
		ydb_string_t *varname[,
		ydb_string_t *subscript, ... ]);

Copies the ``value->used`` bytes at ``value->address`` as the value of
the specified node or intrinsic special variable specified, returning
``YDB_OK`` or an error code such as ``YDB_ERR_INVSVN``.

----------------------
ydb_subscript_next_s()
----------------------

.. code-block:: C

	int ydb_subscript_next_s(int *count,
		ydb_string_t *varname[, ydb_string_t *subscript, ... ]);

``ydb_subscript_next_s()`` returns the next subscript at the deepest
level specified by ``*count``, by copying that next subscript to the
memory referenced by that ``subscript->address``, and setting the
corresponding ``subscript->used`` with its length. If there is no next
subscript at that level, it decrements ``*count``. [#]_

.. [#] This behavior provides symmetry with
       `ydb_subscript_previous_s()`_.

If ``*count`` is zero, ``ydb_subscript_next_s()`` returns the next
local or global variable name, and if ``*varname`` references the
last variable name, ``*count`` is -1 on the return.

--------------------------
ydb_subscript_previous_s()
--------------------------

.. code-block:: C

	int ydb_subscript_previous_s(int *count,
		ydb_string_t *varname[,	ydb_string_t *subscript, ... ]);

``ydb_subscript_previous_s()`` returns the preceding subscript at the
deepest level specified by ``*count``, by copying that previous
subscript to the memory referenced by that ``subscript->address``, and
setting the corresponding ``subscript->used`` to its length. If there
is no previous subscript, it decrements ``*count``. [#]_

.. [#] Since the empty string is a legal subscript and is the first in
       YottaDB's natural collation order, simply setting
       ``subscript->used`` to zero does not discriminate between the
       case where the input specifies the first subscript, and the
       case where there actually is a preceding node with the empty
       string as a subscript. Decrementing ``*count`` allows the
       Simple API to discriminate between the two cases.

If ``*count`` is zero, ``ydb_subscript_previous_s()`` returns the
preceding local or global variable name, and if ``*varname``
references the first variable name, ``*count`` is -1 on the return.

----------
ydb_tp_s()
----------

.. code-block:: C

	int ydb_tp(ydb_string_t *tpfn,
		ydb_string_t *transid,
		ydb_string_t *varnamelist);

The string referenced by ``*tpfn`` is the name of a function returning
a value that has one of the following forms with no embedded spaces:

- ``package.function[(param[,param],...)]`` where ``package.function``
  maps to an external call as described in Chapter 11 (Integrating
  External Routines) of `GT.M Programmers Guide
  <http://tinco.pair.com/bhaskar/gtm/doc/books/pg/UNIX_manual/>`_.
- ``routine^label[(param[,param,...])]`` where ``routine^label`` maps
  to an M entry reference as described in Chapter 5 (General Language
  Features of M) of `GT.M Programmers Guide
  <http://tinco.pair.com/bhaskar/gtm/doc/books/pg/UNIX_manual/>`_.

In both cases, ``package.function`` or ``routine^label`` should
return one of the following:

- ``YDB_OK`` — application logic indicates that the transaction can
  be committed (the YottaDB engine may still decide that a restart is
  required to ensure ACID transaction properties)
- ``YDB_RESTART``  — application logic indicates that the
  transaction should restart
- ``YDB_ROLLBACK`` — application logic indicates that the transaction
  should not be committed

----------------
ydb_withdraw_s()
----------------

.. code-block:: C

	int ydb_withdraw_s(int namecount,
		int count,
		ydb_string_t *varname[,
		ydb_string_t *subscript, ...][, ...]);

``namecount`` (≥1) is the number of variable names in the call.

Deletes the root node in each of the local or global variable
trees or subtrees specified, leaving the subtrees intact.

Comprehensive API
=================

The Comprehensive API is a project for the future.

Utility Functions
=================

Utility functions are functions that are not core to YottaDB
functionality, but which may be useful to C application code.

**Need to add hiber_start, hiber_start_wait_any, start_timer,
and cancel_timer to this section.**

----------
ydb_free()
----------

.. code-block:: C

	int ydb_free(void *ptr)

Releases memory previously allocated by ``ydb_malloc()``. Passing
``ydb_free()`` a pointer not previously provided to the application by
``ydb_malloc()`` can result in unpredictable behavior. The signature
of ``ydb_free()`` matches that of the POSIX ``free()`` call.

------------
ydb_malloc()
------------

.. code-block:: C

	void *ydb_malloc(size_t size)

With a signature matching that of the POSIX ``malloc()`` call,
``ydb_malloc()`` returns an address to a block of memory of the
requested size, or NULL if it is unable to satisfy the request. As
``ydb_malloc()`` uses a `buddy system
<https://en.wikipedia.org/wiki/Buddy_memory_allocation>`_, it may be
more efficient than the system ``malloc()``. Also, it provdes
debugging functionality under the control of the environment variable
``ydbdbglevel``.

-------------
ydb_message()
-------------

.. code-block:: C

	int ydb_message(ydb_string_t *msgtext, int status)

Set ``msgtext->address`` to a location that has the text for the
condition corresponding to ``status``, and both ``msgtext->length`` and
``msgtext->used`` to its length (with no trailing null
character). Note: as ``msgtext->address`` points to an address in a
read-only region of memory, any attempt to modify the message will
result in a segmentation violation (SIGSEGV). ``ydb_message()``
returns ``YDB_OK`` for a valid ``status`` and
``YDB_ERR_UNKNOWN`` if ``status`` does not map to a known error.

================
Programming in M
================

As YottaDB is built on `FIS GT.M <http://fis-gtm.com>`_ , it includes
a complete implementation of the `M <https://en.wikipedia.org/wiki/MUMPS>`_ programming language (also
known as MUMPS) that mostly conforms to
`ISO/IEC 11756:1999 <http://www.iso.ch/iso/en/CatalogueDetailPage.CatalogueDetail?CSNUMBER=29268&ICS1=35&ICS2=60&ICS3=&scopelist>`_.
The
`GT.M Programmers Guide  <http://tinco.pair.com/bhaskar/gtm/doc/books/pg/UNIX_manual/>`_
documents programming YottaDB in M and is not duplicated here.

=================
Programming Notes
=================

Numeric Considerations
======================

To ensure the accuracy of financial calculations, [#]_ YottaDB internally
stores numbers as, and performs arithmetic using, a scaled packed
decimal representation with 18 signicant decimal digits, with
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

Signals
=======

As ``libyottadb.so`` includes a database engine that uses timers and
signals, YottaDB uses signals, especially timers.  YottaDB strongly
discourages the use of signals, especially SIGALARM, in application
code functions. Use the exposed timer APIs for application timing
functionality (see `Utility Functions`_).
