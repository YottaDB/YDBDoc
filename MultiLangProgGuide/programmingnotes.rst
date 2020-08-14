.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.     #
.. # All rights reserved.                                        #
.. #                                                             #
.. #     This source code contains the intellectual property     #
.. #     of its copyright holder(s), and is made available       #
.. #     under a license.  If you do not know the terms of       #
.. #     the license, please stop and do not read further.       #
.. #                                                             #
.. ###############################################################

.. _Programming Notes:

============================================
Programming Notes (Avoiding Common Pitfalls)
============================================

.. contents::
   :depth: 5

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

In contexts where a string is coerced to a number (for example
`ydb_incr_s() / ydb_incr_st() <./cprogram.html#ydb-incr-s-ydb-incr-st>`_) the coercion rules are the same as the
`M “+” unary operator <https://docs.yottadb.com/ProgrammersGuide/langfeat.html#arithmetic-operators>`_.

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
  functionality (see `Utility Functions <./cprogram.html#utility-functions>`_).
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
  parameters are IGNORED. In M mode with a yottadb executable, behavior is
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
  dumps in the description of `ydb_fork_n_core() <./cprogram.html#ydb-fork-n-core>`_. Although YottaDB
  normally cleans up processes' interaction with databases on exit,
  these signals can indicate that the process is in a bad state and that
  its code and data cannot be trusted. The process therefore does
  not attempt to clean up before exit. After a fatal signal, *no*
  YottaDB functions can be called except `ydb_exit() <./cprogram.html#id30>`_.  In the
  event an application *must* use its own handler for one of
  these signals, it must either save YottaDB's handler, and drive
  it before process termination or call `ydb_exit() <./cprogram.html#id30>`_ prior to
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

As database operations such as `ydb_set_s() <./cprogram.html#ydb-set-s-ydb-set-st>`_ set timers, subsequent
system calls can terminate prematurely with an EINTR. Such system
calls should be wrapped to restart them when this occurs. An example
from the file `eintr_wrappers.h
<https://gitlab.com/YottaDB/DB/YDB/blob/master/sr_port/eintr_wrappers.h>`_
demonstrates how YottaDB itself is coded to handle system calls that
terminate prematurely with an EINTR:

.. code-block:: C

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
`Client/Server Operation <./MultiLangProgGuide.html#client-server-operation>`_).

To reiterate because of its importance: **never** replace YottaDB's
:code:`SIGALRM` handler.

Forking
=======

In this section, :code:`fork()` refers to the :code:`fork()` system
call as well as other functions that may use :code:`fork()` under the
covers or effect similar functionality by other means.

Before a process that performs buffered IO executes :code:`fork()`, it
should execute :code:`fflush()`. Otherwise, the child process will
inherit unflushed buffers from the parent, which the child process
will flush when it executes an :code:`fflush()`. This is a general
programming admonition, not specific to YottaDB except to the extent
that M code within a parent process may have executed :code:`write`
commands which are still buffered when C code within the same
process calls :code:`fork()`.

An application that calls YottaDB functions from multiple threads
within a process *must* ensure that only one thread at a time calls
:code:`fork()`. Failure to do so can result in unanticipated results,
including abnormal process termination and structural damage to
database files.

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
  return. If an application calls `ydb_get_s() / ydb_get_st() <./cprogram.html#ydb-get-s-ydb-get-st>`_ for
  the value of `$zstatus <./MultiLangProgGuide.html#zstatus>`_ for the complete error text when a YottaDB
  function returns an `error return code <./cprogram.html#error-return-code>`_, for a single-threaded
  application, `$zstatus <./MultiLangProgGuide.html#zstatus>`_ has correct and current information, since
  calls to YottaDB are entirely under the control of that single
  application thread. For a multi-threaded application, between the
  time a function returns with an `error return code <./cprogram.html#error-return-code>`_, and a
  subsequent call to `ydb_get_st() <./cprogram.html#ydb-get-s-ydb-get-st>`_ to
  get the value of `$zstatus <./MultiLangProgGuide.html#zstatus>`_,
  another thread may call YottaDB, and the `$zstatus <./MultiLangProgGuide.html#zstatus>`_ returned will
  be from that subsequent call. A :code:`*errstr` parameter in
  functions for multi-threaded applications provides the `$zstatus <./MultiLangProgGuide.html#zstatus>`_ for
  that call to the caller.

  - An application that does not want the `$zstatus <./MultiLangProgGuide.html#zstatus>`_ string can pass
    a :code:`NULL` value for :code:`*errstr`.

  - The string in :code:`errstr->buf_addr` is always null terminated, which
    allows :code:`*errstr` to be passed to standard system functions
    like :code:`printf()`.

  - In the event a buffer provided by an application is not long
    enough for a `$zstatus <./MultiLangProgGuide.html#zstatus>`_, YottaDB truncates the string to be
    reported, rather than issuing an INVSTRLEN error (since a second
    error while attempting to report an error is likely to add
    confusion rather than enlightenment).

    - :code:`errstr->len_used` is always set to the length of `$zstatus <./MultiLangProgGuide.html#zstatus>`_,
      whether or not it is truncated.
    - If :code:`errstr->len_used` is greater than
      :code:`errstr->len_alloc-1` it means `$zstatus <./MultiLangProgGuide.html#zstatus>`_ has been
      truncated.

- A multi-threaded application is permitted to use the YottaDB
  single-thread functions *as long as the application ensures that all
  YottaDB access is performed only by one thread.* A thread may use
  the `ydb_thread_is_main() <./cprogram.html#ydb-thread-is-main>`_
  to determine whether it is the thread that is calling
  YottaDB. YottaDB strongly recommends against this application design
  pattern: this functionality only exists to provide backward
  compatibility to a specific existing application code base.

Even though the YottaDB data management engine is single-threaded and
operates in a single thread, [#]_ it supports both single- and
multi-threaded applications. Multi-threaded applications may call
multi-threaded `Simple API <./cprogram.html#simple-api>`_ functions – those whose names end in
:code:`_st()` – as well as utility functions – those whose names end
in :code:`_t()`. Single-threaded applications may call the `Simple
API <./cprogram.html#simple-api>`_ single-threaded functions – those whose names end in
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
applications that do not use `Transaction Processing <./MultiLangProgGuide.html#transaction-processing>`_, transaction
processing in a threaded environment requires special consideration
(see `Threads and Transaction Processing`_).

`Programming in M <./MultiLangProgGuide.html#programming-in-m>`_ is single-threaded and single-threaded
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

As discussed in `Transaction Processing <./MultiLangProgGuide.html#transaction-processing>`_,
`ydb_tp_s() or ydb_tp_st() <./cprogram.html#ydb-tp-s-ydb-tp-st>`_ are called with a pointer to the function that is
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
`ydb_tp_st() <./cprogram.html#ydb-tp-s-ydb-tp-st>`_. Therefore

- The YottaDB engine must know the transaction nesting level at which
  it is operating, responding to requests for service at that level,
  and block any transaction invocations at a higher (enclosing) level
  until the current transactio is closed (committed or rolled back).
- After a transaction has closed, any further calls from threads
  invoking YottaDB for the closed transaction must receive errors.

To accomplish this, the `Simple API <./cprogram.html#simple-api>`_ functions for threaded
applications – those ending in :code:`_st()` – have a :code:`tptoken`
first parameter used as follows to provide the required transaction
context of a thread.

- When an application calls a `Simple API <./cprogram.html#simple-api>`_ function outside a
  transaction, it provides a value of :code:`YDB_NOTTP` for
  :code:`tptoken`.
- When an application calls `ydb_tp_st() <./cprogram.html#ydb-tp-s-ydb-tp-st>`_, it generates a
  :code:`tptoken` as the first parameter when it calls the function
  that implements the logic for the transaction. Any threads that this
  function spawns must provide this :code:`tptoken` to
  YottaDB. Passing in a different or incorrect :code:`tptoken` can
  result in hard-to-debug application behavior, including deadlocks.
- When a `Simple API <./cprogram.html#simple-api>`_ function is called:

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

Memory allocated by `ydb_malloc() <./cprogram.html#ydb-malloc>`_ must be explicitly freed by
`ydb_free() <./cprogram.html#ydb-free>`_.
`ydb_exit() <./cprogram.html#id30>`_ does not free memory, and any
memory allocated but not freed prior to `ydb_exit() <./cprogram.html#id30>`_ is released
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
