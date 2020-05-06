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

================
Programming in C
================

.. contents::
   :depth: 5


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
`Transaction Processing <./MultiLangProgGuide.html#transaction-processing>`_ for a discussion of restarts.

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
`$zstatus <./MultiLangProgGuide.html#zstatus>`_.

Error messages can be raised by the YottaDB runtime system or by the
underlying operating system.

- A full set of YottaDB error messages and numbers is in the `YottaDB
  Messages and Recovery Procedures Manual
  <https://docs.yottadb.com/MessageRecovery/>`_.
- Linux error messages are described in Linux documentation,
  e.g. `errno <https://linux.die.net/man/3/errno>`_.

Remember that the error codes returned by YottaDB functions are the
negated numeric values of the error codes above.

:CODE:`YDB_ERR_CALLINAFTERXIT` – A YottaDB function was called after
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
return values. Note that as the number of parameters is a count, when
array subscripts start at 0, an array subscript of *n* corresponds to
*n+1* parameters.

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
`ydb_hiber_start()`_. Note that even if timer resolution is in
nanoseconds, the accuracy is always determined by the underlying
hardware and operating system, as well as factors such as system load.

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

:CODE:`YDB_SEVERITY_INFORMATIONAL` – The number corresponds to an informational
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
`transaction <./MultiLangProgGuide.html#transaction>`_.

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
a :code:`ydb_buffer_t` structure to refer to a literal (such as a
variable name). With a string literal, and
a pointer to a :code:`ydb_buffer_t` structure,
set:

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

.. _HASH128_STATE_INIT():

:code:`HASH128_STATE_INIT(hash128_state_t *state, ydb_uint8 addl_seed)`  - Use this macro to initialize a variable in order to compute a 128-bit MurMurHash using `ydb_mmrhash_128_ingest()`_.

Example:

.. code-block:: C

   // Initialize state struct
   HASH128_STATE_INIT(hash_state, 0);

YottaDB functions are divided into:

- Simple API — a core set of functions that provides easy-to-use
  access to the major features of YottaDB.
- Comprehensive API — a more elaborate set of functions for
  specialized or optimized access to additional functionality within
  :code:`libyottadb.so` that YottaDB itself uses. The Comprehensive API is
  a project for the future.
- Utility Functions — Functions useful to a C application using
  YottaDB.

:code:`YDB_STRING_TO_BUFFER` — Sets a :code:`ydb_buffer_t` structure
to point to an existing null-terminated C string, i.e.,

.. code-block:: C

   #define YDB_STRING_TO_BUFFER(STRING, BUFFERP)                           \
   {                                                                       \
	   (BUFFERP)->buf_addr = STRING;                                   \
	   (BUFFERP)->len_used = (BUFFERP)->len_alloc = strlen(STRING);    \
   }


Simple API
==========

As all subscripts and node data passed to YottaDB using the Simple API
are strings, use the :code:`sprintf()` and :code:`atoi()/strtoul()` family of
functions to convert between numeric values and strings which are
`canonical numbers <./programmingnotes.html#canonical-numbers>`_.

Note that *all* parameters passed to Simple API functions must be properly allocated and initialized where needed
prior to the function call, including return values. This also specifically includes all members of `ydb_buffer_t` structs
for parameters containing input values, but only `buf_addr` and `len_alloc` members for return values. To facilitate
initialization of the `ydb_buffer_t` members, you may find the `YDB_MALLOC_BUFFER` macro helpful for
heap allocations.

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
discussion in `Threads <./programmingnotes.html#threads>`_ provides more detailed information.

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

Please see the `Simple API introduction <./cprogram.html#simple-api>`_ for details about parameter allocation.

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

Please see the `Simple API introduction <./cprogram.html#simple-api>`_ for details about parameter allocation.

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

Please see the `Simple API introduction <./cprogram.html#simple-api>`_ for details about parameter allocation.

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

To the user-allocated location pointed to by :code:`ret_value->buf_addr`,
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

Please see the `Simple API introduction <./cprogram.html#simple-api>`_ for details about parameter allocation.

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
  the value to a number if it is not a `canonical number <./programmingnotes.html#canonical-numbers>`_, defaulting to
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

Please see the `Simple API introduction <./cprogram.html#simple-api>`_ for details about parameter allocation.

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

Please see the `Simple API introduction <./cprogram.html#simple-api>`_ for details about parameter allocation.

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
noted in the `Concepts <./MultiLangProgGuide.html#concepts>`_ section, a lock whose count goes from 1 to 0
is released. A lock whose name is specified, but which the process
does not hold, is ignored.

As releasing a lock cannot fail, the function returns :CODE:`YDB_OK`,
unless there is an error such as an invalid name that results in the
return of an error code such as :CODE:`YDB_ERR_INVVARNAME`. Errors
result in an appropriate `error return code`_. :CODE:`YDB_ERR_PARAMINVALID`
is returned when :code:`len_alloc` < :code:`len_used` or the :code:`len_used` is non-zero
and :code:`buf_addr` is NULL in at least one subscript in :code:`subsarray`.

Please see the `Simple API introduction <./cprogram.html#simple-api>`_ for details about parameter allocation.

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

Please see the `Simple API introduction <./cprogram.html#simple-api>`_ for details about parameter allocation.

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

Please see the `Simple API introduction <./cprogram.html#simple-api>`_ for details about parameter allocation.

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
facilitate reverse depth-first traversal of a local or global
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

Please see the `Simple API introduction <./cprogram.html#simple-api>`_ for details about parameter allocation.

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

Please see the `Simple API introduction <./cprogram.html#simple-api>`_ for details about parameter allocation.

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
:code:`ydb_str2zwr_st()` provide the `zwrite formatted <./programmingnotes.html#zwrite-formatted>`_ version of
the string pointed to by :code:`*str`, returning:

- :CODE:`YDB_OK`;
- :CODE:`YDB_ERR_INVSTRLEN` if the :code:`*zwr` buffer is not long enough;
- :CODE:`YDB_ERR_PARAMINVALID` if :code:`zwr` is NULL or :code:`zwr->buf_addr` is
  NULL and the return value has a non-zero :code:`len_used`; or
- another applicable `error return code`_.

Please see the `Simple API introduction <./cprogram.html#simple-api>`_ for details about parameter allocation.

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

Please see the `Simple API introduction <./cprogram.html#simple-api>`_ for details about parameter allocation.

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

Please see the `Simple API introduction <./cprogram.html#simple-api>`_ for details about parameter allocation.

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
referenced by :code:`tpfn` passing it :code:`tpfnparm` as a
parameter. Additionally, :code:`ydb_tp_st()` also generates a
new :code:`tptoken` that it passes as a parameter to the
function referenced by its :code:`tpfn` parameter.

As discussed under `Transaction Processing <./MultiLangProgGuide.html#transaction-processing>`_, a function implementing
transaction processing logic should use the intrinsic special variable
:code:`$trestart` to manage any externally visible action (which
YottaDB recommends against, but which may be unavoidable). The
function referenced by :code:`tpfn` should return one of the
following:

- :CODE:`YDB_OK` — application logic indicates that the transaction can
  be committed (the YottaDB engine may still decide that a restart is
  required to ensure ACID transaction properties) as discussed under
  `Transaction Processing <./MultiLangProgGuide.html#transaction-processing>`_.
- :CODE:`YDB_TP_RESTART`  — application logic indicates that the
  transaction should restart.
- :CODE:`YDB_TP_ROLLBACK` — application logic indicates that the
  transaction should not be committed.
- :CODE:`YDB_ERR_PARAMINVALID` when :code:`len_alloc` < :code:`len_used` or the :code:`len_used` is non-zero
  and :code:`buf_addr` is NULL in at least one variable name in :code:`varnames`.
- An `error return code`_ returned by a YottaDB function called by the
  function. This case is treated the same way as if `YDB_TP_ROLLBACK` was returned
  (i.e. the application indicates that this transaction should not be committed).

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
- :CODE:`YDB_ERR_TPTIMEOUT` (see `Transaction Processing <./MultiLangProgGuide.html#transaction-processing>`_); or
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

.. note:: If the transaction logic receives a :code:`YDB_TP_RESTART` from a YottaDB function that it calls, it *must* return that value to the calling :code:`ydb_tp_s()` or :code:`ydb_tp_st()`. Failure to do so could result in application level data inconsistencies and hard to debug application code.

Please see the `Simple API introduction <./cprogram.html#simple-api>`_ for details about parameter allocation.

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
string described by the `zwrite formatted <./programmingnotes.html#zwrite-formatted>`_ string pointed to by
:code:`*zwr`, returning

- :CODE:`YDB_OK` (with :code:`str->len_used` set to zero if the zwrite formatted string has an error);
- :CODE:`YDB_ERR_INVSTRLEN` error if the :code:`*str` buffer is not long enough;
- :CODE:`YDB_ERR_PARAMINVALID` either if the :code:`*str` buffer is NULL or the return value contains a
  non-zero :code:`len_used`  and the :code:`str->buf_addr` is NULL.

Please see the `Simple API introduction <./cprogram.html#simple-api>`_ for details about parameter allocation.

Comprehensive API
=================

The Comprehensive API is a project for the future.

Utility Functions
=================

Utility functions are functions that are not core to YottaDB
functionality, but which are useful to application code.

Utility functions whose names end in :code:`_t()` are for use by
multi-threaded applications, and those which do not are for
single-threaded applications. The discussion in `Threads <./programmingnotes.html#threads>`_ provides
more detailed information.

`ydb_hiber_start()`_ and `ydb_hiber_start_wait_any()`_ are for use only with the SimpleAPI and not with the
threaded Simple API.

`ydb_exit()`_, `ydb_fork_n_core()`_, and
`ydb_init()`_ do not have separate variants for single- and
multi-threaded applications and are suitable for both.

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

.. _ydb_ci_tab_open():
.. _ydb_ci_tab_open_t():

---------------------------------------
ydb_ci_tab_open() / ydb_ci_tab_open_t()
---------------------------------------

.. code-block:: C

        int ydb_ci_tab_open(char *fname, uintptr_t *ret_value)

        int ydb_ci_tab_open_t(uint64_t tptoken,
                ydb_buffer_t *errstr, char *fname, uintptr_t *ret_value)

Opens the call-in table contained in the file name :code:`fname`. Using the filled in :code:`ret_value`
handle in a later :code:`ydb_ci_tab_switch()`/:code:`ydb_ci_tab_switch_t()` call, one can switch to
this call-in table as the currently active call-in table. All calls to
:code:`ydb_cip()`/:code:`ydb_cip_t()`/:code:`ydb_ci()`/:code:`ydb_ci_t()` use the currently active
call-in table. This lets applications open any number of call-in tables across the lifetime of a process.
The :code:`ydb_ci` environment variable, if set, points to the default call-in table that YottaDB uses
unless the active call-in table is switched using :code:`ydb_ci_tab_switch()`/:code:`ydb_ci_tab_switch_t()`.
The call-in table pointed to by :code:`ydb_ci`, the default call-in table, need not be explicitly opened
with :code:`ydb_ci_tab_open()`/:code:`ydb_ci_tab_open_t()`.

Returns:

- :code:`YDB_OK` if the open was successful and fills in a handle to the opened table in :code:`ret_value`; or
- :code:`YDB_ERR_PARAMINVALID` if the input parameters :code:`fname` or :code:`ret_value` are NULL; or
- a negative error return code (for example, if the call-in table in the file had parse errors).

Please see the `Simple API introduction <./cprogram.html#simple-api>`_ for details about parameter allocation.

-------------------------------------------
ydb_ci_tab_switch() / ydb_ci_tab_switch_t()
-------------------------------------------

.. code-block:: C

        int ydb_ci_tab_switch(uintptr_t new_handle, uintptr_t *ret_old_handle)

        int ydb_ci_tab_switch_t(uint64_t tptoken,
                ydb_buffer_t *errstr, uintptr_t new_handle, uintptr_t *ret_old_handle)

Switches the currently active call-in table to the handle :code:`new_handle` (returned by a previous call
to :code:`ydb_ci_tab_open()`/:code:`ydb_ci_tab_open_t()`) and fills in the previously
active call-in table handle in :code:`*ret_old_handle`. An application that wishes to switch back to the
previous call-in table at a later point would call :code:`ydb_ci_tab_switch()`/:code:`ydb_ci_tab_switch_t()`
again with :code:`*ret_old_handle` as the :code:`new_handle` parameter. The special value of NULL passed in
:code:`new_handle` switches the active call-in table to the default call-in table (the call-in table pointed
to by the :code:`ydb_ci` environment variable).

Returns:

- :code:`YDB_OK` if the open was successful and fills in a handle to the opened table in :code:`ret_value`; or
- :code:`YDB_ERR_PARAMINVALID` if the output parameter :code:`ret_old_handle` is NULL or if the
  input parameter :code:`new_handle` points to an invalid handle (i.e. not returned by a prior
  :code:`ydb_ci_tab_open()`/:code:`ydb_ci_tab_open_t()`) call); or
- a negative error return code

Note that application code using the :code:`ydb_cip()`/:code:`ydb_cip_t()` functions provides
YottaDB with a pointer to a :code:`ci_name_descriptor` structure that includes a handle. YottaDB uses the
current call-in table to set the handle the first time that the associated function is called. Thereafter,
the handle is immutable, and switching the call-in table leaves unchanged the mapping for functions whose
handles have already been set. Use :code:`ydb_ci()`/:code:`ydb_ci_t()` for application code that requires
the called function to change when the call-in table changes.

Please see the `Simple API introduction <./cprogram.html#simple-api>`_ for details about parameter allocation.

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
a :code:`YDB_ERR_CALLINAFTERXIT` error.

Note that:

- a typical application should not need to call
  :code:`ydb_exit()`, but should instead just terminate with a call to
  :code:`exit()` which will perform any cleanup needed by YottaDB; and
- calling :code:`ydb_exit()` before calling any other YottaDB function
  does nothing, i.e., it is a no-op.

:code:`ydb_exit()` returns :code:`YDB_OK` on success, and a positive non-zero value on error.
If :code:`ydb_exit()` has already been called, later calls to :code:`ydb_exit()` in the same process return :code:`YDB_OK` with no further action, since all resources related to YottaDB are already cleaned up by the first call.

If an external call attempts to call :code:`ydb_exit()`, a :code:`YDB_ERR_INVYDBEXIT` error is returned, since YottaDB
is required to remain operational even after the external call returns. For information about this error, see
`INVYDBEXIT <https://docs.yottadb.com/MessageRecovery/errors.html#invydbexit>`_ in the Messages and Recovery Procedures guide.

:code:`ydb_exit()` can be used with both the Simple API and threaded Simple API.

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

A :code:`PARAMINVALID` error is issued if the input :code:`fileid` parameter is NULL.

Please see the `Simple API introduction <./cprogram.html#simple-api>`_ for details about parameter allocation.

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

A :code:`PARAMINVALID` error is issued if the input :code:`fileid` parameter is NULL.

Please see the `Simple API introduction <./cprogram.html#simple-api>`_ for details about parameter allocation.

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
return :code:`YDB_OK`, or an error return code.

A :code:`PARAMINVALID` error is issued if the input :code:`filename` or :code:`fileid` parameter is NULL.

Please see the `Simple API introduction <./cprogram.html#simple-api>`_ for details about parameter allocation.

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

:code:`ydb_fork_n_core()` can be used with both the Simple API and threaded Simple API.

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

:code:`ydb_free()` should not be used in
multiple threads in multi-threaded programs. (See the `Threads <./programmingnotes.html#threads>`_ section for details). However, the :CODE:`YDB_FREE_BUFFER` macro is safe
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

:code:`ydb_hiber_start()` should not be used in multiple threads in multi-threaded programs. (See the `Threads <./programmingnotes.html#threads>`_ section for details).

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

:code:`ydb_hiber_start_wait_any()` should not be used in multiple threads in multi-threaded programs. (See the `Threads <./programmingnotes.html#threads>`_ section for details).

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
set its own signal handlers (see `Signals <./programmingnotes.html#signals>`_): :code:`ydb_init()`
sets signal handlers, and in case an application
wishes to set its own signal handlers for signals not used by YottaDB,
it can call :code:`ydb_init()` before setting
its signal handlers.

:code:`ydb_init()` returns :code:`YDB_OK` on success, and a positive non-zero value otherwise.
On failure, the error message text corresponding to the non-zero return value can be obtained
by immediately calling :code:`ydb_zstatus()`.

If :code:`ydb_init()` has already been called, later calls to :code:`ydb_init()` in the same
process return :code:`YDB_OK` with no further action, since the YottaDB runtime has already been initialized.

:code:`ydb_init()` can be used with both the Simple API and threaded Simple API.

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

:code:`ydb_malloc()` should not be used in
multiple threads in multi-threaded programs. (See the `Threads <./programmingnotes.html#threads>`_ section for details). However, the :CODE:`YDB_MALLOC_BUFFER` macro is safe
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

Please see the `Simple API introduction <./cprogram.html#simple-api>`_ for details about parameter allocation.

-----------------------------
ydb_mmrhash_32()
-----------------------------

.. code-block:: C

    void ydb_mmrhash_32(const void *key, int len, uint4 seed, uint4 *out4);

This function returns in :code:`*out4` the 32-bit (4-byte) MurmurHash of :code:`len` bytes at :code:`*key`.

Please see the `Simple API introduction <./cprogram.html#simple-api>`_ for details about parameter allocation.

---------------------------
ydb_mmrhash_128()
---------------------------

.. code-block:: C

    void ydb_mmrhash_128(const void *key, int len, uint4 seed, ydb_uint16 *out);

This function returns  in :code:`*out` the 128-bit (16-byte) MurmurHash of :code:`len` bytes at :code:`*key`.

Please see the `Simple API introduction <./cprogram.html#simple-api>`_ for details about parameter allocation.

.. _ydb_mmrhash_128_ingest():
.. _ydb_mmrhash_128_result():

----------------------------------------------------
ydb_mmrhash_128_ingest() / ydb_mmrhash_128_result()
----------------------------------------------------

.. code-block:: C

    void ydb_mmrhash_128_ingest(hash128_state_t *state, const void *key, int len);

    void ydb_mmrhash_128_result(hash128_state_t *state, uint4 addl_seed, ydb_uint16 *out);

These functions enable users to get a MurmurHash through a series of incremental operations.

The sequence is to first initialize the "state" variable using the `HASH128_STATE_INIT()`_ macro, then call :code:`ydb_mmrhash_128_ingest()` one or more times and finally call :code:`ydb_mmrhash_128_result()` to
obtain the final hash value. "key" points to the input character array (of length "len") for the hash. "addl_seed" can either be the last four bytes of the input, or at the application's discretion, an additional seed or salt.
An example is to set it to the sum of the "len" values passed in across all calls to :code:`ydb_mmrhash_128_ingest` before :code:`ydb_mmrhash_128_result` is called. "out" points to the structure holding the 16-byte hash result.

Example:

.. code-block:: C

   // Initialize state struct
   HASH128_STATE_INIT(hash_state, 0);

   // Create keys/strings to ingest
   char *key1 = "ifembu8r308j243h5g3h84t7yf23h0h";
   char *key2 = "ougoh2408rh2fhe08yh2ti8rhhrguo2r3huocdiWEN23";

   // Add keys to hash
   ydb_mmrhash_128_ingest(&hash_state, (void*)key1, strlen(key1));
   ydb_mmrhash_128_ingest(&hash_state, (void*)key2, strlen(key2));

   // Produce result
   ydb_mmrhash_128_result(hash_state, 0, &hash);

Please see the `Simple API introduction <./cprogram.html#simple-api>`_ for details about parameter allocation.

--------------------------------
ydb_mmrhash_128_hex()
--------------------------------

.. code-block:: C

    void ydb_mmrhash_128_hex(const ydb_uint16 *hash, unsigned char *out);

This function returns a hex formatted representation of a 16-byte hash value. As the function does no checking, if :code:`*out` is not at least 32 bytes, a buffer overflow can occur, potentially with unpleasant consequences such as abnormal process termination with a SIG-11, or worse.

Example:

.. parsed-literal::
   char out[16];
   ydb_mmrhash_128_hex(&hash, out);

Please see the `Simple API introduction <./cprogram.html#simple-api>`_ for details about parameter allocation.

------------------------------------
ydb_mmrhash_128_bytes()
------------------------------------

.. code-block:: C

    void ydb_mmrhash_128_bytes(const ydb_uint16 *hash, unsigned char *out);

This function converts the 16-byte hash stored in a "ydb_uint16" structure (2 8-byte integers) into a byte array "out" of 16 characters.
It is also internally used by `ydb_mmrhash_128_hex()`_.

Example:

.. parsed-literal::
   char out[16];
   ydb_mmrhash_128_bytes(&hash, out);

.. _ydb_stdout_stderr_adjust():
.. _ydb_stdout_stderr_adjust_t():

Please see the `Simple API introduction <./cprogram.html#simple-api>`_ for details about parameter allocation.

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

Please see the `Simple API introduction <./cprogram.html#simple-api>`_ for details about parameter allocation.

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
specific application code base (see discussion under `Threads <./programmingnotes.html#threads>`_).

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

Please see the `Simple API introduction <./cprogram.html#simple-api>`_ for details about parameter allocation.

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
                unsigned int handler_data_len,
                char *handler_data);

        int ydb_timer_start_t(uint64_t tptoken,
                ydb_buffer_t *errstr,
                intptr_t timer_id,
                unsigned long long limit_nsec,
                ydb_funcptr_retvoid_t handler,
                unsigned int handler_data_len,
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

Please see the `Simple API introduction <./cprogram.html#simple-api>`_ for details about parameter allocation.

Calling M Routines
===================

M routines can be called from C with the following functions which are described in the `M Programmers Guide <https://docs.yottadb.com/ProgrammersGuide/extrout.html#calls-from-external-routines-call-ins>`_:

* `ydb_ci() <https://docs.yottadb.com/ProgrammersGuide/extrout.html#ydb-ci>`_
* `ydb_ci_t() <https://docs.yottadb.com/ProgrammersGuide/extrout.html#ydb-ci-t>`_
* `ydb_cip() <https://docs.yottadb.com/ProgrammersGuide/extrout.html#ydb-cip>`_
* `ydb_cip_t() <https://docs.yottadb.com/ProgrammersGuide/extrout.html#ydb-cip-t>`_
* `ydb_zstatus() <https://docs.yottadb.com/ProgrammersGuide/extrout.html#id2>`_

Historically, the predecessors of the functions to call M routines
returned positive return codes. In order to maintain backward
compatibility, values returned by the above are positive values, whereas
YottaDB `error return codes`_ are negative. For example, to return an
invalid string length (`YDB_ERR_INVSTRLEN`_), the :code:`ydb_ci*()` functions
return :code:`-YDB_ERR_INVSTRLEN`, which is a positve value because
:code:`YDB_ERR_STRLEN` is a negative value.

Effective release
`r1.30. <https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.30>`_
:code:`ydb_zstatus()` returns an :code:`int`.
