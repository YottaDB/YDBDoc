.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2019-2023 YottaDB LLC and/or its subsidiaries.#
.. # All rights reserved.                                        #
.. #                                                             #
.. #     This document contains the intellectual property        #
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

--------------
Sample Program
--------------
In order to help understand a lot of the description below, you can download the `wordfreq.c <https://gitlab.com/YottaDB/DB/YDBTest/blob/master/simpleapi/inref/wordfreq.c>`_ sample program, with a `reference input file <https://gitlab.com/YottaDB/DB/YDBTest/blob/master/simpleapi/outref/wordfreq_input.txt>`_ and `corresponding reference output file <https://gitlab.com/YottaDB/DB/YDBTest/blob/master/simpleapi/outref/wordfreq_output.txt>`_ and compile it. Here are some copy pastable instructions. The diff exit code should be zero, as the files should be identical. For the following example, you will need to use the bash shell, have git, diff and a C compiler (gcc here) installed.

.. code-block:: bash

        mkdir test
        cd test
        git clone https://gitlab.com/YottaDB/DB/YDBTest
        source /usr/local/etc/ydb_env_set
        gcc `pkg-config --cflags yottadb` wordfreq.c `pkg-config --libs yottadb` -o wordfreq
        ./wordfreq < YDBTest/simpleapi/outref/wordfreq_input.txt > wordfreq.out
        diff wordfreq.out YDBTest/simpleapi/outref/wordfreq_output.txt
        echo $?

.. _c-sym-const:

------------------
Symbolic Constants
------------------

The :code:`libyottadb.h` file defines several symbolic constants, which are one of the following types:

- Function Return Codes, which in turn are one of:

  - Normal Return Codes
  - Error Return Codes

- Limits
- Other

Symbolic constants all fit within the range of a C :code:`int`.

+++++++++++++++++++++
Function Return Codes
+++++++++++++++++++++

Return codes from calls to YottaDB are usually of type :code:`int` and occasionally other types. Normal return codes are non-negative (greater than or equal to zero); error return codes are negative.

~~~~~~~~~~~~~~~~~~~
Normal Return Codes
~~~~~~~~~~~~~~~~~~~

Symbolic constants for normal return codes have :code:`YDB_` prefixes other than :code:`YDB_ERR_`.

^^^^^^^^^^^^^^^^
YDB_LOCK_TIMEOUT
^^^^^^^^^^^^^^^^

This return code from lock acquisition functions indicates that the specified timeout was reached without the requested locks being acquired.

^^^^^^
YDB_OK
^^^^^^

This the standard return code of all functions following successful execution.

^^^^^^^^^^^^^^
YDB_TP_RESTART
^^^^^^^^^^^^^^

Return code to YottaDB from an application function that implements a transaction to indicate that it wishes YottaDB to restart the transaction, or by a YottaDB function invoked within a transaction to its caller that the database engine has detected that it will be unable to commit the transaction and will need to restart. Application code designed to be executed within a transaction should be written to recognize this return code and in turn perform any cleanup required and return to the YottaDB :ref:`ydb-tp-s-st-fn` invocation from which it was called. See :ref:`txn-proc` for a discussion of restarts.

^^^^^^^^^^^^^^^
YDB_TP_ROLLBACK
^^^^^^^^^^^^^^^

Return code to YottaDB from an application function that implements a transaction, and in turn returned to the caller indicating that the transaction was not committed.

.. _err-ret-codes:

~~~~~~~~~~~~~~~~~~
Error Return Codes
~~~~~~~~~~~~~~~~~~

Symbolic constants for error codes returned by calls to YottaDB are prefixed with :code:`YDB_ERR_` and are all less than zero. The symbolic constants below are not a complete list of all error messages that YottaDB functions can return — error return codes can indicate system errors and database errors, not just application errors. A process that receives a negative return code, including one not listed here, can call :ref:`ydb-get-s-st-fn` to get the value of :ref:`zstatus-isv`.

Error messages can be raised by the YottaDB runtime system or by the underlying operating system.

- A full set of YottaDB error messages and numbers is in the `YottaDB Messages and Recovery Procedures Manual <../MessageRecovery/index.html>`_.
- Linux error messages are described in Linux documentation, e.g. `errno <https://linux.die.net/man/3/errno>`_.

Remember that the error codes returned by YottaDB functions are the negated numeric values of the error codes above.

^^^^^^^^^^^^^^^^^^^^^^
YDB_ERR_CALLINAFTERXIT
^^^^^^^^^^^^^^^^^^^^^^

A YottaDB function was called after :code:`ydb_exit()` was called.

^^^^^^^^^^^^^^^^^^^
YDB_ERR_FATALERROR1
^^^^^^^^^^^^^^^^^^^

A fatal error occurred. The process is generating a core dump and terminating. As a process cannot receive a fatal error code, this error appears in the syslog.

^^^^^^^^^^^^^^^^^^^
YDB_ERR_FATALERROR2
^^^^^^^^^^^^^^^^^^^

A fatal error occurred. The process is terminating without generating a core dump. As a process cannot receive a fatal error code, this error appears in the syslog.

^^^^^^^^^^^^^^^
YDB_ERR_GVUNDEF
^^^^^^^^^^^^^^^

No value exists at a requested global variable node.

^^^^^^^^^^^^^^^^^^^^
YDB_ERR_INVNAMECOUNT
^^^^^^^^^^^^^^^^^^^^

A :code:`namecount` parameter has an invalid value.

^^^^^^^^^^^^^^^^^^
YDB_ERR_INSUFFSUBS
^^^^^^^^^^^^^^^^^^

A call to :ref:`ydb-node-next-s-st-fn` or :ref:`ydb-node-previous-s-st-fn` did not provide enough parameters for the return values. Note that as the number of parameters is a count, when array subscripts start at 0, an array subscript of *n* corresponds to *n+1* parameters.

.. _YDB-ERR-INVSTRLEN:

^^^^^^^^^^^^^^^^^
YDB_ERR_INVSTRLEN
^^^^^^^^^^^^^^^^^

A buffer provided by the caller is not long enough for a string to be returned, or the length of a string passed as a parameter exceeds :code:`YDB_MAX_STR`. In the event the return code is :code:`YDB_ERR_INVSTRLEN` and if :code:`*xyz` is a :code:`ydb_buffer_t` structure whose :code:`xyz->len_alloc` indicates insufficient space, then :code:`xyz->len_used` is set to the size required of a sufficiently large buffer. In this case the :code:`len_used` field of a :code:`ydb_buffer_t` structure is greater than the :code:`len_alloc` field, and the caller is responsible for correcting the :code:`xyz->len_used` field.

^^^^^^^^^^^^^^
YDB_ERR_INVSVN
^^^^^^^^^^^^^^

A special variable name provided by the caller is invalid.

^^^^^^^^^^^^^^^^^^
YDB_ERR_INVVARNAME
^^^^^^^^^^^^^^^^^^

A variable name provided by the caller is invalid.

^^^^^^^^^^^^^^^
YDB_ERR_KEY2BIG
^^^^^^^^^^^^^^^

The length of a global variable name and subscripts exceeds the limit configured for the database region to which it is mapped.

^^^^^^^^^^^^^^^
YDB_ERR_LVUNDEF
^^^^^^^^^^^^^^^

No value exists at a requested local variable node.

^^^^^^^^^^^^^^^^^^^^^^^
YDB_ERR_MAXNRSUBSCRIPTS
^^^^^^^^^^^^^^^^^^^^^^^

The number of subscripts specified in the call exceeds :code:`YDB_MAX_SUBS`.

^^^^^^^^^^^^^^^^^^^^^^^
YDB_ERR_MINNRSUBSCRIPTS
^^^^^^^^^^^^^^^^^^^^^^^
The number of subscripts cannot be negative.

^^^^^^^^^^^^^^^^^^^^
YDB_ERR_NAMECOUNT2HI
^^^^^^^^^^^^^^^^^^^^
The number of variable names specified to :ref:`ydb-delete-excl-s-st-fn` or :ref:`ydb-tp-s-st-fn` exceeded the :code:`YDB_MAX_NAMES`.

^^^^^^^^^^^^^^^
YDB_ERR_NODEEND
^^^^^^^^^^^^^^^
In the event a call to :ref:`ydb-node-next-s-st-fn`, :ref:`ydb-node-previous-s-st-fn`, :ref:`ydb-subscript-next-s-st-fn`, or :ref:`ydb-subscript-previous-s-st-fn` wish to report that there no further nodes/subscripts in their traversals, they return this value.

^^^^^^^^^
YDB_NOTOK
^^^^^^^^^

:ref:`ydb-file-name-to-id-idt-fn` was called with a NULL pointer to a filename.

^^^^^^^^^^^^^^^^
YDB_ERR_NUMOFLOW
^^^^^^^^^^^^^^^^

A :ref:`ydb-incr-s-st-fn` operation resulted in a numeric overflow.

^^^^^^^^^^^^^^^^^^^^
YDB_ERR_PARAMINVALID
^^^^^^^^^^^^^^^^^^^^

A parameter provided by the caller is invalid.

^^^^^^^^^^^^^^^^^^^^^
YDB_ERR_SIMPLEAPINEST
^^^^^^^^^^^^^^^^^^^^^

An attempt was made to nest Simple API calls, which cannot be nested.

^^^^^^^^^^^^^^^^^^^^^
YDB_ERR_SUBSARRAYNULL
^^^^^^^^^^^^^^^^^^^^^

The :code:`subs_used` parameter of a function is greater than zero, but the :code:`subsarray` parameter is a NULL pointer.

^^^^^^^^^^^^^^^
YDB_ERR_SVNOSET
^^^^^^^^^^^^^^^

The application inappropriately attempted to modify the value of an intrinsic special variable such as an attempt to modify :code:`$trestart` using :ref:`ydb-set-s-st-fn`.

^^^^^^^^^^^^^^^^^
YDB_ERR_TIME2LONG
^^^^^^^^^^^^^^^^^

This return code indicates that a value greater than :code:`YDB_MAX_TIME_NSEC` was specified for a time duration.

^^^^^^^^^^^^^^^^^
YDB_ERR_TPTIMEOUT
^^^^^^^^^^^^^^^^^

This return code from :ref:`ydb-tp-s-st-fn` indicates that the transaction took too long to commit.

^^^^^^^^^^^^^^^^
YDB_ERR_UNIMPLOP
^^^^^^^^^^^^^^^^

An operation that is not supported for an intrinsic special variable – of the :ref:`c-simple-api` functions only :ref:`ydb-get-s-st-fn` and :ref:`ydb-set-s-st-fn` are supported – was attempted on an intrinsic special variable.

^^^^^^^^^^^^^^^^^^^^
YDB_ERR_VARNAME2LONG
^^^^^^^^^^^^^^^^^^^^

A variable name length exceeds YottaDB's limit.

++++++
Limits
++++++

Symbolic constants for limits are prefixed with :code:`YDB_MAX_` or :code:`YDB_MIN_`.

~~~~~~~~~~~~~
YDB_MAX_IDENT
~~~~~~~~~~~~~

The maximum space in bytes required to store a complete variable name, not including the preceding caret for a global variable. Therefore, when allocating space for a string to hold a global variable name, add 1 for the caret.

~~~~~~~~~~~~~~~~~~
YDB_MAX_M_LINE_LEN
~~~~~~~~~~~~~~~~~~

The maximum M source code line length, in bytes.

~~~~~~~~~~~~~
YDB_MAX_NAMES
~~~~~~~~~~~~~

The maximum number of variable names that can be passed to :ref:`ydb-delete-excl-s-st-fn` or :ref:`ydb-tp-s-st-fn`.

~~~~~~~~~~~
YDB_MAX_STR
~~~~~~~~~~~

The maximum length of a string (or blob) in bytes. A caller to :ref:`ydb-get-s-st-fn` whose :code:`*ret_value` parameter provides a buffer of :code:`YDB_MAX_STR` will never get a :code:`YDB_ERR_INVSTRLEN` error.

~~~~~~~~~~~~
YDB_MAX_SUBS
~~~~~~~~~~~~

The maximum number of subscripts for a local or global variable.

~~~~~~~~~~~~~~~~~
YDB_MAX_TIME_NSEC
~~~~~~~~~~~~~~~~~

The maximum value in nanoseconds that an application can instruct libyottab to wait, e.g., until the process is able to acquire locks it needs before timing out, or for :ref:`ydb-hiber-start-fn`. Note that even if timer resolution is in nanoseconds, the accuracy is always determined by the underlying hardware and operating system, as well as factors such as system load.

~~~~~~~~~~~~~~
YDB_MAX_YDBERR
~~~~~~~~~~~~~~

The absolute (positive) value of any YottaDB function error return code. If the absolute value of an error return code is greater than :code:`YDB_MAX_YDBERR`, then it is an error code from elsewhere, e.g. `errno <https://linux.die.net/man/3/errno>`_. Also, see :code:`YDB_IS_YDBERR()`.

~~~~~~~~~~~~~~
YDB_MIN_YDBERR
~~~~~~~~~~~~~~

The absolute (positive) value of any YottaDB function error return code. If the absolute value of an error return code is less than :code:`YDB_MIN_YDBERR`, then it is an error code from elsewhere, e.g. `errno <https://linux.die.net/man/3/errno>`_. Also, see :code:`YDB_IS_YDBERR()`.

++++++++
Severity
++++++++

Symbolic constants for the severities of message numbers in return codes and :code:`$zstatus` are prefixed with :code:`YDB_SEVERITY_`.

~~~~~~~~~~~~~~~~~~
YDB_SEVERITY_ERROR
~~~~~~~~~~~~~~~~~~

The number corresponds to an error from which the process can recover.

~~~~~~~~~~~~~~~~~~
YDB_SEVERITY_FATAL
~~~~~~~~~~~~~~~~~~

The number corresponds to an error that terminated the process.

~~~~~~~~~~~~~~~~~~~~~~~~~~
YDB_SEVERITY_INFORMATIONAL
~~~~~~~~~~~~~~~~~~~~~~~~~~

The number corresponds to an informational message.

~~~~~~~~~~~~~~~~~~~~
YDB_SEVERITY_SUCCESS
~~~~~~~~~~~~~~~~~~~~

The number corresponds to the successful completion of a requested operation.

~~~~~~~~~~~~~~~~~~~~
YDB_SEVERITY_WARNING
~~~~~~~~~~~~~~~~~~~~

The number corresponds to a warning, i.e., it indicates a possible problem.

+++++
Other
+++++

Other symbolic constants have a prefix of :code:`YDB_`.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
YDB_DEL_NODE and YDB_DEL_TREE
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As values of the :code:`deltype` parameter, these values indicate to :ref:`ydb-delete-s-st-fn` whether to delete an entire subtree or just the node at the root, leaving the subtree intact.

~~~~~~~~~
YDB_NOTTP
~~~~~~~~~

As a value of the :code:`tptoken` parameter of the :ref:`c-simple-api` multi-threaded functions – those ending in :code:`_st()`, indicates that the caller is not within a :ref:`transaction <txn-proc>`.

.. _c-data-struct:

----------------------------------
Data Structures & Type Definitions
----------------------------------

++++++++++++
ydb_buffer_t
++++++++++++

:code:`ydb_buffer_t` is a descriptor for a string [#]_ value, and consists of the following fields:

- :code:`buf_addr` — pointer to an :code:`unsigned char`, the starting address of a string.
- :code:`len_alloc` and :code:`len_used` — fields of type :code:`unsigned int` where:

  - :code:`len_alloc` is the number of bytes allocated to store the string,
  - :code:`len_used` is the length in bytes of the currently stored string, and
  - :code:`len_alloc` ≥ :code:`len_used` except when a :ref:`YDB-ERR-INVSTRLEN` occurs.

.. [#] Strings in YottaDB are arbitrary sequences of bytes that are not
       null-terminated. Other languages may refer to them as binary
       data or blobs.

++++++++++++
ydb_string_t
++++++++++++

:code:`ydb_string_t` is a descriptor for a string provided for compatibility with existing code, and consists of the following fields:

- :code:`address` — pointer to an :code:`unsigned char`, the starting address of a string.
- :code:`length` — the length of the string starting at the :code:`address` field.

+++++++++++++
ydb_tpfnptr_t
+++++++++++++

:code:`ydb_tpfnptr_t` is a pointer to a function which returns an integer, with one parameter, a pointer to an arbitrary structure:

.. code-block:: C

        typedef int (*ydb_tpfnptr_t)(void *tpfnparm);

++++++++++++++
ydb_tp2fnptr_t
++++++++++++++

:code:`ydb_tp2fnptr_t` is a pointer to a function which returns an integer, with three parameters, a :code:`tptoken`, a :code:`*errstr` pointer, and a pointer to an arbitrary structure:

.. code-block:: C

        typedef int (*ydb_tp2fnptr_t)(uint64_t tptoken, ydb_buffer_t *errstr, void *tpfnparm)

Functions to implement transaction processing logic for single-threaded applications are referenced by :code:`ydb_tpfnptr_t` and functions to implement transaction processing logic for multi-threaded applications are referenced by :code:`ydb_tp2fnptr_t`.

------
Macros
------

+++++++++++++
YDB_ASSERT(x)
+++++++++++++

Conditionally include this macro in code for debugging and testing purposes. If :code:`x` is non-zero, it prints an error message on :code:`stderr` and generates a core file by calling :ref:`ydb-fork-n-core-fn`.

++++++++++++++++++++++++++++++++++++
YDB_BUFFER_IS_SAME(buffer1, buffer2)
++++++++++++++++++++++++++++++++++++

Use this macro to test whether the memory locations (strings) pointed to by two :code:`ydb_buffer_t` structures have the same content, returning :code:`FALSE` (0) if they differ and a non-zero value if the contents are identical.

++++++++++++++++++++++++++++++++++++++++++++++++++++
YDB_COPY_BUFFER_TO_BUFFER(source, destination, done)
++++++++++++++++++++++++++++++++++++++++++++++++++++

Use this macro to copy the memory locations (strings) pointed to by :code:`source` to the memory locations pointed to by :code:`destination` and set:

- :code:`destination->len_used` to :code:`source->len_used`; and
- :code:`done` to :code:`TRUE` if :code:`destination->len_alloc` ≥ :code:`source->len_used` and the underlying :code:`memcpy()` completed successfully, and :code:`FALSE` otherwise.

+++++++++++++++++++++++++++++++++++++++++++++++++
YDB_COPY_LITERAL_TO_BUFFER(literal, buffer, done)
+++++++++++++++++++++++++++++++++++++++++++++++++

Use this macro to copy a literal string to previously allocated memory referenced by a :code:`ydb_buffer_t` structure (for example, to set an initial subscript to sequence through nodes). It sets:

- :code:`buffer->len_used` to the size of the literal; and
- :code:`done` to :code:`TRUE` if :code:`buffer->len_alloc` ≥ the size of the literal excluding its terminating null byte and the underlying :code:`memcpy()` completed successfully, and :code:`FALSE` otherwise.

+++++++++++++++++++++++++++++++++++++++++++++++
YDB_COPY_STRING_TO_BUFFER(string, buffer, done)
+++++++++++++++++++++++++++++++++++++++++++++++

Use this macro to copy a null-terminated string to previously allocated memory referenced by a :code:`ydb_buffer_t` structure. This macro requires the code to also :code:`#include <string.h>`. It sets:

- :code:`buffer->len_used` to the size of the copied string; and
- :code:`done` to :code:`TRUE` if :code:`buffer->len_alloc` ≥ the size of the string to be copied and the underlying :code:`memcpy()` completed successfully, and :code:`FALSE` otherwise.

++++++++++++++++++++++++
YDB_FREE_BUFFER(BUFFERP)
++++++++++++++++++++++++

Use this macro to free the buffer malloced using :code:`YDB_MALLOC_BUFFER`.

- `free()` call is used on :code:`BUFFERP->buf_addr`.

++++++++++++++++++++++++++++++++++++++
YDB_LITERAL_TO_BUFFER(literal, buffer)
++++++++++++++++++++++++++++++++++++++

Use this macro to set a :code:`ydb_buffer_t` structure to refer to a literal (such as a variable name). With a string literal, and a pointer to a :code:`ydb_buffer_t` structure, set:

- :code:`buffer->buf_addr` to the address of :code:`literal`; and
- :code:`buffer->len_used` and :code:`buffer->len_alloc` to the length of :code:`literal` excluding the terminating null byte.

+++++++++++++++++++++
YDB_IS_YDBERR(msgnum)
+++++++++++++++++++++

Returns TRUE if the absolute value of :code:`msgnum` lies between :code:`YDB_MIN_YDBERR` and :code:`YDB_MAX_YDBERR`.

++++++++++++++++++++++++++++++
YDB_MALLOC_BUFFER(BUFFERP,LEN)
++++++++++++++++++++++++++++++

Use this macro to to allocate a buffer using :code:`malloc()` of length LEN and assign it to an already allocated :code:`ydb_buffer_t` structure.

- :code:`BUFFERP->buf_addr` is set to the malloced buffer.
- :code:`BUFFERP->len_alloc` is set to the malloced length.
- :code:`BUFFERP->len_used` is set to 0.

++++++++++++++++++++++++++++++
YDB_SEVERITY(msgnum, severity)
++++++++++++++++++++++++++++++

The :ref:`error return code <err-ret-codes>` from a function indicates both the nature of an error as well as its severity. For message :code:`msgnum`, the variable :code:`severity` is set to one of the :code:`YDB_SEVERITY_*` symbolic constants. :code:`YDB_SEVERITY()` is only meaningful for :ref:`error return codes <err-ret-codes>` and not other numbers. Use  :code:`YDB_IS_YDBERR()` to determine whether a return code is a YottaDB :ref:`error return code <err-ret-codes>`.

.. _HASH128-STATE-INIT-fn:

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
HASH128_STATE_INIT(hash128_state_t \*state, ydb_uint8 addl_seed)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Use this macro to initialize a variable in order to compute a 128-bit MurMurHash using :ref:`ydb-mmrhash-128-ingest-result-fn`.

Example:

.. code-block:: C

   // Initialize state struct
   HASH128_STATE_INIT(hash_state, 0);

++++++++++++++++++++
YDB_STRING_TO_BUFFER
++++++++++++++++++++

Sets a :code:`ydb_buffer_t` structure
to point to an existing null-terminated C string, i.e.,

.. code-block:: C

   #define YDB_STRING_TO_BUFFER(STRING, BUFFERP)                           \
   {                                                                       \
	   (BUFFERP)->buf_addr = STRING;                                   \
	   (BUFFERP)->len_used = (BUFFERP)->len_alloc = strlen(STRING);    \
   }


YottaDB functions are divided into:

- Simple API — a core set of functions that provides easy-to-use access to the major features of YottaDB.
- Comprehensive API — a more elaborate set of functions for specialized or optimized access to additional functionality within :code:`libyottadb.so` that YottaDB itself uses. The Comprehensive API is a project for the future.
- Utility Functions — Functions useful to a C application using YottaDB.

.. _c-simple-api:

----------
Simple API
----------

As all subscripts and node data passed to YottaDB using the Simple API are strings, use the :code:`sprintf()` and :code:`atoi()/strtoul()` family of functions to convert between numeric values and strings which are :ref:`canonical-numbers`.

Note that *all* parameters passed to Simple API functions must be properly allocated and initialized where needed prior to the function call, including return values. This also specifically includes all members of :code:`ydb_buffer_t` structs for parameters containing input values, but only :code:`buf_addr` and :code:`len_alloc` members for return values. To facilitate initialization of the :code:`ydb_buffer_t` members, you may find the :code:`YDB_MALLOC_BUFFER` macro helpful for heap allocations.

To allow the YottaDB Simple API functions to handle a variable tree whose nodes have varying numbers of subscripts, the actual number of subscripts is itself passed as a parameter. In the prototypes of functions, parameters of the form:

- :code:`ydb_buffer_t *varname` refers to the name of a variable;
- :code:`int subs_used` and :code:`int *subs_used` refer to an actual number of subscripts; and
- :code:`ydb_buffer_t *subsarray` refers to an array of :code:`ydb_buffer_t` structures used to pass subscripts whose actual number is defined by :code:`subs_used` or :code:`*subs_used` parameters.

To pass an intrinsic special variable, or unsubscripted local or global variable, :code:`subs_used` should be zero and :code:`*subsarray` should be NULL.

**Caveat:** Specifying a :code:`subs_used` that exceeds the actual number of parameters passed in :code:`*subsarray` will almost certainly result in an unpleasant bug that is difficult to troubleshoot.

Functions specific to the YottaDB Simple API for single-threaded applications end in :code:`_s()` and those for multi-threaded applications end in :code:`_st()`, with the latter functions typically differing from their counterparts of the former type with two additional parameters, :code:`tptoken`, and :code:`errstr`. The discussion in :ref:`threads` provides more detailed information.

.. _ydb-data-s-st-fn:

++++++++++++++++++++++++++++
ydb_data_s() / ydb_data_st()
++++++++++++++++++++++++++++

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

In the location pointed to by :code:`ret_value`, :code:`ydb_data_s()` and :code:`ydb_data_st()` return the following information about the local or global variable node identified by :code:`*varname`, :code:`subs_used` and :code:`*subsarray`.

- 0 — There is neither a value nor a subtree, i.e., it is undefined.
- 1 — There is a value, but no subtree
- 10 — There is no value, but there is a subtree.
- 11 — There are both a value and a subtree.

It is an error to call :code:`ydb_data_s()` or :code:`ydb_data_st()` on an intrinsic special variable; doing so results in the :code:`YDB_ERR_UNIMPLOP` error. :code:`ydb_data_s() / ydb_data_st()` returns:

- :code:`YDB_OK`; or
- an :ref:`error return code <err-ret-codes>`.

The error :code:`YDB_ERR_PARAMINVALID` is returned when

- :code:`ret_value` is NULL
- :code:`len_alloc` < :code:`len_used` or the :code:`len_used` is non-zero and :code:`buf_addr` is NULL in at least one subscript, in :code:`subsarray`.

Please see the :ref:`Simple API introduction <c-simple-api>` for details about parameter allocation.

.. _ydb-delete-s-st-fn:

++++++++++++++++++++++++++++++++
ydb_delete_s() / ydb_delete_st()
++++++++++++++++++++++++++++++++

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

Delete nodes in the local or global variable tree or subtree specified. A value of :code:`YDB_DEL_NODE` or :code:`YDB_DEL_TREE` for :code:`deltype` specifies whether to delete just the node at the root, leaving the (sub)tree intact, or to delete the node as well as the (sub)tree.

Intrinsic special variables cannot be deleted.

:code:`ydb_delete_s()` and :code:`ydb_delete_st()` return :code:`YDB_OK`, a :code:`YDB_ERR_UNIMPLOP` if :code:`deltype` is neither :code:`YDB_DEL_NODE` nor :code:`YDB_DEL_TREE`, :code:`YDB_ERR_PARAMINVALID` is returned when :code:`len_alloc` < :code:`len_used` or the :code:`len_used` is non-zero and :code:`buf_addr` is NULL in at least one subscript in :code:`subsarray`, or another :ref:`error return code <err-ret-codes>`.

- :code:`YDB_OK`;
- :code:`YDB_ERR_UNIMPLOP` if :code:`deltype` is neither :code:`YDB_DEL_NODE` nor :code:`YDB_DEL_TREE`; or
- another :ref:`error return code <err-ret-codes>`.

Please see the :ref:`Simple API introduction <c-simple-api>` for details about parameter allocation.

.. _ydb-delete-excl-s-st-fn:

++++++++++++++++++++++++++++++++++++++++++
ydb_delete_excl_s() / ydb_delete_excl_st()
++++++++++++++++++++++++++++++++++++++++++

.. code-block:: C

        int ydb_delete_excl_s(int namecount,
                ydb_buffer_t *varnames);

        int ydb_delete_excl_st(uint64_t tptoken,
                ydb_buffer_t *errstr,
                int namecount, ydb_buffer_t *varnames);

:code:`ydb_delete_excl_s()` and :code:`ydb_delete_excl_st()` delete the trees of all local variables except those in the :code:`*varnames` array. It is an error for :code:`*varnames` to include a global or intrinsic special variable.

In the special case where :code:`namecount` is zero, :code:`ydb_delete_excl_s()` and :code:`ydb_delete_excl_st()` delete all local variables. In this case, the :code:`varnames` parameter is ignored so any value can be passed in for that parameter but we recommend that applications pass a :code:`NULL` value.

If your application mixes M and non M code, and you wish to use :code:`ydb_delete_excl_s()` to delete local variables that are aliases, formal parameters, or actual parameters passed by reference, make sure you understand what (sub)trees are being deleted. This warning does not apply to applications that do not include M code.

:code:`ydb_delete_excl_s()` and :code:`ydb_delete_excl_st()` return :code:`YDB_OK`, :code:`YDB_ERR_NAMECOUNT2HI` if more than :code:`YDB_MAX_NAMES` are specified, or another :ref:`error return code <err-ret-codes>`. :code:`YDB_ERR_PARAMINVALID` is returned when :code:`len_alloc` < :code:`len_used` or the :code:`len_used` is non-zero and :code:`buf_addr` is NULL in at least one variable name in :code:`varnames`.

Note that specifying a larger value for :code:`namecount` than the number of variable names actually provided in :code:`*varnames` can result in a buffer overflow.

Please see the :ref:`Simple API introduction <c-simple-api>` for details about parameter allocation.

.. _ydb-get-s-st-fn:

++++++++++++++++++++++++++
ydb_get_s() / ydb_get_st()
++++++++++++++++++++++++++

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

To the user-allocated location pointed to by :code:`ret_value->buf_addr`, :code:`ydb_get_s()` and :code:`ydb_get_st()` copy the value of the specified node or intrinsic special variable, setting :code:`ret_value->len_used` on both normal and error returns (the latter case as long as the data exists). Return values are:

- :code:`YDB_OK` for a normal return;
- :code:`YDB_ERR_GVUNDEF`, :code:`YDB_ERR_INVSVN`, or :code:`YDB_ERR_LVUNDEF` as appropriate if no such variable or node exists;
- :code:`YDB_ERR_INVSTRLEN` if :code:`ret_value->len_alloc` is insufficient for the value at the node;
- :code:`YDB_ERR_PARAMINVALID` when :code:`ret_value` is NULL or :code:`ret_value->buf_addr` is NULL and the return value has a non-zero :code:`len_used`; or :code:`len_alloc` < :code:`len_used` or the :code:`len_used` is non-zero and :code:`buf_addr` is NULL in at least one subscript in :code:`subsarray`; or
- another applicable :ref:`error return code <err-ret-codes>`.

Notes:

- In the unlikely event an application wishes to know the length of the value at a node, but not access the data, it can call :code:`ydb_get_s()` or :code:`ydb_get_st()` and provide an output buffer (:code:`retvalue->len_alloc`) with a length of zero, since even in the case of a :code:`YDB_ERR_INVSTRLEN` error, :code:`retvalue->len_used` is set.
- Within a transaction implemented by :ref:`ydb-tp-s-st-fn` application code observes stable data at global variable nodes because YottaDB :ref:`txn-proc` ensures ACID properties, restarting the transaction if a value changes.
- Outside a transaction, a global variable node can potentially be changed by another, concurrent, process between the time that a process calls :ref:`ydb-data-s-st-fn` to ascertain the existence of the data and a subsequent call to :ref:`ydb-get-s-st-fn` to get that data. A caller of :ref:`ydb-get-s-st-fn` to access a global variable node should code in anticipation of a potential :code:`YDB_ERR_GVUNDEF`, unless it is known from application design that this cannot happen.

Please see the :ref:`Simple API introduction <c-simple-api>` for details about parameter allocation.

.. _ydb-incr-s-st-fn:

++++++++++++++++++++++++++++
ydb_incr_s() / ydb_incr_st()
++++++++++++++++++++++++++++

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

- convert the value in the specified node to a number if it is not one already, using a zero value if the node does not exist;
- increment it by the value specified by :code:`*increment`, converting the value to a number if it is not a :ref:`canonical number <canonical-numbers>`, defaulting to 1 if the parameter is NULL; and
- store the value as a canonical number in :code:`*ret_value`.

Return values:

- The normal return value is :code:`YDB_OK`.
- If the atomic increment results in a numeric overflow, the function returns a :code:`YDB_ERR_NUMOFLOW` error; in this case, the value in the node is untouched and that in :code:`*ret_value` is unreliable.
- :code:`YDB_ERR_INVSTRLEN` if :code:`ret_value->len_alloc` is insufficient for the result. As with :ref:`ydb-get-s-st-fn`, in this case :code:`ret_value->len_used` is set to the required length.
- Other errors return the corresponding :ref:`error return code <err-ret-codes>`.

Notes:

- Intrinsic special variables cannot be atomically incremented, and an attempt to do so returns the :code:`YDB_ERR_UNIMPLOP` error.
- The value of the empty string coerced to a numeric value is 0.

Please see the :ref:`Simple API introduction <c-simple-api>` for details about parameter allocation.

.. _ydb-lock-s-st-fn:

++++++++++++++++++++++++++++
ydb_lock_s() / ydb_lock_st()
++++++++++++++++++++++++++++

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

Release any locks held by the process, and attempt to acquire all the requested locks. Except in the case of an error, the release is unconditional. On return, the function will have acquired all requested locks or none of them. If no locks are requested (:code:`namecount` is zero), the function releases all locks and returns :code:`YDB_OK`.

:code:`timeout_nsec` specifies a time in nanoseconds that the function waits to acquire the requested locks. If :code:`timeout_nsec` is zero, the function makes exactly one attempt to acquire the locks

Return values:

- If all requested locks are successfully acquired, the function returns :code:`YDB_OK`.
- If it is not able to acquire all requested locks in the specified time, it acquires no locks, returning with a :code:`YDB_LOCK_TIMEOUT` return value.
- If the requested :code:`timeout_nsec` exceeds :code:`YDB_MAX_TIME_NSEC`, the function immediately returns :code:`YDB_ERR_TIME2LONG`.
- :code:`YDB_ERR_PARAMINVALID` is returned when :code:`len_alloc` < :code:`len_used` or the :code:`len_used` is non-zero and :code:`buf_addr` is NULL in at least one subscript in :code:`subsarray`.
- In other cases, the function returns an :ref:`error return code <err-ret-codes>`.

Please see the :ref:`Simple API introduction <c-simple-api>` for details about parameter allocation.

.. _ydb-lock-decr-s-st-fn:

++++++++++++++++++++++++++++++++++++++
ydb_lock_decr_s() / ydb_lock_decr_st()
++++++++++++++++++++++++++++++++++++++

.. code-block:: C

        int ydb_lock_decr_s(ydb_buffer_t *varname,
                int subs_used,
                ydb_buffer_t *subsarray);

        int ydb_lock_decr_st(uint64_t tptoken,
                ydb_buffer_t *errstr,
                ydb_buffer_t *varname,
                int subs_used,
                ydb_buffer_t *subsarray);

Decrements the count of the specified lock held by the process. As noted in the :ref:`mlpg-concepts` section, a lock whose count goes from 1 to 0 is released. A lock whose name is specified, but which the process does not hold, is ignored.

As releasing a lock cannot fail, the function returns :code:`YDB_OK`, unless there is an error such as an invalid name that results in the return of an error code such as :code:`YDB_ERR_INVVARNAME`. Errors result in an appropriate :ref:`error return code <err-ret-codes>`. :code:`YDB_ERR_PARAMINVALID` is returned when :code:`len_alloc` < :code:`len_used` or the :code:`len_used` is non-zero and :code:`buf_addr` is NULL in at least one subscript in :code:`subsarray`.

Please see the :ref:`Simple API introduction <c-simple-api>` for details about parameter allocation.

.. _ydb-lock-incr-s-st-fn:

++++++++++++++++++++++++++++++++++++++
ydb_lock_incr_s() / ydb_lock_incr_st()
++++++++++++++++++++++++++++++++++++++

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

Without releasing any locks held by the process attempt to acquire the requested lock, incrementing it if already held.

:code:`timeout_nsec` specifies a time in nanoseconds that the function waits to acquire the requested locks. If :code:`timeout_nsec` is zero, the function makes exactly one attempt to acquire the locks

Return values:

- If all requested locks are successfully acquired, the function returns :code:`YDB_OK`.
- If it is not able to acquire all requested locks in the specified time, it acquires no locks, returning with a :code:`YDB_LOCK_TIMEOUT` return value.
- If the requested :code:`timeout_nsec` exceeds :code:`YDB_MAX_TIME_NSEC`, the function immediately returns :code:`YDB_ERR_TIME2LONG`.
- :code:`YDB_ERR_PARAMINVALID` is returned when :code:`len_alloc` < :code:`len_used` or the :code:`len_used` is non-zero and :code:`buf_addr` is NULL in at least one subscript in :code:`subsarray`.
- In other cases, the function returns an :ref:`error return code <err-ret-codes>`.

Please see the :ref:`Simple API introduction <c-simple-api>` for details about parameter allocation.

.. _ydb-node-next-s-st-fn:

++++++++++++++++++++++++++++++++++++++
ydb_node_next_s() / ydb_node_next_st()
++++++++++++++++++++++++++++++++++++++

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

:code:`ydb_node_next_s()` and :code:`ydb_node_next_st()` facilitate traversal of a local or global variable tree. As the number of subscripts can differ between the input node of the call and the output node reported by the call :code:`*ret_subs_used` is an input as well as an output parameter:

- On input, :code:`*ret_subs_used` specifies the number of elements allocated for returning the subscripts of the next node.
- On normal output (:code:`YDB_OK` return code), :code:`*ret_subs_used` contains the actual number of subscripts returned. See below for error return codes

Return values of :code:`ydb_node_next_s()` and :code:`ydb_node_next_st()` are:

- :code:`YDB_OK` with the next node, if there is one, changing :code:`*ret_subs_used` and :code:`*ret_subsarray` parameters to those of the next node. If there is no next node (i.e., the input node is the last), :code:`*ret_subs_used` on output is :code:`YDB_NODE_END`.
- :code:`YDB_ERR_INSUFFSUBS` if :code:`*ret_subs_used` specifies insufficient parameters to return the subscript. In this case :code:`*ret_subs_used` reports the actual number of subscripts required.
- :code:`YDB_ERR_INVSTRLEN` if one of the :code:`ydb_buffer_t` structures pointed to by :code:`*ret_subsarray` does not have enough space for the subscript. In this case, :code:`*ret_subs_used` is the index into the :code:`*ret_subsarray` array with the error, and the :code:`len_used` field of that structure specifies the size required.
- :code:`YDB_ERR_NODEEND` to indicate that that there are no more nodes. In this case, :code:`*ret_subs_used` is unchanged.
- :code:`YDB_ERR_PARAMINVALID` if :code:`ret_subs_used` is NULL or :code:`ret_subsarray` is NULL or one of the :code:`ydb_buffer_t` structures pointed to by :code:`*ret_subsarray` has a NULL buf_addr. In the last case, :code:`*ret_subs_used` is the index into the :code:`*ret_subsarray` array with the NULL buf_addr.
- Another :ref:`error return code <err-ret-codes>`, in which case the application should consider the values of :code:`*ret_subs_used` and the :code:`*ret_subsarray` to be undefined.

Please see the :ref:`Simple API introduction <c-simple-api>` for details about parameter allocation.

.. _ydb-node-previous-s-st-fn:

++++++++++++++++++++++++++++++++++++++++++++++
ydb_node_previous_s() / ydb_node_previous_st()
++++++++++++++++++++++++++++++++++++++++++++++

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

Analogous to :ref:`ydb-node-next-s-st-fn`, :code:`ydb_node_previous_s()` and :code:`ydb_node_previous_st()` facilitate reverse traversal of a local or global variable tree, except that :code:`ydb_node_previous_s()` and :code:`ydb_node_previous_st()` search for and report the predecessor node. Unlike :ref:`ydb-node-next-s-st-fn`, :code:`*ret_subs_used` can be zero if the previous node is the unsubscripted root.

Return values of :code:`ydb_node_previous_s()` and :code:`ydb_node_previous_st()` are:

- :code:`YDB_OK` with the previous node, if there is one, changing :code:`*ret_subs_used` and :code:`*ret_subsarray` parameters to those of the previous node.
- :code:`YDB_ERR_INSUFFSUBS` if :code:`*ret_subs_used` specifies insufficient parameters to return the subscript. In this case :code:`*ret_subs_used` reports the actual number of subscripts required.
- :code:`YDB_ERR_INVSTRLEN` if one of the :code:`ydb_buffer_t` structures pointed to by :code:`*ret_subsarray` does not have enough space for the subscript. In this case, :code:`*ret_subs_used` is the index into the :code:`*ret_subsarray` array with the error, and the :code:`len_used` field of that structure specifies the size required.
- :code:`YDB_ERR_NODEEND` to indicate that that there are no more nodes. In this case, :code:`*ret_subs_used` is unchanged.
- :code:`YDB_ERR_PARAMINVALID` if :code:`ret_subs_used` is NULL or :code:`ret_subsarray` is NULL or one of the :code:`ydb_buffer_t` structures pointed to by :code:`*ret_subsarray` has a NULL buf_addr. In the last case, :code:`*ret_subs_used` is the index into the :code:`*ret_subsarray` array with the NULL buf_addr.
- Another :ref:`error return code <err-ret-codes>`, in which case the application should consider the values of :code:`*ret_subs_used` and the :code:`*ret_subsarray` to be undefined.

Please see the :ref:`Simple API introduction <c-simple-api>` for details about parameter allocation.

.. _ydb-set-s-st-fn:

++++++++++++++++++++++++++
ydb_set_s() / ydb_set_st()
++++++++++++++++++++++++++

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

:code:`ydb_set_s()` and :code:`ydb_set_st()` copy the :code:`value->len_used` bytes at :code:`value->buf_addr` as the value of the specified node or intrinsic special variable specified. A NULL :code:`value` parameter is treated as equivalent to one that points to a :code:`ydb_buffer_t` specifying an empty string. Return values are:

- :code:`YDB_OK` for a normal return;
- :code:`YDB_ERR_INVSVN` if no such intrinsic special variable exists;
- :code:`YDB_ERR_PARAMINVALID` when :code:`len_alloc` < :code:`len_used` or the :code:`len_used` is non-zero and :code:`buf_addr` is NULL in at least one subscript in :code:`subsarray` or :code:`increment`; or
- another applicable :ref:`error return code <err-ret-codes>`.

Please see the :ref:`Simple API introduction <c-simple-api>` for details about parameter allocation.

.. _ydb-str2zwr-s-st-fn:

++++++++++++++++++++++++++++++++++
ydb_str2zwr_s() / ydb_str2zwr_st()
++++++++++++++++++++++++++++++++++

.. code-block:: C

        int ydb_str2zwr_s(ydb_buffer_t *str, ydb_buffer_t *zwr);

        int ydb_str2zwr_st(uint64_t tptoken,
                ydb_buffer_t *errstr,
                ydb_buffer_t *str, ydb_buffer_t *zwr);

In the buffer referenced by :code:`*zwr`, :code:`ydb_str2zwr_s()` and :code:`ydb_str2zwr_st()` provide the :ref:`zwrite formatted <zwrite-format>` version of the string pointed to by :code:`*str`, returning:

- :code:`YDB_OK`;
- :code:`YDB_ERR_INVSTRLEN` if the :code:`*zwr` buffer is not long enough;
- :code:`YDB_ERR_PARAMINVALID` if :code:`zwr` is NULL or :code:`zwr->buf_addr` is NULL and the return value has a non-zero :code:`len_used`; or
- another applicable :ref:`error return code <err-ret-codes>`.

Please see the :ref:`Simple API introduction <c-simple-api>` for details about parameter allocation.

.. _ydb-subscript-next-s-st-fn:

++++++++++++++++++++++++++++++++++++++++++++++++
ydb_subscript_next_s() / ydb_subscript_next_st()
++++++++++++++++++++++++++++++++++++++++++++++++

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

:code:`ydb_subscript_next_s()` and :code:`ydb_subscript_next_st()` provide a primitive for implementing traversal of a tree by searching for the next subscript at the level specified by :code:`subs_used`, i.e., the next subscript after the one referred to by :code:`subsarray[subs_used-1].buf_addr`. A node need not exist at the subscripted variable name provided as input to the function. If :code:`subsarray[subs_used-1].len_used` is zero, :code:`ret_value->buf_addr` points to first node at that level with a subscript that is not the empty string. :code:`ydb_subscript_next_s()` and :code:`ydb_subscript_next_st()` return:

- :code:`YDB_OK`, in which case :code:`ret_value->buf_addr` points to the value of that next subscript;
- :code:`YDB_ERR_NODEEND` when there are no more subscripts at that level, in which case :code:`*ret_value` is unchanged;
- :code:`YDB_ERR_PARAMINVALID` when

  - :code:`ret_value` is NULL;
  - :code:`ret_value->buf_addr` is NULL and the return value has a non-zero :code:`len_used`; or
  - :code:`len_alloc` < :code:`len_used` or the :code:`len_used` is non-zero and :code:`buf_addr` is NULL in at least one subscript in :code:`subsarray`

- or another :ref:`error return code <err-ret-codes>`.

In the special case where :code:`subs_used` is zero, and the function returns :code:`YDB_OK`, :code:`ret_value->buf_addr` points to the next local or global variable name, with :code:`YDB_ERR_NODEEND` indicating an end to the traversal.

Please see the :ref:`Simple API introduction <c-simple-api>` for details about parameter allocation.

.. _ydb-subscript-previous-s-st-fn:

++++++++++++++++++++++++++++++++++++++++++++++++++++++++
ydb_subscript_previous_s() / ydb_subscript_previous_st()
++++++++++++++++++++++++++++++++++++++++++++++++++++++++

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

:code:`ydb_subscript_previous_s()` and :code:`ydb_subscript_previous_st()` provide a primitive for implementing reverse traversal of a tree by searching for the previous subscript at the level specified by :code:`subs_used`. i.e. the subscript preceding the one referred to by :code:`subsarray[subs_used-1].buf_addr`. A node need not exist at the subscripted variable name provided as input to the function. If :code:`subsarray[subs_used-1].len_used` is zero, :code:`ret_value->buf_addr` points to last node at that level with a subscript that is not the empty string. :code:`ydb_subscript_previous_s()` and :code:`ydb_subscript_previous_st()` return:

- :code:`YDB_OK`, in which case :code:`ret_value->buf_addr` points to the value of that previous subscript;
- :code:`YDB_ERR_NODEEND` when there are no more subscripts at that level, in which case :code:`*ret_value` is unchanged;
- :code:`YDB_ERR_PARAMINVALID` when

  - :code:`ret_value` is NULL;
  - :code:`ret_value->buf_addr` is NULL and the return value has a non-zero :code:`len_used`; or
  - :code:`len_alloc` < :code:`len_used` or the :code:`len_used` is non-zero and :code:`buf_addr` is NULL in at least one subscript in :code:`subsarray`

- or another :ref:`error return code <err-ret-codes>`.

In the special case where :code:`subs_used` is zero, and the function returns :code:`YDB_OK`, :code:`ret_value->buf_addr` points to the previous local or global variable name, with :code:`YDB_ERR_NODEEND` indicating an end to the traversal.

Please see the :ref:`Simple API introduction <c-simple-api>` for details about parameter allocation.

.. _ydb-tp-s-st-fn:

++++++++++++++++++++++++
ydb_tp_s() / ydb_tp_st()
++++++++++++++++++++++++

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

:code:`ydb_tp_s()` and :code:`ydp_tp_st()` call the function referenced by :code:`tpfn` passing it :code:`tpfnparm` as a parameter. Additionally, :code:`ydb_tp_st()` also generates a new :code:`tptoken` that it passes as a parameter to the function referenced by its :code:`tpfn` parameter.

As discussed under :ref:`txn-proc`, a function implementing transaction processing logic should use the intrinsic special variable :code:`$trestart` to manage any externally visible action (which YottaDB recommends against, but which may be unavoidable). The function referenced by :code:`tpfn` should return one of the following:

- :code:`YDB_OK` — application logic indicates that the transaction can be committed (the YottaDB engine may still decide that a restart is required to ensure ACID transaction properties) as discussed under :ref:`txn-proc`.
- :code:`YDB_TP_RESTART`  — application logic indicates that the transaction should restart.
- :code:`YDB_TP_ROLLBACK` — application logic indicates that the transaction should not be committed.
- :code:`YDB_ERR_PARAMINVALID` when :code:`len_alloc` < :code:`len_used` or the :code:`len_used` is non-zero and :code:`buf_addr` is NULL in at least one variable name in :code:`varnames`.
- An :ref:`error return code <err-ret-codes>` returned by a YottaDB function called by the function. This case is treated the same way as if `YDB_TP_ROLLBACK` was returned (i.e. the application indicates that this transaction should not be committed).

:code:`transid` is a string, up to the first 8 bytes of which are recorded in the commit record of journal files for database regions participating in the transaction. If not NULL or the empty string, a case-insensitive value of :code:`"BA"` or :code:`"BATCH"` indicates that at transaction commit, YottaDB need not ensure Durability (it always ensures Atomicity, Consistency, and Isolation). Use of this value may improve latency and throughput for those applications where an alternative mechanism (such as a checkpoint) provides acceptable Durability. If a transaction that is not flagged as :code:`"BATCH"` follows one or more transactions so flagged, Durability of the later transaction ensures Durability of the the earlier :code:`"BATCH"` transaction(s).

If :code:`namecount>0`, :code:`varnames[i]` where :code:`0≤i<namecount` specifies local variable names whose values are restored to their original values when the transaction is restarted. In the special case where :code:`namecount=1` and :code:`varnames[0]` provides the value :code:`"*"`, all local variables are restored on a restart. It is an error for a :code:`varnames` to include a global or intrinsic special variable.

A top level :code:`ydb_tp_s()` and :code:`ydb_tp_st()` can return:

- :code:`YDB_OK`;
- :code:`YDB_TP_ROLLBACK`;
- :code:`YDB_ERR_TPTIMEOUT` (see :ref:`txn-proc`); or
- an :ref:`error return code <err-ret-codes>`, including :code:`YDB_ERR_NAMECOUNT2HI`.

A :code:`ydb_tp_s()` or :code:`ydb_tp_st()` call that is within another transaction (i.e., a nested transaction) can also return :code:`YDB_TP_RESTART` to its caller. [#]_

.. [#] An enclosing transaction can result not just from another
       :code:`ydb_tp_s()` or :code:`ydb_tp_st()` higher in the stack,
       but also (for single-threaded applications) from an M
       :code:`tstart` command as well as a database trigger resulting
       from a :ref:`ydb-delete-s-st-fn`, or :ref:`ydb-set-s-st-fn`.

.. note::

   If the transaction logic receives a :code:`YDB_TP_RESTART` from a YottaDB function that it calls, it *must* return that value to the calling :code:`ydb_tp_s()` or :code:`ydb_tp_st()`. Failure to do so could result in application level data inconsistencies and hard to debug application code.

Please see the :ref:`Simple API introduction <c-simple-api>` for details about parameter allocation.

.. _ydb-zwr2str-s-st-fn:

++++++++++++++++++++++++++++++++++
ydb_zwr2str_s() / ydb_zwr2str_st()
++++++++++++++++++++++++++++++++++

.. code-block:: C

        int ydb_zwr2str_s(ydb_buffer_t *zwr, ydb_buffer_t *str);

        int ydb_zwr2str_st(uint64_t tptoken,
                ydb_buffer_t *errstr,
                ydb_buffer_t *zwr, ydb_buffer_t *str);

In the buffer referenced by :code:`*str`, :code:`ydb_zwr2str_s()` and :code:`ydb_zwr2str_st()` provide the string described by the :ref:`zwrite formatted <zwrite-format>` string pointed to by :code:`*zwr`, returning

- :code:`YDB_OK` (with :code:`str->len_used` set to zero if the zwrite formatted string has an error);
- :code:`YDB_ERR_INVSTRLEN` error if the :code:`*str` buffer is not long enough;
- :code:`YDB_ERR_PARAMINVALID` either if the :code:`*str` buffer is NULL or the return value contains a
  non-zero :code:`len_used`  and the :code:`str->buf_addr` is NULL.

Please see the :ref:`Simple API introduction <c-simple-api>` for details about parameter allocation.

-----------------
Comprehensive API
-----------------

The Comprehensive API is a project for the future.

-------------------
YottaDB C to M APIs
-------------------

YottaDB C code has the ability to call M code. This allows you to reuse
existing M mcode written previously, as well as write code in M that may be
easier to write than writing the same code in C, then call it from C from your
application.

The C API needs a small text file called a "call-in table" that maps typed C
parameters to the typeless M code. This call-in table can be set as an
environment variable :code:`ydb_ci`, or it can be set from the C code at
runtime.

Here's a listing of these APIs. The APIs ending with _t are for use from
threaded applications. The discussion in :ref:`threads` provides more detailed
information. See the `Programmers Guide Call-In Interface
<../ProgrammersGuide/extrout.html#call-in-intf>`_ for full description as well
as a compilable example.

+--------------------------------+----------------------------------------------------------------------------------------------+
| API                            | Description                                                                                  |
+================================+==============================================================================================+
| :code:`ydb_ci`/                |  The most common API to use. Call an M function by its name in a call-in table.              |
| :code:`ydb_ci_t`               |                                                                                              |
+--------------------------------+----------------------------------------------------------------------------------------------+
| :code:`ydb_cip`/               | :code:`ydb_ci*` looks up the function each time it is called. While this takes a very small  |
| :code:`ydb_cip_t`              | amount of time, it can prove costly with thousands or millions of invocations. This version  |
|                                | allows you to cache the name lookup; but it's harder to use.                                 |
+--------------------------------+----------------------------------------------------------------------------------------------+
| :code:`ydb_ci_tab_open`/       | This opens a call-in table in a specific file.                                               |
| :code:`ydb_ci_tab_open_t`      |                                                                                              |
+--------------------------------+----------------------------------------------------------------------------------------------+
| :code:`ydb_ci_tab_switch`/     | This switches to a call-in table just opened above. You can have multiple call-in tables     |
| :code:`ydb_ci_tab_switch_t`    | open at the same time and switch between them.                                               |
+--------------------------------+----------------------------------------------------------------------------------------------+

.. _utility-funcs:

-----------------
Utility Functions
-----------------

Utility functions are functions that are not core to YottaDB functionality, but which are useful to application code.

Utility functions whose names end in :code:`_t()` are for use by multi-threaded applications, and those which do not are for single-threaded applications. The discussion in :ref:`threads` provides more detailed information.

:ref:`ydb-hiber-start-fn` and :ref:`ydb-hiber-start-wait-any-fn` are for use only with the SimpleAPI and not with the threaded Simple API.

:ref:`ydb-exit-fn`, :ref:`ydb-fork-n-core-fn`, and :ref:`ydb-init-fn` do not have separate variants for single- and multi-threaded applications and are suitable for both.


+++++++++++++++++++++++++++++++
ydb_call_variadic_plist_func()
+++++++++++++++++++++++++++++++

.. code-block:: C

   int ydb_call_variadic_plist_func(ydb_vplist_func cgfunc, gparam_list *cvplist)

:code:`ydb_call_variadic_plist_func` allows a language wrapper to make pseudo variadic calls to routines if the wrapper doesn't support variadic calls. Since some variadic calls are required to interface properly with YottaDB interfaces (e.g., :code:`ydb_ci()`, :code:`ydb_cip`, and :code:`ydb_lock_st()` etc.) this routine is needed. The return value is the same as the return value from the function, if any, with a 0 return value indicating successful completion.

The :code:`ydb_vplist_func` type is defined as follows:

.. code-block:: C

   typedef uintptr_t (*ydb_vplist_func)();

The :code:`gparam_list` type is defined as follows:

.. code-block:: C

   typedef struct gparam_list_struct
   {
	intptr_t	n;				/* Count of parameter/arguments */
	void    	*arg[MAX_GPARAM_LIST_ARGS];	/* Parameter/argument array */
   } gparam_list;

The first field :code:`n` is the count of valid parameters, which can have a maximum value of MAX_GPARAM_LIST_ARGS (currently 36).

To use :code:`ydb_call_variadic_plist_func()`, the :code:`cvplist` array needs to be filled in. Each element in the array is sized to hold a pointer. The :code:`arg` array holds all of the parameters (a maximum of 36 entries at this time) to be passed to the function. If a parameter does not fit as a single element, multiple elements can be used but this must be done in accordance with the calling API of the particular system.

.. note::

   On a 32 bit machine, each argument is only 32 bits wide so to pass a 64 bit value like a :code:`ydb_double_t`, the value will have to be split across two parameter slots. Alternatively, it may be easier to use a :code:`ydb_double_t *` type instead of :code:`ydb_double_t` so the parameter only takes one slot.

++++++++++++++++
ydb_child_init()
++++++++++++++++

YottaDB r1.22 and before required the use of a function :code:`ydb_child_init()` immediately after a :code:`fork()` to avoid database damage and other possible side-effects.

Effective YottaDB r1.24, this function is not needed. It gets automatically invoked by YottaDB as needed. Any existing usages of this function in an application can be safely removed assuming YottaDB r1.24 or later is in use.

.. _ydb-eintr-handler-handlert-fn:

+++++++++++++++++++++++++++++++++++++++++++
ydb_eintr_handler() / ydb_eintr_handler_t()
+++++++++++++++++++++++++++++++++++++++++++

.. code-block:: C

        int ydb_eintr_handler(void)

        int ydb_eintr_handler_t(uint64_t tptoken, ydb_buffer_t *errstr)

:code:`ydb_eintr_handler()` needs to be invoked by a SimpleAPI application whenever a system call that it invokes (e.g. :code:`accept()`, :code:`select()`) returns an error with `errno <https://linux.die.net/man/3/errno>`_ set to :code:`EINTR` (this usually means a signal interrupted the system call). This ensures that YottaDB takes appropriate action corresponding to the interrupting signal in a timely fashion. For example, if the signal :code:`SIGTERM` was sent externally to this SimpleAPI application process, the appropriate action is to terminate the process as soon as a safe/logical point is reached.

Note that not invoking :code:`ydb_eintr_handler()` as part of an :code:`EINTR` situation can cause the SimpleAPI application to behave unexpectedly. For example, in the :code:`SIGTERM` case, the process would not terminate how ever many signals are sent.

:code:`ydb_eintr_handler_t()` is very similar to :code:`ydb_eintr_handler()` except that it needs to be invoked by a SimpleThreadAPI application.

.. _ydb-exit-fn:

++++++++++
ydb_exit()
++++++++++

.. code-block:: C

        int ydb_exit(void)

When a caller no longer wishes to use YottaDB, a call to :code:`ydb_exit()` cleans up the process connection/access to all databases and cleans up its data structures. Therafter, any attempt to call a YottaDB function produces a :code:`YDB_ERR_CALLINAFTERXIT` error.

Note that:

- a typical application should not need to call :code:`ydb_exit()`, but should instead just terminate with a call to :code:`exit()` which will perform any cleanup needed by YottaDB; and
- calling :code:`ydb_exit()` before calling any other YottaDB function does nothing, i.e., it is a no-op.

:code:`ydb_exit()` returns :code:`YDB_OK` on success, and a positive non-zero value on error. If :code:`ydb_exit()` has already been called, later calls to :code:`ydb_exit()` in the same process return :code:`YDB_OK` with no further action, since all resources related to YottaDB are already cleaned up by the first call.

If an external call attempts to call :code:`ydb_exit()`, a :code:`YDB_ERR_INVYDBEXIT` error is returned, since YottaDB is required to remain operational even after the external call returns. For information about this error, see `INVYDBEXIT <../MessageRecovery/errors.html#invydbexit-error>`_ in the Messages and Recovery Procedures guide.

:code:`ydb_exit()` can be used with both the Simple API and threaded Simple API.

.. _ydb-file-id-free-freet-fn:

+++++++++++++++++++++++++++++++++++++++++
ydb_file_id_free() / ydb_file_id_free_t()
+++++++++++++++++++++++++++++++++++++++++

.. code-block:: C

        int ydb_file_id_free(ydb_fileid_ptr_t fileid)

        int ydb_file_id_free_t(uint64_t tptoken,
                ydb_buffer_t *errstr, ydb_fileid_ptr_t fileid)

Releases the memory used by a :code:`fileid` structure previously generated by :ref:`ydb-file-name-to-id-idt-fn`. Calling the function twice for the same pointer, unless it has been returned a second time by a different :ref:`ydb-file-name-to-id-idt-fn` is an application error with undefined consequences.

A :code:`PARAMINVALID` error is issued if the input :code:`fileid` parameter is NULL.

Please see the :ref:`Simple API introduction <c-simple-api>` for details about parameter allocation.

.. _ydb-file-is-identical-identicalt-fn:

+++++++++++++++++++++++++++++++++++++++++++++++++++
ydb_file_is_identical() / ydb_file_is_identical_t()
+++++++++++++++++++++++++++++++++++++++++++++++++++

.. code-block:: C

        int ydb_file_is_identical(ydb_fileid_ptr_t fileid1,
                ydb_fileid_ptr_t fileid2)

        int ydb_file_is_identical_t(uint64_t tptoken,
                ydb_buffer_t *errstr,
                ydb_fileid_ptr_t fileid1,
                ydb_fileid_ptr_t fileid2)

Given two pointers to :code:`fileid` structures (see :ref:`ydb-file-name-to-id-idt-fn`), :code:`ydb_file_is_identical()` and :code:`ydb_file_is_identical_t()` return YDB_OK if the two :code:`fileid` structures are the same file and YDB_NOTOK otherwise.

A :code:`PARAMINVALID` error is issued if the input :code:`fileid` parameter is NULL.

Please see the :ref:`Simple API introduction <c-simple-api>` for details about parameter allocation.

.. _ydb-file-name-to-id-idt-fn:

+++++++++++++++++++++++++++++++++++++++++++++++
ydb_file_name_to_id() / ydb_file_name_to_id_t()
+++++++++++++++++++++++++++++++++++++++++++++++

.. code-block:: C

        int ydb_file_name_to_id(ydb_string_t *filename,
                ydb_fileid_ptr_t *fileid)

        int ydb_file_name_to_id_t(uint64_t tptoken,
                ydb_buffer_t *errstr,
                ydb_string_t *filename,
                ydb_fileid_ptr_t *fileid)

As a file is potentially reachable through different paths, and application code may need to check whether two paths do indeed lead to the same file, YottaDB provides a mechanism to do so. Provided with a path to a file, YottaDB creates an internal structure called a :code:`fileid` that uniquely identifies the file if such a structure does not already exist for that file, and provides the caller with a pointer to that structure. The layout and contents of the fileid structure are opaque to the caller, which **must not** modify the pointer or the structure it points to.

When the :code:`fileid` structure for a file is no longer needed, an application should call :ref:`ydb-file-id-free-freet-fn` to release the structure and avoid a memory leak.

:code:`ydb_file_name_to_id()` and :code:`ydb_file_name_to_id_t()` return :code:`YDB_OK`, or an error return code.

A :code:`PARAMINVALID` error is issued if the input :code:`filename` or :code:`fileid` parameter is NULL.

Please see the :ref:`Simple API introduction <c-simple-api>` for details about parameter allocation.

.. _ydb-fork-n-core-fn:

+++++++++++++++++
ydb_fork_n_core()
+++++++++++++++++

.. code-block:: C

        void ydb_fork_n_core(void)

A core is a snapshot of a process, to help debug application code, for example to troubleshoot an out-of-design condition. When a process executes :code:`ydb_fork_n_core()`, it forks. The child process sends itself a signal to generate a core and terminate. On termination of the child process, the parent process continues execution. Note that depending on the nature of the condition necessitating a core, an :code:`exit()` may well be the right action for the parent process. An :code:`exit()` call will drive YottaDB exit handlers to perform clean shutdown of databases and devices the process has open.

The content, location, and naming of cores is managed by the operating system – see :code:`man 5 core` for details. We recommend that you set :code:`kernel.core_uses_pid` to 1 to make it easier to identify and track cores. As cores will likely contain protected confidential information, you *must* ensure appropriate configuration and management of cores.

In a multi-threaded environment, only the thread that executes :code:`ydb_fork_n_core()` or :code:`ydb_fork_n_core()` survives in the child and is dumped.

:code:`ydb_fork_n_core()` can be used with both the Simple API and threaded Simple API.

.. _ydb-free-fn:

++++++++++
ydb_free()
++++++++++

.. code-block:: C

        void ydb_free(void *ptr)


Releases memory previously allocated by :ref:`ydb-malloc-fn`. Passing :code:`ydb_free()` a pointer not previously provided to the application by :ref:`ydb-malloc-fn` can result in unpredictable behavior. The signature of :code:`ydb_free()` matches that of the POSIX :code:`free()` call.

:code:`ydb_free()` should not be used in multiple threads in multi-threaded programs. (See the :ref:`threads` section for details). However, the :code:`YDB_FREE_BUFFER` macro is safe to use in multiple threads.

.. _ydb-hiber-start-fn:

+++++++++++++++++
ydb_hiber_start()
+++++++++++++++++

.. code-block:: C

        int ydb_hiber_start(unsigned long long sleep_nsec)

The process or thread sleeps for the time in nanoseconds specified by :code:`sleep_nsec`. If a value greater than :code:`YDB_MAX_TIME_NSEC` is specified, :code:`ydb_hiber_start()` immediately returns with a :code:`YDB_ERR_TIME2LONG` error; otherwise they return :code:`YDB_OK` after the elapsed time.

:code:`ydb_hiber_start()` should not be used in multiple threads in multi-threaded programs. (See the :ref:`threads` section for details).

.. _ydb-hiber-start-wait-any-fn:

++++++++++++++++++++++++++
ydb_hiber_start_wait_any()
++++++++++++++++++++++++++

.. code-block:: C

        int ydb_hiber_start_wait_any(unsigned long long sleep_nsec)

The process or thread sleeps for the time in nanoseconds specified by :code:`sleep_nsec` or until it receives a signal. If a value greater than :code:`YDB_MAX_TIME_NSEC` is specified, :code:`ydb_hiber_start_wait_any()` immediately returns with a :code:`YDB_ERR_TIME2LONG` error; otherwise they return :code:`YDB_OK` after the elapsed time or when the wait is terminated by a signal.

:code:`ydb_hiber_start_wait_any()` should not be used in multiple threads in multi-threaded programs. (See the :ref:`threads` section for details).

.. _ydb-init-fn:

++++++++++
ydb_init()
++++++++++

.. code-block:: C

        int ydb_init(void)

:code:`ydb_init()` initializes the YottaDB runtime environment. As YottaDB automatically initializes the runtime on the first call to its API or first M code invocation, there is usually no need to explicitly call :code:`ydb_init()`. The exception is when an application wishes to set its own signal handlers (see :ref:`signals`): :code:`ydb_init()` sets signal handlers, and in case an application wishes to set its own signal handlers for signals not used by YottaDB, it can call :code:`ydb_init()` before setting its signal handlers.

:code:`ydb_init()` returns :code:`YDB_OK` on success, and a positive non-zero value otherwise. On failure, the error message text corresponding to the non-zero return value can be obtained by immediately calling :code:`ydb_zstatus()`.

If :code:`ydb_init()` has already been called, later calls to :code:`ydb_init()` in the same process return :code:`YDB_OK` with no further action, since the YottaDB runtime has already been initialized.

:code:`ydb_init()` can be used with both the Simple API and threaded Simple API.

.. _ydb-malloc-fn:

++++++++++++
ydb_malloc()
++++++++++++

.. code-block:: C

        void *ydb_malloc(size_t size)

With a signature matching that of the POSIX :code:`malloc()` call, :code:`ydb_malloc()` returns an address to a block of memory of the requested size, or NULL if it is unable to satisfy the request. :code:`ydb_malloc()` uses a `buddy system <https://en.wikipedia.org/wiki/Buddy_memory_allocation>`_, and provides debugging functionality under the control of the environment variable :code:`ydb_dbglvl` whose values are a mask as described in `gtmdbglvl.h <https://gitlab.com/YottaDB/DB/YDB/blob/master/sr_port/gtmdbglvl.h>`_.

:code:`ydb_malloc()` should not be used in multiple threads in multi-threaded programs. (See the :ref:`threads` section for details). However, the :code:`YDB_MALLOC_BUFFER` macro is safe to use in multiple threads.

.. _ydb-message-messaget-fn:

+++++++++++++++++++++++++++++++
ydb_message() / ydb_message_t()
+++++++++++++++++++++++++++++++

.. code-block:: C

        int ydb_message(int errnum, ydb_buffer_t *msg_buff)

        int ydb_message_t(uint64_t tptoken, ydb_buffer_t *errstr,
                int errnum, ydb_buffer_t *msg_buff)

The functions return the error message text template for the error number specified by :code:`errnum`.

- If :code:`errnum` does not correspond to an error that YottaDB recognizes, the return the error :code:`YDB_ERR_UNKNOWNSYSERR`, leaving the structures referenced by :code:`msg_buff` unaltered.
- Otherwise, if the length of the text exceeds :code:`msg_buff->len_alloc` they return the error :code:`YDB_ERR_INVSTRLEN`. In this case :code:`msg_buff->len_used` is greater than :code:`msg_buff->len_alloc`.
- Otherwise, if :code:`msg_buff->buf_addr` is NULL, they return the error :code:`YDB_ERR_PARAMINVALID`.
- Otherwise, the copy the text to the buffer specified by :code:`msg_buff->buf_addr`, set :code:`msg_buff->len_used` to its length, and return :code:`YDB_OK`.

Please see the :ref:`Simple API introduction <c-simple-api>` for details about parameter allocation.

++++++++++++++++
ydb_mmrhash_32()
++++++++++++++++

.. code-block:: C

    void ydb_mmrhash_32(const void *key, int len, uint4 seed, uint4 *out4);

This function returns in :code:`*out4` the 32-bit (4-byte) MurmurHash of :code:`len` bytes at :code:`*key`.

Please see the :ref:`Simple API introduction <c-simple-api>` for details about parameter allocation.

.. _ydb-mmrhash-128-fn:

+++++++++++++++++
ydb_mmrhash_128()
+++++++++++++++++

.. code-block:: C

    void ydb_mmrhash_128(const void *key, int len, uint4 seed, ydb_uint16 *out);

This function returns in :code:`*out` the 128-bit (16-byte) MurmurHash of :code:`len` bytes at :code:`*key`.

Please see the :ref:`Simple API introduction <c-simple-api>` for details about parameter allocation.

.. _ydb-mmrhash-128-ingest-result-fn:

+++++++++++++++++++++++++++++++++++++++++++++++++++
ydb_mmrhash_128_ingest() / ydb_mmrhash_128_result()
+++++++++++++++++++++++++++++++++++++++++++++++++++

.. code-block:: C

    void ydb_mmrhash_128_ingest(hash128_state_t *state, const void *key, int len);

    void ydb_mmrhash_128_result(hash128_state_t *state, uint4 addl_seed, ydb_uint16 *out);

These functions enable users to get a MurmurHash through a series of incremental operations.

The sequence is to first initialize the "state" variable using the :ref:`HASH128-STATE-INIT-fn` macro, then call :code:`ydb_mmrhash_128_ingest()` one or more times and finally call :code:`ydb_mmrhash_128_result()` to obtain the final hash value. "key" points to the input character array (of length "len") for the hash. "addl_seed" can either be the last four bytes of the input, or at the application's discretion, an additional seed or salt. An example is to set it to the sum of the "len" values passed in across all calls to :code:`ydb_mmrhash_128_ingest` before :code:`ydb_mmrhash_128_result` is called. "out" points to the structure holding the 16-byte hash result.

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

Please see the :ref:`Simple API introduction <c-simple-api>` for details about parameter allocation.

.. _ydb-mmrhash-128-hex-fn:

+++++++++++++++++++++
ydb_mmrhash_128_hex()
+++++++++++++++++++++

.. code-block:: C

    void ydb_mmrhash_128_hex(const ydb_uint16 *hash, unsigned char *out);

This function returns a hex formatted representation of a 16-byte hash value. As the function does no checking, if :code:`*out` is not at least 32 bytes, a buffer overflow can occur, potentially with unpleasant consequences such as abnormal process termination with a SIG-11, or worse.

Example:

.. code-block:: C

   char out[16];
   ydb_mmrhash_128_hex(&hash, out);

Please see the :ref:`Simple API introduction <c-simple-api>` for details about parameter allocation.

+++++++++++++++++++++++
ydb_mmrhash_128_bytes()
+++++++++++++++++++++++

.. code-block:: C

    void ydb_mmrhash_128_bytes(const ydb_uint16 *hash, unsigned char *out);

This function converts the 16-byte hash stored in a "ydb_uint16" structure (2 8-byte integers) into a byte array "out" of 16 characters. It is also internally used by :ref:`ydb-mmrhash-128-hex-fn`.

Example:

.. code-block:: C

   char out[16];
   ydb_mmrhash_128_bytes(&hash, out);

Please see the :ref:`Simple API introduction <c-simple-api>` for details about parameter allocation.

.. _ydb-stdout-stderr-adjust-adjustt-fn:

+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
ydb_stdout_stderr_adjust() / ydb_stdout_stderr_adjust_t()
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++

.. code-block:: C

        int ydb_stdout_stderr_adjust(void)

        int ydb_stdout_stderr_adjust_t(uint64 tptoken,
                ydb_buffer_t *errstr)

The functions check whether stdout (file descriptor 1) and stderr (file descriptor 2) are the same file, and if so, route stderr writes to stdout instead. This ensures that output appears in the order in which it was written; otherwise owing to IO buffering, output can appear in an order different from that in which it was written. Application code which mixes C and M code, and which explicitly redirects stdout or stderr (e.g., using :code:`dup2()`), should call one of these functions as soon as possible after the redirection. :code:`ydb_stdout_stderr_adjust()` and :code:`ydb_stdout_stderr_adjust_t()` return :code:`YDB_OK`.

Please see the :ref:`Simple API introduction <c-simple-api>` for details about parameter allocation.

.. _ydb-thread-is-main-fn:

++++++++++++++++++++
ydb_thread_is_main()
++++++++++++++++++++

.. code-block:: C

        int ydb_thread_is_main(void)

The functions return :code:`YDB_OK` if the thread is the main thread of the process, and another value if the thread is not. YottaDB recommends against application code that requires use of these functions, which exist only to provide backward compatibility to a specific application code base (see discussion under :ref:`threads`).

.. _ydb-timer-cancel-cancelt-fn:

+++++++++++++++++++++++++++++++++++++++++
ydb_timer_cancel() / ydb_timer_cancel_t()
+++++++++++++++++++++++++++++++++++++++++

.. code-block:: C

        void ydb_timer_cancel(intptr_t timer_id)

        void ydb_timer_cancel_t(uint64_t tptoken,
                ydb_buffer_t *errstr, intptr_t timer_id)

Cancel a timer identified by :code:`timer_id` and previously started with :ref:`ydb-timer-start-startt-fn`.

Please see the :ref:`Simple API introduction <c-simple-api>` for details about parameter allocation.

.. _ydb-timer-start-startt-fn:

+++++++++++++++++++++++++++++++++++++++
ydb_timer_start() / ydb_timer_start_t()
+++++++++++++++++++++++++++++++++++++++

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

Start a timer. Unless canceled, when the timer expires, :code:`ydb_timer_start()` and :code:`ydb_timer_start_t()` invoke a handler function, providing that function with input data.

:code:`timer_id` is an identifier for the the timer. It is the responsibility of application code to ensure that :code:`timer_id` is different from those of any other active / pending timers.

:code:`limit_nsec` is the minimum number of nanoseconds before the timer expires and invokes the handler function. Owing to overhead and system load, the actual time will almost always be greater than this value.

:code:`handler` is a pointer to the function to be called when the timer expires.

:code:`handler_data` is a pointer to the data to be passed to :code:`handler` and :code:`handler_data_len` is the length of the data at :code:`*handler_data`. Note that the data it points to **must** be on the heap rather than on the stack, as the stack frame may no longer be valid when the timer expires.

If the requested :code:`timeout_nsec` exceeds :code:`YDB_MAX_TIME_NSEC`, the functions return :code:`YDB_ERR_TIME2LONG`; otherwise they return :code:`YDB_OK`.

Please see the :ref:`Simple API introduction <c-simple-api>` for details about parameter allocation.

------------------
Calling M Routines
------------------

M routines can be called from C with the following functions which are described in the `M Programmers Guide <../ProgrammersGuide/extrout.html#calls-ext-rt-call-ins>`_:

* `ydb_ci() <../ProgrammersGuide/extrout.html#ydb-ci-intf>`_
* `ydb_ci_t() <../ProgrammersGuide/extrout.html#ydb-ci-t-intf>`_
* `ydb_cip() <../ProgrammersGuide/extrout.html#ydb-cip-intf>`_
* `ydb_cip_t() <../ProgrammersGuide/extrout.html#ydb-cip-t-intf>`_
* `ydb_zstatus() <../ProgrammersGuide/extrout.html#ydb-zstatus>`_

Historically, the predecessors of the functions to call M routines returned positive return codes. In order to maintain backward compatibility, values returned by the above are positive values, whereas YottaDB :ref:`err-ret-codes` are negative. For example, to return an invalid string length (:ref:`YDB-ERR-INVSTRLEN`), the :code:`ydb_ci*()` functions return :code:`-YDB_ERR_INVSTRLEN`, which is a positve value because :code:`YDB_ERR_STRLEN` is a negative value.

Effective release `r1.30. <https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.30>`_ :code:`ydb_zstatus()` returns an :code:`int`.
