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

================================
Programming in Go
================================

.. contents::
   :depth: 5

Programming YottaDB in the `Go language <https://golang.org/>`_ is
accomplished through a wrapper for `Simple API <https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#simple-api>`_ threaded functions
that uses `cgo <https://golang.org/cmd/cgo/>`_ to provide a “yottadb”
package for access from Go application code. The wrapper must be
installed on a system after YottaDB is installed.

There are two Go APIs:

- `Go Easy API`_ aims to be a straightforward, easy-to-use API to access
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

As the Go language has important differences from C (for example, it
has structures with methods but lacks macros), below are Go-specific
sections of the `Quick Start <https://docs.yottadb.com/MultiLangProgGuide/MultiLangProgGuide.html#quick-start>`_,
`Concepts <https://docs.yottadb.com/MultiLangProgGuide/MultiLangProgGuide.html#concepts>`_,
`Symbolic Constants <https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#symbolic-constants>`_,
`Data Structures & Type Definitions <https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#data-structures-type-definitions>`_,
`Simple API <https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#simple-api>`_ and `Utility
Functions <https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#utility-functions>`_ sections.
The sections that are specific to Go are intended to supplement, but not subsume, their C counterparts.

Go application code *must not* directly use the YottaDB C API
structures and functions (those prefixed by :code:`C.` or described in
the C `Simple API <https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#simple-api>`_
above) as such usage bypasses important controls,
but should instead use the structures, methods and functions exposed
by the YottaDB Go wrapper. :code:`C.` prefixed structures and
functions are mentioned only for clarity in documentation and brevity
of explanation. For example, :code:`C.ydb_buffer_t` is the C
:code:`ydb_buffer_t` structure defined in `Data Structures & Type
Definitions <https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#data-structures-type-definitions>`_.

All subsections of the `Programming in Go` section are prefixed with
“Go” to ensure unique names for hyperlinking.

As Go implementations are inherently multi-threaded, where the C
`Simple API <https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#simple-api>`_
provides separate functions for use in multi-threaded
applications, e.g., `ydb_get_s() vs. ydb_get_st() <https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#ydb-get-s-ydb-get-st>`_, the Go wrapper
wraps the function for use in multi-threaded applications. Also, as
Go is multi-threaded, calls include an `errstr <https://docs.yottadb.com/MultiLangProgGuide/programmingnotes.html#errstr>`_ parameter to get the
correct `$zstatus <https://docs.yottadb.com/MultiLangProgGuide/MultiLangProgGuide.html#zstatus>`_ for each call.

Go Quick Start
==============

The YottaDB Go wrapper requires a minimum YottaDB version of r1.28 and
is tested with a minimum Go version of 1.10.4. If the Golang packages
on your operating system are older, and the Go wrapper does not work,
please obtain and install a newer Golang implementation.

The `Go Quick Start`_ assumes that YottaDB has already been installed
as described in the `Quick Start <https://docs.yottadb.com/MultiLangProgGuide/MultiLangProgGuide.html#quick-start>`_ section. After completing step 1
(*Installing YottaDB*), download the Go wrapper, install it and
test it.

.. code-block:: bash

        $ go get lang.yottadb.com/go/yottadb
        $ go build lang.yottadb.com/go/yottadb
        $ source $(pkg-config --variable=prefix yottadb)/ydb_env_set
        $ go get -t lang.yottadb.com/go/yottadb
        $ go test lang.yottadb.com/go/yottadb
        ok      lang.yottadb.com/go/yottadb     0.194s
        $

There are a number of programs in the
:code:`go/src/lang.yottadb.com/go/yottadb` directory that you can
look at.

3. Put your GO program in a directory of your choice, e.g.,
   :code:`$ydb_dir` directory and change to that directory.
   As a sample program, you can download the `wordfreq.go program <https://gitlab.com/YottaDB/DB/YDBTest/blob/master/go/inref/wordfreq.go>`_, with a
   `reference input file
   <https://gitlab.com/YottaDB/DB/YDBTest/blob/master/simpleapi/outref/wordfreq_input.txt>`_
   and `corresponding reference output file
   <https://gitlab.com/YottaDB/DB/YDBTest/blob/master/simpleapi/outref/wordfreq_output.txt>`_.
   Compile it thus: :code:`go build wordfreq.go`.

#. Run your program and verify that the output matches the reference output. For example:

.. code-block:: bash

        $ cd $ydb_dir
        $ wget https://gitlab.com/YottaDB/DB/YDBTest/raw/master/go/inref/wordfreq.go
        $ go build wordfreq.go
        $ wget https://gitlab.com/YottaDB/DB/YDBTest/raw/master/simpleapi/outref/wordfreq_input.txt
        $ wget https://gitlab.com/YottaDB/DB/YDBTest/raw/master/simpleapi/outref/wordfreq_output.txt
        $ ./wordfreq <wordfreq_input.txt >wordfreq_output_go.txt
        $ diff wordfreq_output_go.txt wordfreq_output.txt
        $

Note that the :code:`wordfreq.go` program randomly uses local or
global variables (see `Local and Global Variables <https://docs.yottadb.com/MultiLangProgGuide/MultiLangProgGuide.html#local-and-global-variables>`_).

Go Concepts
===========

As the YottaDB wrapper is distributed as a Go package, function calls
to YottaDB are prefixed in Go code with :code:`yottadb.` (e.g.,
application code to call the :code:`ValST()` function would be written
:code:`yottadb.ValST(…)`.

------------------
Go Error Interface
------------------

YottaDB has a comprehensive set of error return codes. Each has a
unique number and a mnemonic. Thus, for example, to return an error
that a buffer allocated for a return value is not large enough,
YottaDB uses the INVSTRLEN error code, which has the numeric value
:code:`yottadb.YDB_ERR_INVSTRLEN`. YottaDB attempts to maintain stability of
the numeric values and mnemonics from release to release, to ensure
applications remain compatible when the underlying YottaDB releases
are upgraded. While the Go :code:`error` interface provides for a call
to return an error as a string (with :code:`nil` for a successful
return), applications in other languages, such as C, expect a numeric
return value.

Where C application code calling YottaDB functions will check the
return code, and if it is not :code:`YDB_OK` access the intrinsic
special variable `$zstatus <https://docs.yottadb.com/MultiLangProgGuide/MultiLangProgGuide.html#zstatus>`_ for more detailed information (through
the :code:`errstr` parameter in a multi-threaded application), Go
application code calling YottaDB methods and functions will check the
:code:`error` interface to determine whether it is :code:`nil`. This means
that Go application code will never see a :code:`yottadb.YDB_OK` return.

The YottaDB Go :code:`error` interface has a structure and a method. Sample usage:

.. code-block:: go

    _, err := yottadb.ValE(yottadb.NOTTP, nil, "^hello", []string{})
   if err != nil {
       errcode := yottadb.ErrorCode(err)
    }

In the documentation:

- Error codes specific to each function are noted. However, common
  errors can also be returned. For example, while the `Go BufferT
  ValStr()`_ method can return INVSTRLEN, it can also return errors
  from the YottaDB engine.
- An error name such as INVSTRLEN refers to the underlying error,
  whether application code references the numeric value or the string.

Go Symbolic Constants
=====================

`YottaDB symbolic constants
<https://godoc.org/lang.yottadb.com/go/yottadb#pkg-constants>`_ are
available in the YottaDB package, for example,
:code:`yottadb.YDB_ERR_INVSTRLEN`.

:code:`NOTTP` (:code:`yottadb.NOTTP`) as a value for parameter :code:`tptoken`
indicates to the invoked YottaDB method or function that the caller is
not inside a `transaction <https://docs.yottadb.com/MultiLangProgGuide/MultiLangProgGuide.html#transaction>`_.

Go Easy API
===========

A global or local variable node, or an intrinsic special variable, is
specified using the construct :code:`varname string, subary
[]string`. For an intrinsic special variable, :code:`subary` must be
the null array, :code:`[]string{}`, or :code:`nil`. For a global or local variable, a
null array or :code:`nil` for :code:`subary` refers to the root node, the entire
tree, or both, depending on the function and context.

As the `Go Easy API`_ involves more copying of data between the Go and
YottaDB runtime systems, it requires the CPU to perform a little more
work than the `Go Simple API`_ does. Whether or not this has a
measurable impact on performance depends on the application and
workload.

The length of strings (values and subscripts) in YottaDB is variable, as is
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

Go DataE()
----------

.. code-block:: go

        func DataE(tptoken uint64, errstr *BufferT, varname string, subary []string) (uint32, error)

Matching `Go DataST()`_, :code:`DataE()` function wraps and returns the
result of `ydb_data_st() <https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#ydb-data-s-ydb-data-st>`_ (0, 1, 10, or 11). In the event of an error, the return
value is unspecified.

Go DeleteE()
------------

.. code-block:: go

        func DeleteE(tptoken uint64, errstr *BufferT, deltype int, varname string, subary []string) error

Matching `Go DeleteST()`_, :code:`DeleteE()` wraps `ydb_delete_st() <https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#ydb-delete-s-ydb-delete-st>`_ to
delete a local or global variable node or (sub)tree, with a value of
:code:`yottadb.YDB_DEL_NODE` for :code:`deltype` specifying that only the
node should be deleted, leaving the (sub)tree untouched, and a value
of :code:`yottadb.YDB_DEL_TREE` specifying that the node as well as the
(sub)tree are to be deleted.

Go DeleteExclE()
----------------

.. code-block:: go

        func DeleteExclE(tptoken uint64, errstr *BufferT, varnames []string) error

Matching `Go DeleteExclST()`_, :code:`DeleteExclE()` wraps
`ydb_delete_excl_st() <https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#ydb-delete-excl-s-ydb-delete-excl-st>`_ to delete all local variables except those
specified. In the event :code:`varnames` has no elements (i.e.,
:code:`[]string{}`), :code:`DeleteExclE()` deletes all local
variables.

In the event that the number of variable names in :code:`varnames`
exceeds :code:`yottadb.YDB_MAX_NAMES`, the error return is
ERRNAMECOUNT2HI. Otherwise, if `ydb_delete_excl_st() <https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#ydb-delete-excl-s-ydb-delete-excl-st>`_ returns an
error, the function returns the error.

As mixing M and Go application code in the same process is not supported, the
warning in `ydb_delete_excl_s() <https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#ydb-delete-excl-s-ydb-delete-excl-st>`_ does not apply.

Go IncrE()
----------

.. code-block:: go

        func IncrE(tptoken uint64, errstr *BufferT, incr, varname string, subary []string) (string, error)

Matching `Go IncrST()`_, :code:`IncrE()` wraps `ydb_incr_st() <https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#ydb-incr-s-ydb-incr-st>`_ to
atomically increment the referenced global or local variable node
coerced to a number with :code:`incr` coerced to a number, with the
result stored in the node and returned by the function.

- If `ydb_incr_st() <https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#ydb-incr-s-ydb-incr-st>`_ returns an error such as NUMOFLOW or INVSTRLEN,
  the function returns the error.
- Otherwise, it returns the incremented value of the node.

With a :code:`nil` value for :code:`incr`, the default increment
is 1. Note that the value of the empty string coerced to an integer is
zero, but 1 is a more useful default value for an omitted parameter in
this case.

Go LockDecrE()
--------------

.. code-block:: go

        func LockDecrE(tptoken uint64, errstr *BufferT, varname string, subary []string) error

Matching `Go LockDecrST()`_ :code:`LockDecrE()` wraps
`ydb_lock_decr_st() <https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#ydb-lock-decr-s-ydb-lock-decr-st>`_ to decrement the count of the lock name
referenced, releasing it if the count goes to zero or ignoring the
invocation if the process does not hold the lock.

Go LockE()
----------

.. code-block:: go

        func LockE(tptoken uint64, errstr *BufferT, timeoutNsec uint64, namesnsubs ... interface{}) error

Matching `Go LockST()`_, :code:`LockE()` releases all lock resources
currently held and then attempts to acquire the named lock resources
referenced. If no lock resources are specified, it simply releases all
lock resources currently held and returns.

:code:`interface{}` is a series of pairs of :code:`varname string` and
:code:`subary []string` parameters, where a null `subary` parameter
(:code:`[]string{}`) specifies the unsubscripted lock resource
name.

If lock resources are specified, upon return, the process will have
acquired all of the named lock resources or none of the named lock
resources.

- If :code:`timeoutNsec` exceeds :code:`yottadb.YDB_MAX_TIME_NSEC`, the
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

Go LockIncrE()
--------------

.. code-block:: go

        func LockIncrE(tptoken uint64, errstr *BufferT, timeoutNsec uint64, varname string, subary []string) error

Matching `Go LockIncrST()`_, :code:`LockIncrE()` wraps
`ydb_lock_incr_st() <https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#ydb-lock-incr-s-ydb-lock-incr-st>`_ to attempt to acquire the referenced lock
resource name without releasing any locks the process already holds.

- If the process already holds the named lock resource, the function
  increments its count and returns.
- If :code:`timeoutNsec` exceeds :code:`yottadb.YDB_MAX_TIME_NSEC`, the
  function returns with an error return TIME2LONG.
- If it is able to aquire the lock resource within :code:`timeoutNsec`
  nanoseconds, it returns holding the lock, otherwise it returns
  LOCKTIMEOUT. If :code:`timeoutNsec` is zero, the function makes
  exactly one attempt to acquire the lock.

Go NodeNextE()
--------------

.. code-block:: go

        func NodeNextE(tptoken uint64, errstr *BufferT,
                varname string, subary []string) ([]string, error)

Matching `Go NodeNextST()`_, :code:`NodeNextE()` wraps
`ydb_node_next_st() <https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#ydb-node-next-s-ydb-node-next-st>`_ to facilitate depth first traversal of a local or
global variable tree. A node or subtree does not have to exist at the
specified key.

- If there is a next node, it returns the subscripts of that next
  node.
- If there is no node following the specified node, the function returns the NODEEND error.

Go NodePrevE()
--------------

.. code-block:: go

        func NodePrevE(tptoken uint64, errstr *BufferT, varname string, subary []string) ([]string, error)

Matching `Go NodePrevST()`_, :code:`NodePrevE()` wraps
`ydb_node_previous_st() <https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#ydb-node-previous-s-ydb-node-previous-st>`_ to facilitate reverse depth first traversal
of a local or global variable tree. A node or subtree does not have to exist at the
specified key.

- If there is a previous node, it returns the subscripts of that
  previous node; an empty string array if that previous node is the root.
- If there is no node preceding the specified node, the function returns the NODEEND error.

Go SetValE()
------------

.. code-block:: go

        func SetValE(tptoken uint64, errstr *BufferT, value, varname string, subary []string) error

Matching `Go SetValST()`_, at the referenced local or global variable
node, or the intrinsic special variable, :code:`SetValE()` wraps
`ydb_set_st() <https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#ydb-set-s-ydb-set-st>`_ to set the value specified by :code:`value`.

Go SubNextE()
-------------

.. code-block:: go

        func SubNextE(tptoken uint64, errstr *BufferT, varname string, subary []string) (string, error)

Matching `Go SubNextST()`_, :code:`SubNextE()` wraps
`ydb_subscript_next_st() <https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#ydb-subscript-next-s-ydb-subscript-next-st>`_ to facilitate breadth-first traversal of a
local or global variable sub-tree. A node or subtree does not have to exist at the
specified key.

- At the level of the last subscript, if there is a next subscript
  with a node and/or a subtree, it returns that subscript.
- If there is no next node or subtree at that level of the subtree,
  the function returns the NODEEND error.

In the special case where :code:`subary` is the null array,
:code:`SubNextE()` returns the name of the next global or local
variable, and the NODEEND error if there is no global or local
variable following  :code:`varname`.

Go SubPrevE()
-------------

.. code-block:: go

        func SubPrevE(tptoken uint64, errstr *BufferT, varname string, subary []string) (string, error)

Matching `Go SubPrevST()`_, :code:`SubPrevE()` wraps
`ydb_subscript_previous_st() <https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#ydb-subscript-previous-s-ydb-subscript-previous-st>`_ to facilitate reverse breadth-first
traversal of a local or global variable sub-tree. A node or subtree does not have to exist at the
specified key.

- At the level of the last subscript, if there is a previous subscript
  with a node and/or a subtree, it returns that subscript.
- If there is no previous node or subtree at that level of the
  subtree, the function returns the NODEEND error.

In the special case where :code:`subary` is the null array
:code:`SubNextE()` returns the name of the previous global or local
variable, and the NODEEND error if there is no global or local
variable preceding :code:`varname`.

Go TpE()
--------

.. code-block:: go

        func TpE(tptoken uint64, errstr *BufferT, tpfn func(uint64, *BufferT) int32,
		transid string, varnames []string) error

Matching `Go TpST()`_, :code:`TpE()` wraps :code:`ydb_tp_st()` to
implement `Transaction Processing <https://docs.yottadb.com/MultiLangProgGuide/MultiLangProgGuide.html#transaction-processing>`_.

Refer to `Go TpST()`_ for a more detailed discussion of YottaDB Go
transaction processing.

Go ValE()
---------

.. code-block:: go

        func ValE(tptoken uint64, errstr *BufferT, varname string, subary []string) (string, error)

Matching `Go ValST()`_, :code:`ValE()` wraps `ydb_get_st() <https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#ydb-get-s-ydb-get-st>`_ to return
the value at the referenced global or local variable node, or
intrinsic special variable.

- If `ydb_get_s() <https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#ydb-get-s-ydb-get-st>`_ returns an error such as GVUNDEF, INVSVN, LVUNDEF,
  the function returns the error.
- Otherwise, it returns the value at the node.

Go Simple API
=============

The Go Simple API consists of `Go Data Structures & Type
Definitions`_, `Go Simple API Access Methods`_, `Go Simple API BufferT Methods`_, `Go Simple API
BufferTArray Methods`_, `Go Simple API KeyT Methods`_ and `Go Simple
API Functions`_. Each of them wraps a function in the C `Simple API <https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#simple-api>`_
– refer to the descriptions of those functions for more detailed
information. The majority of the functionality is in `Go Simple API
KeyT Methods`_.

-------------------------------------
Go Data Structures & Type Definitions
-------------------------------------

The :code:`C.ydb_buffer_t` structure, which is the
:code:`ydb_buffer_t` structure described in `Data Structures & Type
Definitions <https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#data-structures-type-definitions>`_ is used to pass values between Go application code and
YottaDB. The design pattern is that the :code:`ydb_buffer_t`
structures are in memory managed by YottaDB. Go structures contain
pointers to the YottaDB structures so that when the Go garbage
collector moves Go structures, the pointers they contain remain valid.

There are five structures for the interface between YottaDB and Go:

- :code:`BufferT` for data;
- :code:`BufferTArray` for a list of subscripts or a set of variable names;
- :code:`KeyT` for keys where a key in turn consists of a variable or
  lock resource name and subscripts, as discussed in `Concepts
  <https://docs.yottadb.com/MultiLangProgGuide/MultiLangProgGuide.html#concepts>`_; and
- :code:`CallMDesc` references an M routine and caches information to
  accelerate calls from Go to M.
- :code:`CallMTable` to reference `an M code call-in table
  <https://docs.yottadb.com/ProgrammersGuide/extrout.html#calls-from-external-routines-call-ins>`_.

Methods for each structure are classified as `Go Simple API Access
Methods`_, `Go Simple API`_ methods, or `Go Utility API`_ methods. `Go
Simple API Access Methods`_ are methods implemented in the Go wrapper
for managing the structures for data interchange. `Go Simple API`_
methods wrap functionality exposed by the YottaDB API. Methods for
:code:`CallMDesc` and :code:`CallMTable` are listed in the `Go Utility
API`_ section.

----------------------------
Go Simple API Access Methods
----------------------------

Go Simple API Access Methods for BufferT
----------------------------------------

Go BufferT Alloc()
..................

.. code-block:: go

        func (buft *BufferT) Alloc(nBytes uint32)

Allocate a buffer in YottaDB heap space of size :code:`nBytes`; and
set :code:`BufferT`
structure to provide access to that buffer.

Go BufferT Dump()
.................

.. code-block:: go

        func (buft *BufferT) Dump()

For debugging purposes, dump on stdout:

- :code:`cbuft` as a hexadecimal address;
- for the :code:`C.ydb_buffer_t` structure referenced by
  :code:`cbuft`:

  - :code:`buf_addr` as a hexadecimal address, and
  - :code:`len_alloc` and :code:`len_used` as integers; and

- at the address :code:`buf_addr`, the lower of :code:`len_used` or
  :code:`len_alloc` bytes in `zwrite format <https://docs.yottadb.com/MultiLangProgGuide/programmingnotes.html#zwrite-format>`_.

As this function is intended for debugging and provides details of
internal structures, its output is likely to change as internal
implementations change, even when stability of the external API is
maintained.

Go BufferT DumpToWriter()
.........................

.. code-block:: go

        func (buft *BufferT) DumpToWriter(writer io.writer)

For debugging purposes, dump on :code:`writer`:

- :code:`cbuft` as a hexadecimal address;
- for the :code:`C.ydb_buffer_t` structure referenced by
  :code:`cbuft`:

  - :code:`buf_addr` as a hexadecimal address, and
  - :code:`len_alloc` and :code:`len_used` as integers; and

- at the address :code:`buf_addr`, the lower of :code:`len_used` or
  :code:`len_alloc` bytes in `zwrite format <https://docs.yottadb.com/MultiLangProgGuide/programmingnotes.html#zwrite-format>`_.

As this function is intended for debugging and provides details of
internal structures, its output is likely to change as internal
implementations change, even when stability of the external API is
maintained.

Go BufferT Free()
.................

.. code-block:: go

        func (buft *BufferT) Free()

The inverse of the :code:`Alloc()` method: release the buffer in
YottaDB heap space referenced by the :code:`C.ydb_buffer_t` structure,
release the :code:`C.ydb_buffer_t`, and set :code:`cbuft` in the
:code:`BufferT` structure to :code:`nil`.

Go BufferT LenAlloc()
.....................

.. code-block:: go

        func (buft *BufferT) LenAlloc(tptoken uint64, errstr *BufferT) (uint32, error)

- If the underlying structures
  have not yet been allocated, return the STRUCTNOTALLOCD error.
- Otherwise, return the :code:`len_alloc` field of the
  :code:`C.ydb_buffer_t` structure referenced by :code:`cbuft`.

Go BufferT LenUsed()
....................

.. code-block:: go

        func (buft *BufferT) LenUsed(tptoken uint64, errstr *BufferT) (uint32, error)

- If the underlying structures
  have not yet been allocated, return the STRUCTNOTALLOCD error.
- If the :code:`len_used` field of the :code:`C.ydb_buffer_t`
  structure is greater than its :code:`len_alloc` field (owing to a
  prior INVSTRLEN error), return an INVSTRLEN error and the
  :code:`len_used` field of the :code:`C.ydb_buffer_t` structure
  referenced by :code:`cbuft`.
- Otherwise, return the :code:`len_used` field of the
  :code:`C.ydb_buffer_t` structure referenced by :code:`cbuft`.

Go BufferT SetLenUsed()
.......................

.. code-block:: go

        func (buft *BufferT) SetLenUsed(tptoken uint64, errstr *BufferT, newLen uint32) error

Use this method to change the length of a used substring of the
contents of the buffer referenced by the :code:`buf_addr` field of the
referenced :code:`C.ydb_buffer_t`.

- If the underlying structures
  have not yet been allocated, return the STRUCTNOTALLOCD error.
- If :code:`newLen` is greater than the :code:`len_alloc` field of the
  referenced :code:`C.ydb_buffer_t`, make no changes and return with
  an error return of INVSTRLEN.
- Otherwise, set the :code:`len_used` field of the referenced
  :code:`C.ydb_buffer_t` to :code:`newLen`.

Note that even if :code:`newLen` is not greater than the value of
:code:`len_alloc`, setting a :code:`len_used` value greater than the
number of meaningful bytes in the buffer will likely lead to
hard-to-debug errors.

Go BufferT SetValBAry()
.......................

.. code-block:: go

        func (buft *BufferT) SetValBAry(tptoken uint64, errstr *BufferT, value []byte) error

- If the underlying structures
  have not yet been allocated, return the STRUCTNOTALLOCD error.
- If the length of :code:`val` is greater than the :code:`len_alloc`
  field of the :code:`C.ydb_buffer_t` structure referenced by
  :code:`cbuft`, make no changes and return INVSTRLEN.
- Otherwise, copy the bytes of :code:`val` to the referenced buffer
  and set the :code:`len_used` field to the length of
  :code:`val`.

Go BufferT SetValStr()
......................

.. code-block:: go

        func (buft *BufferT) SetValStr(tptoken uint64, errstr *BufferT, value string) error

- If the underlying structures
  have not yet been allocated, return the STRUCTNOTALLOCD error.
- If the length of :code:`val` is greater than the :code:`len_alloc`
  field of the :code:`C.ydb_buffer_t` structure referenced by
  :code:`cbuft`, make no changes and return INVSTRLEN.
- Otherwise, copy the bytes of :code:`val` to the referenced buffer
  and set the :code:`len_used` field to the length of
  :code:`val`.

Go BufferT ValBAry()
....................

.. code-block:: go

        func (buft *BufferT) ValBAry(tptoken uint64, errstr *BufferT) ([]byte, error)

- If the the underlying structures
  have not yet been allocated, return the STRUCTNOTALLOCD error.
- If the :code:`len_used` field of the :code:`C.ydb_buffer_t` structure
  is greater than its :code:`len_alloc` field (owing to a prior
  INVSTRLEN error), return an INVSTRLEN error and :code:`len_alloc`
  bytes as a byte array.
- Otherwise, return :code:`len_used` bytes of the buffer as a byte
  array.

Go BufferT ValStr()
...................

.. code-block:: go

        func (buft *BufferT) ValStr(tptoken uint64, errstr *BufferT) (string, error)

- If the the underlying structures
  have not yet been allocated, return the STRUCTNOTALLOCD error.
- If the :code:`len_used` field of the :code:`C.ydb_buffer_t` structure
  is greater than its :code:`len_alloc` field (owing to a prior
  INVSTRLEN error), return an INVSTRLEN error and :code:`len_alloc`
  bytes as a string.
- Otherwise, return :code:`len_used` bytes of the buffer as a string.

Go Simple API Access Methods for BufferTArray
---------------------------------------------

Go BufferTArray Alloc()
.......................

.. code-block:: go

        func (buftary *BufferTArray) Alloc(numBufs, nBytes uint32)

Allocate an array of :code:`numSubs` buffers in YottaDB heap space, each of of size
:code:`bufSiz`, referenced by the :code:`BufferTArray` structure.

Go BufferTArray Dump()
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
    address :code:`buf_addr`, in `zwrite format <https://docs.yottadb.com/MultiLangProgGuide/programmingnotes.html#zwrite-format>`_.

As this function is intended for debugging and provides details of
internal structures, its output is likely to change as internal
implementations change, even when stability of the external API is
maintained.

Go BufferTArray DumpToWriter()
...............................

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
    address :code:`buf_addr`, in `zwrite format <https://docs.yottadb.com/MultiLangProgGuide/programmingnotes.html#zwrite-format>`_.

As this function is intended for debugging and provides details of
internal structures, its output is likely to change as internal
implementations change, even when stability of the external API is
maintained.

Go BufferTArray ElemAlloc()
...........................

.. code-block:: go

        func (buftary *BufferTArray) ElemAlloc() uint32

- If the underlying structures
  have not yet been allocated, return the STRUCTNOTALLOCD error.
- Otherwise, return the number of allocated buffers.

Go BufferTArray ElemLenAlloc()
..............................

.. code-block:: go

        func (buftary *BufferTArray) ElemLenAlloc(tptoken uint64) uint32

- If the underlying structures
  have not yet been allocated, return the STRUCTNOTALLOCD error.
- Otherwise, return the :code:`len_alloc` from the
  :code:`C.ydb_buffer_t` structures referenced by :code:`cbuftary`,
  all of which have the same value.

Go BufferTArray ElemLenUsed()
.............................

.. code-block:: go

        func (buftary *BufferTArray) ElemLenUsed(tptoken uint64, errstr *BufferT, idx uint32) (uint32, error)

- If the underlying structures
  have not yet been allocated, return the STRUCTNOTALLOCD error.
- If :code:`idx` is greater than or equal to the :code:`elemsAlloc` of the
  :code:`BufferTArray` structure, return with an error return of
  INSUFFSUBS.
- Otherwise, return the :code:`len_used` field of the array element
  specifed by :code:`idx` of the :code:`C.ydb_buffer_t` array referenced
  by :code:`cbuftary`.

Go BufferTArray ElemUsed()
..........................

.. code-block:: go

        func (buftary *BufferTArray) ElemUsed() uint32

- If the underlying structures
  have not yet been allocated, return the STRUCTNOTALLOCD error.
- Otherwise, return the value of the :code:`elemsUsed` field.

Go BufferTArray Free()
......................

.. code-block:: go

        func (buftary *BufferTArray) Free()

The inverse of the :code:`Alloc()` method: release the :code:`numSubs`
buffers and the :code:`C.ydb_buffer_t` array. Set :code:`cbuftary` to
:code:`nil`, and :code:`elemsAlloc` and :code:`elemsUsed` to zero.

Go BufferTArray SetElemLenUsed()
................................

.. code-block:: go

        func (buftary *BufferTArray) SetElemLenUsed(tptoken uint64,
		errstr *BufferT, idx, newLen uint32) error

Use this method to set the number of bytes in :code:`C.ydb_buffer_t`
structure referenced by :code:`cbuft` of the array element specified
by :code:`idx`, for example to change the length of a used substring
of the contents of the buffer referenced by the :code:`buf_addr` field
of the referenced :code:`C.ydb_buffer_t`.

- If the underlying structures
  have not yet been allocated, return the STRUCTNOTALLOCD error.
- If :code:`idx` is greater than or equal to :code:`elemsAlloc`, make no changes
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

Go BufferTArray SetElemUsed()
.............................

.. code-block:: go

        func (buftary *BufferTArray) SetElemUsed(tptoken uint64, errstr *BufferT, newUsed uint32) error

Use this method to set the current number of valid strings (subscripts
or variable names) in the :code:`BufferTArray`.

- If the underlying structures
  have not yet been allocated, return the STRUCTNOTALLOCD error.
- If :code:`newUsed` is greater than :code:`elemsAlloc`, make no
  changes and return with an error return of
  INSUFFSUBS.
- Otherwise, set :code:`elemsUsed` to :code:`newUsed`.

Note that even if :code:`newUsed` is not greater than the value of
:code:`elemsAlloc`, using an :code:`elemsUsed` value greater than the
number of valid values in the array will likely lead to hard-to-debug
errors.

Go BufferTArray SetValBAry()
............................

.. code-block:: go

        func (buftary *BufferTArray) SetValBAry(tptoken uint64, errstr *BufferT, idx uint32, value []byte) error

- If the underlying structures
  have not yet been allocated, return the STRUCTNOTALLOCD error.
- If :code:`idx` is greater than or equal to :code:`elemsAlloc` make no changes
  and return with an error return of INSUFFSUBS.
- If the length of :code:`val` is greater than the
  :code:`len_alloc` field of the array element specified by :code:`idx`,
  make no changes, and return INVSTRLEN.
- Otherwise, copy the bytes of :code:`val` to the referenced buffer
  and set the :code:`len_used` field to the length of
  :code:`val`.

Go BufferTArray SetValStr()
...........................

.. code-block:: go

        func (buftary *BufferTArray) SetValStr(tptoken uint64, errstr *BufferT, idx uint32, value string) error

- If the underlying structures
  have not yet been allocated, return the STRUCTNOTALLOCD error.
- If :code:`idx` is greater than or equal to :code:`elemsAlloc` make no changes
  and return with an error return of INSUFFSUBS.
- If the length of :code:`val` is greater than the
  :code:`len_alloc` field of the array element specified by :code:`idx`,
  make no changes, and return INVSTRLEN.
- Otherwise, copy the bytes of :code:`val` to the referenced buffer
  and set the :code:`len_used` field to the length of
  :code:`val`.

Go BufferTArray ValBAry()
.........................

.. code-block:: go

        func (buftary *BufferTArray) ValBAry(tptoken uint64, errstr *BufferT, idx uint32) ([]byte, error)

- If the underlying structures
  have not yet been allocated, return the STRUCTNOTALLOCD error.
- If :code:`idx` is greater than or equal to :code:`elemsAlloc`, return a zero
  length byte array and an error return of INSUFFSUBS.
- If the :code:`len_used` field of the :code:`C.ydb_buffer_t`
  structure specified by :code:`idx` is greater than its
  :code:`len_alloc` field (owing to a previous INVSTRLEN error),
  return a byte array containing the :code:`len_alloc` bytes at
  :code:`buf_addr` and an INVSTRLEN error.
- Otherwise, return a byte array containing the :code:`len_used` bytes
  at :code:`buf_addr`.

Go BufferTArray ValStr()
........................

.. code-block:: go

        func (buftary *BufferTArray) ValStr(tptoken uint64, errstr *BufferT, idx uint32) (string, error)

- If the underlying structures
  have not yet been allocated, return the STRUCTNOTALLOCD error.
- If :code:`idx` is greater than or equal to :code:`elemsAlloc`, return a zero
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
methods on :code:`SubAry`, the `Go KeyT Alloc()`_, `Go KeyT Dump()`_
and `Go KeyT Free()`_ methods are available for programming
convenience.

Go KeyT Alloc()
...............

.. code-block:: go

        func (key *KeyT) Alloc(varSiz, numSubs, subSiz uint32)

Invoke :code:`Varnm.Alloc(varSiz)` (see `Go BufferT Alloc()`_) and
:code:`SubAry.Alloc(numSubs, subSiz)` (see `Go BufferTArray
Alloc()`_).

Go KeyT Dump()
..............

.. code-block:: go

        func (key *KeyT) Dump()

Invoke :code:`Varnm.Dump()` (see `Go BufferT Dump()`_) and
:code:`SubAry.Dump()` (see `Go BufferTArray Dump()`_).

Go KeyT DumpToWriter()
......................

.. code-block:: go

        func (key *KeyT) DumpToWriter(writer io.writer)

Invoke :code:`Varnm.Dump()` (see `Go BufferT Dump()`_) and
:code:`SubAry.Dump()` (see `Go BufferTArray Dump()`_), sending the
output to :code:`writer`.

Go KeyT Free()
..............

.. code-block:: go

        func (key *KeyT) Free()

Invoke :code:`Varnm.Free()` (see `Go BufferT Free()`_) and
:code:`SubAry.Free()` (see `Go BufferTArray Free()`_).

-----------------------------
Go Simple API BufferT Methods
-----------------------------

Go Str2ZwrST()
--------------

.. code-block:: go

        func (buft *BufferT) Str2ZwrST(tptoken uint64, errstr *BufferT, zwr *BufferT) error

The method wraps `ydb_str2zwr_st() <https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#ydb-str2zwr-s-ydb-str2zwr-st>`_ to provide the string in `zwrite
format <https://docs.yottadb.com/MultiLangProgGuide/programmingnotes.html#zwrite-format>`_.

- If the underlying structures
  have not yet been allocated, return the STRUCTNOTALLOCD error.
- If :code:`len_alloc` is not large enough, set :code:`len_used` to
  the required length, and return an INVSTRLEN error. In this case,
  :code:`len_used` will be greater than :code:`len_alloc` until
  corrected by application code, and the value referenced by
  :code:`zwr` is unspecified.
- Otherwise, set the buffer referenced by :code:`buf_addr` to the
  `zwrite format <https://docs.yottadb.com/MultiLangProgGuide/programmingnotes.html#zwrite-format>`_ string, and set :code:`len_used` to the length.

Note that the length of a string in `zwrite format <https://docs.yottadb.com/MultiLangProgGuide/programmingnotes.html#zwrite-format>`_ is always greater
than or equal to the string in its original, unencoded format.

Go Zwr2StrST()
--------------

.. code-block:: go

        func (buft *BufferT) Zwr2StrST(tptoken uint64, errstr *BufferT, str *BufferT) error

This method wraps `ydb_zwr2str_st() <https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#ydb-zwr2str-s-ydb-zwr2str-st>`_ and is the inverse of `Go Str2ZwrST()`_.

- If the underlying structures
  have not yet been allocated, return the STRUCTNOTALLOCD error.
- If :code:`len_alloc` is not large enough, set :code:`len_used` to
  the required length, and return an INVSTRLEN error. In this case,
  :code:`len_used` will be greater than :code:`len_alloc` until
  corrected by application code.
- If :code:`str` has errors and is not in valid `zwrite format <https://docs.yottadb.com/MultiLangProgGuide/programmingnotes.html#zwrite-format>`_, set
  :code:`len_used` to zero, and return the error code returned by
  `ydb_zwr2str_s() <https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#ydb-zwr2str-s-ydb-zwr2str-st>`_ e.g., INVZWRITECHAR`.
- Otherwise, set the buffer referenced by :code:`buf_addr` to the
  unencoded string, set :code:`len_used` to the length.

----------------------------------
Go Simple API BufferTArray Methods
----------------------------------

Go DeleteExclST()
-----------------

.. code-block:: go

        func (buftary *BufferTArray) DeleteExclST(tptoken uint64, errstr *BufferT) error

:code:`DeleteExclST()` wraps `ydb_delete_excl_st() <https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#ydb-delete-excl-s-ydb-delete-excl-st>`_ to delete all
local variable trees except those of local variables whose names are
specified in the :code:`BufferTArray` structure. In the special case
where :code:`elemsUsed` is zero, the method deletes all local variable
trees.

In the event that the :code:`elemsUsed` exceeds
:code:`yottadb.YDB_MAX_NAMES`, the error return is ERRNAMECOUNT2HI.

As mixing M and Go application code in the same process is not supported, the
warning in `ydb_delete_excl_s() <https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#ydb-delete-excl-s-ydb-delete-excl-st>`_ does not apply.

Go TpST()
---------

.. code-block:: go

        func (buftary *BufferTArray) TpST(tptoken uint64, errstr *BufferT,
		tpfn func(uint64, *BufferT) int, transid string) error

:code:`TpST()` wraps `ydb_tp_st() <https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#ydb-tp-s-ydb-tp-st>`_ to implement `Transaction
Processing <https://docs.yottadb.com/MultiLangProgGuide/MultiLangProgGuide.html#transaction-processing>`_. :code:`tpfn` is a  function with two
parameters, the first of which is a :code:`tptoken` and the second is
an :code:`errstr`.

As an alternative to parameters for :code:`tpfn`, create closures.

A function implementing logic for a transaction should return
:code:`int` with one of the following:

- A normal return (:code:`YDB_OK`) to indicate that per application
  logic, the transaction can be committed. The YottaDB database engine
  will commit the transaction if it is able to, as discussed in
  `Transaction Processing <https://docs.yottadb.com/MultiLangProgGuide/MultiLangProgGuide.html#transaction-processing>`_, and if not, will call the function again.
- :code:`YDB_TP_RESTART` to indicate that the transaction should restart, either
  because application logic has so determined or because a YottaDB
  function called by the function has returned TPRESTART.
- :code:`YDB_TP_ROLLBACK` to indicate that :code:`TpST()` should not commit the
  transaction, and should return ROLLBACK to the caller.

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
Isolation), as discussed under `ydb_tp_st() <https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#ydb-tp-s-ydb-tp-st>`_.

Please see both the description of `ydb_tp_st() <https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#ydb-tp-s-ydb-tp-st>`_
and the sections on `Transaction Processing <https://docs.yottadb.com/MultiLangProgGuide/MultiLangProgGuide.html#transaction-processing>`_ and `Threads and
Transaction Processing <https://docs.yottadb.com/MultiLangProgGuide/programmingnotes.html#threads-and-transaction-processing>`_ for details.

.. note:: If the transaction logic receives a :code:`YDB_TP_RESTART` or :code:`YDB_TP_ROLLBACK` from a YottaDB function or method that it calls, it *must* return that value to the calling :code:`TpE()` or :code:`TpST()`. Failure to do so could result in application level data inconsistencies and hard to debug application code.

--------------------------
Go Simple API KeyT Methods
--------------------------

:code:`KeyT` methods return errors returned by methods that invoke the
underlying :code:`Varnm` and :code:`SubAry` members of :code:`KeyT`
structures.

Go DataST()
-----------

.. code-block:: go

        func (key *KeyT) DataST(tptoken uint64, errstr *BufferT) (uint32, error)

Matching `Go DataE()`_, :code:`DataST()` returns the
result of `ydb_data_st()`_ (0, 1, 10, or 11). In the event of an error, the return
value is unspecified.

Go DeleteST()
-------------

.. code-block:: go

        func (key *KeyT) DeleteS(tptoken uint64, errstr *BufferT, deltype int) error

Matching `Go DeleteE()`_, :code:`DeleteST()` wraps `ydb_delete_st() <https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#ydb-delete-s-ydb-delete-st>`_ to
delete a local or global variable node or (sub)tree, with a value of
:code:`yottadb.YDB_DEL_NODE` for :code:`deltype` specifying that only the
node should be deleted, leaving the (sub)tree untouched, and a value
of :code:`yottadb.YDB_DEL_TREE` specifying that the node as well as the
(sub)tree are to be deleted.

Go IncrST()
-----------

.. code-block:: go

        func (key *KeyT) IncrST(tptoken uint64, errstr *BufferT, incr, retval *BufferT) error

Matching `Go IncrE()`_, :code:`IncrST()` wraps `ydb_incr_st() <https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#ydb-incr-s-ydb-incr-st>`_ to
atomically increment the referenced global or local variable node
coerced to a number, with :code:`incr` coerced to a number. It stores
the result in the node and also returns it through the :code:`BufferT`
structure referenced by :code:`retval`.

- If `ydb_incr_st() <https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#ydb-incr-s-ydb-incr-st>`_ returns an error such as NUMOFLOW, the
  method makes no changes to the structures under :code:`retval` and
  returns the error.
- If the length of the data to be returned exceeds
  :code:`retval.lenAlloc`, the method sets the :code:`len_used`
  of the :code:`C.ydb_buffer_t` referenced by :code:`retval`
  to the required length, and returns an INVSTRLEN error. The value
  referenced by :code:`retval` is unspecified.
- Otherwise, it copies the data to the buffer referenced by the
  :code:`retval.buf_addr`, sets :code:`retval.lenUsed` to its
  length.

With a :code:`nil` value for :code:`incr`, the default increment
is 1. Note that the value of the empty string coerced to an integer is
zero, but 1 is a more useful default value for an omitted parameter in
this case.

Go LockDecrST()
---------------

.. code-block:: go

        func (key *KeyT) LockDecrS(tptoken uint64,
                errstr *BufferT) error

Matching `Go LockDecrE()`_ :code:`LockDecrST()` wraps
`ydb_lock_decr_st() <https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#ydb-lock-decr-s-ydb-lock-decr-st>`_ to decrement the count of the lock name
referenced, releasing it if the count goes to zero or ignoring the
invocation if the process does not hold the lock.

Go LockIncrST()
---------------

.. code-block:: go

        func (key *KeyT) LockIncrST(tptoken uint64,
                errstr *BufferT, timeoutNsec uint64) error

Matching `Go LockIncrE()`_, :code:`LockIncrST()` wraps
`ydb_lock_incr_st() <https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#ydb-lock-incr-s-ydb-lock-incr-st>`_ to attempt to acquire the referenced lock
resource name without releasing any locks the process already holds.

- If the process already holds the named lock resource, the method
  increments its count and returns.
- If :code:`timeoutNsec` exceeds :code:`yottadb.YDB_MAX_TIME_NSEC`, the
  method returns with an error return TIME2LONG.
- If it is able to aquire the lock resource within :code:`timeoutNsec`
  nanoseconds, it returns holding the lock, otherwise it returns
  LOCK_TIMEOUT. If :code:`timeoutNsec` is zero, the method makes
  exactly one attempt to acquire the lock.

Go NodeNextST()
---------------

.. code-block:: go

        func (key *KeyT) NodeNextST(tptoken uint64,
                errstr *BufferT, next *BufferTArray) error

Matching `Go NodeNextE()`_, :code:`NodeNextST()` wraps
`ydb_node_next_st() <https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#ydb-node-next-s-ydb-node-next-st>`_ to facilitate depth first traversal of a local or
global variable tree. A node or subtree does not have to exist at the
specified key.

- If there is a subsequent node:

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

- If there is no subsequent node, the method returns the NODEEND error
  (:code:`yottadb.YDB_ERR_NODEEND`), making no changes to the
  structures below :code:`next`.

Go NodePrevST()
---------------

.. code-block:: go

        func (key *KEyT) NodePrevST(tptoken uint64,
                errstr *BufferT, prev *BufferTArray) error

Matching `Go NodePrevE()`_, :code:`NodePrevST()` wraps
`ydb_node_previous_st() <https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#ydb-node-previous-s-ydb-node-previous-st>`_ to facilitate reverse depth first traversal
of a local or global variable tree. A node or subtree does not have to exist at the
specified key.

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

- If there is no previous node, the method returns the NODEEND error
  making no changes to the structures below :code:`prev`.


Go SetValST()
-------------

.. code-block:: go

        func (key *KeyT) SetST(tptoken uint64,
                errstr *BufferT, value *BufferT) error

Matching `Go SetValE()`_, at the referenced local or global variable
node, or the intrinsic special variable, :code:`SetValST()` wraps
`ydb_set_st() <https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#ydb-set-s-ydb-set-st>`_ to set the value specified by :code:`value`.

Go SubNextST()
--------------

.. code-block:: go

        func (key *KeyT) SubNextST(tptoken uint64,
                errstr *BufferT, retval *BufferT) error

Matching `Go SubNextE()`_, :code:`SubNextST()` wraps
`ydb_subscript_next_st() <https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#ydb-subscript-next-s-ydb-subscript-next-st>`_ to facilitate breadth-first traversal of a
local or global variable sub-tree. A node or subtree does not have to exist at the
specified key.

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

Go SubPrevST()
--------------

.. code-block:: go

        func (key *KeyT) SubPrevST(tptoken uint64,
                errstr *BufferT, retval *BufferT) error

Matching `Go SubPrevE()`_,
:code:`SubPrevST()` wraps `ydb_subscript_previous_st() <https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#ydb-subscript-previous-s-ydb-subscript-previous-st>`_ to facilitate
reverse breadth-first traversal of a local or global variable
sub-tree. A node or subtree does not have to exist at the specified
key.

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

Go ValST()
----------

.. code-block:: go

        func (key *KeyT) ValST(tptoken uint64,
                errstr *BufferT, retval *BufferT) error

Matching `Go ValE()`_, :code:`ValST()` wraps `ydb_get_st()`_ to return
the value at the referenced global or local variable node, or
intrinsic special variable, in the buffer referenced by the
:code:`BufferT` structure referenced by :code:`retval`.

- If `ydb_get_st()`_ returns an error such as GVUNDEF, INVSVN, LVUNDEF,
  the method makes no changes to the structures under :code:`retval`
  and returns the error.
- If the length of the data to be returned exceeds
  :code:`retval.GetLenAlloc()`, the method sets the :code:`len_used` of
  the :code:`C.ydb_buffer_t` referenced by :code:`retval` to the
  required length, and returns an INVSTRLEN error.
- Otherwise, it copies the data to the buffer referenced by the
  :code:`retval.buf_addr`, and sets :code:`retval.lenUsed` to the
  length of the returned value.

-----------------------
Go Simple API Functions
-----------------------

Go LockST()
-----------

.. code-block:: go

        func LockST(tptoken uint64, errstr *BufferT,
                timeoutNsec uint64, lockname ... *KeyT) error

Matching `Go LockE()`_, :code:`LockST()` wraps `ydb_lock_st() <https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#ydb-lock-s-ydb-lock-st>`_ to
release all lock resources currently held and then attempt to acquire
the named lock resources referenced. If no lock resources are
specified, it simply releases all lock resources currently held and
returns.

If lock resources are specified, upon return, the process will have
acquired all of the named lock resources or none of the named lock
resources.

- If :code:`timeoutNsec` exceeds :code:`yottadb.YDB_MAX_TIME_NSEC`, the
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
`Comprehensive API <https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#comprehensive-api>`_

Go Utility API
==============

The Go Utility API contains functions and one method for the
:code:`CallMTable` structure.

-----------
Go CallMT()
-----------

.. code-block:: go

        func CallMT(tptoken uint64, errstr *BufferT, retvallen uint32, rtnname string, rtnargs ...interface{}) (string, error)

As a wrapper for the C function `ydb_ci_t() <https://docs.yottadb.com/ProgrammersGuide/extrout.html#ydb-ci-t>`_, the :code:`CallMT()` function is used to call M routines from Go,
used when a single call to the function is anticipated. :code:`CallMT()` only supports read-only parameters.

- :code:`retvallen` needs to be of sufficient size to hold any value returned by the call. If the output value exceeds the buffer size,
  a SIG-11 failure is likely as it will overwrite adjacently allocated memory, damaging storage management headers.

- If a return value is specified but has not been configured in the call-in descriptor file or vice-versa, a parameter mismatch situation is created.

- :code:`rtnargs` refers to a list of 0 or more arguments passed to
  the called routine. As all arguments are passed as strings after
  conversion by fmt.Sprintf("%v", parm), any argument that can be so
  converted can be used here. Any error returned by fmt.Sprintf() is
  returned as an error by :code:`CallMT()`. Note that passing an array
  will generate a string containing an entire array, which may be
  unexpected. The number of parameters possible is restricted to 34
  (for 64-bit systems) or 33 (for 32-bit systems).

Example:

.. parsed-literal::
   fmt.Println("Golang: Invoking HelloWorld")
      retval, err := yottadb.CallMT(yottadb.NOTTP, nil, 1024, "HelloWorld", "English", "USA")
      if nil != err {
      	panic(fmt.Sprintf("CallMT() call failed: %s", err))
      }
      fmt.Println("Golang: retval =", retval)

The HelloWorld program in the example returns a "HelloWorld" string in a language "English" and a location "USA" specified in the two parameters. :code:`retvallen` is set to be 1024 bytes.

Note that a call-in table is required when calling from Go into M. A call-in table can be specified at process startup with the environment variable :code:`ydb_ci` or using the functions `Go CallMTableOpenT()`_ and `Go CallMTable.CallMTableSwitchT()`_.

-------------------------
Go CallMDesc.CallMDescT()
-------------------------

.. code-block:: go

        func (mdesc *CallMDesc) CallMDescT(tptoken uint64, errstr *BufferT, retvallen uint32, rtnargs ...interface{}) (string, error)

As a wrapper for the C function `ydb_cip_t() <https://docs.yottadb.com/ProgrammersGuide/extrout.html#ydb-cip-t>`_, the :code:`CallMDescT()` is a
method of the :code:`CallMDesc` (call descriptor) structure which, during the first call, saves information in the :code:`CallMDesc` structure that makes all following calls
using the same descriptor structure able to run much faster by bypassing a lookup of the function name and going straight to the M routine being called.
:code:`CallMDescT()` only supports read-only parameters.

- :code:`CallMDescT()` requires a :code:`CallMDesc` structure to have been created and initialized with the :code:`SetRtnName()` method.

Example:

.. parsed-literal::
   var mrtn yottadb.CallMDesc
   fmt.Println("Golang: Invoking HelloWorld")
   mrtn.SetRtnName("HelloWorld")
   retval, err := mrtn.CallMDescT(yottadb.NOTTP, nil, 1024, "English", "USA")
   if nil != err { panic(fmt.Sprintf("CallMDescT() call failed: %s", err)) }
   fmt.Println("Golang: retval =", retval)

-------------------
Go CallMDesc.Free()
-------------------

.. code-block:: go

	func (mdesc *CallMDesc) Free()

Frees a :code:`CallMDesc` structure previously allocated

-------------------------
Go CallMDesc.SetRtnName()
-------------------------

.. code-block:: go

	func (mdesc *CallMDesc) SetRtnName(rtnname string)

Allocates and initializes a structure to cache information to
accelarate Go to M calls made by `Go
CallMDesc.CallMDescT()`_. :code:`rtnname` is a :code:`<c-call-name>`
in a `Call-in table
<https://docs.yottadb.com/ProgrammersGuide/extrout.html#relevant-files-for-call-ins>`_. The
structure can then be used by the `Go CallMDesc.CallMDescT()`_ method
to call an M function. YottaDB looks for :code:`rtnname` in the
current call-in table.

--------------------
Go CallMTableOpenT()
--------------------

.. code-block:: go

	func CallMTableOpenT(tptoken uint64, errstr *BufferT, tablename string) (*CallMTable, error)

If the environment variable :code:`ydb_ci` does not specify an `M code
call-in table
<https://docs.yottadb.com/ProgrammersGuide/extrout.html#calls-from-external-routines-call-ins>`_
at process startup, function :code:`CallMTableOpen()` can be used to
open an initial call-in table. :code:`tablename` is the filename of a
call-in table, and the function opens the file and initializes a
:code:`CallMTable` structure. Processes use the `zroutines intrinsic
special variable
<https://docs.yottadb.com/ProgrammersGuide/isv.html#zroutines>`_
intrinsic special variable to locate M routines to execute, and
:code:`$zroutines` is initialized at process startup from the
:code:`ydb_routines` environment variable.

---------------------------------
Go CallMTable.CallMTableSwitchT()
---------------------------------

.. code-block:: go

	func (newcmtable *CallMTable) CallMTableSwitchT(tptoken uint64, errstr *BufferT) (*CallMTable, error)

Method :code:`CallMTableSwitchT()` enables switching of call-in
tables. :code:`newcmtable` is the new call-in table to be used, which
should have previously been opened with `Go
CallMTableOpenT()`_. :code:`*CallMTable` returns the previously open
call-in table, :code:`*nil` if there was none. As switching the
call-in table does not change :code:`$zroutines`, if the new call-in
table requires a different M routine search path, code will need to
change :code:`$zroutines` appropriately.

----------------
Go Error()
----------------

.. code-block:: go

        func (err *YDBError) Error() string

:code:`Error()` is a method to return the expected error message string.

---------------
Go ErrorCode()
---------------

.. code-block:: go

        func ErrorCode(err error) int

:code:`ErrorCode()` is a function used to find the error return code.

---------
Go Exit()
---------

.. code-block:: go

        func Exit() error

For a process that wishes to close YottaDB databases and no longer use
YottaDB, the function wraps `ydb_exit() <https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#ydb-exit>`_.
If :code:`ydb_exit()` does not send a return value of :code:`YDB_OK`, :code:`Exit()` panics.

Although in theory typical processes should not need to call
:code:`Exit()` because normal process termination should close
databases cleanly, in practice, thread shutdown may not always ensure
that databases are closed cleanly, especially since the C :code:`atexit()` functionality does not reliably work in
Go's multi-threaded environment. Application code should invoke :code:`Exit()` prior to process exit, or when an application intends
to continue with other work beyond use of YottaDB, to ensure that databases are closed cleanly. To accomplish this, you should use a
"defer yottadb.Exit()" statement early in the main routine's initialization.

-------------------
Go IsLittleEndian()
-------------------

.. code-block:: go

        func IsLittleEndian() bool

The function returns :code:`true` if the underlying computing infrastructure
is little endian and :code:`false` otherwise.

-------------
Go MessageT()
-------------

.. code-block:: go

        func Message(tptoken uint64, errstr *BufferT,
                status int) (string, error)

:code:`MessageT()` returns the text template for the error number
specified by :code:`status`.

- If :code:`status` does not correspond to an error that YottaDB
  recognizes, it returns the error UNKNOWNSYSERR.
- Otherwise, it returns the error message text template for the error
  number specified by :code:`status`.

---------------
Go NewError()
---------------

.. code-block:: go

        func NewError(tptoken uint64, errstr *BufferT, errnum int) error

:code:`NewError()` is a function to create a new YDBError from :code:`errstr` and :code:`errnum`, setting the two private fields in the returned YDBError to the provided values.

-------------
Go ReleaseT()
-------------

.. code-block:: go

        func ReleaseT(tptoken uint64, errstr *BufferT) string

Returns a string consisting of six space separated pieces to provide
version information for the Go wrapper and underlying YottaDB release:

- The first piece is always “gowr” to idenfify the Go wrapper.
- The Go wrapper release number, which starts with “v” and is followed
  by three numbers separated by a period (“.”), e.g., “v0.90.0”
  mimicking `Semantic Versioning <https://semver.org/>`_. The first
  is a major release number, the second is a minor release number
  under the major release and the third is a patch level. Even minor
  and patch release numbers indicate
  formally released software. Odd minor release numbers indicate
  software builds from “in flight” code under development, between
  releases. Note that although they follow the same format, Go wrapper
  release numbers are different from the release numbers of the
  underlying YottaDB release as reported by `$zyrelease <https://docs.yottadb.com/MultiLangProgGuide/MultiLangProgGuide.html#zyrelease>`_.
- The third through sixth pieces are `$zyrelease <https://docs.yottadb.com/MultiLangProgGuide/MultiLangProgGuide.html#zyrelease>`_ from the underlying
  YottaDB release.

Go Programming Notes
====================

These `Go Programming Notes`_ supplement rather than supplant the more
general `Programming Notes <https://docs.yottadb.com/MultiLangProgGuide/programmingnotes.html>`_ for C.

----------
Goroutines
----------

In order to avoid restricting Go applications to calling the
single-threaded YottaDB engine from a single goroutine (which would be
unnatural to a Go programmer), the YottaDB Go wrapper calls the
functions of the C `Simple API <https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#simple-api>`_ that support multi-threaded
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
Goroutine, refer to the `Threads <https://docs.yottadb.com/MultiLangProgGuide/programmingnotes.html#threads>`_ discussion about the need for
applications to avoid race conditions when accessing YottaDB
resources.
