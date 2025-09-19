.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2019-2026 YottaDB LLC and/or its subsidiaries.#
.. # All rights reserved.                                        #
.. #                                                             #
.. #     This document contains the intellectual property        #
.. #     of its copyright holder(s), and is made available       #
.. #     under a license.  If you do not know the terms of       #
.. #     the license, please stop and do not read further.       #
.. #                                                             #
.. ###############################################################

.. raw:: html

    <img referrerpolicy="no-referrer-when-downgrade" src="https://download.yottadb.com/goprogram.png" />

================================
Programming in Go
================================

Refer to `YDBGo v2 <https://pkg.go.dev/lang.yottadb.com/go/yottadb/v2>`_ for official v2 package documentation. The following is for YDBGo v1.

.. contents::
   :depth: 5

There are two wrappers for programming YottaDB in `Go <https://golang.org/>`_, `mg_go <https://github.com/chrisemunt/mg_go>`_ and the YottaDB Go wrapper (described below). `mg_go <https://github.com/chrisemunt/mg_go>`_ is developed by `Chris Munt <https://github.com/chrisemunt/>`_ of `MGateway Ltd <https://www.mgateway.com/>`_. We would like to acknowledge his contribution and thank Chris for the value he adds to the YottaDB community.

`mg_go <https://github.com/chrisemunt/mg_go>`_ provides the ability to access YottaDB locally as well as remotely over a network.

The documentation below is specific to the YottaDB Go wrapper. Please use the link to `mg_go <https://github.com/chrisemunt/mg_go>`_ to access its documentation.

The YottaDB Go wrapper wraps the :ref:`c-simple-api` threaded functions and uses `cgo <https://golang.org/cmd/cgo/>`_ to provide a "yottadb" package for access from Go application code. The wrapper must be installed on a system after YottaDB is installed. There are two Go APIs:

- :ref:`go-easy-api` aims to be a straightforward, easy-to-use API to access YottaDB without limiting the functionality of YottaDB. The :ref:`go-easy-api` consists of :ref:`go-easy-api-funcs` that use standard Go data types and structures.
- :ref:`go-simple-api` aims to improve performance by reducing copying between Go and YottaDB heaps by defining structures :code:`BufferT`, :code:`BufferTArray`, and :code:`KeyT` which contain pointers to structures and data in the YottaDB heap. :ref:`go-simple-api` functionality is provided by Go methods where a method can meaningfully be associated with a structure, and by Go functions where there is no meaningful association with a structure.

As the Go language has important differences from C (for example, it has structures with methods but lacks macros), below are Go-specific sections of the :ref:`mlpg-quick-start`, :ref:`mlpg-concepts`, :ref:`c-sym-const`, :ref:`c-data-struct`, :ref:`c-simple-api` and :ref:`utility-funcs` sections. The sections that are specific to Go are intended to supplement, but not subsume, their C counterparts.

Go application code *must not* directly use the YottaDB C API structures and functions (those prefixed by :code:`C.` or described in the :ref:`C Simple API <c-simple-api>`) as such usage bypasses important controls, but should instead use the structures, methods and functions exposed by the YottaDB Go wrapper. :code:`C.` prefixed structures and functions are mentioned only for clarity in documentation and brevity of explanation. For example, :code:`C.ydb_buffer_t` is the C :code:`ydb_buffer_t` structure defined in  :ref:`c-data-struct`.

All subsections of the :code:`Programming in Go` section are prefixed with "Go" to ensure unique names for hyperlinking.

As Go implementations are inherently multi-threaded, where the :ref:`C Simple API <c-simple-api>` provides separate functions for use in multi-threaded applications, e.g., :ref:`ydb-get-s-st-fn`, the Go wrapper wraps the function for use in multi-threaded applications. Also, to accommodate Go's multi-threading, calls include an :ref:`errstr <errstr>` parameter to get the correct :ref:`zstatus-isv` for each call.

.. _go-quick-start:

---------------
Go Quick Start
---------------

Refer to `YDBGo v2 <https://pkg.go.dev/lang.yottadb.com/go/yottadb/v2>`_ for official v2 package documentation. The following is for YDBGo v1.

The YottaDB Go wrapper v1 requires a minimum YottaDB release of r1.30 and is tested with a minimum Go version of 1.18. If the Golang packages on your operating system are older, and the Go wrapper does not work, please obtain and install a newer Golang implementation.

The :ref:`go-quick-start` assumes that YottaDB has already been installed as described in the :ref:`mlpg-quick-start` section. After completing step 1 (*Installing YottaDB*), you can immediately start using YottaDB with a Go application that uses YottaDB. To create a Go application that uses YottaDB, see below.

.. code-block:: bash

        # Sets up a default YottaDB environment
        $ source /usr/local/etc/ydb_env_set

        # Create an empty directory and cd to it
        $ mkdir ydb-example && cd ydb-example

        # Initialize a new Go Module
        $ go mod init example.com/yottadb-example

        # Test program containing the YottaDB Go Import
        $ cat > yottadb-example.go << EOF
        package main

        import (
            "lang.yottadb.com/go/yottadb"
        )

        func main() {
            // Set global node ^hello("world")="Sawadee (hello in Thai)"
            err := yottadb.SetValE(yottadb.NOTTP, nil, "สวัสดี", "^hello", []string{"world"})
            if err != nil {
                panic(err)
            }
        }
        EOF

        # Download the YottaDB Go Module (you can also use "go mod tidy" here)
        $ go get .

        # Run the code; alternatively, you can use `go build` to compile it into an exe
        $ go run .
        $ go build && ./yottadb-example

#. As a sample program, you can download the `wordfreq.go program <https://gitlab.com/YottaDB/DB/YDBTest/-/raw/master/go/inref/wordfreq.go>`_,   with a `reference input file <https://gitlab.com/YottaDB/DB/YDBTest/-/raw/master/simpleapi/outref/wordfreq_input.txt>`_ and `corresponding reference output file <https://gitlab.com/YottaDB/DB/YDBTest/-/raw/master/simpleapi/outref/wordfreq_output.txt>`_.

#. Run your program and verify that the output matches the reference output. For example:

.. code-block:: bash

        $ source /usr/local/etc/ydb_env_set
        $ mkdir yottadb-wordfreq && cd yottadb-wordfreq
        $ go mod init example.com/yottadb-wordfreq
        $ wget https://gitlab.com/YottaDB/DB/YDBTest/raw/master/go/inref/wordfreq.go
        $ go mod tidy
        $ wget https://gitlab.com/YottaDB/DB/YDBTest/raw/master/simpleapi/outref/wordfreq_input.txt
        $ wget https://gitlab.com/YottaDB/DB/YDBTest/raw/master/simpleapi/outref/wordfreq_output.txt
        $ go build wordfreq.go
        $ ./wordfreq <wordfreq_input.txt >wordfreq_output_go.txt
        $ diff wordfreq_output_go.txt wordfreq_output.txt
        $

Note that the :code:`wordfreq.go` program randomly uses local or global variables (see :ref:`lcl-gbl-var`).

-------------
Go Concepts
-------------

As the YottaDB wrapper is distributed as a Go package, function calls to YottaDB are prefixed in Go code with :code:`yottadb.` (e.g., application code to call the :code:`ValST()` function would be written :code:`yottadb.ValST(…)`.

++++++++++++++++++++
Go Error Interface
++++++++++++++++++++

YottaDB has a comprehensive set of error return codes. Each has a unique number and a mnemonic. Thus, for example, to return an error that a buffer allocated for a return value is not large enough, YottaDB uses the INVSTRLEN error code, which has the numeric value :code:`yottadb.YDB_ERR_INVSTRLEN`. YottaDB attempts to maintain stability of the numeric values and mnemonics from release to release, to ensure applications remain compatible when the underlying YottaDB releases are upgraded. While the Go :code:`error` interface provides for a call to return an error as a string (with :code:`nil` for a successful return), applications in other languages, such as C, expect a numeric return value.

The C application code calling YottaDB functions will check the return code. If the return code is not :code:`YDB_OK`, it will access the intrinsic special variable :ref:`zstatus-isv` for more detailed information (though the :code:`errstr` parameter in a multi-threaded application). By contrast, Go application code calling YottaDB methods and functions will check the :code:`error` interface to determine whether it is :code:`nil`. This means that Go application code will never see a :code:`yottadb.YDB_OK` return.

The YottaDB Go :code:`error` interface has a structure and a method. Sample usage:

.. code-block:: go

    _, err := yottadb.ValE(yottadb.NOTTP, nil, "^hello", []string{})
   if err != nil {
       errcode := yottadb.ErrorCode(err)
    }

In the documentation:

- Error codes specific to each function are noted. However, common errors can also be returned. For example, while the `Go BufferT ValStr()`_ method can return INVSTRLEN, it can also return errors from the YottaDB engine.
- An error name such as INVSTRLEN refers to the underlying error, whether application code references the numeric value or the string.

.. _go-err-ret-codes:

~~~~~~~~~~~~~~~~~~~~~
Go Error Return Codes
~~~~~~~~~~~~~~~~~~~~~

In addition to the errors discussed in the :ref:`C Error Return Codes <err-ret-codes>` the Go wrapper has additional errors unique to it.

^^^^^^^^^^^^^
DBRNDWNBYPASS
^^^^^^^^^^^^^

The `DBRNDWNBYPASS <../MessageRecovery/errors.html#dbrndwnbypass>`_ message is sent to the syslog.

.. _invlknmpairlist:

^^^^^^^^^^^^^^^
INVLKNMPAIRLIST
^^^^^^^^^^^^^^^

INVLKNMPAIRLIST, Invalid lockname/subscript pair list (uneven number of lockname/subscript parameters)

Compile Time Error: The :code:`namesnsubs` parameter of :ref:`go-locke` is not a series of alternating :code:`string` and :code:`[]string` parameters.

Action: This is an application syntax bug. Fix the application code.

^^^^^^^^^^^^^
SIGACKTIMEOUT
^^^^^^^^^^^^^

The `SIGACKTIMEOUT <../MessageRecovery/errors.html#sigacktimeout>`_ message is sent to the syslog.

^^^^^^^^^^^^^^^
SIGGORTNTIMEOUT
^^^^^^^^^^^^^^^

The `SIGGORTNTIMEOUT <../MessageRecovery/errors.html#siggortntimeout>`_ message is sent to the syslog.

.. _structunallocd:

^^^^^^^^^^^^^^
STRUCTUNALLOCD
^^^^^^^^^^^^^^

STRUCTUNALLOCD, Structure not previously called with Alloc() method

Run Time Error: The corresponding :code:`Alloc()` method has not been executed for a structure, e.g., a :code:`BufferT`, passed to a method.

Action: This is an application logic bug. Fix the application code.

------------
Go Constants
------------

The file :code:`yottadb.go` in the Go wrapper defines a number of constants used to initialize variables that control signal handling.

- :code:`DefaultMaximumNormalExitWait` is initial value of :code:`MaximumNormalExitWait`, with a default of 60 seconds.
- :code:`DefaultMaximumPanicExitWait` is the initial value of :code:`MaximumPanicExitWait`, with a default of 3 seconds.
- :code:`DefaultMaximumSigShutDownWait` is initial value of :code:`MaximumSigShutDownWait`, with a default of 5 seconds.
- :code:`DefaultMaximumSigAckWait` is initial value of :code:`MaximumSigAckWait`, with a default if 10 seconds.

-----------------------
Go Symbolic Constants
-----------------------

`YottaDB symbolic constants <https://godoc.org/lang.yottadb.com/go/yottadb#pkg-constants>`_ are available in the YottaDB package, for example, :code:`yottadb.YDB_ERR_INVSTRLEN`.

:code:`NOTTP` (:code:`yottadb.NOTTP`) as a value for parameter :code:`tptoken` indicates to the invoked YottaDB method or function that the caller is not inside a :ref:`transaction <txn-proc>`.

.. _go-easy-api:

-------------
Go Easy API
-------------

A global or local variable node, or an intrinsic special variable, is specified using the construct :code:`varname string, subary []string`. For an intrinsic special variable, :code:`subary` must be the null array, :code:`[]string{}`, or :code:`nil`. For a global or local variable, a null array or :code:`nil` for :code:`subary` refers to the root node, the entire tree, or both, depending on the function and context.

As the :ref:`go-easy-api` involves more copying of data between the Go and YottaDB runtime systems, it requires the CPU to perform a little more work than the :ref:`go-simple-api` does. Whether or not this has a measurable impact on performance depends on the application and workload.

The length of strings (values and subscripts) in YottaDB is variable, as is the number of subscripts a local or global variable can have. The :ref:`go-simple-api` requires application code to allocate memory for buffers, raising errors when allocated memory (either size or number of buffers) is insufficient. Requiring application code using the :ref:`go-easy-api` to similarly allocate memory would be at odds with our goal of having it "just work".  Although YottaDB provides functionality to *a priori* determine the length of a value in order to allocate required memory, doing this for every call would adversely affect performance. The :ref:`go-easy-api` therefore allocates buffers initially of a size and number we believe to be reasonable. Whenever a result exceeds its allocation and returns an error, YottaDB expands the allocation transparently to the caller, and repeats the operation, remembering the expanded size for future allocations in the process.

.. _go-easy-api-funcs:

+++++++++++++++++++++++
Go Easy API Functions
+++++++++++++++++++++++

* `Go DataE()`_
* `Go DeleteE()`_
* `Go DeleteExclE()`_
* `Go IncrE()`_
* `Go LockDecrE()`_
* `Go LockE()`_
* `Go LockIncrE()`_
* `Go NodeNextE()`_
* `Go NodePrevE()`_
* `Go SetValE()`_
* `Go SubNextE()`_
* `Go SubPrevE()`_
* `Go TpE()`_
* `Go ValE()`_

.. _go-simple-api:

---------------
Go Simple API
---------------

The Go Simple API consists of :code:`Go Data Structures & Type Definitions`, :code:`Go Simple API BufferT Methods`, :code:`Go Simple API BufferTArray Methods` and :code:`Go Simple API KeyT Methods`. Each of them wraps a function in the :ref:`C Simple API <c-simple-api>` - refer to the descriptions of those functions for more detailed information. The majority of the functionality is in :code:`Go Simple API KeyT Methods`.

-------------------------------------
Go Data Structures & Type Definitions
-------------------------------------

The :code:`C.ydb_buffer_t` structure, which is the :code:`ydb_buffer_t` structure described in :ref:`c-data-struct` is used to pass values between Go application code and YottaDB. The design pattern is that the :code:`ydb_buffer_t` structures are in memory managed by YottaDB. Go structures contain pointers to the YottaDB structures so that when the Go garbage collector moves Go structures, the pointers they contain remain valid.

There are five structures for the interface between YottaDB and Go:

- :code:`BufferT` for data;
- :code:`BufferTArray` for a list of subscripts or a set of variable names;
- :code:`KeyT` for keys where a key in turn consists of a variable or lock resource name and subscripts, as discussed in :ref:`mlpg-concepts`; and
- :code:`CallMDesc` references an M routine and caches information to accelerate calls from Go to M.
- :code:`CallMTable` to reference `an M code call-in table <../ProgrammersGuide/extrout.html#calls-ext-rt-call-ins>`_.

----------------------
Go Wrapper Functions
----------------------

+++++++++++++
Go CallMT()
+++++++++++++

.. code-block:: go

        func CallMT(tptoken uint64, errstr *BufferT, retvallen uint32, rtnname string, rtnargs ...interface{}) (string, error)

As a wrapper for the C function `ydb_ci_t() <../ProgrammersGuide/extrout.html#ydb-ci-t-intf>`_, the :code:`CallMT()` function is used to call M routines from Go, used when a single call to the function is anticipated.

- :code:`retvallen` needs to be of sufficient size to hold any value returned by the call. If the output value exceeds the buffer size, a SIG-11 failure is likely as it will overwrite adjacently allocated memory, damaging storage management headers.

- If a return value is specified but has not been configured in the call-in descriptor file or vice-versa, a parameter mismatch situation is created.

- :code:`rtnargs` refers to a list of 0 or more arguments passed to the called routine. As all arguments are passed as strings after conversion by fmt.Sprintf("%v", parm), any argument that can be so converted can be used here. Any error returned by fmt.Sprintf() is returned as an error by :code:`CallMT()`. Note that passing an array will generate a string containing an entire array, which may be unexpected. The number of parameters possible is restricted to 34 (for 64-bit systems) or 33 (for 32-bit systems).

- Note that functions that are defined in the call-in table (refer `call-in table <../ProgrammersGuide/extrout.html#call-in-table>`_ in the M Programmer's Guide) that have IO (input/output) or O (output) parameters can **only** be defined as string variables (and not literals) as the wrapper will try to put the updated values back into these variables. The parameter values need to be passed by reference otherwise it will result in an error.

Example:

.. code-block:: go

   fmt.Println("Golang: Invoking HelloWorld")
      retval, err := yottadb.CallMT(yottadb.NOTTP, nil, 1024, "HelloWorld", "English", "USA")
      if nil != err {
      	panic(fmt.Sprintf("CallMT() call failed: %s", err))
      }
      fmt.Println("Golang: retval =", retval)

The HelloWorld program in the example returns a "HelloWorld" string in a language "English" and a location "USA" specified in the two parameters. :code:`retvallen` is set to be 1024 bytes.

Note that a call-in table is required when calling from Go into M. A call-in table can be specified at process startup with the environment variable :code:`ydb_ci` or using the functions `Go CallMTable CallMTableOpenT()`_ and `Go CallMTable CallMTableSwitchT()`_.

++++++++++++
Go DataE()
++++++++++++

.. code-block:: go

        func DataE(tptoken uint64, errstr *BufferT, varname string, subary []string) (uint32, error)

Matching `Go KeyT DataST()`_, :code:`DataE()` function wraps and returns the result of :ref:`ydb-data-s-st-fn` (0, 1, 10, or 11). In the event of an error, the return value is unspecified.

++++++++++++++
Go DeleteE()
++++++++++++++

.. code-block:: go

        func DeleteE(tptoken uint64, errstr *BufferT, deltype int, varname string, subary []string) error

Matching `Go KeyT DeleteST()`_, :code:`DeleteE()` wraps :ref:`ydb-delete-s-st-fn` to delete a local or global variable node or (sub)tree, with a value of :code:`yottadb.YDB_DEL_NODE` for :code:`deltype` specifying that only the node should be deleted, leaving the (sub)tree untouched, and a value of :code:`yottadb.YDB_DEL_TREE` specifying that the node as well as the (sub)tree are to be deleted.

++++++++++++++++++
Go DeleteExclE()
++++++++++++++++++

.. code-block:: go

        func DeleteExclE(tptoken uint64, errstr *BufferT, varnames []string) error

Matching `Go BufferTArray DeleteExclST()`_, :code:`DeleteExclE()` wraps :ref:`ydb-delete-excl-s-st-fn` to delete all local variables except those specified. In the event :code:`varnames` has no elements (i.e., :code:`[]string{}`), :code:`DeleteExclE()` deletes all local variables.

In the event that the number of variable names in :code:`varnames` exceeds :code:`yottadb.YDB_MAX_NAMES`, the error return is ERRNAMECOUNT2HI. Otherwise, if :ref:`ydb-delete-excl-s-st-fn` returns an error, the function returns the error.

As mixing M and Go application code in the same process is now supported, make sure you understand what (sub)trees are being deleted when you use :ref:`ydb-delete-excl-s-st-fn`.

++++++++++++++++
Go ErrorCode()
++++++++++++++++

.. code-block:: go

        func ErrorCode(err error) int

:code:`ErrorCode()` is a function used to find the error return code.

+++++++++++
Go Exit()
+++++++++++

.. code-block:: go

        func Exit() error

This function invokes YottaDB's exit handler ydb_exit() to shut down the database properly.
It MUST be called prior to process termination by any application that modifies the database.
This is necessary particularly in Go because Go does not call the C atexit() handler (unless building with certain test options),
so YottaDB itself cannot automatically ensure correct rundown of the database.

If Exit() is not called prior to process termination, steps must be taken to ensure database integrity as documented in `Database Integrity`_,
and unreleased locks may cause small subsequent delays (see `relevant LKE documentation <../AdminOpsGuide/mlocks.html#introduction>`_).

Recommended behaviour is for your main routine to :code:`defer yottadb.Exit()` early in the main routine's initialization, and then for the main routine
to confirm that all goroutines have stopped or have completely finished accessing the database before returning.

- If Go routines that access the database are spawned, it is the main routine's responsibility to ensure that all such threads have
  finished using the database before it calls :code:`yottadb.Exit()`.
- The application must not call Go's os.Exit() function which is a very low-level function that bypasses any defers.
- Care must be taken with any signal notifications (see `Go Using Signals`_) to prevent them from causing premature exit.
- Note that Go *will* run defers on panic, but not on fatal signals such as :code:`SIGSEGV`.

Exit() may be called multiple times by different threads during an application shutdown.

++++++++++++
Go IncrE()
++++++++++++

.. code-block:: go

        func IncrE(tptoken uint64, errstr *BufferT, incr, varname string, subary []string) (string, error)

Matching `Go KeyT IncrST()`_, :code:`IncrE()` wraps :ref:`ydb-incr-s-st-fn` to atomically increment the referenced global or local variable node coerced to a number with :code:`incr` coerced to a number, with the result stored in the node and returned by the function.

- If :ref:`ydb-incr-s-st-fn` returns an error such as NUMOFLOW or INVSTRLEN, the function returns the error.
- Otherwise, it returns the incremented value of the node.

With a :code:`nil` value for :code:`incr`, the default increment is 1. Note that the value of the empty string coerced to an integer is zero, but 1 is a more useful default value for an omitted parameter in this case.

++++++++++++
Go Init()
++++++++++++

.. code-block:: go

        func Init()

The first invocation of any EasyAPI and SimpleAPI function or method automatically initializes the YottaDB runtime system. Applications that need to initialize YottaDB prior to that, can call :code:`Init()`. Calling :code:`Init()` when the YottaDB runtime system is already initialized is a no-op.

+++++++++++++++++++++
Go IsLittleEndian()
+++++++++++++++++++++

.. code-block:: go

        func IsLittleEndian() bool

The function returns :code:`true` if the underlying computing infrastructure is little endian and :code:`false` otherwise.

++++++++++++++++
Go LockDecrE()
++++++++++++++++

.. code-block:: go

        func LockDecrE(tptoken uint64, errstr *BufferT, varname string, subary []string) error

Matching `Go KeyT LockDecrST()`_ :code:`LockDecrE()` wraps :ref:`ydb-lock-decr-s-st-fn` to decrement the count of the lock name referenced, releasing it if the count goes to zero or ignoring the invocation if the process does not hold the lock.

.. _go-locke:

++++++++++++
Go LockE()
++++++++++++

.. code-block:: go

        func LockE(tptoken uint64, errstr *BufferT, timeoutNsec uint64, namesnsubs ... interface{}) error

Matching `Go LockST()`_, :code:`LockE()` releases all lock resources currently held and then attempts to acquire the named lock resources referenced. If no lock resources are specified, it simply releases all lock resources currently held and returns.

:code:`interface{}` is a series of pairs of :code:`varname string` and :code:`subary []string` parameters, where a null `subary` parameter (:code:`[]string{}`) specifies the unsubscripted lock resource name.

If lock resources are specified, upon return, the process will have acquired all of the named lock resources or none of the named lock resources.

- If :code:`timeoutNsec` exceeds :code:`yottadb.YDB_MAX_TIME_NSEC`, the function returns with an error return of TIME2LONG.
- If the lock resource names exceeds the maximum number supported (currently 11), the function returns a PARMOFLOW error.
- If :code:`namesnsubs` is not a series of alternating :code:`string` and :code:`[]string` parameters, the function returns the :ref:`invlknmpairlist` error.
- If it is able to aquire the lock resource(s) within :code:`timeoutNsec` nanoseconds, the function returns holding the lock resource(s); otherwise it returns LOCKTIMEOUT. If :code:`timeoutNsec` is zero, the function makes exactly one attempt to acquire the lock resource(s).

++++++++++++++++
Go LockIncrE()
++++++++++++++++

.. code-block:: go

        func LockIncrE(tptoken uint64, errstr *BufferT, timeoutNsec uint64, varname string, subary []string) error

Matching `Go KeyT LockIncrST()`_, :code:`LockIncrE()` wraps :ref:`ydb-lock-incr-s-st-fn` to attempt to acquire the referenced lock resource name without releasing any locks the process already holds.

- If the process already holds the named lock resource, the function increments its count and returns.
- If :code:`timeoutNsec` exceeds :code:`yottadb.YDB_MAX_TIME_NSEC`, the function returns with an error return TIME2LONG.
- If it is able to aquire the lock resource within :code:`timeoutNsec` nanoseconds, it returns holding the lock, otherwise it returns LOCKTIMEOUT. If :code:`timeoutNsec` is zero, the function makes exactly one attempt to acquire the lock.

+++++++++++++
Go LockST()
+++++++++++++

.. code-block:: go

        func LockST(tptoken uint64, errstr *BufferT, timeoutNsec uint64, lockname ... *KeyT) error

Matching `Go LockE()`_, :code:`LockST()` wraps :ref:`ydb-lock-s-st-fn` to release all lock resources currently held and then attempt to acquire the named lock resources referenced. If no lock resources are specified, it simply releases all lock resources currently held and returns.

If lock resources are specified, upon return, the process will have acquired all of the named lock resources or none of the named lock resources.

- If :code:`timeoutNsec` exceeds :code:`yottadb.YDB_MAX_TIME_NSEC`, the method returns with a TIME2LONG error.
- If the number of lock resource names exceeds the maximum number supported (currently eleven), the function returns a PARMOFLOW error.
- If it is able to aquire the lock resource(s) within :code:`timeoutNsec` nanoseconds, it returns holding the lock resource(s); otherwise it returns LOCKTIMEOUT. If :code:`timeoutNsec` is zero, the method makes exactly one attempt to acquire the lock resource(s).

+++++++++++++++
Go MessageT()
+++++++++++++++

.. code-block:: go

        func MessageT(tptoken uint64, errstr *BufferT, status int) (string, error)

:code:`MessageT()` returns the text template for the error number specified by :code:`status`.

- If :code:`status` does not correspond to an error that YottaDB recognizes, it returns the error UNKNOWNSYSERR.
- Otherwise, it returns the error message text template for the error number specified by :code:`status`.

+++++++++++++++
Go NewError()
+++++++++++++++

.. code-block:: go

        func NewError(tptoken uint64, errstr *BufferT, errnum int) error

:code:`NewError()` is a function to create a new YDBError from :code:`errstr` and :code:`errnum`, setting the two private fields in the returned YDBError to the provided values.

++++++++++++++++
Go NodeNextE()
++++++++++++++++

.. code-block:: go

        func NodeNextE(tptoken uint64, errstr *BufferT, varname string, subary []string) ([]string, error)

Matching `Go KeyT NodeNextST()`_, :code:`NodeNextE()` wraps :ref:`ydb-node-next-s-st-fn` to facilitate traversal of a local or global variable tree. A node or subtree does not have to exist at the specified key.

- If there is a next node, it returns the subscripts of that next node.
- If there is no node following the specified node, the function returns the NODEEND error.

++++++++++++++++
Go NodePrevE()
++++++++++++++++

.. code-block:: go

        func NodePrevE(tptoken uint64, errstr *BufferT, varname string, subary []string) ([]string, error)

Matching `Go KeyT NodePrevST()`_, :code:`NodePrevE()` wraps :ref:`ydb-node-previous-s-st-fn` to facilitate reverse traversal of a local or global variable tree. A node or subtree does not have to exist at the specified key.

- If there is a previous node, it returns the subscripts of that previous node; an empty string array if that previous node is the root.
- If there is no node preceding the specified node, the function returns the NODEEND error.

+++++++++++++++++++++++++++
Go RegisterSignalNotify()
+++++++++++++++++++++++++++

.. code-block:: go

        func RegisterSignalNotify(sig syscall.Signal, notifyChan, ackChan chan bool, notifyWhen YDBHandlerFlag) error

Requests that when the wrapper receives the notification that a given signal (:code:`sig`) has occurred, then if a registration has been done with this function for that signal, we also notify a user channel that the signal has occurred. How and when that notification is sent (relative to YottaDB's own handling of the signal) is controlled by the :code:`YDBHandlerFlag` setting. The flag descriptions are described in the :ref:`go-using-signals` subsection of the Go Programming Notes.

Both :code:`notifyChan` and :code:`ackChan` are channels passed in by the caller. The :code:`notifyChan` is the channel the caller wishes to be notified on when the specified signal occurs. The :code:`ackChan` is the channel that, once the notified routine is done doing whatever they were going to do, the notify routine should post something (any bool) on this channel to notify the wrapper they are done. Signal processing then proceeds depending on when the user notification occurred. Note that before the dispatcher notifies the :code:`notifyChan` user channel, the :code:`ackChan` channel is drained.

+++++++++++++++
Go ReleaseT()
+++++++++++++++

.. code-block:: go

        func ReleaseT(tptoken uint64, errstr *BufferT) string

Returns a string consisting of six space separated pieces to provide version information for the Go wrapper and underlying YottaDB release:

- The first piece is always "gowr" to identify the Go wrapper.
- The Go wrapper release number, which starts with "v" and is followed by three numbers separated by a period ("."), e.g., "v0.90.0" mimicking `Semantic Versioning <https://semver.org/>`_. The first is a major release number, the second is a minor release number under the major release and the third is a patch level. Even minor and patch release numbers indicate formally released software. Odd minor release numbers indicate software builds from "in flight" code under development, between releases. Note that although they follow the same format, Go wrapper release numbers are different from the release numbers of the underlying YottaDB release as reported by :ref:`zyrelease-isv`.
- The third through sixth pieces are :ref:`zyrelease-isv` from the underlying YottaDB release.

++++++++++++++
Go SetValE()
++++++++++++++

.. code-block:: go

        func SetValE(tptoken uint64, errstr *BufferT, value, varname string, subary []string) error

Matching `Go KeyT SetValST()`_, at the referenced local or global variable node, or the intrinsic special variable, :code:`SetValE()` wraps :ref:`ydb-set-s-st-fn` to set the value specified by :code:`value`.

+++++++++++++++
Go SubNextE()
+++++++++++++++

.. code-block:: go

        func SubNextE(tptoken uint64, errstr *BufferT, varname string, subary []string) (string, error)

Matching `Go KeyT SubNextST()`_, :code:`SubNextE()` wraps :ref:`ydb-subscript-next-s-st-fn` to facilitate traversal of a local or global variable sub-tree. A node or subtree does not have to exist at the specified key.

- At the level of the last subscript, if there is a next subscript with a node and/or a subtree, it returns that subscript.
- If there is no next node or subtree at that level of the subtree, the function returns the NODEEND error.

In the special case where :code:`subary` is the null array, :code:`SubNextE()` returns the name of the next global or local variable, and the NODEEND error if there is no global or local variable following  :code:`varname`.

+++++++++++++++
Go SubPrevE()
+++++++++++++++

.. code-block:: go

        func SubPrevE(tptoken uint64, errstr *BufferT, varname string, subary []string) (string, error)

Matching `Go KeyT SubPrevST()`_, :code:`SubPrevE()` wraps :ref:`ydb-subscript-previous-s-st-fn` to facilitate reverse traversal of a local or global variable sub-tree. A node or subtree does not have to exist at the specified key.

- At the level of the last subscript, if there is a previous subscript with a node and/or a subtree, it returns that subscript.
- If there is no previous node or subtree at that level of the subtree, the function returns the NODEEND error.

In the special case where :code:`subary` is the null array :code:`SubPrevE()` returns the name of the previous global or local variable, and the NODEEND error if there is no global or local variable preceding :code:`varname`.

++++++++++
Go TpE()
++++++++++

.. code-block:: go

        func TpE(tptoken uint64, errstr *BufferT, tpfn func(uint64, *BufferT) int32, transid string, varnames []string) error

Matching `Go BufferTArray TpST()`_, :code:`TpE()` wraps :code:`ydb-tp-s-st-fn` to implement :ref:`txn-proc`.

Refer to `Go BufferTArray TpST()`_ for a more detailed discussion of YottaDB Go transaction processing.

+++++++++++++++++++++++++++++
Go UnRegisterSignalNotify()
+++++++++++++++++++++++++++++

.. code-block:: go

        func UnRegisterSignalNotify(sig syscall.Signal) error

Requests a halt to signal notifications for the specified signal. If the signal is a signal that YottaDB does not allow, currently, the wrapper raises a panic (like it does for all other wrapper errors) though this is likely to change in a subsequent release. If the signal is a valid signal but is not being monitored, no error results. In that case, the call is a no-op.

+++++++++++
Go ValE()
+++++++++++

.. code-block:: go

        func ValE(tptoken uint64, errstr *BufferT, varname string, subary []string) (string, error)

Matching `Go KeyT ValST()`_, :code:`ValE()` wraps :ref:`ydb-get-s-st-fn` to return the value at the referenced global or local variable node, or intrinsic special variable.

- If :ref:`ydb-get-s-st-fn` returns an error such as GVUNDEF, INVSVN, LVUNDEF, the function returns the error.
- Otherwise, it returns the value at the node.

++++++++++++++++++++
Go BufferT Alloc()
++++++++++++++++++++

.. code-block:: go

        func (buft *BufferT) Alloc(nBytes uint32)

Allocate a buffer in YottaDB heap space of size :code:`nBytes`; and set :code:`BufferT` structure to provide access to that buffer.

+++++++++++++++++++
Go BufferT Dump()
+++++++++++++++++++

.. code-block:: go

        func (buft *BufferT) Dump()

For debugging purposes, dump on stdout:

- :code:`buft` as a hexadecimal address;
- for the :code:`C.ydb_buffer_t` structure referenced by :code:`buft`:

  - :code:`buf_addr` as a hexadecimal address, and
  - :code:`len_alloc` and :code:`len_used` as integers; and

- at the address :code:`buf_addr`, the lower of :code:`len_used` or :code:`len_alloc` bytes in :ref:`zwrite-format`.

As this function is intended for debugging and provides details of internal structures, its output is likely to change as internal implementations change, even when stability of the external API is maintained.

+++++++++++++++++++++++++++
Go BufferT DumpToWriter()
+++++++++++++++++++++++++++

.. code-block:: go

        func (buft *BufferT) DumpToWriter(writer io.writer)

For debugging purposes, dump on :code:`writer`:

- :code:`buft` as a hexadecimal address;
- for the :code:`C.ydb_buffer_t` structure referenced by :code:`buft`:

  - :code:`buf_addr` as a hexadecimal address, and
  - :code:`len_alloc` and :code:`len_used` as integers; and

- at the address :code:`buf_addr`, the lower of :code:`len_used` or :code:`len_alloc` bytes in :ref:`zwrite-format`.

As this function is intended for debugging and provides details of internal structures, its output is likely to change as internal implementations change, even when stability of the external API is maintained.

+++++++++++++++++++
Go BufferT Free()
+++++++++++++++++++

.. code-block:: go

        func (buft *BufferT) Free()

The inverse of the :code:`Alloc()` method: release the buffer in YottaDB heap space referenced by the :code:`C.ydb_buffer_t` structure, release the :code:`C.ydb_buffer_t`, and set :code:`buft` in the :code:`BufferT` structure to :code:`nil`.

+++++++++++++++++++++++
Go BufferT LenAlloc()
+++++++++++++++++++++++

.. code-block:: go

        func (buft *BufferT) LenAlloc(tptoken uint64, errstr *BufferT) (uint32, error)

- If the underlying structures have not yet been allocated, return the :ref:`structunallocd` error.
- Otherwise, return the :code:`len_alloc` field of the :code:`C.ydb_buffer_t` structure referenced by :code:`buft`.

++++++++++++++++++++++
Go BufferT LenUsed()
++++++++++++++++++++++

.. code-block:: go

        func (buft *BufferT) LenUsed(tptoken uint64, errstr *BufferT) (uint32, error)

- If the underlying structures have not yet been allocated, return the :ref:`structunallocd` error.
- If the :code:`len_used` field of the :code:`C.ydb_buffer_t` structure is greater than its :code:`len_alloc` field (owing to a prior INVSTRLEN error), return an INVSTRLEN error and the :code:`len_used` field of the :code:`C.ydb_buffer_t` structure referenced by :code:`buft`.
- Otherwise, return the :code:`len_used` field of the :code:`C.ydb_buffer_t` structure referenced by :code:`buft`.

+++++++++++++++++++++++++
Go BufferT SetLenUsed()
+++++++++++++++++++++++++

.. code-block:: go

        func (buft *BufferT) SetLenUsed(tptoken uint64, errstr *BufferT, newLen uint32) error

Use this method to change the length of a used substring of the contents of the buffer referenced by the :code:`buf_addr` field of the referenced :code:`C.ydb_buffer_t`.

- If the underlying structures have not yet been allocated, return the :ref:`structunallocd` error.
- If :code:`newLen` is greater than the :code:`len_alloc` field of the referenced :code:`C.ydb_buffer_t`, make no changes and return with an error return of INVSTRLEN.
- Otherwise, set the :code:`len_used` field of the referenced :code:`C.ydb_buffer_t` to :code:`newLen`.

Note that even if :code:`newLen` is not greater than the value of :code:`len_alloc`, setting a :code:`len_used` value greater than the number of meaningful bytes in the buffer will likely lead to hard-to-debug errors.

+++++++++++++++++++++++++
Go BufferT SetValBAry()
+++++++++++++++++++++++++

.. code-block:: go

        func (buft *BufferT) SetValBAry(tptoken uint64, errstr *BufferT, value []byte) error

- If the underlying structures have not yet been allocated, return the :ref:`structunallocd` error.
- If the length of :code:`value` is greater than the :code:`len_alloc` field of the :code:`C.ydb_buffer_t` structure referenced by :code:`buft`, make no changes and return INVSTRLEN.
- Otherwise, copy the bytes of :code:`value` to the referenced buffer and set the :code:`len_used` field to the length of :code:`value`.

++++++++++++++++++++++++
Go BufferT SetValStr()
++++++++++++++++++++++++

.. code-block:: go

        func (buft *BufferT) SetValStr(tptoken uint64, errstr *BufferT, value string) error

- If the underlying structures have not yet been allocated, return the :ref:`structunallocd` error.
- If the length of :code:`value` is greater than the :code:`len_alloc` field of the :code:`C.ydb_buffer_t` structure referenced by :code:`buft`, make no changes and return INVSTRLEN.
- Otherwise, copy the bytes of :code:`value` to the referenced buffer and set the :code:`len_used` field to the length of :code:`value`.

++++++++++++++++++++++++
Go BufferT Str2ZwrST()
++++++++++++++++++++++++

.. code-block:: go

        func (buft *BufferT) Str2ZwrST(tptoken uint64, errstr *BufferT, zwr *BufferT) error

The method wraps :ref:`ydb-str2zwr-s-st-fn` to provide the string in :ref:`zwrite-format`.

- If the underlying structures have not yet been allocated, return the :ref:`structunallocd` error.
- If :code:`len_alloc` is not large enough, set :code:`len_used` to the required length, and return an INVSTRLEN error. In this case, :code:`len_used` will be greater than :code:`len_alloc` until corrected by application code, and the value referenced by :code:`zwr` is unspecified.
- Otherwise, set the buffer referenced by :code:`buf_addr` to the :ref:`zwrite-format` string, and set :code:`len_used` to the length.

Note that the length of a string in :ref:`zwrite-format` is always greater than or equal to the string in its original, unencoded format.

++++++++++++++++++++++
Go BufferT ValBAry()
++++++++++++++++++++++

.. code-block:: go

        func (buft *BufferT) ValBAry(tptoken uint64, errstr *BufferT) ([]byte, error)

- If the underlying structures have not yet been allocated, return the :ref:`structunallocd` error.
- If the :code:`len_used` field of the :code:`C.ydb_buffer_t` structure is greater than its :code:`len_alloc` field (owing to a prior INVSTRLEN error), return an INVSTRLEN error and :code:`len_alloc` bytes as a byte array.
- Otherwise, return :code:`len_used` bytes of the buffer as a byte array.

+++++++++++++++++++++
Go BufferT ValStr()
+++++++++++++++++++++

.. code-block:: go

        func (buft *BufferT) ValStr(tptoken uint64, errstr *BufferT) (string, error)

- If the underlying structures have not yet been allocated, return the :ref:`structunallocd` error.
- If the :code:`len_used` field of the :code:`C.ydb_buffer_t` structure is greater than its :code:`len_alloc` field (owing to a prior INVSTRLEN error), return an INVSTRLEN error and :code:`len_alloc` bytes as a string.
- Otherwise, return :code:`len_used` bytes of the buffer as a string.

++++++++++++++++++++++++
Go BufferT Zwr2StrST()
++++++++++++++++++++++++

.. code-block:: go

        func (buft *BufferT) Zwr2StrST(tptoken uint64, errstr *BufferT, str *BufferT) error

This method wraps :ref:`ydb-zwr2str-s-st-fn` and is the inverse of `Go BufferT Str2ZwrST()`_.

- If the underlying structures have not yet been allocated, return the :ref:`structunallocd` error.
- If :code:`len_alloc` is not large enough, set :code:`len_used` to the required length, and return an INVSTRLEN error. In this case, :code:`len_used` will be greater than :code:`len_alloc` until corrected by application code.
- If :code:`str` is not in valid :ref:`zwrite-format`, set :code:`len_used` to zero, and return :code:`YDB_OK`.
- Otherwise, set the buffer referenced by :code:`buf_addr` to the unencoded string, set :code:`len_used` to the length.

+++++++++++++++++++++++++
Go BufferTArray Alloc()
+++++++++++++++++++++++++

.. code-block:: go

        func (buftary *BufferTArray) Alloc(numSubs, bufSiz uint32)

Allocate an array of :code:`numSubs` buffers in YottaDB heap space, each of of size :code:`bufSiz`, referenced by the :code:`BufferTArray` structure.

++++++++++++++++++++++++++++++++
Go BufferTArray DeleteExclST()
++++++++++++++++++++++++++++++++

.. code-block:: go

        func (buftary *BufferTArray) DeleteExclST(tptoken uint64, errstr *BufferT) error

:code:`DeleteExclST()` wraps :ref:`ydb-delete-excl-s-st-fn` to delete all local variable trees except those of local variables whose names are specified in the :code:`BufferTArray` structure. In the special case where :code:`elemsUsed` is zero, the method deletes all local variable trees.

In the event that the :code:`elemsUsed` exceeds :code:`yottadb.YDB_MAX_NAMES`, the error return is ERRNAMECOUNT2HI.

As mixing M and Go application code in the same process is now supported, make sure you understand what (sub)trees are being deleted when you use :ref:`ydb-delete-excl-s-st-fn`.

++++++++++++++++++++++++
Go BufferTArray Dump()
++++++++++++++++++++++++

.. code-block:: go

        func (buftary *BufferTArray) Dump()

For debugging purposes, dump on stdout:

- :code:`buftary` as a hexadecimal address;
- :code:`elemsAlloc` and :code:`elemsUsed` as integers;
- for each element of the smaller of :code:`elemsAlloc` and :code:`elemsUsed` elements of the :code:`C.ydb_buffer_t` array referenced by :code:`buftary`:

  - :code:`buf_addr` as a hexadecimal address, and
  - :code:`len_alloc` and :code:`len_used` as integers; and
  - the smaller of :code:`len_used` and :code:`len_alloc` bytes at the address :code:`buf_addr`, in :ref:`zwrite-format`.

As this function is intended for debugging and provides details of internal structures, its output is likely to change as internal implementations change, even when stability of the external API is maintained.

++++++++++++++++++++++++++++++++
Go BufferTArray DumpToWriter()
++++++++++++++++++++++++++++++++

.. code-block:: go

        func (buftary *BufferTArray) DumpToWriter(writer io.writer)

For debugging purposes, dump on :code:`writer`:

- :code:`buftary` as a hexadecimal address;
- :code:`elemsAlloc` and :code:`elemsUsed` as integers;
- for each element of the smaller of :code:`elemsAlloc` and :code:`elemsUsed` elements of the :code:`C.ydb_buffer_t` array referenced by :code:`buftary`:

  - :code:`buf_addr` as a hexadecimal address, and
  - :code:`len_alloc` and :code:`len_used` as integers; and
  - the smaller of :code:`len_used` and :code:`len_alloc` bytes at the address :code:`buf_addr`, in :ref:`zwrite-format`.

As this function is intended for debugging and provides details of internal structures, its output is likely to change as internal implementations change, even when stability of the external API is maintained.

+++++++++++++++++++++++++++++
Go BufferTArray ElemAlloc()
+++++++++++++++++++++++++++++

.. code-block:: go

        func (buftary *BufferTArray) ElemAlloc() uint32

- If the underlying structures have not yet been allocated, return the :ref:`structunallocd` error.
- Otherwise, return the number of allocated buffers.

++++++++++++++++++++++++++++++++
Go BufferTArray ElemLenAlloc()
++++++++++++++++++++++++++++++++

.. code-block:: go

        func (buftary *BufferTArray) ElemLenAlloc(tptoken uint64) uint32

- If the underlying structures have not yet been allocated, return the :ref:`structunallocd` error.
- Otherwise, return the :code:`len_alloc` from the :code:`C.ydb_buffer_t` structures referenced by :code:`buftary`, all of which have the same value.

+++++++++++++++++++++++++++++++
Go BufferTArray ElemLenUsed()
+++++++++++++++++++++++++++++++

.. code-block:: go

        func (buftary *BufferTArray) ElemLenUsed(tptoken uint64, errstr *BufferT, idx uint32) (uint32, error)

- If the underlying structures have not yet been allocated, return the :ref:`structunallocd` error.
- If :code:`idx` is greater than or equal to the :code:`elemsAlloc` of the :code:`BufferTArray` structure, return with an error return of INSUFFSUBS.
- Otherwise, return the :code:`len_used` field of the array element specifed by :code:`idx` of the :code:`C.ydb_buffer_t` array referenced by :code:`buftary`.

++++++++++++++++++++++++++++
Go BufferTArray ElemUsed()
++++++++++++++++++++++++++++

.. code-block:: go

        func (buftary *BufferTArray) ElemUsed() uint32

- If the underlying structures have not yet been allocated, return the :ref:`structunallocd` error.
- Otherwise, return the value of the :code:`elemsUsed` field.

++++++++++++++++++++++++
Go BufferTArray Free()
++++++++++++++++++++++++

.. code-block:: go

        func (buftary *BufferTArray) Free()

The inverse of the :code:`Alloc()` method: release the :code:`numSubs` buffers and the :code:`C.ydb_buffer_t` array. Set :code:`buftary` to :code:`nil`, and :code:`elemsAlloc` and :code:`elemsUsed` to zero.

++++++++++++++++++++++++++++++++++
Go BufferTArray SetElemLenUsed()
++++++++++++++++++++++++++++++++++

.. code-block:: go

        func (buftary *BufferTArray) SetElemLenUsed(tptoken uint64, errstr *BufferT, idx, newLen uint32) error

Use this method to set the number of bytes in :code:`C.ydb_buffer_t` structure referenced by :code:`buft` of the array element specified by :code:`idx`, for example to change the length of a used substring of the contents of the buffer referenced by the :code:`buf_addr` field of the referenced :code:`C.ydb_buffer_t`.

- If the underlying structures have not yet been allocated, return the :ref:`structunallocd` error.
- If :code:`idx` is greater than or equal to :code:`elemsAlloc`, make no changes and return an INSUFFSUBS error.
- If :code:`newLen` is greater than the :code:`len_alloc` field of the referenced :code:`C.ydb_buffer_t`, make no changes and return an INVSTRLEN error.
- Otherwise, set the :code:`len_used` field of the referenced :code:`C.ydb_buffer_t` to :code:`newLen`.

Note that even if :code:`newLen` is not greater than the value of :code:`len_alloc`, using a :code:`len_used` value greater than the number of meaningful bytes in the buffer will likely lead to hard-to-debug errors.

+++++++++++++++++++++++++++++++
Go BufferTArray SetElemUsed()
+++++++++++++++++++++++++++++++

.. code-block:: go

        func (buftary *BufferTArray) SetElemUsed(tptoken uint64, errstr *BufferT, newUsed uint32) error

Use this method to set the current number of valid strings (subscripts or variable names) in the :code:`BufferTArray`.

- If the underlying structures have not yet been allocated, return the :ref:`structunallocd` error.
- If :code:`newUsed` is greater than :code:`elemsAlloc`, make no changes and return with an error return of INSUFFSUBS.
- Otherwise, set :code:`elemsUsed` to :code:`newUsed`.

Note that even if :code:`newUsed` is not greater than the value of :code:`elemsAlloc`, using an :code:`elemsUsed` value greater than the number of valid values in the array will likely lead to hard-to-debug errors.

++++++++++++++++++++++++++++++
Go BufferTArray SetValBAry()
++++++++++++++++++++++++++++++

.. code-block:: go

        func (buftary *BufferTArray) SetValBAry(tptoken uint64, errstr *BufferT, idx uint32, value []byte) error

- If the underlying structures have not yet been allocated, return the :ref:`structunallocd` error.
- If :code:`idx` is greater than or equal to :code:`elemsAlloc` make no changes and return with an error return of INSUFFSUBS.
- If the length of :code:`value` is greater than the :code:`len_alloc` field of the array element specified by :code:`idx`, make no changes, and return INVSTRLEN.
- Otherwise, copy the bytes of :code:`value` to the referenced buffer and set the :code:`len_used` field to the length of :code:`value`.

+++++++++++++++++++++++++++++
Go BufferTArray SetValStr()
+++++++++++++++++++++++++++++

.. code-block:: go

        func (buftary *BufferTArray) SetValStr(tptoken uint64, errstr *BufferT, idx uint32, value string) error

- If the underlying structures have not yet been allocated, return the :ref:`structunallocd` error.
- If :code:`idx` is greater than or equal to :code:`elemsAlloc` make no changes and return with an error return of INSUFFSUBS.
- If the length of :code:`value` is greater than the :code:`len_alloc` field of the array element specified by :code:`idx`, make no changes, and return INVSTRLEN.
- Otherwise, copy the bytes of :code:`value` to the referenced buffer and set the :code:`len_used` field to the length of :code:`value`.

++++++++++++++++++++++++
Go BufferTArray TpST()
++++++++++++++++++++++++

.. code-block:: go

        func (buftary *BufferTArray) TpST(tptoken uint64, errstr *BufferT, tpfn func(uint64, *BufferT) int32, transid string) error

:code:`TpST()` wraps :ref:`ydb-tp-s-st-fn` to implement :ref:`txn-proc`. :code:`tpfn` is a  function with two parameters, the first of which is a :code:`tptoken` and the second is an :code:`errstr`.

As an alternative to parameters for :code:`tpfn`, create closures.

A function implementing logic for a transaction should return :code:`int32` with one of the following:

- A normal return (:code:`YDB_OK`) to indicate that per application logic, the transaction can be committed. The YottaDB database engine will commit the transaction if it is able to, as discussed in :ref:`txn-proc`, and if not, will call the function again.
- :code:`YDB_TP_RESTART` to indicate that the transaction should restart, either because application logic has so determined or because a YottaDB function called by the function has returned TPRESTART.
- :code:`YDB_TP_ROLLBACK` to indicate that :code:`TpST()` should not commit the transaction, and should return ROLLBACK to the caller.

The :code:`BufferTArray` receiving the :code:`TpST()` method is a list of local variables whose values should be saved, and restored to their original values when the transaction restarts. If the :code:`buftary` structures have not been allocated or :code:`elemsUsed` is zero, no local variables are saved and restored; and if :code:`elemsUsed` is 1, and that sole element references the string "*" all local variables are saved and restored.

A case-insensitive value of "BA" or "BATCH" for :code:`transid` indicates to YottaDB that it need not ensure Durability for this transaction (it continues to ensure Atomicity, Consistency, and Isolation), as discussed under :ref:`ydb-tp-s-st-fn`.

Please see both the description of :ref:`ydb-tp-s-st-fn` and the sections on :ref:`txn-proc` and :ref:`threads-txn-proc` for details.

.. note::
	If the transaction logic receives a :code:`YDB_TP_RESTART` or :code:`YDB_TP_ROLLBACK` from a YottaDB function or method that it calls, it *must* return that value to the calling :code:`TpE()` or :code:`TpST()`. Failure to do so could result in application level data inconsistencies and hard to debug application code.

+++++++++++++++++++++++++++
Go BufferTArray ValBAry()
+++++++++++++++++++++++++++

.. code-block:: go

        func (buftary *BufferTArray) ValBAry(tptoken uint64, errstr *BufferT, idx uint32) ([]byte, error)

- If the underlying structures have not yet been allocated, return the :ref:`structunallocd` error.
- If :code:`idx` is greater than or equal to :code:`elemsAlloc`, return a zero length byte array and an error return of INSUFFSUBS.
- If the :code:`len_used` field of the :code:`C.ydb_buffer_t` structure specified by :code:`idx` is greater than its :code:`len_alloc` field (owing to a previous INVSTRLEN error), return a byte array containing the :code:`len_alloc` bytes at :code:`buf_addr` and an INVSTRLEN error.
- Otherwise, return a byte array containing the :code:`len_used` bytes at :code:`buf_addr`.

++++++++++++++++++++++++++
Go BufferTArray ValStr()
++++++++++++++++++++++++++

.. code-block:: go

        func (buftary *BufferTArray) ValStr(tptoken uint64, errstr *BufferT, idx uint32) (string, error)

- If the underlying structures have not yet been allocated, return the :ref:`structunallocd` error.
- If :code:`idx` is greater than or equal to :code:`elemsAlloc`, return a zero length string and an error return of INSUFFSUBS.
- If the :code:`len_used` field of the :code:`C.ydb_buffer_t` structure specified by :code:`idx` is greater than its :code:`len_alloc` field (owing to a previous INVSTRLEN error), return a string containing the :code:`len_alloc` bytes at :code:`buf_addr` and the INVSTRLEN error.
- Otherwise, return a string containing the :code:`len_used` bytes at :code:`buf_addr`.


+++++++++++++++++++++++++++
Go CallMDesc CallMDescT()
+++++++++++++++++++++++++++

.. code-block:: go

        func (mdesc *CallMDesc) CallMDescT(tptoken uint64, errstr *BufferT, retvallen uint32, rtnargs ...interface{}) (string, error)

As a wrapper for the C function `ydb_cip_t() <../ProgrammersGuide/extrout.html#ydb-cip-t-intf>`_, the :code:`CallMDescT()` is a method of the :code:`CallMDesc` (call descriptor) structure which, during the first call, saves information in the :code:`CallMDesc` structure that makes all following calls using the same descriptor structure able to run much faster by bypassing a lookup of the function name and going straight to the M routine being called.

- :code:`CallMDescT()` requires a :code:`CallMDesc` structure to have been created and initialized with the :code:`SetRtnName()` method.

Example:

.. code-block:: go

   var mrtn yottadb.CallMDesc
   fmt.Println("Golang: Invoking HelloWorld")
   mrtn.SetRtnName("HelloWorld")
   retval, err := mrtn.CallMDescT(yottadb.NOTTP, nil, 1024, "English", "USA")
   if nil != err { panic(fmt.Sprintf("CallMDescT() call failed: %s", err)) }
   fmt.Println("Golang: retval =", retval)

+++++++++++++++++++++
Go CallMDesc Free()
+++++++++++++++++++++

.. code-block:: go

	func (mdesc *CallMDesc) Free()

Frees a :code:`CallMDesc` structure previously allocated.

+++++++++++++++++++++++++++
Go CallMDesc SetRtnName()
+++++++++++++++++++++++++++

.. code-block:: go

	func (mdesc *CallMDesc) SetRtnName(rtnname string)

Allocates and initializes a structure to cache information to accelarate Go to M calls made by `Go CallMDesc CallMDescT()`_. :code:`rtnname` is a :code:`<c-call-name>` in a `Call-in table <../ProgrammersGuide/extrout.html#call-in-table>`_. The structure can then be used by the `Go CallMDesc CallMDescT()`_ method to call an M function. YottaDB looks for :code:`rtnname` in the current call-in table.

+++++++++++++++++++++++++++++++++
Go CallMTable CallMTableOpenT()
+++++++++++++++++++++++++++++++++

.. code-block:: go

	func CallMTableOpenT(tptoken uint64, errstr *BufferT, tablename string) (*CallMTable, error)

If the environment variable :code:`ydb_ci` does not specify an `M code call-in table <../ProgrammersGuide/extrout.html#calls-ext-rt-call-ins>`_ at process startup, function :code:`CallMTableOpenT()` can be used to open an initial call-in table. :code:`tablename` is the filename of a call-in table, and the function opens the file and initializes a :code:`CallMTable` structure. Processes use the `zroutines intrinsic special variable <../ProgrammersGuide/isv.html#zroutines-isv>`_ intrinsic special variable to locate M routines to execute, and :code:`$zroutines` is initialized at process startup from the :code:`ydb_routines` environment variable.

+++++++++++++++++++++++++++++++++++
Go CallMTable CallMTableSwitchT()
+++++++++++++++++++++++++++++++++++

.. code-block:: go

	func (newcmtable *CallMTable) CallMTableSwitchT(tptoken uint64, errstr *BufferT) (*CallMTable, error)

Method :code:`CallMTableSwitchT()` enables switching of call-in tables. :code:`newcmtable` is the new call-in table to be used, which should have previously been opened with `Go CallMTable CallMTableOpenT()`_. :code:`*CallMTable` returns the previously open call-in table, :code:`*nil` if there was none. As switching the call-in table does not change :code:`$zroutines`, if the new call-in table requires a different M routine search path, code will need to change :code:`$zroutines` appropriately.

+++++++++++++++++
Go KeyT Alloc()
+++++++++++++++++

.. code-block:: go

        func (key *KeyT) Alloc(varSiz, numSubs, subSiz uint32)

Invoke :code:`Varnm.Alloc(varSiz)` (see `Go BufferT Alloc()`_) and :code:`SubAry.Alloc(numSubs, subSiz)` (see `Go BufferTArray Alloc()`_).

++++++++++++++++++
Go KeyT DataST()
++++++++++++++++++

.. code-block:: go

        func (key *KeyT) DataST(tptoken uint64, errstr *BufferT) (uint32, error)

Matching `Go DataE()`_, :code:`DataST()` returns the result of :ref:`ydb-data-s-st-fn` (0, 1, 10, or 11). In the event of an error, the return value is unspecified.

++++++++++++++++++++
Go KeyT DeleteST()
++++++++++++++++++++

.. code-block:: go

        func (key *KeyT) DeleteST(tptoken uint64, errstr *BufferT, deltype int) error

Matching `Go DeleteE()`_, :code:`DeleteST()` wraps :ref:`ydb-delete-s-st-fn` to delete a local or global variable node or (sub)tree, with a value of :code:`yottadb.YDB_DEL_NODE` for :code:`deltype` specifying that only the node should be deleted, leaving the (sub)tree untouched, and a value of :code:`yottadb.YDB_DEL_TREE` specifying that the node as well as the (sub)tree are to be deleted.

++++++++++++++++
Go KeyT Dump()
++++++++++++++++

.. code-block:: go

        func (key *KeyT) Dump()

Invoke :code:`Varnm.Dump()` (see `Go BufferT Dump()`_) and :code:`SubAry.Dump()` (see `Go BufferTArray Dump()`_).

++++++++++++++++++++++++
Go KeyT DumpToWriter()
++++++++++++++++++++++++

.. code-block:: go

        func (key *KeyT) DumpToWriter(writer io.writer)

Invoke :code:`Varnm.Dump()` (see `Go BufferT Dump()`_) and :code:`SubAry.Dump()` (see `Go BufferTArray Dump()`_), sending the output to :code:`writer`.

++++++++++++++++
Go KeyT Free()
++++++++++++++++

.. code-block:: go

        func (key *KeyT) Free()

Invoke :code:`Varnm.Free()` (see `Go BufferT Free()`_) and :code:`SubAry.Free()` (see `Go BufferTArray Free()`_).

++++++++++++++++++
Go KeyT IncrST()
++++++++++++++++++

.. code-block:: go

        func (key *KeyT) IncrST(tptoken uint64, errstr *BufferT, incr, retval *BufferT) error

Matching `Go IncrE()`_, :code:`IncrST()` wraps :ref:`ydb-incr-s-st-fn` to atomically increment the referenced global or local variable node coerced to a number, with :code:`incr` coerced to a number. It stores the result in the node and also returns it through the :code:`BufferT` structure referenced by :code:`retval`.

- If :ref:`ydb-incr-s-st-fn` returns an error such as NUMOFLOW, the method makes no changes to the structures under :code:`retval` and returns the error.
- If the length of the data to be returned exceeds :code:`retval.lenAlloc`, the method sets the :code:`len_used` of the :code:`C.ydb_buffer_t` referenced by :code:`retval` to the required length, and returns an INVSTRLEN error. The value referenced by :code:`retval` is unspecified.
- Otherwise, it copies the data to the buffer referenced by the :code:`retval.buf_addr`, sets :code:`retval.lenUsed` to its length.

With a :code:`nil` value for :code:`incr`, the default increment is 1. Note that the value of the empty string coerced to an integer is zero, but 1 is a more useful default value for an omitted parameter in this case.

++++++++++++++++++++++
Go KeyT LockDecrST()
++++++++++++++++++++++

.. code-block:: go

        func (key *KeyT) LockDecrS(tptoken uint64, errstr *BufferT) error

Matching `Go LockDecrE()`_ :code:`LockDecrST()` wraps :ref:`ydb-lock-decr-s-st-fn` to decrement the count of the lock name referenced, releasing it if the count goes to zero or ignoring the invocation if the process does not hold the lock.

++++++++++++++++++++++
Go KeyT LockIncrST()
++++++++++++++++++++++

.. code-block:: go

        func (key *KeyT) LockIncrST(tptoken uint64, errstr *BufferT, timeoutNsec uint64) error

Matching `Go LockIncrE()`_, :code:`LockIncrST()` wraps :ref:`ydb-lock-incr-s-st-fn` to attempt to acquire the referenced lock resource name without releasing any locks the process already holds.

- If the process already holds the named lock resource, the method increments its count and returns.
- If :code:`timeoutNsec` exceeds :code:`yottadb.YDB_MAX_TIME_NSEC`, the method returns with an error return TIME2LONG.
- If it is able to aquire the lock resource within :code:`timeoutNsec` nanoseconds, it returns holding the lock, otherwise it returns LOCK_TIMEOUT. If :code:`timeoutNsec` is zero, the method makes exactly one attempt to acquire the lock.

++++++++++++++++++++++
Go KeyT NodeNextST()
++++++++++++++++++++++

.. code-block:: go

        func (key *KeyT) NodeNextST(tptoken uint64, errstr *BufferT, next *BufferTArray) error

Matching `Go NodeNextE()`_, :code:`NodeNextST()` wraps :ref:`ydb-node-next-s-st-fn` to facilitate traversal of a local or global variable tree. A node or subtree does not have to exist at the specified key.

- If there is a subsequent node:

  - If the number of subscripts of that next node exceeds :code:`next.elemsAlloc`, the method sets :code:`next.elemsUsed` to the number of subscripts required, and returns an INSUFFSUBS error. In this case the :code:`elemsUsed` is greater than :code:`elemsAlloc`.
  - If one of the :code:`C.ydb_buffer_t` structures referenced by :code:`next` (call the first or only element :code:`n`) has insufficient space for the corresponding subscript, the method sets :code:`next.elemsUsed` to :code:`n`, and the :code:`len_alloc` of that :code:`C.ydb_buffer_t` structure to the actual space required. The method returns an INVSTRLEN error. In this case the :code:`len_used` of that structure is greater than its :code:`len_alloc`.
  - Otherwise, it sets the structure :code:`next` to reference the subscripts of that next node, and :code:`next.elemsUsed` to the number of subscripts.

- If there is no subsequent node, the method returns the NODEEND error (:code:`yottadb.YDB_ERR_NODEEND`), making no changes to the structures below :code:`next`.

++++++++++++++++++++++
Go KeyT NodePrevST()
++++++++++++++++++++++

.. code-block:: go

        func (key *KeyT) NodePrevST(tptoken uint64, errstr *BufferT, prev *BufferTArray) error

Matching `Go NodePrevE()`_, :code:`NodePrevST()` wraps :ref:`ydb-node-previous-s-st-fn` to facilitate reverse traversal of a local or global variable tree. A node or subtree does not have to exist at the specified key.

- If there is a previous node:

  - If the number of subscripts of that previous node exceeds :code:`prev.elemsAlloc`, the method sets :code:`prev.elemsUsed` to the number of subscripts required, and returns an INSUFFSUBS error. In this case the :code:`elemsUsed` is greater than :code:`elemsAlloc`.
  - If one of the :code:`C.ydb_buffer_t` structures referenced by :code:`prev` (call the first or only element :code:`n`) has insufficient space for the corresponding subscript, the method sets :code:`prev.elemsUsed` to :code:`n`, and the :code:`len_alloc` of that :code:`C.ydb_buffer_t` structure to the actual space required. The method returns an INVSTRLEN error. In this case the :code:`len_used` of that structure is greater than its :code:`len_alloc`.
  - Otherwise, it sets the structure :code:`prev` to reference the subscripts of that prev node, and :code:`prev.elemsUsed` to the number of subscripts.

- If there is no previous node, the method returns the NODEEND error making no changes to the structures below :code:`prev`.

++++++++++++++++++++
Go KeyT SetValST()
++++++++++++++++++++

.. code-block:: go

        func (key *KeyT) SetValST(tptoken uint64, errstr *BufferT, value *BufferT) error

Matching `Go SetValE()`_, at the referenced local or global variable node, or the intrinsic special variable, :code:`SetValST()` wraps :ref:`ydb-set-s-st-fn` to set the value specified by :code:`value`.

+++++++++++++++++++++
Go KeyT SubNextST()
+++++++++++++++++++++

.. code-block:: go

        func (key *KeyT) SubNextST(tptoken uint64, errstr *BufferT, retval *BufferT) error

Matching `Go SubNextE()`_, :code:`SubNextST()` wraps :ref:`ydb-subscript-next-s-st-fn` to facilitate traversal of a local or global variable sub-tree. A node or subtree does not have to exist at the specified key.

- At the level of the last subscript, if there is a next subscript with a node and/or a subtree:

  - If the length of that next subscript exceeds :code:`sub.len_alloc`, the method sets :code:`sub.len_used` to the actual length of that subscript, and returns an INVSTRLEN error. In this case :code:`sub.len_used` is greater than :code:`sub.len_alloc`.
  - Otherwise, it copies that subscript to the buffer referenced by :code:`sub.buf_addr`, and sets :code:`sub.len_used` to its length.

- If there is no next node or subtree at that level of the subtree, the method returns the NODEEND error.

+++++++++++++++++++++
Go KeyT SubPrevST()
+++++++++++++++++++++

.. code-block:: go

        func (key *KeyT) SubPrevST(tptoken uint64, errstr *BufferT, retval *BufferT) error

Matching `Go SubPrevE()`_, :code:`SubPrevST()` wraps :ref:`ydb-subscript-previous-s-st-fn` to facilitate reverse traversal of a local or global variable sub-tree. A node or subtree does not have to exist at the specified key.

- At the level of the last subscript, if there is a previous subscript with a node and/or a subtree:

  - If the length of that previous subscript exceeds :code:`sub.len_alloc`, the method sets :code:`sub.len_used` to the actual length of that subscript, and returns an INVSTRLEN error. In this case :code:`sub.len_used` is greater than :code:`sub.len_alloc`.
  - Otherwise, it copies that subscript to the buffer referenced by :code:`sub.buf_addr`, and sets :code:`buf.len_used` to its length.

- If there is no previous node or subtree at that level of the subtree, the method returns the NODEEND error.

+++++++++++++++++
Go KeyT ValST()
+++++++++++++++++

.. code-block:: go

        func (key *KeyT) ValST(tptoken uint64, errstr *BufferT, retval *BufferT) error

Matching `Go ValE()`_, :code:`ValST()` wraps :ref:`ydb-get-s-st-fn` to return the value at the referenced global or local variable node, or intrinsic special variable, in the buffer referenced by :code:`retval`.

- If :ref:`ydb-get-s-st-fn` returns an error such as GVUNDEF, INVSVN, LVUNDEF, the method makes no changes to the structures under :code:`retval` and returns the error.
- If the length of the data to be returned exceeds :code:`retval.GetLenAlloc()`, the method sets the :code:`len_used` of the :code:`C.ydb_buffer_t` referenced by :code:`retval` to the required length, and returns an INVSTRLEN error.
- Otherwise, it copies the data to the buffer referenced by the :code:`retval.buf_addr`, and sets :code:`retval.lenUsed` to the length of the returned value.

++++++++++++
Go Error()
++++++++++++

.. code-block:: go

        func (err *YDBError) Error() string

:code:`Error()` is a method to return the expected error message string.

----------------------
Go Programming Notes
----------------------

These `Go Programming Notes`_ supplement rather than supplant the more general :ref:`Programming Notes` for C.

++++++++++++
Goroutines
++++++++++++

In order to avoid restricting Go applications to calling the single-threaded YottaDB engine from a single goroutine (which would be unnatural to a Go programmer), the YottaDB Go wrapper calls the functions of the C :ref:`c-simple-api` that support multi-threaded applications, and includes logic to maintain the integrity of the engine.

Directly calling YottaDB C API functions bypasses this protection, and may result in unpredictable results (Murphy says that unpredictable results will occur when you least expect them). Therefore, Go application code should only call the YottaDB API exposed in this `Programming in Go`_ section.

Goroutines in a process are dynamically mapped by the Go implementation to run on threads within that process. Since YottaDB resources are held by the process rather than by the thread or the Goroutine, refer to the :ref:`threads` discussion about the need for applications to avoid race conditions when accessing YottaDB resources.

.. _go-using-signals:

++++++++++++++++
Go Using Signals
++++++++++++++++

As discussed in :ref:`Signals <signals>`, the YottaDB runtime system uses signals. When the Go wrapper is in use, the YottaDB runtime system receives signals from the Go runtime system through the Go wrapper, instead of directly from the operating system. The signals for which the wrapper registers handlers with the Go runtime system are the following (fatal signals marked with '*'):

- syscall.SIGABRT *
- syscall.SIGALRM
- syscall.SIGBUS *
- syscall.SIGCONT
- syscall.SIGFPE *
- syscall.SIGHUP
- syscall.SIGILL *
- syscall.SIGINT *
- syscall.SIGQUIT *
- syscall.SIGSEGV *
- syscall.SIGTERM *
- syscall.SIGTRAP *
- syscall.SIGTSTP
- syscall.SIGTTIN
- syscall.SIGTTOU
- syscall.SIGURG
- syscall.SIGUSR1

In addition, the following two signals have the same signal numbers as other handled signals:

- syscall.SIGIO is a duplicate of syscall.SIGURG.
- syscall.SIGIOT is a duplicate of syscall.SIGABRT.

We recommend against use of :code:`signal.Notify()` by applications that need to be notified of signal receipt especially for fatal signals. If application logic uses :code:`signal.Notify()` to be so notified, both it and YottaDB are notified concurrently about the signal. The YottaDB signal handler will cleanly shut the YottaDB engine and terminate the process with a :code:`panic()`, potentially before the application signal handling logic finishes. Conversely, if the application signal handler finishes its logic and terminates the process with a :code:`panic()` that may leave the YottaDB database in an unclean state, potentially with damage to its internal structures.

Instead, the YottaDB Go wrapper provides a notification facility (:code:`yottadb.RegisterSignalNotify()`) for coordination between the YottaDB signal handler and that of the application code. An application can control both when, and whether, the YottaDB signal handler executes, in relation to the application signal handler. There are two functions:

.. code-block:: go

        func RegisterSignalNotify(sig syscall.Signal, notifyChan, ackChan chan bool, notifyWhen YDBHandlerFlag) error
        func UnRegisterSignalNotify(sig syscall.Signal) error

The parameters are:

- :code:`sig` - the signal being registered or unregistered.

- :code:`notifyChan` - the channel to which YottaDB posts, to notify application code of receipt of the signal.

- :code:`ackChan` - the channel to which the application code posts, to notify YottaDB that it has completed its work.

- :code:`notifyWhen` - specifies when or if the YottaDB signal hander runs, with respect to the application signal handler. Choices are:

  - :code:`yottadb.NotifyBeforeYDBSigHandler` - YottaDB notifies the application handler BEFORE the YottaDB signal handler runs. YottaDB expects the application to post to :code:`ackChan` when it completes, after which the YottaDB signal handler runs.
  - :code:`yottadb.NotifyAfterYDBSigHandler` - YottaDB notifies the application handler AFTER the YottaDB signal handler runs. When it completes, YottaDB posts to :code:`notifyChan`, and waits for the application to post to :code:`ackChan`, indicating that YottaDB can continue. Note since YDB processing for fatal signals issues a :code:`panic()`, :code:`yottadb.NotifyAfterYDBSigHandler` is unsuitable for fatal signals.
  - :code:`yottadb.NotifyAsyncYDBSigHandler` - YottaDB notifies the application signal handler and runs its signal handler concurrently. For practical purposes, this is equivalent to application code using :code:`signal.Notify()`. :code:`ackChan` is ignored in this case.
  - :code:`yottadb.NotifyInsteadOfYDBSigHandler` - The application handler is notified but the YottaDB handler is not run. This parameter value should *never* be used for SIGALRM since YottaDB requires that signal internally for correct database operation. For other signals, we recommend against use of this parameter value unless you either (a) know what you are doing, or (b) are following a recommendation made by YottaDB for your specific situtation.

Note that input/output flow control signals (SIGTSTP, SIGTTIN, SIGTTOU) are excluded from this support as the YottaDB runtime performs no terminal IO. Applications that include logic coded in multiple languages (e.g., Go and C) require careful design of IO flow control signal handling whether or not the YottaDB runtime is active.

YottaDB has a strong need to carefully shutdown databases at process end making sure its buffers are all flushed out, releasing held M locks, etc. To satisfy this requirement, use the following safe programming practices:

#. Always put a :code:`defer yottadb.Exit()` in the main program before invoking YottaDB, which allows YottaDB to shutdown correctly. This takes care of the normal exit case where goroutines terminate and control reverts to the main program, which eventually terminates when it exits.
#. Avoid application exits that use "out-of-band" exits (e.g. :code:`os.Exit()`). This bypasses orderly rundown of all goroutines which we have found to be important for a clean shutdown.
#. Ensure that all goroutines that have called YottaDB have completed and shutdown terminating the application process. Failure to do so may cause YottaDB rundown procedures to be cut short or bypassed, resulting in potential database damage.
#. Do not use :code:`signal.Notify()` for any fatal signal. Instead use :code:`yottadb.RegisterSignalNotify()`.
#. Do not use SIGUSR2 which YottaDB uses internally, and will replace any application handler for SIGUSR2 with its own.

.. note::
	This discussion applies *only* to *asynchronous* signals as defined by Go, i.e., signals that are sent to the process by itself, e.g., with :code:`syscall.Kill()`. It does *not* apply to *synchronous* signals that are signals that are raised by the hardware itself or by the OS as a consequence of executing code, i.e., SIGBUS, SIGFPE, SIGILL and SIGSEGV. A process that receives a synchronous signal will terminate without shutting down the database properly. Should such an event occur, you should verify database integrity when convenient (see `MUPIP INTEG <../AdminOpsGuide/dbmgmt.html#integ>`_), and take appropriate action if damage is encountered (see `MUPIP JOURNAL RECOVER BACKWARD <../AdminOpsGuide/ydbjournal.html#backward-recovery>`_ / `MUPIP JOURNAL ROLLBACK BACKWARD <../AdminOpsGuide/ydbjournal.html#rollback-on-line-noo-nline>`_).

++++++++++++++++++++
Database Integrity
++++++++++++++++++++

When a process terminates, the YottaDB runtime system drives an exit handler to assure that all databases are rundown, and that proper cleanup of storage is completed. YottaDB does this by using an :code:`atexit()` handler that it defines during initialization. However, this is unreliable with Go. Therefore, our **strong** recommendation is to add a :code:`defer yottadb.Exit()` statement to the Go main routine. When the main routine exits, this drives the exit handler to do database rundown and clean up.

The above works well in application processes that terminate normally through the main routine. However, when a process terminates abnormally, e.g., with a fatal error, the exit handler is not always driven, and database cleanup does not always occur. For this reason, we also **strongly** recommend a `MUPIP RUNDOWN <../AdminOpsGuide/dbmgmt.html#rundown>`_ of an application database after the last process that has it open terminates, especially if it is not assured that all processes terminated normally. If all processes terminate normally, this is not required.

.. note::

   Fatal error handlers in the YottaDB runtime system use a :code:`panic()` call to unwind the process through the main program, which allows it to run a :code:`yottadb.Exit()` function that the programmer has deferred. However, spawned goroutines that panic will still exit the program immediately without going through the main routine's exit function. For this reason all goroutines should specifically recover from panics and run yottadb.Exit() before reissuing any panic; otherwise MUPIP RUNDOWN will need to be invoked whenever a goroutine panics.
