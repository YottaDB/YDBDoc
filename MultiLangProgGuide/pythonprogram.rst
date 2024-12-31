.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2019-2025 YottaDB LLC and/or its subsidiaries.#
.. # All rights reserved.                                        #
.. #                                                             #
.. #     This document contains the intellectual property        #
.. #     of its copyright holder(s), and is made available       #
.. #     under a license.  If you do not know the terms of       #
.. #     the license, please stop and do not read further.       #
.. #                                                             #
.. ###############################################################

.. raw:: html

    <img referrerpolicy="no-referrer-when-downgrade" src="https://download.yottadb.com/pythonprogram.png" />

================================
Programming in Python
================================

.. contents::
   :depth: 5

There are two wrappers for programming YottaDB in `Python <https://www.python.org/>`_, `mg_python <https://github.com/chrisemunt/mg_python>`_ and the YottaDB Python wrapper (described below). `mg_python <https://github.com/chrisemunt/mg_python>`_ is developed by `Chris Munt <https://github.com/chrisemunt/>`_ of `MGateway Ltd <https://www.mgateway.com/>`_. We would like to acknowledge his contribution and thank Chris for the value he adds to the YottaDB community.

`mg_python <https://github.com/chrisemunt/mg_python>`_ provides the ability to access YottaDB locally as well as remotely over a network.

The documentation below is specific to the YottaDB Python wrapper. Please use the link to `mg_python <https://github.com/chrisemunt/mg_python>`_ to access its documentation.

The YottaDB Python wrapper wraps the non-threaded :ref:`c-simple-api` functions via a Python C extension created using the `Python C API <https://docs.python.org/3/c-api/index.html>`_. This extension provides a :code:`yottadb` module that may be imported into Python application code to enable programmatic access to YottaDB from within a Python programming environment. Note that the Python wrapper includes YottaDB as a dependency and so must be installed on a system *after* YottaDB is installed.

Since Python is a dynamically typed, object oriented language whereas C is statically typed and lacks object orientation, YDBPython abstracts away all C-level constructs in favor of Pythonic representations. This approach spares Python developers from the labors of managing memory and dealing with other "C-isms".

The Python wrapper provides two ways of calling YottaDB API functions:

* Python functions that directly call YottaDB API functions, mapping one Python interface to each YottaDB API function
* Methods on a YottaDB `Key` class provided in the :code:`yottadb` Python module

Note that the YDBPython doesn't include any threaded YottaDB C API functions. These omissions are due to Python's lack of support for thread-level parallelism, which is in turn due to the constraints of the Python `Global Interpreter Lock <https://wiki.python.org/moin/GlobalInterpreterLock>`_. Accordingly, users seeking concurrent computation when programming YottaDB from Python will need to use process-level parallelism via the `multiprocessing <https://docs.python.org/3/library/multiprocessing.html>`_ library module. An example of such parallelization is given in `YDBPython/tests/test_threenp1.py <https://gitlab.com/YottaDB/Lang/YDBPython/-/blob/master/tests/test_threenp1.py>`_.

As a matter of vocabulary, note that Python class methods like :code:`__init__()` and :code:`__iter__()` are called "magic methods" in this document, though they are also sometimes called "dunder" methods.

.. _python-quick-start:

------------------
Python Quick Start
------------------

The YDBPython wrapper requires a minimum YottaDB release of r1.30 installed and used with UTF-8 support, and is tested with a minimum Python version of 3.8. Python 2 is *not* supported. If the Python packages on your operating system are older, and the Python wrapper does not work, please obtain and install a newer Python version.

This section assumes that YottaDB has already been installed. One way to install YottaDB is described in the :ref:`mlpg-quick-start` section. After completing step 2 of that guide, *Installing YottaDB*, follow the instructions below to download, install, and test the Python wrapper:

#. Install prerequisites:

    * Ubuntu/Debian: :code:`sudo apt install python3-dev python3-setuptools libffi-dev`
    * RHEL/CentOS: :code:`yum install gcc python3 python3-setuptools python3-devel libffi-devel`
    * Arch Linux: :code:`sudo pacman -Sy python-{virtualenv,setuptools,pip} libffi`

#. Set YottaDB environment variables:

    #. Set YottaDB environment variables: :code:`source /usr/local/etc/ydb_env_set`
    #. *Optional*: If YottaDB is built with Address Sanitization (ASAN) enabled, :code:`LD_PRELOAD` and :code:`ASAN_OPTIONS` must be set:

        * :code:`export ASAN_OPTIONS="detect_leaks=0:disable_coredump=0:unmap_shadow_on_exit=1:abort_on_error=1"`
        * :code:`export LD_PRELOAD=$(gcc -print-file-name=libasan.so)`

#. Install YDBPython:

    * *Option 1*: From PyPI:

        * *Option 1*: Install in :code:`venv`:

            #. Enter directory where install is desired, e.g. :code:`cd my-python-project`
            #. Install the :code:`python3-venv` package:
                * Ubuntu/Debian: :code:`sudo apt install python3-venv`
                * RHEL/CentOS: :code:`sudo yum install python3-virtualenv`
                * Arch Linux: :code:`sudo pacman -Sy python-virtualenv`
            #. Create venv: :code:`python3 -m venv .venv`
            #. Activate venv: :code:`source .venv/bin/activate`
            #. Install into venv: :code:`pip install yottadb`

        * *Option 2*: Install to user:

            * :code:`pip3 install yottadb`

        * *Option 3*: Install globally (not suggested):

            * :code:`sudo -E pip3 install yottadb`

    * *Option 2*: From source:

        #. Enter code directory :code:`cd YDBPython/`
        #. Run :code:`setup.py` to install:

            * *Option 1*: Install in :code:`venv`:

                #. Install the :code:`python3-venv` package:

                    * Ubuntu/Debian: :code:`sudo apt install python3-venv`
                    * RHEL/CentOS: :code:`sudo yum install python3-virtualenv`
                    * Arch Linux: :code:`sudo pacman -Sy python-virtualenv`

                #. Create venv: :code:`python3 -m venv .venv`
                #. Activate venv: :code:`source .venv/bin/activate`
                #. Install into venv: :code:`python setup.py install`

            * *Option 2*: Install to user:

                * :code:`python3 setup.py install --user`

            * *Option 3*: Install globally (not suggested):

                * :code:`sudo -E python3 setup.py install`

In the above instructions, note that :code:`python3` command is used when using a global Python 3 installation, i.e. one installed for the current system using e.g. `apt-get install`. The :code:`python` command is used when operating within an active virtual environment ("venv") as described above. The reason for the discrepancy is that many systems map the :code:`python` command to Python 2, and use :code:`python3` to call a Python 3 installation. Within a virtual environment, Python binary paths are remapped to allow the :code:`python` command to reference Python 3. The same principle applies to the :code:`pip` command, with :code:`pip3` referencing the Python 3 version of the :code:`pip` command. :code:`pip` references the Python 2 implementation unless called within a virtual environment, where :code:`pip` is an alias for :code:`pip3`.

When building the Python wrapper from source, you may validate that it was built and installed correctly by running its test suite:

#. Enter the directory containing the Python wrapper code repository, e.g. :code:`cd YDBPython/`
#. Install :code:`pytest`, :code:`pytest-order` and :code:`psutil`:

    #. If :code:`pip` for python3 is not installed do so:

        * Ubuntu/Debian: :code:`sudo apt install python3-pip`
        * RHEL/CentOS: :code:`sudo yum install python3-pip`
        * Arch Linux: :code:`sudo pacman -Sy python-pip`

    #. Use :code:`pip` to install :code:`pytest`, :code:`pytest-order` and :code:`psutil`:

        * *Option 1*: Install into :code:`venv`:

            #. Activate :code:`venv` if it is not already: :code:`source .venv/bin/activate`
            #. Install: :code:`pip install pytest pytest-order psutil`

        * *Option 2*: Install for user: :code:`pip3 install --user pytest pytest-order psutil`
        * *Option 3*: Install globally (not suggested): :code:`sudo pip3 install pytest pytest-order psutil`

#. Run the tests:

    * *Option 1*: in :code:`venv`: :code:`python -m pytest`
    * *Option 2*: with user installation: :code:`python3 -m pytest`
    * *Option 3*: with global installation (not suggested): :code:`python3 -m pytest`
    * Note that the :code:`test_wordfreq.py` program randomly uses local or global variables (see :ref:`lcl-gbl-var`).

#. *Optional*: Cleanup between tests:

    * When making changes to code between test runs, some cleanup may be needed to prevent new changes being ignored due to Python caching. To clean up these files: `for artifact in $(cat .gitignore); do rm -rf $artifact; done`. Note that this will delete all files listed in `.gitignore`, including core files. If these or any other such files need to be retained, move or rename them before running the aforementioned command.

There are a number of test programs in the :code:`YDBPython/tests` directory that you can look at for examples of how to use the Python wrapper.

To write your own programs using the YDBPython wrapper, simply import the :code:`yottadb` module into your Python program with :code:`import yottadb` after installing it via one of the methods specified above.

If you would like to import the :code:`yottadb` module in a location outside of the YDBPython repository, you may do the following:

#. Import :code:`yottadb` from an arbitrary directory:

        * *Approach 1*: using a local YDBPython repository, e.g. as built above:

            * *Option 1*: using venv: :code:`pip install --editable /path/to/YDBPython/directory`
            * *Option 2 or Option 3*: using user or global installation: :code:`pip3 install --editable /path/to/YDBPython/directory`

        * *Approach 2*: using the PyPi package:

            * *Option 1*: using venv: :code:`pip install yottadb`
            * *Option 2 or Option 3*: using user or global installation: :code:`pip3 install yottadb`

Note that if using a virtual environment ("venv"), you will need to activate it with :code:`source .venv/bin/activate` before using YDBPython in each new terminal session, and not only at installation time.

---------------
Python Concepts
---------------

As the YottaDB wrapper is distributed as a Python package, function calls to YottaDB are prefixed in Python code with :code:`yottadb.` (e.g., application code to call the :code:`get()` function would be written :code:`yottadb.get(...)`). Alternatively, users may instantiate a :code:`Key` object and use the methods on that object to call YottaDB API functions, e.g.:

.. code-block:: python

    key = yottadb.Key("^myglobal")["sub1"]["sub2"]
    key.get()

+++++++++++++++++++++++++
Python Exception Handling
+++++++++++++++++++++++++

The YottaDB C API has a comprehensive set of error return codes. Each error is comprised of a unique number and a mnemonic. Thus, for example, to return an error that a buffer allocated for a return value is not large enough, YottaDB uses the "INVSTRLEN" error code, which has the numeric value :code:`yottadb.YDB_ERR_INVSTRLEN`. YottaDB attempts to maintain stability of the numeric values and mnemonics from release to release, to ensure applications remain compatible when the underlying YottaDB releases are upgraded.

In contrast, Python applications typically use exceptions to handle errors, rather than numeric codes as C does. To reconcile these two different error handling mechanisms, YDBPython uses a hybrid approach by implementing, with a few exceptions (no pun intended), a generic :code:`yottadb.YDBError` exception class with a :code:`YDBError.code()` method for accessing the error code of the underlying error indicated by YottaDB. Each :code:`yottadb.YDBError` exception raised will include an error message describing the failure. The :code:`YDBError.code()` method is provided as a convenience in cases where a human-readable error message is insufficient and code needs to differentiate handling for different error scenarios.

Below are examples illustrating how to handle exceptions both with and without using the :code:`YDBError.code()` method:

.. code-block:: python

    try:
        yottadb.get(varname="^myglobal", subsarray=("sub1", "sub2"))
    except YDBError:
        print("Generic case: handle any error issued by YottaDB")

    try:
        yottadb.node_next(varname="^myglobal", subsarray=("sub1", "sub2"))
    except YDBNodeEnd:
        print("Specific case: handle YDB_ERR_NODEEND differently")

There are, however, a few special exceptions in YDBPython that are used to signal events that are not necessarily errors, but may need special handling. These are distinguished by unique exception classes apart from :code:`yottadb.YDBError`:

*  :code:`yottadb.YDBTimeoutError`: Raised when a YDBPython function that includes a timeout limit has taken longer than the specified limit to complete execution, e.g. `Python lock()`_.
*  :code:`yottadb.YDBTPRollback`: See `Python tp()`_ for more information.
*  :code:`yottadb.YDBTPRestart`: See `Python tp()`_ for more information.

For example:

.. code-block:: python

    try:
        yottadb.tp(callback, args=(arg1,arg2,))
    except yottadb.YDBTPRestart:
        return

The Python wrapper will also raise exceptions whenever it encounters its own errors, which may occur independently of any interactions with YottaDB itself, for example when incorrect Python types are passed as arguments to wrapper code. In such cases, YDBPython will raise either a :code:`YDBPythonError` with a message describing the error, or else it will raise a built-in Python exception, e.g. :code:`ValueError`. Python built-in exceptions are used whenever possible, with :code:`YDBPythonError` being raised in a handful of unique scenarios not covered by built-in exceptions.

Note that though all YottaDB error codes are implemented as Python exceptions, not all of these exceptions are expected at the Python level since many YottaDB error codes represent C-level issues that Python users are not in a position to address. For instance, the aforementioned "INVSTRLEN" error pertains to a C buffer allocation size error and so is not meaningful to a user of the Python wrapper.

Given the nature of exception handling, there is no "success" exception when a YDBPython wrapper function succeeds. At the C level, the :code:`YDB_OK` code is returned. At the Python level, on the other hand, a successful call simply returns a value, if any, and omits to raise an exception. Accordingly, if an exception is raised, the call was not successful.

-------------------------
Python Symbolic Constants
-------------------------

`YottaDB symbolic constants <https://docs.yottadb.com/MultiLangProgGuide/cprogram.html#symbolic-constants>`_ are available in the YDBPython module, for example, :code:`yottadb.YDB_ERR_INVSTRLEN`.

-------------
Python API
-------------

YottaDB global and local variable nodes may be represented in multiple ways within the YDBPython wrapper. First, YottaDB nodes may be represented as two-element native Python tuples with the variable name as the first element of the tuple and a tuple containing a set of subscripts as the second element. For example, :code:`("mylocal", ("sub1", "sub2"))` represents the YottaDB local variable node :code:`mylocal("sub1","sub2")`. Similarly, YottaDB nodes may be represented by tuples, e.g.: :code:`("^test3", ("sub1", "sub2"))`. Unsubscripted local or global variable nodes may be represented by simply omitting the subscripts from the tuple or function call, for example: :code:`("mylocal",)` or :code:`yottadb.get("mylocal")`.

The Python wrapper also provides a :code:`Key` class for interacting with YottaDB nodes in an object-oriented fashion. Each :code:`Key` represents a combination of a global or local variable name and zero or more subscripts. Operations on this node may be performed by instantiating a :code:`Key` object representing that node's variable name and subscript combination and calling the method corresponding to the desired YottaDB API function on that object. For example:

.. code-block:: python

    key = yottadb.Key("^myglobal")["sub1"]["sub2"]
    key.set("myvalue")
    key.get()  # Returns b"myvalue"

Note that :code:`yottadb.get()` and some other functions return Python :code:`bytes` objects instead of :code:`str` objects. This is because YottaDB stores arbitrary binary data, which is not guaranteed to be UTF-8 encoded, as Python :code:`str` objects are by default. Accordingly, returning `bytes` objects allows users to retrieve arbitrary binary data from YottaDB without getting a :code:`UnicodeEncodeError` for binary data that is not UTF-8 formatted. When *accepting* data (or subscripts, etc.), on the other hand, YDBPython accepts both :code:`str` and :code:`bytes` objects.

New :code:`Key` objects may be created from existing :code:`Key` objects by specifying additional subscripts in brackets, e.g.:

.. code-block:: python

    key1 = yottadb.Key("mylocal")  # key1 represents YottaDB node: `mylocal`
    key2 = key1["sub1"]["sub2"]  # key2 represents YottaDB node: `mylocal("sub1","sub2")`

Intrinsic special variables may be accessed in the same way as global or local variables, with the provision that no subscripts are specified within the node tuple, as such variables are not actual YottaDB nodes. For example:

.. code-block:: python

    print(yottadb.get(("$ZYRELEASE",)))  # Print the current YottaDB release information

The length of strings (values and subscripts) in YottaDB is variable, as is the number of subscripts a local or global variable can have. However, in the case of the Python wrapper, such considerations are handled within the wrapper itself such that users need not concern themselves with memory allocation or management. Rather, users may simply pass valid Python objects to the wrapper (i.e. :code:`str`, :code:`bytes`, or, when setting values, :code:`int` objects), which will take care of any memory allocation and management as needed.

.. _python-api-funcs:

++++++++++++++++++++
Python API Functions
++++++++++++++++++++

* `Python ci()`_
* `Python cip()`_
* `Python data()`_
* `Python delete_node()`_
* `Python delete_tree()`_
* `Python get()`_
* `Python incr()`_
* `Python lock()`_
* `Python lock_decr()`_
* `Python lock_incr()`_
* `Python message()`_
* `Python node_next()`_
* `Python node_previous()`_
* `Python nodes()`_
* `Python open_ci_table()`_
* `Python release()`_
* `Python set()`_
* `Python str2zwr()`_
* `Python subscript_next()`_
* `Python subscript_previous()`_
* `Python subscripts()`_
* `Python switch_ci_table()`_
* `Python tp()`_
* `Python transaction()`_
* `Python zwr2str()`_

.. _python-api:

-----------------------------------------
Python Data Structures & Type Definitions
-----------------------------------------

As noted above, Python and C have significantly different approaches to data structures and memory management. Consequently, the YDBPython wrapper has no data structures that map directly to any C-level structure. Rather, the Python wrapper provides a combination of native Python tuples and :code:`Key` objects for interacting with the underlying YottaDB C API.

Thus only one custom type is provided by the :code:`yottadb` Python module:

- :code:`Key` an object class for representing a YottaDB local, global, or intrinsic special variable providing methods by which to access wrapper functions

All memory is managed internally and implicitly either by the YottaDB wrapper code (and YottaDB itself, for its own operations) or else by the Python runtime. Accordingly, users need not concern themselves with memory management or C-level data structures.

------------------------
Python Wrapper Functions
------------------------

+++++++++++++
Python ci()
+++++++++++++

.. code-block:: python

        def ci(routine: AnyStr, args: Tuple[Any] = (), has_retval: bool = False) -> Any

As a wrapper for the C function , the :code:`ci()` function is used to call M routines from Python, used when a single call to the function is anticipated. :code:`ci()` supports both read-only and read-write parameters.

If the specified routine has a return value, the caller of :code:`ci()` must specify this using the :code:`has_retval` parameter. This instructs the wrapper to internally allocate space for a return value and correctly construct the call to the underlying :code:`ydb_ci()` YottaDB Simple API call. When there is no return value, :code:`None` will be returned.

If a return value is specified but has not been configured in the `call-in descriptor file <https://gitlab.com/YottaDB/Lang/YDBPython/-/blob/master/tests/calltab.ci>`_ or vice-versa, a parameter mismatch situation is created. In the parameter mismatch case, the error returned will be arbitrary and so may be inconsistent across calls. Accordingly, it is recommended to always ensure that routine parameters and return types are correctly specified in the call-in descriptor file.

- :code:`args` refers to a list of 0 or more arguments passed to the called routine. Arguments must be passed as Python :code:`str`, :code:`bytes`, or :code:`int` objects. When calling routines that accept 0 arguments, the :code:`args` field can simply be omitted or an empty :code:`Tuple` passed (the default). Any output arguments will be returned as a Python :code:`bytes` object and can be subsequently cast to another Python type. The number of parameters possible is restricted to 34 (for 64-bit systems) or 33 (for 32-bit systems). If the maximum number of parameters is exceeded, a :code:`ValueError` will be raised.
- :code:`has_retval` is set to :code:`False` by default. Accordingly, if the given routine has a return value :code:`has_retval` will need to explicitly be set to :code:`True`.

For example, see the below setup for a sample :code:`HelloWorld2` routine.

First, the call-in descriptor entry included in a call-in table file, e.g. :code:`calltab.ci`:

.. code-block:: none

    HelloWorld2 : ydb_string_t * entry^helloworld2(I:ydb_string_t *, IO:ydb_string_t *, I:ydb_string_t *)

The contents of the M routine referenced by :code:`calltab.ci` above, i.e. :code:`helloworld2.m`:

.. code-block:: none

    ; Hello world routine driven from Python
    entry(p1,p2,p3)
        if ("1"'=p1)!("24"'=p2)!("3"'=p3) write "FAIL: parameters not as expected" quit "PARM-FAIL"
        set p2a=p2
        set p2="1"
        quit p3_p2a_p1

The Python call-in to the :code:`HelloWorld2` routine:

.. code-block:: python

    print("Python: Invoking HelloWorld2")
    try:
        print(yottadb.ci("HelloWorld2", ["1", "24", "3"], has_retval=True))
    except Exception as e:
        print(e)


The HelloWorld2 program in the example returns a string containing the three parameters, :code:`"1"`, :code:`"24"`, and :code:`"3"` concatenated together in reverse order: :code:`"3241"`. Note that :code:`has_retval` is set to :code:`True` to signal that a return value is expected.

Note that a call-in table is required when calling from Python into M. A call-in table can be specified at process startup with the environment variable :code:`ydb_ci` or using the functions :code:`yottadb.open_ci_table` and :code:`yottadb.switch_ci_table`, e.g:

.. code-block:: python

    cur_handle = yottadb.open_ci_table(cur_dir + "/tests/calltab.ci")
    yottadb.switch_ci_table(cur_handle)

If the underlying `ydb_ci() <../ProgrammersGuide/extrout.html#ydb-ci-t-intf>`_ call returns an error, the function raises an exception containing the error code and message.

+++++++++++++
Python cip()
+++++++++++++

.. code-block:: python

        def cip(routine: AnyStr, args: Tuple[Any] = (), has_retval: bool = False) -> Any

As a wrapper for the C function `ydb_cip() <../ProgrammersGuide/extrout.html#ydb-cip-t-intf>`_, the :code:`cip()` function is used to call M routines from Python, used when repeated calls to the function are anticipated. Performance is slightly improved using :code:`cip()` in such cases since this function saves a hash table lookup compared to :code:`ci()`. :code:`cip()` supports both read-only and read-write parameters.

If the specified routine has a return value, the caller of :code:`cip()` must specify this using the :code:`has_retval` parameter. This instructs the wrapper to internally allocate space for a return value and correctly construct the call to the underlying :code:`ydb_ci()` YottaDB Simple API call. When there is no return value, :code:`None` will be returned.

If a return value is specified but has not been configured in the `call-in descriptor file <https://gitlab.com/YottaDB/Lang/YDBPython/-/blob/master/tests/calltab.ci>`_ or vice-versa, a parameter mismatch situation is created.

- :code:`args` refers to a list of 0 or more arguments passed to the called routine. Arguments must be passed as Python :code:`str`, :code:`bytes`, or :code:`int` objects. When calling routines that accept 0 arguments, the :code:`args` field can simply be omitted or an empty :code:`Tuple` passed (the default). Any output arguments will be returned as a Python :code:`bytes` object and can be subsequently cast to another Python type. The number of parameters possible is restricted to 34 (for 64-bit systems) or 33 (for 32-bit systems). If the maximum number of parameters is exceeded, a :code:`ValueError` will be raised.
- :code:`has_retval` is set to :code:`False` by default. Accordingly, if the given routine has a return value :code:`has_retval` will need to explicitly be set to :code:`True`.

For example, see the below setup for a sample :code:`HelloWorld3` routine.

First, the call-in descriptor entry included in a call-in table file, e.g. :code:`calltab.ci`:

.. code-block:: none

    HelloWorld3 : ydb_string_t * entry^helloworld3(I:ydb_string_t *, IO:ydb_string_t *, I:ydb_string_t *)

The contents of the M routine referenced by :code:`calltab.ci` above, i.e. :code:`helloworld3.m`:

.. code-block:: none

    ; Hello world routine driven from Python
    entry(p1,p2,p3)
        if ("1"'=p1)!("17"'=p2)!("3"'=p3) write "FAIL: parameters not as expected" quit "PARM-FAIL"
        set p2a=p2
        set p2="1"
        quit p3_p2a_p1

The Python call-in to the :code:`HelloWorld3` routine:

.. code-block:: python

    print("Python: Invoking HelloWorld3")
    try:
        print(yottadb.cip("HelloWorld3", ["1", "17", "3"], has_retval=True))
    except Exception as e:
        print(e)


The HelloWorld3 program in the example returns a string containing the three parameters, :code:`"1"`, :code:`"17"`, and :code:`"3"` concatenated together in reverse order: :code:`"3171"`. Note that :code:`has_retval` is set to :code:`True` to signal that a return value is expected.

Note that a call-in table is required when calling from Python into M. Additionally, any M routines that the call-in uses must be in a path referenced by the :code:`ydb_routines` environment variable.

A call-in table can be specified at process startup with the environment variable :code:`ydb_ci` or using the functions :code:`yottadb.open_ci_table` and :code:`yottadb.switch_ci_table`, e.g:

.. code-block:: python

    cur_handle = yottadb.open_ci_table(os.getcwd() + "/tests/calltab.ci")
    yottadb.switch_ci_table(cur_handle)

If the underlying `ydb_cip() <../ProgrammersGuide/extrout.html#ydb-cip-t-intf>`_ call returns an error, the function raises an exception containing the error code and message.

+++++++++++++
Python data()
+++++++++++++

.. code-block:: python

    def data(varname: AnyStr, subsarray: Tuple[AnyStr] = ()) -> int

As a wrapper for the C function :ref:`ydb-data-s-st-fn`, :code:`data()` returns an integer value of 0, 1, 10, or 11 for the specified local or global variable node indicating what data may or may not be stored on or under that node. The meaning of these values is as follows:

+ 0: There is neither a value nor a subtree, i.e., the node is undefined
+ 1: There is a value, but no subtree
+ 10: There is no value, but there is a subtree.
+ 11: There are both a value and a subtree.

- If :code:`subsarray` is omitted, an empty :code:`Tuple` is passed by default, signifying that the variable name node should be referenced without any subscripts.
- If the underlying :ref:`ydb-data-s-st-fn` call returns an error, the function raises an exception containing the error code and message.

.. code-block:: python

    yottadb.set("mylocal", ("sub1", "sub2"), "test")
    print(yottadb.data("mylocal", ("sub1", "sub2"))) # Prints 1
    print(yottadb.data("mylocal", ("sub1",))) # Prints 10
    print(yottadb.data("mylocal", ("sub1", "sub2", "sub3"))) # Prints 0

    yottadb.set("mylocal", ("sub1", "sub2", "sub3"), "test2")
    print(yottadb.data("mylocal", ("sub1", "sub2", "sub3"))) # Prints 1
    print(yottadb.data("mylocal", ("sub1", "sub2"))) # Prints 11

++++++++++++++++++++
Python delete_node()
++++++++++++++++++++

.. code-block:: python

    def delete_node(varname: AnyStr, subsarray: Tuple[AnyStr] = ()) -> None

As a wrapper for the C function :ref:`ydb-delete-s-st-fn`, :code:`delete_node()` deletes the value stored at the given local or global variable node, if any, but leaves any subtree intact.

- If :code:`subsarray` is omitted, an empty :code:`Tuple` is passed by default, signifying that the variable name node should be referenced without any subscripts.
- If the underlying :ref:`ydb-delete-s-st-fn` call returns an error, the function raises an exception containing the error code and message.

.. code-block:: python

    yottadb.set("mylocal", ("sub1",), "test1")
    yottadb.set("mylocal", ("sub1", "sub2"), "test2")
    print(yottadb.get("mylocal", ("sub1",))  # Prints b'test1'
    print(yottadb.get("mylocal", ("sub1", "sub2"))  # Prints b'test2'
    yottadb.delete_node("mylocal", ("sub1",))
    print(yottadb.get("mylocal", ("sub1",))  # Prints None
    print(yottadb.get("mylocal", ("sub1", "sub2"))  # Prints b'test2'

++++++++++++++++++++
Python delete_tree()
++++++++++++++++++++

.. code-block:: python

    def delete_tree(varname: AnyStr, subsarray: Tuple[AnyStr] = ()) -> None

As a wrapper for the C function :ref:`ydb-delete-s-st-fn`, :code:`delete_tree()` deletes both the value and subtree, if any, of the given local or global variable node.

- If :code:`subsarray` is omitted, an empty :code:`Tuple` is passed by default, signifying that the variable name node should be referenced without any subscripts.
- If the underlying :ref:`ydb-delete-s-st-fn` call returns an error, the function raises an exception containing the error code and message.

.. code-block:: python

    print(yottadb.data("mylocal", ("sub1", "sub2"))) # Prints 0
    yottadb.set("mylocal", ("sub1", "sub2"), "test")
    print(yottadb.data("mylocal", ("sub1", "sub2"))) # Prints 1
    print(yottadb.data("mylocal", ("sub1",))) # Prints 10
    yottadb.delete_tree("mylocal", ("sub1",))
    print(yottadb.data("mylocal", ("sub1", "sub2"))) # Prints 0
    print(yottadb.data("mylocal", ("sub1",))) # Prints 0

++++++++++++
Python get()
++++++++++++

.. code-block:: python

    def get(varname: AnyStr, subsarray: Tuple[AnyStr] = ()) -> Optional[bytes]

As a wrapper for the C function :ref:`ydb-get-s-st-fn`, :code:`get()` returns the value at the referenced global or local variable node, or intrinsic special variable.

- If :code:`subsarray` is omitted, an empty :code:`Tuple` is passed by default, signifying that the variable name node should be referenced without any subscripts.
- If the underlying :ref:`ydb-get-s-st-fn` call returns an error of GVUNDEF or LVUNDEF, the function returns a value of :code:`None` and does not raise an exception.
- If the underlying :ref:`ydb-get-s-st-fn` call returns an error other than GVUNDEF or LVUNDEF, the function raises an exception containing the error code and message.
- Otherwise, it returns the value at the node.

.. code-block:: python

    print(yottadb.get("mylocal", ("sub1", "sub2"))  # Prints None
    yottadb.set("mylocal", ("sub1", "sub2"), "test")
    print(yottadb.get("mylocal", ("sub1", "sub2"))  # Prints b'test'

+++++++++++++
Python incr()
+++++++++++++

.. code-block:: python

    def incr(varname: AnyStr, subsarray: Tuple[AnyStr] = (), increment: Union[int, float, str, bytes] = "1") -> bytes

As a wrapper for the C function :ref:`ydb-incr-s-st-fn`, :code:`incr()` atomically increments the referenced global or local variable node by the value of :code:`increment`, with the result stored in the node and returned by the function. The value of the unit of incrementation may be passed as either a Python :code:`str` or :code:`int` object.

- If :code:`subsarray` is omitted, an empty :code:`Tuple` is passed by default, signifying that the variable name node should be referenced without any subscripts.
- If a value for the :code:`increment` parameter is omitted, the default increment is 1.
- If the underlying :ref:`ydb-incr-s-st-fn` call returns an error, the function raises an exception containing the error code and message.

.. code-block:: python

    print(yottadb.get("mylocal", ("sub1", "sub2"))) # Prints None
    print(yottadb.incr("mylocal", ("sub1", "sub2"))) # Prints b'1'
    print(yottadb.incr("mylocal", ("sub1", "sub2"))) # Prints b'2'

+++++++++++++
Python lock()
+++++++++++++

.. code-block:: python

    def lock(keys: Tuple[Tuple[Union[tuple, Optional["Key"]]]] = (), timeout_nsec: int = 0) -> None

As a wrapper for the C function :ref:`ydb-lock-s-st-fn`, :code:`lock()` releases all lock resources currently held and then attempts to acquire the named lock resources referenced. If no lock resources are specified, it simply releases all lock resources currently held and returns.

Lock resources are specified by passing YottaDB keys as a tuple or list of Python :code:`tuple` or :code:`yottadb.Key` objects. Each tuple representing a key must be of the form :code:`(variable_name, (subscript1, subscript2, ...))`, i.e. consist of two elements, a string representing a variable name and a tuple containing a series of strings representing subscripts, if any.

If lock resources are specified, upon return, the process will have acquired all of the named lock resources or none of the named lock resources.

- If :code:`timeout_nsec` exceeds :code:`yottadb.YDB_MAX_TIME_NSEC`, a :code:`yottadb.YDBError` exception will be raised where :code:`yottadb.YDB_ERR_TIME2LONG == YDBError.code()`
- If the lock resource names exceeds the maximum number supported (currently 11), the function raises a :code:`ValueError` exception.
- If :code:`keys` is not a Tuple of tuples representing variable name and subscript pairs, or a series of :code:`yottadb.Key` objects, then the function raises a :code:`TypeError` exception.
- If it is able to acquire the lock resource within :code:`timeout_nsec` nanoseconds, it returns holding the lock, otherwise it raises a :code:`YDBTimeoutError` exception. If :code:`timeout_nsec` is zero, the function makes exactly one attempt to acquire the lock, which is the default behavior if a value for :code:`timeout_nsec` is omitted.
- If the underlying :ref:`ydb-lock-s-st-fn` call returns any other error, the function raises an exception containing the error code and message.

The following example provides a demonstration of basic locking operations. The example locks several keys, then attempts to increment the lock on each key by calling a separately defined :code:`lock_value()` helper function as a separate Python process. Due to the initial locking of each key, each of these :code:`lock_value()` fails with an exit code of 1. Next, all locks are released and a number of new :code:`lock_value()` processes are spawned that again attempt to increment a lock on each key. Since all locks were previously released, these new attempts succeed with each process exiting with a 0 exit code.

.. code-block:: python

    import multiprocessing
    import datetime

    # Lock a value in the database
    def lock_value(key: Union[yottadb.Key, tuple], interval: int = 2, timeout: int = 1):
        # Extract key information from key object to compose lock_incr()/lock_decr() calls
        if isinstance(key, yottadb.Key):
            varname = key.varname
            subsarray = key.subsarray
        else:
            varname = key[0]
            subsarray = key[1]
        if len(subsarray) == 0:
            subsarray = None

        # Attempt to increment lock on key
        has_lock = False
        try:
            yottadb.lock_incr(varname, subsarray, timeout_nsec=(timeout * 1_000_000_000))
            print("Lock Success")
            has_lock = True
        except yottadb.YDBTimeoutError:
            print("Lock Failed")
            sys.exit(1)
        except Exception as e:
            print(f"Lock Error: {repr(e)}")
            sys.exit(2)

        # Attempt to decrement lock on key, after a brief pause to ensure increment has taken effect
        if has_lock:
            time.sleep(interval)
            yottadb.lock_decr(varname, subsarray)
            if timeout != 0 or interval != 0:
                print("Lock Released")

        sys.exit(0)


    t1 = yottadb.Key("^test1")
    t2 = yottadb.Key("^test2")["sub1"]
    t3 = yottadb.Key("^test3")["sub1"]["sub2"]
    keys_to_lock = (t1, t2, t3)
    # Attempt to get locks for keys t1,t2 and t3
    yottadb.lock(keys=keys_to_lock, timeout_nsec=0)
    # Attempt to increment/decrement locks
    processes = []
    for key in keys_to_lock:
        process = multiprocessing.Process(target=lock_value, args=(key,))
        process.start()
        processes.append(process)
    for process in processes:
        process.join()
        print(process.exitcode)  # Prints 1
    # Release all locks
    yottadb.lock()
    # Attempt to increment/decrement locks
    processes = []
    for key in keys_to_lock:
        process = multiprocessing.Process(target=lock_value, args=(key,))
        process.start()
        processes.append(process)
    for process in processes:
        process.join()
        print(process.exitcode)  # Prints 0

++++++++++++++++++
Python lock_decr()
++++++++++++++++++

.. code-block:: python

    def lock_decr(varname: AnyStr, subsarray: Tuple[AnyStr] = ()) -> None

As a wrapper for the C function :ref:`ydb-lock-decr-s-st-fn`, :code:`lock_decr()` decrements the count of the lock name referenced, releasing it if the count goes to zero or ignoring the invocation if the process does not hold the lock.

- If :code:`subsarray` is omitted, an empty :code:`Tuple` is passed by default, signifying that the variable name node should be referenced without any subscripts.
- If the underlying :ref:`ydb-lock-decr-s-st-fn` call returns an error, the function raises an exception containing the error code and message.

.. code-block:: python

    t1 = datetime.datetime.now()
    yottadb.lock_incr("test2", ("sub1",))  # Increment lock on a local variable node, locking it
    t2 = datetime.datetime.now()
    time_elapse = t2.timestamp() - t1.timestamp()
    print(time_elapse)  # Prints time elapsed, should be < 0.01
    yottadb.lock_decr("test2", ("sub1",))  # Decrement lock on a local variable node, releasing it

++++++++++++++++++
Python lock_incr()
++++++++++++++++++

.. code-block:: python

    def lock_incr(varname: AnyStr, subsarray: Tuple[AnyStr] = (), timeout_nsec: int = 0) -> None

As a wrapper for the C function :ref:`ydb-lock-incr-s-st-fn`, :code:`lock_incr()` attempts to acquire the referenced lock resource name without releasing any locks the process already holds.

- If :code:`subsarray` is omitted, an empty :code:`Tuple` is passed by default, signifying that the variable name node should be referenced without any subscripts.
- If the process already holds the named lock resource, the function increments its count and returns.
- If :code:`timeout_nsec` exceeds :code:`yottadb.YDB_MAX_TIME_NSEC`, a :code:`yottadb.YDBError` exception will be raised where :code:`yottadb.YDB_ERR_TIME2LONG == YDBError.code()`
- If it is able to acquire the lock resource within :code:`timeout_nsec` nanoseconds, it returns holding the lock, otherwise it raises a :code:`YDBTimeoutError` exception. If :code:`timeout_nsec` is zero, the function makes exactly one attempt to acquire the lock, which is the default behavior if :code:`timeout_nsec` is omitted.
- If the underlying :ref:`ydb-lock-incr-s-st-fn` call returns any other error, the function raises an exception containing the error code and message.

.. code-block:: python

    t1 = datetime.datetime.now()
    yottadb.lock_incr("test2", ("sub1",))  # Increment lock on a local variable node, locking it
    t2 = datetime.datetime.now()
    time_elapse = t2.timestamp() - t1.timestamp()
    print(time_elapse)  # Prints time elapsed, should be < 0.01
    yottadb.lock_decr("test2", ("sub1",))  # Decrement lock on a local variable node, releasing it

++++++++++++++++
Python message()
++++++++++++++++

.. code-block:: python

    def message(errnum: int) -> str

As a wrapper for the C function :ref:`ydb-message-messaget-fn`, :code:`message()` returns the text template for the error number specified by :code:`errnum`. A negative error number is treated the same as its corresponding positive error number, such that  :code:`yottadb.message(x)` and :code:`yottadb.message(-x)` produce the same output.

- If :code:`errnum` does not correspond to an error that YottaDB recognizes, a :code:`yottadb.YDBError` exception will be raised where :code:`yottadb.YDB_ERR_UNKNOWNSYSERR == YDBError.code()`
- Otherwise, it returns the error message text template for the error number specified by :code:`errnum`.

.. code-block:: python

    print(yottadb.message(-150375522))  # Prints '%YDB-E-INVSTRLEN, Invalid string length !UL: max !UL'

++++++++++++++++++
Python node_next()
++++++++++++++++++

.. code-block:: python

    def node_next(varname: AnyStr, subsarray: Tuple[AnyStr] = ()) -> Tuple[bytes, ...]

As a wrapper for the C function :ref:`ydb-node-next-s-st-fn`, :code:`node_next()` facilitates traversal of a local or global variable tree.

- If :code:`subsarray` is omitted, an empty :code:`Tuple` is passed by default, signifying that the variable name node should be referenced without any subscripts.
- If there is a next node, it returns the subscripts of that next node as a tuple of Python :code:`bytes` objects.
- If there is no node following the specified node, a :code:`yottadb.YDBNodeEnd` exception will be raised.
- If the underlying :ref:`ydb-node-next-s-st-fn` call returns any other error, the function raises an exception containing the error code and message.

.. code-block:: python

    # Initialize a test node and maintain full subscript list for later validation
    subs = []
    for i in range(1, 6):
        all_subs.append((b"sub" + bytes(str(i), encoding="utf-8")))
        yottadb.set("mylocal", subs, ("val" + str(i)))
    # Begin iteration over subscripts of node
    node_subs = ()
    while True:
        try:
            node_subs = yottadb.node_next("mylocal", node_subs)
            print(node_subs)  # Prints (b'sub1',), (b'sub1', b'sub2'), etc. successively
        except yottadb.YDBNodeEnd:
            break

++++++++++++++++++++++
Python node_previous()
++++++++++++++++++++++

.. code-block:: python

    def node_previous(varname: AnyStr, subsarray: Tuple[AnyStr] = ()) -> Tuple[bytes, ...]

As a wrapper for the C function :ref:`ydb-node-previous-s-st-fn`, :code:`node_previous()` facilitates reverse traversal of a local or global variable tree.

- If :code:`subsarray` is omitted, an empty :code:`Tuple` is passed by default, signifying that the variable name node should be referenced without any subscripts.
- If there is a previous node, it returns the subscripts of that previous node as a tuple of Python :code:`bytes` objects, or an empty tuple if that previous node is the root.
- If there is no node preceding the specified node, a :code:`yottadb.YDBNodeEnd` exception will be raised.
- If the underlying :ref:`ydb-node-previous-s-st-fn` call returns any other error, the function raises an exception containing the error code and message.

.. code-block:: python

    # Initialize test node and maintain full subscript list for later validation
    subs = []
    for i in range(1, 6):
        all_subs.append((b"sub" + bytes(str(i), encoding="utf-8")))
        yottadb.set("mylocal", subs, ("val" + str(i)))
    # Begin iteration over subscripts of node
    node_subs = yottadb.node_previous("mylocal", subs)
    print(node_subs)  # Prints (b'sub1', b'sub2', b'sub3', b'sub4')
    while True:
        try:
            node_subs = yottadb.node_previous("mylocal", node_subs)
            print(node_subs)  # Prints (b'sub1', b'sub2', b'sub3'), (b'sub1', b'sub2'), and (b'sub1',), successively
        except yottadb.YDBNodeEnd as e:
            break

++++++++++++++
Python nodes()
++++++++++++++

.. code-block:: python

    def nodes(varname: bytes, subsarray: Tuple[bytes] = ()) -> NodesIter:

The :code:`nodes()` function provides a convenient, Pythonic interface for iteratively performing traversals starting from the given YottaDB local or global variable node, as specified by the :code:`varname` and :code:`subscripts` arguments.

Specifically, :code:`nodes()` returns a Python :code:`NodesIter` iterator object that yields a :code:`List` of subscripts representing the next node in the tree on each iteration, in accordance with the behavior for `Python node_next()`_.

Similarly, the :code:`reversed` version of the returned :code:`NodesIter` iterator will yield a :code:`List` of subscripts representing the previous node in the tree on each iteration, in accordance with the behavior for `Python node_previous()`_.

- If :code:`subsarray` is omitted, an empty :code:`Tuple` is passed by default, signifying that the variable name node should be referenced without any subscripts.
- If there is a next node for a given iteration, the :code:`NodesIter` iterator will return the subscripts of that next node as a tuple of Python :code:`bytes` objects.
- If this iterator is passed to the :code:`next()` built-in function and there is no subscript following the subscript previously returned, a :code:`StopIteration` exception will be raised.
- If the underlying `Python node_next()`_ or `Python node_previous()`_ call returns any other error, the :code:`NodesIter` iterator will raise an exception containing the error code and message.

.. code-block:: python

    # Create list of subscript arrays representing some database nodes
    nodes = [
        (b"sub1",),
        (b"sub1", b"subsub1"),
        (b"sub1", b"subsub2"),
        (b"sub1", b"subsub3"),
        (b"sub2",),
        (b"sub2", b"subsub1"),
        (b"sub2", b"subsub2"),
        (b"sub2", b"subsub3"),
        (b"sub3",),
        (b"sub3", b"subsub1"),
        (b"sub3", b"subsub2"),
        (b"sub3", b"subsub3"),
    ]

    # Set various nodes in the database
    for node in nodes:
        yottadb.set("^myglobal", node, str(nodes.index(node)))

    # Iterate over all nodes under a global variable
    for node in yottadb.nodes("^myglobal"):
        # Prints: b'0', b'1', b'2', b'3', b'4', b'5', b'6', b'7', b'8', b'9', b'10', b'11'
        print(yottadb.get("^myglobal", node))

    # Iterate over some nodes under a global variable, beginning at a
    # subscript in the middle of the tree.
    for node in yottadb.nodes("^myglobal", ("sub2",)):
        # b'5', b'6', b'7', b'8', b'9', b'10', b'11'
        print(yottadb.get("^myglobal", node))

    # Iterate over all nodes under a global variable, in reverse order
    for node in reversed(yottadb.nodes("^myglobal")):
        # b'11', b'10', b'9', b'8', b'7', b'6', b'5', b'4', b'3', b'2', b'1', b'0'
        print(yottadb.get("^myglobal", node))

    # Iterate over some nodes under a global variable in reverse order,
    # beginning at a subscript in the middle of the tree.
    for node in reversed(yottadb.nodes("^myglobal", ("sub2",))):
        # b'7', b'6', b'5', b'4', b'3', b'2', b'1', b'0'
        print(yottadb.get("^myglobal", node))

++++++++++++++++++++++
Python open_ci_table()
++++++++++++++++++++++

.. code-block:: python

    def open_ci_table(filename: AnyStr) -> int

As a wrapper for the C function `ydb_ci_tab_open() <https://docs.yottadb.com/ProgrammersGuide/extrout.html#call-in-intf>`_, the :code:`open_ci_table()` function can be used to open an initial call-in table if the environment variable :code:`ydb_ci` does not specify an `M code call-in table <../ProgrammersGuide/extrout.html#calls-ext-rt-call-ins>`_ at process startup. :code:`filename` is the filename of a call-in table, and the function opens the file and initializes an internal structure representing the call-in table and returns an integer representing a handle for later reference to this call-in table.

After a successful call to :code:`open_ci_table()`, YottaDB processes may then use the `zroutines intrinsic special variable <../ProgrammersGuide/isv.html#zroutines-isv>`_ to locate M routines to execute. :code:`$zroutines` is initialized at process startup from the :code:`ydb_routines` environment variable.

If the underlying `ydb_ci_tab_open() <https://docs.yottadb.com/ProgrammersGuide/extrout.html#call-in-intf>`_ call returns an error, the function raises an exception containing the error code and message.

For an example of how to use :code:`open_ci_table`, see the entry for `Python ci()`_ or `Python cip()`_.

++++++++++++++++
Python release()
++++++++++++++++

.. code-block:: python

    def release() -> str

Returns a string consisting of six space separated pieces to provide version information for the Python wrapper and underlying YottaDB release:

- The first piece is always “pywr” to identify the Python wrapper.
- The Python wrapper release number, which starts with “v” and is followed by three numbers separated by a period (“.”), e.g., “v0.90.0” mimicking `Semantic Versioning <https://semver.org/>`_. The first is a major release number, the second is a minor release number under the major release and the third is a patch level. Even minor and patch release numbers indicate formally released software. Odd minor release numbers indicate software builds from “in flight” code under development, between releases. Note that although they follow the same format, Python wrapper release numbers are different from the release numbers of the underlying YottaDB release as reported by :ref:`zyrelease-isv`.
- The third through sixth pieces are :ref:`zyrelease-isv` from the underlying YottaDB release.

.. code-block:: python

    print(yottadb.release())  # Prints e.g. 'pywr v0.10.0 YottaDB r1.32 Linux x86_64'

++++++++++++
Python set()
++++++++++++

.. code-block:: python

    def set(varname: AnyStr, subsarray: Tuple[AnyStr] = (), value: AnyStr = "") -> None

As a wrapper for the C function :ref:`ydb-set-s-st-fn`, :code:`set()` updates the value at the referenced local or global variable node, or the intrinsic special variable to the value contained in the Python :code:`str` or :code:`bytes` object passed via the :code:`value` parameter.

- If :code:`subsarray` is omitted, an empty :code:`Tuple` is passed by default, signifying that the variable name node should be referenced without any subscripts.
- If :code:`value` is omitted, the node will be set to the empty string by default.
- If the underlying :ref:`ydb-set-s-st-fn` call returns an error, the function raises an exception containing the error code and message.

.. code-block:: python

    print(yottadb.get("mylocal", ("sub1", "sub2")))  # Prints None
    yottadb.set("mylocal", ("sub1", "sub2"), "test")
    print(yottadb.get("mylocal", ("sub1", "sub2")))  # Prints b'test'

++++++++++++++++
Python str2zwr()
++++++++++++++++

.. code-block:: python

    def str2zwr(string: AnyStr) -> bytes

As a wrapper for the C function :ref:`ydb-str2zwr-s-st-fn`, :code:`str2zwr()` provides the given string in :ref:`zwrite-format`.

Note that the return value of this function is always a :code:`bytes` object, reflecting the fact that YottaDB stores all values as binary data, such that a global or local variable node value is not guaranteed to be a valid UTF-8 string. Accordingly, the return value of this function is not guaranteed to be castable to a Python :code:`str` object.

Further, note that the length of a string in :ref:`zwrite-format` is always greater than or equal to the string in its original, unencoded format.

If the underlying :ref:`ydb-str2zwr-s-st-fn` call returns an error, the function raises an exception containing the error code and message.

.. code-block:: python

    print(yottadb.str2zwr(b'X\x00ABC'))  # Prints b'"X"_$C(0)_"ABC"'


+++++++++++++++++++++++
Python subscript_next()
+++++++++++++++++++++++

.. code-block:: python

    def subscript_next(varname: AnyStr, subsarray: Tuple[AnyStr] = ()) -> bytes

As a wrapper for the C function :ref:`ydb-subscript-next-s-st-fn`, :code:`subscript_next()` facilitates traversal of a local or global variable sub-tree. A node or subtree does not have to exist at the specified key.

- If :code:`subsarray` is omitted, an empty :code:`Tuple` is passed by default, signifying that the subscript level is zero, and variable names should be iterated over instead of subscripts.
- If there is a next subscript with a node and/or a subtree, this function returns the subscript at the level of the last subscript in :code:`subsarray`
- If there is no next node or subtree at that level of the subtree, a :code:`yottadb.YDBNodeEnd` exception will be raised.
- If the underlying :ref:`ydb-subscript-next-s-st-fn` call returns any other error, the function raises an exception containing the error code and message.

In the special case where :code:`subsarray` is empty, :code:`subscript_next()` returns the name of the next global or local variable, and raises a :code:`yottadb.YDBNodeEnd` exception if there is no global or local variable following :code:`varname`.

.. code-block:: python

    yottadb.set("^myglobal", ("sub1", "sub2"), "val1")
    yottadb.set("^myglobal", ("sub1", "sub3"), "val2")
    yottadb.set("^myglobal", ("sub1", "sub4"), "val3")
    yottadb.set("^myglobal", ("sub1", "sub5"), "val4")

    # Get first subscript of the second subscript level
    subscript = yottadb.subscript_next("^myglobal", ("sub1", ""))
    print(subscript)  # Prints 'sub2'
    while True:
        try:
            print(yottadb.subscript_next("^myglobal", ("sub1", subscript)))  # Prints 'sub3', 'sub4', and 'sub5', successively
        except yottadb.YDBNodeEnd:
            break

    # subscript_next() also works with subscripts that include data that is not ASCII or valid UTF-8
    yottadb.set("mylocal", (b"sub1\x80",)), "val1"),  # Test subscripts with byte strings that are not ASCII or valid UTF-8
    yottadb.set("mylocal", (b"sub2\x80", "sub7")), "val2"),
    yottadb.set("mylocal", (b"sub3\x80", "sub7")), "val3"),
    yottadb.set("mylocal", (b"sub4\x80", "sub7")), "val4"),
    print(yottadb.subscript_next(varname="mylocal", subsarray=("",)))  # Prints b"sub1\x80"
    print(yottadb.subscript_next(varname="mylocal", subsarray=("sub1\x80",)))  # Prints b"sub2\x80"
    print(yottadb.subscript_next(varname="mylocal", subsarray=("sub2\x80",)))  # Prints b"sub3\x80"
    print(yottadb.subscript_next(varname="mylocal", subsarray=("sub3\x80",)))  # Prints b"sub4\x80"
    try:
        print(yottadb.subscript_next(varname="mylocal", subsarray=("sub4\x80",)))
    except YDBNodeEnd:
        pass

+++++++++++++++++++++++++++
Python subscript_previous()
+++++++++++++++++++++++++++

.. code-block:: python

    def subscript_previous(varname: AnyStr, subsarray: Tuple[AnyStr] = ()) -> bytes

As a wrapper for the C function :ref:`ydb-subscript-previous-s-st-fn`, :code:`subscript_previous()` facilitates reverse traversal of a local or global variable sub-tree. A node or subtree does not have to exist at the specified key.

- If :code:`subsarray` is omitted, an empty :code:`Tuple` is passed by default, signifying that the subscript level is zero, and variable names should be iterated over instead of subscripts.
- If there is a previous subscript with a node and/or a subtree, it returns the subscript at the level of the last subscript in :code:`subsarray`
- If there is no next node or subtree at that level of the subtree, a :code:`yottadb.YDBNodeEnd` exception will be raised.
- If the underlying :ref:`ydb-subscript-previous-s-st-fn` call returns any other error, the function raises an exception containing the error code and message.

In the special case where :code:`subsarray` is empty :code:`subscript_previous()` returns the name of the previous global or local variable, and raises a :code:`yottadb.YDBNodeEnd` exception if there is no global or local variable preceding :code:`varname`.

.. code-block:: python

    yottadb.set("^myglobal", ("sub1", "sub2"), "val1")
    yottadb.set("^myglobal", ("sub1", "sub3"), "val2")
    yottadb.set("^myglobal", ("sub1", "sub4"), "val3")
    yottadb.set("^myglobal", ("sub1", "sub5"), "val4")

    # Get last subscript of the second subscript level
    subscript = yottadb.subscript_previous("^myglobal", ("sub1", ""))
    print(subscript)  # Prints 'sub5'
    while True:
        try:
            print(yottadb.subscript_previous("^myglobal", ("sub1", subscript)))  # Prints 'sub4', 'sub3', and 'sub2', successively
        except yottadb.YDBNodeEnd as e:
            break

+++++++++++++++++++
Python subscripts()
+++++++++++++++++++

.. code-block:: python

    def subscripts(varname: AnyStr, subsarray: Tuple[AnyStr] = ()) -> SubscriptsIter

The :code:`subscripts()` function provides a convenient, Pythonic interface for iteratively performing traversals at the specified subscript level, starting from the given YottaDB local or global variable node, as specified by the :code:`varname` and :code:`subscripts` arguments.

Specifically, :code:`subscripts()` returns a Python :code:`SubscriptsIter` iterator object that yields a :code:`bytes` object representing the next subscript at the given subscript level on each iteration, in accordance with the behavior for `Python subscript_next()`_.

Similarly, the :code:`reversed` version of the returned :code:`SubscriptsIter` iterator will yield a :code:`bytes` object representing the previous subscript at the given subscript level on each iteration, in accordance with the behavior for `Python subscript_previous()`_.

- If :code:`subsarray` is omitted, an empty :code:`Tuple` is passed by default, signifying that the variable name node should be referenced without any subscripts. In this case, :code:`subscripts()` will iterate over every local or global variable in the database starting from the local or global variable name specified.
- If there is a next subscript for a given iteration, the :code:`SubscriptsIter` iterator will return the subscript at that subscript level as a Python :code:`bytes` object.
- If this iterator is passed to the :code:`next()` built-in function and there is no subscript following the subscript previously returned, a :code:`StopIteration` exception will be raised.
- If the underlying `Python subscript_next()`_ or `Python subscript_previous()`_ call returns any other error, the :code:`SubscriptsIter` iterator will raise an exception containing the error code and message.

.. code-block:: python

    subs = [b"sub1", b"sub2", b"sub3"]
    # Set various nodes in the database
    for sub in subs:
        yottadb.set("^myglobal", (sub,), str(subs.index(sub)))

    # Iterate over all subscripts under a global variable
    for subscript in yottadb.subscripts("^myglobal", ("",)):
        print(subscript)  # Prints b'sub1', b'sub2', b'sub3'

    # Iterate over some subscripts under a global variable
    for subscript in yottadb.subscripts("^myglobal", ("sub1",)):
        print(subscript)  # Prints b'sub2', b'sub3'

    # Iterate over all subscripts under a global variable, in reverse
    for subscript in reversed(yottadb.subscripts("^myglobal", ("",))):
        print(subscript)  # Prints b'sub3', b'sub2', b'sub1'

    # Iterate over some subscripts under a global variable, in reverse
    for subscript in reversed(yottadb.subscripts("^myglobal", ("sub3",))):
        print(subscript)  # Prints b'sub2', b'sub1'

++++++++++++++++++++++++
Python switch_ci_table()
++++++++++++++++++++++++

.. code-block:: python

    def switch_ci_table(handle: int) -> int

As a wrapper for the C function `ydb_ci_tab_open() <https://docs.yottadb.com/ProgrammersGuide/extrout.html#call-in-intf>`_, the :code:`switch_ci_table()` function enables switching of call-in tables by allowing users to switch to a call-in table previously opened by :code:`open_ci_table()`, as specified through an integer :code:`handle` argument. This argument should be the return value of a previous call to :code:`open_ci_table()`.

:code:`switch_ci_table()` returns an integer handle to the previously active call-in table, :code:`None` if there was none. Switching the call-in table does not change :code:`$zroutines`, so application code will need to change :code:`$zroutines` appropriately if the new call-in table requires a different M routine search path.

If the underlying `ydb_ci_tab_open() <https://docs.yottadb.com/ProgrammersGuide/extrout.html#call-in-intf>`_ call returns an error, the function raises an exception containing the error code and message.

For an example of how to use :code:`switch_ci_table()`, see the entry for `Python ci()`_ or `Python cip()`_.

+++++++++++
Python tp()
+++++++++++

.. code-block:: python

    def tp(callback: object, args: tuple = None, transid: str = "", varnames: Tuple[AnyStr] = None, **kwargs,)

As a wrapper for the C function :ref:`ydb-tp-s-st-fn`, :code:`tp()` provides an interface for performing basic YottaDB transaction processing from Python code. Specifically, :code:`tp()` allows users of the Python wrapper to safely call user-defined Python functions containing transaction logic that modifies or updates one or more nodes within a YottaDB database.

A function implementing logic for a transaction should raise one of the following YDBPython exceptions depending on the scenario encountered during transaction processing:

- If :code:`args` is not specified, :code:`None` is passed by default.
- If :code:`transid` is not specified, the empty string is passed by default.
- If :code:`varnames` is not specified, :code:`None` is passed by default.
- When application logic successfully completes execution, no exception should be raised and the transaction can be committed. The YottaDB database engine will commit the transaction if it is able to and, if not, it will call the function again.
- :code:`YDBTPRestart` is raised to indicate that the transaction should restart, either because application logic has so determined or because a YottaDB function called by the function has returned :code:`YDB_TP_RESTART`.
- :code:`YDBTPRollback` is raised to indicate that :code:`tp()` should not commit the transaction, and should raise a :code:`YDBTPRollback` to the caller.
- If the underlying :ref:`ydb-tp-s-st-fn` call returns any other error, the function raises an exception containing the error code and message.

The :code:`varnames` list passed to the :code:`tp()` method is a list of local variables whose values should be saved, and restored to their original values when the transaction restarts. If the :code:`varnames` is :code:`None`, no local variables are saved and restored. If :code:`varnames` contains one element and that sole element is the string "*" all local variables are saved and restored.

A case-insensitive value of "BA" or "BATCH" for :code:`transid` indicates to YottaDB that it need not ensure Durability for this transaction (it continues to ensure Atomicity, Consistency, and Isolation), as discussed under :ref:`ydb-tp-s-st-fn`.

Please see both the description of :ref:`ydb-tp-s-st-fn` and the section on :ref:`txn-proc` for details.

.. note:: If the transaction logic encounters a :code:`YDBTPRestart` or :code:`YDBTPRollback` exception from a YottaDB function or method that it calls, it *must* not handle that exception. It should let that be handled by the calling :code:`tp()` function. Failure to do so could result in application level data inconsistencies and hard to debug application code.

The following example demonstrates a simple usage of :code:`tp()`. Specifically, a simple :code:`callback()` function is defined, then wrapped in a simple :code:`wrapper()` function that calls :code:`callback()` using :code:`tp()`, ensuring database integrity via transaction processing. Then, several processes executing the :code:`wrapper()` function are spawned, each of which attempts to increment the same global variable nodes at once. Each of these processes continues trying to increment the nodes until the incrementation is successful, i.e. :code:`YDBTPRestart` is not raised. Finally, these processes are gracefully terminated and the values of the global variable nodes are checked to ensure to success of the incrementation attempts of each :code:`wrapper()` process.

.. code-block:: python

    # Define a simple callback function that attempts to increment the global variable nodes represented
    # by the given Key objects. If a YDBTPRestart is encountered, the function will retry the continue
    # attempting the increment operation until it succeeds.
    def callback(fruit1: yottadb.Key, fruit2: yottadb.Key, fruit3: yottadb.Key) -> int:
        while True:
            try:
                fruit1.incr()
                fruit2.incr()
                fruit3.incr()
                break
            except yottadb.YDBTPRestart:
                continue

        return yottadb.YDB_OK

    # Define a simple wrapper function to call the callback function via tp().
    # This wrapper will then be used to spawn multiple processes, each of which
    # calls tp() using the callback function.
    def wrapper(function: Callable[..., object], args: Tuple[AnyStr]) -> int:
        return yottadb.tp(function, args=args)

    # Create keys
    apples = yottadb.Key("^fruits")["apples"]
    bananas = yottadb.Key("^fruits")["bananas"]
    oranges = yottadb.Key("^fruits")["oranges"]
    # Initialize nodes
    apples_init = "0"
    bananas_init = "5"
    oranges_init = "10"
    apples.value = apples_init
    bananas.value = bananas_init
    oranges.value = oranges_init

    # Spawn some processes that will each call the callback function
    # and attempt to access the same nodes simultaneously. This will
    # trigger YDBTPRestarts, until each callback function successfully
    # updates the nodes.
    num_procs = 10
    processes = []
    for proc in range(0, num_procs):
        # Call the callback function that will attempt to update the given nodes
        process = multiprocessing.Process(target=wrapper, args=(callback, (apples, bananas, oranges)))
        process.start()
        processes.append(process)
    # Gracefully terminate each process and confirm it exited without an error
    for process in processes:
        process.join()
        assert process.exitcode == 0

    # Confirm all nodes incremented by num_procs, i.e. by one per callback process spawned
    assert int(apples.value) == int(apples_init) + num_procs
    assert int(bananas.value) == int(apples_init) + num_procs
    assert int(oranges.value) == int(apples_init) + num_procs

++++++++++++++++++++
Python transaction()
++++++++++++++++++++

.. code-block:: python

    def transaction(function) -> Callable[..., object]

The :code:`transaction()` function is provided as a *decorator* for convenience to simplify the basic case of passing a callback function to :code:`Python tp()` when no special handling is needed. It is not intended to be used on its own, but instead for decorating functions that require transaction processing. Users with more sophisticated transaction processing needs are encouraged to write their own decorator functions for handling transactions.

:code:`transaction()` converts the specified function into a form safe for use in YottaDB database transactions. Specifically, it wraps :code:`function` in a new function definition that includes a call to :code:`Python tp()` and basic transaction exception handling. This new wrapper function is then returned and may then be used as a transaction-safe version of the passed function. Accordingly, :code:`function` should be written as if it were to be passed to :code:`Python tp()`.

Since this function simply wraps the passed function in a new function definition, it will always succeed. However, the resulting wrapper function may raise exceptions depending on its execution. For more information about this behavior, see the entry for :code:`Python tp()`, as the wrapper function is a pre-populated call to this function.

- If the wrapped :code:`function` returns :code:`None`, then :code:`yottadb.YDB_OK` will be returned to the wrapping :code:`Python tp()` call
- If the wrapped :code:`function` returns any other value, this value will be returned directly to the wrapping :code:`Python tp()` call without modification
- If the wrapped :code:`function` raises :code:`yottadb.YDBTPRestart`, then :code:`yottadb.YDB_TP_RESTART` will be returned to the wrapping :code:`Python tp()` call

.. code-block:: python

    # Wrap a simple function with the transaction
    @yottadb.transaction
    def my_transaction(key1: yottadb.Key, value1: str, key2: yottadb.Key, value2: str) -> None:
         key1.value = value1
         key2.value = value2

    # Create Key objects to pass to the newly defined and decorated my_transaction() function
    key1 = yottadb.Key("^myglobal")["sub1"]["sub2"]
    key2 = yottadb.Key("^myglobal")["sub1"]["sub3"]

    # Call the function decorated with transaction()
    status = my_transaction(key1, "val1", key2, "val2")
    # Handle possible results of the call as one would handle results of a call to tp()
    if yottadb.YDB_OK == status:
        # Transaction successful
        print(key1.value)  # Prints 'val1'
        print(key2.value)  # Prints 'val2'
    else if yottadb.YDB_TP_RESTART == status:
        # Restart the transaction
        print(status)
    else if yottadb.YDB_TP_ROLLBACK == status:
        # Do not commit the transaction
        print(status)
    else:
        # Another error occurred
        # Do not commit the transaction
        print(status)

++++++++++++++++
Python zwr2str()
++++++++++++++++

.. code-block:: python

    def zwr2str(string: AnyStr) -> bytes

As a wrapper for the C function :ref:`ydb-zwr2str-s-st-fn`, :code:`zwr2str` takes a string in ZWRITE format and returns it as a regular string. This method is the inverse of `Python str2zwr()`_.

- If :code:`string` has errors and is not in valid :ref:`zwrite-format`, a :code:`YDBError` exception will be raised indicating the error code returned by :ref:`ydb-zwr2str-s-st-fn` e.g., :code:`yottadb.YDB_ERR_INVZWRITECHAR == YDBError.code()`.
- If the underlying :ref:`ydb-zwr2str-s-st-fn` call returns any other error, the function raises an exception containing the error code and message.
- Otherwise, return the value of :code:`string` in :ref:`zwrite-format`.

Note that the return value of this function is always a :code:`bytes` object, reflecting the fact that YottaDB stores all values as binary data, such that a global or local variable node value is not guaranteed to be a valid UTF-8 string. Accordingly, the return value of this function is not guaranteed to be castable to a Python :code:`str` object.

.. code-block:: python

    print(yottadb.zwr2str(b'"X"_$C(0)_"ABC"'))  # Prints b'X\x00ABC'

----------------------------
YottaDB Key class properties
----------------------------

++++++++
Key.data
++++++++

.. code-block:: python

    @property
    def data(self) -> int

Matching `Python data()`_, the :code:`Key.data` property method returns the result of :ref:`ydb-data-s-st-fn` (0, 1, 10, or 11).

In the event of an error in :ref:`ydb-data-s-st-fn`, a :code:`YDBError` exception is raised reflecting YottaDB error code and message.

.. code-block:: python

    key = yottadb.Key("mylocal")["sub1"]["sub2"]
    key.value = "test"
    print(key.data) # Prints 1
    print(key.parent.data) # Prints 10
    print(key["sub3"].data) # Prints 0
    key["sub3"].value = "test2"
    print(key["sub3"].data) # Prints 1
    print(key.data) # Prints 11

+++++++++++++
Key.has_value
+++++++++++++

.. code-block:: python

    @property
    def has_value(self) -> bool

:code:`Key.has_value` provides a class property that returns :code:`True` or :code:`False` depending on whether the global or local variable node represented by the given :code:`Key` object has a value or does not have a value, respectively.

In the event of an error in the underlying :ref:`ydb-data-s-st-fn` call, a :code:`YDBError` exception is raised reflecting the error code and message.

This property references :code:`Key.data` internally, and is provided for convenience.

.. code-block:: python

    key = yottadb.Key("mylocal")["sub1"]["sub2"]
    print(key.has_value) # Prints False
    key.value = "test"
    print(key.has_value) # Prints True

++++++++++++
Key.has_tree
++++++++++++

.. code-block:: python

    @property
    def has_tree(self) -> bool

:code:`Key.has_tree` provides a class property that returns :code:`True` or :code:`False` depending on whether the global or local variable node represented by the given :code:`Key` object has a (sub)tree or does not have a (sub)tree, respectively.

In the event of an error in the underlying :ref:`ydb-data-s-st-fn` call, a :code:`YDBError` exception is raised reflecting the underlying YottaDB error code and message.

This property references :code:`Key.data` internally, and is provided for convenience.

.. code-block:: python

    key = yottadb.Key("mylocal")["sub1"]["sub2"]
    key.value = "test"
    print(key.has_tree) # Prints False
    print(key.parent.has_tree) # Prints True

+++++++++++++
Key.subsarray
+++++++++++++

.. code-block:: python

    @property
    def subsarray(self) -> List[AnyStr]

:code:`Key.subsarray` provides a class property that returns the subscripts of the global or local variable node represented by the given :code:`Key` object as a :code:`List` of :code:`str` or :code:`bytes` objects, depending on whether the :code:`Key` was constructed using :code:`str` or :code:`bytes` objects to specify the variable name or subscripts.

.. code-block:: python

    key = yottadb.Key("mylocal")["sub1"]["sub2"]
    print(key.subsarray) # Prints ["sub1", "sub2"]

++++++++++++++
Key.subscripts
++++++++++++++

.. code-block:: python

    @property
    def subscripts(self) -> Generator

:code:`Key.subscripts` provides a class property that returns a Generator for iterating over subscripts at the level of the global or local variable node represented by the given :code:`Key` object. Each iteration will :code:`yield` the result of a call to :code:`subscript_next`, i.e. a :code:`bytes` object representing a YottaDB subscript.

In the event of an error in an underlying :ref:`ydb-subscript-next-s-st-fn` call, a :code:`YDBError` exception is raised reflecting the underlying YottaDB error code and message.

Example

.. code-block:: python

    key = yottadb.Key("^myglobal")["sub1"]["sub2"]
    for subscript in key.subscripts:
        print(subscript)  # Prints the next subscript at the "sub2" subscript level of the key

++++++++++++++++++
Key.subsarray_keys
++++++++++++++++++

.. code-block:: python

    @property
    def subsarray_keys(self) -> List["Key"]:

:code:`Key.subsarray_keys` provides a class property that returns the subscripts of the global or local variable node represented by the given :code:`Key` object as a :code:`List` of other :code:`Key` objects. Each of these :code:`Key` objects represents a full YottaDB global or local variable node (variable name and subscripts).

.. code-block:: python

    key = yottadb.Key("mylocal")["sub1"]["sub2"]
    print(key.subsarray_keys) # Prints [Key:mylocal("sub1"), Key:mylocal("sub1","sub2")]

+++++++++
Key.value
+++++++++

.. code-block:: python

    @property
    def value(self) -> Optional[AnyStr]

    @value.setter
    def value(self, value: AnyStr) -> None

Acting as a class property, :code:`Key.value` wraps both :ref:`ydb-get-s-st-fn` and :ref:`ydb-set-s-st-fn` to set or get the value at the global or local variable node or intrinsic special variable represented by the given :code:`Key` object.

In the event of an error in the underlying :ref:`ydb-get-s-st-fn` call, a :code:`YDBError` exception is raised reflecting the underlying YottaDB error code and message.

Example:

.. code-block:: python

    key = yottadb.Key("^myglobal")
    key.value = "such wow"
    print(key.value)  # Prints "such wow"

+++++++++++++++
Key.varname_key
+++++++++++++++

.. code-block:: python

    @property
    def varname_key(self) -> Optional["Key"]:

:code:`Key.varname_key` provides a class property that returns a :code:`Key` object for the unsubscripted global or local variable node represented by the given :code:`Key` object as a :code:`str` object.

.. code-block:: python

    key = yottadb.Key("mylocal")["sub1"]["sub2"]
    print(key.varname_key) # Prints Key:mylocal

+++++++++++
Key.varname
+++++++++++

.. code-block:: python

    @property
    def varname(self) -> AnyStr

:code:`Key.varname` provides a class property that returns the name of the global or local variable node represented by the given :code:`Key` object as a :code:`bytes` or :code:`str` object, depending on how the :code:`Key` variable name was specified.

.. code-block:: python

    key = yottadb.Key("mylocal")["sub1"]["sub2"]
    print(key.varname) # Prints 'mylocal'

-----------------------------------
YottaDB Key class regular methods
-----------------------------------

+++++++++++++++++
Key.delete_node()
+++++++++++++++++

.. code-block:: python

    def delete_node(self) -> None

Matching `Python delete_node()`_, :code:`Key.delete_node()` wraps :ref:`ydb-delete-s-st-fn` with a value of :code:`YDB_DEL_NODE` for :code:`deltype` to delete a local or global variable node, specifying that only the node should be deleted, leaving the (sub)tree untouched.

In the event of an error in the underlying :ref:`ydb-delete-s-st-fn` call, a :code:`YDBError` exception is raised reflecting the underlying YottaDB error code and message.

.. code-block:: python

    key = yottadb.Key("mylocal")["sub1"]["sub2"]
    key.value = "test"
    print(key.value) # Prints b'test'
    key.delete_node()
    print(key.value) # Prints None

+++++++++++++++++
Key.delete_tree()
+++++++++++++++++

.. code-block:: python

    def delete_tree(self) -> None

Matching `Python delete_tree()`_, :code:`Key.delete_tree()` wraps :ref:`ydb-delete-s-st-fn` with a value of :code:`YDB_DEL_TREE` for :code:`deltype` to delete the local or global variable node represented by the :code:`Key` object, along with its (sub)tree.

In the event of an error in the underlying :ref:`ydb-delete-s-st-fn` call, a :code:`YDBError` exception is raised reflecting the underlying YottaDB error code and message.

.. code-block:: python

    key = yottadb.Key("mylocal")["sub1"]["sub2"]
    print(key.data) # Prints 0
    key.value = "test"
    print(key.data) # Prints 1
    print(key.parent.data) # Prints 10
    key.parent.delete_tree()
    print(key.data) # Prints 0
    print(key.parent.data) # Prints 0

+++++++++
Key.get()
+++++++++

.. code-block:: python

    def get(self) -> Optional[bytes]

Matching `Python get()`_, :code:`Key.get()` wraps :ref:`ydb-get-s-st-fn` to retrieve the value of the local or global variable node represented by the given :code:`Key` object, returning it as a :code:`bytes` object.

In the event of an error in the underlying :ref:`ydb-get-s-st-fn` call, a :code:`YDBError` exception is raised reflecting the underlying YottaDB error code and message.

.. code-block:: python

    key = yottadb.Key("mylocal")["sub1"]["sub2"]
    print(key.get()) # Prints None
    key.set("test")
    print(key.get()) # Prints b'test'

++++++++++
Key.incr()
++++++++++

.. code-block:: python

    def incr(self, increment: Union[int, float, str, bytes] = "1") -> bytes

Matching `Python incr()`_, :code:`Key.incr()` wraps :ref:`ydb-incr-s-st-fn` to atomically increment the global or local variable node represented by the :code:`Key` object coerced to a number, with :code:`increment` coerced to a number. If successful, the call returns the resulting value as a :code:`bytes` object.

- If :code:`increment` is omitted, a value of 1 is used by default.
- If :ref:`ydb-incr-s-st-fn` returns an error such as NUMOFLOW, an exception will be raised.
- Otherwise, it increments the specified node and returns the resulting value.
- In the event of any other error in the underlying :ref:`ydb-incr-s-st-fn` call, a :code:`YDBError` exception is raised reflecting the underlying YottaDB error code and message.

If unspecified, the default increment is 1. Note that the value of the empty string coerced to an integer is zero, but 1 is a more useful default value for an omitted parameter in this case.

.. code-block:: python

    key = yottadb.Key("mylocal")["sub1"]["sub2"]
    print(key.value) # Prints None
    print(key.incr()) # Prints b'1'
    print(key.incr()) # Prints b'2'

+++++++++++++++
Key.load_json()
+++++++++++++++

.. code-block:: python

    def load_json(self, key: Key = None, spaces: str = "") -> object

The inverse of `Key.save_json()`_, ``Key.load_json()`` retrieves JSON data stored under the YottaDB database node represented by the calling `Key` object, and returns it as a Python object. For example:

.. code-block:: python

    import yottadb
    import requests
    import json


    response = requests.get("https://rxnav.nlm.nih.gov/REST/relatedndc.json?relation=product&ndc=0069-3060")
    original_json = json.loads(response.content)
    key = yottadb.Key("^rxnorm")
    key.delete_tree()
    key.save_json(original_json)

    saved_json = key.load_json()
    key["ndcInfoList"]["ndcInfo"]["3"]["ndc11"].value = b'00069306087'
    revised_json = key.load_json()

    with open('original.json', 'w', encoding='utf-8') as f:
        json.dump(original_json, f, sort_keys = True, indent=4)
    with open('saved.json', 'w', encoding='utf-8') as f:
        json.dump(saved_json, f, sort_keys = True, indent=4)
    with open('revised.json', 'w', encoding='utf-8') as f:
        json.dump(revised_json, f, sort_keys = True, indent=4)

+++++++++++++++
Key.load_tree()
+++++++++++++++

.. code-block:: python

   def load_tree(self) -> dict

The :code:`Key.load_tree()` method retrieves the entire subtree stored under the database node represented by the given :code:`Key` and stores it in a series of nested Python dictionaries.

The nested dictionaries are structured using YottaDB subscripts as keys, with node values stored under a :code:`"value"` key at the appropriate subscript level.

For example, these YottaDB database nodes:

.. code-block::

   ^test4="test4"
   ^test4("sub1")="test4sub1"
   ^test4("sub1","subsub1")="test4sub1subsub1"
   ^test4("sub1","subsub2")="test4sub1subsub2"
   ^test4("sub1","subsub3")="test4sub1subsub3"
   ^test4("sub2")="test4sub2"
   ^test4("sub2","subsub1")="test4sub2subsub1"
   ^test4("sub2","subsub2")="test4sub2subsub2"
   ^test4("sub2","subsub3")="test4sub2subsub3"
   ^test4("sub3")="test4sub3"
   ^test4("sub3","subsub1")="test4sub3subsub1"
   ^test4("sub3","subsub2")="test4sub3subsub2"
   ^test4("sub3","subsub3")="test4sub3subsub3"

To convert these nodes into a Python dictionary, :code:`Key.load_tree()` can be used like so:

.. code-block:: python

    import yottadb


    key = yottadb.Key("^test4")
    print(key.load_tree())

This will produce the following dictionary (formatted for clarity):

.. code-block:: python

    {
        'value': 'test4',
        'sub1': {
            'value': 'test4sub1',
            'subsub1': {
                'value': 'test4sub1subsub1'
            },
            'subsub2': {
                'value': 'test4sub1subsub2'
            },
            'subsub3': {
                'value': 'test4sub1subsub3'
            }
        },
        'sub2': {
            'value': 'test4sub2',
            'subsub1': {
                'value': 'test4sub2subsub1'
            },
            'subsub2': {
                'value': 'test4sub2subsub2'
            },
            'subsub3': {
                'value': 'test4sub2subsub3'
            }
        },
        'sub3': {
            'value': 'test4sub3',
            'subsub1': {
                'value': 'test4sub3subsub1'
            },
            'subsub2': {
                'value': 'test4sub3subsub2'
            },
            'subsub3': {
                'value': 'test4sub3subsub3'
            }
        }
    }

++++++++++
Key.lock()
++++++++++

.. code-block:: python

    def lock(self, timeout_nsec: int = 0) -> None

Matching `Python lock()`_, :code:`Key.lock()` releases all lock resources currently held and then attempts to acquire the named lock resource represented by the given :code:`Key` object. In other words, :code:`Key.lock()` will attempt to acquire a lock for the single key represented by the given :code:`Key` object.

- If :code:`timeout_nsec` is omitted, a value of 0 is used by default.
- If :code:`timeout_nsec` exceeds :code:`yottadb.YDB_MAX_TIME_NSEC`, a :code:`yottadb.YDBError` exception will be raised where :code:`yottadb.YDB_ERR_TIME2LONG == YDBError.code()`
- If it is able to acquire the lock resource within :code:`timeout_nsec` nanoseconds, it returns holding the lock, otherwise it raises a :code:`YDBTimeoutError` exception. If :code:`timeout_nsec` is zero, the function makes exactly one attempt to acquire the lock.
- If the underlying :ref:`ydb-lock-s-st-fn` call returns any other error, the function raises a YDBError exception containing the error code and message.

The following example provides a demonstration of basic :code:`Key` locking operations. The example locks the given :code:`Key`, then attempts to increment the lock on it by calling a separately defined :code:`lock_value()` helper function as a separate Python process. Due to the initial locking of the key, this :code:`lock_value()` fails with an exit code of 1. Next, all locks are released and a new :code:`lock_value()` process is spawned that again attempts to increment the lock on the key. Since all locks were previously released, this new attempt succeeds and the process exits with a 0 exit code.

.. code-block:: python

    import multiprocessing
    import datetime


    # Lock a value in the database
    def lock_value(key: Union[yottadb.Key, tuple], interval: int = 2, timeout: int = 1):
        if isinstance(key, yottadb.Key):
            varname = key.varname
            subsarray = key.subsarray
        else:
            varname = key[0]
            subsarray = key[1]
        if len(subsarray) == 0:
            subsarray = None

        has_lock = False
        try:
            yottadb.lock_incr(varname, subsarray, timeout_nsec=(timeout * 1_000_000_000))
            print("Lock Success")
            has_lock = True
        except yottadb.YDBTimeoutError:
            print("Lock Failed")
            sys.exit(1)
        except Exception as e:
            print(f"Lock Error: {repr(e)}")
            sys.exit(2)

        if has_lock:
            time.sleep(interval)
            yottadb.lock_decr(varname, subsarray)
            if timeout != 0 or interval != 0:
                print("Lock Released")

        sys.exit(0)


    key = yottadb.Key("^test4")["sub1"]["sub2"]
    # Attempt to get the lock
    key.lock()
    # Attempt to increment/decrement the lock
    process = multiprocessing.Process(target=lock_value, args=(key,))
    process.start()
    process.join()
    print(process.exitcode)  # Prints 1
    # Release all locks
    yottadb.lock()
    # Attempt to increment/decrement the lock
    process = multiprocessing.Process(target=lock_value, args=(key,))
    process.start()
    process.join()
    print(process.exitcode)  # Prints 0

+++++++++++++++
Key.lock_decr()
+++++++++++++++

.. code-block:: python

    def lock_decr(self) -> None

Matching `Python lock_decr()`_ :code:`Key.lock_decr()` wraps :ref:`ydb-lock-decr-s-st-fn` to decrement the count of the lock name represented by the given :code:`Key` object, releasing it if the count goes to zero or ignoring the invocation if the process does not hold the lock.

In the event of an error in the underlying :ref:`ydb-lock-decr-s-st-fn` call, a :code:`YDBError` exception is raised reflecting the underlying YottaDB error code and message.

.. code-block:: python

    import multiprocessing
    import datetime


    key = yottadb.Key("^myglobal")["sub1"]
    # For the definition of lock_value(), see the entry for Key.lock()
    process = multiprocessing.Process(target=lock_value, args=(key,))
    process.start()
    time.sleep(0.5)  # Wait for new process to spawn

    t1 = datetime.datetime.now()
    yottadb.Key("mylocal").lock_incr()
    t2 = datetime.datetime.now()

    time_elapse = t2.timestamp() - t1.timestamp()
    print(time_elapse)  # Prints number of seconds elapsed
    key.lock_decr()
    time.sleep(0.5)  # Wait for lock to release
    process.join()

+++++++++++++++
Key.lock_incr()
+++++++++++++++

.. code-block:: python

    def lock_incr(self, timeout_nsec: int = 0) -> None

Matching `Python lock_incr()`_, :code:`Key.lock_incr()` wraps :ref:`ydb-lock-incr-s-st-fn` to attempt to acquire the lock resource name represented by the given :code:`Key` object without releasing any locks the process already holds.

- If :code:`timeout_nsec` is omitted, a value of 0 is used by default.
- If the process already holds the named lock resource, the method increments its count and returns.
- If :code:`timeout_nsec` exceeds :code:`yottadb.YDB_MAX_TIME_NSEC`, the method raises a TIME2LONGError exception.
- If it is able to acquire the lock resource within :code:`timeout_nsec` nanoseconds, it returns holding the lock, otherwise it raises a YDBTimeoutError exception. If :code:`timeout_nsec` is zero, the method makes exactly one attempt to acquire the lock.

For an example of how to use this function, see `Key.lock_decr()`_.

+++++++++++++++
Key.node_next()
+++++++++++++++

.. code-block:: python

    def node_next(varname: AnyStr, subsarray: Tuple[AnyStr] = ()) -> Tuple[bytes, ...]

Matching `Python node_next()`_, :code:`Key.node_next()` wraps :ref:`ydb-node-next-s-st-fn` to facilitate traversal of the local or global variable tree represented by the given :code:`Key` object.

- If there is a next node, it returns the subscripts of that next node as a tuple of Python :code:`bytes` objects.
- If there is no node following the specified node, a :code:`yottadb.YDBNodeEnd` exception will be raised.
- In the event of an error in the underlying :ref:`ydb-node-next-s-st-fn` call, a :code:`YDBError` exception is raised reflecting the underlying YottaDB error code and message.

++++++++++++++++++
Key.replace_tree()
++++++++++++++++++

.. code-block:: python

    def replace_tree(self, tree: dict)

``Key.replace_tree()`` stores data from a nested Python dictionary in YottaDB, replacing the tree in the database with the one defined by the ``tree`` argument. The dictionary must have been previously created using the ``Key.load_tree()`` method, or otherwise match the format used by that method.

Note that this method will delete any nodes and subtrees that exist in the database but are absent from ``tree``.

+++++++++++++++
Key.save_json()
+++++++++++++++

.. code-block:: python

    def save_json(self, json: object, key: Key = None)

``Key.save_json()`` saves JSON data stored in a Python object under the YottaDB node represented by the calling ``Key`` object. For example:

.. code-block:: python

    import yottadb
    import requests
    import json

    response = requests.get("https://rxnav.nlm.nih.gov/REST/relatedndc.json?relation=product&ndc=0069-3060")
    json_data = json.loads(response.content)
    key = yottadb.Key("^rxnav")
    key.save_json(json_data)

This saved JSON data can subsequently be loaded with `Key.load_json()`_.

++++++++++++++++
Key.save_tree()
++++++++++++++++

.. code-block:: python

    def save_tree(self, tree: dict, key: Key = None)

The :code:`Key.save_tree()` method performs the reverse operation of the :code:`Key.load_tree()` method, and stores a Python dictionary representing a YottaDB tree or subtree in the database.

The dictionary passed to :code:`Key.save_tree()` must have been previously generated by a call to :code:`Key.load_tree()` or otherwise maintain the same format. Any such dictionary may, however, be modified after its creation and subsequently passed to :code:`Key.save_tree()`.

For example, consider again these database nodes:

.. code-block::

   ^test4="test4"
   ^test4("sub1")="test4sub1"
   ^test4("sub1","subsub1")="test4sub1subsub1"
   ^test4("sub1","subsub2")="test4sub1subsub2"
   ^test4("sub1","subsub3")="test4sub1subsub3"
   ^test4("sub2")="test4sub2"
   ^test4("sub2","subsub1")="test4sub2subsub1"
   ^test4("sub2","subsub2")="test4sub2subsub2"
   ^test4("sub2","subsub3")="test4sub2subsub3"
   ^test4("sub3")="test4sub3"
   ^test4("sub3","subsub1")="test4sub3subsub1"
   ^test4("sub3","subsub2")="test4sub3subsub2"
   ^test4("sub3","subsub3")="test4sub3subsub3"

These can be retrieved and stored in a dictionary using :code:`Key.load_tree()`, modified, and then stored again in the database using :code:`Key.save_tree()`:

.. code-block:: python

    import yottadb


    key = yottadb.Key("^test4")
    key_dict = key.load_tree()

    key_dict["value"] = "test4new"
    key_dict["sub3"]["subsub3"] = "test4sub3subsub3new"

The database will now contain the following nodes:

.. code-block::

    ^test4="test4new"
    ^test4("sub1")="test4sub1"
    ^test4("sub1","subsub1")="test4sub1subsub1"
    ^test4("sub1","subsub2")="test4sub1subsub2"
    ^test4("sub1","subsub3")="test4sub1subsub3"
    ^test4("sub2")="test4sub2"
    ^test4("sub2","subsub1")="test4sub2subsub1"
    ^test4("sub2","subsub2")="test4sub2subsub2"
    ^test4("sub2","subsub3")="test4sub2subsub3"
    ^test4("sub3")="test4sub3subsub3new"
    ^test4("sub3","subsub1")="test4sub3subsub1"
    ^test4("sub3","subsub2")="test4sub3subsub2"
    ^test4("sub3","subsub3")="test4sub3subsub3"


+++++++++
Key.set()
+++++++++

.. code-block:: python

    def set(self, value: AnyStr = "") -> None

Matching `Python set()`_, :code:`Key.set()` wraps :ref:`ydb-set-s-st-fn` to set the local or global variable node represented by the given :code:`Key` object to the value specified by :code:`value`.

In the event of an error in the underlying :ref:`ydb-set-s-st-fn` call, a :code:`YDBError` exception is raised reflecting the underlying YottaDB error code and message.

.. code-block:: python

    key = yottadb.Key("mylocal")["sub1"]["sub2"]
    print(key.get()) # Prints None
    key.set("test")
    print(key.get()) # Prints b'test'

++++++++++++++++++++
Key.subscript_next()
++++++++++++++++++++

.. code-block:: python

    def subscript_next(self, reset: bool = False) -> bytes

Matching `Python subscript_next()`_, :code:`Key.subscript_next()` wraps :ref:`ydb-subscript-next-s-st-fn` to facilitate traversal of the local or global variable sub-tree at the subscript level represented by the given :code:`Key` object. A node or subtree does not have to exist at the specified key. The :code:`reset` parameter may be used to instruct :code:`Key.subscript_next()` to begin traversal at the first subscript at the current subscript level, even if :code:`Key.subscript_next()` has already traversed over it.

- If :code:`reset` is omitted, it is set to :code:`False` by default.
- At the level of the last subscript, if there is a next subscript with a node and/or a subtree that subscript will be returned as a :code:`bytes` object.
- If there is no next node or subtree at that level of the subtree, a :code:`yottadb.YDBNodeEnd` exception will be raised.
- A :code:`yottadb.YDBNodeEnd` exception will be raised on all subsequent calls to :code:`Key.subscript_next()` after exhausting all nodes and/or subtrees as described above
- To enable re-traversal of the current subscript level, the user may pass a value of :code:`True` to :code:`Key.subscript_next()`, which will cause the function to return the next subscript at the current level, as if :code:`Key.subscript_next()` was not previously called and a :code:`yottadb.YDBNodeEnd` exception was not previously raised.
- In the event of any other error in the underlying :ref:`ydb-subscript-next-s-st-fn` call, a :code:`YDBError` exception is raised reflecting the underlying YottaDB error code and message.

The following example sets a value on multiple nodes at the first subscript level of a local variable, then iterates over each subscript at this level in two ways. First, the subscripts are iterated over using a :code:`Key.subscript_next()` manually in a succession of hard-coded calls.  Then, the starting subscript of the iteration is reset after iterating over all subscripts at that level. Finally, the subscripts are again iterated over, but this time using a :code:`while` loop instead of hard-coded individual calls to :code:`Key.subscript_next()`.

.. code-block:: python

    key = yottadb.Key("testsubsnext")
    key["sub1"] = "1"
    key["sub2"] = "2"
    key["sub3"] = "3"
    key["sub4"] = "4"

    print(key.subscript_next())  # Prints "sub1"
    print(key.subscript_next())  # Prints "sub2"
    print(key.subscript_next())  # Prints "sub3"
    print(key.subscript_next())  # Prints "sub4"

    try:
        key.subscript_next()
    except yottadb.YDBNodeEnd:
        print(key[key.subscript_next(reset=True)].value)  # Prints b"1"
        print(key[key.subscript_next()].value)  # Prints b"2"
        print(key[key.subscript_next()].value)  # Prints b"3"
        print(key[key.subscript_next()].value)  # Prints b"4"

    try:
        sub = key.subscript_next(reset=True)  # Resets starting subscript to ""
    except yottadb.YDBNodeEnd:
        # There are subscripts defined for the given Key, so a reset of subscript_next's
        # next subscript to the default starting subscript of "" should not return
        # a YDBError of YDB_ERR_NODEEND. If, on the other hand, there were no subscripts for the
        # given Key, subscript.next() would always raise a YDBError of YDB_ERR_NODEEND, regardless of
        # whether the `reset` argument is set to True or not.
        assert False

    count = 1
    print(sub)  # Prints "sub1"
    while True:
        try:
            sub = key.subscript_next()
            count += 1
            assert sub == "sub" + str(count)
        except yottadb.YDBNodeEnd:
            break

++++++++++++++++++++++++
Key.subscript_previous()
++++++++++++++++++++++++

.. code-block:: python

    def subscript_previous(self, reset: bool = False) -> bytes

Matching `Python subscript_previous()`_, :code:`Key.subscript_previous()` wraps :ref:`ydb-subscript-previous-s-st-fn` to facilitate reverse traversal of the local or global variable sub-tree at the subscript level represented by the given :code:`Key` object. A node or subtree does not have to exist at the specified key.

- If :code:`reset` is omitted, it is set to :code:`False` by default.
- At the level of the last subscript, if there is a previous subscript with a node and/or a subtree that subscript will be returned as a :code:`bytes` object.
- If there is no previous node or subtree at that level of the subtree, a :code:`yottadb.YDBNodeEnd` exception will be raised.
- In the event of an error in the underlying :ref:`ydb-subscript-previous-s-st-fn` call, a :code:`YDBError` exception is raised reflecting the underlying YottaDB error code and message.

The following example sets a value on multiple nodes at the first subscript level of a local variable, then iterates over each subscript at this level in two ways. First, the subscripts are iterated over using a :code:`Key.subscript_previous()` manually in a succession of hard-coded calls.  Then, the starting subscript of the iteration is reset after iterating over all subscripts at that level. Finally, the subscripts are again iterated over, but this time using a :code:`while` loop instead of hard-coded individual calls to :code:`Key.subscript_previous()`.

.. code-block:: python

    key = yottadb.Key("testsubsprevious")
    key["sub1"] = "1"
    key["sub2"] = "2"
    key["sub3"] = "3"
    key["sub4"] = "4"

    print(key.subscript_previous())  # Prints "sub4"
    print(key.subscript_previous())  # Prints "sub3"
    print(key.subscript_previous())  # Prints "sub2"
    print(key.subscript_previous())  # Prints "sub1"

    try:
        key.subscript_previous()
    except yottadb.YDBNodeEnd:
        print(key[key.subscript_previous(reset=True)].value)  # Prints b"4"
        print(key[key.subscript_previous()].value)  # Prints b"3"
        print(key[key.subscript_previous()].value)  # Prints b"2"
        print(key[key.subscript_previous()].value)  # Prints b"1"

    try:
        sub = key.subscript_previous(reset=True)  # Resets starting subscript to ""
    except yottadb.YDBNodeEnd:
        # There are subscripts defined for the given Key, so a reset of subscript_previous's
        # previous subscript to the default starting subscript of "" should not return
        # a YDBError of YDB_ERR_NODEEND. If, on the other hand, there were no subscripts for the
        # given Key, subscript.previous() would always raise a YDBError of YDB_ERR_NODEEND, regardless of
        # whether the `reset` argument is set to True or not.
        assert False

    count = 4
    print(sub)  # Prints "sub4"
    while True:
        try:
            sub = key.subscript_previous()
            count -= 1
            assert sub == "sub" + str(count)
        except yottadb.YDBNodeEnd as e:
            break

-----------------------------------
YottaDB Key class magic methods
-----------------------------------

++++++++++++
Key.__eq__()
++++++++++++

.. code-block:: python

    def __eq__(self, other) -> bool

The :code:`Key.__eq__()` magic method allows for easy comparison between two :code:`Key` objects, using the Python :code:`==` operator. If the two :code:`Key` objects represent the same YottaDB local or global variable node, then :code:`Key.__eq__()` will return :code:`True`, otherwise it will return :code:`False`. For example:

.. code-block:: python

    key = yottadb.Key("^myglobal")["sub1"]["sub2"]
    key2 = yottadb.Key("^myglobal")["sub1"]["sub2"]
    print(key == key2) # Prints True

    key = yottadb.Key("^myglobal")["sub1"]["sub2"]
    key2 = yottadb.Key("^myglobal")["sub1"]
    print(key == key2) # Prints False

+++++++++++++++++
Key.__getitem__()
+++++++++++++++++

.. code-block:: python

    def __getitem__(self, item)

The :code:`Key.__getitem__()` magic method creates a new :code:`Key` object by adding the specified :code:`item` as an additional subscript on the given :code:`Key` object.

This enables usage of the standard index bracket syntax (:code:`[]`) for the transparent production of new :code:`Key` objects for both in-line, one-off usage and for the creation of new objects for later use.

For example:

.. code-block:: python

    key1 = yottadb.Key("^myglobal")
    key2 = key1["sub1"]
    key3 = key2["sub2"]
    key4 = key2["sub3"]
    print(str(key1)) # Prints '^myglobal'
    print(str(key2)) # Prints '^myglobal("sub1")'
    print(str(key3)) # Prints '^myglobal("sub1","sub2")'
    print(str(key4)) # Prints '^myglobal("sub1","sub3")'

++++++++++++++
Key.__iadd__()
++++++++++++++

.. code-block:: python

    def __iadd__(self, num: Union[int, float, str, bytes]) -> Optional["Key"]

The :code:`Key.__iadd__()` magic method allows for easy incrementation of the YottaDB local or global variable node represented by the :code:`Key` object, using the Python :code:`+=` operator. For example:

In the event of an error in the underlying :ref:`ydb-incr-s-st-fn` call, a :code:`YDBError` exception is raised reflecting the underlying YottaDB error code and message.

.. code-block:: python

    key = yottadb.Key("^myglobal")["sub1"]["sub2"]
    key.value = 2
    key += 2
    print(key.value) # Prints '4'

++++++++++++++
Key.__init__()
++++++++++++++

.. code-block:: python

    def __init__(self, name: AnyStr, parent: Key = None) -> None

The :code:`Key.__init__()` function acts as the constructor for the :code:`Key` class and is used to create new :code:`Key` objects.

Note: Users should not attempt to set :code:`parent`, but omit this parameter. This is because :code:`parent` is used implicitly by several :code:`Key` methods and not intended for use by users. If a user nonetheless passes a valid :code:`parent` argument, i.e. a :code:`Key` object, then a new :code:`Key` will be generated where :code:`name` is appended as an additional subscript at the end of the subscript array of the :code:`parent` :code:`Key`.

The following errors are possible during :code:`Key` creation:
- :code:`TypeError`: when :code:`name` is not of type :code:`bytes` or :code:`str`.
- :code:`TypeError`: when :code:`parent` is not of type :code:`Key` or :code:`None`.
- :code:`ValueError`: if a subscript array is specified for a YottaDB Intrinsic Special Variable (ISV), i.e. :code:`parent` is not :code:`None` and :code:`name` specifies an ISV.
- :code:`ValueError`: if the subscript array, passed via :code:`parent`, exceeds :code:`yottadb.YDB_MAX_SUBS` in length.

.. code-block:: python

    key = yottadb.Key("^myglobal")["sub1"]["sub2"]

    # Set `parent` explicitly (not recommended)
    key = yottadb.Key("sub3", parent=key)  # Raises TypeError for non-Key `parent` argument
    print(str(key)) # Prints '^myglobal("sub1","sub2","sub3")'
    key = yottadb.Key("^myglobal", parent="not a key object")  # Raises TypeError for non-Key `parent` argument

    # Proper ISV Key creation
    key = yottadb.Key("$ZSTATUS")
    print(str(key)) # Prints '$ZSTATUS'
    # Invalid ISV Key creation
    key = yottadb.Key("$ZSTATUS")["sub1"]["sub2"]  # Raises ValueError for subscripted ISV

++++++++++++++
Key.__isub__()
++++++++++++++

.. code-block:: python

    def __isub__(self, num: Union[int, float, str, bytes]) -> Optional["Key"]

The :code:`Key.__isub__()` magic method allows for easy decrementation of the YottaDB local or global variable node represented by the :code:`Key` object, using the Python :code:`-=` operator. For example:

In the event of an error in the underlying :ref:`ydb-incr-s-st-fn` call, a :code:`YDBError` exception is raised reflecting the underlying YottaDB error code and message.

.. code-block:: python

    key = yottadb.Key("^myglobal")["sub1"]["sub2"]
    key.value = 2
    key -= 2
    print(key.value) # Prints '0'

++++++++++++++
Key.__iter__()
++++++++++++++

.. code-block:: python

    def __iter__(self) -> Generator

The :code:`Key.__iter__()` magic method allows for easy iteration over the subscripts at the subscript level of the given :code:`Key` object, beginning from the first subscript. For example,

In the event of an error in an underlying :ref:`ydb-subscript-next-s-st-fn` call, a :code:`YDBError` exception is raised reflecting the underlying YottaDB error code and message.

.. code-block:: python

    fruits = yottadb.Key("^inventory")["fruits"]
    fruits["apples"] = 'in stock'
    fruits["bananas"] = 'in stock'
    fruits["oranges"] = 'sold out'
    fruits["kiwis"] = 'in stock'
    fruits["398576986"] = "in stock"
    fruits["839587329"] = "sold out"
    fruits[b"\x80pomegranates"] = b"\x80sold out"  # byte strings that are not ASCII or valid UTF-8 are also supported
    for fruit in fruits:
        print(f"{fruit}: {fruit.value}")

    # Prints:
    # inventory("fruits",398576986): b'in stock'
    # inventory("fruits",839587329): b'sold out'
    # inventory("fruits","apples"): b'in stock'
    # inventory("fruits","bananas"): b'in stock'
    # inventory("fruits","kiwis"): b'in stock'
    # inventory("fruits","oranges"): b'sold out'
    # inventory("fruits",$ZCH(128)_"pomegranates"): b'\x80sold out'

++++++++++++++
Key.__repr__()
++++++++++++++

.. code-block:: python

    def __repr__(self) -> str

The :code:`Key.__repr__()` magic method returns a Python-readable representation of the :code:`Key` object. Specifically, :code:`Key.__repr__()` produces a representation of the :code:`Key` object that can be passed to the built-in :code:`eval()` function to produce a new instance of the object.

Note, however, that this cannot be done with perfect reliability, as successful object reproduction will depend on how the :code:`yottadb` module is imported. To provide flexibility, :code:`Key.__repr__()` produces a representation as if the :code:`Key` class is imported directly, i.e. `from yottadb import Key`. This allows for :code:`eval()` to be used to reproduce a :code:`Key` object, provided that the :code:`str` passed to it includes any module import prefixes qualifying the :code:`Key` name. For example:

.. code-block:: python

    import yottadb

    key = yottadb.Key("^myglobal")["sub1"]["sub2"]
    print(repr(key)) # Prints Key("^myglobal")["sub1"]["sub2"]

    # Attempt to call eval() without fully qualifying the import prefix for the Key class
    try:
        eval(repr(key))
    except NameError:
        # eval() raises: "NameError: name 'Key' is not defined"
        assert True

    # Call eval() with a fully qualified import prefix for the Key class
    print(repr(eval("yottadb." + repr(key))))  # Prints Key("^myglobal")["sub1"]["sub2"]

++++++++++++++++++
Key.__reversed__()
++++++++++++++++++

.. code-block:: python

    def __reversed__(self) -> Generator

The :code:`Key.__reversed__()` magic method allows for easy iteration over the subscripts at the subscript level of the given :code:`Key` object, beginning from the last subscript. For example,

In the event of an error in an underlying :ref:`ydb-subscript-previous-s-st-fn` call, a :code:`YDBError` exception is raised reflecting the underlying YottaDB error code and message.

.. code-block:: python

    vegetables = yottadb.Key("^inventory")["vegetables"]
    vegetables["carrots"] = 'in stock'
    vegetables["cabbages"] = 'sold out'
    vegetables["potatoes"] = 'in stock'
    vegetables["spinach"] = 'sold out'
    for vegetable in reversed(vegetables):
        print(f"{vegetable}: {vegetable.value}\n")

    # Prints:
    # spinach: sold out
    # potatoes: in stock
    # cabbages: sold out
    # carrots: in stock

+++++++++++++++++
Key.__setitem__()
+++++++++++++++++

.. code-block:: python

    def __setitem__(self, item, value)

The :code:`Key.__setitem__()` magic method provides a simple interface for updating the value at the YottaDB local or global variable node represented by the :code:`Key` object, using the Python :code:`=` operator. For example:

In the event of an error in the underlying :ref:`ydb-set-s-st-fn` call, a :code:`YDBError` exception is raised reflecting the underlying YottaDB error code and message.

.. code-block:: python

    key = yottadb.Key("^myglobal")["sub1"]
    key["sub2"] = "my value"
    print(key["sub2"].value) # Prints 'my value'

+++++++++++++
Key.__str__()
+++++++++++++

.. code-block:: python

    def __str__(self) -> str

The :code:`Key.__str__()` magic method returns a human-readable representation of the :code:`Key` object as a Python :code:`str` object. For a Python-readable representation of the object, use `Key.__repr__()`_.

.. code-block:: python

    key = yottadb.Key("^myglobal")["sub1"]["sub2"]
    print(str(key)) # Prints '^myglobal("sub1","sub2")'
