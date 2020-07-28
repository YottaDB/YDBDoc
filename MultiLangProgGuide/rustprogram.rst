.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.     #
.. # All rights reserved.                                        #
.. #                                                             #
.. #     This source code contains the intellectual property     #
.. #     of its copyright holder(s), and is made available       #
.. #     under a license.  If you do not know the terms of       #
.. #     the license, please stop and do not read further.       #
.. #                                                             #
.. ###############################################################

=====================
Programming in Rust
=====================

.. contents::
   :depth: 5

-----------------
Getting Started
-----------------

Programming YottaDB in the `Rust language <https://www.rust-lang.org/>`_ is accomplished through `yottadb crate <https://yottadb.gitlab.io/Lang/YDBRust/yottadb/index.html>`_, which is a Rust wrapper around the C implementation of YottaDB.

The YottaDB Rust wrapper requires a minimum YottaDB version of r1.30 and is tested with a minimum Rust version of 1.40.

+++++++++++++++++
Install YottaDB
+++++++++++++++++

Follow the instructions in the `Quick Start <../MultiLangProgGuide/MultiLangProgGuide.html#quick-start>`_ section to install YottaDB.

++++++++++++++
Install Rust
++++++++++++++

The nest step is to install Rust. Follow the instructions provided `here <https://www.rust-lang.org/tools/install>`_ to get setup with Rust on your system.

+++++++++++++++++
Install YDBRust
+++++++++++++++++

Clone the YDBRust repository and run an example.

.. code-block:: bash

   git clone https://gitlab.com/YottaDB/Lang/YDBRust/
   cd YDBRust
   cargo run --example say_hello_rust

There are three major APIs that are part of the Rust wrapper:

* `craw <https://yottadb.gitlab.io/Lang/YDBRust/yottadb/craw/index.html>`_, the FFI bindings generated directly by bindgen. Not recommended for normal use.
* `simple_api <https://yottadb.gitlab.io/Lang/YDBRust/yottadb/simple_api/index.html>`_, a wrapper around the :code:`craw` API which handles resizing buffers and various other recoverable errors.
* `context_api <https://yottadb.gitlab.io/Lang/YDBRust/yottadb/context_api/index.html>`_, which is wrapper around the :code:`simple_api` that stores the current tptoken and error buffer so you don't have to keep a track of them yourself.

.. note::

   The context_api is recommended for normal use, but the others are available if your needs are more specialized.

-----------------------------------------
Comparison between Rust and Go wrappers
-----------------------------------------

* Rust has almost no overhead calling into C. There is no reallocation of buffers as with Go.
* Rust has a context API, not an easy API. The difference is that buffers are re-used between calls so it's not constantly allocating and deallocating like the Go Easy API does, nor does it require passing in the tptoken and error buffer like the Go Simple API.
* Rust can pass numbers into M FFI, not just :code:`ydb_string_t`.
