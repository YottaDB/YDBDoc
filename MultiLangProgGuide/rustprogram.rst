.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2021-2024 YottaDB LLC and/or its subsidiaries.#
.. # All rights reserved.                                        #
.. #                                                             #
.. #     This document contains the intellectual property        #
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

  Programming YottaDB in the `Rust language <https://www.rust-lang.org/>`_ is accomplished through the `yottadb crate <https://yottadb.gitlab.io/Lang/YDBRust/yottadb/index.html>`_, which is a Rust wrapper around the YottaDB C API.

  The YottaDB Rust wrapper requires a minimum YottaDB version of r1.30 and is tested with a minimum Rust version of 1.40.

+++++++++++++++++
Install YottaDB
+++++++++++++++++

  Follow the instructions in the :ref:`mlpg-quick-start` section to install YottaDB.

++++++++++++++
Install Rust
++++++++++++++

  The next step is to install Rust. Follow the instructions for `rustup <https://www.rust-lang.org/tools/install>`_ to install Rust on your system, or use the OS package manager if that provides the minimum Rust version.

+++++++++++++++++
Install YDBRust
+++++++++++++++++

  Clone the YDBRust repository and run an example.

  .. code-block:: bash

   git clone https://gitlab.com/YottaDB/Lang/YDBRust/
   cd YDBRust
   echo 'yottadb = "2.0.0"' >> Cargo.toml
   cargo run --example say_hello_rust


----------
Rust API
----------

  There are two major APIs that are part of the Rust wrapper:

    * `craw <https://yottadb.gitlab.io/Lang/YDBRust/yottadb/craw/index.html>`_, the `FFI <https://en.wikipedia.org/wiki/Foreign_function_interface>`_ bindings generated directly by bindgen. *These are not recommended for normal use*, but are available in case functionality is needed beyond that provided by the Context API.
    * The main `Context API <https://yottadb.gitlab.io/Lang/YDBRust/yottadb/struct.Context.html>`_, which is a safe wrapper around the C API which keeps track of the current tptoken and an error buffer. The reason this metadata is necessary is because this crate binds to the threaded version of YottaDB, which requires a :code:`tptoken` and :code:`err_buffer`. See :ref:`transaction processing <txn-proc>` for more details on :code:`tptoken` and transactions.
    * Most operations are encapsulated in methods in the `KeyContext <https://yottadb.gitlab.io/Lang/YDBRust/yottadb/struct.KeyContext.html>`_ struct. Iteration helpers are available to iterate over values in the database in a variety of ways.


  .. note::

   To run any of the examples below, create a file (e.g., rust_example.rs) under the :code:`examples/` sub-directory and run it, from the :code:`YDBRust` directory, using the following command:

     .. code-block:: bash

      $ cargo run --quiet --example rust_example

  Example:

    A basic database operation (set a value, retrieve it, and delete it).

    .. code-block:: rust

     use yottadb::{Context, KeyContext, DeleteType, YDBResult};

     fn main() -> YDBResult<()> {
         let ctx = Context::new();
         let key = KeyContext::new(&ctx, "^MyGlobal", &["SubscriptA", "42"]);

         key.set("This is a persistent message")?;
         let buffer = key.get()?;

         println!("{:?}", String::from_utf8(buffer).unwrap());
         key.delete(DeleteType::DelNode)?;
         Ok(())
     }

  Output:

    .. code-block:: bash

     "This is a persistent message"

++++++++
Macros
++++++++

~~~~~~
ci_t
~~~~~~

  .. code-block:: rust

   macro_rules! ci_t {
       ($tptoken: expr, $err_buffer: expr, $routine: expr $(, $args: expr)* $(,)?) => { ... };
   }

  :code:`ci_t` macro is used to make an FFI call to M.

  Each argument passed (after :code:`routine`) must correspond to the appropriate argument expected by :code:`routine`. If :code:`routine` returns a value, the first argument must be a pointer to an out parameter in which to store the value. All arguments must be `representable as C types <https://doc.rust-lang.org/nomicon/ffi.html#interoperability-with-foreign-code>`_.

  Example:

    Call the M routine described by :code:`HelloWorld1` in the call-in table. See also `examples/m-ffi/helloworld1.m <https://gitlab.com/YottaDB/Lang/YDBRust/-/blob/master/examples/m-ffi/helloworld1.m>`_ and `examples/m-ffi/calltab.ci <https://gitlab.com/YottaDB/Lang/YDBRust/-/blob/master/examples/m-ffi/calltab.ci>`_.

    .. code-block:: rust

     use std::ffi::CString;
     use std::os::raw::c_char;
     use yottadb::{craw, ci_t, TpToken};
     use std::env;

     fn main(){
     	  env::set_var("ydb_routines", "examples/m-ffi");
	  env::set_var("ydb_ci", "examples/m-ffi/calltab.ci");

	  let mut buf = Vec::<u8>::with_capacity(100);
	  let mut msg = craw::ydb_string_t { length: buf.capacity() as u64, address: buf.as_mut_ptr() as *mut c_char };

	  let routine = CString::new("HelloWorld1").unwrap();
	  unsafe {
	  	  ci_t!(TpToken::default(), Vec::new(), &routine, &mut msg as *mut _).unwrap();
		  buf.set_len(msg.length as usize);
	  }
	  println!("{:?}", String::from_utf8_lossy(&buf));
     }

  Output:

    .. code-block:: bash

     "entry called"

~~~~~~~
cip_t
~~~~~~~

  .. code-block:: rust

   macro_rules! cip_t {
       ($tptoken: expr, $err_buffer: expr, $routine: expr, $($args: expr),* $(,)?) => { ... };
   }

  :code:`cip_t` macro is used to make a FFI call to M using a cached function descriptor.

  Each argument passed (after :code:`routine`) must correspond to the appropriate argument expected by :code:`routine`. If :code:`routine` returns a value, the first argument must be a pointer to an out parameter in which to store the value. All arguments must be `representable as C types <https://doc.rust-lang.org/nomicon/ffi.html#interoperability-with-foreign-code>`_.

  Example:

    Call the M routine described by :code:`HelloWorld1` in the call-in table. See also `examples/m-ffi/helloworld1.m <https://gitlab.com/YottaDB/Lang/YDBRust/-/blob/master/examples/m-ffi/helloworld1.m>`_ and `examples/m-ffi/calltab.ci <https://gitlab.com/YottaDB/Lang/YDBRust/-/blob/master/examples/m-ffi/calltab.ci>`_.

    .. code-block:: rust

     use std::env;
     use std::ffi::CString;
     use std::os::raw::c_char;
     use yottadb::{craw, cip_t, CallInDescriptor, TpToken};

     fn main(){
          env::set_var("ydb_routines", "examples/m-ffi");
	  env::set_var("ydb_ci", "examples/m-ffi/calltab.ci");

	  let mut buf = Vec::<u8>::with_capacity(100);
	  let mut msg = craw::ydb_string_t { length: buf.capacity() as u64, address: buf.as_mut_ptr() as *mut c_char };
	  let mut routine = CallInDescriptor::new(CString::new("HelloWorld1").unwrap());
	  unsafe {
		  cip_t!(TpToken::default(), Vec::new(), &mut routine, &mut msg as *mut _).unwrap();
		  buf.set_len(msg.length as usize);
	  }
	  println!("{:?}", String::from_utf8_lossy(&buf));
     }

  Output:

    .. code-block:: bash

     "entry called"

~~~~~~~~~~~
make_ckey
~~~~~~~~~~~

  .. code-block:: rust

   macro_rules! make_ckey {
       ( $ctx:expr, $var:expr $(,)?) => { ... };
       ( $ctx:expr, $gbl:expr $(, $x:expr)+ ) => { ... };
   }

  :code:`make_ckey` macro is used to create a :ref:`KeyContext <keycontext-struct>` with the given subscripts, provided a context.

  Example:

    .. code-block:: rust

     use std::error::Error;
     use yottadb::Context;

     fn main() -> Result<(), Box<dyn Error>> {
         let ctx = Context::new();
	 let key = yottadb::make_ckey!(ctx, "^hello", "world");
	 key.set("This is a persistent message")?;
	 println!("{:?}", key.key);
	 println!("{:?}", String::from_utf8(key.get()?).unwrap());
         Ok(())
     }

  Output:

    .. code-block:: bash

     ^hello("world")
     "This is a persistent message"

~~~~~~~~~~
make_key
~~~~~~~~~~

  .. code-block:: rust

   macro_rules! make_key {
       ( $var:expr $(,)? ) => { ... };
       ( $var: expr $( , $subscript: expr)+ $(,)? ) => { ... };
   }

  :code:`make_key` macro provides a Key object for the given subscripts.

  Example:

    .. code-block:: rust

     fn main() {
        let my_key = yottadb::make_key!("^MyTimeSeriesData", "5");
        println!("{:?}", my_key);
     }

  Output:

    .. code-block:: bash

     ^MyTimeSeriesData("5")

.. note::

   A node is created only when it is set. If the value of the key is not set :code:`key.data()` will always return :code:`NoData`.

+++++++++++++++++++++++++
Struct yottadb::Context
+++++++++++++++++++++++++

  A struct that keeps track of the current transaction and error buffer.

~~~~~~~~~~~~~~
delete_excl()
~~~~~~~~~~~~~~

  As a wrapper for the C function :ref:`ydb_delete_excl_st() <ydb-delete-excl-s-st-fn>`, :code:`delete_excl()` deletes all local variables except for those passed in :code:`saved_variables`.

  .. code-block:: rust

   pub fn delete_excl(&self, saved_variables: &[&str]) -> YDBResult<()>

  Passing an empty :code:`saved_variables` slice deletes all local variables. Attempting to save a global or intrinsic special variable is an error.

  Example:

    .. code-block:: rust

     use yottadb::{Context, KeyContext, YDBResult};

     fn main()-> YDBResult<()> {
         // Create three variables and set all
         let ctx = Context::new();
         let a = KeyContext::variable(&ctx, "deleteExclTestA");
         a.set("test data")?;
         let b = KeyContext::variable(&ctx, "deleteExclTestB");
         b.set("test data 2")?;
         let c = KeyContext::variable(&ctx, "deleteExclTestC");
         c.set("test data 3")?;

         println!("Before deleting any variables:");
         println!("a: {:?}", a.data()?);
         println!("b: {:?}", b.data()?);
         println!("c: {:?}", c.data()?);

         // Delete all variables except `a`
         ctx.delete_excl(&[&a.variable])?;
         println!("After deleting variables b and c:");
         println!("a: {:?}", a.data()?);
         println!("b: {:?}", b.data()?);
         println!("c :{:?}", c.data()?);

         // Delete `a` too
         ctx.delete_excl(&[])?;
         println!("After deleting variable a:");
         println!("a: {:?}", a.data()?);

         Ok(())
     }

  Output:

    .. code-block:: bash

     Before deleting any variables:
     a: ValueData
     b: ValueData
     c: ValueData
     After deleting variables b and c:
     a: ValueData
     b: NoData
     c :NoData
     After deleting variable a:
     a: NoData

~~~~~~~~~~~~~~~~
eintr_handler()
~~~~~~~~~~~~~~~~

  Runs the YottaDB deferred signal handler (if necessary).

  This function must be called if an application has a tight loop inside a transaction which never calls a YDB function.

  .. code-block:: rust

   pub fn eintr_handler(&self) -> YDBResult<()>

~~~~~~~
lock()
~~~~~~~

  As a wrapper for the C function :ref:`ydb_lock_st() <ydb-lock-s-st-fn>`, :code:`lock()` acquires locks specified and releases all others.

  .. code-block:: rust

   pub fn lock(&self, timeout: Duration, locks: &[Key]) -> YDBResult<()>

  This operation is atomic. If any lock cannot be acquired, all locks are released. The :code:`timeout` specifies the maximum time to wait before returning an error. If no locks are specified, all locks are released.

  Note that YottaDB locks are per-process, not per-thread.

  For implementation reasons, there is a hard limit to the number of Keys that can be passed in locks:

    * 64-bit architecture: 10 Keys
    * 32-bit architecture: 9 Keys

  If more than this number of keys are passed, `YDB_ERR_MAXARGCNT <../MessageRecovery/errors.html#maxargcnt-error>`_ will be returned.

  Example:

  .. code-block:: rust

     use std::slice;
     use std::time::Duration;
     use yottadb::{Context, KeyContext, Key};
     use yottadb::YDBResult;

     fn main()->YDBResult<()>  {
        // You can use either a `Key` or a `KeyContext` to acquire a lock.
	// This uses a `KeyContext` to show that you need to use `.key` to get the inner `Key`.
	let ctx = Context::new();
	let a = KeyContext::variable(&ctx, "lockA");

	// Release any locks held and acquire a single lock
	// using `from_ref` here allows us to use `a` later without moving it
	ctx.lock(Duration::from_secs(1), slice::from_ref(&a.key)).unwrap();

	// Release any locks held and acquire multiple locks
	let locks = vec![a.key, Key::variable("lockB")];
	ctx.lock(Duration::from_secs(1), &locks).unwrap();

	// Release all locks
	ctx.lock(Duration::from_secs(0), &[]).unwrap();
	Ok(())
     }

.. _lock-new:

~~~~~~
new()
~~~~~~

  Create a new Context.

  .. code-block:: rust

   pub fn new() -> Context

~~~~~~~~~~
new_key()
~~~~~~~~~~

  Create a KeyContext from this Context.

  .. code-block:: rust

   pub fn new_key<K: Into<Key>>(&self, key: K) -> KeyContext

~~~~~~~~~~
str2zwr()
~~~~~~~~~~

  As a wrapper for the C function :ref:`ydb_str2zwr_st() <ydb-str2zwr-s-st-fn>`, :code:`str2zwr()` serializes the given binary sequence to :ref:`zwrite format <zwrite-format>`, which is printable ASCII.

  .. code-block:: rust

   pub fn str2zwr(&self, original: &[u8]) -> YDBResult<Vec<u8>>

  Example:

    When :code:`ydb_chset=UTF-8` is set, this will preserve UTF-8 characters:

    .. code-block:: rust

     use yottadb::{Context, YDBResult};

     fn main() -> YDBResult<()>{
         let ctx = Context::new();
         let str2zwr = ctx.str2zwr(b"The quick brown dog\x08\x08\x08fox jumps over the lazy fox\x08\x08\x08dog.")?;
         println!("Original string: {}", "The quick brown dog\x08\x08\x08fox jumps over the lazy fox\x08\x08\x08dog.");
         println!("Zwrite formatted string: {:?}",String::from_utf8(str2zwr).unwrap());
         Ok(())
     }

  Output:

    .. code-block:: bash

     Original string: The quick brown fox jumps over the lazy dog.
     Zwrite formatted string: "\"The quick brown dog\"_$C(8,8,8)_\"fox jumps over the lazy fox\"_$C(8,8,8)_\"dog.\""

    When the input is invalid UTF-8, it will use the more verbose zwrite format:

    .. code-block:: rust

     use yottadb::{Context, YDBResult};

     fn main() -> YDBResult<()>{
         let ctx = Context::new();
         let str2zwr = ctx.str2zwr(b"\xff")?;
         println!("{:?}",String::from_utf8(str2zwr).unwrap());
         Ok(())
     }

  Output:

    .. code-block:: bash

     "$ZCH(255)"

~~~~~
tp()
~~~~~

  .. code-block:: rust

   pub fn tp<'a, F>(
       &'a self,
       f: F,
       trans_id: &str,
       locals_to_reset: &[&str]
   ) -> Result<(), Box<dyn Error + Send + Sync>>
   where
       F: FnMut(&'a Self) -> Result<TransactionStatus, Box<dyn Error + Send + Sync>>,

  As a wrapper for the C function :ref:`ydb_tp_st() <ydb-tp-s-st-fn>`, :code:`tp()` is used to execute a transaction, where :code:`f` is the transaction to execute.

  :code:`tp` stands for "transaction processing".

  The parameter :code:`trans_id` is the name logged for the transaction. If :code:`trans_id` has the special value "BATCH", durability is not enforced by YottaDB. See :ref:`ydb_tp_st() <ydb-tp-s-st-fn>` for details.

  The argument passed to :code:`f` is a transaction processing token.

  Application code can return a TransactionStatus in order to rollback or restart. :code:`tp()` behaves as follows:

    * If :code:`f` panics, the transaction is rolled back and the panic resumes afterwards.
    * If :code:`f` returns Ok(TransactionStatus), the transaction will have the behavior documented under TransactionStatus (commit, restart, and rollback, respectively).
    * If :code:`f` returns an Err(YDBError), the status from that error will be returned to the YottaDB engine. As a result, if the status for the YDBError is YDB_TP_RESTART, the transaction will be restarted. Otherwise, the transaction will be rolled back and the error returned from tp().
    * If :code:`f` returns any other Err variant, the transaction will be rolled back and the error returned from tp().

  :code:`f` must be FnMut, not FnOnce, since the YottaDB engine may call f many times if necessary to ensure ACID properties. This may affect your application logic; within a transaction, the :ref:`intrinsic special variable $trestart <trestart-isv>` has the number of times the transaction has been restarted.

  Example:

    Rollback a transaction if an operation fails:

    .. code-block:: rust

     use yottadb::{Context, KeyContext,  TransactionStatus, YDBResult};

     fn fallible_operation() -> Result<(), &'static str> {
        if rand::random() {
           Ok(())
        } else {
           Err("the operation failed")
        }
     }

     fn main() -> YDBResult<()> {
        let _ctx = Context::new();
        let var = KeyContext::variable(&_ctx, "tpRollbackTest");
        var.set("initial value")?;
        println!("starting tp");
        let maybe_err = _ctx.tp(|_ctx| {
           println!("in tp");
           fallible_operation()?;
           println!("succeeded");
           var.set("new value")?;
           Ok(TransactionStatus::Ok)
        }, "BATCH", &[]);
        let expected_val: &[_] = if maybe_err.is_ok() {
           b"new value"
        } else {
           b"initial value"
        };

        let output_buffer = var.get()?;
        println!("Ouput - {:?}", String::from_utf8(output_buffer).unwrap());
        println!("Expected value - {:?}", String::from_utf8((&expected_val).to_vec()).unwrap());
        Ok(())

     }

  Output:

    .. code-block:: bash

     # Output when transaction is unsuccessful

     starting tp
     in tp
     Ouput - "initial value"
     Expected value - "initial value"


     # Output when transaction is successful

     starting tp
     in tp
     succeeded
     Ouput - "new value"
     Expected value - "new value"

    Retry a transaction until it succeeds:

    .. code-block:: rust

     use yottadb::{Context, TransactionStatus};

     fn main(){
         let ctx = Context::new();
         ctx.tp(|_tptoken| {
             if fallible_operation().is_ok() {
                 Ok(TransactionStatus::Ok)
             } else {
                 Ok(TransactionStatus::Restart)
             }
         }, "BATCH", &[]).unwrap();
     }

     fn fallible_operation() -> Result<(), ()> {
         if rand::random() {
             Ok(())
         } else {
             Err(())
         }
     }

~~~~~~~~~~
tptoken()
~~~~~~~~~~

  Return the token for the transaction associated with this Context.

  This allows calling yottadb functions in the craw API that have not yet been wrapped and require a tptoken from inside a transaction.

  .. code-block:: rust

   pub fn tptoken(&self) -> TpToken

  Example:

    .. code-block:: rust

        use std::env;
        use std::ffi::CStr;
        use yottadb::{ci_t, Context, TransactionStatus};

        fn main() {
           env::set_var("ydb_routines", "examples/m-ffi");
    	   env::set_var("ydb_ci", "examples/m-ffi/calltab.ci");
	   let ctx = Context::new();
	   ctx.tp(|ctx| {
               let tptoken = ctx.tptoken();
	       let routine = CStr::from_bytes_with_nul(b"noop\0").unwrap();
	       unsafe { ci_t!(tptoken, Vec::new(), routine)?; }
	       Ok(TransactionStatus::Ok)
	   }, "BATCH", &[]).unwrap();
	}

~~~~~~~~~~
zwr2str()
~~~~~~~~~~

  As a wrapper for the C funtion :ref:`ydb_zwr2str_st() <ydb-zwr2str-s-st-fn>`, :code:`zwr2str()` deserializes a zwrite formatted buffer to the original binary buffer.

  .. code-block:: rust

   pub fn zwr2str(&self, serialized: &[u8]) -> Result<Vec<u8>, YDBError>

  Example:

    .. code-block:: rust

     use yottadb::{Context, YDBResult};

     fn main() -> YDBResult<()>{
         let ctx = Context::new();

         // Use "$ZCH" (instead of "$C") below as that will work in both M and UTF-8 modes (of "ydb_chset" env var)
	 // Note: Cannot use "$ZCHAR" below as "$ZCH" is the only input format recognized by "zwr2str()".
         let out_buf = ctx.zwr2str(b"\"The quick brown dog\"_$ZCH(8,8,8)_\"fox jumps over the lazy fox\"_$ZCH(8,8,8)_\"dog.\"")?;
         println!("Zwrite formatted string: {}","The quick brown dog\"_$ZCH(8,8,8)_\"fox jumps over the lazy fox\"_$ZCH(8,8,8)_\"dog.");
         println!("String after zwr2str: {}",String::from_utf8(out_buf).unwrap());

         Ok(())
     }

  Output:

    .. code-block:: bash

     Zwrite formatted string: The quick brown dog"_$ZCH(8,8,8)_"fox jumps over the lazy fox"_$ZCH(8,8,8)_"dog.
     String after zwr2str: The quick brown fox jumps over the lazy dog.

  :code:`zwr2str()` writes directly to :code:`out_buf` to avoid returning multiple output buffers.

~~~~~~~~~~~~~~~~~~
Call-in functions
~~~~~~~~~~~~~~~~~~

.. _ci-tab-open:

^^^^^^^^^^^^^^
ci_tab_open()
^^^^^^^^^^^^^^

  Open the call-in table stored in :code:`file` and return its file descriptor.

  You can later switch the active call-in table by calling :ref:`ci-tab-switch` with the file descriptor.

  .. code-block:: rust

   pub fn ci_tab_open(&self, file: &CStr) -> YDBResult<CallInTableDescriptor>

  Refer to the :ref:`ci-tab-switch` example to see how :code:`ci_tab_open()` works.

.. _ci-tab-switch:

^^^^^^^^^^^^^^^^^
ci_tab_switch()
^^^^^^^^^^^^^^^^^

  Switch the active call-in table to :code:`new_handle`. Returns the previously active table.

  :code:`new_handle` is a file descriptor returned by :ref:`ci-tab-open`.

  .. code-block:: rust

   pub fn ci_tab_switch(&self,new_handle: CallInTableDescriptor) -> YDBResult<CallInTableDescriptor>

  Example:

    .. code-block:: rust

     use std::env;
     use std::ffi::CString;
     use std::os::raw::c_char;
     use yottadb::{craw, ci_t, TpToken, Context, YDBResult};

     fn main() -> YDBResult<()>{
         env::set_var("ydb_routines", "examples/m-ffi");
	 let ctx = Context::new();
	 let file = CString::new("examples/m-ffi/calltab.ci").unwrap();
	 let descriptor = ctx.ci_tab_open(&file)?;
	 ctx.ci_tab_switch(descriptor)?;

	 let mut buf = Vec::<u8>::with_capacity(100);
	 let mut msg = craw::ydb_string_t { length: buf.capacity() as u64, address: buf.as_mut_ptr() as *mut c_char };

	 let routine = CString::new("HelloWorld1").unwrap();
         unsafe {
           ci_t!(TpToken::default(), Vec::new(), &routine, &mut msg as *mut _).unwrap();
	   buf.set_len(msg.length as usize);
         }
         println!("{:?}", String::from_utf8_lossy(&buf));
         Ok(())
     }

  Output:

    .. code-block:: bash

     "entry called"

~~~~~~~~~~~~~~~~~~
Utility Functions
~~~~~~~~~~~~~~~~~~

^^^^^^^^^^
message()
^^^^^^^^^^

  Return the message corresponding to a YottaDB error code.

  .. code-block:: rust

   pub fn message(&self, status: i32) -> YDBResult<Vec<u8>>

  Example:

    .. code-block:: rust

     use yottadb::{Context, KeyContext};

     fn main(){
        let ctx = Context::new();
        let key = KeyContext::variable(&ctx, "oopsNotDefined");

        let err = key.get().unwrap_err();

        let buf = ctx.message(err.status).unwrap();
        let msg = String::from_utf8(buf).unwrap();
        println!("{:?}", msg);
     }

  Output:

    .. code-block:: bash

     "%YDB-E-LVUNDEF, Undefined local variable: !AD"

^^^^^^^^^^
release()
^^^^^^^^^^

  Return a string in the format :code:`rustwr <rust wrapper version> <$ZYRELEASE>`.

  :ref:`$ZYRELEASE <zyrelease-isv>` is the :ref:`intrinsic special variable <isv-mlpg>` containing the version of the underlying C database and :code:`<rust wrapper version>` is the version of :code:`yottadb` published to crates.io.

  Example:

    .. code-block:: rust

     use yottadb::{Context, YDBError};

     fn main() -> Result<(), YDBError>{
        let ctx = Context::new();
        let release = ctx.release()?;
        println!("{:?}",release);
        Ok(())
     }

  Output:

    .. code-block:: bash

     "rustwr 2.0.0 YottaDB r1.34 Linux x86_64"

.. _keycontext-struct:

++++++++++++++++++++++++++++
Struct yottadb::KeyContext
++++++++++++++++++++++++++++

  A key which keeps track of the current transaction and error buffer.

  Keys are used to get, set, and delete values in the database.

~~~~~~~
data()
~~~~~~~

  As a wrapper for the C function :ref:`ydb_data_st() <ydb-data-s-st-fn>`, :code:`data()` provides information about whether or not a global or local variable node has data and/or a subtree.

  .. code-block:: rust

   pub fn data(&self) -> YDBResult<DataReturn>

  It returns the following information in DataReturn about a local or global variable node:

    * NoData: There is neither a value nor a subtree; i.e. it is undefined
    * ValueData: There is a value, but no subtree
    * TreeData: There is no value, but there is a subtree
    * ValueTreeData: There are both a value and a subtree

  Example:

    .. code-block:: rust

     use yottadb::{Context, make_ckey, DeleteType};
     use std::error::Error;

     fn main() -> Result<(), Box<dyn Error>> {
         let ctx = Context::new();
         let key = make_ckey!(ctx, "^helloDoesNotExist");

         println!("{:?}", key.data()?);
	 key.set("Setting the value for key")?;
	 println!("{:?}", key.data()?);
	 key.delete(DeleteType::DelNode)?;
         Ok(())
     }

  Output:

    .. code-block:: bash

     NoData
     ValueData

~~~~~~~~~
delete()
~~~~~~~~~

  As a wrapper for the C function :ref:`ydb_delete_st() <ydb-delete-s-st-fn>`, :code:`delete()` deletes nodes in the local or global variable tree or subtree specified.

  .. code-block:: rust

   pub fn delete(&self, delete_type: DeleteType) -> YDBResult<()>

  A value of DelNode or DelTree for DeleteType specifies whether to delete just the node at the root, leaving the (sub)tree intact, or to delete the node as well as the (sub)tree.

  Example:

    .. code-block:: rust

     use yottadb::{Context, DeleteType, make_ckey};
     use std::error::Error;

     fn main() -> Result<(), Box<dyn Error>> {
         let ctx = Context::new();
         let key = make_ckey!(ctx, "^helloDeleteMe");

         key.set("Hello world!")?;
         println!("{:?}", String::from_utf8(key.get()?).unwrap());
         key.delete(DeleteType::DelTree)?;

         println!("{:?}", key.data()?);
         Ok(())
     }

  Output:

    .. code-block:: bash

     "Hello world!"
     NoData

~~~~~~
get()
~~~~~~

  As a wrapper for the C function :ref:`ydb_get_st() <ydb-get-s-st-fn>`, :code:`get()` gets the value of this key from the database and returns the value.

  .. code-block:: rust

   pub fn get(&self) -> YDBResult<Vec<u8>>

  Example:

    .. code-block:: rust

     use yottadb::{Context, make_ckey};
     use std::error::Error;

     fn main() -> Result<(), Box<dyn Error>> {
         let ctx = Context::new();
         let key = make_ckey!(ctx, "^hello");

         key.set("Hello world!")?;
         let output_buffer = key.get()?;

         println!( "{:?}", String::from_utf8(output_buffer).unwrap());
         Ok(())
     }

  Output:

    .. code-block:: bash

     "Hello world!"

  :code:`get()` can be used to get the value of an :ref:`Intrinsic Special Variable <isv-mlpg>` as well.

  Example:

    .. code-block:: rust

     use yottadb::{Context, KeyContext, YDBResult};

     fn main() -> YDBResult<()> {
         let ctx = Context::new();
         let key = KeyContext::variable(&ctx, "$zyrelease");

         let zyrelease = key.get()?;

         println!("$zyrelease: {}", String::from_utf8(zyrelease).unwrap());
         Ok(())
     }

  Output:

    .. code-block:: bash

     $zyrelease: YottaDB r1.34 Linux x86_64

~~~~~~~~~~~~~~~~~
get_and_parse()
~~~~~~~~~~~~~~~~~

  Retrieve a value from the database and parse it into a Rust data structure.

  This is a shorthand for :code:`String::from_utf8(key.get()).parse()` that collects the errors into a single enum.

  .. code-block:: rust

   pub fn get_and_parse<T: FromStr>(&self) -> Result<T, ParseError<T::Err>>

  Example:

    Set and retrieve an integer, with error handling.

    .. code-block:: rust

     use yottadb::{Context, ParseError, YDBResult};

     fn main() -> YDBResult<()> {
        let ctx = Context::new();
        let key = ctx.new_key("weekday");
        key.set(5.to_string())?;
        let day: u8 = match key.get_and_parse() {
            Ok(day) => day,
            Err(ParseError::YDB(err)) => return Err(err),
            Err(ParseError::Utf8(err)) => {
                eprintln!("warning: had an invalid string");
                String::from_utf8_lossy(&err.as_bytes()).parse().unwrap()
            }
            Err(ParseError::Parse(err, original)) => {
                panic!("{} is not a valid string: {}", original, err);
            }
        };
        println!("{:?}", day);
        Ok(())
     }

  Output:

    .. code-block:: bash

     5

  Set and retrieve an integer, without error handling.

    .. code-block:: rust

     use yottadb::{Context,  YDBResult};

     fn main() -> YDBResult<()> {
        let ctx = Context::new();
        let key = ctx.new_key("weekday");
        key.set(5.to_string())?;
        let day: u8 = key.get_and_parse().unwrap();
        println!("{:?}", day);
        Ok(())
     }

  Output:

    .. code-block:: bash

     5

~~~~~~~~~~~~
increment()
~~~~~~~~~~~~

  As a wrapper for the C function :ref:`ydb_incr_st() <ydb-incr-s-st-fn>`, :code:`increment()` converts the value to a :ref:`number <canonical-numbers>` and increments it based on the value specified by Option.

  .. code-block:: rust

   pub fn increment(&self, increment: Option<&[u8]>) -> YDBResult<Vec<u8>>

  :code:`increment` defaults to 1 if the value is None.

  Example:

    .. code-block:: rust

     use yottadb::{Context, make_ckey};
     use std::error::Error;

     fn main() -> Result<(), Box<dyn Error>> {
         let ctx = Context::new();
         let key = make_ckey!(ctx, "helloIncrementMe");

         key.set("0")?;
         let mut output_buffer = key.get()?;

         println!("Before increment: {:?}", String::from_utf8(output_buffer).unwrap());
         key.increment(None)?;
         output_buffer = key.get()?;

         println!("Incremented by 1 (default): {:?}", String::from_utf8(output_buffer).unwrap());

         println!("Before increment : {:?}", "100" );
         key.increment(Some(b"100"))?;
         output_buffer = key.get()?;

         println!("After increment: {:?}", String::from_utf8(output_buffer).unwrap());
         Ok(())
     }

  Output:

    .. code-block:: bash

     Before increment: "0"
     Incremented by 1 (default): "1"
     Before increment : "100"
     After increment: "101"

~~~~~~~~~~~~~~~~~
iter_key_nodes()
~~~~~~~~~~~~~~~~~

  Iterates over all nodes for the local or global variable pointed to by the key and returns a copy of the key at each node.

  .. code-block:: rust

   pub fn iter_key_nodes(&mut self) -> ForwardKeyNodeIterator<'_>

~~~~~~~~~~~~~~~~~~~~~~~~~
iter_key_nodes_reverse()
~~~~~~~~~~~~~~~~~~~~~~~~~

  Iterates in reverse order over all nodes for the local or global variable pointed to by the key and returns a copy of the key at each node.

  .. code-block:: rust

   pub fn iter_key_nodes_reverse(&mut self) -> ReverseKeyNodeIterator<'_>

~~~~~~~~~~~~~~~~
iter_key_subs()
~~~~~~~~~~~~~~~~

  Iterates over all subscripts at this level of the local or global variable tree and returns a copy of the key at each subscript.

  .. code-block:: rust

   pub fn iter_key_subs(&mut self) -> ForwardKeySubIterator<'_>

~~~~~~~~~~~~~~~~~~~~~~~~
iter_key_subs_reverse()
~~~~~~~~~~~~~~~~~~~~~~~~

  Iterates in reverse order over all subscripts at this level of the local or global variable tree and returns a copy of the key at each subscript.

  .. code-block:: rust

   pub fn iter_key_subs_reverse(&mut self) -> ReverseKeySubIterator<'_>

~~~~~~~~~~~~~
iter_nodes()
~~~~~~~~~~~~~

  Iterates over all nodes for the local or global variable pointed to by the key and returns the value at each node.

  .. code-block:: rust

   pub fn iter_nodes(&mut self) -> ForwardNodeIterator<'_>

~~~~~~~~~~~~~~~~~~~~~
iter_nodes_reverse()
~~~~~~~~~~~~~~~~~~~~~

  Iterates in reverse order over all nodes for the local or global variable pointed to by the key and returns the value at each node.

  .. code-block:: rust

   pub fn iter_nodes_reverse(&mut self) -> ReverseNodeIterator<'_>

~~~~~~~~~~~~
iter_subs()
~~~~~~~~~~~~

  Iterated over all the subscripts at this level of the local or global variable tree and returns the subscript for each node.

  .. code-block:: rust

   pub fn iter_subs(&mut self) -> ForwardSubIterator<'_>

~~~~~~~~~~~~~~~~~~~~
iter_subs_reverse()
~~~~~~~~~~~~~~~~~~~~

  Iterates in reverse order over all the subscripts at this level of the local or global variable tree and returns the subscript for each node.

  .. code-block:: rust

   pub fn iter_subs_reverse(&mut self) -> ReverseSubIterator<'_>

~~~~~~~~~~~~~~~~~~~
iter_subs_values()
~~~~~~~~~~~~~~~~~~~

  Iterates over all the subscripts at this level of the local or global variable tree and returns the subscript and value for each node.

  .. code-block:: rust

   pub fn iter_subs_values(&mut self) -> ForwardSubValueIterator<'_>

~~~~~~~~~~~~~~~~~~~~~~~~~~~
iter_subs_values_reverse()
~~~~~~~~~~~~~~~~~~~~~~~~~~~

  Iterates in reverse order over all the subscripts at this level of the local or global variable tree and returns the subscript and value for each node.

  .. code-block:: rust

   pub fn iter_subs_values_reverse(&mut self) -> ReverseSubValueIterator<'_>

~~~~~~~~~~~~~~
iter_values()
~~~~~~~~~~~~~~

  Iterates over all the values at this level of the local or global variable tree and returns the value for each node.

  .. code-block:: rust

   pub fn iter_values(&mut self) -> ForwardValueIterator<'_>

~~~~~~~~~~~~~~~~~~~~~~
iter_values_reverse()
~~~~~~~~~~~~~~~~~~~~~~

  Iterates in reverse order over all the values at this level of the local or global variable tree and returns the value for each node.

  .. code-block:: rust

   pub fn iter_values_reverse(&mut self) -> ReverseValueIterator<'_>

.. _lock-decr-kc:

~~~~~~~~~~~~
lock_decr()
~~~~~~~~~~~~

  As a wrapper for the C function :ref:`ydb_lock_decr_st() <ydb-lock-decr-s-st-fn>`, :code:`lock_decr()` decrements the count of a lock held by the process.

  When the count for a lock goes from 1 to 0, it is released. Attempting to decrement a lock not owned by the current process has no effect.

  .. code-block:: rust

   pub fn lock_decr(&self) -> YDBResult<()>

  Example:

    .. code-block:: rust

     use yottadb::{Context, KeyContext, YDBResult};
     use std::time::Duration;

     fn main() -> YDBResult<()> {
        let ctx = Context::new();
        let key = KeyContext::variable(&ctx, "lockDecrTest");
        key.lock_incr(Duration::from_secs(1))?;
        key.lock_decr()?;
        Ok(())
     }

.. _lock-incr-kc:

~~~~~~~~~~~~
lock_incr()
~~~~~~~~~~~~

  As a wrapper for the C function :ref:`ydb_lock_incr_st() <ydb-lock-incr-s-st-fn>`, :code:`lock_incr()` acquires a lock not currently held by the process, or increments the count for locks already held.

  .. code-block:: rust

   pub fn lock_incr(&self, timeout: Duration) -> YDBResult<()>

  :code:`timeout` specifies a time that the function waits to acquire the requested locks. If :code:`timeout` is 0, the function makes exactly one attempt to acquire the lock.

  Example:

    .. code-block:: rust

     use yottadb::{Context, KeyContext, YDBResult};
     use std::time::Duration;
     fn main() -> YDBResult<()>{
        let ctx = Context::new();
        let key = KeyContext::variable(&ctx, "lockIncrTest");
        key.lock_incr(Duration::from_secs(1))?;
        Ok(())
     }

.. _lock-intr-new:

~~~~~~
new()
~~~~~~

  Create a new KeyContext, creating the Key at the same time.

  .. code-block:: rust

   pub fn new<V, S>(ctx: &Context, variable: V, subscripts: &[S]) -> KeyContext
   where
       V: Into<String>,
       S: Into<Vec<u8>> + Clone,

~~~~~~~~~~~~
next_node()
~~~~~~~~~~~~

  As a wrapper for the C function :ref:`ydb_node_next_st() <ydb-node-next-s-st-fn>`, :code:`next_node()` facilitates traversal of a local or global variable tree to return the next node.

  .. code-block:: rust

   pub fn next_node(&mut self) -> YDBResult<KeyContext>

  Example:

    .. code-block:: rust

     use yottadb::{Context, make_ckey};
     use std::error::Error;

     fn main() -> Result<(), Box<dyn Error>> {
         let ctx = Context::new();
         let mut key = make_ckey!(ctx, "^hello", "0", "0");

         key.set("Hello world!")?;
         // Forget the second subscript
         key.truncate(1);
         let k2 = key.next_node()?;

         println!("Current node : {:?}",key.key);
         println!("Next node: {:?}", k2.key);
         Ok(())
     }

  Output:

    .. code-block:: bash

     Current node : ^hello("0")
     Next node: ^hello("0", "0")

~~~~~~~~~~~~~~~~~
next_node_self()
~~~~~~~~~~~~~~~~~

  As a wrapper for the C function :ref:`ydb_node_next_st() <ydb-node-next-s-st-fn>`, :code:`next_mode_self()` facilitates traversal of a local or global variable tree, and passes itself as the output parameter.

  .. code-block:: rust

   pub fn next_node_self(&mut self) -> YDBResult<()>

  Example:

    .. code-block:: rust

     use yottadb::{Context, make_ckey};
     use std::error::Error;

     fn main() -> Result<(), Box<dyn Error>> {
         let ctx = Context::new();
         let mut key = make_ckey!(ctx, "^hello", "0", "0");

         key.set("Hello world!")?;
         // Forget the second subscript
         key.truncate(1);
         println!("Current node (self) : {:?}",key.key);
         key.next_node_self()?;

         println!("Next node (self) : {:?}",key.key);
         Ok(())
     }

  Output:

    .. code-block:: bash

     Current node (self) : ^hello("0")
     Next node (self) : ^hello("0", "0")

~~~~~~~~~~~
next_sub()
~~~~~~~~~~~

  As a wrapper for the C function :ref:`ydb_subscript_next_st() <ydb-subscript-next-s-st-fn>`, :code:`next_sub()` implements traversal of a tree by searching for the next subscript.

  .. code-block:: rust

   pub fn next_sub(&self) -> YDBResult<Vec<u8>>

  Example:

    .. code-block:: rust

     use yottadb::{Context, make_ckey};
     use std::error::Error;

     fn main() -> Result<(), Box<dyn Error>> {
         let ctx = Context::new();
         let mut key = make_ckey!(ctx, "^hello", "0");

         key.set("Hello world!")?;
         key[0] = Vec::from("1");
         key.set("Hello world!")?;
         key[0] = Vec::from("0");
         println!("Current subscript : {:?}", std::str::from_utf8(&key[0]).unwrap());

         let subscript = key.next_sub()?;
         println!("Next subscript : {:?}", String::from_utf8(subscript).unwrap());
         Ok(())
     }

  Output:

    .. code-block:: bash

     Current subscript : "0"
     Next subscript : "1"

~~~~~~~~~~~~~~~~
next_sub_self()
~~~~~~~~~~~~~~~~

  As a wrapper for the C function :ref:`ydb_subscript_next_st() <ydb-subscript-next-s-st-fn>`, :code:`next_sub_self()` implements traversal of a tree by searching for the next subscript, and passes itself as the output parameter.

  .. code-block:: rust

   pub fn next_sub_self(&mut self) -> YDBResult<()>

  Example:

    .. code-block:: rust

     use yottadb::{Context, make_ckey};
     use std::error::Error;

     fn main() -> Result<(), Box<dyn Error>> {
         let ctx = Context::new();
         let mut key = make_ckey!(ctx, "^hello", "a");

         key.set("Hello world!")?;
         key[0] = Vec::from("b");
         key.set("Hello world!")?;
         key[0] = Vec::from("a");
         println!("Current subscript (self) : {:?}", std::str::from_utf8(&key[0]).unwrap());


         // Starting at a, the next sub should be b
         key.next_sub_self()?;
         let curr_key = &key[0];
         println!("Next subscript (self) : {:?}", std::str::from_utf8(curr_key).unwrap());
         Ok(())
     }

  Output:

    .. code-block:: bash

     Current subscript (self) : "a"
     Next subscript (self) : "b"

~~~~~~~~~~~~
prev_node()
~~~~~~~~~~~~

  As a wrapper for the C function :ref:`ydb_node_previous_st() <ydb-node-previous-s-st-fn>`, :code:`prev_node()` facilitates reverse traversal of a local or global variable tree to return the previous node.

  .. code-block:: rust

   pub fn prev_node(&mut self) -> YDBResult<KeyContext>

  Example:

    .. code-block:: rust

     use yottadb::{Context,make_ckey};
     use std::error::Error;

     fn main() -> Result<(), Box<dyn Error>> {
         let ctx = Context::new();
         let mut key = make_ckey!(ctx, "^helloPrevNode", "0", "0");

         key.set("Hello world!")?;
         // Forget the second subscript
         key.truncate(1);
         // Go to the next node, then walk backward
         key[0] = "1".into();
         let k2 = key.prev_node()?;

         println!("Current node: {:?}",key.key);
         println!("Previous node: {:?}",k2.key);
         Ok(())
     }

  Output:

    .. code-block:: bash

     Current node: ^helloPrevNode("1")
     Previous node: ^helloPrevNode("0", "0")

~~~~~~~~~~~~~~~~~
prev_node_self()
~~~~~~~~~~~~~~~~~

  As a wrapper for the C function :ref:`ydb_node_previous_st() <ydb-node-previous-s-st-fn>`, :code:`prev_node_self()` facilitates reverse traversal of a local or global variable tree and reports the predecessor node, passing itself as the output parameter.

  .. code-block:: rust

   pub fn prev_node_self(&mut self) -> YDBResult<()>

  Example:

    .. code-block:: rust

     use yottadb::{Context, make_ckey};
     use std::error::Error;

     fn main() -> Result<(), Box<dyn Error>> {
         let ctx = Context::new();
         let mut key = make_ckey!(ctx, "^hello", "0", "0");

         key.set("Hello world!")?;
         // Forget the second subscript
         key.truncate(1);
         println!("Current node (self) : {:?}",key.key);
         // Go to the next node, then walk backward
         key[0] = Vec::from("1");
         key.prev_node_self()?;

         println!("Previous node (self) : {:?}",key.key);
         Ok(())
     }

  Output:

    .. code-block:: bash

     Current node (self) : ^hello("0")
     Previous node (self) : ^hello("0", "0")

~~~~~~~~~~~
prev_sub()
~~~~~~~~~~~

  As a wrapper for the C function :ref:`ydb_subscript_previous_st() <ydb-subscript-previous-s-st-fn>`, :code:`prev_sub()` implements traversal of a tree by searching for the previous subscript.

  .. code-block:: rust

   pub fn prev_sub(&self) -> YDBResult<Vec<u8>>

  Example:

    .. code-block:: rust

     use yottadb::{Context, make_ckey};
     use std::error::Error;

     fn main() -> Result<(), Box<dyn Error>> {
         let ctx = Context::new();
         let mut key = make_ckey!(ctx, "^hello", "0");

         key.set(b"Hello world!")?;
         key[0] = Vec::from("1");
         key.set("Hello world!")?;
         key[0] = Vec::from("1");
         println!("Current subscript : {:?}", std::str::from_utf8(&key[0]).unwrap());

         let subscript = key.prev_sub()?;
         println!("Previous subscript : {:?}", String::from_utf8(subscript).unwrap());
         Ok(())
     }

  Output:

    .. code-block:: bash

     Current subscript : "1"
     Previous subscript : "0"

~~~~~~~~~~~~~~~~
prev_sub_self()
~~~~~~~~~~~~~~~~

  As a wrapper for the C function :ref:`ydb_subscript_previous_st() <ydb-subscript-previous-s-st-fn>`, :code:`prev_sub_self()` implements reverse traversal of a tree by searching for the previous subscript, and passes itself in as the output parameter.

  .. code-block:: rust

   pub fn prev_sub_self(&mut self) -> YDBResult<()>

  Example:

    .. code-block:: rust

     use yottadb::{Context, make_ckey};
     use std::error::Error;

     fn main() -> Result<(), Box<dyn Error>> {
         let ctx = Context::new();
         let mut key = make_ckey!(ctx, "^hello", "0");

         key.set("Hello world!")?;
         key[0] = Vec::from("1");
         key.set("Hello world!")?;
         key[0] = Vec::from("1");
         println!("Current subscript (self): {:?}", std::str::from_utf8(&key[0]).unwrap());

         key.prev_sub_self()?;
         let curr_key = &key[0];
         println!("Previous subscript (self): {:?}", std::str::from_utf8(curr_key).unwrap());
         Ok(())
     }

  Output:

    .. code-block:: bash

     Current subscript (self): "1"
     Previous subscript (self): "0"

~~~~~~
set()
~~~~~~

  As a wrapper for the C function :ref:`ydb_set_st() <ydb-set-s-st-fn>`, :code:`set()` sets the value of a key in the database.

  .. code-block:: rust

   pub fn set<U: AsRef<[u8]>>(&self, new_val: U) -> YDBResult<()>

  Example:

    .. code-block:: rust

     use yottadb::{Context, make_ckey};
     use std::error::Error;

     fn main() -> Result<(), Box<dyn Error>> {
         let ctx = Context::new();
         let key = make_ckey!(ctx, "^hello");

         key.set("Hello world!")?;
         let output_buffer = key.get()?;

         println!("{:?}", String::from_utf8(output_buffer).unwrap());
         Ok(())
     }

  Output:

    .. code-block:: bash

     "Hello world!"

~~~~~~~~~~~
variable()
~~~~~~~~~~~

  Shortcut for creating a KeyContext with no subscripts.

  .. code-block:: rust

   pub fn variable<V: Into<String>>(ctx: &Context, var: V) -> Self

~~~~~~~~~~~
with_key()
~~~~~~~~~~~

  Create a new KeyContext using an existing key.

  .. code-block:: rust

   pub fn with_key<K: Into<Key>>(ctx: &Context, key: K) -> Self

-----------------------------------------
Advantages of the Rust wrapper
-----------------------------------------

  * Rust has almost no overhead calling into C.
  * Rust has a context API, where the buffers are re-used between calls so it's not constantly allocating and deallocating.
  * Rust can also pass numbers into M FFI, not just :code:`ydb_string_t`.
