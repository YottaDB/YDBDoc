.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2022 YottaDB LLC and/or its subsidiaries.     #
.. # All rights reserved.                                        #
.. #                                                             #
.. #     This document contains the intellectual property        #
.. #     of its copyright holder(s), and is made available       #
.. #     under a license.  If you do not know the terms of       #
.. #     the license, please stop and do not read further.       #
.. #                                                             #
.. ###############################################################

=========================
Programming in Node.js
=========================

.. contents::
   :depth: 5

Programming YottaDB in `Node.js <https://nodejs.org/>`_ is provided by `Nodem <https://github.com/dlwicksell/nodem>`_ which is developed by `David Wicksell <https://github.com/dlwicksell>`_. We would like to acknowledge his contribution and thank him for the value it adds to the YottaDB community. It provides access to :ref:`global and local variables <lcl-gbl-var>` as well as the ability to call functions and procedures coded in M.

Nodem wraps the YottaDB :ref:`c-simple-api` to provide a node.js API.

--------------
Installation
--------------

The minimum required version of Nodem is 0.21.0. It should run on every version of Node.js starting with version 0.12.0, through the current release (v17.7.1 at this time), as well as every version of IO.js. However, in the future, both Node.js and the V8 JavaScript engine at its core could change their APIs in a non-backwards compatible way, which might break Nodem for that version.

In order to use Nodem, YottaDB must be installed and configured correctly, including setting up the required YottaDB environment variables, or setting the appropriate options in the :code:`open()` API. Make sure to have :code:`$ydb_dist` set to the root of the YottaDB instance before compiling Nodem, whether manually or via :code:`npm`. Node.js must be installed and working as well.

Install Nodem using the following command;

.. code-block:: bash

   $ npm install nodem

Update to the latest version using the following command;

.. code-block:: bash

   $ npm update nodem

The following steps need to be performed in order for YottaDB to find the Call-In table and the :code:`v4wNode.m` routine that it maps to:

* Copy :code:`v4wNode.m`, located in the :code:`/nodem/src` directory, into a directory that is specified in the :code:`$ydb_routines` path, or in the :code:`routinesPath` property in the call to the :code:`open()` API.
* Set :code:`$ydb_ci` environment variable, or set the :code:`callinTable` property in the call to the :code:`open()` API, pointing to the file :code:`nodem.ci` located in the :code:`/nodem/resources` directory.

.. note::
   To build and install Nodem from scratch, use the instructions specified `here <https://github.com/dlwicksell/nodem#installation>`_.

-----------
Nodem API
-----------

Arguments can be passed to the API functions in two ways:

#. using positional arguments, or
#. using a single JavaScript object

The output format varies depending on which method is used.


Before any of the API functions can be used, a YottaDB runtime instance needs to be created;

.. code-block:: javascript

   const ydb=require('nodem').Ydb();


Arguments as objects and the objects returned (on success or failure) described below may have comments (lines staring with :code:`//`) that provide information about the arguments or type of value returned. For example:

* If a comment consists of :code:`(optional)` then the property (argument or returned value) is optional.
* If a comment consists of a value within :code:`<>` then it is the default value for that property.
* Any other values present in the comment specifies the domain of values.


A function, taking two arguments (error and result), can be passed to an API function. This will call the API function asynchronously. Currently :code:`data()`, :code:`function()`, :code:`get()`, :code:`increment()`, :code:`kill()`, :code:`lock()`, :code:`merge()`, :code:`nextNode()`, :code:`order()`, :code:`previous()`, :code:`previousNode()`, :code:`procedure()`, :code:`set()`, :code:`unlock()`, and :code:`version()` are the only functions that support asynchronous operation in addition to synchronous operation.

Example:

.. code-block:: javascript

   // get() being called synchronously
   > ydb.get({global:'num'});
   { ok: true, global: 'num', data: 1, defined: true }

   // get() being called asynchronously
   > ydb.get({global:'num'}, (error,result) => {if (!error) {console.log('result:', result);}});
   undefined
   > result: { ok: true, global: 'num', data: 1, defined: true }


A full set of error codes and messages is in the `YottaDB Messages and Recovery Procedures Manual <../MessageRecovery/index.html>`_. An error code and error message are returned as part of the object when a call to an API function fails.

+++++++++++++++++++++++++
Nodem Wrapper Functions
+++++++++++++++++++++++++

~~~~~~~
data()
~~~~~~~

As a wrapper for the C function :ref:`ydb_data_s() <ydb-data-s-st-fn>`, :code:`data()` provides information about whether or not a global or local variable node has data and/or children.

Arguments as an object:

.. code-block:: javascript

   {
	global|local: string,
	subscripts:   string[]|number[]  // (optional)
   }


Returns the following object on success:

.. code-block:: javascript

   {
	ok :          boolean,            // true
	global|local: string,
	subscripts:   string[]|number[],  // (optional)
	defined:      number              // [0|1|10|11]
   }

Returns the following object on failure:

.. code-block:: javascript

   {
        ok:           boolean,  // false
	errorCode:    number,
	errorMessage: string
   }

Example:

.. code-block:: javascript

   > ydb.data({global: 'Population'});
   { ok: true, global: 'Population', defined: 10 }
   > ydb.data({global: 'Population', subscripts: ["USA"]});
   { ok: true, global: 'Population', subscripts: [ 'USA' ], defined: 11 }

To better understand the structure of the Population global variable node refer the :ref:`mlpg-concepts` section. The :code:`Population` global variable has been set as follows:

.. code-block:: javascript

   > ydb.set({global:'Population',subscripts:["USA"],data:325737000})
   {
     ok: true,
     global: 'Population',
     subscripts: [ 'USA' ],
     data: 325737000
   }
   > ydb.set({global:'Population',subscripts:["USA",17900802],data:3929326})
   {
     ok: true,
     global: 'Population',
     subscripts: [ 'USA', 17900802 ],
     data: 3929326
   }
   > ydb.set({global:'Population',subscripts:["USA",18000804],data:5308483})
   {
     ok: true,
     global: 'Population',
     subscripts: [ 'USA', 18000804 ],
     data: 5308483
   }
   > ydb.set({global:'Population',subscripts:["USA",20100401],data:308745538})
   {
     ok: true,
     global: 'Population',
     subscripts: [ 'USA', 20100401 ],
     data: 308745538
   }
   > ydb.set({global:'Population',subscripts:["Belgium"],data:1367000})
   {
     ok: true,
     global: 'Population',
     subscripts: [ 'Belgium' ],
     data: 1367000
   }
   > ydb.set({global:'Population',subscripts:["Thailand"],data:8414000})
   {
     ok: true,
     global: 'Population',
     subscripts: [ 'Thailand' ],
     data: 8414000
   }

Positional arguments:

.. code-block:: javascript

   ^global|local, [subscripts+]

Returns the following on success:

.. code-block:: javascript

   {number} [0|1|10|11]

Returns the following on failure:

.. code-block:: javascript

   {exception string}

Example:

.. code-block:: javascript

   > ydb.data('^Population');
   10
   > ydb.data('^Population', 'Belgium');
   11
   >

~~~~~~
get()
~~~~~~

As a wrapper for the C function :ref:`ydb_get_s() <ydb-get-s-st-fn>`, :code:`get()` gets data from a global variable node, local variable node, or an intrinsic special variable.

Arguments as an object:

.. code-block:: javascript

   {
	global|local: string,
	subscripts:   string[]|number[]  // (optional)
   }

To get the value of an ISV, use the :code:`local` property. See example below.

Returns the following object on success:

.. code-block:: javascript

   {
	ok :          boolean,            // true
	global|local: string,
	subscripts:   string[]|number[],  // (optional)
	data:         string|number,
	defined:      boolean|number      // [false|true]|[0|1]
   }

.. note::

   :code:`get()` returns an empty string if a variable does not exist.

Returns the following object on failure:

.. code-block:: javascript

   {
        ok:           boolean,  // false
	errorCode:    number,
	errorMessage: string
   }

Example:

.. code-block:: javascript

   > ydb.get({global:'Population'});
   { ok: true, global: 'Population', data: '', defined: false }
   > ydb.get({global:'Population', subscripts: ["Belgium"]});
   {
     ok: true,
     global: 'Population',
     subscripts: [ 'Belgium' ],
     data: 3250000,
     defined: true
   }
   > ydb.get({global:'Population', subscripts: ['Belgium',20100401]});
   {
     ok: true,
     global: 'Population',
     subscripts: [ 'Belgium', 20100401 ],
     data: 10938740,
     defined: true
   }
   > ydb.get({local:'$zgbldir'})
   {
     ok: true,
     local: '$zgbldir',
     data: '/home/ydbuser/.yottadb/r1.34_x86_64/g/yottadb.gld',
     defined: true
   }
   >

Positional arguments:

.. code-block:: javascript

   ^global|$ISV|local, [subscripts+]

Returns the following on success:

.. code-block:: javascript

   {string|number}

Returns the following on failure:

.. code-block:: javascript

   {exception string}

Example:

.. code-block:: javascript

   > ydb.get('^Population', 'Belgium');
   3250000
   > ydb.get('^Population', 'USA', 20100401);
   308745538
   > ydb.get('$ZGBLDIR');
   '/home/ydbuser/.yottadb/r1.34_x86_64/g/yottadb.gld'
   >

~~~~~~~~~~~~~
increment()
~~~~~~~~~~~~~

As a wrapper for the C function :ref:`ydb_incr_s() <ydb-incr-s-st-fn>`, :code:`increment()` increments the value in a global or local variable node.

Arguments as an object:

.. code-block:: javascript

   {
	global|local: string,
	subscripts:   string[]|number[],  // (optional)
	increment:    number              // <1> (optional)
   }

Returns the following object on success:

.. code-block:: javascript

   {
	ok :          boolean,            // true
	global|local: string,
	subscripts:   string[]|number[],  // (optional)
	data:         string|number
   }

The :code:`data` property is the string representation of a :ref:`canonical number <canonical-numbers>`.

Returns the following object on failure:

.. code-block:: javascript

   {
        ok:           boolean,  // false
	errorCode:    number,
	errorMessage: string
   }

Example:

.. code-block:: javascript

   > ydb.get({local:'num'});
   { ok: true, local: 'num', data: 4, defined: true }
   > ydb.increment({local:'num'});
   { ok: true, local: 'num', data: 5 }
   >

Positional arguments:

.. code-block:: javascript

   ^global|local, [subscripts+]

Returns the following on success:

.. code-block:: javascript

   {string|number}

Returns the following on failure:

.. code-block:: javascript

   {exception string}

Example:

.. code-block:: javascript

   > ydb.get('^Z');
   155
   > ydb.increment('^Z');
   156
   >

~~~~~~~
kill()
~~~~~~~

As a wrapper for the C function :ref:`ydb_delete_s() <ydb-delete-s-st-fn>`, :code:`kill()` deletes a global or local variable node, or the entire tree.

Arguments as an object:

.. code-block:: javascript

   {
	global|local: string,
	subscripts:   string[]|number[],  // (optional)
        nodeOnly:     boolean|number      // <false>|[<0>|1] (optional)
   }

If no arguments are passed to :code:`kill()`, then all of the local variable nodes will be deleted.

Returns the following object on success, if arguments are passed:

.. code-block:: javascript

   {
	ok :          boolean,            // true
	global|local: string,
	subscripts:   string[]|number[],  // (optional)
	result:       number              // 0 (optional)
   }

Returns the following object on failure:

.. code-block:: javascript

   {
        ok:           boolean,  // false
        errorCode:    number,
	errorMessage: string
   }

Example:

.. code-block:: javascript

   > ydb.localDirectory();
   [ 'num', 'y' ]
   > ydb.kill();
   true
   > ydb.localDirectory();
   []
   > ydb.kill({global:'z'});
   { ok: true, global: 'z' }

Positional arguments:

.. code-block:: javascript

   ^global|local, [subscripts+]

Returns the following on success:

.. code-block:: javascript

   {boolean} true

Returns the following on failure:

.. code-block:: javascript

   {exception string}

Example:

.. code-block:: javascript

   > ydb.get('^Z');
   156
   > ydb.kill('^Z');
   true
   > ydb.get('^Z');
   ''

~~~~~~~~
lock()
~~~~~~~~

As a wrapper for the C function :ref:`ydb_lock_incr_s() <ydb-lock-incr-s-st-fn>`, :code:`lock()` locks a global or local variable node, incrementally.

Arguments as an object:

.. code-block:: javascript

   {
	global|local: string,
	subscripts:   string[]|number[],  // (optional)
	timeout:      number              // (optional)
   }

Returns the following object on success:

.. code-block:: javascript

   {
	ok :          boolean,            // true
	global|local: string,
	subscripts:   string[]|number[],  // (optional)
	result:       number              // [0|1]
   }

Returns the following object on failure:

.. code-block:: javascript

   {
        ok:           boolean,  // false
	errorCode:    number,
	errorMessage: string
   }

Positional arguments:

.. code-block:: javascript

   ^global|local, [subscripts+]

Returns the following on success:

.. code-block:: javascript

   {string|number} [0|1]

Returns the following on failure:

.. code-block:: javascript

   {exception string}

~~~~~~~~~~~~~~~~~~~~~~~~
nextNode()/next_node()
~~~~~~~~~~~~~~~~~~~~~~~~

:code:`nextNode()` returns the next global or local variable node. It wraps the C function :ref:`ydb_node_next_s() <ydb-node-next-s-st-fn>`, and then uses :ref:`ydb_get_s() <ydb-get-s-st-fn>` to get the value of the next node.

Arguments as an object:

.. code-block:: javascript

   {
	global|local: string,
	subscripts:   string[]|number[]  // (optional)
   }

Returns the following object on success:

.. code-block:: javascript

   {
	ok :          boolean,            // true
	global|local: string,
	subscripts:   string[]|number[],  // (optional)
	data:         string|number,
	defined:      boolean|number      // [false|true]|[0|1]
   }

Returns the following object on failure:

.. code-block:: javascript

   {
        ok:           boolean,  // false
	errorCode:    number,
	errorMessage: string
   }

Example:

.. code-block:: javascript

   > ydb.nextNode({global: 'Population'});
   {
     ok: true,
     global: 'Population',
     subscripts: [ 'Belgium' ],
     data: 1367000,
     defined: true
   }
   > ydb.nextNode({global: 'Population', subscripts: ["Belgium"]});
   {
     ok: true,
     global: 'Population',
     subscripts: [ 'Thailand' ],
     data: 8414000,
     defined: true
   }
   > ydb.nextNode({global: 'Population', subscripts: ["Thailand"]});
   {
     ok: true,
     global: 'Population',
     subscripts: [ 'USA' ],
     data: 325737000,
     defined: true
   }
   > ydb.nextNode({global: 'Population', subscripts: ["USA"]});
   {
     ok: true,
     global: 'Population',
     subscripts: [ 'USA', 17900802 ],
     data: 3929326,
     defined: true
   }
   > ydb.nextNode({global: 'Population', subscripts: ["USA",17900802]});
   {
     ok: true,
     global: 'Population',
     subscripts: [ 'USA', 18000804 ],
     data: 5308483,
     defined: true
   }
   >

Positional arguments:

.. code-block:: javascript

   ^global|local, [subscripts+]

Returns the following on success:

.. code-block:: javascript

   {string[]|number[]}

Returns the following on failure:

.. code-block:: javascript

   {exception string}

Example:

.. code-block:: javascript

   > ydb.nextNode('^Population', 'USA');
   [ 'USA', 17900802 ]
   > ydb.nextNode('^Population', 'USA', 17900802);
   [ 'USA', 18000804 ]
   > ydb.nextNode('^Population', 'USA', 18000804);
   [ 'USA', 20100401 ]
   > ydb.nextNode('^Population', 'USA', 20100401);
   []

~~~~~~~~~
order()
~~~~~~~~~

As a wrapper for the C function :ref:`ydb_subscript_next_s() <ydb-subscript-next-s-st-fn>`, :code:`order()` returns the next global or local variable subscript at the same level.

Arguments as an object:

.. code-block:: javascript

   {
	global|local: string,
	subscripts:   string[]|number[]   // (optional)
   }

Returns the following object on success:

.. code-block:: javascript

   {
	ok :          boolean,            // true
	global|local: string,
	subscripts:   string[]|number[],  // (optional)
	result:       string|number
   }

Returns the following object on failure:

.. code-block:: javascript

   {
        ok:           boolean,  // false
	errorCode:    number,
	errorMessage: string
   }

Example:

.. code-block:: javascript

   > ydb.order({global: 'Population', subscripts: ["Thailand"]});
   {
     ok: true,
     global: 'Population',
     subscripts: [ 'USA' ],
     result: 'USA'
   }
   > ydb.order({global: 'Population', subscripts: ["USA"]});
   { ok: true, global: 'Population', subscripts: [ '' ], result: '' }
   > ydb.order({global: 'Population', subscripts: ["USA",17900802]});
   {
     ok: true,
     global: 'Population',
     subscripts: [ 'USA', 18000804 ],
     result: 18000804
   }
   > ydb.order({global: 'Population', subscripts: ["USA",18000804]});
   {
     ok: true,
     global: 'Population',
     subscripts: [ 'USA', 20100401 ],
     result: 20100401
   }

Positional arguments:

.. code-block:: javascript

   ^global|local, [subscripts+]

Returns the following on success:

.. code-block:: javascript

   {string|number}

Returns the following on failure:

.. code-block:: javascript

   {exception string}

Example:

.. code-block:: javascript

   > ydb.order('^Population','Belgium');
   'Thailand'
   > ydb.order('^Population','Thailand');
   'USA'
   > ydb.order('^Population','USA');
   ''
   >

~~~~~~~~~~~~
previous()
~~~~~~~~~~~~

As a wrapper for the C function :ref:`ydb_subscript_previous_s() <ydb-subscript-previous-s-st-fn>`, :code:`previous()` returns the previous global or local variable subscript at the same level.

Arguments as an object:

.. code-block:: javascript

   {
	global|local: string,
	subscripts:   string[]|number[]  // (optional)
   }

Returns the following object on success:

.. code-block:: javascript

   {
	ok :          boolean,            // true
	global|local: string,
	subscripts:   string[]|number[],  // (optional)
	result:       string|number,
   }

Returns the following object on failure:

.. code-block:: javascript

   {
        ok:           boolean,  // false
	errorCode:    number,
	errorMessage: string
   }

Example:

.. code-block:: javascript

   > ydb.previous({global: 'Population', subscripts: ["USA",18000804]});
   {
     ok: true,
     global: 'Population',
     subscripts: [ 'USA', 17900802 ],
     result: 17900802
   }
   > ydb.previous({global: 'Population', subscripts: ["USA",17900802]});
   {
     ok: true,
     global: 'Population',
     subscripts: [ 'USA', '' ],
     result: ''
   }
   >

Positional arguments:

.. code-block:: javascript

   ^global|local, [subscripts+]

Returns the following on success:

.. code-block:: javascript

   {string|number}

Returns the following on failure:

.. code-block:: javascript

   {exception string}

Example:

.. code-block:: javascript

   > ydb.previous('^Population','USA', 18000804);
   17900802
   > ydb.previous('^Population','USA', 17900802);
   ''
   > ydb.previous('^Population','USA');
   'Thailand'
   >

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
previousNode()/previous_node()
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

:code:`previousNode()` returns the previous global or local variable node. It wraps the C function :ref:`ydb_node_previous_s() <ydb-node-previous-s-st-fn>`, and then uses :ref:`ydb_get_s() <ydb-get-s-st-fn>` to get the value of the previous node.

Arguments as an object:

.. code-block:: javascript

   {
	global|local: string,
	subscripts:   string[]|number[]  // (optional)
   }

Returns the following object on success:

.. code-block:: javascript

   {
	ok :          boolean,            // true
	global|local: string,
	subscripts:   string[]|number[],  // (optional)
	data:         string|number,
	defined:      boolean|number      // [false|true]|[0|1]
   }

Returns the following object on failure:

.. code-block:: javascript

   {
        ok:           boolean,  // false
	errorCode:    number,
	errorMessage: string
   }

Example:

.. code-block:: javascript

   > ydb.previousNode({global: 'Population', subscripts: ["USA",17900802]});
   {
     ok: true,
     global: 'Population',
     subscripts: [ 'USA' ],
     data: 325737000,
     defined: true
   }
   > ydb.previousNode({global: 'Population', subscripts: ["USA"]});
   {
     ok: true,
     global: 'Population',
     subscripts: [ 'Thailand' ],
     data: 8414000,
     defined: true
   }
   >

Positional arguments:

.. code-block:: javascript

   ^global|local, [subscripts+]

Returns the following on success:

.. code-block:: javascript

   {string[]|number[]}

Returns the following on failure:

.. code-block:: javascript

   {exception string}

Example:

.. code-block:: javascript

   > ydb.previousNode('^Population','USA', 17900802);
   [ 'USA' ]
   > ydb.previousNode('^Population','USA');
   [ 'Thailand' ]
   > ydb.previousNode('^Population','Thailand');
   [ 'Belgium', 20100401 ]
   > ydb.previousNode('^Population','Belgium', 20100401);
   [ 'Belgium', 18000804 ]
   >

~~~~~~~
set()
~~~~~~~

As a wrapper for C function :ref:`ydb_set_s() <ydb-set-s-st-fn>`, :code:`set()` sets a global variable node, local variable node, or an intrinsic special variable.

Arguments as an object:

.. code-block:: javascript

   {
	global|local: string,
	subscripts:   string[]|number[],  // (optional)
	data:         string|number
   }

Returns the following object on success:

.. code-block:: javascript

   {
	ok :          boolean,            // true
	global|local: string,
	subscripts:   string[]|number[],  // (optional)
	data:         string|number,
	result:       number              // 0 (optional)
   }

Returns the following object on failure:

.. code-block:: javascript

   {
        ok:           boolean,  // false
	errorCode:    number,
	errorMessage: string
   }

Example:

.. code-block:: javascript

   > ydb.set({local:'y', data:'Hello'})
   { ok: true, local: 'y', data: 'Hello' }
   >

Positional arguments:

.. code-block:: javascript

   ^global|$ISV|local, [subscripts+], data

Returns the following on success:

.. code-block:: javascript

   {boolean} true

Returns the following on failure:

.. code-block:: javascript

   {exception string}

Example:

.. code-block:: javascript

   > ydb.set('lclvar1',5);
   true
   > ydb.get('lclvar1');
   5
   > ydb.set('lclvar1','first', 10);
   true
   > ydb.get('lclvar1','first');
   10
   >

~~~~~~~~~~~~~~~
transaction()
~~~~~~~~~~~~~~~

As a wrapper for C function :ref:`ydb_tp_s() <ydb-tp-s-st-fn>`, it provides support for full ACID transactions.

It requires, as the first argument, a JavaScript function that takes no arguments. This function can contain in itself, other Nodem calls, nested :code:`transaction()` calls, or any other JavaScript code. By default no local variables are reset during transaction restarts.

.. note::

   The JavaScript function is run synchronously within the transaction by YottaDB, and every Nodem API that is called within the transaction must also be run synchronously.

An optional second argument, with one or two properties, can be passed to :code:`transaction()`:

* The first property, :code:`variables`, is an array of local variables whose values are reset to their original values whenever the transaction is restarted. If :code:`variables` has :code:`*` as its only array item, then every local variable will be reset during a transaction restart.
* The second property, :code:`type`, is a string which if set to :code:`Batch` (or :code:`batch` or :code:`BATCH`), will run the transaction in batch mode. Batch mode does not ensure Durability (but it always ensures Atomicity, Consistency, and Isolation).

In order to restart a transaction pass the string :code:`Restart` (or :code:`restart` or :code:`RESTART`) as the argument to the return statement. Similarly, in order to rollback a transaction pass the string :code:`Rollback` (or :code:`rollback` or :code:`ROLLBACK`) as the argument to the return statement. Any other argument to the return statement will commit the transaction, including functions without a return statement.

Returns the following on success:

.. code-block:: javascript

   {
	ok:            boolean,  // true
	statusCode:    number,
	statusMessage: string
   }

Returns the following on failure:

.. code-block:: javascript

   {
	ok:           boolean,  // false
	errorCode:    number,
	errorMessage: string
   }

Example:

.. code-block:: javascript

   const ydb=require('nodem').Ydb();
   ydb.open();

   console.log("Value of ^num before transaction: ", ydb.set({ global: 'num', data: 0 }));

   const transResult = ydb.transaction(() => {
       console.log("Starting transaction ... \n");
       let incrementGlobal = ydb.increment({ global: 'num'});
       if (incrementGlobal.errorCode === ydb.tpRestart) return 'Restart';
       if (!incrementGlobal.ok) return 'Rollback';
       console.log("Incrementing ^num: ", incrementGlobal);

       const result = ydb.get({ global: 'num'});
       if (result.errorCode === ydb.tpRestart) return 'Restart';
       if (!result.ok) return 'Rollback';
       console.log("^num: ", result);

       return 'Commit';
   }, { variables: ['*'] });

   console.log("Transaction exited ... \n");
   console.log("Transaction output: ", transResult);

Output:

.. code-block:: javascript

   Value of ^num before transaction:  { ok: true, global: 'num', data: 0 }
   Starting transaction ...

   Incrementing ^num:  { ok: true, global: 'num', data: 1 }
   ^num:  { ok: true, global: 'num', data: 1, defined: true }
   Transaction exited ...

   Transaction output:  { ok: true, statusCode: 0, statusMessage: 'Commit' }

Even though the :code:`transaction()` API runs synchronously, it is fully compatible with the Worker Threads API. By creating a new worker thread and running the :code:`transaction()` API, and any other APIs it calls in it, an asynchronous pattern can be emulated. Running the transaction will not block the main thread or any of the other worker threads. The `transaction.js <https://github.com/glwicksell/nodem/blob/master/examples/transaction.js>`_ example shows how the :code:`transaction()` API can be used with the Worker Threads API. See :ref:`worker-threads-api` for more information.

~~~~~~~~~~
unlock()
~~~~~~~~~~

As a wrapper for C function :ref:`ydb_lock_decr_s <ydb-lock-decr-s-st-fn>`, :code:`unlock()` decrements the count of the specified lock held by the process.

Arguments as an object:

.. code-block:: javascript

   {
	global|local: string,
	subscripts:   string[]|number[]  // (optional)
   }

Returns the following object on success:

.. code-block:: javascript

   {
	ok :          boolean,            // true
	global|local: string,
	subscripts:   string[]|number[],  // (optional)
	result:       number              // 0 (optional)
   }

Returns the following object on failure:

.. code-block:: javascript

   {
        ok:           boolean,  // false
	errorCode:    number,
	errorMessage: string
   }

Positional arguments:

.. code-block:: javascript

   ^global|local, [subscripts+]

Returns the following on success:

.. code-block:: javascript

   {boolean}|{string} true|0

Returns the following on failure:

.. code-block:: javascript

   {exception string}

+++++++++++++++++++++
Other API Functions
+++++++++++++++++++++

~~~~~~~~~
close()
~~~~~~~~~

Cleans up the process connection and/or the access to all the databases. Once the connection is closed, it cannot be reopened during the lifetime of the current process.

Arguments as an object:

.. code-block:: javascript

   {
	resetTerminal:   boolean  // <false> (optional)
   }

By setting the :code:`resetTerminal` property to true, the terminal settings will be reset once the connection to YottaDB has been closed.

Returns the following on success:

.. code-block:: javascript

   undefined|string  // 1

Returns the following on failure:

.. code-block:: javascript

   {exception string}

Example:

.. code-block:: javascript

   > ydb.close();
   undefined

~~~~~~~~~~~~~
configure()
~~~~~~~~~~~~~

Configures the parameters for the current thread's connection to YottaDB.

Arguments as an object:

.. code-block:: javascript

   // All of the following arguments are optional

   {
	charset|encoding: string,                 // [<utf8|utf-8>|m|binary|ascii]
	mode:             string,                 // [<canonical>|string]
	autoRelink:       boolean,                // <false>
	debug:            boolean|string|number   // <false>|[<off>|low|medium|high]|[<0>|1|2|3]
   }

Returns the following on success:

.. code-block:: javascript

   {
	ok:     boolean,        // true
	result: number,         // 1 (optional)
	pid:    number|string,
	tid:    number|string
   }

Example:

.. code-block:: javascript

   > const ydb=require('nodem').Ydb();
   undefined
   > ydb.open();
   { ok: true, pid: 66935, tid: 66935 }
   > ydb.configure({charset:'utf8', mode:'canonical', debug:2});
   [C 66935] DEBUG>  Nodem::configure enter
   [C 66935] DEBUG>>   debug: medium
   [C 66935] DEBUG>>   autoRelink: false
   [C 66935] DEBUG>>   mode: canonical
   [C 66935] DEBUG>>   charset: utf-8
   [C 66935] DEBUG>>   stat_buf: 0
   [C 66935] DEBUG>  Nodem::configure exit

   { ok: true, pid: 66935, tid: 66935 }
   >

~~~~~~~~~~~~
function()
~~~~~~~~~~~~

:code:`function()` is used to call an extrinsic (user-defined) function in M code. See `Extrinsic Functions <../ProgrammersGuide/langfeat.html#extrinsic-functions>`_ for more information.

Arguments as an object:

.. code-block:: javascript

   {
	function:   string,
	arguments:  string[]|number[]|[],  // (optional)
	autoRelink: boolean                // <false> (optional)
   }

Returns the following on success:

.. code-block:: javascript

   {
	ok:        boolean,               // true
	function:  string,
	arguments: string[]|number[]|[],  // (optional)
	result:    string|number
   }

Returns the following on failure:

.. code-block:: javascript

   {
	ok:           boolean,  // false
	errorCode:    number,
	errorMessage: string
   }

Example;

.. code-block:: javascript

   > ydb.function({function: '^HELLOWORLD()'});
   { ok: true, function: 'HELLOWORLD()', result: 'Hello World' }

where :code:`HELLOWORLD` routine is defined as follows:

.. code-block:: bash

   YDB>ZPRINT ^HELLOWORLD
   HELLOWORLD()
           QUIT "Hello World"

Positional arguments:

.. code-block:: javascript

   function, [arguments+]

Returns the following on success:

.. code-block:: javascript

   {string|number}

Returns the following on failure:

.. code-block:: javascript

   {exception string}

Example:

.. code-block:: javascript

   > ydb.function('^HELLOWORLD()');
   'Hello World'
   >

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
globalDirectory()/global_directory()
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Lists all the global variables stored in the database.

Arguments as an object:

.. code-block:: javascript

   // All of the following arguments are optional

   {
	max: number,
	lo:  string,
	hi:  string
   }

:code:`max` can be used to limit the number of global variables that are listed.
Setting :code:`lo` and :code:`hi` will only display the global variables that are between those values, with :code:`lo` included and :code:`hi` excluded. If only :code:`lo` is set, then the interval *ends* at the last global variable. Whereas if only :code:`hi` is set then the interval *starts* at the first global variable. See example below.

Returns the following on success:

.. code-block:: javascript

   [
	<global variable name>*  string
   ]

Returns the following on failure:

.. code-block:: javascript

   {
	ok:           boolean,  // false
	errorCode:    number,
	errorMessage: string
   }

Example:

.. code-block:: javascript

   > ydb.globalDirectory();
   [
     'Crab',
     'Horse',
     'hello',
     'num',
     'v4wTest',
     'x',
     'y'
   ]
   > ydb.globalDirectory({max:2});
   [ 'Crab', 'Horse' ]
   > ydb.globalDirectory({lo:'v', hi:'z'});
   [ 'v4wTest', 'x', 'y' ]

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
localDirectory()/local_directory()
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Lists all the local variables defined in the current scope.

Arguments as an object:

.. code-block:: javascript

   // All of the following arguments are optional

   {
	max: number,
	lo:  string,
	hi:  string
   }

:code:`max` can be used to limit the number of local variables that are listed.
Setting :code:`lo` and :code:`hi` will only display the local variables that are between those values, with :code:`lo` included and :code:`hi` excluded. If only :code:`lo` is set, then the interval *ends* at the last local variable. Whereas if only :code:`hi` is set then the interval *starts* at the first local variable. See example below.

Returns the following on success:

.. code-block:: javascript

   [
	<local variable name>*  string
   ]

Returns the following on failure:

.. code-block:: javascript

   {
	ok:           boolean,  // false
	errorCode:    number,
	errorMessage: string
   }

Example:

.. code-block:: javascript

   > ydb.set({local: 'day', data: 'Friday'});
   { ok: true, local: 'day', data: 'Friday' }
   > ydb.set({local: 'month', data: 'April'});
   { ok: true, local: 'month', data: 'April' }
   > ydb.set({local: 'date', data: 15});
   { ok: true, local: 'date', data: 15 }
   > ydb.localDirectory();
   [ 'date', 'day', 'month' ]
   > ydb.localDirectory({hi:'l'});
   [ 'date', 'day' ]
   >

~~~~~~~~~
merge()
~~~~~~~~~

:code:`merge()` is used to copy the entire tree or sub-tree from a global or local variable node, to another global or local variable node.

Arguments as an object:

.. code-block:: javascript

   {
	from: {
		global|local: string,
		subscripts:   string[]|number[]  // (optional)
	      },
	to:   {
		global|local: string,
		subscripts:   string[]|number[]  // (optional)
              }
   }

Returns the following on success:

.. code-block:: javascript

   {
	ok:     boolean,                           // true
	from:   {
		  global|local:	string,
		  subscripts:	string[]|number[]  // (optional)
	        },
	to:     {
		  global|local:	string,
		  subscripts:	string[]|number[]  // (optional)
       	        },
	result:	number                             // 1
   }

OR:

.. code-block:: javascript

   {
	ok:	       boolean,            // true
	global|local:  string,
	subscripts:    string[]|number[],  // (optional)
	result:	       string              // 1
   }

Returns the following on failure:

.. code-block:: javascript

   {
	ok :          boolean,  // false
	errorCode:    number,
	errorMessage: string
   }

Example:

.. code-block:: javascript

   > ydb.merge({ from: {global: 'PopBelgium'}, to: { global: 'Population', subscripts: ['Belgium']}});
   {
     from: { global: 'PopBelgium' },
     to: { global: 'Population', subscripts: [ 'Belgium' ] },
     ok: true
   }
   > ydb.get({global:'Population', subscripts: ['Belgium',18000804]});
   {
     ok: true,
     global: 'Population',
     subscripts: [ 'Belgium', 18000804 ],
     data: 3250000,
     defined: true
   }
   > ydb.get({global:'Population', subscripts: ['Belgium',20100401]});
   {
     ok: true,
     global: 'Population',
     subscripts: [ 'Belgium', 20100401 ],
     data: 10938740,
     defined: true
   }

Where the :code:`PopBelgium` global variable node has been set as follows:

.. code-block:: javascript

   > ydb.set({global: 'PopBelgium', subscripts: [ 18000804 ], data:3250000});
   {
     ok: true,
     global: 'PopBelgium',
     subscripts: [ 18000804 ],
     data: 3250000
   }
   > ydb.set({global: 'PopBelgium', subscripts: [ 20100401 ], data:10938740});
   {
     ok: true,
     global: 'PopBelgium',
     subscripts: [ 20100401 ],
     data: 10938740
   }

~~~~~~~~
open()
~~~~~~~~

:code:`open()` is used to initialize the YottaDB runtime environment. All the methods, except :code:`help()` and :code:`version()`, require the YottaDB runtime environment to be initialized.

Arguments as an object:

.. code-block:: javascript

   // All of the following arguments are optional

   {
	globalDirectory|namespace: string,                 // <none>
	routinesPath:              string,                 // <none>
	callinTable:               string,                 // <none>
	ipAddress|ip_address:      string,                 // <none>
	tcpPort|tcp_port:          number|string,          // <none>
	charset|encoding:          string,                 // [<utf8|utf-8>|m|binary|ascii]
	mode:                      string,                 // [<canonical>|string]
	autoRelink:                boolean,                // <false>
	debug:                     boolean|string|number,  // <false>|[<off>|low|medium|high]|[<0>|1|2|3]
	threadpoolSize:            number,                 // [1-1024] <4>
	signalHandler:             boolean|object          // [<true>|false] [<1>|0]
   }

where the :code:`signalHandler` object is as follows:

.. code-block:: javascript

   {
	sigint|SIGINT:   boolean,  // [<true>|false] (optional)
	sigterm|SIGTERM: boolean,  // [<true>|false] (optional)
	sigquit|SIGQUIT: boolean   // [<true>|false] (optional)
   }

The :code:`ipAddress` and :code:`tcpPort` properties are used to configure Nodem as a GT.CM client. See :ref:`gt-cm-client` section for more information.

Returns the following on success:

.. code-block:: javascript

   {
	ok:     boolean  // true
	result: number   // optional
	pid:    number
	tid:    number
   }

Returns the following on failure:

.. code-block:: javascript

   {
	ok:           boolean,  // false
	errorCode:    number,
	errorMessage: string
   }

Example:

.. code-block:: javascript

   > ydb.open();
   { ok: true, pid: 77379, tid: 77379 }

~~~~~~~~~~~~~
procedure()
~~~~~~~~~~~~~

Call a procedure or routine label in M code. It is similar to the :code:`function()` API, except that :code:`procedure()` is used to call M procedures or routines that do not return any values.

Arguments as an object:

.. code-block:: javascript

   {
	procedure|routine: string,
	arguments:         string[]|number[]|[],  // (optional)
	autoRelink:        boolean                // <false> (optional)
   }

Returns the following on success:

.. code-block:: javascript

   {
	ok:                boolean,               // true
	procedure|routine: string,
	arguments:         string[]|number[]|[],  // (optional)
	result:            string                 // (optional)
   }

Returns the following on failure:

.. code-block:: javascript

   {
	ok:           boolean,  // false
	errorCode:    number,
	errorMessage: string
   }

Example:

.. code-block:: javascript

   > ydb.procedure({procedure: '^TESTPRCDR', arguments: [155]});
   { ok: true, procedure: 'TESTPRCDR', arguments: [ 155 ] }
   > ydb.get({global: 'Z'})
   { ok: true, global: 'Z', data: 155, defined: true }
   >

where :code:`^TESTPRCDR` routine is defined as follows:

.. code-block:: bash

   YDB>zprint ^TESTPRCDR
   TESTPRCDR(VAL)
	   SET ^Z=VAL

Positional arguments:

.. code-block:: javascript

   procedure, [arguments+]

Returns the following on success:

.. code-block:: javascript

   {undefined}|{string} ''

Returns the following on failure:

.. code-block:: javascript

   {exception string}

Example:

.. code-block:: javascript

   > ydb.get('^Z');
   155
   > ydb.procedure('TESTPRCDR', 175)
   undefined
   > ydb.get('^Z');
   175
   >

~~~~~~~~~~~
version()
~~~~~~~~~~~

Displays the version data. It includes the YottaDB version if the runtime has been initialized.

Passing a function, taking two arguments (error and result), as the last argument calls the API asynchronously.

No arguments are needed for :code:`version()`.

It returns a string on success, and should never fail.

Example:

.. code-block:: javascript

   Welcome to Node.js v12.22.5.
   Type ".help" for more information.
   > const ydb=require('nodem').Ydb();
   undefined
   > ydb.version();
   'Node.js Adaptor for YottaDB: Version: 0.20.2 (ABI=72) [FWS]'
   > ydb.open();
   { ok: true, pid: 20381, tid: 20381 }
   > ydb.version();
   'Node.js Adaptor for YottaDB: Version: 0.20.2 (ABI=72) [FWS]; YottaDB Version: 1.34'

-------------------
Programming Notes
-------------------

The :code:`open()` call does not require any arguments, and connects the YottaDB runtime system to the Global Directory specified by the environment variable :code:`$ydb_gbldir`. To use a different Global Directory, than the one defined by :code:`$ydb_gbldir`, pass an object, to the :code:`open()` API, with a property called either :code:`globalDirectory` or :code:`namespace`, defined as the path to the global directory file for that database, e.g.,

.. code-block:: javascript

   > ydb.open({globalDirectory: process.env.HOME + '/g/db_utf.gld'});

++++++++++++++++++++
Calling M routines
++++++++++++++++++++

Nodem supports setting up a custom routines path, for resolving calls to M functions and procedures, via the :code:`routinesPath` property. By controlling :code:`routinesPath` an application can control the M routines that node.js application code can call, e.g.,

.. code-block:: javascript

   > const HOME = process.env.HOME;
   > ydb.open({routinesPath: `${HOME}/p/r130(${HOME}/p)`});

Nodem also supports setting the Call-In path directly in the :code:`open()` call via the :code:`callinTable` property. This can be handy if Nodem is being run in an environment that has other software that uses the YottaDB Call-In Interface, thus not causing any namespace issues. There is no need to set the :code:`$ydb_ci` environment variable in order for Nodem to be fully functional, e.g.,

.. code-block:: javascript

   > ydb.open({callinTable: process.env.HOME + '/nodem/resources/nodem.ci'});

.. _gt-cm-client:

++++++++++++++
GT.CM Client
++++++++++++++

Nodem can be configured to function as a `GT.CM client <../AdminOpsGuide/gtcm.html#gt-cm-client>`_, allowing it to connect with a remote database. The :code:`ipAddress` and/or :code:`tcpPort` property can be set in the :code:`open()` method, allowing Nodem to set up the environment to connect with a YottaDB database on a remote sever that already has a GT.CM server listening at that address and port.
If only :code:`ipAddress` or :code:`tcpPort` is defined, the other one will be set with a default value; 127.0.0.1 for :code:`ipAddress`, or 6789 for :code:`tcpPort`. Nodem will then set the :code:`$ydb_cm_NODEM` environment variable for that Nodem process only, with the address and port in the :code:`open()` call, e.g.,

.. code-block:: javascript

   > ydb.open({ipAddress: '127.0.0.1', tcpPort: 6789});

If using IPv6, surround the IP address with square brackets, e.g.,

.. code-block:: javascript

   > ydb.open({ipAddress: '[::1]', tcpPort: 6789});

A global directory file will need to be created or modified. It should map one or more database segments to a data file on the remote server being connected to. Note that the prefix to the :code:`-file=` argument in the example below must be NODEM, in order to match the :code:`$ydb_cm_NODEM` environment variable name that Nodem sets up.

.. code-block:: bash

   $ $ydb_dist/mumps -run GDE
   GDE> change -segment DEFAULT -file=NODEM:/home/dlw/g/gtm-server.dat

Make sure to have the data file, on the remote server, set up to the same path as the :code:`-file=` option in the global directory of the GT.CM client configuration. Start the GT.CM server on the same IP address and port as configured in the :code:`open()` call in Nodem.

.. code-block:: bash

   $ $ydb_dist/gtcm_gnp_server -log=gtcm.log -service 6789

.. note::

   GT.CM only allows remote connections for the database access APIs, not the :code:`function()` or :code:`procedure()` APIs. So while using Nodem in a remote GT.CM configuration, any call to the :code:`function()` or :code:`procedure()` APIs will result in local calls, not remote RPC calls. Also, nodes accessed by GT.CM cannot participate in transactions.

+++++++++++++++++++++
Character Encodings
+++++++++++++++++++++

Nodem supports two different character encodings, UTF-8 and M. It defaults to UTF-8 mode. M mode is similar to ASCII, except that it utilizes all 8 bits in a byte and collates slightly differently. Instead of collation based only on the character codes themselves, it sorts numbers before everything else. The character encoding that is set in Nodem is decoupled from the underlying character encoding set up for the YottaDB environment it is running in. So it is possible to work with UTF-8 encoded data in the database, while in Nodem, even if YottaDB hasn't been set up to work with UTF-8 directly. It can be set to UTF-8 mode directly by passing :code:`utf-8` or :code:`utf8`, case insensitively, to the :code:`charset` property. To work with an older byte-encoding scheme, that stores all characters in a single byte, set :code:`charset` to either :code:`m`, :code:`ascii`, or :code:`binary`, case insensitively. One thing to keep in mind is that Node.js internally stores data in UTF-16, but interprets data in UTF-8 in most cases. This can be controlled through the process stream encoding methods inside the Node.js code. Calling those methods to change the encoding to :code:`binary` or :code:`ascii`, will interpret the data as a byte encoding, using the character glyphs in the current locale, e.g.,

.. code-block:: javascript

   > process.stdin.setEncoding('binary');
   > process.stdout.setDefaultEncoding('binary');
   > ydb.open({charset: 'm'}); // For all threads

or

.. code-block:: javascript

   > process.stdin.setEncoding('binary');
   > process.stdout.setDefaultEncoding('binary');
   > ydb.configure({charset: 'm'}); // For the current thread

++++++++++++++++++++
Subscript Handling
++++++++++++++++++++

There are currently two different modes that Nodem supports for handling subscripts. The mode can be set to :code:`canonical` or :code:`string`. The default is :code:`canonical`, and interprets subscripts using the M canonical representation i.e., numeric subscripts will be represented numerically, rather than as strings, and numeric subscripts will collate before string subscripts. The other mode, :code:`string`, interprets all subscripts as strings, e.g.,

.. code-block:: javascript

   > ydb.open({mode: 'string'}); // For all threads

or

.. code-block:: javascript

   > ydb.configure({mode: 'string'}); // For the current thread

++++++++++++++++
Debugging Mode
++++++++++++++++

Nodem also has a debug tracing mode, in case something doesn't seem to be working right, or to see what happens to data as it moves through the Nodem APIs. It has four levels of debugging, defaulting to :code:`off`. The other debug levels are :code:`low`, :code:`medium`, and :code:`high`. Numbers from 0-3 can also be used. The higher the debug level, the more verbose the debug output will be, e.g.,

.. code-block:: javascript

   > ydb.open({debug: 'low'}); // For all threads
   [C 32649] DEBUG>  Nodem::open enter
   [C 32649] DEBUG>>   debug: low
   [C 32649] DEBUG>  Nodem::open exit

   { ok: true, pid: 32649, tid: 32649 }

or

.. code-block:: javascript

   > ydb.open({debug: 2}); // For all threads

or

.. code-block:: javascript

   > ydb.configure({debug: 'high'}); // For the current thread

+++++++++++++++++
Signal Handling
+++++++++++++++++

Nodem handles several common signals that are typically used to stop processes, by cleaning up the process connection, resetting the controlling terminal configuration, and stopping the Node.js process. These signals include :code:`SIGINT`, :code:`SIGTERM`, and :code:`SIGQUIT`. The handling of the :code:`SIGQUIT` signal will also generate a core dump of the process. All three signal handlers are on by default. However, the signal handling can be turned on or off directly, via passing true or false to a :code:`signalHandler` object (with properties for each of the signals) for each individual signal, or all of them at once, e.g.,

.. code-block:: javascript

   > ydb.open({signalHandler: {sigint: true, sigterm: false, sigquit: false}});

or

.. code-block:: javascript

   > ydb.open({signalHandler: false});

++++++++++++++++
Auto-relinking
++++++++++++++++

Nodem supports a feature called auto-relink, which will automatically relink a routine object containing any function or procedure called by the :code:`function()` or :code:`procedure()` API. By default auto-relink is off. It can be enabled in one of four ways. First, pass it as a property of the JavaScript object argument which is passed to the :code:`function()` or :code:`procedure()` API directly, with a value of true, or any non-zero number. This will turn on auto-relink just for that call. It can also be disabled, by setting :code:`autoRelink` to false, or 0, if it was already enabled by one of the global settings, e.g.,

.. code-block:: javascript

   > ydb.function({function: 'version^v4wTest', autoRelink: true});

Second, it can be enabled globally, for every thread, and for every call to the :code:`function()` (or :code:`procedure()`) API, by setting the same property in a JavaScript object passed to the :code:`open()` API, e.g.,

.. code-block:: javascript

   > ydb.open({autoRelink: true});

Third, it can be enabled globally, for the current thread, for every call to the :code:`function()` (or :code:`procedure()`) API, by setting the same property in a JavaScript object passed to the :code:`configure` API, e.g.,

.. code-block:: javascript

   > ydb.configure({autoRelink: true});

Fourth, it can also be enabled globally, for every thread, by setting the environment variable :code:`NODEM_AUTO_RELINK` to 1, or any other non-zero number, e.g.

.. code-block:: bash

   $ export NODEM_AUTO_RELINK=1
   $ node function.js

or

.. code-block:: bash

   $ NODEM_AUTO_RELINK=1 node function.js

++++++++++++++++++++
Asynchronous Calls
++++++++++++++++++++

Nodem's asynchronous APIs do their work in a separate thread pool, pre-allocated by Node.js via `libuv <https://github.com/libuv/libuv>`_. By default, four threads are created, and will take turns executing asynchronous calls, including asynchronous calls from other non-Nodem APIs. Nodem supports setting a different value for the pre-allocated thread pool for asynchronous calls, in its :code:`open()` API, up to a max of 1024, in the latest versions of Node.js, e.g.,

.. code-block:: javascript

   > ydb.open({threadpoolSize: 1024});

However, if the Node.js process executes any call asynchronously, from any API or module, before the YottaDB runtime environment has been initialized, then the :code:`threadpoolSize` property is ignored. So make sure to use :code:`open()`, to initialize the runtime environment first in any process, so as to control how large the pre-allocated thread pool is.

.. note::
   The Node.js core worker_thread API, which also allocates threads from the same worker thread pool in libuv, allows complete control of creating and destroying threads, and does not utilize the threadpoolSize (which just sets the libuv environment variable :code:`UV_THREADPOOL_SIZE`) set in the Nodem :code:`open()` API.

+++++++++++++++++++++++++++++
Restoring Terminal Settings
+++++++++++++++++++++++++++++

YottaDB changes some settings of its controlling terminal device, and Nodem resets them when it closes the database connection. By default, Nodem will restore the terminal device to the state it was in when the :code:`open()` call was invoked. Normally this is the desired option; however, the :code:`close()` call allows setting the terminal to typically sane settings, by setting the :code:`resetTerminal` property to true, or any non-zero number, e.g.,

.. code-block:: javascript

   > ydb.close({resetTerminal: true});

.. _worker-threads-api:

++++++++++++++++++++
Worker Threads API
++++++++++++++++++++

Nodem now supports the `Worker Threads API <https://nodejs.org/api/worker_threads.html>`_, for both synchronous and asynchronous calls. Since YottaDB is single threaded, initializing and cleaning up the runtime environment (i.e., using :code:`open()` and :code:`close()`) should only be done once during the process lifetime. Nodem's :code:`open()` and :code:`close()` APIs will only work when called from the main thread of the process. In order to work with the worker threads API, :code:`open()` should be called in the main thread before creating any worker threads, and :code:`close()` should be called in the main thread after all the worker threads have exited. To have access to the Nodem API, Nodem should be required in each worker thread as well as the main thread.

Nodem has the :code:`configure()` API which allows the worker threads to change the database configuration options of the current thread. There are four configuration options that can be set for the current thread: :code:`charset`, :code:`mode`, :code:`autoRelink`, and :code:`debug`. These options can be set in the :code:`open()` API, by the main thread, before any other Nodem calls are made, or they can be set in the :code:`configure()` API, in the main thread or worker threads, at any time.
