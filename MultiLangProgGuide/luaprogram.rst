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

====================
Programming in Lua
====================

.. contents::
   :depth: 5

Programming YottaDB in `Lua <https://www.lua.org/>`_ is provided by `lua-yottadb <https://github.com/orbitalquark/lua-yottadb>`_, developed by `Mitchell <https://github.com/orbitalquark/>`_ and sponsored by the `University of Antwerp <https://www.uantwerpen.be>`_. We acknowledge their contribution and thank them for the value it adds to the YottaDB community.

lua-yottadb wraps the YottaDB :ref:`c-simple-api` to provide a Lua API.

--------------
Installation
--------------
The YottaDB Lua API requires a minimum YottaDB release of r1.32 and is tested with a minimum Lua version of 5.3.

The *Makefile* assumes that the Lua headers are installed at :code:`/usr/include/lua5.3`. If the Lua headers are elsewhere, either modify the *Makefile* or pass :code:`CFLAGS=-I/path/to/lua` to :code:`make`.

.. code-block:: bash

   $ apt install lua5.3
   $ apt install liblua5.3-dev
   $ git clone https://github.com/orbitalquark/lua-yottadb.git
   $ cd lua-yottadb
   $ make ydb_dist=$(pkg-config --variable=prefix yottadb)
   $

Copy the newly built :code:`_yottadb.so` and :code:`yottadb.lua` files to the path where Lua require() will be able to find it. Refer to the `Lua Modules documentation <https://www.lua.org/manual/5.4/manual.html#6.3>`_ for more information.

---------
Lua API
---------

+++++++++++++++++++++++
Lua Wrapper Functions
+++++++++++++++++++++++

.. _lua-data-func:

~~~~~~~~
data()
~~~~~~~~

As a wrapper for :ref:`ydb_data_s() <ydb-data-s-st-fn>`, :code:`data()` provides information about whether or not a global or local variable node has data and/or subtree.

.. code-block:: lua

   data(varname, subsarray)

**Parameters:**

* varname   : String variable name
* subsarray : Optional list of subscripts

**Return value**:

* integer

Following are the possible return values:

* 0.0  : there is neither a value nor a subtree; undefined variable node
* 1.0  : there is a value but no subtree
* 10.0 : there is no value but there is a subtree
* 11.0 : there are both a value and a subtree

Example:

.. code-block:: lua

   > ydb = require('yottadb')
   > ydb.data('^Population')
   10.0
   > ydb.data('^Population', {'USA'})
   11.0

To better understand the structure of the Population global variable node refer to the :ref:`mlpg-concepts` section. The :code:`Population` global variable has been set as follows:

.. code-block:: lua

   ydb.set('^Population', {'Belgium'}, 1367000)
   ydb.set('^Population', {'Thailand'}, 8414000)
   ydb.set('^Population', {'USA'}, 325737000)
   ydb.set('^Population', {'USA', '17900802'}, 3929326)
   ydb.set('^Population', {'USA', '18000804'}, 5308483)

~~~~~~~~~~~~~~
delete_node()
~~~~~~~~~~~~~~

As a wrapper for the C function :ref:`ydb_delete_s() <ydb-delete-s-st-fn>`, :code:`delete_node()` deletes a global or local variable node.

.. code-block:: lua

   delete_node(varname, subsarray)

**Parameters:**

* varname   : String variable name
* subsarray : Optional list of subscripts

Example:

.. code-block:: lua

   > ydb = require('yottadb')
   > ydb.set('^Population', {'Belgium'}, 1367000)
   > ydb.delete_node('^Population', {'Belgium'})
   > ydb.get('^Population', {'Belgium'})
   nil

~~~~~~~~~~~~~~
delete_tree()
~~~~~~~~~~~~~~

As a wrapper for the C function :ref:`ydb_delete_s() <ydb-delete-s-st-fn>`, :code:`delete_tree()` deletes the entire global or local variable node tree.

.. code-block:: lua

   delete_tree(varname, subsarray)

**Parameters:**

* varname   : String variable name
* subsarray : Optional list of subscripts

Example:

.. code-block:: lua

   > ydb = require('yottadb')
   > ydb.get('^Population', {'USA'})
   325737000
   > ydb.get('^Population', {'USA', '17900802'})
   3929326
   > ydb.get('^Population', {'USA', '18000804'})
   5308483
   > ydb.delete_tree('^Population', {'USA'})
   > ydb.data('^Population', {'USA'})
   0.0

~~~~~~
get()
~~~~~~

As a wrapper for the C function :ref:`ydb_get_s() <ydb-get-s-st-fn>`, :code:`get()` returns the value of a global or local variable node or an intrinsic variable.

.. code-block:: lua

   get(varname, subsarray)

**Parameters:**

* varname   : String variable name
* subsarray : Optional list of subscripts

**Return value**:

* string or nil

The return value is :code:`nil` if the variable node does not exist.

Example:

.. code-block:: lua

   > ydb = require('yottadb')
   > ydb.get('^Population')
   nil
   > ydb.get('^Population', {'Belgium'})
   1367000
   > ydb.get('$zgbldir')
   /home/ydbuser/.yottadb/r1.34_x86_64/g/yottadb.gld

~~~~~~~
incr()
~~~~~~~

As a wrapper for the C function :ref:`ydb_incr_s() <ydb-incr-s-st-fn>`, :code:`increment()` increments the value in a global or local variable node.

.. code-block:: lua

   incr(varname, subsarray, increment)

**Parameters:**

* varname   : String variable name
* subsarray : Optional list of subscripts
* increment : Optional string or number amount to increment by

The default value of :code:`increment` parameter is one.

**Return value**:

* incremented value

Example:

.. code-block:: lua

   > ydb = require('yottadb')
   > ydb.get('num')
   4
   > ydb.incr('num', 3)
   7
   > ydb.incr('num')
   8

~~~~~~~
lock()
~~~~~~~

As a wrapper for the C function :ref:`ydb_lock_s() <ydb-lock-s-st-fn>`, :code:`lock()` releases any locks held by the process and attempts to acquire all the requested locks.

.. code-block:: lua

   lock(keys, timeout)

**Parameters:**

* keys    : Optional list of variable nodes {varname[, subs]} to lock
* timeout : Optional timeout in seconds to wait for the lock

The default value of :code:`timeout` parameter is zero.

If :code:`keys` is omitted then :code:`lock()` just releases all the locks. The :code:`keys` parameter refers to the YottaDB key object. For more information on the key object refer :ref:`key() API function <key-api>`.

~~~~~~~~~~~~
lock_decr()
~~~~~~~~~~~~

As a wrapper for C function :ref:`ydb_lock_decr_s <ydb-lock-decr-s-st-fn>`, :code:`lock_decr()` decrements the count of the specified lock held by the process, releasing it if the count goes to zero or ignoring the invocation if the process does not hold the lock.

.. code-block:: lua

   lock_decr(varname, subsarray)

**Parameters:**

* varname   : String variable name
* subsarray : Optional list of subscripts

~~~~~~~~~~~~
lock_incr()
~~~~~~~~~~~~

As a wrapper for the C function :ref:`ydb_lock_incr_s() <ydb-lock-incr-s-st-fn>`, :code:`lock_incr()` attempts to acquire the requested lock without releasing any locks, incrementing the count if already held.

.. code-block:: lua

   lock_incr(varname, subsarray)

**Parameters:**

* varname   : String variable name
* subsarray : Optional list of subscripts
* timeout   : Optional timeout in seconds to wait for the lock

~~~~~~~~~~~~~
node_next()
~~~~~~~~~~~~~

As a wrapper for the C function :ref:`ydb_node_next_s() <ydb-node-next-s-st-fn>`, :code:`node_next()` returns the next global or local variable node.

.. code-block:: lua

   node_next(varname, subsarray)

**Parameters:**

* varname   : String variable name
* subsarray : Optional list of subscripts

**Return value**:

* list or nil

The return value is :code:`nil` if there is no next node.

Example:

.. code-block:: lua

   > ydb = require('yottadb')
   > print(table.concat(ydb.node_next('^Population'), ', '))
   Belgium
   > print(table.concat(ydb.node_next('^Population', {'Belgium'}), ', '))
   Thailand
   > print(table.concat(ydb.node_next('^Population', {'Thailand'}), ', '))
   USA
   > print(table.concat(ydb.node_next('^Population', {'USA'}), ', '))
   USA, 17900802
   > print(table.concat(ydb.node_next('^Population', {'USA', '17900802'}), ', '))
   USA, 18000804

.. note::

   The format used above to print the next node will give an error if there is no next node, i.e., the value returned is :code:`nil`. This case will have to be handled gracefully. The following code snippet is one way to handle :code:`nil` as the return value:
     .. code-block:: lua

	local ydb = require('yottadb')

	next = ydb.node_next('^Population', {'USA', '18000804'})

	if next ~= nil then
	   print(table.concat(next, ', '))
	else
	   print(next)
	end

~~~~~~~~~~~~~~~~~
node_previous()
~~~~~~~~~~~~~~~~~

As a wrapper for the C function :ref:`ydb_node_previous_s() <ydb-node-previous-s-st-fn>`, :code:`node_previous()` returns the previous global or local variable node.

.. code-block:: lua

   node_previous(varname, subsarray)

**Parameters:**

* varname   : String variable name
* subsarray : Optional list of subscripts

**Return value**:

* list or nil

The return value is :code:`nil` if there is no previous node.

Example:

.. code-block:: lua

   > ydb = require('yottadb')
   > print(table.concat(ydb.node_previous('^Population', {'USA', '18000804'}), ', '))
   USA, 17900802
   > print(table.concat(ydb.node_previous('^Population', {'USA', '17900802'}), ', '))
   USA
   > print(table.concat(ydb.node_previous('^Population', {'USA'}), ', '))
   Thailand
   > print(table.concat(ydb.node_previous('^Population', {'Thailand'}), ', '))
   Belgium

.. note::

   The note on handling nil return values in :code:`node_next()` applies to :code:`node_previous()` as well.

~~~~~~
set()
~~~~~~

As a wrapper for the C function :ref:`ydb_set_s() <ydb-set-s-st-fn>`, :code:`set()` sets the value of the global variable node, local variable node or intrinsic special variable.

.. code-block:: lua

   set(varname, subsarray, value)

**Parameters:**

* varname   : String variable name
* subsarray : Optional list of subscripts
* value     : String value to set, if number is provided it is converted to a string

Example:

.. code-block:: lua

   > ydb = require('yottadb')
   > ydb.set('^Population', {'Belgium'}, 1367000)
   > ydb.set('^Population', {'Thailand'}, 8414000)
   > ydb.set('^Population', {'USA'}, 325737000)
   > ydb.set('^Population', {'USA', '17900802'}, 3929326)
   > ydb.set('^Population', {'USA', '18000804'}, 5308483)

~~~~~~~~~~~
str2zwr()
~~~~~~~~~~~

As a wrapper for the C function :ref:`ydb_str2zwr_s() <ydb-str2zwr-s-st-fn>`, :code:`str2zwr()` returns the :ref:`zwrite formatted <zwrite-format>` version of the string provided.

.. code-block:: lua

   strzwr(s)

**Parameters:**

* s: String to format

**Return value**:

* zwrite formatted string

Example:

.. code-block:: bash

   > ydb=require('yottadb')
   > str='The quick brown dog\b\b\bfox jumps over the lazy fox\b\b\bdog.'
   > print(str)
   The quick brown fox jumps over the lazy dog.
   > ydb.str2zwr(str)
   "The quick brown dog"_$C(8,8,8)_"fox jumps over the lazy fox"_$C(8,8,8)_"dog."

In the above example the escape sequence :code:`\b` (backspace) is used.

~~~~~~~~~~~~~~~~~~
subscript_next()
~~~~~~~~~~~~~~~~~~

As a wrapper for the C function :ref:`ydb_subscript_next_s() <ydb-subscript-next-s-st-fn>`, :code:`subscript_next()` returns the next subscript, at the same level, of a global or local variable node.

.. code-block:: lua

   subscript_next(varname, subsarray)

**Parameters:**

* varname   : String variable name
* subsarray : Optional list of subscripts

**Return value**:

* string (subscript name) or nil

The return value is :code:`nil` if there is no next subscript.

Example:

.. code-block:: lua

   > ydb=require('yottadb')
   > ydb.subscript_next('^Population', {''})
   Belgium
   > ydb.subscript_next('^Population', {'Belgium'})
   Thailand
   > ydb.subscript_next('^Population', {'Thailand'})
   USA

~~~~~~~~~~~~~~~~~~~~~~
subscript_previous()
~~~~~~~~~~~~~~~~~~~~~~

As a wrapper for the C function :ref:`ydb_subscript_previous_s() <ydb-subscript-previous-s-st-fn>`, :code:`subscript_previous()` returns the previous subscript, at the same level, of a global or local variable node.

.. code-block:: lua

   subscript_previous(varname, subsarray)

**Parameters:**

* varname   : String variable name
* subsarray : Optional list of subscripts

**Return value**:

* string (subscript name) or nil

The return value is :code:`nil` if there is no previous subscript.

Example:

.. code-block:: lua

   > ydb=require('yottadb')
   > ydb.subscript_previous('^Population', {'USA', ''})
   18000804
   > ydb.subscript_previous('^Population', {'USA', '18000804'})
   17900802
   > ydb.subscript_previous('^Population', {'USA', '17900802'})
   nil
   > ydb.subscript_previous('^Population', {'USA'})
   Thailand
   >

~~~~~~
tp()
~~~~~~

As a wrapper for the C function :ref:`ydb_tp_s() <ydb-tp-s-st-fn>` , it provides support for full ACID transactions.

.. code-block:: lua

   tp(id, varnames, f, ...)

**Parameters:**

* id       : Optional string transaction id
* varnames : Optional list of variable names to restore on transaction restart
* f        : Function to call
* ...      : Optional arguments to pass to f

Example:

.. code-block:: lua

   local ydb = require('yottadb')

   function transfer_to_savings(t)
      local ok, e = pcall(ydb.incr, '^checking', -t)
      if (ydb.get_error_code(e) == ydb.YDB_TP_RESTART) then
         return ydb.YDB_TP_RESTART
      end
      if (not ok or tonumber(e)<0) then
         return ydb.YDB_TP_ROLLBACK
      end
      local ok, e = pcall(ydb.incr, '^savings', t)
      if (ydb.get_error_code(e) == ydb.YDB_TP_RESTART) then
         return ydb.YDB_TP_RESTART
      end
      if (not ok) then
         return ydb.YDB_TP_ROLLBACK
      end
      return ydb.YDB_OK
   end

   ydb.set('^checking', 200)
   ydb.set('^savings', 85000)

   print("Amount currently in checking account: $" .. ydb.get('^checking'))
   print("Amount currently in savings account: $" .. ydb.get('^savings'))

   print("Transferring $10 from checking to savings")
   local ok, e = pcall(ydb.tp, '', {'*'}, transfer_to_savings, 10)
   if (not e) then
      print("Transfer successful")
   elseif (ydb.get_error_code(e) == ydb.YDB_TP_ROLLBACK) then
      print("Transfer not possible. Insufficient funds")
   end

   print("Amount in checking account: $" .. ydb.get('^checking'))
   print("Amount in savings account: $" .. ydb.get('^savings'))

   print("Transferring $1000 from checking to savings")
   local ok, e = pcall(ydb.tp, '', {'*'}, transfer_to_savings, 1000)
   if (not e) then
      print("Transfer successful")
   elseif (ydb.get_error_code(e) == ydb.YDB_TP_ROLLBACK) then
      print("Transfer not possible. Insufficient funds")
   end

   print("Amount in checking account: $" .. ydb.get('^checking'))
   print("Amount in savings account: $" .. ydb.get('^savings'))

Output:

.. code-block:: bash

   Amount currently in checking account: $200
   Amount currently in savings account: $85000
   Transferring $10 from checking to savings
   Transfer successful
   Amount in checking account: $190
   Amount in savings account: $85010
   Transferring $1000 from checking to savings
   Transfer not possible. Insufficient funds
   Amount in checking account: $190
   Amount in savings account: $85010

.. note::

   When using the :code:`tp()` function, restarts and rollbacks need to be handled appropriately.

~~~~~~~~~~~
zwr2str()
~~~~~~~~~~~

As a wrapper for the C function :ref:`ydb_zwr2str_s() <ydb-zwr2str-s-st-fn>`, :code:`zwr2str()` provides the string format of the zwrite formatted string.

.. code-block:: lua

   zwr2str(s)

**Parameters:**

* s : String in zwrite format

**Return value**:

* string

Example:

.. code-block:: bash

   > ydb=require('yottadb')
   > str1='The quick brown dog\b\b\bfox jumps over the lazy fox\b\b\bdog.'
   > zwr_str=ydb.str2zwr(str1)
   > print(zwr_str)
   "The quick brown dog"_$C(8,8,8)_"fox jumps over the lazy fox"_$C(8,8,8)_"dog."
   > str2=ydb.zwr2str(zwr_str)
   > print(str2)
   The quick brown fox jumps over the lazy dog.
   > str1==str2
   true
   >

+++++++++++++++++++++
Other API Functions
+++++++++++++++++++++
~~~~~~~~~~~~~~~~~
get_error_code()
~~~~~~~~~~~~~~~~~

Returns the :ref:`YottaDB error code <err-ret-codes>` (if any) for the given error message.

.. code-block:: lua

   get_error_code(message)

**Parameters:**

* message : String error message

**Return value:**

* numeric YottaDB error code or nil

The return value is :code:`nil` if the message is not a YDB error.

:code:`get_error_code()` expects the error message string to start with :code:`YDB Error:`.

Example:

.. code-block:: lua

   > ydb=require('yottadb')
   > ydb.get_error_code('YDB Error: -150374122: %YDB-E-ZGBLDIRACC, Cannot access global directory !AD!AD!AD.')
   -150374122

.. _key-api:

~~~~~~~
key()
~~~~~~~

Creates and returns a new YottaDB key object.

**Parameters:**

* varname   : String variable name

**Return value:**

* key

The YottaDB object key has the following fields available:

* name      : key's subscript or variable name
* value     : key's value in the YottaDB database
* data      : refer :ref:`data() <lua-data-func>`
* has_value : checks whether or not the key has a value
* has_tree  : checks whether or not the key has a subtree

The YottaDB key object can access other API functions in the following manner, :code:`key:func()`.

Example:

.. code-block:: lua

   > ydb=require('yottadb')
   > belgium = ydb.key('^Population')('Belgium')
   > belgium.value
   1367000
   > thailand = ydb.key('^Population')('Thailand')
   > thailand.value
   8414000
   > usa = ydb.key('^Population')('USA')
   > usa.value
   325737000
   > print(usa.has_tree)
   true
   > for val in usa(''):subscripts() do
   >> print(val)
   >> end
   17900802
   18000804

~~~~~~~~
nodes()
~~~~~~~~

Returns an iterator for iterating over all the nodes of a global or local variable node.

.. code-block:: lua

   nodes(varname, subsarray, reverse)

**Parameters:**

* varname   : String variable name
* subsarray : Optional list of subscripts
* reverse   : Optional flag that indicates whether to iterate backwards. The default value is false.

**Return value:**

* iterator

Example:

.. code-block:: lua

   > ydb=require('yottadb')
   > for nodes in ydb.nodes('^Population') do
   >> print(table.concat(nodes, ', '))
   >> end
   Belgium
   Thailand
   USA
   USA, 17900802
   USA, 18000804
   > for usa_nodes in ydb.nodes('^Population', {'USA'}) do
   >> print(table.concat(usa_nodes, ', '))
   >> end
   USA, 17900802
   USA, 18000804

~~~~~~~~~~~~~~
subscripts()
~~~~~~~~~~~~~~

Returns an iterator for iterating over all subscripts in a global or local variable node.

.. code-block:: lua

   subscripts(varname, subsarray, reverse)

**Parameters:**

* varname   : String variable name
* subsarray : Optional list of subscripts
* reverse   : Optional flag that indicates whether to iterate backwards. The default value is false.

**Return value:**

* iterator

Example:

.. code-block:: lua

   > ydb=require('yottadb')
   > for subs in ydb.subscripts('^Population', {''}) do
   >> print(subs)
   >> end
   Belgium
   Thailand
   USA
   > for subs in ydb.subscripts('^Population', {'USA', ''}) do
   >> print(subs)
   >> end
   17900802
   18000804
   >

~~~~~~~~~~~~~~
transaction()
~~~~~~~~~~~~~~

Returns a transaction-safe version of the given functions such that it can be called with :ref:`YottaDB Transaction Processing <txn-proc>`.

.. code-block:: lua

   transaction(f)

**Parameters:**

* f : Function to convert

The transaction is committed if the function returns nothing or yottadb.YDB_OK, restarted if the function returns yottadb.YDB_TP_RESTART (f will be called again), or not committed if the function returns yottadb.YDB_TP_ROLLBACK or errors.

**Return value:**

* transaction-safe function


