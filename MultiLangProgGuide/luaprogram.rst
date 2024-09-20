.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2022-2024 YottaDB LLC and/or its subsidiaries.#
.. # All rights reserved.                                        #
.. #                                                             #
.. #     This document contains the intellectual property        #
.. #     of its copyright holder(s), and is made available       #
.. #     under a license.  If you do not know the terms of       #
.. #     the license, please stop and do not read further.       #
.. #                                                             #
.. ###############################################################

.. raw:: html

    <img referrerpolicy="no-referrer-when-downgrade" src="https://download.yottadb.com/luaprogram.png" />

====================
Programming in Lua
====================

.. contents::
   :depth: 5

Programming YottaDB in `Lua <https://www.lua.org/>`_ is provided by `lua-yottadb <https://github.com/anet-be/lua-yottadb>`_, developed by `Mitchell <https://github.com/orbitalquark/>`_ and `Berwyn Hoyt <https://github.com/berwynhoyt/>`_, and sponsored by the `University of Antwerp <https://www.uantwerpen.be>`_. We acknowledge their contribution and thank them for the value it adds to the YottaDB community.

lua-yottadb wraps the YottaDB :ref:`c-simple-api` to provide a Lua API.

--------------
Installation
--------------
The YottaDB Lua API requires a minimum YottaDB release of r1.34 and is tested with a minimum Lua version of 5.1.

.. code-block:: bash

   sudo apt install lua5.4
   sudo apt install liblua5.4-dev
   git clone https://github.com/anet-be/lua-yottadb.git
   cd lua-yottadb
   make
   make test
   sudo make install

-------------------------
Introduction by example
-------------------------

Let's tinker with setting some database values in different ways:

.. code-block:: lua
  :force:

   ydb = require 'yottadb'
   n = ydb.node('^hello')  -- create node object pointing to YottaDB global ^hello

   n:get()  -- get current value of the node in the database
   -- nil
   n:set('Hello World')
   n:get()
   -- Hello World

   -- Equivalent ways to create a new subnode object pointing to an added 'cowboy' subscript
   n2 = ydb.node('^hello')('cowboy')
   n2 = ydb.node('^hello', 'cowboy')
   n2 = n('cowboy')
   n2 = n['cowboy']
   n2 = n.cowboy

   n2:set('Howdy partner!')  -- set ^hello('cowboy') to 'Howdy partner!'
   n2, n2:get()
   -- ^hello("cowboy")	Howdy partner!

   n2.ranches:set(3)  -- create subnode object ydb.node('^hello', 'cowboy', 'ranches') and set to 3
   n2.ranches.__ = 3   -- same as :set() but 3x faster (ugly but direct access to value)

   n3 = n.chinese  -- add a second subscript to '^hello', creating a third object
   n3:set('你好世界!') -- value can be number or string, including UTF-8
   n3, n3:get()
   -- hello("chinese")	你好世界!

We can also use other methods of the node object like ``incr() name() has_value() has_key() lock_incr()``:

.. code-block:: lua
  :force:

   n2.ranches:incr(2)  -- increment
   -- 5
   n2:name()
   -- cowboy
   n2:__name()  -- uglier but 15x faster access to node object methods
   -- cowboy

(Note: lua-yottadb is able to distinguish n:method(n) from subnode creation n.method. See
`details in the notes here <https://htmlpreview.github.io/?https://github.com/anet-be/lua-yottadb/blob/master/docs/yottadb.html#Class_node>`_.)

Now, let's try ``dump`` to see what we've got so far:

.. code-block:: lua
  :force:

   n:dump()

The output will be:

.. code-block:: lua
  :force:

   ^hello="Hello World"
   ^hello("chinese")="你好世界!"
   ^hello("cowboy")="Howdy partner!"
   ^hello("cowboy","ranches")="5"

We can delete a node -- either its *value* or its entire *tree*:

.. code-block:: lua
  :force:

   n:set(nil)  -- delete the value of '^hello', but not any of its child nodes
   n:get()
   -- nil
   n:dump()  -- the values of the child node are still in the database
   -- hello("chinese")="你好世界!!"
   -- hello("cowboy")="Howdy partner!"

   n:kill() -- delete both the value at the '^hello' node and all of its children
   n:dump()
   -- nil

**Doing something useful**

Let's use Lua to calculate the height of 3 oak trees, based on their shadow length and the angle of the sun.
Method ``settree()`` is a handy way to enter literal data into the database from a Lua table constructor:

.. code-block:: lua
  :force:

   trees = ydb.node('^oaks')  -- create node object pointing to YottaDB global ^oaks
   -- store initial data values into database subnodes ^oaks('1', 'shadow'), etc.
   trees:settree({{shadow=10, angle=30}, {shadow=13, angle=30}, {shadow=15, angle=45}})

   for oaktree, value, index in pairs(trees) do
        oaktree.height.__ = oaktree.shadow.__ * math.tan( math.rad(oaktree.angle.__) )
        print( string.format('Oak %s is %.1fm high', index, oaktree.height.__) )
     end
   -- Oak 1 is 5.8m high
   -- Oak 2 is 7.5m high
   -- Oak 3 is 15.0m high

You may also wish to look at ``node:gettree()`` which has multiple uses.
On first appearances, it just loads a database tree into a Lua table (opposite of ``settree`` above),
but it also allows you to iterate over a whole database tree and process each node through a filter function.
For example, to use `print` as a filter function, do ``node:gettree(nil, print)``.
Incidentally, lua-yottadb itself uses ``gettree``, to implement ``node:dump()``.

**Database transactions are also available**

.. code-block:: lua
  :force:

  > Znode = ydb.node('^Ztest')
  > transact = ydb.transaction(function(end_func)
    print("^Ztest starts as", Znode:get())
    Znode:set('value')
    end_func()
    end)

  > transact(ydb.trollback)  -- perform a rollback after setting Znode
  ^Ztest starts as	nil
  YDB Error: 2147483645: YDB_TP_ROLLBACK
  > Znode.get()  -- see that the data didn't get set
  nil

  > tries = 2
  > function trier()  tries=tries-1  if tries>0 then ydb.trestart() end  end
  > transact(trier)  -- restart with initial dbase state and try again
  ^Ztest starts as	nil
  ^Ztest starts as	nil
  > Znode:get()  -- check that the data got set after restart
  value

  > Znode:set(nil)
  > transact(function() end)  -- end the transaction normally without restart
  ^Ztest starts as	nil
  > Znode:get()  -- check that the data got set
  value

---------
Lua API
---------

.. include:: lua-yottadb-ydbdocs.rst
