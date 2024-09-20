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

.. raw:: html

    <img referrerpolicy="no-referrer-when-downgrade" src="https://download.yottadb.com/perlprogram.png" />

=====================
Programming in Perl
=====================

.. contents::
   :depth: 5

------------------
Perl Quick Start
------------------

The `YottaDB Perl pod <https://metacpan.org/pod/YottaDB>`_ (wrapper) was developed by `Stefan Traby <https://metacpan.org/author/OESI>`_. YottaDB would like to acknowledge his contribution and thank him for making it available to the YottaDB community.

The YottaDB Perl wrapper requires a minimum YottaDB version of r1.28.

The `Perl Quick Start`_ assumes that YottaDB has been already installed as described in the :ref:`mlpg-quick-start` section. After installing YottaDB, download the Perl wrapper, install and test it.

.. code-block:: bash

   $ git clone https://gitlab.com/oesiman/yottadb-perl.git
   $ cd yottadb-perl
   $ perl Makefile.PL
   $ make
   $ make test TEST_DB=1 # optional, accesses database
   $ sudo make install

-----------
Functions
-----------

+++++++++++++++++++++++++++++++
$data = y_data $var [, @subs]
+++++++++++++++++++++++++++++++

    The y_data function returns in $data:

.. code-block:: bash

     0  - no value and no subtree
     1  - has a value but no subtree
    10  - no value but a subtree
    11  - a value and a subtree exists

++++++++++++++
y_killall ()
++++++++++++++

    The y_killall function kills all local variables.

+++++++++++++++++++++++++++++++++++++
y_kill_excl [$var0 [,$var1 [,...]]]
+++++++++++++++++++++++++++++++++++++

    The y_kill_excl function deletes all local variables except the specified one(s). y_kill_excl without arguments is the same as y_killall.

++++++++++++++++++++++++++++
y_kill_node $var [, @subs]
++++++++++++++++++++++++++++

    Deletes a node but not a subtree.

++++++++++++++++++++++++++++
y_kill_tree $var [, @subs]
++++++++++++++++++++++++++++

    Deletes a node and all subtrees.

+++++++++++++++++++++++++++++
y_set $var, [@subs,] $value
+++++++++++++++++++++++++++++

    Sets the variable to $value.

+++++++++++++++++++++++++++++++
$value = y_get $var [, @subs]
+++++++++++++++++++++++++++++++

    Sets $value to the value of $var [, @subs]. Returns undef if not defined.

+++++++++++++++++++++++++++++++++++++
$value = y_get_croak $var [, @subs]
+++++++++++++++++++++++++++++++++++++

    Sets $value to the value of $var [, @subs]. Croaks if it is not defined.

++++++++++++++++++++++++++++++++
$value = y_next $var [, @subs]
++++++++++++++++++++++++++++++++

    Returns the next subscript if it exists or undef if it does not. Here is a sample "order-loop":

.. code-block:: bash

    my $x = "";
    while (defined ($x = y_next "^global", "subscript", $x)) {
        # ... do something with $x ...
    }

++++++++++++++++++++++++++++++++++++
$value = y_previous $var [, @subs]
++++++++++++++++++++++++++++++++++++

    Returns the previous subscript if it exists or undef if it does not.

++++++++++++++++++++++++++++++++++++++
(@subs) = y_node_next $var [, @subs]
++++++++++++++++++++++++++++++++++++++

    Returns the next node if it exists or an empty list if it does not.

++++++++++++++++++++++++++++++++++++++++++
(@subs) = y_node_previous $var [, @subs]
++++++++++++++++++++++++++++++++++++++++++

    Returns the previous node if it exists or an empty list if it does not.

+++++++++++++++++++++++++++++++++++++++++++++
$incval = y_incr $var [, @subs], $increment
+++++++++++++++++++++++++++++++++++++++++++++

    Increments $var [, @subs] by $increment and returns the result in $incval.

+++++++++++++++++++++++++++++++++++++++++
$string = y_zwr2str $zwr_encoded_string
+++++++++++++++++++++++++++++++++++++++++

    Decodes the $zwr_encoded_string to $string.

++++++++++++++++++++++++++++++++
$zwrstring = y_str2zwr $string
++++++++++++++++++++++++++++++++

    Encodes $string in zwr-format.

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
$status = y_lock $timeout [, \@name1 [, \@name2 [,...]]]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    Release all lock resources that have been held. If lock resource names are specified then lock all of them, returning 1 upon success or 0 upon failure of locking resources within $timeout limit.
    For example:

.. code-block:: bash

    y_lock 0, ["^temp", 1, "two"],
              ["^temp", 3] or die "can't lock";

++++++++++++++++++++++++++++++++++++++++++++++++
$status = y_lock_incr $timeout, $var [, @subs]
++++++++++++++++++++++++++++++++++++++++++++++++

    Try to acquire a lock on $var [, @subs] for $timeout seconds if not currently being held. Increment lock counter if the lock resource is already held. $timeout may be 0.0001 for example. Returns 1 on timeout 0 otherwise.

++++++++++++++++++++++++++++
y_lock_decr $var [, @subs]
++++++++++++++++++++++++++++

    Decrement lock counter on $var [, @subs] and release the lock if it reaches 0.

+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
$status = y_trans (\&code, $tansid [, lvar0 [, lvar1 ...]])
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    Run a transaction.
